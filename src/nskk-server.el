;;; nskk-server.el --- SKK server (skkserv) client for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Keywords: i18n

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; SKK server (skkserv) TCP client for NSKK (Layer 2: Domain).
;;
;; Layer position: L2 (Domain) -- depends only on nskk-custom and nskk-prolog.
;;
;; Implements the SKK server protocol (skkserv) for remote dictionary
;; lookup.  This module connects to a running skkserv instance over TCP
;; and queries it for kanji readings using command 1 of the protocol.
;;
;; The SKK server protocol (RFC-style):
;;   Client sends: "1" + reading + " "   (no newline needed)
;;   Server found: "1/cand1/cand2/.../\n"
;;   Server miss:  "4" + reading + " \n"
;;   Disconnect:   "0"
;;
;; This module is opt-in: set `nskk-server-enable' to non-nil to activate.
;; When `nskk-server-enable' is nil (default), this module has zero
;; effect on the search pipeline.
;;
;; The connection is persistent (kept alive across multiple lookups).
;; `nskk-server-ensure-open' reconnects automatically if the connection drops.
;;
;; CPS pipeline:
;;
;;   nskk-server-lookup/k
;;     <- nskk--server-lookup-guards-p/k  (guard: enable flag + key validity)
;;         <- nskk-server-live-p/k        (guard: Prolog state + process status)
;;     <- nskk--server-with-response/k    (I/O: send + await response)
;;         <- nskk--server-await-response/k  (polling: wait for newline)
;;     <- nskk--server-parse-response/k   (parse: Prolog response-type dispatch)
;;         -> nskk--server-strip-annotation/k (pure string helper)
;;
;; Prolog predicates maintained by this module:
;;
;;   server-response-type/2
;;     Maps the first byte of a skkserv response to a type atom.
;;     Indexed by :hash on arg-1 (the prefix string).
;;     Facts: ("1" found), ("4" miss)
;;
;;   server-state/1
;;     Tracks connection state as a dynamic fact.
;;     Indexed by :hash on arg-1.
;;     Values: (open) when connected, (closed) when disconnected.
;;     Updated atomically by nskk-server-open and nskk-server-close.
;;
;; Key public API:
;; - `nskk-server-open'         -- establish connection to skkserv
;; - `nskk-server-close'        -- send disconnect command and clean up
;; - `nskk-server-live-p'       -- check if connection is active
;; - `nskk-server-ensure-open'  -- ensure connection, reconnect if needed
;; - `nskk-server-lookup'       -- look up a reading key via command 1
;;
;; Usage:
;;   (setq nskk-server-enable t)
;;   (setq nskk-server-host "localhost")  ; default
;;   (setq nskk-server-portnum 1178)       ; default

;;; Code:

(require 'subr-x)
(require 'nskk-custom)
(require 'nskk-prolog)
(require 'nskk-cps-macros)
(require 'nskk-debug nil t)

(declare-function nskk-debug-message "nskk-debug" (fmt &rest args))

;;;; Customization Group

(defgroup nskk-server nil
  "SKK server (skkserv) client settings."
  :prefix "nskk-server-"
  :group 'nskk)

(defcustom nskk-server-enable nil
  "When non-nil, enable SKK server (skkserv) as a dictionary fallback.
When nil (default), the server is completely disabled and has zero effect
on the search pipeline.  Users without a running skkserv should leave
this nil.

To enable skkserv lookup:
  (setq nskk-server-enable t)
  (setq nskk-server-host \"localhost\")
  (setq nskk-server-portnum 1178)"
  :type 'boolean
  :risky t
  :package-version '(nskk . "0.1.0")
  :group 'nskk-server)

(defcustom nskk-server-host "localhost"
  "Hostname or IP address of the skkserv instance.
Only used when `nskk-server-enable' is non-nil.
Note: connections to non-localhost hosts are unencrypted plaintext TCP."
  :type 'string
  :risky t
  :package-version '(nskk . "0.1.0")
  :group 'nskk-server)

(defcustom nskk-server-portnum 1178
  "TCP port number of the skkserv instance.
The default port 1178 is registered as \\='skkserv\\=' in /etc/services."
  :type 'natnum
  :risky t
  :package-version '(nskk . "0.1.0")
  :group 'nskk-server)

(defcustom nskk-server-coding-system 'euc-jp
  "Coding system used for skkserv communication.
Traditional skkserv implementations use EUC-JP.  Modern servers such as
yaskkserv2 may use UTF-8; set this to \\='utf-8 in that case."
  :type 'coding-system
  :package-version '(nskk . "0.1.0")
  :group 'nskk-server)

(defcustom nskk-server-timeout 1
  "Seconds to wait for a response from skkserv before giving up.
When the timeout is exceeded, `nskk-server-lookup' returns nil without
signalling an error.  Larger values reduce false timeouts on slow networks;
smaller values improve responsiveness when the server is unreachable.
Note: enabling skkserv may exceed the package's < 10ms search latency
target when the server is remote or slow."
  :type 'number
  :package-version '(nskk . "0.1.0")
  :group 'nskk-server)

(defcustom nskk-server-report-response nil
  "When non-nil, log skkserv response timing to the NSKK debug buffer.
Useful for diagnosing latency issues.  Requires `nskk-debug-enabled' to
be non-nil for the log entries to appear."
  :type 'boolean
  :package-version '(nskk . "0.1.0")
  :group 'nskk-server)

;;;; Internal State

(defvar nskk--server-process nil
  "Active skkserv network process object, or nil when disconnected.
Managed by `nskk-server-open' and `nskk-server-close'.")

(defconst nskk--server-buffer-name " *nskk-server*"
  "Name of the working buffer for skkserv I/O.")

(defvar nskk--server-kill-emacs-hook-registered nil
  "Non-nil when `nskk-server-close' is registered on `kill-emacs-hook'.
Used to avoid duplicate registrations (idempotent guard).")

;;;; Prolog Facts
;; Per the index-before-assert invariant, `nskk-prolog-set-index' is called
;; before any fact is added for each predicate.

;; server-response-type/2 — maps response prefix string to type symbol.
;; Hash-indexed on the first argument for O(1) lookup.
(nskk-prolog-set-index 'server-response-type 2 :hash)
(nskk-prolog-<- (server-response-type "1" found))
(nskk-prolog-<- (server-response-type "4" miss))

;; server-state/1 — dynamic connection state fact.
;; Hash-indexed; updated atomically by open/close.  Initialized to closed.
(nskk-prolog-set-index 'server-state 1 :hash)
(nskk-prolog-assert '((server-state closed)))

;;;; Core Functions

(defun/k nskk-server-live-p ()
  "Return non-nil if the skkserv connection is active.
Checks both the Emacs process status and the Prolog server-state/1 fact.
Returns nil when not connected or when `nskk--server-process' is nil."
  (if (and nskk--server-process
           (eq (process-status nskk--server-process) 'open)
           (nskk-prolog-holds-p '(server-state open)))
      (succeed t)
    (fail)))

(defun nskk--server-make-connection ()
  "Open a raw TCP connection to the configured skkserv instance.
Returns the process object on success, or nil if the connection fails.
Binds coding system for both read and write directions."
  (condition-case err
      (let ((coding-system-for-read  nskk-server-coding-system)
            (coding-system-for-write nskk-server-coding-system))
        (open-network-stream
         "nskk-server"
         (get-buffer-create nskk--server-buffer-name)
         nskk-server-host
         nskk-server-portnum
         :type 'plain))
    (error
     (nskk-debug-message "nskk-server-open: connection failed: %s"
                         (error-message-string err))
     nil)))

(defun/done nskk--server-configure-process (proc)
  "Configure PROC after a successful connection.
Sets the query-on-exit flag, records the process, registers the
kill-emacs hook (idempotent), and updates the Prolog server-state/1
fact to \\='open."
  (set-process-query-on-exit-flag proc nil)
  (setq nskk--server-process proc)
  (unless nskk--server-kill-emacs-hook-registered
    (add-hook 'kill-emacs-hook #'nskk-server-close)
    (setq nskk--server-kill-emacs-hook-registered t))
  (nskk-prolog-retract-all 'server-state 1)
  (nskk-prolog-assert '((server-state open)))
  (nskk-debug-message "nskk-server-open: connected to %s:%d"
                      nskk-server-host nskk-server-portnum))

(defun/k nskk-server-open ()
  "Open a TCP connection to the configured skkserv instance.
Connects to `nskk-server-host':`nskk-server-portnum'.  On success,
calls `nskk--server-configure-process' to set up the connection and
update Prolog state, then succeeds with the process object.
Returns nil (via sync wrapper) or fails if disabled or connection fails."
  (if nskk-server-enable
      (let ((proc (nskk--server-make-connection)))
        (if proc
            (progn
              (nskk--server-configure-process proc)
              (succeed proc))
          (fail)))
    (fail)))

(defun/done nskk-server-close ()
  "Send disconnect command to skkserv and clean up the connection.
Sends the protocol command \\='0\\=' to terminate the session cleanly,
kills the process and working buffer, and updates the Prolog
server-state/1 fact to \\='closed.
Safe to call when not connected (idempotent)."
  (when (nskk-server-live-p)
    (ignore-errors (process-send-string nskk--server-process "0")))
  (when nskk--server-process
    (ignore-errors (delete-process nskk--server-process))
    (setq nskk--server-process nil))
  (when-let* ((buf (get-buffer nskk--server-buffer-name)))
    (kill-buffer buf))
  (nskk-prolog-retract-all 'server-state 1)
  (nskk-prolog-assert '((server-state closed)))
  (nskk-debug-message "nskk-server-close: disconnected"))

(defun/k nskk-server-ensure-open ()
  "Ensure the skkserv connection is live, reconnecting if needed.
Succeeds with t if the connection is live after this call.
Fails immediately when `nskk-server-enable' is nil.
Fails when `nskk-server-enable' is non-nil but the connection cannot
be established."
  (if nskk-server-enable
      (if (nskk-server-live-p)
          (succeed t)
        (nskk-debug-message "nskk-server-ensure-open: reconnecting...")
        (<- _open-proc nskk-server-open)
        (succeed t))
    (fail)))

;;;; Protocol Implementation

(defun/k nskk--server-lookup-guards-p (key)
  "Succeed when KEY is safe to send to skkserv, fail otherwise.
String conditions checked synchronously: `nskk-server-enable' is non-nil,
KEY is a non-empty string, and KEY contains no whitespace or control
characters (which would cause protocol desync).
Then chains to `nskk-server-live-p/k' for connection liveness."
  (if (and nskk-server-enable
           (stringp key)
           (not (string-empty-p key))
           (not (string-match-p "[\x00-\x1F\x7F ]" key)))
      (<- live-check nskk-server-live-p)
    (fail)))

(defun/k nskk--server-strip-annotation (s)
  "Succeed with S with any SKK annotation removed.
SKK annotations follow a semicolon: \"word;annotation\" => \"word\".
Succeeds with S unchanged if no semicolon is present."
  (let ((semi (string-search ";" s)))
    (succeed (if semi (substring s 0 semi) s))))

(defun/k nskk--server-parse-response (response)
  "Parse a skkserv command-1 RESPONSE string into a candidate list.
Uses the Prolog server-response-type/2 predicate to dispatch on the
response prefix: \\='found\\=' prefix (\\\"1\\\") yields candidates,
any other prefix fails.

Strips annotations (\\\"word;note\\\" -> \\\"word\\\") via
`nskk--server-strip-annotation'.

Succeeds with the candidate list when at least one candidate is found.
Fails for not-found responses, empty responses, or non-string inputs."
  (if (and (stringp response)
           (not (string-empty-p response))
           (nskk-prolog-holds-p
            `(server-response-type ,(substring response 0 1) found)))
      (let* ((body  (string-trim-right (substring response 1) "[\r\n]+"))
             (parts (split-string body "/" t))
             (candidates
              (delq nil
                    (mapcar (lambda (s)
                              (let ((trimmed (string-trim s)))
                                (unless (string-empty-p trimmed)
                                  (nskk--server-strip-annotation trimmed))))
                            parts))))
        (if candidates (succeed candidates) (fail)))
    (fail)))

(defun/k nskk--server-await-response (proc buf deadline)
  "Poll PROC via BUF for a complete skkserv response line until DEADLINE.
Polls at 0.1-second intervals using `accept-process-output'.
Succeeds with the accumulated response string (containing a newline).
Fails on timeout or disconnection."
  (let (response)
    (while (and (nskk-server-live-p)
                (< (float-time) deadline)
                (not (and response (string-search "\n" response))))
      (accept-process-output proc 0.1)
      (setq response (with-current-buffer buf (buffer-string))))
    (if (and response (string-search "\n" response))
        (succeed response)
      (fail))))

(defun/k nskk--server-with-response (key)
  "Send skkserv command 1 for KEY, await the response.
Erases the I/O buffer, sends the protocol request \"1KEY \", then
delegates to `nskk--server-await-response/k' to poll for the reply.

When `nskk-server-report-response' is non-nil, logs elapsed time.

Succeeds with the raw response string.
Fails on network error or timeout."
  (let ((resp (condition-case err
                  (let* ((proc       nskk--server-process)
                         (buf        (process-buffer proc))
                         (start-time (float-time))
                         (deadline   (+ start-time nskk-server-timeout)))
                    (with-current-buffer buf (erase-buffer))
                    (process-send-string proc (concat "1" key " "))
                    (let ((r (nskk--server-await-response proc buf deadline)))
                      (when nskk-server-report-response
                        (nskk-debug-message
                         "nskk-server-lookup: key=%s elapsed=%.3fms"
                         key (* 1000 (- (float-time) start-time))))
                      r))
                (error
                 (nskk-debug-message "nskk-server-lookup: error for key=%s: %s"
                                     key (error-message-string err))
                 nil))))
    (if resp (succeed resp) (fail))))

(defun/k nskk-server-lookup (key)
  "Look up KEY in the skkserv dictionary using command 1.
Returns a list of candidate strings on success (e.g., (\"漢字\" \"感じ\")),
or nil when the key is not found, the server is unreachable, or any
network error occurs.  Never signals an error to the caller.

OKURI-ARI keys should be passed in their standard SKK format (e.g.,
\"かんじk\" for okurigana words); the server handles them natively.

CPS pipeline: guard -> I/O -> parse."
  (<- _guard nskk--server-lookup-guards-p key)
  (<- resp nskk--server-with-response key)
  (<- result nskk--server-parse-response resp)
  (succeed result))

(provide 'nskk-server)

;;; nskk-server.el ends here
