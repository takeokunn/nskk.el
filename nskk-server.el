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
;; Layer position: L2 (Domain) -- depends only on nskk-custom.
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
;; CPS I/O pipeline used in `nskk-server-lookup':
;;
;;   nskk-server-lookup
;;     -> nskk-server--lookup-guards-p   (pure guard predicate)
;;     -> nskk-server--with-response     (CPS I/O layer: send + await + report)
;;         -> nskk-server--await-response (polling helper)
;;         -> nskk-server--parse-response (continuation / pure parser)
;;             -> nskk-server--strip-annotation (pure string helper)
;;
;; Prolog predicates maintained by this module: none.
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

(require 'nskk-custom)
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
  :safe #'booleanp
  :group 'nskk-server)

(defcustom nskk-server-host "localhost"
  "Hostname or IP address of the skkserv instance.
Only used when `nskk-server-enable' is non-nil.
Note: connections to non-localhost hosts are unencrypted plaintext TCP.
DDSKK equivalent: skk-server-host"
  :type 'string
  :safe #'stringp
  :group 'nskk-server)

(defcustom nskk-server-portnum 1178
  "TCP port number of the skkserv instance.
The default port 1178 is registered as \\='skkserv\\=' in /etc/services.
DDSKK equivalent: skk-server-portnum"
  :type 'integer
  :safe #'integerp
  :group 'nskk-server)

(defcustom nskk-server-coding-system 'euc-jp
  "Coding system used for skkserv communication.
Traditional skkserv implementations use EUC-JP.  Modern servers such as
yaskkserv2 may use UTF-8; set this to \\='utf-8 in that case.
DDSKK equivalent: (set-process-coding-system ... coding coding)"
  :type 'coding-system
  :safe #'coding-system-p
  :group 'nskk-server)

(defcustom nskk-server-timeout 1
  "Seconds to wait for a response from skkserv before giving up.
When the timeout is exceeded, `nskk-server-lookup' returns nil without
signalling an error.  Larger values reduce false timeouts on slow networks;
smaller values improve responsiveness when the server is unreachable.
Note: enabling skkserv may exceed the package's < 10ms search latency
target when the server is remote or slow."
  :type 'number
  :safe #'numberp
  :group 'nskk-server)

(defcustom nskk-server-report-response nil
  "When non-nil, log skkserv response timing to the NSKK debug buffer.
Useful for diagnosing latency issues.  Requires `nskk-debug-enabled' to
be non-nil for the log entries to appear.
DDSKK equivalent: skk-server-report-response"
  :type 'boolean
  :safe #'booleanp
  :group 'nskk-server)

;;;; Internal State

(defvar nskk-server--process nil
  "Active skkserv network process object, or nil when disconnected.
Managed by `nskk-server-open' and `nskk-server-close'.")

(defconst nskk-server--buffer-name " *nskk-server*"
  "Name of the working buffer for skkserv I/O.")

(defvar nskk-server--kill-emacs-hook-registered nil
  "Non-nil when `nskk-server-close' is registered on `kill-emacs-hook'.
Used to avoid duplicate registrations (idempotent guard).")

;;;; Protocol Constants
;; Separate protocol data from dispatch logic per data/logic separation.

(defconst nskk-server--response-found ?1
  "First character of a successful skkserv lookup response.")

;;;; Core Functions

(defun nskk-server-live-p ()
  "Return non-nil if the skkserv connection is active.
Checks the Emacs process status for `nskk-server--process'.
Returns nil when not connected, `nskk-server-enable' is nil,
or when `nskk-server--process' is nil."
  (and nskk-server--process
       (eq (process-status nskk-server--process) 'open)))

(defun nskk-server-open ()
  "Open a TCP connection to the configured skkserv instance.
Connects to `nskk-server-host':`nskk-server-portnum' and sets the
process coding system to `nskk-server-coding-system' on both read and
write directions.  Sets `query-on-exit-flag' to nil on the process so
Emacs exits cleanly without prompting about the open connection.

Registers `nskk-server-close' on `kill-emacs-hook' (idempotent).

Returns the process object on success, or nil if the connection fails.
On failure, silently returns nil without signalling."
  (when nskk-server-enable
    (condition-case err
        (when-let* ((proc (let ((coding-system-for-read  nskk-server-coding-system)
                               (coding-system-for-write nskk-server-coding-system))
                            (open-network-stream
                             "nskk-server"
                             (get-buffer-create nskk-server--buffer-name)
                             nskk-server-host
                             nskk-server-portnum
                             :type 'plain))))
          (set-process-query-on-exit-flag proc nil)
          (setq nskk-server--process proc)
          (unless nskk-server--kill-emacs-hook-registered
            (add-hook 'kill-emacs-hook #'nskk-server-close)
            (setq nskk-server--kill-emacs-hook-registered t))
          (nskk-debug-message "nskk-server-open: connected to %s:%d"
                              nskk-server-host nskk-server-portnum)
          proc)
      (error
       (nskk-debug-message "nskk-server-open: connection failed: %s"
                           (error-message-string err))
       (setq nskk-server--process nil)
       nil))))

(defun nskk-server-close ()
  "Send disconnect command to skkserv and clean up the connection.
Sends the protocol command \\='0\\=' to ask the server to terminate the
session cleanly, then kills the process and working buffer.
Safe to call when not connected (idempotent).
DDSKK equivalent: skk-disconnect-server"
  (when (nskk-server-live-p)
    (ignore-errors (process-send-string nskk-server--process "0")))
  (when nskk-server--process
    (ignore-errors (delete-process nskk-server--process))
    (setq nskk-server--process nil))
  (when-let* ((buf (get-buffer nskk-server--buffer-name)))
    (kill-buffer buf))
  (nskk-debug-message "nskk-server-close: disconnected"))

(defun nskk-server-ensure-open ()
  "Ensure the skkserv connection is live, reconnecting if needed.
Returns non-nil if the connection is live after this call, nil otherwise.
When `nskk-server-enable' is nil, returns nil immediately without
attempting any connection.
DDSKK equivalent: skk-open-server"
  (when nskk-server-enable
    (if (nskk-server-live-p)
        t
      (nskk-debug-message "nskk-server-ensure-open: reconnecting...")
      (when (nskk-server-open)
        t))))

;;;; Protocol Implementation

(defun nskk-server--lookup-guards-p (key)
  "Return non-nil when KEY can be sent to skkserv.
All five conditions must hold: `nskk-server-enable' is non-nil, KEY is
a non-empty string, KEY contains no whitespace or control characters
\(which would cause protocol desync since skkserv uses a trailing space
as the request delimiter), and the connection is currently live."
  (and nskk-server-enable
       (stringp key)
       (> (length key) 0)
       (not (string-match-p "[\x00-\x1F\x7F ]" key))
       (nskk-server-live-p)))

(defun nskk-server--strip-annotation (s)
  "Strip a SKK dictionary annotation from candidate string S.
SKK annotations follow a semicolon: \\\"word;annotation\\\" => \\\"word\\\".
Returns S unchanged if no semicolon is present."
  (let ((semi (string-search ";" s)))
    (if semi (substring s 0 semi) s)))

(defun nskk-server--parse-response (response)
  "Parse a skkserv command-1 RESPONSE string into a list of candidate strings.
Expected formats:
  Found:     \"1/cand1/cand2/.../\\n\"  => (\"cand1\" \"cand2\" ...)
  Not found: \"4...\\n\"               => nil
  Other:     anything else           => nil

Strips annotations (\\\"word;note\\\" -> \\\"word\\\") via
`nskk-server--strip-annotation'."
  (when (and (stringp response)
             (> (length response) 0)
             (= (aref response 0) nskk-server--response-found))
    (let* ((body  (string-trim-right (substring response 1) "[\r\n]+"))
           (parts (split-string body "/" t)))
      (delq nil
            (mapcar (lambda (s)
                      (let ((trimmed (string-trim s)))
                        (when (> (length trimmed) 0)
                          (nskk-server--strip-annotation trimmed))))
                    parts)))))

(defun nskk-server--await-response (proc buf deadline)
  "Poll PROC via BUF for a complete skkserv response line until DEADLINE.
Polls at 0.1-second intervals using `accept-process-output'.
Returns the accumulated response string (containing a newline) or nil
on timeout or disconnection."
  (let (response)
    (while (and (nskk-server-live-p)
                (< (float-time) deadline)
                (not (and response (string-search "\n" response))))
      (accept-process-output proc 0.1)
      (setq response (with-current-buffer buf (buffer-string))))
    response))

(defun nskk-server--with-response (key cont)
  "Send skkserv command 1 for KEY, await the response, then call CONT.
CONT is a function of one argument -- the raw response string (or nil
on timeout or disconnection) -- whose return value is returned to the
caller.  This is the CPS I/O layer; CONT is typically
`nskk-server--parse-response'.

When `nskk-server-report-response' is non-nil, logs elapsed time to
the NSKK debug buffer."
  (let* ((proc       nskk-server--process)
         (buf        (process-buffer proc))
         (start-time (float-time))
         (deadline   (+ start-time nskk-server-timeout)))
    (with-current-buffer buf (erase-buffer))
    (process-send-string proc (concat "1" key " "))
    (let ((response (nskk-server--await-response proc buf deadline)))
      (when nskk-server-report-response
        (nskk-debug-message "nskk-server-lookup: key=%s elapsed=%.3fms"
                            key (* 1000 (- (float-time) start-time))))
      (funcall cont response))))

(defun nskk-server-lookup (key)
  "Look up KEY in the skkserv dictionary using command 1.
Returns a list of candidate strings on success (e.g., (\"漢字\" \"感じ\")),
or nil when the key is not found, the server is unreachable, or any
network error occurs.  Never signals an error to the caller.

OKURI-ARI keys should be passed in their standard SKK format (e.g.,
\"かんじk\" for okurigana words); the server handles them natively.

DDSKK equivalent: skk-search-server-1"
  (when (nskk-server--lookup-guards-p key)
    (condition-case err
        (nskk-server--with-response key #'nskk-server--parse-response)
      (error
       (nskk-debug-message "nskk-server-lookup: error for key=%s: %s"
                           key (error-message-string err))
       nil))))

(defun nskk-server-lookup/k (key on-found on-not-found)
  "CPS variant of `nskk-server-lookup'.
Look up KEY via skkserv and call ON-FOUND with the candidates list, or
ON-NOT-FOUND with no arguments when no candidates are found.
If KEY is not a string, neither ON-FOUND nor ON-NOT-FOUND is called.
Returns the return value of ON-FOUND or ON-NOT-FOUND."
  (let ((result (nskk-server-lookup key)))
    (if result
        (funcall on-found result)
      (funcall on-not-found))))

(provide 'nskk-server)

;;; nskk-server.el ends here
