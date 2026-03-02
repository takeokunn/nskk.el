;;; nskk-server.el --- SKK server (skkserv) client for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
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

;; SKK server (skkserv) TCP client for NSKK (Layer 1: Core Engine).
;;
;; Layer position: L1 (Core Engine) -- depends only on nskk-custom.
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
    ;; Use let-binding for coding system to set it synchronously before any
    ;; data arrives (avoids race with post-hoc set-process-coding-system).
    (condition-case err
        (let* ((coding nskk-server-coding-system)
               (proc (let ((coding-system-for-read coding)
                           (coding-system-for-write coding))
                       (open-network-stream
                        "nskk-server"
                        (get-buffer-create nskk-server--buffer-name)
                        nskk-server-host
                        nskk-server-portnum
                        :type 'plain))))
          (when proc
            (set-process-query-on-exit-flag proc nil)
            (setq nskk-server--process proc)
            ;; Register disconnect hook (idempotent)
            (unless nskk-server--kill-emacs-hook-registered
              (add-hook 'kill-emacs-hook #'nskk-server-close)
              (setq nskk-server--kill-emacs-hook-registered t))
            (nskk-debug-message "nskk-server-open: connected to %s:%d"
                                nskk-server-host nskk-server-portnum)
            proc))
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
    (condition-case nil
        (process-send-string nskk-server--process "0")
      (error nil)))
  (when nskk-server--process
    (condition-case nil
        (delete-process nskk-server--process)
      (error nil))
    (setq nskk-server--process nil))
  (let ((buf (get-buffer nskk-server--buffer-name)))
    (when buf
      (kill-buffer buf)))
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

(defun nskk-server--parse-response (response)
  "Parse a skkserv command-1 RESPONSE string into a list of candidate strings.
Expected formats:
  Found:     \"1/cand1/cand2/.../\\n\"  => (\"cand1\" \"cand2\" ...)
  Not found: \"4...\\n\"               => nil
  Error:     anything else           => nil

Strips annotations (\\\"word;annotation\\\" -> \\\"word\\\")."
  (when (and (stringp response) (> (length response) 0))
    (let ((first-char (aref response 0)))
      (cond
       ((= first-char ?1)
        ;; Strip the leading "1" and trailing newline, then split on "/"
        (let* ((body (string-trim-right (substring response 1) "[\r\n]+"))
               (parts (split-string body "/" t)))
          ;; Strip annotations "word;note" -> "word"
          (delq nil
                (mapcar (lambda (s)
                          (let* ((s (string-trim s))
                                 (semi (string-search ";" s)))
                            (when (> (length s) 0)
                              (if semi (substring s 0 semi) s))))
                        parts))))
       ;; First char ?4 = not found; anything else = error
       (t nil)))))

(defun nskk-server-lookup (key)
  "Look up KEY in the skkserv dictionary using command 1.
Sends \"1\" + KEY + \" \" to the server and waits up to
`nskk-server-timeout' seconds for a response.

Returns a list of candidate strings on success (e.g., (\"漢字\" \"感じ\")),
or nil when the key is not found, the server is unreachable, or any
network error occurs.  Never signals an error to the caller.

OKURI-ARI keys should be passed in their standard SKK format (e.g.,
\"かんじk\" for okurigana words); the server handles them natively.

DDSKK equivalent: skk-search-server-1"
  (when (and nskk-server-enable
             (stringp key)
             (> (length key) 0)
             (nskk-server-live-p))
    (condition-case err
        (let ((response nil)
              (start-time (float-time))
              (buf (process-buffer nskk-server--process)))
          ;; Clear the working buffer before sending request
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (erase-buffer)))
          ;; Send command 1: "1" + key + " " (trailing space is terminator)
          (process-send-string nskk-server--process (concat "1" key " "))
          ;; Poll with accept-process-output until newline received or timeout
          (while (and (nskk-server-live-p)
                      (< (- (float-time) start-time) nskk-server-timeout)
                      (or (null response)
                          (not (string-match-p "\n" response))))
            (accept-process-output nskk-server--process 0.1)
            (when (buffer-live-p buf)
              (setq response
                    (with-current-buffer buf
                      (buffer-string)))))
          ;; Report timing when enabled
          (when nskk-server-report-response
            (nskk-debug-message "nskk-server-lookup: key=%s elapsed=%.3fms"
                                key (* 1000 (- (float-time) start-time))))
          ;; Parse response
          (nskk-server--parse-response response))
      (error
       (nskk-debug-message "nskk-server-lookup: error for key=%s: %s"
                           key (error-message-string err))
       nil))))

(provide 'nskk-server)

;;; nskk-server.el ends here
