;;; nskk-server.el --- SKK server (skkserv) client for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n convenience

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

;; SKK server (skkserv) client for NSKK.

;;; Code:

(require 'subr-x)
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

  (defvar nskk--server-pending-cleanups nil
    "Owned server resources waiting for a later cleanup attempt.
Each entry is a cons of (PROCESS . BUFFER).  Either component may be nil.")

(defconst nskk--server-buffer-name " *nskk-server*"
  "Name of the working buffer for skkserv I/O.")

(defconst nskk--server-max-response-size (* 1024 1024)
  "Maximum size, in bytes, of a single skkserv response.
A well-formed skkserv reply is terminated by a newline and is far smaller
than this cap.  If the accumulated response exceeds this size without a
terminating newline, `nskk--server-await-response' treats the reply as a
protocol error: it resets the connection and fails.  This bounds memory
use against a misbehaving or malicious server that never sends a newline.")

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
  (if (and (processp nskk--server-process)
           (eq (process-status nskk--server-process) 'open)
           (nskk-prolog-holds-p '(server-state open)))
      (succeed t)
    (fail)))

(defun nskk--server-process-filter (proc chunk)
  "Retain a bounded first response line from PROC's decoded CHUNK.
Set the `nskk-response-overflow' process property instead of inserting a
chunk that would exceed `nskk--server-max-response-size'."
  (unless (or (process-get proc 'nskk-response-overflow)
              (process-get proc 'nskk-response-complete))
    (let* ((newline (string-search "\n" chunk))
           (response-chunk (if newline
                               (substring chunk 0 (1+ newline))
                             chunk))
           (output-bytes (or (process-get proc 'nskk-response-bytes) 0))
           (new-size (+ output-bytes (string-bytes response-chunk))))
      (if (> new-size nskk--server-max-response-size)
          (process-put proc 'nskk-response-overflow t)
        (process-put proc 'nskk-response-bytes new-size)
        (when-let* ((buf (process-buffer proc))
                    ((buffer-live-p buf)))
          (with-current-buffer buf
            (insert response-chunk)))
        (when newline
          (process-put proc 'nskk-response-complete t))))))

(defun nskk--server-live-owned-process (proc)
    "Return PROC while its ownership still needs to be retained."
    (condition-case nil
        (and (processp proc) (process-live-p proc) proc)
      ((error quit) (and (processp proc) proc))))

  (defun nskk--server-live-owned-buffer (buffer)
    "Return BUFFER while its ownership still needs to be retained."
    (condition-case nil
        (and (bufferp buffer) (buffer-live-p buffer) buffer)
      ((error quit) (and (bufferp buffer) buffer))))

  (defun nskk--server-register-pending-cleanup (proc buffer)
    "Retain ownership of PROC and BUFFER for a later cleanup attempt."
    (when (or proc buffer)
      (unless (cl-find-if
               (lambda (entry)
                 (and (eq (car entry) proc)
                      (eq (cdr entry) buffer)))
               nskk--server-pending-cleanups)
        (push (cons proc buffer) nskk--server-pending-cleanups))
      (condition-case nil
          (progn
            (unless (memq #'nskk-server-close kill-emacs-hook)
              (add-hook 'kill-emacs-hook #'nskk-server-close))
            (setq nskk--server-kill-emacs-hook-registered t))
        ((error quit) nil))))

  (defun nskk--server-cleanup-owned-resources (proc buffer)
    "Best-effort cleanup of owned PROC and BUFFER.
Return a cons containing the resources that still require cleanup."
    (let ((inhibit-quit t)
          delete-completed
          kill-completed)
      ;; A first call can fail before taking effect.  Retry once, but do not
      ;; repeat calls that returned normally.
      (dotimes (_attempt 2)
        (unless delete-completed
          (condition-case nil
              (when (and proc
                         (or (not (processp proc))
                             (process-live-p proc)))
                (delete-process proc)
                (setq delete-completed t))
            ((error quit) nil))))
      (dotimes (_attempt 2)
        (unless kill-completed
          (condition-case nil
              (when (and (bufferp buffer) (buffer-live-p buffer))
                (with-current-buffer buffer
                  (let ((kill-buffer-hook nil)
                        (kill-buffer-query-functions nil)
                        (buffer-offer-save nil))
                    (kill-buffer buffer)))
                (setq kill-completed t))
            ((error quit) nil))))
      (let ((live-proc (nskk--server-live-owned-process proc))
            (live-buffer (nskk--server-live-owned-buffer buffer)))
        (and (or live-proc live-buffer)
             (cons live-proc live-buffer)))))

  (defun nskk--server-drain-pending-cleanups ()
    "Retry all pending cleanup entries and return non-nil when none remain."
    (let ((pending nskk--server-pending-cleanups))
      (setq nskk--server-pending-cleanups nil)
      (dolist (entry pending)
        (when-let* ((residual
                     (nskk--server-cleanup-owned-resources
                      (car entry) (cdr entry))))
          (nskk--server-register-pending-cleanup
           (car residual) (cdr residual))))
      (null nskk--server-pending-cleanups)))

  (defun nskk--server-cleanup-connection-attempt (proc buffer owned-buffer)
    "Release resources from one failed connection attempt.
Kill BUFFER only when OWNED-BUFFER is non-nil, and retain live remnants."
    (when-let* ((residual
                 (nskk--server-cleanup-owned-resources
                  proc (and owned-buffer buffer))))
      (nskk--server-register-pending-cleanup
       (car residual) (cdr residual)))
    (null nskk--server-pending-cleanups))

  (defun nskk--server-prolog-state-snapshot ()
  "Snapshot only the Prolog storage entries for server-state/1."
  (let* ((key (nskk-prolog-clause-key 'server-state 1))
         (missing (make-symbol "missing")))
    (vector missing
            key
            (gethash key (nskk-prolog-database) missing)
            (gethash key (nskk-prolog-database-tails) missing)
            (gethash key (nskk-prolog-index-config) missing)
            (gethash key (nskk-prolog-hash-indices) missing)
            (gethash key (nskk-prolog-trie-indices) missing)
            (gethash key (nskk-prolog-index-bucket-tail-cache) missing))))

  (defun nskk--server-restore-prolog-state (snapshot)
  "Restore the server-state/1 storage entries from SNAPSHOT."
  (let ((missing (aref snapshot 0))
        (key (aref snapshot 1))
        (inhibit-quit t))
    (dolist (entry
             (list
              (cons (nskk-prolog-database) (aref snapshot 2))
              (cons (nskk-prolog-database-tails) (aref snapshot 3))
              (cons (nskk-prolog-index-config) (aref snapshot 4))
              (cons (nskk-prolog-hash-indices) (aref snapshot 5))
              (cons (nskk-prolog-trie-indices) (aref snapshot 6))
              (cons (nskk-prolog-index-bucket-tail-cache) (aref snapshot 7))))
      (if (eq (cdr entry) missing)
          (remhash key (car entry))
        (puthash key (cdr entry) (car entry))))))

  (defun nskk--server-make-connection ()
    "Open a raw TCP connection to the configured skkserv instance.
Returns the process object on success, or nil if the connection fails.

Connect asynchronously and bound every poll by a finite remaining wait
budget.  Every poll consumes the exact slice passed to
`accept-process-output', so wall-clock adjustments cannot extend the
connection attempt.  PROC remains the wait return condition while normal
process event dispatch stays enabled, allowing the asynchronous connection
sentinel to transition the process out of `connect'.  A process and buffer
created by this attempt are released if any initialization step signals or
the connection does not open."
    (let ((proc nil)
          (buffer nil)
          (owned-buffer nil)
          (completed nil))
      (unwind-protect
          (condition-case err
              (let* ((coding-system-for-read nskk-server-coding-system)
                     (coding-system-for-write nskk-server-coding-system)
                     (remaining-wait nil))
                (setq owned-buffer
                      (not (buffer-live-p
                            (get-buffer nskk--server-buffer-name))))
                (setq buffer (get-buffer-create nskk--server-buffer-name))
                (setq proc (open-network-stream
                            "nskk-server"
                            buffer
                            nskk-server-host
                            nskk-server-portnum
                            :type
                            'plain
                            :nowait
                            t))
                (when (processp proc)
                  (process-put proc 'nskk-server-owned-buffer
                               (and owned-buffer buffer))
                  (set-process-filter proc #'nskk--server-process-filter)
                  (process-put proc 'nskk-response-bytes 0)
                  (process-put proc 'nskk-response-overflow nil)
                  (process-put proc 'nskk-response-complete nil))
                (setq remaining-wait
                      (cond
                       ((not (numberp nskk-server-timeout))
                        (signal 'wrong-type-argument
                                (list 'numberp nskk-server-timeout)))
                       ((and (> nskk-server-timeout 0)
                             (= (- nskk-server-timeout
                                   nskk-server-timeout)
                                0))
                        nskk-server-timeout)
                       (t 0)))
                (while (and (eq (process-status proc) 'connect)
                            (> remaining-wait 0))
                  (let ((slice (min 0.1 remaining-wait)))
                    (setq remaining-wait
                          (max 0 (- remaining-wait slice)))
                    (accept-process-output proc slice nil nil)))
                (if (eq (process-status proc) 'open)
                    (prog1 proc
                      (setq completed t))
                  (nskk-debug-message
                   "nskk-server-open: connection to %s:%d not established (status=%s)"
                   nskk-server-host
                   nskk-server-portnum
                   (process-status proc))
                  nil))
            (error
             (nskk-debug-message
              "nskk-server-open: connection failed: %s"
              (error-message-string err))
             nil))
        (unless completed
          (nskk--server-cleanup-connection-attempt
           proc buffer owned-buffer)))))

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
  "Open or reuse a TCP connection to the configured skkserv instance.
When the current connection is live, succeeds with that process without
reconnecting.  Otherwise connects to `nskk-server-host':`nskk-server-portnum',
configures the new process, updates Prolog state, and succeeds with it.
Returns nil (via sync wrapper) or fails if disabled or connection fails."
  (if nskk-server-enable
      (if (not (nskk--server-drain-pending-cleanups))
          (fail)
        (if (nskk-server-live-p)
            (succeed nskk--server-process)
          (let ((cleanup-required (processp nskk--server-process)))
            (when cleanup-required
              (nskk-server-close))
            (if (or nskk--server-pending-cleanups
                    (and cleanup-required nskk--server-process))
                (fail)
              (let* ((previous-process nskk--server-process)
                     (hook-was-registered
                      nskk--server-kill-emacs-hook-registered)
                     (hook-was-present
                      (memq #'nskk-server-close kill-emacs-hook))
                     (prolog-snapshot (nskk--server-prolog-state-snapshot))
                     (proc (nskk--server-make-connection)))
                (if proc
                    (let* ((buffer (if (processp proc)
                                       (process-buffer proc)
                                     (get-buffer nskk--server-buffer-name)))
                           (owned-process (not (eq proc previous-process)))
                           (owned-buffer
                            (let ((owned
                                   (and owned-process
                                        (processp proc)
                                        (process-get
                                         proc 'nskk-server-owned-buffer))))
                              (when owned
                                (process-put
                                 proc 'nskk-server-owned-buffer buffer))
                              owned)))
                      (condition-case condition
                          (nskk--server-configure-process proc)
                        ((error quit)
                         (condition-case nil
                             (if hook-was-present
                                 (add-hook
                                  'kill-emacs-hook #'nskk-server-close)
                               (remove-hook
                                'kill-emacs-hook #'nskk-server-close))
                           ((error quit) nil))
                         (setq nskk--server-kill-emacs-hook-registered
                               hook-was-registered)
                         (setq nskk--server-process previous-process)
                         (when owned-process
                           (nskk--server-cleanup-connection-attempt
                            proc buffer owned-buffer))
                         (nskk--server-restore-prolog-state prolog-snapshot)
                         (signal (car condition) (cdr condition))))
                      (succeed proc))
                  (fail)))))))
    (fail)))

(defun/done nskk-server-close ()
  "Send disconnect command and drain all resources owned by the server.
The active handle and every pending cleanup entry are reclaimed together.
Cleanup faults are swallowed, but live remnants remain registered so a later
close or open call can retry without creating another connection."
  (let* ((proc nskk--server-process)
         (pending-only (and (null proc) nskk--server-pending-cleanups))
         (buffer
          (and (processp proc)
               (let ((owned
                      (process-get proc 'nskk-server-owned-buffer)))
                 (if (bufferp owned)
                     owned
                   (process-buffer proc)))))
         (owned-buffer
          (and (processp proc)
               (process-get proc 'nskk-server-owned-buffer)))
         (active-cleanup-pending
          (and proc (nskk-prolog-holds-p '(server-state closed))))
         (live
          (and (not active-cleanup-pending)
               proc
               (nskk-server-live-p)))
         (prolog-snapshot
          (and (not active-cleanup-pending)
               (not pending-only)
               (nskk--server-prolog-state-snapshot))))
    (unless (or active-cleanup-pending pending-only)
      (condition-case condition
          (progn
            (nskk-prolog-retract-all 'server-state 1)
            (nskk-prolog-assert '((server-state closed))))
        ((error quit)
         (nskk--server-restore-prolog-state prolog-snapshot)
         (signal (car condition) (cdr condition)))))
    (let ((inhibit-quit t))
      (condition-case nil
          (when live
            (process-send-string proc "0"))
        ((error quit) nil))
      (condition-case nil
          (when (processp proc)
            (set-process-query-on-exit-flag proc nil))
        ((error quit) nil))
      (when proc
        (nskk--server-register-pending-cleanup
         proc (and owned-buffer buffer))
        (setq nskk--server-process nil))
      (nskk--server-drain-pending-cleanups)))
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

(defun nskk--server-key-safe-for-coding-p (key coding-system)
  "Return non-nil when KEY is a lossless skkserv token in CODING-SYSTEM."
  (and (stringp key)
       (not (string-empty-p key))
       coding-system
       (coding-system-p coding-system)
       (not
        (cl-some
         (lambda (char)
           (or (<= char #x20)
               (<= #x7f char #x9f)
               (memq (get-char-code-property char 'general-category)
                     '(Zs Zl Zp))))
         key))
       (condition-case nil
           (let ((encoded (encode-coding-string key coding-system)))
             (and (string= key
                           (decode-coding-string encoded coding-system))
                  (not
                   (cl-some
                    (lambda (byte)
                      (or (<= byte #x20)
                          (= byte #x7f)))
                    encoded))))
         (error nil))))

(defun nskk--server-process-safe-for-coding-p (process line)
  "Return non-nil when PROCESS can safely encode LINE.

Compare base coding systems so EOL variants remain compatible."
  (condition-case nil
      (and nskk-server-coding-system
           (coding-system-p nskk-server-coding-system)
           (process-live-p process)
           (let ((actual (cdr (process-coding-system process))))
             (and actual
                  (eq (coding-system-base actual)
                      (coding-system-base nskk-server-coding-system))
                  (nskk--server-key-safe-for-coding-p
                   line nskk-server-coding-system))))
    ((error quit) nil)))

(defun/k nskk--server-lookup-guards-p (key)
  "Succeed when KEY is safe to send to skkserv, fail otherwise.
String conditions checked synchronously: `nskk-server-enable' is non-nil
and KEY is a non-empty token that encodes losslessly without skkserv
delimiter bytes under `nskk-server-coding-system'.
Then chains to `nskk-server-live-p/k' for connection liveness."
  (if (and nskk-server-enable
           (nskk--server-key-safe-for-coding-p
            key nskk-server-coding-system))
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
`nskk--server-strip-annotation'.  Candidates containing C0, DEL, or C1
control characters are rejected.

Succeeds with the candidate list when at least one candidate is found.
Fails for not-found responses, empty responses, or non-string inputs."
  (if (and (stringp response)
           (not (string-empty-p response))
           (nskk-prolog-holds-p
            `(server-response-type ,(substring response 0 1) found)))
      (let* ((body (string-trim-right (substring response 1) "[\r\n]+"))
             (parts (split-string body "/" t))
             (candidates
              (delq nil
                    (mapcar
                     (lambda (candidate)
                       (unless (cl-some (lambda (char) (or (<= char #x1f) (<= #x7f char #x9f))) candidate)
                         (let* ((trimmed (string-trim candidate))
                                (stripped
                                 (nskk--server-strip-annotation trimmed)))
                           (unless (string-empty-p stripped)
                             stripped))))
                     parts))))
        (if candidates (succeed candidates) (fail)))
    (fail)))

(defun/k nskk--server-await-response (proc buf wait-budget)
  "Poll PROC via BUF for one response line within WAIT-BUDGET seconds.
Polls in slices of at most 0.1 seconds using `accept-process-output'.
Each poll consumes the exact slice from a finite remaining wait budget, so
wall-clock adjustments cannot extend the wait.  Non-positive and non-finite
numeric budgets are treated as zero; non-numeric budgets signal before
polling.

The response is rejected before newline detection when the process filter or
accumulated buffer exceeds `nskk--server-max-response-size'.  On success,
only the first protocol line, including its terminating newline, is returned.
Timeouts, disconnects, incomplete responses, and oversized responses reset
the connection via `nskk-server-close'.

Succeeds with the first complete response line.
Fails after resetting the connection on any incomplete or invalid reply."
  (let ((line-end nil)
        (overflow (and (processp proc)
                       (process-get proc 'nskk-response-overflow)))
        (remaining-wait
         (cond
          ((not (numberp wait-budget))
           (signal 'wrong-type-argument (list 'numberp wait-budget)))
          ((and (> wait-budget 0)
                (= (- wait-budget wait-budget) 0))
           wait-budget)
          (t 0))))
    (while (and (not line-end)
                (not overflow)
                (nskk-server-live-p)
                (> remaining-wait 0))
      (let ((slice (min 0.1 remaining-wait)))
        (setq remaining-wait
              (max 0 (- remaining-wait slice)))
        (accept-process-output proc slice nil t))
      (setq overflow
            (and (processp proc)
                 (process-get proc 'nskk-response-overflow)))
      (with-current-buffer buf
        (goto-char (point-min))
        (cond
         ((or overflow
              (> (- (position-bytes (point-max))
                    (position-bytes (point-min)))
                 nskk--server-max-response-size))
          (setq overflow t))
         ((search-forward "\n" nil t)
          (setq line-end (point))))))
    (cond
     (line-end
      (succeed
       (with-current-buffer buf
         (buffer-substring-no-properties (point-min) line-end))))
     (t
      (when overflow
        (nskk-debug-message
         "nskk-server-lookup: response exceeded %d bytes; resetting connection"
         nskk--server-max-response-size))
      (nskk-server-close)
      (fail)))))

(defun/k nskk--server-with-response (key)
  "Send skkserv command 1 for KEY, await the response.
Before any I/O or process-state mutation, verifies that the explicit
configured coding system and actual process output coding have the same base,
and that the actual output coding can encode KEY losslessly without skkserv
delimiter bytes.

Erases the I/O buffer, resets its byte-limit state, sends the command-1
request, then delegates to `nskk--server-await-response/k' with the finite
timeout budget.

When `nskk-server-report-response' is non-nil, logs elapsed time.

After a send attempt, any non-local exit before response completion discards
the connection so a partial response cannot desynchronize the next request.
Cleanup failures never replace the original condition.

Succeeds with the raw response string.
Fails on validation error, network error, or timeout."
  (let ((proc nskk--server-process))
    (if (not (nskk--server-process-safe-for-coding-p proc key))
        (fail)
      (let ((request-started nil)
            (response-complete nil))
        (let ((resp
               (condition-case err
                   (unwind-protect
                       (let* ((buf (process-buffer proc))
                              (start-time
                               (and nskk-server-report-response
                                    (float-time))))
                         (with-current-buffer buf (erase-buffer))
                         (when (processp proc)
                           (process-put proc 'nskk-response-bytes 0)
                           (process-put proc 'nskk-response-overflow nil)
                           (process-put proc 'nskk-response-complete nil))
                         (setq request-started t)
                         (process-send-string proc (concat "1" key " "))
                         (let ((r (nskk--server-await-response
                                   proc buf nskk-server-timeout)))
                           (setq response-complete t)
                           (when nskk-server-report-response
                             (nskk-debug-message
                              "nskk-server-lookup: key=%s elapsed=%.3fms"
                              key (* 1000 (- (float-time) start-time))))
                           r))
                     (when (and request-started (not response-complete))
                       (condition-case nil
                           (nskk-server-close)
                         ((error quit) nil))))
                 (error
                  (nskk-debug-message
                   "nskk-server-lookup: error for key=%s: %s"
                   key (error-message-string err))
                  nil))))
          (if resp (succeed resp) (fail)))))))

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
