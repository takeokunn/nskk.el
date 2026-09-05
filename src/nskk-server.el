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

(require 'cl-lib)
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

;;;; Wait Budget

(defun nskk--server-wait-budget (seconds)
  "Return SECONDS as a finite, non-negative polling budget.
Signal `wrong-type-argument' when SECONDS is not a number.  Infinities and
NaN yield 0, so a malformed timeout cannot produce an unbounded poll loop."
  (cond
   ((not (numberp seconds))
    (signal 'wrong-type-argument (list 'numberp seconds)))
   ((and (> seconds 0) (= (- seconds seconds) 0)) seconds)
   (t 0)))

;;;; Prolog State Rollback

(defun nskk--server-capture-prolog-state ()
  "Capture rollback state for the server-state/1 fact."
  (nskk-prolog-capture-key-state (nskk-prolog-clause-key 'server-state 1)))

(defun nskk--server-publish-closed-state ()
  "Replace the server-state/1 fact with the closed state."
  (nskk-prolog-retract-all 'server-state 1)
  (nskk-prolog-assert '((server-state closed))))

;;;; Core Functions

(defun nskk-server-live-p ()
  "Return non-nil when the skkserv connection is active.
Both the Emacs process status and the Prolog server-state/1 fact must
agree, so a half-torn-down connection reads as dead."
  (and (processp nskk--server-process)
       (eq (process-status nskk--server-process) 'open)
       (nskk-prolog-holds-p '(server-state open))
       t))

(defun nskk--server-reset-response-state (proc)
  "Clear PROC's accumulated response accounting."
  (process-put proc 'nskk-response-bytes 0)
  (process-put proc 'nskk-response-overflow nil)
  (process-put proc 'nskk-response-complete nil))

(defun nskk--server-response-overflow-p (proc)
  "Return non-nil when PROC's filter rejected the response as oversized."
  (and (processp proc)
       (process-get proc 'nskk-response-overflow)))

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

;;;; Resource Ownership

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

(defun nskk--server-ensure-kill-emacs-hook ()
  "Register `nskk-server-close' on `kill-emacs-hook' at most once."
  (unless (memq #'nskk-server-close kill-emacs-hook)
    (add-hook 'kill-emacs-hook #'nskk-server-close))
  (setq nskk--server-kill-emacs-hook-registered t))

(defun nskk--server-restore-hook-registration (was-present was-registered)
  "Restore `kill-emacs-hook' membership to WAS-PRESENT.
WAS-REGISTERED is restored into `nskk--server-kill-emacs-hook-registered'."
  (condition-case nil
      (if was-present
          (add-hook 'kill-emacs-hook #'nskk-server-close)
        (remove-hook 'kill-emacs-hook #'nskk-server-close))
    ((error quit) nil))
  (setq nskk--server-kill-emacs-hook-registered was-registered))

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
        (nskk--server-ensure-kill-emacs-hook)
      ((error quit) nil))))

(defun nskk--server-cleanup-owned-resources (proc buffer)
  "Best-effort release of owned PROC and BUFFER.
Return a cons of the resources that still require cleanup, or nil when
both were released.  A fault releasing one resource does not strand the
other, and the caller re-registers whatever is returned."
  (let ((inhibit-quit t))
    (condition-case nil
        (when (and proc
                   (or (not (processp proc))
                       (process-live-p proc)))
          (delete-process proc))
      ((error quit) nil))
    (condition-case nil
        (when (and (bufferp buffer) (buffer-live-p buffer))
          (with-current-buffer buffer
            (let ((kill-buffer-hook nil)
                  (kill-buffer-query-functions nil)
                  (buffer-offer-save nil))
              (kill-buffer buffer))))
      ((error quit) nil))
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

;;;; Connection

(defun nskk--server-attach-process (proc buffer owned-buffer)
  "Attach NSKK's filter and response accounting to PROC.
BUFFER is PROC's working buffer, owned by this attempt when OWNED-BUFFER
is non-nil."
  (process-put proc 'nskk-server-owned-buffer (and owned-buffer buffer))
  (set-process-filter proc #'nskk--server-process-filter)
  (nskk--server-reset-response-state proc))

(defun nskk--server-await-connect (proc budget)
  "Poll PROC until it leaves `connect' status or BUDGET seconds elapse.
Each poll consumes its exact slice of BUDGET, so wall-clock adjustments
cannot extend the attempt.  PROC stays the wait condition while normal
event dispatch runs, letting the async connection sentinel fire."
  (let ((remaining budget))
    (while (and (eq (process-status proc) 'connect)
                (> remaining 0))
      (let ((slice (min 0.1 remaining)))
        (setq remaining (max 0 (- remaining slice)))
        (accept-process-output proc slice nil nil)))))

(defun nskk--server-make-connection ()
  "Open a TCP connection to the configured skkserv, or return nil.
Connect asynchronously under a finite wait budget.  A process or buffer
created by an attempt that signals, or that never reaches `open', is
released before returning."
  (let ((proc nil)
        (buffer nil)
        (owned-buffer nil)
        (completed nil))
    (unwind-protect
        (condition-case err
            (let ((coding-system-for-read nskk-server-coding-system)
                  (coding-system-for-write nskk-server-coding-system))
              (setq owned-buffer
                    (not (buffer-live-p
                          (get-buffer nskk--server-buffer-name))))
              (setq buffer (get-buffer-create nskk--server-buffer-name))
              (setq proc (open-network-stream
                          "nskk-server"
                          buffer
                          nskk-server-host
                          nskk-server-portnum
                          :type 'plain
                          :nowait t))
              (when (processp proc)
                (nskk--server-attach-process proc buffer owned-buffer))
              (nskk--server-await-connect
               proc (nskk--server-wait-budget nskk-server-timeout))
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
Records PROC, registers the kill-emacs hook, and publishes the open state."
  (set-process-query-on-exit-flag proc nil)
  (setq nskk--server-process proc)
  (unless nskk--server-kill-emacs-hook-registered
    (add-hook 'kill-emacs-hook #'nskk-server-close)
    (setq nskk--server-kill-emacs-hook-registered t))
  (nskk-prolog-retract-all 'server-state 1)
  (nskk-prolog-assert '((server-state open)))
  (nskk-debug-message "nskk-server-open: connected to %s:%d"
                      nskk-server-host nskk-server-portnum))

(defun nskk--server-claim-buffer-ownership (proc previous-process)
  "Return non-nil when PROC's working buffer is owned by this attempt.
PROC is new when it differs from PREVIOUS-PROCESS.  The ownership property
is rewritten to hold the buffer object so `nskk-server-close' can reclaim it."
  (let ((owned (and (not (eq proc previous-process))
                    (processp proc)
                    (process-get proc 'nskk-server-owned-buffer))))
    (when owned
      (process-put proc 'nskk-server-owned-buffer (process-buffer proc)))
    owned))

(defun/k nskk-server-open ()
  "Open or reuse a TCP connection to the configured skkserv.
Succeed with the live process, reusing the current one when it is still
open.  Fail when the server is disabled, when resources from an earlier
failed attempt are still pending, or when the connection cannot be made.
A fault while configuring a new process rolls back the hook registration,
the process variable, the connection, and the Prolog state."
  (if (not (and nskk-server-enable
                (nskk--server-drain-pending-cleanups)))
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
                 (hook-was-registered nskk--server-kill-emacs-hook-registered)
                 (hook-was-present (memq #'nskk-server-close kill-emacs-hook))
                 (prolog-state (nskk--server-capture-prolog-state))
                 (proc (nskk--server-make-connection)))
            (if (not proc)
                (fail)
              (let* ((buffer (if (processp proc)
                                 (process-buffer proc)
                               (get-buffer nskk--server-buffer-name)))
                     (owned-process (not (eq proc previous-process)))
                     (owned-buffer
                      (nskk--server-claim-buffer-ownership
                       proc previous-process)))
                (condition-case condition
                    (nskk--server-configure-process proc)
                  ((error quit)
                   (nskk--server-restore-hook-registration
                    hook-was-present hook-was-registered)
                   (setq nskk--server-process previous-process)
                   (when owned-process
                     (nskk--server-cleanup-connection-attempt
                      proc buffer owned-buffer))
                   (nskk-prolog-restore-key-state prolog-state)
                   (signal (car condition) (cdr condition))))
                (succeed proc)))))))))

(defun/done nskk-server-close ()
  "Send the disconnect command and reclaim every resource the server owns.
The active handle and all pending cleanup entries drain together.  Cleanup
faults are swallowed, but live remnants stay registered so a later close or
open can retry without creating a second connection."
  (let* ((proc nskk--server-process)
         (pending-only (and (null proc) nskk--server-pending-cleanups))
         (owned-buffer (and (processp proc)
                            (process-get proc 'nskk-server-owned-buffer)))
         (buffer (and (processp proc)
                      (if (bufferp owned-buffer)
                          owned-buffer
                        (process-buffer proc))))
         (active-cleanup-pending
          (and proc (nskk-prolog-holds-p '(server-state closed))))
         (live (and (not active-cleanup-pending)
                    proc
                    (nskk-server-live-p)))
         (prolog-state (and (not active-cleanup-pending)
                            (not pending-only)
                            (nskk--server-capture-prolog-state))))
    (unless (or active-cleanup-pending pending-only)
      (condition-case condition
          (nskk--server-publish-closed-state)
        ((error quit)
         (nskk-prolog-restore-key-state prolog-state)
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
  "Ensure the skkserv connection is live, reconnecting when needed.
Succeed with t when the connection is live after this call.  Fail when
`nskk-server-enable' is nil or the connection cannot be established."
  (if (not nskk-server-enable)
      (fail)
    (if (nskk-server-live-p)
        (succeed t)
      (nskk-debug-message "nskk-server-ensure-open: reconnecting...")
      (<- _open-proc nskk-server-open)
      (succeed t))))

;;;; Protocol Implementation

(defun nskk--server-control-char-p (string)
  "Return non-nil when STRING holds a C0, DEL, or C1 control character."
  (cl-some (lambda (char)
             (or (<= char #x1f)
                 (<= #x7f char #x9f)))
           string))

(defun nskk--server-key-safe-for-coding-p (key coding-system)
  "Return non-nil when KEY is a lossless skkserv token in CODING-SYSTEM.
KEY must be non-empty, free of control and Unicode separator characters,
and must survive an encode/decode round trip without producing bytes that
skkserv reads as protocol delimiters."
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
The coding-system checks run before the liveness check, so an unsendable
key is rejected without observing or mutating process state."
  (if (and nskk-server-enable
           (nskk--server-key-safe-for-coding-p
            key nskk-server-coding-system)
           (nskk-server-live-p))
      (succeed t)
    (fail)))

(defun nskk--server-strip-annotation (candidate)
  "Return CANDIDATE with any SKK annotation removed.
An annotation follows a semicolon: \"word;note\" yields \"word\"."
  (let ((semi (string-search ";" candidate)))
    (if semi (substring candidate 0 semi) candidate)))

(defun nskk--server-parse-candidates (body)
  "Return the candidate list parsed from skkserv response BODY.
Candidates holding control characters are dropped, annotations stripped,
and surrounding whitespace trimmed."
  (delq nil
        (mapcar
         (lambda (candidate)
           (unless (nskk--server-control-char-p candidate)
             (let ((stripped (nskk--server-strip-annotation
                              (string-trim candidate))))
               (unless (string-empty-p stripped)
                 stripped))))
         (split-string body "/" t))))

(defun/k nskk--server-parse-response (response)
  "Parse a skkserv command-1 RESPONSE string into a candidate list.
Dispatch on the response prefix through the Prolog server-response-type/2
predicate; only the found prefix yields candidates.  Succeed with a
non-empty candidate list; fail for not-found, empty, or non-string input."
  (if (and (stringp response)
           (not (string-empty-p response))
           (nskk-prolog-holds-p
            `(server-response-type ,(substring response 0 1) found)))
      (let ((candidates
             (nskk--server-parse-candidates
              (string-trim-right (substring response 1) "[\r\n]+"))))
        (if candidates (succeed candidates) (fail)))
    (fail)))

(defun nskk--server-buffer-over-cap-p ()
  "Return non-nil when the current buffer exceeds the response size cap."
  (> (- (position-bytes (point-max))
        (position-bytes (point-min)))
     nskk--server-max-response-size))

(defun/k nskk--server-await-response (proc buf wait-budget)
  "Poll PROC via BUF for one response line within WAIT-BUDGET seconds.
Poll in slices of at most 0.1 seconds drawn from a finite budget, so
wall-clock adjustments cannot extend the wait; a non-numeric budget
signals before any polling.  Succeed with the first complete line,
including its newline.  On timeout, disconnect, or a reply exceeding
`nskk--server-max-response-size', reset the connection and fail."
  (let ((line-end nil)
        (overflow (nskk--server-response-overflow-p proc))
        (remaining (nskk--server-wait-budget wait-budget)))
    (while (and (not line-end)
                (not overflow)
                (nskk-server-live-p)
                (> remaining 0))
      (let ((slice (min 0.1 remaining)))
        (setq remaining (max 0 (- remaining slice)))
        (accept-process-output proc slice nil t))
      (setq overflow (nskk--server-response-overflow-p proc))
      (with-current-buffer buf
        (goto-char (point-min))
        (cond
         ((or overflow (nskk--server-buffer-over-cap-p))
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

(defun nskk--server-send-request (proc buf key)
  "Erase BUF, reset PROC's response state, and send command 1 for KEY."
  (with-current-buffer buf (erase-buffer))
  (when (processp proc)
    (nskk--server-reset-response-state proc))
  (process-send-string proc (concat "1" key " ")))

(defun/k nskk--server-with-response (key)
  "Send skkserv command 1 for KEY and await the response.
Verify before any I/O that the configured and actual process coding
systems share a base and that KEY encodes losslessly without skkserv
delimiter bytes.  Any non-local exit after the request is sent discards
the connection, so a partial reply cannot desynchronize the next request.
Succeed with the raw response string; fail on validation, network, or
timeout error."
  (let ((proc nskk--server-process))
    (if (not (nskk--server-process-safe-for-coding-p proc key))
        (fail)
      (let* ((request-started nil)
             (response-complete nil)
             (resp
              (condition-case err
                  (unwind-protect
                      (let ((buf (process-buffer proc))
                            (start-time (and nskk-server-report-response
                                             (float-time))))
                        (setq request-started t)
                        (nskk--server-send-request proc buf key)
                        (let ((line (nskk--server-await-response
                                     proc buf nskk-server-timeout)))
                          (setq response-complete t)
                          (when nskk-server-report-response
                            (nskk-debug-message
                             "nskk-server-lookup: key=%s elapsed=%.3fms"
                             key (* 1000 (- (float-time) start-time))))
                          line))
                    (when (and request-started (not response-complete))
                      (condition-case nil
                          (nskk-server-close)
                        ((error quit) nil))))
                (error
                 (nskk-debug-message
                  "nskk-server-lookup: error for key=%s: %s"
                  key (error-message-string err))
                 nil))))
        (if resp (succeed resp) (fail))))))

(defun/k nskk-server-lookup (key)
  "Look up KEY in the skkserv dictionary using command 1.
Returns a list of candidate strings on success (e.g., (\"漢字\" \"感じ\")),
or nil when the key is not found, the server is unreachable, or any
network error occurs.  Never signals an error to the caller.

OKURI-ARI keys should be passed in their standard SKK format (e.g.,
\"かんじk\" for okurigana words); the server handles them natively."
  (<- _guard nskk--server-lookup-guards-p key)
  (<- resp nskk--server-with-response key)
  (<- result nskk--server-parse-response resp)
  (succeed result))

(provide 'nskk-server)

;;; nskk-server.el ends here
