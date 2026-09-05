;;; nskk-server-test.el --- Tests for nskk-server.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for nskk-server.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-server)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Fixtures

(defun nskk-server-test--set-state (state)
  "Replace the server-state/1 fact with STATE."
  (nskk-prolog-retract-all 'server-state 1)
  (nskk-prolog-assert (list (list 'server-state state))))

(defmacro nskk-server-test--with-state (state &rest body)
  "Run BODY with the server-state/1 fact set to STATE, then reset it."
  (declare (indent 1))
  `(unwind-protect
       (progn
         (nskk-server-test--set-state ,state)
         ,@body)
     (nskk-server-test--set-state 'closed)))

(defmacro nskk-server-test--with-pipe (name-var proc-var buf-var &rest body)
  "Run BODY with a live pipe process in PROC-VAR and its buffer in BUF-VAR.
NAME-VAR is a string used to name both.  Both are destroyed afterwards, and
`nskk--server-process' plus the pending-cleanup list are always reset."
  (declare (indent 3))
  `(let* ((,buf-var (generate-new-buffer (format " *%s*" ,name-var)))
          (,proc-var (make-pipe-process :name ,name-var
                                        :buffer ,buf-var
                                        :noquery t)))
     (unwind-protect
         (progn ,@body)
       (setq nskk--server-process nil)
       (setq nskk--server-pending-cleanups nil)
       (nskk-server-test--set-state 'closed)
       (when (process-live-p ,proc-var)
         (delete-process ,proc-var))
       (when (buffer-live-p ,buf-var)
         (kill-buffer ,buf-var)))))

;;;; A. Wait budget

(nskk-deftest-table server-wait-budget-finite
  :columns (input expected)
  :rows ((1 1)
         (0.25 0.25)
         (0 0)
         (-1 0)
         (-0.5 0))
  :body (should (equal (nskk--server-wait-budget input) expected)))

(nskk-describe "nskk--server-wait-budget"
  (nskk-it "treats infinity and NaN as an exhausted budget"
    (should (= (nskk--server-wait-budget 1.0e+INF) 0))
    (should (= (nskk--server-wait-budget -1.0e+INF) 0))
    (should (= (nskk--server-wait-budget (- 1.0e+INF 1.0e+INF)) 0)))

  (nskk-it "signals wrong-type-argument for a non-number"
    (should-error (nskk--server-wait-budget "1") :type 'wrong-type-argument)
    (should-error (nskk--server-wait-budget nil) :type 'wrong-type-argument)))

(nskk-describe "response wait budget"
  (nskk-it "never polls longer than the remaining budget and stays clock-free"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (proc 'mock-proc)
            (clock-calls 0)
            (waits nil))
        (nskk-with-mocks
            ((float-time (lambda (&optional _) (cl-incf clock-calls) 100.0))
             (nskk-server-live-p (lambda () t))
             (nskk-server-close (lambda () nil))
             (accept-process-output
              (lambda (wait-proc timeout &optional _ms just-this-one)
                (should (eq wait-proc proc))
                (should just-this-one)
                (push timeout waits))))
          (should-not (nskk--server-await-response proc buf 0.25))
          (should (= clock-calls 0))
          (should (cl-every (lambda (w) (and (> w 0) (<= w 0.1))) waits))
          (should (< (abs (- (apply #'+ waits) 0.25)) 0.000001))))))

  (nskk-it "signals before polling when the budget is not a number"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (polled nil))
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () t))
             (accept-process-output (lambda (&rest _) (setq polled t))))
          (should-error (nskk--server-await-response 'mock-proc buf "x")
                        :type 'wrong-type-argument)
          (should-not polled)))))

  (nskk-it "does not poll for a non-finite budget and resets the connection"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (polled nil)
            (closed nil))
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () t))
             (nskk-server-close (lambda () (setq closed t)))
             (accept-process-output (lambda (&rest _) (setq polled t))))
          (should-not (nskk--server-await-response 'mock-proc buf 1.0e+INF))
          (should-not polled)
          (should closed))))))

;;;; B. Byte-cap accounting

(nskk-describe "bounded process filter"
  (nskk-it "retains only the first line and marks the response complete"
    (nskk-server-test--with-pipe "nskk-filter-line" proc buf
      (nskk--server-reset-response-state proc)
      (nskk--server-process-filter proc "1/A/B/\ntrailing")
      (should (process-get proc 'nskk-response-complete))
      (should (equal (with-current-buffer buf (buffer-string)) "1/A/B/\n"))))

  (nskk-it "counts decoded chunks across a character boundary"
    (nskk-server-test--with-pipe "nskk-filter-multibyte" proc buf
      (nskk--server-reset-response-state proc)
      (nskk--server-process-filter proc "漢")
      (nskk--server-process-filter proc "字")
      (should (= (process-get proc 'nskk-response-bytes)
                 (+ (string-bytes "漢") (string-bytes "字"))))
      (should (equal (with-current-buffer buf (buffer-string)) "漢字"))))

  (nskk-it "rejects an over-cap chunk instead of retaining it"
    (nskk-server-test--with-pipe "nskk-filter-overflow" proc buf
      (nskk--server-reset-response-state proc)
      (nskk--server-process-filter
       proc (make-string (1+ nskk--server-max-response-size) ?a))
      (should (process-get proc 'nskk-response-overflow))
      (should (equal (with-current-buffer buf (buffer-string)) ""))))

  (nskk-it "accepts a response exactly at the byte cap"
    (nskk-server-test--with-pipe "nskk-filter-at-cap" proc buf
      (nskk--server-reset-response-state proc)
      (nskk--server-process-filter
       proc (make-string nskk--server-max-response-size ?a))
      (should-not (process-get proc 'nskk-response-overflow))
      (should (= (process-get proc 'nskk-response-bytes)
                 nskk--server-max-response-size)))))

(nskk-describe "over-cap response handling"
  (nskk-it "rejects and resets an over-cap reply even when it holds a newline"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (closed nil))
        (insert (make-string (1+ nskk--server-max-response-size) ?a) "\n")
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () t))
             (nskk-server-close (lambda () (setq closed t)))
             (accept-process-output (lambda (&rest _) nil)))
          (should-not (nskk--server-await-response 'mock-proc buf 0.2))
          (should closed)))))

  (nskk-it "returns only the first line when a chunk holds several"
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (insert "1/A/\n1/B/\n")
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () t))
             (nskk-server-close (lambda () nil))
             (accept-process-output (lambda (&rest _) nil)))
          (should (equal (nskk--server-await-response 'mock-proc buf 0.2)
                         "1/A/\n"))))))

  (nskk-it "resets the connection when the server disconnects"
    (with-temp-buffer
      (let ((buf (current-buffer))
            (closed nil))
        (nskk-with-mocks
            ((nskk-server-live-p (lambda () nil))
             (nskk-server-close (lambda () (setq closed t)))
             (accept-process-output (lambda (&rest _) nil)))
          (should-not (nskk--server-await-response 'mock-proc buf 10))
          (should closed))))))

;;;; C. Fail-closed ordering before I/O

(nskk-describe "lookup guards"
  (nskk-it "rejects a nil configured coding before liveness or mutation"
    (let ((nskk-server-enable t)
          (nskk-server-coding-system nil)
          (liveness-checked nil))
      (nskk-with-mocks
          ((nskk-server-live-p
            (lambda () (setq liveness-checked t) t)))
        (should-not (nskk--server-lookup-guards-p "かんじ"))
        (should-not liveness-checked))))

  (nskk-it "rejects an invalid configured coding before liveness"
    (let ((nskk-server-enable t)
          (nskk-server-coding-system 'no-such-coding-system)
          (liveness-checked nil))
      (nskk-with-mocks
          ((nskk-server-live-p
            (lambda () (setq liveness-checked t) t)))
        (should-not (nskk--server-lookup-guards-p "かんじ"))
        (should-not liveness-checked))))

  (nskk-it "rejects a lossy key before liveness"
    (let ((nskk-server-enable t)
          (nskk-server-coding-system 'euc-jp)
          (liveness-checked nil))
      (nskk-with-mocks
          ((nskk-server-live-p
            (lambda () (setq liveness-checked t) t)))
        (should-not (nskk--server-lookup-guards-p "😀"))
        (should-not liveness-checked))))

  (nskk-it "returns nil when the server is disabled even with a live process"
    (let ((nskk-server-enable nil)
          (nskk-server-coding-system 'euc-jp))
      (nskk-with-mocks ((nskk-server-live-p (lambda () t)))
        (should-not (nskk--server-lookup-guards-p "かんじ")))))

  (nskk-it "returns nil when the connection is not live"
    (let ((nskk-server-enable t)
          (nskk-server-coding-system 'euc-jp))
      (nskk-with-mocks ((nskk-server-live-p (lambda () nil)))
        (should-not (nskk--server-lookup-guards-p "かんじ")))))

  (nskk-it "accepts a valid key over a live connection"
    (let ((nskk-server-enable t)
          (nskk-server-coding-system 'euc-jp))
      (nskk-with-mocks ((nskk-server-live-p (lambda () t)))
        (should (nskk--server-lookup-guards-p "かんじ"))))))

(nskk-describe "process coding preflight"
  (nskk-it "accepts an EOL variant sharing the configured base"
    (nskk-server-test--with-pipe "nskk-coding-eol" proc _buf
      (set-process-coding-system proc 'euc-jp-unix 'euc-jp-unix)
      (let ((nskk-server-coding-system 'euc-jp))
        (should (nskk--server-process-safe-for-coding-p proc "かんじ")))))

  (nskk-it "rejects a process whose coding base differs"
    (nskk-server-test--with-pipe "nskk-coding-mismatch" proc _buf
      (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
      (let ((nskk-server-coding-system 'euc-jp))
        (should-not (nskk--server-process-safe-for-coding-p proc "かんじ")))))

  (nskk-it "rejects a dead process without signalling"
    (nskk-server-test--with-pipe "nskk-coding-dead" proc _buf
      (set-process-coding-system proc 'euc-jp-unix 'euc-jp-unix)
      (delete-process proc)
      (let ((nskk-server-coding-system 'euc-jp))
        (should-not (nskk--server-process-safe-for-coding-p proc "かんじ")))))

  (nskk-it "rejects when the configured coding is nil"
    (nskk-server-test--with-pipe "nskk-coding-nil" proc _buf
      (set-process-coding-system proc 'euc-jp-unix 'euc-jp-unix)
      (let ((nskk-server-coding-system nil))
        (should-not (nskk--server-process-safe-for-coding-p proc "かんじ"))))))

;;;; D. Control characters and protocol injection

(nskk-deftest-table server-key-rejected
  :columns (key)
  :rows (("")
         ("a b")
         ("a\tb")
         ("a\nb")
         ("a\x7fz")
         ("a b")
         ("a　b")
         ("a b"))
  :body (should-not (nskk--server-key-safe-for-coding-p key 'euc-jp)))

(nskk-describe "key coding safety"
  (nskk-it "accepts a plain Japanese token"
    (should (nskk--server-key-safe-for-coding-p "かんじ" 'euc-jp))
    (should (nskk--server-key-safe-for-coding-p "かんじk" 'euc-jp)))

  (nskk-it "rejects a key that does not round-trip in the coding system"
    (should-not (nskk--server-key-safe-for-coding-p "😀" 'euc-jp))
    (should (nskk--server-key-safe-for-coding-p "😀" 'utf-8)))

  (nskk-it "rejects a non-string key and an invalid coding system"
    (should-not (nskk--server-key-safe-for-coding-p nil 'euc-jp))
    (should-not (nskk--server-key-safe-for-coding-p 42 'euc-jp))
    (should-not (nskk--server-key-safe-for-coding-p "かんじ" nil))
    (should-not (nskk--server-key-safe-for-coding-p
                 "かんじ" 'no-such-coding-system))))

(nskk-describe "candidate sanitization"
  ;; Build the control character with `string' rather than a \xNN escape in a
  ;; multibyte literal: there Emacs produces a raw byte (#x3FFF9F) instead of
  ;; the C1 character #x9f, so such a literal would not exercise this filter.
  (nskk-it "drops candidates carrying C0, DEL, or C1 controls"
    (should (equal (nskk--server-parse-response
                    (concat "1/漢字/" (string ?a #x01 ?z) "/感じ/\n"))
                   '("漢字" "感じ")))
    (should (equal (nskk--server-parse-response
                    (concat "1/漢字/" (string ?a #x7f ?z) "/\n"))
                   '("漢字")))
    (should (equal (nskk--server-parse-response
                    (concat "1/漢字/" (string ?a #x9f ?z) "/\n"))
                   '("漢字"))))

  (nskk-it "fails when every candidate is rejected"
    (should-not (nskk--server-parse-response
                 (concat "1/" (string ?a #x01 ?z) "/\n")))))

;;;; E. CPS continuation invariant

(nskk-describe "nskk-server-lookup/k"
  ;; defun/k generates NAME/k (ARG... ON-FOUND ON-NOT-FOUND): plain arguments
  ;; come first and the two continuations last.
  (nskk-it "calls exactly one continuation when the lookup succeeds"
    (let ((found 0) (not-found 0) (value 'unset))
      (nskk-with-mocks
          ((nskk--server-lookup-guards-p/k (lambda (_key f _n) (funcall f t)))
           (nskk--server-with-response/k
            (lambda (_key f _n) (funcall f "1/漢字/感じ/\n"))))
        (nskk-server-lookup/k
         "かんじ"
         (lambda (v) (cl-incf found) (setq value v))
         (lambda () (cl-incf not-found))))
      (should (= found 1))
      (should (= not-found 0))
      (should (equal value '("漢字" "感じ")))))

  (nskk-it "calls exactly one continuation when the guard fails"
    (let ((found 0) (not-found 0))
      (nskk-with-mocks
          ((nskk--server-lookup-guards-p/k (lambda (_key _f n) (funcall n))))
        (nskk-server-lookup/k
         "かんじ"
         (lambda (_v) (cl-incf found))
         (lambda () (cl-incf not-found))))
      (should (= found 0))
      (should (= not-found 1))))

  (nskk-it "takes the not-found branch when the response is a miss"
    (let ((found 0) (not-found 0))
      (nskk-with-mocks
          ((nskk--server-lookup-guards-p/k (lambda (_key f _n) (funcall f t)))
           (nskk--server-with-response/k
            (lambda (_key f _n) (funcall f "4かんじ\n"))))
        (nskk-server-lookup/k
         "かんじ"
         (lambda (_v) (cl-incf found))
         (lambda () (cl-incf not-found))))
      (should (= found 0))
      (should (= not-found 1))))

  (nskk-it "returns nil from the sync wrapper when the server is disabled"
    (let ((nskk-server-enable nil)
          (nskk--server-process nil))
      (should-not (nskk-server-lookup "かんじ")))))

;;;; F. Resource ownership

(nskk-describe "connection liveness"
  (nskk-it "requires both an open process and the open Prolog fact"
    (nskk-server-test--with-pipe "nskk-live-both" proc _buf
      (setq nskk--server-process proc)
      (nskk-server-test--with-state 'open
        (should (nskk-server-live-p)))
      (nskk-server-test--with-state 'closed
        (should-not (nskk-server-live-p)))))

  (nskk-it "returns nil when no process is stored"
    (let ((nskk--server-process nil))
      (nskk-server-test--with-state 'open
        (should-not (nskk-server-live-p)))))

  (nskk-it "returns nil for a dead process even when the fact says open"
    (nskk-server-test--with-pipe "nskk-live-dead" proc _buf
      (setq nskk--server-process proc)
      (delete-process proc)
      (nskk-server-test--with-state 'open
        (should-not (nskk-server-live-p))))))

(nskk-describe "buffer ownership on close"
  (nskk-it "kills a buffer this connection created"
    (nskk-server-test--with-pipe "nskk-owns-buffer" proc buf
      (process-put proc 'nskk-server-owned-buffer buf)
      (setq nskk--server-process proc)
      (nskk-server-test--set-state 'open)
      (nskk-server-close)
      (should-not (buffer-live-p buf))))

  (nskk-it "preserves a pre-existing buffer it does not own"
    (nskk-server-test--with-pipe "nskk-foreign-buffer" proc buf
      (process-put proc 'nskk-server-owned-buffer nil)
      (setq nskk--server-process proc)
      (nskk-server-test--set-state 'open)
      (nskk-server-close)
      (should (buffer-live-p buf)))))

(nskk-describe "nskk-server-close"
  (nskk-it "sends the disconnect command only when the connection is live"
    (nskk-server-test--with-pipe "nskk-close-send" proc _buf
      (let ((sent nil))
        (setq nskk--server-process proc)
        (nskk-server-test--set-state 'open)
        (nskk-with-mocks
            ((process-send-string (lambda (_p s) (setq sent s))))
          (nskk-server-close))
        (should (equal sent "0")))))

  (nskk-it "does not send the disconnect command when not live"
    (nskk-server-test--with-pipe "nskk-close-nosend" proc _buf
      (let ((sent nil))
        (setq nskk--server-process proc)
        (nskk-server-test--set-state 'closed)
        (nskk-with-mocks
            ((process-send-string (lambda (_p s) (setq sent s))))
          (nskk-server-close))
        (should-not sent))))

  (nskk-it "clears the stored process and publishes the closed state"
    (nskk-server-test--with-pipe "nskk-close-clears" proc _buf
      (setq nskk--server-process proc)
      (nskk-server-test--set-state 'open)
      (nskk-server-close)
      (should-not nskk--server-process)
      (should (nskk-prolog-holds-p '(server-state closed)))))

  (nskk-it "is a no-op when already disconnected"
    (let ((nskk--server-process nil)
          (nskk--server-pending-cleanups nil))
      (nskk-server-test--set-state 'closed)
      (nskk-server-close)
      (nskk-server-close)
      (should-not nskk--server-process)
      (should-not nskk--server-pending-cleanups))))

;;;; G. Rollback and pending cleanup

(nskk-describe "close rollback"
  (nskk-it "restores the open state when publishing the closed state fails"
    (nskk-server-test--with-pipe "nskk-close-publish-error" proc _buf
      (setq nskk--server-process proc)
      (nskk-server-test--set-state 'open)
      (should (nskk-prolog-holds-p '(server-state open)))
      (nskk-with-mocks
          ((nskk-prolog-assert (lambda (&rest _) (error "publish failed"))))
        (should-error (nskk-server-close)))
      (should (nskk-prolog-holds-p '(server-state open)))
      (should-not (nskk-prolog-holds-p '(server-state closed)))
      (should (eq nskk--server-process proc))))

  ;; A quit must be caught with an explicit `condition-case': ERT records an
  ;; escaping quit as its own QUIT outcome, counted as neither expected nor
  ;; unexpected, so `should-error' here would leave the test reporting nothing.
  (nskk-it "restores the open state when publishing quits"
    (nskk-server-test--with-pipe "nskk-close-publish-quit" proc _buf
      (setq nskk--server-process proc)
      (nskk-server-test--set-state 'open)
      (let ((caught nil))
        (nskk-with-mocks
            ((nskk-prolog-assert (lambda (&rest _) (signal 'quit nil))))
          (condition-case condition
              (nskk-server-close)
            (quit (setq caught condition))))
        (should (eq (car caught) 'quit)))
      (should (nskk-prolog-holds-p '(server-state open)))))

  (nskk-it "leaves the state queryable after a restored publish failure"
    (nskk-server-test--with-pipe "nskk-close-publish-warm" proc _buf
      (setq nskk--server-process proc)
      (nskk-server-test--set-state 'open)
      (nskk-with-mocks
          ((nskk-prolog-assert (lambda (&rest _) (error "publish failed"))))
        (should-error (nskk-server-close)))
      (nskk-server-test--set-state 'closed)
      (should (nskk-prolog-holds-p '(server-state closed)))
      (nskk-server-test--set-state 'open)
      (should (nskk-prolog-holds-p '(server-state open))))))

(nskk-describe "pending cleanup registry"
  (nskk-it "retains a process whose teardown faults and drains it later"
    (nskk-server-test--with-pipe "nskk-pending-retain" proc _buf
      (let ((faulting t)
            (real-delete (symbol-function 'delete-process)))
        (setq nskk--server-process proc)
        (setq nskk--server-pending-cleanups nil)
        (nskk-server-test--set-state 'open)
        (nskk-with-mocks
            ((delete-process
              (lambda (&rest args)
                (if faulting
                    (error "teardown failed")
                  (apply real-delete args)))))
          (nskk-server-close)
          (should (= (length nskk--server-pending-cleanups) 1))
          (should (eq (caar nskk--server-pending-cleanups) proc))
          (should-not nskk--server-process)
          (setq faulting nil)
          (should (nskk--server-drain-pending-cleanups)))
        (should-not nskk--server-pending-cleanups))))

  (nskk-it "does not register a duplicate entry for the same pair"
    (nskk-server-test--with-pipe "nskk-pending-dedup" proc buf
      (setq nskk--server-pending-cleanups nil)
      (nskk--server-register-pending-cleanup proc buf)
      (nskk--server-register-pending-cleanup proc buf)
      (should (= (length nskk--server-pending-cleanups) 1))
      (setq nskk--server-pending-cleanups nil)))

  (nskk-it "registers kill-emacs-hook at most once"
    (let ((kill-emacs-hook nil)
          (nskk--server-kill-emacs-hook-registered nil))
      (nskk--server-ensure-kill-emacs-hook)
      (nskk--server-ensure-kill-emacs-hook)
      (should (= (cl-count #'nskk-server-close kill-emacs-hook) 1))
      (should nskk--server-kill-emacs-hook-registered)))

  (nskk-it "restores hook membership that was absent before a failed attempt"
    (let ((kill-emacs-hook nil)
          (nskk--server-kill-emacs-hook-registered nil))
      (add-hook 'kill-emacs-hook #'nskk-server-close)
      (nskk--server-restore-hook-registration nil nil)
      (should-not (memq #'nskk-server-close kill-emacs-hook))
      (should-not nskk--server-kill-emacs-hook-registered)))

  (nskk-it "blocks open while a pending resource cannot be reclaimed"
    (nskk-server-test--with-pipe "nskk-pending-blocks-open" proc _buf
      (let ((nskk-server-enable t)
            (connected nil))
        (setq nskk--server-process nil)
        (setq nskk--server-pending-cleanups nil)
        (nskk--server-register-pending-cleanup proc nil)
        (nskk-with-mocks
            ((delete-process (lambda (&rest _) (error "stuck")))
             (nskk--server-make-connection
              (lambda () (setq connected t) proc)))
          (should-not (nskk-server-open))
          (should-not connected))
        (setq nskk--server-pending-cleanups nil)))))

;;;; H. Post-send teardown

(nskk-describe "request teardown"
  (nskk-it "closes the connection when awaiting the response signals"
    (nskk-server-test--with-pipe "nskk-teardown-await-error" proc _buf
      (let ((closed 0))
        (setq nskk--server-process proc)
        (set-process-coding-system proc 'euc-jp-unix 'euc-jp-unix)
        (let ((nskk-server-coding-system 'euc-jp))
          (nskk-with-mocks
              ((process-send-string (lambda (&rest _) nil))
               (nskk--server-await-response/k
                (lambda (&rest _) (error "await failed")))
               (nskk-server-close (lambda () (cl-incf closed))))
            (should-not (nskk--server-with-response "かんじ"))))
        (should (= closed 1)))))

  (nskk-it "closes and preserves the original quit when awaiting quits"
    (nskk-server-test--with-pipe "nskk-teardown-await-quit" proc _buf
      (let ((closed 0)
            (caught nil))
        (setq nskk--server-process proc)
        (set-process-coding-system proc 'euc-jp-unix 'euc-jp-unix)
        (let ((nskk-server-coding-system 'euc-jp))
          (nskk-with-mocks
              ((process-send-string (lambda (&rest _) nil))
               (nskk--server-await-response/k
                (lambda (&rest _) (signal 'quit nil)))
               (nskk-server-close (lambda () (cl-incf closed))))
            (condition-case condition
                (nskk--server-with-response "かんじ")
              (quit (setq caught condition)))))
        (should (eq (car caught) 'quit))
        (should (= closed 1)))))

  (nskk-it "does not close when the coding preflight rejects before sending"
    (nskk-server-test--with-pipe "nskk-teardown-no-send" proc _buf
      (let ((closed 0)
            (sent nil))
        (setq nskk--server-process proc)
        (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
        (let ((nskk-server-coding-system 'euc-jp))
          (nskk-with-mocks
              ((process-send-string (lambda (&rest _) (setq sent t)))
               (nskk-server-close (lambda () (cl-incf closed))))
            (should-not (nskk--server-with-response "かんじ"))))
        (should (= closed 0))
        (should-not sent))))

  (nskk-it "keeps the connection after a completed response"
    (nskk-server-test--with-pipe "nskk-teardown-keep" proc _buf
      (let ((closed 0))
        (setq nskk--server-process proc)
        (set-process-coding-system proc 'euc-jp-unix 'euc-jp-unix)
        (let ((nskk-server-coding-system 'euc-jp))
          (nskk-with-mocks
              ((process-send-string (lambda (&rest _) nil))
               (nskk--server-await-response/k
                (lambda (_proc _buf _budget f _n) (funcall f "1/漢字/\n")))
               (nskk-server-close (lambda () (cl-incf closed))))
            (should (equal (nskk--server-with-response "かんじ")
                           "1/漢字/\n"))))
        (should (= closed 0)))))

  (nskk-it "sends command 1 with the key and a trailing space"
    (nskk-server-test--with-pipe "nskk-teardown-request" proc buf
      (let ((sent nil))
        (setq nskk--server-process proc)
        (set-process-coding-system proc 'euc-jp-unix 'euc-jp-unix)
        (with-current-buffer buf (insert "stale"))
        (let ((nskk-server-coding-system 'euc-jp))
          (nskk-with-mocks
              ((process-send-string (lambda (_p s) (setq sent s)))
               (nskk--server-await-response/k
                (lambda (_proc _buf _budget f _n) (funcall f "1/漢字/\n"))))
            (nskk--server-with-response "かんじ")))
        (should (equal sent "1かんじ "))
        (should (equal (with-current-buffer buf (buffer-string)) ""))))))

;;;; I. Protocol parsing

(nskk-deftest-table server-parse-not-found
  :columns (response)
  :rows (("4かんじ\n")
         ("0\n")
         ("3error\n")
         ("")
         ("1\n")
         ("1//\n"))
  :body (should-not (nskk--server-parse-response response)))

(nskk-deftest-table server-parse-invalid-input
  :columns (response)
  :rows ((nil)
         (42)
         ('(1 2)))
  :body (should-not (nskk--server-parse-response response)))

(nskk-deftest-table server-parse-candidates
  :columns (response expected)
  :rows (("1/漢字/\n"           ("漢字"))
         ("1/漢字/感じ/\n"       ("漢字" "感じ"))
         ("1/漢字/感じ/幹事/\n"  ("漢字" "感じ" "幹事"))
         ("1/漢字/感じ"          ("漢字" "感じ"))
         ("1/漢字/感じ/\r\n"     ("漢字" "感じ"))
         ("1/ 漢字 / 感じ /\n"   ("漢字" "感じ"))
         ("1/漢字;かんじ/\n"     ("漢字")))
  :body (should (equal (nskk--server-parse-response response) expected)))

(nskk-deftest-table server-strip-annotation
  :columns (input expected)
  :rows (("漢字;注釈"      "漢字")
         ("感じ;note"      "感じ")
         ("a;b;c"          "a")
         ("漢字"           "漢字")
         (""               ""))
  :body (should (equal (nskk--server-strip-annotation input) expected)))

(nskk-describe "response type facts"
  (nskk-it "maps the found and miss prefixes"
    (should (nskk-prolog-holds-p '(server-response-type "1" found)))
    (should (nskk-prolog-holds-p '(server-response-type "4" miss)))
    (should-not (nskk-prolog-holds-p '(server-response-type "1" miss)))
    (should-not (nskk-prolog-holds-p '(server-response-type "2" found))))

  (nskk-it "returns only strings from a successful parse"
    (let ((result (nskk--server-parse-response "1/漢字/感じ/\n")))
      (should (listp result))
      (should (cl-every #'stringp result)))))

;;;; J. Variable policy

(nskk-deftest-table server-risky-variables
  :columns (symbol)
  :rows ((nskk-server-enable)
         (nskk-server-host)
         (nskk-server-portnum))
  :body (should (get symbol 'risky-local-variable)))

(nskk-describe "variable policy"
  (nskk-it "gives no nskk-server variable a safe-local-variable predicate"
    (should-not
     (cl-some (lambda (sym) (get sym 'safe-local-variable))
              '(nskk-server-enable nskk-server-host nskk-server-portnum
                nskk-server-coding-system nskk-server-timeout
                nskk-server-report-response)))))

;;;; Disabled-server behaviour

(nskk-describe "disabled server"
  (nskk-it "returns nil from the public entry points without connecting"
    (let ((nskk-server-enable nil)
          (nskk--server-process nil)
          (connected nil))
      (nskk-with-mocks
          ((nskk--server-make-connection (lambda () (setq connected t) nil)))
        (should-not (nskk-server-lookup "かんじ"))
        (should-not (nskk-server-ensure-open))
        (should-not (nskk-server-open))
        (should-not (nskk-server-live-p)))
      (should-not connected))))

(nskk-describe "nskk-server-ensure-open"
  (nskk-it "succeeds without reconnecting when already live"
    (let ((nskk-server-enable t)
          (opened nil))
      (nskk-with-mocks
          ((nskk-server-live-p (lambda () t))
           (nskk-server-open/k (lambda (f _n) (setq opened t) (funcall f 'p))))
        (should (nskk-server-ensure-open))
        (should-not opened))))

  (nskk-it "reconnects when the connection is not live"
    (let ((nskk-server-enable t)
          (opened nil))
      (nskk-with-mocks
          ((nskk-server-live-p (lambda () nil))
           (nskk-server-open/k (lambda (f _n) (setq opened t) (funcall f 'p))))
        (should (nskk-server-ensure-open))
        (should opened))))

  (nskk-it "returns nil when the reconnect fails"
    (let ((nskk-server-enable t))
      (nskk-with-mocks
          ((nskk-server-live-p (lambda () nil))
           (nskk-server-open/k (lambda (_f n) (funcall n))))
        (should-not (nskk-server-ensure-open))))))

;;;; Async connect

(nskk-describe "nskk--server-make-connection"
  (nskk-it "returns the process once the connection reaches open"
    (let ((nskk-server-timeout 5))
      (nskk-with-mocks
          ((open-network-stream (lambda (&rest _) 'mock-proc))
           (get-buffer-create (lambda (_) nil))
           (process-status (lambda (_) 'open))
           (accept-process-output (lambda (&rest _) nil)))
        (should (eq (nskk--server-make-connection) 'mock-proc)))))

  (nskk-it "returns nil and deletes the process when connect fails"
    (let ((nskk-server-timeout 5)
          (deleted nil))
      (nskk-with-mocks
          ((open-network-stream (lambda (&rest _) 'failed-proc))
           (get-buffer-create (lambda (_) nil))
           (process-status (lambda (_) 'failed))
           (accept-process-output (lambda (&rest _) nil))
           (delete-process (lambda (proc) (setq deleted proc))))
        (should-not (nskk--server-make-connection))
        (should (eq deleted 'failed-proc)))))

  (nskk-it "gives up at the timeout and deletes a process stuck connecting"
    (let ((nskk-server-timeout 0.3)
          (deleted nil))
      (nskk-with-mocks
          ((open-network-stream (lambda (&rest _) 'stuck-proc))
           (get-buffer-create (lambda (_) nil))
           (process-status (lambda (_) 'connect))
           (accept-process-output (lambda (&rest _) nil))
           (delete-process (lambda (proc) (setq deleted proc))))
        (should-not (nskk--server-make-connection))
        (should (eq deleted 'stuck-proc)))))

  (nskk-it "does not poll for a non-finite connection budget"
    (dolist (budget '(0.0e+NaN 1.0e+INF -1.0e+INF))
      (let ((nskk-server-timeout budget)
            (polls 0)
            (deleted nil))
        (nskk-with-mocks
            ((open-network-stream (lambda (&rest _) 'stuck-proc))
             (get-buffer-create (lambda (_) nil))
             (process-status (lambda (_) 'connect))
             (accept-process-output (lambda (&rest _) (cl-incf polls)))
             (delete-process (lambda (proc) (setq deleted proc))))
          (should-not (nskk--server-make-connection))
          (should (= polls 0))
          (should (eq deleted 'stuck-proc))))))

  (nskk-it "treats a non-numeric connection timeout as a type error and cleans up"
    (let ((nskk-server-timeout 'invalid)
          (polls 0)
          (deleted nil)
          (failure nil))
      (nskk-with-mocks
          ((open-network-stream (lambda (&rest _) 'stuck-proc))
           (get-buffer-create (lambda (_) nil))
           (process-status (lambda (_) 'connect))
           (accept-process-output (lambda (&rest _) (cl-incf polls)))
           (delete-process (lambda (proc) (setq deleted proc)))
           (nskk-debug-message
            (lambda (_fmt message) (setq failure message))))
        (should-not (nskk--server-make-connection))
        (should (= polls 0))
        (should (eq deleted 'stuck-proc))
        (should (string-match-p "numberp" failure)))))

  (nskk-it "never polls longer than the remaining connection budget"
    (let ((nskk-server-timeout 0.025)
          (status 'connect)
          (wait nil))
      (nskk-with-mocks
          ((open-network-stream (lambda (&rest _) 'stuck-proc))
           (get-buffer-create (lambda (_) nil))
           (process-status (lambda (_) status))
           (accept-process-output
            (lambda (_proc timeout &optional _ms _just)
              (setq wait timeout
                    status 'failed)))
           (delete-process (lambda (_) nil)))
        (should-not (nskk--server-make-connection))
        (should (> wait 0))
        (should (<= wait 0.025001)))))

  (nskk-it "keeps the connection budget finite when the wall clock jumps"
    (dolist (clock-values '((100.0 99.0 98.0 101.0)
                            (100.0 101.0 102.0 103.0)))
      (let ((nskk-server-timeout 0.25)
            (readings (copy-sequence clock-values))
            (clock-calls 0)
            (waits nil)
            (deleted nil))
        (nskk-with-mocks
            ((open-network-stream (lambda (&rest _) 'stuck-proc))
             (get-buffer-create (lambda (_) nil))
             (process-status (lambda (_) 'connect))
             (float-time
              (lambda (&optional _)
                (cl-incf clock-calls)
                (or (pop readings) 1000.0)))
             (accept-process-output
              (lambda (proc timeout &optional _ms _just)
                (should (eq proc 'stuck-proc))
                (push timeout waits)))
             (delete-process (lambda (proc) (setq deleted proc))))
          (should-not (nskk--server-make-connection))
          (should (eq deleted 'stuck-proc))
          (should (= clock-calls 0))
          (should (= (length waits) 3))
          (should (cl-every (lambda (w) (and (> w 0) (<= w 0.1))) waits))
          (should (< (abs (- (apply #'+ waits) 0.25)) 0.000001))))))

  (nskk-it "releases the process and owned buffer when initialisation signals"
    (let* ((nskk--server-buffer-name " *nskk-server-init-error*")
           (proc (make-pipe-process :name "nskk-server-init-error"
                                    :buffer nil
                                    :noquery t)))
      (unwind-protect
          (nskk-with-mocks
              ((open-network-stream (lambda (&rest _) proc))
               (set-process-filter
                (lambda (&rest _) (error "filter setup failed"))))
            (should-not (nskk--server-make-connection))
            (should-not (process-live-p proc))
            (should-not (get-buffer nskk--server-buffer-name)))
        (when (process-live-p proc) (delete-process proc))
        (when-let* ((buf (get-buffer nskk--server-buffer-name)))
          (kill-buffer buf)))))

  (nskk-it "releases the process and owned buffer when initialisation quits"
    (let* ((nskk--server-buffer-name " *nskk-server-init-quit*")
           (proc (make-pipe-process :name "nskk-server-init-quit"
                                    :buffer nil
                                    :noquery t))
           (caught nil))
      (unwind-protect
          (nskk-with-mocks
              ((open-network-stream (lambda (&rest _) proc))
               (set-process-filter
                (lambda (&rest _) (signal 'quit '("filter quit")))))
            (condition-case condition
                (nskk--server-make-connection)
              (quit (setq caught condition)))
            (should (eq (car caught) 'quit))
            (should-not (process-live-p proc))
            (should-not (get-buffer nskk--server-buffer-name)))
        (when (process-live-p proc) (delete-process proc))
        (when-let* ((buf (get-buffer nskk--server-buffer-name)))
          (kill-buffer buf))))))

;;;; Coding preflight under quit

(nskk-describe "coding preflight quit safety"
  (nskk-it "fails closed at every preflight stage and stops observing there"
    (dolist (case '((live (configured live))
                    (actual (configured live actual))
                    (base (configured live actual base))
                    (key (configured live actual base base key))))
      (let ((quit-stage (car case))
            (expected (cadr case))
            (observed nil)
            (nskk-server-coding-system 'utf-8))
        (nskk-with-mocks
            ((coding-system-p
              (lambda (_) (push 'configured observed) t))
             (process-live-p
              (lambda (_)
                (push 'live observed)
                (if (eq quit-stage 'live) (signal 'quit nil) t)))
             (process-coding-system
              (lambda (_)
                (push 'actual observed)
                (if (eq quit-stage 'actual)
                    (signal 'quit nil)
                  (cons 'utf-8 'utf-8))))
             (coding-system-base
              (lambda (_)
                (push 'base observed)
                (if (eq quit-stage 'base) (signal 'quit nil) 'utf-8)))
             (nskk--server-key-safe-for-coding-p
              (lambda (&rest _)
                (push 'key observed)
                (if (eq quit-stage 'key) (signal 'quit nil) t))))
          (should-not
           (nskk--server-process-safe-for-coding-p 'mock-process "abc")))
        (should (equal (nreverse observed) expected)))))

  (nskk-it "leaves public lookup state untouched when the preflight quits"
    (dolist (case '((live (configured live))
                    (actual (configured live actual))
                    (base (configured live actual base))
                    (key (configured live actual base base key))))
      (with-temp-buffer
        (insert "sentinel")
        (let* ((buf (current-buffer))
               (proc (make-pipe-process
                      :name (format "nskk-preflight-quit-%s" (car case))
                      :buffer buf
                      :coding '(utf-8 . utf-8)
                      :noquery t))
               (quit-stage (car case))
               (expected (cadr case))
               (observed nil)
               (io-observed nil)
               (result 'uninitialized)
               (nskk-server-enable t)
               (nskk-server-coding-system 'utf-8)
               (nskk--server-process proc))
          (process-put proc 'nskk-response-bytes 17)
          (process-put proc 'nskk-response-overflow 'overflow)
          (process-put proc 'nskk-response-complete 'complete)
          (unwind-protect
              (progn
                (nskk-with-mocks
                    ((nskk--server-lookup-guards-p/k
                      (lambda (_key on-found _nf) (funcall on-found t)))
                     (coding-system-p
                      (lambda (_) (push 'configured observed) t))
                     (process-live-p
                      (lambda (_)
                        (push 'live observed)
                        (if (eq quit-stage 'live) (signal 'quit nil) t)))
                     (process-coding-system
                      (lambda (_)
                        (push 'actual observed)
                        (if (eq quit-stage 'actual)
                            (signal 'quit nil)
                          (cons 'utf-8 'utf-8))))
                     (coding-system-base
                      (lambda (_)
                        (push 'base observed)
                        (if (eq quit-stage 'base) (signal 'quit nil) 'utf-8)))
                     (nskk--server-key-safe-for-coding-p
                      (lambda (&rest _)
                        (push 'key observed)
                        (if (eq quit-stage 'key) (signal 'quit nil) t)))
                     (process-buffer
                      (lambda (&rest _)
                        (push 'buffer io-observed)
                        (error "buffer must not be read")))
                     (process-put
                      (lambda (&rest _)
                        (push 'property-write io-observed)
                        (error "properties must not be written")))
                     (erase-buffer
                      (lambda ()
                        (push 'erase io-observed)
                        (error "buffer must not be erased")))
                     (process-send-string
                      (lambda (&rest _)
                        (push 'send io-observed)
                        (error "request must not be sent")))
                     (nskk-server-close
                      (lambda ()
                        (push 'cleanup io-observed)
                        (error "cleanup must not run"))))
                  (setq result (nskk-server-lookup "abc")))
                (should-not result)
                (should (equal (nreverse observed) expected))
                (should-not io-observed)
                (should (equal (buffer-string) "sentinel"))
                (should (= (process-get proc 'nskk-response-bytes) 17))
                (should (eq (process-get proc 'nskk-response-overflow)
                            'overflow))
                (should (eq (process-get proc 'nskk-response-complete)
                            'complete)))
            (when (process-live-p proc) (delete-process proc))))))))

;;;; Configure-time rollback

(nskk-describe "nskk-server-open configure rollback"
  (nskk-it "rolls back every configure error and quit without altering the signal"
    (let ((real-flag (symbol-function 'set-process-query-on-exit-flag))
          (real-add-hook (symbol-function 'add-hook))
          (real-retract (symbol-function 'nskk-prolog-retract-all))
          (real-assert (symbol-function 'nskk-prolog-assert)))
      (dolist (kind '(error quit))
        (dolist (stage '(query hook retract assert debug))
          (let* ((name (format "nskk-configure-%s-%s" stage kind))
                 (nskk--server-buffer-name (format " *%s*" name))
                 (buffer (get-buffer-create nskk--server-buffer-name))
                 (proc (make-pipe-process :name name
                                          :buffer buffer
                                          :noquery t))
                 (nskk-server-enable t)
                 (nskk--server-process 'previous-process)
                 (nskk--server-pending-cleanups nil)
                 (nskk--server-kill-emacs-hook-registered nil)
                 (kill-emacs-hook nil)
                 (data (list "injected" stage kind))
                 (caught nil)
                 (retract-injected nil))
            (unwind-protect
                (cl-letf
                    (((symbol-function 'nskk--server-make-connection)
                      (lambda ()
                        (process-put proc 'nskk-server-owned-buffer t)
                        proc))
                     ((symbol-function 'set-process-query-on-exit-flag)
                      (lambda (&rest args)
                        (if (eq stage 'query)
                            (signal kind data)
                          (apply real-flag args))))
                     ((symbol-function 'add-hook)
                      (lambda (&rest args)
                        (apply real-add-hook args)
                        (when (eq stage 'hook) (signal kind data))))
                     ((symbol-function 'nskk-prolog-retract-all)
                      (lambda (&rest args)
                        (if (and (eq stage 'retract) (not retract-injected))
                            (progn
                              (setq retract-injected t)
                              (signal kind data))
                          (apply real-retract args))))
                     ((symbol-function 'nskk-prolog-assert)
                      (lambda (facts)
                        (if (and (eq stage 'assert)
                                 (equal facts '((server-state open))))
                            (signal kind data)
                          (funcall real-assert facts))))
                     ((symbol-function 'nskk-debug-message)
                      (lambda (&rest _)
                        (when (eq stage 'debug) (signal kind data)))))
                  (condition-case condition
                      (nskk-server-open)
                    ((error quit) (setq caught condition)))
                  (should (eq (car caught) kind))
                  (should (equal (cdr caught) data))
                  (should (eq nskk--server-process 'previous-process))
                  (should-not nskk--server-kill-emacs-hook-registered)
                  (should-not (memq #'nskk-server-close kill-emacs-hook))
                  (should-not (process-live-p proc))
                  (should-not (buffer-live-p buffer))
                  (should (nskk-prolog-holds-p '(server-state closed))))
              (when (process-live-p proc) (delete-process proc))
              (when (buffer-live-p buffer) (kill-buffer buffer))
              (funcall real-retract 'server-state 1)
              (funcall real-assert '((server-state closed))))))))))

;;;; Open idempotence and buffer-kill hygiene

(nskk-describe "repeated opens"
  (nskk-it "reuses one process and one owned buffer across repeated opens"
    (let ((real-configure (symbol-function 'nskk--server-configure-process))
          (real-delete (symbol-function 'delete-process))
          (real-add-hook (symbol-function 'add-hook))
          (real-retract (symbol-function 'nskk-prolog-retract-all))
          (real-assert (symbol-function 'nskk-prolog-assert))
          (nskk-server-enable t)
          (nskk--server-process nil)
          (nskk--server-pending-cleanups nil)
          (nskk--server-kill-emacs-hook-registered nil)
          (kill-emacs-hook nil)
          (nskk--server-buffer-name " *nskk-idempotent-open*")
          (make-count 0)
          (configure-count 0)
          (delete-count 0)
          (add-hook-count 0)
          (created nil)
          (owned-buffer nil))
      (unwind-protect
          (cl-letf
              (((symbol-function 'nskk--server-make-connection)
                (lambda ()
                  (cl-incf make-count)
                  (let* ((owned (not (get-buffer nskk--server-buffer-name)))
                         (buffer (get-buffer-create nskk--server-buffer-name))
                         (proc (make-pipe-process
                                :name (format "nskk-idempotent-open-%d"
                                              make-count)
                                :buffer buffer
                                :noquery t)))
                    (process-put proc 'nskk-server-owned-buffer owned)
                    (when owned (setq owned-buffer buffer))
                    (push proc created)
                    proc)))
               ((symbol-function 'nskk--server-configure-process)
                (lambda (proc)
                  (cl-incf configure-count)
                  (funcall real-configure proc)))
               ((symbol-function 'delete-process)
                (lambda (proc)
                  (cl-incf delete-count)
                  (funcall real-delete proc)))
               ((symbol-function 'add-hook)
                (lambda (&rest args)
                  (cl-incf add-hook-count)
                  (apply real-add-hook args))))
            (let ((first (nskk-server-open))
                  (hooks-after-first nil)
                  (second nil))
              (setq hooks-after-first add-hook-count)
              (setq second (nskk-server-open))
              (should (processp first))
              (should (eq second first))
              (should (eq nskk--server-process first))
              (should (= make-count 1))
              (should (= configure-count 1))
              (should (= delete-count 0))
              (should (= add-hook-count hooks-after-first))
              (should (memq #'nskk-server-close kill-emacs-hook))
              (should (nskk-prolog-holds-p '(server-state open)))
              (should (= (length created) 1))
              (should (eq (process-buffer first) owned-buffer))
              (should (process-live-p first))
              (nskk-server-close)
              (should (> delete-count 0))
              (should-not (process-live-p first))
              (should-not nskk--server-process)
              (should-not (buffer-live-p owned-buffer))))
        (dolist (proc created)
          (when (process-live-p proc) (funcall real-delete proc)))
        (when-let* ((buf (get-buffer nskk--server-buffer-name)))
          (kill-buffer buf))
        (setq nskk--server-process nil)
        (funcall real-retract 'server-state 1)
        (funcall real-assert '((server-state closed))))))

  (nskk-it "kills an owned buffer without running hooks or query functions"
    (let* ((nskk--server-buffer-name " *nskk-server-close-owned*")
           (buffer (get-buffer-create nskk--server-buffer-name))
           (proc (make-pipe-process :name "nskk-server-close-owned"
                                    :buffer buffer
                                    :noquery nil))
           (nskk--server-process proc)
           (nskk--server-pending-cleanups nil)
           (hook-called nil)
           (query-called nil))
      (process-put proc 'nskk-server-owned-buffer t)
      (set-process-query-on-exit-flag proc t)
      (with-current-buffer buffer
        (setq-local buffer-offer-save t)
        (set-buffer-modified-p t)
        (add-hook 'kill-buffer-hook (lambda () (setq hook-called t)) nil t)
        (add-hook 'kill-buffer-query-functions
                  (lambda () (setq query-called t) nil)
                  nil t))
      (unwind-protect
          (progn
            (nskk-server-close)
            (should-not (buffer-live-p buffer))
            (should-not hook-called)
            (should-not query-called)
            (should-not (process-query-on-exit-flag proc)))
        (when (process-live-p proc)
          (set-process-query-on-exit-flag proc nil)
          (delete-process proc))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((kill-buffer-hook nil)
                  (kill-buffer-query-functions nil)
                  (buffer-offer-save nil))
              (set-buffer-modified-p nil)
              (kill-buffer buffer))))
        (nskk-server-test--set-state 'closed)))))

;;;; Setup faults before the request is sent

(nskk-describe "pre-send setup faults"
  (nskk-it "does not close when preparing the request signals before any send"
    (nskk-server-test--with-pipe "nskk-presend-fault" proc _buf
      (let ((closed 0)
            (sent nil))
        (setq nskk--server-process proc)
        (set-process-coding-system proc 'euc-jp-unix 'euc-jp-unix)
        (let ((nskk-server-coding-system 'euc-jp))
          (nskk-with-mocks
              ((erase-buffer (lambda () (error "buffer must not be erased")))
               (process-send-string (lambda (&rest _) (setq sent t)))
               (nskk-server-close (lambda () (cl-incf closed))))
            (should-not (nskk--server-with-response "かんじ"))))
        (should-not sent)
        (should (= closed 0))))))

(provide 'nskk-server-test)

;;; nskk-server-test.el ends here
