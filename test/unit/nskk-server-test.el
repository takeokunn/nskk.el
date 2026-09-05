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

(provide 'nskk-server-test)

;;; nskk-server-test.el ends here
