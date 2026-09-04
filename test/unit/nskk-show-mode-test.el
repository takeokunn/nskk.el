;;; nskk-show-mode-test.el --- Tests for nskk-show-mode.el -*- lexical-binding: t; -*-
;; Copyright (C) 2026 NSKK Authors
;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test
;; This file is part of NSKK.
;;; Commentary:
;; Tests for nskk-show-mode.el.
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-show-mode)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

(defmacro nskk-show-mode-test-with-tooltip-runtime (&rest body)
  "Run BODY with deterministic tooltip and timer implementations."
  (declare (indent 0)
           (debug t))
  `(let ((nskk--show-mode-tooltip-owner nil)
         (nskk--show-mode-tooltip-generation 0)
         (nskk--show-mode-tooltip-timer nil)
         (nskk-show-mode-test--tooltip-hide-count 0))
     (cl-letf (((symbol-function 'display-graphic-p)
                (lambda (&rest _)
                  t))
               ((symbol-function 'posn-at-point)
                (lambda (&rest _)
                  '(mock-position)))
               ((symbol-function 'tooltip-show)
                (lambda (&rest _)
                  nil))
               ((symbol-function 'tooltip-hide)
                (lambda ()
                  (cl-incf nskk-show-mode-test--tooltip-hide-count)))
               ((symbol-function 'run-with-timer)
                (lambda (_seconds _repeat function &rest args)
                  (vector function args nil)))
               ((symbol-function 'timerp)
                (lambda (object)
                  (and (vectorp object) (= (length object) 3))))
               ((symbol-function 'cancel-timer)
                (lambda (timer)
                  (aset timer 2 t))))
       (unwind-protect (progn
                         ,@body)
         (when nskk--show-mode-tooltip-owner
           (nskk--show-mode-release-tooltip nskk--show-mode-tooltip-owner nil))))))

;;;; Customization Defaults

(nskk-describe
  "nskk-show-mode customization defaults"
  (nskk-it
    "nskk-show-mode-duration is a number"
    (should (numberp nskk-show-mode-duration)))
  (nskk-it
    "nskk-show-mode-style is one of valid choices"
    (should (memq nskk-show-mode-style '(inline tooltip)))))

;;;; Indicator String

(nskk-describe
  "nskk--show-mode-indicator-string"
  (nskk-it
    "returns nil for unknown mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (should (null (nskk--show-mode-indicator-string 'nonexistent-mode))))))

;;;; Exact Indicator String Content

(nskk-deftest-table
  show-mode-indicator-exact-strings
  :columns
  (mode expected-content)
  :rows
  ((hiragana "かな")
    (katakana "カナ")
    (katakana-半角 "ｶﾅ")
    (ascii "SKK")
    (latin "SKK")
    (jisx0208-latin "全英")
    (abbrev "aA"))
  :description
  "nskk--show-mode-indicator-string returns exact [DISPLAY] with the indicator face"
  :body
  (nskk-prolog-test-with-isolated-db
    (nskk-state-initialize-prolog)
    (should
      (equal-including-properties
        (nskk--show-mode-indicator-string mode)
        (propertize (format "[%s]" expected-content)
                    'face 'nskk-show-mode-inline-face)))))

;;;; Cleanup Ordering

(nskk-describe "nskk--show-mode-run-cleanups"
  (nskk-it "runs every cleanup and re-signals the first failure"
    (let (ran caught)
      (condition-case condition
          (nskk--show-mode-run-cleanups
           (lambda () (push 'first ran) (signal 'error '("first failure")))
           (lambda () (push 'second ran) (signal 'error '("second failure")))
           (lambda () (push 'third ran)))
        (error (setq caught condition)))
      (should (equal (nreverse ran) '(first second third)))
      (should (equal caught '(error "first failure"))))))

;;;; Display Guards

(nskk-describe "nskk-show-mode-display no-op conditions"
  (nskk-it "displays nothing when nskk-show-mode-show is nil"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-show-mode-show nil)
              (nskk-show-mode-style 'inline)
              (nskk-show-mode-duration 60)
              (nskk--show-mode-overlay nil)
              (nskk--show-mode-timer nil)
              (nskk--show-mode-last-mode nil))
          (nskk-show-mode-display)
          (should-not nskk--show-mode-overlay)
          (should-not nskk--show-mode-timer)
          (should-not nskk--show-mode-last-mode)))))

  (nskk-it "is a no-op when nskk-current-state is nil"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (setq nskk-current-state nil)
        (let ((nskk-show-mode-show t))
          (should-not (condition-case err
                          (progn (nskk-show-mode-display) nil)
                        (error err)))))))

  (nskk-it "renders inline for a style that is neither inline nor tooltip"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-show-mode-show t)
              (nskk-show-mode-style 'bogus)
              (nskk-show-mode-duration 60)
              (nskk--show-mode-overlay nil)
              (nskk--show-mode-timer nil)
              (nskk--show-mode-last-mode nil))
          (unwind-protect
              (progn
                (nskk-show-mode-display)
                (should (overlayp nskk--show-mode-overlay))
                (should (equal (overlay-get nskk--show-mode-overlay 'after-string)
                               (nskk--show-mode-indicator-string 'hiragana))))
            (when (timerp nskk--show-mode-timer)
              (cancel-timer nskk--show-mode-timer)))))))

  (nskk-it "leaves the mode retryable when tooltip style has no GUI"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-show-mode-show t)
              (nskk-show-mode-style 'tooltip)
              (nskk-show-mode-duration 60)
              (nskk--show-mode-tooltip-owner nil)
              (nskk--show-mode-tooltip-timer nil)
              (nskk--show-mode-overlay nil)
              (nskk--show-mode-timer nil)
              (nskk--show-mode-last-mode nil))
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&rest _) nil)))
            (nskk-show-mode-display)
            (should-not nskk--show-mode-tooltip-owner)
            (should-not nskk--show-mode-tooltip-timer)
            (should-not nskk--show-mode-overlay)
            (should-not nskk--show-mode-last-mode)))))))

;;;; Hide Cleanup

(nskk-describe "nskk-show-mode-hide"
  (nskk-it "clears last-mode after hide"
    (with-temp-buffer
      (setq nskk--show-mode-last-mode 'hiragana)
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil))
        (nskk-show-mode-hide)
        (should (null nskk--show-mode-last-mode)))))

  (nskk-it "continues tooltip release when deleting the overlay errors"
    (with-temp-buffer
      (let* ((overlay (make-overlay (point) (point)))
             (timer (run-with-timer 60 nil #'ignore))
             (real-delete-overlay (symbol-function 'delete-overlay))
             (real-cancel-timer (symbol-function 'cancel-timer))
             (nskk--show-mode-overlay overlay)
             (nskk--show-mode-timer timer)
             (nskk--show-mode-last-mode 'hiragana)
             cancel-called
             tooltip-release-called
             caught)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'delete-overlay)
                         (lambda (_overlay)
                           (error "overlay deletion failed")))
                        ((symbol-function 'cancel-timer)
                         (lambda (_timer)
                           (setq cancel-called t)))
                        ((symbol-function 'nskk--show-mode-release-tooltip)
                         (lambda (_owner _hide)
                           (setq tooltip-release-called t))))
                (condition-case condition
                    (nskk-show-mode-hide)
                  (error
                   (setq caught condition))))
              (should (eq (car caught) 'error))
              (should cancel-called)
              (should tooltip-release-called)
              (should-not nskk--show-mode-last-mode)
              (should-not nskk--show-mode-overlay)
              (should-not nskk--show-mode-timer))
          (funcall real-delete-overlay overlay)
          (funcall real-cancel-timer timer)))))

  (nskk-it "invalidates last-mode when cancelling the timer quits"
    (with-temp-buffer
      (let* ((overlay (make-overlay (point) (point)))
             (timer (run-with-timer 60 nil #'ignore))
             (real-cancel-timer (symbol-function 'cancel-timer))
             (nskk--show-mode-overlay overlay)
             (nskk--show-mode-timer timer)
             (nskk--show-mode-last-mode 'hiragana)
             caught)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'cancel-timer)
                         (lambda (_timer)
                           (signal 'quit nil))))
                (condition-case condition
                    (nskk--show-mode-hide-inline)
                  (quit
                   (setq caught condition))))
              (should (eq (car caught) 'quit))
              (should-not (overlay-buffer overlay))
              (should-not nskk--show-mode-last-mode)
              (should-not nskk--show-mode-overlay)
              (should-not nskk--show-mode-timer))
          (funcall real-cancel-timer timer))))))

;;;; nskk--show-mode-display-inline

(nskk-describe
  "nskk--show-mode-display-inline"
  (nskk-it
    "sets after-string property to the indicator string"
    (with-temp-buffer
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil)
            (nskk-show-mode-duration 60))
        (nskk--show-mode-display-inline "[かな]")
        (unwind-protect (should (equal (overlay-get nskk--show-mode-overlay 'after-string) "[かな]"))
          (when (timerp nskk--show-mode-timer)
            (cancel-timer nskk--show-mode-timer))))))
  (nskk-it
    "overlay priority is 100"
    (with-temp-buffer
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil)
            (nskk-show-mode-duration 60))
        (nskk--show-mode-display-inline "[かな]")
        (unwind-protect (should (= (overlay-get nskk--show-mode-overlay 'priority) 100))
          (when (timerp nskk--show-mode-timer)
            (cancel-timer nskk--show-mode-timer))))))
  (nskk-it
    "clears old inline state when tooltip release quits"
    (with-temp-buffer
      (let* ((overlay (make-overlay (point) (point)))
             (timer (run-with-timer 60 nil #'ignore))
             (real-cancel-timer (symbol-function 'cancel-timer))
             (nskk--show-mode-overlay overlay)
             (nskk--show-mode-timer timer)
             (nskk--show-mode-last-mode 'hiragana)
             caught)
        (unwind-protect (progn
            (cl-letf
              (((symbol-function 'nskk--show-mode-release-tooltip)
                  (lambda (_owner _hide)
                    (signal 'quit nil))))
              (condition-case
                condition
                (nskk--show-mode-display-inline "[カナ]")
                (quit
                  (setq caught condition))))
            (should (eq (car caught) 'quit))
            (should-not (overlay-buffer overlay))
            (should-not nskk--show-mode-last-mode)
            (should-not nskk--show-mode-overlay)
            (should-not nskk--show-mode-timer))
          (funcall real-cancel-timer timer))))))

;;;; nskk-show-mode-hide cancels timer

(nskk-describe "nskk-show-mode-hide timer handling"
  (nskk-it "cancels pending timer when one exists"
    (with-temp-buffer
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil)
            (nskk--show-mode-last-mode nil)
            (nskk-show-mode-duration 60))
        (nskk--show-mode-display-inline "[かな]")
        (should (timerp nskk--show-mode-timer))
        (nskk-show-mode-hide)
        (should (null nskk--show-mode-timer)))))

  (nskk-it "removes the overlay when one exists"
    (with-temp-buffer
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil)
            (nskk--show-mode-last-mode nil)
            (nskk-show-mode-duration 60))
        (nskk--show-mode-display-inline "[かな]")
        (should (overlayp nskk--show-mode-overlay))
        (nskk-show-mode-hide)
        (should (null nskk--show-mode-overlay))))))

;;;; nskk-show-mode-display integration

(nskk-describe "nskk-show-mode-display integration"
  (nskk-it "updates nskk--show-mode-last-mode after first display"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-show-mode-show t)
              (nskk-show-mode-style 'inline)
              (nskk-show-mode-duration 60)
              (nskk--show-mode-overlay nil)
              (nskk--show-mode-timer nil)
              (nskk--show-mode-last-mode nil))
          (nskk-show-mode-display)
          (unwind-protect
              (should (eq nskk--show-mode-last-mode 'hiragana))
            (when (timerp nskk--show-mode-timer)
              (cancel-timer nskk--show-mode-timer)))))))

  (nskk-it "skips display when mode is unchanged (deduplication)"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-show-mode-show t)
              (nskk-show-mode-style 'inline)
              (nskk-show-mode-duration 60)
              (nskk--show-mode-overlay nil)
              (nskk--show-mode-timer nil)
              (nskk--show-mode-last-mode 'hiragana)
              (call-count 0))
          (cl-letf (((symbol-function 'nskk--show-mode-display-inline)
                     (lambda (_s) (cl-incf call-count))))
            (nskk-show-mode-display)
            (should (= call-count 0)))))))

  (nskk-it "re-displays when mode changes"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'katakana))
        (let ((nskk-show-mode-show t)
              (nskk-show-mode-style 'inline)
              (nskk-show-mode-duration 60)
              (nskk--show-mode-overlay nil)
              (nskk--show-mode-timer nil)
              (nskk--show-mode-last-mode 'hiragana)
              (call-count 0))
          (cl-letf (((symbol-function 'nskk--show-mode-display-inline)
                     (lambda (_s) (cl-incf call-count) t)))
            (nskk-show-mode-display)
            (should (= call-count 1)))))))

  (nskk-it "retries the same mode after tooltip display errors"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-show-mode-show t)
              (nskk-show-mode-style 'tooltip)
              (nskk-show-mode-duration 60)
              (nskk--show-mode-last-mode nil))
          (nskk-show-mode-test-with-tooltip-runtime
            (cl-letf (((symbol-function 'tooltip-show)
                       (lambda (&rest _)
                         (error "tooltip display failed"))))
              (should-error (nskk-show-mode-display) :type 'error))
            (should-not nskk--show-mode-last-mode)
            (nskk-show-mode-display)
            (should (eq nskk--show-mode-last-mode 'hiragana)))))))

  (nskk-it "retries the same mode after tooltip timer scheduling quits"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-show-mode-show t)
              (nskk-show-mode-style 'tooltip)
              (nskk-show-mode-duration 60)
              (nskk--show-mode-last-mode nil)
              caught)
          (nskk-show-mode-test-with-tooltip-runtime
            (cl-letf (((symbol-function 'run-with-timer)
                       (lambda (&rest _)
                         (signal 'quit nil))))
              (condition-case condition
                  (nskk-show-mode-display)
                (quit
                 (setq caught condition))))
            (should (eq (car caught) 'quit))
            (should-not nskk--show-mode-last-mode)
            (nskk-show-mode-display)
            (should (eq nskk--show-mode-last-mode 'hiragana))))))))

;;;; Tooltip Global Ownership

(nskk-describe
  "nskk tooltip global ownership"
  (nskk-it
    "replaces a same-buffer tooltip and ignores its stale callback"
    (nskk-show-mode-test-with-tooltip-runtime
      (with-temp-buffer
        (let ((nskk-show-mode-duration 60))
          (nskk--show-mode-display-tooltip "[かな]")
          (let* ((first-generation nskk--show-mode-tooltip-generation)
                 (first-timer nskk--show-mode-tooltip-timer)
                 (first-callback (aref first-timer 0))
                 (first-args (aref first-timer 1)))
            (nskk--show-mode-display-tooltip "[カナ]")
            (should (aref first-timer 2))
            (should (eq nskk--show-mode-tooltip-owner (current-buffer)))
            (should (> nskk--show-mode-tooltip-generation first-generation))
            (should-not (eq nskk--show-mode-tooltip-timer first-timer))
            (apply first-callback first-args)
            (should (eq nskk--show-mode-tooltip-owner (current-buffer)))
            (should nskk--show-mode-tooltip-timer)
            (should (= nskk-show-mode-test--tooltip-hide-count 0))
            (let ((current-timer nskk--show-mode-tooltip-timer))
              (apply (aref current-timer 0) (aref current-timer 1)))
            (should-not nskk--show-mode-tooltip-owner)
            (should-not nskk--show-mode-tooltip-timer)
            (should (= nskk-show-mode-test--tooltip-hide-count 1)))))))
  (nskk-it
    "transfers ownership across buffers and ignores the old callback"
    (nskk-show-mode-test-with-tooltip-runtime
      (let ((first-buffer (generate-new-buffer " *nskk-tooltip-first*"))
            (second-buffer (generate-new-buffer " *nskk-tooltip-second*")))
        (unwind-protect (progn
            (with-current-buffer first-buffer (nskk--show-mode-display-tooltip "[かな]"))
            (let* ((first-timer nskk--show-mode-tooltip-timer)
                   (first-callback (aref first-timer 0))
                   (first-args (aref first-timer 1)))
              (with-current-buffer second-buffer (nskk--show-mode-display-tooltip "[カナ]"))
              (should (aref first-timer 2))
              (should (eq nskk--show-mode-tooltip-owner second-buffer))
              (with-current-buffer
                first-buffer
                (should-not (memq #'nskk--show-mode-tooltip-owner-killed kill-buffer-hook)))
              (with-current-buffer
                second-buffer
                (should (memq #'nskk--show-mode-tooltip-owner-killed kill-buffer-hook)))
              (apply first-callback first-args)
              (should (eq nskk--show-mode-tooltip-owner second-buffer))
              (should (= nskk-show-mode-test--tooltip-hide-count 0))
              (let ((current-timer nskk--show-mode-tooltip-timer))
                (apply (aref current-timer 0) (aref current-timer 1)))
              (should-not nskk--show-mode-tooltip-owner)
              (should (= nskk-show-mode-test--tooltip-hide-count 1))))
          (when (buffer-live-p first-buffer)
            (kill-buffer first-buffer))
          (when (buffer-live-p second-buffer)
            (kill-buffer second-buffer))))))
  (nskk-it
    "does not hide a tooltip owned by another buffer"
    (nskk-show-mode-test-with-tooltip-runtime
      (let ((owner (generate-new-buffer " *nskk-tooltip-owner*"))
            (other (generate-new-buffer " *nskk-tooltip-other*")))
        (unwind-protect (progn
            (with-current-buffer owner (nskk--show-mode-display-tooltip "[かな]"))
            (let ((owner-timer nskk--show-mode-tooltip-timer))
              (with-current-buffer other (nskk-show-mode-hide))
              (should (eq nskk--show-mode-tooltip-owner owner))
              (should (eq nskk--show-mode-tooltip-timer owner-timer))
              (should-not (aref owner-timer 2))
              (should (= nskk-show-mode-test--tooltip-hide-count 0))))
          (when (buffer-live-p owner)
            (kill-buffer owner))
          (when (buffer-live-p other)
            (kill-buffer other))))))
  (nskk-it
    "releases a tooltip when its owner buffer is killed"
    (nskk-show-mode-test-with-tooltip-runtime
      (let ((owner (generate-new-buffer " *nskk-tooltip-killed-owner*")))
        (unwind-protect (progn
            (with-current-buffer owner (nskk--show-mode-display-tooltip "[かな]"))
            (let ((owner-timer nskk--show-mode-tooltip-timer))
              (kill-buffer owner)
              (should-not nskk--show-mode-tooltip-owner)
              (should-not nskk--show-mode-tooltip-timer)
              (should (aref owner-timer 2))
              (should (= nskk-show-mode-test--tooltip-hide-count 1))))
          (when (buffer-live-p owner)
            (kill-buffer owner))))))
  (nskk-it
    "releases a same-buffer tooltip before displaying inline"
    (nskk-show-mode-test-with-tooltip-runtime
      (with-temp-buffer
        (nskk--show-mode-display-tooltip "[かな]")
        (let ((tooltip-timer nskk--show-mode-tooltip-timer))
          (nskk--show-mode-display-inline "[カナ]")
          (should-not nskk--show-mode-tooltip-owner)
          (should-not nskk--show-mode-tooltip-timer)
          (should (aref tooltip-timer 2))
          (should (= nskk-show-mode-test--tooltip-hide-count 1))
          (should (overlayp nskk--show-mode-overlay))
          (should nskk--show-mode-timer)
          (nskk--show-mode-hide-inline)))))
  (nskk-it
    "fails closed when replacement display signals an error"
    (nskk-show-mode-test-with-tooltip-runtime
      (let ((old-owner (generate-new-buffer " *nskk-tooltip-old*"))
            (new-owner (generate-new-buffer " *nskk-tooltip-new*")))
        (unwind-protect (progn
            (with-current-buffer old-owner (nskk--show-mode-display-tooltip "[かな]"))
            (let* ((old-timer nskk--show-mode-tooltip-timer)
                   (old-callback (aref old-timer 0))
                   (old-args (aref old-timer 1)))
              (cl-letf
                (((symbol-function 'tooltip-show)
                    (lambda (&rest _)
                      (error "tooltip display failed"))))
                (with-current-buffer
                  new-owner
                  (should-error (nskk--show-mode-display-tooltip "[カナ]") :type 'error)))
              (should-not nskk--show-mode-tooltip-owner)
              (should-not nskk--show-mode-tooltip-timer)
              (should (aref old-timer 2))
              (with-current-buffer
                old-owner
                (should-not (memq #'nskk--show-mode-tooltip-owner-killed kill-buffer-hook)))
              (with-current-buffer
                new-owner
                (should-not (memq #'nskk--show-mode-tooltip-owner-killed kill-buffer-hook)))
              (should (= nskk-show-mode-test--tooltip-hide-count 1))
              (apply old-callback old-args)
              (should (= nskk-show-mode-test--tooltip-hide-count 1))))
          (when (buffer-live-p old-owner)
            (kill-buffer old-owner))
          (when (buffer-live-p new-owner)
            (kill-buffer new-owner))))))
  (nskk-context
    "failure cleanup"
    (nskk-it
      "fails closed when tooltip timer scheduling signals quit"
      (nskk-show-mode-test-with-tooltip-runtime
        (let ((owner (generate-new-buffer " *nskk-tooltip-timer-failure*"))
              caught)
          (unwind-protect (progn
              (cl-letf
                (((symbol-function 'run-with-timer)
                    (lambda (&rest _)
                      (signal 'quit nil))))
                (condition-case
                  condition
                  (with-current-buffer owner (nskk--show-mode-display-tooltip "[かな]"))
                  (quit
                    (setq caught condition))))
              (should (eq (car caught) 'quit))
              (should-not nskk--show-mode-tooltip-owner)
              (should-not nskk--show-mode-tooltip-timer)
              (with-current-buffer
                owner
                (should-not (memq #'nskk--show-mode-tooltip-owner-killed kill-buffer-hook)))
              (should (= nskk-show-mode-test--tooltip-hide-count 1)))
            (when (buffer-live-p owner)
              (kill-buffer owner))))))
    (nskk-it
      "releases global tooltip when inline cleanup errors"
      (nskk-show-mode-test-with-tooltip-runtime
        (let ((old-owner (generate-new-buffer " *nskk-tooltip-cleanup-old*"))
              (new-owner (generate-new-buffer " *nskk-tooltip-cleanup-new*"))
              old-timer
              inline-overlay
              inline-timer
              caught)
          (unwind-protect (progn
              (with-current-buffer old-owner (nskk--show-mode-display-tooltip "[かな]"))
              (setq old-timer nskk--show-mode-tooltip-timer)
              (with-current-buffer
                new-owner
                (setq inline-overlay (make-overlay (point) (point))
                      inline-timer (vector #'ignore nil nil)
                      nskk--show-mode-overlay inline-overlay
                      nskk--show-mode-timer inline-timer
                      nskk--show-mode-last-mode 'hiragana)
                (cl-letf
                  (((symbol-function 'delete-overlay)
                      (lambda (_overlay)
                        (error "inline cleanup failed"))))
                  (condition-case
                    condition
                    (nskk--show-mode-display-tooltip "[カナ]")
                    (error
                      (setq caught condition))))
                (should-not nskk--show-mode-overlay)
                (should-not nskk--show-mode-timer)
                (should-not nskk--show-mode-last-mode))
              (should (eq (car caught) 'error))
              (should (aref inline-timer 2))
              (should (aref old-timer 2))
              (should-not nskk--show-mode-tooltip-owner)
              (should-not nskk--show-mode-tooltip-timer)
              (with-current-buffer
                old-owner
                (should-not (memq #'nskk--show-mode-tooltip-owner-killed kill-buffer-hook)))
              (should (= nskk-show-mode-test--tooltip-hide-count 1)))
            (when (overlayp inline-overlay)
              (delete-overlay inline-overlay))
            (when (buffer-live-p old-owner)
              (kill-buffer old-owner))
            (when (buffer-live-p new-owner)
              (kill-buffer new-owner))))))
    (nskk-it
      "removes inline state when timer scheduling signals quit"
      (with-temp-buffer
        (let ((nskk-show-mode-duration 60)
              (nskk--show-mode-last-mode 'hiragana)
              (nskk--show-mode-tooltip-owner nil)
              (nskk--show-mode-tooltip-generation 0)
              (nskk--show-mode-tooltip-timer nil)
              caught)
          (cl-letf
            (((symbol-function 'run-with-timer)
                (lambda (&rest _)
                  (signal 'quit nil))))
            (condition-case
              condition
              (nskk--show-mode-display-inline "[かな]")
              (quit
                (setq caught condition))))
          (should (eq (car caught) 'quit))
          (should-not nskk--show-mode-overlay)
          (should-not nskk--show-mode-timer)
          (should-not nskk--show-mode-last-mode)
          (should-not (overlays-at (point))))))
    (nskk-it
      "fails closed when replacing an owner whose cancellation quits"
      (nskk-show-mode-test-with-tooltip-runtime
        (let ((old-owner (generate-new-buffer " *nskk-tooltip-cancel-failure-old*"))
              (new-owner (generate-new-buffer " *nskk-tooltip-cancel-failure-new*"))
              caught)
          (unwind-protect (progn
              (with-current-buffer old-owner (nskk--show-mode-display-tooltip "[かな]"))
              (let* ((old-timer nskk--show-mode-tooltip-timer)
                     (old-callback (aref old-timer 0))
                     (old-args (aref old-timer 1)))
                (cl-letf
                  (((symbol-function 'cancel-timer)
                      (lambda (timer)
                        (if (eq timer old-timer) (signal 'quit nil)
                          (aset timer 2 t)))))
                  (condition-case
                    condition
                    (with-current-buffer new-owner (nskk--show-mode-display-tooltip "[カナ]"))
                    (quit
                      (setq caught condition))))
                (should (eq (car caught) 'quit))
                (should-not nskk--show-mode-tooltip-owner)
                (should-not nskk--show-mode-tooltip-timer)
                (should-not (aref old-timer 2))
                (with-current-buffer
                  old-owner
                  (should-not (memq #'nskk--show-mode-tooltip-owner-killed kill-buffer-hook)))
                (with-current-buffer
                  new-owner
                  (should-not (memq #'nskk--show-mode-tooltip-owner-killed kill-buffer-hook)))
                (should (= nskk-show-mode-test--tooltip-hide-count 1))
                (apply old-callback old-args)
                (should (= nskk-show-mode-test--tooltip-hide-count 1))))
            (when (buffer-live-p old-owner)
              (kill-buffer old-owner))
            (when (buffer-live-p new-owner)
              (kill-buffer new-owner))))))))

;;;; Face Definitions

(nskk-describe
  "nskk-show-mode-inline-face"
  (nskk-it "is defined as a face" (should (facep 'nskk-show-mode-inline-face))))

;;;; Inline Timer Generations

(nskk-describe "nskk inline timer generations"
  (nskk-it "ignores an A callback after B replaces it and accepts B callback"
    (nskk-show-mode-test-with-tooltip-runtime
      (with-temp-buffer
        (let ((nskk-show-mode-duration 60)
              (nskk--show-mode-inline-generation 0))
          (nskk--show-mode-display-inline "[A]")
          (let* ((first-timer nskk--show-mode-timer)
                 (first-callback (aref first-timer 0))
                 (first-args (aref first-timer 1)))
            (nskk--show-mode-display-inline "[B]")
            (let ((second-overlay nskk--show-mode-overlay)
                  (second-timer nskk--show-mode-timer))
              (should (aref first-timer 2))
              (should (eq (aref second-timer 0)
                          #'nskk--show-mode-inline-timeout))
              (apply first-callback first-args)
              (should (eq nskk--show-mode-overlay second-overlay))
              (should (eq nskk--show-mode-timer second-timer))
              (should (equal (overlay-get second-overlay 'after-string)
                             "[B]"))
              (apply (aref second-timer 0) (aref second-timer 1))
              (should-not nskk--show-mode-overlay)
              (should-not nskk--show-mode-timer)))))))

  (nskk-it "manual clear invalidates its pending callback"
    (nskk-show-mode-test-with-tooltip-runtime
      (with-temp-buffer
        (let ((nskk-show-mode-duration 60)
              (nskk--show-mode-inline-generation 0))
          (nskk--show-mode-display-inline "[A]")
          (let* ((timer nskk--show-mode-timer)
                 (callback (aref timer 0))
                 (args (aref timer 1))
                 (generation nskk--show-mode-inline-generation))
            (nskk--show-mode-clear-inline)
            (setq nskk--show-mode-last-mode 'sentinel)
            (should (> nskk--show-mode-inline-generation generation))
            (apply callback args)
            (should (eq nskk--show-mode-last-mode 'sentinel))
            (should-not nskk--show-mode-overlay)
            (should-not nskk--show-mode-timer))))))

  (nskk-it "callback from a killed owner buffer is harmless"
    (nskk-show-mode-test-with-tooltip-runtime
      (let ((owner (generate-new-buffer " *nskk-inline-owner*"))
            timer)
        (unwind-protect
            (progn
              (with-current-buffer owner
                (setq nskk-show-mode-duration 60)
                (nskk--show-mode-display-inline "[A]")
                (setq timer nskk--show-mode-timer))
              (kill-buffer owner)
              (should-not
               (condition-case condition
                   (progn
                     (apply (aref timer 0) (aref timer 1))
                     nil)
                 ((error quit) condition))))
          (when (buffer-live-p owner)
            (kill-buffer owner)))))))

(provide 'nskk-show-mode-test)

;;; nskk-show-mode-test.el ends here
