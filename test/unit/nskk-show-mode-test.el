;;; nskk-show-mode-test.el --- Tests for nskk-show-mode.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-show-mode.el covering:
;; - Function existence (fboundp)
;; - Customization variables
;; - nskk--show-mode-indicator-string for each mode
;; - nskk--show-mode-hide cleanup behavior
;; - nskk-show-mode-display guard conditions

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-show-mode)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Function Existence

(nskk-describe "nskk-show-mode function existence"
  (nskk-it "nskk-show-mode-display is defined"
    (should (fboundp 'nskk-show-mode-display)))
  (nskk-it "nskk--show-mode-indicator-string is defined"
    (should (fboundp 'nskk--show-mode-indicator-string)))
  (nskk-it "nskk--show-mode-hide is defined"
    (should (fboundp 'nskk--show-mode-hide))))

;;;; Customization

(nskk-describe "nskk-show-mode customization variables"
  (nskk-it "nskk-show-mode-show is defined as a boolean defcustom"
    (should (boundp 'nskk-show-mode-show)))
  (nskk-it "nskk-show-mode-style is defined"
    (should (boundp 'nskk-show-mode-style)))
  (nskk-it "nskk-show-mode-duration is a number"
    (should (numberp nskk-show-mode-duration)))
  (nskk-it "nskk-show-mode-style is one of valid choices"
    (should (memq nskk-show-mode-style '(inline tooltip)))))

;;;; Indicator String

(nskk-describe "nskk--show-mode-indicator-string"
  (nskk-it "returns nil for unknown mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (should (null (nskk--show-mode-indicator-string 'nonexistent-mode)))))

  (nskk-it "returns a string for hiragana mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((result (nskk--show-mode-indicator-string 'hiragana)))
        (should (stringp result)))))

  (nskk-it "returns a string for katakana mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((result (nskk--show-mode-indicator-string 'katakana)))
        (should (stringp result)))))

  (nskk-it "result is bracket-wrapped"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((result (nskk--show-mode-indicator-string 'hiragana)))
        (when result
          (should (string-prefix-p "[" result))
          (should (string-suffix-p "]" result))))))

  (nskk-it "returns a string for ascii mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((result (nskk--show-mode-indicator-string 'ascii)))
        (should (stringp result)))))

  (nskk-it "returns a string for jisx0208-latin mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((result (nskk--show-mode-indicator-string 'jisx0208-latin)))
        (should (stringp result)))))

  (nskk-it "returns a string for abbrev mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((result (nskk--show-mode-indicator-string 'abbrev)))
        (should (stringp result)))))

  (nskk-it "result has face property applied"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((result (nskk--show-mode-indicator-string 'hiragana)))
        (when result
          (should (eq (get-text-property 0 'face result)
                      'nskk-show-mode-inline-face)))))))

;;;; Display Guards

(nskk-describe "nskk-show-mode-display no-op conditions"
  (nskk-it "is a no-op when nskk-show-mode-show is nil"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-show-mode-show nil))
          ;; Should not error
          (should-not (condition-case err
                          (progn (nskk-show-mode-display) nil)
                        (error err)))))))

  (nskk-it "is a no-op when nskk-current-state is nil"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (setq nskk-current-state nil)
        (let ((nskk-show-mode-show t))
          (should-not (condition-case err
                          (progn (nskk-show-mode-display) nil)
                        (error err))))))))

;;;; Hide Cleanup

(nskk-describe "nskk--show-mode-hide"
  (nskk-it "is safe to call when overlay is nil"
    (with-temp-buffer
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil)
            (nskk--show-mode-last-mode nil))
        ;; Should not signal an error
        (should-not (condition-case err
                        (progn (nskk--show-mode-hide) nil)
                      (error err))))))

  (nskk-it "clears last-mode after hide"
    (with-temp-buffer
      (setq nskk--show-mode-last-mode 'hiragana)
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil))
        (nskk--show-mode-hide)
        (should (null nskk--show-mode-last-mode))))))

;;;; Exact Indicator String Content

(nskk-deftest-table show-mode-indicator-exact-strings
  :columns (mode expected-content)
  :rows ((hiragana       "かな")
         (katakana       "カナ")
         (ascii          "SKK")
         (jisx0208-latin "全英")
         (abbrev         "aA"))
  :description "nskk--show-mode-indicator-string returns exact [DISPLAY] bracket string"
  :body
  (nskk-prolog-test-with-isolated-db
    (nskk-state-initialize-prolog)
    (should (equal (nskk--show-mode-indicator-string mode)
                   (propertize (format "[%s]" expected-content)
                               'face 'nskk-show-mode-inline-face)))))

;;;; nskk--show-mode-display-inline

(nskk-describe "nskk--show-mode-display-inline"
  (nskk-it "creates an overlay in the current buffer"
    (with-temp-buffer
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil)
            (nskk-show-mode-duration 60))
        (nskk--show-mode-display-inline "[か]")
        (unwind-protect
            (should (overlayp nskk--show-mode-overlay))
          (when (timerp nskk--show-mode-timer)
            (cancel-timer nskk--show-mode-timer))))))

  (nskk-it "sets after-string property to the indicator string"
    (with-temp-buffer
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil)
            (nskk-show-mode-duration 60))
        (nskk--show-mode-display-inline "[か]")
        (unwind-protect
            (should (equal (overlay-get nskk--show-mode-overlay 'after-string)
                           "[か]"))
          (when (timerp nskk--show-mode-timer)
            (cancel-timer nskk--show-mode-timer))))))

  (nskk-it "schedules a timer for auto-hide"
    (with-temp-buffer
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil)
            (nskk-show-mode-duration 60))
        (nskk--show-mode-display-inline "[か]")
        (unwind-protect
            (should (timerp nskk--show-mode-timer))
          (when (timerp nskk--show-mode-timer)
            (cancel-timer nskk--show-mode-timer))))))

  (nskk-it "cancels and replaces previous timer on second call"
    (with-temp-buffer
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil)
            (nskk-show-mode-duration 60)
            first-timer)
        (nskk--show-mode-display-inline "[か]")
        (setq first-timer nskk--show-mode-timer)
        (nskk--show-mode-display-inline "[ア]")
        (unwind-protect
            (should (not (eq first-timer nskk--show-mode-timer)))
          (when (timerp nskk--show-mode-timer)
            (cancel-timer nskk--show-mode-timer))))))

  (nskk-it "overlay priority is 100"
    (with-temp-buffer
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil)
            (nskk-show-mode-duration 60))
        (nskk--show-mode-display-inline "[か]")
        (unwind-protect
            (should (= (overlay-get nskk--show-mode-overlay 'priority) 100))
          (when (timerp nskk--show-mode-timer)
            (cancel-timer nskk--show-mode-timer)))))))

;;;; nskk--show-mode-hide cancels timer

(nskk-describe "nskk--show-mode-hide timer handling"
  (nskk-it "cancels pending timer when one exists"
    (with-temp-buffer
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil)
            (nskk--show-mode-last-mode nil)
            (nskk-show-mode-duration 60))
        (nskk--show-mode-display-inline "[か]")
        ;; Timer was created
        (should (timerp nskk--show-mode-timer))
        (nskk--show-mode-hide)
        ;; Timer was cancelled and cleared
        (should (null nskk--show-mode-timer)))))

  (nskk-it "removes the overlay when one exists"
    (with-temp-buffer
      (let ((nskk--show-mode-overlay nil)
            (nskk--show-mode-timer nil)
            (nskk--show-mode-last-mode nil)
            (nskk-show-mode-duration 60))
        (nskk--show-mode-display-inline "[か]")
        (should (overlayp nskk--show-mode-overlay))
        (nskk--show-mode-hide)
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
              (nskk--show-mode-last-mode 'hiragana)  ; pre-set: same as current
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
              (nskk--show-mode-last-mode 'hiragana)  ; different from current
              (call-count 0))
          (cl-letf (((symbol-function 'nskk--show-mode-display-inline)
                     (lambda (_s) (cl-incf call-count))))
            (nskk-show-mode-display)
            (should (= call-count 1))))))))

;;;; Face Definitions

(nskk-describe "nskk-show-mode-inline-face"
  (nskk-it "is defined as a face"
    (should (facep 'nskk-show-mode-inline-face))))

(provide 'nskk-show-mode-test)

;;; nskk-show-mode-test.el ends here
