;;; nskk-layer-presentation-test.el --- Tests for nskk-layer-presentation.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-layer-presentation.el covering:
;; - Module requirements (keymap, candidate-window, modeline)
;; - Feature provision
;; - Sub-module availability
;; - Candidate window show/hide/navigation behavior
;; - Modeline indicator behavior

;;; Code:

(require 'ert)
(require 'nskk-layer-presentation)
(require 'nskk-state)
(require 'nskk-test-framework)

;;;
;;; Sub-Module Availability Tests
;;;

(nskk-deftest-unit layer-pres-keymap-module-loaded
  "Test that nskk-keymap module is available after loading presentation layer."
  (should (featurep 'nskk-keymap)))

(nskk-deftest-unit layer-pres-candidate-window-module-loaded
  "Test that nskk-candidate-window module is available after loading presentation layer."
  (should (featurep 'nskk-candidate-window)))

(nskk-deftest-unit layer-pres-modeline-module-loaded
  "Test that nskk-modeline module is available after loading presentation layer."
  (should (featurep 'nskk-modeline)))

;;;
;;; Keymap Availability Tests
;;;

(nskk-deftest-unit layer-pres-nskk-mode-map-available
  "Test that nskk-mode-map is available via presentation layer."
  (should (boundp 'nskk-mode-map))
  (should (keymapp nskk-mode-map)))

(nskk-deftest-unit layer-pres-modeline-lighter-available
  "Test that modeline lighter variable is available."
  (should (boundp 'nskk-modeline-lighter)))

;;;
;;; Candidate Window Availability Tests
;;;

(nskk-deftest-unit layer-pres-candidate-window-functions-available
  "Test that candidate window functions are available."
  (should (fboundp 'nskk-candidate-window-show))
  (should (fboundp 'nskk-candidate-window-hide))
  (should (fboundp 'nskk-candidate-window-next))
  (should (fboundp 'nskk-candidate-window-prev)))

;;;
;;; Candidate Window Behavioral Tests
;;;

(nskk-deftest-unit layer-pres-candidate-window-show-creates-overlay
  "Test that showing candidates creates an overlay."
  (with-temp-buffer
    (let ((nskk--candidate-overlay nil)
          (nskk--candidate-index 0))
      (nskk-candidate-window-show '("candidate1" "candidate2") 0)
      (should (overlayp nskk--candidate-overlay))
      (nskk-candidate-window-hide))))

(nskk-deftest-unit layer-pres-candidate-window-hide-removes-overlay
  "Test that hiding candidates removes the overlay."
  (with-temp-buffer
    (let ((nskk--candidate-overlay nil)
          (nskk--candidate-index 0))
      (nskk-candidate-window-show '("candidate1") 0)
      (should (overlayp nskk--candidate-overlay))
      (nskk-candidate-window-hide)
      (should-not nskk--candidate-overlay))))

(nskk-deftest-unit layer-pres-candidate-window-hide-safe-when-nil
  "Test that hiding is safe when overlay is already nil."
  (let ((nskk--candidate-overlay nil))
    (nskk-candidate-window-hide)
    (should-not nskk--candidate-overlay)))

(nskk-deftest-unit layer-pres-candidate-window-next-increments-index
  "Test that next moves candidate index forward."
  (let ((nskk--candidate-index 0)
        (nskk-candidate-window-max-candidates 9))
    (should (nskk-candidate-window-next))
    (should (= nskk--candidate-index 1))))

(nskk-deftest-unit layer-pres-candidate-window-next-stops-at-max
  "Test that next stops at the max candidate boundary."
  (let ((nskk--candidate-index 8)
        (nskk-candidate-window-max-candidates 9))
    (should-not (nskk-candidate-window-next))
    (should (= nskk--candidate-index 8))))

(nskk-deftest-unit layer-pres-candidate-window-prev-decrements-index
  "Test that prev moves candidate index backward."
  (let ((nskk--candidate-index 2)
        (nskk-candidate-window-max-candidates 9))
    (should (nskk-candidate-window-prev))
    (should (= nskk--candidate-index 1))))

(nskk-deftest-unit layer-pres-candidate-window-prev-stops-at-zero
  "Test that prev stops at index 0."
  (let ((nskk--candidate-index 0)
        (nskk-candidate-window-max-candidates 9))
    (should-not (nskk-candidate-window-prev))
    (should (= nskk--candidate-index 0))))

(nskk-deftest-unit layer-pres-candidate-window-next-page
  "Test that next-page advances page counter."
  (let ((nskk--candidate-page 0)
        (nskk--candidate-index 3)
        (nskk-candidate-window-max-candidates 9))
    (should (nskk-candidate-window-next-page 20))
    (should (= nskk--candidate-page 1))
    (should (= nskk--candidate-index 0))))

(nskk-deftest-unit layer-pres-candidate-window-next-page-stops-at-last
  "Test that next-page does not go beyond last page."
  (let ((nskk--candidate-page 1)
        (nskk--candidate-index 0)
        (nskk-candidate-window-max-candidates 9))
    (should-not (nskk-candidate-window-next-page 10))
    (should (= nskk--candidate-page 1))))

(nskk-deftest-unit layer-pres-candidate-window-prev-page
  "Test that prev-page goes back one page."
  (let ((nskk--candidate-page 1)
        (nskk--candidate-index 5)
        (nskk-candidate-window-max-candidates 9))
    (should (nskk-candidate-window-prev-page))
    (should (= nskk--candidate-page 0))
    (should (= nskk--candidate-index 0))))

(nskk-deftest-unit layer-pres-candidate-window-prev-page-stops-at-first
  "Test that prev-page does not go below page 0."
  (let ((nskk--candidate-page 0)
        (nskk--candidate-index 0)
        (nskk-candidate-window-max-candidates 9))
    (should-not (nskk-candidate-window-prev-page))
    (should (= nskk--candidate-page 0))))

;;;
;;; Modeline Indicator Behavioral Tests (via presentation layer)
;;;

(nskk-deftest-unit layer-pres-modeline-indicator-for-hiragana
  "Test modeline indicator returns correct string for hiragana mode."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (let ((indicator (nskk-modeline-indicator)))
      (should (stringp indicator))
      (should (equal (substring-no-properties indicator) "\u3042")))))

(nskk-deftest-unit layer-pres-modeline-indicator-for-katakana
  "Test modeline indicator returns correct string for katakana mode."
  (let ((nskk-current-state (nskk-state-create 'katakana)))
    (let ((indicator (nskk-modeline-indicator)))
      (should (stringp indicator))
      (should (equal (substring-no-properties indicator) "\u30A2")))))

(nskk-deftest-unit layer-pres-modeline-lighter-is-eval-form
  "Test modeline lighter is an :eval form referencing nskk-modeline-indicator."
  (should (listp nskk-modeline-lighter))
  (should (eq (car nskk-modeline-lighter) :eval)))

;;;
;;; Module Feature Tests
;;;

(nskk-deftest-unit layer-pres-provides-feature
  "Test that nskk-layer-presentation provides its feature."
  (should (featurep 'nskk-layer-presentation)))

(provide 'nskk-layer-presentation-test)

;;; nskk-layer-presentation-test.el ends here
