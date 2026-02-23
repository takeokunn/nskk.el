;;; nskk-modeline-test.el --- Modeline indicator tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-modeline.el covering:
;; - Mode string generation for each mode
;; - Face definitions for each mode
;; - Help-echo text for each mode
;; - Modeline indicator composition
;; - Modeline lighter variable
;; - Unknown mode fallback behavior

;;; Code:

(require 'ert)
(require 'nskk-modeline)
(require 'nskk-state)
(require 'nskk-test-framework)

;;;
;;; Mode String Tests
;;;

(nskk-deftest-unit modeline-mode-string-hiragana
  "Test mode string for hiragana mode."
  (should (equal (nskk-modeline--mode-string 'hiragana) "\u3042")))

(nskk-deftest-unit modeline-mode-string-katakana
  "Test mode string for katakana mode."
  (should (equal (nskk-modeline--mode-string 'katakana) "\u30A2")))

(nskk-deftest-unit modeline-mode-string-abbrev
  "Test mode string for abbrev mode."
  (should (equal (nskk-modeline--mode-string 'abbrev) "\uFF1Aa")))

(nskk-deftest-unit modeline-mode-string-direct
  "Test mode string for direct/ASCII mode."
  (should (equal (nskk-modeline--mode-string 'direct) "\u82F1")))

(nskk-deftest-unit modeline-mode-string-unknown
  "Test mode string for unknown mode defaults to NSKK."
  (should (equal (nskk-modeline--mode-string 'unknown-mode) "NSKK"))
  (should (equal (nskk-modeline--mode-string nil) "NSKK")))

(nskk-deftest-unit modeline-mode-string-all-modes
  "Test mode strings cover all expected modes."
  (let ((mode-strings '((hiragana . "\u3042")
                        (katakana . "\u30A2")
                        (abbrev . "\uFF1Aa")
                        (direct . "\u82F1"))))
    (dolist (entry mode-strings)
      (should (equal (nskk-modeline--mode-string (car entry))
                     (cdr entry))))))

;;;
;;; Mode Face Tests
;;;

(nskk-deftest-unit modeline-mode-face-hiragana
  "Test face for hiragana mode."
  (should (eq (nskk-modeline--mode-face 'hiragana)
              'nskk-modeline-hiragana-face)))

(nskk-deftest-unit modeline-mode-face-katakana
  "Test face for katakana mode."
  (should (eq (nskk-modeline--mode-face 'katakana)
              'nskk-modeline-katakana-face)))

(nskk-deftest-unit modeline-mode-face-abbrev
  "Test face for abbrev mode."
  (should (eq (nskk-modeline--mode-face 'abbrev)
              'nskk-modeline-abbrev-face)))

(nskk-deftest-unit modeline-mode-face-direct
  "Test face for direct/ASCII mode."
  (should (eq (nskk-modeline--mode-face 'direct)
              'nskk-modeline-direct-face)))

(nskk-deftest-unit modeline-mode-face-unknown
  "Test face for unknown mode defaults to default face."
  (should (eq (nskk-modeline--mode-face 'unknown-mode) 'default))
  (should (eq (nskk-modeline--mode-face nil) 'default)))

;;;
;;; Mode Help Text Tests
;;;

(nskk-deftest-unit modeline-mode-help-hiragana
  "Test help text for hiragana mode."
  (should (equal (nskk-modeline--mode-help 'hiragana)
                 "Hiragana input mode")))

(nskk-deftest-unit modeline-mode-help-katakana
  "Test help text for katakana mode."
  (should (equal (nskk-modeline--mode-help 'katakana)
                 "Katakana input mode")))

(nskk-deftest-unit modeline-mode-help-abbrev
  "Test help text for abbrev mode."
  (should (equal (nskk-modeline--mode-help 'abbrev)
                 "Abbreviation mode")))

(nskk-deftest-unit modeline-mode-help-direct
  "Test help text for direct/ASCII mode."
  (should (equal (nskk-modeline--mode-help 'direct)
                 "Direct/ASCII input mode")))

(nskk-deftest-unit modeline-mode-help-unknown
  "Test help text for unknown mode."
  (should (equal (nskk-modeline--mode-help 'unknown-mode)
                 "NSKK input method"))
  (should (equal (nskk-modeline--mode-help nil)
                 "NSKK input method")))

;;;
;;; Face Definition Tests
;;;

(nskk-deftest-unit modeline-face-hiragana-defined
  "Test hiragana face is defined."
  (should (facep 'nskk-modeline-hiragana-face)))

(nskk-deftest-unit modeline-face-katakana-defined
  "Test katakana face is defined."
  (should (facep 'nskk-modeline-katakana-face)))

(nskk-deftest-unit modeline-face-abbrev-defined
  "Test abbrev face is defined."
  (should (facep 'nskk-modeline-abbrev-face)))

(nskk-deftest-unit modeline-face-direct-defined
  "Test direct face is defined."
  (should (facep 'nskk-modeline-direct-face)))

(nskk-deftest-unit modeline-face-all-bold
  "Test all mode faces use bold weight."
  (dolist (face '(nskk-modeline-hiragana-face
                  nskk-modeline-katakana-face
                  nskk-modeline-abbrev-face
                  nskk-modeline-direct-face))
    (let ((spec (face-attribute face :weight nil 'default)))
      (should (eq spec 'bold)))))

;;;
;;; Modeline Indicator Composition Tests
;;;

(nskk-deftest-unit modeline-indicator-returns-string
  "Test that modeline indicator returns a propertized string."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (let ((indicator (nskk-modeline-indicator)))
      (should (stringp indicator))
      (should (> (length indicator) 0)))))

(nskk-deftest-unit modeline-indicator-has-face-property
  "Test that modeline indicator has face text property."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (let ((indicator (nskk-modeline-indicator)))
      (should (get-text-property 0 'face indicator)))))

(nskk-deftest-unit modeline-indicator-has-help-echo
  "Test that modeline indicator has help-echo text property."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (let ((indicator (nskk-modeline-indicator)))
      (should (get-text-property 0 'help-echo indicator)))))

(nskk-deftest-unit modeline-indicator-hiragana-mode
  "Test modeline indicator string for hiragana mode."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (let ((indicator (nskk-modeline-indicator)))
      (should (equal (substring-no-properties indicator) "\u3042"))
      (should (eq (get-text-property 0 'face indicator)
                  'nskk-modeline-hiragana-face))
      (should (equal (get-text-property 0 'help-echo indicator)
                     "Hiragana input mode")))))

(nskk-deftest-unit modeline-indicator-katakana-mode
  "Test modeline indicator string for katakana mode."
  (let ((nskk-current-state (nskk-state-create 'katakana)))
    (let ((indicator (nskk-modeline-indicator)))
      (should (equal (substring-no-properties indicator) "\u30A2"))
      (should (eq (get-text-property 0 'face indicator)
                  'nskk-modeline-katakana-face))
      (should (equal (get-text-property 0 'help-echo indicator)
                     "Katakana input mode")))))

;;;
;;; Lighter Variable Tests
;;;

(nskk-deftest-unit modeline-lighter-defined
  "Test that nskk-modeline-lighter is defined."
  (should (boundp 'nskk-modeline-lighter)))

(nskk-deftest-unit modeline-lighter-eval-form
  "Test that nskk-modeline-lighter is an :eval form."
  (should (listp nskk-modeline-lighter))
  (should (eq (car nskk-modeline-lighter) :eval)))

;;;
;;; Consistency Tests
;;;

(nskk-deftest-unit modeline-face-string-help-consistency
  "Test that face, string, and help are consistent for each mode."
  (dolist (mode '(hiragana katakana abbrev direct))
    (let ((str (nskk-modeline--mode-string mode))
          (face (nskk-modeline--mode-face mode))
          (help (nskk-modeline--mode-help mode)))
      ;; Each mode has a non-empty string
      (should (> (length str) 0))
      ;; Each mode has a dedicated face (not 'default)
      (should (not (eq face 'default)))
      ;; Each mode has a non-empty help string
      (should (> (length help) 0)))))

(provide 'nskk-modeline-test)

;;; nskk-modeline-test.el ends here
