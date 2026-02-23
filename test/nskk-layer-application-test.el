;;; nskk-layer-application-test.el --- Tests for nskk-layer-application.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-layer-application.el covering:
;; - Application initialization
;; - Mode management API (existence and behavior)
;; - Input processing API (existence and behavior)
;; - Conversion control API (existence and behavior)
;; - Query functions (existence and behavior)
;; - Error cases

;;; Code:

(require 'ert)
(require 'nskk-layer-application)
(require 'nskk-state)
(require 'nskk-test-framework)

;;;
;;; Helper Macros
;;;

(defmacro nskk-layer-app-test-with-state (initial-mode &rest body)
  "Execute BODY with a fresh state initialized to INITIAL-MODE."
  (declare (indent 1))
  `(let ((nskk-current-state (nskk-state-create ,initial-mode))
         (nskk-converting-active nil))
     ,@body))

;;;
;;; Initialization Tests (existence)
;;;

(nskk-deftest-unit layer-app-initialize-function-exists
  "Test that nskk-initialize function exists."
  (should (fboundp 'nskk-initialize)))

(nskk-deftest-unit layer-app-converting-active-var-exists
  "Test that nskk-converting-active variable exists."
  (should (boundp 'nskk-converting-active)))

;;;
;;; Initialization Behavioral Tests
;;;

(nskk-deftest-unit layer-app-initialize-creates-state
  "Test that nskk-initialize creates a valid state object."
  (let ((nskk-current-state nil)
        (nskk-converting-active t))
    (nskk-initialize)
    (should (nskk-state-p nskk-current-state))
    (should-not nskk-converting-active)))

(nskk-deftest-unit layer-app-initialize-sets-default-mode
  "Test that nskk-initialize sets the default mode from custom."
  (let ((nskk-current-state nil)
        (nskk-converting-active nil)
        (nskk-state-default-mode 'ascii))
    (nskk-initialize)
    (should (eq (nskk-state-mode nskk-current-state) 'ascii))))

(nskk-deftest-unit layer-app-initialize-clears-converting-active
  "Test that nskk-initialize clears the converting-active flag."
  (let ((nskk-current-state nil)
        (nskk-converting-active t))
    (nskk-initialize)
    (should-not nskk-converting-active)))

;;;
;;; Mode Management API Tests (existence)
;;;

(nskk-deftest-unit layer-app-enter-hiragana-exists
  "Test that enter-hiragana-mode function exists."
  (should (fboundp 'nskk-enter-hiragana-mode)))

(nskk-deftest-unit layer-app-enter-katakana-exists
  "Test that enter-katakana-mode function exists."
  (should (fboundp 'nskk-enter-katakana-mode)))

(nskk-deftest-unit layer-app-enter-latin-exists
  "Test that enter-latin-mode function exists."
  (should (fboundp 'nskk-enter-latin-mode)))

(nskk-deftest-unit layer-app-enter-abbrev-exists
  "Test that enter-abbrev-mode function exists."
  (should (fboundp 'nskk-enter-abbrev-mode)))

(nskk-deftest-unit layer-app-toggle-japanese-exists
  "Test that toggle-japanese-mode function exists."
  (should (fboundp 'nskk-toggle-japanese-mode)))

;;;
;;; Mode Management Behavioral Tests
;;;

(nskk-deftest-unit layer-app-enter-hiragana-changes-mode
  "Test that enter-hiragana-mode actually switches to hiragana."
  (nskk-layer-app-test-with-state 'ascii
    (nskk-enter-hiragana-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(nskk-deftest-unit layer-app-enter-katakana-changes-mode
  "Test that enter-katakana-mode actually switches to katakana."
  (nskk-layer-app-test-with-state 'ascii
    (nskk-enter-katakana-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(nskk-deftest-unit layer-app-enter-latin-changes-mode
  "Test that enter-latin-mode actually switches to latin."
  (nskk-layer-app-test-with-state 'hiragana
    (nskk-enter-latin-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))))

(nskk-deftest-unit layer-app-enter-abbrev-changes-mode
  "Test that enter-abbrev-mode actually switches to abbrev."
  (nskk-layer-app-test-with-state 'hiragana
    (nskk-enter-abbrev-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'abbrev))))

(nskk-deftest-unit layer-app-toggle-hiragana-to-katakana
  "Test toggle switches hiragana to katakana."
  (nskk-layer-app-test-with-state 'hiragana
    (nskk-toggle-japanese-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(nskk-deftest-unit layer-app-toggle-katakana-to-hiragana
  "Test toggle switches katakana to hiragana."
  (nskk-layer-app-test-with-state 'katakana
    (nskk-toggle-japanese-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(nskk-deftest-unit layer-app-toggle-from-latin-no-change
  "Test toggle does nothing when in latin mode."
  (nskk-layer-app-test-with-state 'latin
    (nskk-toggle-japanese-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))))

(nskk-deftest-unit layer-app-mode-roundtrip
  "Test switching through all modes and back."
  (nskk-layer-app-test-with-state 'ascii
    (nskk-enter-hiragana-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
    (nskk-enter-katakana-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))
    (nskk-enter-abbrev-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'abbrev))
    (nskk-enter-latin-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))
    (nskk-enter-hiragana-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

;;;
;;; Input Processing API Tests (existence)
;;;

(nskk-deftest-unit layer-app-process-input-exists
  "Test that process-input function exists."
  (should (fboundp 'nskk-process-input)))

;;;
;;; Input Processing Behavioral Tests
;;;

(nskk-deftest-unit layer-app-latin-mode-inserts-char
  "Test that in latin mode, input inserts character directly."
  (nskk-layer-app-test-with-state 'latin
    (with-temp-buffer
      (let ((last-command-event ?a)
            (nskk--romaji-buffer ""))
        (nskk-self-insert 1)
        (should (equal (buffer-string) "a"))))))

(nskk-deftest-unit layer-app-latin-mode-inserts-multiple
  "Test that latin mode inserts character N times."
  (nskk-layer-app-test-with-state 'latin
    (with-temp-buffer
      (let ((last-command-event ?x)
            (nskk--romaji-buffer ""))
        (nskk-self-insert 3)
        (should (equal (buffer-string) "xxx"))))))

(nskk-deftest-unit layer-app-hiragana-mode-converts-vowel
  "Test that in hiragana mode, a vowel is converted to kana."
  (nskk-layer-app-test-with-state 'hiragana
    (with-temp-buffer
      (let ((last-command-event ?a)
            (nskk--romaji-buffer ""))
        (nskk-self-insert 1)
        (should (equal (buffer-string) "\u3042"))))))

(nskk-deftest-unit layer-app-hiragana-mode-converts-consonant-vowel
  "Test that in hiragana mode, consonant+vowel produces kana."
  (nskk-layer-app-test-with-state 'hiragana
    (with-temp-buffer
      (let ((nskk--romaji-buffer ""))
        (let ((last-command-event ?k))
          (nskk-self-insert 1))
        ;; After 'k' alone, buffer may be empty (incomplete)
        (let ((last-command-event ?a))
          (nskk-self-insert 1))
        (should (equal (buffer-string) "\u304B"))))))

(nskk-deftest-unit layer-app-katakana-mode-converts-to-katakana
  "Test that in katakana mode, input produces katakana."
  (nskk-layer-app-test-with-state 'katakana
    (with-temp-buffer
      (let ((last-command-event ?a)
            (nskk--romaji-buffer ""))
        (nskk-self-insert 1)
        (should (equal (buffer-string) "\u30A2"))))))

;;;
;;; Conversion Control API Tests (existence)
;;;

(nskk-deftest-unit layer-app-start-convert-exists
  "Test that start-convert function exists."
  (should (fboundp 'nskk-start-convert)))

(nskk-deftest-unit layer-app-commit-exists
  "Test that commit function exists."
  (should (fboundp 'nskk-commit)))

(nskk-deftest-unit layer-app-convert-or-commit-exists
  "Test that convert-or-commit-selection function exists."
  (should (fboundp 'nskk-convert-or-commit-selection)))

(nskk-deftest-unit layer-app-cancel-exists
  "Test that cancel function exists."
  (should (fboundp 'nskk-cancel)))

(nskk-deftest-unit layer-app-next-exists
  "Test that next function exists."
  (should (fboundp 'nskk-next)))

(nskk-deftest-unit layer-app-previous-exists
  "Test that previous function exists."
  (should (fboundp 'nskk-previous)))

;;;
;;; Conversion Control Behavioral Tests
;;;

(nskk-deftest-unit layer-app-commit-clears-conversion-state
  "Test that nskk-commit clears converting-active when in conversion."
  (with-temp-buffer
    (nskk-layer-app-test-with-state 'hiragana
      (insert "test")
      (push-mark (point-min) t)
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (setq nskk-converting-active t)
      (nskk-commit)
      (should-not nskk-converting-active))))

(nskk-deftest-unit layer-app-commit-replaces-buffer-content
  "Test that nskk-commit replaces preedit with selected candidate."
  (with-temp-buffer
    (nskk-layer-app-test-with-state 'hiragana
      (insert "preedit")
      (push-mark (point-min) t)
      (setf (nskk-state-candidates nskk-current-state) '("committed"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (setq nskk-converting-active t)
      (nskk-commit)
      (should (equal (buffer-string) "committed")))))

(nskk-deftest-unit layer-app-commit-noop-when-not-converting
  "Test that nskk-commit is a no-op when not in conversion."
  (with-temp-buffer
    (nskk-layer-app-test-with-state 'hiragana
      (insert "text")
      (setq nskk-converting-active nil)
      (nskk-commit)
      (should (equal (buffer-string) "text"))
      (should-not nskk-converting-active))))

(nskk-deftest-unit layer-app-cancel-clears-conversion
  "Test that nskk-cancel deactivates conversion state."
  (nskk-layer-app-test-with-state 'hiragana
    (setq nskk-converting-active t)
    (nskk-cancel)
    (should-not nskk-converting-active)))

(nskk-deftest-unit layer-app-cancel-noop-when-not-converting
  "Test that nskk-cancel is safe when not in conversion state."
  (nskk-layer-app-test-with-state 'hiragana
    (setq nskk-converting-active nil)
    (nskk-cancel)
    (should-not nskk-converting-active)))

(nskk-deftest-unit layer-app-convert-or-commit-commits-when-converting
  "Test convert-or-commit commits when already in conversion."
  (with-temp-buffer
    (nskk-layer-app-test-with-state 'hiragana
      (insert "preedit")
      (push-mark (point-min) t)
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (setq nskk-converting-active t)
      (nskk-convert-or-commit-selection)
      (should-not nskk-converting-active))))

;;;
;;; Query Function Tests (existence)
;;;

(nskk-deftest-unit layer-app-in-conversion-p-exists
  "Test that in-conversion-p function exists."
  (should (fboundp 'nskk-in-conversion-p)))

(nskk-deftest-unit layer-app-current-mode-exists
  "Test that current-mode function exists."
  (should (fboundp 'nskk-current-mode)))

;;;
;;; Query Function Behavioral Tests
;;;

(nskk-deftest-unit layer-app-in-conversion-p-false-initially
  "Test that in-conversion-p returns nil when not converting."
  (nskk-layer-app-test-with-state 'hiragana
    (should-not (nskk-in-conversion-p))))

(nskk-deftest-unit layer-app-in-conversion-p-true-when-active
  "Test that in-conversion-p returns non-nil when converting."
  (nskk-layer-app-test-with-state 'hiragana
    (setq nskk-converting-active t)
    (should (nskk-in-conversion-p))))

(nskk-deftest-unit layer-app-current-mode-returns-mode
  "Test that current-mode returns the current mode symbol."
  (nskk-layer-app-test-with-state 'hiragana
    (should (eq (nskk-current-mode) 'hiragana))))

(nskk-deftest-unit layer-app-current-mode-after-switch
  "Test that current-mode reflects mode changes."
  (nskk-layer-app-test-with-state 'ascii
    (nskk-enter-katakana-mode)
    (should (eq (nskk-current-mode) 'katakana))))

(nskk-deftest-unit layer-app-current-mode-nil-without-state
  "Test that current-mode returns nil when no state exists."
  (let ((nskk-current-state nil))
    (should-not (nskk-current-mode))))

;;;
;;; All Functions are Interactive Tests
;;;

(nskk-deftest-unit layer-app-mode-functions-interactive
  "Test that mode management functions are interactive."
  (should (commandp 'nskk-enter-hiragana-mode))
  (should (commandp 'nskk-enter-katakana-mode))
  (should (commandp 'nskk-enter-latin-mode))
  (should (commandp 'nskk-enter-abbrev-mode))
  (should (commandp 'nskk-toggle-japanese-mode)))

(nskk-deftest-unit layer-app-conversion-functions-interactive
  "Test that conversion control functions are interactive."
  (should (commandp 'nskk-start-convert))
  (should (commandp 'nskk-commit))
  (should (commandp 'nskk-convert-or-commit-selection))
  (should (commandp 'nskk-cancel))
  (should (commandp 'nskk-next))
  (should (commandp 'nskk-previous)))

;;;
;;; Module Feature Tests
;;;

(nskk-deftest-unit layer-app-provides-feature
  "Test that nskk-layer-application provides its feature."
  (should (featurep 'nskk-layer-application)))

(provide 'nskk-layer-application-test)

;;; nskk-layer-application-test.el ends here
