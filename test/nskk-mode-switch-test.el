;;; nskk-mode-switch-test.el --- Mode switching tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-mode-switch.el covering:
;; - Mode switching functions (hiragana, katakana, latin, abbrev)
;; - Mode validation
;; - Toggle katakana/hiragana
;; - Conversion state management
;; - Internal mode setter with validation
;; - Modeline update trigger

;;; Code:

(require 'ert)
(require 'nskk-mode-switch)
(require 'nskk-state)
(require 'nskk-test-framework)

;;;
;;; Helper Macros
;;;

(defmacro nskk-mode-switch-test-with-state (initial-mode &rest body)
  "Execute BODY with a fresh state initialized to INITIAL-MODE."
  (declare (indent 1))
  `(let ((nskk-current-state (nskk-state-create ,initial-mode))
         (nskk-converting-active nil))
     ,@body))

;;;
;;; Internal Mode Setter Tests
;;;

(nskk-deftest-unit mode-switch-set-mode-hiragana
  "Test internal set mode to hiragana."
  (nskk-mode-switch-test-with-state 'ascii
    (nskk--set-mode 'hiragana)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(nskk-deftest-unit mode-switch-set-mode-katakana
  "Test internal set mode to katakana."
  (nskk-mode-switch-test-with-state 'ascii
    (nskk--set-mode 'katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(nskk-deftest-unit mode-switch-set-mode-latin
  "Test internal set mode to latin."
  (nskk-mode-switch-test-with-state 'hiragana
    (nskk--set-mode 'latin)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))))

(nskk-deftest-unit mode-switch-set-mode-abbrev
  "Test internal set mode to abbrev."
  (nskk-mode-switch-test-with-state 'hiragana
    (nskk--set-mode 'abbrev)
    (should (eq (nskk-state-mode nskk-current-state) 'abbrev))))

(nskk-deftest-unit mode-switch-set-mode-invalid
  "Test internal set mode with invalid mode signals error."
  (nskk-mode-switch-test-with-state 'ascii
    (should-error (nskk--set-mode 'invalid-mode))))

(nskk-deftest-unit mode-switch-set-mode-nil
  "Test internal set mode with nil signals error."
  (nskk-mode-switch-test-with-state 'ascii
    (should-error (nskk--set-mode nil))))

;;;
;;; Public Mode Switching Function Tests
;;;

(nskk-deftest-unit mode-switch-to-hiragana
  "Test switching to hiragana mode."
  (nskk-mode-switch-test-with-state 'ascii
    (nskk-set-mode-hiragana)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(nskk-deftest-unit mode-switch-to-katakana
  "Test switching to katakana mode."
  (nskk-mode-switch-test-with-state 'ascii
    (nskk-set-mode-katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(nskk-deftest-unit mode-switch-to-latin
  "Test switching to latin mode."
  (nskk-mode-switch-test-with-state 'hiragana
    (nskk-set-mode-latin)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))))

(nskk-deftest-unit mode-switch-to-abbrev
  "Test switching to abbrev mode."
  (nskk-mode-switch-test-with-state 'hiragana
    (nskk-set-mode-abbrev)
    (should (eq (nskk-state-mode nskk-current-state) 'abbrev))))

;;;
;;; Toggle Katakana Tests
;;;

(nskk-deftest-unit mode-switch-toggle-hiragana-to-katakana
  "Test toggling from hiragana to katakana."
  (nskk-mode-switch-test-with-state 'hiragana
    (nskk-toggle-katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(nskk-deftest-unit mode-switch-toggle-katakana-to-hiragana
  "Test toggling from katakana to hiragana."
  (nskk-mode-switch-test-with-state 'katakana
    (nskk-toggle-katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(nskk-deftest-unit mode-switch-toggle-from-latin-no-change
  "Test toggling from latin does nothing."
  (nskk-mode-switch-test-with-state 'latin
    (nskk-toggle-katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))))

(nskk-deftest-unit mode-switch-toggle-from-ascii-no-change
  "Test toggling from ascii does nothing."
  (nskk-mode-switch-test-with-state 'ascii
    (nskk-toggle-katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'ascii))))

(nskk-deftest-unit mode-switch-toggle-roundtrip
  "Test toggling twice returns to original mode."
  (nskk-mode-switch-test-with-state 'hiragana
    (nskk-toggle-katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))
    (nskk-toggle-katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

;;;
;;; Conversion State Tests
;;;

(nskk-deftest-unit mode-switch-converting-p-false-initially
  "Test converting-p returns nil initially."
  (let ((nskk-converting-active nil))
    (should-not (nskk-converting-p))))

(nskk-deftest-unit mode-switch-converting-p-true-when-active
  "Test converting-p returns non-nil when active."
  (let ((nskk-converting-active t))
    (should (nskk-converting-p))))

(nskk-deftest-unit mode-switch-commit-current-clears-active
  "Test commit-current clears converting-active."
  ;; nskk-input-commands redefines nskk-commit-current with state checks,
  ;; so we need full buffer/mark/candidates setup.
  (with-temp-buffer
    (nskk-mode-switch-test-with-state 'hiragana
      (insert "test")
      (push-mark (point-min) t)
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (setq nskk-converting-active t)
      (nskk-commit-current)
      (should-not nskk-converting-active))))

(nskk-deftest-unit mode-switch-commit-current-noop-when-not-converting
  "Test commit-current is a no-op when not converting."
  (let ((nskk-converting-active nil))
    ;; Should not error
    (nskk-commit-current)
    (should-not nskk-converting-active)))

;;;
;;; Clear Conversion Context Tests
;;;

(nskk-deftest-unit mode-switch-clear-context-when-converting
  "Test that mode switch clears conversion context."
  ;; nskk-input-commands redefines nskk-commit-current with state checks,
  ;; so we need full buffer/mark/candidates setup.
  (with-temp-buffer
    (nskk-mode-switch-test-with-state 'hiragana
      (insert "test")
      (push-mark (point-min) t)
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (setq nskk-converting-active t)
      (nskk--clear-conversion-context)
      ;; Converting should be deactivated
      (should-not nskk-converting-active))))

(nskk-deftest-unit mode-switch-clear-context-when-not-converting
  "Test clear context is safe when not converting."
  (let ((nskk-converting-active nil))
    ;; Should not error
    (nskk--clear-conversion-context)
    (should-not nskk-converting-active)))

;;;
;;; Mode Transition Sequences Tests
;;;

(nskk-deftest-unit mode-switch-sequence-ascii-hiragana-katakana
  "Test mode transition: ascii -> hiragana -> katakana."
  (nskk-mode-switch-test-with-state 'ascii
    (nskk-set-mode-hiragana)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
    (nskk-set-mode-katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(nskk-deftest-unit mode-switch-sequence-hiragana-latin-hiragana
  "Test mode transition: hiragana -> latin -> hiragana."
  (nskk-mode-switch-test-with-state 'hiragana
    (nskk-set-mode-latin)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))
    (nskk-set-mode-hiragana)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(nskk-deftest-unit mode-switch-sequence-all-modes
  "Test transitioning through all valid modes."
  (nskk-mode-switch-test-with-state 'hiragana
    (nskk-set-mode-katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))
    (nskk-set-mode-latin)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))
    (nskk-set-mode-abbrev)
    (should (eq (nskk-state-mode nskk-current-state) 'abbrev))
    (nskk-set-mode-hiragana)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

;;;
;;; Interactive Command Tests
;;;

(nskk-deftest-unit mode-switch-interactive-commands
  "Test that mode switching functions are interactive."
  (should (commandp 'nskk-set-mode-hiragana))
  (should (commandp 'nskk-set-mode-katakana))
  (should (commandp 'nskk-set-mode-latin))
  (should (commandp 'nskk-set-mode-abbrev))
  (should (commandp 'nskk-toggle-katakana)))

;;;
;;; Update Modeline Tests
;;;

(nskk-deftest-unit mode-switch-update-modeline-callable
  "Test that update-modeline function is callable."
  ;; Should not error
  (nskk--update-modeline))

(provide 'nskk-mode-switch-test)

;;; nskk-mode-switch-test.el ends here
