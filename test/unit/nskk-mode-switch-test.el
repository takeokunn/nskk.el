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
(require 'nskk-input-commands)
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

(nskk-deftest-unit mode-switch-to-jisx0208-latin
  "Test switching to jisx0208-latin mode."
  (nskk-mode-switch-test-with-state 'hiragana
    (nskk-set-mode-jisx0208-latin)
    (should (eq (nskk-state-mode nskk-current-state) 'jisx0208-latin))))

(nskk-deftest-unit mode-switch-set-mode-jisx0208-latin
  "Test internal set mode to jisx0208-latin."
  (nskk-mode-switch-test-with-state 'ascii
    (nskk--set-mode 'jisx0208-latin)
    (should (eq (nskk-state-mode nskk-current-state) 'jisx0208-latin))))

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
  ;; so we need full buffer/marker/candidates setup.
  (with-temp-buffer
    (nskk-mode-switch-test-with-state 'hiragana
      (insert "test")
      (nskk--set-conversion-start-marker (point-min))
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
  ;; so we need full buffer/marker/candidates setup.
  (with-temp-buffer
    (nskk-mode-switch-test-with-state 'hiragana
      (insert "test")
      (nskk--set-conversion-start-marker (point-min))
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

(nskk-deftest-unit mode-switch-clear-context-all-side-effects
  "Verify nskk--clear-conversion-context clears all 6 state elements."
  (with-temp-buffer
    ;; Setup: create state with active conversion
    (setq-local nskk-current-state (nskk-state-create 'hiragana))
    (setq-local nskk-converting-active t)
    (setq-local nskk--romaji-buffer "ka")
    ;; Setup overlay
    (insert "test text")
    (setq-local nskk--conversion-overlay (make-overlay 1 5))
    (overlay-put nskk--conversion-overlay 'display "テスト")
    ;; Setup marker
    (setq-local nskk--conversion-start-marker (make-marker))
    (set-marker nskk--conversion-start-marker 1 (current-buffer))
    ;; Setup candidates
    (setf (nskk-state-candidates nskk-current-state) '("漢字" "感じ"))
    (setf (nskk-state-current-index nskk-current-state) 1)
    ;; Act
    (nskk--clear-conversion-context)
    ;; Assert all 6 side effects
    ;; 1. converting-active cleared
    (should-not nskk-converting-active)
    ;; 2. overlay deleted
    (should-not (overlay-buffer nskk--conversion-overlay))
    ;; 3. marker cleared
    (should-not (marker-position nskk--conversion-start-marker))
    ;; 4. romaji buffer cleared
    (should (string-empty-p nskk--romaji-buffer))
    ;; 5. candidates cleared
    (should-not (nskk-state-candidates nskk-current-state))
    ;; 6. current-index reset
    (should (= (nskk-state-current-index nskk-current-state) 0))
    ;; 7. henkan-phase reset
    (should (null (nskk-state-henkan-phase nskk-current-state)))))

(nskk-deftest-unit mode-switch-clear-context-resets-henkan-phase
  "Test that clear conversion context resets henkan-phase."
  (with-temp-buffer
    (setq-local nskk-current-state (nskk-state-create 'hiragana))
    (setq-local nskk-converting-active t)
    (setq-local nskk--romaji-buffer "")
    ;; Set henkan-phase to active
    (nskk-state-set-henkan-phase nskk-current-state 'active)
    (should (eq (nskk-state-henkan-phase nskk-current-state) 'active))
    ;; Act
    (nskk--clear-conversion-context)
    ;; henkan-phase should be nil
    (should (null (nskk-state-henkan-phase nskk-current-state)))))

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
    (nskk-set-mode-jisx0208-latin)
    (should (eq (nskk-state-mode nskk-current-state) 'jisx0208-latin))
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
  (should (commandp 'nskk-set-mode-jisx0208-latin))
  (should (commandp 'nskk-toggle-katakana)))

;;;
;;; Update Modeline Tests
;;;

(nskk-deftest-unit mode-switch-update-modeline-callable
  "Test that update-modeline function is callable."
  ;; Should not error
  (nskk--update-modeline))

;;;
;;; Error Handling Tests: Set Mode Without Initialization
;;;

(nskk-deftest-unit mode-switch-set-mode-uninitialized-state
  "Test that set-mode signals error when nskk-current-state is unbound."
  ;; Save and unbind nskk-current-state
  (let ((saved-state (and (boundp 'nskk-current-state) nskk-current-state)))
    (unwind-protect
        (progn
          ;; Unbind the variable completely
          (makunbound 'nskk-current-state)
          ;; Now should-error should catch the "NSKK state not initialized" error
          (should-error (nskk--set-mode 'hiragana)
                        :type 'error))
      ;; Restore the previous state
      (if saved-state
          (setq nskk-current-state saved-state)
        (and (boundp 'nskk-current-state) (makunbound 'nskk-current-state))))))

(nskk-deftest-unit mode-switch-set-mode-nil-state
  "Test that set-mode properly checks state is bound."
  ;; Create a scope where nskk-current-state is not bound at all
  (let ((nskk-current-state 'unused))  ; Dummy value
    (makunbound 'nskk-current-state)   ; Actually unbind it
    (should-error (nskk--set-mode 'hiragana)
                  :type 'error)))

(nskk-deftest-unit mode-switch-set-mode-uninitialized-recoverable
  "Test recovery after uninitialized state error."
  (let ((saved-state (and (boundp 'nskk-current-state) nskk-current-state)))
    (unwind-protect
        (progn
          ;; Unbind state
          (makunbound 'nskk-current-state)
          ;; First attempt fails
          (should-error (nskk--set-mode 'hiragana))
          ;; Now initialize state and retry
          (setq nskk-current-state (nskk-state-create 'ascii))
          ;; Should succeed
          (nskk--set-mode 'hiragana)
          (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))
      ;; Restore
      (if saved-state
          (setq nskk-current-state saved-state)
        (and (boundp 'nskk-current-state) (makunbound 'nskk-current-state))))))

;;;
;;; Error Handling Tests: Invalid Mode Validation
;;;

(nskk-deftest-unit mode-switch-set-mode-symbol-validation
  "Test that set-mode validates mode is a valid symbol."
  (nskk-mode-switch-test-with-state 'ascii
    (should-error (nskk--set-mode 'not-a-real-mode)
                  :type 'error)))

(nskk-deftest-unit mode-switch-set-mode-rejects-string
  "Test that set-mode rejects string mode names."
  (nskk-mode-switch-test-with-state 'ascii
    (should-error (nskk--set-mode "hiragana")
                  :type 'error)))

(nskk-deftest-unit mode-switch-set-mode-rejects-number
  "Test that set-mode rejects numeric mode values."
  (nskk-mode-switch-test-with-state 'ascii
    (should-error (nskk--set-mode 123)
                  :type 'error)))

(nskk-deftest-unit mode-switch-set-mode-rejects-nil
  "Test that set-mode rejects nil as mode."
  (nskk-mode-switch-test-with-state 'ascii
    (should-error (nskk--set-mode nil)
                  :type 'error)))

(nskk-deftest-unit mode-switch-set-mode-all-invalid-modes
  "Test that set-mode rejects a variety of invalid modes."
  (nskk-mode-switch-test-with-state 'ascii
    (dolist (invalid-mode '(foo bar baz invalid-mode unknown mode))
      (should-error (nskk--set-mode invalid-mode)
                    :type 'error))))

;;;
;;; Error Handling Tests: Mode Validation at State-Level
;;;

(nskk-deftest-unit mode-switch-state-set-mode-invalid-returns-nil
  "Test that state-set with invalid mode raises error."
  (nskk-mode-switch-test-with-state 'ascii
    ;; Should raise error on invalid mode
    (should-error (nskk-state-set nskk-current-state 'mode 'invalid-mode))
    ;; Mode should not change
    (should (eq (nskk-state-mode nskk-current-state) 'ascii))))

(nskk-deftest-unit mode-switch-state-set-mode-valid-returns-value
  "Test that state-set with valid mode returns the mode value."
  (nskk-mode-switch-test-with-state 'ascii
    (let ((result (nskk-state-set nskk-current-state 'mode 'hiragana)))
      ;; Should return the mode value on success
      (should (eq result 'hiragana))
      ;; Mode should change
      (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))))

(nskk-deftest-unit mode-switch-state-set-mode-preserves-previous
  "Test that state-set preserves previous mode on transition."
  (nskk-mode-switch-test-with-state 'ascii
    (should (eq (nskk-state-previous-mode nskk-current-state) 'ascii))

    (nskk-state-set nskk-current-state 'mode 'hiragana)
    (should (eq (nskk-state-previous-mode nskk-current-state) 'ascii))
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))

    (nskk-state-set nskk-current-state 'mode 'katakana)
    (should (eq (nskk-state-previous-mode nskk-current-state) 'hiragana))
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

;;;
;;; Error Handling Tests: Public API Error Reporting
;;;

(nskk-deftest-unit mode-switch-set-mode-hiragana-uninitialized
  "Test that set-mode-hiragana signals error when state uninitialized."
  (let ((saved-state (and (boundp 'nskk-current-state) nskk-current-state)))
    (unwind-protect
        (progn
          (makunbound 'nskk-current-state)
          (should-error (nskk-set-mode-hiragana)
                        :type 'error))
      (if saved-state
          (setq nskk-current-state saved-state)
        (and (boundp 'nskk-current-state) (makunbound 'nskk-current-state))))))

(nskk-deftest-unit mode-switch-set-mode-katakana-uninitialized
  "Test that set-mode-katakana signals error when state uninitialized."
  (let ((saved-state (and (boundp 'nskk-current-state) nskk-current-state)))
    (unwind-protect
        (progn
          (makunbound 'nskk-current-state)
          (should-error (nskk-set-mode-katakana)
                        :type 'error))
      (if saved-state
          (setq nskk-current-state saved-state)
        (and (boundp 'nskk-current-state) (makunbound 'nskk-current-state))))))

(nskk-deftest-unit mode-switch-set-mode-latin-uninitialized
  "Test that set-mode-latin signals error when state uninitialized."
  (let ((saved-state (and (boundp 'nskk-current-state) nskk-current-state)))
    (unwind-protect
        (progn
          (makunbound 'nskk-current-state)
          (should-error (nskk-set-mode-latin)
                        :type 'error))
      (if saved-state
          (setq nskk-current-state saved-state)
        (and (boundp 'nskk-current-state) (makunbound 'nskk-current-state))))))

(nskk-deftest-unit mode-switch-set-mode-abbrev-uninitialized
  "Test that set-mode-abbrev signals error when state uninitialized."
  (let ((saved-state (and (boundp 'nskk-current-state) nskk-current-state)))
    (unwind-protect
        (progn
          (makunbound 'nskk-current-state)
          (should-error (nskk-set-mode-abbrev)
                        :type 'error))
      (if saved-state
          (setq nskk-current-state saved-state)
        (and (boundp 'nskk-current-state) (makunbound 'nskk-current-state))))))

;;;
;;; Error Handling Tests: Mode Transition With Conversion Context
;;;

(nskk-deftest-unit mode-switch-set-mode-clears-conversion-context
  "Test that mode switch clears active conversion context."
  (with-temp-buffer
    (nskk-mode-switch-test-with-state 'hiragana
      (insert "test")
      (nskk--set-conversion-start-marker (point-min))
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (setq nskk-converting-active t)

      ;; Switch mode
      (nskk--set-mode 'katakana)

      ;; Conversion context should be cleared
      (should-not nskk-converting-active)
      (should (eq (nskk-state-mode nskk-current-state) 'katakana)))))

(nskk-deftest-unit mode-switch-set-mode-clears-conversion-even-on-same-mode
  "Test that set-mode clears conversion context even if mode doesn't change."
  (with-temp-buffer
    (nskk-mode-switch-test-with-state 'hiragana
      (insert "test")
      (nskk--set-conversion-start-marker (point-min))
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (setq nskk-converting-active t)

      ;; Try to set same mode
      (nskk--set-mode 'hiragana)

      ;; Conversion context should still be cleared
      (should-not nskk-converting-active)
      (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))))

;;;
;;; Error Recovery Tests: Multiple Transitions
;;;

(nskk-deftest-unit mode-switch-error-recovery-sequence
  "Test that mode switching can recover from partial errors."
  (nskk-mode-switch-test-with-state 'ascii
    ;; Valid transition
    (nskk--set-mode 'hiragana)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))

    ;; Invalid transition attempt (should fail without changing mode)
    (should-error (nskk--set-mode 'invalid-mode))
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))

    ;; Valid transition after error
    (nskk--set-mode 'katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(nskk-deftest-unit mode-switch-error-preserves-state-integrity
  "Test that failed mode transitions don't corrupt state."
  (nskk-mode-switch-test-with-state 'hiragana
    ;; Set up state
    (nskk-state-set nskk-current-state 'input-buffer "test")
    (nskk-state-set nskk-current-state 'henkan-position 0)
    (nskk-state-set-candidates nskk-current-state '("a" "b" "c"))

    (let ((mode-before (nskk-state-mode nskk-current-state))
          (input-before (nskk-state-input-buffer nskk-current-state))
          (pos-before (nskk-state-henkan-position nskk-current-state))
          (cands-before (nskk-state-candidates nskk-current-state)))

      ;; Try invalid mode change
      (should-error (nskk--set-mode 'not-real))

      ;; All state should be preserved
      (should (eq (nskk-state-mode nskk-current-state) mode-before))
      (should (string= (nskk-state-input-buffer nskk-current-state) input-before))
      (should (= (nskk-state-henkan-position nskk-current-state) pos-before))
      (should (equal (nskk-state-candidates nskk-current-state) cands-before)))))

(provide 'nskk-mode-switch-test)

;;; nskk-mode-switch-test.el ends here
