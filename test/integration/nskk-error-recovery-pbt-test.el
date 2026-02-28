;;; nskk-error-recovery-pbt-test.el --- Error recovery PBT tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test, property-based
;; Homepage: https://github.com/takeokunn/nskk.el

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

;; Property-based tests for error recovery from invalid state transitions.
;;
;; This file tests that the system handles errors gracefully and
;; maintains state consistency even when invalid operations are attempted.
;;
;; Properties tested:
;; - error-recovery-invalid-mode: Invalid mode errors but state stays consistent
;; - error-recovery-nil-state-operations: Operations on nil state return nil
;; - error-recovery-concurrent-mode-switch: Rapid mode switches don't corrupt state

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-state)


;;;;
;;;; Helper Data
;;;;

(defconst nskk-pbt--invalid-modes
  '(invalid-mode nil 123 "hiragana" :keyword
    random-symbol another-invalid wrong-mode
    HIRAGANA KATAKANA ASCII)
  "List of invalid mode values for testing.")


;;;;
;;;; Property 1: Error Recovery Invalid Mode
;;;;

(ert-deftest nskk-state-machine-error-recovery-invalid-mode ()
  "Setting invalid mode errors but state stays consistent."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((valid-mode (nskk-pbt--generate-valid-mode))
             (state (nskk-state-create valid-mode))
             (invalid-mode (nskk-pbt--random-choice nskk-pbt--invalid-modes)))
        ;; Set some state before attempting invalid operation
        (nskk-state-set state 'input-buffer "test-input")
        ;; Attempt to set invalid mode (should error)
        (condition-case _err
            (progn
              (nskk-state-set state 'mode invalid-mode)
              ;; If we get here, the invalid mode was accepted (failure)
              (push (list :invalid-mode invalid-mode
                          :error "no error raised"
                          :actual-mode (nskk-state-mode state))
                    failures))
          (error
           ;; Error expected - verify state consistency
           (unless (and (nskk-state-p state)
                        (eq (nskk-state-mode state) valid-mode)
                        (string= (nskk-state-input-buffer state) "test-input")
                        (stringp (nskk-state-converted-buffer state))
                        (listp (nskk-state-candidates state))
                        (integerp (nskk-state-current-index state)))
             (push (list :invalid-mode invalid-mode
                          :error "state corrupted after error"
                          :actual-mode (nskk-state-mode state)
                          :input (nskk-state-input-buffer state))
                    failures))))))
    (when failures
      (ert-fail (format "Invalid mode error recovery failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Property 2: Nil State Operations
;;;;

(ert-deftest nskk-state-machine-error-recovery-nil-state-operations ()
  "Operations on nil state return nil without crash."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let ((op (nskk-pbt--random-int 0 9)))
        (condition-case err
            (pcase op
              (0 (should-not (nskk-state-get nil 'mode)))
              (1 (should-not (nskk-state-set nil 'mode 'hiragana)))
              (2 (should-not (nskk-state-reset nil)))
              (3 (should-not (nskk-state-append-input nil ?a)))
              (4 (should-not (nskk-state-delete-last-char nil)))
              (5 (should-not (nskk-state-clear-input nil)))
              (6 (should-not (nskk-state-next-candidate nil)))
              (7 (should-not (nskk-state-previous-candidate nil)))
              (8 (should-not (nskk-state-current-candidate nil)))
              (9 (should-not (nskk-state-transition nil 'ascii 'hiragana))))
          (error
           (push (list :op op :error err) failures)))))
    (when failures
      (ert-fail (format "Nil state operations crashed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Property 3: Concurrent Mode Switch
;;;;

(ert-deftest nskk-state-machine-error-recovery-concurrent-mode-switch ()
  "Rapid mode switches do not corrupt state."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((state (nskk-state-create (nskk-pbt--generate-valid-mode)))
             (num-switches (nskk-pbt--random-int 10 100))
             (valid-modes '(ascii hiragana katakana latin abbrev)))
        ;; Set some initial state
        (nskk-state-set state 'input-buffer "initial")
        (nskk-state-set state 'candidates '("a" "b"))
        ;; Perform rapid mode switches
        (dotimes (_ num-switches)
          (let ((new-mode (nskk-pbt--random-choice valid-modes)))
            (nskk-state-set state 'mode new-mode)))
        ;; After all switches, verify state consistency
        (let ((final-mode (nskk-state-mode state))
              (prev-mode (nskk-state-previous-mode state))
              (input (nskk-state-input-buffer state))
              (converted (nskk-state-converted-buffer state)))
          (unless (and (nskk-state-p state)
                       (nskk-state-valid-mode-p final-mode)
                       (nskk-state-valid-mode-p prev-mode)
                       (stringp input)
                       (stringp converted))
            (push (list :num-switches num-switches
                        :final-mode final-mode
                        :prev-mode prev-mode
                        :valid-state (nskk-state-p state))
                  failures)))))
    (when failures
      (ert-fail (format "Rapid mode switch failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Additional Property: Error Recovery After Reset
;;;;

(ert-deftest nskk-state-machine-error-recovery-reset-after-error ()
  "State reset after error produces clean state."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((state (nskk-state-create 'hiragana)))
        ;; Set up complex state
        (nskk-state-set state 'input-buffer "complex-input")
        (nskk-state-set state 'converted-buffer "some-conversion")
        (nskk-state-set state 'candidates '("a" "b" "c"))
        (nskk-state-set state 'current-index 1)
        (nskk-state-set state 'henkan-position 3)
        ;; Attempt invalid operation (should error)
        (condition-case _err
            (nskk-state-set state 'mode 'invalid-mode-xyz)
          (error nil))
        ;; Reset the state
        (nskk-state-reset state)
        ;; Verify clean state
        (unless (and (nskk-state-p state)
                     (string= (nskk-state-input-buffer state) "")
                     (string= (nskk-state-converted-buffer state) "")
                     (null (nskk-state-candidates state))
                     (= (nskk-state-current-index state) 0)
                     (null (nskk-state-henkan-position state))
                     ;; Mode should be preserved through reset
                     (nskk-state-valid-mode-p (nskk-state-mode state)))
          (push (list :input (nskk-state-input-buffer state)
                      :converted (nskk-state-converted-buffer state)
                      :candidates (nskk-state-candidates state)
                      :mode (nskk-state-mode state))
                failures))))
    (when failures
      (ert-fail (format "Reset after error failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Additional Property: Mixed Valid/Invalid Operations
;;;;

(ert-deftest nskk-state-machine-error-recovery-mixed-operations ()
  "Mixed valid and invalid operations leave state consistent."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((state (nskk-state-create 'hiragana))
             (ops (nskk-pbt--random-int 5 15)))
        ;; Mix of valid and invalid operations
        (dotimes (_ ops)
          (let ((op (nskk-pbt--random-int 0 5)))
            (condition-case _err
                (pcase op
                  (0 ;; Valid mode change
                   (nskk-state-set state 'mode (nskk-pbt--generate-valid-mode)))
                  (1 ;; Invalid mode change (may error)
                   (nskk-state-set state 'mode
                                   (nskk-pbt--random-choice nskk-pbt--invalid-modes)))
                  (2 ;; Valid input append
                   (nskk-state-append-input state
                                            (nskk-pbt--random-choice
                                             (string-to-list "abcde"))))
                  (3 ;; Valid delete
                   (nskk-state-delete-last-char state))
                  (4 ;; Valid clear
                   (nskk-state-clear-input state))
                  (5 ;; Valid reset
                   (nskk-state-reset state)))
              (error nil))))
        ;; After all operations, state should still be valid
        (unless (and (nskk-state-p state)
                     (nskk-state-valid-mode-p (nskk-state-mode state))
                     (stringp (nskk-state-input-buffer state))
                     (stringp (nskk-state-converted-buffer state))
                     (listp (nskk-state-candidates state))
                     (integerp (nskk-state-current-index state)))
          (push (list :mode (nskk-state-mode state)
                      :input (nskk-state-input-buffer state)
                      :valid (nskk-state-p state))
                failures))))
    (when failures
      (ert-fail (format "Mixed operations consistency failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;; Enhanced PBT Coverage
;;;;

;;;
;;; Shrinking Property: Error recovery after invalid mode
;;;

(nskk-property-test-with-shrinking error-recovery-invalid-mode-shrinking
  ((mode valid-mode))
  (let* ((state (nskk-state-create mode))
         (invalid-mode (nskk-pbt--random-choice nskk-pbt--invalid-modes)))
    (nskk-state-set state 'input-buffer "test-input")
    ;; Attempt invalid mode assignment; expect an error that leaves state intact
    (let ((error-raised nil))
      (condition-case _err
          (nskk-state-set state 'mode invalid-mode)
        (error
         (setq error-raised t)))
      ;; Either an error was raised and state is consistent, or no error but
      ;; state remains a valid nskk-state struct.
      (and (nskk-state-p state)
           (nskk-state-valid-mode-p (nskk-state-mode state))
           (stringp (nskk-state-input-buffer state))
           (stringp (nskk-state-converted-buffer state))
           (listp (nskk-state-candidates state))
           (integerp (nskk-state-current-index state))
           ;; If no error was raised, mode must still be valid (accepted silently)
           (or error-raised
               (nskk-state-valid-mode-p (nskk-state-mode state))))))
  50)

;;;
;;; Scenario: System recovers from rapid mode switches without state corruption
;;;

(nskk-scenario-test rapid-mode-switch-no-corruption
  "Scenario: rapid mode switches never corrupt state structural invariants."
  (let* ((state (nskk-state-create 'hiragana))
         (valid-modes '(ascii hiragana katakana latin abbrev))
         (failures nil))
    ;; Perform 200 rapid mode switches
    (dotimes (_ 200)
      (let ((new-mode (nskk-pbt--random-choice valid-modes)))
        (nskk-state-set state 'mode new-mode)))
    ;; Verify every structural invariant
    (unless (and (nskk-state-p state)
                 (nskk-state-valid-mode-p (nskk-state-mode state))
                 (nskk-state-valid-mode-p (nskk-state-previous-mode state))
                 (stringp (nskk-state-input-buffer state))
                 (stringp (nskk-state-converted-buffer state))
                 (listp (nskk-state-candidates state))
                 (integerp (nskk-state-current-index state))
                 (>= (nskk-state-current-index state) 0))
      (push :state-corrupted failures))
    (when failures
      (ert-fail (format "State corrupted after rapid mode switches: %S" failures)))))


(provide 'nskk-error-recovery-pbt-test)

;;; nskk-error-recovery-pbt-test.el ends here
