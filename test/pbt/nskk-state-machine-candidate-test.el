;;; nskk-state-machine-candidate-test.el --- Candidate navigation state machine tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test, state-machine, property-based
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

;; State machine property-based tests for NSKK candidate navigation.
;;
;; This file tests invariants of candidate navigation using property-based
;; testing techniques. Each test verifies that a specific property
;; holds across many random candidate navigation sequences.
;;
;; Properties tested:
;; - candidate-index-bounds: current-index is always < length(candidates)
;; - candidate-next-wraps: Next candidate wraps around to 0 after last
;; - candidate-previous-wraps: Previous candidate wraps to last from 0
;; - candidate-current-valid: Current candidate is always in the list
;; - candidate-set-resets-index: Setting candidates resets index to 0
;; - candidate-empty-list-safe: Empty candidate list doesn't crash

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-state)


;;;;
;;;; Helper Functions for Candidate Operations
;;;;

(defun nskk-sm--generate-candidates (&optional count)
  "Generate a list of random candidates.
COUNT defaults to 0-10 if not specified."
  (nskk-pbt--generate-candidates count))

(defun nskk-sm--set-random-candidates (state _trigger)
  "Set random candidates in STATE."
  (let ((candidates (nskk-sm--generate-candidates)))
    (nskk-state-set-candidates state candidates))
  state)

(defun nskk-sm--navigate-next (state _trigger)
  "Navigate to next candidate in STATE."
  (nskk-state-next-candidate state)
  state)

(defun nskk-sm--navigate-previous (state _trigger)
  "Navigate to previous candidate in STATE."
  (nskk-state-previous-candidate state)
  state)

(defun nskk-sm--clear-candidates (state _trigger)
  "Clear candidates in STATE."
  (nskk-state-set-candidates state nil)
  state)

(defun nskk-sm--safe-candidate-ops (state _trigger)
  "Perform safe candidate operations on STATE even with empty list."
  (let ((op (nskk-pbt--random-int 0 3)))
    (pcase op
      (0 (nskk-state-next-candidate state))
      (1 (nskk-state-previous-candidate state))
      (2 (nskk-state-current-candidate state))
      (3 (nskk-state-set-candidates state nil))))
  state)

(defun nskk-sm--replace-candidates (state _trigger)
  "Replace candidates with a new random list."
  (let ((new-candidates (nskk-sm--generate-candidates (nskk-pbt--random-int 0 10))))
    (nskk-state-set-candidates state new-candidates))
  state)


;;;;
;;;; Property 1: Candidate Index Bounds
;;;;

(nskk-state-machine-test candidate-index-bounds
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set-candidates state (nskk-sm--generate-candidates 5))
    state)
  ((nav nskk-sm--navigate-next)
   (nav nskk-sm--navigate-next)
   (nav nskk-sm--navigate-next)
   (nav nskk-sm--navigate-next))
  (lambda (state)
    (let ((candidates (nskk-state-candidates state))
          (index (nskk-state-current-index state)))
      (or (null candidates)
          (and (>= index 0)
               (< index (length candidates))))))
  50)

(ert-deftest nskk-state-machine-candidate-index-bounds-description ()
  "current-index is always < length(candidates) when candidates non-nil."
  (message "Testing: candidate-index-bounds property"))


;;;;
;;;; Property 2: Candidate Next Wraps
;;;;

(ert-deftest nskk-state-machine-candidate-next-wraps ()
  "Next candidate wraps around to 0 after last."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((state (nskk-state-create 'hiragana))
             (candidates '("a" "b" "c" "d"))
             (len (length candidates)))
        (nskk-state-set-candidates state candidates)
        ;; Start at index 0
        (should (= (nskk-state-current-index state) 0))
        ;; Navigate to the end
        (dotimes (_ (1- len))
          (nskk-state-next-candidate state))
        ;; Should be at last index
        (should (= (nskk-state-current-index state) (1- len)))
        ;; Next should wrap to 0
        (nskk-state-next-candidate state)
        (unless (= (nskk-state-current-index state) 0)
          (push (list :expected 0
                      :actual (nskk-state-current-index state))
                failures))))
    (when failures
      (ert-fail (format "Next wrap failed for %d cases:\n%S"
                        (length failures) failures)))))


;;;;
;;;; Property 3: Candidate Previous Wraps
;;;;

(ert-deftest nskk-state-machine-candidate-previous-wraps ()
  "Previous candidate wraps to last from 0."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((state (nskk-state-create 'hiragana))
             (candidates '("a" "b" "c" "d"))
             (len (length candidates)))
        (nskk-state-set-candidates state candidates)
        ;; Start at index 0
        (should (= (nskk-state-current-index state) 0))
        ;; Previous should wrap to last
        (nskk-state-previous-candidate state)
        (unless (= (nskk-state-current-index state) (1- len))
          (push (list :expected (1- len)
                      :actual (nskk-state-current-index state))
                failures))))
    (when failures
      (ert-fail (format "Previous wrap failed for %d cases:\n%S"
                        (length failures) failures)))))


;;;;
;;;; Property 4: Candidate Current Valid
;;;;

(nskk-state-machine-test candidate-current-valid
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set-candidates state '("first" "second" "third" "fourth"))
    state)
  ((nav nskk-sm--navigate-next)
   (nav nskk-sm--navigate-next)
   (nav nskk-sm--navigate-previous)
   (nav nskk-sm--navigate-next)
   (nav nskk-sm--navigate-next))
  (lambda (state)
    (let ((candidates (nskk-state-candidates state))
          (current (nskk-state-current-candidate state)))
      (or (null candidates)
          (member current candidates))))
  50)

(ert-deftest nskk-state-machine-candidate-current-valid-description ()
  "Current candidate is always in the list."
  (message "Testing: candidate-current-valid property"))


;;;;
;;;; Property 5: Candidate Set Resets Index
;;;;

(ert-deftest nskk-state-machine-candidate-set-resets-index ()
  "Setting candidates resets index to 0."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((state (nskk-state-create 'hiragana))
             (initial-candidates '("a" "b" "c"))
             (new-candidates '("x" "y" "z" "w")))
        ;; Set initial candidates
        (nskk-state-set-candidates state initial-candidates)
        ;; Navigate to a non-zero index
        (nskk-state-next-candidate state)
        (nskk-state-next-candidate state)
        (should (= (nskk-state-current-index state) 2))
        ;; Set new candidates
        (nskk-state-set-candidates state new-candidates)
        ;; Index should be reset to 0
        (unless (= (nskk-state-current-index state) 0)
          (push (list :expected 0
                      :actual (nskk-state-current-index state))
                failures))))
    (when failures
      (ert-fail (format "Set candidates reset index failed for %d cases:\n%S"
                        (length failures) failures)))))


;;;;
;;;; Property 6: Candidate Empty List Safe
;;;;

(nskk-state-machine-test candidate-empty-list-safe
  (nskk-state-create 'hiragana)
  ((op nskk-sm--safe-candidate-ops)
   (op nskk-sm--safe-candidate-ops)
   (op nskk-sm--safe-candidate-ops)
   (op nskk-sm--safe-candidate-ops)
   (op nskk-sm--safe-candidate-ops))
  (lambda (state)
    (and (nskk-state-p state)
         (>= (nskk-state-current-index state) 0)
         (listp (nskk-state-candidates state))))
  50)

(ert-deftest nskk-state-machine-candidate-empty-list-safe-description ()
  "Empty candidate list doesn't crash."
  (message "Testing: candidate-empty-list-safe property"))


;;;;
;;;; Additional Property: Candidate Navigation Cycle
;;;;

(nskk-state-machine-test candidate-navigation-cycle
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set-candidates state '("a" "b" "c" "d" "e"))
    state)
  ((next nskk-sm--navigate-next)
   (next nskk-sm--navigate-next)
   (prev nskk-sm--navigate-previous)
   (prev nskk-sm--navigate-previous))
  (lambda (state)
    (let ((index (nskk-state-current-index state)))
      (>= index 0)))
  50)

(ert-deftest nskk-state-machine-candidate-navigation-cycle-description ()
  "Navigating forward and backward should maintain consistency."
  (message "Testing: candidate-navigation-cycle property"))


;;;;
;;;; Property: Single Candidate Navigation
;;;;

(ert-deftest nskk-state-machine-single-candidate-nav ()
  "Navigation with single candidate should stay at index 0."
  (let* ((state (nskk-state-create 'hiragana))
         (runs 50))
    (nskk-state-set-candidates state '("only"))
    (dotimes (_ runs)
      (nskk-state-next-candidate state)
      (should (= (nskk-state-current-index state) 0))
      (nskk-state-previous-candidate state)
      (should (= (nskk-state-current-index state) 0))
      (should (string= (nskk-state-current-candidate state) "only")))))


;;;;
;;;; Property: Candidate List Replacement
;;;;

(nskk-state-machine-test candidate-replacement-safe
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set-candidates state '("initial"))
    state)
  ((replace nskk-sm--replace-candidates)
   (replace nskk-sm--replace-candidates)
   (replace nskk-sm--replace-candidates)
   (nav nskk-sm--navigate-next)
   (replace nskk-sm--replace-candidates))
  (lambda (state)
    (let ((candidates (nskk-state-candidates state))
          (index (nskk-state-current-index state)))
      (and (listp candidates)
           (>= index 0)
           (or (null candidates)
               (< index (length candidates))))))
  50)

(ert-deftest nskk-state-machine-candidate-replacement-safe-description ()
  "Replacing candidate list should always result in valid state."
  (message "Testing: candidate-replacement-safe property"))


;;;;
;;;; Property: Full Navigation Cycle
;;;;

(ert-deftest nskk-state-machine-full-navigation-cycle ()
  "Full cycle through all candidates should return to start."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((state (nskk-state-create 'hiragana))
             (candidates '("a" "b" "c" "d" "e"))
             (len (length candidates)))
        (nskk-state-set-candidates state candidates)
        ;; Navigate through all candidates (full cycle)
        (dotimes (_ len)
          (nskk-state-next-candidate state))
        ;; Should be back at index 0
        (unless (= (nskk-state-current-index state) 0)
          (push (list :expected 0
                      :actual (nskk-state-current-index state)
                      :len len)
                failures))
        ;; Navigate backward through all candidates (full cycle)
        (dotimes (_ len)
          (nskk-state-previous-candidate state))
        ;; Should still be at index 0
        (unless (= (nskk-state-current-index state) 0)
          (push (list :expected 0
                      :actual (nskk-state-current-index state)
                      :len len)
                failures))))
    (when failures
      (ert-fail (format "Full cycle failed for %d cases:\n%S"
                        (length failures) failures)))))


;;;;
;;;; Negative Test Cases
;;;;

(ert-deftest nskk-state-machine-candidate-negative-nil-state ()
  "Candidate operations on nil state should not crash."
  (should-not (nskk-state-next-candidate nil))
  (should-not (nskk-state-previous-candidate nil))
  (should-not (nskk-state-current-candidate nil)))

(ert-deftest nskk-state-machine-candidate-negative-nil-candidates ()
  "Operations with nil candidates should return nil safely."
  (let ((state (nskk-state-create 'hiragana)))
    ;; No candidates set
    (should-not (nskk-state-current-candidate state))
    ;; Next/previous should return nil and not crash
    (should-not (nskk-state-next-candidate state))
    (should-not (nskk-state-previous-candidate state))))

(ert-deftest nskk-state-machine-candidate-negative-invalid-index ()
  "Setting invalid index should not crash state."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set-candidates state '("a" "b" "c"))
    ;; Set a valid index
    (nskk-state-set state 'current-index 2)
    (should (= (nskk-state-current-index state) 2))
    ;; Try to set an out-of-bounds index
    ;; (The state doesn't prevent this, but operations should still work)
    (nskk-state-set state 'current-index 100)
    ;; Current candidate may be nil, but state should be valid
    (should (nskk-state-p state))))

(ert-deftest nskk-state-machine-candidate-negative-empty-string-candidates ()
  "Empty strings in candidate list should work."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set-candidates state '("" "a" "" "b" ""))
    (should (string= (nskk-state-current-candidate state) ""))
    (nskk-state-next-candidate state)
    (should (string= (nskk-state-current-candidate state) "a"))))


;;;;
;;;; Stress Test: Many Navigation Operations
;;;;

(ert-deftest nskk-state-machine-candidate-stress-navigation ()
  "Stress test with many candidate navigation operations."
  (let ((state (nskk-state-create 'hiragana))
        (operations 0))
    (nskk-state-set-candidates state '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j"))
    ;; Perform 1000 random navigation operations
    (dotimes (_ 1000)
      (cl-incf operations)
      (let ((op (nskk-pbt--random-int 0 1)))
        (pcase op
          (0 (nskk-state-next-candidate state))
          (1 (nskk-state-previous-candidate state))))
      ;; Verify invariant after each operation
      (let ((index (nskk-state-current-index state)))
        (should (>= index 0))
        (should (< index 10))))
    (message "Completed %d candidate navigation operations" operations)))


;;;;
;;;; Property: Large Candidate List
;;;;

(ert-deftest nskk-state-machine-large-candidate-list ()
  "Test navigation with large candidate list."
  (let* ((state (nskk-state-create 'hiragana))
         (large-candidates (cl-loop for i from 1 to 100
                                    collect (format "candidate-%d" i))))
    (nskk-state-set-candidates state large-candidates)
    ;; Navigate to end
    (dotimes (_ 99)
      (nskk-state-next-candidate state))
    (should (= (nskk-state-current-index state) 99))
    (should (string= (nskk-state-current-candidate state) "candidate-100"))
    ;; One more should wrap to beginning
    (nskk-state-next-candidate state)
    (should (= (nskk-state-current-index state) 0))
    (should (string= (nskk-state-current-candidate state) "candidate-1"))))


(provide 'nskk-state-machine-candidate-test)

;;; nskk-state-machine-candidate-test.el ends here
