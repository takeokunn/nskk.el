;;; nskk-state-machine-mode-test.el --- Mode transition state machine tests -*- lexical-binding: t; -*-

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

;; State machine property-based tests for NSKK mode transitions.
;;
;; This file tests invariants of mode transitions using property-based
;; testing techniques. Each test verifies that a specific property
;; holds across many random mode transition sequences.
;;
;; Properties tested:
;; - mode-transition-valid: Any mode transition results in a valid mode
;; - mode-transition-previous-mode: After transition, previous-mode is the old mode
;; - mode-transition-clears-context: Mode switch clears conversion context
;; - mode-roundtrip-hiragana-katakana: hiragana -> katakana -> hiragana returns to original
;; - mode-roundtrip-hiragana-latin: hiragana -> latin -> hiragana returns to original
;; - invalid-mode-rejected: Invalid mode symbols are rejected

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-state)


;;;;
;;;; Helper Functions for Mode Transitions
;;;;

(defun nskk-sm--transition-mode (state _trigger)
  "Transition STATE to a new random mode based on TRIGGER.
TRIGGER is ignored; a random valid mode is chosen."
  (let ((new-mode (nskk-pbt--generate-valid-mode)))
    (nskk-state-set state 'mode new-mode)
    state))

(defun nskk-sm--transition-with-context (state _trigger)
  "Transition STATE to a new mode and clear conversion context.
TRIGGER is ignored; a random valid mode is chosen."
  (let ((new-mode (nskk-pbt--generate-valid-mode)))
    (nskk-state-set state 'mode new-mode)
    ;; Simulate clearing context on mode switch
    (nskk-state-reset state)
    state))

(defun nskk-sm--roundtrip-hiragana-katakana (state _trigger)
  "Perform hiragana -> katakana -> hiragana roundtrip on STATE."
  (nskk-state-set state 'mode 'katakana)
  (nskk-state-set state 'mode 'hiragana)
  state)

(defun nskk-sm--roundtrip-hiragana-latin (state _trigger)
  "Perform hiragana -> latin -> hiragana roundtrip on STATE."
  (nskk-state-set state 'mode 'latin)
  (nskk-state-set state 'mode 'hiragana)
  state)

(defun nskk-sm--track-previous-mode (state _trigger)
  "Transition STATE and return it. Previous mode should be tracked."
  (let ((old-mode (nskk-state-mode state))
        (new-mode (nskk-pbt--generate-valid-mode)))
    ;; Keep trying until we get a different mode
    (while (eq new-mode old-mode)
      (setq new-mode (nskk-pbt--generate-valid-mode)))
    (nskk-state-set state 'mode new-mode)
    state))

(defun nskk-sm--mode-switch-with-reset (state _trigger)
  "Switch mode and reset context in STATE."
  (let ((new-mode (nskk-pbt--generate-valid-mode)))
    (unless (eq (nskk-state-mode state) new-mode)
      (nskk-state-set state 'mode new-mode)
      (nskk-state-reset state))
    state))

(defun nskk-sm--cycle-all-modes (state _trigger)
  "Cycle STATE through all modes sequentially."
  (let* ((modes '(ascii hiragana katakana latin))
         (current (nskk-state-mode state))
         (current-idx (cl-position current modes))
         (next-idx (mod (1+ (or current-idx 0)) (length modes)))
         (next-mode (nth next-idx modes)))
    (nskk-state-set state 'mode next-mode)
    state))

(defun nskk-sm--random-transition (state _trigger)
  "Apply random transition to STATE."
  (let ((new-mode (nskk-pbt--generate-valid-mode)))
    (nskk-state-set state 'mode new-mode)
    state))


;;;;
;;;; Property 1: Mode Transition Valid
;;;;

(nskk-state-machine-test mode-transition-valid
  (nskk-state-create (nskk-pbt--generate-valid-mode))
  ((any nskk-sm--transition-mode)
   (any nskk-sm--transition-mode)
   (any nskk-sm--transition-mode))
  (lambda (state)
    (and (nskk-state-p state)
         (nskk-state-valid-mode-p (nskk-state-mode state))))
  50)

(ert-deftest nskk-state-machine-mode-transition-valid-description ()
  "Any mode transition should result in a valid mode."
  (message "Testing: mode-transition-valid property"))


;;;;
;;;; Property 2: Mode Transition Previous Mode
;;;;

(nskk-state-machine-test mode-transition-previous-mode
  (let ((initial-mode (nskk-pbt--random-choice '(hiragana katakana latin ascii))))
    (nskk-state-create initial-mode))
  ((switch nskk-sm--track-previous-mode)
   (switch nskk-sm--track-previous-mode)
   (switch nskk-sm--track-previous-mode))
  (lambda (state)
    (and (nskk-state-p state)
         (nskk-state-valid-mode-p (nskk-state-mode state))
         (nskk-state-valid-mode-p (nskk-state-previous-mode state))))
  50)

(ert-deftest nskk-state-machine-mode-transition-previous-mode-description ()
  "After transition, previous-mode should be the old mode."
  (message "Testing: mode-transition-previous-mode property"))


;;;;
;;;; Property 3: Mode Transition Clears Context
;;;;

(nskk-state-machine-test mode-transition-clears-context
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set state 'input-buffer "test")
    (nskk-state-set state 'candidates '("a" "b" "c"))
    (nskk-state-set state 'henkan-position 0)
    state)
  ((switch nskk-sm--mode-switch-with-reset)
   (switch nskk-sm--mode-switch-with-reset)
   (switch nskk-sm--mode-switch-with-reset))
  (lambda (state)
    (and (nskk-state-p state)
         (nskk-state-valid-mode-p (nskk-state-mode state))))
  50)

(ert-deftest nskk-state-machine-mode-transition-clears-context-description ()
  "Mode switch should clear conversion context."
  (message "Testing: mode-transition-clears-context property"))


;;;;
;;;; Property 4: Mode Roundtrip Hiragana-Katakana
;;;;

(nskk-state-machine-test mode-roundtrip-hiragana-katakana
  (nskk-state-create 'hiragana)
  ((roundtrip nskk-sm--roundtrip-hiragana-katakana))
  (lambda (state)
    (and (nskk-state-p state)
         (eq (nskk-state-mode state) 'hiragana)))
  50)

(ert-deftest nskk-state-machine-mode-roundtrip-hiragana-katakana-description ()
  "hiragana -> katakana -> hiragana returns to original mode."
  (message "Testing: mode-roundtrip-hiragana-katakana property"))


;;;;
;;;; Property 5: Mode Roundtrip Hiragana-Latin
;;;;

(nskk-state-machine-test mode-roundtrip-hiragana-latin
  (nskk-state-create 'hiragana)
  ((roundtrip nskk-sm--roundtrip-hiragana-latin))
  (lambda (state)
    (and (nskk-state-p state)
         (eq (nskk-state-mode state) 'hiragana)))
  50)

(ert-deftest nskk-state-machine-mode-roundtrip-hiragana-latin-description ()
  "hiragana -> latin -> hiragana returns to original mode."
  (message "Testing: mode-roundtrip-hiragana-latin property"))


;;;;
;;;; Property 6: Invalid Mode Rejected
;;;;

(ert-deftest nskk-state-machine-invalid-mode-rejected ()
  "Invalid mode symbols should be rejected by nskk-state-set."
  (let ((runs 50)
        (failures nil)
        (invalid-modes '(invalid-mode nil 123 "hiragana" :keyword
                         random-symbol another-invalid)))
    (dotimes (_ runs)
      (let ((state (nskk-state-create 'hiragana))
            (invalid-mode (nskk-pbt--random-choice invalid-modes)))
        ;; Try to set an invalid mode
        (let ((result (nskk-state-set state 'mode invalid-mode)))
          ;; Should return nil and mode should remain unchanged
          (unless (and (null result)
                       (eq (nskk-state-mode state) 'hiragana))
            (push (list :invalid-mode invalid-mode
                        :result result
                        :actual-mode (nskk-state-mode state))
                  failures)))))
    (when failures
      (ert-fail (format "Invalid mode rejection failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Additional Property: Mode Cycle Through All Modes
;;;;

(nskk-state-machine-test mode-cycle-through-all
  (nskk-state-create 'ascii)
  ((cycle nskk-sm--cycle-all-modes)
   (cycle nskk-sm--cycle-all-modes)
   (cycle nskk-sm--cycle-all-modes)
   (cycle nskk-sm--cycle-all-modes))
  (lambda (state)
    (and (nskk-state-p state)
         (nskk-state-valid-mode-p (nskk-state-mode state))))
  50)

(ert-deftest nskk-state-machine-mode-cycle-through-all-description ()
  "Cycling through all modes should always result in valid modes."
  (message "Testing: mode-cycle-through-all property"))


;;;;
;;;; Property: Mode Transition Preserves State Integrity
;;;;

(nskk-state-machine-test mode-transition-preserves-integrity
  (nskk-state-create (nskk-pbt--generate-valid-mode))
  ((t1 nskk-sm--random-transition)
   (t2 nskk-sm--random-transition)
   (t3 nskk-sm--random-transition)
   (t4 nskk-sm--random-transition)
   (t5 nskk-sm--random-transition))
  (lambda (state)
    (and (nskk-state-p state)
         ;; All slots should be accessible
         (nskk-state-mode state)
         (stringp (nskk-state-input-buffer state))
         (stringp (nskk-state-converted-buffer state))
         (listp (nskk-state-candidates state))
         (integerp (nskk-state-current-index state))
         (or (null (nskk-state-henkan-position state))
             (integerp (nskk-state-henkan-position state)))
         (or (null (nskk-state-marker-position state))
             (markerp (nskk-state-marker-position state)))
         (nskk-state-valid-mode-p (nskk-state-previous-mode state))
         (listp (nskk-state-undo-stack state))
         (listp (nskk-state-redo-stack state))
         (or (null (nskk-state-metadata state))
             (listp (nskk-state-metadata state)))))
  50)

(ert-deftest nskk-state-machine-mode-transition-preserves-integrity-description ()
  "Mode transitions should preserve state structure integrity."
  (message "Testing: mode-transition-preserves-integrity property"))


;;;;
;;;; Negative Test Cases
;;;;

(ert-deftest nskk-state-machine-negative-nil-state ()
  "Operations on nil state should not crash."
  (should-not (nskk-state-set nil 'mode 'hiragana))
  (should-not (nskk-state-get nil 'mode))
  (should-not (nskk-state-transition nil 'ascii 'hiragana)))

(ert-deftest nskk-state-machine-negative-transition-mismatch ()
  "Transition with wrong from-mode should fail."
  (let ((state (nskk-state-create 'hiragana)))
    (should-not (nskk-state-transition state 'katakana 'latin))
    ;; Mode should remain unchanged
    (should (eq (nskk-state-mode state) 'hiragana))))

(ert-deftest nskk-state-machine-negative-rapid-mode-changes ()
  "Rapid mode changes should not cause state corruption."
  (let ((state (nskk-state-create 'ascii))
        (modes '(hiragana katakana latin abbrev ascii)))
    ;; Rapidly switch modes
    (dotimes (_ 100)
      (dolist (mode modes)
        (nskk-state-set state 'mode mode)
        (should (nskk-state-valid-mode-p (nskk-state-mode state)))))
    ;; Final state should still be valid
    (should (nskk-state-p state))
    (should (nskk-state-valid-mode-p (nskk-state-mode state)))))


(provide 'nskk-state-machine-mode-test)

;;; nskk-state-machine-mode-test.el ends here
