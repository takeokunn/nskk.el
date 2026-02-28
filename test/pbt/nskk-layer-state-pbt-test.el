;;; nskk-layer-state-pbt-test.el --- Property-Based Tests for State Management Layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n, testing, property-based

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Property-Based Tests for the State Management layer (nskk-state.el).
;;
;; This file tests state structure properties, state transitions,
;; undo/redo operations, and candidate management using property-based
;; testing techniques to ensure correctness across a wide range of inputs.
;;
;; Test Categories:
;; - State Structure Properties: Validity of created states and slot types
;; - State Transition Properties: Mode transitions and their properties
;; - Undo/Redo Properties: Stack management and involution
;; - Candidate Management Properties: Index bounds and navigation

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-state)

;;;
;;; Register Additional Generators for State Tests
;;;

;; These generators use Emacs' built-in (random) which is seeded by the
;; nskk-property-test-seeded macro. We use (abs (random)) to ensure
;; positive values where needed.

;; Initialize random state with a positive seed to ensure (random)
;; returns positive numbers when called without a limit.
(random (abs (random)))

(nskk-register-generator 'pbt-integer
  (lambda ()
    (- (random 101) 50)))  ; Range: -50 to 50

(nskk-register-generator 'pbt-positive-integer
  (lambda ()
    (+ 2 (random 19))))  ; Range: 2 to 20

(nskk-register-generator 'pbt-index
  (lambda ()
    (random 11)))  ; Range: 0 to 10

;;;
;;; Helper Functions
;;;

(defun nskk-pbt--state-valid-p (state)
  "Check if STATE is a valid nskk-state structure."
  (and (nskk-state-p state)
       (nskk-state-valid-mode-p (nskk-state-mode state))
       (stringp (nskk-state-input-buffer state))
       (stringp (nskk-state-converted-buffer state))
       (listp (nskk-state-candidates state))
       (integerp (nskk-state-current-index state))
       (>= (nskk-state-current-index state) 0)
       (listp (nskk-state-undo-stack state))
       (listp (nskk-state-redo-stack state))))

(defun nskk-pbt--copy-state (state)
  "Create a copy of STATE for comparison."
  (when (nskk-state-p state)
    (let ((copy (nskk-state-create (nskk-state-mode state))))
      (nskk-state-set copy 'input-buffer (nskk-state-input-buffer state))
      (nskk-state-set copy 'converted-buffer (nskk-state-converted-buffer state))
      (nskk-state-set copy 'candidates (nskk-state-candidates state))
      (nskk-state-set copy 'current-index (nskk-state-current-index state))
      (nskk-state-set copy 'henkan-position (nskk-state-henkan-position state))
      (nskk-state-set copy 'previous-mode (nskk-state-previous-mode state))
      (nskk-state-set copy 'undo-stack (nskk-state-undo-stack state))
      (nskk-state-set copy 'redo-stack (nskk-state-redo-stack state))
      copy)))

(defun nskk-pbt--states-equal-p (state1 state2 &rest ignore-slots)
  "Check if STATE1 and STATE2 are equal, ignoring IGNORE-SLOTS."
  (and (nskk-state-p state1)
       (nskk-state-p state2)
       (cl-loop for slot in '(mode input-buffer converted-buffer
                               candidates current-index henkan-position
                               previous-mode undo-stack redo-stack)
                unless (memq slot ignore-slots)
                do (unless (equal (nskk-state-get state1 slot)
                                  (nskk-state-get state2 slot))
                     (cl-return nil))
                finally return t)))

;;;
;;; State Structure Properties
;;;

(nskk-property-test-seeded state-create-valid
  ((mode valid-mode))
  (let ((state (nskk-state-create mode)))
    (and (nskk-state-p state)
         (nskk-state-valid-mode-p (nskk-state-mode state))
         (stringp (nskk-state-input-buffer state))
         (stringp (nskk-state-converted-buffer state))
         (listp (nskk-state-candidates state))
         (integerp (nskk-state-current-index state))
         (>= (nskk-state-current-index state) 0)))
  100)

(nskk-property-test-seeded state-get-set-inverse
  ((mode valid-mode)
   (idx pbt-index))
  (let* ((state (nskk-state-create mode))
         (str-val "test-string")
         (int-val (max 0 idx)))
    (nskk-state-set state 'input-buffer str-val)
    (and (equal (nskk-state-get state 'input-buffer) str-val)
         (progn
           (nskk-state-set state 'current-index int-val)
           (equal (nskk-state-get state 'current-index) int-val))
         (progn
           (nskk-state-set state 'mode 'hiragana)
           (equal (nskk-state-get state 'mode) 'hiragana))))
  100)

(nskk-property-test-seeded state-slot-types
  ((mode valid-mode)
   (cands state-object))
  (let ((state (nskk-state-create mode)))
    (nskk-state-set state 'input-buffer "test")
    (nskk-state-set state 'converted-buffer "converted")
    (nskk-state-set state 'candidates (plist-get cands :candidates))
    (and (symbolp (nskk-state-mode state))
         (memq (nskk-state-mode state) nskk-state-modes)
         (stringp (nskk-state-input-buffer state))
         (stringp (nskk-state-converted-buffer state))
         (listp (nskk-state-candidates state))
         (integerp (nskk-state-current-index state))
         (>= (nskk-state-current-index state) 0)))
  100)

;;;
;;; State Transition Properties
;;;

(nskk-property-test-seeded state-transition-validity
  ((from-mode valid-mode)
   (to-mode valid-mode))
  (let ((state (nskk-state-create from-mode)))
    (and (nskk-state-transition state from-mode to-mode)
         (eq (nskk-state-mode state) to-mode)
         (let ((state2 (nskk-state-create from-mode)))
           (not (nskk-state-transition state2 'invalid-mode to-mode)))
         (let ((state3 (nskk-state-create from-mode)))
           (not (nskk-state-transition state3 from-mode 'invalid-mode)))))
  100)

(nskk-property-test-seeded state-transition-preserves-structure
  ((from-mode valid-mode)
   (to-mode valid-mode))
  (let* ((state (nskk-state-create from-mode))
         (input-str "test-input"))
    (nskk-state-set state 'input-buffer input-str)
    (nskk-state-set state 'candidates '("a" "b" "c"))
    (nskk-state-transition state from-mode to-mode)
    (and (nskk-state-p state)
         (eq (nskk-state-mode state) to-mode)
         (equal (nskk-state-input-buffer state) input-str)
         (equal (nskk-state-candidates state) '("a" "b" "c"))
         (eq (nskk-state-previous-mode state) from-mode)))
  100)

(nskk-property-test-seeded state-reset-clears-buffers
  ((mode valid-mode))
  (let ((state (nskk-state-create mode)))
    (nskk-state-set state 'input-buffer "test")
    (nskk-state-set state 'converted-buffer "converted")
    (nskk-state-set state 'candidates '("a" "b" "c"))
    (nskk-state-set state 'current-index 2)
    (nskk-state-set state 'henkan-position 5)
    (nskk-state-set state 'undo-stack '((input . "old")))
    (nskk-state-set state 'redo-stack '((input . "redo")))
    (nskk-state-reset state)
    (and (string= (nskk-state-input-buffer state) "")
         (string= (nskk-state-converted-buffer state) "")
         (null (nskk-state-candidates state))
         (= (nskk-state-current-index state) 0)
         (null (nskk-state-henkan-position state))
         (null (nskk-state-undo-stack state))
         (null (nskk-state-redo-stack state))
         (eq (nskk-state-mode state) mode)))
  100)

;;;
;;; Undo/Redo Properties
;;;

(nskk-property-test-seeded undo-stack-grows
  ((mode valid-mode))
  (let ((state (nskk-state-create mode)))
    (and (null (nskk-state-undo-stack state))
         (progn
           (nskk-state-set state 'undo-stack (list (cons 'input "first")))
           (and (listp (nskk-state-undo-stack state))
                (= (length (nskk-state-undo-stack state)) 1)))
         (progn
           (nskk-state-set state 'undo-stack
                           (cons (cons 'input "second")
                                 (nskk-state-undo-stack state)))
           (and (listp (nskk-state-undo-stack state))
                (= (length (nskk-state-undo-stack state)) 2)))))
  100)

(nskk-property-test-seeded redo-stack-after-undo
  ((mode valid-mode))
  (let ((state (nskk-state-create mode)))
    (nskk-state-set state 'input-buffer "original")
    (let ((saved-input (nskk-state-input-buffer state)))
      (nskk-state-set state 'undo-stack
                      (list (cons 'input-buffer saved-input)))
      (nskk-state-set state 'input-buffer "modified")
      (let ((undo-entry (car (nskk-state-undo-stack state))))
        (nskk-state-set state 'redo-stack (list undo-entry))
        (and (listp (nskk-state-redo-stack state))
             (= (length (nskk-state-redo-stack state)) 1)
             (equal (cdr (car (nskk-state-redo-stack state))) saved-input)))))
  100)

(nskk-property-test-seeded undo-redo-involution
  ((mode valid-mode))
  (let* ((state (nskk-state-create mode))
         (original-input "original")
         (new-input "modified"))
    (nskk-state-set state 'input-buffer original-input)
    (nskk-state-set state 'undo-stack
                    (list (cons 'input-buffer original-input)))
    (nskk-state-set state 'input-buffer new-input)
    (nskk-state-set state 'redo-stack
                    (list (cons 'input-buffer new-input)))
    (nskk-state-set state 'input-buffer
                    (cdr (car (nskk-state-undo-stack state))))
    (nskk-state-set state 'input-buffer
                    (cdr (car (nskk-state-redo-stack state))))
    (equal (nskk-state-input-buffer state) new-input))
  100)

;;;
;;; Candidate Management Properties
;;;

(nskk-property-test-seeded candidate-index-bounded
  ((mode valid-mode)
   (num-candidates pbt-index))
  (let* ((state (nskk-state-create mode))
         (count num-candidates)
         (candidates (cl-loop for i from 1 to count
                              collect (format "candidate-%d" i))))
    (nskk-state-set-candidates state candidates)
    (if (= count 0)
        (and (null (nskk-state-candidates state))
             (= (nskk-state-current-index state) 0))
      (and (< (nskk-state-current-index state) (length candidates))
           (>= (nskk-state-current-index state) 0))))
  100)

(nskk-property-test-seeded candidate-next-previous-inverse
  ((mode valid-mode)
   (num-candidates pbt-positive-integer))
  (let* ((state (nskk-state-create mode))
         (count (max 2 num-candidates))  ; Ensure at least 2 candidates
         (candidates (cl-loop for i from 1 to count
                              collect (format "c%d" i))))
    (nskk-state-set-candidates state candidates)
    (let ((original-index (nskk-state-current-index state)))
      (nskk-state-next-candidate state)
      (nskk-state-previous-candidate state)
      (let ((after-next-prev (nskk-state-current-index state)))
        (nskk-state-set state 'current-index original-index)
        (nskk-state-previous-candidate state)
        (nskk-state-next-candidate state)
        (let ((after-prev-next (nskk-state-current-index state)))
          (and (= after-next-prev original-index)
               (= after-prev-next original-index))))))
  100)

(nskk-property-test-seeded candidate-wrap-around
  ((mode valid-mode)
   (num-candidates pbt-positive-integer))
  (let* ((state (nskk-state-create mode))
         (count (min 5 num-candidates))
         (candidates (cl-loop for i from 1 to count
                              collect (format "c%d" i))))
    (nskk-state-set-candidates state candidates)
    (let ((start-index (nskk-state-current-index state)))
      (cl-loop repeat count
               do (nskk-state-next-candidate state))
      (let ((after-next-wrap (nskk-state-current-index state)))
        (cl-loop repeat count
                 do (nskk-state-previous-candidate state))
        (let ((after-prev-wrap (nskk-state-current-index state)))
          (and (= after-next-wrap start-index)
               (= after-prev-wrap start-index))))))
  100)

;;;
;;; Additional State Invariant Properties
;;;

(nskk-property-test-seeded state-mode-always-valid
  ((mode valid-mode)
   (to-mode valid-mode))
  (let ((state (nskk-state-create mode)))
    (and (nskk-state-valid-mode-p (nskk-state-mode state))
         (progn
           (nskk-state-transition state mode to-mode)
           (nskk-state-valid-mode-p (nskk-state-mode state)))
         (progn
           (nskk-state-reset state)
           (nskk-state-valid-mode-p (nskk-state-mode state)))))
  100)

(nskk-property-test-seeded state-current-index-non-negative
  ((mode valid-mode)
   (idx pbt-index))
  (let ((state (nskk-state-create mode)))
    (nskk-state-set state 'current-index (abs idx))
    (nskk-state-set-candidates state '("a" "b" "c"))
    (nskk-state-next-candidate state)
    (nskk-state-previous-candidate state)
    (>= (nskk-state-current-index state) 0))
  100)

(nskk-property-test-seeded state-previous-mode-tracked
  ((mode1 valid-mode)
   (mode2 valid-mode)
   (mode3 valid-mode))
  (let ((state (nskk-state-create mode1)))
    (and (eq (nskk-state-previous-mode state) mode1)
         (progn
           (nskk-state-transition state mode1 mode2)
           (eq (nskk-state-previous-mode state) mode1))
         (progn
           (nskk-state-transition state mode2 mode3)
           (eq (nskk-state-previous-mode state) mode2))))
  100)

(nskk-property-test-seeded state-buffers-string-type
  ((mode valid-mode))
  (let ((state (nskk-state-create mode)))
    (nskk-state-set state 'input-buffer "test")
    (nskk-state-set state 'converted-buffer "converted")
    (nskk-state-append-input state ?x)
    (and (stringp (nskk-state-input-buffer state))
         (stringp (nskk-state-converted-buffer state))
         (progn
           (nskk-state-delete-last-char state)
           (stringp (nskk-state-input-buffer state)))
         (progn
           (nskk-state-clear-input state)
           (stringp (nskk-state-input-buffer state)))))
  100)

(nskk-property-test-seeded state-metadata-persistence
  ((mode valid-mode)
   (key-1 pbt-index)
   (key-2 pbt-index))
  (let ((state (nskk-state-create mode))
        (val1 (format "value-%d" (abs key-1)))
        (val2 (format "value-%d" (abs key-2))))
    (nskk-state-put-metadata state :test-key-1 val1)
    (nskk-state-put-metadata state :test-key-2 val2)
    (and (equal (nskk-state-get-metadata state :test-key-1) val1)
         (equal (nskk-state-get-metadata state :test-key-2) val2)
         (progn
           (nskk-state-put-metadata state :test-key-1 "updated")
           (equal (nskk-state-get-metadata state :test-key-1) "updated"))
         (equal (nskk-state-get-metadata state :test-key-2) val2)))
  100)

(nskk-property-test-seeded state-henkan-position-nil-or-nonnegative
  ((mode valid-mode)
   (pos pbt-index))
  (let ((state (nskk-state-create mode)))
    (nskk-state-set state 'henkan-position (when (> pos 0) pos))
    (let ((henkan-pos (nskk-state-henkan-position state)))
      (or (null henkan-pos)
          (and (integerp henkan-pos)
               (>= henkan-pos 0)))))
  100)

(nskk-property-test-seeded state-append-delete-inverse
  ((mode valid-mode)
   (ch pbt-index))
  (let* ((state (nskk-state-create mode))
         (char (mod (abs ch) 256)))
    (if (<= char 0)
        t  ; Nothing to test when char is 0
      (let ((original (nskk-state-input-buffer state)))
        (nskk-state-append-input state char)
        (nskk-state-delete-last-char state)
        (equal (nskk-state-input-buffer state) original))))
  100)

(provide 'nskk-layer-state-pbt-test)

;;; nskk-layer-state-pbt-test.el ends here
