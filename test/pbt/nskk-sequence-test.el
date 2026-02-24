;;; nskk-sequence-test.el --- Sequence-based property tests for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: NSKK Contributors
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

;; Sequence-based property tests for the NSKK input method.
;;
;; This file tests properties over random key press sequences to verify
;; system invariants hold regardless of input order.
;;
;; Properties tested:
;; - State integrity after any sequence of key presses
;; - Buffer bounds and consistency
;; - Mode validity preservation
;; - Romaji buffer consistency
;; - Deterministic replay behavior
;; - Undo/redo invariants
;; - Japanese typing correctness
;; - Mode switch idempotency

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-state)
(require 'nskk-input-commands)

(eval-when-compile (require 'cl-lib))


;;;;
;;;; Helper Functions for Sequence Testing
;;;;

(defun nskk-sequence-test--execute-keys (state key-sequence)
  "Execute KEY-SEQUENCE on STATE, returning updated state.
Simulates key presses without actual buffer insertion."
  (let ((current-state state))
    (dolist (key key-sequence current-state)
      (setq current-state (nskk-sequence-test--process-key current-state key)))))

(defun nskk-sequence-test--process-key (state key)
  "Process a single KEY press on STATE, returning updated state.
KEY is a string representing the key (e.g., \"a\", \"C-j\", \"q\")."
  (when (nskk-state-p state)
    (cond
     ;; Mode switch keys
     ((string= key "C-j")
      (nskk-state-set state 'mode 'hiragana)
      state)
     ((string= key "q")
      (let ((current-mode (nskk-state-mode state)))
        (cond
         ((eq current-mode 'hiragana)
          (nskk-state-set state 'mode 'katakana))
         ((eq current-mode 'katakana)
          (nskk-state-set state 'mode 'hiragana))
         (t state))
        state))
     ((string= key "l")
      (nskk-state-set state 'mode 'latin)
      state)
     ((string= key ";")
      (nskk-state-set state 'mode 'abbrev)
      state)
     ;; Regular character input
     ((and (stringp key) (= (length key) 1))
      (let* ((char (aref key 0))
             (current-buffer (nskk-state-input-buffer state)))
        ;; Append to input buffer
        (nskk-state-set state 'input-buffer (concat current-buffer key))
        state))
     ;; Unknown key - pass through
     (t state))))

(defun nskk-sequence-test--valid-state-p (state)
  "Check if STATE has valid structure and values."
  (and (nskk-state-p state)
       ;; Mode is valid
       (nskk-state-valid-mode-p (nskk-state-mode state))
       ;; Buffers are strings
       (stringp (nskk-state-input-buffer state))
       (stringp (nskk-state-converted-buffer state))
       ;; Candidates is a list
       (listp (nskk-state-candidates state))
       ;; Index is non-negative integer
       (integerp (nskk-state-current-index state))
       (>= (nskk-state-current-index state) 0)
       ;; Previous mode is valid
       (nskk-state-valid-mode-p (nskk-state-previous-mode state))
       ;; Stacks are lists
       (listp (nskk-state-undo-stack state))
       (listp (nskk-state-redo-stack state))))

(defun nskk-sequence-test--buffer-bounds-p (state)
  "Check if STATE buffer lengths are within reasonable bounds."
  (and (nskk-state-p state)
       (let ((input-len (length (nskk-state-input-buffer state)))
             (converted-len (length (nskk-state-converted-buffer state))))
         (and (>= input-len 0)
              (>= converted-len 0)
              (<= input-len 1000)
              (<= converted-len 1000)))))

(defun nskk-sequence-test--romaji-buffer-consistent-p (state)
  "Check if romaji buffer in STATE contains only valid characters."
  (let ((romaji (nskk-state-input-buffer state)))
    (and (stringp romaji)
         (or (string-empty-p romaji)
             (string-match-p "^[a-zA-Z]*$" romaji)))))

(defun nskk-sequence-test--mode-valid-p (state)
  "Check if mode in STATE is one of the valid NSKK modes."
  (nskk-state-valid-mode-p (nskk-state-mode state)))

(defun nskk-sequence-test--states-equal-p (state1 state2)
  "Check if STATE1 and STATE2 have equivalent mode and buffer content."
  (and (nskk-state-p state1)
       (nskk-state-p state2)
       (eq (nskk-state-mode state1) (nskk-state-mode state2))
       (string= (nskk-state-input-buffer state1)
                (nskk-state-input-buffer state2))
       (string= (nskk-state-converted-buffer state1)
                (nskk-state-converted-buffer state2))))


;;;;
;;;; Property 1: State Never Corrupt
;;;;

(nskk-sequence-test state-never-corrupt
  key-sequence
  (nskk-state-create 'hiragana)
  (lambda (final-state)
    (nskk-sequence-test--valid-state-p final-state))
  75)


;;;;
;;;; Property 2: Buffer Bounds
;;;;

(nskk-sequence-test buffer-bounds-valid
  key-sequence
  (nskk-state-create 'ascii)
  (lambda (final-state)
    (nskk-sequence-test--buffer-bounds-p final-state))
  75)


;;;;
;;;; Property 3: Mode Always Valid
;;;;

(nskk-sequence-test mode-always-valid
  key-sequence
  (nskk-state-create 'hiragana)
  (lambda (final-state)
    (nskk-sequence-test--mode-valid-p final-state))
  75)


;;;;
;;;; Property 4: Romaji Buffer Consistent
;;;;

(nskk-sequence-test romaji-buffer-consistent
  lowercase-key-sequence
  (nskk-state-create 'hiragana)
  (lambda (final-state)
    (nskk-sequence-test--romaji-buffer-consistent-p final-state))
  75)


;;;;
;;;; Property 5: Deterministic Replay
;;;;

(ert-deftest nskk-sequence-deterministic-replay ()
  "Same key sequence produces same result."
  (let ((runs 75)
        (failures nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "Sequence test 'deterministic-replay' seed: %d" test-seed)
    (dotimes (run runs)
      (let* ((key-sequence (nskk-generate 'key-sequence))
             (initial-state1 (nskk-state-create 'hiragana))
             (initial-state2 (nskk-state-create 'hiragana))
             (result1 (nskk-sequence-test--execute-keys initial-state1 key-sequence))
             (result2 (nskk-sequence-test--execute-keys initial-state2 key-sequence)))
        (unless (nskk-sequence-test--states-equal-p result1 result2)
          (push (list :seed test-seed
                      :run run
                      :key-sequence key-sequence
                      :result1-mode (nskk-state-mode result1)
                      :result2-mode (nskk-state-mode result2)
                      :result1-buffer (nskk-state-input-buffer result1)
                      :result2-buffer (nskk-state-input-buffer result2))
                failures))))
    (when failures
      (ert-fail (format "Determinism failed for %d cases (seed: %d):\n%S"
                        (length failures) test-seed
                        (take 3 failures))))))


;;;;
;;;; Property 6: Undo/Redo Invariant
;;;;

(defun nskk-sequence-test--simulate-undo (state)
  "Simulate undo operation on STATE.
Returns updated state with undo applied."
  (when (and (nskk-state-p state)
             (nskk-state-undo-stack state))
    (let* ((undo-item (car (nskk-state-undo-stack state)))
           (remaining-stack (cdr (nskk-state-undo-stack state)))
           (redo-stack (nskk-state-redo-stack state)))
      ;; Push current state to redo stack
      (setf (nskk-state-redo-stack state)
            (cons (list :mode (nskk-state-mode state)
                        :input-buffer (nskk-state-input-buffer state))
                  redo-stack))
      ;; Apply undo
      (when (listp undo-item)
        (when (plist-get undo-item :mode)
          (setf (nskk-state-mode state) (plist-get undo-item :mode)))
        (when (plist-get undo-item :input-buffer)
          (setf (nskk-state-input-buffer state) (plist-get undo-item :input-buffer))))
      (setf (nskk-state-undo-stack state) remaining-stack)))
  state)

(defun nskk-sequence-test--simulate-redo (state)
  "Simulate redo operation on STATE.
Returns updated state with redo applied."
  (when (and (nskk-state-p state)
             (nskk-state-redo-stack state))
    (let* ((redo-item (car (nskk-state-redo-stack state)))
           (remaining-stack (cdr (nskk-state-redo-stack state)))
           (undo-stack (nskk-state-undo-stack state)))
      ;; Push current state to undo stack
      (setf (nskk-state-undo-stack state)
            (cons (list :mode (nskk-state-mode state)
                        :input-buffer (nskk-state-input-buffer state))
                  undo-stack))
      ;; Apply redo
      (when (listp redo-item)
        (when (plist-get redo-item :mode)
          (setf (nskk-state-mode state) (plist-get redo-item :mode)))
        (when (plist-get redo-item :input-buffer)
          (setf (nskk-state-input-buffer state) (plist-get redo-item :input-buffer))))
      (setf (nskk-state-redo-stack state) remaining-stack)))
  state)

(ert-deftest nskk-sequence-undo-redo-invariant ()
  "Undo + Redo returns to original state (when both succeed)."
  (let ((runs 75)
        (failures nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "Sequence test 'undo-redo-invariant' seed: %d" test-seed)
    (dotimes (run runs)
      (let* ((initial-state (nskk-state-create 'hiragana))
             ;; Setup: add something to undo stack
             (state-with-history
              (progn
                (setf (nskk-state-undo-stack initial-state)
                      (list (list :mode 'ascii :input-buffer "test")))
                (setf (nskk-state-mode initial-state) 'hiragana)
                (setf (nskk-state-input-buffer initial-state) "")
                initial-state))
             ;; Record original state
             (original-mode (nskk-state-mode state-with-history))
             (original-buffer (nskk-state-input-buffer state-with-history))
             ;; Apply undo then redo
             (after-undo (nskk-sequence-test--simulate-undo
                          (copy-sequence state-with-history)))
             (after-redo (nskk-sequence-test--simulate-redo after-undo)))
        ;; Check if we returned to original state
        (when (and after-undo after-redo)
          (unless (and (eq (nskk-state-mode after-redo) original-mode)
                       (string= (nskk-state-input-buffer after-redo) original-buffer))
            (push (list :seed test-seed
                        :run run
                        :original-mode original-mode
                        :original-buffer original-buffer
                        :final-mode (nskk-state-mode after-redo)
                        :final-buffer (nskk-state-input-buffer after-redo))
                  failures)))))
    (when failures
      (ert-fail (format "Undo/Redo invariant failed for %d cases (seed: %d):\n%S"
                        (length failures) test-seed
                        (take 3 failures))))))


;;;;
;;;; Property 7: Typing Japanese
;;;;

(defun nskk-sequence-test--simulate-japanese-input (state key-sequence)
  "Simulate Japanese input KEY-SEQUENCE on STATE.
Returns updated state with input accumulated."
  (let ((current-state state))
    (dolist (key key-sequence current-state)
      (when (and (stringp key) (= (length key) 1))
        (let ((char (aref key 0)))
          ;; Only process lowercase letters for Japanese input
          (when (and (<= ?a char) (<= char ?z))
            (let ((current-buffer (nskk-state-input-buffer current-state)))
              (nskk-state-set current-state 'input-buffer
                              (concat current-buffer key)))))))))

(ert-deftest nskk-sequence-typing-japanese-no-crash ()
  "Typing valid romaji sequence should not crash."
  (let ((runs 75)
        (errors nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "Sequence test 'typing-japanese-no-crash' seed: %d" test-seed)
    (dotimes (run runs)
      (condition-case err
          (let* ((state (nskk-state-create 'hiragana))
                 (romaji-seq (nskk-generate 'romaji-basic))
                 ;; Convert romaji string to key sequence
                 (key-seq (cl-loop for char across romaji-seq
                                   collect (char-to-string char)))
                 (final-state (nskk-sequence-test--simulate-japanese-input
                               state key-seq)))
            ;; Just verify we got a valid state back
            (unless (nskk-state-p final-state)
              (push (list :seed test-seed :run run :error "Invalid final state")
                    errors)))
        (error
         (push (list :seed test-seed :run run :error (error-message-string err))
               errors))))
    (when errors
      (ert-fail (format "Typing Japanese failed with %d errors (seed: %d):\n%S"
                        (length errors) test-seed
                        (take 3 errors))))))

(ert-deftest nskk-sequence-invalid-romaji-no-crash ()
  "Invalid romaji should not crash."
  (let ((runs 75)
        (errors nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "Sequence test 'invalid-romaji-no-crash' seed: %d" test-seed)
    (dotimes (run runs)
      (condition-case err
          (let* ((state (nskk-state-create 'hiragana))
                 ;; Generate potentially invalid sequences
                 (key-seq (nskk-generate 'key-sequence))
                 (final-state (nskk-sequence-test--execute-keys state key-seq)))
            ;; Verify state is still valid
            (unless (nskk-sequence-test--valid-state-p final-state)
              (push (list :seed test-seed :run run :error "State corrupted")
                    errors)))
        (error
         (push (list :seed test-seed :run run :error (error-message-string err))
               errors))))
    (when errors
      (ert-fail (format "Invalid romaji handling failed with %d errors (seed: %d):\n%S"
                        (length errors) test-seed
                        (take 3 errors))))))


;;;;
;;;; Property 8: Mode Switch Idempotent
;;;;

(defun nskk-sequence-test--get-mode-switch-key (target-mode)
  "Get the key that switches to TARGET-MODE."
  (pcase target-mode
    ('hiragana "C-j")
    ('katakana "q")
    ('latin "l")
    ('abbrev ";")
    (_ nil)))

(ert-deftest nskk-sequence-mode-switch-idempotent-toggle ()
  "Pressing mode switch key twice returns to original mode (for toggle keys)."
  (let ((runs 75)
        (failures nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "Sequence test 'mode-switch-idempotent-toggle' seed: %d" test-seed)
    (dotimes (run runs)
      ;; Test hiragana <-> katakana toggle with 'q' key
      (let* ((initial-mode (nskk-pbt--random-choice '(hiragana katakana)))
             (state (nskk-state-create initial-mode))
             ;; Press 'q' twice
             (after-first (nskk-sequence-test--process-key state "q"))
             (after-second (nskk-sequence-test--process-key after-first "q")))
        ;; After two 'q' presses, mode should return to initial
        (unless (eq (nskk-state-mode after-second) initial-mode)
          (push (list :seed test-seed
                      :run run
                      :initial-mode initial-mode
                      :after-first (nskk-state-mode after-first)
                      :after-second (nskk-state-mode after-second))
                failures))))
    (when failures
      (ert-fail (format "Mode switch idempotency failed for %d cases (seed: %d):\n%S"
                        (length failures) test-seed
                        (take 3 failures))))))

(ert-deftest nskk-sequence-mode-switch-consistent ()
  "Same mode switch key always produces same result."
  (let ((runs 75)
        (failures nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "Sequence test 'mode-switch-consistent' seed: %d" test-seed)
    (dotimes (run runs)
      (let* ((initial-mode (nskk-generate 'valid-mode))
             (switch-key (nskk-pbt--random-choice '("C-j" "l" ";")))
             (state1 (nskk-state-create initial-mode))
             (state2 (nskk-state-create initial-mode))
             (result1 (nskk-sequence-test--process-key state1 switch-key))
             (result2 (nskk-sequence-test--process-key state2 switch-key)))
        ;; Both should produce the same mode
        (unless (eq (nskk-state-mode result1) (nskk-state-mode result2))
          (push (list :seed test-seed
                      :run run
                      :initial-mode initial-mode
                      :switch-key switch-key
                      :result1-mode (nskk-state-mode result1)
                      :result2-mode (nskk-state-mode result2))
                failures))))
    (when failures
      (ert-fail (format "Mode switch consistency failed for %d cases (seed: %d):\n%S"
                        (length failures) test-seed
                        (take 3 failures))))))


;;;;
;;;; Additional Sequence Tests: Edge Cases
;;;;

(ert-deftest nskk-sequence-empty-sequence ()
  "Empty key sequence should leave state unchanged."
  (let ((runs 75)
        (failures nil))
    (dotimes (run runs)
      (let* ((initial-mode (nskk-generate 'valid-mode))
             (state (nskk-state-create initial-mode))
             (result (nskk-sequence-test--execute-keys state nil)))
        (unless (nskk-sequence-test--states-equal-p state result)
          (push (list :run run
                      :initial-mode initial-mode
                      :result-mode (nskk-state-mode result))
                failures))))
    (when failures
      (ert-fail (format "Empty sequence test failed for %d cases:\n%S"
                        (length failures) (take 3 failures))))))

(ert-deftest nskk-sequence-long-sequence-no-overflow ()
  "Very long sequences should not cause overflow or crash."
  (let ((runs 75)
        (errors nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "Sequence test 'long-sequence-no-overflow' seed: %d" test-seed)
    (dotimes (run runs)
      (condition-case err
          (let* ((state (nskk-state-create 'hiragana))
                 ;; Generate a long sequence (50-100 keys)
                 (long-seq (nskk-generate 'key-sequence-of-length
                                          (nskk-pbt--random-int 50 100)))
                 (final-state (nskk-sequence-test--execute-keys state long-seq)))
            ;; Verify state is still valid
            (unless (nskk-sequence-test--valid-state-p final-state)
              (push (list :seed test-seed :run run :error "Invalid state")
                    errors))
            ;; Verify buffer bounds
            (unless (nskk-sequence-test--buffer-bounds-p final-state)
              (push (list :seed test-seed :run run :error "Buffer overflow")
                    errors)))
        (error
         (push (list :seed test-seed :run run :error (error-message-string err))
               errors))))
    (when errors
      (ert-fail (format "Long sequence test failed with %d errors (seed: %d):\n%S"
                        (length errors) test-seed
                        (take 3 errors))))))

(ert-deftest nskk-sequence-mixed-typing-and-mode-switch ()
  "Mixed typing and mode switching should maintain state integrity."
  (let ((runs 75)
        (failures nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "Sequence test 'mixed-typing-mode-switch' seed: %d" test-seed)
    (dotimes (run runs)
      (let* ((state (nskk-state-create 'hiragana))
             (key-seq (nskk-generate 'key-sequence))
             (final-state (nskk-sequence-test--execute-keys state key-seq)))
        ;; State should always be valid regardless of sequence
        (unless (and (nskk-sequence-test--valid-state-p final-state)
                     (nskk-sequence-test--buffer-bounds-p final-state)
                     (nskk-sequence-test--mode-valid-p final-state))
          (push (list :seed test-seed
                      :run run
                      :key-sequence (take 10 key-seq)
                      :valid-p (nskk-sequence-test--valid-state-p final-state)
                      :bounds-p (nskk-sequence-test--buffer-bounds-p final-state)
                      :mode-p (nskk-sequence-test--mode-valid-p final-state))
                failures))))
    (when failures
      (ert-fail (format "Mixed sequence test failed for %d cases (seed: %d):\n%S"
                        (length failures) test-seed
                        (take 3 failures))))))


;;;;
;;;; Pure Typing Tests
;;;;

(ert-deftest nskk-sequence-pure-typing ()
  "Pure typing (letters only) should accumulate in buffer."
  (let ((runs 75)
        (failures nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "Sequence test 'pure-typing' seed: %d" test-seed)
    (dotimes (run runs)
      (let* ((state (nskk-state-create 'hiragana))
             (key-seq (nskk-generate 'typing-key-sequence))
             (final-state (nskk-sequence-test--execute-keys state key-seq))
             (buffer (nskk-state-input-buffer final-state))
             (expected-length (length key-seq)))
        ;; Buffer should contain all typed characters
        (unless (= (length buffer) expected-length)
          (push (list :seed test-seed
                      :run run
                      :key-count expected-length
                      :buffer-length (length buffer)
                      :buffer buffer)
                failures))))
    (when failures
      (ert-fail (format "Pure typing test failed for %d cases (seed: %d):\n%S"
                        (length failures) test-seed
                        (take 3 failures))))))


;;;;
;;;; Mode Switch Only Tests
;;;;

(ert-deftest nskk-sequence-mode-switch-only ()
  "Mode switches only should not affect input buffer."
  (let ((runs 75)
        (failures nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "Sequence test 'mode-switch-only' seed: %d" test-seed)
    (dotimes (run runs)
      (let* ((state (nskk-state-create 'hiragana))
             (mode-keys '("C-j" "q" "l" ";"))
             ;; Generate sequence of only mode switches
             (key-seq (cl-loop repeat (nskk-pbt--random-int 1 20)
                               collect (nskk-pbt--random-choice mode-keys)))
             (final-state (nskk-sequence-test--execute-keys state key-seq))
             (buffer (nskk-state-input-buffer final-state)))
        ;; Buffer should be empty (no character input)
        (unless (string-empty-p buffer)
          (push (list :seed test-seed
                      :run run
                      :key-sequence key-seq
                      :buffer buffer)
                failures))))
    (when failures
      (ert-fail (format "Mode switch only test failed for %d cases (seed: %d):\n%S"
                        (length failures) test-seed
                        (take 3 failures))))))


;;;;
;;;; Stack Size Bounded Tests
;;;;

(ert-deftest nskk-sequence-stack-size-bounded ()
  "Undo + Redo stack size should be bounded."
  (let ((runs 75)
        (failures nil)
        (max-stack-size 100)
        (test-seed (abs (random))))
    (random test-seed)
    (message "Sequence test 'stack-size-bounded' seed: %d" test-seed)
    (dotimes (run runs)
      (let* ((state (nskk-state-create 'hiragana))
             (key-seq (nskk-generate 'key-sequence-of-length
                                     (nskk-pbt--random-int 10 50)))
             (final-state (nskk-sequence-test--execute-keys state key-seq))
             (undo-size (length (nskk-state-undo-stack final-state)))
             (redo-size (length (nskk-state-redo-stack final-state)))
             (total-size (+ undo-size redo-size)))
        ;; Total stack size should be bounded
        (when (> total-size max-stack-size)
          (push (list :seed test-seed
                      :run run
                      :undo-size undo-size
                      :redo-size redo-size
                      :total-size total-size
                      :max-allowed max-stack-size)
                failures))))
    (when failures
      (ert-fail (format "Stack size bounded test failed for %d cases (seed: %d):\n%S"
                        (length failures) test-seed
                        (take 3 failures))))))


(provide 'nskk-sequence-test)

;;; nskk-sequence-test.el ends here
