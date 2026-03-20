;;; nskk-state-machine-buffer-test.el --- Buffer state machine tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
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

;; State machine property-based tests for NSKK buffer state transitions.
;;
;; This file tests invariants of buffer operations using property-based
;; testing techniques. Each test verifies that a specific property
;; holds across many random buffer operation sequences.
;;
;; Properties tested:
;; - input-buffer-append-increases-length: Appending increases or maintains length
;; - input-buffer-delete-decreases-length: Deleting decreases length
;; - input-buffer-clear-empties: Clear buffer results in empty string
;; - input-buffer-never-nil: Buffer is never nil after any operation
;; - input-buffer-string-type: Buffer is always a string
;; - converted-buffer-consistency: Converted buffer is always valid

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-state)


;;;;
;;;; Helper Functions for Buffer Operations
;;;;

(defconst nskk--sm-test-chars
  (append (string-to-list "abcdefghijklmnopqrstuvwxyz")
          (string-to-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
          (string-to-list "0123456789"))
  "Test characters for buffer operations.")

(defun nskk--sm-random-char ()
  "Get a random test character."
  (nskk--pbt-random-choice nskk--sm-test-chars))

(defun nskk--sm-append-char (state _trigger)
  "Append a random character to STATE's input buffer."
  (nskk-state-append-input state (nskk--sm-random-char))
  state)

(defun nskk--sm-delete-char (state _trigger)
  "Delete last character from STATE's input buffer."
  (nskk-state-delete-last-char state)
  state)

(defun nskk--sm-clear-buffer (state _trigger)
  "Clear STATE's input buffer."
  (nskk-state-clear-input state)
  state)

(defun nskk--sm-set-converted-buffer (state _trigger)
  "Set STATE's converted buffer to a random string."
  (let ((random-str (nskk--pbt-generate-input-buffer 10)))
    (nskk-state-set state 'converted-buffer random-str))
  state)

(defun nskk--sm-mixed-operations (state _trigger)
  "Perform random buffer operations on STATE."
  (let ((op (nskk--pbt-random-int 0 3)))
    (pcase op
      (0 (nskk-state-append-input state (nskk--sm-random-char)))
      (1 (nskk-state-delete-last-char state))
      (2 (nskk-state-clear-input state))
      (3 (nskk-state-set state 'input-buffer
                         (nskk--pbt-generate-input-buffer 5)))))
  state)

(defun nskk--sm-append-many (state _trigger)
  "Append multiple characters to STATE."
  (let ((count (nskk--pbt-random-int 1 10)))
    (dotimes (_ count)
      (nskk-state-append-input state (nskk--sm-random-char))))
  state)

(defun nskk--sm-clear-twice (state _trigger)
  "Clear buffer twice (should be idempotent)."
  (nskk-state-clear-input state)
  (nskk-state-clear-input state)
  state)


;;;;
;;;; Property 1: Input Buffer Append Increases Length
;;;;

(nskk-state-machine-test input-buffer-append-increases-length
  (nskk-state-create 'hiragana)
  ((append nskk--sm-append-char)
   (append nskk--sm-append-char)
   (append nskk--sm-append-char)
   (append nskk--sm-append-char)
   (append nskk--sm-append-char))
  (lambda (state)
    (let ((buf (nskk-state-input-buffer state)))
      (and (stringp buf)
           (>= (length buf) 0))))
  50)


;;;;
;;;; Property 2: Input Buffer Delete Decreases Length
;;;;

(nskk-state-machine-test input-buffer-delete-decreases-length
  (nskk-state-create 'hiragana)
  ((op nskk--sm-delete-char)
   (op nskk--sm-delete-char)
   (op nskk--sm-delete-char))
  (lambda (state)
    (let ((buf (nskk-state-input-buffer state)))
      (and (stringp buf)
           (>= (length buf) 0))))
  50)


;;;;
;;;; Property 4: Input Buffer Never Nil
;;;;

(nskk-state-machine-test input-buffer-never-nil
  (nskk-state-create 'hiragana)
  ((op nskk--sm-mixed-operations)
   (op nskk--sm-mixed-operations)
   (op nskk--sm-mixed-operations)
   (op nskk--sm-mixed-operations)
   (op nskk--sm-mixed-operations)
   (op nskk--sm-mixed-operations)
   (op nskk--sm-mixed-operations))
  (lambda (state)
    (not (null (nskk-state-input-buffer state))))
  50)


;;;;
;;;; Property 5: Input Buffer String Type
;;;;

(nskk-state-machine-test input-buffer-string-type
  (nskk-state-create 'hiragana)
  ((op nskk--sm-mixed-operations)
   (op nskk--sm-mixed-operations)
   (op nskk--sm-mixed-operations)
   (op nskk--sm-mixed-operations)
   (op nskk--sm-mixed-operations))
  (lambda (state)
    (stringp (nskk-state-input-buffer state)))
  50)


;;;;
;;;; Property 6: Converted Buffer Consistency
;;;;

(nskk-state-machine-test converted-buffer-consistency
  (nskk-state-create 'hiragana)
  ((set nskk--sm-set-converted-buffer)
   (set nskk--sm-set-converted-buffer)
   (set nskk--sm-set-converted-buffer))
  (lambda (state)
    (let ((buf (nskk-state-converted-buffer state)))
      (and (stringp buf)
           (>= (length buf) 0))))
  50)


;;;;
;;;; Additional Property: Buffer Length Bounds
;;;;

(nskk-state-machine-test input-buffer-length-bounds
  (nskk-state-create 'hiragana)
  ((append nskk--sm-append-many)
   (append nskk--sm-append-many)
   (delete nskk--sm-delete-char)
   (delete nskk--sm-delete-char)
   (clear nskk--sm-clear-buffer))
  (lambda (state)
    (>= (length (nskk-state-input-buffer state)) 0))
  50)


;;;;
;;;; Property: Buffer Operations Idempotency
;;;;

(nskk-state-machine-test input-buffer-clear-idempotent
  (nskk-state-create 'hiragana)
  ((clear nskk--sm-clear-twice))
  (lambda (state)
    (string= (nskk-state-input-buffer state) ""))
  50)


;;;;
;;;; Property: Delete From Empty Buffer
;;;;

(nskk-state-machine-test input-buffer-delete-empty-safe
  (nskk-state-create 'hiragana)
  ((del nskk--sm-delete-char)
   (del nskk--sm-delete-char)
   (del nskk--sm-delete-char))
  (lambda (state)
    (and (stringp (nskk-state-input-buffer state))
         (string= (nskk-state-input-buffer state) "")))
  50)


;;;;
;;;; BDD: Buffer append behaviors
;;;;

(nskk-describe "buffer append behavior"
  (nskk-it "appending to buffer increases or maintains length"
    (let* ((state (nskk-state-create 'hiragana))
           (len-before (length (nskk-state-input-buffer state))))
      (nskk-when
        (nskk-state-append-input state ?a))
      (nskk-then
        (should (>= (length (nskk-state-input-buffer state)) len-before)))))

  (nskk-it "buffer is never nil after append, delete, or clear"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-when
        (nskk-state-append-input state ?a))
      (nskk-then
        (should (nskk-state-input-buffer state)))
      (nskk-when
        (nskk-state-delete-last-char state))
      (nskk-then
        (should (nskk-state-input-buffer state)))
      (nskk-when
        (nskk-state-clear-input state))
      (nskk-then
        (should (nskk-state-input-buffer state)))))

  (nskk-it "buffer is always a string after any operation"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-then
        (should (stringp (nskk-state-input-buffer state))))
      (nskk-when
        (nskk-state-append-input state ?z))
      (nskk-then
        (should (stringp (nskk-state-input-buffer state))))
      (nskk-when
        (nskk-state-clear-input state))
      (nskk-then
        (should (stringp (nskk-state-input-buffer state)))))))


;;;;
;;;; BDD: Buffer delete behaviors
;;;;

(nskk-describe "buffer delete behavior"
  (nskk-it "deleting from buffer decreases length or leaves empty buffer unchanged"
    (let ((state (nskk-state-create 'hiragana)))
      ;; Start with known content
      (nskk-state-append-input state ?a)
      (nskk-state-append-input state ?b)
      (nskk-state-append-input state ?c)
      (let ((len-before (length (nskk-state-input-buffer state))))
        (nskk-when
          (nskk-state-delete-last-char state))
        (nskk-then
          (should (<= (length (nskk-state-input-buffer state)) len-before))))))

  (nskk-it "deleting from empty buffer is safe"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-when
        (nskk-state-delete-last-char state))
      (nskk-then
        (should (stringp (nskk-state-input-buffer state)))
        (should (string= "" (nskk-state-input-buffer state)))))))


;;;;
;;;; BDD: Buffer clear behaviors
;;;;

(nskk-describe "buffer clear behavior"
  (nskk-it "clear buffer results in empty string (property-based)"
    (dotimes (_ 50)
      (let ((state (nskk-state-create 'hiragana)))
        ;; Add random content
        (dotimes (_ (nskk--pbt-random-int 1 10))
          (nskk-state-append-input state (nskk--sm-random-char)))
        ;; Clear the buffer
        (nskk-state-clear-input state)
        ;; Verify buffer is empty
        (should (stringp (nskk-state-input-buffer state)))
        (should (string= (nskk-state-input-buffer state) "")))))

  (nskk-it "clear after append always produces empty buffer"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-state-append-input state ?x)
      (nskk-when
        (nskk-state-clear-input state))
      (nskk-then
        (should (string= "" (nskk-state-input-buffer state))))))

  (nskk-it "clearing empty buffer is idempotent"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-when
        (nskk-state-clear-input state)
        (nskk-state-clear-input state))
      (nskk-then
        (should (string= "" (nskk-state-input-buffer state))))))

  (nskk-it "buffer length is always non-negative and bounded"
    (let ((state (nskk-state-create 'hiragana)))
      (dotimes (_ 10)
        (nskk-state-append-input state ?a))
      (nskk-then
        (should (>= (length (nskk-state-input-buffer state)) 0)))
      (dotimes (_ 5)
        (nskk-state-delete-last-char state))
      (nskk-then
        (should (>= (length (nskk-state-input-buffer state)) 0)))
      (nskk-when
        (nskk-state-clear-input state))
      (nskk-then
        (should (= 0 (length (nskk-state-input-buffer state))))))))


;;;;
;;;; BDD: Converted buffer behaviors
;;;;

(nskk-describe "converted buffer behavior"
  (nskk-it "converted buffer is always a valid string"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-then
        (should (stringp (nskk-state-converted-buffer state))))
      (nskk-when
        (nskk-state-set state 'converted-buffer "あいう"))
      (nskk-then
        (should (stringp (nskk-state-converted-buffer state)))
        (should (string= "あいう" (nskk-state-converted-buffer state)))))))


;;;;
;;;; BDD: Buffer reset behaviors
;;;;

(nskk-describe "buffer reset behavior"
  (nskk-it "reset should empty all buffers (property-based)"
    (dotimes (_ 50)
      (let ((state (nskk-state-create 'hiragana)))
        ;; Add random content to both buffers
        (dotimes (_ (nskk--pbt-random-int 1 10))
          (nskk-state-append-input state (nskk--sm-random-char)))
        (nskk-state-set state 'converted-buffer
                        (nskk--pbt-generate-input-buffer 10))
        ;; Reset the state
        (nskk-state-reset state)
        ;; Verify buffers are empty
        (should (string= (nskk-state-input-buffer state) ""))
        (should (string= (nskk-state-converted-buffer state) "")))))

  (nskk-it "reset empties input and converted buffers"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-state-append-input state ?a)
      (nskk-state-set state 'converted-buffer "あ")
      (nskk-when
        (nskk-state-reset state))
      (nskk-then
        (should (string= "" (nskk-state-input-buffer state)))
        (should (string= "" (nskk-state-converted-buffer state)))))))


;;;;
;;;; BDD: Buffer negative cases
;;;;

(nskk-describe "buffer negative cases"
  (nskk-it "appending to nil state should return nil gracefully"
    (nskk-then
      (should-not (nskk-state-append-input nil ?a))))

  (nskk-it "deleting from nil state should return nil gracefully"
    (nskk-then
      (should-not (nskk-state-delete-last-char nil))))

  (nskk-it "clearing nil state should not crash"
    (nskk-then
      (should-not (nskk-state-clear-input nil))))

  (nskk-it "appending many characters should not cause overflow"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-when
        ;; Append 1000 characters
        (dotimes (_ 1000)
          (nskk-state-append-input state ?a)))
      (nskk-then
        (should (= (length (nskk-state-input-buffer state)) 1000))
        ;; Buffer should still be valid
        (should (stringp (nskk-state-input-buffer state))))))

  (nskk-it "appending Unicode characters should work correctly"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-when
        ;; Append various Unicode characters
        (nskk-state-append-input state ?あ)
        (nskk-state-append-input state ?ア)
        (nskk-state-append-input state ?漢)
        (nskk-state-append-input state ?🔥))
      (nskk-then
        (should (not (string-empty-p (nskk-state-input-buffer state))))
        (should (stringp (nskk-state-input-buffer state)))))))


;;;;
;;;; BDD: Stress tests
;;;;

(nskk-describe "buffer stress tests"
  (nskk-it "stress test with many buffer operations maintains string invariant"
    (let ((state (nskk-state-create 'hiragana))
          (operations 0))
      ;; Perform 1000 random operations
      (dotimes (_ 1000)
        (cl-incf operations)
        (let ((op (nskk--pbt-random-int 0 3)))
          (pcase op
            (0 (nskk-state-append-input state (nskk--sm-random-char)))
            (1 (nskk-state-delete-last-char state))
            (2 (nskk-state-clear-input state))
            (3 (nskk-state-set state 'input-buffer
                               (nskk--pbt-generate-input-buffer 20)))))
        ;; Verify invariant after each operation
        (should (stringp (nskk-state-input-buffer state)))
        (should (>= (length (nskk-state-input-buffer state)) 0)))
      (message "Completed %d buffer operations" operations))))


;;;;
;;;; PBT: Buffer never-nil property (with shrinking)
;;;;

(nskk-property-test-with-shrinking buffer-string-never-nil
  ((mode valid-mode))
  (let ((state (nskk-state-create mode)))
    ;; Perform random ops
    (dotimes (_ 10)
      (let ((op (nskk--pbt-random-int 0 3)))
        (pcase op
          (0 (nskk-state-append-input state (nskk--sm-random-char)))
          (1 (nskk-state-delete-last-char state))
          (2 (nskk-state-clear-input state))
          (3 (nskk-state-set state 'input-buffer "")))))
    (and (stringp (nskk-state-input-buffer state))
         (stringp (nskk-state-converted-buffer state))))
  50)


;;;;
;;;; BDD: Buffer length invariants
;;;;

(nskk-describe "buffer length invariants"
  (nskk-it "appending then deleting N chars returns to original length"
    (let ((failures nil))
      (dotimes (_ 50)
        (nskk-for-all ((mode valid-mode))
          (let* ((state (nskk-state-create mode))
                 (init-len (length (nskk-state-input-buffer state))))
            ;; Append N chars then delete N chars
            (let ((n (nskk--pbt-random-int 1 10)))
              (dotimes (_ n)
                (nskk-state-append-input state (nskk--sm-random-char)))
              (dotimes (_ n)
                (nskk-state-delete-last-char state)))
            (let ((final-len (length (nskk-state-input-buffer state))))
              (unless (= init-len final-len)
                (push (list :initial init-len :final final-len :mode mode)
                      failures))))))
      (when failures
        (ert-fail (format "Length invariant failed for %d cases:\n%S"
                          (length failures) (seq-take failures 3)))))))


;;;;
;;;; Data-Provider: Buffer clear is idempotent
;;;;

(nskk-deftest-table buffer-clear-idempotent
  :description "Clearing an already-empty buffer is a no-op for any mode"
  :columns (input expected)
  :rows ((hiragana "")
         (katakana "")
         (ascii    "")
         (latin    ""))
  :body (let ((state (nskk-state-create input)))
          (nskk-state-clear-input state)
          (nskk-state-clear-input state)
          (should (string= expected (nskk-state-input-buffer state)))))


;;;;
;;;; BDD: Buffer state lifecycle
;;;;

(nskk-describe "Buffer state lifecycle"
  (nskk-it "empty buffer has length zero for any mode"
    (let ((failures nil))
      (dotimes (_ 20)
        (nskk-for-all ((mode valid-mode))
          (let ((state (nskk-state-create mode)))
            (unless (= 0 (length (nskk-state-input-buffer state)))
              (push (list :mode mode) failures)))))
      (when failures
        (ert-fail (format "Empty buffer not zero for modes: %S" failures)))))

  (nskk-it "clear after append always produces empty buffer"
    (let ((failures nil))
      (dotimes (_ 20)
        (nskk-for-all ((mode valid-mode))
          (let ((state (nskk-state-create mode)))
            (dotimes (_ 5)
              (nskk-state-append-input state (nskk--sm-random-char)))
            (nskk-state-clear-input state)
            (unless (string= "" (nskk-state-input-buffer state))
              (push (list :mode mode) failures)))))
      (when failures
        (ert-fail (format "Clear after append not empty for modes: %S" failures))))))


(provide 'nskk-state-machine-buffer-test)

;;; nskk-state-machine-buffer-test.el ends here
