;;; nskk-sequence-test.el --- Sequence-based property tests for NSKK -*- lexical-binding: t; -*-

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
(require 'nskk-input)

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


(provide 'nskk-sequence-test)

;;; nskk-sequence-test.el ends here
