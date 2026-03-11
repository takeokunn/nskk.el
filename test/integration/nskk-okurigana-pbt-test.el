;;; nskk-okurigana-pbt-test.el --- Okurigana PBT tests -*- lexical-binding: t; -*-

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

;; Property-based tests for okurigana detection and state management.
;;
;; This file tests invariants of the okurigana subsystem:
;; - Uppercase letters A-Z are detected as okurigana markers
;; - Lowercase letters are not detected as okurigana markers
;; - Non-alpha characters are not detected as okurigana markers
;; - Setting and getting okurigana in state is consistent
;;
;; Properties tested:
;; - okurigana-uppercase-detected: All A-Z are detected as okurigana markers
;; - okurigana-lowercase-not-detected: Lowercase a-z return nil
;; - okurigana-non-alpha-not-detected: Numbers, symbols return nil
;; - okurigana-state-set-get-roundtrip: Set/get okurigana in state is consistent

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-state)
(require 'nskk-input)


;;;;
;;;; Property 1: Uppercase Characters Detected as Okurigana
;;;;

(nskk-property-test nskk-state-machine-okurigana-uppercase-detected
  ((ch uppercase-char))
  (let* ((char (string-to-char ch))
         (result (nskk-detect-okurigana-char char)))
    (and result
         (characterp result)
         (= result (downcase char))))
  50)


;;;;
;;;; Property 2: Lowercase Characters Not Detected as Okurigana
;;;;

(nskk-property-test nskk-state-machine-okurigana-lowercase-not-detected
  ((ch lowercase-char))
  (let* ((char (string-to-char ch))
         (result (nskk-detect-okurigana-char char)))
    (not result))
  50)


;;;;
;;;; Property 3: Non-Alpha Characters Not Detected as Okurigana
;;;;

(nskk-property-test nskk-state-machine-okurigana-non-alpha-not-detected
  ((ch non-alpha-char))
  (let* ((char (string-to-char ch))
         (result (nskk-detect-okurigana-char char)))
    (not result))
  50)


;;;;
;;;; Property 4: Okurigana State Set-Get Roundtrip
;;;;

(nskk-property-test nskk-state-machine-okurigana-state-set-get-roundtrip
  ((ch lowercase-char))
  (let* ((state (nskk-state-create 'hiragana))
         (okuri-char (string-to-char ch)))
    (nskk-state-set-okurigana state okuri-char)
    (equal (nskk-state-get-okurigana state) okuri-char))
  50)


;;;;
;;;; Additional Property: Okurigana Detection Determinism
;;;;

(nskk-property-test nskk-state-machine-okurigana-detection-deterministic
  ((ch any-char))
  (let* ((char (string-to-char ch))
         (result1 (nskk-detect-okurigana-char char))
         (result2 (nskk-detect-okurigana-char char)))
    (equal result1 result2))
  50)


;;;;
;;;; Additional Property: Okurigana Nil State Safety
;;;;

(nskk-property-test nskk-state-machine-okurigana-nil-state-safe
  ((ch lowercase-char))
  (let ((okuri-char (string-to-char ch)))
    ;; These should return nil without crash
    (nskk-state-set-okurigana nil okuri-char)
    (nskk-state-get-okurigana nil)
    ;; Also test detect on nil
    (nskk-detect-okurigana-char nil)
    t)
  50)


;;;;
;;;; Exhaustive: All 26 uppercase letters detected as okurigana triggers
;;;;

(nskk-property-test-exhaustive uppercase-letters-are-okurigana-triggers
  (number-sequence ?A ?Z)
  (let ((result (nskk-detect-okurigana-char item)))
    (and result
         (characterp result)
         (= result (downcase item)))))


;;;;
;;;; Seeded PBT: Okurigana full-pattern — consonant stored in state
;;;;

(nskk-property-test-seeded okurigana-full-pattern-state-roundtrip
  ((pattern okurigana-full-pattern))
  (let* ((consonant (plist-get pattern :consonant))
         (state (nskk-state-create 'hiragana))
         (detected (nskk-detect-okurigana-char consonant)))
    (when detected
      (nskk-state-set-okurigana state detected)
      (equal (nskk-state-get-okurigana state) detected)))
  50)


;;;;
;;;; Exhaustive: Consonant-lowercase symmetry for the 14 okurigana consonants
;;;;

(nskk-property-test-exhaustive okurigana-consonant-lowercase-symmetry
  '(?K ?S ?T ?N ?H ?M ?Y ?R ?W ?G ?Z ?D ?B ?P)
  (equal (nskk-detect-okurigana-char item) (downcase item)))


(provide 'nskk-okurigana-pbt-test)

;;; nskk-okurigana-pbt-test.el ends here
