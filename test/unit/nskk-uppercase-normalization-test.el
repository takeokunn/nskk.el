;;; nskk-uppercase-normalization-test.el --- Uppercase normalization tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test, uppercase

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Comprehensive tests for uppercase normalization during Japanese input.
;; Tests that uppercase letters (used to trigger henkan mode) are properly
;; normalized to lowercase before romaji conversion, preventing bugs where
;; "H O" produces "oお" instead of "▽ほ".

;;; Code:

(require 'ert)
(require 'nskk-input)
(require 'nskk-state)
(require 'nskk-converter)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;
;;; Helper Macros
;;;

(defmacro nskk-uppercase-test-with-state (initial-mode &rest body)
  "Execute BODY with a fresh state initialized to INITIAL-MODE.
Also initializes romaji table and enables auto-start-henkan."
  (declare (indent 1))
  `(let ((nskk-current-state (nskk-state-create ,initial-mode))
         (nskk--conversion-overlay nil)
         (nskk-converter-auto-start-henkan t))
     (nskk--initialize-romaji-table)
     ,@body))

(defmacro nskk-uppercase-test-with-romaji (&rest body)
  "Execute BODY with a fresh romaji buffer and standard romaji table."
  (declare (indent 0))
  `(progn
     (nskk--initialize-romaji-table)
     (let ((nskk--romaji-buffer ""))
       ,@body)))

;;;
;;; Phase 1: Basic Uppercase Normalization Tests
;;;

(nskk-describe "Phase 1: Basic uppercase normalization"
  (nskk-it "H O produces ▽ほ (the main bug case)"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?H 1)
         (should (nskk--conversion-start-active-p))
         (should (string-match-p "\u25BD" (buffer-string))))
        (nskk-when
         (nskk-process-japanese-input ?O 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "ho"))
         (should (string-match-p "\u25BD\u307B" (buffer-string))))))

  (nskk-it "K A produces ▽か"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?K 1)
         (should (nskk--conversion-start-active-p)))
        (nskk-when
         (nskk-process-japanese-input ?A 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "ka"))
         (should (string-match-p "\u25BD\u304B" (buffer-string))))))

  (nskk-it "S I produces ▽し"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?S 1)
         (should (nskk--conversion-start-active-p)))
        (nskk-when
         (nskk-process-japanese-input ?I 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "si"))
         (should (string-match-p "\u25BD\u3057" (buffer-string))))))

  (nskk-it "T U produces ▽つ"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?T 1)
         (should (nskk--conversion-start-active-p)))
        (nskk-when
         (nskk-process-japanese-input ?U 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "tu"))
         (should (string-match-p "\u25BD\u3064" (buffer-string))))))

  (nskk-it "N E produces ▽ね"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?N 1)
         (should (nskk--conversion-start-active-p)))
        (nskk-when
         (nskk-process-japanese-input ?E 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "ne"))
         (should (string-match-p "\u25BD\u306D" (buffer-string))))))

  (nskk-it "uppercase K followed by lowercase a produces ▽か"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?K 1)
         (should (nskk--conversion-start-active-p)))
        (nskk-when
         (nskk-process-japanese-input ?a 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "ka"))
         (should (string-match-p "\u25BD\u304B" (buffer-string))))))

  (nskk-it "uppercase S followed by lowercase i produces ▽し"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?S 1)
         (should (nskk--conversion-start-active-p)))
        (nskk-when
         (nskk-process-japanese-input ?i 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "si"))
         (should (string-match-p "\u25BD\u3057" (buffer-string)))))))

;;;
;;; Phase 2: Vowel Tests (all vowels with H)
;;;

(nskk-describe "Phase 2: Vowel tests with H"
  (nskk-it "H A produces ▽は"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?H 1))
        (nskk-when
         (nskk-process-japanese-input ?A 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "ha"))
         (should (string-match-p "\u25BD\u306F" (buffer-string))))))

  (nskk-it "H I produces ▽ひ"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?H 1))
        (nskk-when
         (nskk-process-japanese-input ?I 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "hi"))
         (should (string-match-p "\u25BD\u3072" (buffer-string))))))

  (nskk-it "H U produces ▽ふ"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?H 1))
        (nskk-when
         (nskk-process-japanese-input ?U 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "hu"))
         (should (string-match-p "\u25BD\u3075" (buffer-string))))))

  (nskk-it "H E produces ▽へ"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?H 1))
        (nskk-when
         (nskk-process-japanese-input ?E 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "he"))
         (should (string-match-p "\u25BD\u3078" (buffer-string))))))

  (nskk-it "H O produces ▽ほ"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?H 1))
        (nskk-when
         (nskk-process-japanese-input ?O 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "ho"))
         (should (string-match-p "\u25BD\u307B" (buffer-string)))))))

;;;
;;; Phase 3: Mixed Case Tests
;;;

(nskk-describe "Phase 3: Mixed case tests"
  (nskk-it "H o (uppercase H, lowercase o) produces ▽ほ"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?H 1))
        (nskk-when
         (nskk-process-japanese-input ?o 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "ho"))
         (should (string-match-p "\u25BD\u307B" (buffer-string))))))

  (nskk-it "h O (lowercase h, uppercase O) does NOT trigger henkan start"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?h 1))
        (nskk-when
         (should-not (nskk--conversion-start-active-p))
         (nskk-process-japanese-input ?O 1))
        (nskk-then
         ;; Second uppercase letter should trigger henkan
         (should (nskk--conversion-start-active-p))
         (should (equal nskk--romaji-buffer "ho"))))))

  (nskk-it "h o (all lowercase) does NOT trigger henkan start"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?h 1))
        (nskk-when
         (should-not (nskk--conversion-start-active-p))
         (nskk-process-japanese-input ?o 1))
        (nskk-then
         (should-not (nskk--conversion-start-active-p))
         (should (equal nskk--romaji-buffer ""))
         (should (string-match-p "\u307B" (buffer-string)))))))

;;;
;;; Phase 4: Okurigana Preservation Tests
;;;

(nskk-describe "Phase 4: Okurigana preservation"
  (nskk-it "H o K produces ▽ほ*k (okurigana should work)"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?H 1)
         (should (nskk--conversion-start-active-p)))
        (nskk-when
         (nskk-process-japanese-input ?o 1)
         (nskk-process-japanese-input ?K 1))
        (nskk-then
         ;; After "ho", the buffer should contain "ho"
         ;; Then uppercase K should trigger okurigana
         (should (string-match-p "\u25BD\u307B\\*k" (buffer-string))))))

  (nskk-it "H O K produces ▽ほ*k (uppercase O followed by uppercase K)"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?H 1)
         (should (nskk--conversion-start-active-p)))
        (nskk-when
         (nskk-process-japanese-input ?O 1)
         (nskk-process-japanese-input ?K 1))
        (nskk-then
         ;; After "HO", uppercase K should trigger okurigana
         (should (string-match-p "\u25BD\u307B\\*k" (buffer-string))))))

  (nskk-it "uppercase consonant after complete romaji triggers okurigana"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?K 1)
         (nskk-process-japanese-input ?A 1))
        (nskk-when
         ;; Now type uppercase K to trigger okurigana
         (nskk-process-japanese-input ?K 1))
        (nskk-then
         (should (string-match-p "\u25BD\u304B\\*k" (buffer-string)))))))

;;;
;;; Additional Edge Cases
;;;

(nskk-describe "Edge cases and corner cases"
  (nskk-it "multiple uppercase letters in sequence"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?K 1))
        (nskk-when
         ;; Second uppercase should not reset marker
         (let ((first-marker-pos (nskk--get-conversion-start)))
           (nskk-process-japanese-input ?K 1)
           (should (= (nskk--get-conversion-start) first-marker-pos))))))

  (nskk-it "uppercase letter when auto-start-henkan is disabled"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-overlay nil)
            (nskk-converter-auto-start-henkan nil))
        (nskk--initialize-romaji-table)
        (nskk-given
         (nskk-process-japanese-input ?K 1))
        (nskk-then
         (should-not (nskk--conversion-start-active-p))
         (should (equal nskk--romaji-buffer "k"))))))

  (nskk-it "uppercase letter followed by non-romaji character"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?H 1))
        (nskk-when
         ;; Space should not cause conversion
         (let ((result (nskk-convert-input-to-kana ?\s)))
           (should (stringp result))
           (should (string-empty-p result))))))

  (nskk-it "romaji buffer is cleared after complete conversion"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?H 1)
         (nskk-process-japanese-input ?O 1))
        (nskk-then
         ;; After "ho" conversion, buffer should contain the converted kana
         (should (string-match-p "\u25BD\u307B" (buffer-string))))))

  (nskk-it "uppercase normalization works in katakana mode"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'katakana
        (nskk-given
         (nskk-process-japanese-input ?H 1))
        (nskk-when
         (nskk-process-japanese-input ?O 1))
        (nskk-then
         (should (equal nskk--romaji-buffer "ho"))
         ;; Should display katakana ホ
         (should (string-match-p "\u25BD\u30DB" (buffer-string)))))))

;;;
;;; Integration Tests: Full Conversion Flow
;;;

(nskk-describe "Integration: full conversion flow with uppercase"
  (nskk-it "uppercase K A followed by Space triggers conversion"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        (nskk-given
         (nskk-process-japanese-input ?K 1)
         (nskk-process-japanese-input ?A 1))
        (nskk-when
         ;; Simulate space press to trigger conversion
         (let ((nskk-current-state nskk-current-state))
           ;; Space should initiate conversion
           (should (string-match-p "\u25BD\u304B" (buffer-string)))))))

  (nskk-it "consecutive uppercase-started conversions"
    (with-temp-buffer
      (nskk-uppercase-test-with-state 'hiragana
        ;; First word
        (nskk-given
         (nskk-process-japanese-input ?H 1)
         (nskk-process-japanese-input ?O 1))
        (nskk-when
         (should (string-match-p "\u25BD\u307B" (buffer-string)))
         ;; Commit first word
         (nskk-state-force-henkan-phase nskk-current-state nil)
         (nskk--clear-conversion-start-marker)
         (erase-buffer)
         ;; Second word
         (nskk-process-japanese-input ?K 1)
         (nskk-process-japanese-input ?A 1))
        (nskk-then
         (should (string-match-p "\u25BD\u304B" (buffer-string)))))))

;;;
;;; Regression Test: Main Bug Case
;;;

(ert-deftest nskk-regression-uppercase-ho-bug ()
  "Regression test: H O should produce ▽ほ (not oお)"
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--conversion-overlay nil)
          (nskk-converter-auto-start-henkan t)
          (nskk--romaji-buffer ""))
      (nskk--initialize-romaji-table)
      (nskk-process-japanese-input ?H 1)
      (nskk-process-japanese-input ?O 1)
      (should (equal nskk--romaji-buffer "ho"))
      (should (string-match-p "\u25BD\u307B" (buffer-string)))
      ;; Verify it does NOT produce "oお"
      (should-not (string-match-p "o\u304A" (buffer-string))))))

(provide 'nskk-uppercase-normalization-test)

;;; nskk-uppercase-normalization-test.el ends here
