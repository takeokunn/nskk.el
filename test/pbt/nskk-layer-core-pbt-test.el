;;; nskk-layer-core-pbt-test.el --- Property-based tests for Core Engine layer -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

;; Author: nskk.el contributors
;; Keywords: input method, japanese, property-based testing
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
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

;; Property-based tests for the Core Engine layer of nskk.el.
;;
;; This file tests the following properties:
;; - Romaji conversion properties (determinism, non-nil results, length bounds)
;; - Kana conversion properties (reversibility, length preservation)
;; - Character classification properties (hiragana, katakana, japanese detection)
;; - Width conversion properties (zenkaku/hankaku length handling)

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-converter)
(require 'nskk-core)

;;;
;;; Register Additional Generators for Core Layer Tests
;;;

;; These generators use Emacs' built-in (random) which is seeded by the
;; nskk-property-test-seeded macro. We use (abs (random)) to ensure
;; positive values where needed.

;; Initialize random state with a positive seed to ensure (random)
;; returns positive numbers when called without a limit.
(random (abs (random)))

(nskk-register-generator 'katakana-string
  (lambda (&optional length)
    (let ((chars '("ア" "イ" "ウ" "エ" "オ"
                   "カ" "キ" "ク" "ケ" "コ"
                   "サ" "シ" "ス" "セ" "ソ"
                   "タ" "チ" "ツ" "テ" "ト"
                   "ナ" "ニ" "ヌ" "ネ" "ノ"))
          (len (or length (+ 1 (abs (random 10))))))
      (mapconcat #'identity
                 (cl-loop repeat len
                          collect (nth (abs (random (length chars))) chars))
                 ""))))

(nskk-register-generator 'zenkaku-katakana-string
  (lambda (&optional length)
    (let ((chars '("ア" "イ" "ウ" "エ" "オ"
                   "カ" "キ" "ク" "ケ" "コ"
                   "サ" "シ" "ス" "セ" "ソ"
                   "タ" "チ" "ツ" "テ" "ト"
                   "ガ" "ギ" "グ" "ゲ" "ゴ"))
          (len (or length (+ 1 (abs (random 10))))))
      (mapconcat #'identity
                 (cl-loop repeat len
                          collect (nth (abs (random (length chars))) chars))
                 ""))))

(nskk-register-generator 'zenkaku-katakana-simple-string
  (lambda (&optional length)
    (let ((chars '("ア" "イ" "ウ" "エ" "オ"
                   "カ" "キ" "ク" "ケ" "コ"
                   "サ" "シ" "ス" "セ" "ソ"
                   "タ" "チ" "ツ" "テ" "ト"))
          (len (or length (+ 1 (abs (random 10))))))
      (mapconcat #'identity
                 (cl-loop repeat len
                          collect (nth (abs (random (length chars))) chars))
                 ""))))

(nskk-register-generator 'hankaku-katakana-string
  (lambda (&optional length)
    (let ((chars '("ｱ" "ｲ" "ｳ" "ｴ" "ｵ"
                   "ｶ" "ｷ" "ｸ" "ｹ" "ｺ"
                   "ｻ" "ｼ" "ｽ" "ｾ" "ｿ"
                   "ﾀ" "ﾁ" "ﾂ" "ﾃ" "ﾄ"
                   "ｶﾞ" "ｷﾞ" "ｸﾞ" "ｹﾞ" "ｺﾞ"))
          (len (or length (+ 1 (abs (random 10))))))
      (mapconcat #'identity
                 (cl-loop repeat len
                          collect (nth (abs (random (length chars))) chars))
                 ""))))

(nskk-register-generator 'japanese-char
  (lambda ()
    (let ((ranges '((#x3040 . #x309F)  ; Hiragana
                    (#x30A0 . #x30FF)  ; Katakana
                    (#x4E00 . #x9FFF)  ; CJK Unified Ideographs
                    (#x3400 . #x4DBF)  ; CJK Extension A
                    (#xFF65 . #xFF9F)))) ; Half-width Katakana
      (let* ((range (nth (abs (random (length ranges))) ranges))
             (start (car range))
             (end (cdr range)))
        (+ start (abs (random (- end start))))))))

(nskk-register-generator 'any-char
  (lambda ()
    (let ((ranges '((#x0000 . #x007F)  ; Basic Latin
                    (#x0080 . #x00FF)  ; Latin-1 Supplement
                    (#x3040 . #x309F)  ; Hiragana
                    (#x30A0 . #x30FF)  ; Katakana
                    (#x4E00 . #x9FFF)  ; CJK Unified Ideographs
                    (#x3400 . #x4DBF)  ; CJK Extension A
                    (#xFF65 . #xFF9F)))) ; Half-width Katakana
      (let* ((range (nth (abs (random (length ranges))) ranges))
             (start (car range))
             (end (cdr range)))
        (+ start (abs (random (- end start))))))))

;;; Romaji Conversion Properties

(nskk-property-test-seeded romaji-conversion-deterministic
  ((input romaji-pattern))
  (let* ((result1 (nskk-convert-romaji input))
         (result2 (nskk-convert-romaji input)))
    (equal result1 result2))
  100)

(nskk-property-test-seeded romaji-conversion-non-nil
  ((input romaji-pattern))
  (let* ((result (nskk-convert-romaji input)))
    (not (null result)))
  100)

(nskk-property-test-seeded romaji-conversion-length-bound
  ((input romaji-pattern))
  (let* ((result (nskk-convert-romaji input)))
    (when (and result (> (length input) 0))
      (let ((input-len (length input))
            (output-len (length result)))
        ;; Output should be at least 1/4 of input length
        ;; (accounting for multi-character romaji sequences)
        (>= output-len (/ input-len 4)))))
  100)

;;; Kana Conversion Properties

(nskk-property-test-seeded hiragana-to-katakana-reversible
  ((original hiragana-string))
  (let* ((katakana (nskk-core-hiragana-to-katakana original))
         (roundtrip (nskk-core-katakana-to-hiragana katakana)))
    (equal original roundtrip))
  100)

(nskk-property-test-seeded katakana-to-hiragana-reversible
  ((original katakana-string))
  (let* ((hiragana (nskk-core-katakana-to-hiragana original))
         (roundtrip (nskk-core-hiragana-to-katakana hiragana)))
    (equal original roundtrip))
  100)

(nskk-property-test-seeded kana-conversion-length-preserved
  ((original hiragana-string))
  (let* ((katakana (nskk-core-hiragana-to-katakana original))
         (roundtrip (nskk-core-katakana-to-hiragana katakana)))
    (and (= (length original) (length katakana))
         (= (length original) (length roundtrip))))
  100)

;;; Character Classification Properties

(nskk-property-test-seeded hiragana-p-correct
  ((char japanese-char))
  (let* ((is-hiragana (nskk-core-hiragana-p char)))
    ;; Verify against character code range
    (let ((in-hiragana-range (and (>= char #x3040) (<= char #x309F))))
      (eq is-hiragana in-hiragana-range)))
  100)

(nskk-property-test-seeded katakana-p-correct
  ((char japanese-char))
  (let* ((is-katakana (nskk-core-katakana-p char)))
    ;; Verify against character code range
    (let ((in-katakana-range (and (>= char #x30A0) (<= char #x30FF))))
      (eq is-katakana in-katakana-range)))
  100)

(nskk-property-test-seeded japanese-p-exhaustive
  ((char any-char))
  (let* ((is-japanese (nskk-core-japanese-p char)))
    ;; Check against known Japanese character ranges
    (let ((in-japanese-range
           (or
            ;; Hiragana
            (and (>= char #x3040) (<= char #x309F))
            ;; Katakana
            (and (>= char #x30A0) (<= char #x30FF))
            ;; CJK Unified Ideographs (Kanji)
            (and (>= char #x4E00) (<= char #x9FFF))
            ;; CJK Unified Ideographs Extension A
            (and (>= char #x3400) (<= char #x4DBF))
            ;; Half-width Katakana
            (and (>= char #xFF65) (<= char #xFF9F)))))
      (eq is-japanese in-japanese-range)))
  100)

;;; Width Conversion Properties

(nskk-property-test-seeded zenkaku-hankaku-length
  ((zenkaku zenkaku-katakana-string))
  (let* ((hankaku (nskk-core-zenkaku-to-hankaku zenkaku)))
    (when (and zenkaku hankaku)
      ;; Hankaku length should be >= zenkaku length
      ;; (some zenkaku chars expand to 2 hankaku chars with dakuten)
      (>= (length hankaku) (length zenkaku))))
  100)

(nskk-property-test-seeded hankaku-zenkaku-length
  ((hankaku hankaku-katakana-string))
  (let* ((zenkaku (nskk-core-hankaku-to-zenkaku hankaku)))
    (when (and hankaku zenkaku)
      ;; Zenkaku length should be <= hankaku length
      ;; (hankaku dakuten pairs combine)
      (<= (length zenkaku) (length hankaku))))
  100)

(nskk-property-test-seeded width-conversion-roundtrip
  ((original zenkaku-katakana-simple-string))
  (let* ((hankaku (nskk-core-zenkaku-to-hankaku original))
         (roundtrip (nskk-core-hankaku-to-zenkaku hankaku)))
    (when (and original hankaku roundtrip)
      ;; Roundtrip should preserve length for simple katakana
      (= (length original) (length roundtrip))))
  100)

(provide 'nskk-layer-core-pbt-test)

;;; nskk-layer-core-pbt-test.el ends here
