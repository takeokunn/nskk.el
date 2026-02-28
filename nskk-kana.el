;;; nskk-kana.el --- Core conversion utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

;; This file is NOT part of GNU Emacs.

;; This file is part of NSKK (Next-generation SKK).
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides core conversion utilities for NSKK.
;; It handles:
;; - Hiragana to katakana conversion
;; - Katakana to hiragana conversion
;; - Zenkaku (full-width) to hankaku (half-width) conversion
;; - Hankaku to zenkaku conversion
;; - Character classification functions
;;
;; Performance target: < 0.01ms per character conversion
;; Memory: Minimal overhead with vector-based lookups

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'nskk-macros))

(defgroup nskk-kana nil
  "Kana conversion and state management settings."
  :prefix "nskk-"
  :group 'nskk)

;; Character code ranges for classification
;; Based on Unicode Standard 15.0
;;
;; Hiragana: #x3040-#x309F
;;   - Includes: Hiragana letters, small letters, iteration marks,
;;     voiced/semi-voiced sound marks
;;
;; Katakana: #x30A0-#x30FF
;;   - Includes: Katakana letters, small letters, extension letters,
;;     voiced/semi-voiced sound marks, iteration marks
;;
;; CJK Unified Ideographs (Kanji): #x4E00-#x9FFF
;;   - Main CJK block containing common kanji
;;
;; CJK Unified Ideographs Extension A: #x3400-#x4DBF
;;   - Rare/historical kanji characters
;;
;; Half-width and Full-width Forms (Katakana subset): #xFF65-#xFF9F
;;   - Half-width katakana characters used in legacy systems

(defconst nskk--hiragana-start #x3040
  "Start code point of hiragana block (Unicode U+3040).")

(defconst nskk--hiragana-end #x309F
  "End code point of hiragana block (Unicode U+309F).")

(defconst nskk--katakana-start #x30A0
  "Start code point of katakana block (Unicode U+30A0).")

(defconst nskk--katakana-end #x30FF
  "End code point of katakana block (Unicode U+30FF).")

(defconst nskk--han-start #x4E00
  "Start code point of CJK Unified Ideographs block (Unicode U+4E00).")

(defconst nskk--han-end #x9FFF
  "End code point of CJK Unified Ideographs block (Unicode U+9FFF).")

(defconst nskk--han-extension-a-start #x3400
  "Start code point of CJK Unified Ideographs Extension A (Unicode U+3400).
Contains rare and historical kanji characters.")

(defconst nskk--han-extension-a-end #x4DBF
  "End code point of CJK Unified Ideographs Extension A (Unicode U+4DBF).")

(defconst nskk--hankaku-katakana-start #xFF65
  "Start code point of half-width katakana block (Unicode U+FF65).
Half-width katakana characters from the Half-width and Full-width Forms block.")

(defconst nskk--hankaku-katakana-end #xFF9F
  "End code point of half-width katakana block (Unicode U+FF9F).")

;; Hankaku katakana conversion table
;; Maps zenkaku katakana (as string) to hankaku katakana (as string)
(defvar nskk--zenkaku-to-hankaku-table
  (let ((table (make-hash-table :test 'equal :size 200)))
    (nskk-fill-hash-table table
      ;; Basic katakana
      ("ア" "ｱ") ("イ" "ｲ") ("ウ" "ｳ") ("エ" "ｴ") ("オ" "ｵ")
      ("カ" "ｶ") ("キ" "ｷ") ("ク" "ｸ") ("ケ" "ｹ") ("コ" "ｺ")
      ("サ" "ｻ") ("シ" "ｼ") ("ス" "ｽ") ("セ" "ｾ") ("ソ" "ｿ")
      ("タ" "ﾀ") ("チ" "ﾁ") ("ツ" "ﾂ") ("テ" "ﾃ") ("ト" "ﾄ")
      ("ナ" "ﾅ") ("ニ" "ﾆ") ("ヌ" "ﾇ") ("ネ" "ﾈ") ("ノ" "ﾉ")
      ("ハ" "ﾊ") ("ヒ" "ﾋ") ("フ" "ﾌ") ("ヘ" "ﾍ") ("ホ" "ﾎ")
      ("マ" "ﾏ") ("ミ" "ﾐ") ("ム" "ﾑ") ("メ" "ﾒ") ("モ" "ﾓ")
      ("ヤ" "ﾔ") ("ユ" "ﾕ") ("ヨ" "ﾖ")
      ("ラ" "ﾗ") ("リ" "ﾘ") ("ル" "ﾙ") ("レ" "ﾚ") ("ロ" "ﾛ")
      ("ワ" "ﾜ") ("ヲ" "ｦ") ("ン" "ﾝ") ("ヴ" "ｳﾞ")
      ;; Dakuten / Handakuten
      ("゛" "ﾞ") ("゜" "ﾟ")
      ;; Small katakana
      ("ァ" "ｧ") ("ィ" "ｨ") ("ゥ" "ｩ") ("ェ" "ｪ") ("ォ" "ｫ")
      ("ッ" "ｯ") ("ャ" "ｬ") ("ュ" "ｭ") ("ョ" "ｮ") ("ヮ" "ﾜ")
      ;; Dakuten extended
      ("ガ" "ｶﾞ") ("ギ" "ｷﾞ") ("グ" "ｸﾞ") ("ゲ" "ｹﾞ") ("ゴ" "ｺﾞ")
      ("ザ" "ｻﾞ") ("ジ" "ｼﾞ") ("ズ" "ｽﾞ") ("ゼ" "ｾﾞ") ("ゾ" "ｿﾞ")
      ("ダ" "ﾀﾞ") ("ヂ" "ﾁﾞ") ("ヅ" "ﾂﾞ") ("デ" "ﾃﾞ") ("ド" "ﾄﾞ")
      ("バ" "ﾊﾞ") ("ビ" "ﾋﾞ") ("ブ" "ﾌﾞ") ("ベ" "ﾍﾞ") ("ボ" "ﾎﾞ")
      ;; Handakuten extended
      ("パ" "ﾊﾟ") ("ピ" "ﾋﾟ") ("プ" "ﾌﾟ") ("ペ" "ﾍﾟ") ("ポ" "ﾎﾟ")
      ;; Punctuation
      ("。" "｡") ("、" "､") ("・" "･") ("ー" "ｰ"))
    table)
  "Hash table mapping zenkaku to hankaku katakana.")

;; Hankaku to zenkaku conversion table
;; Inverse of the above
(defvar nskk--hankaku-to-zenkaku-table
  (let ((table (make-hash-table :test 'equal :size 200)))
    (nskk-fill-hash-table table
      ;; Basic katakana
      ("ｱ" "ア") ("ｲ" "イ") ("ｳ" "ウ") ("ｴ" "エ") ("ｵ" "オ")
      ("ｶ" "カ") ("ｷ" "キ") ("ｸ" "ク") ("ｹ" "ケ") ("ｺ" "コ")
      ("ｻ" "サ") ("ｼ" "シ") ("ｽ" "ス") ("ｾ" "セ") ("ｿ" "ソ")
      ("ﾀ" "タ") ("ﾁ" "チ") ("ﾂ" "ツ") ("ﾃ" "テ") ("ﾄ" "ト")
      ("ﾅ" "ナ") ("ﾆ" "ニ") ("ﾇ" "ヌ") ("ﾈ" "ネ") ("ﾉ" "ノ")
      ("ﾊ" "ハ") ("ﾋ" "ヒ") ("ﾌ" "フ") ("ﾍ" "ヘ") ("ﾎ" "ホ")
      ("ﾏ" "マ") ("ﾐ" "ミ") ("ﾑ" "ム") ("ﾒ" "メ") ("ﾓ" "モ")
      ("ﾔ" "ヤ") ("ﾕ" "ユ") ("ﾖ" "ヨ")
      ("ﾗ" "ラ") ("ﾘ" "リ") ("ﾙ" "ル") ("ﾚ" "レ") ("ﾛ" "ロ")
      ("ﾜ" "ワ") ("ｦ" "ヲ") ("ﾝ" "ン") ("ｳﾞ" "ヴ")
      ;; Small katakana
      ("ｧ" "ァ") ("ｨ" "ィ") ("ｩ" "ゥ") ("ｪ" "ェ") ("ｫ" "ォ")
      ("ｯ" "ッ") ("ｬ" "ャ") ("ｭ" "ュ") ("ｮ" "ョ")
      ;; Punctuation
      ("｡" "。") ("､" "、") ("･" "・") ("ｰ" "ー")
      ;; Combining marks
      ("ﾞ" "゛") ("ﾟ" "゜")
      ;; Dakuten extended
      ("ｶﾞ" "ガ") ("ｷﾞ" "ギ") ("ｸﾞ" "グ") ("ｹﾞ" "ゲ") ("ｺﾞ" "ゴ")
      ("ｻﾞ" "ザ") ("ｼﾞ" "ジ") ("ｽﾞ" "ズ") ("ｾﾞ" "ゼ") ("ｿﾞ" "ゾ")
      ("ﾀﾞ" "ダ") ("ﾁﾞ" "ヂ") ("ﾂﾞ" "ヅ") ("ﾃﾞ" "デ") ("ﾄﾞ" "ド")
      ("ﾊﾞ" "バ") ("ﾋﾞ" "ビ") ("ﾌﾞ" "ブ") ("ﾍﾞ" "ベ") ("ﾎﾞ" "ボ")
      ;; Handakuten extended
      ("ﾊﾟ" "パ") ("ﾋﾟ" "ピ") ("ﾌﾟ" "プ") ("ﾍﾟ" "ペ") ("ﾎﾟ" "ポ"))
    table)
  "Hash table mapping hankaku to zenkaku katakana.")

;; Hiragana to katakana offset
;; Each hiragana character can be converted to katakana by adding this offset
(defconst nskk--kana-offset 96
  "Offset between hiragana and katakana code points.
 katakana = hiragana + 96")

;;; Character Classification Functions

(defun nskk-kana-hiragana-p (char)
  "Check if CHAR is a hiragana character."
  (and (integerp char)
       (>= char nskk--hiragana-start)
       (<= char nskk--hiragana-end)))

(defun nskk-kana-katakana-p (char)
  "Check if CHAR is a katakana character."
  (and (integerp char)
       (>= char nskk--katakana-start)
       (<= char nskk--katakana-end)))

(defun nskk-kana-han-p (char)
  "Check if CHAR is a han (kanji) character.
Includes both main CJK Unified Ideographs (U+4E00-U+9FFF) and
CJK Unified Ideographs Extension A (U+3400-U+4DBF) which contains
rare and historical kanji."
  (and (integerp char)
       (or (and (>= char nskk--han-start)
                (<= char nskk--han-end))
           (and (>= char nskk--han-extension-a-start)
                (<= char nskk--han-extension-a-end)))))

(defun nskk-kana-hankaku-katakana-p (char)
  "Check if CHAR is a half-width katakana character.
Half-width katakana are in the range U+FF65-U+FF9F,
part of the Half-width and Full-width Forms Unicode block."
  (and (integerp char)
       (>= char nskk--hankaku-katakana-start)
       (<= char nskk--hankaku-katakana-end)))

(defun nskk-kana-japanese-p (char)
  "Check if CHAR is a Japanese character.
Recognizes the following Unicode ranges:
- Hiragana (U+3040-U+309F)
- Katakana (U+30A0-U+30FF)
- CJK Unified Ideographs (U+4E00-U+9FFF)
- CJK Unified Ideographs Extension A (U+3400-U+4DBF)
- Half-width Katakana (U+FF65-U+FF9F)"
  (or (nskk-kana-hiragana-p char)
      (nskk-kana-katakana-p char)
      (nskk-kana-han-p char)
      (nskk-kana-hankaku-katakana-p char)))

;;; Hiragana/Katakana Conversion Functions

(defun nskk-kana-hiragana-to-katakana (char)
  "Convert hiragana CHAR to katakana.
Returns converted character or CHAR if not hiragana."
  (if (nskk-kana-hiragana-p char)
      (+ char nskk--kana-offset)
    char))

(defun nskk-kana-katakana-to-hiragana (char)
  "Convert katakana CHAR to hiragana.
Returns converted character or CHAR if not katakana."
  (if (nskk-kana-katakana-p char)
      (- char nskk--kana-offset)
    char))

(defun nskk-kana--map-string-chars (string converter)
  "Apply CONVERTER function to each char in STRING, return new string."
  (when (stringp string)
    (let ((result (make-string (length string) ?\0)))
      (dotimes (i (length string))
        (aset result i (funcall converter (aref string i))))
      result)))

(defun nskk-kana-string-hiragana-to-katakana (string)
  "Convert hiragana in STRING to katakana."
  (nskk-kana--map-string-chars string #'nskk-kana-hiragana-to-katakana))

(defun nskk-kana-string-katakana-to-hiragana (string)
  "Convert katakana in STRING to hiragana."
  (nskk-kana--map-string-chars string #'nskk-kana-katakana-to-hiragana))

;;; Hankaku/Zenkaku Conversion Functions

(defun nskk-kana-zenkaku-to-hankaku (string-or-char)
  "Convert zenkaku katakana STRING-OR-CHAR to hankaku."
  (if (stringp string-or-char)
      (nskk-kana--zenkaku-string-to-hankaku string-or-char)
    ;; Single character case
    (let ((str (char-to-string string-or-char)))
      (or (gethash str nskk--zenkaku-to-hankaku-table)
          str))))

(defun nskk-kana--zenkaku-string-to-hankaku (string)
  "Internal: Convert zenkaku katakana STRING to hankaku.
Uses vector accumulation for O(n) performance."
  (when (stringp string)
    (let* ((len (length string))
           ;; Pre-allocate vector with worst-case size (2x for multi-char conversions)
           (result-vec (make-string (* len 2) ?\0))
           (result-pos 0)
           (i 0))
      (while (< i len)
        (let ((char (aref string i)))
          ;; Look up single character
          (let ((hankaku (gethash (char-to-string char)
                                  nskk--zenkaku-to-hankaku-table)))
            (if hankaku
                ;; Copy converted string to result vector
                (let ((hankaku-len (length hankaku)))
                  (dotimes (j hankaku-len)
                    (aset result-vec (+ result-pos j) (aref hankaku j)))
                  (setq result-pos (+ result-pos hankaku-len)))
              ;; No conversion, keep original
              (aset result-vec result-pos char)
              (setq result-pos (1+ result-pos)))))
        (setq i (1+ i)))
      ;; Return only the filled portion
      (substring result-vec 0 result-pos))))

(defun nskk-kana-hankaku-to-zenkaku (string-or-char)
  "Convert hankaku katakana STRING-OR-CHAR to zenkaku.
Handles combined dakuten/handakuten marks (e.g., \"ｶﾞ\" -> \"ガ\")."
  (if (stringp string-or-char)
      (nskk-kana--hankaku-string-to-zenkaku string-or-char)
    ;; Single character case
    (let ((hankaku (char-to-string string-or-char)))
      (or (gethash hankaku nskk--hankaku-to-zenkaku-table)
          hankaku))))

(defun nskk-kana--hankaku-string-to-zenkaku (string)
  "Internal: Convert hankaku katakana STRING to zenkaku.
Handles dakuten/handakuten combinations.
Uses vector accumulation for O(n) performance."
  (when (stringp string)
    (let* ((len (length string))
           ;; Pre-allocate vector with worst-case size (same length)
           (result-vec (make-string len ?\0))
           (result-pos 0)
           (i 0))
      (while (< i len)
        ;; Try to match 2-character combination first
        (if (< (1+ i) len)
            (let ((two-chars (concat (char-to-string (aref string i))
                                    (char-to-string (aref string (1+ i))))))
              (let ((zenkaku (gethash two-chars nskk--hankaku-to-zenkaku-table)))
                (if zenkaku
                    (progn
                      ;; Copy converted string to result vector
                      (let ((zenkaku-len (length zenkaku)))
                        (dotimes (j zenkaku-len)
                          (aset result-vec (+ result-pos j) (aref zenkaku j)))
                        (setq result-pos (+ result-pos zenkaku-len)))
                      (setq i (+ i 2)))
                  ;; No 2-char match, try 1-char
                  (let ((one-char (char-to-string (aref string i))))
                    (let ((zenkaku (gethash one-char nskk--hankaku-to-zenkaku-table)))
                      (aset result-vec result-pos (aref (or zenkaku one-char) 0))
                      (setq result-pos (1+ result-pos))
                      (setq i (1+ i)))))))
          ;; Last character
          (let ((one-char (char-to-string (aref string i))))
            (let ((zenkaku (gethash one-char nskk--hankaku-to-zenkaku-table)))
              (aset result-vec result-pos (aref (or zenkaku one-char) 0))
              (setq result-pos (1+ result-pos))
              (setq i (1+ i))))))
      ;; Return only the filled portion
      (substring result-vec 0 result-pos))))

(defun nskk-kana-string-zenkaku-to-hankaku (string)
  "Convert zenkaku katakana in STRING to hankaku.
Alias for nskk-kana-zenkaku-to-hankaku for string argument."
  (nskk-kana--zenkaku-string-to-hankaku string))

(defun nskk-kana-string-hankaku-to-zenkaku (string)
  "Convert hankaku katakana in STRING to zenkaku.
Alias for nskk-kana-hankaku-to-zenkaku for string argument."
  (nskk-kana--hankaku-string-to-zenkaku string))

(provide 'nskk-kana)

;;; nskk-kana.el ends here
