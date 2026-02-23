;;; nskk-core.el --- Core conversion utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda

;; Author: Takeshi Umeda <takeokunn@gmail.com>
;; Maintainer: Takeshi Umeda <takeokunn@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1
;; Keywords: japanese, input-method, mvc

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

;; Character code ranges for classification
;; Hiragana: #x3040-#x309F
;; Katakana: #x30A0-#x30FF
;; Han (Kanji): #x4E00-#x9FFF

(defconst nskk--hiragana-start #x3040
  "Start code point of hiragana block.")

(defconst nskk--hiragana-end #x309F
  "End code point of hiragana block.")

(defconst nskk--katakana-start #x30A0
  "Start code point of katakana block.")

(defconst nskk--katakana-end #x30FF
  "End code point of katakana block.")

(defconst nskk--han-start #x4E00
  "Start code point of han (kanji) block.")

(defconst nskk--han-end #x9FFF
  "End code point of han (kanji) block.")

;; Hankaku katakana conversion table
;; Maps zenkaku katakana (as string) to hankaku katakana (as string)
(defvar nskk--zenkaku-to-hankaku-table
  (let ((table (make-hash-table :test 'equal :size 200)))
    ;; Basic katakana
    (puthash "ア" "ｱ" table)
    (puthash "イ" "ｲ" table)
    (puthash "ウ" "ｳ" table)
    (puthash "エ" "ｴ" table)
    (puthash "オ" "ｵ" table)
    (puthash "カ" "ｶ" table)
    (puthash "キ" "ｷ" table)
    (puthash "ク" "ｸ" table)
    (puthash "ケ" "ｹ" table)
    (puthash "コ" "ｺ" table)
    (puthash "サ" "ｻ" table)
    (puthash "シ" "ｼ" table)
    (puthash "ス" "ｽ" table)
    (puthash "セ" "ｾ" table)
    (puthash "ソ" "ｿ" table)
    (puthash "タ" "ﾀ" table)
    (puthash "チ" "ﾁ" table)
    (puthash "ツ" "ﾂ" table)
    (puthash "テ" "ﾃ" table)
    (puthash "ト" "ﾄ" table)
    (puthash "ナ" "ﾅ" table)
    (puthash "ニ" "ﾆ" table)
    (puthash "ヌ" "ﾇ" table)
    (puthash "ネ" "ﾈ" table)
    (puthash "ノ" "ﾉ" table)
    (puthash "ハ" "ﾊ" table)
    (puthash "ヒ" "ﾋ" table)
    (puthash "フ" "ﾌ" table)
    (puthash "ヘ" "ﾍ" table)
    (puthash "ホ" "ﾎ" table)
    (puthash "マ" "ﾏ" table)
    (puthash "ミ" "ﾐ" table)
    (puthash "ム" "ﾑ" table)
    (puthash "メ" "ﾒ" table)
    (puthash "モ" "ﾓ" table)
    (puthash "ヤ" "ﾔ" table)
    (puthash "ユ" "ﾕ" table)
    (puthash "ヨ" "ﾖ" table)
    (puthash "ラ" "ﾗ" table)
    (puthash "リ" "ﾘ" table)
    (puthash "ル" "ﾙ" table)
    (puthash "レ" "ﾚ" table)
    (puthash "ロ" "ﾛ" table)
    (puthash "ワ" "ﾜ" table)
    (puthash "ヲ" "ｦ" table)
    (puthash "ン" "ﾝ" table)
    (puthash "ヴ" "ｳﾞ" table)

    ;; Dakuten and handakuten
    (puthash "゛" "ﾞ" table)
    (puthash "゜" "ﾟ" table)

    ;; Small katakana
    (puthash "ァ" "ｧ" table)
    (puthash "ィ" "ｨ" table)
    (puthash "ゥ" "ｩ" table)
    (puthash "ェ" "ｪ" table)
    (puthash "ォ" "ｫ" table)
    (puthash "ッ" "ｯ" table)
    (puthash "ャ" "ｬ" table)
    (puthash "ュ" "ｭ" table)
    (puthash "ョ" "ｮ" table)
    (puthash "ヮ" "ﾜ" table)

    ;; Extended katakana with dakuten/handakuten
    (puthash "ガ" "ｶﾞ" table)
    (puthash "ギ" "ｷﾞ" table)
    (puthash "グ" "ｸﾞ" table)
    (puthash "ゲ" "ｹﾞ" table)
    (puthash "ゴ" "ｺﾞ" table)
    (puthash "ザ" "ｻﾞ" table)
    (puthash "ジ" "ｼﾞ" table)
    (puthash "ズ" "ｽﾞ" table)
    (puthash "ゼ" "ｾﾞ" table)
    (puthash "ゾ" "ｿﾞ" table)
    (puthash "ダ" "ﾀﾞ" table)
    (puthash "ヂ" "ﾁﾞ" table)
    (puthash "ヅ" "ﾂﾞ" table)
    (puthash "デ" "ﾃﾞ" table)
    (puthash "ド" "ﾄﾞ" table)
    (puthash "バ" "ﾊﾞ" table)
    (puthash "ビ" "ﾋﾞ" table)
    (puthash "ブ" "ﾌﾞ" table)
    (puthash "ベ" "ﾍﾞ" table)
    (puthash "ボ" "ﾎﾞ" table)
    (puthash "パ" "ﾊﾟ" table)
    (puthash "ピ" "ﾋﾟ" table)
    (puthash "プ" "ﾌﾟ" table)
    (puthash "ペ" "ﾍﾟ" table)
    (puthash "ポ" "ﾎﾟ" table)

    ;; Punctuation and symbols
    (puthash "。" "｡" table)
    (puthash "、" "､" table)
    (puthash "・" "･" table)
    (puthash "ー" "ｰ" table)

    table)
  "Hash table mapping zenkaku to hankaku katakana.")

;; Hankaku to zenkaku conversion table
;; Inverse of the above
(defvar nskk--hankaku-to-zenkaku-table
  (let ((table (make-hash-table :test 'equal :size 200)))
    ;; Basic katakana
    (puthash "ｱ" "ア" table)
    (puthash "ｲ" "イ" table)
    (puthash "ｳ" "ウ" table)
    (puthash "ｴ" "エ" table)
    (puthash "ｵ" "オ" table)
    (puthash "ｶ" "カ" table)
    (puthash "ｷ" "キ" table)
    (puthash "ｸ" "ク" table)
    (puthash "ｹ" "ケ" table)
    (puthash "ｺ" "コ" table)
    (puthash "ｻ" "サ" table)
    (puthash "ｼ" "シ" table)
    (puthash "ｽ" "ス" table)
    (puthash "ｾ" "セ" table)
    (puthash "ｿ" "ソ" table)
    (puthash "ﾀ" "タ" table)
    (puthash "ﾁ" "チ" table)
    (puthash "ﾂ" "ツ" table)
    (puthash "ﾃ" "テ" table)
    (puthash "ﾄ" "ト" table)
    (puthash "ﾅ" "ナ" table)
    (puthash "ﾆ" "ニ" table)
    (puthash "ﾇ" "ヌ" table)
    (puthash "ﾈ" "ネ" table)
    (puthash "ﾉ" "ノ" table)
    (puthash "ﾊ" "ハ" table)
    (puthash "ﾋ" "ヒ" table)
    (puthash "ﾌ" "フ" table)
    (puthash "ﾍ" "ヘ" table)
    (puthash "ﾎ" "ホ" table)
    (puthash "ﾏ" "マ" table)
    (puthash "ﾐ" "ミ" table)
    (puthash "ﾑ" "ム" table)
    (puthash "ﾒ" "メ" table)
    (puthash "ﾓ" "モ" table)
    (puthash "ﾔ" "ヤ" table)
    (puthash "ﾕ" "ユ" table)
    (puthash "ﾖ" "ヨ" table)
    (puthash "ﾗ" "ラ" table)
    (puthash "ﾘ" "リ" table)
    (puthash "ﾙ" "ル" table)
    (puthash "ﾚ" "レ" table)
    (puthash "ﾛ" "ロ" table)
    (puthash "ﾜ" "ワ" table)
    (puthash "ｦ" "ヲ" table)
    (puthash "ﾝ" "ン" table)
    (puthash "ｳﾞ" "ヴ" table)

    ;; Small katakana
    (puthash "ｧ" "ァ" table)
    (puthash "ｨ" "ィ" table)
    (puthash "ｩ" "ゥ" table)
    (puthash "ｪ" "ェ" table)
    (puthash "ｫ" "ォ" table)
    (puthash "ｯ" "ッ" table)
    (puthash "ｬ" "ャ" table)
    (puthash "ｭ" "ュ" table)
    (puthash "ｮ" "ョ" table)

    ;; Punctuation and symbols
    (puthash "｡" "。" table)
    (puthash "､" "、" table)
    (puthash "･" "・" table)
    (puthash "ｰ" "ー" table)

    ;; Combining marks
    (puthash "ﾞ" "゛" table)
    (puthash "ﾟ" "゜" table)

    ;; Extended katakana with dakuten/handakuten
    (puthash "ｶﾞ" "ガ" table)
    (puthash "ｷﾞ" "ギ" table)
    (puthash "ｸﾞ" "グ" table)
    (puthash "ｹﾞ" "ゲ" table)
    (puthash "ｺﾞ" "ゴ" table)
    (puthash "ｻﾞ" "ザ" table)
    (puthash "ｼﾞ" "ジ" table)
    (puthash "ｽﾞ" "ズ" table)
    (puthash "ｾﾞ" "ゼ" table)
    (puthash "ｿﾞ" "ゾ" table)
    (puthash "ﾀﾞ" "ダ" table)
    (puthash "ﾁﾞ" "ヂ" table)
    (puthash "ﾂﾞ" "ヅ" table)
    (puthash "ﾃﾞ" "デ" table)
    (puthash "ﾄﾞ" "ド" table)
    (puthash "ﾊﾞ" "バ" table)
    (puthash "ﾋﾞ" "ビ" table)
    (puthash "ﾌﾞ" "ブ" table)
    (puthash "ﾍﾞ" "ベ" table)
    (puthash "ﾎﾞ" "ボ" table)
    (puthash "ﾊﾟ" "パ" table)
    (puthash "ﾋﾟ" "ピ" table)
    (puthash "ﾌﾟ" "プ" table)
    (puthash "ﾍﾟ" "ペ" table)
    (puthash "ﾎﾟ" "ポ" table)

    table)
  "Hash table mapping hankaku to zenkaku katakana.")

;; Hiragana to katakana offset
;; Each hiragana character can be converted to katakana by adding this offset
(defconst nskk--kana-offset 96
  "Offset between hiragana and katakana code points.
 katakana = hiragana + 96")

;;; Character Classification Functions

(defun nskk-core-hiragana-p (char)
  "Check if CHAR is a hiragana character."
  (and (integerp char)
       (>= char nskk--hiragana-start)
       (<= char nskk--hiragana-end)))

(defun nskk-core-katakana-p (char)
  "Check if CHAR is a katakana character."
  (and (integerp char)
       (>= char nskk--katakana-start)
       (<= char nskk--katakana-end)))

(defun nskk-core-han-p (char)
  "Check if CHAR is a han (kanji) character."
  (and (integerp char)
       (>= char nskk--han-start)
       (<= char nskk--han-end)))

(defun nskk-core-japanese-p (char)
  "Check if CHAR is a Japanese character (hiragana, katakana, or han)."
  (or (nskk-core-hiragana-p char)
      (nskk-core-katakana-p char)
      (nskk-core-han-p char)))

;;; Hiragana/Katakana Conversion Functions

(defun nskk-core-hiragana-to-katakana (char)
  "Convert hiragana CHAR to katakana.
Returns converted character or CHAR if not hiragana."
  (if (nskk-core-hiragana-p char)
      (+ char nskk--kana-offset)
    char))

(defun nskk-core-katakana-to-hiragana (char)
  "Convert katakana CHAR to hiragana.
Returns converted character or CHAR if not katakana."
  (if (nskk-core-katakana-p char)
      (- char nskk--kana-offset)
    char))

(defun nskk-core-string-hiragana-to-katakana (string)
  "Convert hiragana in STRING to katakana."
  (when (stringp string)
    (let ((result (make-string (length string) ?\0)))
      (dotimes (i (length string))
        (aset result i
              (nskk-core-hiragana-to-katakana (aref string i))))
      result)))

(defun nskk-core-string-katakana-to-hiragana (string)
  "Convert katakana in STRING to hiragana."
  (when (stringp string)
    (let ((result (make-string (length string) ?\0)))
      (dotimes (i (length string))
        (aset result i
              (nskk-core-katakana-to-hiragana (aref string i))))
      result)))

;;; Hankaku/Zenkaku Conversion Functions

(defun nskk-core-zenkaku-to-hankaku (string-or-char)
  "Convert zenkaku katakana STRING-OR-CHAR to hankaku."
  (if (stringp string-or-char)
      (nskk-core--zenkaku-string-to-hankaku string-or-char)
    ;; Single character case
    (let ((str (char-to-string string-or-char)))
      (or (gethash str nskk--zenkaku-to-hankaku-table)
          str))))

(defun nskk-core--zenkaku-string-to-hankaku (string)
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

(defun nskk-core-hankaku-to-zenkaku (string-or-char)
  "Convert hankaku katakana STRING-OR-CHAR to zenkaku.
Handles combined dakuten/handakuten marks (e.g., \"ｶﾞ\" -> \"ガ\")."
  (if (stringp string-or-char)
      (nskk-core--hankaku-string-to-zenkaku string-or-char)
    ;; Single character case
    (let ((hankaku (char-to-string string-or-char)))
      (or (gethash hankaku nskk--hankaku-to-zenkaku-table)
          hankaku))))

(defun nskk-core--hankaku-string-to-zenkaku (string)
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

(defun nskk-core-string-zenkaku-to-hankaku (string)
  "Convert zenkaku katakana in STRING to hankaku.
Alias for nskk-core-zenkaku-to-hankaku for string argument."
  (nskk-core--zenkaku-string-to-hankaku string))

(defun nskk-core-string-hankaku-to-zenkaku (string)
  "Convert hankaku katakana in STRING to zenkaku.
Alias for nskk-core-hankaku-to-zenkaku for string argument."
  (nskk-core--hankaku-string-to-zenkaku string))

(provide 'nskk-core)

;;; nskk-core.el ends here
