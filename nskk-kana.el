;;; nskk-kana.el --- Kana character classification and conversion -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
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

;; This module provides kana character classification and conversion utilities
;; for NSKK.  It handles:
;; - Hiragana to katakana conversion
;; - Katakana to hiragana conversion
;; - Zenkaku (full-width) to hankaku (half-width) katakana conversion
;; - Hankaku to zenkaku katakana conversion
;; - Character classification predicates
;;
;; Character classification and conversion rules are expressed as Prolog
;; facts and rules, with hash table caches for hot-path performance.
;; The `nskk-kana--fill-hash-table' macro fills conversion tables at load
;; time.
;;
;; Unicode ranges used:
;; - Hiragana: U+3040-U+309F
;; - Katakana: U+30A0-U+30FF
;; - CJK Unified Ideographs (Kanji): U+4E00-U+9FFF
;; - CJK Unified Ideographs Extension A: U+3400-U+4DBF
;; - Half-width Katakana: U+FF65-U+FF9F

;;; Code:

(require 'nskk-prolog)

;;;; Customization

(defgroup nskk-kana nil
  "Kana character classification and conversion settings."
  :prefix "nskk-kana-"
  :group 'nskk)

;;;; Unicode Code Point Constants
;;
;; Based on Unicode Standard 15.0.

(defconst nskk-kana--hiragana-start #x3040
  "Start code point of hiragana block (Unicode U+3040).")

(defconst nskk-kana--hiragana-end #x309F
  "End code point of hiragana block (Unicode U+309F).")

(defconst nskk-kana--katakana-start #x30A0
  "Start code point of katakana block (Unicode U+30A0).")

(defconst nskk-kana--katakana-end #x30FF
  "End code point of katakana block (Unicode U+30FF).")

(defconst nskk-kana--han-start #x4E00
  "Start code point of CJK Unified Ideographs block (Unicode U+4E00).")

(defconst nskk-kana--han-end #x9FFF
  "End code point of CJK Unified Ideographs block (Unicode U+9FFF).")

(defconst nskk-kana--han-extension-a-start #x3400
  "Start code point of CJK Unified Ideographs Extension A (Unicode U+3400).
Contains rare and historical kanji characters.")

(defconst nskk-kana--han-extension-a-end #x4DBF
  "End code point of CJK Unified Ideographs Extension A (Unicode U+4DBF).")

(defconst nskk-kana--hankaku-katakana-start #xFF65
  "Start code point of half-width katakana block (Unicode U+FF65).
Part of the Half-width and Full-width Forms Unicode block.")

(defconst nskk-kana--hankaku-katakana-end #xFF9F
  "End code point of half-width katakana block (Unicode U+FF9F).")

(defconst nskk-kana--kana-offset 96
  "Code point offset between hiragana and katakana.
A katakana character equals the corresponding hiragana plus this offset.")

;;;; Internal Macros

(defmacro nskk-kana--fill-hash-table (table &rest entries)
  "Fill TABLE with ENTRIES, each entry being a list (KEY VALUE).
Returns TABLE."
  (declare (indent 1) (debug t))
  `(prog1 ,table
     ,@(mapcar (lambda (entry)
                 `(puthash ,(car entry) ,(cadr entry) ,table))
               entries)))

(defmacro nskk-kana--define-range-predicate (name prolog-name start end docstring)
  "Define a character range predicate NAME backed by a Prolog rule.
PROLOG-NAME is the Prolog predicate symbol.
START and END are defconst symbols for the Unicode range boundaries.
DOCSTRING documents the generated ELisp predicate.

Generates:
  - A Prolog rule: (PROLOG-NAME ?c) with arithmetic range check
  - An ELisp predicate: (NAME char) => boolean"
  (declare (indent 1) (debug t))
  `(progn
     (nskk-prolog-<- (,prolog-name \?c)
       (>= \?c ,start)
       (<= \?c ,end))
     (defun ,name (char)
       ,docstring
       (and (integerp char)
            (not (null (nskk-prolog-query-one
                        (list ',prolog-name char))))))))

;;;; Prolog Database Initialization

;; Initialize classification predicates as Prolog rules.
;; Each rule asserts: (predicate ?c) :- (>= ?c start) (<= ?c end)

(nskk-kana--define-range-predicate
 nskk-kana-hiragana-p kana-hiragana
 nskk-kana--hiragana-start nskk-kana--hiragana-end
 "Return non-nil if CHAR is a hiragana character (U+3040-U+309F).")

(nskk-kana--define-range-predicate
 nskk-kana-katakana-p kana-katakana
 nskk-kana--katakana-start nskk-kana--katakana-end
 "Return non-nil if CHAR is a katakana character (U+30A0-U+30FF).")

(nskk-kana--define-range-predicate
 nskk-kana-hankaku-katakana-p kana-hankaku-katakana
 nskk-kana--hankaku-katakana-start nskk-kana--hankaku-katakana-end
 "Return non-nil if CHAR is a half-width katakana character (U+FF65-U+FF9F).
Half-width katakana are part of the Half-width and Full-width Forms block.")

;; Han (kanji) spans two disjoint Unicode ranges; define it manually.
(nskk-prolog-<- (kana-han \?c)
  (>= \?c nskk-kana--han-start)
  (<= \?c nskk-kana--han-end))
(nskk-prolog-<- (kana-han \?c)
  (>= \?c nskk-kana--han-extension-a-start)
  (<= \?c nskk-kana--han-extension-a-end))

(defun nskk-kana-han-p (char)
  "Return non-nil if CHAR is a han (kanji) character.
Recognizes both CJK Unified Ideographs (U+4E00-U+9FFF) and
CJK Unified Ideographs Extension A (U+3400-U+4DBF)."
  (and (integerp char)
       (not (null (nskk-prolog-query-one (list 'kana-han char))))))

;; Japanese composite: any of hiragana, katakana, han, hankaku-katakana.
(nskk-prolog-<- (kana-japanese \?c) (kana-hiragana \?c))
(nskk-prolog-<- (kana-japanese \?c) (kana-katakana \?c))
(nskk-prolog-<- (kana-japanese \?c) (kana-han \?c))
(nskk-prolog-<- (kana-japanese \?c) (kana-hankaku-katakana \?c))

(defun nskk-kana-japanese-p (char)
  "Return non-nil if CHAR is a Japanese character.
Recognizes the following Unicode ranges:
- Hiragana (U+3040-U+309F)
- Katakana (U+30A0-U+30FF)
- CJK Unified Ideographs (U+4E00-U+9FFF)
- CJK Unified Ideographs Extension A (U+3400-U+4DBF)
- Half-width Katakana (U+FF65-U+FF9F)"
  (and (integerp char)
       (not (null (nskk-prolog-query-one (list 'kana-japanese char))))))

;; Hiragana <-> katakana conversion via arithmetic offset.
(nskk-prolog-<- (kana-hiragana-to-katakana \?h \?k)
  (kana-hiragana \?h)
  (is \?k (+ \?h nskk-kana--kana-offset)))

(nskk-prolog-<- (kana-katakana-to-hiragana \?k \?h)
  (kana-katakana \?k)
  (is \?h (- \?k nskk-kana--kana-offset)))

;;;; Zenkaku/Hankaku Conversion Tables
;;
;; Both Prolog facts and hash table caches are maintained (dual-write pattern).
;; The hash tables provide O(1) hot-path performance.
;; The Prolog facts expose the mappings to the rest of the Prolog database.

(nskk-prolog-set-index 'zenkaku-to-hankaku 2 :hash)
(nskk-prolog-set-index 'hankaku-to-zenkaku 2 :hash)

(defconst nskk-kana--zenkaku-to-hankaku-table
  (let ((table (make-hash-table :test 'equal :size 200)))
    (nskk-kana--fill-hash-table table
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
      ;; Dakuten / handakuten combining marks
      ("゛" "ﾞ") ("゜" "ﾟ")
      ;; Small katakana
      ("ァ" "ｧ") ("ィ" "ｨ") ("ゥ" "ｩ") ("ェ" "ｪ") ("ォ" "ｫ")
      ("ッ" "ｯ") ("ャ" "ｬ") ("ュ" "ｭ") ("ョ" "ｮ") ("ヮ" "ﾜ")
      ;; Voiced (dakuten) extended
      ("ガ" "ｶﾞ") ("ギ" "ｷﾞ") ("グ" "ｸﾞ") ("ゲ" "ｹﾞ") ("ゴ" "ｺﾞ")
      ("ザ" "ｻﾞ") ("ジ" "ｼﾞ") ("ズ" "ｽﾞ") ("ゼ" "ｾﾞ") ("ゾ" "ｿﾞ")
      ("ダ" "ﾀﾞ") ("ヂ" "ﾁﾞ") ("ヅ" "ﾂﾞ") ("デ" "ﾃﾞ") ("ド" "ﾄﾞ")
      ("バ" "ﾊﾞ") ("ビ" "ﾋﾞ") ("ブ" "ﾌﾞ") ("ベ" "ﾍﾞ") ("ボ" "ﾎﾞ")
      ;; Semi-voiced (handakuten) extended
      ("パ" "ﾊﾟ") ("ピ" "ﾋﾟ") ("プ" "ﾌﾟ") ("ペ" "ﾍﾟ") ("ポ" "ﾎﾟ")
      ;; Punctuation
      ("。" "｡") ("、" "､") ("・" "･") ("ー" "ｰ")))
  "Hash table (string -> string) mapping zenkaku katakana to hankaku equivalents.")

(defconst nskk-kana--hankaku-to-zenkaku-table
  (let ((table (make-hash-table :test 'equal :size 200)))
    (nskk-kana--fill-hash-table table
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
      ("ﾜ" "ワ") ("ｦ" "ヲ") ("ﾝ" "ン")
      ;; Small katakana
      ("ｧ" "ァ") ("ｨ" "ィ") ("ｩ" "ゥ") ("ｪ" "ェ") ("ｫ" "ォ")
      ("ｯ" "ッ") ("ｬ" "ャ") ("ｭ" "ュ") ("ｮ" "ョ")
      ;; Punctuation
      ("｡" "。") ("､" "、") ("･" "・") ("ｰ" "ー")
      ;; Combining marks
      ("ﾞ" "゛") ("ﾟ" "゜")
      ;; Voiced (dakuten) extended — two-character hankaku sequences
      ("ｶﾞ" "ガ") ("ｷﾞ" "ギ") ("ｸﾞ" "グ") ("ｹﾞ" "ゲ") ("ｺﾞ" "ゴ")
      ("ｻﾞ" "ザ") ("ｼﾞ" "ジ") ("ｽﾞ" "ズ") ("ｾﾞ" "ゼ") ("ｿﾞ" "ゾ")
      ("ﾀﾞ" "ダ") ("ﾁﾞ" "ヂ") ("ﾂﾞ" "ヅ") ("ﾃﾞ" "デ") ("ﾄﾞ" "ド")
      ("ﾊﾞ" "バ") ("ﾋﾞ" "ビ") ("ﾌﾞ" "ブ") ("ﾍﾞ" "ベ") ("ﾎﾞ" "ボ")
      ;; Semi-voiced (handakuten) extended
      ("ﾊﾟ" "パ") ("ﾋﾟ" "ピ") ("ﾌﾟ" "プ") ("ﾍﾟ" "ペ") ("ﾎﾟ" "ポ")
      ;; Voiced u
      ("ｳﾞ" "ヴ")))
  "Hash table (string -> string) mapping hankaku katakana to zenkaku equivalents.
Includes two-character dakuten/handakuten sequences (e.g., \"ｶﾞ\" -> \"ガ\").")

;; Populate Prolog facts from the hash tables.
;; This is done after table initialization to keep all mappings in one place.
(maphash (lambda (k v)
           (nskk-prolog-assert (list (list 'zenkaku-to-hankaku k v))))
         nskk-kana--zenkaku-to-hankaku-table)

(maphash (lambda (k v)
           (nskk-prolog-assert (list (list 'hankaku-to-zenkaku k v))))
         nskk-kana--hankaku-to-zenkaku-table)

;;;; Character Conversion Functions

(defun nskk-kana-hiragana-to-katakana (char)
  "Convert hiragana CHAR to katakana.
Returns the converted character code, or CHAR unchanged if not hiragana."
  (if (nskk-kana-hiragana-p char)
      (+ char nskk-kana--kana-offset)
    char))

(defun nskk-kana-katakana-to-hiragana (char)
  "Convert katakana CHAR to hiragana.
Returns the converted character code, or CHAR unchanged if not katakana."
  (if (nskk-kana-katakana-p char)
      (- char nskk-kana--kana-offset)
    char))

(defun nskk-kana--map-string-chars (string converter)
  "Apply CONVERTER to each character in STRING, returning a new string.
Returns nil if STRING is not a string."
  (when (stringp string)
    (let ((result (make-string (length string) ?\0)))
      (dotimes (i (length string))
        (aset result i (funcall converter (aref string i))))
      result)))

(defun nskk-kana-string-hiragana-to-katakana (string)
  "Convert all hiragana characters in STRING to katakana."
  (nskk-kana--map-string-chars string #'nskk-kana-hiragana-to-katakana))

(defun nskk-kana-string-katakana-to-hiragana (string)
  "Convert all katakana characters in STRING to hiragana."
  (nskk-kana--map-string-chars string #'nskk-kana-katakana-to-hiragana))

;;;; Zenkaku/Hankaku Conversion Functions

(defun nskk-kana-zenkaku-to-hankaku (string-or-char)
  "Convert zenkaku katakana STRING-OR-CHAR to hankaku.
For a string, converts each recognized zenkaku character; unrecognized
characters are passed through unchanged.  For a character, returns the
hankaku string equivalent, or a one-character string if unrecognized."
  (if (stringp string-or-char)
      (nskk-kana--zenkaku-string-to-hankaku string-or-char)
    (let ((str (char-to-string string-or-char)))
      (or (gethash str nskk-kana--zenkaku-to-hankaku-table) str))))

(defun nskk-kana--zenkaku-string-to-hankaku (string)
  "Convert zenkaku katakana in STRING to hankaku.
Unrecognized characters are passed through unchanged.
Uses a pre-allocated buffer for O(n) performance."
  (when (stringp string)
    (let* ((len (length string))
           ;; Worst case: each zenkaku char expands to 2 hankaku chars.
           (result-vec (make-string (* len 2) ?\0))
           (result-pos 0)
           (i 0))
      (while (< i len)
        (let* ((char (aref string i))
               (hankaku (gethash (char-to-string char)
                                 nskk-kana--zenkaku-to-hankaku-table)))
          (if hankaku
              (let ((hlen (length hankaku)))
                (dotimes (j hlen)
                  (aset result-vec (+ result-pos j) (aref hankaku j)))
                (setq result-pos (+ result-pos hlen)))
            (aset result-vec result-pos char)
            (setq result-pos (1+ result-pos))))
        (setq i (1+ i)))
      (substring result-vec 0 result-pos))))

(defun nskk-kana-hankaku-to-zenkaku (string-or-char)
  "Convert hankaku katakana STRING-OR-CHAR to zenkaku.
Handles combined dakuten/handakuten marks (e.g., \"ｶﾞ\" -> \"ガ\").
Unrecognized characters are passed through unchanged."
  (if (stringp string-or-char)
      (nskk-kana--hankaku-string-to-zenkaku string-or-char)
    (let ((str (char-to-string string-or-char)))
      (or (gethash str nskk-kana--hankaku-to-zenkaku-table) str))))

(defun nskk-kana--hankaku-string-to-zenkaku (string)
  "Convert hankaku katakana in STRING to zenkaku.
Handles two-character dakuten/handakuten sequences (e.g., \"ｶﾞ\" -> \"ガ\").
Unrecognized characters are passed through unchanged."
  (when (stringp string)
    (let* ((len (length string))
           ;; Output is never longer than input in character count.
           (result-vec (make-string len ?\0))
           (result-pos 0)
           (i 0))
      (while (< i len)
        (if (< (1+ i) len)
            ;; Try two-character sequence first (dakuten combinations).
            (let* ((two (concat (char-to-string (aref string i))
                                (char-to-string (aref string (1+ i)))))
                   (zen2 (gethash two nskk-kana--hankaku-to-zenkaku-table)))
              (if zen2
                  (progn
                    (let ((zlen (length zen2)))
                      (dotimes (j zlen)
                        (aset result-vec (+ result-pos j) (aref zen2 j)))
                      (setq result-pos (+ result-pos zlen)))
                    (setq i (+ i 2)))
                ;; Fall back to single-character lookup.
                (let* ((one (char-to-string (aref string i)))
                       (zen1 (gethash one nskk-kana--hankaku-to-zenkaku-table)))
                  (aset result-vec result-pos (aref (or zen1 one) 0))
                  (setq result-pos (1+ result-pos))
                  (setq i (1+ i)))))
          ;; Last character: single-character lookup only.
          (let* ((one (char-to-string (aref string i)))
                 (zen1 (gethash one nskk-kana--hankaku-to-zenkaku-table)))
            (aset result-vec result-pos (aref (or zen1 one) 0))
            (setq result-pos (1+ result-pos))
            (setq i (1+ i)))))
      (substring result-vec 0 result-pos))))

(provide 'nskk-kana)

;;; nskk-kana.el ends here
