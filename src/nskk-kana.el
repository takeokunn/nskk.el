;;; nskk-kana.el --- Kana character classification and conversion -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n convenience

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

;; Kana character classification and conversion.

;;; Code:

(require 'cl-lib)
(require 'nskk-prolog)
(require 'nskk-cps-macros)

;;;; Unicode Code Point Constants
;;
;; Based on Unicode Standard 15.0.

(defconst nskk--kana-hiragana-start #x3040
  "Start code point of hiragana block (Unicode U+3040).")

(defconst nskk--kana-hiragana-end #x309F
  "End code point of hiragana block (Unicode U+309F).")

(defconst nskk--kana-katakana-start #x30A0
  "Start code point of katakana block (Unicode U+30A0).")

(defconst nskk--kana-katakana-end #x30FF
  "End code point of katakana block (Unicode U+30FF).")

(defconst nskk--kana-han-start #x4E00
  "Start code point of CJK Unified Ideographs block (Unicode U+4E00).")

(defconst nskk--kana-han-end #x9FFF
  "End code point of CJK Unified Ideographs block (Unicode U+9FFF).")

(defconst nskk--kana-han-extension-a-start #x3400
  "Start code point of CJK Unified Ideographs Extension A (Unicode U+3400).
Contains rare and historical kanji characters.")

(defconst nskk--kana-han-extension-a-end #x4DBF
  "End code point of CJK Unified Ideographs Extension A (Unicode U+4DBF).")

(defconst nskk--kana-hankaku-katakana-start #xFF65
  "Start code point of half-width katakana block (Unicode U+FF65).
Part of the Half-width and Full-width Forms Unicode block.")

(defconst nskk--kana-hankaku-katakana-end #xFF9F
  "End code point of half-width katakana block (Unicode U+FF9F).")

(defconst nskk--kana-kana-offset 96
  "Code point offset between hiragana and katakana.
A katakana character equals the corresponding hiragana plus this offset.")

(defconst nskk--kana-latin-width-offset #xFEE0
  "Code point offset between ASCII and its JIS X 0208 full-width form.
A full-width latin character equals the corresponding ASCII character
plus this offset.  Space is excluded: it maps to the ideographic space
\(U+3000), which does not follow the offset.")

;;;; Internal Macros

(defmacro nskk--kana-fill-hash-table (table &rest entries)
  "Fill TABLE with ENTRIES, each entry being a list (KEY VALUE).
Returns TABLE."
  (declare (indent 1) (debug t))
  `(prog1 ,table
     ,@(mapcar (lambda (entry)
                 `(puthash ,(car entry) ,(cadr entry) ,table))
               entries)))

;;;; Character Classification Predicates

(nskk-prolog-<- (kana-hiragana \?c)
  (>= \?c nskk--kana-hiragana-start)
  (<= \?c nskk--kana-hiragana-end))

(defsubst nskk-kana-hiragana-p (char)
  "Return non-nil if CHAR is a hiragana character (U+3040-U+309F)."
  (and (integerp char)
       (>= char nskk--kana-hiragana-start)
       (<= char nskk--kana-hiragana-end)))

(nskk-prolog-<- (kana-katakana \?c)
  (>= \?c nskk--kana-katakana-start)
  (<= \?c nskk--kana-katakana-end))

(defsubst nskk-kana-katakana-p (char)
  "Return non-nil if CHAR is a katakana character (U+30A0-U+30FF)."
  (and (integerp char)
       (>= char nskk--kana-katakana-start)
       (<= char nskk--kana-katakana-end)))

(nskk-prolog-<- (kana-hankaku-katakana \?c)
  (>= \?c nskk--kana-hankaku-katakana-start)
  (<= \?c nskk--kana-hankaku-katakana-end))

(defsubst nskk-kana-hankaku-katakana-p (char)
  "Return non-nil if CHAR is a half-width katakana character (U+FF65-U+FF9F).
Half-width katakana are part of the Half-width and Full-width Forms block."
  (and (integerp char)
       (>= char nskk--kana-hankaku-katakana-start)
       (<= char nskk--kana-hankaku-katakana-end)))

(nskk-prolog-<- (kana-han \?c)
  (>= \?c nskk--kana-han-start)
  (<= \?c nskk--kana-han-end))
(nskk-prolog-<- (kana-han \?c)
  (>= \?c nskk--kana-han-extension-a-start)
  (<= \?c nskk--kana-han-extension-a-end))

(defun nskk-kana-han-p (char)
  "Return non-nil if CHAR is a han (kanji) character.
Recognizes both CJK Unified Ideographs (U+4E00-U+9FFF) and
CJK Unified Ideographs Extension A (U+3400-U+4DBF)."
  (and (integerp char)
       (nskk-prolog-holds-p (list 'kana-han char))
       t))

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
       (nskk-prolog-holds-p (list 'kana-japanese char))
       t))

;;;; Zenkaku/Hankaku Conversion Tables

(defconst nskk--kana-zenkaku-to-hankaku-table
  (let ((table (make-hash-table :test 'equal :size 200)))
    (nskk--kana-fill-hash-table table
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

(defconst nskk--kana-hankaku-to-zenkaku-table
  (let ((table (make-hash-table :test 'equal :size 200)))
    (nskk--kana-fill-hash-table table
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

;;;; Character Conversion Functions

(defun nskk-kana-hiragana-to-katakana (char)
  "Convert hiragana CHAR to katakana.
Returns the converted character code, or CHAR unchanged if not hiragana."
  (if (nskk-kana-hiragana-p char)
      (+ char nskk--kana-kana-offset)
    char))

(defun nskk-kana-katakana-to-hiragana (char)
  "Convert katakana CHAR to hiragana.
Returns the converted character code, or CHAR unchanged if not katakana."
  (if (nskk-kana-katakana-p char)
      (- char nskk--kana-kana-offset)
    char))

(defun/k nskk--kana-map-string-chars (string converter)
  "Apply CONVERTER to each character in STRING.
Succeeds with the converted string if STRING is a string.
Fails if STRING is not a string."
  (if (stringp string)
      (succeed (apply #'string (mapcar converter (string-to-list string))))
    (fail)))

(defun/k nskk-kana-string-hiragana-to-katakana (string)
  "Convert all hiragana characters in STRING to katakana.
Succeeds with the converted string if STRING is a string.
Fails if STRING is not a string."
  (<- result nskk--kana-map-string-chars string #'nskk-kana-hiragana-to-katakana)
  (succeed result))

(defun/k nskk-kana-string-katakana-to-hiragana (string)
  "Convert all katakana characters in STRING to hiragana.
Succeeds with the converted string if STRING is a string.
Fails if STRING is not a string."
  (<- result nskk--kana-map-string-chars string #'nskk-kana-katakana-to-hiragana)
  (succeed result))

;;;; Zenkaku/Hankaku Conversion Functions

(defun nskk--kana-zenkaku-string-to-hankaku (string)
  "Convert each zenkaku katakana character in STRING to its hankaku form.
Characters with no hankaku equivalent are passed through unchanged.
STRING must be a string; callers own the type dispatch."
  (cl-loop for char across string
           for str = (char-to-string char)
           concat (or (gethash str nskk--kana-zenkaku-to-hankaku-table)
                      str)))

(defun nskk-kana-zenkaku-to-hankaku (string-or-char)
  "Convert zenkaku katakana STRING-OR-CHAR to hankaku.
For a string, converts each recognized zenkaku character; unrecognized
characters are passed through unchanged.  For a character (integer), returns
the hankaku string equivalent, or a one-character string if unrecognized.
For any other type, returns STRING-OR-CHAR unchanged.

Not injective: ヮ and ワ both map to ﾜ because JIS X 0201 has no half-width
small wa, so `nskk-kana-hankaku-to-zenkaku' cannot recover ヮ."
  (pcase string-or-char
    ((pred stringp) (nskk--kana-zenkaku-string-to-hankaku string-or-char))
    ((pred integerp)
     (let ((str (char-to-string string-or-char)))
       (or (gethash str nskk--kana-zenkaku-to-hankaku-table) str)))
    (_ string-or-char)))

(defun nskk--kana-hankaku-lookup-at (string i len)
  "Look up hankaku->zenkaku conversion at position I in STRING (length LEN).
Tries the two-character sequence at I and I+1 first, so that a base character
followed by a combining dakuten mark converts as one unit rather than two.
Returns a cons (ZENKAKU . ADVANCE) where ADVANCE is 2 (two-char match) or 1."
  (let* ((c1  (char-to-string (aref string i)))
         (two (and (< (1+ i) len)
                   (concat c1 (char-to-string (aref string (1+ i))))))
         (z2  (and two (gethash two nskk--kana-hankaku-to-zenkaku-table))))
    (if z2
        (cons z2 2)
      (cons (or (gethash c1 nskk--kana-hankaku-to-zenkaku-table) c1)
            1))))

(defun nskk--kana-hankaku-string-to-zenkaku (string)
  "Convert hankaku katakana in STRING to zenkaku.
STRING must be a string; callers own the type dispatch."
  (let ((parts nil) (i 0) (len (length string)))
    (while (< i len)
      (let ((pair (nskk--kana-hankaku-lookup-at string i len)))
        (push (car pair) parts)
        (setq i (+ i (cdr pair)))))
    (apply #'concat (nreverse parts))))

(defun nskk-kana-hankaku-to-zenkaku (string-or-char)
  "Convert hankaku katakana STRING-OR-CHAR to zenkaku.
Handles combined dakuten/handakuten marks (e.g., \"ｶﾞ\" -> \"ガ\").
Unrecognized characters are passed through unchanged.
For any other type, returns STRING-OR-CHAR unchanged."
  (pcase string-or-char
    ((pred stringp) (nskk--kana-hankaku-string-to-zenkaku string-or-char))
    ((pred integerp)
     (let ((str (char-to-string string-or-char)))
       (or (gethash str nskk--kana-hankaku-to-zenkaku-table) str)))
    (_ string-or-char)))

;;;; JIS X 0208 Latin Width

(defun nskk-jisx0208-latin-char (char)
  "Return the JIS X 0208 full-width code point for ASCII CHAR.
Space maps to the ideographic space (U+3000); printable ASCII
\(U+0021-U+007E) maps to U+FF01-U+FF5E.  CHAR is returned unchanged
when it falls outside both ranges."
  (cond
   ((= char #x20) #x3000)
   ((and (>= char #x21) (<= char #x7E))
    (+ char nskk--kana-latin-width-offset))
   (t char)))

;;;; Prolog Facts Initialization

(defvar nskk--kana-initialized nil
  "Non-nil when zenkaku/hankaku Prolog facts have been populated from hash tables.
Classification predicates and Prolog range rules are asserted at load time.")

(nskk-prolog-<- (module-initialized-flag nskk--kana-initialized))

(defconst nskk--kana-conversion-rules
  '((hiragana      insert     identity)
    (katakana      insert     nskk-kana-string-hiragana-to-katakana)
    (katakana-半角  insert     nskk--hiragana-to-hankaku)
    (hiragana      normalize  identity)
    (katakana      normalize  nskk-kana-string-katakana-to-hiragana)
    (katakana-半角  normalize  nskk--hankaku-to-hiragana))
  "Rows of `kana-conversion/3': (MODE DIRECTION FUNCTION).
Queried by `nskk-kana-convert-for-mode' and `nskk-kana-normalize-for-lookup',
and by `nskk-input.el' and `nskk-henkan.el' across module boundaries.")

(defun/done nskk-kana-initialize ()
  "Populate the `kana-conversion/3' Prolog fact table.
Classification predicates and Prolog range rules are installed at module
load time; this function only populates the conversion table.
Idempotent: subsequent calls are no-ops."
  (unless nskk--kana-initialized
    (nskk-prolog-set-index 'kana-conversion 3 :hash)
    (nskk-prolog-bulk-facts kana-conversion nskk--kana-conversion-rules)
    (setq nskk--kana-initialized t)))

;;;; Cross-Script Conversion Helpers

(defun nskk--hiragana-to-hankaku (kana)
  "Convert hiragana KANA to half-width katakana via full-width intermediate."
  (nskk-kana-zenkaku-to-hankaku (nskk-kana-string-hiragana-to-katakana kana)))

(defun nskk--hankaku-to-hiragana (text)
  "Convert half-width katakana TEXT to hiragana via full-width intermediate."
  (nskk-kana-string-katakana-to-hiragana (nskk-kana-hankaku-to-zenkaku text)))

(defun nskk-kana-convert-for-mode (kana mode)
  "Convert hiragana KANA to the script expected by MODE.
MODE is a mode symbol used in `kana-conversion/3' (for example,
`hiragana', `katakana', or `katakana-半角').
Returns converted text as a string; unknown MODE falls back to identity."
  (nskk-kana-initialize)
  (funcall (or (nskk-prolog-query-value
                `(kana-conversion ,mode insert \?fn) '\?fn)
                'identity)
           kana))

(defun nskk-kana-normalize-for-lookup (text mode)
  "Normalize MODE-script TEXT to hiragana for dictionary lookup.
MODE is a mode symbol used in `kana-conversion/3'.
Returns a hiragana string suitable as a dictionary lookup key;
unknown MODE falls back to identity."
  (nskk-kana-initialize)
  (funcall (or (nskk-prolog-query-value
                `(kana-conversion ,mode normalize \?fn) '\?fn)
               'identity)
           text))

(provide 'nskk-kana)

;;; nskk-kana.el ends here
