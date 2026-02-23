;;; nskk-converter.el --- Romaji to kana conversion engine -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda

;; Author: Takeshi Umeda <takeokunn@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

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

;; This module implements the core romaji-to-kana conversion engine (Layer 4).
;;
;; Layer Responsibilities:
;; - Core Engine Layer implements pure conversion functions
;; - NO state management - state is handled by Application Layer (Layer 3)
;; - Provides conversion utilities that are independent of application state
;;
;; It handles:
;; - Romaji to kana conversion using a table-driven approach
;; - Partial and complete romaji sequence matching
;; - Vowel-based conversion (a, i, u, e, o)
;; - Consonant-vowel combinations (ka, ki, ku, ke, ko, etc.)
;; - Special combinations (sha, shu, sho, cha, chu, cho, etc.)
;; - Small kana and digraph handling
;;
;; Performance target: < 0.1ms for single conversion operation
;; Memory: Minimal overhead with shared romaji table

;;; Code:

(require 'cl-lib)

;; Romaji conversion table
;; Maps romaji sequences to their kana equivalents (as strings for multi-byte)
(defvar nskk--romaji-table
  (make-hash-table :test 'equal :size 200)
  "Romaji to kana conversion table.")

(defun nskk--initialize-romaji-table ()
  "Initialize the romaji conversion table."
  (clrhash nskk--romaji-table)

  ;; Vowels
  (puthash "a" "あ" nskk--romaji-table)
  (puthash "i" "い" nskk--romaji-table)
  (puthash "u" "う" nskk--romaji-table)
  (puthash "e" "え" nskk--romaji-table)
  (puthash "o" "お" nskk--romaji-table)

  ;; K row
  (puthash "ka" "か" nskk--romaji-table)
  (puthash "ki" "き" nskk--romaji-table)
  (puthash "ku" "く" nskk--romaji-table)
  (puthash "ke" "け" nskk--romaji-table)
  (puthash "ko" "こ" nskk--romaji-table)

  ;; G row
  (puthash "ga" "が" nskk--romaji-table)
  (puthash "gi" "ぎ" nskk--romaji-table)
  (puthash "gu" "ぐ" nskk--romaji-table)
  (puthash "ge" "げ" nskk--romaji-table)
  (puthash "go" "ご" nskk--romaji-table)

  ;; S row
  (puthash "sa" "さ" nskk--romaji-table)
  (puthash "shi" "し" nskk--romaji-table)
  (puthash "si" "し" nskk--romaji-table)
  (puthash "su" "す" nskk--romaji-table)
  (puthash "se" "せ" nskk--romaji-table)
  (puthash "so" "そ" nskk--romaji-table)

  ;; Z row
  (puthash "za" "ざ" nskk--romaji-table)
  (puthash "ji" "じ" nskk--romaji-table)
  (puthash "zi" "じ" nskk--romaji-table)
  (puthash "zu" "ず" nskk--romaji-table)
  (puthash "ze" "ぜ" nskk--romaji-table)
  (puthash "zo" "ぞ" nskk--romaji-table)

  ;; T row
  (puthash "ta" "た" nskk--romaji-table)
  (puthash "chi" "ち" nskk--romaji-table)
  (puthash "ti" "ち" nskk--romaji-table)
  (puthash "tsu" "つ" nskk--romaji-table)
  (puthash "tu" "つ" nskk--romaji-table)
  (puthash "te" "て" nskk--romaji-table)
  (puthash "to" "と" nskk--romaji-table)

  ;; D row
  (puthash "da" "だ" nskk--romaji-table)
  (puthash "di" "ぢ" nskk--romaji-table)
  (puthash "du" "づ" nskk--romaji-table)
  (puthash "de" "で" nskk--romaji-table)
  (puthash "do" "ど" nskk--romaji-table)

  ;; N row (including special n')
  (puthash "na" "な" nskk--romaji-table)
  (puthash "ni" "に" nskk--romaji-table)
  (puthash "nu" "ぬ" nskk--romaji-table)
  (puthash "ne" "ね" nskk--romaji-table)
  (puthash "no" "の" nskk--romaji-table)
  (puthash "n'" "ん" nskk--romaji-table)
  (puthash "nn" "ん" nskk--romaji-table)

  ;; H row
  (puthash "ha" "は" nskk--romaji-table)
  (puthash "hi" "ひ" nskk--romaji-table)
  (puthash "fu" "ふ" nskk--romaji-table)
  (puthash "hu" "ふ" nskk--romaji-table)
  (puthash "he" "へ" nskk--romaji-table)
  (puthash "ho" "ほ" nskk--romaji-table)

  ;; B row
  (puthash "ba" "ば" nskk--romaji-table)
  (puthash "bi" "び" nskk--romaji-table)
  (puthash "bu" "ぶ" nskk--romaji-table)
  (puthash "be" "べ" nskk--romaji-table)
  (puthash "bo" "ぼ" nskk--romaji-table)

  ;; P row
  (puthash "pa" "ぱ" nskk--romaji-table)
  (puthash "pi" "ぴ" nskk--romaji-table)
  (puthash "pu" "ぷ" nskk--romaji-table)
  (puthash "pe" "ぺ" nskk--romaji-table)
  (puthash "po" "ぽ" nskk--romaji-table)

  ;; M row
  (puthash "ma" "ま" nskk--romaji-table)
  (puthash "mi" "み" nskk--romaji-table)
  (puthash "mu" "む" nskk--romaji-table)
  (puthash "me" "め" nskk--romaji-table)
  (puthash "mo" "も" nskk--romaji-table)

  ;; Y row
  (puthash "ya" "や" nskk--romaji-table)
  (puthash "yu" "ゆ" nskk--romaji-table)
  (puthash "yo" "よ" nskk--romaji-table)

  ;; R row
  (puthash "ra" "ら" nskk--romaji-table)
  (puthash "ri" "り" nskk--romaji-table)
  (puthash "ru" "る" nskk--romaji-table)
  (puthash "re" "れ" nskk--romaji-table)
  (puthash "ro" "ろ" nskk--romaji-table)

  ;; W row
  (puthash "wa" "わ" nskk--romaji-table)
  (puthash "wo" "を" nskk--romaji-table)

  ;; Special combinations with small kana
  (puthash "sha" "しゃ" nskk--romaji-table)
  (puthash "shu" "しゅ" nskk--romaji-table)
  (puthash "sho" "しょ" nskk--romaji-table)
  (puthash "tsa" "つぁ" nskk--romaji-table)
  (puthash "tsi" "つぃ" nskk--romaji-table)
  (puthash "tse" "つぇ" nskk--romaji-table)
  (puthash "tso" "つぉ" nskk--romaji-table)
  (puthash "fa" "ふぁ" nskk--romaji-table)
  (puthash "fi" "ふぃ" nskk--romaji-table)
  (puthash "fe" "ふぇ" nskk--romaji-table)
  (puthash "fo" "ふぉ" nskk--romaji-table)

  ;; Digraph combinations
  (puthash "kya" "きゃ" nskk--romaji-table)
  (puthash "kyu" "きゅ" nskk--romaji-table)
  (puthash "kyo" "きょ" nskk--romaji-table)

  (puthash "gya" "ぎゃ" nskk--romaji-table)
  (puthash "gyu" "ぎゅ" nskk--romaji-table)
  (puthash "gyo" "ぎょ" nskk--romaji-table)

  (puthash "ja" "じゃ" nskk--romaji-table)
  (puthash "ju" "じゅ" nskk--romaji-table)
  (puthash "jo" "じょ" nskk--romaji-table)

  (puthash "cha" "ちゃ" nskk--romaji-table)
  (puthash "chu" "ちゅ" nskk--romaji-table)
  (puthash "cho" "ちょ" nskk--romaji-table)

  (puthash "nya" "にゃ" nskk--romaji-table)
  (puthash "nyu" "にゅ" nskk--romaji-table)
  (puthash "nyo" "にょ" nskk--romaji-table)

  (puthash "hya" "ひゃ" nskk--romaji-table)
  (puthash "hyu" "ひゅ" nskk--romaji-table)
  (puthash "hyo" "ひょ" nskk--romaji-table)

  (puthash "bya" "びゃ" nskk--romaji-table)
  (puthash "byu" "びゅ" nskk--romaji-table)
  (puthash "byo" "びょ" nskk--romaji-table)

  (puthash "pya" "ぴゃ" nskk--romaji-table)
  (puthash "pyu" "ぴゅ" nskk--romaji-table)
  (puthash "pyo" "ぴょ" nskk--romaji-table)

  (puthash "mya" "みゃ" nskk--romaji-table)
  (puthash "myu" "みゅ" nskk--romaji-table)
  (puthash "myo" "みょ" nskk--romaji-table)

  (puthash "rya" "りゃ" nskk--romaji-table)
  (puthash "ryu" "りゅ" nskk--romaji-table)
  (puthash "ryo" "りょ" nskk--romaji-table)

  ;; Small vowels for explicit input
  (puthash "la" "ぁ" nskk--romaji-table)
  (puthash "li" "ぃ" nskk--romaji-table)
  (puthash "lu" "ぅ" nskk--romaji-table)
  (puthash "le" "ぇ" nskk--romaji-table)
  (puthash "lo" "ぉ" nskk--romaji-table)

  (puthash "xa" "ぁ" nskk--romaji-table)
  (puthash "xi" "ぃ" nskk--romaji-table)
  (puthash "xu" "ぅ" nskk--romaji-table)
  (puthash "xe" "ぇ" nskk--romaji-table)
  (puthash "xo" "ぉ" nskk--romaji-table)

  ;; Small ya/yu/yo
  (puthash "xya" "ゃ" nskk--romaji-table)
  (puthash "xyu" "ゅ" nskk--romaji-table)
  (puthash "xyo" "ょ" nskk--romaji-table)

  ;; Small tsu
  (puthash "xtsu" "っ" nskk--romaji-table)
  (puthash "xtu" "っ" nskk--romaji-table)

  ;; Iteration mark
  (puthash "-" "ー" nskk--romaji-table)

  ;; Consonant-only (for partial match)
  (puthash "k" :incomplete nskk--romaji-table)
  (puthash "g" :incomplete nskk--romaji-table)
  (puthash "s" :incomplete nskk--romaji-table)
  (puthash "z" :incomplete nskk--romaji-table)
  (puthash "t" :incomplete nskk--romaji-table)
  (puthash "d" :incomplete nskk--romaji-table)
  (puthash "n" :incomplete nskk--romaji-table)
  (puthash "h" :incomplete nskk--romaji-table)
  (puthash "b" :incomplete nskk--romaji-table)
  (puthash "p" :incomplete nskk--romaji-table)
  (puthash "m" :incomplete nskk--romaji-table)
  (puthash "y" :incomplete nskk--romaji-table)
  (puthash "r" :incomplete nskk--romaji-table)
  (puthash "w" :incomplete nskk--romaji-table))

;; Initialize on load
(nskk--initialize-romaji-table)

(define-inline nskk-converter-lookup (romaji)
  "Look up ROMAJI in conversion table.
Returns kana string if found, nil if not found.
Returns :incomplete if ROMAJI is a partial match.
Declared inline for hot path optimization."
  (inline-letevals (romaji)
    (inline-quote
     (when (stringp ,romaji)
       (gethash ,romaji nskk--romaji-table)))))

(defun nskk-convert-romaji (romaji)
  "Convert full ROMAJI string to kana.
Returns converted kana string for string input, nil for nil input.
This is a convenience wrapper for nskk-converter-convert."
  (cond
   ((null romaji) nil)
   ((not (stringp romaji)) nil)
   ((zerop (length romaji)) "")
   (t
    (nskk-convert-romaji--internal (downcase romaji)))))

(defun nskk-convert-romaji--internal (input)
  "Internal romaji conversion for INPUT string."
  (let ((result nil)
        (remaining input)
        (iteration 0)
        (len 0))
    (while (and remaining (> (setq len (length remaining)) 0) (< iteration 100))
      (let ((c0 (aref remaining 0)))
        (cond
         ;; Double consonant (sokuon): same ASCII consonant repeated, not vowel/n
         ((and (> len 1)
               (>= c0 ?a) (<= c0 ?z)
               (not (memq c0 '(?a ?i ?u ?e ?o ?n)))
               (= c0 (aref remaining 1)))
          (setq result (concat result "っ"))
          (setq remaining (substring remaining 1)))

         ;; Standalone 'n' at end of string
         ((and (= len 1) (= c0 ?n))
          (setq result (concat result "ん"))
          (setq remaining nil))

         ;; "nn" at end of string -> "ん"
         ((and (= c0 ?n) (= len 2) (= (aref remaining 1) ?n))
          (setq result (concat result "ん"))
          (setq remaining nil))

         ;; "nn" followed by more characters -> "ん" + continue from second n
         ((and (= c0 ?n) (> len 2) (= (aref remaining 1) ?n))
          (setq result (concat result "ん"))
          (setq remaining (substring remaining 1)))

         ;; "n'" -> "ん"  (39 = ASCII code for single quote)
         ((and (= c0 ?n) (> len 1) (= (aref remaining 1) 39))
          (setq result (concat result "ん"))
          (setq remaining (if (> len 2) (substring remaining 2) nil)))

         ;; 'n' before consonant (not vowel, not y, not n, not quote)
         ((and (= c0 ?n) (> len 1)
               (not (memq (aref remaining 1) '(?a ?i ?u ?e ?o ?y ?n 39))))
          (setq result (concat result "ん"))
          (setq remaining (substring remaining 1)))

         ;; Normal table-driven conversion
         (t
          (let ((conv (nskk-converter-convert remaining)))
            (if (or (null conv) (eq (car conv) :incomplete))
                (progn
                  (setq result (concat result remaining))
                  (setq remaining nil))
              (setq result (concat result (car conv)))
              (setq remaining (let ((rest (cdr conv)))
                                (if (and (stringp rest) (> (length rest) 0))
                                    rest
                                  nil))))))))
      (setq iteration (1+ iteration)))
    result))

(defun nskk-converter-convert (romaji)
  "Convert ROMAJI string to kana.
Returns (kana . remaining-romaji) cons cell.
If conversion fails, returns nil.
If incomplete, returns (:incomplete . romaji)."
  (when (and (stringp romaji) (> (length romaji) 0))
    ;; Try longest match first (up to 4 chars for digraphs)
    (cl-loop for len from (min 4 (length romaji)) downto 1
             for prefix = (substring romaji 0 len)
             for result = (nskk-converter-lookup prefix)
             when (and result (stringp result))
             return (cons result (substring romaji len))
             when (eq result :incomplete)
             return (cons :incomplete romaji))))

(defun nskk-converter-get-possible-completions (romaji)
  "Get list of possible completions for ROMAJI prefix.
Returns list of (romaji . kana) pairs."
  (when (stringp romaji)
    (let ((completions '()))
      (maphash
       (lambda (key value)
         (when (and (string-prefix-p romaji key)
                    (stringp value))
           (push (cons key value) completions)))
       nskk--romaji-table)
      (nreverse completions))))

(provide 'nskk-converter)

;;; nskk-converter.el ends here
