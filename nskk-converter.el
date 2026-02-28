;;; nskk-converter.el --- Romaji to kana conversion engine -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

;; This file is NOT part of GNU Emacs.
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

;; Romaji-to-kana conversion engine for NSKK (Layer 2: Domain).
;;
;; Layer position: L2 (Domain) -- depends on nskk-prolog and nskk-custom.
;;
;; Implements pure conversion functions with no buffer or state side effects.
;; All conversion state (pending romaji, henkan phase) is managed by the
;; Application layer (nskk-input, nskk-henkan).
;;
;; It handles:
;; - Romaji-to-kana conversion using a table-driven longest-match approach
;; - Partial and complete romaji sequence matching
;; - Vowel-based conversion (a, i, u, e, o)
;; - Consonant-vowel combinations (ka, ki, ku, ke, ko, etc.)
;; - Special combinations (sha, shu, sho, cha, chu, cho, etc.)
;; - Small kana and digraph handling
;; - Sokuon (っ) detection: doubled consonant
;; - Hatsuon (ん) detection: n followed by non-vowel/non-y/non-n/non-quote
;;
;; Romaji styles are registered via `nskk-converter-register-style' and
;; loaded on demand.  Standard SKK style is initialized at load time.
;; AZIK style is registered by nskk-azik.el and loaded when configured.

;;;; Prolog Predicates
;;
;; This module maintains the following predicates in the global Prolog database:
;;
;; `romaji-to-kana/2' --- (romaji-to-kana ROMAJI KANA)
;;   Maps a romaji ASCII string to its kana equivalent string.
;;   Indexed with :trie for O(k) prefix lookups (longest-match-first).
;;   ~192 facts; populated by `nskk--initialize-romaji-table'.
;;   Example: (romaji-to-kana "ka" "か")
;;
;; `hatsuon-blocker/1' --- (hatsuon-blocker CHAR)
;;   Integer character codes that do NOT produce ん after `n'.
;;   Blocked set: ?a ?i ?u ?e ?o ?y ?n ?\'.
;;   Indexed with :hash for O(1) membership tests.
;;
;; `sokuon-blocker/1' --- (sokuon-blocker CHAR)
;;   Integer character codes that cannot be doubled to produce っ.
;;   Blocked set: ?a ?i ?u ?e ?o ?n.
;;   Indexed with :hash for O(1) membership tests.
;;
;; `hatsuon-trigger/1' --- (hatsuon-trigger ?C) :- (not (hatsuon-blocker ?C))
;;   Rule: character C produces ん when it is NOT a hatsuon-blocker.
;;   Queried by `nskk-convert-n--internal' for context-sensitive ん insertion.
;;
;; `sokuon-eligible/1' --- (sokuon-eligible ?C) :- (not (sokuon-blocker ?C))
;;   Rule: character C is eligible for っ doubling when NOT a sokuon-blocker.
;;   Queried by `nskk-convert-romaji--internal' for double-consonant detection.

;;; Code:

(require 'cl-lib)
(require 'nskk-prolog)
(require 'nskk-custom)

;; Romaji conversion table
;; Maps romaji sequences to their kana equivalents (as strings for multi-byte)
(defvar nskk--romaji-table
  (make-hash-table :test 'equal :size 200)
  "Romaji to kana conversion table.")

(defvar nskk--style-registry '((standard . nskk--initialize-romaji-table))
  "Registry mapping style symbols to their initialization functions.")

(defconst nskk--sokuon-blockers '(?a ?i ?u ?e ?o ?n)
  "Characters that cannot be doubled to produce っ (small tsu).")

(defconst nskk--hatsuon-blockers '(?a ?i ?u ?e ?o ?y ?n ?')
  "Characters after n that do NOT trigger ん conversion.")

(defun nskk--initialize-romaji-table ()
  "Initialize the romaji conversion table as Prolog facts."
  ;; Clear existing romaji rules
  (nskk-prolog-retract-all 'romaji-to-kana 2)
  (nskk-prolog-retract-all 'hatsuon-blocker 1)
  (nskk-prolog-retract-all 'sokuon-blocker 1)
  (nskk-prolog-retract-all 'hatsuon-trigger 1)
  (nskk-prolog-retract-all 'sokuon-eligible 1)

  ;; Also clear the hash table for backward compat
  (clrhash nskk--romaji-table)

  ;; Configure trie-based indexing for longest-match-first
  (nskk-prolog-set-index 'romaji-to-kana 2 :trie)

  ;; Vowels
  (nskk-converter-add-rule "a" "あ")
  (nskk-converter-add-rule "i" "い")
  (nskk-converter-add-rule "u" "う")
  (nskk-converter-add-rule "e" "え")
  (nskk-converter-add-rule "o" "お")

  ;; K row
  (nskk-converter-add-rule "ka" "か")
  (nskk-converter-add-rule "ki" "き")
  (nskk-converter-add-rule "ku" "く")
  (nskk-converter-add-rule "ke" "け")
  (nskk-converter-add-rule "ko" "こ")

  ;; G row
  (nskk-converter-add-rule "ga" "が")
  (nskk-converter-add-rule "gi" "ぎ")
  (nskk-converter-add-rule "gu" "ぐ")
  (nskk-converter-add-rule "ge" "げ")
  (nskk-converter-add-rule "go" "ご")

  ;; S row
  (nskk-converter-add-rule "sa" "さ")
  (nskk-converter-add-rule "shi" "し")
  (nskk-converter-add-rule "si" "し")
  (nskk-converter-add-rule "su" "す")
  (nskk-converter-add-rule "se" "せ")
  (nskk-converter-add-rule "so" "そ")

  ;; Z row
  (nskk-converter-add-rule "za" "ざ")
  (nskk-converter-add-rule "ji" "じ")
  (nskk-converter-add-rule "zi" "じ")
  (nskk-converter-add-rule "zu" "ず")
  (nskk-converter-add-rule "ze" "ぜ")
  (nskk-converter-add-rule "zo" "ぞ")

  ;; T row
  (nskk-converter-add-rule "ta" "た")
  (nskk-converter-add-rule "chi" "ち")
  (nskk-converter-add-rule "ti" "ち")
  (nskk-converter-add-rule "tsu" "つ")
  (nskk-converter-add-rule "tu" "つ")
  (nskk-converter-add-rule "te" "て")
  (nskk-converter-add-rule "to" "と")

  ;; D row
  (nskk-converter-add-rule "da" "だ")
  (nskk-converter-add-rule "di" "ぢ")
  (nskk-converter-add-rule "du" "づ")
  (nskk-converter-add-rule "de" "で")
  (nskk-converter-add-rule "do" "ど")

  ;; N row
  (nskk-converter-add-rule "na" "な")
  (nskk-converter-add-rule "ni" "に")
  (nskk-converter-add-rule "nu" "ぬ")
  (nskk-converter-add-rule "ne" "ね")
  (nskk-converter-add-rule "no" "の")
  (nskk-converter-add-rule "n'" "ん")
  (nskk-converter-add-rule "nn" "ん")

  ;; H row
  (nskk-converter-add-rule "ha" "は")
  (nskk-converter-add-rule "hi" "ひ")
  (nskk-converter-add-rule "fu" "ふ")
  (nskk-converter-add-rule "hu" "ふ")
  (nskk-converter-add-rule "he" "へ")
  (nskk-converter-add-rule "ho" "ほ")

  ;; B row
  (nskk-converter-add-rule "ba" "ば")
  (nskk-converter-add-rule "bi" "び")
  (nskk-converter-add-rule "bu" "ぶ")
  (nskk-converter-add-rule "be" "べ")
  (nskk-converter-add-rule "bo" "ぼ")

  ;; P row
  (nskk-converter-add-rule "pa" "ぱ")
  (nskk-converter-add-rule "pi" "ぴ")
  (nskk-converter-add-rule "pu" "ぷ")
  (nskk-converter-add-rule "pe" "ぺ")
  (nskk-converter-add-rule "po" "ぽ")

  ;; M row
  (nskk-converter-add-rule "ma" "ま")
  (nskk-converter-add-rule "mi" "み")
  (nskk-converter-add-rule "mu" "む")
  (nskk-converter-add-rule "me" "め")
  (nskk-converter-add-rule "mo" "も")

  ;; Y row
  (nskk-converter-add-rule "ya" "や")
  (nskk-converter-add-rule "yu" "ゆ")
  (nskk-converter-add-rule "yo" "よ")

  ;; R row
  (nskk-converter-add-rule "ra" "ら")
  (nskk-converter-add-rule "ri" "り")
  (nskk-converter-add-rule "ru" "る")
  (nskk-converter-add-rule "re" "れ")
  (nskk-converter-add-rule "ro" "ろ")

  ;; W row
  (nskk-converter-add-rule "wa" "わ")
  (nskk-converter-add-rule "wo" "を")

  ;; Special combinations
  (nskk-converter-add-rule "sha" "しゃ")
  (nskk-converter-add-rule "shu" "しゅ")
  (nskk-converter-add-rule "she" "しぇ")
  (nskk-converter-add-rule "sho" "しょ")
  (nskk-converter-add-rule "tsa" "つぁ")
  (nskk-converter-add-rule "tsi" "つぃ")
  (nskk-converter-add-rule "tse" "つぇ")
  (nskk-converter-add-rule "tso" "つぉ")
  (nskk-converter-add-rule "fa" "ふぁ")
  (nskk-converter-add-rule "fi" "ふぃ")
  (nskk-converter-add-rule "fe" "ふぇ")
  (nskk-converter-add-rule "fo" "ふぉ")

  ;; Digraphs
  (nskk-converter-add-rule "kya" "きゃ")
  (nskk-converter-add-rule "kyu" "きゅ")
  (nskk-converter-add-rule "kyo" "きょ")
  (nskk-converter-add-rule "gya" "ぎゃ")
  (nskk-converter-add-rule "gyu" "ぎゅ")
  (nskk-converter-add-rule "gyo" "ぎょ")
  (nskk-converter-add-rule "ja" "じゃ")
  (nskk-converter-add-rule "ju" "じゅ")
  (nskk-converter-add-rule "je" "じぇ")
  (nskk-converter-add-rule "jo" "じょ")
  (nskk-converter-add-rule "cha" "ちゃ")
  (nskk-converter-add-rule "chu" "ちゅ")
  (nskk-converter-add-rule "che" "ちぇ")
  (nskk-converter-add-rule "cho" "ちょ")
  (nskk-converter-add-rule "nya" "にゃ")
  (nskk-converter-add-rule "nyu" "にゅ")
  (nskk-converter-add-rule "nyo" "にょ")
  (nskk-converter-add-rule "hya" "ひゃ")
  (nskk-converter-add-rule "hyu" "ひゅ")
  (nskk-converter-add-rule "hyo" "ひょ")
  (nskk-converter-add-rule "bya" "びゃ")
  (nskk-converter-add-rule "byu" "びゅ")
  (nskk-converter-add-rule "byo" "びょ")
  (nskk-converter-add-rule "pya" "ぴゃ")
  (nskk-converter-add-rule "pyu" "ぴゅ")
  (nskk-converter-add-rule "pyo" "ぴょ")
  (nskk-converter-add-rule "mya" "みゃ")
  (nskk-converter-add-rule "myu" "みゅ")
  (nskk-converter-add-rule "myo" "みょ")
  (nskk-converter-add-rule "rya" "りゃ")
  (nskk-converter-add-rule "ryu" "りゅ")
  (nskk-converter-add-rule "ryo" "りょ")

  ;; Small kana
  (nskk-converter-add-rule "la" "ぁ")
  (nskk-converter-add-rule "li" "ぃ")
  (nskk-converter-add-rule "lu" "ぅ")
  (nskk-converter-add-rule "le" "ぇ")
  (nskk-converter-add-rule "lo" "ぉ")
  (nskk-converter-add-rule "xa" "ぁ")
  (nskk-converter-add-rule "xi" "ぃ")
  (nskk-converter-add-rule "xu" "ぅ")
  (nskk-converter-add-rule "xe" "ぇ")
  (nskk-converter-add-rule "xo" "ぉ")
  (nskk-converter-add-rule "xya" "ゃ")
  (nskk-converter-add-rule "xyu" "ゅ")
  (nskk-converter-add-rule "xyo" "ょ")
  (nskk-converter-add-rule "lya" "ゃ")
  (nskk-converter-add-rule "lyu" "ゅ")
  (nskk-converter-add-rule "lyo" "ょ")
  (nskk-converter-add-rule "xtsu" "っ")
  (nskk-converter-add-rule "xtu" "っ")
  (nskk-converter-add-rule "ltsu" "っ")
  (nskk-converter-add-rule "ltu" "っ")

  ;; V row
  (nskk-converter-add-rule "va" "ゔぁ")
  (nskk-converter-add-rule "vi" "ゔぃ")
  (nskk-converter-add-rule "vu" "ゔ")
  (nskk-converter-add-rule "ve" "ゔぇ")
  (nskk-converter-add-rule "vo" "ゔぉ")
  (nskk-converter-add-rule "vya" "ゔゃ")
  (nskk-converter-add-rule "vyu" "ゔゅ")
  (nskk-converter-add-rule "vyo" "ゔょ")

  ;; Additional digraphs
  (nskk-converter-add-rule "dya" "ぢゃ")
  (nskk-converter-add-rule "dyu" "ぢゅ")
  (nskk-converter-add-rule "dyo" "ぢょ")

  ;; Alternative rows
  (nskk-converter-add-rule "zya" "じゃ")
  (nskk-converter-add-rule "zyu" "じゅ")
  (nskk-converter-add-rule "zyo" "じょ")
  (nskk-converter-add-rule "tya" "ちゃ")
  (nskk-converter-add-rule "tyu" "ちゅ")
  (nskk-converter-add-rule "tyo" "ちょ")
  (nskk-converter-add-rule "sya" "しゃ")
  (nskk-converter-add-rule "syu" "しゅ")
  (nskk-converter-add-rule "sye" "しぇ")
  (nskk-converter-add-rule "syo" "しょ")
  (nskk-converter-add-rule "jya" "じゃ")
  (nskk-converter-add-rule "jyu" "じゅ")
  (nskk-converter-add-rule "jye" "じぇ")
  (nskk-converter-add-rule "jyo" "じょ")

  ;; -ye extensions
  (nskk-converter-add-rule "tye" "ちぇ")
  (nskk-converter-add-rule "zye" "じぇ")
  (nskk-converter-add-rule "dye" "ぢぇ")
  (nskk-converter-add-rule "gye" "ぎぇ")
  (nskk-converter-add-rule "kye" "きぇ")
  (nskk-converter-add-rule "nye" "にぇ")
  (nskk-converter-add-rule "hye" "ひぇ")
  (nskk-converter-add-rule "bye" "びぇ")
  (nskk-converter-add-rule "pye" "ぴぇ")
  (nskk-converter-add-rule "mye" "みぇ")
  (nskk-converter-add-rule "rye" "りぇ")

  ;; Th/Dh/Wh rows
  (nskk-converter-add-rule "tha" "てぁ")
  (nskk-converter-add-rule "thi" "てぃ")
  (nskk-converter-add-rule "thu" "てゅ")
  (nskk-converter-add-rule "the" "てぇ")
  (nskk-converter-add-rule "tho" "てょ")
  (nskk-converter-add-rule "dha" "でぁ")
  (nskk-converter-add-rule "dhi" "でぃ")
  (nskk-converter-add-rule "dhu" "でゅ")
  (nskk-converter-add-rule "dhe" "でぇ")
  (nskk-converter-add-rule "dho" "でょ")
  (nskk-converter-add-rule "wha" "うぁ")
  (nskk-converter-add-rule "whi" "うぃ")
  (nskk-converter-add-rule "whu" "う")
  (nskk-converter-add-rule "whe" "うぇ")
  (nskk-converter-add-rule "who" "うぉ")
  (nskk-converter-add-rule "wi" "ゐ")
  (nskk-converter-add-rule "we" "ゑ")

  ;; Small wa, ka, ke
  (nskk-converter-add-rule "xwa" "ゎ")
  (nskk-converter-add-rule "xka" "ゕ")
  (nskk-converter-add-rule "xke" "ゖ")

  ;; Long vowel mark
  (nskk-converter-add-rule "-" "ー")

  ;; Character classification for hatsuon/sokuon rules
  ;; Hatsuon blockers: chars after 'n' that do NOT trigger ん
  (nskk-prolog-set-index 'hatsuon-blocker 1 :hash)
  (dolist (c '(?a ?i ?u ?e ?o ?y ?n ?'))
    (nskk-prolog-assert (list (list 'hatsuon-blocker c))))

  ;; Sokuon blockers: chars that cannot be doubled for っ
  (nskk-prolog-set-index 'sokuon-blocker 1 :hash)
  (dolist (c '(?a ?i ?u ?e ?o ?n))
    (nskk-prolog-assert (list (list 'sokuon-blocker c))))

  ;; Hatsuon trigger: n followed by this char produces ん
  (nskk-prolog-<- (hatsuon-trigger \?c)
    (not (hatsuon-blocker \?c)))

  ;; Sokuon eligible: doubled char produces っ
  (nskk-prolog-<- (sokuon-eligible \?c)
    (not (sokuon-blocker \?c)))

  ;; Auto-derive incomplete markers from populated hash table
  (nskk-converter--populate-incomplete-markers))

(defun nskk-converter--populate-incomplete-markers ()
  "Populate nskk--romaji-table with :incomplete for all proper romaji prefixes.
Auto-derived from the complete romaji entries already in the table.
This eliminates manual maintenance of prefix lists when adding new rules."
  (let (keys)
    (maphash (lambda (k _v) (when (stringp k) (push k keys)))
             nskk--romaji-table)
    (dolist (romaji keys)
      (let ((len (length romaji)))
        (dotimes (i (1- len))
          (let ((prefix (substring romaji 0 (1+ i))))
            (unless (gethash prefix nskk--romaji-table)
              (puthash prefix :incomplete nskk--romaji-table))))))))

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

(defun nskk-convert-n--internal (remaining)
  "Handle all ん-producing cases where REMAINING begins with character n.
Precondition: REMAINING must be a non-empty string whose first character is ?n.
Returns (kana . rest) cons cell if ん is produced, or nil to fall through
to normal table-driven conversion (e.g. for \"na\" -> \"な\")."
  (let ((len (length remaining)))
    (cond
     ;; Standalone \"n\" at end of input
     ((= len 1)
      (cons "ん" nil))
     ;; \"nn\" sequence: double-n always produces ん
     ((= (aref remaining 1) ?n)
      (if (= len 2)
          (cons "ん" nil)
        (cons "ん" (substring remaining 1))))
     ;; \"n'\" sequence: n followed by single quote (ASCII 39)
     ((= (aref remaining 1) 39)
      (cons "ん" (if (> len 2) (substring remaining 2) nil)))
     ;; \"n\" before a consonant (not in hatsuon-blocker set)
     ((not (memq (aref remaining 1) nskk--hatsuon-blockers))
      (cons "ん" (substring remaining 1)))
     ;; n followed by vowel/y/etc: fall through to table lookup (\"na\"->\"な\")
     (t nil))))

(defun nskk-convert-romaji--internal (input)
  "Internal romaji conversion for INPUT string.
Accumulates converted parts as a list and joins at the end to avoid
the O(n²) string allocation of repeated `concat' calls in a loop."
  (let ((parts nil)   ; list of converted string segments (prepended, reversed at end)
        (remaining input)
        (len 0))
    (while (and remaining (> (setq len (length remaining)) 0))
      (let* ((c0 (aref remaining 0))
             (n-result (when (= c0 ?n) (nskk-convert-n--internal remaining))))
        (cond
         ;; Double consonant (sokuon): same ASCII consonant repeated, not vowel/n.
         ;; The (< c0 128) guard ensures kana characters (e.g. "かか") never
         ;; accidentally trigger sokuon, keeping nskk-convert-romaji idempotent.
         ((and (> len 1)
               (< c0 128)
               (= c0 (aref remaining 1))
               (not (memq c0 nskk--sokuon-blockers)))
          (push "っ" parts)
          (setq remaining (substring remaining 1)))

         ;; n-prefix producing ん (falls through to table if n-result is nil)
         ((and (= c0 ?n) n-result)
          (push (car n-result) parts)
          (setq remaining (cdr n-result)))

         ;; Normal table-driven conversion (including "na", "ni", etc.)
         (t
          (let ((conv (nskk-converter-convert remaining)))
            (if (or (null conv) (eq (car conv) :incomplete))
                (progn
                  (push remaining parts)
                  (setq remaining nil))
              (push (car conv) parts)
              (setq remaining (let ((rest (cdr conv)))
                                (if (and (stringp rest) (> (length rest) 0))
                                    rest
                                  nil)))))))))
    (apply #'concat (nreverse parts))))

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
Returns list of (romaji . kana) pairs using Prolog trie prefix search."
  (when (stringp romaji)
    (nskk-prolog-trie-prefix-search 'romaji-to-kana 2 romaji)))

(defun nskk-converter-add-rule (romaji kana)
  "Add ROMAJI -> KANA mapping.
Adds to both Prolog database and hash table."
  (puthash romaji kana nskk--romaji-table)
  (when (stringp kana)
    (nskk-prolog-assert (list (list 'romaji-to-kana romaji kana)))))

(defun nskk-converter-remove-rule (romaji)
  "Remove ROMAJI from conversion table."
  (prog1 (remhash romaji nskk--romaji-table)
    (nskk-prolog-retract (list 'romaji-to-kana romaji '\?_))))

(defun nskk-converter-get-rule (romaji)
  "Return the KANA mapped to ROMAJI, or nil if not found."
  (gethash romaji nskk--romaji-table))

(defun nskk-converter-register-style (style init-fn)
  "Register INIT-FN as the initialization function for STYLE.
INIT-FN is called with no arguments and should populate the romaji table."
  (setf (alist-get style nskk--style-registry) init-fn))

(defun nskk-converter-load-style (style)
  "Load romaji rules for STYLE.
Clears existing table and calls the registered initialization function.
Valid styles are defined in `nskk--style-registry'.
Signals user-error if STYLE is not a registered style."
  (let ((init-fn (alist-get style nskk--style-registry)))
    (if init-fn
        (progn
          (clrhash nskk--romaji-table)
          (nskk-prolog-retract-all 'romaji-to-kana 2)
          (funcall init-fn)
          (nskk-converter--populate-incomplete-markers)
          style)
      (user-error "Unknown romaji style: %s" style))))

(defmacro nskk-converter-define-style (name docstring &rest rules)
  "Define a new input style NAME with RULES.
DOCSTRING describes the style.
RULES is a list of (romaji kana) pairs."
  (declare (doc-string 2) (indent 2) (debug (symbolp stringp body)))
  `(progn
     (defun ,(intern (format "nskk--init-%s-rules" name)) ()
       ,docstring
       ,@(mapcar (lambda (rule)
                   `(nskk-converter-add-rule ,(car rule) ,(cadr rule)))
                 rules))
     (nskk-converter-register-style ',name
       ',(intern (format "nskk--init-%s-rules" name)))))

(defvar nskk--converter-initialized nil
  "Non-nil when the romaji-to-kana conversion table has been initialized.")

(defun nskk-converter-initialize ()
  "Initialize the romaji-to-kana conversion table.
Idempotent: subsequent calls are no-ops."
  (unless nskk--converter-initialized
    (nskk--initialize-romaji-table)
    (setq nskk--converter-initialized t)))

(provide 'nskk-converter)

;;; nskk-converter.el ends here
