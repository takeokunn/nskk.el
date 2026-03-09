;;; nskk-converter.el --- Romaji to kana conversion engine -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
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
;; Character classification for hatsuon and sokuon is handled by the
;; `nskk--hatsuon-blockers' and `nskk--sokuon-blockers' defconsts (plain
;; Emacs Lisp lists), not Prolog predicates.

;;; Code:

(require 'cl-lib)
(require 'nskk-cps-macros)
(require 'nskk-prolog)
(require 'nskk-custom)

;; Romaji conversion table
;; Maps romaji sequences to their kana equivalents (as strings for multi-byte)
(defvar nskk--romaji-table
  (make-hash-table :test 'equal :size 200)
  "Romaji to kana conversion table.")

(defvar nskk--style-registry '((standard . nskk--initialize-romaji-table))
  "Registry mapping style symbols to their initialization functions.
An alist of the form ((STYLE-SYMBOL . INIT-FN) ...) where INIT-FN is a
zero-argument function that populates the romaji table via
`nskk-converter-add-rule'.  Use `nskk-converter-register-style' to add
entries; do not modify this variable directly.")

(defconst nskk--hatsuon-blockers '(?a ?i ?u ?e ?o ?y ?n ?')
  "Characters after n that do not trigger ん insertion.
When the char following n is in this list, n stays in the buffer.")

(defconst nskk--sokuon-blockers '(?a ?i ?u ?e ?o ?n)
  "Characters that cannot be doubled to produce っ.
Vowels and n are excluded; other consonants trigger sokuon.")

(defconst nskk--standard-romaji-rules
  '(;; Vowels
    ("a" "あ") ("i" "い") ("u" "う") ("e" "え") ("o" "お")
    ;; K row
    ("ka" "か") ("ki" "き") ("ku" "く") ("ke" "け") ("ko" "こ")
    ("kya" "きゃ") ("kyu" "きゅ") ("kye" "きぇ") ("kyo" "きょ")
    ;; G row
    ("ga" "が") ("gi" "ぎ") ("gu" "ぐ") ("ge" "げ") ("go" "ご")
    ("gya" "ぎゃ") ("gyu" "ぎゅ") ("gye" "ぎぇ") ("gyo" "ぎょ")
    ;; S row
    ("sa" "さ") ("shi" "し") ("si" "し") ("su" "す") ("se" "せ") ("so" "そ")
    ("sha" "しゃ") ("shu" "しゅ") ("she" "しぇ") ("sho" "しょ")
    ("sya" "しゃ") ("syu" "しゅ") ("sye" "しぇ") ("syo" "しょ")
    ;; Z row
    ("za" "ざ") ("ji" "じ") ("zi" "じ") ("zu" "ず") ("ze" "ぜ") ("zo" "ぞ")
    ("ja" "じゃ") ("ju" "じゅ") ("je" "じぇ") ("jo" "じょ")
    ("zya" "じゃ") ("zyu" "じゅ") ("zye" "じぇ") ("zyo" "じょ")
    ("jya" "じゃ") ("jyu" "じゅ") ("jye" "じぇ") ("jyo" "じょ")
    ;; T row
    ("ta" "た") ("chi" "ち") ("ti" "ち") ("tsu" "つ") ("tu" "つ")
    ("te" "て") ("to" "と")
    ("cha" "ちゃ") ("chu" "ちゅ") ("che" "ちぇ") ("cho" "ちょ")
    ("tya" "ちゃ") ("tyu" "ちゅ") ("tye" "ちぇ") ("tyo" "ちょ")
    ("tsa" "つぁ") ("tsi" "つぃ") ("tse" "つぇ") ("tso" "つぉ")
    ("tha" "てぁ") ("thi" "てぃ") ("thu" "てゅ") ("the" "てぇ") ("tho" "てょ")
    ;; D row
    ("da" "だ") ("di" "ぢ") ("du" "づ") ("de" "で") ("do" "ど")
    ("dya" "ぢゃ") ("dyu" "ぢゅ") ("dye" "ぢぇ") ("dyo" "ぢょ")
    ("dha" "でぁ") ("dhi" "でぃ") ("dhu" "でゅ") ("dhe" "でぇ") ("dho" "でょ")
    ;; N row
    ("na" "な") ("ni" "に") ("nu" "ぬ") ("ne" "ね") ("no" "の")
    ("n'" "ん") ("nn" "ん")
    ("nya" "にゃ") ("nyu" "にゅ") ("nye" "にぇ") ("nyo" "にょ")
    ;; H row
    ("ha" "は") ("hi" "ひ") ("fu" "ふ") ("hu" "ふ") ("he" "へ") ("ho" "ほ")
    ("hya" "ひゃ") ("hyu" "ひゅ") ("hye" "ひぇ") ("hyo" "ひょ")
    ("fa" "ふぁ") ("fi" "ふぃ") ("fe" "ふぇ") ("fo" "ふぉ")
    ;; B row
    ("ba" "ば") ("bi" "び") ("bu" "ぶ") ("be" "べ") ("bo" "ぼ")
    ("bya" "びゃ") ("byu" "びゅ") ("bye" "びぇ") ("byo" "びょ")
    ;; P row
    ("pa" "ぱ") ("pi" "ぴ") ("pu" "ぷ") ("pe" "ぺ") ("po" "ぽ")
    ("pya" "ぴゃ") ("pyu" "ぴゅ") ("pye" "ぴぇ") ("pyo" "ぴょ")
    ;; M row
    ("ma" "ま") ("mi" "み") ("mu" "む") ("me" "め") ("mo" "も")
    ("mya" "みゃ") ("myu" "みゅ") ("mye" "みぇ") ("myo" "みょ")
    ;; Y row
    ("ya" "や") ("yu" "ゆ") ("yo" "よ")
    ;; R row
    ("ra" "ら") ("ri" "り") ("ru" "る") ("re" "れ") ("ro" "ろ")
    ("rya" "りゃ") ("ryu" "りゅ") ("rye" "りぇ") ("ryo" "りょ")
    ;; W row
    ("wa" "わ") ("wo" "を") ("wi" "ゐ") ("we" "ゑ")
    ("wha" "うぁ") ("whi" "うぃ") ("whu" "う") ("whe" "うぇ") ("who" "うぉ")
    ;; V row
    ("va" "ゔぁ") ("vi" "ゔぃ") ("vu" "ゔ") ("ve" "ゔぇ") ("vo" "ゔぉ")
    ("vya" "ゔゃ") ("vyu" "ゔゅ") ("vyo" "ゔょ")
    ;; Small kana
    ("la" "ぁ") ("li" "ぃ") ("lu" "ぅ") ("le" "ぇ") ("lo" "ぉ")
    ("xa" "ぁ") ("xi" "ぃ") ("xu" "ぅ") ("xe" "ぇ") ("xo" "ぉ")
    ("xya" "ゃ") ("xyu" "ゅ") ("xyo" "ょ")
    ("lya" "ゃ") ("lyu" "ゅ") ("lyo" "ょ")
    ("xtsu" "っ") ("xtu" "っ") ("ltsu" "っ") ("ltu" "っ")
    ("xwa" "ゎ") ("xka" "ゕ") ("xke" "ゖ")
    ;; Long vowel mark
    ("-" "ー"))
  "Standard SKK romaji-to-kana conversion rules.
Each entry is (ROMAJI KANA) where both are strings.
This data constant is used by `nskk--initialize-romaji-table' to populate
the conversion table, separating rule data from initialization logic.")

(defmacro nskk-converter-define-rules (&rest rule-pairs)
  "Define multiple romaji-to-kana rules at once.
Each element of RULE-PAIRS must be a list (ROMAJI KANA) where ROMAJI and
KANA are string literals.  Expands to a `progn' with one
`nskk-converter-add-rule' call per pair.

Example:
  (nskk-converter-define-rules
    (\"a\" \"あ\")
    (\"i\" \"い\")
    (\"u\" \"う\"))"
  (declare (indent 0) (debug t))
  `(progn
     ,@(mapcar (lambda (pair)
                 `(nskk-converter-add-rule ,(car pair) ,(cadr pair)))
               rule-pairs)))

(defun nskk--initialize-romaji-table ()
  "Populate the romaji conversion table with standard SKK rules.
Rules are sourced from `nskk--standard-romaji-rules'.
This function only populates rules; clearing, trie setup, and
incomplete-marker population are handled by `nskk-converter-load-style'.
As a side effect, asserts all rules as `romaji-to-kana' Prolog facts via
`nskk-prolog-bulk-facts', making them available for Prolog queries."
  (dolist (rule nskk--standard-romaji-rules)
    (puthash (car rule) (cadr rule) nskk--romaji-table))
  (nskk-prolog-bulk-facts romaji-to-kana nskk--standard-romaji-rules))

(defun nskk-converter--populate-incomplete-markers ()
  "Populate nskk--romaji-table with :incomplete for all proper romaji prefixes.
Auto-derived from the complete romaji entries already in the table.
This eliminates manual maintenance of prefix lists when adding new rules.
O(n·k) where n is number of complete rules and k is average romaji length."
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

;; Hand-written pair: nskk-convert-romaji always succeeds (nil for nil input,
;; "" for empty, kana string otherwise).  Tests call /k with a single
;; on-done continuation, so defun/k (2 continuations) cannot be used.
(defun nskk-convert-romaji (romaji)
  "Convert full ROMAJI string to kana.
Returns converted kana string for string input, nil for nil input.
This is a convenience wrapper for nskk-converter-convert."
  (cond
    ((null romaji) nil)
    ((not (stringp romaji)) nil)
    ((zerop (length romaji)) "")
    (t (nskk-convert-romaji--internal (downcase romaji)))))

(defun nskk-convert-romaji/k (romaji on-done &optional _on-not-found)
  "CPS wrapper for `nskk-convert-romaji'.
Calls ON-DONE with the converted kana string (or nil).
The optional _ON-NOT-FOUND arg is accepted for compatibility with the `<-'
CPS bind macro. [CPS]"
  (funcall on-done (nskk-convert-romaji romaji)))

(defun nskk-convert-n--internal/k (remaining on-hatsuon on-fallthrough)
  "CPS variant of `nskk-convert-n--internal'.
Handle all ん-producing cases where REMAINING begins with character n.
Precondition: REMAINING must be a non-empty string whose first character is ?n.
ON-HATSUON is called as (funcall ON-HATSUON kana rest) where kana is \"ん\"
and rest is the unconsumed remainder (may be nil).
ON-FALLTHROUGH is called as (funcall ON-FALLTHROUGH) when REMAINING does not
produce ん and should fall through to normal table-driven conversion."
  (let ((len (length remaining)))
    (cond
     ;; Standalone \"n\" at end of input
     ((= len 1)
      (funcall on-hatsuon "ん" nil))
     ;; \"nn\" sequence: double-n always produces ん
     ((= (aref remaining 1) ?n)
      (if (= len 2)
          (funcall on-hatsuon "ん" nil)
        (funcall on-hatsuon "ん" (substring remaining 1))))
     ;; \"n'\" sequence: n followed by single quote (ASCII 39)
     ((= (aref remaining 1) ?')
      (funcall on-hatsuon "ん" (if (> len 2) (substring remaining 2) nil)))
     ;; \"n\" before a consonant (not in hatsuon-blocker set)
     ((not (memq (aref remaining 1) nskk--hatsuon-blockers))
      (funcall on-hatsuon "ん" (substring remaining 1)))
     ;; n followed by vowel/y/etc: fall through to table lookup (\"na\"->\"な\")
     (t (funcall on-fallthrough)))))

(defun nskk-convert-n--internal (remaining)
  "Handle all ん-producing cases where REMAINING begins with character n.
Precondition: REMAINING must be a non-empty string whose first character is ?n.
Returns (kana . rest) cons cell if ん is produced, or nil to fall through
to normal table-driven conversion (e.g. for \"na\" -> \"な\")."
  (nskk-convert-n--internal/k remaining
    #'cons          ; on-hatsuon: (cons kana rest)
    (lambda () nil)))

;; Hand-written pair: nskk-convert-romaji--internal/k uses a single-value
;; continuation (on-done receives the result string), which differs from
;; defun/k (2 continuations) and defun/done (on-done called with no args).
(defun nskk-convert-romaji--internal (input)
  "Internal romaji conversion for INPUT string.
Accumulates converted parts as a list and joins at the end to avoid
the O(n²) string allocation of repeated `concat' calls in a loop.

The function body uses an iterative accumulator for stack safety on long
inputs; CPS discipline is applied at the function boundary via `succeed' and
within the loop via `nskk-converter-convert/k' continuations. [CPS]"
  (let ((parts nil)
        (remaining input)
        (len 0))
    (while (and remaining (> (setq len (length remaining)) 0))
      (let* ((c0 (aref remaining 0))
             (n-result (when (= c0 ?n) (nskk-convert-n--internal remaining))))
        (cond
          ;; Double consonant (sokuon): same ASCII consonant repeated, not vowel/n.
          ;; The (< c0 128) guard ensures kana characters (e.g. "かか") never
          ;; accidentally trigger sokuon, keeping nskk-convert-romaji idempotent.
          ;; Only apply sokuon if the doubled pattern is NOT a complete rule.
          ;; This allows AZIK hatsuon rules (e.g., "kk" → "きん") to take priority.
          ((and (> len 1)
                (< c0 128)
                (= c0 (aref remaining 1))
                (not (memq c0 nskk--sokuon-blockers))
                (not (stringp (nskk-converter-lookup (substring remaining 0 2)))))
           (push "っ" parts)
           (setq remaining (substring remaining 1)))

         ;; n-prefix producing ん (falls through to table if n-result is nil)
         ((and (= c0 ?n) n-result)
          (push (car n-result) parts)
          (setq remaining (cdr n-result)))

         ;; Normal table-driven conversion via CPS (including "na", "ni", etc.)
         (t
          (nskk-converter-convert/k remaining
            (lambda (kana rest)
              (push kana parts)
              (setq remaining (and (stringp rest) (> (length rest) 0) rest)))
            (lambda (_romaji)
              ;; Partial match: keep remaining unconverted and stop
              (push remaining parts)
              (setq remaining nil))
            (lambda ()
              ;; No match: pass through unconverted and stop
              (push remaining parts)
              (setq remaining nil)))))))
    (apply #'concat (nreverse parts))))

(defun nskk-convert-romaji--internal/k (input on-done &optional _on-not-found)
  "CPS wrapper for `nskk-convert-romaji--internal'.
Calls ON-DONE with the converted kana string.
The optional _ON-NOT-FOUND arg is accepted for compatibility with the `<-'
CPS bind macro, which always passes two continuation arguments. [CPS]"
  (funcall on-done (nskk-convert-romaji--internal input)))

(defun nskk-converter-convert/k (romaji on-match on-incomplete on-fail)
  "CPS variant of `nskk-converter-convert'.
Convert ROMAJI string to kana and dispatch to a continuation.
ON-MATCH is called as (funcall ON-MATCH kana remaining) when a complete match
is found; kana is the converted string, remaining is the unconsumed romaji.
ON-INCOMPLETE is called as (funcall ON-INCOMPLETE romaji) when ROMAJI is a
proper prefix of a known sequence (no full match yet).
ON-FAIL is called as (funcall ON-FAIL) when ROMAJI is nil, empty, or has no
match and is not a known prefix."
  (if (not (and (stringp romaji) (> (length romaji) 0)))
      (funcall on-fail)
    (cl-loop for len from (min 4 (length romaji)) downto 1
             for prefix = (substring romaji 0 len)
             for result = (nskk-converter-lookup prefix)
             when (and result (stringp result))
             return (funcall on-match result (substring romaji len))
             when (eq result :incomplete)
             return (funcall on-incomplete romaji)
             finally return (funcall on-fail))))

(defun nskk-converter-convert (romaji)
  "Convert ROMAJI string to kana.
Returns (kana . remaining-romaji) cons cell on complete match.
Returns (:incomplete . romaji) on partial prefix match.
Returns nil when ROMAJI is nil, empty, or has no match."
  (nskk-converter-convert/k romaji
    #'cons                              ; on-match:      (cons kana remaining)
    (lambda (r) (cons :incomplete r))   ; on-incomplete: (:incomplete . romaji)
    (lambda () nil)))

(defun/k nskk-converter-get-possible-completions (romaji)
  "Get list of possible completions for ROMAJI prefix.
Returns list of (romaji . kana) pairs using Prolog trie prefix search."
  (if (stringp romaji)
      (let ((result (nskk-prolog-trie-prefix-search 'romaji-to-kana 2 romaji)))
        (if result (succeed result) (fail)))
    (fail)))

(defun/done nskk-converter-add-rule (romaji kana)
  "Add ROMAJI -> KANA mapping to hash table and Prolog database.
Called for side effects."
  (puthash romaji kana nskk--romaji-table)
  (when (stringp kana)
    (nskk-prolog-assert (list (list 'romaji-to-kana romaji kana)))))

(defun nskk-converter-remove-rule (romaji)
  "Remove ROMAJI from conversion table and Prolog database.
Called for side effects; return value is unspecified."
  (remhash romaji nskk--romaji-table)
  (nskk-prolog-retract (list 'romaji-to-kana romaji '\?_)))

(defun/k nskk-converter-get-rule (romaji)
  "Return the KANA string mapped to ROMAJI, or nil if not found.
Note: returns the symbol `:incomplete' for known romaji prefixes
that are not themselves complete conversion rules."
  (let ((result (gethash romaji nskk--romaji-table)))
    (if result (succeed result) (fail))))

(defun/done nskk-converter-register-style (style init-fn)
  "Register INIT-FN as the initialization function for STYLE.
INIT-FN is called with no arguments and should populate the romaji table
via `nskk-converter-add-rule'.  Called for side effects."
  (setf (alist-get style nskk--style-registry) init-fn))

(defun nskk-converter-load-style (style)
  "Load romaji rules for STYLE.
Clears the existing table and Prolog database, sets trie indexing,
calls the registered initialization function, then populates incomplete-prefix
markers.  Each registered init-fn is responsible only for asserting conversion
rules; orchestration (clear, index, populate) is handled by this function.
Valid styles are defined in `nskk--style-registry'.
Signals user-error if STYLE is not a registered style."
  (let ((init-fn (alist-get style nskk--style-registry)))
    (if init-fn
        (progn
          (clrhash nskk--romaji-table)
          (nskk-prolog-retract-all 'romaji-to-kana 2)
          (nskk-prolog-set-index 'romaji-to-kana 2 :trie)
          (funcall init-fn)
          (nskk-converter--populate-incomplete-markers)
          style)
      (user-error "Unknown romaji style: %s" style))))

(defmacro nskk-converter-define-style (name docstring &rest rules)
  "Define a new input style NAME with RULES and register it.
DOCSTRING describes the style.  RULES is a list of (ROMAJI KANA) pairs.
Generates an init function `nskk--init-NAME-rules' and registers it under
NAME via `nskk-converter-register-style'.

Example:
  (nskk-converter-define-style my-style
    \"My custom romaji style.\"
    (\"v\" \"ゔ\")
    (\"va\" \"ゔぁ\"))"
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

(defun/done nskk-converter-initialize ()
  "Initialize the romaji-to-kana conversion table.
Idempotent: subsequent calls are no-ops."
  (unless nskk--converter-initialized
    (nskk-converter-load-style 'standard)
    (setq nskk--converter-initialized t)))

(provide 'nskk-converter)

;;; nskk-converter.el ends here
