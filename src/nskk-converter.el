;;; nskk-converter.el --- Romaji to kana conversion engine -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n convenience

;; This file is NOT part of GNU Emacs.
;;
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

;; Romaji to kana conversion engine.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nskk-cps-macros)
(require 'nskk-prolog)

;;;; Customization

(defgroup nskk-converter nil
  "Romaji to Kana conversion settings."
  :prefix "nskk-converter-"
  :group 'nskk)

(defcustom nskk-converter-auto-start-henkan t
  "Whether to automatically start conversion on uppercase input."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-converter)

(defcustom nskk-converter-romaji-style 'standard
  "Romaji input style for Japanese conversion.
\\='standard - Standard SKK romaji (default)
\\='azik     - AZIK extended romaji with efficiency shortcuts"
  :type '(choice (const :tag "Standard SKK" standard)
                 (const :tag "AZIK" azik))
  :safe (lambda (v) (memq v '(standard azik)))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-converter)

;; Romaji conversion table
;; Maps romaji sequences to their kana equivalents (as strings for multi-byte)
(defvar nskk-mode-map)
(defvar nskk--romaji-table
  (make-hash-table :test (quote equal) :size 200)
  "Romaji to kana conversion table.
Used as a cache by AZIK and for hash-based lookups.  The Prolog trie
is the primary source of truth for nskk-converter-lookup.")

(defconst nskk--converter-missing
  (make-symbol "nskk-converter-missing")
  "Private sentinel distinguishing a missing romaji entry from stored nil.")

(defvar nskk--style-registry '((standard . nskk-initialize-romaji-table))
  "Registry mapping style symbols to their initialization functions.
An alist of the form ((STYLE-SYMBOL . INIT-FN) ...) where INIT-FN is a
zero-argument function that populates the romaji table via
`nskk-converter-add-rule'.  Use `nskk-converter-register-style' to add
entries; do not modify this variable directly.")

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
    ("-" "ー")
    ;; Basic punctuation
    ("." "。") ("," "、")
    ("[" "「") ("]" "」")
    ;; z-prefix symbols
    ("z-" "〜") ("z." "…") ("z," "‥")
    ("z[" "『") ("z]" "』") ("z/" "・")
    ("zh" "←") ("zj" "↓") ("zk" "↑") ("zl" "→")
    ("z " "　"))
  "Standard SKK romaji-to-kana conversion rules.
Each entry is (ROMAJI KANA) where both are strings.
This data constant is used by `nskk-initialize-romaji-table' to populate
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

(defun nskk-initialize-romaji-table ()
  "Populate romaji-to-kana Prolog facts from `nskk--standard-romaji-rules'.
Also populates `nskk--romaji-table' hash table for AZIK and input lookups."
  (dolist (rule nskk--standard-romaji-rules)
    (puthash (car rule) (cadr rule) nskk--romaji-table))
  (nskk-prolog-bulk-facts romaji-to-kana nskk--standard-romaji-rules))

(defun nskk--converter-populate-incomplete-markers ()
  "Populate the romaji table with :incomplete for all proper prefixes.
Auto-derived from the complete romaji entries already in the table."
  (let (keys)
    (maphash
     (lambda (key _value)
       (when (stringp key)
         (push key keys)))
     nskk--romaji-table)
    (dolist (romaji keys)
      (dotimes (index (1- (length romaji)))
        (let ((prefix (substring romaji 0 (1+ index))))
          (when (eq (gethash prefix nskk--romaji-table
                             nskk--converter-missing)
                    nskk--converter-missing)
            (puthash prefix :incomplete nskk--romaji-table)))))))

(defun nskk--converter-lookup-raw (romaji)
  "Return the internal ROMAJI lookup result without copying it.
Callers must treat returned strings as read-only."
  (when (stringp romaji)
    (let ((result (gethash romaji nskk--romaji-table
                           nskk--converter-missing)))
      (if (eq result nskk--converter-missing)
          (when (nskk-prolog-trie-has-prefix-p
                 (quote romaji-to-kana) 2 romaji)
            :incomplete)
        result))))

(defun nskk-converter-lookup (romaji)
  "Look up ROMAJI in the romaji-to-kana conversion system.
Return a fresh detached kana string for a complete rule, :incomplete for a
proper prefix, or nil for no match.  The hash table handles O(1) exact
lookups, with the Prolog trie used for prefix detection."
  (let ((result (nskk--converter-lookup-raw romaji)))
    (if (stringp result)
        (nskk-prolog-copy-term result)
      result)))

(defconst nskk--romaji-char-max 127
  "Maximum ASCII character code accepted as romaji input.
Characters above this value (non-ASCII) are never valid romaji and bypass
the sokuon/hatsuon detection logic in `nskk--sokuon-p'.")

(defun/k nskk-convert-romaji (romaji)
  "Convert ROMAJI string to kana by repeatedly applying longest-match conversion.
Returns converted kana string for string input, nil for nil input."
  (succeed (cond
             ((not (stringp romaji)) nil)
             ((string-empty-p romaji) "")
             (t (nskk-convert-romaji--internal (downcase romaji))))))

(defun nskk--sokuon-p (c0 remaining)
  "Return non-nil if C0 and REMAINING qualify as a sokuon (っ) trigger.
C0 is a character integer, typically (aref remaining 0) from the caller.
REMAINING is the unconsumed romaji string; must have length >= 2 for the
doubling check to succeed (length < 2 always returns nil).
Sokuon occurs when C0 is an ASCII consonant not in the `sokuon-blocker' table,
C0 equals the second character of REMAINING, and the two-character pair is
neither a complete romaji rule nor a prefix of a longer rule (allowing
AZIK entries like \"kk\" -> \"きん\" and prefixes like \"xx\" -> :incomplete
to override sokuon via `nskk-converter-lookup')."
  (and (> (length remaining) 1)
       (<= c0 nskk--romaji-char-max)
       (= c0 (aref remaining 1))
       (not (nskk-prolog-holds-p `(sokuon-blocker ,c0)))
       (not (nskk--converter-lookup-raw (substring remaining 0 2)))))

(defun/3k nskk--convert-step-n (remaining)
    (on-kana on-partial on-fail)
  "Handle n-prefix conversion for REMAINING (which starts with ?n).
ON-KANA is called as (funcall ON-KANA kana rest) when hatsuon produces ん.
ON-PARTIAL and ON-FAIL are forwarded to `nskk-converter-convert/k' when
the n-prefix falls through to the trie (e.g. na, ni, nya)."
  (let ((len (length remaining)))
    (cond
     ((= len 1)
      (funcall on-kana "ん" nil))
     ;; nn -> ん (keep second n for potential next match)
     ((= (aref remaining 1) ?n)
      (funcall on-kana "ん" (if (> len 2) (substring remaining 1) nil)))
     ((= (aref remaining 1) ?')
      (funcall on-kana "ん" (if (> len 2) (substring remaining 2) nil)))
     ((not (nskk-prolog-holds-p `(hatsuon-blocker ,(aref remaining 1))))
      (funcall on-kana "ん" (substring remaining 1)))
     ;; n + vowel/y: fall through to trie lookup (na->な, etc.)
     (t (nskk-converter-convert/k remaining on-kana on-partial on-fail)))))

(defun/3k nskk--convert-step (remaining)
    (on-kana on-partial on-fail)
  "Dispatch one conversion step for REMAINING (must be a non-empty string).
ON-KANA is called as (funcall ON-KANA kana rest) on a successful conversion.
ON-PARTIAL is called as (funcall ON-PARTIAL remaining) when REMAINING is a
known incomplete prefix with no full match yet.
ON-FAIL is called as (funcall ON-FAIL) when REMAINING has no match and is not
a known prefix.
Handles sokuon, hatsuon (via `nskk--convert-step-n/k'), and normal trie lookup
in a flat cond."
  (let ((c0 (aref remaining 0)))
    (cond
     ((nskk--sokuon-p c0 remaining)
      (funcall on-kana "っ" (substring remaining 1)))
     ((= c0 ?n)
      (nskk--convert-step-n/k remaining on-kana on-partial on-fail))
     (t (nskk-converter-convert/k remaining on-kana on-partial on-fail)))))

(defun nskk--convert-loop/k (remaining parts on-found on-not-found)
  "Run the tail-recursive conversion loop in CPS style.
REMAINING is the unconsumed input, PARTS is accumulated kana (reversed).
Always calls ON-FOUND with the assembled kana string.
Hand-written explicit pair because ON-FOUND must be passed as a value
to the recursive call (defun/k only rewrites succeed/fail call forms).
ON-NOT-FOUND is forwarded unchanged through recursion."
  (if (or (null remaining) (string-empty-p remaining))
      (funcall on-found (apply #'concat (nreverse parts)))
    (nskk--convert-step/k remaining
      (lambda (kana rest)
        (nskk--convert-loop/k
         (and (stringp rest) (not (string-empty-p rest)) rest)
         (cons kana parts)
         on-found on-not-found))
      (lambda (partial)
        (funcall on-found
                 (apply #'concat (nreverse (cons partial parts)))))
      (lambda ()
        (funcall on-found
                 (apply #'concat (nreverse (cons remaining parts))))))))

(defun nskk--convert-loop (remaining parts)
  "Tail-recursive conversion loop for REMAINING and PARTS (sync wrapper)."
  (nskk--convert-loop/k remaining parts #'identity #'ignore))
(put 'nskk--convert-loop/k 'nskk--cps-continuation-pattern :found-not-found)

(defun/k nskk-convert-romaji--internal (input)
  "Internal romaji conversion via tail-recursive CPS loop.
INPUT must be a non-empty, already-downcased string."
  (succeed (nskk--convert-loop input nil)))

(defun/3k nskk-converter-convert (romaji)
    (on-match on-incomplete on-fail)
  "Convert ROMAJI string to kana via longest-match lookup.
ON-MATCH is called as (funcall ON-MATCH kana remaining) when a complete match
is found; kana is the converted string, remaining is the unconsumed romaji.
ON-INCOMPLETE is called as (funcall ON-INCOMPLETE romaji) when ROMAJI is a
proper prefix of a known sequence (no full match yet).
ON-FAIL is called as (funcall ON-FAIL) when ROMAJI is nil, empty, or has no
match and is not a known prefix.
Uses `nskk-converter-lookup' for each candidate prefix length (4 down to 1)."
  (if (or (not (stringp romaji)) (string-empty-p romaji))
      (funcall on-fail)
    (cl-loop for len from (min 4 (length romaji)) downto 1
             for prefix = (substring romaji 0 len)
             for result = (nskk--converter-lookup-raw prefix)
             when (stringp result)
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
    #'cons
    (lambda (r) (cons :incomplete r))
    (lambda () nil)))

(defun/k nskk-converter-get-possible-completions (romaji)
  "Get list of possible completions for ROMAJI prefix.
ROMAJI is a string prefix to search.
On success, calls on-found with a list of (romaji . kana) pairs.
On failure (nil/non-string input or no completions), calls on-not-found.
The sync wrapper returns the list on success, nil on failure."
  (if (stringp romaji)
      (let ((result (nskk-prolog-trie-prefix-search 'romaji-to-kana 2 romaji)))
        (if result (succeed result) (fail)))
    (fail)))

(defun nskk--converter-find-hash-entry (key)
  "Return the physical equal hash entry for KEY, or nil if absent."
  (catch 'found
    (maphash
     (lambda (actual-key value)
       (when (equal actual-key key)
         (throw 'found (list actual-key value))))
     nskk--romaji-table)
    nil))

(defun nskk--converter-call-with-hash-journal (lookup-key operation)
  "Call OPERATION and restore LOOKUP-KEY's exact entry on error or quit."
  (let ((entry (nskk--converter-find-hash-entry lookup-key)))
    (condition-case condition
        (funcall operation)
      ((error quit)
       (let ((inhibit-quit t))
         (remhash lookup-key nskk--romaji-table)
         (when entry
           (puthash (car entry) (cadr entry) nskk--romaji-table)))
       (signal (car condition) (cdr condition))))))

(defun nskk--converter-replace-hash-entry (lookup-key new-key value)
  "Replace LOOKUP-KEY with physical NEW-KEY mapped to VALUE."
  (remhash lookup-key nskk--romaji-table)
  (puthash new-key value nskk--romaji-table))

(defun nskk--converter-delete-hash-entry (lookup-key)
  "Delete the hash entry equal to LOOKUP-KEY."
  (remhash lookup-key nskk--romaji-table))

(defun/done nskk-converter-add-rule (romaji kana)
  "Add a caller-detached ROMAJI -> KANA mapping atomically.
String mappings replace the first matching Prolog clause in the same
transaction as the hash publication.  Non-string KANA values retain identity
and affect only the hash table."
  (if (stringp kana)
      (let* ((owned-rule (nskk-prolog-copy-term (cons romaji kana)))
             (owned-romaji (car owned-rule))
             (owned-kana (cdr owned-rule)))
        (nskk--converter-call-with-hash-journal
         owned-romaji
         (lambda ()
           (nskk-prolog-replace-clause-transaction
            (list 'romaji-to-kana owned-romaji '\?_)
            (list (list 'romaji-to-kana owned-romaji owned-kana))
            (lambda ()
              (nskk--converter-replace-hash-entry
               owned-romaji owned-romaji owned-kana))))))
    (let ((owned-romaji (nskk-prolog-copy-term romaji)))
      (nskk--converter-call-with-hash-journal
       owned-romaji
       (lambda ()
         (nskk--converter-replace-hash-entry
          owned-romaji owned-romaji kana))))))

(defun/done nskk-converter-remove-rule (romaji)
  "Remove ROMAJI atomically from the hash table and Prolog database."
  (let ((owned-romaji (nskk-prolog-copy-term romaji)))
    (nskk--converter-call-with-hash-journal
     owned-romaji
     (lambda ()
       (nskk-prolog-replace-clause-transaction
        (list 'romaji-to-kana owned-romaji '\?_)
        nil
        (lambda ()
          (nskk--converter-delete-hash-entry owned-romaji)))))))

(defun/k nskk-converter-get-rule (romaji)
  "Return the value mapped to ROMAJI in the conversion table.
On success, calls on-found with the kana string or `:incomplete'.
On failure (key absent), calls on-not-found.
The sync wrapper returns the kana string, `:incomplete', or nil.
Delegates to `nskk-converter-lookup' for unified hash+trie lookup."
  (let ((result (nskk-converter-lookup romaji)))
    (if result (succeed result) (fail))))

(defun/done nskk-converter-register-style (style init-fn)
  "Register INIT-FN as the initialization function for STYLE.
INIT-FN is called with no arguments and should populate the romaji table
via `nskk-converter-add-rule'.  Called for side effects."
  (setf (alist-get style nskk--style-registry) init-fn))

(progn
  (defvar nskk--converter-style-transaction-hash-tables nil
    "Additional hash-table variables included in style transactions.")
  (defvar nskk--converter-style-transaction-variables nil
    "Additional replacement-only variables included in style transactions.
Registered initializers must replace these values rather than mutate the
objects reachable from their pre-transaction values."))

(defun nskk-converter-register-style-transaction-hash-table (symbol)
  "Register SYMBOL as an additional hash-table variable for style transactions.
SYMBOL must name a `defvar' owned by the caller's module; its value is
included, staged, and detached alongside the built-in romaji-table state
whenever `nskk--converter-stage-style-state' runs."
  (add-to-list 'nskk--converter-style-transaction-hash-tables symbol))

(defun nskk-converter-register-style-transaction-variable (symbol)
  "Register SYMBOL as an additional replacement-only style-transaction variable.
SYMBOL must name a `defvar' owned by the caller's module.  Its value is
replaced (not mutated) on publish, so registered initializers must assign
a fresh value rather than mutating the object reachable from the
pre-transaction value."
  (add-to-list 'nskk--converter-style-transaction-variables symbol))

(defun nskk-converter-style-transaction-hash-tables ()
  "Return the full list of registered style-transaction hash-table variables."
  nskk--converter-style-transaction-hash-tables)

(defun nskk-converter-set-style-transaction-hash-tables (value)
  "Replace the full list of registered style-transaction hash-table variables.
VALUE replaces the list outright; use
`nskk-converter-register-style-transaction-hash-table' to append instead."
  (setq nskk--converter-style-transaction-hash-tables value))

(defun nskk-converter-style-transaction-variables ()
  "Return the full list of registered replacement-only style-transaction variables."
  nskk--converter-style-transaction-variables)

(defun nskk-converter-set-style-transaction-variables (value)
  "Replace the registered replacement-only style-transaction list with VALUE.
Replaces the list outright; use
`nskk-converter-register-style-transaction-variable' to append instead."
  (setq nskk--converter-style-transaction-variables value))

(defun nskk-romaji-table ()
  "Return the current romaji-to-kana hot-path cache hash table."
  nskk--romaji-table)

(defun nskk-set-romaji-table (value)
  "Set the romaji-to-kana hot-path cache hash table to VALUE."
  (setq nskk--romaji-table value))
(defun nskk--converter-empty-hash-table-copy (table)
  "Return an empty hash table with the same parameters as TABLE."
  (let ((copy (copy-hash-table table)))
    (clrhash copy)
    copy))
(defun nskk--converter-stage-style-state (init-fn)
  "Run INIT-FN against isolated converter state and return that state."
  (let*
      ((store-values
        (list
         (nskk--converter-empty-hash-table-copy nskk--romaji-table)))
       (root-symbols
        '(nskk--romaji-table))
       (prolog-store-values
        (list (nskk-prolog-database)
              (nskk-prolog-database-tails)
              (nskk-prolog-index-config)
              (nskk-prolog-hash-indices)
              (nskk-prolog-trie-indices)
              (nskk-prolog-index-bucket-tail-cache)))
       (extension-registry
        (delete-dups
         (copy-sequence nskk--converter-style-transaction-hash-tables)))
       (_validated-extensions
        (dolist (symbol extension-registry)
          (unless (symbolp symbol)
            (error "Invalid style transaction hash-table variable: %S" symbol))))
       (extension-symbols
        (cl-remove-if-not #'boundp extension-registry))
       (extension-values
        (mapcar
         (lambda (symbol)
           (let ((value (symbol-value symbol)))
             (unless (hash-table-p value)
               (error "Style transaction variable is not a hash table: %S"
                      symbol))
             value))
         extension-symbols))
       (transaction-symbols
        (delete-dups
         (copy-sequence nskk--converter-style-transaction-variables)))
       (_validated-transaction-symbols
        (dolist (symbol transaction-symbols)
          (unless (and (symbolp symbol)
                       (not (memq symbol root-symbols))
                       (not (memq symbol extension-registry))
                       (not (eq symbol 'nskk-mode-map)))
            (error "Invalid replacement-only style transaction variable: %S"
                   symbol))))
       (transaction-boundness (mapcar #'boundp transaction-symbols))
       (unbound-variable-sentinel
        (make-symbol "style-transaction-variable-unbound"))
       (transaction-values
        (cl-mapcar
         (lambda (symbol bound-p)
           (if bound-p
               (symbol-value symbol)
             unbound-variable-sentinel))
         transaction-symbols
         transaction-boundness))
       (mode-map-bound-p (boundp 'nskk-mode-map))
       (mode-map-value
        (when mode-map-bound-p
          (let ((value (symbol-value 'nskk-mode-map)))
            (unless (or (null value) (keymapp value))
              (error "Nskk-mode-map is not a keymap: %S" value))
            value)))
       (copied-state
        (nskk-prolog-copy-term
         (list store-values prolog-store-values extension-values
               (when mode-map-bound-p mode-map-value))))
       (copied-store-values (nth 0 copied-state))
       (copied-prolog-store-values (nth 1 copied-state))
       (copied-extension-values (nth 2 copied-state))
       (copied-mode-map
        (when mode-map-bound-p
          (nth 3 copied-state)))
       (unbound-mode-map-sentinel (make-symbol "nskk-mode-map-unbound"))
       (symbols
        (append root-symbols
                extension-symbols
                transaction-symbols
                (list 'nskk-mode-map)))
       (values
        (append copied-store-values
                copied-extension-values
                transaction-values
                (list
                 (if mode-map-bound-p
                     copied-mode-map
                   unbound-mode-map-sentinel)))))
    (nskk-prolog-with-database-fields
        ((database (nth 0 copied-prolog-store-values))
         (database-tails (nth 1 copied-prolog-store-values))
         (index-config (nth 2 copied-prolog-store-values))
         (hash-indices (nth 3 copied-prolog-store-values))
         (trie-indices (nth 4 copied-prolog-store-values))
         (index-bucket-tail-cache (nth 5 copied-prolog-store-values)))
      (cl-progv symbols values
        (cl-mapc
         (lambda (symbol bound-p)
           (unless bound-p
             (makunbound symbol)))
         transaction-symbols
         transaction-boundness)
        (unless mode-map-bound-p
          (makunbound 'nskk-mode-map))
        (nskk-prolog-retract-all 'romaji-to-kana 2)
        (nskk-prolog-set-index 'romaji-to-kana 2 :trie)
        (funcall init-fn)
        (nskk--converter-populate-incomplete-markers)
        (let ((staged-mode-map-bound-p (boundp 'nskk-mode-map))
              (staged-prolog-state (nskk-prolog-state-snapshot)))
          (when staged-mode-map-bound-p
            (let ((value (symbol-value 'nskk-mode-map)))
              (unless (or (null value) (keymapp value))
                (error "Nskk-mode-map is not a keymap: %S" value))))
          (list
           :romaji-table nskk--romaji-table
           :prolog-database (nskk-prolog-database)
           :prolog-database-tails (nskk-prolog-database-tails)
           :prolog-index-config (nskk-prolog-index-config)
           :prolog-hash-indices (nskk-prolog-hash-indices)
           :prolog-trie-indices (nskk-prolog-trie-indices)
           :prolog-index-bucket-tail-cache
           (nskk-prolog-index-bucket-tail-cache)
           :prolog-state staged-prolog-state
           :extension-hash-tables
           (mapcar
            (lambda (symbol)
              (cons symbol (symbol-value symbol)))
            extension-symbols)
           :transaction-variables
           (mapcar
            (lambda (symbol)
              (list symbol
                    (boundp symbol)
                    (and (boundp symbol) (symbol-value symbol))))
            transaction-symbols)
           :mode-map-bound-p staged-mode-map-bound-p
           :mode-map
           (when staged-mode-map-bound-p
             (symbol-value 'nskk-mode-map))))))))
(defun nskk--converter-replace-keymap-contents (target source)
  "Replace TARGET contents with SOURCE while retaining TARGET identity."
  (unless (and (consp target) (keymapp target) (consp source) (keymapp source))
    (error "Cannot publish invalid keymap state"))
  (setcdr target (cdr source)))
(defun nskk--converter-publish-style-state (state)
  "Atomically publish a detached copy of staged converter STATE."
  (let* ((root-symbols
          (list
           (quote nskk--romaji-table)))
         (staged-tables
          (list
           (plist-get state :romaji-table)))
         (staged-prolog-state (plist-get state :prolog-state))
         (staged-extensions (plist-get state :extension-hash-tables))
         (staged-variables (plist-get state :transaction-variables))
         (mode-map-bound-p (plist-get state :mode-map-bound-p))
         (staged-mode-map (plist-get state :mode-map))
         (mode-map-symbol (quote nskk-mode-map)))
    (unless (cl-every (function hash-table-p) staged-tables)
      (error "Cannot publish invalid converter table state"))
    (unless (and (vectorp staged-prolog-state)
                 (= (length staged-prolog-state) 8))
      (error "Cannot publish invalid Prolog state"))
    (dolist (entry staged-extensions)
      (unless (and (consp entry)
                   (symbolp (car entry))
                   (hash-table-p (cdr entry)))
        (error "Cannot publish invalid extension state: %S" entry)))
    (let ((seen-symbols nil))
      (dolist (entry staged-variables)
        (unless (and (proper-list-p entry)
                     (= (length entry) 3)
                     (symbolp (car entry))
                     (memq (nth 1 entry) (quote (nil t)))
                     (not (memq (car entry) seen-symbols))
                     (not (memq (car entry) root-symbols))
                     (not (assq (car entry) staged-extensions))
                     (not (eq (car entry) mode-map-symbol)))
          (error "Cannot publish invalid transaction variable state: %S"
                 entry))
        (push (car entry) seen-symbols)))
    (when (and mode-map-bound-p
               (not (or (null staged-mode-map)
                        (keymapp staged-mode-map))))
      (error "Cannot publish invalid mode map state"))
    (let* ((prepared-state
            (nskk-prolog-copy-term
             (list staged-tables staged-prolog-state staged-extensions
                   (when mode-map-bound-p staged-mode-map))))
           (tables (nth 0 prepared-state))
           (prolog-state (nth 1 prepared-state))
           (extensions (nth 2 prepared-state))
           (variables (mapcar (function copy-sequence) staged-variables))
           (new-mode-map
            (when mode-map-bound-p
              (nth 3 prepared-state))))
      (unless (cl-every (function hash-table-p) tables)
        (error "Cannot publish invalid copied converter table state"))
      (unless (and (vectorp prolog-state) (= (length prolog-state) 8))
        (error "Cannot publish invalid copied Prolog state"))
      (dolist (entry extensions)
        (unless (and (consp entry)
                     (symbolp (car entry))
                     (hash-table-p (cdr entry)))
          (error "Cannot publish invalid copied extension state: %S" entry)))
      (when (and mode-map-bound-p
                 (not (or (null new-mode-map)
                          (keymapp new-mode-map))))
        (error "Cannot publish invalid copied mode map state"))
      (let* ((old-tables (mapcar (function symbol-value) root-symbols))
             (old-prolog-state (nskk-prolog-state-snapshot))
             (old-extensions
              (mapcar
               (lambda (entry)
                 (cons (car entry) (symbol-value (car entry))))
               extensions))
             (old-variables
              (mapcar
               (lambda (entry)
                 (let ((symbol (car entry)))
                   (list symbol
                         (boundp symbol)
                         (and (boundp symbol) (symbol-value symbol)))))
               variables))
             (old-mode-map-bound-p (boundp mode-map-symbol))
             (old-mode-map
              (when old-mode-map-bound-p
                (symbol-value mode-map-symbol))))
        (when (and old-mode-map-bound-p
                   (not (or (null old-mode-map)
                            (keymapp old-mode-map))))
          (error "Cannot replace invalid public mode map state"))
        (let ((old-mode-map-car
               (when (consp old-mode-map)
                 (car old-mode-map)))
              (old-mode-map-cdr
               (when (consp old-mode-map)
                 (cdr old-mode-map)))
              (mode-map-contents-replaced-p nil))
          (cl-labels
              ((restore-with-retry
                (operation)
                (let ((attempt 0)
                      (restored-p nil))
                  (while (and (< attempt 2) (not restored-p))
                    (setq attempt (1+ attempt))
                    (condition-case nil
                        (progn
                          (funcall operation)
                          (setq restored-p t))
                      ((error quit) nil)))
                  restored-p))
               (publish-variable
                (entry)
                (if (nth 1 entry)
                    (set (car entry) (nth 2 entry))
                  (makunbound (car entry)))))
            (condition-case condition
                (let ((inhibit-quit t))
                  (cl-mapc
                   (lambda (symbol value)
                     (set symbol value))
                   root-symbols
                   tables)
                  (nskk-prolog-state-restore prolog-state)
                  (dolist (entry extensions)
                    (set (car entry) (cdr entry)))
                  (dolist (entry variables)
                    (publish-variable entry))
                  (cond
                   ((not mode-map-bound-p)
                    (makunbound mode-map-symbol))
                   ((and (consp old-mode-map)
                         (consp new-mode-map))
                    (setq mode-map-contents-replaced-p t)
                    (nskk--converter-replace-keymap-contents
                     old-mode-map
                     new-mode-map))
                   (t
                    (set mode-map-symbol new-mode-map))))
              ((error quit)
               (let ((inhibit-quit t))
                 (cl-mapc
                  (lambda (symbol value)
                    (restore-with-retry
                     (lambda ()
                       (set symbol value))))
                  root-symbols
                  old-tables)
                 (restore-with-retry
                  (lambda ()
                    (nskk-prolog-state-restore old-prolog-state)))
                 (dolist (entry old-extensions)
                   (restore-with-retry
                    (lambda ()
                      (set (car entry) (cdr entry)))))
                 (dolist (entry old-variables)
                   (restore-with-retry
                    (lambda ()
                      (publish-variable entry))))
                 (when mode-map-contents-replaced-p
                   (restore-with-retry
                    (lambda ()
                      (setcar old-mode-map old-mode-map-car)))
                   (restore-with-retry
                    (lambda ()
                      (setcdr old-mode-map old-mode-map-cdr))))
                 (restore-with-retry
                  (if old-mode-map-bound-p
                      (lambda ()
                        (set mode-map-symbol old-mode-map))
                    (lambda ()
                      (makunbound mode-map-symbol)))))
               (signal (car condition) (cdr condition))))))))))
(defun/k
  nskk-converter-load-style
  (style)
  "Load romaji conversion STYLE into the converter atomically.

STYLE is a symbol registered via `nskk-converter-register-style'.  The
initializer and incomplete-marker finalization run against isolated converter,
Prolog, extension, and keymap state.  Nothing is published if either step
signals an error or quit.  On success the staged state is published while
retaining the identity of `nskk-mode-map'.

Returns succeed(STYLE) on success, or fail() if STYLE is not registered."
  (let ((init-fn (alist-get style nskk--style-registry)))
    (if init-fn (let ((state (nskk--converter-stage-style-state init-fn)))
        (nskk--converter-publish-style-state state)
        (succeed style))
      (fail))))

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

(nskk-prolog-<- (module-initialized-flag nskk--converter-initialized))

(defun/done nskk-converter-initialize ()
  "Initialize the romaji-to-kana conversion table.
Idempotent: subsequent calls are no-ops."
  (unless nskk--converter-initialized
    (nskk-converter-load-style 'standard)
    (nskk-prolog-define-fact-table sokuon-blocker (:arity 1 :index :hash)
      (?a) (?i) (?u) (?e) (?o) (?n))
    (nskk-prolog-define-fact-table hatsuon-blocker (:arity 1 :index :hash)
      (?a) (?i) (?u) (?e) (?o) (?y) (?n) (?\'))
    (nskk-prolog-define-fact-table vowel-char (:arity 1 :index :hash)
      (?a) (?i) (?u) (?e) (?o))
    (nskk-prolog-define-fact-table uppercase-vowel-char (:arity 1 :index :hash)
      (?A) (?I) (?U) (?E) (?O))
    (setq nskk--converter-initialized t)))

(provide 'nskk-converter)

;;; nskk-converter.el ends here
