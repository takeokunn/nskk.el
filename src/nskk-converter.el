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
(require 'nskk-custom)

;; Romaji conversion table
;; Maps romaji sequences to their kana equivalents (as strings for multi-byte)
(defvar nskk-mode-map)
(defvar nskk--romaji-table
  (make-hash-table :test 'equal :size 200)
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
                 'romaji-to-kana 2 romaji)
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

(defun/done nskk-converter-register-style (style init-fn)
  "Register INIT-FN as the initialization function for STYLE.
INIT-FN is called with no arguments and should populate the romaji table
via `nskk-converter-add-rule'.  Called for side effects."
  (setf (alist-get style nskk--style-registry) init-fn))

(defvar nskk--converter-style-transaction-hash-tables nil
  "Additional hash-table variables included in style transactions.")

(defvar nskk--converter-style-transaction-variables nil
  "Additional replacement-only variables included in style transactions.
Registered initializers must replace these values rather than mutate the
objects reachable from their pre-transaction values.")

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

(defun nskk-converter-style-transaction-variables ()
  "Return all registered replacement-only style-transaction variables."
  nskk--converter-style-transaction-variables)

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

(defun nskk--converter-build-style-transaction-plan ()
  "Build the isolated dynamic-binding plan for staging style transaction state.
Reads the live romaji table, Prolog store, and registered extension and
transaction variables, validates them, and detaches a copy of everything
that will be dynamically rebound.  Returns a plist with
:copied-prolog-store-values, :extension-symbols, :transaction-symbols,
:transaction-boundness, :mode-map-bound-p, :symbols, and :values, consumed by
`nskk--converter-stage-style-state' to enter the detached `cl-progv' scope."
  (let* ((store-values
          (list (nskk--converter-empty-hash-table-copy nskk--romaji-table)))
         (root-symbols '(nskk--romaji-table))
         (prolog-store-values
          (list (nskk-prolog-database)
                (nskk-prolog-database-tails)
                (nskk-prolog-index-config)
                (nskk-prolog-hash-indices)
                (nskk-prolog-trie-indices)
                (nskk-prolog-index-bucket-tail-cache)))
         (extension-registry
          (delete-dups
           (copy-sequence nskk--converter-style-transaction-hash-tables))))
    (dolist (symbol extension-registry)
      (unless (symbolp symbol)
        (error "Invalid style transaction hash-table variable: %S" symbol)))
    (let* ((extension-symbols
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
             (copy-sequence nskk--converter-style-transaction-variables))))
      (dolist (symbol transaction-symbols)
        (unless (and (symbolp symbol)
                     (not (memq symbol root-symbols))
                     (not (memq symbol extension-registry))
                     (not (eq symbol 'nskk-mode-map)))
          (error "Invalid replacement-only style transaction variable: %S"
                 symbol)))
      (let* ((transaction-boundness (mapcar #'boundp transaction-symbols))
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
             (unbound-mode-map-sentinel (make-symbol "nskk-mode-map-unbound")))
        (list
         :copied-prolog-store-values copied-prolog-store-values
         :extension-symbols extension-symbols
         :transaction-symbols transaction-symbols
         :transaction-boundness transaction-boundness
         :mode-map-bound-p mode-map-bound-p
         :symbols (append root-symbols
                           extension-symbols
                           transaction-symbols
                           (list 'nskk-mode-map))
         :values (append copied-store-values
                          copied-extension-values
                          transaction-values
                          (list
                           (if mode-map-bound-p
                               copied-mode-map
                             unbound-mode-map-sentinel))))))))

(defun nskk--converter-stage-style-state (init-fn)
  "Run INIT-FN against isolated converter state and return that state."
  (let* ((plan (nskk--converter-build-style-transaction-plan))
         (copied-prolog-store-values
          (plist-get plan :copied-prolog-store-values))
         (extension-symbols (plist-get plan :extension-symbols))
         (transaction-symbols (plist-get plan :transaction-symbols))
         (transaction-boundness (plist-get plan :transaction-boundness))
         (mode-map-bound-p (plist-get plan :mode-map-bound-p))
         (symbols (plist-get plan :symbols))
         (values (plist-get plan :values)))
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

(defun nskk--converter-restore-with-retry (operation)
  "Call OPERATION up to twice, ignoring `error' or `quit', until it succeeds.
Return non-nil once OPERATION completes without signaling."
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

(defun nskk--converter-publish-variable (entry)
  "Publish ENTRY, a (SYMBOL BOUND-P VALUE) list, to the live SYMBOL."
  (if (nth 1 entry)
      (set (car entry) (nth 2 entry))
    (makunbound (car entry))))

(defun nskk--converter-validate-and-prepare-publish-state (state)
  "Validate STATE and return a detached, re-validated copy ready to publish.
Returns a plist with :root-symbols, :mode-map-symbol, :mode-map-bound-p,
:tables, :prolog-state, :extensions, :variables, and :new-mode-map."
  (let* ((root-symbols (list 'nskk--romaji-table))
         (staged-tables (list (plist-get state :romaji-table)))
         (staged-prolog-state (plist-get state :prolog-state))
         (staged-extensions (plist-get state :extension-hash-tables))
         (staged-variables (plist-get state :transaction-variables))
         (mode-map-bound-p (plist-get state :mode-map-bound-p))
         (staged-mode-map (plist-get state :mode-map))
         (mode-map-symbol 'nskk-mode-map))
    (unless (cl-every #'hash-table-p staged-tables)
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
                     (memq (nth 1 entry) '(nil t))
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
           (variables (mapcar #'copy-sequence staged-variables))
           (new-mode-map
            (when mode-map-bound-p
              (nth 3 prepared-state))))
      (unless (cl-every #'hash-table-p tables)
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
      (list :root-symbols root-symbols
            :mode-map-symbol mode-map-symbol
            :mode-map-bound-p mode-map-bound-p
            :tables tables
            :prolog-state prolog-state
            :extensions extensions
            :variables variables
            :new-mode-map new-mode-map))))

(defun nskk--converter-capture-publish-rollback-baseline
    (root-symbols extensions variables mode-map-symbol)
  "Capture the pre-publish baseline needed to roll back a failed publish.
ROOT-SYMBOLS and EXTENSIONS name the live variables about to be overwritten;
VARIABLES supplies the symbols whose current binding must be snapshotted;
MODE-MAP-SYMBOL is the live keymap variable.  Captured eagerly, before any
field's commit begins, so a fault partway through publication always has a
complete baseline to restore.
Returns a plist with :old-tables, :old-prolog-state, :old-extensions,
:old-variables, :old-mode-map-bound-p, :old-mode-map, :old-mode-map-car, and
:old-mode-map-cdr."
  (let* ((old-tables (mapcar #'symbol-value root-symbols))
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
    (list :old-tables old-tables
          :old-prolog-state old-prolog-state
          :old-extensions old-extensions
          :old-variables old-variables
          :old-mode-map-bound-p old-mode-map-bound-p
          :old-mode-map old-mode-map
          :old-mode-map-car (when (consp old-mode-map) (car old-mode-map))
          :old-mode-map-cdr (when (consp old-mode-map) (cdr old-mode-map)))))

(defun nskk--converter-publish-commit-state
    (root-symbols tables prolog-state extensions variables)
  "Publish TABLES, PROLOG-STATE, EXTENSIONS and VARIABLES into live state.
ROOT-SYMBOLS names the live variables receiving TABLES.
Caller must already hold `inhibit-quit'; this function neither binds nor
clears it, so a signal here still reaches the caller's rollback handler.

Publishing the keymap is deliberately not part of this function -- see
`nskk--converter-publish-style-state'."
  (cl-mapc (lambda (symbol value) (set symbol value)) root-symbols tables)
  (nskk-prolog-state-restore prolog-state)
  (dolist (entry extensions)
    (set (car entry) (cdr entry)))
  (dolist (entry variables)
    (nskk--converter-publish-variable entry)))

(defun nskk--converter-publish-rollback
    (root-symbols old-tables old-prolog-state old-extensions old-variables
     mode-map-contents-replaced-p old-mode-map old-mode-map-car
     old-mode-map-cdr old-mode-map-bound-p mode-map-symbol)
  "Restore live converter state to its pre-publish baseline after a failed
commit.  Caller must already hold `inhibit-quit'; mirrors
`nskk--converter-publish-commit-state' plus the caller's keymap step."
  (cl-mapc
   (lambda (symbol value)
     (nskk--converter-restore-with-retry (lambda () (set symbol value))))
   root-symbols
   old-tables)
  (nskk--converter-restore-with-retry
   (lambda () (nskk-prolog-state-restore old-prolog-state)))
  (dolist (entry old-extensions)
    (nskk--converter-restore-with-retry
     (lambda () (set (car entry) (cdr entry)))))
  (dolist (entry old-variables)
    (nskk--converter-restore-with-retry
     (lambda () (nskk--converter-publish-variable entry))))
  (when mode-map-contents-replaced-p
    (nskk--converter-restore-with-retry
     (lambda () (setcar old-mode-map old-mode-map-car)))
    (nskk--converter-restore-with-retry
     (lambda () (setcdr old-mode-map old-mode-map-cdr))))
  (nskk--converter-restore-with-retry
   (if old-mode-map-bound-p
       (lambda () (set mode-map-symbol old-mode-map))
     (lambda () (makunbound mode-map-symbol)))))

(defun nskk--converter-publish-style-state (state)
  "Atomically publish a detached copy of staged converter STATE."
  (let* ((prepared (nskk--converter-validate-and-prepare-publish-state state))
         (root-symbols (plist-get prepared :root-symbols))
         (mode-map-symbol (plist-get prepared :mode-map-symbol))
         (mode-map-bound-p (plist-get prepared :mode-map-bound-p))
         (tables (plist-get prepared :tables))
         (prolog-state (plist-get prepared :prolog-state))
         (extensions (plist-get prepared :extensions))
         (variables (plist-get prepared :variables))
         (new-mode-map (plist-get prepared :new-mode-map))
         (baseline (nskk--converter-capture-publish-rollback-baseline
                    root-symbols extensions variables mode-map-symbol))
         (old-tables (plist-get baseline :old-tables))
         (old-prolog-state (plist-get baseline :old-prolog-state))
         (old-extensions (plist-get baseline :old-extensions))
         (old-variables (plist-get baseline :old-variables))
         (old-mode-map-bound-p (plist-get baseline :old-mode-map-bound-p))
         (old-mode-map (plist-get baseline :old-mode-map))
         (old-mode-map-car (plist-get baseline :old-mode-map-car))
         (old-mode-map-cdr (plist-get baseline :old-mode-map-cdr))
         (mode-map-contents-replaced-p nil))
    (condition-case condition
        (let ((inhibit-quit t))
          (nskk--converter-publish-commit-state
           root-symbols tables prolog-state extensions variables)
          (cond
           ((not mode-map-bound-p)
            (makunbound mode-map-symbol))
           ((and (consp old-mode-map) (consp new-mode-map))
            ;; Record the splice before performing it.  The splice mutates the
            ;; live keymap in place, so if it signals partway the handler must
            ;; still know to restore car/cdr -- a flag derived from this step's
            ;; return value would be lost to the non-local exit.
            (setq mode-map-contents-replaced-p t)
            (nskk--converter-replace-keymap-contents old-mode-map new-mode-map))
           (t
            (set mode-map-symbol new-mode-map))))
      ((error quit)
       (let ((inhibit-quit t))
         (nskk--converter-publish-rollback
          root-symbols old-tables old-prolog-state old-extensions
          old-variables mode-map-contents-replaced-p old-mode-map
          old-mode-map-car old-mode-map-cdr old-mode-map-bound-p
          mode-map-symbol))
       (signal (car condition) (cdr condition))))))

(defun/k nskk-converter-load-style (style)
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
