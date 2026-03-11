;;; nskk-azik.el --- AZIK extended romaji input support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: i18n

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

;; AZIK extended romaji input for NSKK (Layer 5: Presentation).
;;
;; Layer position: L5 (Presentation) -- depends on nskk-converter,
;;   nskk-keymap, nskk-prolog, nskk-cps-macros.  Loaded optionally by
;;   nskk-input.el when `nskk-converter-romaji-style' is set to \\='azik.
;;
;; Implements the AZIK specification for efficient Japanese input.  AZIK
;; reduces keystrokes compared to standard romaji by using consonant suffix
;; keys for hatsuon (ん extension) and double vowel sequences.
;;
;; Architecture:
;; - Rule data is stored in `defconst' tables for data/logic separation.
;;   See `nskk--azik-special-keys', `nskk--azik-extension-rows', etc.
;; - Core macros (`nskk-azik-hatsuon', `nskk-azik-double-vowel', etc.)
;;   generate azik-rule/2 Prolog facts from literal arguments.
;; - Meta-macros (`nskk--azik-init-extension-rows',
;;   `nskk--azik-init-youon-rows') expand entire rule tables at compile
;;   time by iterating the defconst data, eliminating runtime iteration
;;   and the need for separate runtime-equivalent functions.
;; - A bridge rule (romaji-to-kana ?r ?k) :- (azik-rule ?r ?k) connects
;;   azik-rule/2 to romaji-to-kana/2 for unified Prolog queries.
;;   Note: the bridge rule is NOT indexed by the trie (variable first arg).
;; - The hash table is populated from azik-rule/2 for hot-path lookups.
;;   `nskk-converter-lookup' (inline) reads only from the hash, never Prolog.
;;
;; Partial match markers (:incomplete entries) are derived automatically
;; from azik-rule/2 by scanning all romaji keys of length > 1 and
;; computing their proper prefixes.
;;
;; Prolog predicates maintained by this module:
;; - `azik-rule/2'  -- (romaji kana) AZIK-specific conversion rules,
;;     hash-indexed on first arg for O(1) lookup.
;;
;; AZIK rule categories (in `azik-rule/2'):
;; 1. Special keys (; -> っ, : -> ー)
;; 2. Consonant compatibility (x=しゃ行, c=ちゃ行)
;; 3. Hatsuon extensions (z/k/j/d/l -> +ん)
;; 4. Double vowel extensions (q/h/w/p -> +vowel pair)
;; 5. Youon compatibility (g substitutes for y)
;; 6. Same-finger alternatives (f suffix)
;; 7. Word shortcuts
;; 8. Foreign word extensions
;;
;; X/C Prefix Extensions:
;; The x and c prefixes provide compatibility rows for sha/shu/sho and
;; cha/chu/cho, and support all AZIK extension keys enabling compound
;; input like xhka → しゅうか (shuuka).

;;; Code:

(require 'cl-lib)
(require 'nskk-cps-macros)
(require 'nskk-converter)
(require 'nskk-keymap)
(require 'nskk-prolog)

(defgroup nskk-azik nil
  "AZIK extended romaji input settings."
  :prefix "nskk-azik-"
  :group 'nskk-converter)

(defcustom nskk-azik-keyboard-type 'jp106
  "Keyboard layout for AZIK mappings.
Affects key position-based shortcuts.
\\='jp106 - Japanese 106-key layout (default)
\\='us101 - US 101-key layout"
  :type '(choice (const :tag "Japanese 106-key" jp106)
                 (const :tag "US 101-key" us101))
  :group 'nskk-azik)

;;;; Compile-time Rule Macros

(defmacro nskk-azik-hatsuon (prefix a i u e o)
  "Define AZIK hatsuon (撥音) extensions as azik-rule/2 Prolog facts.
PREFIX is the consonant key string.
A/I/U/E/O are the base kana for each vowel position.
Generates: prefix+z→A+ん, prefix+k→I+ん, prefix+j→U+ん,
           prefix+d→E+ん, prefix+l→O+ん."
  (declare (indent 0) (debug t))
  `(progn
     (nskk-prolog-<- (azik-rule ,(concat prefix "z") ,(concat a "ん")))
     (nskk-prolog-<- (azik-rule ,(concat prefix "k") ,(concat i "ん")))
     (nskk-prolog-<- (azik-rule ,(concat prefix "j") ,(concat u "ん")))
     (nskk-prolog-<- (azik-rule ,(concat prefix "d") ,(concat e "ん")))
     (nskk-prolog-<- (azik-rule ,(concat prefix "l") ,(concat o "ん")))))

(defmacro nskk-azik-double-vowel (prefix a u e o)
  "Define AZIK double vowel (二重母音) extensions as azik-rule/2 Prolog facts.
PREFIX is the consonant key string.
A/U/E/O are the base kana for each vowel position.
Generates: prefix+q→A+い, prefix+h→U+う, prefix+w→E+い, prefix+p→O+う."
  (declare (indent 0) (debug t))
  `(progn
     (nskk-prolog-<- (azik-rule ,(concat prefix "q") ,(concat a "い")))
     (nskk-prolog-<- (azik-rule ,(concat prefix "h") ,(concat u "う")))
     (nskk-prolog-<- (azik-rule ,(concat prefix "w") ,(concat e "い")))
     (nskk-prolog-<- (azik-rule ,(concat prefix "p") ,(concat o "う")))))

(defmacro nskk-azik-extensions (prefix a i u e o &optional dv-o)
  "Define hatsuon + double vowel extensions for a consonant row.
PREFIX is the consonant key string.
A/I/U/E/O are the base kana for each vowel position.
DV-O overrides O for double vowel (e.g., わ行 uses うぉ instead of を)."
  (declare (indent 0) (debug t))
  `(progn
     (nskk-azik-hatsuon ,prefix ,a ,i ,u ,e ,o)
     (nskk-azik-double-vowel ,prefix ,a ,u ,e ,(or dv-o o))))

(defmacro nskk-azik-youon (prefix a i u e o)
  "Define AZIK youon (拗音) row with base rules + all extensions.
PREFIX is the key combo (e.g., \"kg\" for きゃ行).
A/I/U/E/O are the base kana.
Base rules generated for a/u/e/o only (no i for youon).
Hatsuon and double vowel extensions are generated for all positions."
  (declare (indent 0) (debug t))
  `(progn
     (nskk-prolog-<- (azik-rule ,(concat prefix "a") ,a))
     (nskk-prolog-<- (azik-rule ,(concat prefix "u") ,u))
     (nskk-prolog-<- (azik-rule ,(concat prefix "e") ,e))
     (nskk-prolog-<- (azik-rule ,(concat prefix "o") ,o))
     (nskk-azik-extensions ,prefix ,a ,i ,u ,e ,o)))

;;;; Static Rule Data

(defconst nskk--azik-special-keys
  '((";" "っ") (":" "ー"))
  "Special keys: semicolon → っ (geminate stop), colon → ー (prolonged sound).")

(defconst nskk--azik-consonant-compat-rules
  '(("xa" "しゃ") ("xi" "し") ("xu" "しゅ") ("xe" "しぇ") ("xo" "しょ")
    ("ca" "ちゃ") ("ci" "ち") ("cu" "ちゅ") ("ce" "ちぇ") ("co" "ちょ"))
  "Consonant compatibility rules.
x-prefix: しゃ行 (overrides standard small-kana xa=ぁ).
c-prefix: ちゃ行.")

(eval-and-compile
(defconst nskk--azik-extension-rows
  '(("k" "か" "き" "く" "け" "こ")
    ("s" "さ" "し" "す" "せ" "そ")
    ("t" "た" "ち" "つ" "て" "と")
    ("n" "な" "に" "ぬ" "ね" "の")
    ("h" "は" "ひ" "ふ" "へ" "ほ")
    ("m" "ま" "み" "む" "め" "も")
    ("y" "や" "い" "ゆ" "え" "よ")
    ("r" "ら" "り" "る" "れ" "ろ")
    ("w" "わ" "うぃ" "う" "うぇ" "を" "うぉ")
    ("g" "が" "ぎ" "ぐ" "げ" "ご")
    ("z" "ざ" "じ" "ず" "ぜ" "ぞ")
    ("d" "だ" "ぢ" "づ" "で" "ど")
    ("b" "ば" "び" "ぶ" "べ" "ぼ")
    ("p" "ぱ" "ぴ" "ぷ" "ぺ" "ぽ")
    ("x" "しゃ" "し" "しゅ" "しぇ" "しょ")
    ("c" "ちゃ" "ち" "ちゅ" "ちぇ" "ちょ"))
  "Consonant rows for AZIK hatsuon + double-vowel extension rules.
Each entry is (PREFIX A I U E O) or (PREFIX A I U E O DV-O) where DV-O
overrides O for the double-vowel rule.

Special rows:
- The w-row has 7 elements: the extra element is the DV-O override (うぉ).
- The x-row provides sha/shu/sho compatibility (しゃ/し/しゅ/しぇ/しょ).
- The c-row provides cha/chu/cho compatibility (ちゃ/ち/ちゅ/ちぇ/ちょ).

The x and c rows support all extension keys (z/k/j/d/l for hatsuon,
q/h/w/p for diphthong) enabling compound input like xhka → しゅうか.")

(defconst nskk--azik-youon-rows
  '(("kg" "きゃ" "きぃ" "きゅ" "きぇ" "きょ")
    ("hg" "ひゃ" "ひぃ" "ひゅ" "ひぇ" "ひょ")
    ("mg" "みゃ" "みぃ" "みゅ" "みぇ" "みょ")
    ("rg" "りゃ" "りぃ" "りゅ" "りぇ" "りょ")
    ("gg" "ぎゃ" "ぎぃ" "ぎゅ" "ぎぇ" "ぎょ")
    ("jg" "じゃ" "じぃ" "じゅ" "じぇ" "じょ")
    ("bg" "びゃ" "びぃ" "びゅ" "びぇ" "びょ")
    ("pg" "ぴゃ" "ぴぃ" "ぴゅ" "ぴぇ" "ぴょ"))
  "Youon (拗音) rows for AZIK rules.
Each entry is (PREFIX A I U E O) passed to `nskk-azik-youon'.")
)

(defconst nskk--azik-same-finger-rules
  '(("kf" "き") ("nf" "ぬ") ("mf" "む") ("gf" "ぐ")
    ("pf" "ぷ") ("rf" "る") ("yf" "ゆ"))
  "Same-finger alternative rules (f suffix for ring-finger consonants).")

(defconst nskk--azik-word-shortcuts
  '(("km" "かも") ("kr" "から") ("gr" "がら") ("kt" "こと") ("gt" "ごと")
    ("zr" "ざる") ("st" "した") ("sr" "する") ("tt" "たち") ("dt" "だち")
    ("tb" "たび") ("tm" "ため") ("tr" "たら") ("ds" "です") ("dm" "でも")
    ("nr" "なる") ("nt" "にち") ("nb" "ねば") ("ht" "ひと") ("bt" "びと")
    ("ms" "ます") ("mt" "また") ("mn" "もの") ("yr" "よる")
    ("rr" "られ") ("wt" "わた") ("wr" "われ"))
  "Word shortcut rules for common Japanese words and particles.")

(defconst nskk--azik-foreign-extensions
  '(("tgi" "てぃ") ("tgu" "とぅ") ("dci" "でぃ") ("dcu" "どぅ") ("wso" "うぉ"))
  "Foreign word extension rules for non-native Japanese sounds.")

(defconst nskk--azik-compound-rules
  '(("kak" "かく") ("kaq" "かい") ("kakz" "かかん"))
  "Compound rules inserted into the hash table after the prefix-restore pass.

These rules are NOT asserted into azik-rule/2 to avoid the prefix-restore
step demoting their 2-char prefixes (e.g., \"ka\") back to :incomplete.
Adding them after the restore pass lets the greedy longest-match finder
discover e.g. \"kak\" before \"ka\", enabling compound input like
xhkak → しゅうかく (shuukaku).

Format: each entry is (ROMAJI KANA) where ROMAJI is the full key string
and KANA is the output string.")

;;;; Meta-macros (compile-time table expansion)

(defmacro nskk--azik-init-extension-rows ()
  "Assert hatsuon + double-vowel rules for all rows at compile time.
Iterates `nskk--azik-extension-rows' at macro-expansion time, producing
one `nskk-azik-extensions' call per row without any runtime iteration."
  (declare (indent 0) (debug t))
  `(progn
     ,@(mapcar (lambda (row) `(nskk-azik-extensions ,@row))
               nskk--azik-extension-rows)))

(defmacro nskk--azik-init-youon-rows ()
  "Assert youon rules for all rows at compile time.
Iterates `nskk--azik-youon-rows' at macro-expansion time, producing
one `nskk-azik-youon' call per row without any runtime iteration."
  (declare (indent 0) (debug t))
  `(progn
     ,@(mapcar (lambda (row) `(nskk-azik-youon ,@row))
               nskk--azik-youon-rows)))

;;;; Runtime Helpers

(defun/done nskk--azik-assert-rules (rules)
  "Assert RULES as azik-rule/2 Prolog facts at runtime.
RULES is a list of (ROMAJI KANA) string pairs."
  (dolist (rule rules)
    (nskk-prolog-assert `((azik-rule ,(car rule) ,(cadr rule))))))

(defun/done nskk--azik-sync-to-romaji-hash ()
  "Populate the romaji hash table from azik-rule/2 for hot-path lookups.
Called after all azik-rule/2 facts have been asserted.

`nskk-converter-lookup' (inline) reads from hash only, so we must sync
all azik-rule facts into the hash.  AZIK entries override any conflicting
standard entries (e.g. xa).

We use `puthash' directly into `nskk--romaji-table' rather than
`nskk-converter-add-rule' because the Prolog facts already exist
\(`nskk-converter-add-rule' would double-assert them).  This step is
purely a hash-cache sync from the Prolog truth source."
  (dolist (subst (nskk-prolog-query '(azik-rule \?r \?k)))
    (let ((romaji (nskk-prolog-walk '\?r subst))
          (kana   (nskk-prolog-walk '\?k subst)))
      (when (and (stringp romaji) (stringp kana))
        (puthash romaji kana nskk--romaji-table)))))

(defun/done nskk--azik-register-partial-prefixes ()
  "Register :incomplete markers for all proper prefixes of AZIK romaji keys.
Called after `nskk--azik-sync-to-romaji-hash' has populated the hash.

Scan all romaji keys of length > 1 and compute every proper prefix;
these become :incomplete entries so the converter knows to keep
accumulating input.  This automatically covers single-char consonants
\(k, g, ...) and 2-char youon prefixes (kg, hg, ...) without a
hand-maintained list."
  (let ((partials (make-hash-table :test 'equal)))
    (dolist (subst (nskk-prolog-query '(azik-rule \?r \?_)))
      (let* ((romaji (nskk-prolog-walk '\?r subst))
             (len    (and (stringp romaji) (length romaji))))
        (when (and len (> len 1))
          (dotimes (i (1- len))
            (puthash (substring romaji 0 (1+ i)) t partials)))))
    (maphash (lambda (prefix _)
               (unless (gethash prefix nskk--romaji-table)
                 (nskk-converter-add-rule prefix :incomplete)))
             partials)))

(defun nskk--azik-is-prefix-of-longer-p (key)
  "Return non-nil if KEY is a proper prefix of a longer entry in the romaji hash."
  (let ((key-len (length key)))
    (cl-loop for k being the hash-keys of nskk--romaji-table
             thereis (and (> (length k) key-len)
                          (string-prefix-p key k)))))

(defun/done nskk--azik-restore-standard-prefixes ()
  "Demote AZIK complete rules that are proper prefixes of longer hash entries.
Called after `nskk--azik-sync-to-romaji-hash' and
`nskk--azik-register-partial-prefixes'.

Problem: `nskk-azik-double-vowel' generates rules like \"sh\"→\"すう\" which
overwrite the standard romaji `:incomplete' marker for \"sh\".  But the
standard romaji table retains \"sha\"→\"しゃ\", \"shi\"→\"し\", etc.  When the
user types \"sha\", the hash returns \"すう\" at \"sh\" (a complete match) and
\"sha\" is never reached — the engine emits \"すう\" then
\"あ\" instead of \"しゃ\".

Fix: scan all complete (string-valued) key/value pairs in a single pass;
demote any key that is a proper prefix of some longer entry back to
`:incomplete' so multi-char standard rules remain reachable."
  ;; Safety: `puthash' here only updates the value of an existing key (k came
  ;; from the maphash iteration itself), never inserts a new key.  Updating
  ;; existing keys during `maphash' is documented-safe in Emacs Lisp.
  (maphash (lambda (k v)
              (when (and (stringp k) (stringp v)
                         (nskk--azik-is-prefix-of-longer-p k))
                (puthash k :incomplete nskk--romaji-table)))
            nskk--romaji-table))

;;;; Main Initialization

(defun/done nskk--init-azik-rules ()
  "Initialize AZIK romaji rules.
Sets up standard romaji as base, then asserts AZIK-specific rules
into the azik-rule/2 Prolog predicate.  A bridge rule connects
azik-rule/2 to romaji-to-kana/2 for unified Prolog queries.
The hash table is populated from azik-rule/2 for hot-path lookups."

  ;; Step 1: Initialize standard romaji rules as the base.
  ;; Clears and repopulates both romaji-to-kana/2 and the hash table
  ;; before AZIK rules are added.
  (nskk--initialize-romaji-table)

  ;; Step 2: Set up azik-rule/2 predicate (index before assert).
  (nskk-prolog-retract-all 'azik-rule 2)
  (nskk-prolog-set-index 'azik-rule 2 :hash)

  ;; Step 3: Assert all AZIK rule categories.

  ;; 3a. Special keys (; → っ, : → ー)
  (nskk--azik-assert-rules nskk--azik-special-keys)

  ;; 3b. Consonant compatibility (x=しゃ行, c=ちゃ行)
  (nskk--azik-assert-rules nskk--azik-consonant-compat-rules)

  ;; 3c. Hatsuon (撥音) + double vowel (二重母音) extensions.
  ;;     Extension keys: z=a+ん, k=i+ん, j=u+ん, d=e+ん, l=o+ん
  ;;                     q=a+い, h=u+う, w=e+い, p=o+う
  (nskk--azik-init-extension-rows)

  ;; 3d. Youon (拗音) compatibility — g substitutes for y.
  ;;     Each row: base (a/u/e/o) + hatsuon (5) + double vowel (4)
  (nskk--azik-init-youon-rows)

  ;; 3e. Same-finger alternatives (f suffix)
  (nskk--azik-assert-rules nskk--azik-same-finger-rules)

  ;; 3f. Word shortcuts
  (nskk--azik-assert-rules nskk--azik-word-shortcuts)

  ;; 3g. Foreign word extensions
  (nskk--azik-assert-rules nskk--azik-foreign-extensions)

  ;; Step 4: Bridge rule — AZIK rules are also romaji-to-kana.
  ;; Placed after all azik-rule/2 facts so Prolog queries on
  ;; romaji-to-kana/2 can discover AZIK rules via this rule.
  ;;
  ;; Trie limitation: the variable first arg (?r) means this rule is NOT
  ;; inserted into the trie index.  Use nskk-prolog-query on azik-rule/2
  ;; directly for enumeration; hot-path lookups use the hash cache.
  (nskk-prolog-<- (romaji-to-kana \?r \?k)
    (azik-rule \?r \?k))

  ;; Step 5: Populate hash table from azik-rule/2, then register
  ;; :incomplete markers for all proper prefixes.
  (nskk--azik-sync-to-romaji-hash)
  (nskk--azik-register-partial-prefixes)

  ;; Step 6: Restore standard-romaji prefix semantics.
  ;; AZIK sync may have overwritten standard `:incomplete' markers with
  ;; complete AZIK rules (e.g. "sh"→"すう") that shadow longer standard-
  ;; romaji entries ("sha"→"しゃ").  Demote any such key back to
  ;; `:incomplete' so multi-char standard rules remain reachable.
  (nskk--azik-restore-standard-prefixes)

  ;; Step 7: Compound rules added after prefix restore.
  ;; Inserting after the restore pass ensures 2-char prefixes like "ka"
  ;; remain complete, enabling sequences like xhkak → しゅうかく.
  (dolist (rule nskk--azik-compound-rules)
    (puthash (car rule) (cadr rule) nskk--romaji-table))

  ;; Step 8: Set up AZIK toggle key binding.
  (nskk--setup-azik-toggle-key))

;; Register AZIK style
;;;###autoload
(nskk-converter-register-style 'azik 'nskk--init-azik-rules)

(provide 'nskk-azik)

;;; nskk-azik.el ends here
