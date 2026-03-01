;;; nskk-azik.el --- AZIK extended romaji input support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
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

;; AZIK extended romaji input for NSKK (Layer 3: Application).
;;
;; Layer position: L3 (Application) -- depends on nskk-converter and
;;   nskk-prolog.  Loaded optionally by nskk-input.el when
;;   `nskk-converter-romaji-style' is set to \\='azik.
;;
;; Implements the AZIK specification for efficient Japanese input.  AZIK
;; reduces keystrokes compared to standard romaji by using consonant suffix
;; keys for hatsuon (ん extension) and double vowel sequences.
;;
;; Architecture:
;; - Standard romaji rules are initialized via nskk--initialize-romaji-table.
;; - AZIK-specific rules are stored in the `azik-rule/2' Prolog predicate.
;; - The `nskk-azik-rules' macro asserts batches of azik-rule/2 facts at
;;   load time, expanding into individual nskk-prolog-<- calls.
;; - A bridge rule (romaji-to-kana ?r ?k) :- (azik-rule ?r ?k) connects
;;   azik-rule/2 to romaji-to-kana/2 for unified Prolog queries.
;;   Note: the bridge rule is NOT indexed by the trie (variable first arg);
;;   ground trie queries on romaji-to-kana/2 bypass AZIK rules.
;; - The hash table is populated from azik-rule/2 for hot-path lookups.
;;   `nskk-converter-lookup' (inline) reads only from the hash, never Prolog.
;;
;; Partial match markers (:incomplete entries in the hash) are derived
;; automatically from azik-rule/2 by scanning all romaji keys of length > 1
;; and computing their proper prefixes.  This covers 2-char youon prefixes
;; (e.g., "kg", "hg") automatically.
;;
;; Prolog predicates maintained by this module:
;; - `azik-rule/2'  -- (romaji kana) AZIK-specific conversion rules,
;;     hash-indexed on first arg for O(1) lookup.
;;
;; AZIK rule categories (stored in azik-rule/2):
;; 1. Special keys (; -> っ, : -> ー)
;; 2. Consonant compatibility (x=しゃ行, c=ちゃ行)
;; 3. Hatsuon extensions (z/k/j/d/l -> +ん)
;; 4. Double vowel extensions (q/h/w/p -> +vowel pair)
;; 5. Youon compatibility (g substitutes for y)
;; 6. Same-finger alternatives (f suffix)
;; 7. Word shortcuts
;; 8. Foreign word extensions

;;; Code:

(require 'nskk-converter)
(require 'nskk-prolog)

(defgroup nskk-azik nil
  "AZIK extended romaji input settings."
  :prefix "nskk-azik-"
  :group 'nskk-converter)

(defcustom nskk-azik-q-behavior 'context-aware
  "Behavior of q key when AZIK style is active.
\\='context-aware - Produce ん when there is pending romaji input,
                 otherwise toggle hiragana/katakana mode (recommended)
\\='always-n      - Always produce ん (AZIK purist)
\\='toggle-only   - Keep standard SKK toggle behavior (use nn for ん)"
  :type '(choice (const :tag "Context-aware (Recommended)" context-aware)
                 (const :tag "Always produce ん" always-n)
                 (const :tag "Toggle only" toggle-only))
  :group 'nskk-azik)

(defcustom nskk-azik-keyboard-type 'jp106
  "Keyboard layout for AZIK mappings.
Affects key position-based shortcuts.
\\='jp106 - Japanese 106-key layout (default)
\\='us101 - US 101-key layout"
  :type '(choice (const :tag "Japanese 106-key" jp106)
                 (const :tag "US 101-key" us101))
  :group 'nskk-azik)

;;;; AZIK Macros

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
Base rules generated for a/u/e/o only (no i).
Hatsuon and double vowel extensions are generated for all positions."
  (declare (indent 0) (debug t))
  `(progn
     ;; Base rules (no i for youon)
     (nskk-prolog-<- (azik-rule ,(concat prefix "a") ,a))
     (nskk-prolog-<- (azik-rule ,(concat prefix "u") ,u))
     (nskk-prolog-<- (azik-rule ,(concat prefix "e") ,e))
     (nskk-prolog-<- (azik-rule ,(concat prefix "o") ,o))
     ;; Hatsuon + double vowel
     (nskk-azik-extensions ,prefix ,a ,i ,u ,e ,o)))

(defmacro nskk-azik-rules (&rest rules)
  "Assert multiple azik-rule/2 Prolog facts from a literal rule list.
Each element of RULES must be a (ROMAJI KANA) pair of string literals.
Expands at compile time into individual nskk-prolog-<- calls."
  (declare (debug t) (indent 0))
  `(progn
     ,@(mapcar (lambda (rule)
                 `(nskk-prolog-<- (azik-rule ,(car rule) ,(cadr rule))))
               rules)))

(defconst nskk-azik--extension-rows
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
    ("p" "ぱ" "ぴ" "ぷ" "ぺ" "ぽ"))
  "Consonant rows for AZIK hatsuon + double-vowel extension rules.
Each entry is (PREFIX A I U E O) passed to `nskk-azik-extensions'.
The w-row has 7 elements: the extra element is the DV-O override (うぉ).")

(defconst nskk-azik--youon-rows
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

;;;; Runtime assertion helpers (for use with `apply' + `dolist')
;; The macros `nskk-azik-extensions' and `nskk-azik-youon' are compile-time
;; constructs and cannot be passed to `apply'.  These functions are the runtime
;; equivalents, calling `nskk-prolog-assert' directly.

(defun nskk-azik--fn-hatsuon (prefix a i u e o)
  "Register AZIK hatsuon (撥音) extensions at runtime.
Runtime equivalent of `nskk-azik-hatsuon' for use with `apply'."
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "z") (concat a "ん"))))
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "k") (concat i "ん"))))
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "j") (concat u "ん"))))
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "d") (concat e "ん"))))
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "l") (concat o "ん")))))

(defun nskk-azik--fn-double-vowel (prefix a u e o)
  "Register AZIK double-vowel (二重母音) extensions at runtime.
Runtime equivalent of `nskk-azik-double-vowel' for use with `apply'."
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "q") (concat a "い"))))
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "h") (concat u "う"))))
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "w") (concat e "い"))))
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "p") (concat o "う")))))

(defun nskk-azik--fn-extensions (prefix a i u e o &optional dv-o)
  "Register hatsuon + double-vowel extensions at runtime.
Runtime equivalent of `nskk-azik-extensions' for use with `apply'.
PREFIX/A/I/U/E/O are the consonant and vowel kana strings.
DV-O overrides O for double vowel (e.g., わ行 uses うぉ instead of を)."
  (nskk-azik--fn-hatsuon prefix a i u e o)
  (nskk-azik--fn-double-vowel prefix a u e (or dv-o o)))

(defun nskk-azik--fn-youon (prefix a i u e o)
  "Register AZIK youon (拗音) row at runtime.
Runtime equivalent of `nskk-azik-youon' for use with `apply'."
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "a") a)))
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "u") u)))
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "e") e)))
  (nskk-prolog-assert (list (list 'azik-rule (concat prefix "o") o)))
  (nskk-azik--fn-extensions prefix a i u e o))

(defun nskk--init-azik-rules ()
  "Initialize AZIK romaji rules.
Sets up standard romaji as base, then asserts AZIK-specific rules
into the azik-rule/2 Prolog predicate.  A bridge rule connects
azik-rule/2 to romaji-to-kana/2 for unified Prolog queries.
The hash table is populated from azik-rule/2 for hot-path lookups."

  ;; ============================================================
  ;; Step 1: Initialize standard romaji rules as the base.
  ;; nskk--initialize-romaji-table is an internal function in
  ;; nskk-converter.el.  Calling it here avoids duplicating the
  ;; ~50-rule standard romaji set; it clears and repopulates both
  ;; romaji-to-kana/2 and the hash table before AZIK rules are added.
  ;; ============================================================
  (nskk--initialize-romaji-table)

  ;; ============================================================
  ;; Step 2: Set up azik-rule/2 predicate (index before assert).
  ;; ============================================================
  (nskk-prolog-retract-all 'azik-rule 2)
  (nskk-prolog-set-index 'azik-rule 2 :hash)

  ;; ============================================================
  ;; 1. Special keys
  ;; ============================================================
  (nskk-prolog-<- (azik-rule ";" "っ"))
  (nskk-prolog-<- (azik-rule ":" "ー"))

  ;; ============================================================
  ;; 2. Consonant compatibility keys (x/c prefixes)
  ;; ============================================================
  ;; x prefix - しゃ行互換 (overrides standard small-kana xa=ぁ)
  ;; c prefix - ちゃ行互換
  (nskk-azik-rules
   ("xa" "しゃ") ("xi" "し") ("xu" "しゅ") ("xe" "しぇ") ("xo" "しょ")
   ("ca" "ちゃ") ("ci" "ち") ("cu" "ちゅ") ("ce" "しぇ") ("co" "ちょ"))

  ;; ============================================================
  ;; 3-4. Hatsuon (撥音) + Double vowel (二重母音) extensions
  ;; Extension keys: z=a+ん, k=i+ん, j=u+ん, d=e+ん, l=o+ん
  ;;                 q=a+い, h=u+う, w=e+い, p=o+う
  ;; ============================================================
  (dolist (row nskk-azik--extension-rows)
    (apply #'nskk-azik--fn-extensions row))

  ;; ============================================================
  ;; 5. Youon compatibility (g substitutes for y)
  ;; Each row: base (a/u/e/o) + hatsuon (5) + double vowel (4)
  ;; ============================================================
  (dolist (row nskk-azik--youon-rows)
    (apply #'nskk-azik--fn-youon row))

  ;; ============================================================
  ;; 6. Same-finger alternatives (f suffix)
  ;; ============================================================
  (nskk-azik-rules
   ("kf" "き") ("nf" "ぬ") ("mf" "む") ("gf" "ぐ")
   ("pf" "ぷ") ("rf" "る") ("yf" "ゆ"))

  ;; ============================================================
  ;; 7. Word shortcuts
  ;; ============================================================
  (nskk-azik-rules
   ("km" "かも") ("kr" "から") ("gr" "がら") ("kt" "こと") ("gt" "ごと")
   ("zr" "ざる") ("st" "した") ("sr" "する") ("tt" "たち") ("dt" "だち")
   ("tb" "たび") ("tm" "ため") ("tr" "たら") ("ds" "です") ("dm" "でも")
   ("nr" "なる") ("nt" "にち") ("nb" "ねば") ("ht" "ひと") ("bt" "びと")
   ("ms" "ます") ("mt" "また") ("mn" "もの") ("yr" "よる")
   ("rr" "られ") ("wt" "わた") ("wr" "われ"))

  ;; ============================================================
  ;; 8. Foreign word extensions
  ;; ============================================================
  (nskk-azik-rules
   ("tgi" "てぃ") ("tgu" "とぅ") ("dci" "でぃ") ("dcu" "どぅ") ("wso" "うぉ"))

  ;; ============================================================
  ;; Step 3: Bridge rule — AZIK rules are also romaji-to-kana.
  ;; Placed after all azik-rule/2 facts so Prolog queries on
  ;; romaji-to-kana/2 can discover AZIK rules via this rule.
  ;;
  ;; Trie limitation: because the head has a variable first arg (?r),
  ;; this rule is NOT inserted into the trie index.  Ground queries on
  ;; romaji-to-kana/2 via the trie will not find AZIK rules through
  ;; this bridge.  Use nskk-prolog-query on azik-rule/2 directly for
  ;; enumeration; hot-path lookups use the hash cache (Step 4).
  ;; ============================================================
  (nskk-prolog-<- (romaji-to-kana \?r \?k)
    (azik-rule \?r \?k))

  ;; ============================================================
  ;; Step 4: Populate hash table from azik-rule/2 for hot-path.
  ;; nskk-converter-lookup reads from hash only (inline function),
  ;; so we must sync all azik-rule facts into the hash.
  ;; AZIK entries override any conflicting standard entries (e.g. xa).
  ;;
  ;; We use puthash directly into nskk--romaji-table rather than
  ;; nskk-converter-add-rule because the Prolog facts already exist
  ;; (nskk-converter-add-rule would double-assert them).  This step is
  ;; purely a hash-cache sync from the Prolog truth source.
  ;; ============================================================
  (dolist (subst (nskk-prolog-query '(azik-rule \?r \?k)))
    (let ((romaji (nskk-prolog-walk '\?r subst))
          (kana (nskk-prolog-walk '\?k subst)))
      (when (and (stringp romaji) (stringp kana))
        (puthash romaji kana nskk--romaji-table))))

  ;; ============================================================
  ;; Partial match markers — derived from azik-rule/2, hash only.
  ;; :incomplete is not a string so it stays out of azik-rule/2.
  ;; Scan all romaji keys of length > 1 and compute every proper
  ;; prefix; these become :incomplete entries so the converter
  ;; knows to keep accumulating input.  This automatically covers
  ;; single-char consonants (k, g, ...) and 2-char youon prefixes
  ;; (kg, hg, ...) without a hand-maintained list.
  ;; ============================================================
  (let ((partials (make-hash-table :test 'equal)))
    (dolist (subst (nskk-prolog-query '(azik-rule \?r \?_)))
      (let* ((romaji (nskk-prolog-walk '\?r subst))
             (len (and (stringp romaji) (length romaji))))
        (when (and len (> len 1))
          (dotimes (i (1- len))
            (puthash (substring romaji 0 (1+ i)) t partials)))))
    (maphash (lambda (prefix _)
               (unless (gethash prefix nskk--romaji-table)
                 (nskk-converter-add-rule prefix :incomplete)))
             partials)))

;; Register AZIK style
(nskk-converter-register-style 'azik #'nskk--init-azik-rules)

(provide 'nskk-azik)

;;; nskk-azik.el ends here
