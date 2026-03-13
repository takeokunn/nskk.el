;;; nskk-azik.el --- AZIK extended romaji input support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
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
;; Partial match markers and standard-prefix restoration are handled by
;; `nskk--azik-finalize-hash-table' using Prolog classification predicates:
;; - `azik-vowel-char/1'      -- succeeds for vowel character codes (a/i/u/e/o).
;; - `azik-key-extends/2'     -- (prefix char) for every proper prefix in hash.
;; - `azik-nonvowel-ext/1'    -- derived rule; succeeds when key has non-vowel ext.
;; - `azik-vowel-shadow/1'    -- derived rule; succeeds when all extensions are vowels.
;; - `nskk--azik-classify-key/k' -- CPS classifier using azik-vowel-shadow/1.
;;
;; Prolog predicates maintained by this module:
;; - `azik-rule/2'         -- (romaji kana) AZIK-specific conversion rules,
;;     hash-indexed on first arg for O(1) lookup.
;; - `azik-vowel-char/1'   -- integer character code facts for a/i/u/e/o.
;; - `azik-key-extends/2'  -- (prefix char) prefix→next-char extension map.
;; - `azik-nonvowel-ext/1' -- rule: KEY has at least one non-vowel extension.
;; - `azik-vowel-shadow/1' -- rule: KEY is AZIK-complete with vowel-only exts.
;;
;; AZIK rule categories (in `azik-rule/2'):
;; 1. Special keys (; -> っ, : -> ー)
;; 2. Consonant compatibility (x=しゃ行, c=ちゃ行)
;; 3. Hatsuon extensions (z/k/j/d/l -> +ん)
;; 4. Double vowel extensions (q/h/w/p -> +vowel pair)
;; 5. Youon compatibility (g-substitution AZIK-specific + y-prefix DDSKK-compatible)
;; 6. Same-finger alternatives (hf=ふ, kf=き, nf=ぬ, mf=む, gf=ぐ, pf=ぷ, rf=る, yf=ゆ)
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

(defconst nskk--azik-jp106-rules
  '(("+" "っ"))
  "JP106-specific AZIK rules: + (Shift+;) maps to っ (sokuon).
On JP106 keyboards, Shift+; produces + instead of :.  This rule makes +
equivalent to ; (→ っ) in romaji contexts where colon-okurigana does not apply.")

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
    ("f" "ふぁ" "ふぃ" "ふ" "ふぇ" "ふぉ")
    ("j" "じゃ" "じ" "じゅ" "じぇ" "じょ")
    ("v" "ゔぁ" "ゔぃ" "ゔ" "ゔぇ" "ゔぉ")
    ("x" "しゃ" "し" "しゅ" "しぇ" "しょ")
    ("c" "ちゃ" "ち" "ちゅ" "ちぇ" "ちょ"))
  "Consonant rows for AZIK hatsuon + double-vowel extension rules.
Each entry is (PREFIX A I U E O) or (PREFIX A I U E O DV-O) where DV-O
overrides O for the double-vowel rule.

Special rows:
- The w-row has 7 elements: the extra element is the DV-O override (うぉ).
- The f-row covers foreign-sound ふぁ/ふぃ/ふ/ふぇ/ふぉ (enabling fq→ふぁい etc.).
- The j-row covers じゃ行 extensions (jq→じゃい, jj→じゅん, jh→じゅう etc.).
- The v-row covers ゔ行 extensions (vq→ゔぁい, vd→ゔぇん etc.).
- The x-row provides sha/shu/sho compatibility (しゃ/し/しゅ/しぇ/しょ).
- The c-row provides cha/chu/cho compatibility (ちゃ/ち/ちゅ/ちぇ/ちょ).

The x and c rows support all extension keys (z/k/j/d/l for hatsuon,
q/h/w/p for diphthong) enabling compound input like xhka → しゅうか.")

(defconst nskk--azik-youon-rows
  '(;; g-substitution youon (AZIK-specific: g replaces y)
    ("ng" "にゃ" "にぃ" "にゅ" "にぇ" "にょ")
    ("kg" "きゃ" "きぃ" "きゅ" "きぇ" "きょ")
    ("hg" "ひゃ" "ひぃ" "ひゅ" "ひぇ" "ひょ")
    ("mg" "みゃ" "みぃ" "みゅ" "みぇ" "みょ")
    ("rg" "りゃ" "りぃ" "りゅ" "りぇ" "りょ")
    ("gg" "ぎゃ" "ぎぃ" "ぎゅ" "ぎぇ" "ぎょ")
    ("jg" "じゃ" "じぃ" "じゅ" "じぇ" "じょ")
    ("bg" "びゃ" "びぃ" "びゅ" "びぇ" "びょ")
    ("pg" "ぴゃ" "ぴぃ" "ぴゅ" "ぴぇ" "ぴょ")
    ;; standard romaji y-prefix youon (DDSKK-compatible: ry, ky, etc.)
    ;; Enables AZIK extension keys on standard y-prefix sequences:
    ;;   ryp → りょう, ryh → りゅう, ryz → りゃん, etc.
    ("ny" "にゃ" "にぃ" "にゅ" "にぇ" "にょ")
    ("ky" "きゃ" "きぃ" "きゅ" "きぇ" "きょ")
    ("hy" "ひゃ" "ひぃ" "ひゅ" "ひぇ" "ひょ")
    ("my" "みゃ" "みぃ" "みゅ" "みぇ" "みょ")
    ("ry" "りゃ" "りぃ" "りゅ" "りぇ" "りょ")
    ("gy" "ぎゃ" "ぎぃ" "ぎゅ" "ぎぇ" "ぎょ")
    ("jy" "じゃ" "じぃ" "じゅ" "じぇ" "じょ")
    ("by" "びゃ" "びぃ" "びゅ" "びぇ" "びょ")
    ("py" "ぴゃ" "ぴぃ" "ぴゅ" "ぴぇ" "ぴょ"))
  "Youon (拗音) rows for AZIK rules.
Each entry is (PREFIX A I U E O) passed to `nskk-azik-youon'.

Two parallel sets of rows are provided:
- g-substitution (AZIK-specific): ng/kg/hg/mg/rg/gg/jg/bg/pg
  These use g as a y-substitute, the original AZIK design.
- y-prefix (DDSKK-compatible): ny/ky/hy/my/ry/gy/jy/by/py
  These add AZIK extension keys (hatsuon z/k/j/d/l, diphthong q/h/w/p)
  to standard romaji y-prefix youon sequences.  e.g. ryp → りょう.")
)

(defconst nskk--azik-same-finger-rules
  '(("kf" "き") ("hf" "ふ") ("nf" "ぬ") ("mf" "む") ("gf" "ぐ")
    ("pf" "ぷ") ("rf" "る") ("yf" "ゆ"))
  "Same-finger alternative rules (f suffix for ergonomic consonant alternatives).
hf=ふ avoids the h→u same-hand sequence (h and f share the left index finger).")

(defconst nskk--azik-word-shortcuts
  '(("km" "かも") ("kr" "から") ("gr" "がら") ("kt" "こと") ("gt" "ごと")
    ("zr" "ざる") ("st" "した") ("ss" "せい") ("sr" "する") ("tt" "たち") ("dt" "だち")
    ("tb" "たび") ("tm" "ため") ("tr" "たら") ("ds" "です") ("dm" "でも")
    ("nr" "なる") ("nt" "にち") ("nb" "ねば") ("ht" "ひと") ("bt" "びと")
    ("ms" "ます") ("mt" "また") ("mn" "もの") ("yr" "よる")
    ("rr" "られ") ("wt" "わた") ("wr" "われ"))
  "Word shortcut rules for common Japanese words and particles.")

(defconst nskk--azik-foreign-extensions
  '(("tgi" "てぃ") ("tgu" "とぅ") ("dci" "でぃ") ("dcu" "どぅ") ("wso" "うぉ"))
  "Foreign word extension rules for non-native Japanese sounds.")

(defconst nskk--azik-foreign-hatsuon-rules
  '(;; tg prefix: k→i-variant+ん, j→u-variant+ん
    ("tgk" "てぃん") ("tgj" "とぅん")
    ;; dc prefix: k→i-variant+ん, j→u-variant+ん
    ("dck" "でぃん") ("dcj" "どぅん")
    ;; wso prefix: k→single-variant+ん
    ("wsok" "うぉん"))
  "Hatsuon (撥音) extensions for foreign word prefixes.
k suffix → i-variant + ん, j suffix → u-variant + ん.
Only k (i-position) and j (u-position) are used because tg/dc foreign
prefixes have only two variants (i and u); the remaining hatsuon keys
(z/d/l for a/e/o positions) have no corresponding vowel variant.
For wso (single o-variant), k suffix → うぉん.")

(defconst nskk--azik-foreign-double-vowel-rules
  '(;; tg prefix: q/h→i-variant vowel repeat, w/p→u-variant vowel repeat
    ("tgq" "てぃい") ("tgh" "てぃい") ("tgw" "とぅう") ("tgp" "とぅう")
    ;; dc prefix: q/h→i-variant vowel repeat, w/p→u-variant vowel repeat
    ("dcq" "でぃい") ("dch" "でぃい") ("dcw" "どぅう") ("dcp" "どぅう")
    ;; wso prefix: all keys→nearest (o-variant vowel repeat)
    ("wsoq" "うぉお") ("wsoh" "うぉお") ("wsow" "うぉお") ("wsop" "うぉお"))
  "Double-vowel extensions for foreign word prefixes.
q/h → i-variant's vowel repeat, w/p → u-variant's vowel repeat.
For wso (single o-variant), all keys produce うぉお.")

(defconst nskk--azik-compound-rules
  '(("kak" "かく") ("kaq" "かい") ("kakz" "かかん")
    ("wso" "うぉ"))
  "Compound rules inserted into the hash table after the prefix-restore pass.

These rules are NOT asserted into azik-rule/2 to avoid the prefix-restore
step demoting their 2-char prefixes (e.g., \"ka\") back to :incomplete.
Adding them after the restore pass lets the greedy longest-match finder
discover e.g. \"kak\" before \"ka\", enabling compound input like
xhkak → しゅうかく (shuukaku).

The \"wso\" entry restores the mapping after finalize demotes it: adding
foreign hatsuon/double-vowel extensions (wsok, wsoq, etc.) causes the
finalize step to classify \"wso\" as :incomplete (non-vowel extensions).

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

(defvar nskk--azik-vowel-shadow-set (make-hash-table :test 'equal)
  "Set of AZIK rule keys that are vowel-only-shadowed.
A key K is in this set when every longer hash entry prefixed by K extends K
by exactly one vowel character (a/i/u/e/o).  These keys are kept as complete
rules in the hash (not demoted to :incomplete) and instead use the
`azik-vowel-deferred' emit-and-correct mechanism in
`nskk-convert-input-to-kana/k': the AZIK kana is emitted tentatively, and if
the next character is a vowel, the emission is retroactively replaced by the
longer standard-romaji rule.
Rebuilt from scratch on each call to `nskk--azik-finalize-hash-table'.")

(defun/done nskk--azik-init-char-facts ()
  "Assert azik-vowel-char/1 for each Japanese romaji vowel character code.
Must be called at init time after the Prolog DB is fresh for this session.
Character codes: ?a=97, ?i=105, ?u=117, ?e=101, ?o=111."
  (nskk-prolog-retract-all 'azik-vowel-char 1)
  (dolist (ch '(?a ?i ?u ?e ?o))
    (nskk-prolog-assert `((azik-vowel-char ,ch)))))

(defun/done nskk--azik-init-key-extend-facts ()
  "Assert azik-key-extends/2 from the romaji hash for prefix extension analysis.
For every romaji key K of length > 1, asserts (azik-key-extends PREFIX CH)
for each proper prefix PREFIX of K and next character CH at that position.
Deduplicates (PREFIX, CH) pairs before asserting.
Must be called after `nskk--azik-sync-to-romaji-hash'."
  (nskk-prolog-retract-all 'azik-key-extends 2)
  (nskk-prolog-set-index 'azik-key-extends 2 :hash)
  (let ((seen (make-hash-table :test 'equal)))
    (maphash
     (lambda (k _)
       (let ((len (length k)))
         (when (> len 1)
           (dotimes (i (1- len))
             (let* ((pfx  (substring k 0 (1+ i)))
                    (ch   (aref k (1+ i)))
                    (pair (cons pfx ch)))
               (unless (gethash pair seen)
                 (puthash pair t seen)
                 (nskk-prolog-assert `((azik-key-extends ,pfx ,ch)))))))))
     nskk--romaji-table)))

(defun/k nskk--azik-classify-key (key)
  "Classify romaji KEY for prefix-restore using Prolog shadow rules.
Calls (succeed :vowel-shadow) when azik-vowel-shadow/1 holds for KEY:
  all longer hash entries extend KEY by exactly one vowel character.
Calls (succeed :incomplete) when KEY has any extension but is not vowel-shadow.
Calls (fail) when KEY has no longer extensions at all."
  (cond
   ((nskk-prolog-holds-p `(azik-vowel-shadow ,key))
    (succeed :vowel-shadow))
   ((nskk-prolog-holds-p `(azik-key-extends ,key \?ext))
    (succeed :incomplete))
   (t (fail))))

(defun/done nskk--azik-finalize-hash-table ()
  "Register :incomplete prefixes and restore standard-romaji semantics.
Called after `nskk--azik-init-key-extend-facts' populates azik-key-extends/2.

Performs two passes using azik-key-extends/2 facts:
1. Register :incomplete markers for each prefix not yet in the hash.
   Proper prefixes of longer rules must be :incomplete so the converter
   keeps accumulating input.
2. Classify complete hash entries that are prefixes of longer entries
   using `nskk--azik-classify-key/k':
   - :vowel-shadow → record in `nskk--azik-vowel-shadow-set', keep complete.
   - :incomplete   → demote in the hash so longer standard rules
     remain reachable."
  (clrhash nskk--azik-vowel-shadow-set)
  (let ((registered (make-hash-table :test 'equal)))
    (dolist (subst (nskk-prolog-query '(azik-key-extends \?pfx \?ch)))
      (let ((pfx (nskk-prolog-walk '\?pfx subst)))
        (when (and (stringp pfx) (not (gethash pfx registered)))
          (puthash pfx t registered)
          (unless (gethash pfx nskk--romaji-table)
            (nskk-converter-add-rule pfx :incomplete))))))
  (maphash
   (lambda (k v)
     (when (stringp v)
       (nskk--azik-classify-key/k k
         (lambda (kind)
           (pcase kind
             (:vowel-shadow (puthash k t nskk--azik-vowel-shadow-set))
             (:incomplete   (puthash k :incomplete nskk--romaji-table))))
         #'ignore)))
   nskk--romaji-table))

;;;; Main Initialization

(defun/done nskk--init-azik-rules ()
  "Initialize AZIK romaji rules.
Sets up standard romaji as base, then asserts AZIK-specific rules
into the azik-rule/2 Prolog predicate.  A bridge rule connects
azik-rule/2 to romaji-to-kana/2 for unified Prolog queries.
The hash table is populated from azik-rule/2 for hot-path lookups."

  ;; Step 1: Standard romaji base.
  (nskk--initialize-romaji-table)

  ;; Step 2: Set up azik-rule/2 predicate (index before assert).
  (nskk-prolog-retract-all 'azik-rule 2)
  (nskk-prolog-set-index 'azik-rule 2 :hash)

  ;; Step 3: Assert all AZIK rule categories.
  (nskk--azik-assert-rules nskk--azik-special-keys)          ; ; → っ, : → ー
  (nskk--azik-assert-rules nskk--azik-consonant-compat-rules) ; x=しゃ行, c=ちゃ行
  (nskk--azik-init-extension-rows)                            ; hatsuon + double vowel
  (nskk--azik-init-youon-rows)                                ; g-sub + y-prefix youon
  (nskk--azik-assert-rules nskk--azik-same-finger-rules)      ; f-suffix ergonomics
  (nskk--azik-assert-rules nskk--azik-word-shortcuts)         ; common word pairs
  (nskk--azik-assert-rules nskk--azik-foreign-extensions)     ; foreign sound rules
  (nskk--azik-assert-rules nskk--azik-foreign-hatsuon-rules)  ; foreign hatsuon (+ん)
  (nskk--azik-assert-rules nskk--azik-foreign-double-vowel-rules) ; foreign double-vowel

  ;; Step 3b: JP106-specific rules (+ → っ for Shift+; key).
  (when (and (boundp 'nskk-azik-keyboard-type)
             (eq nskk-azik-keyboard-type 'jp106))
    (nskk--azik-assert-rules nskk--azik-jp106-rules))

  ;; Step 4: Bridge rule — AZIK rules are also romaji-to-kana.
  ;; The variable first arg (?r) is NOT trie-indexed; use azik-rule/2
  ;; directly for enumeration.  Hot-path lookups use the hash cache.
  (nskk-prolog-<- (romaji-to-kana \?r \?k) (azik-rule \?r \?k))

  ;; Step 5: Sync azik-rule/2 → hash table.
  (nskk--azik-sync-to-romaji-hash)

  ;; Step 6: Build Prolog classification predicates from the current hash.
  ;;   azik-vowel-char/1   — character codes for a/i/u/e/o (integer facts).
  ;;   azik-key-extends/2  — (PREFIX CH) for every proper prefix in the hash.
  ;;   azik-nonvowel-ext/1 — succeeds when KEY has a non-vowel extension.
  ;;   azik-vowel-shadow/1 — succeeds when KEY's extensions are all vowels.
  (nskk--azik-init-char-facts)
  (nskk--azik-init-key-extend-facts)
  (nskk-prolog-retract-all 'azik-nonvowel-ext 1)
  (nskk-prolog-<- (azik-nonvowel-ext \?k)
    (azik-key-extends \?k \?ch)
    (not (azik-vowel-char \?ch)))
  (nskk-prolog-retract-all 'azik-vowel-shadow 1)
  (nskk-prolog-<- (azik-vowel-shadow \?k)
    (azik-rule \?k \?_kana)
    (azik-key-extends \?k \?_ext)
    (not (azik-nonvowel-ext \?k)))

  ;; Step 7: Register :incomplete prefixes + restore standard-romaji semantics.
  ;; Uses azik-key-extends/2 and azik-vowel-shadow/1 queries via Prolog.
  (nskk--azik-finalize-hash-table)

  ;; Step 8: Compound rules added after finalize.
  ;; Inserting after finalize ensures 2-char prefixes like "ka" remain
  ;; complete, enabling sequences like xhkak → しゅうかく.
  (dolist (rule nskk--azik-compound-rules)
    (puthash (car rule) (cadr rule) nskk--romaji-table))

  ;; Step 9: AZIK toggle key binding.
  (nskk--setup-azik-toggle-key))

;; Register AZIK style
;;;###autoload
(nskk-converter-register-style 'azik 'nskk--init-azik-rules)

(provide 'nskk-azik)

;;; nskk-azik.el ends here
