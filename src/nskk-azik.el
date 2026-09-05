;;; nskk-azik.el --- AZIK extended romaji input support -*- lexical-binding: t; -*-

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

;; AZIK extended romaji input support.

;;; Code:

(require 'nskk-cps-macros)
(require 'nskk-converter)
(require 'nskk-prolog)
;; nskk-mode-map is defined in nskk-keymap.el (L5), loaded before AZIK style init.
(defvar nskk-mode-map)
(declare-function nskk-toggle-japanese-mode "nskk-input")
(declare-function nskk-initialize-romaji-table "nskk-converter")

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
  :safe (lambda (v) (memq v '(jp106 us101)))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-azik)

(defcustom nskk-azik-conversion-table nil
  "Additional AZIK conversion rules in `(ROMAJI KANA)' pair format.

Entries from this table are applied after the built-in AZIK rules during
style initialization, so user-defined entries override conflicting defaults.
Leave this as nil to use the built-in AZIK table only.  Malformed entries are
ignored at runtime."
  :type '(repeat (list string string))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-azik)

(defun nskk--azik-conversion-rule-p (rule)
  "Return non-nil when RULE is a well-formed `(ROMAJI KANA)' pair."
  (and (consp rule)
       (stringp (car rule))
       (consp (cdr rule))
       (stringp (cadr rule))
       (null (cddr rule))))

;;;; Rule Generation Helpers

(defun nskk--azik-hatsuon-pairs (prefix a i u e o)
  "Return AZIK hatsuon (撥音) extension pairs for a consonant row.
PREFIX is the consonant key string.
A/I/U/E/O are the base kana for each vowel position.
Generates: prefix+z→A+ん, prefix+k→I+ん, prefix+j→U+ん,
           prefix+d→E+ん, prefix+l→O+ん."
  (list
   (list (concat prefix "z") (concat a "ん"))
   (list (concat prefix "k") (concat i "ん"))
   (list (concat prefix "j") (concat u "ん"))
   (list (concat prefix "d") (concat e "ん"))
   (list (concat prefix "l") (concat o "ん"))))

(defun nskk--azik-double-vowel-pairs (prefix a u e o &optional dv-p-str)
  "Return AZIK double vowel (二重母音) extension pairs for a consonant row.
PREFIX is the consonant key string.
A/U/E/O are the base kana for each vowel position.
Optional DV-P-STR overrides the p-suffix output string (used for foreign rows
that use ー instead of O+う, e.g. ふぉー for f-row).
Generates: prefix+q→A+い, prefix+h→U+う, prefix+w→E+い, prefix+p→DV-P-STR or O+う."
  (list
   (list (concat prefix "q") (concat a "い"))
   (list (concat prefix "h") (concat u "う"))
   (list (concat prefix "w") (concat e "い"))
   (list (concat prefix "p") (or dv-p-str (concat o "う")))))

(defun nskk--azik-extension-pairs (prefix a i u e o &optional dv-o dv-p-str)
  "Return hatsuon + double vowel extension pairs for a consonant row.
PREFIX is the consonant key string.
A/I/U/E/O are the base kana for each vowel position.
DV-O overrides O for double vowel (e.g., わ行 uses うぉ instead of を).
DV-P-STR overrides the p-suffix output (e.g., foreign rows use ー instead of う)."
  (append (nskk--azik-hatsuon-pairs prefix a i u e o)
          (nskk--azik-double-vowel-pairs prefix a u e (or dv-o o) dv-p-str)))

(defun nskk--azik-youon-pairs (prefix a i u e o)
  "Return AZIK youon (拗音) base rules + all extension pairs for a row.
PREFIX is the key combo (e.g., \"kg\" for きゃ行).
A/I/U/E/O are the base kana.
Base rules generated for a/u/e/o only (no i for youon).
Hatsuon and double vowel extensions are generated for all positions."
  (append
   (list (list (concat prefix "a") a)
         (list (concat prefix "u") u)
         (list (concat prefix "e") e)
         (list (concat prefix "o") o))
   (nskk--azik-extension-pairs prefix a i u e o)))

;;;; Static Rule Data

(defconst nskk--azik-extension-rows
  '(("k" "か" "き" "く" "け" "こ")
    ("s" "さ" "し" "す" "せ" "そ")
    ("t" "た" "ち" "つ" "て" "と")
    ("n" "な" "に" "ぬ" "ね" "の")
    ("h" "は" "ひ" "ふ" "へ" "ほ")
    ("m" "ま" "み" "む" "め" "も")
    ("y" "や" "い" "ゆ" "え" "よ")
    ("r" "ら" "り" "る" "れ" "ろ")
    ("w" "わ" "うぃ" "う" "うぇ" "を" "うぉ" "うぉー")
    ("g" "が" "ぎ" "ぐ" "げ" "ご")
    ("z" "ざ" "じ" "ず" "ぜ" "ぞ")
    ("d" "だ" "ぢ" "づ" "で" "ど")
    ("b" "ば" "び" "ぶ" "べ" "ぼ")
    ("p" "ぱ" "ぴ" "ぷ" "ぺ" "ぽ")
    ("f" "ふぁ" "ふぃ" "ふ" "ふぇ" "ふぉ" nil "ふぉー")
    ("j" "じゃ" "じ" "じゅ" "じぇ" "じょ")
    ("v" "ゔぁ" "ゔぃ" "ゔ" "ゔぇ" "ゔぉ" nil "ゔぉー")
    ("x" "しゃ" "し" "しゅ" "しぇ" "しょ")
    ("c" "ちゃ" "ち" "ちゅ" "ちぇ" "ちょ"))
  "Consonant rows for AZIK hatsuon + double-vowel extension rules.
Each entry is (PREFIX A I U E O) or (PREFIX A I U E O DV-O) or
\(PREFIX A I U E O DV-O DV-P-STR) where DV-O overrides O for the double-vowel
rule, and DV-P-STR overrides the p-suffix output (used for foreign loanword rows
that use ー instead of O+う, e.g., fp→ふぉー instead of ふぉう).

Why some rows carry more than six elements:
- w-row: DV-O=うぉ and DV-P-STR=うぉー, since わ行 diphthongs use うぉ, not を.
- f-row and v-row: foreign-sound rows whose p-suffix is a long vowel
  (fp→ふぉー, vp→ゔぉー) rather than O+う.
- x-row and c-row: sha/shu/sho and cha/chu/cho compatibility.  They take the
  full extension-key set (z/k/j/d/l hatsuon, q/h/w/p diphthong) so compound
  input such as xhka→しゅうか resolves.")

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
    ;; standard romaji y-prefix youon (ny/ky/hy/my/ry/gy/jy/by/py):
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
Each entry is (PREFIX A I U E O) passed to `nskk--azik-youon-pairs'.

Two parallel sets of rows are provided:
- g-substitution (AZIK-specific): ng/kg/hg/mg/rg/gg/jg/bg/pg
- y-prefix youon: ny/ky/hy/my/ry/gy/jy/by/py, adding AZIK extension keys
  (hatsuon z/k/j/d/l, diphthong q/h/w/p) to standard y-prefix sequences.")

(defconst nskk--azik-extension-rule-pairs
  (apply #'append
         (mapcar (lambda (row) (apply #'nskk--azik-extension-pairs row))
                 nskk--azik-extension-rows))
  "Flattened (ROMAJI KANA) pairs for `nskk--azik-extension-rows'.")

(defconst nskk--azik-youon-rule-pairs
  (apply #'append
         (mapcar (lambda (row) (apply #'nskk--azik-youon-pairs row))
                 nskk--azik-youon-rows))
  "Flattened (ROMAJI KANA) pairs for `nskk--azik-youon-rows'.")

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

;;;; Runtime Helpers

(defun/done nskk--azik-sync-to-romaji-hash ()
  "Populate the romaji hash table from azik-rule/2 for hot-path lookups.
Called after all azik-rule/2 facts have been asserted.

`nskk-converter-lookup' (inline) reads from hash only, so we must sync
all azik-rule facts into the hash.  AZIK entries override any conflicting
standard entries (e.g. xa).

We use `puthash' directly into `nskk-romaji-table' rather than
`nskk-converter-add-rule' because the Prolog facts already exist
\(`nskk-converter-add-rule' would double-assert them).  This step is
purely a hash-cache sync from the Prolog truth source."
  (dolist (binding (nskk-prolog-query-bindings '(azik-rule \?r \?k) '(\?r \?k)))
    (let ((romaji (car binding))
          (kana   (cadr binding)))
      (when (and (stringp romaji) (stringp kana))
        (puthash romaji kana (nskk-romaji-table))))))

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

(nskk-converter-register-style-transaction-hash-table
             'nskk--azik-vowel-shadow-set)

(defun nskk-azik-vowel-shadow-set ()
  "Return the current AZIK vowel-shadowed key set hash table."
  nskk--azik-vowel-shadow-set)

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
     (nskk-romaji-table))))

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
          (unless (gethash pfx (nskk-romaji-table))
            (nskk-converter-add-rule pfx :incomplete))))))
  (maphash
   (lambda (k v)
     (when (stringp v)
       (nskk--azik-classify-key/k k
         (lambda (kind)
           (pcase kind
             (:vowel-shadow (puthash k t nskk--azik-vowel-shadow-set))
             (:incomplete   (puthash k :incomplete (nskk-romaji-table)))))
         #'ignore)))
   (nskk-romaji-table)))

;;;; AZIK Toggle Key Setup

;; azik-toggle-key/2: (KEYBOARD-TYPE KEY-STRING)
;; Maps keyboard type symbol to the toggle key string for AZIK mode.
;; Only jp106 and us101 are enumerated; unrecognized types fall back to "@"
;; at the Elisp level (no fact is asserted for them).
(nskk-prolog-define-fact-table azik-toggle-key (:arity 2 :index :hash)
  (jp106 "@")
  (us101 "["))

;; azik-colon-trigger-char/1: (CHAR-CODE)
;; Characters that arm the colon-okurigana pending state in AZIK mode.
;; ?: (colon) is the trigger on US101 keyboards (Shift+;).
;; On JP106, `:' is a bare key producing ー (long vowel) via the romaji
;; table; `nskk--azik-colon-key-p' excludes JP106 so `:' falls through
;; to the normal path.  JP106 uses `+' (Shift+;) for sokuon okurigana
;; via the `plus-jp106' char-type path instead.
(nskk-prolog-define-fact-table azik-colon-trigger-char (:arity 1 :index :hash)
  (?:))

;; azik-plain-vowel-kana/1: (CHAR-CODE)
;; Plain vowel kana characters.  After any of these at preedit end with an
;; empty romaji buffer, AZIK colon-okurigana arming is skipped; instead the
;; romaji table produces ー directly via the normal `colon → ー' rule.
(nskk-prolog-define-fact-table azik-plain-vowel-kana (:arity 1 :index :hash)
  (?あ) (?い) (?う) (?え) (?お)
  (?ア) (?イ) (?ウ) (?エ) (?オ)
  (?ー))

(defvar nskk--azik-toggle-key-state nil
  "Last AZIK toggle key and its displaced binding as (KEY . BINDING).")
(nskk-converter-register-style-transaction-variable
 'nskk--azik-toggle-key-state)

(defun nskk--setup-azik-toggle-key ()
  "Set up AZIK toggle key binding based on keyboard type.
Binds @ for jp106 keyboard or [ for us101 keyboard to
`nskk-toggle-japanese-mode' in `nskk-mode-map'.
Restores the displaced binding when the keyboard type changes."
  (when (boundp 'nskk-mode-map)
    (let* ((key (or (nskk-prolog-query-value
                     `(azik-toggle-key ,nskk-azik-keyboard-type \?k) '\?k)
                    "@"))
           (target #'nskk-toggle-japanese-mode)
           (old-key (car-safe nskk--azik-toggle-key-state))
           (old-binding (cdr-safe nskk--azik-toggle-key-state)))
      (when (and old-key
                 (not (equal old-key key))
                 (eq (lookup-key nskk-mode-map old-key) target))
        (if old-binding
            (keymap-set nskk-mode-map old-key old-binding)
          (keymap-unset nskk-mode-map old-key t)))
      (let ((current (lookup-key nskk-mode-map key)))
        (unless (and (equal old-key key) (eq current target))
          (setq nskk--azik-toggle-key-state (cons key current)))
        (keymap-set nskk-mode-map key target)))))

;;;; Main Initialization

(defconst nskk--azik-special-key-rules
  '((";" "っ")
    (":" "ー"))
  "Special keys: ; → っ (geminate stop), : → ー (prolonged sound).")

(defconst nskk--azik-consonant-compat-rules
  '(("xa" "しゃ") ("xi" "し") ("xu" "しゅ") ("xe" "しぇ") ("xo" "しょ")
    ("ca" "ちゃ") ("ci" "ち") ("cu" "ちゅ") ("ce" "ちぇ") ("co" "ちょ"))
  "Consonant compatibility: x-prefix = しゃ行, c-prefix = ちゃ行.")

(defconst nskk--azik-same-finger-rules
  '(("kf" "き") ("hf" "ふ") ("nf" "ぬ") ("mf" "む") ("gf" "ぐ")
    ("pf" "ぷ") ("rf" "る") ("yf" "ゆ"))
  "Same-finger alternatives (f suffix for ergonomic consonant alternatives).
hf=ふ avoids the h→u same-hand sequence (h and f share the left index
finger); the other entries follow the same rationale.")

(defconst nskk--azik-word-shortcut-rules
  '(("km" "かも") ("kr" "から") ("gr" "がら") ("kt" "こと") ("gt" "ごと")
    ("zr" "ざる") ("st" "した") ("ss" "せい") ("sr" "する") ("tt" "たち") ("dt" "だち")
    ("tb" "たび") ("tm" "ため") ("tr" "たら") ("ds" "です") ("dm" "でも")
    ("nr" "なる") ("nt" "にち") ("nb" "ねば") ("ht" "ひと") ("bt" "びと")
    ("ms" "ます") ("mt" "また") ("mn" "もの") ("yr" "よる")
    ("rr" "られ") ("wt" "わた") ("wr" "われ"))
  "Word shortcuts for common Japanese words and particles.")

(defconst nskk--azik-foreign-word-rules
  '(("tgi" "てぃ") ("tgu" "てゅ") ("dci" "でぃ") ("dcu" "でゅ") ("wso" "うぉ"))
  "Foreign word extensions for non-native Japanese sounds.")

(defconst nskk--azik-foreign-hatsuon-rules
  '(("tgk" "てぃん") ("tgj" "とぅん")
    ("dck" "でぃん") ("dcj" "どぅん")
    ("wsok" "うぉん"))
  "Hatsuon extensions for foreign word prefixes (+ん).
tg/dc: k→i-variant+ん, j→u-variant+ん.  wso: k→うぉん.")

(defconst nskk--azik-foreign-double-vowel-rules
  '(("tgq" "てぃい") ("tgh" "てゅー") ("tgw" "とぅう") ("tgp" "とぅー")
    ("dcq" "でぃい") ("dch" "でゅー") ("dcw" "どぅう") ("dcp" "どぅー")
    ("wsoq" "うぉお") ("wsoh" "うぉお") ("wsow" "うぉお") ("wsop" "うぉお"))
  "Double-vowel extensions for foreign word prefixes.
q→i-variant vowel repeat, w→u-variant vowel repeat.  tg/dc h/p use
ddskk-compatible long-vowel forms.  For wso (single o-variant), all
keys produce うぉお.")

(defconst nskk--azik-n-suffix-hatsuon-rules
  '(("bn" "ばん") ("cn" "ちゃん") ("dn" "だん") ("fn" "ふぁん") ("gn" "がん")
    ("hn" "はん") ("jn" "じゃん") ("kn" "かん") ("pn" "ぱん") ("rn" "らん")
    ("sn" "さん") ("tn" "たん") ("vn" "ゔぁん") ("wn" "わん") ("yn" "やん")
    ("zn" "ざん"))
  "n-suffix hatsuon: Cn → A+ん (ddskk compatible).
Complements the z/k/j/d/l hatsuon suffixes in `nskk--azik-hatsuon-pairs'.")

(defconst nskk--azik-v-suffix-rules
  '(("dv" "でん") ("jv" "じゅう") ("kv" "きん") ("mv" "むん") ("nv" "ぬん")
    ("pv" "ぽう") ("sv" "さい") ("yv" "ゆう") ("zv" "ざい"))
  "v-suffix same-finger alternatives (ddskk compatible).")

(defconst nskk--azik-x-suffix-rules
  '(("bx" "べい") ("cx" "ちぇい") ("zx" "ぜい"))
  "x-suffix same-finger alternatives: ei-vowel shortcuts (ddskk compatible).")

(defconst nskk--azik-additional-shortcut-rules
  '(("br" "ばら") ("cc" "ちゃ") ("cf" "ちぇ") ("cv" "ちゃい") ("df" "で")
    ("dg" "だが") ("dr" "である") ("dy" "でぃ") ("fm" "ふむ") ("fr" "ふる")
    ("fs" "ふぁい") ("jf" "じゅ") ("mr" "まる") ("sf" "さい") ("sm" "しも")
    ("wf" "わい") ("yi" "ゐ") ("zc" "ざ") ("zf" "ぜ"))
  "Additional word shortcuts and consonant alternatives (ddskk compatible).")

(defconst nskk--azik-xx-prefix-rules
  '(("xxa" "ぁ") ("xxi" "ぃ") ("xxu" "ぅ") ("xxe" "ぇ") ("xxo" "ぉ")
    ("xxh" "←") ("xxj" "↓") ("xxk" "↑") ("xxl" "→"))
  "xx-prefix small kana and arrows (ddskk compatible).
Arrows via xx-prefix restore ←↓↑→ since zh/zj/zk/zl are consumed by
z-row hatsuon extensions.")

(defconst nskk--azik-misc-compat-rules
  '(("x;" ";") ("kA" "ヵ") ("kE" "ヶ") ("wA" "ヮ")
    ("kyn" "きゃん") ("y<" "←") ("y>" "→") ("y^" "↑"))
  "Literal semicolon escape, small katakana, youon n-extension,
y-prefix arrow alternatives (ddskk compatible).")

(defconst nskk--azik-core-and-compat-rule-pairs
  (append nskk--azik-special-key-rules
          nskk--azik-consonant-compat-rules
          nskk--azik-same-finger-rules
          nskk--azik-word-shortcut-rules
          nskk--azik-foreign-word-rules
          nskk--azik-foreign-hatsuon-rules
          nskk--azik-foreign-double-vowel-rules
          nskk--azik-n-suffix-hatsuon-rules
          nskk--azik-v-suffix-rules
          nskk--azik-x-suffix-rules
          nskk--azik-additional-shortcut-rules
          nskk--azik-xx-prefix-rules
          nskk--azik-misc-compat-rules)
  "AZIK core, ergonomic, and ddskk-compatibility rule pairs.
Covers everything not already handled by `nskk--azik-extension-rule-pairs'/
`nskk--azik-youon-rule-pairs'.  Concatenates the per-category rule tables
above, preserving the order they were asserted in before they were split
apart.  These 134 keys are currently distinct, so no key's value depends on
that order; were a duplicate ever introduced, the winner would be the one
asserted last, because `nskk--azik-sync-to-romaji-hash' populates the
lookup cache with one `puthash' per fact in assertion order.")

(defun nskk--azik-init-core-and-compat-rules ()
  "Assert `nskk--azik-core-and-compat-rule-pairs' as azik-rule/2 facts.
Also asserts the JP106-specific + → っ rule.  This one stays a runtime
conditional rather than joining the data table because it reads the
`nskk-azik-keyboard-type' defcustom at init time, and tests rebind that
variable dynamically to exercise both keyboard types."
  (nskk-prolog-bulk-facts azik-rule nskk--azik-core-and-compat-rule-pairs)
  (when (and (boundp 'nskk-azik-keyboard-type)
             (eq nskk-azik-keyboard-type 'jp106))
    (nskk-prolog-<- (azik-rule "+" "っ"))))

(defun nskk--azik-reset-rule-database ()
  "Reset the romaji hash table and azik-rule/2 predicate to a fresh state."
  (nskk-initialize-romaji-table)
  (nskk-prolog-retract-all 'azik-rule 2)
  (nskk-prolog-set-index 'azik-rule 2 :hash))

(defun nskk--azik-assert-rule-facts ()
  "Assert all azik-rule/2 facts and bridge them into romaji-to-kana/2.
Prolog is the single source of truth; `nskk--azik-sync-to-romaji-hash'
populates the hash-table read cache from it afterward."
  (nskk-prolog-bulk-facts azik-rule nskk--azik-extension-rule-pairs)
  (nskk-prolog-bulk-facts azik-rule nskk--azik-youon-rule-pairs)
  (nskk--azik-init-core-and-compat-rules)
  ;; The variable first arg (?r) is NOT trie-indexed; use azik-rule/2
  ;; directly for enumeration.  Hot-path lookups use the hash cache.
  (nskk-prolog-<- (romaji-to-kana \?r \?k) (azik-rule \?r \?k))
  (nskk--azik-sync-to-romaji-hash))

(defun nskk--azik-init-derived-facts ()
  "Assert the Prolog facts and rules `nskk--azik-finalize-hash-table' needs.
azik-vowel-char/1 and azik-key-extends/2 are asserted directly;
azik-nonvowel-ext/1 and azik-vowel-shadow/1 are derived rules built on top
of them."
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
    (not (azik-nonvowel-ext \?k))))

(defun nskk--azik-apply-compound-rules ()
  "Insert `nskk--azik-compound-rules' directly into the romaji hash table."
  (dolist (rule nskk--azik-compound-rules)
    (puthash (car rule) (cadr rule) (nskk-romaji-table))))

(defun nskk--azik-apply-user-overrides ()
  "Apply `nskk-azik-conversion-table' overrides on top of the built-ins.
Canonicalizes user rules as azik-rule/2 facts behind the generic bridge,
then mirrors them into the conversion hash for direct lookup."
  (dolist (rule nskk-azik-conversion-table)
    (when (nskk--azik-conversion-rule-p rule)
      (let* ((owned-rule (nskk-prolog-copy-term rule))
             (romaji (car owned-rule))
             (kana (cadr owned-rule)))
        (while (nskk-prolog-retract `(azik-rule ,romaji \?_)))
        (nskk-prolog-assert `((azik-rule ,romaji ,kana)))
        (puthash romaji kana (nskk-romaji-table))))))

(defun/done nskk--init-azik-rules ()
  "Initialize AZIK romaji rules.
Sets up standard romaji as base, then asserts AZIK-specific rules
into the azik-rule/2 Prolog predicate.  A bridge rule connects
azik-rule/2 to romaji-to-kana/2 for unified Prolog queries.
The hash table is populated from azik-rule/2 for hot-path lookups."
  (nskk--azik-reset-rule-database)
  (nskk--azik-assert-rule-facts)
  (nskk--azik-init-derived-facts)
  (nskk--azik-finalize-hash-table)
  ;; Compound rules must be inserted after finalize: finalize would
  ;; otherwise demote their 2-char prefixes (e.g., "ka") to :incomplete,
  ;; blocking sequences like xhkak → しゅうかく.
  (nskk--azik-apply-compound-rules)
  (nskk--azik-apply-user-overrides)
  (nskk--setup-azik-toggle-key))

;; Register AZIK style
(nskk-converter-register-style 'azik 'nskk--init-azik-rules)

(provide 'nskk-azik)

;;; nskk-azik.el ends here
