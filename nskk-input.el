;;; nskk-input.el --- Input processing for NSKK -*- lexical-binding: t; -*-

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

;; Input processing and mode switching for NSKK (Layer 5: Presentation).
;;
;; Layer position: L5 (Presentation) -- depends on nskk-henkan, nskk-kana,
;;   nskk-state, nskk-converter, nskk-prolog; optionally loads nskk-azik.
;;
;; Handles character routing, romaji-to-kana accumulation, mode switching,
;; and AZIK-aware key handlers.  Bridges keymap events and the henkan
;; conversion pipeline (L3).
;;
;; Prolog predicates registered by this module:
;;   input-route/2          -- maps input mode to routing action (katakana-半角 → process-japanese)
;;   toggle-mode/2          -- hiragana<->katakana toggle table (katakana-半角 → hiragana)
;;   q-key-action/3         -- q key dispatch (style, buf-state, action)
;;   semicolon-key-action/2 -- semicolon key dispatch (style, action)
;;   kakutei-action/2       -- C-j dispatch (state, action)
;;   romaji-classify/3      -- romaji input classification (class, doubled-eligible, result-type)
;;
;; Full-width character mapping is handled by `nskk--fullwidth-char-table'
;; (a plain hash table defvar), not a Prolog predicate.
;;
;; Key public API:
;;   `nskk-self-insert'              -- main entry point for character input
;;   `nskk-set-mode-hiragana'        -- switch to hiragana mode
;;   `nskk-set-mode-katakana'        -- switch to katakana mode
;;   `nskk-set-mode-latin'           -- switch to ASCII/latin mode
;;   `nskk-set-mode-abbrev'          -- switch to abbrev mode
;;   `nskk-set-mode-jisx0208-latin'  -- switch to full-width latin mode
;;   `nskk-toggle-japanese-mode'     -- convert preedit script or toggle hiragana<->katakana
;;   `nskk-current-mode'             -- return current mode symbol
;;   `nskk-handle-q-key'             -- q key with AZIK dispatch
;;   `nskk-handle-semicolon-key'     -- semicolon key with AZIK dispatch
;;   `nskk-convert-input-to-kana'    -- accumulate romaji, emit kana
;;   `nskk--maybe-load-azik-style'   -- load AZIK if configured

;;; Code:

(require 'cl-lib)
(require 'nskk-cps-macros)
(require 'nskk-henkan)
(require 'nskk-kana)
(require 'nskk-state)
(require 'nskk-converter)
(require 'nskk-prolog)
(require 'nskk-azik nil t)
(require 'nskk-debug nil t)

(declare-function nskk-state-get-mode "nskk-state")
(declare-function nskk-state-p "nskk-state")
(declare-function nskk-state-mode "nskk-state")
(declare-function nskk-converter-load-style "nskk-converter")
(declare-function nskk-converter-convert "nskk-converter")
(declare-function nskk-kana-string-hiragana-to-katakana "nskk-kana")
(declare-function nskk-kana-zenkaku-to-hankaku "nskk-kana")
;; From nskk-henkan.el:
(declare-function nskk-commit-current "nskk-henkan")
(declare-function nskk--trigger-okuri-conversion "nskk-henkan")
(declare-function nskk--set-conversion-start-marker "nskk-henkan")
(declare-function nskk--insert-marker "nskk-henkan")
(declare-function nskk--conversion-start-active-p "nskk-henkan")
(declare-function nskk-process-okurigana-input "nskk-henkan")
(declare-function nskk-converting-p "nskk-henkan")
(declare-function nskk-henkan-kakutei-convert-script "nskk-henkan")
(declare-function nskk-state-get-metadata "nskk-state")
(declare-function nskk-state-get-okurigana "nskk-state")
(declare-function nskk-state-set-okurigana "nskk-state")
(declare-function nskk--flush-romaji-before-okuri "nskk-henkan")
(declare-function nskk-state-henkan-phase "nskk-state")
(declare-function nskk--clear-conversion-context "nskk-henkan")
(declare-function nskk--show-pending-romaji "nskk-henkan" (text))
(declare-function nskk--clear-pending-romaji "nskk-henkan" ())
(defvar nskk--romaji-buffer)                         ;; Forward declaration from nskk-henkan.el
(defvar nskk-henkan-on-marker)                        ;; Forward declaration from nskk-henkan.el
(defvar nskk--conversion-start-marker)               ;; Forward declaration from nskk-henkan.el
(defvar nskk--conversion-overlay)                    ;; Forward declaration from nskk-henkan.el
(defvar nskk--pending-romaji-overlay)                ;; Forward declaration from nskk-henkan.el
(defvar nskk--henkan-count)                          ;; Forward declaration from nskk-henkan.el
(defvar nskk--henkan-candidate-list-active)          ;; Forward declaration from nskk-henkan.el
(defvar nskk-henkan-select-candidate-by-key-function) ;; Forward declaration from nskk-henkan.el
(defvar nskk-converter-romaji-style)                 ;; Forward declaration from nskk-converter.el
(defvar nskk-azik-q-behavior)                        ;; Forward declaration from nskk-azik.el
(defvar nskk--hatsuon-blockers)                      ;; Forward declaration from nskk-converter.el
(defvar nskk--sokuon-blockers)                       ;; Forward declaration from nskk-converter.el

(defvar-local nskk--sticky-shift-pending nil
  "Non-nil when sticky shift is pending (next char treated as uppercase).")

(defvar nskk--azik-vowel-shadow-set)    ;; Forward declaration from nskk-azik.el

(defvar-local nskk--deferred-azik-state nil
  "Non-nil when an AZIK two-char rule was tentatively emitted (sokuon case).
Value is a cons (CONSONANT-CHAR . KANA-STRING).  CONSONANT-CHAR is the
doubled character (e.g. ?k) and KANA-STRING is the tentatively emitted
kana (e.g. \"きん\").  On the next input:
  - Vowel: delete tentative kana, insert っ, reset romaji buffer to
    CONSONANT-CHAR, then process consonant+vowel normally (sokuon).
  - Non-vowel: clear deferred state without retroactive correction.
Satisfies kk→きん (AZIK hatsuon) and kka→っか (standard sokuon).
See also `nskk--deferred-vowel-shadow-state' for the vowel-shadow variant.")

(defvar-local nskk--deferred-vowel-shadow-state nil
  "Non-nil when a vowel-shadowed AZIK rule was tentatively emitted.
Value is a cons (ROMAJI-STRING . KANA-STRING).  ROMAJI-STRING is the romaji
prefix (e.g. \"sh\") and KANA-STRING is the tentatively emitted kana
(e.g. \"すう\").  On the next input:
  - Vowel: delete tentative kana, reset romaji buffer to ROMAJI-STRING, then
    process ROMAJI-STRING+vowel as the longer standard rule
    (e.g. \"sha\"→\"しゃ\").
    Unlike `nskk--deferred-azik-state', NO っ is inserted.
  - Non-vowel: clear deferred state without retroactive correction.
Satisfies Sh→すう (AZIK double-vowel) while preserving sha→しゃ (standard romaji).")

(defvar-local nskk--azik-colon-okuri-pending nil
  "Non-nil when the AZIK colon-okurigana trigger (`:') has been armed.
When non-nil, `*' has been inserted in the buffer and the next alphabetic
consonant will become the okurigana consonant, emitting っ and firing the
dictionary lookup at the position right after `*'.
Cleared when a consonant fires `nskk--fire-azik-colon-okuri'.
See also `nskk--azik-colon-okuri-deferred'.")

(defvar-local nskk--azik-colon-okuri-deferred nil
  "Non-nil when an AZIK colon-okurigana consonant was tentatively emitted.
Value is a cons (CONSONANT-CHAR . PLACEHOLDER-STRING).  CONSONANT-CHAR is
the okurigana consonant (e.g. ?t) and PLACEHOLDER-STRING is the tentative
ASCII string inserted as a placeholder (e.g. \"t\") to capture preedit-end
right after `*' for the dict query.  On the next input:
  - Vowel: delete placeholder, insert っ, reset romaji buffer to CONSONANT-CHAR,
    then process consonant+vowel normally (completing the okurigana syllable).
  - Non-vowel: clear deferred state without correction.
Produces the correct okurigana kana (e.g. って) after the dict lookup has
already fired with the consonant-only query (e.g. \"つかt\").
See `nskk--apply-colon-okuri-correction'.")

(defvar-local nskk--numeric-mode nil
  "Non-nil when in numeric input mode for SKK numeric conversion.")

;;;; Mode Setter Macro

(defmacro nskk-define-mode-setter (mode)
  "Define an interactive mode setter function for MODE.
Creates `nskk-set-mode-MODE' that switches to MODE and updates modeline."
  (declare (indent 1) (debug t))
  (let ((fn-name (intern (format "nskk-set-mode-%s" mode))))
    `(defun ,fn-name ()
       ,(format "Switch to %s mode." mode)
       (interactive)
       (nskk--set-mode ',mode)
       ;; nskk-modeline is an optional display feature; guard avoids
       ;; hard dependency when running without the modeline module.
       (when (fboundp 'nskk-modeline-update)
         (nskk-modeline-update)))))

;;;; Mode Switching

;;;###autoload
(nskk-define-mode-setter hiragana)
;;;###autoload
(nskk-define-mode-setter katakana)
;;;###autoload
(nskk-define-mode-setter latin)
;;;###autoload
(nskk-define-mode-setter jisx0208-latin)

(defun/done nskk--activate-preedit-mode ()
  "Set up preedit state after switching to abbrev-style mode.
Places `nskk--conversion-start-marker' at point, inserts the ▽ marker,
activates henkan phase to `on', and refreshes the modeline.
Called by `nskk-set-mode-abbrev' and `nskk-set-mode-numeric' after
`nskk--set-mode' has already switched the internal mode."
  (nskk--set-conversion-start-marker (point))
  (nskk--insert-marker nskk-henkan-on-marker)
  (nskk-with-current-state
    (nskk-state-set-henkan-phase nskk-current-state 'on))
  (when (fboundp 'nskk-modeline-update)
    (nskk-modeline-update)))

;;;###autoload
(defun/done nskk-set-mode-abbrev ()
  "Switch to abbrev mode and set up ▽ preedit marker for dictionary lookup.
Sets up the conversion start marker and inserts ▽ after the mode switch
so that `nskk--has-preedit' detects preedit state and Space triggers
`nskk-start-conversion' with the accumulated ASCII text as the lookup key.
This is DDSKK-compatible: /word SPC → dictionary lookup → candidate."
  :interactive t
  (nskk--set-mode 'abbrev)
  (nskk--activate-preedit-mode))

;;;###autoload
(defun/done nskk-set-mode-numeric ()
  "Switch to numeric input mode for SKK numeric conversion.
Reuses abbrev mode mechanics (literal character input) with an additional
`nskk--numeric-mode' flag that triggers candidate post-processing in
`nskk-start-conversion'.  Inserts \\=# as the first preedit character.
Dictionary keys that begin with # trigger numeric candidate expansion."
  :interactive t
  (nskk--set-mode 'abbrev)
  (nskk--activate-preedit-mode)
  (setq nskk--numeric-mode t)
  (insert "#"))

;;;###autoload
(defun/done nskk-toggle-japanese-mode ()
  "Convert preedit kana to opposite script, or toggle hiragana<->katakana.
In ▽ preedit phase (henkan-phase `on'): delegates to
`nskk-henkan-kakutei-convert-script' which queries `script-toggle/2'
(Prolog) for the target script, converts and commits the preedit text,
and clears conversion state without changing the input mode (DDSKK-compatible).
In idle state: queries `toggle-mode/2' (Prolog) for the target mode and
switches via `nskk--set-mode' (hiragana↔katakana toggle).
Falls through to `self-insert-command' when no toggle-mode fact exists for
the current mode (e.g. ascii, latin), so that the AZIK toggle key (@/[)
self-inserts in non-Japanese modes."
  :interactive t
  (if (nskk-with-current-state
        (eq (nskk-state-henkan-phase nskk-current-state) 'on))
      (nskk-henkan-kakutei-convert-script)
    (let* ((current-mode (when (boundp 'nskk-current-state)
                           (nskk-state-mode nskk-current-state)))
           (target (nskk-prolog-query-value
                    `(toggle-mode ,current-mode ,'\?target) '\?target)))
      (if target
          (progn
            (nskk-debug-log "[INPUT] toggle-mode: from=%s to=%s" current-mode target)
            (nskk--set-mode target)
            (when (fboundp 'nskk-modeline-update)
              (nskk-modeline-update)))
        (self-insert-command 1)))))

(defun/done nskk--set-mode (mode)
  "Internal mode setter with validation.
MODE is the target mode symbol.
Signals user-error if NSKK state is not initialized."
  (unless (and (boundp 'nskk-current-state) (nskk-state-p nskk-current-state))
    (user-error "NSKK state not initialized"))
  (nskk-debug-log "[INPUT] set-mode: mode=%s" mode)
  (nskk-state-set nskk-current-state 'mode mode)
  (nskk--clear-conversion-context))


(defun/k nskk-current-mode ()
  "Return the current NSKK input mode symbol.
Returns a mode symbol such as `hiragana', `katakana', `ascii',
`latin', `abbrev', or `jisx0208-latin', or nil if no state is active."
  (let ((m (nskk-state-get-mode)))
    (if m (succeed m) (fail))))

;;;; AZIK Style Initialization

(defun/done nskk--maybe-load-azik-style ()
  "Load AZIK romaji style when both conditions hold:
1. The `nskk-azik' feature is loaded (checked with `featurep').
2. `nskk-converter-romaji-style' is the symbol `azik'.
Side effect: calls `nskk-converter-load-style' with `azik'."
  (when (and (featurep 'nskk-azik)
             (eq nskk-converter-romaji-style 'azik))
    (nskk-converter-load-style 'azik)))

;;;; AZIK-specific Key Handlers

;;;###autoload
(defun/done nskk-handle-q-key ()
  "Handle q key press based on current romaji style.
In AZIK mode:
  - If pending-romaji+q is a complete AZIK hash match (e.g. kq→かい),
    fire the AZIK rule via `nskk-process-japanese-input'.
  - Otherwise, produce \u3093 (katakana mode: \u30f3)
In standard mode: toggle mode (default SKK behavior)."
  :interactive t
  (let* ((style (if (eq nskk-converter-romaji-style 'azik) 'azik 'standard))
         (combined (concat nskk--romaji-buffer "q"))
         (buf-state (cond
                     ((and (eq style 'azik)
                           (stringp (gethash combined nskk--romaji-table)))
                      'azik-complete)
                     ((string-empty-p nskk--romaji-buffer) 'empty)
                     (t 'pending)))
         (action (nskk-prolog-query-value
                  `(q-key-action ,style ,buf-state ,'\?action)
                  '\?action)))
    (nskk-debug-log "[INPUT] q-key: style=%s buf-state=%s action=%s" style buf-state action)
    (pcase action
      ('toggle-mode  (nskk-toggle-japanese-mode))
      ('insert-n     (insert (if (memq (nskk-state-mode nskk-current-state)
                                       '(katakana katakana-半角))
                                 "\u30f3" "\u3093")))
      ('fire-romaji  (nskk-process-japanese-input ?q 1))
      (_             nil))))

(defun nskk--sticky-shift-dispatch ()
  "Execute the sticky-shift sub-dispatch for semicolon in standard mode.
Three arms in priority order:
  1. Already pending: double-semicolon cancels sticky shift and inserts \";\".
  2. Japanese mode active: arm the sticky-shift pending flag.
  3. Non-Japanese mode: fall through to self-insert.
Returns non-nil when the sticky-shift action was consumed (arms 1 or 2),
nil when falling through to self-insert (arm 3)."
  (cond
   ;; Arm 1 — double semicolon: cancel sticky shift, insert literal ";"
   (nskk--sticky-shift-pending
    (setq nskk--sticky-shift-pending nil)
    (insert ";")
    t)
   ;; Arm 2 — Japanese mode: arm the sticky-shift pending flag
   ((and nskk-current-state
         (nskk-prolog-holds-p
          `(japanese-mode ,(nskk-state-mode nskk-current-state))))
    (setq nskk--sticky-shift-pending t)
    t)
   ;; Arm 3 — non-Japanese mode: caller should fall through to self-insert
   (t nil)))

;;;###autoload
(defun/k nskk-handle-semicolon-key ()
  "Handle semicolon key press.
In AZIK mode: produce small tsu (\u3063).
In standard mode + Japanese mode: sticky shift (next char is uppercase).
Double semicolon in sticky-shift state: cancel sticky, insert literal \";\".
In standard mode + non-Japanese mode: self-insert (on-not-found path).

`on-found' is called (with t) when the key was consumed as a Japanese action:
  AZIK small-tsu insertion, sticky-shift armed, or sticky-shift cancelled.
`on-not-found' is called when key is not consumed (self-insert path)."
  :interactive t
  ;; Outer arm 1 — AZIK style: insert small tsu via romaji processor
  ;; Outer arm 2 — standard style: delegate to sticky-shift sub-dispatch
  ;;               (on-found when consumed, on-not-found when non-Japanese)
  ;; Outer arm 3 — self-insert mode (e.g. abbrev/latin): pass through
  (let* ((style (if (eq nskk-converter-romaji-style 'azik) 'azik 'standard))
         (action (nskk-prolog-query-value
                  `(semicolon-key-action ,style ,'\?action) '\?action)))
    (nskk-debug-log "[INPUT] semicolon-key: style=%s action=%s" style action)
    (pcase action
      ;; Outer arm 1: AZIK — fire the romaji rule for ";" → っ
      ('insert-small-tsu
       (nskk-process-japanese-input ?\; 1)
       (succeed t))
      ;; Outer arm 2: standard sticky-shift sub-dispatch
      ('sticky-shift
       (if (nskk--sticky-shift-dispatch)
           (succeed t)
         (fail)))
      ;; Outer arm 3: explicit self-insert (latin/abbrev style routes here)
      ('self-insert
       (nskk-self-insert 1)
       (fail))
      (_ (fail)))))

;;;; Input Processing

(defun nskk--implicit-kakutei-needed-p ()
  "Return non-nil when implicit kakutei should fire before inserting a new char.
True when converting (▼ active), the candidate list is not showing, and
okurigana is not currently being built."
  (and (nskk-converting-p)
       (not nskk--henkan-candidate-list-active)
       (not (and (eq (nskk-state-henkan-phase nskk-current-state) 'active)
                 (nskk-state-get-metadata nskk-current-state
                                          'okurigana-in-progress)))))

(defun nskk--route-input (char n mode)
  "Route CHAR (with repeat N) based on current input MODE.
In abbrev mode: insert CHAR directly via `nskk-process-abbrev-input'.
Otherwise: query `input-route/2' and dispatch to the appropriate handler."
  (nskk-debug-log "[INPUT] route: mode=%s" mode)
  (if (eq mode 'abbrev)
      (progn
        (nskk-debug-log "[INPUT] abbrev-input: char=%c" char)
        (nskk-process-abbrev-input char))
    (pcase (nskk-prolog-query-value `(input-route ,mode ,'\?action) '\?action)
      ('insert-direct    (nskk-insert-char char n))
      ('insert-fullwidth (nskk-insert-fullwidth-char char n))
      (_                 (nskk-process-japanese-input char n)))))

;;;###autoload
(defun/done nskk-self-insert (n)
  "Process self-insert input, routing the typed character based on current mode.
N is the prefix repeat count from `last-command-event'.

Three-stage pipeline:
  1. Candidate selection -- `nskk--try-candidate-selection/k' short-circuits
     when CHAR matches a candidate key; on-found = #\\='ignore (consumed).
  2. Implicit kakutei -- DDSKK-compatible commit before a new character.
  3. Mode routing -- `nskk--route-input' dispatches to abbrev, direct, or
     Japanese input based on `input-route/2' Prolog query."
  :interactive "p"
  (let ((char (if (integerp last-command-event)
                  last-command-event
                (aref last-command-event 0)))
        (mode (or (nskk-state-get-mode) 'ascii)))
    (nskk-debug-log "[INPUT] self-insert: char=%c mode=%s" char mode)
    (nskk--try-candidate-selection/k char
      #'ignore
      (lambda ()
        (when (nskk--implicit-kakutei-needed-p)
          (nskk-debug-log "[INPUT] implicit-kakutei: char=%c" char)
          (nskk-commit-current))
        (nskk--route-input char n mode)))))

(defun/done nskk-insert-char (char &optional n)
  "Insert CHAR into the buffer N times without any kana conversion."
  (let ((n (or n 1)))
    (insert (make-string n char))))

(defvar nskk--fullwidth-char-table
  (let ((h (make-hash-table :test 'eq :size 96)))
    (puthash ?\s ?\u3000 h)
    (cl-loop for c from ?! to ?~
             do (puthash c (+ c #xFEE0) h))
    h)
  "Hash table mapping ASCII characters to JIS X 0208 full-width equivalents.
Space maps to ideographic space U+3000; printable ASCII (! through ~) maps
to the full-width range FF01-FF5E via offset +#xFEE0.")

(defun/done nskk-insert-fullwidth-char (char &optional n)
  "Insert full-width version of ASCII CHAR N times.
Converts ASCII characters (SPC and !-~) to their JIS X 0208 full-width
equivalents using `nskk--fullwidth-char-table'.
Space (SPC) maps to ideographic space (U+3000); printable ASCII
characters (! through ~) map to FF01-FF5E via offset +#xFEE0."
  (let ((n (or n 1))
        (fw-char (or (gethash char nskk--fullwidth-char-table)
                     char)))             ; non-ASCII: pass through
    (insert (make-string n fw-char))))

(defun/k nskk--try-candidate-selection (char)
  "Try to select a candidate using CHAR as a selection key.
Calls on-found with t if the candidate list is active, CHAR was a valid
selection key, and the candidate was committed.
Calls on-not-found when the list is not active or CHAR did not match."
  (cond
   ((not nskk--henkan-candidate-list-active)
    (fail))
   (t
    (nskk-debug-log "[INPUT] candidate-selection: char=%c" char)
    (let ((index (when nskk-henkan-select-candidate-by-key-function
                   (funcall nskk-henkan-select-candidate-by-key-function
                            char
                            (nskk-state-candidates nskk-current-state)
                            (nskk-state-current-index nskk-current-state)))))
      (if index
          (progn
            (setf (nskk-state-current-index nskk-current-state) index)
            (nskk-commit-current)
            (succeed t))
        (fail))))))

;;;; Japanese Input Processing

(defun/done nskk--setup-henkan-start-marker (char)
  "Set up conversion start marker for CHAR as a henkan start.
Inserts the ▽ marker, sets conversion start position at point,
and marks henkan phase to `on'.
Called when an uppercase letter triggers auto-henkan-start."
  (nskk-debug-log "[INPUT] henkan-start: char=%c" char)
  (nskk--set-conversion-start-marker (point))
  (nskk--insert-marker nskk-henkan-on-marker)
  (nskk-with-current-state
    (nskk-state-set-henkan-phase nskk-current-state 'on)))

(defun/done nskk--emit-converted-kana (converted n)
  "Insert CONVERTED kana N times, handling okurigana if active, then call on-done.
When the current state has okurigana set, triggers okurigana conversion
after inserting CONVERTED and clears the okurigana state.  Otherwise,
inserts CONVERTED directly.  The on-done continuation is called with no
arguments after all insertions and side effects complete."
  (let ((okuri (nskk-with-current-state
                    (nskk-state-get-okurigana nskk-current-state))))
    (if okuri
        (let ((preedit-end (point)))
          (dotimes (_ n) (insert converted))
          (nskk-debug-log "[INPUT] okuri-conversion: okuri=%s kana=%s" okuri converted)
          (nskk--trigger-okuri-conversion okuri preedit-end)
          (nskk-state-set-okurigana nskk-current-state nil))
      (dotimes (_ n) (insert converted)))))

(defun/done nskk--process-kana-result (kana n)
  "Convert KANA to mode-specific script, emit N times, then call on-done.
KANA is a hiragana string from the romaji converter (may be empty string).
When KANA is empty, clears pending romaji display and calls on-done with no
insertions.  Applies mode-specific conversion before inserting:
  - `katakana': `nskk-kana-string-hiragana-to-katakana'
  - `katakana-半角': `nskk-kana-zenkaku-to-hankaku' (after katakana)
  - otherwise: kana is used as-is (hiragana)
The on-done continuation is called with no arguments after all insertions
and side effects."
  (let* ((mode (nskk-state-get-mode))
         (converted (cond
                     ((string-empty-p kana) nil)
                     ((eq mode 'katakana)
                      (nskk-kana-string-hiragana-to-katakana kana))
                     ((eq mode 'katakana-半角)
                      (nskk-kana-zenkaku-to-hankaku
                       (nskk-kana-string-hiragana-to-katakana kana)))
                     (t kana))))
    (nskk--clear-pending-romaji)
    (when converted
      (nskk-debug-log "[INPUT] kana-emitted: kana=%s mode=%s" converted mode)
      (nskk--emit-converted-kana converted n))
    (unless (string-empty-p nskk--romaji-buffer)
      (nskk--show-pending-romaji nskk--romaji-buffer))))

(defun/k nskk--compute-effective-char (char)
  "Compute effective character and classification flags for CHAR in Japanese input.
Returns a list (EFFECTIVE-CHAR IS-HENKAN-START NORMALIZE-VOWEL-P) via on-found.
Always calls on-found; never calls on-not-found.

Classification uses `effective-char-class/5' Prolog query on four pre-computed
boolean context symbols.  See `nskk--init-effective-char-rules' for the full
dispatch table.

UPPER-READY:  char is A-Z AND `nskk-converter-auto-start-henkan' is non-nil.
CONV-ACTIVE:  `nskk--conversion-start-active-p' is non-nil.
BUF-NONEMPTY: `nskk--romaji-buffer' is non-empty.
VOWEL-OR-CTX: char is uppercase vowel (A/I/U/E/O), OR no preedit yet
              (`nskk--has-preedit' nil), OR okurigana is pending in state.

Results:
  `henkan-start'    -- IS-HENKAN-START=t; effective-char=(downcase char)
  `normalize-vowel' -- NORMALIZE-VOWEL-P=t; effective-char=(downcase char)
  `normal'          -- both flags nil; effective-char=char unchanged"
  (let* ((upper-ready  (and (characterp char) (<= ?A char) (<= char ?Z)
                            nskk-converter-auto-start-henkan))
         (conv-active  (nskk--conversion-start-active-p))
         (buf-nonempty (not (string-empty-p nskk--romaji-buffer)))
         (vowel-or-ctx (or (memq char '(?A ?I ?U ?E ?O))
                           (not (nskk--has-preedit))
                           (nskk-with-current-state
                             (nskk-state-get-okurigana nskk-current-state))))
         (class (nskk-prolog-query-value
                 `(effective-char-class ,'\?class
                                        ,(if upper-ready  'yes 'no)
                                        ,(if conv-active  'yes 'no)
                                        ,(if buf-nonempty 'yes 'no)
                                        ,(if vowel-or-ctx 'yes 'no))
                 '\?class)))
    (pcase class
      ('henkan-start    (succeed (list (downcase char) t   nil)))
      ('normalize-vowel (succeed (list (downcase char) nil t)))
      (_                (succeed (list char             nil nil))))))

(defun/done nskk--fire-azik-colon-okuri (char)
  "Handle AZIK colon-okurigana fire: pending consonant CHAR arrives.
Clears `nskk--azik-colon-okuri-pending', sets CHAR as okurigana in state,
emits CHAR as a placeholder kana string to capture preedit-end, arms
`nskk--azik-colon-okuri-deferred' for retroactive っ correction on the
next vowel, and shows the pending romaji display.
Called from `nskk-process-japanese-input' when context=colon-pending
and char-type=alphabetic-lower."
  (setq nskk--azik-colon-okuri-pending nil)
  (let ((placeholder (char-to-string char)))
    (nskk-with-current-state
      (nskk-state-set-okurigana nskk-current-state char))
    (nskk--emit-converted-kana placeholder 1)
    (setq nskk--azik-colon-okuri-deferred (cons char placeholder))
    (setq nskk--romaji-buffer (char-to-string char))
    (nskk--show-pending-romaji nskk--romaji-buffer)))

(defun/done nskk--arm-azik-colon-trigger ()
  "Handle AZIK colon-okurigana arm: `:' key in eligible preedit context.
Flushes the current romaji buffer before the okurigana boundary, inserts
the `*' okurigana marker, and sets `nskk--azik-colon-okuri-pending' to
await the next consonant.
Called from `nskk-process-japanese-input' when context=azik-arm-eligible
and char-type=colon."
  (nskk-with-current-state
    (nskk--flush-romaji-before-okuri)
    (nskk--insert-marker nskk-okurigana-marker))
  (setq nskk--azik-colon-okuri-pending t))

(defun/done nskk--process-normal-japanese-input (char n)
  "Handle normal Japanese input for CHAR with repeat count N.
Computes effective char and flags via `nskk--compute-effective-char',
sets up the henkan-start marker when appropriate, then either routes
through okurigana processing or delegates to the romaji-to-kana converter.
Called from `nskk-process-japanese-input' for the `normal' action class."
  (cl-destructuring-bind (effective-char is-henkan-start normalize-vowel-p)
      (nskk--compute-effective-char char)
    (when is-henkan-start
      (nskk--setup-henkan-start-marker char))
    (if (and (not is-henkan-start)
             (not normalize-vowel-p)
             (nskk-process-okurigana-input char))
        (nskk-debug-log "[INPUT] okurigana-processed: char=%c" char)
      ;; When okurigana is pending but re-entry was blocked (e.g. second N
      ;; in YoNN), downcase uppercase chars so the romaji converter sees
      ;; "nn" instead of "nN".
      (let ((eff (if (and (not normalize-vowel-p)
                          (characterp effective-char)
                          (<= ?A effective-char) (<= effective-char ?Z)
                          (nskk-with-current-state
                            (nskk-state-get-okurigana nskk-current-state)))
                     (downcase effective-char)
                   effective-char)))
        (nskk--process-kana-result
         (or (nskk-convert-input-to-kana eff) "") n)))))

(defun/done nskk-process-japanese-input (char n)
  "Process input in Japanese mode (hiragana/katakana), then call on-done.
CHAR is the input character.  N is the repeat count.

Sticky shift: if `nskk--sticky-shift-pending' is set, treats CHAR as
uppercase (a-z → A-Z) before dispatch.

Dispatch is driven by a `japanese-input-class/3' Prolog query on two
pre-computed classification symbols:

CONTEXT -- input context from runtime state:
  `colon-pending'      -- `nskk--azik-colon-okuri-pending' is set
  `azik-arm-eligible'  -- AZIK style + conversion active + preedit +
                          no okurigana
  `other'              -- everything else

CHAR-TYPE -- character classification:
  `alphabetic-lower'   -- ASCII a-z
  `colon'              -- the `:' character
  `other'              -- anything else

Actions:
  fire   → `nskk--fire-azik-colon-okuri'
  arm    → `nskk--arm-azik-colon-trigger'
  normal → `nskk--process-normal-japanese-input'"
  (when nskk--sticky-shift-pending
    (setq nskk--sticky-shift-pending nil)
    (when (and (characterp char) (<= ?a char) (<= char ?z))
      (setq char (upcase char))))
  (let* ((char-type (cond
                     ((and (characterp char) (<= ?a char) (<= char ?z)) 'alphabetic-lower)
                     ((eq char ?:)                                       'colon)
                     (t                                                  'other)))
         (input-context (cond
                         (nskk--azik-colon-okuri-pending
                          'colon-pending)
                         ((and (eq nskk-converter-romaji-style 'azik)
                               (nskk--conversion-start-active-p)
                               (nskk--has-preedit)
                               (not (nskk-with-current-state
                                      (nskk-state-get-okurigana nskk-current-state))))
                          'azik-arm-eligible)
                         (t 'other)))
         (action (nskk-prolog-query-value
                  `(japanese-input-class ,'\?action ,input-context ,char-type)
                  '\?action)))
    (nskk-debug-log "[INPUT] japanese-input: char=%c ctx=%s char-type=%s action=%s"
                    char input-context char-type action)
    (pcase action
      ('fire   (nskk--fire-azik-colon-okuri char))
      ('arm    (nskk--arm-azik-colon-trigger))
      ('normal (nskk--process-normal-japanese-input char n)))))

;;;; Abbrev Input Processing

(defun/done nskk-process-abbrev-input (char)
  "Process input CHAR in abbrev mode.
CHAR is inserted directly after the ▽ preedit marker into the buffer.
Dictionary lookup is triggered by `nskk-start-conversion' when Space
is pressed: it extracts text between ▽ and point as the dictionary key.
CHAR bypasses the romaji buffer — ASCII input is used verbatim as the
lookup key, consistent with DDSKK abbrev mode behavior."
  (insert char))

;;;; Romaji-to-Kana Conversion

(defun/k nskk--emit-hatsuon-prefix (new-buffer-value)
  "Emit ん for the trailing `n' in `nskk--romaji-buffer', then call on-found.

Reads `nskk--romaji-buffer' to compute any kana prefix before the trailing `n',
then writes `nskk--romaji-buffer' ← NEW-BUFFER-VALUE as a side effect.
Calls on-found with the prefix kana (possibly empty) concatenated with ん.
Always succeeds.

Precondition: `nskk--romaji-buffer' must be non-empty and its last character
must be `n'.  This invariant is guaranteed by callers in
`nskk-convert-input-to-kana/k', which check the buffer state before dispatching.
NEW-BUFFER-VALUE is the string to leave in the buffer after emission
\(e.g., \"n\" for the n+n case, or (char-to-string char) for n+consonant)."
  (let* ((prefix-without-n
          (substring nskk--romaji-buffer 0 (1- (length nskk--romaji-buffer))))
         (prefix-kana
          (if (> (length prefix-without-n) 0)
              (let ((prev (nskk-converter-convert prefix-without-n)))
                (if (and prev (stringp (car prev))) (car prev) ""))
            "")))
    (setq nskk--romaji-buffer new-buffer-value)
    (succeed (concat prefix-kana "\u3093"))))

(defun nskk--romaji-result-type (result)
  "Pre-classify RESULT from `nskk-converter-convert' into a symbol.
Returns `match' when RESULT is a cons with a string car,
`incomplete' when RESULT is a cons with :incomplete car,
or `no-result' otherwise."
  (cond
   ((and (consp result) (stringp (car result))) 'match)
   ((and (consp result) (eq (car result) :incomplete)) 'incomplete)
   (t 'no-result)))

(defvar nskk--romaji-classify-cache
  (make-hash-table :test 'equal :size 16)
  "Memoization cache for `nskk--classify-romaji-input'.
Keys are (DOUBLED-ELIGIBLE . RESULT-TYPE) cons cells; values are the
classification symbol returned by the romaji-classify/3 Prolog query.
At most 15 entries (5 doubled-eligible × 3 result-type combinations).
Cleared whenever `nskk--init-romaji-classify-rules' reasserts the table.")

(defun/done nskk--init-romaji-classify-rules ()
  "Assert romaji-classify/3 facts for romaji input classification.
Facts encode the priority-ordered dispatch table for
`nskk--classify-romaji-input'.  The 3 arguments are:

  CLASS            -- the classification symbol to return; one of:
                       `nn-double'      (n+n → emit ん; DDSKK-compatible)
                       `azik-deferred'  (doubled consonant, AZIK match, emit
                                         kana tentatively for sokuon correction)
                       `sokuon'         (doubled consonant, no AZIK match →
                                         emit っ)
                       `n-consonant'    (n+consonant → emit ん, keep consonant)
                       `match'          (converter produced a kana string)
                       `incomplete'     (converter needs more input)
                       `no-match'       (no rule; pass through as-is)

  DOUBLED-ELIGIBLE -- pre-computed doubling context symbol:
                       `nn'             (last=n AND char=n)
                       `eligible-match' (same doubled, not blocked,
                                         result=match)
                       `eligible-other' (same doubled, not blocked,
                                         result≠match)
                       `n-consonant'    (last=n, char not a hatsuon-blocker)
                       `not-eligible'   (all other cases)

  RESULT-TYPE      -- pre-computed result symbol:
                       `match', `incomplete', `no-result'

Facts are asserted in priority order (nn-double > azik-deferred > sokuon >
n-consonant > match > incomplete > no-match).  Also clears
`nskk--romaji-classify-cache' so stale memoized lookups are evicted."
  (clrhash nskk--romaji-classify-cache)
  (nskk-prolog-define-fact-table romaji-classify (:arity 3 :index :list)
    ;; nn-double: highest priority — n+n sequence
    (nn-double      nn             \?result-type)
    ;; azik-deferred: doubled eligible consonant with a complete match
    (azik-deferred  eligible-match \?result-type)
    ;; sokuon: doubled eligible consonant, no complete match
    (sokuon         eligible-other \?result-type)
    ;; n-consonant: last=n and char is not a hatsuon-blocker (any result-type).
    ;; Must precede the generic ?doubled rows so the concrete n-consonant value
    ;; is matched before the ?doubled wildcard fires.
    (n-consonant    n-consonant    \?result-type)
    ;; match: converter returned a kana string
    (match          \?doubled      match)
    ;; incomplete: converter returned :incomplete
    (incomplete     \?doubled      incomplete)
    ;; no-match: fallback
    (no-match       \?doubled      no-result)))

(defun nskk--classify-romaji-input (char last-buf-char result)
  "Classify romaji input into a dispatch state symbol.
CHAR is the new input character (integer).
LAST-BUF-CHAR is the last character in `nskk--romaji-buffer', or nil if empty.
RESULT is the return value of `nskk-converter-convert' on the full input.

Returns one of: `nn-double', `azik-deferred', `match', `n-consonant',
`sokuon', `incomplete', or `no-match'.

Two-stage Prolog dispatch:
  1. Query `doubled-context/6' with five pre-computed boolean symbols to
     obtain the `doubled-eligible' classification (replaces the 5-arm cond).
  2. Query `romaji-classify/3' with doubled-eligible and result-type for the
     final class (memoized in `nskk--romaji-classify-cache').

Pre-computed boolean inputs for doubled-context/6:
  LAST-IS-N   -- last buffer char is `n'
  CHAR-IS-N   -- current char is `n'
  SAME-OK     -- same char doubled AND not in sokuon-blockers
  N-OK        -- char not in hatsuon-blockers AND
                 direct n+char lookup not :incomplete"
  (let* ((result-type      (nskk--romaji-result-type result))
         (last-is-n        (eql last-buf-char ?n))
         (char-is-n        (eql char ?n))
         (same-ok          (and (eql last-buf-char char)
                                (not (memq char nskk--sokuon-blockers))))
         (n-ok             (and (not (memq char nskk--hatsuon-blockers))
                                (not (eq (nskk-converter-lookup (string ?n char)) :incomplete))))
         (doubled-eligible (or (nskk-prolog-query-value
                                `(doubled-context ,'\?de
                                                  ,(if last-is-n 'yes 'no)
                                                  ,(if char-is-n 'yes 'no)
                                                  ,(if same-ok   'yes 'no)
                                                  ,(if n-ok      'yes 'no)
                                                  ,result-type)
                                '\?de)
                               'not-eligible))
         (cache-key        (cons doubled-eligible result-type)))
    (or (gethash cache-key nskk--romaji-classify-cache)
        (let ((class (or (nskk-prolog-query-value
                          `(romaji-classify ,'\?class ,doubled-eligible ,result-type)
                          '\?class)
                         'no-match)))
          (puthash cache-key class nskk--romaji-classify-cache)
          class))))

;; Classification taxonomy for `nskk-convert-input-to-kana/k':
;;
;;  Priority  Class           Trigger condition
;;  --------  --------------  ---------------------------------------------------
;;  1         nn-double       last=n AND char=n → emit ん, clear buffer (DDSKK-compatible)
;;  2         azik-deferred   same doubled consonant, AZIK match → emit kana
;;                            tentatively; may be retroactively corrected to っ
;;                            on the next vowel input
;;  3         sokuon          same doubled consonant, no AZIK match → emit っ
;;  4         n-consonant     last=n AND char is not a hatsuon-blocker AND
;;                            no complete match exists for the combined input.
;;                            emit ん for the pending n, then process char.
;;                            Yields to `match' when an AZIK rule matches
;;                            (e.g. "nq"→"ない", "nk"→"にん" in AZIK mode).
;;  5         match           converter returned a kana string → emit kana
;;  6         incomplete      converter returned :incomplete → accumulate
;;  7         no-match        no rule applies → pass input through as-is

(defun/done nskk--apply-all-deferred-corrections (char)
  "Apply all pending retroactive kana corrections for CHAR.
Sequentially applies the three deferred-correction handlers in order:
  1. `nskk--apply-deferred-azik-correction'   -- AZIK sokuon (っ) correction
  2. `nskk--apply-vowel-shadow-correction'     -- vowel-shadow AZIK correction
  3. `nskk--apply-colon-okuri-correction'      -- colon-okurigana っ correction
Each is a no-op when its respective deferred-state variable is nil."
  (nskk--apply-deferred-azik-correction char)
  (nskk--apply-vowel-shadow-correction char)
  (nskk--apply-colon-okuri-correction char))

(defun/done nskk--apply-deferred-azik-correction (char)
  "Apply retroactive sokuon correction when a deferred AZIK state is pending.
When CHAR is a vowel and `nskk--deferred-azik-state' is set, deletes the
tentatively emitted kana, inserts っ, and resets the romaji buffer to the
consonant so that consonant+vowel is processed normally as sokuon.
A non-vowel CHAR merely clears the deferred state without correction."
  (when nskk--deferred-azik-state
    (let ((deferred-cons (car nskk--deferred-azik-state))
          (deferred-kana (cdr nskk--deferred-azik-state)))
      (setq nskk--deferred-azik-state nil)
      (when (memq char '(?a ?i ?u ?e ?o))
        (nskk-debug-log "[INPUT] azik-deferred-correction: cons=%c kana=%s"
                        deferred-cons deferred-kana)
        (delete-char (- (length deferred-kana)))
        (insert "\u3063")
        (setq nskk--romaji-buffer (char-to-string deferred-cons))))))

(defun/done nskk--apply-vowel-shadow-correction (char)
  "Apply retroactive correction when a vowel-shadow deferred state is pending.
When CHAR is a vowel and `nskk--deferred-vowel-shadow-state' is set, deletes
the tentatively emitted kana and resets the romaji buffer to the deferred
romaji prefix so that romaji+vowel is processed as the longer standard rule.
Unlike `nskk--apply-deferred-azik-correction', no っ is inserted.
A non-vowel CHAR merely clears the deferred state without correction."
  (when nskk--deferred-vowel-shadow-state
    (let ((deferred-romaji (car nskk--deferred-vowel-shadow-state))
          (deferred-kana   (cdr nskk--deferred-vowel-shadow-state)))
      (setq nskk--deferred-vowel-shadow-state nil)
      (when (memq char '(?a ?i ?u ?e ?o))
        (nskk-debug-log "[INPUT] azik-vowel-shadow-correction: romaji=%s kana=%s"
                        deferred-romaji deferred-kana)
        (delete-char (- (length deferred-kana)))
        (setq nskk--romaji-buffer deferred-romaji)))))

(defun/done nskk--apply-colon-okuri-correction (char)
  "Apply retroactive sokuon insertion for AZIK colon-okurigana sequence.
When CHAR is a vowel and `nskk--azik-colon-okuri-deferred' is set, deletes
the tentative placeholder consonant, inserts っ, and resets the romaji
buffer to the consonant so that consonant+vowel completes the okurigana
syllable (e.g. `t' + `e' → て, giving って as the full okurigana kana).
A non-vowel CHAR merely clears the deferred state without correction."
  (when nskk--azik-colon-okuri-deferred
    (let ((deferred-cons (car nskk--azik-colon-okuri-deferred))
          (deferred-placeholder (cdr nskk--azik-colon-okuri-deferred)))
      (setq nskk--azik-colon-okuri-deferred nil)
      (when (memq char '(?a ?i ?u ?e ?o))
        (nskk-debug-log "[INPUT] colon-okuri-correction: cons=%c placeholder=%s"
                        deferred-cons deferred-placeholder)
        (delete-char (- (length deferred-placeholder)))
        (insert "\u3063")
        (setq nskk--romaji-buffer (char-to-string deferred-cons))))))

;;;; Romaji-to-Kana Class Handlers
;;
;; One named CPS function per romaji-classify/3 class.  Each is dispatched
;; from `nskk-convert-input-to-kana/k' via a flat pcase on the class symbol.
;; All handlers follow `defun/k' CPS conventions: on-found receives a kana
;; string; on-not-found is called with no arguments (incomplete/pending only).

(defun/k nskk--kana-handle-nn-double (result)
  "Handle nn-double class: n+n → emit ん, clear buffer.
RESULT is the converter output (ignored; ん is always emitted directly).
Calls `nskk--emit-hatsuon-prefix' with empty new-buffer-value, then
calls on-found with the resulting kana (prefix-kana + ん)."
  (ignore result)
  (<- kana nskk--emit-hatsuon-prefix "")
  (succeed kana))

(defun/k nskk--kana-handle-azik-vowel-deferred (input result)
  "Handle azik-vowel-deferred class: emit AZIK kana tentatively.
INPUT is the full romaji string; RESULT is the converter output cons.
Records `nskk--deferred-vowel-shadow-state' so the next vowel can retract
this emission and process INPUT+vowel as the longer standard-romaji rule
\(e.g. \"sh\"→\"すう\" then \"sha\"→\"しゃ\")."
  (let ((kana (car result)))
    (nskk-debug-log "[INPUT] azik-vowel-deferred-emit: input=%s kana=%s" input kana)
    (setq nskk--deferred-vowel-shadow-state (cons input kana))
    (setq nskk--romaji-buffer "")
    (succeed kana)))

(defun/k nskk--kana-handle-azik-deferred (input result)
  "Handle azik-deferred class: emit AZIK kana tentatively for sokuon.
INPUT is the full romaji string; RESULT is the converter output cons.
Records `nskk--deferred-azik-state' so the next vowel can retract this
emission, insert っ, and restart from the consonant
\(e.g. \"kk\"→\"きん\" then \"kka\"→\"っか\")."
  (let ((kana (car result)))
    (nskk-debug-log "[INPUT] azik-deferred-emit: input=%s kana=%s" input kana)
    (setq nskk--deferred-azik-state (cons (aref input 0) kana))
    (setq nskk--romaji-buffer "")
    (succeed kana)))

(defun/k nskk--kana-handle-match (result)
  "Handle match class: converter produced a complete kana string.
RESULT is the converter output cons (KANA . REMAINING).
Sets `nskk--romaji-buffer' to REMAINING (or empty) and calls on-found
with KANA."
  (let ((kana      (car result))
        (remaining (cdr result)))
    (nskk-debug-log "[INPUT] romaji-converted: kana=%s" kana)
    (setq nskk--romaji-buffer
          (if (and (stringp remaining) (> (length remaining) 0)) remaining ""))
    (succeed kana)))

(defun/k nskk--kana-handle-n-consonant (char result)
  "Handle n-consonant class: last=n + consonant → emit ん then process char.
CHAR is the current input character; RESULT is the converter output (ignored;
ん is emitted unconditionally for the pending n).
Flushes ん for the pending n (leaving CHAR in buffer), then immediately
checks if CHAR itself is a complete converter match and emits it too."
  (ignore result)
  (nskk-debug-log "[INPUT] romaji-hatsuon-n+consonant: char=%c" char)
  (<- hatsuon-kana nskk--emit-hatsuon-prefix (char-to-string char))
  (let ((char-result (nskk-converter-convert (char-to-string char))))
    (if (and (consp char-result) (stringp (car char-result)))
        (progn
          (setq nskk--romaji-buffer "")
          (succeed (concat hatsuon-kana (car char-result))))
      (succeed hatsuon-kana))))

(defun/k nskk--kana-handle-sokuon (char)
  "Handle sokuon class: doubled consonant → emit っ (U+3063).
CHAR is the current character.  Resets `nskk--romaji-buffer' to the
single-char string of CHAR and calls on-found with っ."
  (nskk-debug-log "[INPUT] romaji-sokuon: char=%c" char)
  (setq nskk--romaji-buffer (char-to-string char))
  (succeed "\u3063"))

(defun/k nskk--kana-handle-incomplete (char input)
  "Handle incomplete class: accumulate romaji or flush on non-alpha mismatch.
CHAR is the current character; INPUT is the full pending romaji string.
Normal path: sets `nskk--romaji-buffer' to INPUT and calls on-not-found
\(more input needed to complete the sequence).
Special path: if CHAR is not a-z but has a standalone complete match
\(e.g. AZIK `;'→っ), flushes the buffer and recurses via
`nskk-convert-input-to-kana/k', then calls on-found with the result."
  (nskk-debug-log "[INPUT] romaji-incomplete: input=%s" input)
  (if (and (not (<= ?a char ?z))
           (stringp (nskk-converter-lookup (char-to-string char))))
      (progn
        (setq nskk--romaji-buffer "")
        (<- kana nskk-convert-input-to-kana char)
        (succeed kana))
    (setq nskk--romaji-buffer input)
    (fail)))

(defun/k nskk--kana-handle-no-match (input)
  "Handle no-match class: pass INPUT through unchanged.
INPUT is the full romaji input string.  Resets `nskk--romaji-buffer'
to empty and calls on-found with INPUT verbatim."
  (nskk-debug-log "[INPUT] romaji-no-match: input=%s" input)
  (setq nskk--romaji-buffer "")
  (succeed input))

(defun/k nskk-convert-input-to-kana (char)
  "Convert input CHAR to kana, calling ON-FOUND or ON-NOT-FOUND when done.
Accumulates romaji characters in `nskk--romaji-buffer' and converts
to kana when a complete romaji sequence is recognized.

ON-FOUND is called with a kana string when output is produced.
ON-NOT-FOUND is called with no arguments when input is still incomplete.

Processing pipeline:
  1. Apply pending deferred corrections via
     `nskk--apply-all-deferred-corrections'.
  2. Compute INPUT = buffer + char, query converter for RESULT.
  3. Classify via `nskk--classify-romaji-input' (two Prolog queries).
     Promote `match' → `azik-vowel-deferred' when in the vowel-shadow set.
  4. Dispatch class to one of eight named handlers."
  (nskk--apply-all-deferred-corrections char)
  (let* ((input        (concat nskk--romaji-buffer (char-to-string char)))
         (result       (nskk-converter-convert input))
         (buf-len      (length nskk--romaji-buffer))
         (last-buf-char (and (> buf-len 0) (aref nskk--romaji-buffer (1- buf-len))))
         (class        (let ((c (nskk--classify-romaji-input char last-buf-char result)))
                         (if (and (eq c 'match)
                                  (bound-and-true-p nskk--azik-vowel-shadow-set)
                                  (gethash input nskk--azik-vowel-shadow-set))
                             'azik-vowel-deferred
                           c))))
    (pcase class
      ('nn-double
       (<- kana nskk--kana-handle-nn-double result) (succeed kana))
      ('azik-vowel-deferred
       (<- kana nskk--kana-handle-azik-vowel-deferred input result) (succeed kana))
      ('azik-deferred
       (<- kana nskk--kana-handle-azik-deferred input result) (succeed kana))
      ('match
       (<- kana nskk--kana-handle-match result) (succeed kana))
      ('n-consonant
       (<- kana nskk--kana-handle-n-consonant char result) (succeed kana))
      ('sokuon
       (<- kana nskk--kana-handle-sokuon char) (succeed kana))
      ('incomplete
       (<- kana nskk--kana-handle-incomplete char input) (succeed kana))
      (_
       (<- kana nskk--kana-handle-no-match input) (succeed kana)))))

;; Override the sync wrapper generated by `defun/k' to return "" instead of
;; nil on incomplete romaji (failure path).  Callers and tests rely on ""
;; as the sentinel for "still accumulating", not nil.
(with-no-warnings
(defun nskk-convert-input-to-kana (char)
  "Convert input CHAR to kana.
Returns the kana string produced, or \"\" if the romaji buffer is still
incomplete (waiting for more characters to complete a sequence).
See `nskk-convert-input-to-kana/k' for the CPS variant with explicit
continuations."
  (or (nskk-convert-input-to-kana/k char #'identity #'ignore) "")))

(defvar nskk--input-initialized nil
  "Non-nil when input routing Prolog predicates have been initialized.")

;;;; Private per-family Prolog initializers

(defun/done nskk--init-input-routing-rules ()
  "Assert input-route/2 facts.
All input modes are covered here, including abbrev (process-abbrev)."
  (nskk-prolog-define-fact-table input-route (:arity 2 :index :hash)
    (ascii          insert-direct)
    (latin          insert-direct)
    (jisx0208-latin insert-fullwidth)
    (abbrev         process-abbrev)
    (hiragana       process-japanese)
    (katakana       process-japanese)
    (katakana-半角   process-japanese)))

(defun/done nskk--init-toggle-rules ()
  "Assert toggle-mode/2 facts."
  (nskk-prolog-define-fact-table toggle-mode (:arity 2 :index :hash)
    (hiragana    katakana)
    (katakana    hiragana)
    (katakana-半角 hiragana)))

(defun/done nskk--init-q-key-rules ()
  "Assert q-key-action/3 facts.
Simplified context-aware q-key handling:
- standard: always toggle mode
- azik + azik-complete: fire the AZIK romaji rule (e.g. kq→かい)
- azik + pending: insert ん/ン
- azik + empty: insert ん/ン (standalone q)"
  (nskk-prolog-define-fact-table q-key-action (:arity 3 :index :hash)
    (standard \?buf toggle-mode)
    (azik azik-complete fire-romaji)
    (azik pending insert-n)
    (azik empty insert-n)))

(defun/done nskk--init-semicolon-rules ()
  "Assert semicolon-key-action/2 facts.
Standard mode uses sticky-shift (virtual Shift via semicolon key)."
  (nskk-prolog-define-fact-table semicolon-key-action (:arity 2 :index :hash)
    (azik     insert-small-tsu)
    (standard sticky-shift)))

(defun/done nskk--init-kakutei-rules ()
  "Assert kakutei-action/2 facts for kakutei key dispatch."
  (nskk-prolog-define-fact-table kakutei-action (:arity 2 :index :hash)
    (converting     commit-candidate)
    (preedit        commit-preedit)
    (romaji-pending clear-romaji)
    (hiragana-idle  insert-newline)
    (katakana-idle  enter-hiragana)
    (direct-idle    enter-hiragana)))

(defun nskk--azik-complete-match-p (char)
  "Return non-nil when AZIK is active and pending-romaji+CHAR is a complete match.
Checks `nskk--romaji-table' for a string value at the key formed by
concatenating `nskk--romaji-buffer' with the string representation of CHAR.
Used by AZIK-aware key handlers (l, q) to detect multi-char AZIK rules."
  (and (eq nskk-converter-romaji-style 'azik)
       (stringp (gethash (concat nskk--romaji-buffer (char-to-string char))
                         nskk--romaji-table))))

(defun nskk--romaji-has-match-p (char)
  "Return non-nil when pending-romaji+CHAR is a complete conversion rule.
Checks the romaji conversion table for a string value at the key formed by
concatenating `nskk--romaji-buffer' with the string representation of CHAR.
Unlike `nskk--azik-complete-match-p', this works in any romaji style and
is used by key handlers that need to check for z-prefix or other rules."
  (and (not (string-empty-p nskk--romaji-buffer))
       (stringp (nskk-converter-lookup
                 (concat nskk--romaji-buffer (char-to-string char))))))

(defun/done nskk--init-japanese-input-class-rules ()
  "Assert japanese-input-class/3 facts for nskk-process-japanese-input dispatch.
Replaces the 3-arm cond in `nskk-process-japanese-input' with a Prolog table.

Arguments (all pre-computed classification symbols):
  CONTEXT   -- state of the input context:
               `colon-pending'       -- nskk--azik-colon-okuri-pending is set
               `azik-arm-eligible'   -- AZIK style + active preedit +
                                       no okurigana
               `other'               -- everything else
  CHAR-TYPE -- character classification:
               `alphabetic-lower'    -- ASCII a-z
               `colon'               -- the `:' character
               `other'               -- anything else
Result (first arg):
  `fire'   -- call nskk--fire-azik-colon-okuri
  `arm'    -- call nskk--arm-azik-colon-trigger
  `normal' -- call nskk--process-normal-japanese-input"
  (nskk-prolog-define-fact-table japanese-input-class (:arity 3 :index :list)
    (fire   colon-pending      alphabetic-lower)
    (arm    azik-arm-eligible  colon)
    (normal \?ctx              \?ct)))

(defun/done nskk--init-doubled-context-rules ()
  "Assert doubled-context/6 facts for romaji doubled-consonant pre-classification.
Replaces the 5-arm cond computing `doubled-eligible' in
`nskk--classify-romaji-input' with a Prolog lookup.

Arguments (pre-computed boolean symbols `yes'/`no', or `result-type' values):
  LAST-IS-N   -- last romaji buffer char is `n' (yes/no)
  CHAR-IS-N   -- current input char is `n' (yes/no)
  SAME-OK     -- same char doubled AND not in sokuon-blockers (yes/no)
  N-OK        -- last=n path: char not in hatsuon-blockers AND
                 direct two-char lookup not :incomplete (yes/no)
  RESULT-TYPE -- `match', `incomplete', or `no-result'
Result (first arg): doubled-eligible symbol for romaji-classify/3."
  (nskk-prolog-define-fact-table doubled-context (:arity 6 :index :list)
    ;; Priority 1: n+n — most specific n-prefix rule
    (nn              yes yes \?so \?no \?rt)
    ;; Priority 2: last=n + eligible consonant, not a complete match
    (n-consonant     yes no  \?so yes incomplete)
    (n-consonant     yes no  \?so yes no-result)
    ;; Priority 3: same eligible doubled with a complete converter match
    (eligible-match  no  \?cn yes \?no match)
    ;; Priority 4: same eligible doubled, any other result
    (eligible-other  no  \?cn yes \?no \?rt)
    ;; Priority 5: fallback
    (not-eligible    \?ln \?cn \?so \?no \?rt)))

(defun/done nskk--init-effective-char-rules ()
  "Assert effective-char-class/5 facts for nskk--compute-effective-char.
Replaces the nested and/or logic with a Prolog dispatch table.

Arguments (pre-computed boolean symbols `yes'/`no'):
  UPPER-READY   -- char is A-Z AND nskk-converter-auto-start-henkan non-nil
  CONV-ACTIVE   -- nskk--conversion-start-active-p is non-nil
  BUF-NONEMPTY  -- nskk--romaji-buffer is non-empty
  VOWEL-OR-CTX  -- char is uppercase vowel (A/I/U/E/O),
                   OR no preedit yet, OR okurigana is pending in state
Result (first arg): char class symbol."
  (nskk-prolog-define-fact-table effective-char-class (:arity 5 :index :list)
    ;; henkan-start: uppercase+auto-start, no active conversion
    (henkan-start    yes no  \?bn \?voc)
    ;; normalize-vowel: uppercase, conversion active, buffer non-empty, vowel-or-ctx
    (normalize-vowel yes yes yes  yes)
    ;; normal: all other cases (includes okurigana trigger: upper, conv, non-empty, no vowel-ctx)
    (normal          \?ur \?ca \?bn \?voc)))

(defun/done nskk-input-initialize ()
  "Initialize input routing Prolog predicates for all key dispatch tables.
Idempotent: subsequent calls are no-ops."
  (unless nskk--input-initialized
    (nskk--init-input-routing-rules)
    (nskk--init-toggle-rules)
    (nskk--init-q-key-rules)
    (nskk--init-semicolon-rules)
    (nskk--init-kakutei-rules)
    (nskk--init-romaji-classify-rules)
    (nskk--init-japanese-input-class-rules)
    (nskk--init-doubled-context-rules)
    (nskk--init-effective-char-rules)
    (setq nskk--input-initialized t)))

(provide 'nskk-input)

;;; nskk-input.el ends here
