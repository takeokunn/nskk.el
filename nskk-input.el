;;; nskk-input.el --- Input processing for NSKK -*- lexical-binding: t; -*-

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

;; Input processing and mode switching for NSKK (Layer 4: Input).
;;
;; Layer position: L4 (Input) -- depends on nskk-henkan, nskk-kana,
;;   nskk-state, nskk-converter, nskk-prolog.  Optionally loads nskk-azik.
;;
;; Handles character routing, romaji-to-kana accumulation, mode switching,
;; and AZIK-aware key handlers.  This layer bridges keymap events (L5) and
;; the henkan conversion pipeline (L3).
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
;;   `nskk-toggle-japanese-mode'     -- toggle hiragana<->katakana
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
(declare-function nskk-state-get-metadata "nskk-state")
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

(defvar-local nskk--deferred-azik-state nil
  "Non-nil when an AZIK two-char rule was tentatively emitted.
Value is a cons (CONSONANT-CHAR . KANA-STRING).  CONSONANT-CHAR is the
doubled character (e.g. ?k) and KANA-STRING is the tentatively emitted
kana (e.g. \"きん\").  On the next input:
  - Vowel: delete tentative kana, insert っ, reset romaji buffer to
    CONSONANT-CHAR, then process consonant+vowel normally (sokuon).
  - Non-vowel: clear deferred state without retroactive correction.
Satisfies kk→きん (AZIK hatsuon) and kka→っか (standard sokuon).")

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
  "Toggle between hiragana and katakana modes."
  :interactive t
  (let* ((current-mode (when (boundp 'nskk-current-state)
                         (nskk-state-mode nskk-current-state)))
         (target (nskk-prolog-query-value
                  `(toggle-mode ,current-mode ,'\?target) '\?target)))
    (when target
      (nskk-debug-log "[INPUT] toggle-mode: from=%s to=%s" current-mode target)
      (nskk--set-mode target)
      (when (fboundp 'nskk-modeline-update)
        (nskk-modeline-update)))))

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
  - If there is other pending romaji input, produce \u3093
  - Otherwise, toggle hiragana/katakana mode
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
      ('insert-n     (insert "\u3093"))
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

;;;###autoload
(defun/done nskk-self-insert (n)
  "Process self-insert input, routing the typed character based on current mode.
N is the prefix repeat count from `last-command-event'."
  :interactive "p"
  (let ((char (if (integerp last-command-event)
                  last-command-event
                (aref last-command-event 0)))
        (current-mode (or (nskk-state-get-mode) 'ascii)))
    (nskk-debug-log "[INPUT] self-insert: char=%c mode=%s" char current-mode)
    ;; P1: candidate selection — short-circuits when key matches a candidate.
    (unless (nskk--try-candidate-selection char)
      ;; P2: implicit kakutei (確定) — DDSKK-compatible commit before new char.
      (when (nskk--implicit-kakutei-needed-p)
        (nskk-debug-log "[INPUT] implicit-kakutei: char=%c" char)
        (nskk-commit-current))
      ;; P3: abbrev mode — direct insertion, bypassing Prolog routing.
      ;; In DDSKK, skk-abbrev-mode-map binds all ASCII to skk-abbrev-insert
      ;; = self-insert-command, bypassing all romaji/kana routing.
      (if (eq current-mode 'abbrev)
          (progn
            (nskk-debug-log "[INPUT] abbrev-input: char=%c" char)
            (nskk-process-abbrev-input char))
        ;; P4–P5: mode-based routing via Prolog.
        (let ((route (nskk-prolog-query-value
                      `(input-route ,current-mode ,'\?action) '\?action)))
          (nskk-debug-log "[INPUT] route: mode=%s route=%s" current-mode route)
          (pcase route
            ('insert-direct    (nskk-insert-char char n))
            ('insert-fullwidth (nskk-insert-fullwidth-char char n))
            (_                 (nskk-process-japanese-input char n))))))))

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
Returns a list (EFFECTIVE-CHAR IS-HENKAN-START NORMALIZE-VOWEL-P).

EFFECTIVE-CHAR is (downcase CHAR) when IS-HENKAN-START or NORMALIZE-VOWEL-P
is non-nil; otherwise CHAR itself is returned unchanged.

IS-HENKAN-START is non-nil when all of the following hold:
  - CHAR is an uppercase ASCII letter (A-Z)
  - `nskk-converter-auto-start-henkan' is non-nil
  - No conversion is currently active (see `nskk--conversion-start-active-p')

NORMALIZE-VOWEL-P is non-nil when CHAR is an uppercase vowel (A I U E O),
a conversion is already active, and the romaji buffer is non-empty.
This handles the case where a pending consonant (e.g. \"h\") should be
completed by the vowel (\"O\" → \"o\" → \"ほ\") rather than starting
okurigana.  Uppercase consonants always function as okurigana markers per
DDSKK behavior; the empty-buffer case for vowels also triggers okurigana."
  (let* ((is-henkan-start (and (characterp char)
                               (<= ?A char) (<= char ?Z)
                               nskk-converter-auto-start-henkan
                               (not (nskk--conversion-start-active-p))))
         (normalize-vowel-p
          (and (characterp char)
               (<= ?A char) (<= char ?Z)
               (nskk--conversion-start-active-p)
               (memq char '(?A ?I ?U ?E ?O))
               (not (string-empty-p nskk--romaji-buffer))))
         (effective-char (if (or normalize-vowel-p is-henkan-start)
                             (downcase char)
                           char)))
    (succeed (list effective-char is-henkan-start normalize-vowel-p))))

(defun/done nskk-process-japanese-input (char n)
  "Process input in Japanese mode (hiragana/katakana), then call on-done.
CHAR is the input character.
N is the repeat count.
The on-done continuation is called with no arguments after all side effects
complete: after okurigana processing, after pending-romaji display, and
after kana emission.
When CHAR is uppercase and `nskk-converter-auto-start-henkan' is non-nil,
set the conversion start marker at the current point and process the
lowercase version of the letter as normal romaji input."
  ;; Sticky shift: treat next character as uppercase
  (when nskk--sticky-shift-pending
    (setq nskk--sticky-shift-pending nil)
    (when (and (characterp char) (<= ?a char) (<= char ?z))
      (setq char (upcase char))))
  (cl-destructuring-bind (effective-char is-henkan-start normalize-vowel-p)
      (nskk--compute-effective-char char)
    (when is-henkan-start
      (nskk--setup-henkan-start-marker char))
    ;; Okurigana path: when the char is consumed as okurigana input,
    ;; there is nothing more to do.  Henkan-start chars are never
    ;; routed through okurigana (the condition mirrors the original
    ;; `if (and (not is-henkan-start) ...)' guard exactly).
    ;; Uppercase vowels trigger okurigana unless the romaji buffer has an
    ;; incomplete consonant (normalize-vowel-p), in which case they
    ;; complete the romaji pair.  Uppercase consonants always trigger okurigana.
    (if (and (not is-henkan-start)
             (not normalize-vowel-p)
             (nskk-process-okurigana-input char))
        (nskk-debug-log "[INPUT] okurigana-processed: char=%c" char)
      ;; Main romaji-to-kana path.
      ;; nskk-convert-input-to-kana returns a kana string on success or nil
      ;; (on-not-found/pending).  nskk--process-kana-result treats empty or nil
      ;; kana as pending and calls nskk--show-pending-romaji, so both cases are
      ;; unified by coercing nil to "".
      (nskk--process-kana-result
       (or (nskk-convert-input-to-kana effective-char) "") n))))

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
                       `nn-double'      (n+n → emit ん, keep \"n\" in buffer)
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

Pre-computes Elisp boolean values (blocker membership) then queries the
`romaji-classify/3' Prolog table for the classification.  Priority order
is encoded in fact assertion order: nn-double > azik-deferred > sokuon >
n-consonant > match > incomplete > no-match."
  (let* ((result-type (nskk--romaji-result-type result))
         (same-doubled (eql last-buf-char char))
         (not-sokuon-blocked (not (memq char nskk--sokuon-blockers)))
         (doubled-eligible
          (cond
           ;; nn takes priority over other doubled checks
           ((and (eql last-buf-char ?n) (eql char ?n))
            'nn)
           ;; n followed by non-hatsuon-blocker: n-consonant case.
           ;; Yield to `match' when the combined input has a complete AZIK rule
           ;; (e.g. "nq"→"ない", "nk"→"にん") so AZIK takes priority over ん-emit.
           ((and (eql last-buf-char ?n)
                 (not (memq char nskk--hatsuon-blockers))
                 (not (eq result-type 'match)))
            'n-consonant)
           ;; same eligible consonant doubled with a complete match
           ((and same-doubled not-sokuon-blocked (eq result-type 'match))
            'eligible-match)
           ;; same eligible consonant doubled without a complete match
           ((and same-doubled not-sokuon-blocked)
            'eligible-other)
           ;; fallback
           (t 'not-eligible))))
    (let ((cache-key (cons doubled-eligible result-type)))
      (or (gethash cache-key nskk--romaji-classify-cache)
          (let ((class (or (nskk-prolog-query-value
                            `(romaji-classify ,'\?class ,doubled-eligible ,result-type)
                            '\?class)
                           'no-match)))
            (puthash cache-key class nskk--romaji-classify-cache)
            class)))))

;; Classification taxonomy for `nskk-convert-input-to-kana/k':
;;
;;  Priority  Class           Trigger condition
;;  --------  --------------  ---------------------------------------------------
;;  1         nn-double       last=n AND char=n → emit ん, keep "n" in buffer
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

(defun/k nskk-convert-input-to-kana (char)
  "Convert input CHAR to kana, calling ON-FOUND or ON-NOT-FOUND when done.
Accumulates romaji characters in `nskk--romaji-buffer' and converts
to kana when a complete romaji sequence is recognized.

ON-FOUND is called with a kana string when a kana string is produced
(including non-empty strings from no-match passthrough).
ON-NOT-FOUND is called with no arguments when the romaji buffer is still
incomplete (waiting for more characters to complete a sequence).

When `nskk--deferred-azik-state' is set, calls
`nskk--apply-deferred-azik-correction' first: a vowel triggers retroactive
sokuon correction; a non-vowel merely clears the state."
  (nskk--apply-deferred-azik-correction char)
  (let* ((input (concat nskk--romaji-buffer (char-to-string char)))
         (result (nskk-converter-convert input))
         (buf-len (length nskk--romaji-buffer))
         (last-buf-char (and (> buf-len 0)
                             (aref nskk--romaji-buffer (1- buf-len)))))
    (pcase (nskk--classify-romaji-input char last-buf-char result)
      ;; Priority 1: n+n → emit ん, leave "n" in buffer for next consonant.
      ;; Checked before `n-consonant' because nn is the most specific n-prefix rule.
      ('nn-double
       (nskk-debug-log "[INPUT] romaji-hatsuon-nn: input=%s" input)
       (<- hatsuon-kana nskk--emit-hatsuon-prefix "n")
       (succeed hatsuon-kana))

      ('azik-deferred
       ;; Emit the AZIK kana tentatively and record the deferred state.
       ;; If the next character is a vowel, retroactive correction (above)
       ;; will undo this emission and produce sokuon instead.
       (let ((kana (car result)))
         (nskk-debug-log "[INPUT] azik-deferred-emit: input=%s kana=%s" input kana)
         (setq nskk--deferred-azik-state (cons (aref input 0) kana))
         (setq nskk--romaji-buffer "")
         (succeed kana)))
      ('match
       (let ((kana (car result))
             (remaining (cdr result)))
         (nskk-debug-log "[INPUT] romaji-converted: input=%s kana=%s" input kana)
         (setq nskk--romaji-buffer
               (if (and (stringp remaining) (> (length remaining) 0))
                   remaining
                 ""))
         (succeed kana)))
      ;; Priority 4: n+consonant → emit ん for pending n, then try char alone.
      ;; Precedes `match' in priority so that e.g. "nk" reliably emits ん
      ;; even if the full string "nk" happened to have a converter match.
      ;; After emitting ん, the single char is checked: if it is itself a
      ;; complete match (e.g. AZIK ";"→っ), emit it immediately rather than
      ;; leaving it stranded in the buffer.
      ('n-consonant
       (nskk-debug-log "[INPUT] romaji-hatsuon-n+consonant: input=%s char=%c" input char)
       ;; Flush ん for the pending n, leaving char in romaji buffer.
       ;; Then immediately check if char itself is a complete match (e.g. AZIK
       ;; ";"→っ): if so, emit it now and clear the buffer so it is not lost.
       (<- hatsuon-kana nskk--emit-hatsuon-prefix (char-to-string char))
       (let ((char-result (nskk-converter-convert (char-to-string char))))
         (if (and (consp char-result) (stringp (car char-result)))
             (progn
               (setq nskk--romaji-buffer "")
               (succeed (concat hatsuon-kana (car char-result))))
           (succeed hatsuon-kana))))
      ('sokuon
       (nskk-debug-log "[INPUT] romaji-sokuon: input=%s" input)
       (setq nskk--romaji-buffer (char-to-string char))
       (succeed "\u3063"))
      ('incomplete
       (nskk-debug-log "[INPUT] romaji-incomplete: input=%s" input)
       (setq nskk--romaji-buffer input)
       (fail))
      (_
       (nskk-debug-log "[INPUT] romaji-no-match: input=%s" input)
       (setq nskk--romaji-buffer "")
       (succeed input)))))

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
- azik + pending: insert ん
- azik + empty: toggle mode"
  (nskk-prolog-define-fact-table q-key-action (:arity 3 :index :hash)
    (standard \?buf toggle-mode)
    (azik azik-complete fire-romaji)
    (azik pending insert-n)
    (azik empty toggle-mode)))

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
    (setq nskk--input-initialized t)))

(provide 'nskk-input)

;;; nskk-input.el ends here
