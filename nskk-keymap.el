;;; nskk-keymap.el --- Keymap definitions for NSKK -*- lexical-binding: t; -*-

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
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; State-aware key dispatch for NSKK (Layer 5: Presentation).
;;
;; Layer position: L5 (Presentation) -- depends on nskk-state and nskk-prolog.
;; State mutations are delegated to the Application layer (nskk-input,
;; nskk-henkan); this module only reads state to determine dispatch.
;;
;; Provides interactive handlers for the special keys in nskk-mode-map:
;; q, l, L, /, x, SPC, RET, C-g, C-n, C-p, DEL.  Each handler checks the current
;; NSKK conversion state before acting, falling through to `self-insert-command'
;; or `keyboard-quit' when NSKK is in ASCII mode or state is inactive.
;;
;; Prolog predicates maintained by this module:
;; - `key-action/3'  -- (key state action) dispatch rules for x, SPC, RET, C-g, C-n, C-p, C-f, C-b
;;
;; Key architecture:
;; - `nskk-define-key-handler' -- macro that generates Prolog-dispatched
;;     interactive commands.  Queries `key-action/3' to decide the action
;;     based on the current dispatch state (converting, preedit, normal).
;; - `nskk-with-japanese-mode' -- macro for q/l/L// handlers.  Performs
;;     implicit kakutei (確定) first if in converting (▼) or preedit (▽)
;;     state, then executes the mode-switch action.  Uses `japanese-mode/1'
;;     (defined in nskk-state.el) to guard the idle Japanese-mode case.
;; - `nskk--current-key-state' -- returns `converting', `preedit', or
;;     `normal' for use by the dispatch system.
;;
;; Key public API (all interactive):
;; - `nskk-handle-q'       -- toggle hiragana/katakana
;; - `nskk-handle-l'       -- switch to ASCII mode
;; - `nskk-handle-upper-l' -- switch to full-width latin mode
;; - `nskk-handle-slash'   -- enter abbrev mode
;; - `nskk-handle-x'       -- previous candidate
;; - `nskk-handle-space'   -- start conversion / next candidate
;; - `nskk-handle-return'  -- commit candidate (no newline) / insert newline
;; - `nskk-handle-cancel'  -- cancel conversion / preedit / keyboard-quit
;; - `nskk-handle-ctrl-n'  -- next candidate / next-line fallthrough
;; - `nskk-handle-ctrl-p'  -- previous candidate / previous-line fallthrough
;; - `nskk-handle-ctrl-f'  -- commit then forward-char / forward-char fallthrough
;; - `nskk-handle-ctrl-b'  -- commit then backward-char / backward-char fallthrough
;; - `nskk-handle-ctrl-a'  -- commit then beginning-of-line / beginning-of-line fallthrough
;; - `nskk-handle-ctrl-e'  -- commit then end-of-line / end-of-line fallthrough

;;; Code:

(require 'nskk-state)
(require 'nskk-prolog)

;; Functions in nskk-input.el
(declare-function nskk-toggle-japanese-mode "nskk-input")
(declare-function nskk-set-mode-latin "nskk-input")
(declare-function nskk-set-mode-abbrev "nskk-input")
(declare-function nskk-set-mode-jisx0208-latin "nskk-input")
(declare-function nskk-self-insert "nskk-input")
(declare-function nskk-handle-q-key "nskk-input")
(declare-function nskk--azik-complete-match-p "nskk-input")
(declare-function nskk-process-japanese-input "nskk-input")
(defvar nskk-converter-romaji-style)
;; Functions in nskk-henkan.el
(declare-function nskk-converting-p "nskk-henkan")
(declare-function nskk--has-preedit "nskk-henkan")
(declare-function nskk--get-conversion-start "nskk-henkan")
(declare-function nskk-start-conversion "nskk-henkan")
(declare-function nskk-next-candidate "nskk-henkan")
(declare-function nskk-commit-current "nskk-henkan")
(declare-function nskk-previous-candidate "nskk-henkan")
(declare-function nskk-cancel-conversion "nskk-henkan")
(declare-function nskk-cancel-conversion-to-reading "nskk-henkan")
(declare-function nskk-cancel-preedit "nskk-henkan")
(declare-function nskk-henkan-kakutei "nskk-henkan")
(declare-function nskk-dynamic-complete "nskk-henkan")
(declare-function nskk-set-mode-numeric "nskk-input")
(defvar nskk-henkan-on-marker)
;; Variables in nskk-azik.el
(defvar nskk-azik-keyboard-type)

(defvar nskk-annotate-mode-map-hook nil
  "Hook run when annotating mode map.
DDSKK equivalent to `skk-annotate-mode-map-hook'.")

(defvar nskk-annotate-minibuffer-map-hook nil
  "Hook run when annotating minibuffer map.
DDSKK equivalent to `skk-annotate-minibuffer-map-hook'.")

;;;; Prolog Key-Action Rules

;; Key dispatch rules: (key-action KEY STATE ACTION)
;; Order matters within each key: first match wins.
(nskk-prolog-define-fact-table key-action (:arity 3 :index :hash)
  ;; Space
  (space converting next-candidate)
  (space preedit   start-conversion)
  (space normal    self-insert)
  ;; Return
  (return converting commit-candidate)
  (return normal   newline)
  ;; Cancel
  (cancel converting cancel-conversion)
  (cancel preedit   cancel-preedit)
  (cancel normal    keyboard-quit)
  ;; X
  (x converting previous-candidate)
  (x normal    self-insert)
  ;; C-n
  (ctrl-n converting kakutei-then-next-line)
  (ctrl-n preedit    next-line)
  (ctrl-n normal     next-line)
  ;; C-p
  (ctrl-p converting kakutei-then-previous-line)
  (ctrl-p preedit    previous-line)
  (ctrl-p normal     previous-line)
  ;; C-f / right-arrow
  (ctrl-f converting kakutei-then-forward)
  (ctrl-f preedit    forward-char)
  (ctrl-f normal     forward-char)
  ;; C-b / left-arrow
  (ctrl-b converting kakutei-then-backward)
  (ctrl-b preedit    backward-char)
  (ctrl-b normal     backward-char)
  ;; C-a / home-key
  (ctrl-a converting kakutei-then-bol)
  (ctrl-a preedit    beginning-of-line)
  (ctrl-a normal     beginning-of-line)
  ;; C-e / end-key
  (ctrl-e converting kakutei-then-eol)
  (ctrl-e preedit    end-of-line)
  (ctrl-e normal     end-of-line)
  ;; Backspace
  (backspace preedit    delete-preedit-char)
  (backspace converting cancel-conversion)
  (backspace normal     backward-delete)
  ;; Tab (dynamic completion)
  (tab preedit    dynamic-complete)
  (tab converting pass-through)
  (tab normal     pass-through))

;;;; Preedit Marker Mode Rules
;; Modes where a set conversion-start marker indicates preedit state:
;; abbrev, hiragana, katakana, katakana-半角.
;; Ground facts allow O(1) hash lookup (avoids variable-head rule limitations).
(nskk-prolog-define-fact-table preedit-marker-mode (:arity 1 :index :hash)
  (abbrev)
  (hiragana)
  (katakana)
  (katakana-半角))

;;;; State Detection

(defun nskk--key-state-base ()
  "Internal helper: return base key dispatch state, or nil if neither active.
Inspects `nskk-converting-p' and `nskk--has-preedit' in priority order:
returns `converting' when the ▼ phase is active, `preedit' when the ▽ phase
is active, and nil otherwise (callers are responsible for further
classification).
Called by `nskk--current-key-state' and `nskk--current-kakutei-state'."
  (cond
   ((nskk-converting-p) 'converting)
   ((nskk--has-preedit) 'preedit)
   (t nil)))

(defun nskk--current-key-state ()
  "Return current key dispatch state.
Returns one of the symbols `converting', `preedit', or `normal'.

`preedit' means the preedit phase is active, NOT necessarily that
there is non-empty preedit text.  Specifically:
- Standard preedit (hiragana/katakana ▽): `nskk--has-preedit' is true,
  i.e. there is at least one character between the ▽ marker and point.
- Abbrev preedit: returned whenever mode is `abbrev' and the conversion
  start marker is set.
- Japanese preedit (hiragana/katakana ▽): returned when marker is set,
  even if no characters have been typed yet.
Both cases use the `preedit-marker-mode/1' Prolog predicate.

This distinction mirrors DDSKK: in `skk-abbrev-mode-map', SPC is bound
directly to `skk-start-henkan' with no runtime text-presence check."
  (or (nskk--key-state-base)
      ;; Mode with marker set: preedit-marker-mode/1 covers abbrev and
      ;; Japanese modes (hiragana/katakana) — DDSKK-compatible.
      (when (and nskk-current-state
                 (nskk--get-conversion-start)
                 (nskk-prolog-query-one
                  `(preedit-marker-mode ,(nskk-state-mode nskk-current-state))))
        'preedit)
      'normal))

(defun nskk--current-kakutei-state ()
  "Return kakutei dispatch state for `kakutei-action/2' Prolog query.
States (in priority order):
  `converting'     -- henkan-active (▼ phase)
  `preedit'        -- henkan-on (▽ phase)
  `romaji-pending' -- incomplete romaji in `nskk--romaji-buffer'
  `hiragana-idle'  -- hiragana mode, no pending input
  `katakana-idle'  -- katakana/katakana-半角 mode, no pending input
  `direct-idle'    -- ascii/latin/jisx0208-latin/abbrev, no pending input"
  (or (nskk--key-state-base)
      (and (boundp 'nskk--romaji-buffer)
           (not (string-empty-p nskk--romaji-buffer))
           'romaji-pending)
      (and nskk-current-state
           (eq (nskk-state-mode nskk-current-state) 'hiragana)
           'hiragana-idle)
      (and nskk-current-state
           (nskk-prolog-query-one
            `(japanese-mode ,(nskk-state-mode nskk-current-state)))
           'katakana-idle)
      'direct-idle))

;;;; Internal Macros

(defun nskk--japanese-mode-active-p ()
  "Return non-nil if the current NSKK mode is a Japanese input mode.
Queries the `japanese-mode/1' Prolog predicate for the mode stored in
`nskk-current-state'.  Returns nil when state is unset."
  (and nskk-current-state
       (nskk-prolog-query-one
        `(japanese-mode ,(nskk-state-mode nskk-current-state)))))

(defmacro nskk-with-japanese-mode (action &rest fallback)
  "Execute ACTION if currently in Japanese input mode.
Performs implicit kakutei (確定) if input state is active:
- If converting (▼ active): commit the current candidate first.
- If preedit (▽ active) in a Japanese mode (hiragana, katakana or
  katakana-半角): commit preedit kana as-is via `nskk-henkan-kakutei',
  then execute ACTION.  This matches ddskk behaviour where pressing a
  mode-switch key (l, L, /, q) during preedit commits the kana and
  exits preedit before switching modes.  In abbrev mode the preedit
  buffer holds raw ASCII, not kana, so mode-switch keys fall through
  to FALLBACK (typically `self-insert-command').
- If in a Japanese mode (hiragana or katakana) but idle: execute
  ACTION directly.
Otherwise execute FALLBACK forms (typically `self-insert-command')."
  (declare (indent 1) (debug t))
  `(cond
    ((nskk-converting-p)
     (nskk-commit-current)
     ,action)
    ((and (nskk--has-preedit)
          (nskk--japanese-mode-active-p))
     (nskk-henkan-kakutei)
     ,action)
    ((nskk--japanese-mode-active-p)
     ,action)
    (t ,@fallback)))

(defmacro nskk--safe-nav-command (cmd error-type)
  "Call CMD interactively, silently ignoring ERROR-TYPE signals.
Internal macro for navigation commands that may signal buffer-boundary errors.
CMD should be a quoted command symbol (e.g., #\\='next-line).
ERROR-TYPE should be an error symbol (e.g., end-of-buffer)."
  (declare (indent 0) (debug t))
  `(condition-case nil
       (call-interactively ,cmd)
     (,error-type nil)))

(defmacro nskk-define-key-handler (key docstring &rest clauses)
  "Define an interactive Prolog-dispatched key handler for KEY.
DOCSTRING is the function documentation string.
CLAUSES are `pcase' match arms applied to the action symbol returned
by the `key-action/3' Prolog query for KEY.

The generated function is named `nskk-handle-KEY' and is interactive.
It calls `nskk--current-key-state' to determine the current dispatch
state (converting, preedit, or normal) before querying Prolog.

If the Prolog query returns nil (no matching rule), the wildcard `_'
arm in CLAUSES fires.  Every key-action rule set must be complete for
all three states (converting, preedit, normal) to avoid silent nil
dispatches masking missing rules."
  (declare (indent 2) (debug t))
  (let ((fn-name (intern (format "nskk-handle-%s" key))))
    `(defun ,fn-name ()
       ,docstring
       (interactive)
       (let* ((state (nskk--current-key-state))
              (action (nskk-prolog-query-value
                       (list 'key-action ',key state '\?a) '\?a)))
         (pcase action
           ,@clauses)))))

(defun nskk-handle-q ()
  "Handle q key: toggle between hiragana and katakana (or AZIK romaji dispatch).
In henkan-active mode (▼), perform implicit kakutei first.
When AZIK is active and pending-romaji+q is a complete hash match
\(e.g. kq→かい), delegates to `nskk-handle-q-key' to fire the AZIK rule.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (nskk-with-japanese-mode (nskk-handle-q-key)
    (self-insert-command 1)))

(defun nskk-handle-l ()
  "Handle l key: switch to ASCII mode (or AZIK romaji dispatch).
In henkan-active mode (▼), perform implicit kakutei first.
When AZIK is active and pending-romaji+l is a complete hash match
\(e.g. hl→ほん), fires the AZIK rule via `nskk-process-japanese-input'
instead of switching to latin mode.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (let* ((style (if (eq nskk-converter-romaji-style 'azik) 'azik 'standard))
         (buf-state (if (nskk--azik-complete-match-p ?l) 'azik-complete 'other))
         (action (nskk-prolog-query-value
                  `(l-key-action ,style ,buf-state ,'\?a) '\?a)))
    (pcase action
      ('fire-romaji (nskk-process-japanese-input ?l 1))
      ('latin-mode  (nskk-with-japanese-mode (nskk-set-mode-latin)
                      (self-insert-command 1)))
      (_            (self-insert-command 1)))))

(defun nskk-handle-upper-l ()
  "Handle L key: switch to full-width latin (jisx0208-latin) mode.
In henkan-active mode (▼), perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (nskk-with-japanese-mode (nskk-set-mode-jisx0208-latin)
    (self-insert-command 1)))

(defun nskk-handle-slash ()
  "Handle / key: enter abbrev mode from Japanese mode.
In henkan-active mode (▼), perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (nskk-with-japanese-mode (nskk-set-mode-abbrev)
    (self-insert-command 1)))

(nskk-define-key-handler x
  "Handle x key: select previous candidate.
In conversion mode (▼), moves to the previous candidate.
Otherwise delegates to `nskk-self-insert' so that romaji multi-char
sequences (e.g. xa → ぁ) are handled correctly in Japanese modes."
  ('previous-candidate (nskk-previous-candidate))
  (_ (nskk-self-insert 1)))

(nskk-define-key-handler space
  "Handle SPC key: start conversion or select next candidate.
In conversion mode (▼), moves to the next candidate.
In preedit mode (▽), initiates dictionary conversion.
Otherwise delegates to `nskk-self-insert' so that mode-based routing
applies: jisx0208-latin produces ideographic space (U+3000); all other
modes insert a literal ASCII space."
  ('next-candidate (nskk-next-candidate))
  ('start-conversion (nskk-start-conversion))
  (_ (nskk-self-insert 1)))

(nskk-define-key-handler return
  "Handle RET key: commit current conversion or insert newline.
In conversion mode (▼), commits the selected candidate without inserting
a newline.  In preedit mode (▽) or normal mode, inserts a newline
unconditionally (preedit text is left in place)."
  ('commit-candidate (nskk-commit-current))
  (_ (newline)))

(nskk-define-key-handler ctrl-n
  "Handle C-n/down-arrow: commit conversion then move to next line.
In conversion mode (▼), commits the current candidate then moves to next line.
In preedit mode (▽) or normal mode, delegates to \\[next-line]."
  ('kakutei-then-next-line (nskk-commit-current)
                           (nskk--safe-nav-command #'next-line end-of-buffer))
  ('next-line              (nskk--safe-nav-command #'next-line end-of-buffer)))

(nskk-define-key-handler ctrl-p
  "Handle C-p/up-arrow: commit conversion then move to previous line.
In conversion mode (▼), commits the current candidate then moves to previous
line.  In preedit mode (▽) or normal mode, delegates to \\[previous-line]."
  ('kakutei-then-previous-line (nskk-commit-current)
                               (nskk--safe-nav-command #'previous-line beginning-of-buffer))
  ('previous-line              (nskk--safe-nav-command #'previous-line beginning-of-buffer)))

(nskk-define-key-handler ctrl-f
  "Handle C-f/right-arrow: commit conversion then move forward, else forward-char.
In conversion mode (▼), commits the current candidate then moves point forward.
In preedit mode (▽) or normal mode, delegates to \\[forward-char]."
  ('kakutei-then-forward (nskk-commit-current)
                         (nskk--safe-nav-command #'forward-char end-of-buffer))
  ('forward-char (nskk--safe-nav-command #'forward-char end-of-buffer)))

(nskk-define-key-handler ctrl-b
  "Handle C-b/left-arrow: commit conversion then move backward, else backward-char.
In conversion mode (▼), commits the current candidate then moves point backward.
In preedit mode (▽) or normal mode, delegates to \\[backward-char]."
  ('kakutei-then-backward (nskk-commit-current)
                          (nskk--safe-nav-command #'backward-char beginning-of-buffer))
  ('backward-char (nskk--safe-nav-command #'backward-char beginning-of-buffer)))

(nskk-define-key-handler ctrl-a
  "Handle C-a/Home: commit then go to beginning of line.
In conversion mode (▼), commits the current candidate then moves to BOL.
In preedit mode (▽) or normal mode, delegates to \\[beginning-of-line]."
  ('kakutei-then-bol (nskk-commit-current)
                     (call-interactively #'beginning-of-line))
  ;; beginning-of-line does not signal beginning-of-buffer; no error suppression needed
  ('beginning-of-line (call-interactively #'beginning-of-line)))

(nskk-define-key-handler ctrl-e
  "Handle C-e/End: commit then go to end of line.
In conversion mode (▼), commits the current candidate then moves to EOL.
In preedit mode (▽) or normal mode, delegates to \\[end-of-line]."
  ('kakutei-then-eol (nskk-commit-current)
                     (call-interactively #'end-of-line))
  ;; end-of-line does not signal end-of-buffer; no error suppression needed
  ('end-of-line (call-interactively #'end-of-line)))

(nskk-define-key-handler cancel
  "Handle C-g: cancel current conversion or preedit.
In conversion mode (▼), cancels conversion and restores kana reading to
buffer without re-entering preedit (▽) state.
In preedit mode (▽), discards preedit text and resets state.
Otherwise calls `keyboard-quit'."
  ('cancel-conversion (nskk-cancel-conversion-to-reading))
  ('cancel-preedit (nskk-cancel-preedit))
  (_ (keyboard-quit)))

(nskk-define-key-handler backspace
  "Handle DEL key: delete last preedit char or backward-delete-char.
In preedit mode (▽), deletes the last accumulated character from preedit.
If ▽ is present with no accumulated chars, cancels preedit entirely via
`nskk-cancel-preedit' (clears marker and resets all state).
In conversion mode (▼), cancels conversion and restores kana reading to
buffer without re-entering preedit (▽) state.
Otherwise delegates to `delete-char', silently ignoring
beginning-of-buffer errors so DEL on an empty buffer is a no-op."
  ('delete-preedit-char
   (let ((start (nskk--get-conversion-start)))
     (if (and start (> (point) (+ start (length nskk-henkan-on-marker))))
         (delete-char -1)
       ;; Empty preedit (▽ in buffer but no content after it).
       ;; Cancel the preedit entirely, clearing the marker and all state.
       (nskk-cancel-preedit))))
  ('cancel-conversion (nskk-cancel-conversion-to-reading))
  (_ (ignore-errors (delete-char -1))))

(nskk-define-key-handler tab
  "Handle TAB key: dynamic completion in preedit, otherwise pass through.
In preedit mode (\u25bd), searches the dictionary for keys matching the
current reading prefix and completes inline.  Repeated TAB cycles.
In other modes, delegates to `indent-for-tab-command'."
  ('dynamic-complete (nskk-dynamic-complete))
  (_ (indent-for-tab-command)))

(defun nskk-handle-hash ()
  "Handle # key: enter numeric input mode in Japanese mode.
In henkan-active mode (\u25bc), perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (nskk-with-japanese-mode (nskk-set-mode-numeric)
    (self-insert-command 1)))

;;;; AZIK Toggle Key Setup

(defun nskk--setup-azik-toggle-key ()
  "Set up AZIK toggle key binding based on keyboard type.
Binds @ for jp106 keyboard or [ for us101 keyboard to
`nskk-toggle-japanese-mode' in `nskk-mode-map'.
Does nothing if `nskk-azik-keyboard-type' is not bound (AZIK not loaded)."
  (when (and (boundp 'nskk-mode-map)
             (boundp 'nskk-azik-keyboard-type))
    (let ((key (pcase nskk-azik-keyboard-type
                 ('jp106 "@")
                 ('us101 "[")
                 (_ "@"))))
      (define-key nskk-mode-map (kbd key) #'nskk-toggle-japanese-mode))))

(provide 'nskk-keymap)

;;; nskk-keymap.el ends here
