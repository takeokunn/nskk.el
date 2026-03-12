;;; nskk-keymap.el --- Keymap definitions for NSKK -*- lexical-binding: t; -*-

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
;; - `key-action/3'            -- (key state action) dispatch rules
;; - `mode-switch-preaction/2' -- (class pre-action) implicit kakutei before mode-switch
;; - `preedit-marker-mode/1'   -- modes where marker set implies preedit state
;; - `l-key-action/3'          -- AZIK-aware dispatch for the l key
;; - `kakutei-idle-state/2'    -- mode -> kakutei idle-state classification
;; - `azik-toggle-key/2'       -- keyboard type -> AZIK toggle key string
;;
;; Key architecture:
;; - `nskk-define-key-handler'       -- macro that generates Prolog-dispatched
;;     interactive commands.  Queries `key-action/3' to decide the action
;;     based on the current dispatch state (converting, preedit, normal).
;; - `nskk-define-mode-switch-handler' -- macro for q/L// handlers.  Calls
;;     `nskk--with-japanese-mode/k' which performs implicit kakutei (kakutei)
;;     first via `mode-switch-preaction/2', then executes the mode action.
;;     Falls through to `self-insert-command' when not in a Japanese mode.
;; - `nskk--current-key-state'   -- returns `converting', `preedit', or
;;     `normal' for use by the dispatch system.
;; - `nskk--japanese-mode-class' -- returns `converting', `preedit-japanese',
;;     `idle-japanese', or `other' for use by mode-switch dispatch.
;;
;; Key public API (all interactive):
;;
;; Mode-switch handlers (via `nskk-define-mode-switch-handler' or manual defun/done):
;; - `nskk-handle-q'       -- convert preedit to opposite script, or toggle hiragana/katakana
;; - `nskk-handle-upper-l' -- switch to full-width latin mode
;; - `nskk-handle-slash'   -- enter abbrev mode
;; - `nskk-handle-hash'    -- enter numeric input mode
;;
;; Manually defined handler (AZIK-aware Prolog dispatch):
;; - `nskk-handle-l'       -- switch to ASCII mode (l-key-action/3 dispatch)
;;
;; Prolog-dispatched handlers (via `nskk-define-key-handler'):
;; - `nskk-handle-x'       -- previous candidate
;; - `nskk-handle-space'   -- start conversion / next candidate
;; - `nskk-handle-return'  -- commit candidate (no newline) / insert newline
;; - `nskk-handle-ctrl-n'  -- commit then next-line / next-line fallthrough
;; - `nskk-handle-ctrl-p'  -- commit then prev-line / previous-line fallthrough
;; - `nskk-handle-ctrl-f'  -- commit then forward-char / forward-char fallthrough
;; - `nskk-handle-ctrl-b'  -- commit then backward-char / backward-char fallthrough
;; - `nskk-handle-ctrl-a'  -- commit then beginning-of-line / beginning-of-line fallthrough
;; - `nskk-handle-ctrl-e'  -- commit then end-of-line / end-of-line fallthrough
;; - `nskk-handle-cancel'  -- cancel conversion / preedit / keyboard-quit
;; - `nskk-handle-backspace' -- delete preedit char / rollback conversion / backward-delete
;; - `nskk-handle-tab'     -- dynamic completion in preedit / pass-through

;;; Code:

(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-cps-macros)

;; Functions in nskk-input.el
(declare-function nskk-toggle-japanese-mode "nskk-input")
(declare-function nskk-set-mode-latin "nskk-input")
(declare-function nskk-set-mode-abbrev "nskk-input")
(declare-function nskk-set-mode-jisx0208-latin "nskk-input")
(declare-function nskk-self-insert "nskk-input")
(declare-function nskk-handle-q-key "nskk-input")
(declare-function nskk--azik-complete-match-p "nskk-input")
(declare-function nskk--romaji-has-match-p "nskk-input")
(declare-function nskk-process-japanese-input "nskk-input")
(declare-function nskk-set-mode-numeric "nskk-input")
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
(declare-function nskk-rollback-conversion "nskk-henkan")
(declare-function nskk-cancel-preedit "nskk-henkan")
(declare-function nskk-henkan-kakutei "nskk-henkan")
(declare-function nskk-henkan-kakutei-convert-script "nskk-henkan")
(declare-function nskk-dynamic-complete "nskk-henkan")
(defvar nskk-henkan-on-marker)
;; Variables in nskk-azik.el
(defvar nskk-azik-keyboard-type)

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
  ;; Note: no (return preedit ...) row -- the wildcard `_' in nskk-handle-return
  ;; fires for preedit, calling (newline).  This is intentional (DDSKK-compatible).
  ;; Cancel
  (cancel converting rollback-to-reading)
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
  (backspace converting rollback-to-reading)
  (backspace normal     backward-delete)
  ;; Tab (dynamic completion)
  (tab preedit    dynamic-complete)
  (tab converting pass-through)
  (tab normal     pass-through))

;;;; Preedit Marker Mode Rules
;; Modes where a set conversion-start marker indicates preedit state:
;; abbrev, hiragana, katakana, katakana-han.
;; Ground facts allow O(1) hash lookup (avoids variable-head rule limitations).
(nskk-prolog-define-fact-table preedit-marker-mode (:arity 1 :index :hash)
  (abbrev)
  (hiragana)
  (katakana)
  (katakana-半角))

;;;; L-Key Dispatch Rules

;; l-key-action/3: AZIK-aware dispatch for the l key.
;; (l-key-action STYLE BUF-STATE ACTION)
;; STYLE is `azik' or `standard' (from nskk-converter-romaji-style).
;; BUF-STATE is `azik-complete' when pending-romaji+l is a complete AZIK hash
;; match; `other' otherwise.  Standard mode uses a wildcard variable so any
;; buf-state maps to latin-mode.
(nskk-prolog-define-fact-table l-key-action (:arity 3 :index :hash)
  (azik azik-complete fire-romaji)
  (azik other         latin-mode)
  (standard \?buf     latin-mode))

;;;; Kakutei Idle-State Classification Rules

;; kakutei-idle-state/2: (MODE IDLE-STATE)
;; Maps a Japanese/direct mode symbol to its kakutei idle-state keyword.
;; Used by `nskk--current-kakutei-state' to classify the idle arm via a
;; single O(1) hash lookup instead of a 3-branch cond.
(nskk-prolog-define-fact-table kakutei-idle-state (:arity 2 :index :hash)
  (hiragana       hiragana-idle)
  (katakana       katakana-idle)
  (katakana-半角  katakana-idle)
  (ascii          direct-idle)
  (latin          direct-idle)
  (jisx0208-latin direct-idle)
  (abbrev         direct-idle)
  (numeric        direct-idle))

;;;; Mode-Switch Pre-Action Rules

;; mode-switch-preaction/2: (CLASS PRE-ACTION)
;; Maps a japanese-mode class (from `nskk--japanese-mode-class') to the
;; implicit kakutei pre-action to perform before a mode-switch key (q, l, L, /, #).
;; Queried by `nskk--with-japanese-mode/k'.
(nskk-prolog-define-fact-table mode-switch-preaction (:arity 2 :index :hash)
  (converting       commit-current)
  (preedit-japanese henkan-kakutei)
  (idle-japanese    noop)
  (other            fallback))

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
- Standard preedit (hiragana/katakana): `nskk--has-preedit' is true,
  i.e. there is at least one character between the marker and point.
- Abbrev preedit: returned whenever mode is `abbrev' and the conversion
  start marker is set.
- Japanese preedit (hiragana/katakana): returned when marker is set,
  even if no characters have been typed yet.
Both cases use the `preedit-marker-mode/1' Prolog predicate.

This distinction mirrors DDSKK: in `skk-abbrev-mode-map', SPC is bound
directly to `skk-start-henkan' with no runtime text-presence check."
  (or (nskk--key-state-base)
      ;; Mode with marker set: preedit-marker-mode/1 covers abbrev and
      ;; Japanese modes (hiragana/katakana) -- DDSKK-compatible.
      (when (and nskk-current-state
                 (nskk--get-conversion-start)
                 (nskk-prolog-holds-p
                  `(preedit-marker-mode ,(nskk-state-mode nskk-current-state))))
        'preedit)
      'normal))

(defun/k nskk--current-kakutei-state ()
  "Return kakutei dispatch state for `kakutei-action/2' Prolog query.
States (in priority order):
  `converting'     -- henkan-active (▼ phase)
  `preedit'        -- henkan-on (▽ phase)
  `romaji-pending' -- incomplete romaji in `nskk--romaji-buffer'
  `hiragana-idle'  -- hiragana mode, no pending input
  `katakana-idle'  -- katakana/katakana-han mode, no pending input
  `direct-idle'    -- ascii/latin/jisx0208-latin/abbrev, no pending input"
  (let ((base (nskk--key-state-base)))
    (cond
     (base (succeed base))
     ((and (boundp 'nskk--romaji-buffer)
           (not (string-empty-p nskk--romaji-buffer)))
      (succeed 'romaji-pending))
     (t
      ;; Idle arm: classify by mode via `kakutei-idle-state/2' Prolog query.
      ;; Falls back to `direct-idle' when no fact matches (unknown mode).
      (succeed (or (and nskk-current-state
                        (nskk-prolog-query-value
                         `(kakutei-idle-state ,(nskk-state-mode nskk-current-state) \?s)
                         '\?s))
                   'direct-idle))))))

(defun nskk--japanese-mode-active-p ()
  "Return non-nil if the current NSKK mode is a Japanese input mode.
Queries the `japanese-mode/1' Prolog predicate for the mode stored in
`nskk-current-state'.  Returns nil when state is unset."
  (and nskk-current-state
       (nskk-prolog-holds-p
        `(japanese-mode ,(nskk-state-mode nskk-current-state)))))

(defun nskk--japanese-mode-class ()
  "Return mode classification for mode-switch key dispatch.
Returns one of:
  `converting'       -- henkan-active (▼ phase)
  `preedit-japanese' -- preedit active in a Japanese mode (hiragana/katakana)
  `idle-japanese'    -- Japanese mode (hiragana/katakana), no active preedit
  `other'            -- ASCII/latin/abbrev mode, or no state

Note: abbrev preedit maps to `other', not `preedit-japanese', because
abbrev preedit holds raw ASCII -- mode-switch keys fall through to
`self-insert-command' in that state."
  (let ((base (nskk--key-state-base)))
    (cond
     ((eq base 'converting) 'converting)
     ((and (eq base 'preedit) (nskk--japanese-mode-active-p)) 'preedit-japanese)
     ((nskk--japanese-mode-active-p) 'idle-japanese)
     (t 'other))))

;;;; Internal Macros

(defun nskk--execute-preaction (preact)
  "Execute the implicit kakutei pre-action PREACT before a mode-switch.
PREACT is one of: `commit-current' (commit candidate),
`henkan-kakutei' (commit preedit kana as-is), `noop' (no pre-action)."
  (pcase preact
    ('commit-current (nskk-commit-current))
    ('henkan-kakutei (nskk-henkan-kakutei))
    ('noop           nil)))

(defun/k nskk--with-japanese-mode ()
  "Dispatch mode-switch based on current Japanese mode class.
Queries `mode-switch-preaction/2' to determine the implicit kakutei
pre-action for the current mode class, executes it, then calls succeed.
Calls fail when not in a Japanese mode (`other' class maps to `fallback')."
  (let* ((class (nskk--japanese-mode-class))
         (preact (nskk-prolog-query-value
                  `(mode-switch-preaction ,class \?a) '\?a)))
    (pcase preact
      ('fallback (fail))
      (_ (nskk--execute-preaction preact) (succeed nil)))))

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

(defmacro nskk-define-mode-switch-handler (key docstring action)
  "Define an interactive mode-switch key handler for KEY.
DOCSTRING is the function documentation string.
ACTION is the Elisp form to execute after the implicit kakutei pre-action.

The generated function is named `nskk-handle-KEY' and is interactive.
It uses `nskk--with-japanese-mode/k' to perform implicit kakutei
as needed based on `mode-switch-preaction/2', then executes ACTION.
Falls through to `self-insert-command' when not in a Japanese input mode."
  (declare (indent 2) (debug t))
  (let ((fn-name (intern (format "nskk-handle-%s" key))))
    `(defun/done ,fn-name ()
       ,docstring
       :interactive t
       (nskk--with-japanese-mode/k
        (lambda (_) ,action)
        (lambda () (self-insert-command 1))))))

(defun/done nskk-handle-q ()
  "Handle q key: convert preedit kana to opposite script, or toggle mode.
In ▽ preedit phase (hiragana/katakana Japanese mode):
  - AZIK mode: delegates to `nskk-handle-q-key' so that AZIK romaji rules
    take priority (e.g. \"tq\" → \"たい\") and standalone q inserts ん.
    `nskk-henkan-kakutei-convert-script' is NOT called; in AZIK the toggle
    key is @ (jp106) or [ (us101), not q.
  - Standard mode: converts the accumulated kana to the opposite script via
    `nskk-henkan-kakutei-convert-script' and commits without changing the
    input mode (DDSKK-compatible: q in ▽ preedit converts script only; mode
    toggle is suppressed).
In henkan-active (▼) mode: performs implicit kakutei first via
`mode-switch-preaction/2', then delegates to `nskk-handle-q-key'.
In idle Japanese mode: delegates to `nskk-handle-q-key' (hiragana↔katakana
toggle, or AZIK romaji dispatch when pending-romaji+q is a complete hash match).
In ASCII mode or when NSKK state is inactive, falls through to
`self-insert-command'."
  :interactive t
  (if (eq (nskk--japanese-mode-class) 'preedit-japanese)
      (if (eq nskk-converter-romaji-style 'azik)
          (nskk-handle-q-key)
        (nskk-henkan-kakutei-convert-script))
    (nskk--with-japanese-mode/k
     (lambda (_) (nskk-handle-q-key))
     (lambda () (self-insert-command 1)))))

(defun nskk--l-key-dispatch-state ()
  "Return (STYLE . BUF-STATE) cons for l-key Prolog dispatch.
STYLE is `azik' if `nskk-converter-romaji-style' is `azik', else
`standard'.  BUF-STATE is `azik-complete' when STYLE is `azik' and
the current pending-romaji+l sequence is a complete AZIK hash match
\(via `nskk--azik-complete-match-p'); `other' otherwise.
Used by `nskk-handle-l' to query `l-key-action/3'."
  (let ((style (if (eq nskk-converter-romaji-style 'azik) 'azik 'standard)))
    (cons style
          (if (and (eq style 'azik) (nskk--azik-complete-match-p ?l))
              'azik-complete
            'other))))

(defun/done nskk-handle-l ()
  "Handle l key: switch to ASCII mode, or fire romaji rule when pending.
In AZIK mode, `nskk--azik-complete-match-p' is checked first so the AZIK
table takes priority even when the romaji buffer is empty (e.g. a
standalone \\='l\\=' -> kana custom rule would be honoured).
When pending romaji combined with l forms a complete conversion rule
\(e.g. \"zl\" -> \"->\" in standard mode, or AZIK multi-char rules like
\"hl\" -> \"ほん\"), fires the romaji processor instead of switching to
latin mode.
In henkan-active mode, perform implicit kakutei first when not routing
to romaji.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  :interactive t
  (if (or (nskk--azik-complete-match-p ?l)
          (nskk--romaji-has-match-p ?l))
      (nskk-process-japanese-input ?l 1)
    (nskk--with-japanese-mode/k
     (lambda (_) (nskk-set-mode-latin))
     (lambda () (self-insert-command 1)))))

(nskk-define-mode-switch-handler upper-l
  "Handle L key: switch to full-width latin (jisx0208-latin) mode.
In henkan-active mode, perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (nskk-set-mode-jisx0208-latin))

(defun/done nskk-handle-slash ()
  "Handle / key: enter abbrev mode, or fire romaji rule when pending.
When pending romaji combined with / forms a complete conversion rule
\(e.g. \"z\" + \"/\" → \"・\"), fires the romaji processor instead.
In henkan-active mode, perform implicit kakutei first when not routing
to romaji.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  :interactive t
  (if (nskk--romaji-has-match-p ?/)
      (nskk-process-japanese-input ?/ 1)
    (nskk--with-japanese-mode/k
     (lambda (_) (nskk-set-mode-abbrev))
     (lambda () (self-insert-command 1)))))

(nskk-define-key-handler x
  "Handle x key: select previous candidate.
In conversion mode, moves to the previous candidate.
Otherwise delegates to `nskk-self-insert' so that romaji multi-char
sequences (e.g. xa -> small-a) are handled correctly in Japanese modes."
  ('previous-candidate (nskk-previous-candidate))
  (_ (nskk-self-insert 1)))

(nskk-define-key-handler space
  "Handle SPC key: start conversion or select next candidate.
In conversion mode, moves to the next candidate.
In preedit mode, initiates dictionary conversion.
Otherwise delegates to `nskk-self-insert' so that mode-based routing
applies: jisx0208-latin produces ideographic space (U+3000); all other
modes insert a literal ASCII space."
  ('next-candidate (nskk-next-candidate))
  ('start-conversion (nskk-start-conversion))
  (_ (nskk-self-insert 1)))

(nskk-define-key-handler return
  "Handle RET key: commit current conversion or insert newline.
In conversion mode, commits the selected candidate without inserting
a newline.  In preedit mode or normal mode, inserts a newline
unconditionally (preedit text is left in place)."
  ('commit-candidate (nskk-commit-current))
  (_ (newline)))

(nskk-define-key-handler ctrl-n
  "Handle C-n/down-arrow: commit conversion then move to next line.
In conversion mode, commits the current candidate then moves down.
In preedit mode or normal mode, delegates to \\[next-line]."
  ('kakutei-then-next-line (nskk-commit-current)
                           (nskk--safe-nav-command #'next-line end-of-buffer))
  ('next-line      (nskk--safe-nav-command #'next-line end-of-buffer)))

(nskk-define-key-handler ctrl-p
  "Handle C-p/up-arrow: commit conversion then move to previous line.
In conversion mode, commits the current candidate then moves up.
In preedit mode or normal mode, delegates to \\[previous-line]."
  ('kakutei-then-previous-line (nskk-commit-current)
                               (nskk--safe-nav-command #'previous-line beginning-of-buffer))
  ('previous-line      (nskk--safe-nav-command #'previous-line beginning-of-buffer)))

(nskk-define-key-handler ctrl-f
  "Handle C-f/right-arrow: commit conversion then move forward, else forward-char.
In conversion mode, commits the current candidate then moves point forward.
In preedit mode or normal mode, delegates to \\[forward-char]."
  ('kakutei-then-forward (nskk-commit-current)
                         (nskk--safe-nav-command #'forward-char end-of-buffer))
  ('forward-char (nskk--safe-nav-command #'forward-char end-of-buffer)))

(nskk-define-key-handler ctrl-b
  "Handle C-b/left-arrow: commit conversion then move backward, else backward-char.
In conversion mode, commits the current candidate then moves point backward.
In preedit mode or normal mode, delegates to \\[backward-char]."
  ('kakutei-then-backward (nskk-commit-current)
                          (nskk--safe-nav-command #'backward-char beginning-of-buffer))
  ('backward-char (nskk--safe-nav-command #'backward-char beginning-of-buffer)))

(nskk-define-key-handler ctrl-a
  "Handle C-a/Home: commit then go to beginning of line.
In conversion mode, commits the current candidate then moves to BOL.
In preedit mode or normal mode, delegates to \\[beginning-of-line]."
  ('kakutei-then-bol (nskk-commit-current)
                     (call-interactively #'beginning-of-line))
  ;; beginning-of-line does not signal beginning-of-buffer; no error suppression needed
  ('beginning-of-line (call-interactively #'beginning-of-line)))

(nskk-define-key-handler ctrl-e
  "Handle C-e/End: commit then go to end of line.
In conversion mode, commits the current candidate then moves to EOL.
In preedit mode or normal mode, delegates to \\[end-of-line]."
  ('kakutei-then-eol (nskk-commit-current)
                     (call-interactively #'end-of-line))
  ;; end-of-line does not signal end-of-buffer; no error suppression needed
  ('end-of-line (call-interactively #'end-of-line)))

(nskk-define-key-handler cancel
  "Handle C-g: cancel current conversion or preedit (DDSKK-compatible).
In conversion mode, rolls back to preedit state so the user
can edit the reading or re-convert.
In preedit mode, discards preedit text and resets state entirely.
Otherwise calls `keyboard-quit'."
  ('rollback-to-reading (nskk-cancel-conversion-to-reading))
  ('cancel-preedit (nskk-cancel-preedit))
  (_ (keyboard-quit)))

(defun nskk--backspace-in-preedit ()
  "Delete the last preedit character, or cancel preedit if empty.
Called when backspace is pressed in preedit state.
If point is beyond the marker content area, deletes one character
backward with `delete-char'.  If the preedit area is empty (marker in
buffer but no characters typed yet), cancels the entire preedit via
`nskk-cancel-preedit'."
  (let ((start (nskk--get-conversion-start)))
    (if (and start (> (point) (+ start (length nskk-henkan-on-marker))))
        (delete-char -1)
      (nskk-cancel-preedit))))

(nskk-define-key-handler backspace
  "Handle DEL key: delete last preedit char or backward-delete-char.
In preedit mode, deletes the last accumulated character from preedit.
If the marker is present with no accumulated chars, cancels preedit entirely via
`nskk-cancel-preedit' (clears marker and resets all state).
In conversion mode, rolls back to preedit state (DDSKK-compatible)
so the user can edit the reading and re-convert.
Otherwise delegates to `delete-char', silently ignoring
beginning-of-buffer errors so DEL on an empty buffer is a no-op."
  ('delete-preedit-char (nskk--backspace-in-preedit))
  ('rollback-to-reading (nskk-cancel-conversion-to-reading))
  (_ (ignore-errors (delete-char -1))))

(nskk-define-key-handler tab
  "Handle TAB key: dynamic completion in preedit, otherwise pass through.
In preedit mode, searches the dictionary for keys matching the
current reading prefix and completes inline.  Repeated TAB cycles.
In other modes, delegates to `indent-for-tab-command'."
  ('dynamic-complete (nskk-dynamic-complete))
  (_ (indent-for-tab-command)))

(nskk-define-mode-switch-handler hash
  "Handle # key: enter numeric input mode in Japanese mode.
In henkan-active mode, perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (nskk-set-mode-numeric))

;;;; AZIK Toggle Key Setup

;; azik-toggle-key/2: (KEYBOARD-TYPE KEY-STRING)
;; Maps keyboard type symbol to the toggle key string for AZIK mode.
;; Only jp106 and us101 are enumerated; unrecognized types fall back to "@"
;; at the Elisp level (no fact is asserted for them).
(nskk-prolog-define-fact-table azik-toggle-key (:arity 2 :index :hash)
  (jp106 "@")
  (us101 "["))

(defun nskk--setup-azik-toggle-key ()
  "Set up AZIK toggle key binding based on keyboard type.
Binds @ for jp106 keyboard or [ for us101 keyboard to
`nskk-toggle-japanese-mode' in `nskk-mode-map'.
Falls back to `@' for unrecognized keyboard types.
Does nothing if `nskk-azik-keyboard-type' is not bound (AZIK not loaded)."
  (when (and (boundp 'nskk-mode-map)
             (boundp 'nskk-azik-keyboard-type))
    (let ((key (or (nskk-prolog-query-value
                    `(azik-toggle-key ,nskk-azik-keyboard-type \?k) '\?k)
                   "@")))
      (keymap-set nskk-mode-map key #'nskk-toggle-japanese-mode))))

(provide 'nskk-keymap)

;;; nskk-keymap.el ends here
