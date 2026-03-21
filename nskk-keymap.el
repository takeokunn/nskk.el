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

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

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
;; q, l, L, /, x, SPC, RET, C-g, C-n, C-p, DEL, TAB, #.  Each handler checks the current
;; NSKK conversion state before acting, falling through to `self-insert-command'
;; or `keyboard-quit' when NSKK is in ASCII mode or state is inactive.
;;
;; Prolog predicates maintained by this module:
;; - `key-action/3'            -- (key state action) dispatch rules
;; - `key-state-map/2'         -- (rich-state simple-state) classification mapping
;; - `mode-class-map/2'        -- (rich-state mode-class) classification mapping
;; - `q-key-dispatch/3'        -- (class style action) q-key dispatch
;; - `mode-switch-preaction/2' -- (class pre-action) implicit kakutei before mode-switch
;; - `state-classify/4'        -- (phase text-presence mode-category classification)
;; - `kakutei-active-state/3'  -- (classification text-presence kakutei-state)
;; - `l-key-action/3'          -- AZIK-aware dispatch for the l key
;; - `kakutei-idle-state/2'    -- mode -> kakutei idle-state classification
;;
;; Key architecture:
;; - `nskk-define-key-handler'       -- macro that generates Prolog-dispatched
;;     interactive commands.  Queries `key-action/3' to decide the action
;;     based on the current dispatch state (converting, preedit, normal).
;; - `nskk-define-mode-switch-handler' -- macro for q/L// handlers.  Calls
;;     `nskk--with-japanese-mode/k' which performs implicit kakutei (kakutei)
;;     first via `mode-switch-preaction/2', then executes the mode action.
;;     Falls through to `self-insert-command' when not in a Japanese mode.
;; - `nskk--classify-state'      -- returns a rich 6-value classification via
;;     `state-classify/4' Prolog table lookup on three orthogonal features
;;     (`converting', `preedit-japanese', `preedit-pending',
;;     `preedit-marker', `idle-japanese', `idle-direct') as the single
;;     source of truth for all state queries.
;; - `nskk--current-key-state'   -- returns `converting', `preedit', or
;;     `normal' via `key-state-map/2' Prolog lookup.
;; - `nskk--japanese-mode-class' -- returns `converting', `preedit-japanese',
;;     `idle-japanese', or `other' via `mode-class-map/2' Prolog lookup.
;; - `nskk-define-nav-handler'   -- macro that generates commit-then-navigate
;;     handlers, reducing 6 repetitive `nskk-define-key-handler' expansions.
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
;; - `nskk-handle-l'       -- candidate selection, romaji rule, or ASCII mode switch
;;                            (via nskk--handle-l-action / l-key-action/3 dispatch)
;; - `nskk-handle-upper-x' -- purge candidate from dictionary (X key)
;;
;; Prolog-dispatched handlers (via `nskk-define-key-handler'):
;; - `nskk-handle-x'       -- previous candidate
;; - `nskk-handle-space'   -- start conversion / next candidate
;; - `nskk-handle-return'  -- commit candidate (no newline) / insert newline
;; - `nskk-handle-ctrl-n'  -- commit then next-line / next-line fallthrough
;; - `nskk-handle-ctrl-p'  -- previous candidate (in conversion) / commit then prev-line / previous-line fallthrough
;; - `nskk-handle-ctrl-f'  -- commit then forward-char / forward-char fallthrough
;; - `nskk-handle-ctrl-b'  -- commit then backward-char / backward-char fallthrough
;; - `nskk-handle-ctrl-a'  -- commit then beginning-of-line / beginning-of-line fallthrough
;; - `nskk-handle-ctrl-e'  -- commit then end-of-line / end-of-line fallthrough
;; - `nskk-handle-cancel'  -- cancel conversion / preedit / keyboard-quit
;; - `nskk-handle-backspace' -- delete preedit char / rollback conversion / backward-delete
;; - `nskk-handle-tab'     -- dynamic completion in preedit / pass-through

;;; Code:

(require 'subr-x)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-cps-macros)

;; Functions in nskk-input.el
(declare-function nskk-set-mode-latin "nskk-input")
(declare-function nskk-set-mode-abbrev "nskk-input")
(declare-function nskk-set-mode-jisx0208-latin "nskk-input")
(declare-function nskk-self-insert "nskk-input")
(declare-function nskk-handle-q-key "nskk-input")
(declare-function nskk--azik-complete-match-p "nskk-input")
(declare-function nskk--romaji-has-match-p "nskk-input")
(declare-function nskk-process-japanese-input "nskk-input")
(declare-function nskk-set-mode-numeric "nskk-input")
(declare-function nskk--try-candidate-selection/k "nskk-input" (char on-found on-not-found))
(defvar nskk-mode)                    ;; Forward declaration from nskk.el
(defvar nskk-converter-romaji-style)
(defvar nskk--romaji-buffer)          ;; Forward declaration from nskk-state.el
;; AZIK deferred state variables from nskk-input.el
(defvar nskk--deferred-azik-state)
(defvar nskk--deferred-vowel-shadow-state)
(defvar nskk--azik-colon-okuri-pending)
(defvar nskk--azik-colon-okuri-deferred)
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
(declare-function nskk--clear-azik-pending-state "nskk-henkan")
(declare-function nskk--show-pending-romaji "nskk-henkan")
(declare-function nskk--clear-pending-romaji "nskk-henkan")
(declare-function nskk--reset-romaji-buffer "nskk-henkan")
(declare-function nskk-henkan-kakutei "nskk-henkan")
(declare-function nskk-henkan-kakutei-convert-script "nskk-henkan")
(declare-function nskk-dynamic-complete "nskk-henkan")
(declare-function nskk-purge-from-jisyo "nskk-henkan")
(defvar nskk-henkan-on-marker)
(defvar nskk-dcomp-style)

;;;; Prolog Key-Action Rules

;; Key dispatch rules: (key-action KEY STATE ACTION)
;; Order matters within each key: first match wins.
(nskk-prolog-define-fact-table key-action (:arity 3 :index :hash)
  ;; Space
  (space converting next-candidate)
  (space preedit   start-conversion)
  (space normal    self-insert)
  ;; Return
  (return converting       commit-candidate)
  (return preedit          kakutei-and-newline)
  (return normal           newline)
  ;; Cancel
  (cancel converting rollback-to-reading)
  (cancel preedit   cancel-preedit)
  (cancel normal    keyboard-quit)
  ;; X
  (x converting previous-candidate)
  (x normal    self-insert)
  ;; C-n
  (ctrl-n converting kakutei-then-next-line)
  (ctrl-n preedit    kakutei-then-next-line)
  (ctrl-n normal     next-line)
  ;; C-p: previous candidate in conversion, commit+nav in preedit
  (ctrl-p converting previous-candidate)
  (ctrl-p preedit    kakutei-then-previous-line)
  (ctrl-p normal     previous-line)
  ;; C-f / right-arrow
  (ctrl-f converting kakutei-then-forward)
  (ctrl-f preedit    kakutei-then-forward)
  (ctrl-f normal     forward-char)
  ;; C-b / left-arrow
  (ctrl-b converting kakutei-then-backward)
  (ctrl-b preedit    kakutei-then-backward)
  (ctrl-b normal     backward-char)
  ;; C-a / home-key
  (ctrl-a converting kakutei-then-bol)
  (ctrl-a preedit    kakutei-then-bol)
  (ctrl-a normal     beginning-of-line)
  ;; C-e / end-key
  (ctrl-e converting kakutei-then-eol)
  (ctrl-e preedit    kakutei-then-eol)
  (ctrl-e normal     end-of-line)
  ;; Backspace
  (backspace preedit    delete-preedit-char)
  (backspace converting rollback-to-reading)
  (backspace normal     backward-delete)
  ;; Tab (dynamic completion)
  (tab preedit    dynamic-complete)
  (tab converting pass-through)
  (tab normal     pass-through))

;;;; L-Key Dispatch Rules

;; l-key-action/3: AZIK-aware dispatch for the l key.
;; (l-key-action STYLE BUF-STATE ACTION)
;; STYLE is `azik' or `standard' (from nskk-converter-romaji-style).
;; BUF-STATE is `azik-complete' when pending-romaji+l forms a complete rule
;; match (AZIK hash or standard romaji rule); `other' otherwise.  Both styles
;; map `azik-complete' to fire-romaji (e.g. "zl" -> "->" in standard mode) and
;; `other' to latin-mode.
(nskk-prolog-define-fact-table l-key-action (:arity 3 :index :hash)
  (azik     azik-complete fire-romaji)
  (azik     other         latin-mode)
  (standard azik-complete fire-romaji)
  (standard other         latin-mode))

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
  ;; preedit-pending: uppercase trigger fired (▽ marker in buffer) but no kana
  ;; emitted yet.  Commit (remove ▽, clear state) before navigation or mode-switch
  ;; so the cursor can move freely and the next preedit starts from a clean state.
  (preedit-pending  henkan-kakutei)
  (idle-japanese    noop)
  (other            fallback))

;; mode-category/2 is defined in nskk-state.el (L0) alongside japanese-mode/1.
;; It maps input mode symbols to orthogonal categories (japanese, marker-mode, other).

;;;; Rich State Classification

;; key-state-map/2: Maps rich state classification to simple dispatch state.
;; Used by `nskk--current-key-state' to reduce the 6-value classification
;; to the 3-value dispatch state (converting, preedit, normal).
(nskk-prolog-define-fact-table key-state-map (:arity 2 :index :hash)
  (converting       converting)
  (preedit-japanese preedit)
  (preedit-pending  preedit)
  (preedit-marker   preedit)
  (idle-japanese    normal)
  (idle-direct      normal))

;; mode-class-map/2: Maps rich state classification to mode-switch class.
;; Used by `nskk--japanese-mode-class' to produce the class symbol
;; for `mode-switch-preaction/2' dispatch.
(nskk-prolog-define-fact-table mode-class-map (:arity 2 :index :hash)
  (converting       converting)
  (preedit-japanese preedit-japanese)
  (preedit-pending  preedit-pending)
  (preedit-marker   other)
  (idle-japanese    idle-japanese)
  (idle-direct      other))

;; state-classify/4: (PHASE TEXT-PRESENCE MODE-CATEGORY CLASSIFICATION)
;; The single source of truth for state classification, expressed as a
;; declarative decision table.  Maps three orthogonal feature dimensions
;; (phase, text-presence, mode-category) to the rich 6-value classification.
;; Queried by `nskk--classify-state'.
(nskk-prolog-define-fact-table state-classify (:arity 4 :index :hash)
  ;; converting: always `converting' regardless of text or mode
  (converting has-text japanese     converting)
  (converting has-text marker-mode  converting)
  (converting has-text other        converting)
  (converting no-text  japanese     converting)
  (converting no-text  marker-mode  converting)
  (converting no-text  other        converting)
  ;; henkan-on + japanese: text presence distinguishes preedit-japanese / preedit-pending
  (henkan-on  has-text japanese     preedit-japanese)
  (henkan-on  no-text  japanese     preedit-pending)
  ;; henkan-on + marker-mode (abbrev): always preedit-marker
  (henkan-on  has-text marker-mode  preedit-marker)
  (henkan-on  no-text  marker-mode  preedit-marker)
  ;; henkan-on + other: should not occur; fallback to idle-direct
  (henkan-on  has-text other        idle-direct)
  (henkan-on  no-text  other        idle-direct)
  ;; idle: mode-category determines japanese vs direct
  (idle       has-text japanese     idle-japanese)
  (idle       no-text  japanese     idle-japanese)
  (idle       has-text marker-mode  idle-direct)
  (idle       no-text  marker-mode  idle-direct)
  (idle       has-text other        idle-direct)
  (idle       no-text  other        idle-direct))

(defun nskk--compute-phase ()
  "Return the current henkan phase: `converting', `henkan-on', or `idle'.
Uses `nskk-converting-p' for converting detection (matches
`converting-phase/1' Prolog fact table), then checks
`nskk--get-conversion-start' for henkan-on.  Returns `idle' otherwise."
  (cond
   ((nskk-converting-p) 'converting)
   ((and nskk-current-state (nskk--get-conversion-start)) 'henkan-on)
   (t 'idle)))

(defun nskk--compute-text-presence ()
  "Return `has-text' if preedit text exists past the marker, `no-text' otherwise."
  (if (nskk--has-preedit) 'has-text 'no-text))

(defun nskk--compute-mode-category ()
  "Return the current mode category: `japanese', `marker-mode', or `other'.
Queries `mode-category/2' to map the current input mode to a category.
Returns `other' when no state exists."
  (if nskk-current-state
      (or (nskk-prolog-query-value
           `(mode-category ,(nskk-state-mode nskk-current-state) \?c) '\?c)
          'other)
    'other))

(defun nskk--classify-state ()
  "Return a rich state classification symbol for the current NSKK state.
Returns one of:
  `converting'       -- henkan-active (▼ phase)
  `preedit-japanese' -- preedit with kana text in a Japanese mode
  `preedit-pending'  -- ▽ marker set in a Japanese mode, but no kana
                        text emitted yet (first romaji still accumulating)
  `preedit-marker'   -- marker set in a marker-mode (abbrev)
  `idle-japanese'    -- Japanese mode, no active preedit
  `idle-direct'      -- ASCII/latin/abbrev idle, or no state

This is the single source of truth for state classification.  All other
classifiers (`nskk--current-key-state', `nskk--japanese-mode-class',
`nskk--current-kakutei-state') are derived from this function via Prolog
mapping tables.

Computes three orthogonal feature dimensions (phase, text-presence,
mode-category) and queries `state-classify/4' for the classification."
  (or (nskk-prolog-query-value
       `(state-classify ,(nskk--compute-phase)
                        ,(nskk--compute-text-presence)
                        ,(nskk--compute-mode-category)
                        \?c)
       '\?c)
      'idle-direct))

(defun nskk--current-key-state ()
  "Return current key dispatch state: `converting', `preedit', or `normal'.
Queries `key-state-map/2' to reduce the rich classification from
`nskk--classify-state' to a simple 3-value dispatch state."
  (or (nskk-prolog-query-value
       `(key-state-map ,(nskk--classify-state) \?s) '\?s)
      'normal))

;; kakutei-active-state/3: (CLASSIFICATION TEXT-PRESENCE KAKUTEI-STATE)
;; Maps (classify-state, text-presence) → kakutei state for active input.
;; Only converting and preedit states are listed; absent combinations
;; fall through to romaji-pending / idle-state checks.
;; `preedit-marker' with `has-text' maps to `preedit' (commit abbrev text);
;; `preedit-marker' with `no-text' is absent (falls through to idle).
(nskk-prolog-define-fact-table kakutei-active-state (:arity 3 :index :hash)
  (converting       has-text converting)
  (converting       no-text  converting)
  (preedit-japanese  has-text preedit)
  (preedit-japanese  no-text  preedit)
  (preedit-pending   has-text preedit)
  (preedit-pending   no-text  preedit)
  (preedit-marker    has-text preedit))

(defun/k nskk--current-kakutei-state ()
  "Return kakutei dispatch state for `kakutei-action/2' Prolog query.
States (in priority order):
  `converting'     -- henkan-active (▼ phase)
  `preedit'        -- henkan-on (▽ phase)
  `romaji-pending' -- incomplete romaji in `nskk--romaji-buffer'
  `hiragana-idle'  -- hiragana mode, no pending input
  `katakana-idle'  -- katakana/katakana-han mode, no pending input
  `direct-idle'    -- ascii/latin/jisx0208-latin/abbrev, no pending input

Queries `kakutei-active-state/3' for converting/preedit states, then
falls through to romaji-pending check, then to `kakutei-idle-state/2'."
  (let* ((cls  (nskk--classify-state))
         (text (nskk--compute-text-presence))
         (active (nskk-prolog-query-value
                  `(kakutei-active-state ,cls ,text \?s) '\?s)))
    (if active
        (succeed active)
      (if (and (boundp 'nskk--romaji-buffer)
               (not (string-empty-p nskk--romaji-buffer)))
          (succeed 'romaji-pending)
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
  `preedit-japanese' -- preedit with kana text in Japanese mode
  `preedit-pending'  -- ▽ marker in Japanese mode, no kana text yet
  `idle-japanese'    -- Japanese mode, no active preedit
  `other'            -- ASCII/latin/abbrev mode, or no state

Queries `mode-class-map/2' to map the rich classification from
`nskk--classify-state' to the mode-switch class symbol."
  (or (nskk-prolog-query-value
       `(mode-class-map ,(nskk--classify-state) \?c) '\?c)
      'other))

;;;; Commit-by-Phase Helper

(defun nskk--commit-by-phase ()
  "Commit the current NSKK state, dispatching based on classify-state.
Queries `mode-switch-preaction/2' for the action matching the current
state classification and executes it via `nskk--execute-preaction'.
No-op when classify-state maps to `noop' or `fallback'."
  (let ((preact (nskk-prolog-query-value
                 `(mode-switch-preaction ,(nskk--classify-state) \?a) '\?a)))
    (unless (eq preact 'fallback)
      (nskk--execute-preaction preact))))

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

;;;; Q-Key Dispatch Rules

;; q-key-dispatch/3: (CLASS STYLE ACTION)
;; Maps (classify-state class, romaji style) to q-key action.
;; All class+style combinations are enumerated since fact tables
;; do not support variables.
(nskk-prolog-define-fact-table q-key-dispatch (:arity 3 :index :hash)
  (preedit-japanese azik     fire-romaji)
  (preedit-japanese standard convert-script)
  (preedit-pending  azik     fire-romaji)
  (preedit-pending  standard convert-script)
  (converting       azik     mode-switch)
  (converting       standard mode-switch)
  (idle-japanese    azik     mode-switch)
  (idle-japanese    standard mode-switch)
  (idle-direct      azik     self-insert)
  (idle-direct      standard self-insert)
  (preedit-marker   azik     self-insert)
  (preedit-marker   standard self-insert))

(defun/done nskk-handle-q ()
  "Handle q key: convert preedit kana to opposite script, or toggle mode.
In ▽ preedit phase (hiragana/katakana Japanese mode):
  - AZIK mode: always delegates to `nskk-handle-q-key' so that AZIK romaji
    rules take priority (e.g. \"tq\" → \"たい\", standalone q → ん).
  - Standard mode: converts the accumulated kana to the opposite script via
    `nskk-henkan-kakutei-convert-script' and commits without changing the
    input mode.
In henkan-active (▼) mode: performs implicit kakutei first via
`mode-switch-preaction/2', then delegates to `nskk-handle-q-key'.
In idle Japanese mode: delegates to `nskk-handle-q-key' (AZIK ん insertion
or romaji dispatch when pending-romaji+q is a complete hash match).
In ASCII mode or when NSKK state is inactive, falls through to
`self-insert-command'.

Dispatched via `q-key-dispatch/3' Prolog table."
  :interactive t
  (let* ((cls (nskk--classify-state))
         (style (if (eq nskk-converter-romaji-style 'azik) 'azik 'standard))
         (action (nskk-prolog-query-value
                  `(q-key-dispatch ,cls ,style \?a) '\?a)))
    (pcase action
      ('fire-romaji     (if (and (eq cls 'preedit-japanese)
                                 (string-empty-p nskk--romaji-buffer)
                                 (not (eq style 'azik)))
                            (nskk-henkan-kakutei-convert-script)
                          (nskk-handle-q-key)))
      ('convert-script  (nskk-henkan-kakutei-convert-script))
      ('mode-switch     (let ((saved-romaji nskk--romaji-buffer))
                          (nskk--with-japanese-mode/k
                           (lambda (_)
                             (setq nskk--romaji-buffer saved-romaji)
                             (nskk-handle-q-key))
                           (lambda () (self-insert-command 1)))))
      (_                (self-insert-command 1)))))

(defun/done nskk--handle-l-action ()
  "Helper for `nskk-handle-l' mode-switch and fire-romaji actions.
Dispatched via `l-key-action/3' Prolog table (style, buf-state, action)."
  (let* ((style (if (eq nskk-converter-romaji-style 'azik) 'azik 'standard))
         (buf-state (if (or (nskk--azik-complete-match-p ?l)
                            (nskk--romaji-has-match-p ?l))
                        'azik-complete
                      'other))
         (action (nskk-prolog-query-value
                  `(l-key-action ,style ,buf-state ,'\?a) '\?a)))
    (pcase action
      ('fire-romaji (nskk-process-japanese-input ?l 1))
      ('latin-mode
       (nskk--with-japanese-mode/k
         (lambda (_) (nskk-set-mode-latin))
         (lambda () (self-insert-command 1))))
      (_ (self-insert-command 1)))))

(defun/done nskk-handle-l ()
  "Handle l key: candidate selection, romaji rule, or mode toggle.
In candidate list selection mode, the key acts as a selection key.
In idle Japanese mode, switches to ASCII (latin) mode or fires a romaji rule.
In ASCII mode or when NSKK state is inactive, falls through to
`self-insert-command'."
  :interactive t
  (nskk--try-candidate-selection/k ?l
    #'ignore
    (lambda () (nskk--handle-l-action))))

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

(defun nskk-handle-upper-x ()
  "Handle X key: purge current candidate from user dictionary.
In ▼ (conversion) mode, calls `nskk-purge-from-jisyo' to remove the
current candidate after user confirmation.
Otherwise delegates to `nskk-self-insert'."
  (interactive)
  (if (nskk-converting-p)
      (nskk-purge-from-jisyo)
    (nskk-self-insert 1)))

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
  "Handle RET key: commit current conversion or preedit, then insert newline.
In conversion mode, commits the selected candidate without inserting
a newline.  In preedit (▽) mode, commits the raw kana reading via
`nskk-henkan-kakutei' then inserts a newline (matching DDSKK behavior).
In normal mode, inserts a newline unconditionally."
  ('commit-candidate (nskk-commit-current))
  ('kakutei-and-newline
   (nskk-henkan-kakutei)
   (newline))
  (_ (newline)))

(defmacro nskk-define-nav-handler (key docstring kakutei-action nav-action nav-cmd &optional error-type)
  "Define a commit-then-navigate handler for KEY.
DOCSTRING is the function documentation string.
KAKUTEI-ACTION and NAV-ACTION are the Prolog action symbols.
NAV-CMD is the navigation command to call.
ERROR-TYPE, if non-nil, is the error to suppress via `nskk--safe-nav-command'."
  (declare (indent 2) (debug t))
  (let ((nav-form (if error-type
                      `(nskk--safe-nav-command ,nav-cmd ,error-type)
                    `(call-interactively ,nav-cmd))))
    `(nskk-define-key-handler ,key
       ,docstring
       (',kakutei-action (nskk--commit-by-phase) ,nav-form)
       (',nav-action ,nav-form))))

(nskk-define-nav-handler ctrl-n
  "Handle C-n/down-arrow: commit then move to next line.
In conversion (▼) or preedit (▽) mode, commits then moves down.
In normal mode, delegates to \\[next-line]."
  kakutei-then-next-line next-line #'next-line end-of-buffer)

(nskk-define-key-handler ctrl-p
  "Handle C-p/up-arrow: previous candidate or move to previous line.
In conversion (▼) mode, shows the previous candidate.
In preedit (▽) mode, commits then moves up.
In normal mode, delegates to \\[previous-line]."
  ('previous-candidate (nskk-previous-candidate))
  ('kakutei-then-previous-line
   (nskk--commit-by-phase)
   (nskk--safe-nav-command #'previous-line beginning-of-buffer))
  ('previous-line
   (nskk--safe-nav-command #'previous-line beginning-of-buffer)))

(nskk-define-nav-handler ctrl-f
  "Handle C-f/right-arrow: commit then move forward, else forward-char.
In conversion (▼) or preedit (▽) mode, commits then moves forward.
In normal mode, delegates to \\[forward-char]."
  kakutei-then-forward forward-char #'forward-char end-of-buffer)

(nskk-define-nav-handler ctrl-b
  "Handle C-b/left-arrow: commit then move backward, else backward-char.
In conversion (▼) or preedit (▽) mode, commits then moves backward.
In normal mode, delegates to \\[backward-char]."
  kakutei-then-backward backward-char #'backward-char beginning-of-buffer)

(nskk-define-nav-handler ctrl-a
  "Handle C-a/Home: commit then go to beginning of line.
In conversion (▼) or preedit (▽) mode, commits then moves to BOL.
In normal mode, delegates to \\[beginning-of-line]."
  kakutei-then-bol beginning-of-line #'beginning-of-line)

(nskk-define-nav-handler ctrl-e
  "Handle C-e/End: commit then go to end of line.
In conversion (▼) or preedit (▽) mode, commits then moves to EOL.
In normal mode, delegates to \\[end-of-line]."
  kakutei-then-eol end-of-line #'end-of-line)

(nskk-define-key-handler cancel
  "Handle C-g: cancel current conversion or preedit.
In conversion mode, rolls back to preedit (▽) state so the user
can edit the reading or re-convert.
In preedit mode, discards preedit text and resets state entirely.
Otherwise clears any residual AZIK deferred state and calls
`keyboard-quit'."
  ('rollback-to-reading (nskk-rollback-conversion))
  ('cancel-preedit (nskk-cancel-preedit))
  (_ (nskk--clear-azik-pending-state) (keyboard-quit)))

(defun nskk--backspace-retract-pending ()
  "Retract one pending input state if any is active.
Checks AZIK deferred state and romaji buffer in priority order:
DA > DV > CP > CD > romaji-buffer.
SP is excluded: it has no visible buffer artifact to retract.
Returns non-nil if backspace was consumed (caller must not delete further).
Caller is responsible for preedit boundary checks after retraction."
  (cond
   ;; 1. DA: deferred-azik-state -- delete tentative kana
   ((and (boundp 'nskk--deferred-azik-state) nskk--deferred-azik-state)
    (delete-char (- (length (cdr nskk--deferred-azik-state))))
    (setq nskk--deferred-azik-state nil)
    t)
   ;; 2. DV: deferred-vowel-shadow-state -- delete tentative kana
   ((and (boundp 'nskk--deferred-vowel-shadow-state)
         nskk--deferred-vowel-shadow-state)
    (delete-char (- (length (cdr nskk--deferred-vowel-shadow-state))))
    (setq nskk--deferred-vowel-shadow-state nil)
    t)
   ;; 3. CP: colon-okuri-pending -- delete `*' marker
   ((and (boundp 'nskk--azik-colon-okuri-pending)
         nskk--azik-colon-okuri-pending)
    (delete-char -1)
    (setq nskk--azik-colon-okuri-pending nil)
    t)
   ;; 4. CD: colon-okuri-deferred -- delete placeholder, reset romaji
   ((and (boundp 'nskk--azik-colon-okuri-deferred)
         nskk--azik-colon-okuri-deferred)
    (delete-char (- (length (cdr nskk--azik-colon-okuri-deferred))))
    (setq nskk--azik-colon-okuri-deferred nil)
    (nskk--reset-romaji-buffer)
    t)
   ;; 5. Non-empty romaji buffer -- delete last char, update overlay
   ((and (boundp 'nskk--romaji-buffer)
         (not (string-empty-p nskk--romaji-buffer)))
    (setq nskk--romaji-buffer (substring nskk--romaji-buffer 0 -1))
    (if (string-empty-p nskk--romaji-buffer)
        (nskk--clear-pending-romaji)
      (nskk--show-pending-romaji nskk--romaji-buffer))
    t)))

(defun nskk--backspace-in-preedit ()
  "Delete pending romaji, AZIK deferred state, or last preedit char.
Called when backspace is pressed in preedit state.
Priority: DA > DV > CP > CD > romaji-buffer > buffer text.
If point drifted left of preedit boundary, clamp it instead."
  (let* ((start (nskk--get-conversion-start))
         (preedit-min (and start (+ start (length nskk-henkan-on-marker)))))
    (when preedit-min
      (cond
       ((nskk--backspace-retract-pending)
        (when (<= (point) preedit-min) (nskk-cancel-preedit)))
       ;; Existing logic -- delete committed kana or cancel
       ((> (point) preedit-min)
        (delete-char -1))
       ((= (point) preedit-min)
        (nskk-cancel-preedit))
       (t
        (goto-char preedit-min))))))

(nskk-define-key-handler backspace
  "Handle DEL key: delete last preedit char or backward-delete-char.
In preedit mode, deletes the last accumulated character from preedit.
If the marker is present with no accumulated chars, cancels preedit entirely via
`nskk-cancel-preedit' (clears marker and resets all state).
In conversion mode, rolls back to preedit state so the user can
edit the reading and re-convert.
Otherwise delegates to `delete-char', silently ignoring
beginning-of-buffer errors so DEL on an empty buffer is a no-op."
  ('delete-preedit-char (nskk--backspace-in-preedit))
  ('rollback-to-reading (nskk-rollback-conversion))
  (_ (unless (nskk--backspace-retract-pending)
       (ignore-errors (delete-char -1)))))

(nskk-define-key-handler tab
  "Handle TAB key: dynamic completion in preedit, otherwise pass through.
In preedit mode, behavior depends on `nskk-dcomp-style':
  capf  -- triggers `completion-at-point' (works with corfu/company).
  cycle -- traditional inline cycling via `nskk-dynamic-complete'.
In other modes, delegates to the underlying major-mode TAB binding
\(e.g. `org-cycle' in Org mode).  Falls back to `indent-for-tab-command'
when no major-mode binding exists."
  ('dynamic-complete (if (eq nskk-dcomp-style 'capf)
                         (completion-at-point)
                       (nskk-dynamic-complete)))
  (_ (let ((cmd (let ((nskk-mode nil))
                  (key-binding "\t"))))
       (if (commandp cmd)
           (call-interactively cmd)
         (indent-for-tab-command)))))

(nskk-define-mode-switch-handler hash
  "Handle # key: enter numeric input mode in Japanese mode.
In henkan-active mode, perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (nskk-set-mode-numeric))

(provide 'nskk-keymap)

;;; nskk-keymap.el ends here
