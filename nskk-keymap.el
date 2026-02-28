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

;;; Code:

(require 'nskk-state)
(require 'nskk-prolog)

;; Functions in nskk-input.el
(declare-function nskk-toggle-japanese-mode "nskk-input")
(declare-function nskk-set-mode-latin "nskk-input")
(declare-function nskk-set-mode-abbrev "nskk-input")
(declare-function nskk-set-mode-jisx0208-latin "nskk-input")
(declare-function nskk-self-insert "nskk-input")
;; Functions in nskk-henkan.el
(declare-function nskk-converting-p "nskk-henkan")
(declare-function nskk--has-preedit "nskk-henkan")
(declare-function nskk--get-conversion-start "nskk-henkan")
(declare-function nskk-start-conversion "nskk-henkan")
(declare-function nskk-next-candidate "nskk-henkan")
(declare-function nskk-commit-current "nskk-henkan")
(declare-function nskk-previous-candidate "nskk-henkan")
(declare-function nskk-cancel-conversion "nskk-henkan")
(declare-function nskk-cancel-preedit "nskk-henkan")
(declare-function nskk-henkan-kakutei "nskk-henkan")
(defvar nskk-henkan-on-marker)

(defvar nskk-annotate-mode-map-hook nil
  "Hook run when annotating mode map.
DDSKK equivalent to `skk-annotate-mode-map-hook'.")

(defvar nskk-annotate-minibuffer-map-hook nil
  "Hook run when annotating minibuffer map.
DDSKK equivalent to `skk-annotate-minibuffer-map-hook'.")

;;;; Prolog Key-Action Rules

(nskk-prolog-set-index 'key-action 3 :hash)

;; Space key rules (order matters: first match wins with cut)
(nskk-prolog-<- (key-action space converting next-candidate))
(nskk-prolog-<- (key-action space preedit start-conversion))
(nskk-prolog-<- (key-action space normal self-insert))

;; Return key rules
(nskk-prolog-<- (key-action return converting commit-candidate))
(nskk-prolog-<- (key-action return normal newline))

;; Cancel key rules
(nskk-prolog-<- (key-action cancel converting cancel-conversion))
(nskk-prolog-<- (key-action cancel preedit cancel-preedit))
(nskk-prolog-<- (key-action cancel normal keyboard-quit))

;; X key rules
(nskk-prolog-<- (key-action x converting previous-candidate))
(nskk-prolog-<- (key-action x normal self-insert))

;; C-n key rules
(nskk-prolog-<- (key-action ctrl-n converting next-candidate))
(nskk-prolog-<- (key-action ctrl-n preedit   next-line))
(nskk-prolog-<- (key-action ctrl-n normal    next-line))

;; C-p key rules
(nskk-prolog-<- (key-action ctrl-p converting previous-candidate))
(nskk-prolog-<- (key-action ctrl-p preedit   previous-line))
(nskk-prolog-<- (key-action ctrl-p normal    previous-line))

;; C-f / right-arrow key rules: commit then forward-char
(nskk-prolog-<- (key-action ctrl-f converting kakutei-then-forward))
(nskk-prolog-<- (key-action ctrl-f preedit   forward-char))
(nskk-prolog-<- (key-action ctrl-f normal    forward-char))

;; C-b / left-arrow key rules: commit then backward-char
(nskk-prolog-<- (key-action ctrl-b converting kakutei-then-backward))
(nskk-prolog-<- (key-action ctrl-b preedit   backward-char))
(nskk-prolog-<- (key-action ctrl-b normal    backward-char))

;; Backspace key rules
(nskk-prolog-<- (key-action backspace preedit    delete-preedit-char))
(nskk-prolog-<- (key-action backspace converting cancel-conversion))
(nskk-prolog-<- (key-action backspace normal     backward-delete))

;;;; State Detection

(defun nskk--current-key-state ()
  "Return current key dispatch state.
Returns `converting', `preedit', or `normal'.
In abbrev mode with an active conversion start marker, always returns
`preedit' so that SPC triggers `nskk-start-conversion' regardless of
point position (DDSKK-compatible: SPC is directly bound to
`skk-start-henkan' in `skk-abbrev-mode-map', no dynamic check needed)."
  (cond
   ((nskk-converting-p) 'converting)
   ((nskk--has-preedit) 'preedit)
   ;; Abbrev mode with marker set: always treat as preedit context.
   ;; nskk-start-conversion guards on non-empty text internally,
   ;; so pressing SPC immediately after / (no text yet) is safe.
   ((and nskk-current-state
         (eq (nskk-state-mode nskk-current-state) 'abbrev)
         (nskk--get-conversion-start))
    'preedit)
   (t 'normal)))

(defun nskk--current-kakutei-state ()
  "Return kakutei dispatch state for `kakutei-action/2' Prolog query.
States (in priority order):
  `converting'     -- henkan-active (▼ phase)
  `preedit'        -- henkan-on (▽ phase)
  `romaji-pending' -- incomplete romaji in `nskk--romaji-buffer'
  `japanese-idle'  -- hiragana/katakana/katakana-半角, no pending input
  `direct-idle'    -- ascii/latin/jisx0208-latin/abbrev, no pending input"
  (cond
   ((nskk-converting-p) 'converting)
   ((nskk--has-preedit) 'preedit)
   ((and (boundp 'nskk--romaji-buffer)
         (not (string-empty-p nskk--romaji-buffer)))
    'romaji-pending)
   ((and nskk-current-state
         (nskk-prolog-query-one
          `(japanese-mode ,(nskk-state-mode nskk-current-state))))
    'japanese-idle)
   (t 'direct-idle)))

;;;; Internal Macros

(defmacro nskk-with-japanese-mode (action &rest fallback)
  "Execute ACTION if currently in Japanese input mode.
Performs implicit kakutei (確定) if input state is active:
- If converting (▼ active): commit the current candidate first.
- If preedit (▽ active): commit preedit kana as-is via
  `nskk-henkan-kakutei', then execute ACTION.  This matches ddskk
  behaviour where pressing a mode-switch key (l, L, /, q) during
  preedit commits the kana and exits preedit before switching modes.
- If in a Japanese mode (hiragana or katakana) but idle: execute
  ACTION directly.
Otherwise execute FALLBACK forms (typically `self-insert-command')."
  (declare (indent 1) (debug t))
  `(cond
    ((nskk-converting-p)
     (nskk-commit-current)
     ,action)
    ((nskk--has-preedit)
     (nskk-henkan-kakutei)
     ,action)
    ((and nskk-current-state
          (nskk-prolog-query-one
           `(japanese-mode ,(nskk-state-mode nskk-current-state))))
     ,action)
    (t ,@fallback)))

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
  "Handle q key: toggle between hiragana and katakana.
In henkan-active mode (▼), perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (nskk-with-japanese-mode (nskk-toggle-japanese-mode)
    (self-insert-command 1)))

(defun nskk-handle-l ()
  "Handle l key: switch to ASCII mode.
In henkan-active mode (▼), perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (nskk-with-japanese-mode (nskk-set-mode-latin)
    (self-insert-command 1)))

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
Otherwise falls through to `self-insert-command'."
  ('previous-candidate (nskk-previous-candidate))
  (_ (self-insert-command 1)))

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
  "Handle C-n key: select next candidate when converting, else next-line.
In conversion mode (▼), advances to the next conversion candidate.
In preedit mode (▽) or normal mode, delegates to \\[next-line]."
  ('next-candidate (nskk-next-candidate))
  ('next-line      (condition-case nil
                     (call-interactively #'next-line)
                   (end-of-buffer nil))))

(nskk-define-key-handler ctrl-p
  "Handle C-p key: select previous candidate when converting, else previous-line.
In conversion mode (▼), moves to the previous conversion candidate.
In preedit mode (▽) or normal mode, delegates to \\[previous-line]."
  ('previous-candidate (nskk-previous-candidate))
  ('previous-line      (condition-case nil
                         (call-interactively #'previous-line)
                       (beginning-of-buffer nil))))

(nskk-define-key-handler ctrl-f
  "Handle C-f/right-arrow: commit conversion then move forward, else forward-char.
In conversion mode (▼), commits the current candidate then moves point forward.
In preedit mode (▽) or normal mode, delegates to \\[forward-char]."
  ('kakutei-then-forward (nskk-commit-current)
                         (condition-case nil
                           (call-interactively #'forward-char)
                         (end-of-buffer nil)))
  ('forward-char (condition-case nil
                   (call-interactively #'forward-char)
                 (end-of-buffer nil))))

(nskk-define-key-handler ctrl-b
  "Handle C-b/left-arrow: commit conversion then move backward, else backward-char.
In conversion mode (▼), commits the current candidate then moves point backward.
In preedit mode (▽) or normal mode, delegates to \\[backward-char]."
  ('kakutei-then-backward (nskk-commit-current)
                          (condition-case nil
                            (call-interactively #'backward-char)
                          (beginning-of-buffer nil)))
  ('backward-char (condition-case nil
                    (call-interactively #'backward-char)
                  (beginning-of-buffer nil))))

(nskk-define-key-handler cancel
  "Handle C-g: cancel current conversion or preedit.
In conversion mode (▼), cancels conversion and restores preedit text.
In preedit mode (▽), discards preedit text and resets state.
Otherwise calls `keyboard-quit'."
  ('cancel-conversion (nskk-cancel-conversion))
  ('cancel-preedit (nskk-cancel-preedit))
  (_ (keyboard-quit)))

(nskk-define-key-handler backspace
  "Handle DEL key: delete last preedit char or backward-delete-char.
In preedit mode (▽), deletes the last accumulated character from preedit.
If ▽ is present with no accumulated chars (empty abbrev preedit after /),
cancels preedit via `nskk-cancel-preedit' (DDSKK-compatible).
In conversion mode (▼), cancels conversion (equivalent to C-g).
Otherwise delegates to `backward-delete-char'."
  ('delete-preedit-char
   (let ((start (nskk--get-conversion-start)))
     (if (and start (> (point) (+ start (length nskk-henkan-on-marker))))
         (delete-char -1)
       ;; Empty abbrev preedit: ▽ in buffer but no chars after it.
       (nskk-cancel-preedit))))
  ('cancel-conversion (nskk-cancel-conversion))
  (_ (call-interactively #'backward-delete-char)))

(provide 'nskk-keymap)

;;; nskk-keymap.el ends here
