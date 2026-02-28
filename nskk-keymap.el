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
;; Provides interactive handlers for the eight special keys in nskk-mode-map:
;; q, l, L, /, x, SPC, RET, C-g.  Each handler checks the current NSKK
;; conversion state before acting, falling through to `self-insert-command'
;; or `keyboard-quit' when NSKK is in ASCII mode or state is inactive.
;;
;; Prolog predicates maintained by this module:
;; - `key-action/3'  -- (key state action) dispatch rules for x, SPC, RET, C-g
;;
;; Key architecture:
;; - `nskk-define-key-handler' -- macro that generates Prolog-dispatched
;;     interactive commands.  Queries `key-action/3' to decide the action
;;     based on the current dispatch state (converting, preedit, normal).
;; - `nskk-with-japanese-mode' -- macro for q/l/L// handlers.  Uses the
;;     `japanese-mode/1' predicate (defined in nskk-state.el) to guard
;;     mode-switch actions.
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
;; - `nskk-handle-return'  -- commit and newline
;; - `nskk-handle-cancel'  -- cancel conversion / preedit / keyboard-quit

;;; Code:

(require 'nskk-state)
(require 'nskk-prolog)

;; Functions in nskk-input.el
(declare-function nskk-toggle-japanese-mode "nskk-input")
(declare-function nskk-set-mode-latin "nskk-input")
(declare-function nskk-set-mode-abbrev "nskk-input")
(declare-function nskk-set-mode-jisx0208-latin "nskk-input")
;; Functions in nskk-henkan.el
(declare-function nskk-converting-p "nskk-henkan")
(declare-function nskk--has-preedit "nskk-henkan")
(declare-function nskk-start-conversion "nskk-henkan")
(declare-function nskk-next-candidate "nskk-henkan")
(declare-function nskk-commit-current "nskk-henkan")
(declare-function nskk-previous-candidate "nskk-henkan")
(declare-function nskk-cancel-conversion "nskk-henkan")
(declare-function nskk-cancel-preedit "nskk-henkan")

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
(nskk-prolog-<- (key-action return converting commit-and-newline))
(nskk-prolog-<- (key-action return normal newline))

;; Cancel key rules
(nskk-prolog-<- (key-action cancel converting cancel-conversion))
(nskk-prolog-<- (key-action cancel preedit cancel-preedit))
(nskk-prolog-<- (key-action cancel normal keyboard-quit))

;; X key rules
(nskk-prolog-<- (key-action x converting previous-candidate))
(nskk-prolog-<- (key-action x normal self-insert))

;;;; State Detection

(defun nskk--current-key-state ()
  "Return current key dispatch state.
Returns `converting', `preedit', or `normal'."
  (cond
   ((nskk-converting-p) 'converting)
   ((nskk--has-preedit) 'preedit)
   (t 'normal)))

;;;; Internal Macros

(defmacro nskk-with-japanese-mode (action &rest fallback)
  "Execute ACTION if currently in Japanese input mode.
If currently converting (▼ active), commit the candidate first
then execute ACTION.  If in a Japanese mode as classified by the
`japanese-mode/1' Prolog predicate (hiragana or katakana), execute
ACTION directly.  Otherwise execute FALLBACK forms."
  (declare (indent 1) (debug t))
  `(cond
    ((nskk-converting-p)
     (nskk-commit-current)
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
state (converting, preedit, or normal) before querying Prolog."
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
Otherwise inserts a literal space via `self-insert-command'."
  ('next-candidate (nskk-next-candidate))
  ('start-conversion (nskk-start-conversion))
  (_ (self-insert-command 1)))

(nskk-define-key-handler return
  "Handle RET key: commit current conversion and insert newline.
In conversion mode (▼), commits the selected candidate then inserts
a newline.  In preedit mode (▽) or normal mode, inserts a newline
unconditionally (preedit text is left in place)."
  ('commit-and-newline (nskk-commit-current) (newline))
  (_ (newline)))

(nskk-define-key-handler cancel
  "Handle C-g: cancel current conversion or preedit.
In conversion mode (▼), cancels conversion and restores preedit text.
In preedit mode (▽), discards preedit text and resets state.
Otherwise calls `keyboard-quit'."
  ('cancel-conversion (nskk-cancel-conversion))
  ('cancel-preedit (nskk-cancel-preedit))
  (_ (keyboard-quit)))

(provide 'nskk-keymap)

;;; nskk-keymap.el ends here
