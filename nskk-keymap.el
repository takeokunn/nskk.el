;;; nskk-keymap.el --- Keymap definitions for NSKK -*- lexical-binding: t; -*-

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

;; Copyright (C) 2026 NSKK Contributors

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

;; Key handling utility functions for NSKK (Layer 1: Presentation Layer).
;;
;; These functions provide state-aware key dispatch for special keys
;; (q, l, SPC, RET).  They check the current NSKK state before
;; intercepting keys, falling through to `self-insert-command' when
;; NSKK is in ASCII mode or the state is not active.
;;
;; State reads use nskk-state accessors directly (acceptable for L1),
;; but state mutations are routed through the Application Layer API.

;;; Code:

(require 'nskk-state)
(require 'nskk-prolog)

;; Functions in nskk-input.el
(declare-function nskk-toggle-japanese-mode "nskk-input")
(declare-function nskk-enter-latin-mode "nskk-input")
(declare-function nskk-enter-abbrev-mode "nskk-input")
(declare-function nskk-enter-jisx0208-latin-mode "nskk-input")
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
DDSKK equivalent: skk-annotate-mode-map-hook")

(defvar nskk-annotate-minibuffer-map-hook nil
  "Hook run when annotating minibuffer map.
DDSKK equivalent: skk-annotate-minibuffer-map-hook")

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
  "If in Japanese mode (hiragana/katakana), execute ACTION.
If converting, commit first then execute ACTION.
Otherwise execute FALLBACK forms."
  (declare (indent 1) (debug t))
  `(cond
    ((nskk-converting-p)
     (nskk-commit-current)
     ,action)
    ((and nskk-current-state
          (memq (nskk-state-mode nskk-current-state) '(hiragana katakana)))
     ,action)
    (t ,@fallback)))

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
  (nskk-with-japanese-mode (nskk-enter-latin-mode)
    (self-insert-command 1)))

(defun nskk-handle-upper-l ()
  "Handle L key: switch to full-width latin (jisx0208-latin) mode.
In henkan-active mode (▼), perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (nskk-with-japanese-mode (nskk-enter-jisx0208-latin-mode)
    (self-insert-command 1)))

(defun nskk-handle-slash ()
  "Handle / key: enter abbrev mode from Japanese mode.
In henkan-active mode (▼), perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (nskk-with-japanese-mode (nskk-enter-abbrev-mode)
    (self-insert-command 1)))

(defun nskk-handle-x ()
  "Handle x key: select previous candidate."
  (interactive)
  (let* ((state (nskk--current-key-state))
         (action (nskk-prolog-query-value
                  `(key-action x ,state ,'\?a) '\?a)))
    (pcase action
      ('previous-candidate (nskk-previous-candidate))
      (_ (self-insert-command 1)))))

(defun nskk-handle-space ()
  "Handle SPC key: start conversion or select next candidate."
  (interactive)
  (let* ((state (nskk--current-key-state))
         (action (nskk-prolog-query-value
                  `(key-action space ,state ,'\?a) '\?a)))
    (pcase action
      ('next-candidate (nskk-next-candidate))
      ('start-conversion (nskk-start-conversion))
      (_ (self-insert-command 1)))))

(defun nskk-handle-return ()
  "Handle RET key: commit current conversion and insert newline."
  (interactive)
  (let* ((state (nskk--current-key-state))
         (action (nskk-prolog-query-value
                  `(key-action return ,state ,'\?a) '\?a)))
    (pcase action
      ('commit-and-newline (nskk-commit-current) (newline))
      (_ (newline)))))

(defun nskk-handle-cancel ()
  "Handle C-g: cancel conversion or preedit."
  (interactive)
  (let* ((state (nskk--current-key-state))
         (action (nskk-prolog-query-value
                  `(key-action cancel ,state ,'\?a) '\?a)))
    (pcase action
      ('cancel-conversion (nskk-cancel-conversion))
      ('cancel-preedit (nskk-cancel-preedit))
      (_ (keyboard-quit)))))

(provide 'nskk-keymap)

;;; nskk-keymap.el ends here
