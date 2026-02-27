;;; nskk-keymap.el --- Keymap definitions for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda
;;
;; This file is part of NSKK (Next-generation SKK).
;;
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

;; Author: NSKK Developers
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

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
(require 'nskk-layer-application)

(declare-function nskk-toggle-japanese-mode "nskk-mode-switch")
(declare-function nskk-enter-latin-mode "nskk-layer-application")
(declare-function nskk-converting-p "nskk-input-commands")
(declare-function nskk--has-preedit "nskk-input-commands")
(declare-function nskk-start-conversion "nskk-input-commands")
(declare-function nskk-next-candidate "nskk-input-commands")
(declare-function nskk-commit-current "nskk-input-commands")
(declare-function nskk-previous-candidate "nskk-input-commands")
(declare-function nskk-cancel-conversion "nskk-input-commands")
(declare-function nskk-cancel-preedit "nskk-input-commands")
(declare-function nskk-enter-abbrev-mode "nskk-layer-application")
(declare-function nskk-enter-jisx0208-latin-mode "nskk-layer-application")

(defun nskk-handle-q ()
  "Handle q key: toggle between hiragana and katakana.
In henkan-active mode (▼), perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (cond
   ;; Implicit kakutei: if converting, commit first then toggle
   ((nskk-converting-p)
    (nskk-commit-current)
    (nskk-toggle-japanese-mode))
   ;; Normal toggle in Japanese mode
   ((and nskk-current-state
         (memq (nskk-state-mode nskk-current-state) '(hiragana katakana)))
    (nskk-toggle-japanese-mode))
   (t
    (self-insert-command 1))))

(defun nskk-handle-l ()
  "Handle l key: switch to ASCII mode.
In henkan-active mode (▼), perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (cond
   ;; Implicit kakutei: if converting, commit first then enter latin
   ((nskk-converting-p)
    (nskk-commit-current)
    (nskk-enter-latin-mode))
   ;; Normal switch in Japanese mode
   ((and nskk-current-state
         (memq (nskk-state-mode nskk-current-state) '(hiragana katakana)))
    (nskk-enter-latin-mode))
   (t
    (self-insert-command 1))))

(defun nskk-handle-upper-l ()
  "Handle L key: switch to full-width latin (jisx0208-latin) mode.
In henkan-active mode (▼), perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (cond
   ;; Implicit kakutei
   ((nskk-converting-p)
    (nskk-commit-current)
    (nskk-enter-jisx0208-latin-mode))
   ;; Switch from Japanese mode
   ((and nskk-current-state
         (memq (nskk-state-mode nskk-current-state) '(hiragana katakana)))
    (nskk-enter-jisx0208-latin-mode))
   (t
    (self-insert-command 1))))

(defun nskk-handle-slash ()
  "Handle / key: enter abbrev mode from Japanese mode.
In henkan-active mode (▼), perform implicit kakutei first.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (cond
   ;; Implicit kakutei
   ((nskk-converting-p)
    (nskk-commit-current)
    (nskk-enter-abbrev-mode))
   ;; Enter abbrev from Japanese mode
   ((and nskk-current-state
         (memq (nskk-state-mode nskk-current-state) '(hiragana katakana)))
    (nskk-enter-abbrev-mode))
   (t
    (self-insert-command 1))))

(defun nskk-handle-x ()
  "Handle x key: select previous candidate.
When converting, go to previous candidate.
Otherwise fall through to `self-insert-command'."
  (interactive)
  (if (nskk-converting-p)
      (nskk-previous-candidate)
    (self-insert-command 1)))

(defun nskk-handle-space ()
  "Handle SPC key: start conversion or select next candidate.
When converting, cycle to next candidate.  When preedit text
exists, start dictionary conversion.  Otherwise insert a space."
  (interactive)
  (cond
   ((nskk-converting-p)
    (nskk-next-candidate))
   ((nskk--has-preedit)
    (nskk-start-conversion))
   (t
    (self-insert-command 1))))

(defun nskk-handle-return ()
  "Handle RET key: commit current conversion and insert newline.
In ddskk, RET confirms the candidate AND inserts a newline.
When not in conversion state, fall through to `newline'."
  (interactive)
  (if (nskk-converting-p)
      (progn
        (nskk-commit-current)
        (newline))
    (newline)))

(defun nskk-handle-cancel ()
  "Handle C-g: cancel conversion or preedit.
In conversion mode (▼), cancel and restore preedit.
In preedit mode (▽), remove marker and clear preedit.
Otherwise, pass through to `keyboard-quit'."
  (interactive)
  (cond
   ((nskk-converting-p)
    (nskk-cancel-conversion))
   ((nskk--has-preedit)
    (nskk-cancel-preedit))
   (t
    (keyboard-quit))))

(provide 'nskk-keymap)

;;; nskk-keymap.el ends here
