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

(declare-function nskk-toggle-japanese-mode "nskk-layer-application")
(declare-function nskk-enter-latin-mode "nskk-layer-application")
(declare-function nskk-next "nskk-layer-application")
(declare-function nskk-commit "nskk-layer-application")

(defun nskk-handle-q ()
  "Handle q key: toggle between hiragana and katakana.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (if (and nskk-current-state
           (memq (nskk-state-mode nskk-current-state) '(hiragana katakana)))
      (nskk-toggle-japanese-mode)
    (self-insert-command 1)))

(defun nskk-handle-l ()
  "Handle l key: switch to ASCII mode.
In ASCII mode or when NSKK state is inactive, fall through to
`self-insert-command'."
  (interactive)
  (if (and nskk-current-state
           (memq (nskk-state-mode nskk-current-state) '(hiragana katakana)))
      (nskk-enter-latin-mode)
    (self-insert-command 1)))

(defun nskk-handle-space ()
  "Handle SPC key: start conversion or select next candidate.
When not in a convertible state, fall through to `self-insert-command'."
  (interactive)
  (if (and nskk-current-state
           (nskk-state-in-henkan-mode-p nskk-current-state))
      (nskk-next)
    (self-insert-command 1)))

(defun nskk-handle-return ()
  "Handle RET key: commit current conversion.
When not in a convertible state, fall through to `newline'."
  (interactive)
  (if (and nskk-current-state
           (nskk-state-in-henkan-mode-p nskk-current-state))
      (nskk-commit)
    (newline)))

(provide 'nskk-keymap)

;;; nskk-keymap.el ends here
