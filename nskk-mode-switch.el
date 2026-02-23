;;; nskk-mode-switch.el --- Mode switching logic for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda

;; Author: NSKK Contributors
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
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

;; This file implements mode switching logic for NSKK (Layer 3: Application Layer).
;; It provides state transitions between hiragana, katakana, latin, and abbrev modes.
;;
;; Layer Responsibilities:
;; - Application Layer coordinates between UI and Core Engine layers
;; - Uses state management APIs (nskk-state) for mode tracking
;; - Does NOT directly depend on Core Engine implementation (nskk-converter)
;; - Core Engine operations are invoked through layer interfaces (nskk-layer-core)
;;
;; All mode transitions are validated for consistency.

;;; Code:

(require 'nskk-state)

(declare-function nskk-modeline-update "nskk-modeline")

(defvar-local nskk-converting-active nil
  "Non-nil when conversion mode is active.")

(defun nskk-set-mode-hiragana ()
  "Switch to hiragana mode."
  (interactive)
  (nskk--set-mode 'hiragana)
  (nskk-modeline-update))

(defun nskk-set-mode-katakana ()
  "Switch to katakana mode."
  (interactive)
  (nskk--set-mode 'katakana)
  (nskk-modeline-update))

(defun nskk-set-mode-latin ()
  "Switch to latin mode."
  (interactive)
  (nskk--set-mode 'latin)
  (nskk-modeline-update))

(defun nskk-set-mode-abbrev ()
  "Switch to abbrev mode."
  (interactive)
  (nskk--set-mode 'abbrev)
  (nskk-modeline-update))

(defun nskk-toggle-katakana ()
  "Toggle between hiragana and katakana modes."
  (interactive)
  (let ((current-mode (when (boundp 'nskk-current-state)
                        (nskk-state-mode nskk-current-state))))
    (if (eq current-mode 'hiragana)
        (nskk-set-mode-katakana)
      (when (eq current-mode 'katakana)
        (nskk-set-mode-hiragana)))))

(defun nskk--set-mode (mode)
  "Internal mode setter with validation.
MODE is the target mode symbol."
  (unless (memq mode '(hiragana katakana latin abbrev))
    (error "Invalid mode: %s" mode))
  (when (boundp 'nskk-current-state)
    (nskk-state-set nskk-current-state 'mode mode))
  (nskk--clear-conversion-context))

(defun nskk--clear-conversion-context ()
  "Clear conversion context when switching modes."
  (when nskk-converting-active
    (setq nskk-converting-active nil)
    (nskk-modeline-update)))

(provide 'nskk-mode-switch)

;;; nskk-mode-switch.el ends here
