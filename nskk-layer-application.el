;;; nskk-layer-application.el --- Application layer interface for NSKK -*- lexical-binding: t; -*-

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

;; This file provides the Application Layer (Layer 3) interface for NSKK.
;;
;; Layer Responsibilities:
;; - Application Layer coordinates between UI Layer (Layer 1-2) and Core Engine (Layer 4)
;; - Defines the public API for user interactions and mode management
;; - Manages application state and coordinates conversions
;; - Uses State Management (nskk-state) for mode tracking
;; - Routes Core Engine operations through layer interfaces (nskk-layer-core)
;; - Does NOT directly depend on Core Engine implementation modules
;;
;; Dependency Flow:
;; - UI Layer → Application Layer → Core Engine Layer
;; - Application Layer → State Management
;; - Application Layer → Data Access Layer (for dictionary operations)

;;; Code:

(require 'nskk-mode-switch)
(require 'nskk-input-commands)
(require 'nskk-state)

;; nskk-converting-active is defined in nskk-mode-switch.el

;; Main entry point
(defun nskk-initialize ()
  "Initialize NSKK application layer."
  (nskk-state-initialize)
  (setq nskk-converting-active nil))

;; Mode management API
(defun nskk-enter-hiragana-mode ()
  "Enter hiragana input mode."
  (interactive)
  (nskk-set-mode-hiragana))

(defun nskk-enter-katakana-mode ()
  "Enter katakana input mode."
  (interactive)
  (nskk-set-mode-katakana))

(defun nskk-enter-latin-mode ()
  "Enter latin input mode."
  (interactive)
  (nskk-set-mode-latin))

(defun nskk-enter-abbrev-mode ()
  "Enter abbrev input mode."
  (interactive)
  (nskk-set-mode-abbrev))

(defun nskk-toggle-japanese-mode ()
  "Toggle between hiragana and katakana modes."
  (interactive)
  (nskk-toggle-katakana))

;; Input processing API
(defun nskk-process-input (_char)
  "Process input character _CHAR."
  (interactive (list last-command-event))
  (nskk-self-insert 1))

;; Conversion control API
(defun nskk-start-convert ()
  "Start conversion of current preedit."
  (interactive)
  (nskk-convert))

(defun nskk-commit ()
  "Commit current conversion."
  (interactive)
  (nskk-commit-current))

(defun nskk-convert-or-commit-selection ()
  "Start conversion or commit current candidate."
  (interactive)
  (nskk-convert-or-commit))

(defun nskk-cancel ()
  "Cancel conversion and return to input state."
  (interactive)
  (nskk-cancel-conversion))

(defun nskk-next ()
  "Select next conversion candidate."
  (interactive)
  (nskk-next-candidate))

(defun nskk-previous ()
  "Select previous conversion candidate."
  (interactive)
  (nskk-previous-candidate))

;; Query functions
(defun nskk-in-conversion-p ()
  "Return non-nil if currently in conversion state."
  (nskk-converting-p))

(defun nskk-current-mode ()
  "Return current input mode."
  (nskk-state-get-mode))

(provide 'nskk-layer-application)

;;; nskk-layer-application.el ends here
