;;; nskk-modeline.el --- Mode line indicator for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda

;; Author: Takeshi Umeda <takeokunn@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

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

;; Mode line indicator showing current input mode (Layer 1: UI Layer).
;;
;; Layer Responsibilities:
;; - UI Layer displays mode information to user
;; - Uses Application Layer API (nskk-current-mode) to query mode
;; - Manages cursor color per mode when nskk-use-color-cursor is enabled
;;
;; Displays mode indicator in modeline (ddskk-compatible strings):
;; - かな (hiragana)
;; - カナ (katakana)
;; - aA  (abbrev)
;; - SKK (ascii/latin)
;; - 全英 (jisx0208-latin / full-width latin)
;;
;; Conversion markers (▽/▼) are displayed inline in the buffer,
;; not in the modeline.
;;
;; Uses distinct faces for each mode for visual clarity.

;;; Code:

(require 'nskk-layer-application)
(require 'nskk-custom)
(require 'nskk-state)

;; Forward declarations for cursor color variables (defined in nskk-custom.el).
(defvar nskk-use-color-cursor)
(defvar nskk-cursor-hiragana-color)
(defvar nskk-cursor-katakana-color)
(defvar nskk-cursor-latin-color)
(defvar nskk-cursor-jisx0208-latin-color)
(defvar nskk-cursor-abbrev-color)

(defvar-local nskk--last-cursor-color nil
  "Last cursor color applied, to avoid redundant set-cursor-color calls.")

(defvar-local nskk--last-modeline-indicator nil
  "Last modeline indicator string, to avoid redundant updates.")

(defgroup nskk-modeline nil
  "Mode line indicator settings for NSKK."
  :prefix "nskk-modeline-"
  :group 'nskk-ui)

(defface nskk-modeline-hiragana-face
  '((t (:foreground "#4CAF50" :weight bold)))
  "Face for hiragana mode indicator."
  :group 'nskk-modeline)

(defface nskk-modeline-katakana-face
  '((t (:foreground "#2196F3" :weight bold)))
  "Face for katakana mode indicator."
  :group 'nskk-modeline)

(defface nskk-modeline-abbrev-face
  '((t (:foreground "#FF9800" :weight bold)))
  "Face for abbrev mode indicator."
  :group 'nskk-modeline)

(defface nskk-modeline-jisx0208-latin-face
  '((t (:foreground "#FFD700" :weight bold)))
  "Face for full-width latin mode indicator."
  :group 'nskk-modeline)

(defface nskk-modeline-direct-face
  '((t (:foreground "#9E9E9E" :weight bold)))
  "Face for direct mode indicator."
  :group 'nskk-modeline)

(defvar nskk-modeline-lighter
  '(:eval (nskk-modeline-indicator))
  "Mode line lighter for NSKK.")

(defun nskk-modeline-indicator (&optional state)
  "Return mode line indicator string for current mode.
When STATE (an nskk-state struct) is provided, derive the mode from it.
Otherwise fall back to `nskk-current-mode' for backward compatibility."
  (let* ((mode (if state
                   (nskk-state-mode state)
                 (nskk-current-mode)))
         (mode-name (or (cdr (assq mode nskk-modeline-mode-names))
                        (nskk-modeline--mode-string mode)))
         (formatted (format-spec nskk-modeline-format
                                 `((?m . ,mode-name)))))
    (propertize formatted
                'face (nskk-modeline--mode-face mode)
                'help-echo (nskk-modeline--mode-help mode))))

(defun nskk-modeline--mode-string (mode)
  "Return mode string for MODE."
  (declare (pure t) (side-effect-free t))
  (pcase mode
    ('hiragana "かな")
    ('katakana "カナ")
    ('abbrev "aA")
    ('ascii "SKK")
    ('latin "SKK")
    ('jisx0208-latin "全英")
    (_ "NSKK")))

(defun nskk-modeline--mode-face (mode)
  "Return face for MODE."
  (declare (pure t) (side-effect-free t))
  (pcase mode
    ('hiragana 'nskk-modeline-hiragana-face)
    ('katakana 'nskk-modeline-katakana-face)
    ('abbrev 'nskk-modeline-abbrev-face)
    ((or 'ascii 'latin 'direct) 'nskk-modeline-direct-face)
    ('jisx0208-latin 'nskk-modeline-jisx0208-latin-face)
    (_ 'default)))

(defun nskk-modeline--mode-help (mode)
  "Return help text for MODE."
  (declare (pure t) (side-effect-free t))
  (pcase mode
    ('hiragana "Hiragana input mode")
    ('katakana "Katakana input mode")
    ('abbrev "Abbreviation mode")
    ((or 'ascii 'latin 'direct) "Direct/ASCII input mode")
    ('jisx0208-latin "Full-width latin input mode")
    (_ "NSKK input method")))

(defun nskk-modeline-update ()
  "Update modeline and cursor to reflect current NSKK state.
Only triggers redisplay when the indicator actually changes."
  (when (and (boundp 'nskk-current-state) nskk-current-state)
    (let ((indicator (nskk-modeline-indicator nskk-current-state)))
      (unless (equal indicator nskk--last-modeline-indicator)
        (setq nskk--last-modeline-indicator indicator)
        (let ((entry (assq 'nskk-mode minor-mode-alist)))
          (when entry
            (setcar (cdr entry) indicator)))
        (force-mode-line-update)))
    (nskk-cursor-update)))

(defun nskk-cursor-update ()
  "Update cursor color to reflect current NSKK mode.
Only calls `set-cursor-color' when the color actually changes."
  (when (and nskk-use-color-cursor
             (boundp 'nskk-current-state)
             nskk-current-state)
    (let* ((mode (nskk-state-mode nskk-current-state))
           (color (nskk-cursor--mode-color mode)))
      (when (and color (not (equal color nskk--last-cursor-color)))
        (set-cursor-color color)
        (setq nskk--last-cursor-color color)))))

(defun nskk-cursor--mode-color (mode)
  "Return cursor color for MODE."
  (pcase mode
    ('hiragana nskk-cursor-hiragana-color)
    ('katakana nskk-cursor-katakana-color)
    ((or 'ascii 'latin) nskk-cursor-latin-color)
    ('jisx0208-latin nskk-cursor-jisx0208-latin-color)
    ('abbrev nskk-cursor-abbrev-color)
    (_ nil)))

(provide 'nskk-modeline)

;;; nskk-modeline.el ends here
