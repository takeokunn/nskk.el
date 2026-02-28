;;; nskk-modeline.el --- Mode line indicator for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
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

;;; Commentary:

;; Mode line indicator showing current input mode (Layer 1: UI Layer).
;;
;; Layer Responsibilities:
;; - UI Layer displays mode information to user
;; - Uses nskk-state-get-mode to query current mode
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

(require 'nskk-state)
(require 'nskk-prolog)

(defgroup nskk-modeline nil
  "Mode line indicator settings for NSKK."
  :prefix "nskk-modeline-"
  :group 'nskk-ui)

(defcustom nskk-modeline-format "[%m]"
  "Modeline format string.
%m is replaced with the mode name."
  :type 'string
  :group 'nskk-modeline)

(defcustom nskk-modeline-mode-names
  '((ascii . "SKK")
    (hiragana . "かな")
    (katakana . "カナ")
    (abbrev . "aA")
    (latin . "SKK")
    (jisx0208-latin . "全英"))
  "Mode name display mapping for modeline."
  :type 'alist
  :group 'nskk-modeline)

(defcustom nskk-use-color-cursor t
  "Whether to change cursor color based on input mode."
  :type 'boolean
  :group 'nskk-ui)

(defcustom nskk-cursor-hiragana-color
  (if (eq (frame-parameter nil 'background-mode) 'dark) "coral4" "pink")
  "Cursor color for hiragana mode."
  :type 'color
  :group 'nskk-ui)

(defcustom nskk-cursor-katakana-color
  (if (eq (frame-parameter nil 'background-mode) 'dark) "forestgreen" "green")
  "Cursor color for katakana mode."
  :type 'color
  :group 'nskk-ui)

(defcustom nskk-cursor-latin-color
  (if (eq (frame-parameter nil 'background-mode) 'dark) "ivory4" "gray")
  "Cursor color for ASCII/latin mode."
  :type 'color
  :group 'nskk-ui)

(defcustom nskk-cursor-jisx0208-latin-color "gold"
  "Cursor color for full-width latin mode."
  :type 'color
  :group 'nskk-ui)

(defcustom nskk-cursor-abbrev-color "royalblue"
  "Cursor color for abbrev mode."
  :type 'color
  :group 'nskk-ui)

(defvar-local nskk--last-cursor-color nil
  "Last cursor color applied, to avoid redundant set-cursor-color calls.")

(defvar-local nskk--last-modeline-indicator nil
  "Last modeline indicator string, to avoid redundant updates.")

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

;;;; Prolog Mode Facts

(nskk-prolog-set-index 'mode-info 4 :hash)
(nskk-prolog-<- (mode-info hiragana "かな" nskk-modeline-hiragana-face "Hiragana input mode"))
(nskk-prolog-<- (mode-info katakana "カナ" nskk-modeline-katakana-face "Katakana input mode"))
(nskk-prolog-<- (mode-info abbrev "aA" nskk-modeline-abbrev-face "Abbreviation mode"))
(nskk-prolog-<- (mode-info ascii "SKK" nskk-modeline-direct-face "Direct/ASCII input mode"))
(nskk-prolog-<- (mode-info latin "SKK" nskk-modeline-direct-face "Direct/ASCII input mode"))
(nskk-prolog-<- (mode-info direct "SKK" nskk-modeline-direct-face "Direct/ASCII input mode"))
(nskk-prolog-<- (mode-info jisx0208-latin "全英" nskk-modeline-jisx0208-latin-face "Full-width latin input mode"))

;; Cursor color mapping
(nskk-prolog-set-index 'cursor-color 2 :hash)
(nskk-prolog-<- (cursor-color hiragana nskk-cursor-hiragana-color))
(nskk-prolog-<- (cursor-color katakana nskk-cursor-katakana-color))
(nskk-prolog-<- (cursor-color ascii nskk-cursor-latin-color))
(nskk-prolog-<- (cursor-color latin nskk-cursor-latin-color))
(nskk-prolog-<- (cursor-color jisx0208-latin nskk-cursor-jisx0208-latin-color))
(nskk-prolog-<- (cursor-color abbrev nskk-cursor-abbrev-color))

(defvar nskk-modeline-lighter
  '(:eval (nskk-modeline-indicator))
  "Mode line lighter for NSKK.")

(defun nskk-modeline-indicator (&optional state)
  "Return mode line indicator string for current mode.
When STATE (an nskk-state struct) is provided, derive the mode from it.
Otherwise fall back to `nskk-state-get-mode' for backward compatibility.
Uses a single Prolog query to fetch mode string, face, and help text."
  (let* ((mode (if state
                   (nskk-state-mode state)
                 (nskk-state-get-mode)))
         (info (nskk-prolog-query-values
                `(mode-info ,mode ,'\?s ,'\?f ,'\?h)
                '(\?s \?f \?h)))
         (mode-name (or (cdr (assq mode nskk-modeline-mode-names))
                        (or (nth 0 info) "NSKK")))
         (face (or (nth 1 info) 'default))
         (help (or (nth 2 info) "NSKK input method"))
         (formatted (format-spec nskk-modeline-format
                                 `((?m . ,mode-name)))))
    (propertize formatted 'face face 'help-echo help)))

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
  "Return cursor color for MODE via Prolog lookup."
  (let ((color-var (nskk-prolog-query-value
                    `(cursor-color ,mode ,'\?c) '\?c)))
    (when (and color-var (boundp color-var))
      (symbol-value color-var))))

(provide 'nskk-modeline)

;;; nskk-modeline.el ends here
