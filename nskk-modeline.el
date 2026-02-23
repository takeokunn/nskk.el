;;; nskk-modeline.el --- Mode line indicator for NSKK -*- lexical-binding: t; -*-

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

;; Mode line indicator showing current input mode (Layer 1: UI Layer).
;;
;; Layer Responsibilities:
;; - UI Layer displays mode information to user
;; - Uses Application Layer API (nskk-current-mode) to query mode
;; - Does NOT directly access state management (nskk-state)
;;
;; Displays mode indicator in modeline:
;; - あ (hiragana)
;; - ア (katakana)
;; - ：a (abbrev)
;; - 英 (direct/ASCII)
;;
;; Uses distinct faces for each mode for visual clarity.

;;; Code:

(require 'nskk-layer-application)

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

(defface nskk-modeline-direct-face
  '((t (:foreground "#9E9E9E" :weight bold)))
  "Face for direct mode indicator."
  :group 'nskk-modeline)

(defvar nskk-modeline-lighter
  '(:eval (nskk-modeline-indicator))
  "Mode line lighter for NSKK.")

(defun nskk-modeline-indicator ()
  "Return mode line indicator string for current mode."
  (let ((mode (nskk-current-mode)))
    (propertize (nskk-modeline--mode-string mode)
                'face (nskk-modeline--mode-face mode)
                'help-echo (nskk-modeline--mode-help mode))))

(defun nskk-modeline--mode-string (mode)
  "Return mode string for MODE."
  (pcase mode
    ('hiragana "あ")
    ('katakana "ア")
    ('abbrev "：a")
    ('direct "英")
    (_ "NSKK")))

(defun nskk-modeline--mode-face (mode)
  "Return face for MODE."
  (pcase mode
    ('hiragana 'nskk-modeline-hiragana-face)
    ('katakana 'nskk-modeline-katakana-face)
    ('abbrev 'nskk-modeline-abbrev-face)
    ('direct 'nskk-modeline-direct-face)
    (_ 'default)))

(defun nskk-modeline--mode-help (mode)
  "Return help text for MODE."
  (pcase mode
    ('hiragana "Hiragana input mode")
    ('katakana "Katakana input mode")
    ('abbrev "Abbreviation mode")
    ('direct "Direct/ASCII input mode")
    (_ "NSKK input method")))

(provide 'nskk-modeline)

;;; nskk-modeline.el ends here
