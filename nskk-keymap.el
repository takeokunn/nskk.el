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

;; Keymap definitions matching ddskk bindings (Layer 1: UI Layer).
;;
;; Layer Responsibilities:
;; - UI Layer defines key bindings for user interaction
;; - Does NOT directly access state management (nskk-state)
;; - Routes all operations through Application Layer APIs (nskk-layer-application)
;; - Maintains separation between UI and business logic

;;; Code:

(require 'nskk-layer-application)

;; TODO: Keybindings are disabled until state-aware conditional dispatch is
;; implemented.  The bindings below (SPC, q, l, etc.) unconditionally intercept
;; regular typing, which makes the mode unusable for normal editing.  They need
;; to be wrapped in a dispatch mechanism that checks the current NSKK state
;; (e.g., conversion-active, henkan mode) before intercepting keys.
;;
;; Planned bindings (to be added with state-aware dispatch):
;;   SPC  -> nskk-convert-or-commit-selection
;;   RET  -> nskk-commit
;;   q/Q  -> nskk-cancel
;;   l    -> nskk-enter-hiragana-mode
;;   L    -> nskk-enter-katakana-mode
;;   /    -> nskk-toggle-japanese-mode
;;   x    -> convert to katakana
;;   C-x  -> convert to half-width katakana
;;   C-f  -> forward character in completion
;;   C-b  -> backward character in completion

(provide 'nskk-keymap)

;;; nskk-keymap.el ends here
