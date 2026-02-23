;;; nskk-layer-presentation.el --- Presentation layer interface for NSKK -*- lexical-binding: t; -*-

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
;; Keywords: Japanese, input method

;;; Commentary:

;; Presentation layer interface coordinating UI components.
;;
;; This layer provides the interface between core functionality and
;; user-facing UI components:
;; - Keymap management (nskk-keymap)
;; - Candidate window display (nskk-candidate-window)
;; - Mode line indicator (nskk-modeline)
;;
;; Architecture:
;; - Coordinates UI updates based on state changes
;; - Delegates rendering to specialized components
;; - Handles user input from keymap
;; - Provides clean separation of concerns

;;; Code:

(require 'nskk-keymap)
(require 'nskk-candidate-window)
(require 'nskk-modeline)

(provide 'nskk-layer-presentation)

;;; nskk-layer-presentation.el ends here
