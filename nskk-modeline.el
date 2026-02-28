;;; nskk-modeline.el --- Mode line indicator for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
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
;; Architecture:
;; - All mode display data (strings, faces, help text) is stored as
;;   Prolog `mode-info/4' facts, queried at display time via
;;   `nskk-modeline-indicator'.
;; - The `nskk-define-mode-entry' macro co-locates face definition
;;   and Prolog fact assertion into a single declaration per mode.
;; - Cursor color per mode is stored as `cursor-color/2' Prolog facts.
;; - The mode-line lighter uses `(:eval (nskk-modeline-indicator))' in
;;   `define-minor-mode', so Emacs calls this function directly on each
;;   mode-line redisplay without manual `minor-mode-alist' mutation.
;;
;; Displays mode indicator in modeline (ddskk-compatible strings):
;; - かな (hiragana)
;; - カナ (katakana)
;; - aA  (abbrev)
;; - SKK (ascii / latin / direct)
;; - 全英 (jisx0208-latin / full-width latin)
;;
;; Conversion markers (▽/▼) are displayed inline in the buffer,
;; not in the modeline.
;;
;; Customization:
;; - `nskk-modeline-format': Control the format string (%m = mode name).
;; - `nskk-use-color-cursor': Enable/disable cursor color changes.
;; - `nskk-cursor-*-color': Cursor color per mode.
;;
;; Breaking change from earlier versions:
;; `nskk-modeline-mode-names' has been removed.  Mode display strings
;; are now defined solely via Prolog `mode-info/4' facts.

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

;;;; Macro

(defmacro nskk-define-mode-entry (mode display face-or-spec help)
  "Define a modeline entry for input MODE.
MODE is the mode symbol (e.g., `hiragana').
DISPLAY is the mode-line display string (e.g., \"かな\").
FACE-OR-SPEC is either:
  - A list like (:foreground COLOR :weight WEIGHT): creates
    `nskk-modeline-MODE-face' with this spec.
  - A symbol naming an existing face: uses it directly.
HELP is the help-echo tooltip string.

Emits `defface' (when FACE-OR-SPEC is a list) and asserts a
Prolog `mode-info/4' fact for `nskk-modeline-indicator'."
  (declare (indent 1) (debug t))
  (let ((face-sym (if (listp face-or-spec)
                      (intern (format "nskk-modeline-%s-face" mode))
                    face-or-spec)))
    `(progn
       ,@(when (listp face-or-spec)
           `((defface ,face-sym
               '((t ,face-or-spec))
               ,(format "Face for %s mode indicator." mode)
               :group 'nskk-modeline)))
       (nskk-prolog-<- (mode-info ,mode ,display ,face-sym ,help)))))

;;;; Prolog Mode Facts

(nskk-prolog-set-index 'mode-info 4 :hash)

(nskk-define-mode-entry hiragana "かな"
  (:foreground "#4CAF50" :weight bold)
  "Hiragana input mode")

(nskk-define-mode-entry katakana "カナ"
  (:foreground "#2196F3" :weight bold)
  "Katakana input mode")

(nskk-define-mode-entry abbrev "aA"
  (:foreground "#FF9800" :weight bold)
  "Abbreviation mode")

(nskk-define-mode-entry jisx0208-latin "全英"
  (:foreground "#FFD700" :weight bold)
  "Full-width latin input mode")

(defface nskk-modeline-direct-face
  '((t (:foreground "#9E9E9E" :weight bold)))
  "Face for direct (ASCII/latin) mode indicator."
  :group 'nskk-modeline)

(nskk-define-mode-entry ascii  "SKK" nskk-modeline-direct-face "Direct/ASCII input mode")
(nskk-define-mode-entry latin  "SKK" nskk-modeline-direct-face "Direct/ASCII input mode")
(nskk-define-mode-entry direct "SKK" nskk-modeline-direct-face "Direct/ASCII input mode")

;; Cursor color mapping
(nskk-prolog-set-index 'cursor-color 2 :hash)
(nskk-prolog-<- (cursor-color hiragana nskk-cursor-hiragana-color))
(nskk-prolog-<- (cursor-color katakana nskk-cursor-katakana-color))
(nskk-prolog-<- (cursor-color ascii nskk-cursor-latin-color))
(nskk-prolog-<- (cursor-color latin nskk-cursor-latin-color))
(nskk-prolog-<- (cursor-color jisx0208-latin nskk-cursor-jisx0208-latin-color))
(nskk-prolog-<- (cursor-color abbrev nskk-cursor-abbrev-color))
(nskk-prolog-<- (cursor-color direct nskk-cursor-latin-color))

(defun nskk-modeline-indicator ()
  "Return mode-line indicator string for the current NSKK input mode.
Queries Prolog `mode-info/4' for the display string, face, and
help-echo text.  The string is formatted via `nskk-modeline-format'.

Returns an empty string when `nskk-current-state' is nil or unbound
(state not yet initialized).  When the current mode has no `mode-info/4'
fact, falls back to \"NSKK\" with `default' face.

Intended for use as the `:eval' expression in `define-minor-mode'
`:lighter'; it is called on every mode-line redisplay and must be fast."
  (if (and (boundp 'nskk-current-state) nskk-current-state)
      (let* ((mode (nskk-state-mode nskk-current-state))
             (info (nskk-prolog-query-values
                    `(mode-info ,mode ,'\?s ,'\?f ,'\?h)
                    '(\?s \?f \?h)))
             (name (or (nth 0 info) "NSKK"))
             (face (or (nth 1 info) 'default))
             (help (or (nth 2 info) "NSKK input method"))
             (text (format-spec nskk-modeline-format `((?m . ,name)))))
        (propertize text 'face face 'help-echo help))
    ""))

(defun nskk-modeline-update ()
  "Update the modeline and cursor color to reflect current NSKK state."
  (nskk-cursor-update)
  (force-mode-line-update))

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
  "Return cursor color string for input MODE, or nil if none is registered.
MODE is a mode symbol such as `hiragana' or `ascii'.

Looks up a `cursor-color/2' Prolog fact for MODE.  The fact stores the
name of a bound `defcustom' variable (e.g. `nskk-cursor-hiragana-color'),
not a color string directly.  That variable is then dereferenced via
`symbol-value' so that runtime customization of `nskk-cursor-*-color'
variables is picked up without reloading.

Returns nil when no `cursor-color/2' fact exists for MODE or when the
resolved variable is not bound."
  (let ((color-var (nskk-prolog-query-value
                    `(cursor-color ,mode ,'\?c) '\?c)))
    (when (and color-var (boundp color-var))
      (symbol-value color-var))))

(provide 'nskk-modeline)

;;; nskk-modeline.el ends here
