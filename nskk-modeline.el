;;; nskk-modeline.el --- Mode line indicator for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Keywords: i18n

;; This file is NOT part of GNU Emacs.
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

;; Mode line indicator showing current input mode (Layer 5: Presentation).
;;
;; Layer position: L5 (Presentation) -- depends on nskk-state, nskk-prolog,
;;   nskk-custom, and nskk-cps-macros.
;;
;; Architecture:
;; - All mode display data (strings, faces, help text, cursor colors) is stored
;;   as unified `mode-properties/5' Prolog facts in nskk-state.el:
;;     (mode-properties MODE DISPLAY-STRING FACE HELP-TEXT CURSOR-FACE)
;; - This module queries `mode-properties/5' at display time; it does not
;;   maintain its own separate Prolog predicates for mode data.
;; - The `nskk-define-mode-entry' macro defines only the face for each mode;
;;   the corresponding `mode-properties/5' fact is declared in nskk-state.el.
;; - The mode-line lighter uses `(:eval (nskk-modeline-indicator))' in
;;   `define-minor-mode', so Emacs calls this function directly on each
;;   mode-line redisplay without manual `minor-mode-alist' mutation.
;;
;; Displays mode indicator in modeline:
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
;; - `nskk-cursor-*' faces: Cursor color per mode (defined in nskk-custom.el).

;;; Code:

(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-custom)
(require 'nskk-cps-macros)

(defvar-local nskk--last-cursor-color nil
  "Last cursor color applied, to avoid redundant `set-cursor-color' calls.")

;;;; Face Definitions

(defmacro nskk-define-mode-entry (mode _display face-or-spec _help)
  "Define the display face for input MODE.
MODE is the mode symbol (e.g., `hiragana').
_DISPLAY and _HELP are accepted for readability but unused here;
the corresponding `mode-properties/5' Prolog fact in nskk-state.el
is the single source of truth for display data.
FACE-OR-SPEC is either:
  - A list like (:foreground COLOR :weight WEIGHT): creates
    `nskk-modeline-MODE-face' with this spec.
  - A symbol naming an existing face: uses it directly (no-op)."
  (declare (indent 1) (debug t))
  (when (listp face-or-spec)
    (let ((face-sym (intern (format "nskk-modeline-%s-face" mode))))
      `(defface ,face-sym
         '((t ,face-or-spec))
         ,(format "Face for %s mode indicator." mode)
         :group 'nskk-modeline))))

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

;; ascii, latin, and direct modes share `nskk-modeline-direct-face', defined above.

;;;; Modeline Indicator

(defvar-local nskk--modeline-indicator-cache nil
  "Memoized modeline data as (MODE . (DISPLAY-STRING FACE HELP-TEXT)) or nil.
Invalidated when the current NSKK mode changes.")

(defun/k nskk--modeline-with-data (mode)
  "Fetch display data for MODE from cache or Prolog.
on-found is called with a list (DISPLAY-STRING FACE HELP-TEXT).
The result is memoized in `nskk--modeline-indicator-cache' keyed by
MODE so the Prolog engine is only queried when the mode changes.
Calls on-not-found when MODE has no `mode-properties/5' fact."
  (if (and nskk--modeline-indicator-cache
           (eq (car nskk--modeline-indicator-cache) mode))
      (succeed (cdr nskk--modeline-indicator-cache))
    (let ((info (nskk-prolog-query-values
                 `(mode-properties ,mode ,'\?s ,'\?f ,'\?h ,'\?c)
                 '(\?s \?f \?h))))
      (if info
          (progn (setq nskk--modeline-indicator-cache (cons mode info))
                 (succeed info))
        (fail)))))

(defun nskk-modeline-indicator ()
  "Return mode-line indicator string for the current NSKK input mode.
Queries `mode-properties/5' for the display string, face, and
help-echo text via `nskk--modeline-with-data'.  The string is
formatted via `nskk-modeline-format'.

Falls back to \"NSKK\" with `default' face when the current mode has
no `mode-properties/5' fact.  Returns an empty string when
`nskk-current-state' is nil or unbound."
  (if (and (boundp 'nskk-current-state) nskk-current-state)
      (let* ((mode (nskk-state-mode nskk-current-state))
             (data (nskk--modeline-with-data mode)))
        (if data
            (propertize (format-spec nskk-modeline-format
                                     `((?m . ,(nth 0 data))))
                        'face (nth 1 data)
                        'help-echo (nth 2 data))
          (propertize (format-spec nskk-modeline-format '((?m . "NSKK")))
                      'face 'default
                      'help-echo "NSKK input method")))
    ""))

(defun nskk--modeline-clear-cache ()
  "Clear the memoized modeline indicator cache.
Call this after any mode change to ensure the next redisplay
re-queries the Prolog database for the new mode's display data."
  (setq nskk--modeline-indicator-cache nil))

(defun/done nskk-modeline-update ()
  "Update the mode line and cursor color to reflect the current NSKK state.
Clears the memoized indicator cache, updates the cursor color, and
forces a mode-line redisplay."
  (nskk--modeline-clear-cache)
  (nskk-cursor-update)
  (force-mode-line-update))

;;;; Cursor Color

(defun nskk--cursor-with-color (mode)
  "Return cursor color string for input MODE, or nil if none is registered.
MODE is a mode symbol such as `hiragana' or `ascii'.
Returns nil when MODE has no `mode-properties/5' fact, the cursor
face is not defined, or its :background attribute is `unspecified'."
  (when-let* ((face (nskk-prolog-query-value
                     `(mode-properties ,mode ,'\?s ,'\?f ,'\?h ,'\?c) '\?c))
              (face-p (facep face))
              (color (face-attribute face :background nil t))
              (color-valid (not (memq color '(nil unspecified)))))
    color))

(defun/done nskk-cursor-update ()
  "Update cursor color to reflect the current NSKK mode.
Calls `set-cursor-color' only when the color differs from the last
applied color (`nskk--last-cursor-color').
Does nothing when `nskk-use-color-cursor' is nil or `nskk-current-state'
is unbound."
  (when (and nskk-use-color-cursor
             (boundp 'nskk-current-state)
             nskk-current-state)
    (when-let* ((mode (nskk-state-mode nskk-current-state))
                (color (nskk--cursor-with-color mode)))
      (when (not (equal color nskk--last-cursor-color))
        (set-cursor-color color)
        (setq nskk--last-cursor-color color)))))

(provide 'nskk-modeline)

;;; nskk-modeline.el ends here
