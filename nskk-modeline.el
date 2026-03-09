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

;; Mode line indicator showing current input mode (Layer 5: Presentation Layer).
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
;; - `nskk-cursor-*' faces: Cursor color per mode (defined in nskk-custom.el).

;;; Code:

(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-custom)

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

(nskk-define-mode-entry ascii  "SKK" nskk-modeline-direct-face "Direct/ASCII input mode")
(nskk-define-mode-entry latin  "SKK" nskk-modeline-direct-face "Direct/ASCII input mode")
(nskk-define-mode-entry direct "SKK" nskk-modeline-direct-face "Direct/ASCII input mode")

;;;; Modeline Indicator

(defvar-local nskk--modeline-indicator-cache nil
  "Memoized modeline data as (MODE . (DISPLAY-STRING FACE HELP-TEXT)) or nil.
Invalidated when the current NSKK mode changes.")

(defun nskk-modeline--with-data (mode k)
  "Fetch display data for MODE (via cache or Prolog) and call K with it.
K is called with a list (DISPLAY-STRING FACE HELP-TEXT).  The result
is memoized in `nskk--modeline-indicator-cache' keyed by MODE so the
Prolog engine is only queried when the mode actually changes."
  (let ((data (if (and nskk--modeline-indicator-cache
                       (eq (car nskk--modeline-indicator-cache) mode))
                  (cdr nskk--modeline-indicator-cache)
                (let ((info (nskk-prolog-query-values
                             `(mode-properties ,mode ,'\?s ,'\?f ,'\?h ,'\?c)
                             '(\?s \?f \?h))))
                  (setq nskk--modeline-indicator-cache (cons mode info))
                  info))))
    (funcall k data)))

(defun nskk-modeline-indicator ()
  "Return mode-line indicator string for the current NSKK input mode.
Queries `mode-properties/5' for the display string, face, and
help-echo text via `nskk-modeline--with-data'.  The string is
formatted via `nskk-modeline-format'.

Returns an empty string when `nskk-current-state' is nil or unbound."
  (if (and (boundp 'nskk-current-state) nskk-current-state)
      (let ((mode (nskk-state-mode nskk-current-state)))
        (nskk-modeline--with-data
         mode
         (lambda (data)
           (let ((name (or (nth 0 data) "NSKK"))
                 (face (or (nth 1 data) 'default))
                 (help (or (nth 2 data) "NSKK input method")))
             (propertize (format-spec nskk-modeline-format `((?m . ,name)))
                         'face face 'help-echo help)))))
    ""))

(defun nskk-modeline--clear-cache ()
  "Clear the memoized modeline indicator cache.
Call this after any mode change to ensure the next redisplay
re-queries the Prolog database for the new mode's display data."
  (setq nskk--modeline-indicator-cache nil))

(defun nskk-modeline-update ()
  "Update the modeline and cursor color to reflect current NSKK state."
  (nskk-modeline--clear-cache)
  (nskk-cursor-update)
  (force-mode-line-update))

;;;; Cursor Color

(defun nskk-cursor--with-color (mode k)
  "Fetch the cursor color for MODE and call K with it, or return nil.
Looks up `mode-properties/5' for the cursor face symbol, reads its
:background attribute, and calls K only when a valid color is found.
Returns nil when MODE has no fact, the face is unbound, or :background
is `unspecified'."
  (when-let* ((face (nskk-prolog-query-value
                     `(mode-properties ,mode ,'\?s ,'\?f ,'\?h ,'\?c) '\?c))
              (_ (facep face))
              (color (face-attribute face :background nil t))
              (_ (not (memq color '(nil unspecified)))))
    (funcall k color)))

(defun nskk-cursor-update ()
  "Update cursor color to reflect current NSKK mode.
Only calls `set-cursor-color' when the color actually changes."
  (when (and nskk-use-color-cursor
             (boundp 'nskk-current-state)
             nskk-current-state)
    (when-let* ((mode (nskk-state-mode nskk-current-state))
                (color (nskk-cursor--mode-color mode))
                (_ (not (equal color nskk--last-cursor-color))))
      (set-cursor-color color)
      (setq nskk--last-cursor-color color))))

(defun nskk-cursor--mode-color (mode)
  "Return cursor color string for input MODE, or nil if none is registered.
MODE is a mode symbol such as `hiragana' or `ascii'.

Delegates to `nskk-cursor--with-color', passing `identity' as the
continuation.  Returns nil when no fact exists for MODE, the face is
not defined, or its :background is `unspecified'."
  (nskk-cursor--with-color mode #'identity))

(provide 'nskk-modeline)

;;; nskk-modeline.el ends here
