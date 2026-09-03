;;; nskk-modeline.el --- Mode line indicator for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n convenience

;; This file is NOT part of GNU Emacs.
;;
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

;; Mode line indicator for NSKK.

;;; Code:

(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-custom)
(require 'nskk-cps-macros)

;; Optional: nskk-show-mode provides inline mode indicator on mode change.
(declare-function nskk-show-mode-display "nskk-show-mode")

(defconst nskk--last-cursor-color-parameter 'nskk--last-cursor-color
  "Frame parameter holding the last cursor color NSKK applied.")

(defconst nskk--saved-cursor-color-parameter 'nskk--saved-cursor-color
  "Frame parameter holding the cursor color saved before NSKK activation.")

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
forces a mode-line redisplay.  Also triggers inline mode indicator display
when `nskk-show-mode-show' is non-nil (via `nskk-show-mode-display')."
  (nskk--modeline-clear-cache)
  (nskk-cursor-update)
  (force-mode-line-update)
  (when (and (boundp 'nskk-show-mode-show) nskk-show-mode-show
             (fboundp 'nskk-show-mode-display))
    (nskk-show-mode-display)))

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
  "Update the selected frame cursor color for the current NSKK mode.
The frame original is saved immediately before its first NSKK color change.
Calls `set-cursor-color` only when the color differs from the last color
applied to that frame.  Does nothing when cursor coloring is disabled or
`nskk-current-state` is unavailable."
  (when (and nskk-use-color-cursor
             (boundp (quote nskk-current-state))
             nskk-current-state)
    (when-let* ((mode (nskk-state-mode nskk-current-state))
                (color (nskk--cursor-with-color mode))
                (frame (selected-frame)))
      (nskk-cursor-color-save frame)
      (unless
          (equal
           color
           (frame-parameter frame nskk--last-cursor-color-parameter))
        (set-cursor-color color)
        (set-frame-parameter
         frame nskk--last-cursor-color-parameter color)))))

(defun nskk-cursor-color-save (&optional frame)
  "Save FRAME cursor color before NSKK changes it.
FRAME defaults to the selected frame.  Each frame keeps its own idempotent
snapshot.  The sentinel t records a nil cursor color so nil continues to
mean that no snapshot exists."
  (when nskk-use-color-cursor
    (let ((target-frame (or frame (selected-frame))))
      (when (null
             (frame-parameter
              target-frame nskk--saved-cursor-color-parameter))
        (set-frame-parameter
         target-frame nskk--saved-cursor-color-parameter
         (or (frame-parameter target-frame (quote cursor-color)) t))))))

(defun nskk--other-nskk-buffers-active-p (&optional frame)
  "Return non-nil if another active NSKK buffer is displayed on FRAME.
FRAME defaults to the selected frame.  The current buffer is excluded
because this predicate is called while that buffer is being disabled."
  (let ((target-frame (or frame (selected-frame))))
    (and (boundp 'nskk-mode)
         (catch 'found
           (dolist (buf (buffer-list))
             (when (and (buffer-live-p buf)
                        (not (eq buf (current-buffer)))
                        (buffer-local-value 'nskk-mode buf)
                        (get-buffer-window buf target-frame))
               (throw 'found t)))))))

(defun nskk-cursor-color-restore (&optional frame all-frames)
  "Restore cursor colors saved by `nskk-cursor-color-save`.
FRAME defaults to the selected frame.  When ALL-FRAMES is non-nil,
consider every live frame with an NSKK snapshot.  A frame is retained
while another active NSKK buffer is displayed there.  Otherwise its exact
saved color and bookkeeping parameters are restored without selecting it."
  (if all-frames
      (dolist (target-frame (frame-list))
        (when (frame-parameter
               target-frame nskk--saved-cursor-color-parameter)
          (nskk-cursor-color-restore target-frame)))
    (let* ((target-frame (or frame (selected-frame)))
           (saved-color
            (frame-parameter
             target-frame nskk--saved-cursor-color-parameter)))
      (unless (nskk--other-nskk-buffers-active-p target-frame)
        (when saved-color
          (set-frame-parameter
           target-frame (quote cursor-color)
           (unless (eq saved-color t) saved-color)))
        (set-frame-parameter
         target-frame nskk--saved-cursor-color-parameter nil)
        (set-frame-parameter
         target-frame nskk--last-cursor-color-parameter nil)))))

(provide 'nskk-modeline)

;;; nskk-modeline.el ends here
