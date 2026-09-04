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
(require 'seq)

(declare-function nskk-show-mode-display "nskk-show-mode")

(defconst nskk--last-cursor-color-parameter 'nskk--last-cursor-color
  "Frame parameter holding the last cursor color NSKK applied.")

(defconst nskk--saved-cursor-color-parameter 'nskk--saved-cursor-color
  "Frame parameter holding the cursor color saved before NSKK activation.")

;;;; Face Definitions

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
  "Face for jisx0208-latin mode indicator."
  :group 'nskk-modeline)

(defface nskk-modeline-direct-face
  '((t (:foreground "#9E9E9E" :weight bold)))
  "Face for direct (ASCII/latin) mode indicator."
  :group 'nskk-modeline)

;;;; Modeline Indicator

(defvar-local nskk--modeline-indicator-cache nil
  "Memoized modeline data as (MODE . (DISPLAY-STRING FACE HELP-TEXT)) or nil.
Invalidated when the current NSKK mode changes.")

(defun nskk--modeline-cache-put (mode info)
  "Memoize INFO as the modeline display data for MODE and return INFO."
  (setq nskk--modeline-indicator-cache (cons mode info))
  info)

(defun/k nskk--modeline-with-data (mode)
  "Fetch display data for MODE from cache or Prolog.
The result is memoized in `nskk--modeline-indicator-cache' keyed by
MODE so the Prolog engine is only queried when the mode changes.
Sync wrapper returns a list (DISPLAY-STRING FACE HELP-TEXT), or nil when
MODE has no `mode-properties/5' fact.
The /k variant calls ON-FOUND with that list, ON-NOT-FOUND otherwise."
  (if (and nskk--modeline-indicator-cache
           (eq (car nskk--modeline-indicator-cache) mode))
      (succeed (cdr nskk--modeline-indicator-cache))
    (let ((info (nskk-prolog-query-values
                 `(mode-properties ,mode ,'\?s ,'\?f ,'\?h ,'\?c)
                 '(\?s \?f \?h))))
      (if info
          (succeed (nskk--modeline-cache-put mode info))
        (fail)))))

(defun nskk--modeline-render (display face help)
  "Return the mode-line lighter for DISPLAY carrying FACE and HELP."
  (propertize (format-spec nskk-modeline-format `((?m . ,display)))
              'face face
              'help-echo help))

(defun nskk-modeline-indicator ()
  "Return mode-line indicator string for the current NSKK input mode.
Queries `mode-properties/5' for the display string, face, and
help-echo text via `nskk--modeline-with-data/k'.  The string is
formatted via `nskk-modeline-format'.

Falls back to \"NSKK\" with `default' face when the current mode has
no `mode-properties/5' fact.  Returns an empty string when
`nskk-current-state' is nil or unbound."
  (if (bound-and-true-p nskk-current-state)
      (nskk--modeline-with-data/k
       (nskk-state-mode nskk-current-state)
       (lambda (data) (apply #'nskk--modeline-render data))
       (lambda () (nskk--modeline-render "NSKK" 'default "NSKK input method")))
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

(defun/k nskk--cursor-with-color (mode)
  "Return the cursor color string for input MODE.
MODE is a mode symbol such as `hiragana' or `ascii'.
Sync wrapper returns nil when MODE has no `mode-properties/5' fact, the
cursor face is not defined, or its :background attribute is `unspecified'.
The /k variant calls ON-FOUND with the color, ON-NOT-FOUND otherwise."
  (let* ((face (nskk-prolog-query-value
                `(mode-properties ,mode ,'\?s ,'\?f ,'\?h ,'\?c) '\?c))
         (color (and face (facep face) (face-attribute face :background nil t))))
    (if (and color (not (eq color 'unspecified)))
        (succeed color)
      (fail))))

(defun nskk--cursor-apply-color (frame color)
  "Set FRAME's cursor to COLOR unless FRAME already holds that COLOR."
  (unless (equal color (frame-parameter frame nskk--last-cursor-color-parameter))
    (set-cursor-color color)
    (set-frame-parameter frame nskk--last-cursor-color-parameter color)))

(defun/done nskk-cursor-update ()
  "Update the selected frame cursor color for the current NSKK mode.
The frame original is saved immediately before its first NSKK color change.
Calls `set-cursor-color' only when the color differs from the last color
applied to that frame.  Does nothing when cursor coloring is disabled or
`nskk-current-state' is unavailable."
  (when (and nskk-use-color-cursor (bound-and-true-p nskk-current-state))
    (when-let* ((mode (nskk-state-mode nskk-current-state))
                (color (nskk--cursor-with-color mode))
                (frame (selected-frame)))
      (nskk-cursor-color-save frame)
      (nskk--cursor-apply-color frame color))))

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
         (or (frame-parameter target-frame 'cursor-color) t))))))

(defun nskk--other-nskk-buffers-active-p (&optional frame)
  "Return non-nil if another active NSKK buffer is displayed on FRAME.
FRAME defaults to the selected frame.  The current buffer is excluded
because this predicate is called while that buffer is being disabled."
  (let ((target-frame (or frame (selected-frame))))
    (and (boundp 'nskk-mode)
         (seq-some (lambda (buf)
                     (and (buffer-live-p buf)
                          (not (eq buf (current-buffer)))
                          (buffer-local-value 'nskk-mode buf)
                          (get-buffer-window buf target-frame)))
                   (buffer-list))
         t)))

(defun nskk--cursor-color-restore-frame (frame)
  "Restore FRAME's saved cursor color and clear NSKK's bookkeeping parameters.
Does nothing while another active NSKK buffer is still displayed on FRAME."
  (let ((saved-color (frame-parameter frame nskk--saved-cursor-color-parameter)))
    (unless (nskk--other-nskk-buffers-active-p frame)
      (when saved-color
        (set-frame-parameter frame 'cursor-color
                             (unless (eq saved-color t) saved-color)))
      (set-frame-parameter frame nskk--saved-cursor-color-parameter nil)
      (set-frame-parameter frame nskk--last-cursor-color-parameter nil))))

(defun nskk-cursor-color-restore (&optional frame all-frames)
  "Restore cursor colors saved by `nskk-cursor-color-save'.
FRAME defaults to the selected frame.  When ALL-FRAMES is non-nil,
consider every live frame with an NSKK snapshot.  A frame is retained
while another active NSKK buffer is displayed there.  Otherwise its exact
saved color and bookkeeping parameters are restored without selecting it."
  (if all-frames
      (dolist (target-frame (frame-list))
        (when (frame-parameter target-frame nskk--saved-cursor-color-parameter)
          (nskk--cursor-color-restore-frame target-frame)))
    (nskk--cursor-color-restore-frame (or frame (selected-frame)))))

(provide 'nskk-modeline)

;;; nskk-modeline.el ends here
