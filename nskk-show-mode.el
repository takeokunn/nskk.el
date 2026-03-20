;;; nskk-show-mode.el --- Inline mode indicator for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Keywords: i18n

;; This file is NOT part of GNU Emacs.

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

;; Inline mode indicator display for NSKK (Layer 5: Presentation).
;;
;; Layer position: L5 (Presentation) -- depends on nskk-state, nskk-prolog,
;;   nskk-custom, and nskk-cps-macros.
;;
;; When `nskk-show-mode-show' is non-nil, displays a brief mode indicator
;; near the cursor when the input mode changes.  The indicator looks like
;; "[か]", "[ア]", "[英]", "[SKK]" etc. and disappears after a short delay.
;;
;; This is the nskk.el equivalent of ddskk's `skk-show-mode.el'.
;;
;; Prolog predicates maintained by this module: none.
;; Mode display data is queried from `mode-properties/5' in nskk-state.el.

;;; Code:

(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-custom)
(require 'nskk-cps-macros)

;;;; Customization

(defgroup nskk-show-mode nil
  "Inline mode indicator settings for NSKK."
  :prefix "nskk-show-mode-"
  :group 'nskk-ui)

(defcustom nskk-show-mode-show nil
  "When non-nil, display a brief mode indicator near the cursor on mode change.
The indicator shows the current input mode name (e.g. \"[か]\", \"[ア]\")
for a short duration then disappears automatically."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-show-mode)

(defcustom nskk-show-mode-style 'inline
  "Display style for the mode indicator.
\\='inline    -- display as an overlay after-string at point (default)
\\='tooltip   -- display using Emacs tooltip (GUI only)"
  :type '(choice (const :tag "Inline overlay" inline)
                 (const :tag "Tooltip" tooltip))
  :safe #'symbolp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-show-mode)

(defcustom nskk-show-mode-duration 1.0
  "Duration in seconds to display the inline mode indicator."
  :type 'number
  :safe #'numberp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-show-mode)

;;;; Faces

(defface nskk-show-mode-inline-face
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for the inline mode indicator text."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-show-mode)

;;;; Buffer-Local State

(defvar-local nskk--show-mode-overlay nil
  "Overlay for displaying the inline mode indicator near the cursor.
Declared here following project convention that overlay vars live in
module that owns them (nskk-show-mode.el manages this overlay's lifecycle).")

(defvar-local nskk--show-mode-timer nil
  "Timer for auto-hiding the mode indicator overlay.
Cancelled and re-created each time the mode indicator is displayed.")

(defvar-local nskk--show-mode-last-mode nil
  "Last mode for which the indicator was displayed.
Used to avoid redundant indicator display when mode has not changed.")

;;;; Internal Implementation

(defun nskk--show-mode-indicator-string (mode)
  "Return the bracket-wrapped indicator string for MODE.
Queries `mode-properties/5' for the display string, wraps it in brackets.
Returns nil when MODE has no `mode-properties/5' fact."
  (when-let* ((data (nskk-prolog-query-values
                     `(mode-properties ,mode ,'\?s ,'\?f ,'\?h ,'\?c)
                     '(\?s)))
              (display-str (car data)))
    (propertize (format "[%s]" display-str)
                'face 'nskk-show-mode-inline-face)))

(defun nskk--show-mode-hide ()
  "Hide the mode indicator overlay and cancel the timer.
Safe to call when overlay and timer are nil."
  (nskk-delete-overlay nskk--show-mode-overlay)
  (when (timerp nskk--show-mode-timer)
    (cancel-timer nskk--show-mode-timer)
    (setq nskk--show-mode-timer nil))
  (setq nskk--show-mode-last-mode nil))

(defun nskk--show-mode-display-inline (indicator-str)
  "Display INDICATOR-STR as an inline overlay after-string at point."
  ;; Cancel any pending hide timer
  (when (timerp nskk--show-mode-timer)
    (cancel-timer nskk--show-mode-timer)
    (setq nskk--show-mode-timer nil))
  ;; Create/update overlay at point
  (let ((pos (point)))
    (nskk-ensure-overlay nskk--show-mode-overlay pos pos
      'after-string indicator-str
      'priority 100))
  ;; Schedule automatic removal
  (let ((buf (current-buffer)))
    (setq nskk--show-mode-timer
          (run-with-timer nskk-show-mode-duration nil
                          (lambda ()
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (nskk--show-mode-hide))))))))

(defun nskk--show-mode-display-tooltip (indicator-str)
  "Display INDICATOR-STR using the Emacs tooltip API (GUI only)."
  (when (and (display-graphic-p) (posn-at-point))
    (tooltip-show indicator-str)
    (when (timerp nskk--show-mode-timer)
      (cancel-timer nskk--show-mode-timer))
    (setq nskk--show-mode-timer
          (run-with-timer nskk-show-mode-duration nil #'tooltip-hide))))

;;;; Public API

(defun/done nskk-show-mode-display ()
  "Display mode indicator near cursor if `nskk-show-mode-show' is non-nil.
Queries the current mode from `nskk-current-state', builds the indicator
string via `mode-properties/5', and displays it briefly.
No-op when `nskk-show-mode-show' is nil or state is unset."
  (when (and nskk-show-mode-show
             (boundp 'nskk-current-state)
             nskk-current-state)
    (let* ((mode (nskk-state-mode nskk-current-state))
           (indicator (nskk--show-mode-indicator-string mode)))
      ;; Only display when mode changed and indicator string is available
      (when (and indicator
                 (not (eq mode nskk--show-mode-last-mode)))
        (setq nskk--show-mode-last-mode mode)
        (pcase nskk-show-mode-style
          ('tooltip (nskk--show-mode-display-tooltip indicator))
          (_        (nskk--show-mode-display-inline indicator)))))))

(provide 'nskk-show-mode)

;;; nskk-show-mode.el ends here
