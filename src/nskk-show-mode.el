;;; nskk-show-mode.el --- Inline mode indicator for NSKK -*- lexical-binding: t; -*-
;; Copyright (C) 2026 NSKK Contributors
;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n convenience
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
(defgroup
  nskk-show-mode
  nil
  "Inline mode indicator settings for NSKK."
  :prefix
  "nskk-show-mode-"
  :group
  'nskk-ui)

(defcustom
  nskk-show-mode-show
  nil
  "When non-nil, display a brief mode indicator near the cursor on mode change.
The indicator shows the current input mode name (e.g. \"[か]\", \"[ア]\")
for a short duration then disappears automatically."
  :type
  'boolean
  :safe
  #'booleanp
  :package-version
  '(nskk . "0.1.0")
  :group
  'nskk-show-mode)

(defcustom
  nskk-show-mode-style
  'inline
  "Display style for the mode indicator.
\\='inline    -- display as an overlay after-string at point (default)
\\='tooltip   -- display using Emacs tooltip (GUI only)"
  :type
  '(choice (const :tag "Inline overlay" inline) (const :tag "Tooltip" tooltip))
  :safe
  #'symbolp
  :package-version
  '(nskk . "0.1.0")
  :group
  'nskk-show-mode)

(defcustom
  nskk-show-mode-duration
  1.0
  "Duration in seconds to display the inline mode indicator."
  :type
  'number
  :safe
  #'numberp
  :package-version
  '(nskk . "0.1.0")
  :group
  'nskk-show-mode)

;;;; Faces
(defface
  nskk-show-mode-inline-face
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for the inline mode indicator text."
  :package-version
  '(nskk . "0.1.0")
  :group
  'nskk-show-mode)

;;;; Buffer-Local State
(defvar-local
  nskk--show-mode-overlay
  nil
  "Overlay for displaying the inline mode indicator near the cursor.
Declared here following project convention that overlay vars live in
module that owns them (nskk-show-mode.el manages this overlay's lifecycle).")

(defvar-local
  nskk--show-mode-timer
  nil
  "Timer for auto-hiding the mode indicator overlay.
Cancelled and re-created each time the mode indicator is displayed.")

(progn
  (defvar-local
    nskk--show-mode-last-mode
    nil
    "Last mode for which the indicator was displayed.
Used to avoid redundant indicator display when mode has not changed.")

  (defvar-local
    nskk--show-mode-inline-generation
    0
    "Generation used to reject callbacks from replaced inline displays."))

;;;; Tooltip State
(defvar nskk--show-mode-tooltip-owner nil
  "Buffer that owns the currently visible mode tooltip.")

(defvar nskk--show-mode-tooltip-generation 0
  "Generation used to reject callbacks from replaced mode tooltips.")

(defvar nskk--show-mode-tooltip-timer nil
  "Timer for the currently visible mode tooltip.")

;;;; Internal Implementation
(defun nskk--show-mode-indicator-string (mode)
  "Return the bracket-wrapped indicator string for MODE.
Queries `mode-properties/5' for the display string, wraps it in brackets.
Returns nil when MODE has no `mode-properties/5' fact."
  (when-let*
    ((data
        (nskk-prolog-query-values
          `(mode-properties ,mode ,'\?s ,'\?f ,'\?h ,'\?c)
          '(\?s)))
      (display-str (car data)))
    (propertize (format "[%s]" display-str) 'face 'nskk-show-mode-inline-face)))

(defun nskk--show-mode-run-cleanups (&rest cleanups)
  "Run CLEANUPS in order, then re-signal the first error or quit."
  (let (first-condition)
    (dolist (cleanup cleanups)
      (condition-case
        condition
        (funcall cleanup)
        ((error quit)
          (unless first-condition
            (setq first-condition condition)))))
    (when first-condition
      (signal (car first-condition) (cdr first-condition)))))

(defun nskk--show-mode-clear-inline ()
  "Remove the current buffer's inline indicator and cancel its timer.
Cleanup attempts both resources before re-signaling the first failure."
  (cl-incf nskk--show-mode-inline-generation)
  (let ((overlay nskk--show-mode-overlay)
        (timer nskk--show-mode-timer))
    (setq nskk--show-mode-overlay nil
          nskk--show-mode-timer nil)
    (nskk--show-mode-run-cleanups
      (lambda ()
        (when (overlayp overlay)
          (delete-overlay overlay)))
      (lambda ()
        (when (timerp timer)
          (cancel-timer timer))))))

(progn
  (defun nskk--show-mode-hide-inline ()
    "Hide only the current buffer's inline indicator."
    (setq nskk--show-mode-last-mode nil)
    (nskk--show-mode-clear-inline))

  (defun nskk--show-mode-inline-timeout (owner generation)
    "Hide OWNER's inline indicator if GENERATION is still current."
    (when (buffer-live-p owner)
      (with-current-buffer owner
        (when (= generation nskk--show-mode-inline-generation)
          (nskk--show-mode-hide-inline))))))

(defun nskk--show-mode-release-tooltip (owner hide)
  "Release the global tooltip when OWNER still owns it.
When HIDE is non-nil, also hide the visible tooltip.  Return non-nil
when the tooltip was released.  Cleanup attempts every resource before
re-signaling the first failure."
  (when (and owner (eq owner nskk--show-mode-tooltip-owner))
    (cl-incf nskk--show-mode-tooltip-generation)
    (let ((timer nskk--show-mode-tooltip-timer))
      (setq nskk--show-mode-tooltip-owner nil
            nskk--show-mode-tooltip-timer nil)
      (nskk--show-mode-run-cleanups
        (lambda ()
          (when (timerp timer)
            (cancel-timer timer)))
        (lambda ()
          (when (buffer-live-p owner)
            (with-current-buffer
              owner
              (remove-hook 'kill-buffer-hook #'nskk--show-mode-tooltip-owner-killed t))))
        (lambda ()
          (when hide
            (tooltip-hide))))
      t)))

(defun nskk--show-mode-abort-tooltip ()
  "Fail closed by invalidating and hiding all global tooltip state."
  (cl-incf nskk--show-mode-tooltip-generation)
  (let ((owner nskk--show-mode-tooltip-owner)
        (timer nskk--show-mode-tooltip-timer))
    (setq nskk--show-mode-tooltip-owner nil
          nskk--show-mode-tooltip-timer nil)
    (when (timerp timer)
      (condition-case nil (cancel-timer timer) ((error quit) nil)))
    (when (buffer-live-p owner)
      (condition-case
        nil
        (with-current-buffer
          owner
          (remove-hook 'kill-buffer-hook #'nskk--show-mode-tooltip-owner-killed t))
        ((error quit) nil)))
    (condition-case nil (tooltip-hide) ((error quit) nil))))

(defun nskk--show-mode-tooltip-timeout (owner generation)
  "Hide OWNER's tooltip if GENERATION is still current."
  (when (and
      (eq owner nskk--show-mode-tooltip-owner)
      (= generation nskk--show-mode-tooltip-generation))
    (nskk--show-mode-release-tooltip owner t)))

(defun nskk--show-mode-tooltip-owner-killed ()
  "Release the global tooltip when its owner buffer is killed."
  (nskk--show-mode-release-tooltip (current-buffer) t))

(defun nskk--show-mode-hide ()
  "Hide indicators owned by the current buffer.
A tooltip owned by another buffer is left untouched."
  (let ((owner (current-buffer)))
    (nskk--show-mode-run-cleanups
      #'nskk--show-mode-hide-inline
      (lambda ()
        (nskk--show-mode-release-tooltip owner t)))))

(defun nskk--show-mode-display-inline (indicator-str)
  "Display INDICATOR-STR as an inline overlay after-string at point.
If cleanup or scheduling fails, remove all owned state and re-signal the
original condition."
  (let ((owner (current-buffer)))
    (condition-case condition
        (progn
          (nskk--show-mode-run-cleanups
            (lambda ()
              (nskk--show-mode-release-tooltip owner t))
            #'nskk--show-mode-clear-inline)
          (let ((pos (point)))
            (nskk-ensure-overlay
              nskk--show-mode-overlay
              pos
              pos
              'after-string
              indicator-str
              'priority
              100))
          (let ((generation nskk--show-mode-inline-generation))
            (setq nskk--show-mode-timer
                  (run-with-timer
                    nskk-show-mode-duration
                    nil
                    #'nskk--show-mode-inline-timeout
                    owner
                    generation))))
      ((error quit)
       (condition-case nil
           (nskk--show-mode-run-cleanups
             #'nskk--show-mode-hide-inline
             (lambda ()
               (nskk--show-mode-release-tooltip owner t)))
         ((error quit) nil))
       (signal (car condition) (cdr condition))))))

(defun nskk--show-mode-display-tooltip (indicator-str)
  "Display INDICATOR-STR using the Emacs tooltip API (GUI only).
If displaying, replacing, cleanup, or scheduling fails, release all owned
state and re-signal the original condition."
  (when (and (display-graphic-p) (posn-at-point))
    (let ((owner (current-buffer)))
      (condition-case
        condition
        (progn
          (nskk--show-mode-run-cleanups
            #'nskk--show-mode-clear-inline
            (lambda ()
              (when nskk--show-mode-tooltip-owner
                (nskk--show-mode-release-tooltip nskk--show-mode-tooltip-owner nil))))
          (tooltip-show indicator-str)
          (cl-incf nskk--show-mode-tooltip-generation)
          (setq nskk--show-mode-tooltip-owner owner)
          (add-hook 'kill-buffer-hook #'nskk--show-mode-tooltip-owner-killed nil t)
          (let ((generation nskk--show-mode-tooltip-generation))
            (setq nskk--show-mode-tooltip-timer (run-with-timer
                nskk-show-mode-duration
                nil
                #'nskk--show-mode-tooltip-timeout
                owner
                generation))))
        ((error quit)
          (setq nskk--show-mode-last-mode nil)
          (nskk--show-mode-abort-tooltip)
          (signal (car condition) (cdr condition)))))))

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
        (pcase nskk-show-mode-style
          ('tooltip (nskk--show-mode-display-tooltip indicator))
          (_        (nskk--show-mode-display-inline indicator)))
        (setq nskk--show-mode-last-mode mode)))))

(provide 'nskk-show-mode)

;;; nskk-show-mode.el ends here
