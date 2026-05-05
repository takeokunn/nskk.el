;;; nskk-inline.el --- Inline candidate display for NSKK -*- lexical-binding: t; -*-

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

;; Inline candidate display for NSKK (Layer 5: Presentation).
;;
;; Layer position: L5 (Presentation) -- depends on nskk-state, nskk-custom,
;;   and nskk-cps-macros.
;;
;; When `nskk-show-inline' is non-nil, displays conversion candidates
;; inline in the buffer rather than in the echo area.  This is the nskk.el
;; equivalent of ddskk's `skk-inline.el' and `skk-show-inline' variable.
;;
;; Display modes:
;; - nil            : Use echo area (default, no inline display)
;; - t or 'horizontal : Show candidate to the right of the conversion point
;; - 'vertical      : Show candidate below the conversion point
;;
;; The inline display uses Emacs overlays with `after-string' property,
;; anchored at the end of the conversion overlay.
;;
;; Dictionary registration mode badge:
;; When entering dictionary registration mode, an inline badge
;; \"↓辞書登録中↓\" is displayed at the conversion point.  This is
;; controlled by `nskk-show-inline' being non-nil.
;;
;; Prolog predicates maintained by this module: none.
;; State is tracked via the shared `nskk--conversion-overlay' in nskk-state.el.

;;; Code:

(require 'subr-x)
(require 'nskk-state)
(require 'nskk-custom)
(require 'nskk-cps-macros)

;;;; Customization

(defgroup nskk-inline nil
  "Inline candidate display settings for NSKK."
  :prefix "nskk-inline-"
  :group 'nskk-ui)

(defcustom nskk-show-inline nil
  "When non-nil, display conversion candidates inline in the buffer.
Possible values:
  nil               -- Use echo area only (default)
  t or \\='horizontal -- Show candidate to the right of preedit (horizontal)
  \\='vertical        -- Show candidate below preedit (vertical, one per line)"
  :type '(choice (const :tag "Echo area only" nil)
                 (const :tag "Inline horizontal" t)
                 (const :tag "Inline vertical" vertical))
  :safe (lambda (v) (memq v '(nil t vertical horizontal)))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-inline)

;;;; Faces

(defface nskk-inline-face
  '((t (:inherit shadow :slant italic)))
  "Face for the inline candidate display text."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-inline)

(defface nskk-jisyo-registration-badge-face
  '((t (:inherit font-lock-warning-face :weight bold)))
  "Face for the dictionary registration badge \"↓辞書登録中↓\"."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-inline)

;;;; Buffer-Local State

(defvar-local nskk--inline-overlay nil
  "Overlay for displaying inline candidate text.
Zero-length overlay anchored at the end of the conversion overlay.
Declared in nskk-inline.el since this overlay is managed entirely here.")

;;;; Internal Helpers

(defun nskk--inline-anchor ()
  "Return the buffer position to anchor the inline display overlay.
Uses the end of `nskk--conversion-overlay' when available,
falling back to point."
  (or (and (overlayp nskk--conversion-overlay)
           (overlay-end nskk--conversion-overlay))
      (point)))

(defun nskk--inline-build-horizontal (candidate)
  "Build inline display string for CANDIDATE (horizontal style).
Shows candidate as a grayed-out suffix after the preedit text."
  (propertize (concat " " candidate)
              'face 'nskk-inline-face))

(defun nskk--inline-build-vertical (candidate)
  "Build inline display string for CANDIDATE (vertical style).
Shows candidate on the next line below the preedit text."
  (propertize (concat "\n" candidate)
              'face 'nskk-inline-face))

;;;; Public API

(defun/done nskk-inline-show-candidate (candidate)
  "Display CANDIDATE inline if `nskk-show-inline' is non-nil.
The display style (horizontal/vertical) is controlled by `nskk-show-inline'.
No-op when `nskk-show-inline' is nil."
  (when (and nskk-show-inline candidate (not (string-empty-p candidate)))
    (let* ((style nskk-show-inline)
           (display-str (pcase style
                          ('vertical (nskk--inline-build-vertical candidate))
                          (_ (nskk--inline-build-horizontal candidate))))
           (anchor (nskk--inline-anchor)))
      (nskk-ensure-overlay nskk--inline-overlay anchor anchor
        'after-string display-str
        'priority 98))))

(defun/done nskk-inline-hide ()
  "Hide the inline candidate display overlay."
  (nskk-delete-overlay nskk--inline-overlay))

(defun/done nskk-inline-show-registration-badge ()
  "Display the dictionary registration badge inline.
Shows \"↓辞書登録中↓\" at the conversion point when `nskk-show-inline'
is non-nil.  This badge indicates that the user is in dictionary
registration mode."
  (when nskk-show-inline
    (let* ((badge (propertize "↓辞書登録中↓"
                              'face 'nskk-jisyo-registration-badge-face))
           (anchor (nskk--inline-anchor)))
      (nskk-ensure-overlay nskk--inline-overlay anchor anchor
        'after-string (concat "\n" badge)
        'priority 98))))

(provide 'nskk-inline)

;;; nskk-inline.el ends here
