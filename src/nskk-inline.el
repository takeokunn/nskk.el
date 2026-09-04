;;; nskk-inline.el --- Inline candidate display for NSKK -*- lexical-binding: t; -*-

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

;; Inline candidate display for NSKK: a zero-length overlay carrying an
;; `after-string' at the end of the conversion overlay, so the current
;; candidate is shown without inserting buffer text.
;;
;; nskk-henkan.el never calls into this module by name.  Everything is driven
;; through the presentation-action table in nskk-prolog.el, and the four phases
;; registered at the bottom of this file are the whole interface:
;;
;;   show-candidate           -- draw the current candidate
;;   show-registration-badge  -- draw the nested-registration notice
;;   cleanup                  -- remove the overlay
;;   finalize                 -- remove it again; see `nskk--inline-finalize'
;;
;; Candidates come from dictionaries and are untrusted, so each one passes
;; through `nskk-display-sanitize' before it reaches an overlay.  The overlay
;; priority comes from `nskk-overlay-priority-inline' in nskk-state.el, which
;; also records why that number guarantees less than it appears to.

;;; Code:

(require 'subr-x)
(require 'nskk-state)
(require 'nskk-custom)
(require 'nskk-cps-macros)
(require 'nskk-prolog)

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
                 (const :tag "Inline horizontal (explicit symbol)" horizontal)
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
Departs from the nskk-state.el convention that buffer-local overlay
variables are declared there: no code outside this file creates, moves or
deletes it, so there is no cross-module owner for state.el to arbitrate.")

;;;; Internal

(defun nskk--inline-anchor ()
  "Return the buffer position to anchor the inline display overlay.
Uses the end of `nskk-state-conversion-overlay' when available,
falling back to point."
  (or (and (overlayp (nskk-state-conversion-overlay))
           (overlay-end (nskk-state-conversion-overlay)))
      (point)))

(defun nskk--inline-render (candidate style)
  "Return the overlay after-string showing CANDIDATE under STYLE.
STYLE is an `nskk-show-inline' value: `vertical' puts CANDIDATE on the
line below the preedit, any other non-nil value appends it to the preedit
line.  CANDIDATE is untrusted dictionary text and is sanitized here; the
separator carries the face along with the candidate."
  (nskk-display-sanitize candidate 'nskk-inline-face
                         (if (eq style 'vertical) "\n" " ")))

(defun/done nskk--inline-finalize ()
  "Remove the inline overlay again, in the `finalize' cleanup phase.
Duplicating `nskk-inline-hide' is deliberate.
`nskk--run-presentation-actions' in nskk-henkan.el runs every callback
through a `condition-case' that swallows `error' and `quit', so a
`cleanup'-phase callback that signals leaves the overlay behind and
`finalize' is the sweep that still removes it.  Registering
`nskk-inline-hide' for both phases would not do: the fault-injection test
in test/unit/nskk-henkan-test.el stubs `nskk-inline-hide' to always
signal and then asserts the overlay is gone, which only a second,
separately-named callback can satisfy."
  (nskk-delete-overlay nskk--inline-overlay))

;;;; Public API

(defun/done nskk-inline-show-candidate (candidate)
  "Display CANDIDATE inline if `nskk-show-inline' is non-nil.
`nskk-show-inline' also selects the layout; see `nskk--inline-render'.
No-op when `nskk-show-inline' is nil or CANDIDATE is nil or empty."
  (when (and nskk-show-inline candidate (not (string-empty-p candidate)))
    (let ((anchor (nskk--inline-anchor)))
      (nskk-ensure-overlay nskk--inline-overlay anchor anchor
        'after-string (nskk--inline-render candidate nskk-show-inline)
        'priority nskk-overlay-priority-inline))))

(defun/done nskk-inline-hide ()
  "Hide the inline candidate display overlay."
  (nskk-delete-overlay nskk--inline-overlay))

(defun/done nskk-inline-show-registration-badge ()
  "Display the dictionary registration badge inline.
Shows \"↓辞書登録中↓\" at the conversion point when `nskk-show-inline'
is non-nil, telling the user that typing now feeds a nested dictionary
registration rather than the buffer.

The badge takes its own line whatever style `nskk-show-inline' selects.
It is a panel rather than a candidate, the same shape as the
multi-candidate completion panel in nskk-henkan.el.  The newline stays
outside the `propertize' so it carries no face, unlike the separator in
`nskk--inline-render'."
  (when nskk-show-inline
    (let ((anchor (nskk--inline-anchor)))
      (nskk-ensure-overlay nskk--inline-overlay anchor anchor
        'after-string (concat "\n" (propertize
                                    "↓辞書登録中↓"
                                    'face 'nskk-jisyo-registration-badge-face))
        'priority nskk-overlay-priority-inline))))

(nskk-prolog-register-presentation-action 'cleanup 'nskk-inline-hide)
(nskk-prolog-register-presentation-action 'finalize 'nskk--inline-finalize)
(nskk-prolog-register-presentation-action
 'show-candidate 'nskk-inline-show-candidate)
(nskk-prolog-register-presentation-action
 'show-registration-badge 'nskk-inline-show-registration-badge)

(provide 'nskk-inline)

;;; nskk-inline.el ends here
