;;; nskk-annotation.el --- Annotation display for NSKK -*- lexical-binding: t; -*-

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
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Annotation (注釈) display for NSKK (Layer 5: Presentation).
;;
;; Layer position: L5 (Presentation) -- depends on nskk-custom,
;;   nskk-prolog, and nskk-cps-macros.
;;
;; When `nskk-show-annotation' is non-nil, displays annotation text
;; from dictionary entries alongside the conversion candidate.
;;
;; SKK dictionary entries can contain annotations in the format:
;;   読み /候補1;注釈1/候補2;注釈2/
;;
;; Annotations are stored in a separate Prolog predicate `dict-annotation/3'
;; keyed by (reading, candidate) pairs.  This is populated at dict load time
;; when `nskk-show-annotation' is non-nil.
;;
;; When enabled, annotation text is shown in the echo area when a candidate
;; with an annotation is displayed during conversion.  The annotation is
;; hidden once the candidate is committed.
;;
;; This is the nskk.el equivalent of ddskk's `skk-annotation.el'.
;;
;; Prolog predicates maintained by this module:
;; - `dict-annotation/3' -- (reading candidate annotation-text)
;;   Stores annotation strings for candidates that have ';annotation' in
;;   the dictionary entry.  Hash-indexed for O(1) lookup.

;;; Code:

(require 'cl-lib)
(require 'nskk-prolog)
(require 'nskk-custom)
(require 'nskk-cps-macros)

;;;; Customization

(defgroup nskk-annotation nil
  "Annotation display settings for NSKK."
  :prefix "nskk-annotation-"
  :group 'nskk-ui)

(defcustom nskk-show-annotation nil
  "When non-nil, display candidate annotations in the echo area.
Dictionary entries may include annotation text after ';' in candidate strings.
For example: /漢字;common kanji/感じ/ has an annotation for 漢字.
When enabled, the annotation text appears in the echo area alongside the
converted candidate during the \u25bc selection phase."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-annotation)

;;;; Faces

(defface nskk-annotation-face
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Face for annotation text displayed in the echo area."
  :group 'nskk-annotation)

;;;; Buffer-Local State

(defvar-local nskk--annotation-current nil
  "Annotation text for the currently displayed candidate, or nil.")

(defvar-local nskk--annotation-visible t
  "Whether annotation display is currently toggled on.
When nil, annotations are suppressed even if `nskk-show-annotation' is t.")

;;;; Prolog Infrastructure

(defvar nskk--annotation-initialized nil
  "Non-nil when `dict-annotation/3' Prolog facts have been registered.")

(defun nskk-annotation-initialize ()
  "Initialize Prolog infrastructure for annotation storage.
Sets up the `dict-annotation/3' hash-indexed fact table.
Idempotent: safe to call multiple times."
  (unless nskk--annotation-initialized
    (nskk-prolog-set-index 'dict-annotation 3 :hash)
    (setq nskk--annotation-initialized t)))

(defun nskk-annotation-register (reading candidate annotation)
  "Register ANNOTATION for CANDIDATE with READING in Prolog.
Stores as `dict-annotation/3' fact for O(1) retrieval at display time."
  (nskk-prolog-assert `((dict-annotation ,reading ,candidate ,annotation))))

(defun nskk-annotation-lookup (reading candidate)
  "Look up annotation for CANDIDATE with READING.
Returns the annotation string or nil if none registered."
  (when nskk--annotation-initialized
    (nskk-prolog-query-value
     `(dict-annotation ,reading ,candidate \?a) '\?a)))

;;;; Annotation Loading from Dictionary

(defun nskk--annotation-load-from-candidates (reading candidates-with-annots)
  "Register annotations from CANDIDATES-WITH-ANNOTS for READING.
CANDIDATES-WITH-ANNOTS is a list of (candidate . annotation-or-nil) pairs
as returned by `nskk--dict-parse-candidates-with-annotations'."
  (dolist (pair candidates-with-annots)
    (let ((candidate (car pair))
          (annotation (cdr pair)))
      (when (and annotation (not (string-empty-p annotation)))
        (nskk-annotation-register reading candidate annotation)))))

;;;; Internal Display

(defun nskk--annotation-format (annotation)
  "Return formatted annotation string for display in echo area.
Wraps ANNOTATION text in brackets with `nskk-annotation-face' applied."
  (when (and annotation (not (string-empty-p annotation)))
    (propertize (concat " [" annotation "]")
                'face 'nskk-annotation-face)))

;;;; Public API

(defun/done nskk-annotation-show-for-candidate (reading candidate)
  "Display annotation for CANDIDATE with READING if available.
Looks up annotation via `nskk-annotation-lookup' and shows it in the echo
area alongside the candidate text.  No-op when `nskk-show-annotation' is nil."
  (when nskk-show-annotation
    (let ((annotation (nskk-annotation-lookup reading candidate)))
      (setq nskk--annotation-current annotation)
      (when (and annotation nskk--annotation-visible)
        (let ((ann-str (nskk--annotation-format annotation)))
          (when ann-str
            (let ((message-log-max nil))
              (message "%s%s" candidate ann-str))))))))

(defun nskk-annotation-clear ()
  "Clear the current annotation state."
  (setq nskk--annotation-current nil))

(defun nskk-annotation-toggle-display ()
  "Toggle visibility of the current candidate annotation.
When hidden, shows annotation if one is available; when shown, hides it."
  (interactive)
  (setq nskk--annotation-visible (not nskk--annotation-visible))
  (if (and nskk--annotation-visible nskk--annotation-current)
      (let ((ann-str (nskk--annotation-format nskk--annotation-current)))
        (when ann-str
          (let ((message-log-max nil))
            (message "%s" ann-str))))
    (message nil)))

(provide 'nskk-annotation)

;;; nskk-annotation.el ends here
