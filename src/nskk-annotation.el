;;; nskk-annotation.el --- Annotation display for NSKK -*- lexical-binding: t; -*-

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

;; Annotation display for NSKK.

;;; Code:

(require 'subr-x)
(require 'nskk-prolog)
(require 'nskk-cps-macros)
(require 'nskk-state)

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
converted candidate during the ▼ selection phase."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-annotation)

;;;; Faces

(defface nskk-annotation-face
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Face for annotation text displayed in the echo area."
  :package-version '(nskk . "0.1.0")
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

(nskk-prolog-<- (module-initialized-flag nskk--annotation-initialized))

(defun nskk-annotation-initialize ()
  "Initialize annotation storage.  Safe to call more than once."
  (unless nskk--annotation-initialized
    (nskk-prolog-set-index 'dict-annotation 3 :hash)
    (setq nskk--annotation-initialized t)))

(defun nskk-annotation-register (reading candidate annotation)
  "Register ANNOTATION for CANDIDATE with READING.
Lookup returns the first annotation registered for a given READING and
CANDIDATE.  Later registrations for the same pair are still stored, but
are never returned."
  (nskk-prolog-assert `((dict-annotation ,reading ,candidate ,annotation))))

(defun/k nskk-annotation-lookup (reading candidate)
  "Return the annotation registered for CANDIDATE with READING.
Fails when none is registered, which the synchronous wrapper reports as nil."
  (let ((annotation
         (and nskk--annotation-initialized
              (nskk-prolog-query-value
               `(dict-annotation ,reading ,candidate \?a) '\?a))))
    (if annotation
        (succeed annotation)
      (fail))))

;;;; Annotation Loading from Dictionary

(defun nskk-annotation-load-from-candidates (reading candidates-with-annots)
  "Register annotations from CANDIDATES-WITH-ANNOTS for READING.
CANDIDATES-WITH-ANNOTS is a list of (candidate . annotation-or-nil) pairs.
Pairs whose annotation is nil or empty are skipped."
  (dolist (pair candidates-with-annots)
    (let ((candidate (car pair))
          (annotation (cdr pair)))
      (when (and annotation (not (string-empty-p annotation)))
        (nskk-annotation-register reading candidate annotation)))))

;;;; Internal Display

(defun/k nskk--annotation-format (annotation)
  "Return ANNOTATION bracketed and faced for echo-area display.
Fails when ANNOTATION is nil or empty, which the synchronous wrapper
reports as nil."
  (if (and annotation (not (string-empty-p annotation)))
      (succeed (nskk-display-sanitize annotation 'nskk-annotation-face " [" "]"))
    (fail)))

(defun nskk--annotation-echo (format-string &rest args)
  "Show FORMAT-STRING formatted with ARGS in the echo area without logging it."
  (let ((message-log-max nil))
    (apply #'message format-string args)))

;;;; Public API

(defun/done nskk-annotation-show-for-candidate (reading candidate)
  "Display the annotation for CANDIDATE with READING in the echo area.
Sets `nskk--annotation-current' to the annotation found, or to nil.
Performs no lookup when `nskk-show-annotation' is nil.
Clears the previous annotation first, so a failing lookup cannot leave the
prior candidate's annotation current."
  (setq nskk--annotation-current nil)
  (when nskk-show-annotation
    (setq nskk--annotation-current
          (nskk-annotation-lookup reading candidate)))
  (when-let* (((and nskk--annotation-current nskk--annotation-visible))
              (ann-str (nskk--annotation-format nskk--annotation-current)))
    (nskk--annotation-echo "%s%s" (substring-no-properties candidate) ann-str)))

(defun nskk-annotation-clear ()
  "Clear the current annotation state."
  (setq nskk--annotation-current nil))

;;;###autoload
(defun nskk-annotation-toggle-display ()
  "Toggle whether the current candidate's annotation is shown.
Clears the echo area when toggled off or when no annotation is current."
  (interactive)
  (setq nskk--annotation-visible (not nskk--annotation-visible))
  (if (and nskk--annotation-visible nskk--annotation-current)
      (when-let* ((ann-str (nskk--annotation-format nskk--annotation-current)))
        (nskk--annotation-echo "%s" ann-str))
    (message nil)))

(provide 'nskk-annotation)

;;; nskk-annotation.el ends here
