;;; nskk-candidate-window.el --- Candidate window UI for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda
;;
;; This file is part of NSKK (Next-generation SKK).
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

;; Author: NSKK Developers
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

;;; Commentary:

;; Candidate window UI using overlays for display (Layer 1: UI Layer).
;;
;; Layer Responsibilities:
;; - UI Layer displays candidate window to the user
;; - Receives candidate data from Application Layer
;; - Does NOT directly access state management (nskk-state)
;; - Does NOT directly query Core Engine or Data Access Layer
;;
;; Provides an overlay-based candidate window that:
;; - Shows up to 9 candidates (numbered 1-9)
;; - Supports page navigation for more candidates
;; - Highlights selected candidate
;; - Displays candidate annotations inline
;; - Automatic positioning near point

;;; Code:

;; No state dependency - UI layer receives data through function parameters

(defgroup nskk-candidate-window nil
  "Candidate window UI for NSKK."
  :prefix "nskk-candidate-window-"
  :group 'nskk)

(defcustom nskk-candidate-window-max-candidates 9
  "Maximum number of candidates to display per page."
  :type 'integer
  :group 'nskk-candidate-window)

(defcustom nskk-candidate-window-show-numbers t
  "Show candidate numbers (1-9) in the window."
  :type 'boolean
  :group 'nskk-candidate-window)

(defface nskk-candidate-face
  '((t (:inherit default)))
  "Face for candidate text."
  :group 'nskk-candidate-window)

(defface nskk-candidate-selected-face
  '((t (:inherit highlight :weight bold)))
  "Face for selected candidate."
  :group 'nskk-candidate-window)

(defface nskk-candidate-annotation-face
  '((t (:inherit shadow :height 0.85)))
  "Face for candidate annotations."
  :group 'nskk-candidate-window)

(defvar nskk--candidate-overlay nil
  "Overlay for candidate window display.")

(defvar nskk--candidate-page 0
  "Current page number (0-indexed).")

(defvar nskk--candidate-index 0
  "Current candidate index on page (0-indexed).")

(defun nskk-candidate-window-show (candidates start-index)
  "Show candidate window with CANDIDATES starting at START-INDEX."
  (let ((page-candidates (seq-subseq candidates
                                     start-index
                                     (min (+ start-index nskk-candidate-window-max-candidates)
                                          (length candidates))))
        (buffer (current-buffer))
        (point (point)))
    (nskk-candidate-window-hide)
    (setq nskk--candidate-overlay
          (make-overlay point point buffer))
    (overlay-put nskk--candidate-overlay 'before-string
                 (nskk-candidate-window--format page-candidates start-index))
    (overlay-put nskk--candidate-overlay 'window (selected-window))))

(defun nskk-candidate-window-hide ()
  "Hide the candidate window."
  (when nskk--candidate-overlay
    (delete-overlay nskk--candidate-overlay)
    (setq nskk--candidate-overlay nil)))

(defun nskk-candidate-window--format (candidates start-index)
  "Format CANDIDATES for display, starting numbering at START-INDEX."
  (let ((lines nil))
    (cl-loop for candidate in candidates
             for i from start-index
             for page-i from 0
             do
             (let ((prefix (if nskk-candidate-window-show-numbers
                               (format "%d. " (1+ page-i))
                             ""))
                   (face (if (= page-i nskk--candidate-index)
                            'nskk-candidate-selected-face
                          'nskk-candidate-face))
                   (annotation (nskk-candidate-window--annotation candidate)))
               (push (concat prefix
                            (propertize candidate 'face face)
                            (if annotation
                                (concat " "
                                        (propertize annotation
                                                   'face 'nskk-candidate-annotation-face))
                              ""))
                     lines)))
    (string-join (nreverse lines) "\n")))

(defun nskk-candidate-window--annotation (_candidate)
  "Get annotation for _CANDIDATE."
  ;; TODO: Implement annotation lookup
  nil)

(defun nskk-candidate-window-next-page (total-candidates)
  "Move to next page of candidates.
TOTAL-CANDIDATES is the total number of available candidates."
  (let ((max-page (/ (1- total-candidates) nskk-candidate-window-max-candidates)))
    (when (< nskk--candidate-page max-page)
      (cl-incf nskk--candidate-page)
      (setq nskk--candidate-index 0)
      t)))

(defun nskk-candidate-window-prev-page ()
  "Move to previous page of candidates."
  (when (> nskk--candidate-page 0)
    (cl-decf nskk--candidate-page)
    (setq nskk--candidate-index 0)
    t))

(defun nskk-candidate-window-next ()
  "Move to next candidate on current page."
  (when (< nskk--candidate-index (1- nskk-candidate-window-max-candidates))
    (cl-incf nskk--candidate-index)
    t))

(defun nskk-candidate-window-prev ()
  "Move to previous candidate on current page."
  (when (> nskk--candidate-index 0)
    (cl-decf nskk--candidate-index)
    t))

(provide 'nskk-candidate-window)

;;; nskk-candidate-window.el ends here
