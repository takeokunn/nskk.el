;;; nskk-candidate-window.el --- Candidate display UI for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda

;; Author: NSKK Contributors
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
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

;; Candidate display UI for NSKK (Layer 1: UI Layer).
;;
;; Implements ddskk-compatible candidate display:
;; - First N-1 candidates shown inline one-by-one with ▼ marker
;; - After Nth SPC press, switches to echo area list with home-row
;;   selection keys (a s d f j k l)
;; - Shows [残り N] count for remaining candidates
;; - Supports page navigation (SPC = next page, x = prev page)
;; - Direct selection by pressing the corresponding key

;;; Code:

(require 'nskk-custom)

(defgroup nskk-candidate-window nil
  "Candidate display UI for NSKK."
  :prefix "nskk-candidate-"
  :group 'nskk-ui)

(defface nskk-candidate-key-face
  '((t (:foreground "#FF9800" :weight bold)))
  "Face for candidate selection key labels."
  :group 'nskk-candidate-window)

(defface nskk-candidate-face
  '((t (:inherit default)))
  "Face for candidate text."
  :group 'nskk-candidate-window)

(defface nskk-candidate-annotation-face
  '((t (:inherit shadow :height 0.85)))
  "Face for candidate annotations."
  :group 'nskk-candidate-window)

(defvar-local nskk--candidate-list-page 0
  "Current page in candidate list display (0-indexed).")

(defvar-local nskk--candidate-list-active nil
  "Non-nil when the echo area candidate list is active.")

(defun nskk-candidate-show-list (candidates current-index)
  "Show CANDIDATES in echo area starting from CURRENT-INDEX.
Displays candidates with home-row selection keys.
Returns the list of displayed candidates for selection mapping."
  (let* ((keys nskk-henkan-show-candidates-keys)
         (per-page (min nskk-henkan-number-to-display-candidates
                        (length keys)))
         (page-start current-index)
         (page-end (min (+ page-start per-page) (length candidates)))
         (page-candidates (cl-subseq candidates page-start page-end))
         (remaining (- (length candidates) page-end))
         (parts nil))
    ;; Build the display string
    (cl-loop for cand in page-candidates
             for key in keys
             do (push (concat
                       (propertize (format "%c:" key) 'face 'nskk-candidate-key-face)
                       (propertize cand 'face 'nskk-candidate-face))
                      parts))
    (let ((display-str (string-join (nreverse parts) " ")))
      ;; Add remaining count
      (when (> remaining 0)
        (setq display-str
              (concat display-str
                      (format " [残り %d]" remaining))))
      ;; Show in echo area (suppress *Messages* logging)
      (let ((message-log-max nil))
        (message "%s" display-str)))
    ;; Store state
    (setq nskk--candidate-list-active t)
    (setq nskk--candidate-list-page (/ current-index per-page))
    ;; Return the page candidates for key selection
    page-candidates))

(defun nskk-candidate-list-active-p ()
  "Return non-nil if the echo area candidate list is currently displayed."
  nskk--candidate-list-active)

(defun nskk-candidate-hide-list ()
  "Hide the echo area candidate list."
  (when nskk--candidate-list-active
    (setq nskk--candidate-list-active nil)
    (setq nskk--candidate-list-page 0)
    (message nil)))

(defun nskk-candidate-list-select-by-key (key candidates current-index)
  "Select candidate by KEY press from CANDIDATES at CURRENT-INDEX.
KEY is the character pressed.  Returns the selected candidate index
relative to the full candidates list, or nil if KEY is not a valid
selection key."
  (let* ((keys nskk-henkan-show-candidates-keys)
         (key-pos (cl-position key keys)))
    (when key-pos
      (let ((absolute-index (+ current-index key-pos)))
        (when (< absolute-index (length candidates))
          absolute-index)))))

(provide 'nskk-candidate-window)

;;; nskk-candidate-window.el ends here
