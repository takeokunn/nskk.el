;;; nskk-candidate-window.el --- Candidate display UI for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
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

;; Candidate display UI for NSKK (Layer 5: Presentation).
;;
;; Layer position: L5 (Presentation) -- depends on nskk-prolog, nskk-henkan,
;;   and nskk-custom.  Wired into the henkan pipeline via hooks in nskk.el.
;;
;; Implements ddskk-compatible candidate display:
;; - First N-1 candidates shown inline one-by-one with a ▼ marker
;; - After the Nth SPC press, switches to echo area list with home-row
;;   selection keys (a s d f j k l)
;; - Shows [残り N] count for remaining candidates
;; - Supports page navigation (SPC = next page, x = prev page)
;; - Direct selection by pressing the corresponding key
;;
;; Prolog predicates maintained by this module:
;; - `candidate-selection-key'/2: (candidate-selection-key KEY POSITION)
;;   Maps each home-row selection key character to its 0-based page
;;   position.  Initialized at load time from
;;   `nskk-henkan-show-candidates-keys'.  Uses hash indexing for O(1)
;;   key dispatch during candidate selection.
;;
;; Key public API:
;; - `nskk-candidate-show-list'         Display a page of candidates in echo area
;; - `nskk-candidate-hide-list'         Clear the echo area candidate display
;; - `nskk-candidate-list-active-p'     Non-nil when candidate list is visible
;; - `nskk-candidate-list-select-by-key' Return absolute index for a key press
;;
;; Hook integration:
;; - `nskk-henkan-show-candidates-functions': called to display page
;; - `nskk-henkan-hide-candidates-functions': called to clear display
;; - `nskk-henkan-select-candidate-by-key-function': key->index lookup
;;
;; These hooks are wired in `nskk.el' during `nskk--enable'.

;;; Code:

(require 'cl-lib)
(require 'nskk-prolog)
(require 'nskk-henkan)
(require 'nskk-custom)

;;;; Prolog Candidate Key Selection Facts

(defun nskk--candidate-init-key-facts ()
  "Initialize Prolog facts for `candidate-selection-key'/2.
Source: `nskk-henkan-show-candidates-keys'.
Maps each selection key character to its 0-based page position.
Uses hash indexing for O(1) key dispatch during candidate selection."
  (nskk-prolog-set-index 'candidate-selection-key 2 :hash)
  (let ((i 0))
    (dolist (k nskk-henkan-show-candidates-keys)
      (nskk-prolog-assert `((candidate-selection-key ,k ,i)))
      (cl-incf i))))

(nskk--candidate-init-key-facts)

(defface nskk-candidate-key-face
  '((t (:foreground "#FF9800" :weight bold)))
  "Face for candidate selection key labels."
  :group 'nskk-candidate-window)

(defface nskk-candidate-face
  '((t (:inherit default)))
  "Face for candidate text."
  :group 'nskk-candidate-window)

(defvar-local nskk--candidate-list-page 0
  "Current page in candidate list display (0-indexed).")

(defvar-local nskk--candidate-list-active nil
  "Non-nil when the echo area candidate list is active.")

(defun nskk-candidate-show-list (candidates current-index)
  "Display CANDIDATES in echo area starting at CURRENT-INDEX.
Shows candidates with home-row selection keys and a [残り N] remaining
count when more candidates exist beyond the current page.
Returns the page candidates (a sublist of CANDIDATES) for key mapping."
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
  "Return the absolute index of the candidate selected by KEY.
CANDIDATES is the full candidate list; CURRENT-INDEX is the page start
offset.  KEY is the character pressed.  Returns the selected index
relative to the full CANDIDATES list, or nil if KEY is not a valid
selection key or if the resulting index is out of range."
  (when-let* ((pos (nskk-prolog-query-value
                    `(candidate-selection-key ,key ,'\?pos) '\?pos)))
    (let ((absolute-index (+ current-index pos)))
      (when (< absolute-index (length candidates))
        absolute-index))))

(provide 'nskk-candidate-window)

;;; nskk-candidate-window.el ends here
