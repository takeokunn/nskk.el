;;; nskk-candidate-window.el --- Candidate display UI for NSKK -*- lexical-binding: t; -*-

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

;; Candidate display UI for NSKK (Layer 5: Presentation).
;;
;; Layer position: L5 (Presentation) -- depends on nskk-prolog, nskk-custom,
;;   and nskk-cps-macros (for defun/done).
;;   Wired into the henkan pipeline via hooks in nskk.el.
;;
;; Candidate display:
;; - First N-1 candidates shown inline one-by-one with a ▼ marker
;; - After the Nth SPC press, switches to overlay list display below
;;   the conversion region with home-row selection keys (a s d f j k l)
;; - Shows [残り N] count for remaining candidates
;; - Supports page navigation (SPC = next page, x = prev page)
;; - Direct selection by pressing the corresponding key
;;
;; Display mechanism:
;; - Uses an overlay `after-string' on a zero-length overlay anchored at the
;;   end of `nskk--conversion-overlay'.  The after-string begins with \n so
;;   the candidate list appears on the line below the preedit text.
;; - Works on both terminal (TUI) and graphical (GUI) displays.
;; - Zero external dependencies; uses Emacs built-in overlay API.
;; - The overlay variable `nskk--candidate-overlay' is declared in
;;   nskk-state.el (project convention: all overlay vars live there).
;;
;; Prolog predicates maintained by this module:
;; - `candidate-selection-key'/2: (candidate-selection-key KEY POSITION)
;;   Maps each home-row selection key character to its 0-based page
;;   position.  Initialized at load time from
;;   `nskk-henkan-show-candidates-keys'.  Uses hash indexing for O(1)
;;   key dispatch during candidate selection.
;;
;; Key public API (direct-style sync variants):
;; - `nskk-candidate-show-list'            Display a page of candidates via overlay
;; - `nskk-candidate-hide-list'            Delete the candidate overlay
;; - `nskk-candidate-list-active-p'        Non-nil when candidate overlay is visible
;; - `nskk-candidate-list-select-by-key'   Return absolute index for a key press
;;
;; CPS variants (for use in continuation-passing chains):
;; - `nskk-candidate-show-list/k'          CPS wrapper; calls on-done with page candidates
;; - `nskk-candidate-hide-list/k'          CPS wrapper; calls on-done after hiding
;; - `nskk-candidate-list-active-p/k'      CPS wrapper; calls on-done with active state
;; - `nskk-candidate-list-select-by-key/k' CPS wrapper; on-found/on-not-found dispatch
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
(require 'nskk-custom)
(require 'nskk-cps-macros)
(eval-and-compile (require 'nskk-state))

(defvar nskk--conversion-overlay)  ;; defined in nskk-state.el
(defvar nskk--candidate-overlay)   ;; defined in nskk-state.el

;;;; Prolog Candidate Key Selection Facts

(defvar nskk--candidate-key-facts-initialized nil
  "Non-nil when `candidate-selection-key'/2 Prolog facts have been asserted.

Guards against duplicate assertions on file reload (e.g. `eval-buffer').")

(defun nskk--candidate-init-key-facts ()
  "Initialize Prolog facts for `candidate-selection-key'/2.

Source: `nskk-henkan-show-candidates-keys'.
Maps each selection key character to its 0-based page position.
Uses hash indexing for O(1) key dispatch during candidate selection.
Idempotent: safe to call multiple times."
  (unless nskk--candidate-key-facts-initialized
    (nskk-prolog-retract-all 'candidate-selection-key 2)
    (nskk-prolog-set-index 'candidate-selection-key 2 :hash)
    (cl-loop for k in nskk-henkan-show-candidates-keys
             for i from 0
             do (nskk-prolog-assert `((candidate-selection-key ,k ,i))))
    (setq nskk--candidate-key-facts-initialized t)))

(nskk--candidate-init-key-facts)

(defface nskk-candidate-key-face
  '((t (:inherit font-lock-warning-face :weight bold)))
  "Face for candidate selection key labels."
  :group 'nskk-candidate-window)

(defface nskk-candidate-face
  '((t (:inherit default)))
  "Face for candidate text."
  :group 'nskk-candidate-window)

(defvar-local nskk--candidate-list-active nil
  "Non-nil when the candidate list overlay is currently displayed.")

;;;; Overlay Display Helpers

(defun nskk--candidate-build-string (page-candidates keys remaining)
  "Build the overlay after-string for PAGE-CANDIDATES.

KEYS is the list of selection key characters.
REMAINING is the count of candidates beyond the current page.
Returns a string starting with \\n to appear below the preedit line."
  (let* ((entries (cl-loop for cand in page-candidates
                           for key  in keys
                           collect (concat
                                    (propertize (format "%c:" key)
                                                'face 'nskk-candidate-key-face)
                                    (propertize cand
                                                'face 'nskk-candidate-face))))
         (body   (string-join entries " "))
         (suffix (when (> remaining 0) (format " [残り %d]" remaining))))
    (concat "\n" body suffix)))

(defun nskk--candidate-overlay-anchor ()
  "Return the buffer position to anchor the candidate overlay.
Uses the end of `nskk--conversion-overlay' when available,
falling back to point when the conversion overlay is absent or deleted."
  (or (and (overlayp nskk--conversion-overlay)
           (overlay-end nskk--conversion-overlay))
      (point)))

(defun nskk--candidate-page-slice (candidates start-index per-page)
  "Return a plist describing one page of CANDIDATES.

START-INDEX is the 0-based index of the first candidate on this page.
PER-PAGE is the maximum number of candidates per page.
Returns a plist with:
  :slice     — the sublist of CANDIDATES for this page
  :remaining — count of candidates beyond this page"
  (let* ((page-end (min (+ start-index per-page) (length candidates)))
         (slice (cl-subseq candidates start-index page-end))
         (remaining (- (length candidates) page-end)))
    (list :slice slice :remaining remaining)))

;;;; Public API

(defun/k nskk-candidate-show-list (candidates current-index)
  "Display CANDIDATES via overlay starting at CURRENT-INDEX.

Shows candidates with home-row selection keys and a [残り N] remaining
count when more candidates exist beyond the current page.
Returns the page candidates (a sublist of CANDIDATES) for key mapping.
CURRENT-INDEX must be aligned to a page boundary (a multiple of PER-PAGE),
as computed by the henkan pipeline."
  (let* ((keys nskk-henkan-show-candidates-keys)
         (per-page (min nskk-henkan-number-to-display-candidates (length keys)))
         (page (nskk--candidate-page-slice candidates current-index per-page))
         (page-candidates (plist-get page :slice))
         (remaining (plist-get page :remaining))
         (after-str (nskk--candidate-build-string page-candidates keys remaining))
         (anchor (nskk--candidate-overlay-anchor)))
    (nskk-ensure-overlay nskk--candidate-overlay anchor anchor 'after-string after-str)
    (setq nskk--candidate-list-active t)
    (succeed page-candidates)))

(defun/k nskk-candidate-list-active-p ()
  "Return non-nil if the candidate list overlay is currently displayed."
  (succeed nskk--candidate-list-active))

(defun/done nskk-candidate-hide-list ()
  "Hide the candidate list by deleting its overlay."
  (when nskk--candidate-list-active
    (nskk-delete-overlay nskk--candidate-overlay)
    (setq nskk--candidate-list-active nil)))

(defun/k nskk-candidate-list-select-by-key (key candidates current-index)
  "Return the absolute index of the candidate selected by KEY.

CANDIDATES is the full candidate list; CURRENT-INDEX is the page start
offset.  KEY is the character pressed.  Returns the selected index
relative to the full CANDIDATES list, or nil if KEY is not a valid
selection key or if the resulting index is out of range."
  (let ((pos (nskk-prolog-query-value
              `(candidate-selection-key ,key ,'\?pos) '\?pos)))
    (if pos
        (let ((absolute-index (+ current-index pos)))
          (if (< absolute-index (length candidates))
              (succeed absolute-index)
            (fail)))
      (fail))))

(provide 'nskk-candidate-window)

;;; nskk-candidate-window.el ends here
