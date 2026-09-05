;;; nskk-candidate-window.el --- Candidate display UI for NSKK -*- lexical-binding: t; -*-

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

;; Candidate display UI for NSKK.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nskk-prolog)
(require 'nskk-cps-macros)
(eval-and-compile (require 'nskk-state))
;; `nskk--candidate-init-key-facts' is called at top level below and reads
;; `nskk-henkan-show-candidates-keys' during load, so the defining module must
;; be loaded here -- a forward `defvar' would leave the symbol void.
(require 'nskk-henkan)

;;;; Customization

(defgroup nskk-candidate-window nil
  "Candidate display UI for NSKK."
  :prefix "nskk-candidate-"
  :group 'nskk-ui)

;;;; Prolog Candidate Key Selection Facts

(defvar nskk--candidate-key-facts-initialized nil
  "Non-nil when `candidate-selection-key'/2 Prolog facts have been asserted.

Guards against duplicate assertions on file reload (e.g. `eval-buffer').")

(nskk-prolog-<- (module-initialized-flag nskk--candidate-key-facts-initialized))

(defun nskk--candidate-init-key-facts ()
  "Initialize Prolog facts for `candidate-selection-key'/2.

Source: `nskk-henkan-show-candidates-keys'.
Also registers uppercase variants for DDSKK compatibility.
Idempotent: safe to call multiple times."
  (unless nskk--candidate-key-facts-initialized
    (nskk-prolog-retract-all 'candidate-selection-key 2)
    (nskk-prolog-set-index 'candidate-selection-key 2 :hash)
    (cl-loop for k in nskk-henkan-show-candidates-keys
             for i from 0
             do (nskk-prolog-assert `((candidate-selection-key ,k ,i)))
                (let ((upper (upcase k)))
                  (unless (= upper k)
                    (nskk-prolog-assert `((candidate-selection-key ,upper ,i))))))
    (setq nskk--candidate-key-facts-initialized t)))

(nskk--candidate-init-key-facts)

(defface nskk-candidate-key-face
  '((t (:inherit font-lock-warning-face :weight bold)))
  "Face for candidate selection key labels."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-candidate-window)

(defface nskk-candidate-face
  '((t (:inherit default)))
  "Face for candidate text."
  :package-version '(nskk . "0.1.0")
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
                                    (nskk-display-sanitize cand 'nskk-candidate-face))))
         (body   (string-join entries " "))
         (suffix (when (> remaining 0) (format " [残り %d]" remaining))))
    (concat "\n" body suffix)))

(defun nskk--candidate-anchor-position ()
  "Return the buffer position to anchor the candidate overlay.
Uses the end of the overlay returned by `nskk-state-conversion-overlay'
when available, falling back to point when the conversion overlay is
absent or deleted."
  (let ((overlay (nskk-state-conversion-overlay)))
    (or (and (overlayp overlay) (overlay-end overlay))
        (point))))

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

;;;; Overlay State Transitions

(defun nskk--candidate-clear-list-state ()
  "Clear candidate overlay state even when the active flag drifted."
  (setq nskk--candidate-list-active nil)
  ;; Commit the cleared state before deleting, mirroring `nskk-delete-overlay's
  ;; setq-before-delete ordering so a fault-injected `delete-overlay' cannot
  ;; leave state pointing at an overlay that is mid-deletion.
  (let ((ov (nskk-state-candidate-overlay)))
    (nskk-state-set-candidate-overlay nil)
    (when (overlayp ov)
      (delete-overlay ov))))

(defun nskk--candidate-commit-overlay (anchor after-str)
  "Move or create the candidate overlay at ANCHOR, setting AFTER-STR.
Reuses the live overlay if one exists, else creates one and commits it
to state.  On error or quit, rolls back via
`nskk--candidate-clear-list-state' and re-signals the original condition."
  (condition-case condition
      (let ((ov (nskk-state-candidate-overlay)))
        ;; Commit a freshly-created overlay to state immediately, before
        ;; `overlay-put' below, so a fault mid-`overlay-put' still finds
        ;; the overlay in state and `nskk--candidate-clear-list-state' can
        ;; roll it back instead of leaking it.
        (if (overlayp ov)
            (move-overlay ov anchor anchor (current-buffer))
          (setq ov (make-overlay anchor anchor))
          (nskk-state-set-candidate-overlay ov))
        (overlay-put ov 'after-string after-str)
        (setq nskk--candidate-list-active t))
    ((error quit)
     (condition-case nil
         (nskk--candidate-clear-list-state)
       ((error quit) nil))
     (signal (car condition) (cdr condition)))))

;;;; Public API

(defun/k nskk-candidate-show-list (candidates current-index)
  "Display CANDIDATES via overlay starting at CURRENT-INDEX.

Shows candidates with home-row selection keys and a [残り N] remaining
count when more candidates exist beyond the current page.
Returns the page candidates (a sublist of CANDIDATES) for key mapping.
CURRENT-INDEX is the 0-based index of the first candidate to show.  The
henkan pipeline passes page-aligned values within range.  An index equal
to the length of CANDIDATES yields an empty page; an index past the end
signals an error."
  (let* ((keys nskk-henkan-show-candidates-keys)
         (per-page (min nskk-henkan-number-to-display-candidates (length keys)))
         (page (nskk--candidate-page-slice candidates current-index per-page))
         (page-candidates (plist-get page :slice)))
    (nskk--candidate-commit-overlay
     (nskk--candidate-anchor-position)
     (nskk--candidate-build-string page-candidates keys (plist-get page :remaining)))
    (succeed page-candidates)))

(defun/k nskk-candidate-list-active-p ()
  "Return non-nil if the candidate list overlay is currently displayed."
  (succeed nskk--candidate-list-active))

(defun/done nskk-candidate-hide-list ()
  "Hide the candidate list and repair any overlay state drift."
  (nskk--candidate-clear-list-state))

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
