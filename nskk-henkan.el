;;; nskk-henkan.el --- Conversion pipeline for NSKK -*- lexical-binding: t; -*-

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

;; Conversion (henkan) pipeline for NSKK (Layer 3: Application).
;;
;; Layer position: L3 (Application) -- depends on nskk-kana, nskk-state,
;;   nskk-search, nskk-dictionary, nskk-prolog, nskk-converter, nskk-custom.
;;
;; This module orchestrates the full Japanese input conversion (henkan) flow:
;; preedit management, dictionary search dispatch, candidate navigation,
;; okurigana processing, dictionary registration, and conversion state cleanup.
;;
;; Architecture:
;;   Buffer operations (insert, delete, overlays, markers) are handled by
;;   imperative Emacs Lisp functions.  Decision logic (what action to take
;;   given current state) is encoded as Prolog facts and rules, queried at
;;   runtime via `nskk-henkan-dispatch'.
;;
;; Prolog predicates defined in this module:
;;   core-search-type/2          -- maps search type keyword to search function
;;   converting-phase/1          -- enumerates valid converting phases
;;   okurigana-char/2            -- maps uppercase ASCII to its lowercase
;;   okurigana-trigger/1         -- true if char is an okurigana trigger
;;   candidate-nav-next-action/3 -- (count threshold action) next-key dispatch
;;   candidate-nav-prev-action/2 -- (list-state action) prev-key dispatch
;;   search-result-action/2      -- (has/no candidates) post-search dispatch
;;   convert-or-commit-action/2  -- (converting state) SPC-without-preedit dispatch
;;   max-registration-depth/1    -- maximum recursive registration nesting
;;   registration-allowed/1      -- true if nesting depth is within limit
;;   should-update-overlay/1     -- phases that require overlay display
;;
;; Key public functions:
;;   `nskk-convert'              -- start conversion when preedit exists
;;   `nskk-convert-or-commit'    -- start conversion or commit active candidate
;;   `nskk-next-candidate'       -- advance candidate selection
;;   `nskk-previous-candidate'   -- reverse candidate selection
;;   `nskk-commit-current'       -- insert selected candidate and clear state
;;   `nskk-cancel-conversion'    -- rollback active conversion
;;   `nskk-cancel-preedit'       -- cancel preedit input
;;   `nskk-core-search'          -- dictionary search with type dispatch
;;   `nskk-detect-okurigana-char' -- uppercase consonant detection
;;   `nskk-process-okurigana-input' -- okurigana boundary handling
;;
;; Hook points:
;;   `nskk-henkan-show-candidates-functions' -- called to display candidate list
;;   `nskk-henkan-hide-candidates-functions' -- called to hide candidate list
;;   `nskk-start-henkan-hook'    -- run before conversion starts
;;   `nskk-post-henkan-hook'     -- run after conversion completes
;;   `nskk-after-henkan-hook'    -- run after conversion is committed

;;; Code:

(require 'nskk-kana)
(require 'nskk-state)
(require 'nskk-search)
(require 'nskk-dictionary)
(require 'nskk-prolog)
(require 'nskk-converter)
(require 'nskk-custom)

(declare-function nskk-state-p "nskk-state")
(declare-function nskk-state-candidates "nskk-state")
(declare-function nskk-state-current-index "nskk-state")
(declare-function nskk-state-set-candidates "nskk-state")
(declare-function nskk-state-set-okurigana "nskk-state")
(declare-function nskk-state-get-okurigana "nskk-state")
(declare-function nskk-state-set-henkan-phase "nskk-state")
(declare-function nskk-dict-register-word "nskk-dictionary")
(declare-function nskk-dict-lookup "nskk-dictionary")
(declare-function nskk-search-prefix "nskk-search")
(declare-function nskk-search-partial "nskk-search")
(declare-function nskk-state-henkan-phase "nskk-state")
(declare-function nskk-converter-convert "nskk-converter")

;;;; Buffer Modification Guard

(defmacro nskk-without-modification (&rest body)
  "Execute BODY without triggering modification hooks or undo recording."
  (declare (indent 0) (debug t))
  `(let ((inhibit-modification-hooks t)
         (buffer-undo-list t))
     ,@body))

(defmacro nskk-henkan-dispatch (action-sym query &rest clauses)
  "Evaluate QUERY, bind result to ACTION-SYM, then dispatch via pcase.
QUERY should be a `nskk-prolog-query-value' call that returns an action symbol.
CLAUSES are (action-symbol body...) pairs passed directly to `pcase'."
  (declare (indent 2) (debug t))
  `(let ((,action-sym ,query))
     (pcase ,action-sym
       ,@(mapcar (lambda (c) `(',(car c) ,@(cdr c))) clauses))))

(defmacro nskk-henkan-with-preedit (start-var &rest body)
  "Execute BODY with START-VAR bound to conversion start when preedit exists.
Preedit exists when the conversion start marker is set and point is past
the marker position plus the \u25bd marker length.  Does nothing if no preedit."
  (declare (indent 1) (debug t))
  (let ((start-sym (make-symbol "--nskk-preedit-start--")))
    `(let ((,start-sym (nskk--get-conversion-start)))
       (when (and ,start-sym
                  (> (point) (+ ,start-sym (length nskk-henkan-on-marker))))
         (let ((,start-var ,start-sym))
           ,@body)))))

(defmacro nskk-with-conversion-context (vars &rest body)
  "Execute BODY when actively converting, with VARS bound to state data.
VARS must be a two-element list: (CANDIDATES-VAR INDEX-VAR).
Guards on `nskk-converting-p' and valid `nskk-current-state'.
CANDIDATES-VAR is bound to the current candidate list.
INDEX-VAR is bound to the current candidate index."
  (declare (indent 1) (debug t))
  `(when (nskk-converting-p)
     (nskk-with-current-state
       (let* ((,(car vars) (nskk-state-candidates nskk-current-state))
              (,(cadr vars) (nskk-state-current-index nskk-current-state)))
         ,@body))))

(defvar nskk-start-henkan-hook nil
  "Hook run before conversion starts.
DDSKK equivalent to `skk-start-henkan-hook'.")
(put 'nskk-start-henkan-hook 'permanent-local t)

(defvar nskk-henkan-hook nil
  "Hook run during conversion.
DDSKK equivalent to `skk-henkan-hook'.")

(defvar nskk-post-henkan-hook nil
  "Hook run after conversion completes.
DDSKK equivalent to `skk-post-henkan-hook'.")

(defvar nskk-after-henkan-hook nil
  "Hook run after conversion is committed.
DDSKK equivalent to `skk-after-henkan-hook'.")

(defvar nskk-henkan-select-hook nil
  "Hook run when a candidate is selected.
DDSKK equivalent to `skk-henkan-select-hook'.")

(defvar nskk--romaji-buffer)            ;; defined in nskk-state.el
(defvar nskk--system-dict-index)       ;; defined in nskk-dictionary.el
(defvar nskk--henkan-count)            ;; defined in nskk-state.el
(defvar nskk--registration-depth)      ;; defined in nskk-state.el
(defvar nskk--conversion-overlay)      ;; defined in nskk-state.el
(defvar nskk--conversion-start-marker) ;; defined in nskk-state.el

;;;; Candidate Display Hooks

(defvar nskk-henkan-show-candidates-functions nil
  "Abnormal hook called to display candidates.
Called with (candidates current-index).")

(defvar nskk-henkan-hide-candidates-functions nil
  "Abnormal hook called to hide candidate display.")

(defvar nskk-henkan--candidate-list-active nil
  "Non-nil when candidate list display is active.
Set by candidate window implementation via hooks.")

(defvar nskk-henkan-select-candidate-by-key-function nil
  "Function to select a candidate by key press.
Called with (key candidates current-index).
Returns the selected candidate index, or nil if KEY is not valid.")

;;;; Dictionary Search API

(defun nskk-core-search (key &optional type limit)
  "Search dictionary for KEY.
TYPE is search type: :exact (default), :prefix, or :regex.
LIMIT is maximum results (default: 100)."
  (when (stringp key)
    (let* ((search-type (or type :exact))
           (action (nskk-prolog-query-value
                    `(core-search-type ,search-type ,'\?a) '\?a)))
      (pcase action
        ('dict-lookup (nskk-dict-lookup key))
        ('prefix-search
         (when nskk--system-dict-index
           (nskk-search-prefix nskk--system-dict-index key nil limit)))
        ('partial-search
         (when nskk--system-dict-index
           (nskk-search-partial nskk--system-dict-index key nil limit)))
        (_ (error "Unknown search type: %s" search-type))))))

;;;; Henkan Marker Constants

(defconst nskk-henkan-on-marker "\u25bd"
  "Marker character for henkan-on state (reading input).")

(defconst nskk-henkan-active-marker "\u25bc"
  "Marker character for henkan-active state (conversion active).")

(defconst nskk-okurigana-marker "*"
  "Marker character for okurigana boundary.")

(defconst nskk-henkan-on-marker-regexp (regexp-quote nskk-henkan-on-marker)
  "Pre-computed regexp for henkan-on marker.")

(defconst nskk-henkan-active-marker-regexp (regexp-quote nskk-henkan-active-marker)
  "Pre-computed regexp for henkan-active marker.")

(defconst nskk-okurigana-marker-regexp (regexp-quote nskk-okurigana-marker)
  "Pre-computed regexp for okurigana boundary marker.")

;;;; Marker Operation Helpers

(defun nskk--insert-marker (marker)
  "Insert MARKER string without triggering modification hooks or undo."
  (nskk-without-modification
    (insert marker)))

(defun nskk--delete-marker-at (pos marker-regexp)
  "Delete marker matching MARKER-REGEXP at POS without triggering hooks."
  (save-excursion
    (goto-char pos)
    (when (looking-at marker-regexp)
      (nskk-without-modification
        (delete-char (length (match-string 0)))))))

(defun nskk--replace-marker-at (pos old-regexp new-marker)
  "Replace marker matching OLD-REGEXP at POS with NEW-MARKER."
  (save-excursion
    (goto-char pos)
    (when (looking-at old-regexp)
      (nskk-without-modification
        (delete-char (length (match-string 0)))
        (insert new-marker)))))

;;;; Overlay Management

(defun nskk--update-overlay (start end text)
  "Update overlay to show TEXT from START to END."
  (unless (overlayp nskk--conversion-overlay)
    (setq nskk--conversion-overlay (make-overlay start end)))
  (move-overlay nskk--conversion-overlay start end (current-buffer))
  (overlay-put nskk--conversion-overlay 'display text)
  (overlay-put nskk--conversion-overlay 'face 'highlight))

;;;; Conversion State Helpers

(defun nskk-converting-p ()
  "Check if currently in conversion state."
  (and (boundp 'nskk-current-state)
       nskk-current-state
       (nskk-state-p nskk-current-state)
       (let ((phase (nskk-state-henkan-phase nskk-current-state)))
         (nskk-prolog-query-one `(converting-phase ,phase)))))

(defsubst nskk--has-preedit ()
  "Check if there is preedit text to convert.
Returns non-nil when the conversion start marker is set and point is
past the marker position plus the \u25bd marker length."
  (let ((start (nskk--get-conversion-start)))
    (and start (> (point) (+ start (length nskk-henkan-on-marker))))))

(defun nskk--get-conversion-start ()
  "Get conversion start position from the dedicated marker.
Returns the marker position as an integer, or nil if no marker is set."
  (when (and (markerp nskk--conversion-start-marker)
             (marker-position nskk--conversion-start-marker))
    (marker-position nskk--conversion-start-marker)))

(defun nskk--set-conversion-start-marker (pos)
  "Set the conversion start marker to POS in the current buffer.
Creates a new marker if one does not already exist."
  (unless (markerp nskk--conversion-start-marker)
    (setq nskk--conversion-start-marker (make-marker)))
  (set-marker nskk--conversion-start-marker pos (current-buffer)))

(defun nskk--clear-conversion-start-marker ()
  "Clear the conversion start marker, releasing the position."
  (when (markerp nskk--conversion-start-marker)
    (set-marker nskk--conversion-start-marker nil)))

(defun nskk--conversion-start-active-p ()
  "Return non-nil if the conversion start marker is currently active."
  (and (markerp nskk--conversion-start-marker)
       (marker-position nskk--conversion-start-marker)))

(defun nskk--restore-preedit ()
  "Restore preedit text after cancel.
Clears the conversion overlay, start marker, and romaji buffer."
  (when (overlayp nskk--conversion-overlay)
    (delete-overlay nskk--conversion-overlay))
  (nskk--clear-conversion-start-marker)
  (setq nskk--romaji-buffer ""))

;;;; Clear Conversion Context

(defun nskk--clear-conversion-context ()
  "Clear conversion context when switching modes."
  (when (and (boundp 'nskk--conversion-overlay)
             (overlayp nskk--conversion-overlay))
    (delete-overlay nskk--conversion-overlay))
  (when (and (boundp 'nskk--conversion-start-marker)
             (markerp nskk--conversion-start-marker))
    (set-marker nskk--conversion-start-marker nil))
  (when (boundp 'nskk--romaji-buffer)
    (setq nskk--romaji-buffer ""))
  (nskk-with-current-state
    (setf (nskk-state-candidates nskk-current-state) nil)
    (setf (nskk-state-current-index nskk-current-state) 0)
    (nskk-state-set-okurigana nskk-current-state nil)
    (nskk-state-set-henkan-phase nskk-current-state nil)))

;;;; Kakutei (Commit Preedit As-Is)

(defun nskk-henkan-kakutei ()
  "Commit preedit text as-is without conversion.
Removes the henkan-on marker, clears the conversion start marker,
resets the romaji buffer, and clears the henkan phase."
  (let ((start (nskk--get-conversion-start)))
    (when start
      (nskk--delete-marker-at start nskk-henkan-on-marker-regexp)))
  (nskk--clear-conversion-start-marker)
  (setq nskk--romaji-buffer "")
  (nskk-with-current-state
    (nskk-state-set-henkan-phase nskk-current-state nil)))

;;;; Conversion Control

(defun nskk-convert ()
  "Start conversion when preedit text exists.
Uses `nskk-henkan-with-preedit' to guard on preedit presence."
  (interactive)
  (nskk-henkan-with-preedit _start
    (nskk-start-conversion)))

(defun nskk-convert-or-commit ()
  "Start conversion or commit current candidate.
When actively converting (\u25bc phase), commits the current candidate.
Otherwise, starts conversion if preedit exists.
Uses Prolog `convert-or-commit-action/2' for dispatch."
  (interactive)
  (let ((conv-state (if (nskk-converting-p) 'converting 'not-converting)))
    (nskk-henkan-dispatch action
        (nskk-prolog-query-value
         `(convert-or-commit-action ,conv-state ,'\?a) '\?a)
      (commit-current (nskk-commit-current))
      (start-conversion (nskk-convert)))))

(defun nskk-cancel-conversion ()
  "Cancel conversion and return to input state."
  (interactive)
  (when (nskk-converting-p)
    (nskk-rollback-conversion)))

(defun nskk-cancel-preedit ()
  "Cancel preedit input and remove the \u25bd marker.
Deletes preedit text between the conversion start marker and point,
including the \u25bd marker character, and resets state."
  (interactive)
  (let ((start (nskk--get-conversion-start)))
    (when start
      ;; Delete everything from marker position (including ▽) to point
      (delete-region start (point))
      (goto-char start)))
  ;; Clear all preedit state
  (nskk--clear-conversion-start-marker)
  (setq nskk--romaji-buffer "")
  (setq nskk--henkan-count 0)
  (nskk-with-current-state
    (nskk-state-set-henkan-phase nskk-current-state nil)))

(defun nskk-rollback-conversion ()
  "Rollback to pre-conversion state.
Deletes the \u25bd or \u25bc marker and restores preedit text."
  (interactive)
  (when (nskk-converting-p)
    ;; Remove marker character from buffer
    (let ((start (nskk--get-conversion-start)))
      (when start
        (or (nskk--delete-marker-at start nskk-henkan-active-marker-regexp)
            (nskk--delete-marker-at start nskk-henkan-on-marker-regexp))))
    (nskk--restore-preedit)
    (setq nskk--henkan-count 0)
    (run-hook-with-args 'nskk-henkan-hide-candidates-functions)
    (setq nskk-henkan--candidate-list-active nil)
    ;; Reset henkan phase
    (nskk-with-current-state
      (nskk-state-set-henkan-phase nskk-current-state nil))))

;;;; Candidate Navigation

(defun nskk-next-candidate ()
  "Select next conversion candidate.
For the first N-1 candidates (N = `nskk-henkan-show-candidates-nth'),
show candidates inline one-by-one with the \u25bc overlay.  On the Nth press,
switch to echo area candidate list display.
Uses Prolog `candidate-nav-next-action/3' to dispatch the navigation mode."
  (interactive)
  (when (nskk-converting-p)
    (cl-incf nskk--henkan-count)
    (nskk-henkan-dispatch action
        (nskk-prolog-query-value
         `(candidate-nav-next-action ,nskk--henkan-count
                                     ,nskk-henkan-show-candidates-nth ,'\?a)
         '\?a)
      (select-next    (nskk--select-candidate 'next))
      (show-list-next (nskk--show-candidate-list-next)))))

(defun nskk-previous-candidate ()
  "Select previous conversion candidate.
In candidate list display mode, shows the previous page.
In inline mode, decrements the counter and shows the previous candidate.
Uses Prolog `candidate-nav-prev-action/2' to dispatch the navigation mode."
  (interactive)
  (when (nskk-converting-p)
    (let ((list-state (if nskk-henkan--candidate-list-active
                          'list-active 'not-active)))
      (nskk-henkan-dispatch action
          (nskk-prolog-query-value
           `(candidate-nav-prev-action ,list-state ,'\?a) '\?a)
        (show-list-prev (nskk--show-candidate-list-prev))
        (select-prev
         (when (> nskk--henkan-count 0)
           (cl-decf nskk--henkan-count))
         (nskk--select-candidate 'previous))))))

(defun nskk-commit-current ()
  "Commit current conversion candidate.
Replaces preedit text (including \u25bc marker) with the selected candidate,
then clears all conversion state: overlay, start marker, candidates,
henkan phase, and romaji buffer.
Uses `nskk-with-conversion-context' to guard on active conversion state."
  (interactive)
  (nskk-with-conversion-context (candidates index)
    (let* ((candidate (nth index candidates))
           (start (nskk--get-conversion-start))
           (end (point)))
      ;; Delete overlay and insert actual text
      (when (overlayp nskk--conversion-overlay)
        (delete-overlay nskk--conversion-overlay))
      (when (and start candidate)
        ;; Delete region including the ▼ marker
        (delete-region start end)
        (goto-char start)
        (insert candidate))
      ;; Clear all conversion state
      (nskk--clear-conversion-start-marker)
      (setq nskk--romaji-buffer "")
      (setf (nskk-state-candidates nskk-current-state) nil)
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-set-henkan-phase nskk-current-state nil)
      (setq nskk--henkan-count 0)
      (run-hook-with-args 'nskk-henkan-hide-candidates-functions)
      (setq nskk-henkan--candidate-list-active nil))))

(defun nskk--select-candidate (direction)
  "Select candidate in DIRECTION (next or previous)."
  (let* ((candidates (nskk-state-candidates nskk-current-state))
         (total (length candidates)))
    (when (> total 0)
      (let* ((current (nskk-state-current-index nskk-current-state))
             (new-index (if (eq direction 'next)
                            (mod (1+ current) total)
                          (mod (+ current total -1) total))))
        (setf (nskk-state-current-index nskk-current-state) new-index)
        (let* ((candidate (nth new-index candidates))
               (start (nskk--get-conversion-start))
               ;; Account for ▼ marker in start position
               (text-start (when start (+ start (length nskk-henkan-active-marker))))
               (end (point)))
          (nskk--update-overlay text-start end candidate))))))

(defun nskk--show-candidate-list-next ()
  "Show next page of candidates in echo area list.
When all candidates are exhausted, trigger dictionary registration."
  (let* ((candidates (nskk-state-candidates nskk-current-state))
         (current (nskk-state-current-index nskk-current-state))
         (per-page (min nskk-henkan-number-to-display-candidates
                        (length nskk-henkan-show-candidates-keys)))
         (next-start (if nskk-henkan--candidate-list-active
                         (+ current per-page)
                       current)))
    (if (>= next-start (length candidates))
        ;; All candidates exhausted: trigger dictionary registration
        (nskk--exhaust-candidates)
      ;; Show next page
      (setf (nskk-state-current-index nskk-current-state) next-start)
      (nskk-state-set-henkan-phase nskk-current-state 'list)
      (run-hook-with-args 'nskk-henkan-show-candidates-functions candidates next-start)
      (setq nskk-henkan--candidate-list-active t))))

(defun nskk--show-candidate-list-prev ()
  "Show previous page of candidates in echo area list."
  (let* ((candidates (nskk-state-candidates nskk-current-state))
         (current (nskk-state-current-index nskk-current-state))
         (per-page (min nskk-henkan-number-to-display-candidates
                        (length nskk-henkan-show-candidates-keys)))
         (prev-start (- current per-page)))
    (when (< prev-start 0)
      (setq prev-start 0))
    (setf (nskk-state-current-index nskk-current-state) prev-start)
    (run-hook-with-args 'nskk-henkan-show-candidates-functions candidates prev-start)
    (setq nskk-henkan--candidate-list-active t)))

;;;; Okurigana Handling

(defun nskk-detect-okurigana-char (char)
  "Check if CHAR is an okurigana marker (uppercase consonant).
Returns the lowercase consonant if it's a marker, nil otherwise."
  (when (characterp char)
    (nskk-prolog-query-value `(okurigana-char ,char ,'\?lc) '\?lc)))

(defun nskk-process-okurigana-input (char)
  "Process CHAR as potential okurigana marker.
If CHAR is uppercase and the conversion start marker is active,
store okurigana context, insert * boundary marker, and put the
consonant into the romaji buffer for deferred kana accumulation."
  (let ((okuri-char (nskk-detect-okurigana-char char)))
    (when (and okuri-char
               (nskk--conversion-start-active-p))
      (nskk-with-current-state
      ;; Flush any pending romaji before okurigana
      (let ((pending (nskk-convert-input-to-kana-final)))
        (when (and (stringp pending) (not (string-empty-p pending)))
          (insert pending)))
      ;; Insert okurigana boundary marker
      (nskk--insert-marker nskk-okurigana-marker)
      ;; Store okurigana consonant in state
      (nskk-state-set-okurigana nskk-current-state okuri-char)
      ;; Put consonant into romaji buffer
      (setq nskk--romaji-buffer (char-to-string okuri-char))
      t))))

(defun nskk-convert-input-to-kana-final ()
  "Convert remaining romaji buffer to kana and clear buffer.
Returns the converted kana string."
  (let ((result (nskk-converter-convert nskk--romaji-buffer)))
    (prog1
        (if (and result (stringp (car result)))
            (car result)
          nskk--romaji-buffer)
      (setq nskk--romaji-buffer ""))))

(defun nskk-start-conversion-with-okuri (okuri-char)
  "Start conversion with okurigana context OKURI-CHAR.
Searches dictionary with okuri-ari type using the conversion start
marker for the preedit region.  Handles \u25bd marker in preedit text."
  (let* ((start (nskk--get-conversion-start))
         ;; Skip ▽ marker
         (text-start (when start
                       (save-excursion
                         (goto-char start)
                         (if (looking-at nskk-henkan-on-marker-regexp)
                             (+ start (length nskk-henkan-on-marker))
                           start))))
         (end (point))
         (text (when (and text-start (> end text-start))
                 (buffer-substring-no-properties text-start end)))
         (query (when text (concat text (char-to-string okuri-char)))))
    (when query
      ;; Replace ▽ with ▼
      (nskk--replace-marker-at start nskk-henkan-on-marker-regexp nskk-henkan-active-marker)
      ;; Search with okuri-ari type
      (let* ((candidates (nskk-core-search query :exact))
             (primary (car candidates)))
        (when primary
          (nskk--update-overlay (+ start (length nskk-henkan-active-marker)) end primary)
          (nskk-with-current-state
            (nskk-state-set-candidates nskk-current-state candidates)
            (setf (nskk-state-current-index nskk-current-state) 0)
            (nskk-state-set-henkan-phase nskk-current-state 'active)))))))

(defun nskk--trigger-okuri-conversion (okuri-char preedit-end)
  "Trigger conversion with okurigana OKURI-CHAR ending at PREEDIT-END.
PREEDIT-END is the buffer position before the okurigana kana was inserted.
Removes the * boundary marker and searches the dictionary."
  (let* ((start (nskk--get-conversion-start))
         ;; Skip ▽ marker for text extraction
         (text-start (when start
                       (save-excursion
                         (goto-char start)
                         (if (looking-at nskk-henkan-on-marker-regexp)
                             (+ start (length nskk-henkan-on-marker))
                           start))))
         ;; Find and account for * marker
         (text-with-marker (when (and text-start (> preedit-end text-start))
                             (buffer-substring-no-properties text-start preedit-end)))
         (text (when text-with-marker
                 (replace-regexp-in-string nskk-okurigana-marker-regexp "" text-with-marker)))
         (query (when text (concat text (char-to-string okuri-char)))))
    (when query
      ;; Remove the * marker from the buffer
      (save-excursion
        (goto-char (or text-start start))
        (when (search-forward nskk-okurigana-marker preedit-end t)
          (delete-char (- (length nskk-okurigana-marker)))))
      ;; Replace ▽ with ▼
      (nskk--replace-marker-at start nskk-henkan-on-marker-regexp nskk-henkan-active-marker)
      (let* ((candidates (nskk-core-search query :exact))
             (primary (car candidates)))
        (when primary
          (nskk--update-overlay (+ start (length nskk-henkan-active-marker)) (point) primary)
          (nskk-with-current-state
            (nskk-state-set-candidates nskk-current-state candidates)
            (setf (nskk-state-current-index nskk-current-state) 0)
            (nskk-state-set-henkan-phase nskk-current-state 'active)))))))

;;;; Conversion Pipeline

(defun nskk-start-conversion ()
  "Start dictionary conversion for the preedit text.
Extract the preedit text between the \u25bd marker and point, then
search the dictionary using `nskk-core-search'.  Uses Prolog
`search-result-action/2' to dispatch: if candidates are found,
displays the first via the overlay and stores all in state;
if no candidates, opens the dictionary registration minibuffer.
The \u25bd marker is replaced with \u25bc when conversion begins."
  (nskk-henkan-with-preedit start
    (let* ((end (point))
           ;; Skip the ▽ marker when extracting preedit text
           (text-start (save-excursion
                         (goto-char start)
                         (if (looking-at nskk-henkan-on-marker-regexp)
                             (+ start (length nskk-henkan-on-marker))
                           start)))
           (text (when (> end text-start)
                   (buffer-substring-no-properties text-start end))))
      (when (and text (not (string-empty-p text)))
        (let* ((candidates (nskk-core-search text))
               (result-sym (if candidates 'has-candidates 'no-candidates)))
          (nskk-henkan-dispatch action
              (nskk-prolog-query-value
               `(search-result-action ,result-sym ,'\?a) '\?a)
            (show-overlay
             (setq nskk--henkan-count 1)
             ;; Replace ▽ with ▼
             (nskk--replace-marker-at start nskk-henkan-on-marker-regexp
                                      nskk-henkan-active-marker)
             ;; Display first candidate via overlay
             (nskk--update-overlay (+ start (length nskk-henkan-active-marker))
                                   (point) (car candidates))
             ;; Store candidates in state and set phase
             (nskk-with-current-state
               (nskk-state-set-candidates nskk-current-state candidates)
               (setf (nskk-state-current-index nskk-current-state) 0)
               (nskk-state-set-henkan-phase nskk-current-state 'active)))
            (start-registration
             ;; No candidates found: open dictionary registration
             (let ((registered (nskk-start-registration text)))
               (when registered
                 (delete-region start end)
                 (goto-char start)
                 (insert registered)
                 (nskk--clear-conversion-start-marker)
                 (setq nskk--romaji-buffer "")
                 (setq nskk--henkan-count 0))))))))))

;;;; Dictionary Registration

(defun nskk-start-registration (reading)
  "Start dictionary registration for READING.
Opens a minibuffer prompt for the user to enter the desired text.
READING is the headword that could not be converted.
Supports recursive registration up to `max-registration-depth' levels:
depth 1 shows [辞書登録], depth 2 shows [[辞書登録]], etc.
Returns the registered word on success, or nil if the user cancels
or if the maximum nesting depth (as defined by Prolog `registration-allowed/1')
has been reached."
  (when (nskk-prolog-query-one `(registration-allowed ,nskk--registration-depth))
    (nskk-with-current-state
      (nskk-state-set-henkan-phase nskk-current-state 'registration))
    (cl-incf nskk--registration-depth)
    (let* ((depth-brackets (make-string nskk--registration-depth ?\[))
           (depth-close (make-string nskk--registration-depth ?\]))
           (prompt (format "%s辞書登録%s %s: "
                           depth-brackets depth-close reading))
           (result nil))
      (unwind-protect
          (progn
            (setq result (read-from-minibuffer prompt))
            (when (and result (not (string-empty-p result)))
              (nskk-dict-register-word reading result)
              result))
        (cl-decf nskk--registration-depth)
        (nskk-with-current-state
          (nskk-state-set-henkan-phase nskk-current-state nil))))))

(defun nskk--exhaust-candidates ()
  "Handle exhausted candidates by triggering dictionary registration.
If registration succeeds, insert the registered word and clean up state.
If the user cancels, wrap around to the first candidate in list display."
  (run-hook-with-args 'nskk-henkan-hide-candidates-functions)
  (setq nskk-henkan--candidate-list-active nil)
  (let* ((start (nskk--get-conversion-start))
         (text-start (when start
                       (save-excursion
                         (goto-char start)
                         (if (looking-at nskk-henkan-active-marker-regexp)
                             (+ start (length nskk-henkan-active-marker))
                           start))))
         (text (when (and text-start (> (point) text-start))
                 (buffer-substring-no-properties text-start (point))))
         (registered (when text (nskk-start-registration text))))
    (if registered
        (progn
          (when (overlayp nskk--conversion-overlay)
            (delete-overlay nskk--conversion-overlay))
          (delete-region start (point))
          (goto-char start)
          (insert registered)
          (nskk--clear-conversion-start-marker)
          (setq nskk--romaji-buffer "")
          (setq nskk--henkan-count 0)
          (nskk-with-current-state
            (setf (nskk-state-candidates nskk-current-state) nil)
            (setf (nskk-state-current-index nskk-current-state) 0)
            (nskk-state-set-henkan-phase nskk-current-state nil)))
      ;; Registration cancelled: wrap to first candidate in list
      (let ((candidates (nskk-state-candidates nskk-current-state)))
        (setf (nskk-state-current-index nskk-current-state) 0)
        (setq nskk--henkan-count nskk-henkan-show-candidates-nth)
        (nskk-state-set-henkan-phase nskk-current-state 'list)
        (run-hook-with-args 'nskk-henkan-show-candidates-functions candidates 0)
        (setq nskk-henkan--candidate-list-active t)))))

(defvar nskk--henkan-initialized nil
  "Non-nil when henkan Prolog predicates have been initialized.")

(defun nskk-henkan-initialize ()
  "Initialize henkan pipeline Prolog predicates.
Idempotent: subsequent calls are no-ops."
  (unless nskk--henkan-initialized
    ;; Core search type mapping
    (nskk-prolog-set-index 'core-search-type 2 :hash)
    (nskk-prolog-<- (core-search-type :exact dict-lookup))
    (nskk-prolog-<- (core-search-type :prefix prefix-search))
    (nskk-prolog-<- (core-search-type :regex partial-search))

    ;; Converting phase facts
    (nskk-prolog-set-index 'converting-phase 1 :hash)
    (nskk-prolog-<- (converting-phase active))
    (nskk-prolog-<- (converting-phase list))
    (nskk-prolog-<- (converting-phase registration))

    ;; Okurigana character classification
    (nskk-prolog-set-index 'okurigana-char 2 :hash)
    (dolist (c (number-sequence ?A ?Z))
      (nskk-prolog-assert `((okurigana-char ,c ,(downcase c)))))

    ;; Okurigana trigger predicate
    (nskk-prolog-<- (okurigana-trigger \?c)
      (okurigana-char \?c \?_))

    ;; Candidate navigation action rules
    (nskk-prolog-set-index 'candidate-nav-next-action 3 :list)
    (nskk-prolog-<- (candidate-nav-next-action \?count \?threshold select-next)
      (< \?count \?threshold))
    (nskk-prolog-<- (candidate-nav-next-action \?count \?threshold show-list-next)
      (>= \?count \?threshold))

    (nskk-prolog-set-index 'candidate-nav-prev-action 2 :hash)
    (nskk-prolog-<- (candidate-nav-prev-action list-active  show-list-prev))
    (nskk-prolog-<- (candidate-nav-prev-action not-active   select-prev))

    ;; Search result action dispatch
    (nskk-prolog-set-index 'search-result-action 2 :hash)
    (nskk-prolog-<- (search-result-action has-candidates  show-overlay))
    (nskk-prolog-<- (search-result-action no-candidates   start-registration))

    ;; Convert-or-commit action dispatch
    (nskk-prolog-set-index 'convert-or-commit-action 2 :hash)
    (nskk-prolog-<- (convert-or-commit-action converting     commit-current))
    (nskk-prolog-<- (convert-or-commit-action not-converting start-conversion))

    ;; Registration depth guard
    (nskk-prolog-set-index 'max-registration-depth 1 :hash)
    (nskk-prolog-<- (max-registration-depth 3))
    (nskk-prolog-<- (registration-allowed \?depth)
      (max-registration-depth \?max)
      (< \?depth \?max))

    ;; Overlay update phase guard
    (nskk-prolog-set-index 'should-update-overlay 1 :hash)
    (nskk-prolog-<- (should-update-overlay active))
    (nskk-prolog-<- (should-update-overlay list))

    (setq nskk--henkan-initialized t)))

(provide 'nskk-henkan)

;;; nskk-henkan.el ends here
