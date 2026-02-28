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
;; Conversion (henkan) pipeline for NSKK.
;; Manages conversion start/commit/cancel, candidate navigation,
;; okurigana processing, dictionary registration, and shared conversion state.

;;; Code:

(require 'nskk-kana)
(require 'nskk-state)
(require 'nskk-search)
(require 'nskk-dictionary)
(require 'nskk-prolog)
(eval-when-compile (require 'nskk-state))

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
(declare-function nskk-kana-string-hiragana-to-katakana "nskk-kana")
(declare-function nskk-converter-convert "nskk-converter")

(defgroup nskk-henkan nil
  "Conversion (henkan) pipeline settings."
  :prefix "nskk-henkan-"
  :group 'nskk-ui)

(defcustom nskk-henkan-show-candidates-nth 5
  "Number of SPC presses before showing candidate list.
After this many candidates shown one-by-one, switch to echo area
candidate list display with selection keys."
  :type 'integer
  :group 'nskk-henkan)

(defcustom nskk-henkan-number-to-display-candidates 7
  "Number of candidates to display per page in candidate list."
  :type 'integer
  :group 'nskk-henkan)

(defcustom nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)
  "Selection keys for candidate list display.
These keys allow direct candidate selection in the echo area list."
  :type '(repeat character)
  :group 'nskk-henkan)

(defvar nskk-start-henkan-hook nil
  "Hook run before conversion starts.
DDSKK equivalent: skk-start-henkan-hook")
(put 'nskk-start-henkan-hook 'permanent-local t)

(defvar nskk-henkan-hook nil
  "Hook run during conversion.
DDSKK equivalent: skk-henkan-hook")

(defvar nskk-post-henkan-hook nil
  "Hook run after conversion completes.
DDSKK equivalent: skk-post-henkan-hook")

(defvar nskk-after-henkan-hook nil
  "Hook run after conversion is committed.
DDSKK equivalent: skk-after-henkan-hook")

(defvar nskk-henkan-select-hook nil
  "Hook run when a candidate is selected.
DDSKK equivalent: skk-henkan-select-hook")

(defvar nskk--romaji-buffer)  ;; defined in nskk-state.el
(defvar nskk--system-dict-index) ;; defined in nskk-dictionary.el

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

;;;; Prolog Core Search Type Mapping

(nskk-prolog-set-index 'core-search-type 2 :hash)
(nskk-prolog-<- (core-search-type :exact dict-lookup))
(nskk-prolog-<- (core-search-type :prefix prefix-search))
(nskk-prolog-<- (core-search-type :regex partial-search))

;;;; Prolog Converting Phase Facts

(nskk-prolog-set-index 'converting-phase 1 :hash)
(nskk-prolog-<- (converting-phase active))
(nskk-prolog-<- (converting-phase list))
(nskk-prolog-<- (converting-phase registration))

;;;; Prolog Okurigana Character Classification

(nskk-prolog-set-index 'okurigana-char 2 :hash)
(dolist (c (number-sequence ?A ?Z))
  (nskk-prolog-assert `((okurigana-char ,c ,(downcase c)))))

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
         (not (null (nskk-prolog-query `(converting-phase ,phase)))))))

(defun nskk--has-preedit ()
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
  "Start conversion."
  (interactive)
  (when (nskk--has-preedit)
    (nskk-start-conversion)))

(defun nskk-convert-or-commit ()
  "Start conversion or commit current candidate."
  (interactive)
  (if (nskk-converting-p)
      (nskk-commit-current)
    (nskk-convert)))

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
For the first N-1 candidates, show inline with \u25bc.
On Nth press, switch to echo area candidate list."
  (interactive)
  (when (nskk-converting-p)
    (cl-incf nskk--henkan-count)
    (if (>= nskk--henkan-count nskk-henkan-show-candidates-nth)
        ;; Switch to or advance in candidate list display
        (nskk--show-candidate-list-next)
      ;; Inline one-by-one display
      (nskk--select-candidate 'next))))

(defun nskk-previous-candidate ()
  "Select previous conversion candidate.
In list display mode, go to previous page.
In inline mode, show previous candidate."
  (interactive)
  (when (nskk-converting-p)
    (if nskk-henkan--candidate-list-active
        (nskk--show-candidate-list-prev)
      (progn
        (when (> nskk--henkan-count 0)
          (cl-decf nskk--henkan-count))
        (nskk--select-candidate 'previous)))))

(defun nskk-commit-current ()
  "Commit current conversion candidate.
Replaces preedit text (including \u25bc marker) with the selected candidate,
clears all conversion state including the overlay, marker, and candidates."
  (interactive)
  (when (nskk-converting-p)
    (nskk-with-current-state
    (let* ((candidates (nskk-state-candidates nskk-current-state))
           (index (nskk-state-current-index nskk-current-state))
           (candidate (nth index candidates))
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
      (setq nskk-henkan--candidate-list-active nil)))))

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
Get the preedit text between the conversion start marker and point,
search the dictionary using `nskk-core-search', and if candidates
are found, display the first candidate via the overlay and store all
candidates in the state.  If no candidates are found, leave the
preedit text as-is without entering conversion mode.
The \u25bd marker is replaced with \u25bc when conversion begins."
  (let* ((start (nskk--get-conversion-start))
         (end (point))
         ;; Skip the ▽ marker when extracting preedit text
         (text-start (when start
                       (save-excursion
                         (goto-char start)
                         (if (looking-at nskk-henkan-on-marker-regexp)
                             (+ start (length nskk-henkan-on-marker))
                           start))))
         (text (when (and text-start (> end text-start))
                 (buffer-substring-no-properties text-start end))))
    (when (and text (not (string-empty-p text)))
      (let ((candidates (nskk-core-search text)))
        (if candidates
            (progn
              (setq nskk--henkan-count 1)
              ;; Replace ▽ with ▼
              (nskk--replace-marker-at start nskk-henkan-on-marker-regexp nskk-henkan-active-marker)
              ;; Display first candidate via overlay
              (nskk--update-overlay (+ start (length nskk-henkan-active-marker)) (point) (car candidates))
              ;; Store candidates in state and set phase
              (nskk-with-current-state
                (nskk-state-set-candidates nskk-current-state candidates)
                (setf (nskk-state-current-index nskk-current-state) 0)
                (nskk-state-set-henkan-phase nskk-current-state 'active)))
          ;; No candidates found: start dictionary registration
          (let ((registered (nskk-start-registration text)))
            (when registered
              ;; Delete entire preedit region including ▽ marker
              (delete-region start end)
              (goto-char start)
              (insert registered)
              (nskk--clear-conversion-start-marker)
              (setq nskk--romaji-buffer "")
              (setq nskk--henkan-count 0))))))))

;;;; Dictionary Registration

(defun nskk-start-registration (reading)
  "Start dictionary registration for READING.
Opens a minibuffer prompt for the user to enter the desired text.
READING is the headword that could not be converted.
Supports recursive registration: depth 1 shows [辞書登録],
depth 2 shows [[辞書登録]], etc.
Returns the registered word on success, or nil if cancelled (C-g)."
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
        (nskk-state-set-henkan-phase nskk-current-state nil)))))

(defun nskk--exhaust-candidates ()
  "Handle exhausted candidates by triggering dictionary registration.
If registration succeeds, insert the registered word and clean up state.
If cancelled (C-g), wrap around to the first candidate in list display."
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

;;;; Conversion Lifecycle Operations

(defun nskk-henkan-start-conversion (state &optional candidates)
  "Start conversion process in STATE.
Sets henkan-position, initializes CANDIDATES, resets index.
Returns STATE if input exists, nil otherwise."
  (nskk-with-state state
    (let ((input-buffer (nskk-state-input-buffer state)))
      (when (and input-buffer (> (length input-buffer) 0))
        (nskk-state-set state 'henkan-position 0)
        (nskk-state-set state 'candidates (or candidates '()))
        (nskk-state-set state 'current-index 0)
        state))))

(defun nskk-henkan-commit-conversion (state)
  "Commit the current conversion in STATE.
Moves selected candidate to converted-buffer and clears conversion state.
Returns STATE on success, nil otherwise."
  (nskk-with-state state
    (let ((henkan-pos (nskk-state-henkan-position state))
          (candidates (nskk-state-candidates state))
          (current-idx (nskk-state-current-index state)))
      (when (and henkan-pos candidates (> (length candidates) 0))
        (let ((selected (nth current-idx candidates)))
          (when selected
            (nskk-state-set state 'converted-buffer
                            (concat (or (nskk-state-converted-buffer state) "") selected))))
        (nskk-state-set state 'input-buffer "")
        (nskk-state-set state 'candidates nil)
        (nskk-state-set state 'current-index 0)
        (nskk-state-set state 'henkan-position nil)
        state))))

(defun nskk-henkan-cancel-conversion (state &optional original-input)
  "Cancel the current conversion in STATE.
Restores ORIGINAL-INPUT or clears conversion state. Returns STATE."
  (nskk-with-state state
    (nskk-state-set state 'input-buffer (or original-input ""))
    (nskk-state-set state 'candidates nil)
    (nskk-state-set state 'current-index 0)
    (nskk-state-set state 'henkan-position nil)
    state))

(defun nskk-henkan-in-conversion-p (state)
  "Check if STATE is currently in conversion mode."
  (nskk-with-state state
    (and (nskk-state-henkan-position state)
         (nskk-state-input-buffer state)
         (> (length (nskk-state-input-buffer state)) 0))))

(defun nskk-henkan-has-candidates-p (state)
  "Check if STATE has conversion candidates available."
  (nskk-with-state state
    (let ((candidates (nskk-state-candidates state)))
      (and candidates (> (length candidates) 0)))))

(defun nskk-henkan-get-current-candidate (state)
  "Get the currently selected candidate from STATE."
  (nskk-with-state state
    (let ((candidates (nskk-state-candidates state))
          (idx (nskk-state-current-index state)))
      (when (and candidates idx (< idx (length candidates)))
        (nth idx candidates)))))

(provide 'nskk-henkan)

;;; nskk-henkan.el ends here
