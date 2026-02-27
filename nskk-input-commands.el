;;; nskk-input-commands.el --- Input processing commands for NSKK -*- lexical-binding: t; -*-

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

;; This file implements input processing commands (Layer 3: Application Layer).
;;
;; Layer Responsibilities:
;; - Application Layer handles character input and conversion control
;; - Routes conversion requests through Core Engine Layer API (nskk-layer-core)
;; - Uses State Management API (nskk-state) for mode and context tracking
;; - Does NOT directly depend on Core Engine implementation (nskk-converter)
;;
;; Functions:
;; - Character input processing and mode-aware routing
;; - Conversion control (start, commit, cancel)
;; - Candidate navigation

;;; Code:

(require 'nskk-mode-switch)
(require 'nskk-layer-core)
(require 'nskk-state)
(require 'nskk-custom)
(require 'nskk-candidate-window)
(require 'nskk-azik nil t)  ; Optional - only if available

(declare-function nskk-toggle-japanese-mode "nskk-mode-switch")
(declare-function nskk-core-convert-romaji "nskk-layer-core")
(declare-function nskk-core-search "nskk-layer-core")
(declare-function nskk-layer-core-hiragana-to-katakana "nskk-layer-core")
(declare-function nskk-state-get-mode "nskk-state")
(declare-function nskk-state-p "nskk-state")
(declare-function nskk-state-candidates "nskk-state")
(declare-function nskk-state-current-index "nskk-state")
(declare-function nskk-state-set-candidates "nskk-state")
(declare-function nskk-state-set-okurigana "nskk-state")
(declare-function nskk-state-get-okurigana "nskk-state")
(declare-function nskk-state-set-henkan-phase "nskk-state")
(declare-function nskk-converter-load-style "nskk-converter")
(declare-function nskk-candidate-show-list "nskk-candidate-window")
(declare-function nskk-candidate-list-active-p "nskk-candidate-window")
(declare-function nskk-candidate-hide-list "nskk-candidate-window")
(declare-function nskk-candidate-list-select-by-key "nskk-candidate-window")
(declare-function nskk-dict-register-word "nskk-dict-io")

(defvar nskk--romaji-buffer)

(defconst nskk-henkan-on-marker "▽"
  "Marker character for henkan-on state (reading input).")

(defconst nskk-henkan-active-marker "▼"
  "Marker character for henkan-active state (conversion active).")

(defconst nskk-okurigana-marker "*"
  "Marker character for okurigana boundary.")

(defconst nskk-henkan-on-marker-regexp nskk-henkan-on-marker-regexp
  "Pre-computed regexp for henkan-on marker.")

(defconst nskk-henkan-active-marker-regexp nskk-henkan-active-marker-regexp
  "Pre-computed regexp for henkan-active marker.")

(defconst nskk-okurigana-marker-regexp nskk-okurigana-marker-regexp
  "Pre-computed regexp for okurigana boundary marker.")

;; Marker operation helpers (suppress modification hooks for performance)
(defun nskk--insert-marker (marker)
  "Insert MARKER string without triggering modification hooks or undo."
  (let ((inhibit-modification-hooks t)
        (buffer-undo-list t))
    (insert marker)))

(defun nskk--delete-marker-at (pos marker-regexp)
  "Delete marker matching MARKER-REGEXP at POS without triggering hooks."
  (save-excursion
    (goto-char pos)
    (when (looking-at marker-regexp)
      (let ((inhibit-modification-hooks t)
            (buffer-undo-list t))
        (delete-char (length (match-string 0)))))))

(defun nskk--replace-marker-at (pos old-regexp new-marker)
  "Replace marker matching OLD-REGEXP at POS with NEW-MARKER."
  (save-excursion
    (goto-char pos)
    (when (looking-at old-regexp)
      (let ((inhibit-modification-hooks t)
            (buffer-undo-list t))
        (delete-char (length (match-string 0)))
        (insert new-marker)))))

;; AZIK style initialization
(defun nskk--maybe-load-azik-style ()
  "Load AZIK style if configured and not already loaded."
  (when (and (featurep 'nskk-azik)
             (eq nskk-converter-romaji-style 'azik))
    (nskk-converter-load-style 'azik)))

;; AZIK-specific key handlers
(defun nskk-handle-q-key ()
  "Handle q key press based on current romaji style and configuration.
In AZIK mode with context-aware behavior:
  - If there is pending romaji input, produce ん
  - Otherwise, toggle hiragana/katakana mode
In always-n mode: always produce ん
In toggle-only mode: toggle mode regardless of context
In standard mode: toggle mode (default SKK behavior)."
  (interactive)
  (cond
   ;; Standard mode: normal toggle behavior
   ((not (eq nskk-converter-romaji-style 'azik))
    (nskk-toggle-japanese-mode))
   ;; AZIK always-n mode
   ((eq nskk-azik-q-behavior 'always-n)
    (insert "ん"))
   ;; AZIK toggle-only mode
   ((eq nskk-azik-q-behavior 'toggle-only)
    (nskk-toggle-japanese-mode))
   ;; AZIK context-aware mode (default)
   (t
    (if (string-empty-p nskk--romaji-buffer)
        (nskk-toggle-japanese-mode)
      (insert "ん")))))

(defun nskk-handle-semicolon-key ()
  "Handle semicolon key press.
In AZIK mode: produce small tsu (っ)
In standard mode with empty buffer: start abbrev mode (if applicable)
Otherwise: process as normal input."
  (interactive)
  (if (eq nskk-converter-romaji-style 'azik)
      (insert "っ")
    ;; Standard behavior - could trigger abbrev mode
    (nskk-self-insert 1)))

;; Romaji input buffer for incremental kana conversion
(defvar-local nskk--romaji-buffer ""
  "Buffer for accumulating romaji input before conversion to kana.")

(defvar-local nskk--henkan-count 0
  "Number of times SPC has been pressed during current conversion.")

;; Conversion start marker for henkan point tracking
(defvar-local nskk--conversion-start-marker nil
  "Marker for the position where conversion (henkan) input started.
Set when the user types an uppercase letter in Japanese mode to begin
composing a word for conversion.  This replaces the use of `mark' for
SKK conversion tracking, so that SKK does not interfere with the
Emacs mark ring.")

;; Input processing
(defun nskk-self-insert (n)
  "Process self-insert input character.
N is the command prefix argument."
  (interactive "p")
  (let ((char (if (integerp last-command-event)
                  last-command-event
                (aref last-command-event 0)))
        (current-mode (or (nskk-state-get-mode) 'ascii)))
    (cond
     ;; Candidate list active: check if this is a selection key
     ((and (nskk-candidate-list-active-p)
           (nskk--try-candidate-selection char))
      nil)  ; handled by nskk--try-candidate-selection
     ;; ASCII/Latin mode: direct insertion
     ((memq current-mode '(ascii latin))
      (nskk-insert-char char n))
     ;; Abbrev mode: handle abbreviations
     ((eq current-mode 'abbrev)
      (nskk-process-abbrev-input char))
     ;; Full-width Latin mode: convert to full-width ASCII
     ((eq current-mode 'jisx0208-latin)
      (nskk-insert-fullwidth-char char n))
     ;; Japanese modes: convert and insert
     (t
      (nskk-process-japanese-input char n)))))

(defun nskk-insert-char (char &optional n)
  "Insert CHAR N times without conversion."
  (let ((n (or n 1)))
    (dotimes (_ n)
      (insert char))))

(defun nskk-insert-fullwidth-char (char &optional n)
  "Insert full-width version of ASCII CHAR N times.
Converts ASCII characters (0x21-0x7E) to their JIS X 0208 full-width
equivalents (0xFF01-0xFF5E).  Space (0x20) becomes ideographic space (0x3000)."
  (let ((n (or n 1))
        (fw-char (cond
                  ((= char ?\s) ?\u3000)  ; space -> ideographic space
                  ((and (>= char ?!) (<= char ?~))
                   (+ char #xFF01 (- ?!)))  ; ASCII printable -> full-width
                  (t char))))              ; non-ASCII: pass through
    (dotimes (_ n)
      (insert fw-char))))

(defun nskk--try-candidate-selection (char)
  "Try to select a candidate using CHAR as a selection key.
Returns non-nil if CHAR was a valid selection key and the candidate was selected."
  (let ((index (nskk-candidate-list-select-by-key
                char
                (nskk-state-candidates nskk-current-state)
                (nskk-state-current-index nskk-current-state))))
    (when index
      ;; Select and commit the candidate
      (setf (nskk-state-current-index nskk-current-state) index)
      (nskk-commit-current)
      t)))

(defun nskk-process-japanese-input (char n)
  "Process input in Japanese mode (hiragana/katakana).
CHAR is the input character.
N is the repeat count.
When CHAR is uppercase and `nskk-converter-auto-start-henkan' is non-nil,
set the conversion start marker at the current point and process the
lowercase version of the letter as normal romaji input."
  ;; Detect uppercase letter for henkan start point
  (let ((is-henkan-start (and (characterp char)
                              (<= ?A char) (<= char ?Z)
                              nskk-converter-auto-start-henkan
                              (not (nskk--conversion-start-active-p)))))
    (when is-henkan-start
      ;; Set the conversion start marker at current point
      (nskk--set-conversion-start-marker (point))
      ;; Insert ▽ marker (ddskk-compatible inline marker)
      (nskk--insert-marker nskk-henkan-on-marker)
      ;; Set henkan phase to 'on
      (when (and (boundp 'nskk-current-state) (nskk-state-p nskk-current-state))
        (nskk-state-set-henkan-phase nskk-current-state 'on)))
    (let ((effective-char (if is-henkan-start (downcase char) char)))
      ;; Check for okurigana marker (uppercase consonant while henkan is active)
      (if (and (not is-henkan-start)
               (nskk-process-okurigana-input effective-char))
          ;; Okurigana was processed, no further action needed
          nil
        ;; Normal processing
        (let* ((kana (nskk-convert-input-to-kana effective-char))
               (mode (nskk-state-get-mode))
               (converted (cond
                           ((string-empty-p kana) nil)
                           ((eq mode 'katakana)
                            (nskk-layer-core-hiragana-to-katakana kana))
                           (t kana))))
          (when converted
            (let ((okuri (and (boundp 'nskk-current-state)
                              (nskk-state-p nskk-current-state)
                              (nskk-state-get-okurigana nskk-current-state))))
              (if okuri
                  ;; Okurigana kana completed: insert kana then trigger conversion
                  (let ((preedit-end (point)))
                    (dotimes (_ n)
                      (insert converted))
                    (nskk--trigger-okuri-conversion okuri preedit-end)
                    (nskk-state-set-okurigana nskk-current-state nil))
                ;; Normal insertion
                (dotimes (_ n)
                  (insert converted))))))))))

;; Overlay management for conversion display
(defvar-local nskk--conversion-overlay nil
  "Overlay for displaying converted text.")

(defun nskk--update-overlay (start end text)
  "Update overlay to show TEXT from START to END."
  (unless (overlayp nskk--conversion-overlay)
    (setq nskk--conversion-overlay (make-overlay start end)))
  (move-overlay nskk--conversion-overlay start end (current-buffer))
  (overlay-put nskk--conversion-overlay 'display text)
  (overlay-put nskk--conversion-overlay 'face 'highlight))

;; Conversion control
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
  "Cancel preedit input and remove the ▽ marker.
Deletes preedit text between the conversion start marker and point,
including the ▽ marker character, and resets state."
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
  (when (and (boundp 'nskk-current-state)
             (nskk-state-p nskk-current-state))
    (nskk-state-set-henkan-phase nskk-current-state nil)))

(defun nskk-rollback-conversion ()
  "Rollback to pre-conversion state.
Deletes the ▽ or ▼ marker and restores preedit text."
  (interactive)
  (when (nskk-converting-p)
    ;; Remove marker character from buffer
    (let ((start (nskk--get-conversion-start)))
      (when start
        (or (nskk--delete-marker-at start nskk-henkan-active-marker-regexp)
            (nskk--delete-marker-at start nskk-henkan-on-marker-regexp))))
    (nskk--restore-preedit)
    (setq nskk-converting-active nil)
    (setq nskk--henkan-count 0)
    (nskk-candidate-hide-list)
    ;; Reset henkan phase
    (when (and (boundp 'nskk-current-state)
               (nskk-state-p nskk-current-state))
      (nskk-state-set-henkan-phase nskk-current-state nil))))

(defun nskk-next-candidate ()
  "Select next conversion candidate.
For the first N-1 candidates, show inline with ▼.
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
    (if (nskk-candidate-list-active-p)
        (nskk--show-candidate-list-prev)
      (progn
        (when (> nskk--henkan-count 0)
          (cl-decf nskk--henkan-count))
        (nskk--select-candidate 'previous)))))

;; Helper functions
(defun nskk-converting-p ()
  "Check if currently in conversion state."
  nskk-converting-active)

(defun nskk-commit-current ()
  "Commit current conversion candidate.
Replaces preedit text (including ▼ marker) with the selected candidate,
clears all conversion state including the overlay, marker, and candidates."
  (interactive)
  (when (and nskk-converting-active
             (boundp 'nskk-current-state)
             (nskk-state-p nskk-current-state))
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
      (setq nskk-converting-active nil)
      (setq nskk--romaji-buffer "")
      (setf (nskk-state-candidates nskk-current-state) nil)
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-set-henkan-phase nskk-current-state nil)
      (setq nskk--henkan-count 0)
      (nskk-candidate-hide-list))))

(defun nskk-process-abbrev-input (char)
  "Process input in abbrev mode."
  ;; TODO: Implement abbrev mode processing
  (insert char))

(defun nskk-convert-input-to-kana (char)
  "Convert input CHAR to kana using the romaji-to-kana converter.
Accumulates romaji characters in `nskk--romaji-buffer' and converts
to kana when a complete romaji sequence is recognized.
Returns the converted kana string, or an empty string if the input
is still incomplete (waiting for more characters)."
  (let* ((input (concat nskk--romaji-buffer (char-to-string char)))
         (result (nskk-core-convert-romaji input)))
    (cond
     ;; Successful conversion: return kana, keep remainder in buffer
     ((and result (stringp (car result)))
      (let ((kana (car result))
            (remaining (cdr result)))
        (setq nskk--romaji-buffer
              (if (and (stringp remaining) (> (length remaining) 0))
                  remaining
                ""))
        kana))
     ;; n + consonant rule: "n" followed by non-vowel/non-y/non-n/non-quote
     ((and (not (string-empty-p nskk--romaji-buffer))
           (= (aref nskk--romaji-buffer (1- (length nskk--romaji-buffer))) ?n)
           (not (memq char '(?a ?i ?u ?e ?o ?y ?n ?\'))))
      ;; Emit ん for the trailing n, buffer the new char for next input
      (let ((prefix-without-n (substring nskk--romaji-buffer 0 (1- (length nskk--romaji-buffer)))))
        (setq nskk--romaji-buffer (char-to-string char))
        (let ((prefix-kana (if (> (length prefix-without-n) 0)
                               (let ((prev (nskk-core-convert-romaji prefix-without-n)))
                                 (if (and prev (stringp (car prev)))
                                     (car prev)
                                   ""))
                             "")))
          (concat prefix-kana "ん"))))
     ;; Sokuon rule: same consonant doubled (not vowel, not n)
     ((and (> (length nskk--romaji-buffer) 0)
           (let ((last-buf-char (aref nskk--romaji-buffer (1- (length nskk--romaji-buffer)))))
             (and (= last-buf-char char)
                  (not (memq char '(?a ?i ?u ?e ?o ?n))))))
      ;; Emit っ, keep the second consonant as new buffer
      (setq nskk--romaji-buffer (char-to-string char))
      "っ")
     ;; Incomplete: buffer the input, return empty string
     ((and result (eq (car result) :incomplete))
      (setq nskk--romaji-buffer input)
      "")
     ;; No match at all: flush the buffer as-is and return the character
     (t
      (setq nskk--romaji-buffer "")
      input))))

;; Okurigana detection and processing
(defun nskk-detect-okurigana-char (char)
  "Check if CHAR is an okurigana marker (uppercase consonant).
Returns the lowercase consonant if it's a marker, nil otherwise."
  (when (and (characterp char)
             (<= ?A char) (<= char ?Z))
    (downcase char)))

(defun nskk-process-okurigana-input (char)
  "Process CHAR as potential okurigana marker.
If CHAR is uppercase and the conversion start marker is active,
store okurigana context, insert * boundary marker, and put the
consonant into the romaji buffer for deferred kana accumulation."
  (let ((okuri-char (nskk-detect-okurigana-char char)))
    (when (and okuri-char
               (nskk--conversion-start-active-p)
               (boundp 'nskk-current-state)
               (nskk-state-p nskk-current-state))
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
      t)))

(defun nskk-convert-input-to-kana-final ()
  "Convert remaining romaji buffer to kana and clear buffer.
Returns the converted kana string."
  (let ((result (nskk-core-convert-romaji nskk--romaji-buffer)))
    (prog1
        (if (and result (stringp (car result)))
            (car result)
          nskk--romaji-buffer)
      (setq nskk--romaji-buffer ""))))

(defun nskk-start-conversion-with-okuri (okuri-char)
  "Start conversion with okurigana context OKURI-CHAR.
Searches dictionary with okuri-ari type using the conversion start
marker for the preedit region.  Handles ▽ marker in preedit text."
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
          (setq nskk-converting-active t)
          (nskk--update-overlay (+ start (length nskk-henkan-active-marker)) end primary)
          (when (and (boundp 'nskk-current-state)
                     (nskk-state-p nskk-current-state))
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
          (setq nskk-converting-active t)
          (nskk--update-overlay (+ start (length nskk-henkan-active-marker)) (point) primary)
          (when (and (boundp 'nskk-current-state)
                     (nskk-state-p nskk-current-state))
            (nskk-state-set-candidates nskk-current-state candidates)
            (setf (nskk-state-current-index nskk-current-state) 0)
            (nskk-state-set-henkan-phase nskk-current-state 'active)))))))

(defun nskk--has-preedit ()
  "Check if there is preedit text to convert.
Returns non-nil when the conversion start marker is set and point is
past the marker position plus the ▽ marker length."
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

;; Conversion pipeline

(defun nskk-start-conversion ()
  "Start dictionary conversion for the preedit text.
Get the preedit text between the conversion start marker and point,
search the dictionary using `nskk-core-search', and if candidates
are found, display the first candidate via the overlay and store all
candidates in the state.  If no candidates are found, leave the
preedit text as-is without entering conversion mode.
The ▽ marker is replaced with ▼ when conversion begins."
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
              ;; Enter conversion mode
              (setq nskk-converting-active t)
              ;; Display first candidate via overlay
              (nskk--update-overlay (+ start (length nskk-henkan-active-marker)) (point) (car candidates))
              ;; Store candidates in state and set phase
              (when (and (boundp 'nskk-current-state)
                         (nskk-state-p nskk-current-state))
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
              (setq nskk-converting-active nil)
              (setq nskk--romaji-buffer "")
              (setq nskk--henkan-count 0))))))))

;; Hook integration for AZIK style loading
;; Note: Call (nskk--maybe-load-azik-style) from nskk-mode activation
;; Example: (add-hook 'nskk-mode-hook #'nskk--maybe-load-azik-style)

(defun nskk--show-candidate-list-next ()
  "Show next page of candidates in echo area list.
When all candidates are exhausted, trigger dictionary registration."
  (let* ((candidates (nskk-state-candidates nskk-current-state))
         (current (nskk-state-current-index nskk-current-state))
         (per-page (min nskk-henkan-number-to-display-candidates
                        (length nskk-henkan-show-candidates-keys)))
         (next-start (if (nskk-candidate-list-active-p)
                         (+ current per-page)
                       current)))
    (if (>= next-start (length candidates))
        ;; All candidates exhausted: trigger dictionary registration
        (nskk--exhaust-candidates)
      ;; Show next page
      (setf (nskk-state-current-index nskk-current-state) next-start)
      (nskk-state-set-henkan-phase nskk-current-state 'list)
      (nskk-candidate-show-list candidates next-start))))

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
    (nskk-candidate-show-list candidates prev-start)))

;;; Dictionary Registration

(defvar-local nskk--registration-depth 0
  "Current nesting depth of dictionary registration.")

(defun nskk-start-registration (reading)
  "Start dictionary registration for READING.
Opens a minibuffer prompt for the user to enter the desired text.
READING is the headword that could not be converted.
Supports recursive registration: depth 1 shows [辞書登録],
depth 2 shows [[辞書登録]], etc.
Returns the registered word on success, or nil if cancelled (C-g)."
  (when (and (boundp 'nskk-current-state)
             (nskk-state-p nskk-current-state))
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
      (when (and (boundp 'nskk-current-state)
                 (nskk-state-p nskk-current-state))
        (nskk-state-set-henkan-phase nskk-current-state nil)))))

(defun nskk--exhaust-candidates ()
  "Handle exhausted candidates by triggering dictionary registration.
If registration succeeds, insert the registered word and clean up state.
If cancelled (C-g), wrap around to the first candidate in list display."
  (nskk-candidate-hide-list)
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
          (setq nskk-converting-active nil)
          (setq nskk--romaji-buffer "")
          (setq nskk--henkan-count 0)
          (when (and (boundp 'nskk-current-state)
                     (nskk-state-p nskk-current-state))
            (setf (nskk-state-candidates nskk-current-state) nil)
            (setf (nskk-state-current-index nskk-current-state) 0)
            (nskk-state-set-henkan-phase nskk-current-state nil)))
      ;; Registration cancelled: wrap to first candidate in list
      (let ((candidates (nskk-state-candidates nskk-current-state)))
        (setf (nskk-state-current-index nskk-current-state) 0)
        (setq nskk--henkan-count nskk-henkan-show-candidates-nth)
        (nskk-state-set-henkan-phase nskk-current-state 'list)
        (nskk-candidate-show-list candidates 0)))))

(provide 'nskk-input-commands)

;;; nskk-input-commands.el ends here
