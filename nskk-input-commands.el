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
(require 'nskk-azik nil t)  ; Optional - only if available

(declare-function nskk-toggle-japanese-mode "nskk-mode-switch")
(declare-function nskk-start-conversion "nskk-layer-core")
(declare-function nskk-core-convert-romaji "nskk-layer-core")
(declare-function nskk-core-search "nskk-layer-core")
(declare-function nskk-state-get-mode "nskk-state")
(declare-function nskk-state-p "nskk-state")
(declare-function nskk-state-candidates "nskk-state")
(declare-function nskk-state-current-index "nskk-state")
(declare-function nskk-state-set-candidates "nskk-state")
(declare-function nskk-converter-load-style "nskk-converter")

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
     ;; ASCII/Latin mode: direct insertion
     ((memq current-mode '(ascii latin))
      (nskk-insert-char char n))
     ;; Abbrev mode: handle abbreviations
     ((eq current-mode 'abbrev)
      (nskk-process-abbrev-input char))
     ;; Japanese modes: convert and insert
     (t
      (nskk-process-japanese-input char n)))))

(defun nskk-insert-char (char &optional n)
  "Insert CHAR N times without conversion."
  (let ((n (or n 1)))
    (dotimes (_ n)
      (insert char))))

(defun nskk-process-japanese-input (char n)
  "Process input in Japanese mode (hiragana/katakana).
CHAR is the input character.
N is the repeat count."
  ;; First check for okurigana marker (uppercase consonant)
  (if (nskk-process-okurigana-input char)
      ;; Okurigana was processed, no further action needed
      nil
    ;; Normal processing
    (let* ((kana (nskk-convert-input-to-kana char))
           (mode (nskk-state-get-mode))
           (converted (cond
                       ((string-empty-p kana) nil)
                       ((eq mode 'katakana)
                        (nskk-layer-core-hiragana-to-katakana kana))
                       (t kana))))
      (when converted
        (dotimes (_ n)
          (insert converted))))))

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

(defun nskk-rollback-conversion ()
  "Rollback to pre-conversion state."
  (interactive)
  (when (nskk-converting-p)
    (nskk--restore-preedit)
    (setq nskk-converting-active nil)))

(defun nskk-next-candidate ()
  "Select next conversion candidate."
  (interactive)
  (when (nskk-converting-p)
    (nskk--select-candidate 'next)))

(defun nskk-previous-candidate ()
  "Select previous conversion candidate."
  (interactive)
  (when (nskk-converting-p)
    (nskk--select-candidate 'previous)))

;; Helper functions
(defun nskk-converting-p ()
  "Check if currently in conversion state."
  nskk-converting-active)

(defun nskk-commit-current ()
  "Commit current conversion candidate."
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
      (delete-region start end)
      (goto-char start)
      (insert candidate)
      (setq nskk-converting-active nil)
      (setf (nskk-state-candidates nskk-current-state) nil)
      (setf (nskk-state-current-index nskk-current-state) 0))))

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
If CHAR is uppercase, store okurigana context and start conversion.
Returns t if okurigana was processed, nil otherwise."
  (let ((okuri-char (nskk-detect-okurigana-char char)))
    (when (and okuri-char
               (not (string-empty-p nskk--romaji-buffer))
               (boundp 'nskk-current-state)
               (nskk-state-p nskk-current-state))
      ;; Store okurigana consonant in state
      (nskk-state-set-okurigana nskk-current-state okuri-char)
      ;; Convert remaining romaji to kana
      (let ((kana (nskk-convert-input-to-kana-final)))
        (when kana
          (insert kana)
          ;; Start conversion with okurigana context
          (nskk-start-conversion-with-okuri okuri-char)
          t)))))

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
Searches dictionary with okuri-ari type."
  (let* ((start (nskk--get-conversion-start))
         (end (point))
         (text (buffer-substring start end))
         (query (concat text okuri-char)))
    (setq nskk-converting-active t)
    ;; Search with okuri-ari type
    (let* ((candidates (nskk-core-search query :exact 'okuri-ari))
           (primary (car candidates)))
      (when primary
        (nskk--update-overlay start end primary)
        (when (boundp 'nskk-current-state)
          (nskk-state-set-candidates nskk-current-state candidates)
          (setf (nskk-state-current-index nskk-current-state) 0))))))

(defun nskk--has-preedit ()
  "Check if there's preedit to convert."
  (> (point) (nskk--get-conversion-start)))

(defun nskk--get-conversion-start ()
  "Get conversion start position."
  (mark t))

(defun nskk--restore-preedit ()
  "Restore preedit text after cancel.
Clears the conversion overlay."
  (when (overlayp nskk--conversion-overlay)
    (delete-overlay nskk--conversion-overlay)))

(defun nskk--select-candidate (direction)
  "Select candidate in DIRECTION (next or previous)."
  (let* ((candidates (nskk-state-candidates nskk-current-state))
         (current (nskk-state-current-index nskk-current-state))
         (total (length candidates))
         (new-index (if (eq direction 'next)
                        (mod (1+ current) total)
                      (mod (1- current) total))))
    (setf (nskk-state-current-index nskk-current-state) new-index)
    (let* ((candidate (nth new-index candidates))
           (start (nskk--get-conversion-start))
           (end (point)))
      (nskk--update-overlay start end candidate))))

;; Hook integration for AZIK style loading
;; Note: Call (nskk--maybe-load-azik-style) from nskk-mode activation
;; Example: (add-hook 'nskk-mode-hook #'nskk--maybe-load-azik-style)

(provide 'nskk-input-commands)

;;; nskk-input-commands.el ends here
