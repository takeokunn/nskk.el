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
                (aref last-command-event 0))))
    (cond
     ;; Latin mode: direct insertion
     ((eq (nskk-get-mode) 'latin)
      (nskk-insert-char char n))
     ;; Abbrev mode: handle abbreviations
     ((eq (nskk-get-mode) 'abbrev)
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
  (let* ((kana (nskk-convert-input-to-kana char))
         (mode (nskk-get-mode))
         (converted (cond
                     ((string-empty-p kana) nil)
                     ((eq mode 'katakana)
                      (nskk-layer-core-hiragana-to-katakana kana))
                     (t kana))))
    (when converted
      (dotimes (_ n)
        (insert converted)))))

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
(defun nskk-get-mode ()
  "Get current input mode."
  (nskk-state-get-mode))

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

(defun nskk--has-preedit ()
  "Check if there's preedit to convert."
  (> (point) (nskk--get-conversion-start)))

(defun nskk--get-conversion-start ()
  "Get conversion start position."
  (mark t))

(defun nskk-start-conversion ()
  "Start conversion process."
  (let ((start (nskk--get-conversion-start))
        (end (point)))
    (setq nskk-converting-active t)
    (let* ((text (buffer-substring start end))
           ;; Use Core layer API for dictionary search
           (candidates (nskk-core-search text :prefix))
           (primary (car candidates)))
      (nskk--update-overlay start end primary)
      ;; Store candidates in global state
      (when (boundp 'nskk-current-state)
        (nskk-state-set-candidates nskk-current-state candidates)
        (setf (nskk-state-current-index nskk-current-state) 0)))))

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

(provide 'nskk-input-commands)

;;; nskk-input-commands.el ends here
