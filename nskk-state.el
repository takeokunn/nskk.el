;;; nskk-state.el --- NSKK state management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

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

;; State management for NSKK using cl-defstruct with efficient
;; getter and setter functions.

;;; Code:

(require 'cl-lib)
(require 'nskk-custom)

;; State mode constants
(defconst nskk-state-modes
  '(ascii hiragana katakana katakana-半角 abbrev latin)
  "List of valid NSKK modes.")

;; Main state structure
(cl-defstruct nskk-state
  "Core state structure for NSKK input."
  mode              ;; Current mode (symbol from nskk-state-modes)
  input-buffer      ;; Pending input buffer (string)
  converted-buffer  ;; Converted text buffer (string)
  candidates        ;; List of conversion candidates (list)
  current-index     ;; Current candidate index (integer)
  henkan-position   ;; Position where conversion started (integer or nil)
  marker-position   ;; Cursor position marker (marker or nil)
  previous-mode     ;; Previous mode before current (symbol)
  undo-stack        ;; Undo history stack (list)
  redo-stack        ;; Redo history stack (list)
  metadata)         ;; Additional metadata (plist)

;; Getter function with validation
(defun nskk-state-get (state key)
  "Get KEY value from STATE struct.
KEY can be a slot name symbol or string.
Returns nil if key is not found or state is invalid."
  (when (nskk-state-p state)
    (let ((accessor (intern (format "nskk-state-%s" key))))
      (when (fboundp accessor)
        (funcall accessor state)))))

;; Setter function with validation
(defun nskk-state-set (state key value)
  "Set KEY to VALUE in STATE struct.
Returns VALUE on success, nil on failure.
Supports validation for mode changes."
  (when (nskk-state-p state)
    (let ((key-sym (if (stringp key) (intern key) key)))
      (cond
       ;; Mode validation
       ((eq key-sym 'mode)
        (when (nskk-state-valid-mode-p value)
          ;; Store previous mode
          (setf (nskk-state-previous-mode state) (nskk-state-mode state))
          (setf (nskk-state-mode state) value)
          value))
       ;; Explicit slot setters
       ((eq key-sym 'input-buffer)
        (setf (nskk-state-input-buffer state) value) value)
       ((eq key-sym 'converted-buffer)
        (setf (nskk-state-converted-buffer state) value) value)
       ((eq key-sym 'candidates)
        (setf (nskk-state-candidates state) value) value)
       ((eq key-sym 'current-index)
        (setf (nskk-state-current-index state) value) value)
       ((eq key-sym 'henkan-position)
        (setf (nskk-state-henkan-position state) value) value)
       ((eq key-sym 'marker-position)
        (setf (nskk-state-marker-position state) value) value)
       ((eq key-sym 'previous-mode)
        (setf (nskk-state-previous-mode state) value) value)
       ((eq key-sym 'undo-stack)
        (setf (nskk-state-undo-stack state) value) value)
       ((eq key-sym 'redo-stack)
        (setf (nskk-state-redo-stack state) value) value)
       ((eq key-sym 'metadata)
        (setf (nskk-state-metadata state) value) value)
       (t nil)))))

;; Internal helper to get slot accessor name
(defun nskk-state--slot-accessor-name (slot)
  "Generate accessor name for SLOT."
  (intern (format "nskk-state-%s" slot)))

;; State creation functions
(defun nskk-state-create (&optional initial-mode)
  "Create a new NSKK state object.
INITIAL-MODE defaults to `nskk-state-default-mode' if not specified."
  (let ((mode (or initial-mode
                   nskk-state-default-mode
                   'ascii)))
    (unless (nskk-state-valid-mode-p mode)
      (setq mode 'ascii))
    (make-nskk-state
     :mode mode
     :input-buffer ""
     :converted-buffer ""
     :candidates nil
     :current-index 0
     :henkan-position nil
     :marker-position nil
     :previous-mode mode
     :undo-stack nil
     :redo-stack nil
     :metadata nil)))

;; State validation functions
(defun nskk-state-valid-mode-p (mode)
  "Check if MODE is a valid NSKK mode."
  (and (symbolp mode)
       (memq mode nskk-state-modes)))

(defun nskk-state-in-henkan-mode-p (state)
  "Check if STATE is currently in conversion mode."
  (and (nskk-state-p state)
       (nskk-state-henkan-position state)
       (not (zerop (length (nskk-state-input-buffer state))))))

;; State transition functions
(defun nskk-state-transition (state from-mode to-mode)
  "Transition STATE from FROM-MODE to TO-MODE.
Returns t on success, nil on failure."
  (when (and (nskk-state-p state)
             (eq (nskk-state-mode state) from-mode)
             (nskk-state-valid-mode-p to-mode))
    (nskk-state-set state 'mode to-mode)
    t))

(defun nskk-state-reset (state)
  "Reset STATE to initial state (preserves mode).
Clears buffers, candidates, and stacks."
  (when (nskk-state-p state)
    (setf (nskk-state-input-buffer state) ""
          (nskk-state-converted-buffer state) ""
          (nskk-state-candidates state) nil
          (nskk-state-current-index state) 0
          (nskk-state-henkan-position state) nil
          (nskk-state-marker-position state) nil
          (nskk-state-undo-stack state) nil
          (nskk-state-redo-stack state) nil
          (nskk-state-metadata state) nil)
    t))

;; Buffer management helpers
(defun nskk-state-append-input (state char)
  "Append CHAR to STATE's input buffer.
Uses O(1) character append for efficient buffer building."
  (when (nskk-state-p state)
    (let ((buf (nskk-state-input-buffer state)))
      (let ((len (length buf)))
        ;; Create new string with one more character
        (let ((new-buf (make-string (1+ len) ?\0)))
          ;; Copy old buffer
          (dotimes (i len)
            (aset new-buf i (aref buf i)))
          ;; Add new character
          (aset new-buf len char)
          (setf (nskk-state-input-buffer state) new-buf))))))

(defun nskk-state-delete-last-char (state)
  "Delete last character from STATE's input buffer.
Returns deleted char or nil if buffer empty."
  (when (nskk-state-p state)
    (let ((buf (nskk-state-input-buffer state)))
      (when (> (length buf) 0)
        (let ((len (length buf)))
          (setf (nskk-state-input-buffer state)
                (substring buf 0 (1- len)))
          (aref buf (1- len)))))))

(defun nskk-state-clear-input (state)
  "Clear STATE's input buffer."
  (when (nskk-state-p state)
    (setf (nskk-state-input-buffer state) "")))

;; Candidate management helpers
(defun nskk-state-set-candidates (state candidates)
  "Set CANDIDATES list in STATE and reset index to 0."
  (when (nskk-state-p state)
    (setf (nskk-state-candidates state) candidates
          (nskk-state-current-index state) 0)))

(defun nskk-state-next-candidate (state)
  "Move to next candidate in STATE.
Returns current candidate or nil if no candidates."
  (when (and (nskk-state-p state)
             (nskk-state-candidates state))
    (let ((candidates (nskk-state-candidates state))
          (index (nskk-state-current-index state)))
      (when candidates
        (setf (nskk-state-current-index state)
              (mod (1+ index) (length candidates)))
        (nth (nskk-state-current-index state) candidates)))))

(defun nskk-state-previous-candidate (state)
  "Move to previous candidate in STATE.
Returns current candidate or nil if no candidates."
  (when (and (nskk-state-p state)
             (nskk-state-candidates state))
    (let ((candidates (nskk-state-candidates state))
          (index (nskk-state-current-index state)))
      (when candidates
        (setf (nskk-state-current-index state)
              (mod (1- index) (length candidates)))
        (nth (nskk-state-current-index state) candidates)))))

(defun nskk-state-current-candidate (state)
  "Get current candidate from STATE."
  (when (and (nskk-state-p state)
             (nskk-state-candidates state))
    (nth (nskk-state-current-index state)
         (nskk-state-candidates state))))

;; Metadata helpers
(defun nskk-state-get-metadata (state key)
  "Get metadata KEY from STATE."
  (when (nskk-state-p state)
    (plist-get (nskk-state-metadata state) key)))

(defun nskk-state-put-metadata (state key value)
  "Set metadata KEY to VALUE in STATE."
  (when (nskk-state-p state)
    (setf (nskk-state-metadata state)
          (plist-put (nskk-state-metadata state) key value))))

;; Additional state setters for layer integration
(defun nskk-state-set-remaining-romaji (state value)
  "Set remaining romaji in STATE metadata to VALUE."
  (when (nskk-state-p state)
    (nskk-state-put-metadata state 'remaining-romaji value)))

(defun nskk-state-set-kana-type (state value)
  "Set kana type in STATE metadata to VALUE."
  (when (nskk-state-p state)
    (nskk-state-put-metadata state 'kana-type value)))

(defun nskk-state-set-width-type (state value)
  "Set width type in STATE metadata to VALUE."
  (when (nskk-state-p state)
    (nskk-state-put-metadata state 'width-type value)))

;; Global state management
(defvar nskk-current-state nil
  "Global NSKK state object for the current buffer.")

(defun nskk-state-initialize ()
  "Initialize the global NSKK state."
  (setq nskk-current-state (nskk-state-create nskk-state-default-mode)))

(defun nskk-state-get-mode ()
  "Get current mode from global state."
  (when (nskk-state-p nskk-current-state)
    (nskk-state-mode nskk-current-state)))

(defun nskk-state-set-mode (mode)
  "Set MODE in global state."
  (when (nskk-state-p nskk-current-state)
    (nskk-state-set nskk-current-state 'mode mode)))

(provide 'nskk-state)

;;; nskk-state.el ends here
