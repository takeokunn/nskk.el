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

(declare-function nskk-debug-message "nskk-debug" (format-string &rest args))

;; State mode constants
(defconst nskk-state-modes
  '(ascii hiragana katakana katakana-半角 abbrev latin jisx0208-latin)
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
  henkan-phase      ;; Henkan phase: nil, on (▽), active (▼), list, registration
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
        (unless (nskk-state-valid-mode-p value)
          (error "Invalid mode: %s. Valid modes: %s" value nskk-state-modes))
        ;; Store previous mode
        (setf (nskk-state-previous-mode state) (nskk-state-mode state))
        (setf (nskk-state-mode state) value)
        value)
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
       ((eq key-sym 'henkan-phase)
        (setf (nskk-state-henkan-phase state) value) value)
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
     :henkan-phase nil
     :metadata nil)))

;; State validation functions
(defun nskk-state-valid-mode-p (mode)
  "Check if MODE is a valid NSKK mode."
  (declare (pure t) (side-effect-free t))
  (and (symbolp mode)
       (memq mode nskk-state-modes)))

(defun nskk-state-in-henkan-mode-p (state)
  "Check if STATE is currently in conversion mode."
  (and (nskk-state-p state)
       (memq (nskk-state-henkan-phase state) '(on active list registration))))

(defun nskk-state-henkan-on-p (state)
  "Check if STATE is in henkan-on phase (▽)."
  (and (nskk-state-p state)
       (eq (nskk-state-henkan-phase state) 'on)))

(defun nskk-state-henkan-active-p (state)
  "Check if STATE is in henkan-active phase (▼)."
  (and (nskk-state-p state)
       (eq (nskk-state-henkan-phase state) 'active)))

(defconst nskk-state-henkan-phases '(nil on active list registration)
  "List of valid henkan phases.")

(defun nskk-state-set-henkan-phase (state phase)
  "Set henkan PHASE in STATE.
PHASE must be nil, on, active, list, or registration."
  (when (nskk-state-p state)
    (unless (memq phase nskk-state-henkan-phases)
      (error "Invalid henkan phase: %s. Valid phases: %s" phase nskk-state-henkan-phases))
    (setf (nskk-state-henkan-phase state) phase)))

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
          (nskk-state-henkan-phase state) nil
          (nskk-state-metadata state) nil)
    t))

;; Buffer management helpers
(defun nskk-state-append-input (state char)
  "Append CHAR to STATE's input buffer.
CHAR must be a valid character (integer).
Returns the new buffer string on success, nil on failure.
Uses `concat' with a list to avoid intermediate string allocation."
  (when (and (nskk-state-p state)
             (characterp char))
    (let ((buf (nskk-state-input-buffer state)))
      ;; Ensure buf is a string (defensive check)
      (unless (stringp buf)
        (setq buf ""))
      (let ((new-buf (concat buf (list char))))
        (setf (nskk-state-input-buffer state) new-buf)
        new-buf))))

(defun nskk-state-delete-last-char (state)
  "Delete last character from STATE's input buffer.
Returns the deleted character on success, nil if buffer is empty
or state is invalid.
Properly handles multibyte characters."
  (when (nskk-state-p state)
    (let ((buf (nskk-state-input-buffer state)))
      ;; Ensure buf is a string (defensive check)
      (when (and (stringp buf)
                 (> (length buf) 0))
        ;; Get the last character before deleting
        (let ((last-char (aref buf (1- (length buf))))
              (new-buf (substring buf 0 (1- (length buf)))))
          (setf (nskk-state-input-buffer state) new-buf)
          last-char)))))

(defun nskk-state-clear-input (state)
  "Clear STATE's input buffer.
Returns t on success, nil if state is invalid."
  (when (nskk-state-p state)
    (setf (nskk-state-input-buffer state) "")
    t))

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

(defun nskk-state-set-okurigana (state value)
  "Set okurigana in STATE metadata to VALUE.
VALUE should be the okurigana consonant (e.g., \\='k\\=' for \\='く\\=')."
  (when (nskk-state-p state)
    (nskk-state-put-metadata state 'okurigana value)))

(defun nskk-state-get-okurigana (state)
  "Get okurigana from STATE metadata."
  (when (nskk-state-p state)
    (nskk-state-get-metadata state 'okurigana)))

;; Buffer-local state management
(defvar-local nskk-current-state nil
  "Buffer-local NSKK state object for the current buffer.")

(defun nskk-state-initialize ()
  "Initialize NSKK state for the current buffer."
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
