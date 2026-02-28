;;; nskk-state.el --- NSKK state management -*- lexical-binding: t; -*-

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

;; Buffer-local state management for NSKK using `cl-defstruct'.
;;
;; The central `nskk-state' struct tracks:
;; - Current input mode: hiragana, katakana, ascii, latin, abbrev,
;;   jisx0208-latin
;; - Henkan (conversion) phase: whether the user is mid-conversion (▼)
;; - Candidate list and selection index
;; - Preedit text (reading under construction, shown with ▽ marker)
;;
;; State is stored buffer-locally in `nskk-current-state' and mutated
;; exclusively through the setter functions defined in this module.

;;; Code:

(require 'cl-lib)
(require 'nskk-prolog)
(eval-when-compile (require 'nskk-macros))

(defgroup nskk-state nil
  "State management settings."
  :prefix "nskk-state-"
  :group 'nskk-kana)

(defcustom nskk-state-default-mode 'ascii
  "Default input mode when NSKK is activated."
  :type '(choice (const :tag "ASCII" ascii)
                 (const :tag "Hiragana" hiragana)
                 (const :tag "Katakana" katakana)
                 (const :tag "Full-width Latin" jisx0208-latin))
  :group 'nskk-state)

(defcustom nskk-state-undo-limit 100
  "Maximum number of undo operations to keep in history."
  :type 'integer
  :group 'nskk-state)

;;;; Internal Macros

(defmacro nskk-with-candidates (state &rest body)
  "Execute BODY only if STATE has non-nil candidates.
Binds `candidates' and `index' for use in BODY."
  (declare (indent 1) (debug t))
  (let ((s (make-symbol "state")))
    `(let ((,s ,state))
       (when (and (nskk-state-p ,s)
                  (nskk-state-candidates ,s))
         (let ((candidates (nskk-state-candidates ,s))
               (index (nskk-state-current-index ,s)))
           ,@body)))))

(defmacro nskk-reset-state-fields (state &rest field-specs)
  "Reset fields in STATE according to FIELD-SPECS.
Each spec is (FIELD DEFAULT-VALUE).
Example: (nskk-reset-state-fields s (input-buffer \"\") (candidates nil))"
  (declare (indent 1) (debug t))
  `(setf ,@(cl-mapcan
            (lambda (spec)
              (list `(,(intern (format "nskk-state-%s" (car spec))) ,state)
                    (cadr spec)))
            field-specs)))

(defmacro nskk-state-slot-dispatch (state key-sym value &rest slots)
  "Generate cond dispatch for STATE struct slot setters.
KEY-SYM is a symbol being dispatched on, VALUE is the value to set.
SLOTS is a list of slot name symbols.
Each generates: ((eq KEY-SYM \\='SLOT) (setf (nskk-state-SLOT STATE) VALUE) VALUE)
Falls through to nil if no slot matches."
  (declare (indent 3) (debug t))
  `(cond
    ,@(mapcar (lambda (slot)
                `((eq ,key-sym ',slot)
                  (setf (,(intern (format "nskk-state-%s" slot)) ,state) ,value)
                  ,value))
              slots)
    (t nil)))

(declare-function nskk-debug-message "nskk-debug" (format-string &rest args))

;; State mode constants
(defconst nskk-state-modes
  '(ascii hiragana katakana katakana-半角 abbrev latin jisx0208-latin)
  "List of valid NSKK modes.")

;;;; Prolog Mode/Phase Facts
(nskk-prolog-set-index 'valid-mode 1 :hash)
(dolist (m nskk-state-modes)
  (nskk-prolog-assert (list (list 'valid-mode m))))

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

;;;; State Validation Macros

(defmacro nskk-with-state (state &rest body)
  "Execute BODY only if STATE is a valid `nskk-state' struct.
Returns nil if STATE is invalid."
  (declare (indent 1) (debug t))
  `(when (nskk-state-p ,state)
     ,@body))

;; Getter function with validation
(defun nskk-state-get (state key)
  "Get KEY value from STATE struct.
KEY can be a slot name symbol or string.
Returns nil if key is not found or state is invalid."
  (nskk-with-state state
    (let ((accessor (intern (format "nskk-state-%s" key))))
      (when (fboundp accessor)
        (funcall accessor state)))))

;; Setter function with validation
(defun nskk-state-set (state key value)
  "Set KEY to VALUE in STATE struct.
Returns VALUE on success, nil on failure.
Supports validation for mode changes."
  (nskk-with-state state
    (let ((key-sym (if (stringp key) (intern key) key)))
      (if (eq key-sym 'mode)
          ;; Mode requires validation + previous-mode tracking
          (progn
            (unless (nskk-state-valid-mode-p value)
              (error "Invalid mode: %s. Valid modes: %s" value nskk-state-modes))
            (setf (nskk-state-previous-mode state) (nskk-state-mode state))
            (setf (nskk-state-mode state) value)
            value)
        ;; All other slots: generated dispatch
        (nskk-state-slot-dispatch state key-sym value
          input-buffer converted-buffer candidates current-index
          henkan-position marker-position previous-mode
          undo-stack redo-stack henkan-phase metadata)))))

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
  (and (symbolp mode)
       (not (null (nskk-prolog-query `(valid-mode ,mode))))))

(defun nskk-state-in-henkan-mode-p (state)
  "Check if STATE is currently in conversion mode."
  (nskk-with-state state
    (memq (nskk-state-henkan-phase state) '(on active list registration))))

(defun nskk-state-henkan-on-p (state)
  "Check if STATE is in henkan-on phase (▽)."
  (nskk-with-state state
    (eq (nskk-state-henkan-phase state) 'on)))

(defun nskk-state-henkan-active-p (state)
  "Check if STATE is in henkan-active phase (▼)."
  (nskk-with-state state
    (eq (nskk-state-henkan-phase state) 'active)))

(defconst nskk-state-henkan-phases '(nil on active list registration)
  "List of valid henkan phases.")

(nskk-prolog-set-index 'valid-henkan-phase 1 :hash)
(dolist (p nskk-state-henkan-phases)
  (nskk-prolog-assert (list (list 'valid-henkan-phase p))))

;; Transition rules: any valid mode can transition to any other valid mode
(nskk-prolog-<- (can-transition \?from \?to)
  (valid-mode \?from) (valid-mode \?to))

(defun nskk-state-set-henkan-phase (state phase)
  "Set henkan PHASE in STATE.
PHASE must be nil, on, active, list, or registration."
  (nskk-with-state state
    (unless (nskk-prolog-query `(valid-henkan-phase ,phase))
      (error "Invalid henkan phase: %s. Valid phases: %s" phase nskk-state-henkan-phases))
    (setf (nskk-state-henkan-phase state) phase)))

;; State transition functions
(defun nskk-state-transition (state from-mode to-mode)
  "Transition STATE from FROM-MODE to TO-MODE.
Returns t on success, nil on failure."
  (nskk-with-state state
    (when (and (eq (nskk-state-mode state) from-mode)
               (nskk-state-valid-mode-p to-mode))
      (nskk-state-set state 'mode to-mode)
      t)))

(defun nskk-state-reset (state)
  "Reset STATE to initial state (preserves mode).
Clears buffers, candidates, and stacks."
  (nskk-with-state state
    (nskk-reset-state-fields state
      (input-buffer "")
      (converted-buffer "")
      (candidates nil)
      (current-index 0)
      (henkan-position nil)
      (marker-position nil)
      (undo-stack nil)
      (redo-stack nil)
      (henkan-phase nil)
      (metadata nil))
    t))

;; Buffer management helpers
(defun nskk-state-append-input (state char)
  "Append CHAR to STATE's input buffer.
CHAR must be a valid character (integer).
Returns the new buffer string on success, nil on failure.
Uses `concat' with a list to avoid intermediate string allocation."
  (nskk-with-state state
    (when (characterp char)
      (let ((buf (nskk-state-input-buffer state)))
        ;; Ensure buf is a string (defensive check)
        (unless (stringp buf)
          (setq buf ""))
        (let ((new-buf (concat buf (list char))))
          (setf (nskk-state-input-buffer state) new-buf)
          new-buf)))))

(defun nskk-state-delete-last-char (state)
  "Delete last character from STATE's input buffer.
Returns the deleted character on success, nil if buffer is empty
or state is invalid.
Properly handles multibyte characters."
  (nskk-with-state state
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
  (nskk-with-state state
    (setf (nskk-state-input-buffer state) "")
    t))

;; Candidate management helpers
(defun nskk-state-set-candidates (state candidates)
  "Set CANDIDATES list in STATE and reset index to 0."
  (nskk-with-state state
    (setf (nskk-state-candidates state) candidates
          (nskk-state-current-index state) 0)))

(defun nskk-state-next-candidate (state)
  "Move to next candidate in STATE.
Returns current candidate or nil if no candidates."
  (nskk-with-candidates state
    (setf (nskk-state-current-index state)
          (mod (1+ index) (length candidates)))
    (nth (nskk-state-current-index state) candidates)))

(defun nskk-state-previous-candidate (state)
  "Move to previous candidate in STATE.
Returns current candidate or nil if no candidates."
  (nskk-with-candidates state
    (setf (nskk-state-current-index state)
          (mod (1- index) (length candidates)))
    (nth (nskk-state-current-index state) candidates)))

(defun nskk-state-current-candidate (state)
  "Get current candidate from STATE."
  (nskk-with-state state
    (when (nskk-state-candidates state)
      (nth (nskk-state-current-index state)
           (nskk-state-candidates state)))))

;; Metadata helpers
(defun nskk-state-get-metadata (state key)
  "Get metadata KEY from STATE."
  (nskk-with-state state
    (plist-get (nskk-state-metadata state) key)))

(defun nskk-state-put-metadata (state key value)
  "Set metadata KEY to VALUE in STATE."
  (nskk-with-state state
    (setf (nskk-state-metadata state)
          (plist-put (nskk-state-metadata state) key value))))

;; Additional state setters for layer integration
(defun nskk-state-set-remaining-romaji (state value)
  "Set remaining romaji in STATE metadata to VALUE."
  (nskk-with-state state
    (nskk-state-put-metadata state 'remaining-romaji value)))

(defun nskk-state-set-kana-type (state value)
  "Set kana type in STATE metadata to VALUE."
  (nskk-with-state state
    (nskk-state-put-metadata state 'kana-type value)))

(defun nskk-state-set-width-type (state value)
  "Set width type in STATE metadata to VALUE."
  (nskk-with-state state
    (nskk-state-put-metadata state 'width-type value)))

(defun nskk-state-set-okurigana (state value)
  "Set okurigana in STATE metadata to VALUE.
VALUE should be the okurigana consonant (e.g., \\='k\\=' for \\='く\\=')."
  (nskk-with-state state
    (nskk-state-put-metadata state 'okurigana value)))

(defun nskk-state-get-okurigana (state)
  "Get okurigana from STATE metadata."
  (nskk-with-state state
    (nskk-state-get-metadata state 'okurigana)))

;; Buffer-local state management
(defvar-local nskk-current-state nil
  "Buffer-local NSKK state object for the current buffer.")

(defmacro nskk-with-current-state (&rest body)
  "Execute BODY with `nskk-current-state' validated.
Binds nothing extra; simply guards against nil/invalid state."
  (declare (indent 0) (debug t))
  `(when (and (boundp 'nskk-current-state)
              (nskk-state-p nskk-current-state))
     ,@body))

(defun nskk-state-initialize ()
  "Initialize NSKK state for the current buffer."
  (setq nskk-current-state (nskk-state-create nskk-state-default-mode)))

(defun nskk-state-get-mode ()
  "Get current mode from global state."
  (nskk-with-state nskk-current-state
    (nskk-state-mode nskk-current-state)))

(defun nskk-state-set-mode (mode)
  "Set MODE in global state."
  (nskk-with-state nskk-current-state
    (nskk-state-set nskk-current-state 'mode mode)))

;;;; Shared Buffer-Local State

(defvar-local nskk--romaji-buffer ""
  "Buffer for accumulating romaji input before conversion to kana.")

(defvar-local nskk--conversion-start-marker nil
  "Marker for the position where conversion (henkan) input started.
Set when the user types an uppercase letter in Japanese mode to begin
composing a word for conversion.  This replaces the use of `mark' for
SKK conversion tracking, so that SKK does not interfere with the
Emacs mark ring.")

(defvar-local nskk--conversion-overlay nil
  "Overlay for displaying converted text.")

(defvar-local nskk--henkan-count 0
  "Number of times SPC has been pressed during current conversion.")

(defvar-local nskk--registration-depth 0
  "Current nesting depth of dictionary registration.")

(provide 'nskk-state)

;;; nskk-state.el ends here
