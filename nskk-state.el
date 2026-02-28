;;; nskk-state.el --- NSKK state management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
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
;;
;; Prolog predicates provided by this module:
;; - `valid-mode/1': validates mode symbols against `nskk-state-modes'
;; - `japanese-mode/1': identifies modes where Japanese input is active
;; - `can-transition/2': validates any mode-to-mode transition
;; - `valid-henkan-transition/2': validates henkan phase transitions
;; - `henkan-mode-phase/1': classifies phases as active henkan phases
;; - `state-slot-default/2': default values for state struct slots
;; - `resettable-field/1': fields cleared by `nskk-state-reset'
;; - `metadata-key/1': valid metadata key symbols

;;; Code:

(require 'cl-lib)
(require 'nskk-prolog)
(defgroup nskk-state nil
  "State management settings."
  :prefix "nskk-state-"
  :group 'nskk)

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

;;;; Japanese Input Mode Classification
(nskk-prolog-set-index 'japanese-mode 1 :hash)
(nskk-prolog-<- (japanese-mode hiragana))
(nskk-prolog-<- (japanese-mode katakana))
(nskk-prolog-<- (japanese-mode katakana-半角))

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
Supports validation for mode and henkan-phase changes."
  (nskk-with-state state
    (let ((key-sym (if (stringp key) (intern key) key)))
      (cond
       ((eq key-sym 'mode)
        ;; Mode requires validation + previous-mode tracking
        (unless (nskk-state-valid-mode-p value)
          (error "Invalid mode: %s. Valid modes: %s" value nskk-state-modes))
        (setf (nskk-state-previous-mode state) (nskk-state-mode state))
        (setf (nskk-state-mode state) value)
        value)
       ((eq key-sym 'henkan-phase)
        ;; Henkan phase requires transition validation
        (nskk-state-set-henkan-phase state value)
        value)
       (t
        ;; All other slots: generated dispatch
        (nskk-state-slot-dispatch state key-sym value
          input-buffer converted-buffer candidates current-index
          henkan-position marker-position previous-mode
          undo-stack redo-stack metadata))))))

;; Internal helper to get default value from Prolog
(defun nskk-state--get-default (slot)
  "Get default value for SLOT from state-slot-default Prolog facts."
  (nskk-prolog-query-value `(state-slot-default ,slot ,'\?v) '\?v))

;; State creation functions
(defun nskk-state-create (&optional initial-mode)
  "Create a new NSKK state object.
INITIAL-MODE defaults to `nskk-state-default-mode' if not specified.
Slot defaults are sourced from state-slot-default Prolog facts."
  (let ((mode (or initial-mode
                   nskk-state-default-mode
                   'ascii)))
    (unless (nskk-state-valid-mode-p mode)
      (setq mode 'ascii))
    (make-nskk-state
     :mode mode
     :input-buffer (nskk-state--get-default 'input-buffer)
     :converted-buffer (nskk-state--get-default 'converted-buffer)
     :candidates (nskk-state--get-default 'candidates)
     :current-index (nskk-state--get-default 'current-index)
     :henkan-position (nskk-state--get-default 'henkan-position)
     :marker-position (nskk-state--get-default 'marker-position)
     :previous-mode mode
     :undo-stack (nskk-state--get-default 'undo-stack)
     :redo-stack (nskk-state--get-default 'redo-stack)
     :henkan-phase (nskk-state--get-default 'henkan-phase)
     :metadata (nskk-state--get-default 'metadata))))

;; State validation functions
(defun nskk-state-valid-mode-p (mode)
  "Check if MODE is a valid NSKK mode."
  (and (symbolp mode)
       (not (null (nskk-prolog-query `(valid-mode ,mode))))))

(defun nskk-state-in-henkan-mode-p (state)
  "Check if STATE is currently in conversion mode.
Queries the henkan-mode-phase Prolog predicate.
Uses `nskk-prolog-query-one' for early termination.
Note: nil phase (the common case) short-circuits before the query."
  (nskk-with-state state
    (let ((phase (nskk-state-henkan-phase state)))
      (and phase
           (not (null (nskk-prolog-query-one `(henkan-mode-phase ,phase))))))))

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

;;;; Henkan Phase Transition Graph
(nskk-prolog-set-index 'valid-henkan-transition 2 :hash)
(nskk-prolog-<- (valid-henkan-transition nil on))
(nskk-prolog-<- (valid-henkan-transition on active))
(nskk-prolog-<- (valid-henkan-transition on registration))
(nskk-prolog-<- (valid-henkan-transition on nil))
(nskk-prolog-<- (valid-henkan-transition active nil))
(nskk-prolog-<- (valid-henkan-transition active list))
(nskk-prolog-<- (valid-henkan-transition list nil))
(nskk-prolog-<- (valid-henkan-transition list registration))
(nskk-prolog-<- (valid-henkan-transition registration nil))
(nskk-prolog-<- (valid-henkan-transition registration list))

;;;; Henkan Mode Phase Classification
(nskk-prolog-set-index 'henkan-mode-phase 1 :hash)
(nskk-prolog-<- (henkan-mode-phase on))
(nskk-prolog-<- (henkan-mode-phase active))
(nskk-prolog-<- (henkan-mode-phase list))
(nskk-prolog-<- (henkan-mode-phase registration))

;;;; State Slot Defaults
(nskk-prolog-set-index 'state-slot-default 2 :hash)
(nskk-prolog-<- (state-slot-default input-buffer ""))
(nskk-prolog-<- (state-slot-default converted-buffer ""))
(nskk-prolog-<- (state-slot-default candidates nil))
(nskk-prolog-<- (state-slot-default current-index 0))
(nskk-prolog-<- (state-slot-default henkan-position nil))
(nskk-prolog-<- (state-slot-default marker-position nil))
(nskk-prolog-<- (state-slot-default undo-stack nil))
(nskk-prolog-<- (state-slot-default redo-stack nil))
(nskk-prolog-<- (state-slot-default henkan-phase nil))
(nskk-prolog-<- (state-slot-default metadata nil))

;;;; Resettable Fields
(nskk-prolog-set-index 'resettable-field 1 :hash)
(nskk-prolog-<- (resettable-field input-buffer))
(nskk-prolog-<- (resettable-field converted-buffer))
(nskk-prolog-<- (resettable-field candidates))
(nskk-prolog-<- (resettable-field current-index))
(nskk-prolog-<- (resettable-field henkan-position))
(nskk-prolog-<- (resettable-field marker-position))
(nskk-prolog-<- (resettable-field undo-stack))
(nskk-prolog-<- (resettable-field redo-stack))
(nskk-prolog-<- (resettable-field henkan-phase))
(nskk-prolog-<- (resettable-field metadata))

;;;; Metadata Key Registry
(nskk-prolog-set-index 'metadata-key 1 :hash)
(nskk-prolog-<- (metadata-key remaining-romaji))
(nskk-prolog-<- (metadata-key kana-type))
(nskk-prolog-<- (metadata-key width-type))
(nskk-prolog-<- (metadata-key okurigana))

(defun nskk-state-set-henkan-phase (state phase)
  "Set henkan PHASE in STATE with transition validation.
PHASE must be nil, on, active, list, or registration.
Validates that the transition from the current phase to PHASE
follows the valid-henkan-transition graph.
Same-phase transitions (no-op) are always allowed."
  (nskk-with-state state
    (unless (nskk-prolog-query `(valid-henkan-phase ,phase))
      (error "Invalid henkan phase: %s. Valid phases: %s" phase nskk-state-henkan-phases))
    (let ((current (nskk-state-henkan-phase state)))
      (unless (or (eq current phase)
                  (nskk-prolog-query `(valid-henkan-transition ,current ,phase)))
        (error "Invalid henkan phase transition: %s -> %s" current phase)))
    (setf (nskk-state-henkan-phase state) phase)))

(defun nskk-state-force-henkan-phase (state phase)
  "Force set henkan PHASE in STATE, bypassing transition validation.
Only validates that PHASE is a valid henkan phase.
Use for test setup or emergency reset."
  (nskk-with-state state
    (unless (nskk-prolog-query `(valid-henkan-phase ,phase))
      (error "Invalid henkan phase: %s. Valid phases: %s" phase nskk-state-henkan-phases))
    (setf (nskk-state-henkan-phase state) phase)))

;; State transition functions
(defun nskk-state-transition (state from-mode to-mode)
  "Transition STATE from FROM-MODE to TO-MODE.
Uses can-transition Prolog rule for validation.
Returns t on success, nil on failure."
  (nskk-with-state state
    (when (and (eq (nskk-state-mode state) from-mode)
               (nskk-prolog-query `(can-transition ,from-mode ,to-mode)))
      (nskk-state-set state 'mode to-mode)
      t)))

(defun nskk-state-reset (state)
  "Reset STATE to initial state (preserves mode).
Iterates resettable-field Prolog facts and restores each
to its state-slot-default value.
Returns nil (side-effect function)."
  (nskk-with-state state
    (dolist (slot (nskk-prolog-query-all-values '(resettable-field \?s) '\?s))
      (let ((default (nskk-state--get-default slot)))
        (if (eq slot 'henkan-phase)
            (nskk-state-force-henkan-phase state default)
          (nskk-state-set state slot default))))
    nil))

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
