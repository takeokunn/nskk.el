;;; nskk-state.el --- NSKK state management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Version: 0.1.0
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

;; Buffer-local state management for NSKK (Layer 2: Domain).
;;
;; Layer position: L2 (Domain) -- depends on nskk-prolog and nskk-custom.
;;
;; Manages the per-buffer `nskk-state' struct and the Prolog facts that
;; encode valid modes, valid transitions, and struct slot defaults.
;;
;; The central `nskk-state' struct (cl-defstruct) tracks:
;; - Current input mode: hiragana, katakana, ascii, latin, abbrev,
;;   jisx0208-latin, katakana-半角
;; - Henkan (conversion) phase: nil, on (▽), active (▼), list, registration
;; - Candidate list and selection index
;; - Input and converted text buffers
;; - Conversion start position (henkan-position)
;; - Okurigana consonant (stored as metadata)
;; - Undo/redo stacks
;;
;; State is stored buffer-locally in `nskk-current-state' and mutated
;; exclusively through the setter functions defined in this module.
;; The `nskk-state-set' dispatcher routes mode and henkan-phase changes
;; through validated setters; all other slots are dispatched via the
;; `nskk-state-slot-dispatch' macro.
;;
;; Prolog predicates provided by this module:
;; - `mode-properties/5'         -- unified mode descriptor
;;     (mode display-string face help-text cursor-face)
;;     Single source of truth for all mode display data.
;; - `valid-mode/1'              -- derived rule: succeeds iff mode has
;;     a mode-properties/5 fact; used as a guard in transition rules.
;; - `japanese-mode/1'           -- hiragana, katakana, katakana-半角
;; - `can-transition/2'          -- valid-mode/1 to valid-mode/1
;; - `valid-henkan-phase/1'      -- membership: nil, on, active, list, registration
;; - `valid-henkan-transition/2' -- (from to) transition graph
;; - `henkan-mode-phase/1'       -- active henkan phases (on, active, list, registration)
;; - `preedit-phase/1'           -- preedit (non-converting) phases
;; - `registration-phase/1'      -- dict-registration phases
;; - `state-slot-default/2'      -- (slot default-value) for struct creation
;; - `resettable-field/1'        -- fields reset by `nskk-state-reset'
;;
;; Key public API:
;; - `nskk-state-create'           -- create a new state struct
;; - `nskk-state-get' / `nskk-state-set' -- generic accessor/mutator
;; - `nskk-state-valid-mode-p'     -- validate a mode symbol
;; - `nskk-state-set-henkan-phase' -- validated phase transition
;; - `nskk-state-force-henkan-phase' -- unvalidated (test/emergency use)
;; - `nskk-state-in-henkan-mode-p' -- test for active conversion
;; - `nskk-state-henkan-on-p'      -- test for ▽ phase
;; - `nskk-state-henkan-active-p'  -- test for ▼ phase
;; - `nskk-state-transition'       -- validated mode switch
;; - `nskk-state-reset'            -- reset to initial values (preserves mode)
;; - `nskk-state-append-input'     -- append char to input buffer
;; - `nskk-state-delete-last-char' -- delete last char from input buffer
;; - `nskk-state-next-candidate' / `nskk-state-previous-candidate'
;;
;; CPS variants (suffix /k) — pass result to continuation; never return a value:
;; - `nskk-state-append-input/k'        -- on-done(new-buf) / on-fail()
;; - `nskk-state-delete-last-char/k'    -- on-deleted(char) / on-empty()
;; - `nskk-state-transition/k'          -- on-found(t) / on-not-found()
;; - `nskk-state-set-henkan-phase/k'    -- on-found(t) / on-not-found()
;; - `nskk-state-next-candidate/k'      -- on-candidate(cand) / on-empty()
;; - `nskk-state-previous-candidate/k'  -- on-candidate(cand) / on-empty()

;;; Code:

(require 'cl-lib)
(require 'nskk-prolog)
(require 'nskk-custom)
(require 'nskk-cps-macros)

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
Each slot generates:
  ((eq KEY-SYM \\='SLOT) (setf (nskk-state-SLOT STATE) VALUE) VALUE)
Falls through to nil if no slot matches."
  (declare (indent 3) (debug t))
  (let ((state-sym (make-symbol "state"))
        (val-sym   (make-symbol "val")))
    `(let ((,state-sym ,state)
           (,val-sym   ,value))
       (cond
        ,@(mapcar (lambda (slot)
                    `((eq ,key-sym ',slot)
                      (setf (,(intern (format "nskk-state-%s" slot)) ,state-sym) ,val-sym)
                      ,val-sym))
                  slots)
        (t nil)))))

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

;;;; State Validation Macros

(defmacro nskk-with-state (state &rest body)
  "Execute BODY only if STATE is a valid `nskk-state' struct.
Returns nil if STATE is invalid."
  (declare (indent 1) (debug t))
  `(when (nskk-state-p ,state)
     ,@body))

;; Getter function with validation
;;
;; NOTE on nskk-state-get CPS semantics:
;; Many slots (henkan-position, candidates, metadata, marker-position, etc.)
;; have nil as a legitimate value.  Using (if v (succeed v) (fail)) would
;; incorrectly call the not-found continuation for valid nil slot values.
;; Therefore nskk-state-get/k always calls on-found when the accessor exists,
;; even when the slot value is nil.  It calls on-not-found only when STATE is
;; invalid or KEY has no corresponding accessor (fboundp check fails).
(defun/k nskk-state-get (state key)
  "Return the value of KEY slot from STATE struct.
KEY can be a slot name symbol or string.
Returns nil if KEY is not found or STATE is invalid."
  (if (nskk-state-p state)
      (let ((accessor (intern (format "nskk-state-%s" key))))
        (if (fboundp accessor)
            (succeed (funcall accessor state))
          (fail)))
    (fail)))

;; Setter function with validation
(defun/k nskk-state-set (state key value)
  "Set KEY to VALUE in STATE struct, returning VALUE on success.
Returns nil when STATE is invalid or KEY is not a known slot.
Applies validation for mode and henkan-phase changes."
  (let ((result (nskk-with-state state
                  (let ((key-sym (if (stringp key) (intern key) key)))
                    (cond
                     ((eq key-sym 'mode)
                      (unless (nskk-state-valid-mode-p value)
                        (error "Invalid mode: %s. Valid modes: %s" value nskk-state-modes))
                      (setf (nskk-state-previous-mode state) (nskk-state-mode state))
                      (setf (nskk-state-mode state) value)
                      value)
                     ((eq key-sym 'henkan-phase)
                      (nskk-state-set-henkan-phase state value)
                      value)
                     (t
                      (nskk-state-slot-dispatch state key-sym value
                        input-buffer converted-buffer candidates current-index
                        henkan-position marker-position previous-mode
                        undo-stack redo-stack metadata)))))))
    (if result (succeed result) (fail))))

;; Internal helper to get default value from Prolog
(defun nskk--state-get-default (slot)
  "Get default value for SLOT from state-slot-default Prolog facts."
  (nskk-prolog-query-value `(state-slot-default ,slot ,'\?v) '\?v))

;; State creation functions
(defun/k nskk-state-create (&optional initial-mode)
  "Create a new NSKK state object.
INITIAL-MODE defaults to `nskk-state-default-mode' if not specified.
Slot defaults are sourced from state-slot-default Prolog facts.

NOTE: The generated `nskk-state-create/k' variant places ON-FOUND and
ON-NOT-FOUND after the &optional INITIAL-MODE parameter.  Callers MUST
always pass both continuation arguments explicitly."
  (let ((mode (or initial-mode
                   nskk-state-default-mode
                   'ascii)))
    (unless (nskk-state-valid-mode-p mode)
      (setq mode 'ascii))
    (succeed
     (make-nskk-state
      :mode mode
      :input-buffer (nskk--state-get-default 'input-buffer)
      :converted-buffer (nskk--state-get-default 'converted-buffer)
      :candidates (nskk--state-get-default 'candidates)
      :current-index (nskk--state-get-default 'current-index)
      :henkan-position (nskk--state-get-default 'henkan-position)
      :marker-position (nskk--state-get-default 'marker-position)
      :previous-mode mode
      :undo-stack (nskk--state-get-default 'undo-stack)
      :redo-stack (nskk--state-get-default 'redo-stack)
      :henkan-phase (nskk--state-get-default 'henkan-phase)
      :metadata (nskk--state-get-default 'metadata)))))

;; State validation functions
(defun/k nskk-state-valid-mode-p (mode)
  "Return non-nil if MODE is a valid NSKK input mode symbol."
  (if (and (symbolp mode)
           (nskk-prolog-holds-p `(valid-mode ,mode)))
      (succeed t)
    (fail)))

(defun/k nskk-state-in-henkan-mode-p (state)
  "Check if STATE is currently in conversion mode.
Queries the henkan-mode-phase Prolog predicate.
Note: nil phase (the common case) short-circuits before the Prolog query."
  (if (and (nskk-state-p state)
           (let ((phase (nskk-state-henkan-phase state)))
             (and phase
                  (nskk-prolog-holds-p `(henkan-mode-phase ,phase)))))
      (succeed t)
    (fail)))

(defun/k nskk-state-henkan-on-p (state)
  "Check if STATE is in henkan-on phase (▽)."
  (if (and (nskk-state-p state)
           (eq (nskk-state-henkan-phase state) 'on))
      (succeed t)
    (fail)))

(defun/k nskk-state-henkan-active-p (state)
  "Check if STATE is in henkan-active phase (▼)."
  (if (and (nskk-state-p state)
           (eq (nskk-state-henkan-phase state) 'active))
      (succeed t)
    (fail)))

(defconst nskk-state-henkan-phases '(nil on active list registration)
  "List of valid henkan phases.")

(defun/k nskk-state-set-henkan-phase (state phase)
  "Set henkan PHASE in STATE with transition validation.
PHASE must be nil, on, active, list, or registration.
Validates that the transition from the current phase to PHASE
follows the valid-henkan-transition graph.
Same-phase transitions (no-op) are always allowed.
Calls on-found with t on success; on-not-found on invalid transition."
  (let ((ok (condition-case _err
                 (progn
                   (nskk-with-state state
                     (unless (nskk-prolog-holds-p `(valid-henkan-phase ,phase))
                       (error "Invalid henkan phase: %s. Valid phases: %s"
                              phase nskk-state-henkan-phases))
                     (let ((current (nskk-state-henkan-phase state)))
                       (unless (or (eq current phase)
                                   (nskk-prolog-holds-p `(valid-henkan-transition ,current ,phase)))
                         (error "Invalid henkan phase transition: %s -> %s" current phase)))
                     (setf (nskk-state-henkan-phase state) phase))
                   t)
               (error nil))))
    (if ok (succeed t) (fail))))


;; Override the defun/k-generated sync wrapper to re-raise validation errors.
;; The /k body converts errors to (fail); the sync wrapper must re-signal them.
(with-no-warnings
(defun nskk-state-set-henkan-phase (state phase)
  "Set henkan PHASE in STATE with transition validation.  [CPS override]
PHASE must be nil, on, active, list, or registration.
Validates the transition; signals an error for invalid transitions.
Overrides the defun/k-generated sync wrapper to re-raise validation errors
rather than silently returning nil."
  (unless (nskk-state-p state)
    (error "nskk-state-set-henkan-phase: STATE must be an nskk-state, got %S" state))
  (let ((current (nskk-state-henkan-phase state)))
    (nskk-state-set-henkan-phase/k
     state phase
     #'ignore
     (lambda ()
       (if (not (nskk-prolog-holds-p `(valid-henkan-phase ,phase)))
           (error "Invalid henkan phase: %s. Valid phases: %s"
                  phase nskk-state-henkan-phases)
         (error "Invalid henkan phase transition: %s -> %s" current phase)))))))

(defun/done nskk-state-force-henkan-phase (state phase)
  "Force set henkan PHASE in STATE, bypassing transition validation.
Only validates that PHASE is a valid henkan phase.
Use for test setup or emergency reset."
  (nskk-with-state state
    (unless (nskk-prolog-holds-p `(valid-henkan-phase ,phase))
      (error "Invalid henkan phase: %s. Valid phases: %s" phase nskk-state-henkan-phases))
    (setf (nskk-state-henkan-phase state) phase)))

;; State transition functions
(defun/k nskk-state-transition (state from-mode to-mode)
  "Transition STATE from FROM-MODE to TO-MODE.
Uses can-transition Prolog rule for validation.
Calls on-found with t on success; on-not-found on failure."
  (let ((ok (nskk-with-state state
               (when (and (eq (nskk-state-mode state) from-mode)
                          (nskk-prolog-holds-p `(can-transition ,from-mode ,to-mode)))
                 (nskk-state-set state 'mode to-mode)
                 t))))
    (if ok (succeed t) (fail))))

(defun/done nskk-state-reset (state)
  "Reset STATE to initial state (preserves mode).
Iterates resettable-field Prolog facts and restores each
to its state-slot-default value.
Returns nil (side-effect function)."
  (nskk-with-state state
    (dolist (slot (nskk-prolog-query-all-values '(resettable-field \?s) '\?s))
      (let ((default (nskk--state-get-default slot)))
        (if (eq slot 'henkan-phase)
            (nskk-state-force-henkan-phase state default)
          (nskk-state-set state slot default))))))

;; Buffer management helpers
(defun/k nskk-state-append-input (state char)
  "Append CHAR to STATE's input buffer.
CHAR must be a valid character (integer).
Returns the new buffer string on success, nil on failure.
Uses `concat' with a list to avoid intermediate string allocation."
  (if (and (nskk-state-p state) (characterp char))
      (let ((buf (nskk-state-input-buffer state)))
        (unless (stringp buf)
          (setq buf ""))
        (let ((new-buf (concat buf (list char))))
          (setf (nskk-state-input-buffer state) new-buf)
          (succeed new-buf)))
    (fail)))

(defun/k nskk-state-delete-last-char (state)
  "Delete last character from STATE's input buffer.
Returns the deleted character on success, nil if buffer is empty
or state is invalid.
Properly handles multibyte characters."
  (if (nskk-state-p state)
      (let ((buf (nskk-state-input-buffer state)))
        (if (and (stringp buf) (> (length buf) 0))
            (let ((last-char (aref buf (1- (length buf))))
                  (new-buf (substring buf 0 (1- (length buf)))))
              (setf (nskk-state-input-buffer state) new-buf)
              (succeed last-char))
          (fail)))
    (fail)))

(defun/done nskk-state-clear-input (state)
  "Clear STATE's input buffer.
Returns t on success, nil if state is invalid."
  (nskk-with-state state
    (setf (nskk-state-input-buffer state) "")))

;; Candidate management helpers
(defun/done nskk-state-set-candidates (state candidates)
  "Set CANDIDATES list in STATE and reset the current index to 0."
  (nskk-with-state state
    (setf (nskk-state-candidates state) candidates
          (nskk-state-current-index state) 0)))

(defun/k nskk-state-next-candidate (state)
  "Move to next candidate in STATE.
Returns current candidate or nil if no candidates."
  (if (and (nskk-state-p state)
           (nskk-state-candidates state))
      (let* ((candidates (nskk-state-candidates state))
             (index (nskk-state-current-index state)))
        (setf (nskk-state-current-index state)
              (mod (1+ index) (length candidates)))
        (succeed (nth (nskk-state-current-index state) candidates)))
    (fail)))

(defun/k nskk-state-previous-candidate (state)
  "Move to previous candidate in STATE.
Returns current candidate or nil if no candidates."
  (if (and (nskk-state-p state)
           (nskk-state-candidates state))
      (let* ((candidates (nskk-state-candidates state))
             (index (nskk-state-current-index state)))
        (setf (nskk-state-current-index state)
              (mod (1- index) (length candidates)))
        (succeed (nth (nskk-state-current-index state) candidates)))
    (fail)))

(defun/k nskk-state-current-candidate (state)
  "Return the currently selected candidate from STATE, or nil if none."
  (if (and (nskk-state-p state)
           (nskk-state-candidates state))
      (succeed (nth (nskk-state-current-index state)
                    (nskk-state-candidates state)))
    (fail)))

;; Metadata helpers
(defmacro nskk-define-metadata-setter (field &optional docstring)
  "Define `nskk-state-set-FIELD' as a metadata setter for FIELD.
FIELD is a symbol naming the metadata key and the suffix of the
generated function name.
Optional DOCSTRING overrides the default documentation string.
Generated function signature: (nskk-state-set-FIELD state value)"
  (declare (indent 1) (debug t))
  (let* ((fname (intern (format "nskk-state-set-%s" field)))
         (doc (or docstring
                  (format "Set %s in STATE metadata to VALUE." field))))
    `(defun ,fname (state value)
       ,doc
       (nskk-with-state state
         (nskk-state-put-metadata state ',field value)))))

(defun/k nskk-state-get-metadata (state key)
  "Return the value of metadata KEY from STATE's metadata plist."
  (if (nskk-state-p state)
      (succeed (plist-get (nskk-state-metadata state) key))
    (fail)))

(defun/done nskk-state-put-metadata (state key value)
  "Set metadata KEY to VALUE in STATE's metadata plist."
  (nskk-with-state state
    (setf (nskk-state-metadata state)
          (plist-put (nskk-state-metadata state) key value))))

;; Additional state setters for layer integration
(nskk-define-metadata-setter remaining-romaji)
(nskk-define-metadata-setter kana-type)
(nskk-define-metadata-setter width-type)
(nskk-define-metadata-setter okurigana
  "Set okurigana in STATE metadata to VALUE.
VALUE should be the okurigana consonant (e.g., \\='k\\=' for \\='く\\=').")

(defun/k nskk-state-get-okurigana (state)
  "Return the okurigana consonant from STATE's metadata, or nil if unset."
  (if (nskk-state-p state)
      (succeed (nskk-state-get-metadata state 'okurigana))
    (fail)))

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

(defun/k nskk-state-get-mode ()
  "Return the current NSKK input mode symbol from `nskk-current-state'.
Returns nil when no state is active in the current buffer."
  (if (and (boundp 'nskk-current-state)
           (nskk-state-p nskk-current-state))
      (succeed (nskk-state-mode nskk-current-state))
    (fail)))

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

(defvar-local nskk--pending-romaji-overlay nil
  "Overlay for displaying pending/incomplete romaji input.
Unlike `nskk--conversion-overlay' which uses the \\='display property on a
real buffer range, this overlay uses \\='after-string on a zero-length overlay
at point -- no buffer text exists yet for the incomplete romaji sequence.")

(defvar-local nskk--candidate-overlay nil
  "Overlay for candidate list display (Phase 2: list selection mode).
Zero-length overlay anchored at the end of the conversion overlay.
Managed by `nskk-candidate-window.el'; declared here following the
project convention that all buffer-local overlay variables live in
nskk-state.el alongside `nskk--conversion-overlay' and
`nskk--pending-romaji-overlay'.")

;;;; Overlay and Marker Management Macros

(defmacro nskk-ensure-overlay (var start end &rest props)
  "Move or create an overlay for VAR covering START to END in the current buffer.
Reuses VAR's existing overlay object if it satisfies `overlayp', creating
a new one otherwise.  In both cases the overlay is moved to START..END and
any PROPS (a plist of property value pairs) are applied via `overlay-put'.
VAR is mutated via `setq' when a new overlay is created."
  (declare (indent 2) (debug t))
  `(progn
     (if (overlayp ,var)
         (move-overlay ,var ,start ,end (current-buffer))
       (setq ,var (make-overlay ,start ,end)))
     (cl-loop for (prop val) on (list ,@props) by #'cddr
              do (overlay-put ,var prop val))))

(defmacro nskk-delete-overlay (var)
  "Delete the overlay in VAR and set VAR to nil.
Safe to call when VAR is nil or not an overlay (idempotent)."
  (declare (indent 0) (debug t))
  `(when (overlayp ,var)
     (delete-overlay ,var)
     (setq ,var nil)))

(defmacro nskk-ensure-marker (var pos)
  "Move or create a marker for VAR positioned at POS in the current buffer.
Creates a new marker and assigns it to VAR via `setq' if VAR is not
already a marker.  In both cases the marker is moved to POS unconditionally."
  (declare (indent 0) (debug t))
  `(progn
     (unless (markerp ,var)
       (setq ,var (make-marker)))
     (set-marker ,var ,pos (current-buffer))))

(defvar-local nskk--henkan-count 0
  "Number of times SPC has been pressed during current conversion.")

(defvar-local nskk--registration-depth 0
  "Current nesting depth of dictionary registration.")

(defvar nskk--state-prolog-initialized nil
  "Non-nil when state machine Prolog predicates have been initialized.")

(defun/done nskk-state-initialize-prolog ()
  "Initialize NSKK state machine Prolog predicates (idempotent).
Subsequent calls are no-ops guarded by `nskk--state-prolog-initialized'."
  (unless nskk--state-prolog-initialized
    ;; Unified mode properties
    ;; mode-properties/5: (MODE DISPLAY-STRING FACE HELP-TEXT CURSOR-FACE)
    ;; NOTE: Faces (nskk-modeline-*-face) are defined in nskk-modeline.el, which
    ;; is loaded after nskk-state.el.  Prolog facts store the face symbols as
    ;; data—they are not evaluated at assertion time, so forward references are safe.
    ;; Similarly, cursor face symbols are stored as data and dereferenced via
    ;; `face-attribute' at runtime (see `nskk--cursor-with-color' in nskk-modeline.el).
    (nskk-prolog-define-fact-table mode-properties (:arity 5 :index :hash)
      (hiragana "かな" nskk-modeline-hiragana-face
                "Hiragana input mode" nskk-cursor-hiragana)
      (katakana "カナ" nskk-modeline-katakana-face
                "Katakana input mode" nskk-cursor-katakana)
      (katakana-半角 "ｶﾅ" nskk-modeline-katakana-face
                   "Half-width katakana input mode" nskk-cursor-katakana)
      (abbrev "aA" nskk-modeline-abbrev-face
              "Abbreviation mode" nskk-cursor-abbrev)
      (ascii "SKK" nskk-modeline-direct-face
             "Direct/ASCII input mode" nskk-cursor-latin)
      (latin "SKK" nskk-modeline-direct-face
             "Direct/ASCII input mode" nskk-cursor-latin)
      (jisx0208-latin "全英" nskk-modeline-jisx0208-latin-face
                      "Full-width latin input mode" nskk-cursor-jisx0208-latin))

    ;; valid-mode/1: derived rule — a symbol is a valid mode iff it has a mode-properties fact.
    ;; Uses distinct variable names (?disp, ?face, ?help, ?cur) to avoid
    ;; spurious unification failures that occur with repeated ?_ names.
    (nskk-prolog-<- (valid-mode \?m)
      (mode-properties \?m \?disp \?face \?help \?cur))

    ;; Japanese input mode classification
    (nskk-prolog-define-fact-table japanese-mode (:arity 1 :index :hash)
      (hiragana)
      (katakana)
      (katakana-半角))

    ;; Valid henkan phases (derived from nskk-state-henkan-phases defconst)
    (nskk-prolog-define-fact-table valid-henkan-phase (:arity 1 :index :hash)
      (nil)
      (on)
      (active)
      (list)
      (registration))

    ;; Transition rules: any valid mode can transition to any other valid mode
    (nskk-prolog-<- (can-transition \?from \?to)
      (valid-mode \?from) (valid-mode \?to))

    ;; Henkan phase transition graph
    (nskk-prolog-define-fact-table valid-henkan-transition (:arity 2 :index :hash)
      (nil on)
      (on active)
      (on registration)
      (on nil)
      (active on)
      (active nil)
      (active list)
      (list on)
      (list nil)
      (list registration)
      (registration nil)
      (registration list))

    ;; Henkan mode phase classification
    (nskk-prolog-define-fact-table henkan-mode-phase (:arity 1 :index :hash)
      (on)
      (active)
      (list)
      (registration))

    ;; Preedit phase classification
    ;; Phases in which the user is building preedit text (▽ marker visible)
    ;; but no dictionary search has started yet.
    (nskk-prolog-define-fact-table preedit-phase (:arity 1 :index :hash)
      (normal)
      (preedit))

    ;; Registration phase classification
    ;; Phases related to dictionary registration (active nesting).
    (nskk-prolog-define-fact-table registration-phase (:arity 1 :index :hash)
      (active)
      (list))

    ;; State slot defaults
    (nskk-prolog-define-fact-table state-slot-default (:arity 2 :index :hash)
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

    ;; Resettable fields: derived rule — any slot with a default is resettable.
    ;; Eliminates the need to list the same 10 slots twice.
    (nskk-prolog-<- (resettable-field \?slot)
      (state-slot-default \?slot \?_))

    (setq nskk--state-prolog-initialized t)))

(provide 'nskk-state)

;;; nskk-state.el ends here
