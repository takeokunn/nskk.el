;;; nskk-state.el --- NSKK state management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n convenience

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

;; NSKK state management.

;;; Code:

(require 'cl-lib)
(require 'nskk-prolog)
(require 'nskk-cps-macros)

;;;; Customization

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
  :safe (lambda (v) (memq v '(ascii hiragana katakana jisx0208-latin)))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-state)

(defface nskk-cursor-hiragana
  '((((background dark)) (:background "coral4"))
    (t (:background "pink")))
  "Cursor color face for hiragana mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-ui)

(defface nskk-cursor-katakana
  '((((background dark)) (:background "forestgreen"))
    (t (:background "green")))
  "Cursor color face for katakana mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-ui)

(defface nskk-cursor-latin
  '((((background dark)) (:background "ivory4"))
    (t (:background "gray")))
  "Cursor color face for ASCII/latin mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-ui)

(defface nskk-cursor-jisx0208-latin
  '((t (:background "gold")))
  "Cursor color face for full-width latin mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-ui)

(defface nskk-cursor-abbrev
  '((t (:background "royalblue")))
  "Cursor color face for abbrev mode.
The :background attribute is used as the cursor color via `face-attribute'."
  :package-version '(nskk . "0.1.0")
  :group 'nskk-ui)

;; Main state structure
(cl-defstruct nskk-state
  "Core state structure for NSKK input."
  mode              ;; Current mode (symbol; validated by `nskk-state-valid-mode-p')
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

;;;; Mode Validation
;;
;; `nskk--valid-modes' mirrors the mode-properties/5 fact table defined in
;; `nskk-state-initialize-prolog'.  Both must stay in sync; the unit tests
;; assert the two agree in both directions.  Prolog facts remain
;; authoritative for cross-module queries, and this is a read-only cache
;; that keeps mode validation off the Prolog engine.

(defconst nskk--valid-modes
  '(hiragana katakana katakana-半角 abbrev ascii latin jisx0208-latin)
  "Mirror of the mode symbols carried by the mode-properties/5 fact table.
Must stay in sync with the facts asserted in `nskk-state-initialize-prolog'.")

(defsubst nskk-state-valid-mode-p (mode)
  "Return non-nil if MODE is a valid NSKK input mode symbol."
  (memq mode nskk--valid-modes))

;;;; Mode Setter

(defun nskk-state-set-mode (state value)
  "Set mode slot in STATE to VALUE, recording the outgoing mode.
Returns VALUE on success, nil when STATE is not an `nskk-state'.
Signals an error when VALUE is not a valid mode symbol."
  (when (nskk-state-p state)
    (unless (nskk-state-valid-mode-p value)
      (error "Invalid mode: %s" value))
    (setf (nskk-state-previous-mode state) (nskk-state-mode state)
          (nskk-state-mode state) value)
    value))

;;;; State Creation

(defun nskk-state-create (&optional initial-mode)
  "Create a new NSKK state object.
INITIAL-MODE defaults to `nskk-state-default-mode', falling back to
\\='ascii when neither names a valid mode."
  (nskk-state-initialize-prolog)
  (let ((mode (let ((m (or initial-mode nskk-state-default-mode 'ascii)))
                (if (nskk-state-valid-mode-p m) m 'ascii))))
    (make-nskk-state
     :mode             mode
     :input-buffer     ""
     :converted-buffer ""
     :candidates       nil
     :current-index    0
     :henkan-position  nil
     :marker-position  nil
     :previous-mode    mode
     :undo-stack       nil
     :redo-stack       nil
     :henkan-phase     nil
     :metadata         nil)))

;;;; Henkan Phase
;;
;; These sets are the sole definition of the phase machine.  Phase
;; validation runs on every conversion keypress, so it stays in Elisp
;; rather than going through the Prolog engine.

(defconst nskk--valid-henkan-phases
  '(nil on active list registration)
  "Henkan phases `nskk-state-set-henkan-phase' accepts.")

(defconst nskk--valid-henkan-transitions
  '((nil      . on)
    (on       . active)
    (on       . registration)
    (on       . nil)
    (active   . on)
    (active   . nil)
    (active   . list)
    (list     . on)
    (list     . nil)
    (list     . registration)
    (registration . nil)
    (registration . list))
  "Permitted henkan phase transitions as (FROM . TO) pairs.")

(defsubst nskk--henkan-transition-valid-p (from to)
  "Return non-nil if FROM -> TO is a permitted henkan phase transition.
Duplicate FROM keys rule out `assq', so this is a linear scan over
`nskk--valid-henkan-transitions'."
  (cl-find-if (lambda (pair) (and (eq (car pair) from) (eq (cdr pair) to)))
              nskk--valid-henkan-transitions))

(defun nskk-state-set-henkan-phase (state phase)
  "Set henkan PHASE in STATE, validating the transition from the current phase.
Same-phase transitions are permitted and change nothing.
Signals an error for an invalid phase or an invalid transition."
  (unless (nskk-state-p state)
    (error "nskk-state-set-henkan-phase: STATE must be an nskk-state, got %S" state))
  (unless (memq phase nskk--valid-henkan-phases)
    (error "Invalid henkan phase: %s" phase))
  (let ((current (nskk-state-henkan-phase state)))
    (unless (or (eq current phase)
                (nskk--henkan-transition-valid-p current phase))
      (error "Invalid henkan phase transition: %s -> %s" current phase)))
  (setf (nskk-state-henkan-phase state) phase))

(defun/done nskk-state-force-henkan-phase (state phase)
  "Force set henkan PHASE in STATE, bypassing transition validation.
Use for test setup or emergency reset.
Signals an error when PHASE is not a valid henkan phase."
  (nskk-with-state state
    (unless (memq phase nskk--valid-henkan-phases)
      (error "Invalid henkan phase: %s" phase))
    (setf (nskk-state-henkan-phase state) phase)))

;;;; Generic Setter

(defun/k nskk-state-set (state key value)
  "Set KEY to VALUE in STATE struct and return VALUE.
KEY can be a slot name symbol or string.
Routes \\='mode and \\='henkan-phase through their validating setters and
\\='candidates through `nskk-state-set-candidates'; other slots are set
directly.
Sync wrapper returns VALUE, or nil when STATE is invalid or KEY is unknown.
The /k variant calls ON-FOUND with VALUE, or ON-NOT-FOUND when STATE is
invalid or KEY names no slot.  A KEY of \\='mode or \\='henkan-phase carrying
an invalid VALUE signals an error rather than calling ON-NOT-FOUND."
  (if (nskk-state-p state)
      (let* ((key-sym (if (stringp key) (intern-soft key) key))
             (known-key-p t)
             (result
              (pcase key-sym
                ('mode         (nskk-state-set-mode state value))
                ('henkan-phase (nskk-state-set-henkan-phase state value))
                ('candidates   (nskk-state-set-candidates state value) value)
                ('input-buffer     (setf (nskk-state-input-buffer     state) value))
                ('converted-buffer (setf (nskk-state-converted-buffer state) value))
                ('current-index    (setf (nskk-state-current-index    state) value))
                ('henkan-position  (setf (nskk-state-henkan-position  state) value))
                ('marker-position  (setf (nskk-state-marker-position  state) value))
                ('previous-mode    (setf (nskk-state-previous-mode    state) value))
                ('undo-stack       (setf (nskk-state-undo-stack       state) value))
                ('redo-stack       (setf (nskk-state-redo-stack       state) value))
                ('metadata         (setf (nskk-state-metadata         state) value))
                (_ (setq known-key-p nil)))))
        (if known-key-p (succeed result) (fail)))
    (fail)))

;;;; Candidate Management

(defun/done nskk-state-set-candidates (state candidates)
  "Set CANDIDATES list in STATE and reset the current index to 0."
  (nskk-with-state state
    (setf (nskk-state-candidates state) candidates
          (nskk-state-current-index state) 0)))

;;;; Metadata Helpers

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

(defun nskk-state-set-okurigana (state value)
  "Set okurigana in STATE metadata to VALUE.
VALUE should be the okurigana consonant (e.g. \\='k\\=' for \\='く\\=')."
  (nskk-with-state state
    (nskk-state-put-metadata state 'okurigana value)))

(defun/k nskk-state-get-okurigana (state)
  "Return the okurigana consonant from STATE's metadata, or nil if unset."
  (<- val nskk-state-get-metadata state 'okurigana)
  (succeed val))

;;;; Buffer-local State Management

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

(defvar-local nskk--dcomp-multiple-overlay nil
  "Overlay for displaying multiple dynamic completion candidates inline.
Zero-length overlay anchored at the end of the preedit text.
Managed by dcomp-multiple display logic in nskk-henkan.el; declared here
following the project convention that all buffer-local overlay variables
live in nskk-state.el.")

(defvar-local nskk--henkan-count 0
  "Number of times SPC has been pressed during current conversion.")

(defvar-local nskk--registration-depth 0
  "Current nesting depth of dictionary registration.")

;;;; Display String Construction

(defun nskk-display-sanitize (text face &optional prefix suffix)
  "Return TEXT stripped of every text property and propertized with FACE.
PREFIX and SUFFIX are concatenated around the stripped TEXT before FACE is
applied, so they carry FACE too.

Dictionary candidates and annotations are untrusted input.  A `display',
`keymap' or `local-map' property surviving into an overlay `after-string'
lets a dictionary entry redraw the buffer or rebind keys, so stripping
happens here rather than at each display site.

Only TEXT is stripped.  PREFIX and SUFFIX must be trusted literals: `concat'
carries each argument's own text properties into the result, and the
`propertize' here only adds FACE, so a property on PREFIX or SUFFIX reaches
the caller intact.  Measured -- passing a PREFIX carrying `display' leaves
that `display' on the result."
  (propertize (concat prefix (substring-no-properties text) suffix)
              'face face))

;;;; Overlay Display Priorities
;;
;; NSKK's candidate displays are zero-length overlays carrying an
;; `after-string', several of which resolve to the same anchor -- the end of
;; the conversion overlay.  These constants only give names to the integers
;; those sites already used; the numbers are unchanged.
;;
;; What the numbers buy is narrower than it looks.  The Emacs Lisp manual
;; ("Overlay Properties") defines `priority' for overlays that "cover the same
;; character", and breaks ties by nesting -- "if neither is nested in the other
;; then you should not make assumptions about which overlay will prevail".  A
;; zero-length overlay covers no character and cannot nest inside another at
;; the same position, so the manual does not describe the case NSKK has: the
;; display order of several `after-string's at one position.  Treat the
;; ordering below as an unverified hint rather than a guarantee.
;;
;; The same node warns that "any overlay with a positive priority value will
;; override all the overlays without a priority", and that integer priorities
;; should therefore be used with care.  `nskk-candidate-window.el' sets no
;; priority and is deliberately left out of this ladder: giving it one would
;; change how its overlay ranks against every unprioritized overlay in the
;; buffer, NSKK's own and other packages'.

(defconst nskk-overlay-priority-inline 98
  "Overlay priority used by the inline candidate and registration badge.")

(defconst nskk-overlay-priority-dcomp-multiple 99
  "Overlay priority used by the multi-candidate dynamic completion panel.")

(defconst nskk-overlay-priority-mode-indicator 100
  "Overlay priority used by the `nskk-show-mode' input-mode indicator.
Anchored at point rather than at the conversion overlay, so it collides
with the others only when point happens to sit at that same position.")

;;;; Overlay Management Macros

(defmacro nskk-ensure-overlay (var start end &rest props)
  "Move or create an overlay for VAR covering START to END in the current buffer.
Reuses VAR's existing overlay object if it satisfies `overlayp', creating
a new one otherwise.  In both cases the overlay is moved to START..END and
any PROPS (a plist of property value pairs) are applied via `overlay-put'.
VAR is mutated via `setq' when a new overlay is created."
  (declare (indent 2) (debug t))
  (let ((overlay (gensym "nskk-overlay")))
    `(let ((,overlay (if (overlayp ,var)
                         (move-overlay ,var ,start ,end (current-buffer))
                       (setq ,var (make-overlay ,start ,end)))))
       (cl-loop for (prop val) on (list ,@props) by #'cddr
                do (overlay-put ,overlay prop val)))))

(defmacro nskk-delete-overlay (var)
  "Delete the overlay in VAR and clear VAR before deleting it.
Safe to call when VAR is nil or not an overlay (idempotent).  Clearing
VAR first ensures cleanup failures cannot leave a stale reference."
  (declare (indent 0) (debug t))
  (let ((old-overlay (gensym "nskk-old-overlay")))
    `(let ((,old-overlay ,var))
       (setq ,var nil)
       (when (overlayp ,old-overlay)
         (delete-overlay ,old-overlay)))))

;;;; Shared Buffer-Local State — Accessor API
;;
;; Each of the 8 buffer-local variables above has a getter/setter pair
;; generated below.  Other modules go through these accessors rather than
;; referencing the `nskk--' variables directly, so state.el remains their
;; single owner.  Each accessor targets one fixed, always-bound
;; `defvar-local', so a plain getter/setter pair is the complete API --
;; there is no unknown-slot case to distinguish from a nil value.

(defmacro nskk-define-buffer-local-getter (var)
  "Generate `nskk-state-NAME' returning the value of buffer-local VAR.
NAME is VAR with its `nskk--' prefix removed.  VAR must already be
declared via `defvar-local'."
  (declare (indent 0) (debug t))
  (let ((getter (intern (format "nskk-state-%s"
                                (string-remove-prefix "nskk--" (symbol-name var))))))
    `(defun ,getter ()
       ,(format "Return the current value of `%s'." var)
       ,var)))

(defmacro nskk-define-buffer-local-setter (var)
  "Generate `nskk-state-set-NAME' assigning to buffer-local VAR.
NAME is VAR with its `nskk--' prefix removed.  VAR must already be
declared via `defvar-local'."
  (declare (indent 0) (debug t))
  (let ((setter (intern (format "nskk-state-set-%s"
                                (string-remove-prefix "nskk--" (symbol-name var))))))
    `(defun ,setter (value)
       ,(format "Set `%s' to VALUE and return VALUE." var)
       (setq ,var value))))

(nskk-define-buffer-local-getter nskk--romaji-buffer)
(nskk-define-buffer-local-setter nskk--romaji-buffer)
(nskk-define-buffer-local-getter nskk--conversion-start-marker)
(nskk-define-buffer-local-setter nskk--conversion-start-marker)
(nskk-define-buffer-local-getter nskk--conversion-overlay)
(nskk-define-buffer-local-setter nskk--conversion-overlay)
(nskk-define-buffer-local-getter nskk--pending-romaji-overlay)
(nskk-define-buffer-local-setter nskk--pending-romaji-overlay)
(nskk-define-buffer-local-getter nskk--candidate-overlay)
(nskk-define-buffer-local-setter nskk--candidate-overlay)
(nskk-define-buffer-local-getter nskk--dcomp-multiple-overlay)
(nskk-define-buffer-local-setter nskk--dcomp-multiple-overlay)
(nskk-define-buffer-local-getter nskk--henkan-count)
(nskk-define-buffer-local-setter nskk--henkan-count)
(nskk-define-buffer-local-getter nskk--registration-depth)
(nskk-define-buffer-local-setter nskk--registration-depth)

;;;; Prolog Predicates

(defvar nskk--state-prolog-initialized nil
  "Non-nil when state machine Prolog predicates have been initialized.")

(nskk-prolog-<- (module-initialized-flag nskk--state-prolog-initialized))

(defun/done nskk--state-init-mode-properties ()
  "Assert the mode-properties/5 fact table.
Faces named here are defined in nskk-modeline.el, which loads after this
file.  Prolog stores the face symbols as data rather than evaluating them
at assertion time, so the forward references are safe; cursor faces are
dereferenced via `face-attribute' at runtime."
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
                    "Full-width latin input mode" nskk-cursor-jisx0208-latin)))

(defun/done nskk--state-init-mode-categories ()
  "Assert the mode-category/2 fact table and the japanese-mode/1 rule.
Categories classify input modes orthogonally to their display properties:
`japanese' for kana modes, `marker-mode' for modes using a conversion-start
marker, `other' for direct input."
  (nskk-prolog-define-fact-table mode-category (:arity 2 :index :hash)
    (hiragana      japanese)
    (katakana      japanese)
    (katakana-半角  japanese)
    (abbrev        marker-mode)
    (ascii         other)
    (latin         other)
    (jisx0208-latin other))
  (nskk-prolog-<- (japanese-mode \?m) (mode-category \?m japanese)))

(defun/done nskk-state-initialize-prolog ()
  "Initialize NSKK state machine Prolog predicates (idempotent).
Subsequent calls are no-ops guarded by `nskk--state-prolog-initialized'.
The helpers this calls carry no guard of their own."
  (unless nskk--state-prolog-initialized
    (nskk--state-init-mode-properties)
    (nskk--state-init-mode-categories)
    (setq nskk--state-prolog-initialized t)))

(provide 'nskk-state)

;;; nskk-state.el ends here
