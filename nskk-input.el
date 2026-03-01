;;; nskk-input.el --- Input processing for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: japanese i18n

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

;; Input processing and mode switching for NSKK (Layer 4: Input).
;;
;; Layer position: L4 (Input) -- depends on nskk-henkan, nskk-kana,
;;   nskk-state, nskk-converter, nskk-prolog.  Optionally loads nskk-azik.
;;
;; Handles character routing, romaji-to-kana accumulation, mode switching,
;; and AZIK-aware key handlers.  This layer bridges keymap events (L5) and
;; the henkan conversion pipeline (L3).
;;
;; Prolog predicates registered by this module:
;;   input-route/2          -- maps input mode to routing action (katakana-半角 → process-japanese)
;;   toggle-mode/2          -- hiragana<->katakana toggle table (katakana-半角 → hiragana)
;;   q-key-action/4         -- q key dispatch (style, behavior, buf-state, action)
;;   semicolon-key-action/2 -- semicolon key dispatch (style, action)
;;   kakutei-action/2       -- C-j dispatch (state, action)
;;   fullwidth-char/2       -- ASCII to JIS X0208 full-width character table
;;   romaji-input-action/2    -- romaji input state to action dispatch table
;;
;; Key public API:
;;   `nskk-self-insert'              -- main entry point for character input
;;   `nskk-set-mode-hiragana'        -- switch to hiragana mode
;;   `nskk-set-mode-katakana'        -- switch to katakana mode
;;   `nskk-set-mode-latin'           -- switch to ASCII/latin mode
;;   `nskk-set-mode-abbrev'          -- switch to abbrev mode
;;   `nskk-set-mode-jisx0208-latin'  -- switch to full-width latin mode
;;   `nskk-toggle-japanese-mode'     -- toggle hiragana<->katakana
;;   `nskk-current-mode'             -- return current mode symbol
;;   `nskk-handle-q-key'             -- q key with AZIK dispatch
;;   `nskk-handle-semicolon-key'     -- semicolon key with AZIK dispatch
;;   `nskk-convert-input-to-kana'    -- accumulate romaji, emit kana
;;   `nskk--maybe-load-azik-style'   -- load AZIK if configured

;;; Code:

(require 'cl-lib)
(require 'nskk-henkan)
(require 'nskk-kana)
(require 'nskk-state)
(require 'nskk-converter)
(require 'nskk-prolog)
(require 'nskk-azik nil t)
(require 'nskk-debug nil t)

(declare-function nskk-state-get-mode "nskk-state")
(declare-function nskk-state-p "nskk-state")
(declare-function nskk-state-mode "nskk-state")
(declare-function nskk-converter-load-style "nskk-converter")
(declare-function nskk-converter-convert "nskk-converter")
(declare-function nskk-kana-string-hiragana-to-katakana "nskk-kana")
;; From nskk-henkan.el:
(declare-function nskk-commit-current "nskk-henkan")
(declare-function nskk--trigger-okuri-conversion "nskk-henkan")
(declare-function nskk--set-conversion-start-marker "nskk-henkan")
(declare-function nskk--insert-marker "nskk-henkan")
(declare-function nskk--conversion-start-active-p "nskk-henkan")
(declare-function nskk-process-okurigana-input "nskk-henkan")
(declare-function nskk-converting-p "nskk-henkan")
(declare-function nskk--clear-conversion-context "nskk-henkan")
(declare-function nskk--show-pending-romaji "nskk-henkan" (text))
(declare-function nskk--clear-pending-romaji "nskk-henkan" ())
(defvar nskk--romaji-buffer)
(defvar nskk-henkan-on-marker)
(defvar nskk--conversion-start-marker)
(defvar nskk--conversion-overlay)
(defvar nskk--pending-romaji-overlay)
(defvar nskk--henkan-count)
(defvar nskk-henkan--candidate-list-active)
(defvar nskk-henkan-select-candidate-by-key-function)
(defvar nskk-converter-romaji-style)
(defvar nskk-azik-q-behavior)

;;;; Mode Setter Macro

(defmacro nskk-define-mode-setter (mode)
  "Define an interactive mode setter function for MODE.
Creates `nskk-set-mode-MODE' that switches to MODE and updates modeline."
  (declare (indent 1) (debug t))
  (let ((fn-name (intern (format "nskk-set-mode-%s" mode))))
    `(defun ,fn-name ()
       ,(format "Switch to %s mode." mode)
       (interactive)
       (nskk--set-mode ',mode)
       ;; nskk-modeline is an optional display feature; guard avoids
       ;; hard dependency when running without the modeline module.
       (when (fboundp 'nskk-modeline-update)
         (nskk-modeline-update)))))

;;;; Mode Switching

(nskk-define-mode-setter hiragana)
(nskk-define-mode-setter katakana)
(nskk-define-mode-setter latin)
(nskk-define-mode-setter jisx0208-latin)

;;;###autoload
(defun nskk-set-mode-abbrev ()
  "Switch to abbrev mode and set up ▽ preedit marker for dictionary lookup.
Sets up the conversion start marker and inserts ▽ after the mode switch
so that `nskk--has-preedit' detects preedit state and Space triggers
`nskk-start-conversion' with the accumulated ASCII text as the lookup key.
This is DDSKK-compatible: /word SPC → dictionary lookup → candidate."
  (interactive)
  (nskk--set-mode 'abbrev)
  ;; Set up preedit marker after mode switch.  nskk--set-mode calls
  ;; nskk--clear-conversion-context which zeros the marker, so the
  ;; marker must be placed at the current point after that reset.
  (nskk--set-conversion-start-marker (point))
  (nskk--insert-marker nskk-henkan-on-marker)
  (nskk-with-current-state
    (nskk-state-set-henkan-phase nskk-current-state 'on))
  (when (fboundp 'nskk-modeline-update)
    (nskk-modeline-update)))

;;;###autoload
(defun nskk-toggle-japanese-mode ()
  "Toggle between hiragana and katakana modes."
  (interactive)
  (let* ((current-mode (when (boundp 'nskk-current-state)
                         (nskk-state-mode nskk-current-state)))
         (target (nskk-prolog-query-value
                  `(toggle-mode ,current-mode ,'\?target) '\?target)))
    (when target
      (nskk-debug-log "[INPUT] toggle-mode: from=%s to=%s" current-mode target)
      (nskk--set-mode target)
      (when (fboundp 'nskk-modeline-update)
        (nskk-modeline-update)))))

(defun nskk--set-mode (mode)
  "Internal mode setter with validation.
MODE is the target mode symbol.
Signals user-error if NSKK state is not initialized."
  (unless (and (boundp 'nskk-current-state) (nskk-state-p nskk-current-state))
    (user-error "NSKK state not initialized"))
  (nskk-debug-log "[INPUT] set-mode: mode=%s" mode)
  (nskk-state-set nskk-current-state 'mode mode)
  (nskk--clear-conversion-context))


(defun nskk-current-mode ()
  "Return the current NSKK input mode symbol.
Returns a mode symbol such as `hiragana', `katakana', `ascii',
`latin', `abbrev', or `jisx0208-latin', or nil if no state is active."
  (nskk-state-get-mode))

;;;; AZIK Style Initialization

(defun nskk--maybe-load-azik-style ()
  "Load AZIK style if configured and not already loaded."
  (when (and (featurep 'nskk-azik)
             (eq nskk-converter-romaji-style 'azik))
    (nskk-converter-load-style 'azik)))

;;;; AZIK-specific Key Handlers

;;;###autoload
(defun nskk-handle-q-key ()
  "Handle q key press based on current romaji style and configuration.
In AZIK mode with context-aware behavior:
  - If there is pending romaji input, produce \u3093
  - Otherwise, toggle hiragana/katakana mode
In always-n mode: always produce \u3093
In toggle-only mode: toggle mode regardless of context
In standard mode: toggle mode (default SKK behavior)."
  (interactive)
  (let* ((style (if (eq nskk-converter-romaji-style 'azik) 'azik 'standard))
         (behavior (if (featurep 'nskk-azik) nskk-azik-q-behavior 'context-aware))
         (buf-state (if (string-empty-p nskk--romaji-buffer) 'empty 'pending))
         (action (nskk-prolog-query-value
                  `(q-key-action ,style ,behavior ,buf-state ,'\?action)
                  '\?action)))
    (nskk-debug-log "[INPUT] q-key: style=%s behavior=%s buf-state=%s action=%s" style behavior buf-state action)
    (pcase action
      ('toggle-mode (nskk-toggle-japanese-mode))
      ('insert-n    (insert "\u3093"))
      (_            nil))))

;;;###autoload
(defun nskk-handle-semicolon-key ()
  "Handle semicolon key press.
In AZIK mode: produce small tsu (\u3063).
In standard mode: self-insert (pass through to `nskk-self-insert')."
  (interactive)
  (let* ((style (if (eq nskk-converter-romaji-style 'azik) 'azik 'standard))
         (action (nskk-prolog-query-value
                  `(semicolon-key-action ,style ,'\?action) '\?action)))
    (nskk-debug-log "[INPUT] semicolon-key: style=%s action=%s" style action)
    (pcase action
      ('insert-small-tsu (insert "\u3063"))
      ('self-insert      (nskk-self-insert 1))
      (_                 nil))))

;;;; Input Processing

(defun nskk-self-insert (n)
  "Process self-insert input character.
N is the command prefix argument."
  (interactive "p")
  (let ((char (if (integerp last-command-event)
                  last-command-event
                (aref last-command-event 0)))
        (current-mode (or (nskk-state-get-mode) 'ascii)))
    (nskk-debug-log "[INPUT] self-insert: char=%c mode=%s" char current-mode)
    ;; Priority 1: candidate selection (special case, not mode-based)
    (unless (and nskk-henkan--candidate-list-active
                 (progn
                   (nskk-debug-log "[INPUT] candidate-selection: char=%c" char)
                   (nskk--try-candidate-selection char)))
      ;; Priority 2: abbrev mode - direct insertion (DDSKK-compatible).
      ;; In DDSKK, skk-abbrev-mode-map binds all ASCII to skk-abbrev-insert
      ;; = self-insert-command, bypassing all romaji/kana routing.
      ;; We replicate this by short-circuiting Prolog routing in abbrev mode.
      (if (eq current-mode 'abbrev)
          (progn
            (nskk-debug-log "[INPUT] abbrev-input: char=%c" char)
            (nskk-process-abbrev-input char))
        ;; Priority 3: mode-based routing via Prolog
        (let ((route (nskk-prolog-query-value
                      `(input-route ,current-mode ,'\?action) '\?action)))
          (nskk-debug-log "[INPUT] route: mode=%s route=%s" current-mode route)
          (pcase route
            ('insert-direct    (nskk-insert-char char n))
            ('insert-fullwidth (nskk-insert-fullwidth-char char n))
            ('process-japanese (nskk-process-japanese-input char n))
            (_                 (nskk-process-japanese-input char n))))))))

(defun nskk-insert-char (char &optional n)
  "Insert CHAR N times without conversion."
  (let ((n (or n 1)))
    (dotimes (_ n)
      (insert char))))

(defun nskk-insert-fullwidth-char (char &optional n)
  "Insert full-width version of ASCII CHAR N times.
Converts ASCII characters (SPC and !-~) to their JIS X 0208 full-width
equivalents using the fullwidth-char/2 Prolog table.
Space (SPC) maps to ideographic space (U+3000); printable ASCII
characters (! through ~) map to FF01-FF5E via offset +#xFEE0."
  (let* ((n (or n 1))
         (fw-char (or (nskk-prolog-query-value
                       `(fullwidth-char ,char ,'\?fw) '\?fw)
                      char)))             ; non-ASCII: pass through
    (dotimes (_ n)
      (insert fw-char))))

(defun nskk--try-candidate-selection (char)
  "Try to select a candidate using CHAR as a selection key.
Returns non-nil if CHAR was a valid selection key and
the candidate was selected."
  (let ((index (when nskk-henkan-select-candidate-by-key-function
                 (funcall nskk-henkan-select-candidate-by-key-function
                          char
                          (nskk-state-candidates nskk-current-state)
                          (nskk-state-current-index nskk-current-state)))))
    (when index
      ;; Select and commit the candidate
      (setf (nskk-state-current-index nskk-current-state) index)
      (nskk-commit-current)
      t)))

;;;; Japanese Input Processing

(defun nskk--setup-henkan-start-marker (char)
  "Set up conversion start marker for CHAR as a henkan start.
Inserts the ▽ marker, sets conversion start position at point,
and marks henkan phase to `on'.
Called when an uppercase letter triggers auto-henkan-start."
  (nskk-debug-log "[INPUT] henkan-start: char=%c" char)
  (nskk--set-conversion-start-marker (point))
  (nskk--insert-marker nskk-henkan-on-marker)
  (nskk-with-current-state
    (nskk-state-set-henkan-phase nskk-current-state 'on)))

(defun nskk--emit-converted-kana (converted n)
  "Insert CONVERTED kana N times, handling okurigana if active.
When the current state has okurigana set, triggers okurigana conversion
after inserting CONVERTED.  Otherwise, inserts CONVERTED directly."
  (let ((okuri (nskk-with-current-state
                    (nskk-state-get-okurigana nskk-current-state))))
    (if okuri
        (let ((preedit-end (point)))
          (dotimes (_ n) (insert converted))
          (nskk-debug-log "[INPUT] okuri-conversion: okuri=%s kana=%s" okuri converted)
          (nskk--trigger-okuri-conversion okuri preedit-end)
          (nskk-state-set-okurigana nskk-current-state nil))
      (dotimes (_ n) (insert converted)))))

(defun nskk-process-japanese-input (char n)
  "Process input in Japanese mode (hiragana/katakana).
CHAR is the input character.
N is the repeat count.
When CHAR is uppercase and `nskk-converter-auto-start-henkan' is non-nil,
set the conversion start marker at the current point and process the
lowercase version of the letter as normal romaji input."
  (cl-block nskk-process-japanese-input
    (let* ((is-henkan-start (and (characterp char)
                                 (<= ?A char) (<= char ?Z)
                                 nskk-converter-auto-start-henkan
                                 (not (nskk--conversion-start-active-p))))
           (effective-char (if is-henkan-start (downcase char) char)))
      (when is-henkan-start
        (nskk--setup-henkan-start-marker char))
      ;; Okurigana path: when the char is consumed as okurigana input,
      ;; there is nothing more to do.  Henkan-start chars are never
      ;; routed through okurigana (the condition mirrors the original
      ;; `if (and (not is-henkan-start) ...)' guard exactly).
      (when (and (not is-henkan-start) (nskk-process-okurigana-input char))
        (nskk-debug-log "[INPUT] okurigana-processed: char=%c" char)
        (cl-return-from nskk-process-japanese-input))
      ;; Main romaji-to-kana path.
      (let* ((kana (nskk-convert-input-to-kana effective-char))
             (mode (nskk-state-get-mode))
             (converted (cond
                         ((string-empty-p kana) nil)
                         ((eq mode 'katakana)
                          (nskk-kana-string-hiragana-to-katakana kana))
                         (t kana))))
        (if (string-empty-p kana)
            (nskk--show-pending-romaji nskk--romaji-buffer)
          (nskk--clear-pending-romaji))
        (when converted
          (nskk-debug-log "[INPUT] kana-emitted: kana=%s mode=%s" converted mode)
          (nskk--emit-converted-kana converted n))
        (when (and (not (string-empty-p kana))
                   (not (string-empty-p nskk--romaji-buffer)))
          (nskk--show-pending-romaji nskk--romaji-buffer))))))

;;;; Abbrev Input Processing

(defun nskk-process-abbrev-input (char)
  "Process input CHAR in abbrev mode.
CHAR is inserted directly after the ▽ preedit marker into the buffer.
Dictionary lookup is triggered by `nskk-start-conversion' when Space
is pressed: it extracts text between ▽ and point as the dictionary key.
CHAR bypasses the romaji buffer — ASCII input is used verbatim as the
lookup key, consistent with DDSKK abbrev mode behavior."
  (insert char))

;;;; Romaji-to-Kana Conversion

(defun nskk--emit-hatsuon-prefix (new-buffer-value)
  "Emit ん for the trailing `n' in `nskk--romaji-buffer' and update the buffer.

Reads `nskk--romaji-buffer' to compute any kana prefix before the trailing `n',
then writes `nskk--romaji-buffer' ← NEW-BUFFER-VALUE as a side effect.
Returns the prefix kana (possibly empty) concatenated with ん.

Precondition: `nskk--romaji-buffer' must be non-empty and its last character
must be `n'.  This invariant is guaranteed by callers in
`nskk-convert-input-to-kana', which check the buffer state before dispatching.
NEW-BUFFER-VALUE is the string to leave in the buffer after emission
\(e.g., \"n\" for the n+n case, or (char-to-string char) for n+consonant)."
  (let* ((prefix-without-n
          (substring nskk--romaji-buffer 0 (1- (length nskk--romaji-buffer))))
         (prefix-kana
          (if (> (length prefix-without-n) 0)
              (let ((prev (nskk-converter-convert prefix-without-n)))
                (if (and prev (stringp (car prev))) (car prev) ""))
            "")))
    (setq nskk--romaji-buffer new-buffer-value)
    (concat prefix-kana "\u3093")))

(defun nskk--classify-romaji-input (char last-buf-char result)
  "Classify romaji input into a dispatch state symbol.
CHAR is the new input character (integer).
LAST-BUF-CHAR is the last character in `nskk--romaji-buffer', or nil if empty.
RESULT is the return value of `nskk-converter-convert' on the full input.

Returns one of: `nn-double', `match', `n-consonant', `sokuon',
`incomplete', or `no-match'.  The returned symbol can be looked up in
the `romaji-input-action/2' Prolog dispatch table."
  (cond
   ;; n + n: emit ん and keep second 'n' pending (must precede `match' branch)
   ((and last-buf-char (= last-buf-char ?n) (= char ?n))
    'nn-double)
   ;; Successful conversion: return kana, keep remainder in buffer
   ((and result (stringp (car result)))
    'match)
   ;; n + consonant: emit ん for trailing n, buffer new char
   ((and last-buf-char (= last-buf-char ?n)
         (nskk-prolog-query-one `(hatsuon-trigger ,char)))
    'n-consonant)
   ;; Sokuon: same consonant doubled (not vowel, not n)
   ((and last-buf-char (= last-buf-char char)
         (nskk-prolog-query-one `(sokuon-eligible ,char)))
    'sokuon)
   ;; Incomplete: buffer the input
   ((and result (eq (car result) :incomplete))
    'incomplete)
   ;; No match: flush buffer
   (t
    'no-match)))

(defun nskk-convert-input-to-kana (char)
  "Convert input CHAR to kana using the romaji-to-kana converter.
Accumulates romaji characters in `nskk--romaji-buffer' and converts
to kana when a complete romaji sequence is recognized.
Returns the converted kana string, or an empty string if the input
is still incomplete (waiting for more characters)."
  (let* ((input (concat nskk--romaji-buffer (char-to-string char)))
         (result (nskk-converter-convert input))
         (buf-len (length nskk--romaji-buffer))
         (last-buf-char (and (> buf-len 0)
                             (aref nskk--romaji-buffer (1- buf-len)))))
    (pcase (nskk--classify-romaji-input char last-buf-char result)
      ('nn-double
       (nskk-debug-log "[INPUT] romaji-hatsuon-nn: input=%s" input)
       (nskk--emit-hatsuon-prefix "n"))
      ('match
       (let ((kana (car result))
             (remaining (cdr result)))
         (nskk-debug-log "[INPUT] romaji-converted: input=%s kana=%s" input kana)
         (setq nskk--romaji-buffer
               (if (and (stringp remaining) (> (length remaining) 0))
                   remaining
                 ""))
         kana))
      ('n-consonant
       (nskk-debug-log "[INPUT] romaji-hatsuon-n+consonant: input=%s char=%c" input char)
       (nskk--emit-hatsuon-prefix (char-to-string char)))
      ('sokuon
       (nskk-debug-log "[INPUT] romaji-sokuon: input=%s" input)
       (setq nskk--romaji-buffer (char-to-string char))
       "\u3063")
      ('incomplete
       (nskk-debug-log "[INPUT] romaji-incomplete: input=%s" input)
       (setq nskk--romaji-buffer input)
       "")
      (_
       (nskk-debug-log "[INPUT] romaji-no-match: input=%s" input)
       (setq nskk--romaji-buffer "")
       input))))

(defvar nskk--input-initialized nil
  "Non-nil when input routing Prolog predicates have been initialized.")

(defun nskk-input-initialize ()
  "Initialize input routing and fullwidth character Prolog predicates.
Idempotent: subsequent calls are no-ops."
  (unless nskk--input-initialized
    ;; Input routing rules.
    ;; Note: abbrev mode is NOT listed here — in abbrev mode, `nskk-self-insert'
    ;; short-circuits to `nskk-process-abbrev-input' before consulting Prolog,
    ;; mirroring DDSKK's `skk-abbrev-mode-map' direct binding approach.
    (nskk-prolog-set-index 'input-route 2 :hash)
    (nskk-prolog-<- (input-route ascii insert-direct))
    (nskk-prolog-<- (input-route latin insert-direct))
    (nskk-prolog-<- (input-route jisx0208-latin insert-fullwidth))
    (nskk-prolog-<- (input-route hiragana process-japanese))
    (nskk-prolog-<- (input-route katakana process-japanese))
    (nskk-prolog-<- (input-route katakana-半角 process-japanese))

    ;; Mode toggle rules
    (nskk-prolog-set-index 'toggle-mode 2 :hash)
    (nskk-prolog-<- (toggle-mode hiragana katakana))
    (nskk-prolog-<- (toggle-mode katakana hiragana))
    (nskk-prolog-<- (toggle-mode katakana-半角 hiragana))

    ;; Key behavior rules
    (nskk-prolog-set-index 'q-key-action 4 :hash)
    ;; Non-AZIK: always toggle (behavior and buffer-state don't matter)
    (nskk-prolog-<- (q-key-action standard \?behavior \?buf toggle-mode))
    ;; AZIK: behavior-driven dispatch
    (nskk-prolog-<- (q-key-action azik always-n \?buf insert-n))
    (nskk-prolog-<- (q-key-action azik toggle-only \?buf toggle-mode))
    (nskk-prolog-<- (q-key-action azik context-aware empty toggle-mode))
    (nskk-prolog-<- (q-key-action azik context-aware pending insert-n))

    (nskk-prolog-set-index 'semicolon-key-action 2 :hash)
    (nskk-prolog-<- (semicolon-key-action azik insert-small-tsu))
    (nskk-prolog-<- (semicolon-key-action standard self-insert))

    ;; C-j (kakutei) dispatch rules
    (nskk-prolog-set-index 'kakutei-action 2 :hash)
    (nskk-prolog-<- (kakutei-action converting     commit-candidate))
    (nskk-prolog-<- (kakutei-action preedit        commit-preedit))
    (nskk-prolog-<- (kakutei-action romaji-pending clear-romaji))
    (nskk-prolog-<- (kakutei-action hiragana-idle  insert-newline))
    (nskk-prolog-<- (kakutei-action katakana-idle  enter-hiragana))
    (nskk-prolog-<- (kakutei-action direct-idle    enter-hiragana))

    ;; Full-width character table
    (nskk-prolog-set-index 'fullwidth-char 2 :hash)
    ;; Space maps to ideographic space (special case)
    (nskk-prolog-assert '((fullwidth-char ?\s ?\u3000)))
    ;; ASCII printable range (! 0x21 to ~ 0x7E) maps to FF01-FF5E (offset +#xFEE0)
    (cl-loop for c from ?! to ?~
             do (nskk-prolog-assert `((fullwidth-char ,c ,(+ c #xFEE0)))))

    ;; Romaji input dispatch table (documentation + queryable registry)
    ;; Maps classified input state to action symbol for `nskk-convert-input-to-kana'.
    (nskk-prolog-set-index 'romaji-input-action 2 :hash)
    (nskk-prolog-deffacts romaji-input-action
      (nn-double   emit-nn-hatsuon)
      (match       apply-match)
      (n-consonant emit-n-hatsuon)
      (sokuon      emit-sokuon)
      (incomplete  buffer-input)
      (no-match    flush-buffer))

    (setq nskk--input-initialized t)))

(provide 'nskk-input)

;;; nskk-input.el ends here
