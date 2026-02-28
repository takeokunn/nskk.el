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
;;   input-route/2          -- maps input mode to routing action
;;   toggle-mode/2          -- hiragana<->katakana toggle table
;;   q-key-action/4         -- q key dispatch (style, behavior, buf-state, action)
;;   semicolon-key-action/2 -- semicolon key dispatch (style, action)
;;   fullwidth-char/2       -- ASCII to JIS X0208 full-width character table
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

(require 'nskk-henkan)
(require 'nskk-kana)
(require 'nskk-state)
(require 'nskk-converter)
(require 'nskk-prolog)
(require 'nskk-azik nil t)

(declare-function nskk-modeline-update "nskk-modeline" ())
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
(defvar nskk--romaji-buffer)
(defvar nskk-henkan-on-marker)
(defvar nskk--conversion-start-marker)
(defvar nskk--conversion-overlay)
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
       (nskk-modeline-update))))

;;;; Mode Switching

(nskk-define-mode-setter hiragana)
(nskk-define-mode-setter katakana)
(nskk-define-mode-setter latin)
(nskk-define-mode-setter abbrev)
(nskk-define-mode-setter jisx0208-latin)

(defun nskk-toggle-japanese-mode ()
  "Toggle between hiragana and katakana modes."
  (interactive)
  (let* ((current-mode (when (boundp 'nskk-current-state)
                         (nskk-state-mode nskk-current-state)))
         (target (nskk-prolog-query-value
                  `(toggle-mode ,current-mode ,'\?target) '\?target)))
    (when target
      (nskk--set-mode target)
      (nskk-modeline-update))))

(defun nskk--set-mode (mode)
  "Internal mode setter with validation.
MODE is the target mode symbol."
  (unless (and (boundp 'nskk-current-state) (nskk-state-p nskk-current-state))
    (error "NSKK state not initialized"))
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
    (pcase action
      ('toggle-mode (nskk-toggle-japanese-mode))
      ('insert-n    (insert "\u3093"))
      (_            nil))))

(defun nskk-handle-semicolon-key ()
  "Handle semicolon key press.
In AZIK mode: produce small tsu (\u3063).
In standard mode: self-insert (pass through to `nskk-self-insert')."
  (interactive)
  (let* ((style (if (eq nskk-converter-romaji-style 'azik) 'azik 'standard))
         (action (nskk-prolog-query-value
                  `(semicolon-key-action ,style ,'\?action) '\?action)))
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
    ;; Priority 1: candidate selection (special case, not mode-based)
    (unless (and nskk-henkan--candidate-list-active
                 (nskk--try-candidate-selection char))
      ;; Priority 2: mode-based routing via Prolog
      (let ((route (nskk-prolog-query-value
                    `(input-route ,current-mode ,'\?action) '\?action)))
        (pcase route
          ('insert-direct (nskk-insert-char char n))
          ('process-abbrev (nskk-process-abbrev-input char))
          ('insert-fullwidth (nskk-insert-fullwidth-char char n))
          ('process-japanese (nskk-process-japanese-input char n))
          (_ (nskk-process-japanese-input char n)))))))

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
      (nskk-with-current-state
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
                            (nskk-kana-string-hiragana-to-katakana kana))
                           (t kana))))
          (when converted
            (let ((okuri (nskk-with-current-state
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

;;;; Abbrev Input Processing

(defun nskk-process-abbrev-input (char)
  "Process input CHAR in abbrev mode.
Currently inserts CHAR directly.  Full `abbrev-mode' lookup
\(dictionary-assisted expansion) is not yet implemented."
  (insert char))

;;;; Romaji-to-Kana Conversion

(defun nskk-convert-input-to-kana (char)
  "Convert input CHAR to kana using the romaji-to-kana converter.
Accumulates romaji characters in `nskk--romaji-buffer' and converts
to kana when a complete romaji sequence is recognized.
Returns the converted kana string, or an empty string if the input
is still incomplete (waiting for more characters)."
  (let* ((input (concat nskk--romaji-buffer (char-to-string char)))
         (result (nskk-converter-convert input)))
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
           (nskk-prolog-query-one `(hatsuon-trigger ,char)))
      ;; Emit ん for the trailing n, buffer the new char for next input
      (let ((prefix-without-n (substring nskk--romaji-buffer 0 (1- (length nskk--romaji-buffer)))))
        (setq nskk--romaji-buffer (char-to-string char))
        (let ((prefix-kana (if (> (length prefix-without-n) 0)
                               (let ((prev (nskk-converter-convert prefix-without-n)))
                                 (if (and prev (stringp (car prev)))
                                     (car prev)
                                   ""))
                             "")))
          (concat prefix-kana "\u3093"))))
     ;; Sokuon rule: same consonant doubled (not vowel, not n)
     ((and (> (length nskk--romaji-buffer) 0)
           (let ((last-buf-char (aref nskk--romaji-buffer (1- (length nskk--romaji-buffer)))))
             (and (= last-buf-char char)
                  (nskk-prolog-query-one `(sokuon-eligible ,char)))))
      ;; Emit っ, keep the second consonant as new buffer
      (setq nskk--romaji-buffer (char-to-string char))
      "\u3063")
     ;; Incomplete: buffer the input, return empty string
     ((and result (eq (car result) :incomplete))
      (setq nskk--romaji-buffer input)
      "")
     ;; No match at all: flush the buffer as-is and return the character
     (t
      (setq nskk--romaji-buffer "")
      input))))

(defvar nskk--input-initialized nil
  "Non-nil when input routing Prolog predicates have been initialized.")

(defun nskk-input-initialize ()
  "Initialize input routing and fullwidth character Prolog predicates.
Idempotent: subsequent calls are no-ops."
  (unless nskk--input-initialized
    ;; Input routing rules
    (nskk-prolog-set-index 'input-route 2 :hash)
    (nskk-prolog-<- (input-route ascii insert-direct))
    (nskk-prolog-<- (input-route latin insert-direct))
    (nskk-prolog-<- (input-route abbrev process-abbrev))
    (nskk-prolog-<- (input-route jisx0208-latin insert-fullwidth))
    (nskk-prolog-<- (input-route hiragana process-japanese))
    (nskk-prolog-<- (input-route katakana process-japanese))

    ;; Mode toggle rules
    (nskk-prolog-set-index 'toggle-mode 2 :hash)
    (nskk-prolog-<- (toggle-mode hiragana katakana))
    (nskk-prolog-<- (toggle-mode katakana hiragana))

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

    ;; Full-width character table
    (nskk-prolog-set-index 'fullwidth-char 2 :hash)
    ;; Space maps to ideographic space (special case)
    (nskk-prolog-assert '((fullwidth-char ?\s ?\u3000)))
    ;; ASCII printable range (! 0x21 to ~ 0x7E) maps to FF01-FF5E (offset +#xFEE0)
    (cl-loop for c from ?! to ?~
             do (nskk-prolog-assert `((fullwidth-char ,c ,(+ c #xFEE0)))))

    (setq nskk--input-initialized t)))

(provide 'nskk-input)

;;; nskk-input.el ends here
