;;; nskk-henkan.el --- Conversion pipeline for NSKK -*- lexical-binding: t; -*-

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

;; Conversion (henkan) pipeline for NSKK (Layer 3: Application).
;;
;; Layer position: L3 (Application) -- depends on nskk-kana, nskk-state,
;;   nskk-search, nskk-dictionary, nskk-prolog, nskk-converter, nskk-custom.
;;
;; This module orchestrates the full Japanese input conversion (henkan) flow:
;; preedit management, dictionary search dispatch, candidate navigation,
;; okurigana processing, dictionary registration, and conversion state cleanup.
;;
;; Architecture:
;;   Buffer operations (insert, delete, overlays, markers) are handled by
;;   imperative Emacs Lisp functions.  Decision logic (what action to take
;;   given current state) is encoded as Prolog facts and rules, queried at
;;   runtime via `nskk-henkan-dispatch'.
;;
;; Prolog predicates defined in this module:
;;   core-search-type/2          -- maps search type keyword to search function
;;   converting-phase/1          -- enumerates valid converting phases
;;   okurigana-char/2            -- maps uppercase ASCII to its lowercase
;;   candidate-nav-next-action/3 -- (count threshold action) next-key dispatch
;;   candidate-nav-prev-action/2 -- (list-state action) prev-key dispatch
;;   search-result-action/2      -- (has/no candidates) post-search dispatch
;;   convert-or-commit-action/2  -- (converting state) SPC-without-preedit dispatch
;;   should-update-overlay/1     -- phases that require overlay display
;;   script-toggle/2             -- (mode target) opposite-script for q-key/toggle-key in ▽ preedit
;;   search-backend/2            -- maps integer search backend index to search function
;;   vowel-okurigana-char/1      -- set of lowercase vowels that are immediate okurigana
;;   preedit-phase/1             -- phases where preedit display is active
;;   script-converter/2          -- (target-script converter-fn) for script conversion dispatch
;;   disable-cleanup/2           -- (action handler) cleanup actions on nskk-mode disable
;;
;; External Prolog tables queried by this module:
;;   clearable-input-var/1       -- defined in nskk-input.el; input state vars cleared on reset
;;   presentation-action/2       -- callbacks registered by presentation modules
;;
;; Key macros:
;;   `nskk-without-modification'      -- inhibit undo/modification hooks in body
;;   `nskk-henkan-dispatch'           -- dispatch on Prolog query result
;;   `nskk-henkan-with-preedit'       -- execute body when preedit text exists
;;   `nskk-with-conversion-context'   -- bind candidates and index for conversion
;;   `nskk-when-bound'                -- execute body when variable is bound
;;   `nskk-when-bound-and'            -- execute body when variable is bound and satisfies pred
;;
;; Key public functions:
;;   `nskk-henkan-kakutei-convert-script' -- commit preedit converted to opposite kana script
;;   `nskk-convert'              -- start conversion when preedit exists
;;   `nskk-convert-or-commit'    -- start conversion or commit active candidate
;;   `nskk-next-candidate'       -- advance candidate selection
;;   `nskk-previous-candidate'   -- reverse candidate selection
;;   `nskk-commit-current'       -- insert selected candidate and clear state
;;   `nskk-cancel-conversion'    -- rollback active conversion
;;   `nskk-cancel-preedit'       -- cancel preedit input
;;   `nskk-core-search'          -- dictionary search with type dispatch
;;   `nskk-detect-okurigana-char' -- uppercase consonant detection
;;   `nskk-process-okurigana-input' -- okurigana boundary handling
;;
;; Hook points:
;;   `nskk-henkan-show-candidates-functions' -- called to display candidate list
;;   `nskk-henkan-hide-candidates-functions' -- called to hide candidate list
;;   `nskk-henkan-select-candidate-by-key-function' -- maps key to candidate index

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'nskk-cps-macros)
(require 'nskk-kana)
(require 'nskk-state)
(require 'nskk-search)
(require 'nskk-dictionary)
(require 'nskk-prolog)
(require 'nskk-converter)
(require 'nskk-custom)
(require 'nskk-debug nil t)

(declare-function nskk-state-p "nskk-state")
(declare-function nskk-state-candidates "nskk-state")
(declare-function nskk-state-current-index "nskk-state")
(declare-function nskk-state-set-candidates "nskk-state")
(declare-function nskk-state-set-okurigana "nskk-state")
(declare-function nskk-state-get-okurigana "nskk-state")
(declare-function nskk-state-put-metadata "nskk-state")
(declare-function nskk-state-get-metadata "nskk-state")
(declare-function nskk-state-set-henkan-phase "nskk-state")
(declare-function nskk-state-mode "nskk-state")
(declare-function nskk-state-previous-mode "nskk-state")
(declare-function nskk-dict-register-word "nskk-dictionary")
(declare-function nskk-dict-register-word/k "nskk-dictionary")
(declare-function nskk-dict-unregister-word "nskk-dictionary")
(declare-function nskk-dict-unregister-word/k "nskk-dictionary")
(declare-function nskk-dict-lookup "nskk-dictionary")
(declare-function nskk-dict-lookup/k "nskk-dictionary")
(declare-function nskk-search-prefix/k "nskk-search")
(declare-function nskk-search-partial/k "nskk-search")
(declare-function nskk-state-henkan-phase "nskk-state")
(declare-function nskk-converter-convert "nskk-converter")
(declare-function nskk-kana-string-hiragana-to-katakana "nskk-kana")
(declare-function nskk-kana-string-hiragana-to-katakana/k "nskk-kana" (string on-found on-not-found))
(declare-function nskk-kana-string-katakana-to-hiragana/k "nskk-kana" (string on-found on-not-found))
(declare-function nskk-server-ensure-open "nskk-server")
(declare-function nskk-server-lookup "nskk-server")
(declare-function nskk-server-lookup/k "nskk-server")

(declare-function nskk-program-dict-lookup "nskk-program-dictionary" (key))
(declare-function nskk-program-dict-lookup/k "nskk-program-dictionary"
                  (key on-found on-not-found))
(declare-function nskk-program-dict-builtin-lookup "nskk-program-dictionary" (key))
(declare-function nskk-program-dict-builtin-lookup/k "nskk-program-dictionary"
                  (key on-found on-not-found))
(declare-function nskk-state-force-henkan-phase "nskk-state")
(declare-function nskk-state-get-mode "nskk-state")
(declare-function nskk-kana-string-katakana-to-hiragana "nskk-kana")
(declare-function nskk-kana-hankaku-to-zenkaku "nskk-kana")
(declare-function nskk-study-after-kakutei "nskk-study")
(declare-function nskk-study-reorder "nskk-study")
(declare-function nskk-search-learn "nskk-search")
(declare-function nskk-prolog-trie-prefix-search "nskk-prolog")
(declare-function nskk-converter-convert/k "nskk-converter" (romaji on-match on-incomplete on-fail))

;; From nskk-input.el (loaded after nskk-henkan.el)
(defvar nskk--numeric-mode)
(declare-function nskk--set-mode "nskk-input")

;; From nskk-keymap.el (L5, loaded after nskk-henkan.el)
(defvar nskk-mode-map)
(declare-function nskk--compute-phase "nskk-keymap")

;; From nskk.el (Main layer, loaded after nskk-henkan.el)
(declare-function nskk-mode "nskk")

;;;; Dynamic Completion State

(defvar nskk--dcomp-multiple-overlay)  ;; defined in nskk-state.el

(defvar-local nskk--dcomp-prefix nil
  "The original preedit prefix used for dynamic completion search.")

(defvar-local nskk--dcomp-candidates nil
  "List of reading strings matching `nskk--dcomp-prefix'.")

(defvar-local nskk--dcomp-index 0
  "Current cycling index into `nskk--dcomp-candidates'.")

;;;; Undo-Kakutei State

(defvar-local nskk--last-kakutei-record nil
  "Undo record for the most recent kakutei, or nil.
A plist with keys: :reading, :candidates, :index,
:committed-text, :buffer-start, :buffer-end, :mode,
:registered-p, :registered-reading, :registered-word.
Set by `nskk-commit-current' and
`nskk--insert-registered-and-reset'.
Invalidated by `nskk--post-command-handler' when any
subsequent non-undo command is executed.")

;;;; Conversion State Macros

(defmacro nskk-reset-henkan-state ()
  "Reset conversion state for `nskk-current-state'.
Clears the candidate list, resets the candidate index to 0 (via
`nskk-state-set-candidates'), clears okurigana, and transitions
henkan-phase to nil.
Requires `nskk-current-state' to be bound (use inside `nskk-with-current-state'
or `nskk-with-conversion-context')."
  (declare (indent 0) (debug t))
  `(progn
     (nskk-state-set-candidates nskk-current-state nil)
     (nskk-state-set-okurigana nskk-current-state nil)
     (nskk-state-put-metadata nskk-current-state 'okurigana-in-progress nil)
     (nskk-state-set-henkan-phase nskk-current-state nil)))

;; Forward declarations for variables defined in the "Candidate Display Hooks"
;; section below.  Required so nskk--dismiss-candidate-list (defined here, near
;; its primary caller nskk-henkan-do-reset) can reference them without triggering
;; a byte-compiler "assignment to free variable" warning.
(defvar nskk-henkan-hide-candidates-functions)
(defvar nskk--henkan-candidate-list-active)

(defun nskk--run-all-candidate-hide-hooks ()
  "Run every candidate hide hook and return the first signaled condition.
Unlike `run-hooks', one failing hook does not prevent subsequent cleanup hooks."
  (let (first-condition)
    (run-hook-wrapped
     'nskk-henkan-hide-candidates-functions
     (lambda (function)
       (condition-case condition
           (funcall function)
         ((error quit)
          (unless first-condition
            (setq first-condition condition))))
       nil))
    first-condition))

(defun nskk--run-candidate-show-transaction
    (candidates index previous-index previous-phase previous-count)
  "Run candidate show hooks and roll state back if one signals.
CANDIDATES and INDEX are passed to each show hook.  PREVIOUS-INDEX,
PREVIOUS-PHASE, and PREVIOUS-COUNT are restored after an `error' or `quit'."
  (condition-case condition
      (progn
        (run-hook-with-args
         'nskk-henkan-show-candidates-functions candidates index)
        (setq nskk--henkan-candidate-list-active t))
    ((error quit)
     (setf (nskk-state-current-index nskk-current-state) previous-index
           (nskk-state-henkan-phase nskk-current-state) previous-phase)
     (setq nskk--henkan-count previous-count
           nskk--henkan-candidate-list-active nil)
     (unwind-protect
         (condition-case nil
             (nskk--run-all-candidate-hide-hooks)
           ((error quit) nil))
       (setf (nskk-state-current-index nskk-current-state) previous-index
             (nskk-state-henkan-phase nskk-current-state) previous-phase)
       (setq nskk--henkan-count previous-count
             nskk--henkan-candidate-list-active nil))
     (signal (car condition) (cdr condition)))))

(defun nskk--dismiss-candidate-list ()
  "Dismiss the candidate list display and clear list-active state.
Runs every `nskk-henkan-hide-candidates-functions' hook and resets
`nskk--henkan-candidate-list-active'.  Called by any operation that exits
active candidate list display: cancel, rollback, commit, exhaustion."
  (let ((condition
         (unwind-protect
             (nskk--run-all-candidate-hide-hooks)
           (setq nskk--henkan-candidate-list-active nil))))
    (when condition
      (signal (car condition) (cdr condition)))))

(defun/done nskk-henkan-do-reset ()
  "Reset all henkan conversion state after a commit or registration.
Cleanup is exhaustive: every reset step runs even when an earlier step
signals.  The first `error' or `quit' is re-signaled after canonical state
has been restored."
  (let (first-condition)
    (cl-labels ((run-cleanup
                 (thunk)
                 (condition-case condition
                     (funcall thunk)
                   ((error quit)
                    (unless first-condition
                      (setq first-condition condition))))))
      ;; Hide hooks run before state is cleared so every backend can tear down
      ;; its own UI.  Later cleanup must not be skipped when a hook fails.
      (run-cleanup #'nskk--dismiss-candidate-list)
      (run-cleanup (lambda ()
                     (nskk-delete-overlay nskk--conversion-overlay)))
      (run-cleanup #'nskk--clear-conversion-start-marker)
      (run-cleanup #'nskk--reset-romaji-buffer)
      (run-cleanup (lambda () (setq nskk--henkan-count 0)))
      (run-cleanup #'nskk--clear-azik-pending-state)
      (run-cleanup (lambda ()
                     (nskk-with-current-state
                       (nskk-reset-henkan-state))))

      ;; A cleanup callback can mutate conversion state before signaling.
      ;; Re-assert the terminal invariants without invoking callbacks again.
      (run-cleanup
       (lambda ()
         (when (overlayp nskk--conversion-overlay)
           (unwind-protect
               (delete-overlay nskk--conversion-overlay)
             (setq nskk--conversion-overlay nil)))))
      (run-cleanup
       (lambda ()
         (when (overlayp nskk--pending-romaji-overlay)
           (unwind-protect
               (delete-overlay nskk--pending-romaji-overlay)
             (setq nskk--pending-romaji-overlay nil)))))
      (run-cleanup
       (lambda ()
         (when (markerp nskk--conversion-start-marker)
           (set-marker nskk--conversion-start-marker nil))))
      (run-cleanup
       (lambda ()
         (progn
           (setq nskk--romaji-buffer ""
                 nskk--henkan-count 0
                 nskk--henkan-candidate-list-active nil)
           (dolist (symbol '(nskk--azik-colon-okuri-pending
                             nskk--azik-colon-okuri-deferred
                             nskk--azik-sokuon-okuri-kana-pending
                             nskk--deferred-azik-state
                             nskk--deferred-vowel-shadow-state
                             nskk--sticky-shift-pending))
             (when (boundp symbol)
               (set symbol nil))))))
      (run-cleanup
       (lambda ()
         (when (and (boundp 'nskk-current-state)
                    (nskk-state-p nskk-current-state))
           (setf (nskk-state-candidates nskk-current-state) nil
                 (nskk-state-current-index nskk-current-state) 0
                 (nskk-state-henkan-phase nskk-current-state) nil
                 (nskk-state-metadata nskk-current-state)
                 (let ((metadata (nskk-state-metadata nskk-current-state)))
                   (setq metadata (plist-put metadata 'okurigana nil))
                   (plist-put metadata 'okurigana-in-progress nil))))))
      (when first-condition
        (signal (car first-condition) (cdr first-condition))))))

(defmacro nskk-set-active-candidates (candidates)
  "Set CANDIDATES as active in `nskk-current-state' and enter conversion phase.
Sets candidates, resets index to 0 (via `nskk-state-set-candidates'), and
sets henkan-phase to `active'.
CANDIDATES should be a non-nil list of candidate strings.
Requires `nskk-current-state' to be bound (use inside
`nskk-with-current-state')."
  (declare (indent 0) (debug t))
  `(progn
     (nskk-state-set-candidates nskk-current-state ,candidates)
     (nskk-state-set-henkan-phase nskk-current-state 'active)))

;;;; Buffer Modification Guard

(defmacro nskk-without-modification (&rest body)
  "Execute BODY without triggering modification hooks or undo recording."
  (declare (indent 0) (debug t))
  `(let ((inhibit-modification-hooks t)
         (buffer-undo-list t))
     ,@body))

(defmacro nskk-henkan-dispatch (action-sym query &rest clauses)
  "Evaluate QUERY, bind result to ACTION-SYM, then dispatch via pcase.
QUERY should be a `nskk-prolog-query-value' call that returns an action symbol.
CLAUSES are (action-symbol body...) pairs passed directly to `pcase'."
  (declare (indent 2) (debug t))
  `(let ((,action-sym ,query))
     (pcase ,action-sym
       ,@(mapcar (lambda (c) `(',(car c) ,@(cdr c))) clauses))))

(defmacro nskk-henkan-with-preedit (start-var &rest body)
  "Execute BODY with START-VAR bound to conversion start when preedit exists.
Preedit exists when the conversion start marker is set and point is past
the marker position plus the \u25bd marker length.  Does nothing if no preedit."
  (declare (indent 1) (debug t))
  (let ((start-sym (make-symbol "--nskk-preedit-start--")))
    `(let ((,start-sym (nskk--get-conversion-start)))
       (when (and ,start-sym
                  (>= (point) (+ ,start-sym (length nskk-henkan-on-marker))))
         (let ((,start-var ,start-sym))
           ,@body)))))

(defmacro nskk-with-conversion-context (vars &rest body)
  "Execute BODY when actively converting, with VARS bound to state data.
VARS must be a two-element list: (CANDIDATES-VAR INDEX-VAR).
Guards on `nskk-converting-p' and valid `nskk-current-state'.
CANDIDATES-VAR is bound to the current candidate list.
INDEX-VAR is bound to the current candidate index."
  (declare (indent 1) (debug t))
  (let ((ctx-sym (make-symbol "ctx")))
    `(when (nskk-converting-p)
       (nskk-with-current-state
         (let* ((,ctx-sym nskk-current-state)
                (,(car vars) (nskk-state-candidates ,ctx-sym))
                (,(cadr vars) (nskk-state-current-index ,ctx-sym)))
           ,@body)))))

(defvar nskk--romaji-buffer)            ;; defined in nskk-state.el
(defvar nskk--henkan-count)            ;; defined in nskk-state.el
(defvar nskk--registration-depth)      ;; defined in nskk-state.el
(defvar nskk--conversion-overlay)      ;; defined in nskk-state.el
(defvar nskk--pending-romaji-overlay)  ;; defined in nskk-state.el
(defvar nskk--candidate-overlay)       ;; defined in nskk-state.el
(defvar nskk--conversion-start-marker) ;; defined in nskk-state.el

;;;; Candidate Display Hooks

(defvar nskk-henkan-show-candidates-functions nil
  "Abnormal hook called to display candidates.
Each function is called with two arguments: CANDIDATES (a list of
strings) and CURRENT-INDEX (the 0-based page start offset).")

(defvar nskk-henkan-hide-candidates-functions nil
  "Normal hook called to hide candidate display.
Each function is called with no arguments.")

(defvar-local nskk--henkan-candidate-list-active nil
  "Non-nil when candidate list display is active.
Set by candidate window implementation via hooks.")

(defvar nskk-henkan-select-candidate-by-key-function nil
  "Function to select a candidate by key press.
Called with (key candidates current-index).
Returns the selected candidate index, or nil if KEY is not valid.")

;;;; Error Symbols

(define-error 'nskk-henkan-unknown-search-type
  "Unknown search type in nskk-henkan" 'error)

;;;; Private Predicates

(defsubst nskk--standalone-n-p (buf)
  "Return non-nil if BUF is exactly the single romaji character \"n\"."
  (and (= (length buf) 1) (= (aref buf 0) ?n)))

;;;; Dictionary Search API

;; Optional-backend adapters: these wrappers guard on `fboundp' so that
;; nskk-server.el and nskk-program-dictionary.el remain truly optional.
;; Each backend's own /k function also checks the enable flag internally.

(defun/k nskk--optional-server-lookup (key)
  "Attempt skkserv lookup; call on-not-found if absent, disabled, or unreachable."
  (if (and (fboundp 'nskk-server-lookup/k)
           (boundp 'nskk-server-enable)
           nskk-server-enable
           (nskk-server-ensure-open))
      (<-or result nskk-server-lookup key
        :found (succeed result)
        :fail  (fail))
    (fail)))

(defun/k nskk--optional-program-dict-builtin-lookup (key)
  "Attempt built-in program-dict lookup; call on-not-found if module not loaded.
Guards on `fboundp' so that nskk-program-dictionary.el remains truly optional."
  (if (fboundp 'nskk-program-dict-builtin-lookup/k)
      (<-or result nskk-program-dict-builtin-lookup key
        :found (succeed result)
        :fail  (fail))
    (fail)))

(defun/k nskk--optional-program-dict-lookup (key)
  "Attempt program-dict lookup; call on-not-found if module not loaded."
  (if (fboundp 'nskk-program-dict-lookup/k)
      (<-or result nskk-program-dict-lookup key
        :found (succeed result)
        :fail  (fail))
    (fail)))

(defun/k nskk--optional-kakutei-lookup (key)
  "Attempt kakutei (confirmed) dict lookup; fail if not loaded or no entry.
When `nskk-kakutei-jisyo' is configured and `nskk-dict-lookup-kakutei/k'
is available, tries a confirmed lookup.  A confirmed entry has exactly one
candidate and is committed immediately without candidate selection.
Calls on-not-found for any non-confirmed result."
  (if (fboundp 'nskk-dict-lookup-kakutei/k)
      (<-or result nskk-dict-lookup-kakutei key
        :found (succeed (list result))
        :fail  (fail))
    (fail)))

(defun nskk--merge-candidates-user-first (primary secondary)
  "Merge candidate lists PRIMARY and SECONDARY, keeping PRIMARY first.
Duplicates (compared with `equal') are removed, preserving the first equal
candidate object.  Neither input list is modified."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (candidates (list primary secondary))
      (dolist (candidate candidates)
        (unless (gethash candidate seen)
          (puthash candidate t seen)
          (push candidate result))))
    (nreverse result)))

(defun/k nskk--core-dict-and-server (key)
  "Look up KEY in the local dictionary and skkserv, then combine results.
When `nskk-search-merge-user-dict-with-server' is non-nil, the local
dictionary (which includes user-registered and learned words) is searched
first and its candidates are merged ahead of the server's, with duplicates
removed.  Otherwise the historical behavior is preserved: the server is
tried first and the local dictionary is consulted only on a server miss."
  (if nskk-search-merge-user-dict-with-server
      (<-or local nskk-dict-lookup key
        :found (<-or srv nskk--optional-server-lookup key
                 :found (succeed (nskk--merge-candidates-user-first local srv))
                 :fail  (succeed local))
        :fail  (<-or srv nskk--optional-server-lookup key
                 :found (succeed srv)
                 :fail  (fail)))
    (<-or srv nskk--optional-server-lookup key
      :found (succeed srv)
      :fail  (<-or local nskk-dict-lookup key
               :found (succeed local)
               :fail  (fail)))))

;; Search strategy: exact match → prefix fallback → partial match → skkserv (remote).
;; Each stage calls on-not-found to fall through to the next.
;; The type argument selects which stage is executed for a given call:
;;   :exact   → dict-lookup (local dict first, then skkserv if enabled)
;;   :prefix  → prefix-search (trie prefix scan on system-dict-index)
;;   :partial → partial-search (substring scan on system-dict-index)
;; Callers that want to try multiple strategies must chain them manually via
;; the on-not-found continuation, calling nskk-core-search/k again with a
;; different type.
;;
(defun/k nskk-core-search (key &optional type limit)
  "Search the dictionary for KEY and return a list of candidates.
TYPE is the search type: :exact (default), :prefix, or :partial.
LIMIT caps the number of returned results (default: 100).
Returns nil when no candidates are found.

Contract: exactly one of on-found or on-not-found is always called.
When KEY is not a string (including nil), on-not-found is called
immediately so callers receive a definitive result in all cases.

NOTE: The generated `nskk-core-search/k' variant places ON-FOUND and
ON-NOT-FOUND after the &optional TYPE and LIMIT parameters.  Callers MUST
always pass both continuation arguments explicitly."
  (if (stringp key)
      (let* ((search-type (or type :exact))
             (action (nskk-prolog-query-value
                      `(core-search-type ,search-type ,'\?a) '\?a)))
        (nskk-debug-log "[HENKAN] search: key=%s type=%s" key (or type 'exact))
        (pcase action
          ('dict-lookup
           ;; Fallback chain:
           ;;   kakutei-dict (confirmed, optional) → dict-and-server
           ;;   → builtin-handlers → program-dict.
           ;; Kakutei dictionary is checked first: if it returns a single
           ;; confirmed candidate, it is committed immediately without
           ;; showing the candidate selection menu.
           ;; `nskk--core-dict-and-server' combines the local dictionary and
           ;; skkserv.  By default the server takes priority (historical
           ;; behavior); when `nskk-search-merge-user-dict-with-server' is
           ;; non-nil it merges the user dictionary ahead of the server.
           ;; Enable-flag guards live inside each backend's own /k function.
           ;; fboundp guards in the optional-* wrappers handle unloaded modules.
           (<-or result nskk--optional-kakutei-lookup key
             :found (succeed result)
             :fail  (<-or ds nskk--core-dict-and-server key
               :found (succeed ds)
               :fail  (<-or b nskk--optional-program-dict-builtin-lookup key
                 :found (succeed b)
                 :fail  (<-or p nskk--optional-program-dict-lookup key
                   :found (succeed p)
                   :fail  (fail))))))
          ('prefix-search
           (if (nskk-dict-system-index)
               (<-or r nskk-search-prefix (nskk-dict-system-index) key nil limit
                 :found (succeed r)
                 :fail  (fail))
             (fail)))
          ('partial-search
           (if (nskk-dict-system-index)
               (<-or r nskk-search-partial (nskk-dict-system-index) key nil limit
                 :found (succeed r)
                 :fail  (fail))
             (fail)))
          (_ (signal 'nskk-henkan-unknown-search-type (list search-type)))))
    (fail)))

;;;; Henkan Marker Constants

(defconst nskk-henkan-on-marker "\u25bd"
  "Marker character for henkan-on state (reading input).")

(defconst nskk-henkan-active-marker "\u25bc"
  "Marker character for henkan-active state (conversion active).")

(defconst nskk-okurigana-marker "*"
  "Marker character for okurigana boundary.")

(defconst nskk-henkan-on-marker-regexp (regexp-quote nskk-henkan-on-marker)
  "Pre-computed regexp for henkan-on marker.")

(defconst nskk-henkan-active-marker-regexp (regexp-quote nskk-henkan-active-marker)
  "Pre-computed regexp for henkan-active marker.")

(defvar nskk--registration-display-reading nil
  "Display-format reading shown in the registration prompt, or nil.
Okurigana registrations bind this to the \"stem*kana\" display form
\(e.g. \"ほ*け\") while the reading passed to `nskk-start-registration'
is the dictionary key (e.g. \"ほk\").  The dictionary key is what lookup
uses, so it must be what gets registered; the display form is only for
the minibuffer prompt.")

(defconst nskk-okurigana-marker-regexp (regexp-quote nskk-okurigana-marker)
  "Pre-computed regexp for okurigana boundary marker.")

;;;; Marker Operation Helpers

(defun nskk--insert-marker (marker)
  "Insert MARKER string without triggering modification hooks or undo."
  (nskk-without-modification
    (insert marker)))

(defun nskk--delete-marker-at (pos marker-regexp)
  "Delete marker matching MARKER-REGEXP at POS without triggering hooks."
  (save-excursion
    (goto-char pos)
    (when (looking-at marker-regexp)
      (nskk-without-modification
        (delete-char (length (match-string 0)))))))

(defun nskk--replace-marker-at (pos old-regexp new-marker)
  "Replace marker matching OLD-REGEXP at POS with NEW-MARKER."
  (save-excursion
    (goto-char pos)
    (when (looking-at old-regexp)
      (nskk-without-modification
        (delete-char (length (match-string 0)))
        (insert new-marker)))))

;;;; Overlay Management

(defun/done nskk--update-overlay (start end text)
  "Update overlay to show TEXT from START to END."
  (nskk-ensure-overlay nskk--conversion-overlay start end
                       'display text 'face 'highlight))

(defun/done nskk--show-pending-romaji (text)
  "Show pending romaji TEXT via an after-string overlay at point.
Creates a zero-length overlay at the current insertion point and sets
its \\='after-string property to TEXT.  Uses \\='after-string rather than
\\='display because no buffer text exists yet for the incomplete romaji --
the characters are buffered in `nskk--romaji-buffer', not yet committed."
  (when (and (stringp text) (not (string-empty-p text)))
    (nskk-ensure-overlay nskk--pending-romaji-overlay (point) (point)
                         'after-string text)))

(defun/done nskk--clear-pending-romaji ()
  "Delete the pending romaji overlay if it exists.
Safe to call even when no overlay is active (idempotent)."
  (nskk-delete-overlay nskk--pending-romaji-overlay))

(defun nskk--reset-romaji-buffer ()
  "Clear pending romaji overlay and reset the romaji buffer to empty."
  (nskk--clear-pending-romaji)
  (setq nskk--romaji-buffer ""))

;;;; Conversion State Helpers

(defun/k nskk-converting-p ()
  "Return non-nil if currently converting (▼ or list display phase).
Converting phases are the fixed set declared by `nskk-henkan-initialize'."
  (if (and (boundp 'nskk-current-state)
           nskk-current-state
           (nskk-state-p nskk-current-state)
           (memq (nskk-state-henkan-phase nskk-current-state)
                 '(active list registration)))
      (succeed t)
    (fail)))

(defsubst nskk--has-preedit ()
  "Check if there is preedit text to convert.
Returns non-nil when the conversion start marker is set and point is
past the marker position plus the \u25bd marker length."
  (let ((start (nskk--get-conversion-start)))
    (and start (> (point) (+ start (length nskk-henkan-on-marker))))))

(defsubst nskk--preedit-ends-with-plain-vowel-p ()
  "Return non-nil when preedit ends with a plain vowel kana and romaji empty.
Plain vowel kana are enumerated by `azik-plain-vowel-kana/1'.
In AZIK mode, `:' after a plain vowel kana should produce ー via the
romaji table rather than arming colon-okurigana."
  (and (string-empty-p nskk--romaji-buffer)
       (let ((start (nskk--get-conversion-start)))
         (when start
           (let ((text-start (nskk--skip-marker-pos start nskk-henkan-on-marker-regexp)))
             (and (> (point) text-start)
                  (nskk-prolog-holds-p
                   `(azik-plain-vowel-kana ,(char-before (point))))))))))

(defun nskk--get-conversion-start ()
  "Get conversion start position from the dedicated marker.
Returns the marker position as an integer, or nil if no marker is set."
  (when (and (markerp nskk--conversion-start-marker)
             (marker-position nskk--conversion-start-marker))
    (marker-position nskk--conversion-start-marker)))

(defun/k nskk-preedit-string ()
  "Return the current preedit text (excluding the \u25bd marker).
Returns nil if no preedit is active."
  (let ((start (nskk--get-conversion-start)))
    (if start
        (let ((text-start (nskk--skip-marker-pos start nskk-henkan-on-marker-regexp)))
          (if (> (point) text-start)
              (succeed (buffer-substring-no-properties text-start (point)))
            (fail)))
      (fail))))

(defun nskk--skip-marker-pos (pos marker-regexp)
  "Return position after marker at POS, or POS if no marker is present.
Performs a non-destructive `looking-at' check at POS via `save-excursion'.
MARKER-REGEXP is the regexp to match; on success the advance is taken from
`match-end', so the result is always exact regardless of marker width."
  (save-excursion
    (goto-char pos)
    (if (looking-at marker-regexp)
        (match-end 0)
      pos)))

(defun nskk--set-conversion-start-marker (pos)
  "Set the conversion start marker to POS in the current buffer.
Creates a new marker if one does not already exist."
  (nskk-ensure-marker nskk--conversion-start-marker pos))

(defun nskk--clear-conversion-start-marker ()
  "Clear the conversion start marker, releasing the position."
  (when (markerp nskk--conversion-start-marker)
    (set-marker nskk--conversion-start-marker nil)))

(defun nskk--conversion-start-active-p ()
  "Return non-nil if the conversion start marker is currently active."
  (and (markerp nskk--conversion-start-marker)
       (marker-position nskk--conversion-start-marker)))

;;;; Clear Conversion Context

(defmacro nskk-when-bound (var &rest body)
  "Execute BODY if VAR is bound (but possibly nil or empty)."
  (declare (indent 1) (debug t))
  `(when (boundp ',var)
     ,@body))

(defmacro nskk-when-bound-and (var pred &rest body)
  "Execute BODY if VAR is bound and satisfies PRED."
  (declare (indent 2) (debug t))
  `(when (and (boundp ',var) (,pred ,var))
     ,@body))

(defun/done nskk--clear-azik-pending-state ()
  "Clear AZIK and sticky-shift pending state variables if bound.
Resets `nskk--azik-colon-okuri-pending', `nskk--azik-colon-okuri-deferred',
`nskk--azik-sokuon-okuri-kana-pending', `nskk--deferred-azik-state',
`nskk--deferred-vowel-shadow-state', and `nskk--sticky-shift-pending' to
nil.  Guards each with `boundp' so that the function is safe to call when
AZIK or sticky-shift is not loaded.
Called from `nskk-henkan-kakutei', `nskk-cancel-preedit',
`nskk-rollback-conversion', `nskk-henkan-do-reset', and
`nskk-cancel-conversion-to-reading' to prevent stale pending state from
leaking into the next preedit context."
  (dolist (sym '(nskk--azik-colon-okuri-pending
                 nskk--azik-colon-okuri-deferred
                 nskk--azik-sokuon-okuri-kana-pending
                 nskk--deferred-azik-state
                 nskk--deferred-vowel-shadow-state
                 nskk--sticky-shift-pending))
    (when (boundp sym)
      (set sym nil))))

(defun nskk--clear-conversion-context ()
  "Clear all conversion and input context for mode switching or mode disable.
Called by `nskk--disable' (mode teardown) and mode-switch commands.
Clears: conversion overlay, candidate list display, presentation state (if
present), conversion-start-marker, romaji-buffer, dynamic completion
state (dcomp-candidates, dcomp-prefix, dcomp-index, dcomp-multiple-overlay),
all AZIK/sticky/numeric input state variables via the `clearable-input-var/1'
Prolog fact table, and the current `nskk-state' henkan phase and candidates
via `nskk-reset-henkan-state'.
Cleanup is exhaustive: every step runs under `inhibit-quit', terminal state
is re-asserted after callbacks, and the first `error' or `quit' is re-signaled
unchanged.  Does not reset the input mode."
  (let ((inhibit-quit t)
        first-condition
        clearable-input-vars)
    (cl-labels ((run-cleanup
                 (thunk)
                 (condition-case condition
                     (funcall thunk)
                   ((error quit)
                    (unless first-condition
                      (setq first-condition condition))))))
      (run-cleanup (lambda ()
                     (nskk-delete-overlay nskk--conversion-overlay)))
      (run-cleanup #'nskk--dismiss-candidate-list)
      (dolist (callback (nskk-prolog-presentation-actions 'cleanup))
        (run-cleanup (lambda () (when (fboundp callback) (funcall callback)))))
      (run-cleanup
       (lambda ()
         (nskk-when-bound-and nskk--conversion-start-marker markerp
           (set-marker nskk--conversion-start-marker nil))))
      (run-cleanup
       (lambda ()
         (nskk-when-bound nskk--romaji-buffer
           (nskk--reset-romaji-buffer))))
      (run-cleanup
       (lambda ()
         (setq nskk--dcomp-candidates nil
               nskk--dcomp-prefix nil
               nskk--dcomp-index 0)))
      (run-cleanup (lambda ()
                     (nskk-delete-overlay nskk--dcomp-multiple-overlay)))
      (dolist (callback (nskk-prolog-presentation-actions 'finalize))
        (run-cleanup (lambda () (when (fboundp callback) (funcall callback)))))
      (run-cleanup
       (lambda ()
         (setq clearable-input-vars
               (nskk-prolog-query-all-values
                '(clearable-input-var \?v) '\?v))))
      (dolist (symbol clearable-input-vars)
        (run-cleanup
         (lambda ()
           (when (boundp symbol)
             (set symbol nil)))))
      (run-cleanup
       (lambda ()
         (nskk-with-current-state
           (nskk-reset-henkan-state))))

      ;; Cleanup callbacks can mutate conversion state before signaling.
      ;; Re-assert terminal invariants without invoking callbacks again.
      (run-cleanup (lambda ()
                     (nskk-delete-overlay nskk--conversion-overlay)))
      (run-cleanup (lambda ()
                     (nskk-delete-overlay nskk--pending-romaji-overlay)))
      (run-cleanup (lambda ()
                     (nskk-delete-overlay nskk--dcomp-multiple-overlay)))
      (run-cleanup
       (lambda ()
         (nskk-when-bound-and nskk--conversion-start-marker markerp
           (set-marker nskk--conversion-start-marker nil))))
      (run-cleanup
       (lambda ()
         (nskk-when-bound nskk--romaji-buffer
           (setq nskk--romaji-buffer ""))))
      (run-cleanup
       (lambda ()
         (setq nskk--dcomp-candidates nil
               nskk--dcomp-prefix nil
               nskk--dcomp-index 0
               nskk--henkan-candidate-list-active nil)))
      (dolist (symbol clearable-input-vars)
        (run-cleanup
         (lambda ()
           (when (boundp symbol)
             (set symbol nil)))))
      (run-cleanup
       (lambda ()
         (when (and (boundp 'nskk-current-state)
                    (nskk-state-p nskk-current-state))
           (setf (nskk-state-candidates nskk-current-state) nil
                 (nskk-state-current-index nskk-current-state) 0
                 (nskk-state-henkan-phase nskk-current-state) nil
                 (nskk-state-metadata nskk-current-state)
                 (let ((metadata (nskk-state-metadata nskk-current-state)))
                   (setq metadata (plist-put metadata 'okurigana nil))
                   (plist-put metadata 'okurigana-in-progress nil))))))
      (when first-condition
        (signal (car first-condition) (cdr first-condition))))))

;;;; Kakutei (Commit Preedit As-Is)

(defun/done nskk-henkan-kakutei ()
  "Commit preedit text as-is without dictionary conversion (確定).
Removes the henkan-on marker (▽), clears the conversion start marker,
resets the romaji buffer, clears all AZIK and sticky-shift pending state
variables (see `nskk--clear-azik-pending-state'), and clears the henkan phase.
When called in abbrev mode, restores the previous Japanese input mode
via `nskk--restore-abbrev-mode'."
  (let ((was-abbrev (nskk-with-current-state
                      (eq (nskk-state-mode nskk-current-state) 'abbrev)))
        (start (nskk--get-conversion-start)))
    (when start
      (nskk--delete-marker-at start nskk-henkan-on-marker-regexp))
    (nskk--clear-conversion-start-marker)
    (nskk--reset-romaji-buffer)
    ;; Clear AZIK okurigana pending state that may have been armed in preedit.
    (nskk--clear-azik-pending-state)
    (nskk-with-current-state
      (nskk-state-set-henkan-phase nskk-current-state nil))
    (nskk--restore-abbrev-mode was-abbrev)))

(defun nskk--replace-preedit-with-converted (text-start start converted)
  "Replace preedit at TEXT-START with CONVERTED text and remove ▽ marker at START."
  (atomic-change-group
    (delete-region text-start (point))
    (nskk--delete-marker-at start nskk-henkan-on-marker-regexp)
    (insert converted)))

(defun/done nskk-henkan-kakutei-convert-script ()
  "Convert preedit kana to the opposite script and commit (確定変換).
Queries `script-toggle/2' (Prolog) for the target script:
- hiragana → katakana via `nskk-kana-string-hiragana-to-katakana/k'
- katakana → hiragana via `nskk-kana-string-katakana-to-hiragana/k'
Removes ▽ marker and clears state.  Mode is NOT changed.
Pending romaji is discarded.
Used by `nskk-handle-q' / `nskk-toggle-japanese-mode' in ▽ preedit."
  (let ((start (nskk--get-conversion-start)))
    (when start
      (nskk-with-current-state
        (let* ((mode       (nskk-state-mode nskk-current-state))
               (target     (nskk-prolog-query-value
                            `(script-toggle ,mode ,'\?t) '\?t))
               (text-start (nskk--skip-marker-pos
                            start nskk-henkan-on-marker-regexp))
               (preedit    (buffer-substring-no-properties text-start (point)))
               (converter  (when target
                             (nskk-prolog-query-value
                              `(script-converter ,target \?fn) '\?fn))))
          (when converter
            (funcall converter preedit
                     (lambda (converted)
                       (nskk--replace-preedit-with-converted text-start start converted))
                     #'ignore))))))
  (nskk--clear-conversion-start-marker)
  (nskk--reset-romaji-buffer)
  (nskk--clear-azik-pending-state)
  (nskk-with-current-state
    (nskk-state-set-henkan-phase nskk-current-state nil)))

;;;; Undo-Kakutei

;;;###autoload
(defun nskk-undo-kakutei ()
  "Undo the most recent kakutei (確定) operation.
Reverts the committed text in the buffer, restores ▼ mode with
the original candidates, and if the kakutei involved a dictionary
registration, unregisters the word from the user dictionary.
Only valid immediately after a kakutei; any intervening input
invalidates the undo record.  When no undo record exists, falls
through to `undo'."
  (interactive)
  (let ((record nskk--last-kakutei-record))
    (if (null record)
        (undo)
      (let* ((reading        (plist-get record :reading))
             (candidates     (plist-get record :candidates))
             (index          (plist-get record :index))
             (committed-text (plist-get record :committed-text))
             (okuri-kana     (plist-get record :okuri-kana))
             (buf-start      (plist-get record :buffer-start))
             (buf-end        (plist-get record :buffer-end))
             (registered-p   (plist-get record :registered-p))
             (reg-reading    (plist-get record :registered-reading))
             (reg-word       (plist-get record :registered-word))
             (saved-point    (point))
             (saved-state-object
              (and (boundp 'nskk-current-state) nskk-current-state))
             (working-state
              (and (nskk-state-p saved-state-object)
                   (copy-nskk-state saved-state-object)))
             (saved-marker nskk--conversion-start-marker)
             (saved-marker-buffer
              (and (markerp saved-marker) (marker-buffer saved-marker)))
             (saved-marker-position
              (and (markerp saved-marker) (marker-position saved-marker)))
             (saved-marker-insertion-type
              (and (markerp saved-marker)
                   (marker-insertion-type saved-marker)))
             (saved-overlay nskk--conversion-overlay)
             (saved-overlay-buffer
              (and (overlayp saved-overlay) (overlay-buffer saved-overlay)))
             (saved-overlay-start
              (and saved-overlay-buffer (overlay-start saved-overlay)))
             (saved-overlay-end
              (and saved-overlay-buffer (overlay-end saved-overlay)))
             (saved-dict-snapshot
	      (and registered-p reg-reading reg-word
		   (cons nskk-dict-modified
			 (copy-tree (nskk-dict-lookup reg-reading))))))
        (when working-state
          ;; `plist-put' may mutate an existing plist, so isolate the only
          ;; mutable state graph touched by this operation.
          (setf (nskk-state-metadata working-state)
                (copy-tree (nskk-state-metadata working-state))))
        ;; Retain the record until every part of the undo succeeds.
        (if (and buf-start buf-end (<= buf-end (point-max))
                 (string= committed-text
                          (buffer-substring-no-properties
                           buf-start buf-end)))
            (condition-case err
                (progn
                  ;; Work against disposable ownership objects.  The detached
                  ;; marker and overlay can be restored without inheriting
                  ;; position changes from the temporary buffer edits.
                  (when (markerp saved-marker)
                    (set-marker saved-marker nil))
                  (when (overlayp saved-overlay)
                    (delete-overlay saved-overlay))
                  (setq nskk-current-state working-state
                        nskk--conversion-start-marker nil
                        nskk--conversion-overlay nil)
                  (atomic-change-group
                    (delete-region buf-start buf-end)
                    (goto-char buf-start)
                    (insert nskk-henkan-active-marker)
                    (let ((candidate (nth index candidates)))
                      (when candidate
                        (insert (substring-no-properties candidate))))
                    (let ((ov-start (+ buf-start
                                       (length nskk-henkan-active-marker)))
                          (ov-end   (point)))
                      (when okuri-kana
                        (insert okuri-kana))
                      (when (and registered-p reg-reading reg-word)
                        (nskk-dict-unregister-word reg-reading reg-word))
                      (nskk--set-conversion-start-marker buf-start)
                      (nskk--update-overlay
                       ov-start ov-end (nth index candidates)))
                    ;; nil -> active is intentionally restored without the
                    ;; normal transition graph.
                    (nskk-with-current-state
                     (nskk-state-set-candidates
                      nskk-current-state candidates)
                     (setf (nskk-state-current-index
                            nskk-current-state) index)
                     (nskk-state-force-henkan-phase
                      nskk-current-state 'active)
                     (nskk-state-put-metadata
                      nskk-current-state 'henkan-reading reading)
                     (when okuri-kana
                       (nskk-state-put-metadata
                        nskk-current-state 'okurigana-in-progress t)
                       (nskk-state-put-metadata
                        nskk-current-state 'okurigana-query reading))))
                  (when working-state
                    (when-let* ((record-mode (plist-get record :mode)))
                      (setf (nskk-state-mode working-state) record-mode))
                    (cl-replace saved-state-object working-state)
                    (setq nskk-current-state saved-state-object))
                  (when (and (markerp saved-marker)
                             (not (eq saved-marker
                                      nskk--conversion-start-marker)))
                    (set-marker saved-marker nil))
                  (when (and (overlayp saved-overlay)
                             (not (eq saved-overlay
                                      nskk--conversion-overlay)))
                    (delete-overlay saved-overlay))
                  (setq nskk--last-kakutei-record nil))
              ((error quit)
               ;; Only compensate an unregister that observably changed the
               ;; dictionary.  A pre-mutation signal must not duplicate WORD.
               (progn
		 (when (and registered-p reg-reading reg-word
			    (not (equal (cdr saved-dict-snapshot)
					(nskk-dict-lookup reg-reading))))
		   (condition-case nil
		       (nskk-dict-register-word reg-reading reg-word)
		     ((error quit) nil)))
		 (when saved-dict-snapshot
		   (setq nskk-dict-modified (car saved-dict-snapshot))))
               (unless (eq nskk--conversion-start-marker saved-marker)
                 (when (markerp nskk--conversion-start-marker)
                   (set-marker nskk--conversion-start-marker nil)))
               (setq nskk--conversion-start-marker saved-marker)
               (when (markerp saved-marker)
                 (set-marker saved-marker saved-marker-position
                             saved-marker-buffer)
                 (set-marker-insertion-type
                  saved-marker saved-marker-insertion-type))
               (unless (eq nskk--conversion-overlay saved-overlay)
                 (when (overlayp nskk--conversion-overlay)
                   (delete-overlay nskk--conversion-overlay)))
               (setq nskk--conversion-overlay saved-overlay)
               (when (and (overlayp saved-overlay) saved-overlay-buffer)
                 (move-overlay saved-overlay saved-overlay-start
                               saved-overlay-end saved-overlay-buffer))
               (setq nskk-current-state saved-state-object)
               (goto-char saved-point)
               (signal (car err) (cdr err))))
          (message "NSKK: Cannot undo kakutei -- buffer has changed"))))))

(defun nskk--invalidate-undo-kakutei ()
  "Clear `nskk--last-kakutei-record' to invalidate undo.
Called from `nskk--post-command-handler' when any non-undo
command runs."
  (when nskk--last-kakutei-record
    (setq nskk--last-kakutei-record nil)))

;;;; Purge from Dictionary

;;;###autoload
(defun nskk-purge-from-jisyo ()
  "Purge the current candidate from the user dictionary.
Only valid during ▼ (active conversion) mode.  Prompts for
confirmation before removing the candidate.  After purging,
if candidates remain, shows the next one; otherwise rolls
back to ▽ (preedit) mode."
  (interactive)
  (when (and (nskk-converting-p)
             (boundp 'nskk-current-state)
             (nskk-state-p nskk-current-state))
    (let* ((candidates (nskk-state-candidates nskk-current-state))
           (index      (nskk-state-current-index nskk-current-state))
           (candidate  (nth index candidates))
           (reading    (nskk-state-get-metadata
                        nskk-current-state 'henkan-reading)))
      (when (and candidate reading
                 (yes-or-no-p
                  (format "Really purge \"%s\" (%s)? "
                          (substring-no-properties candidate)
                          (substring-no-properties reading))))
        (nskk-dict-unregister-word reading candidate)
        (let ((remaining (cl-remove candidate candidates
                                    :test #'equal :count 1)))
          (if remaining
              ;; Show next candidate from remaining list.
              (let ((new-idx (min index
                                  (1- (length remaining)))))
                (nskk-state-set-candidates
                 nskk-current-state remaining)
                (setf (nskk-state-current-index
                       nskk-current-state) new-idx)
                (let ((new-cand (nth new-idx remaining)))
                  (nskk--update-overlay
                   (+ (nskk--get-conversion-start)
                      (length nskk-henkan-active-marker))
                   (overlay-end nskk--conversion-overlay)
                   new-cand)))
            ;; No candidates left: rollback to preedit.
            (nskk-cancel-conversion-to-reading)))))))

;;;; Conversion Control

(defun/done nskk-convert ()
  "Start conversion when preedit text exists.
Uses `nskk-henkan-with-preedit' to guard on preedit presence."
  :interactive t
  (nskk-henkan-with-preedit _start
    (nskk-start-conversion)))

(defun/done nskk-convert-or-commit ()
  "Start conversion or commit current candidate.
When actively converting (▼ phase), commits the current candidate.
Otherwise, starts conversion if preedit exists.
Uses Prolog `convert-or-commit-action/2' for dispatch."
  :interactive t
  (let ((conv-state (if (nskk-converting-p) 'converting 'not-converting)))
    (nskk-henkan-dispatch action
        (nskk-prolog-query-value
         `(convert-or-commit-action ,conv-state ,'\?a) '\?a)
      (commit-current
       (nskk-commit-current))
      (start-conversion
       (nskk-convert)))))

(defun/done nskk-cancel-conversion-to-reading ()
  "Cancel active conversion, restoring the kana reading to the buffer.
Unlike `nskk-rollback-conversion', does NOT return to preedit (▽) state.
Removes the ▼ marker and the overlay, resets all conversion state, and
leaves the kana reading text in the buffer without any preedit marker.
Used by the DEL key handler."
  (when (nskk-converting-p)
    (let ((start (nskk--get-conversion-start))
          (saved-point (point)))
      ;; Remove only the ▼ marker character(s) at start, keeping kana reading.
      (when start
        (condition-case err
            (atomic-change-group
              (save-excursion
                (goto-char start)
                (when (looking-at nskk-henkan-active-marker-regexp)
                  (delete-region start (match-end 0)))))
          ((error quit)
           (goto-char saved-point)
           (signal (car err) (cdr err)))))
      ;; Clear overlays and conversion state only after buffer success.
      (nskk-delete-overlay nskk--conversion-overlay)
      (nskk--clear-conversion-start-marker)
      (nskk--reset-romaji-buffer)
      (setq nskk--henkan-count 0)
      (nskk--dismiss-candidate-list)
      (nskk--clear-azik-pending-state)
      (nskk-with-current-state
        (nskk-reset-henkan-state)))))

(defun/done nskk-cancel-conversion ()
  "Cancel active conversion and return to preedit (▽) phase.
Delegates to `nskk-rollback-conversion', which replaces the ▼
marker with ▽ and clears candidates, overlay, and conversion state.
Does nothing when not currently converting."
  :interactive t
  (nskk-debug-log "[HENKAN] cancel-conversion")
  (when (nskk-converting-p)
    (nskk-rollback-conversion)))

(defun nskk--restore-abbrev-mode (was-abbrev)
  "Restore previous Japanese mode when exiting abbrev/numeric preedit.
WAS-ABBREV is non-nil when the active mode was abbrev at exit time.
Uses setf directly on the struct slot to avoid updating previous-mode
\(this is a restore, not a user-initiated mode switch).
Also clears `nskk--numeric-mode' since numeric mode reuses abbrev.
No-op when WAS-ABBREV is nil or previous-mode is nil or abbrev."
  (when was-abbrev
    (nskk-when-bound nskk--numeric-mode (setq nskk--numeric-mode nil))
    (let ((prev-mode (nskk-with-current-state
                       (nskk-state-previous-mode nskk-current-state))))
      (when (and prev-mode (not (eq prev-mode 'abbrev)))
        (nskk-with-current-state
          ;; Use raw setf rather than nskk-state-transition/k: this is a
          ;; restore of a known-valid previous mode (registration session
          ;; exit), not a user-initiated transition between modes.
          (setf (nskk-state-mode nskk-current-state) prev-mode))))))

(defun/done nskk-cancel-preedit ()
  "Cancel preedit input and remove the ▽ marker.
Deletes preedit text between the conversion start marker and point,
including the ▽ marker character, and resets state.
When called in abbrev mode, restores the previous
Japanese input mode via `nskk--restore-abbrev-mode'."
  :interactive t
  (let* ((start      (nskk--get-conversion-start))
         (was-abbrev (nskk-with-current-state
                       (eq (nskk-state-mode nskk-current-state) 'abbrev)))
         (saved-point (point)))
    ;; Remove preedit text including the ▽ marker.
    (when start
      (condition-case err
          (atomic-change-group
            (delete-region start (point))
            (goto-char start))
        ((error quit)
         (goto-char saved-point)
         (signal (car err) (cdr err)))))
    ;; Reset all preedit state.
    (nskk--clear-conversion-start-marker)
    (nskk--reset-romaji-buffer)
    (setq nskk--henkan-count 0)
    ;; Clear okurigana state (including stale AZIK colon-okurigana).
    ;; nskk-reset-henkan-state also clears henkan-phase; use it here so
    ;; that a cancelled colon-okurigana sequence (Ka:) does not leave
    ;; nskk-state-get-okurigana returning a stale value on the next SPC.
    (nskk-with-current-state
      (nskk-reset-henkan-state))
    ;; Clear AZIK okurigana pending state.  cancel-preedit does not go through
    ;; nskk--clear-conversion-context, so clear directly here.
    (nskk--clear-azik-pending-state)
    ;; Restore abbrev mode if applicable.
    (nskk--restore-abbrev-mode was-abbrev)))

(defun/done nskk-rollback-conversion ()
  "Rollback to pre-conversion state.
Replaces the ▼ marker with ▽ and returns to preedit phase.
The conversion start marker remains active so the user lands back in
the preedit (▽) state (C-g from ▼ returns to ▽).

If point drifted outside the conversion region (e.g. from an unmapped
cursor-movement key pressed before DEL), it is repositioned to the end
of the preedit reading text so that `nskk--has-preedit' returns t."
  :interactive t
  (when (nskk-converting-p)
    (let ((start (nskk--get-conversion-start)))
      ;; Save overlay-end before deletion: this is the end of the reading kana
      ;; in buffer coordinates.  We use it to reposition point if it drifted.
      (let ((preedit-end (when (overlayp nskk--conversion-overlay)
                           (overlay-end nskk--conversion-overlay)))
            (saved-point (point)))
        ;; The okurigana suffix removal and marker replacement are one buffer
        ;; transaction; either both become visible or neither does.
        (condition-case err
            (atomic-change-group
              (when (and preedit-end
                         (nskk-with-current-state
                           (nskk-state-get-metadata
                            nskk-current-state 'okurigana-in-progress))
                         (> (point) preedit-end))
                (delete-region preedit-end (point)))
              (when start
                (nskk--replace-marker-at
                 start nskk-henkan-active-marker-regexp
                 nskk-henkan-on-marker)))
          ((error quit)
           (goto-char saved-point)
           (signal (car err) (cdr err))))
        ;; Clear non-buffer conversion state only after buffer success.
        (nskk-delete-overlay nskk--conversion-overlay)
        (nskk--reset-romaji-buffer)
        (setq nskk--henkan-count 0)
        (nskk--dismiss-candidate-list)
        ;; Restore to preedit (on) phase -- C-g/DEL from ▼ returns to ▽.
        (nskk-with-current-state
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-state-put-metadata
           nskk-current-state 'okurigana-in-progress nil)
          (nskk-state-put-metadata
           nskk-current-state 'okurigana-query nil))
        (nskk--clear-azik-pending-state)
        ;; Reposition point to end of reading kana if it drifted outside the
        ;; conversion region.  This ensures nskk--has-preedit returns t.
        (when (and preedit-end start
                   (or (< (point)
                          (+ start (length nskk-henkan-on-marker)))
                       (> (point) preedit-end)))
          (goto-char preedit-end))))))

;;;; Candidate Navigation

(defun/k nskk-next-candidate ()
  "Select next conversion candidate.
For the first N-1 candidates (N = `nskk-henkan-show-candidates-nth'),
show candidates one-by-one with the ▼ overlay.  On the Nth press,
switch to overlay candidate list display below the conversion region.
Uses Prolog `candidate-nav-next-action/3' to dispatch the navigation mode."
  :interactive t
  (nskk-debug-log "[HENKAN] next-candidate: direction=next")
  (if (nskk-converting-p)
      (progn
        (cl-incf nskk--henkan-count)
        (let ((action (nskk-prolog-query-value
                       `(candidate-nav-next-action ,nskk--henkan-count
                                                   ,nskk-henkan-show-candidates-nth
                                                   ,'\?a)
                       '\?a)))
          (pcase action
            ('select-next
             (nskk--select-candidate 'next)
             (let* ((candidates (nskk-state-candidates nskk-current-state))
                    (index (nskk-state-current-index nskk-current-state))
                    (candidate (nth index candidates)))
               (succeed candidate)))
            ('show-list-next
             (nskk--show-candidate-list-next)
             (fail)))))
    ;; Not converting: signal nothing happened
    (fail)))

(defun/k nskk-previous-candidate ()
  "Select previous conversion candidate.
In candidate list display mode, shows the previous page.
In single-candidate mode, decrements the counter and shows the previous
candidate.
Uses Prolog `candidate-nav-prev-action/2' to dispatch the navigation mode."
  :interactive t
  (nskk-debug-log "[HENKAN] prev-candidate: direction=prev")
  (if (nskk-converting-p)
      (let ((action (nskk-prolog-query-value
                     `(candidate-nav-prev-action
                       ,(if nskk--henkan-candidate-list-active
                            'list-active 'not-active)
                       ,'\?a)
                     '\?a)))
        (pcase action
          ('show-list-prev
           (nskk--show-candidate-list-prev)
           ;; succeed with nil: signals "showed previous list page" (no candidate selected).
           ;; Callers using the sync wrapper receive nil — same as "not currently converting".
           (succeed nil))
          ('select-prev
           (when (> nskk--henkan-count 0)
             (cl-decf nskk--henkan-count))
           (nskk--select-candidate 'previous)
           (let* ((candidates (nskk-state-candidates nskk-current-state))
                  (index (nskk-state-current-index nskk-current-state))
                  (candidate (nth index candidates)))
             (succeed candidate)))))
    ;; Not converting: signal nothing happened
    (fail)))

(defun/k nskk-commit-current ()
  "Commit current conversion candidate.
Replaces preedit text (including ▼ marker) with the selected candidate,
then clears all conversion state: overlay, start marker, candidates,
henkan phase, and romaji buffer.
Guards on active conversion state and a valid `nskk-current-state'.

Uses the overlay boundary (not point) as the deletion range, so that
cursor drift caused by unmapped keys or mouse clicks does not corrupt
the buffer.  For okurigana conversions the kana suffix (e.g. \"く\" in
\"書く\") already sits in the buffer after the overlay-end; it is left
in place and will immediately follow the inserted candidate."
  :interactive t
  (nskk-debug-log "[HENKAN] commit-current")
  ;; Use nskk-with-conversion-context + nskk-with-current-state so the
  ;; defun/k CPS transformer can see and transform (succeed) directly.
  ;; Project macros are not reliably visible to macroexpand in the CPS
  ;; transformer during byte-compilation.
  (when (and (nskk-converting-p)
             (boundp 'nskk-current-state)
             (nskk-state-p nskk-current-state))
    (let* ((candidates    (nskk-state-candidates nskk-current-state))
           (index         (nskk-state-current-index nskk-current-state))
           (candidate     (nth index candidates))
           (start         (nskk--get-conversion-start))
           (mode          (nskk-state-mode nskk-current-state))
           (was-abbrev    (eq mode 'abbrev))
           (abbrev-restore-mode
            (when was-abbrev
              (nskk-state-previous-mode nskk-current-state)))
           (reading       (nskk-state-get-metadata
                           nskk-current-state 'henkan-reading))
           (committed-p   nil)
           ;; NOTE: (overlayp obj) returns t even after delete-overlay — the
           ;; Lisp object persists but overlay-end returns nil for a deleted
           ;; overlay.  Always check overlay-end result, not just overlayp.
           (overlay-end-pos (when (and (overlayp nskk--conversion-overlay)
                                       (overlay-end nskk--conversion-overlay))
                              (overlay-end nskk--conversion-overlay)))
           ;; When an unbound command (M-b, mouse click, etc.) moves point
           ;; backward into the conversion area, (point) is less than
           ;; overlay-end.  Use overlay-end as the authoritative deletion
           ;; boundary so that the entire conversion text is removed.
           ;; When point >= overlay-end (normal case, or okurigana where
           ;; kana sits after the overlay), use (point) to also cover
           ;; the okurigana suffix.
           (end           (if (and overlay-end-pos (> overlay-end-pos (point)))
                              overlay-end-pos
                            (point)))
           ;; Okurigana kana (e.g. "く" in "書く") sits between overlay-end
           ;; and point.  Capture it before deletion so it can be re-inserted
           ;; after the candidate, placing point after the okurigana kana.
           (okuri-kana    (when (and overlay-end-pos (< overlay-end-pos end))
                            (buffer-substring-no-properties overlay-end-pos end))))
      (when (and start candidate)
        (let ((committed (substring-no-properties candidate))
              (committed-with-okuri
               (concat (substring-no-properties candidate) (or okuri-kana ""))))
          ;; Publish the buffer replacement and undo record exactly once before
          ;; any cleanup callback or learning backend can fail.
          (atomic-change-group
            (delete-region start end)
            (goto-char start)
            (insert committed)
            (when okuri-kana
              (insert okuri-kana)))
          (setq nskk--last-kakutei-record
                (list :reading reading
                      :candidates candidates
                      :index index
                      :committed-text committed-with-okuri
                      :okuri-kana okuri-kana
                      :buffer-start start
                      :buffer-end (point)
                      :mode mode
                      :registered-p nil
                      :registered-reading nil
                      :registered-word nil)
                committed-p t)))

      (let (first-condition)
        (cl-labels ((run-cleanup
                     (thunk)
                     (condition-case condition
                         (funcall thunk)
                       ((error quit)
                        (unless first-condition
                          (setq first-condition condition))))))
          ;; Reset first so a hide-hook failure remains the primary condition.
          ;; Abbrev restoration still runs, and neither failure reaches learning.
          (run-cleanup #'nskk-henkan-do-reset)
          (run-cleanup (lambda ()
                         (nskk--restore-abbrev-mode was-abbrev)))

          ;; The abbrev callback can dirty state before signaling.  Re-assert
          ;; both conversion and mode invariants without invoking hooks again.
          (run-cleanup
           (lambda ()
             (when (overlayp nskk--conversion-overlay)
               (unwind-protect
                   (delete-overlay nskk--conversion-overlay)
                 (setq nskk--conversion-overlay nil)))
             (when (overlayp nskk--pending-romaji-overlay)
               (unwind-protect
                   (delete-overlay nskk--pending-romaji-overlay)
                 (setq nskk--pending-romaji-overlay nil)))
             (when (markerp nskk--conversion-start-marker)
               (set-marker nskk--conversion-start-marker nil))
             (progn
               (setq nskk--romaji-buffer ""
                     nskk--henkan-count 0
                     nskk--henkan-candidate-list-active nil)
               (dolist (symbol '(nskk--azik-colon-okuri-pending
                                 nskk--azik-colon-okuri-deferred
                                 nskk--azik-sokuon-okuri-kana-pending
                                 nskk--deferred-azik-state
                                 nskk--deferred-vowel-shadow-state
                                 nskk--sticky-shift-pending))
                 (when (boundp symbol)
                   (set symbol nil))))
             (when (and (boundp 'nskk-current-state)
                        (nskk-state-p nskk-current-state))
               (setf (nskk-state-candidates nskk-current-state) nil
                     (nskk-state-current-index nskk-current-state) 0
                     (nskk-state-henkan-phase nskk-current-state) nil
                     (nskk-state-metadata nskk-current-state)
                     (let ((metadata
                            (nskk-state-metadata nskk-current-state)))
                       (setq metadata (plist-put metadata 'okurigana nil))
                       (plist-put metadata 'okurigana-in-progress nil))))
             (when was-abbrev
               (when (boundp 'nskk--numeric-mode)
                 (setq nskk--numeric-mode nil))
               (when (and abbrev-restore-mode
                          (not (eq abbrev-restore-mode 'abbrev))
                          (nskk-state-p nskk-current-state))
                 (setf (nskk-state-mode nskk-current-state)
                       abbrev-restore-mode)))))
          (when first-condition
            (signal (car first-condition) (cdr first-condition)))))

      ;; Learning is deliberately last.  Each backend retains its normal
      ;; fail-fast ordering and propagates the exact original condition.
      (when (and committed-p reading)
        (when (fboundp 'nskk-study-after-kakutei)
          (nskk-study-after-kakutei reading candidate index))
        (nskk-search-learn reading candidate))
      (succeed candidate))))

(defun nskk--select-candidate (direction)
  "Select candidate in DIRECTION (next or previous).
This function must be called from within a `nskk-with-conversion-context' body
where `nskk-current-state' is guaranteed valid."
  (let* ((candidates (nskk-state-candidates nskk-current-state))
         (total      (length candidates)))
    (when (> total 0)
      (let* ((current   (nskk-state-current-index nskk-current-state))
             (new-index (if (eq direction 'next)
                            (mod (1+ current) total)
                          (mod (+ current total -1) total)))
             (candidate (nth new-index candidates))
             (start     (nskk--get-conversion-start))
             ;; Skip ▼ marker unconditionally: during candidate cycling the
             ;; marker is always present, so no looking-at guard is needed.
             (text-start (when start (+ start (length nskk-henkan-active-marker))))
             ;; Preserve the existing overlay end so that okurigana kana
             ;; (which sits after the overlay) is not consumed by the overlay.
             ;; Without this, cycling candidates would extend the overlay to
             ;; (point), swallowing the okurigana kana and losing it on commit.
             (end       (if (overlayp nskk--conversion-overlay)
                            (overlay-end nskk--conversion-overlay)
                          (point))))
        (setf (nskk-state-current-index nskk-current-state) new-index)
        (nskk--update-overlay text-start end candidate)
        (dolist (callback (nskk-prolog-presentation-actions 'show-candidate))
          (when (fboundp callback) (funcall callback candidate)))))))

(defun nskk--show-candidate-list-next ()
  "Show next page of candidates in overlay list below the conversion region.
When all candidates are exhausted, trigger dictionary registration.
This function must be called from within a `nskk-with-conversion-context' body
where `nskk-current-state' is guaranteed valid."
  (let* ((candidates (nskk-state-candidates nskk-current-state))
         (current (nskk-state-current-index nskk-current-state))
         (previous-phase (nskk-state-henkan-phase nskk-current-state))
         (previous-count nskk--henkan-count)
         (per-page (min nskk-henkan-number-to-display-candidates
                        (length nskk-henkan-show-candidates-keys)))
         (next-start (if nskk--henkan-candidate-list-active
                         (+ current per-page)
                       current)))
    (if (>= next-start (length candidates))
        ;; All candidates exhausted: trigger dictionary registration
        (nskk--exhaust-candidates)
      ;; Show next page
      (setf (nskk-state-current-index nskk-current-state) next-start)
      (nskk-state-set-henkan-phase nskk-current-state 'list)
      (nskk--run-candidate-show-transaction
       candidates next-start current previous-phase previous-count))))

(defun nskk--show-candidate-list-prev ()
  "Show previous page of candidates in overlay list below the conversion region.
This function must be called from within a `nskk-with-conversion-context' body
where `nskk-current-state' is guaranteed valid."
  (let* ((candidates (nskk-state-candidates nskk-current-state))
         (current (nskk-state-current-index nskk-current-state))
         (previous-phase (nskk-state-henkan-phase nskk-current-state))
         (previous-count nskk--henkan-count)
         (per-page (min nskk-henkan-number-to-display-candidates
                        (length nskk-henkan-show-candidates-keys)))
         (prev-start (- current per-page)))
    (when (< prev-start 0)
      (setq prev-start 0))
    (setf (nskk-state-current-index nskk-current-state) prev-start)
    (nskk--run-candidate-show-transaction
     candidates prev-start current previous-phase previous-count)))

;;;; Okurigana Handling

(defun/k nskk-detect-okurigana-char (char)
  "Return the okurigana consonant for CHAR when CHAR is an uppercase consonant.
Returns nil when CHAR is not an uppercase ASCII consonant character."
  (if (characterp char)
      (let ((result (nskk-prolog-query-value `(okurigana-char ,char ,'\?lc) '\?lc)))
        (if result (succeed result) (fail)))
    (fail)))

(defun nskk--flush-romaji-before-okuri ()
  "Flush the pending romaji buffer before inserting an okurigana boundary.
Converts a standalone \\='n\\=' to ん (word boundary), converts complete
romaji to kana, and silently drops incomplete sequences.
In katakana mode the kana is converted before insertion.
Must be called inside `nskk-with-current-state'."
  (unless (string-empty-p nskk--romaji-buffer)
    (let* ((buf nskk--romaji-buffer)
           (kana (cond
                  ;; Standalone n at word boundary → ん.
                  ;; Checked first so "nK" emits ん rather than being discarded.
                  ((nskk--standalone-n-p buf) "ん")
                  ;; Complete romaji: converter returns (kana . rest).
                  ;; :incomplete and nil results are silently dropped.
                  (t (let ((result (nskk-converter-convert buf)))
                       (when (and result (stringp (car result)))
                         (car result)))))))
      (when kana
        (insert (if (eq (nskk-state-mode nskk-current-state) 'katakana)
                    (nskk-kana-string-hiragana-to-katakana kana)
                  kana)))))
  (nskk--reset-romaji-buffer))

(defun/done nskk--handle-vowel-okuri (okuri-char)
  "Handle vowel okurigana OKURI-CHAR.
Vowel okurigana (a/i/u/e/o) is immediately complete — no following
character needed.  The kana is inserted and dictionary conversion is
triggered at once, preventing a spurious second okurigana boundary.
Must be called inside `nskk-with-current-state'."
  (let ((preedit-end (point)))
    (setq nskk--romaji-buffer (char-to-string okuri-char))
    (nskk-convert-input-to-kana-final/k
      (lambda (kana)
        (let ((converted (if (eq (nskk-state-mode nskk-current-state) 'katakana)
                             (nskk-kana-string-hiragana-to-katakana kana)
                           kana)))
          (insert converted)
          (nskk--trigger-okuri-conversion okuri-char preedit-end)
          (nskk-state-set-okurigana nskk-current-state nil)))
      #'ignore)))

(defun nskk--handle-consonant-okuri (okuri-char on-consumed)
  "Handle consonant okurigana OKURI-CHAR, then call ON-CONSUMED.
Puts the consonant into the romaji buffer for deferred kana completion —
the kana is produced when the user types the following vowel (e.g. K +
u → く).  Shows the pending consonant as an after-string overlay.
Must be called inside `nskk-with-current-state'."
  (setq nskk--romaji-buffer (char-to-string okuri-char))
  (nskk--show-pending-romaji nskk--romaji-buffer)
  (funcall on-consumed))

(defun nskk--setup-okurigana-context (okuri-char)
  "Prepare okurigana conversion context for OKURI-CHAR.
Flushes any pending romaji, inserts the * boundary marker, and records
OKURI-CHAR in state.  Must be called inside `nskk-with-current-state'."
  (nskk--flush-romaji-before-okuri)
  (nskk--insert-marker nskk-okurigana-marker)
  (nskk-state-set-okurigana nskk-current-state okuri-char))

(defun/k nskk-process-okurigana-input (char)
  "Process CHAR as potential okurigana marker.
If CHAR is uppercase and the conversion start marker is active,
store okurigana context, insert * boundary marker, and put the
consonant into the romaji buffer for deferred kana accumulation.
Calls on-found with t when CHAR was handled as okurigana;
on-not-found otherwise.
Skips when okurigana is already pending (e.g. second N in YoNN);
the caller downcases and routes through kana conversion instead."
  (let ((okuri-char (nskk-detect-okurigana-char char)))
    (if (and okuri-char
             (nskk--conversion-start-active-p)
             ;; Don't re-enter okurigana when already in okurigana zone.
             ;; The second uppercase consonant (e.g. N in YoNN) must complete
             ;; the kana sequence, not insert another * marker.
             (not (nskk-with-current-state
                    (nskk-state-get-okurigana nskk-current-state))))
        ;; call/cc captures on-found as K so inner lambdas can call it.
        (call/cc (lambda (K)
          (nskk-with-current-state
            (nskk--setup-okurigana-context okuri-char)
            (if (nskk-prolog-holds-p `(vowel-okurigana-char ,okuri-char))
                (nskk--handle-vowel-okuri/k okuri-char (lambda () (funcall K t)))
              (nskk--handle-consonant-okuri okuri-char (lambda () (funcall K t)))))))
      (fail))))

(defun/k nskk-convert-input-to-kana-final ()
  "Convert remaining romaji buffer to kana and call on-found with the result.
Handles trailing standalone `n' as \u3093 (hatsuon at end of input),
A standalone `n' at word boundary emits \u3093.
Always calls on-found: with \"\" when buffer is empty, with \u3093 for
standalone n, or with the converted kana (falling back to raw buffer)."
  (cond
   ((string-empty-p nskk--romaji-buffer)
    (nskk--reset-romaji-buffer)
    (succeed ""))
   ;; Standalone 'n' — the incremental converter returns :incomplete for
   ;; "n" (awaiting "na"/"nn"/etc.), but at conversion time it means \u3093.
   ((nskk--standalone-n-p nskk--romaji-buffer)
    (nskk--reset-romaji-buffer)
    (succeed "\u3093"))
   ;; General: use CPS converter; fall back to raw buffer string on failure.
   ;; call/cc captures on-found as K so inner lambdas can call it.
   (t
    (call/cc (lambda (K)
      (let ((buf nskk--romaji-buffer))
        (nskk--reset-romaji-buffer)
        (nskk-converter-convert/k buf
          (lambda (kana _remaining) (funcall K kana))
          (lambda (_romaji) (funcall K buf))
          (lambda () (funcall K buf)))))))))

(defun nskk--remove-okuri-marker (search-start preedit-end)
  "Remove the okurigana boundary marker (*) from the buffer.
Searches forward from SEARCH-START up to PREEDIT-END and deletes the
marker character when found.  Buffer position is preserved via
`save-excursion'."
  (save-excursion
    (goto-char search-start)
    (when (search-forward nskk-okurigana-marker preedit-end t)
      (delete-char (- (length nskk-okurigana-marker))))))

(defun/k nskk--extract-okuri-query (start preedit-end okuri-char)
  "Build the dictionary search query for okurigana conversion.
START is the conversion start position, PREEDIT-END is the end of preedit,
OKURI-CHAR is the triggering okurigana character.
Calls on-found with the query string; on-not-found when data is invalid."
  (let* ((text-start (and start
                          (nskk--skip-marker-pos start nskk-henkan-on-marker-regexp)))
         (text-with-marker (and text-start
                                (> preedit-end text-start)
                                (buffer-substring-no-properties text-start preedit-end)))
         (text (and text-with-marker
                    (replace-regexp-in-string nskk-okurigana-marker-regexp "" text-with-marker))))
    (if text
        (succeed (concat text (char-to-string okuri-char)))
      (fail))))

(defun nskk--apply-okuri-candidates (start text-start preedit-end candidates query)
  "Mutate buffer and state after finding okurigana candidates.
Removes the * marker, replaces ▽ with ▼, updates the conversion overlay
to show the first candidate over the reading stem, and sets active state.
START is the conversion start position (▽ marker start).
TEXT-START is the position after the ▽ marker, or nil (falls back to START).
PREEDIT-END is the buffer position before the okurigana kana was inserted.
CANDIDATES is the non-nil list of candidate strings.
QUERY is the dict lookup key stored in okurigana-query metadata."
  (let ((candidates (if (fboundp 'nskk-study-reorder)
                        (nskk-study-reorder query candidates)
                      candidates)))
    (nskk--remove-okuri-marker (or text-start start) preedit-end)
    (nskk--replace-marker-at start nskk-henkan-on-marker-regexp nskk-henkan-active-marker)
    (let ((okuri-kana-start (- preedit-end (length nskk-okurigana-marker))))
      (nskk--update-overlay (+ start (length nskk-henkan-active-marker))
                            okuri-kana-start (car candidates)))
    (nskk-with-current-state
      (nskk-set-active-candidates candidates)
      (nskk-state-put-metadata nskk-current-state 'okurigana-in-progress t)
      (nskk-state-put-metadata nskk-current-state 'okurigana-query query)
      (nskk-state-put-metadata nskk-current-state 'henkan-reading query))
    (setq nskk--henkan-count 1)))

(defun nskk--build-okuri-registration-reading (text-start preedit-end query)
  "Build the display-format reading string for okurigana dict registration.
Format: \"stem*kana\" (e.g. \"ほ*け\").
Falls back to QUERY when no okuri-kana.
Captures okuri-kana from the buffer BEFORE the * marker is removed.
TEXT-START is the position after ▽.
PREEDIT-END is the pre-okurigana position."
  (let* ((okuri-kana (buffer-substring-no-properties preedit-end (point)))
         (raw-stem   (buffer-substring-no-properties text-start preedit-end))
         (stem       (replace-regexp-in-string nskk-okurigana-marker-regexp "" raw-stem)))
    (if (string-empty-p okuri-kana)
        query
      (concat stem nskk-okurigana-marker okuri-kana))))

(defun/3k nskk--trigger-okuri-conversion (okuri-char preedit-end)
    (on-found on-not-found on-register)
  "Trigger conversion with okurigana OKURI-CHAR ending at PREEDIT-END.
PREEDIT-END is the buffer position before the okurigana kana was inserted.
ON-FOUND is called with the candidates list when a dict match is found.
ON-NOT-FOUND is called when no candidates exist and registration is cancelled.
ON-REGISTER is called after a word is successfully registered and inserted.
Searches the dictionary first; only mutates the buffer when
candidates are found."
  (nskk-debug-log "[HENKAN] trigger-okuri: okuri-char=%c" okuri-char)
  (let* ((start      (nskk--get-conversion-start))
         (text-start (and start (nskk--skip-marker-pos start nskk-henkan-on-marker-regexp))))
    (nskk--extract-okuri-query/k start preedit-end okuri-char
      (lambda (query)
        (nskk-core-search/k query :exact nil
          (lambda (candidates)
            (nskk--apply-okuri-candidates start text-start preedit-end candidates query)
            (funcall on-found candidates))
          (lambda ()
            ;; Register under the dictionary key QUERY (e.g. "ほk"), the
            ;; same key lookup uses; the "stem*kana" form is display-only.
            (let ((nskk--registration-display-reading
                   (nskk--build-okuri-registration-reading
                    text-start preedit-end query)))
              (nskk--remove-okuri-marker (or text-start start) preedit-end)
              (nskk-start-registration/k query
                (lambda (registered)
                  (if registered
                      (nskk--insert-registered-and-reset registered start on-register query)
                    (funcall on-not-found)))
                #'ignore)))))
      on-not-found)))

(defun nskk--trigger-okuri-conversion (okuri-char preedit-end)
  "Trigger conversion with okurigana OKURI-CHAR ending at PREEDIT-END.
Sync entry point; calls `nskk--trigger-okuri-conversion/k' with
no-op continuations.
See `nskk--trigger-okuri-conversion/k' for the full conversion logic."
  (nskk--trigger-okuri-conversion/k okuri-char preedit-end #'ignore #'ignore #'ignore))

;;;; Conversion Pipeline

(defun nskk--start-conv-apply-found (start end lookup-text raw-candidates numeric-info on-found)
  "Apply a successful dict search: display candidate, store state, call ON-FOUND.
START is the conversion start position (▽ marker start).  END is the preedit
end position captured before the search; used as the overlay end.
LOOKUP-TEXT is the dict lookup key (used for debug logging only).
RAW-CANDIDATES is the search result list.
NUMERIC-INFO is non-nil in numeric mode (cons of num-str and base-key);
when set, candidates are post-processed by `nskk--numeric-process-candidates'.
ON-FOUND is called with the final candidates list.

Side effects: replaces ▽ with ▼ in the buffer, updates the conversion
overlay, sets `nskk--henkan-count' to 1, and stores candidates in state."
  (let* ((base-candidates (if (and raw-candidates numeric-info)
                              (nskk--numeric-process-candidates raw-candidates (car numeric-info))
                            raw-candidates))
         (candidates (if (fboundp 'nskk-study-reorder)
                         (nskk-study-reorder lookup-text base-candidates)
                       base-candidates)))
    (nskk-debug-log "[HENKAN] candidates-found: key=%s count=%d" lookup-text (length candidates))
    (setq nskk--henkan-count 1)
    (nskk--replace-marker-at start nskk-henkan-on-marker-regexp nskk-henkan-active-marker)
    (nskk--update-overlay (+ start (length nskk-henkan-active-marker)) end (car candidates))
    (nskk-with-current-state
      (nskk-set-active-candidates candidates)
      (nskk-state-put-metadata nskk-current-state 'henkan-reading lookup-text))
    (funcall on-found candidates)))

(defun nskk--start-conv-register (text start _end on-not-found on-register)
  "Handle no-candidates in start-conversion: open dict registration.
TEXT is the preedit reading for the registration prompt.  START and END
delimit the preedit region in the buffer.
ON-NOT-FOUND is called (no args) when registration is cancelled or skipped.
ON-REGISTER is called (no args) after a word is successfully registered."
  (nskk-debug-log "[HENKAN] no-candidates: key=%s" text)
  (nskk-start-registration/k text
    (lambda (registered)
      (if registered
          (nskk--insert-registered-and-reset registered start on-register text)
        (funcall on-not-found)))
    #'ignore))

(defun nskk--insert-registered-and-reset (registered start on-done &optional reading)
  "Insert REGISTERED word at START, reset henkan state, and call ON-DONE.
Shared by all registration callbacks in the conversion pipeline.
Optional READING is the original reading used for registration; when
non-nil, an undo record is stored so `nskk-undo-kakutei' can revert
and unregister the word.
When called in abbrev mode, restores the previous Japanese input mode
via `nskk--restore-abbrev-mode'."
  (let ((was-abbrev (nskk-with-current-state
                      (eq (nskk-state-mode nskk-current-state) 'abbrev))))
    (atomic-change-group
      (delete-region start (point))
      (goto-char start)
      (insert registered))
    ;; Store undo record for registration undo.
    (when reading
      (let ((mode (nskk-with-current-state (nskk-state-mode nskk-current-state))))
        (setq nskk--last-kakutei-record
              (list :reading reading
                    :candidates (list registered)
                    :index 0
                    :committed-text registered
                    :buffer-start start
                    :buffer-end (point)
                    :mode (or mode 'hiragana)
                    :registered-p t
                    :registered-reading reading
                    :registered-word registered))))
    (nskk-henkan-do-reset)
    (nskk--restore-abbrev-mode was-abbrev)
    (when (functionp on-done) (funcall on-done))))

(defun nskk--start-conversion-normal (start on-found on-not-found on-register)
  "Execute the normal (non-okurigana) conversion path.
START is the conversion start position.  Flushes pending romaji,
extracts preedit text, handles katakana-半角 and numeric mode,
then searches the dictionary.
ON-FOUND, ON-NOT-FOUND, ON-REGISTER are the three continuations."
  (let* ((end         (progn
                        (let ((pending (nskk-convert-input-to-kana-final)))
                          (when (and (stringp pending) (not (string-empty-p pending)))
                            (insert pending)))
                        (point)))
         (text-start  (nskk--skip-marker-pos start nskk-henkan-on-marker-regexp))
         (text        (when (> end text-start)
                        (buffer-substring-no-properties text-start end)))
         (lookup-text (when text
                        (if (eq (nskk-state-get-mode) 'katakana-半角)
                            (nskk-kana-string-katakana-to-hiragana
                             (nskk-kana-hankaku-to-zenkaku text))
                          text)))
         (numeric-info (when (and lookup-text (boundp 'nskk--numeric-mode) nskk--numeric-mode)
                         (nskk--numeric-parse-reading lookup-text)))
         (search-key  (when lookup-text
                        (if numeric-info (cdr numeric-info) lookup-text))))
    (when (and text search-key)
      (nskk-<-or (raw-candidates) (nskk-core-search/k search-key nil nil)
                 (nskk--start-conv-register text start end on-not-found on-register)
        (nskk--start-conv-apply-found start end lookup-text raw-candidates numeric-info on-found)))))

(defun/3k nskk-start-conversion ()
    (on-found on-not-found on-register)
  "Start dictionary conversion for the preedit text.
Extracts preedit text between the ▽ marker and point, flushes any
pending romaji (e.g. trailing \='n\=' → ん), then searches the dictionary
via `nskk-core-search/k'.

ON-FOUND is called with the candidates list when the dict search succeeds.
ON-NOT-FOUND is called with no arguments when no candidates were found and
registration was cancelled or skipped (user entered nothing).
ON-REGISTER is called with no arguments after a word is successfully
registered via `nskk-start-registration' and inserted.

Registration is a distinct third outcome, not a subcase of not-found:
the user may cancel (→ on-not-found) or complete it (→ on-register).
Delegates to `nskk--start-conversion-normal' for the main pipeline."
  (let ((okuri (nskk-with-current-state
                 (nskk-state-get-okurigana nskk-current-state))))
    (if okuri
        (let ((preedit-end (save-excursion
                             ;; When no * marker exists (okurigana state set
                             ;; but marker already consumed), fall back to
                             ;; point instead of a bogus (1+ (point)).
                             (if (search-backward nskk-okurigana-marker nil t)
                                 (1+ (point))
                               (point)))))
          (nskk-with-current-state
            (nskk-state-set-okurigana nskk-current-state nil))
          (nskk--trigger-okuri-conversion/k okuri preedit-end on-found on-not-found on-register))
      (nskk-henkan-with-preedit start
        (nskk--start-conversion-normal start on-found on-not-found on-register)))))

(defun nskk-start-conversion ()
  "Start dictionary conversion for the preedit text.
Sync entry point; calls `nskk-start-conversion/k' with no-op continuations.
See `nskk-start-conversion/k' for the full conversion logic."
  (interactive)
  (nskk-start-conversion/k #'ignore #'ignore #'ignore))

;;;; Dictionary Registration

(defun nskk--registration-prompt (depth reading)
  "Build the minibuffer prompt string for registration at nesting DEPTH.
DEPTH 1 → \"[辞書登録] READING: \", DEPTH 2 → \"[[辞書登録]] READING: \", etc."
  (let ((open  (make-string depth ?\[))
        (close (make-string depth ?\])))
    (format "%s辞書登録%s %s: " open close (substring-no-properties reading))))

(defun nskk--read-registration-entry-with-kana (prompt)
  "Read a registration entry from the minibuffer for PROMPT with nskk-mode active.
Sets up a dedicated keymap so \\`RET' and \\`C-j' commit the current
conversion instead of exiting with a raw newline, and so \\`C-g' aborts
the registration via `abort-recursive-edit' instead of cascading to the
preedit-clear handler in `nskk-mode-map'."
  (let* ((exit-fn (lambda ()
                    (interactive)
                    (let ((phase (nskk--compute-phase)))
                      (cond
                       ((eq phase 'converting) (nskk-commit-current))
                       ((eq phase 'henkan-on) (nskk-henkan-kakutei))
                       (t (exit-minibuffer))))))
         (reg-map (let ((map (make-sparse-keymap)))
                    (set-keymap-parent map nskk-mode-map)
                    (define-key map (kbd "C-j") exit-fn)
                    (define-key map (kbd "RET") exit-fn)
                    (define-key map (kbd "C-g") #'abort-recursive-edit)
                    map)))
    (minibuffer-with-setup-hook
        (lambda ()
          (nskk-mode 1)
          (nskk--set-mode 'hiragana)
          (setq-local minor-mode-overriding-map-alist
                      (list (cons 'nskk-mode reg-map))))
      (read-from-minibuffer prompt))))

(defun nskk--read-registration-entry (reading)
  "Read a registration entry for READING from the minibuffer.
Returns the entered non-empty string, or nil if the user cancels
\(empty input or \\`C-g').
Uses `nskk-use-kana-in-registration' to choose the input method."
  (condition-case nil
      (let* ((shown (or nskk--registration-display-reading reading))
             (entry (if nskk-use-kana-in-registration
                        (nskk--read-registration-entry-with-kana
                         (nskk--registration-prompt nskk--registration-depth shown))
                      (read-from-minibuffer
                       (nskk--registration-prompt nskk--registration-depth shown)))))
        (and (not (string-empty-p entry)) entry))
    (quit nil)))

(defun nskk--commit-registration-word (reading entry)
  "Register ENTRY for READING in the dictionary and update learning state."
  (nskk-dict-register-word reading entry)
  (when (fboundp 'nskk-study-after-kakutei)
    (nskk-study-after-kakutei reading entry))
  (nskk-search-learn reading entry))

(defun nskk--run-registration-session/k (reading on-found _on-not-found)
  "Open the minibuffer for registering READING.
Calls ON-FOUND with the registered word string on success, or nil if the
user cancels.  _ON-NOT-FOUND is unused.

Manages `nskk--registration-depth' and the henkan-phase transactionally:
every cleanup step runs under `inhibit-quit', the first body or cleanup
condition is re-signaled unchanged, and terminal state is re-asserted even
when a cleanup callback mutates state before signaling.  Delegates input to
`nskk--read-registration-entry' and commit to
`nskk--commit-registration-word'.  [CPS]"
  (if (< nskk--registration-depth nskk-max-registration-depth)
      (let ((state nskk-current-state)
            (prev-phase (nskk-state-henkan-phase nskk-current-state))
            (prev-depth nskk--registration-depth)
            (result nil)
            first-condition)
        (nskk-with-current-state
          (nskk-state-force-henkan-phase state 'registration))
        (cl-incf nskk--registration-depth)
        (condition-case condition
            (progn
              (dolist (callback (nskk-prolog-presentation-actions
                                 'show-registration-badge))
                (when (fboundp callback) (funcall callback)))
              (let ((entry (nskk--read-registration-entry reading)))
                (when entry
                  (setq result entry)
                  (nskk--commit-registration-word reading entry))))
          ((error quit)
           (setq first-condition condition)))
        (let ((inhibit-quit t))
          (cl-labels ((run-cleanup
                       (thunk)
                       (condition-case condition
                           (funcall thunk)
                         ((error quit)
                          (unless first-condition
                            (setq first-condition condition))))))
            (run-cleanup (lambda ()
                           (cl-decf nskk--registration-depth)))
            (run-cleanup (lambda ()
                           (dolist (callback (nskk-prolog-presentation-actions
                                              'cleanup))
                             (when (fboundp callback) (funcall callback)))))
            (run-cleanup (lambda ()
                           (dolist (callback (nskk-prolog-presentation-actions
                                              'finalize))
                             (when (fboundp callback) (funcall callback)))))
            (run-cleanup
             (lambda ()
               (nskk-with-current-state
                 (nskk-state-force-henkan-phase state prev-phase))))

            ;; Cleanup callbacks may signal before or after mutating state.
            ;; Re-assert the session invariants without invoking them again.
            (run-cleanup (lambda ()
                           (setq nskk--registration-depth prev-depth)))
             (run-cleanup
              (lambda ()
                (setf (nskk-state-henkan-phase state) prev-phase)))))
        (when first-condition
          (signal (car first-condition) (cdr first-condition)))
        (funcall on-found result))
    (funcall on-found nil)))

(put 'nskk--run-registration-session/k 'nskk--cps-continuation-pattern :found-not-found)

(defun/k nskk-start-registration (reading)
  "Start dictionary registration for READING.
Opens a minibuffer prompt for the user to enter the desired text.
READING is the headword that could not be converted.
Supports recursive registration up to `nskk-max-registration-depth' levels:
depth 1 shows [辞書登録], depth 2 shows [[辞書登録]], etc."
  (nskk-debug-log "[HENKAN] start-registration: reading=%s" reading)
  (<- result nskk--run-registration-session reading)
  (succeed result))

(defun nskk--wrap-to-first-candidate ()
  "Reset candidate display to the first page.
Resets index to 0, restores `list' phase, and re-fires the show-candidates
hook.  Assumes `nskk-current-state' is bound."
  (let ((candidates (nskk-state-candidates nskk-current-state))
        (previous-index (nskk-state-current-index nskk-current-state))
        (previous-phase (nskk-state-henkan-phase nskk-current-state))
        (previous-count nskk--henkan-count))
    (setf (nskk-state-current-index nskk-current-state) 0)
    (setq nskk--henkan-count nskk-henkan-show-candidates-nth)
    (nskk-state-set-henkan-phase nskk-current-state 'list)
    (nskk--run-candidate-show-transaction
     candidates 0 previous-index previous-phase previous-count)))

(defun/done nskk--exhaust-candidates ()
  "Handle exhausted candidates by triggering dictionary registration.
If registration succeeds, insert the registered word and clean up state.
If the user cancels, wrap around to the first candidate in list display."
  ;; Dismiss the candidate list BEFORE opening the registration buffer so
  ;; the UI is clean during the nested registration session.
  (nskk--dismiss-candidate-list)
  (let* ((start (nskk--get-conversion-start))
         (text-start (when start
                       (nskk--skip-marker-pos start nskk-henkan-active-marker-regexp)))
         (text (when (and text-start (> (point) text-start))
                 (buffer-substring-no-properties text-start (point)))))
    (if text
        (let* ((query (and (nskk-with-current-state
                             (nskk-state-get-metadata nskk-current-state 'okurigana-in-progress))
                           (nskk-with-current-state
                             (nskk-state-get-metadata nskk-current-state 'okurigana-query))))
               ;; Register under the dictionary key ("ほk" for okurigana,
               ;; the plain reading otherwise) — the key lookup uses.
               (reading (if (stringp query) query text))
               (nskk--registration-display-reading
                (when (stringp query)
                  (let ((okuri-kana (buffer-substring-no-properties
                                     (overlay-end nskk--conversion-overlay) (point)))
                        (stem (substring query 0 (- (length query) 1))))
                    (concat stem nskk-okurigana-marker okuri-kana)))))
          (nskk-start-registration/k reading
            (lambda (registered)
              (if registered
                  (progn
                    (nskk-delete-overlay nskk--conversion-overlay)
                    (nskk--insert-registered-and-reset registered start #'ignore reading))
                ;; Registration cancelled: wrap back to first candidate page.
                (nskk--wrap-to-first-candidate)))
            #'ignore))
      ;; No preedit text: wrap back to first candidate page.
      (nskk--wrap-to-first-candidate))))

;;;; Dynamic Completion (動的補完)

(defun nskk--dcomp-search-prefix (prefix)
  "Search for dictionary keys with PREFIX for dynamic completion.
Returns strict prefix matches with user entries before system entries.
Keys present in both dictionaries are retained only once."
  (let ((keys nil)
        (seen (make-hash-table :test (quote equal))))
    (dolist (pair (nskk-prolog-trie-prefix-search (quote user-dict-entry) 2 prefix))
      (let ((key (car pair)))
        (when (and key (not (equal key prefix)) (not (gethash key seen)))
          (puthash key t seen)
          (push key keys))))
    (setq keys (nreverse keys))
    (let ((sys-keys nil))
      (dolist (pair (nskk-prolog-trie-prefix-search (quote system-dict-entry) 2 prefix))
        (let ((key (car pair)))
          (when (and key (not (equal key prefix)) (not (gethash key seen)))
            (puthash key t seen)
            (push key sys-keys))))
      (nconc keys (nreverse sys-keys)))))

(defun nskk--dcomp-replace-preedit (new-text)
  "Replace the current preedit text with NEW-TEXT for dynamic completion."
  (let ((start (nskk--get-conversion-start)))
    (when start
      (let ((text-start (nskk--skip-marker-pos start nskk-henkan-on-marker-regexp)))
        (atomic-change-group
          (delete-region text-start (point))
          (goto-char text-start)
          (insert new-text))))))

;;;; Dynamic Completion Multiple Display

(defun nskk--dcomp-multiple-build-string (candidates selected-index prefix)
  "Build overlay after-string for dcomp multiple display.
CANDIDATES is the list of reading strings.
SELECTED-INDEX is the currently selected 0-based index.
PREFIX is the original preedit prefix for highlighting the trailing part.
Returns a multi-line string starting with \\n."
  (let* ((rows (min (or (and (boundp 'nskk-dcomp-multiple-rows)
                             nskk-dcomp-multiple-rows)
                        7)
                    (length candidates)))
         (display-candidates (cl-subseq candidates 0 rows))
         (prefix-len (length prefix)))
    (mapconcat
     (lambda (pair)
       (let* ((idx (car pair))
              (cand (substring-no-properties (cdr pair)))
              (is-selected (= idx selected-index))
              (prefix-part (if (and (> (length cand) prefix-len)
                                   (string-prefix-p prefix cand))
                               (substring cand 0 prefix-len)
                             cand))
              (trailing-part (if (and (> (length cand) prefix-len)
                                     (string-prefix-p prefix cand))
                                 (substring cand prefix-len)
                               ""))
              (cand-str (concat
                         (propertize prefix-part
                                     'face (if is-selected
                                               'nskk-dcomp-multiple-selected-face
                                             'nskk-dcomp-multiple-face))
                         (propertize trailing-part
                                     'face (if is-selected
                                               'nskk-dcomp-multiple-selected-face
                                             'nskk-dcomp-multiple-trailing-face)))))
         (concat "  " cand-str)))
     (cl-loop for i from 0
              for c in display-candidates
              collect (cons i c))
     "\n")))

(defun nskk--dcomp-multiple-show (candidates selected-index prefix)
  "Display dcomp multiple candidate list below preedit.
CANDIDATES is the full list; SELECTED-INDEX is current selection; PREFIX
is the original preedit prefix for display styling."
  (when (and (boundp 'nskk-dcomp-multiple-activate)
             nskk-dcomp-multiple-activate
             candidates)
    (let* ((after-str (nskk--dcomp-multiple-build-string
                       candidates selected-index prefix))
           (anchor (or (and (overlayp nskk--conversion-overlay)
                            (overlay-end nskk--conversion-overlay))
                       (point))))
      (nskk-ensure-overlay nskk--dcomp-multiple-overlay anchor anchor
        'after-string (concat "\n" after-str)
        'priority 99))))

(defun/done nskk-dynamic-complete ()
  "Complete the preedit reading from dictionary prefix matches.
Called when Tab is pressed in preedit (▽) phase.  Searches
for dict keys that start with the current reading and replaces
the preedit with the first match.  Subsequent calls cycle through
all matches.
When `nskk-dcomp-multiple-activate' is non-nil, also displays all
matching candidates below the preedit text."
  (let ((preedit (nskk-preedit-string)))
    (when (and preedit (not (string-empty-p preedit)))
      (cond
       ;; Cycling: current preedit is the original prefix or a completion
       ((and nskk--dcomp-candidates
             (or (equal preedit nskk--dcomp-prefix)
                 (member preedit nskk--dcomp-candidates)))
        (setq nskk--dcomp-index
              (mod (1+ nskk--dcomp-index)
                   (length nskk--dcomp-candidates)))
        (nskk--dcomp-replace-preedit
         (nth nskk--dcomp-index nskk--dcomp-candidates))
        ;; Discard pending romaji — the completed reading supersedes it.
        (nskk--reset-romaji-buffer)
        ;; Update multiple display if enabled
        (nskk--dcomp-multiple-show nskk--dcomp-candidates
                                   nskk--dcomp-index
                                   nskk--dcomp-prefix))
       ;; Fresh search
       (t
        (let ((matches (nskk--dcomp-search-prefix preedit)))
          (when matches
            (setq nskk--dcomp-prefix preedit
                  nskk--dcomp-candidates matches
                  nskk--dcomp-index 0)
            (nskk--dcomp-replace-preedit (car matches))
            ;; Discard pending romaji — the completed reading supersedes it.
            (nskk--reset-romaji-buffer)
            ;; Show multiple candidates if enabled
            (nskk--dcomp-multiple-show matches 0 preedit))))))))

(defun nskk-completion-at-point ()
  "CAPF backend for dynamic completion in preedit (▽) mode.
Returns a completion spec when the cursor is inside an active preedit
region, or nil otherwise.  The completion table performs prefix search
against both user and system dictionaries.

Intended to be added to `completion-at-point-functions' (buffer-local)
when `nskk-dcomp-style' is \\='capf."
  (when-let* ((start (nskk--get-conversion-start))
              (text-start (nskk--skip-marker-pos start nskk-henkan-on-marker-regexp)))
    (when (> (point) text-start)
      (list text-start
            (point)
            (completion-table-dynamic
             (lambda (prefix)
               (nskk--dcomp-search-prefix prefix)))
            :exclusive 'no))))

;;;; SKK Numeric Conversion (数値変換)

(defconst nskk--kanji-digits
  ["〇" "一" "二" "三" "四" "五" "六" "七" "八" "九"]
  "Kanji digit characters indexed by numeric value.")

(defun nskk--numeric-parse-reading (reading)
  "Parse numeric READING like \"#1ko\" into (NUM-STR . BASE-KEY).
Extracts the digit(s) after '#' and returns the number string
and the base dictionary key with '#' prefix.
Example: \"#1ko\" → (\"1\" . \"#ko\"), \"#123ji\" → (\"123\" . \"#ji\")."
  (when (string-match "^#\\([0-9]+\\)\\(.*\\)$" reading)
    (cons (match-string 1 reading)
          (concat "#" (match-string 2 reading)))))

(defun nskk--numeric-to-kanji (num-str)
  "Convert NUM-STR to kanji numerals digit-by-digit (漢数字).
Each digit is independently converted: \"12\" → \"一二\"."
  (mapconcat (lambda (c)
               (aref nskk--kanji-digits (- c ?0)))
             num-str ""))

(defun nskk--numeric-to-fullwidth (num-str)
  "Convert NUM-STR to full-width Arabic digits.
Each digit is shifted to the full-width Unicode range: \"1\" → \"１\"."
  (mapconcat (lambda (c)
               (char-to-string (+ c #xFEE0)))
             num-str ""))

(defun nskk--n-to-kanji-place (n)
  "Recursively convert positive integer N to kanji with place values.
Leading 一 is dropped for 十, 百, 千 (e.g. 1000 → 千, not 一千),
but kept for 万 and higher (e.g. 10000 → 一万)."
  (cond
   ((>= n 10000)
    (let* ((q (/ n 10000)) (r (% n 10000)))
      (concat (nskk--n-to-kanji-place q) "万"
              (if (= r 0) "" (nskk--n-to-kanji-place r)))))
   ((>= n 1000)
    (let* ((q (/ n 1000)) (r (% n 1000)))
      (concat (if (= q 1) "" (aref nskk--kanji-digits q)) "千"
              (if (= r 0) "" (nskk--n-to-kanji-place r)))))
   ((>= n 100)
    (let* ((q (/ n 100)) (r (% n 100)))
      (concat (if (= q 1) "" (aref nskk--kanji-digits q)) "百"
              (if (= r 0) "" (nskk--n-to-kanji-place r)))))
   ((>= n 10)
    (let* ((q (/ n 10)) (r (% n 10)))
      (concat (if (= q 1) "" (aref nskk--kanji-digits q)) "十"
              (if (= r 0) "" (aref nskk--kanji-digits r)))))
   (t (aref nskk--kanji-digits n))))

(defun nskk--numeric-to-place-values (num-str)
  "Convert NUM-STR to kanji numerals with place values (漢数字位取り).
Examples: \"10\" → \"十\", \"100\" → \"百\", \"1024\" → \"千二十四\"."
  (let ((n (string-to-number num-str)))
    (if (= n 0) "〇"
      (nskk--n-to-kanji-place n))))

(defun nskk--numeric-convert (num-str type)
  "Convert numeric string NUM-STR according to SKK numeric type code TYPE.
TYPE is an integer:
  0 = literal (no change)
  1 = full-width Arabic (全角数字)
  2 = kanji digit-by-digit (漢数字)
  3 = kanji with place values (漢数字位取り)
  4 = positional (序数)
  8 = comma-grouped decimal"
  (pcase type
    (0 num-str)                                    ; literal
    (1 (nskk--numeric-to-fullwidth num-str))       ; full-width
    ((or 2 4) (nskk--numeric-to-kanji num-str))   ; kanji digit-by-digit
    (3 (nskk--numeric-to-place-values num-str))    ; kanji with place values
    (_ num-str)))

(defun nskk--numeric-process-candidate (candidate num-str)
  "Process CANDIDATE by replacing #N patterns with converted NUM-STR.
Each #N in CANDIDATE is replaced with `nskk--numeric-convert' applied
to NUM-STR with conversion type N."
  (let ((result candidate))
    (while (string-match "#\\([0-9]\\)" result)
      (let* ((type (string-to-number (match-string 1 result)))
             (converted (nskk--numeric-convert num-str type)))
        (setq result (replace-match converted t t result))))
    result))

(defun nskk--numeric-process-candidates (candidates num-str)
  "Process CANDIDATES by replacing #N patterns with converted NUM-STR."
  (mapcar (lambda (c) (nskk--numeric-process-candidate c num-str))
          candidates))

(defun nskk--normalize-for-lookup (text)
  "Normalize TEXT to hiragana for dictionary lookup based on current input mode.
Reads the mode from `nskk-current-state' and delegates to
`nskk-kana-normalize-for-lookup'.  Falls back to identity for unknown modes."
  (nskk-kana-normalize-for-lookup
   text
   (nskk-with-current-state (nskk-state-mode nskk-current-state))))

(defvar nskk--henkan-initialized nil
  "Non-nil when henkan Prolog predicates have been initialized.")

(defun/done nskk-henkan-initialize ()
  "Initialize henkan pipeline Prolog predicates.
Idempotent: subsequent calls are no-ops."
  (unless nskk--henkan-initialized
    ;; Core search type mapping
    (nskk-prolog-define-fact-table core-search-type (:arity 2 :index :hash)
      (:exact   dict-lookup)
      (:prefix  prefix-search)
      (:partial partial-search))

    ;; dict-lookup backend order (declarative; actual dispatch uses nskk--optional-*-lookup/k)
    (nskk-prolog-define-fact-table search-backend (:arity 2 :index :hash)
      (1 dict-lookup)
      (2 skkserv-lookup)
      (3 program-dict-lookup))

    ;; Converting phase facts — authoritative list of phases where a candidate
    ;; conversion is in progress (▼ or list display).
    (nskk-prolog-define-fact-table converting-phase (:arity 1 :index :hash)
      (active) (list) (registration))

    ;; Okurigana character classification: uppercase A-Z → lowercase equivalent
    (nskk-prolog-set-index 'okurigana-char 2 :hash)
    (nskk-prolog-bulk-facts okurigana-char
      (mapcar (lambda (c) (list c (downcase c))) (number-sequence ?A ?Z)))

    ;; Vowel okurigana chars: immediately convertible without a following character.
    ;; Separating this as a fact table (data) rather than a local memq (logic)
    ;; allows Prolog-level composition with okurigana-char rules.
    (nskk-prolog-define-fact-table vowel-okurigana-char (:arity 1 :index :hash)
      (?a) (?i) (?u) (?e) (?o))

    ;; Candidate navigation action rules
    (nskk-prolog-set-index 'candidate-nav-next-action 3 :list)
    (nskk-prolog-<- (candidate-nav-next-action \?count \?threshold select-next)
      (< \?count \?threshold))
    (nskk-prolog-<- (candidate-nav-next-action \?count \?threshold show-list-next)
      (>= \?count \?threshold))

    (nskk-prolog-define-fact-table candidate-nav-prev-action (:arity 2 :index :hash)
      (list-active show-list-prev)
      (not-active  select-prev))

    ;; Search result action dispatch
    (nskk-prolog-define-fact-table search-result-action (:arity 2 :index :hash)
      (has-candidates show-overlay)
      (no-candidates  start-registration))

    ;; Convert-or-commit action dispatch
    (nskk-prolog-define-fact-table convert-or-commit-action (:arity 2 :index :hash)
      (converting     commit-current)
      (not-converting start-conversion))

    ;; Preedit phase facts — the only preedit phase is `on'.
    ;; Used by nskk.el and nskk-input.el to guard preedit-state queries.
    (nskk-prolog-define-fact-table preedit-phase (:arity 1 :index :hash)
      (on))

    ;; Script-toggle direction for q-key/AZIK-toggle-key in ▽ preedit mode.
    ;; Maps the current input mode to the target script for kakutei-convert-script.
    ;; Queried by `nskk-henkan-kakutei-convert-script' at commit time.
    (nskk-prolog-define-fact-table script-toggle (:arity 2 :index :hash)
      (hiragana katakana)
      (katakana hiragana))

    ;; Script-to-CPS-converter mapping for dynamic script conversion dispatch.
    ;; Maps target script name to the CPS /k converter function symbol.
    (nskk-prolog-define-fact-table script-converter (:arity 2 :index :hash)
      (katakana nskk-kana-string-hiragana-to-katakana/k)
      (hiragana nskk-kana-string-katakana-to-hiragana/k))

    ;; Overlay update phase guard
    (nskk-prolog-define-fact-table should-update-overlay (:arity 1 :index :hash)
      (active)
      (list))

    ;; clearable-input-var/1 is defined in nskk-input-initialize (nskk-input.el)
    ;; because the listed symbols are all nskk-input internal variables.
    ;; Queried by `nskk--clear-conversion-context' below.

    ;; Disable/cleanup action dispatch: maps henkan phase to the cleanup
    ;; action to perform when nskk-mode is disabled or context is reset.
    ;; Queried by the nskk-mode disable hook in nskk.el.
    (nskk-prolog-define-fact-table disable-cleanup (:arity 2 :index :hash)
      (active       cancel-conversion)
      (list         cancel-conversion)
      (on           cancel-preedit)
      (registration cancel-preedit))

    (setq nskk--henkan-initialized t)))

(provide 'nskk-henkan)

;;; nskk-henkan.el ends here
