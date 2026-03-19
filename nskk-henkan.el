;;; nskk-henkan.el --- Conversion pipeline for NSKK -*- lexical-binding: t; -*-

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
(declare-function nskk-prolog-trie-prefix-search "nskk-prolog")
(declare-function nskk-converter-convert/k "nskk-converter" (romaji on-match on-incomplete on-fail))

;; From nskk-input.el (loaded after nskk-henkan.el)
(defvar nskk--numeric-mode)

;;;; Dynamic Completion State

(defvar nskk--dcomp-multiple-overlay)  ;; defined in nskk-state.el

(defvar-local nskk--dcomp-prefix nil
  "The original preedit prefix used for dynamic completion search.")

(defvar-local nskk--dcomp-candidates nil
  "List of reading strings matching `nskk--dcomp-prefix'.")

(defvar-local nskk--dcomp-index 0
  "Current cycling index into `nskk--dcomp-candidates'.")

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

(defun nskk--dismiss-candidate-list ()
  "Dismiss the candidate list display and clear list-active state.
Runs `nskk-henkan-hide-candidates-functions' hooks and resets
`nskk--henkan-candidate-list-active'.  Called by any operation that
exits active candidate list display: cancel, rollback, commit, exhaustion."
  (run-hook-with-args 'nskk-henkan-hide-candidates-functions)
  (setq nskk--henkan-candidate-list-active nil))

(defun/done nskk-henkan-do-reset ()
  "Reset all henkan conversion state after a commit or registration.
Clears: conversion-start-marker, pending-romaji overlay, romaji-buffer,
henkan-count, candidate list display, and all state fields (candidates,
okurigana, phase) via `nskk-reset-henkan-state'."
  (nskk--clear-conversion-start-marker)
  (nskk--reset-romaji-buffer)
  (setq nskk--henkan-count 0)
  (nskk--dismiss-candidate-list)
  (nskk-with-current-state
    (nskk-reset-henkan-state)))

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
  (if (not (stringp key))
      (fail)
    (let* ((search-type (or type :exact))
           (action (nskk-prolog-query-value
                    `(core-search-type ,search-type ,'\?a) '\?a)))
      (nskk-debug-log "[HENKAN] search: key=%s type=%s" key (or type 'exact))
      (pcase action
        ('dict-lookup
         ;; Flat fallback chain:
         ;;   kakutei-dict (confirmed, optional) → local dict → skkserv
         ;;   → builtin-handlers → program-dict.
         ;; Kakutei dictionary is checked first: if it returns a single
         ;; confirmed candidate, it is committed immediately without
         ;; showing the candidate selection menu.
         ;; Enable-flag guards live inside each backend's own /k function.
         ;; fboundp guards in the optional-* wrappers handle unloaded modules.
         (<-or result nskk--optional-kakutei-lookup key
           :found (succeed result)
           :fail  (<-or result2 nskk-dict-lookup key
             :found (succeed result2)
             :fail  (<-or r nskk--optional-server-lookup key
               :found (succeed r)
               :fail  (<-or b nskk--optional-program-dict-builtin-lookup key
                 :found (succeed b)
                 :fail  (<-or p nskk--optional-program-dict-lookup key
                   :found (succeed p)
                   :fail  (fail)))))))
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
        (_ (signal 'nskk-henkan-unknown-search-type (list search-type)))))))

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
Queries the `converting-phase/1' Prolog table for the current henkan phase."
  (if (and (boundp 'nskk-current-state)
           nskk-current-state
           (nskk-state-p nskk-current-state)
           (nskk-prolog-holds-p
            `(converting-phase ,(nskk-state-henkan-phase nskk-current-state))))
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
Plain vowel kana: あいうえおアイウエオ or ー.
In AZIK mode, `:' after a plain vowel kana should produce ー via the
romaji table rather than arming colon-okurigana."
  (and (string-empty-p nskk--romaji-buffer)
       (let ((start (nskk--get-conversion-start)))
         (when start
           (let ((text-start (nskk--skip-marker-pos start nskk-henkan-on-marker-regexp)))
             (and (> (point) text-start)
                  (memq (char-before (point))
                        '(?あ ?い ?う ?え ?お
                          ?ア ?イ ?ウ ?エ ?オ
                          ?ー))))))))

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

(defun nskk--clear-conversion-context ()
  "Clear all conversion and input context for mode switching or mode disable.
Called by `nskk--disable' (mode teardown) and mode-switch commands.
Clears: conversion overlay, candidate list display, inline overlay (if
present), conversion-start-marker, romaji-buffer, dynamic completion
state (dcomp-candidates, dcomp-prefix, dcomp-index, dcomp-multiple-overlay),
all AZIK/sticky/numeric input state variables via the `clearable-input-var/1'
Prolog fact table, and the current `nskk-state' henkan phase and candidates
via `nskk-reset-henkan-state'.
Does not reset the input mode."
  (nskk-delete-overlay nskk--conversion-overlay)
  (nskk--dismiss-candidate-list)
  ;; Clear inline display if enabled
  (when (fboundp 'nskk-inline-hide)
    (nskk-inline-hide))
  (nskk-when-bound-and nskk--conversion-start-marker markerp
    (set-marker nskk--conversion-start-marker nil))
  (nskk-when-bound nskk--romaji-buffer
    (nskk--reset-romaji-buffer))
  ;; Clear dynamic completion state and multiple display
  (setq nskk--dcomp-candidates nil
        nskk--dcomp-prefix nil
        nskk--dcomp-index 0)
  (nskk-delete-overlay nskk--dcomp-multiple-overlay)
  ;; Clear all registered input state variables via Prolog fact table.
  ;; The clearable-input-var/1 table is defined by
  ;; `nskk-input-initialize' in nskk-input.el.
  (dolist (sym (nskk-prolog-query-all-values '(clearable-input-var \?v) '\?v))
    (when (boundp sym) (set sym nil)))
  (nskk-with-current-state
    (nskk-reset-henkan-state)))

;;;; Kakutei (Commit Preedit As-Is)

(defun/done nskk-henkan-kakutei ()
  "Commit preedit text as-is without dictionary conversion (確定).
Removes the henkan-on marker (▽), clears the conversion start marker,
resets the romaji buffer, clears AZIK colon-okurigana pending state,
and clears the henkan phase."
  (let ((start (nskk--get-conversion-start)))
    (when start
      (nskk--delete-marker-at start nskk-henkan-on-marker-regexp)))
  (nskk--clear-conversion-start-marker)
  (nskk--reset-romaji-buffer)
  ;; Clear AZIK colon-okurigana state that may have been armed in preedit.
  ;; Without this, navigating away after arming colon-okurigana leaves
  ;; nskk--azik-colon-okuri-pending set, causing the next keypress to
  ;; misroute as colon-pending with no valid preedit context.
  (when (boundp 'nskk--azik-colon-okuri-pending)
    (setq nskk--azik-colon-okuri-pending nil))
  (when (boundp 'nskk--azik-colon-okuri-deferred)
    (setq nskk--azik-colon-okuri-deferred nil))
  (nskk-with-current-state
    (nskk-state-set-henkan-phase nskk-current-state nil)))

(defun nskk--replace-preedit-with-converted (text-start start converted)
  "Replace preedit at TEXT-START with CONVERTED text and remove ▽ marker at START."
  (delete-region text-start (point))
  (nskk--delete-marker-at start nskk-henkan-on-marker-regexp)
  (insert converted))

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
  (nskk-with-current-state
    (nskk-state-set-henkan-phase nskk-current-state nil)))

;;;; Conversion Control

;;;###autoload
(defun/done nskk-convert ()
  "Start conversion when preedit text exists.
Uses `nskk-henkan-with-preedit' to guard on preedit presence."
  :interactive t
  (nskk-henkan-with-preedit _start
    (nskk-start-conversion)))

;;;###autoload
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

;;;###autoload
(defun/done nskk-cancel-conversion-to-reading ()
  "Cancel active conversion, restoring the kana reading to the buffer.
Unlike `nskk-rollback-conversion', does NOT return to preedit (▽) state.
Removes the ▼ marker and the overlay, resets all conversion state, and
leaves the kana reading text in the buffer without any preedit marker.
Used by the DEL key handler."
  (when (nskk-converting-p)
    (let ((start (nskk--get-conversion-start)))
      ;; Delete overlay (buffer content becomes visible again: ▼ + kana reading)
      (nskk-delete-overlay nskk--conversion-overlay)
      ;; Remove only the ▼ marker character(s) at start, keeping kana reading
      (when start
        (save-excursion
          (goto-char start)
          (when (looking-at nskk-henkan-active-marker-regexp)
            (delete-region start (match-end 0)))))
      ;; Clear all conversion state
      (nskk--clear-conversion-start-marker)
      (nskk--reset-romaji-buffer)
      (setq nskk--henkan-count 0)
      (nskk--dismiss-candidate-list)
      (nskk-with-current-state
        (nskk-reset-henkan-state)))))

;;;###autoload
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
  "Restore previous Japanese mode when cancelling from abbrev preedit.
WAS-ABBREV is non-nil when the active mode was abbrev at cancel time.
Uses setf directly on the struct slot to avoid updating previous-mode
\(this is a restore, not a user-initiated mode switch).
No-op when WAS-ABBREV is nil or previous-mode is nil or abbrev."
  (when was-abbrev
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
                       (eq (nskk-state-mode nskk-current-state) 'abbrev))))
    ;; Remove preedit text including the ▽ marker
    (when start
      (delete-region start (point))
      (goto-char start))
    ;; Reset all preedit state
    (nskk--clear-conversion-start-marker)
    (nskk--reset-romaji-buffer)
    (setq nskk--henkan-count 0)
    ;; Clear okurigana state (including stale AZIK colon-okurigana).
    ;; nskk-reset-henkan-state also clears henkan-phase; use it here so
    ;; that a cancelled colon-okurigana sequence (Ka:) does not leave
    ;; nskk-state-get-okurigana returning a stale value on the next SPC.
    (nskk-with-current-state
      (nskk-reset-henkan-state))
    ;; Clear buffer-local AZIK colon-okurigana pending/deferred state.
    ;; These are also enumerated in clearable-input-var/1 but cancel-preedit
    ;; does not go through nskk--clear-conversion-context, so reset them here
    ;; directly to avoid a stale colon-pending arm surviving into the next preedit.
    (when (boundp 'nskk--azik-colon-okuri-pending)
      (setq nskk--azik-colon-okuri-pending nil))
    (when (boundp 'nskk--azik-colon-okuri-deferred)
      (setq nskk--azik-colon-okuri-deferred nil))
    (when (boundp 'nskk--azik-sokuon-okuri-kana-pending)
      (setq nskk--azik-sokuon-okuri-kana-pending nil))
    ;; Restore abbrev mode if applicable
    (nskk--restore-abbrev-mode was-abbrev)))

;;;###autoload
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
                           (overlay-end nskk--conversion-overlay))))
        ;; Replace ▼ with ▽ to return to preedit display.
        (when start
          (nskk--replace-marker-at start nskk-henkan-active-marker-regexp
                                   nskk-henkan-on-marker))
        ;; Clear candidate overlay (don't clear start marker — we're back in preedit)
        (nskk-delete-overlay nskk--conversion-overlay)
        (nskk--reset-romaji-buffer)
        (setq nskk--henkan-count 0)
        (nskk--dismiss-candidate-list)
        ;; Restore to preedit (on) phase -- C-g/DEL from ▼ returns to ▽
        (nskk-with-current-state
          (nskk-state-set-henkan-phase nskk-current-state 'on))
        (when (boundp 'nskk--azik-sokuon-okuri-kana-pending)
          (setq nskk--azik-sokuon-okuri-kana-pending nil))
        ;; Reposition point to end of reading kana if it
        ;; drifted outside the conversion region.  This ensures nskk--has-preedit
        ;; returns t and the preedit state is fully functional after rollback.
        (when (and preedit-end start
                   (or (< (point) (+ start (length nskk-henkan-on-marker)))
                       (> (point) preedit-end)))
          (goto-char preedit-end))))))

;;;; Candidate Navigation

;;;###autoload
(defun/k nskk-next-candidate ()
  "Select next conversion candidate.
For the first N-1 candidates (N = `nskk-henkan-show-candidates-nth'),
show candidates inline one-by-one with the ▼ overlay.  On the Nth press,
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

;;;###autoload
(defun/k nskk-previous-candidate ()
  "Select previous conversion candidate.
In candidate list display mode, shows the previous page.
In inline mode, decrements the counter and shows the previous candidate.
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

;;;###autoload
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
  ;; Inline nskk-with-conversion-context + nskk-with-current-state so the
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
      ;; Delete overlay before buffer modification to avoid stale display.
      (nskk-delete-overlay nskk--conversion-overlay)
      (when (and start candidate)
        ;; Delete from start to end: covers reading kana under the overlay
        ;; AND any okurigana kana between overlay-end and point.
        ;; Re-insert okuri-kana explicitly so point ends after it.
        (delete-region start end)
        (goto-char start)
        ;; Strip text properties (e.g. nskk-no-learn from built-in program dict
        ;; handlers) before inserting into the buffer.
        (insert (substring-no-properties candidate))
        (when okuri-kana
          (insert okuri-kana)))
      ;; Clear all conversion state (includes candidate list dismissal).
      (nskk-henkan-do-reset)
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
        ;; Show inline candidate if configured
        (when (fboundp 'nskk-inline-show-candidate)
          (nskk-inline-show-candidate candidate))))))

(defun nskk--show-candidate-list-next ()
  "Show next page of candidates in overlay list below the conversion region.
When all candidates are exhausted, trigger dictionary registration.
This function must be called from within a `nskk-with-conversion-context' body
where `nskk-current-state' is guaranteed valid."
  (let* ((candidates (nskk-state-candidates nskk-current-state))
         (current (nskk-state-current-index nskk-current-state))
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
      (run-hook-with-args 'nskk-henkan-show-candidates-functions candidates next-start)
      (setq nskk--henkan-candidate-list-active t))))

(defun nskk--show-candidate-list-prev ()
  "Show previous page of candidates in overlay list below the conversion region.
This function must be called from within a `nskk-with-conversion-context' body
where `nskk-current-state' is guaranteed valid."
  (let* ((candidates (nskk-state-candidates nskk-current-state))
         (current (nskk-state-current-index nskk-current-state))
         (per-page (min nskk-henkan-number-to-display-candidates
                        (length nskk-henkan-show-candidates-keys)))
         (prev-start (- current per-page)))
    (when (< prev-start 0)
      (setq prev-start 0))
    (setf (nskk-state-current-index nskk-current-state) prev-start)
    (run-hook-with-args 'nskk-henkan-show-candidates-functions candidates prev-start)
    (setq nskk--henkan-candidate-list-active t)))

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
  (nskk--remove-okuri-marker (or text-start start) preedit-end)
  (nskk--replace-marker-at start nskk-henkan-on-marker-regexp nskk-henkan-active-marker)
  (let ((okuri-kana-start (- preedit-end (length nskk-okurigana-marker))))
    (nskk--update-overlay (+ start (length nskk-henkan-active-marker))
                          okuri-kana-start (car candidates)))
  (nskk-with-current-state
    (nskk-set-active-candidates candidates)
    (nskk-state-put-metadata nskk-current-state 'okurigana-in-progress t)
    (nskk-state-put-metadata nskk-current-state 'okurigana-query query))
  (setq nskk--henkan-count 1))

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
            (let ((reading (nskk--build-okuri-registration-reading
                            text-start preedit-end query)))
              (nskk--remove-okuri-marker (or text-start start) preedit-end)
              (nskk-start-registration/k reading
                (lambda (registered)
                  (if registered
                      (nskk--insert-registered-and-reset registered start on-register)
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
  (let ((candidates (if (and raw-candidates numeric-info)
                        (nskk--numeric-process-candidates raw-candidates (car numeric-info))
                      raw-candidates)))
    (nskk-debug-log "[HENKAN] candidates-found: key=%s count=%d" lookup-text (length candidates))
    (setq nskk--henkan-count 1)
    (nskk--replace-marker-at start nskk-henkan-on-marker-regexp nskk-henkan-active-marker)
    (nskk--update-overlay (+ start (length nskk-henkan-active-marker)) end (car candidates))
    (nskk-with-current-state
      (nskk-set-active-candidates candidates))
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
          (nskk--insert-registered-and-reset registered start on-register)
        (funcall on-not-found)))
    #'ignore))

(defun nskk--insert-registered-and-reset (registered start on-done)
  "Insert REGISTERED word at START, reset henkan state, and call ON-DONE.
Shared by all registration callbacks in the conversion pipeline."
  (delete-region start (point))
  (goto-char start)
  (insert registered)
  (nskk-henkan-do-reset)
  (when (functionp on-done) (funcall on-done)))

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
                             (search-backward nskk-okurigana-marker nil t)
                             (1+ (point)))))
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
    (format "%s辞書登録%s %s: " open close reading)))

(defun nskk--run-registration-session (reading)
  "Open the minibuffer for registering READING; return the entered string or nil.
Manages `nskk--registration-depth' and restores the henkan-phase via
`unwind-protect', so depth and phase are always consistent on exit.

`unwind-protect' is mandatory here because the user can abort at any time
via \[keyboard-quit] or if an error is signalled inside
`read-from-minibuffer'.  Without the cleanup form the depth counter would
remain incremented, blocking future registration attempts.

`nskk--registration-depth' tracks nested (recursive) registration: each
call increments it before opening the minibuffer and decrements it in the
cleanup form, regardless of how the minibuffer session ends.  This allows
the user to register a word for an unknown reading that itself appeared
during registration (e.g. registering \"かんじ\" → user types \"漢字\" →
\"漢\" is unknown → nested call for \"かん\"), up to
`nskk-max-registration-depth' levels.

The cleanup form guarantees two invariants on exit:
  1. `nskk--registration-depth' is decremented (preventing leaks).
  2. The henkan-phase is restored to its value before registration."
  (when (< nskk--registration-depth nskk-max-registration-depth)
    (let ((prev-phase (nskk-state-henkan-phase nskk-current-state))
          (result nil))
      (nskk-with-current-state
        (nskk-state-force-henkan-phase nskk-current-state 'registration))
      (cl-incf nskk--registration-depth)
      ;; Show inline registration badge when inline display is enabled
      (when (fboundp 'nskk-inline-show-registration-badge)
        (nskk-inline-show-registration-badge))
      (unwind-protect
          (let ((entry (read-from-minibuffer
                        (nskk--registration-prompt nskk--registration-depth reading))))
            (when (not (string-empty-p entry))
              (setq result entry)
              (nskk-dict-register-word reading entry)))
        (cl-decf nskk--registration-depth)
        ;; Hide inline badge on exit
        (when (fboundp 'nskk-inline-hide)
          (nskk-inline-hide))
        (nskk-with-current-state
          (nskk-state-force-henkan-phase nskk-current-state prev-phase)))
      result)))

(defun/k nskk-start-registration (reading)
  "Start dictionary registration for READING.
Opens a minibuffer prompt for the user to enter the desired text.
READING is the headword that could not be converted.
Supports recursive registration up to `nskk-max-registration-depth' levels:
depth 1 shows [辞書登録], depth 2 shows [[辞書登録]], etc.
Returns the registered word on success, or nil if the user cancels
or if the maximum nesting depth has been reached."
  (nskk-debug-log "[HENKAN] start-registration: reading=%s" reading)
  ;; Normalize: succeed with nil when cancelled (empty string), so
  ;; callbacks can use `(if registered ...)' without treating "" as success.
  (let* ((result     (nskk--run-registration-session reading))
         (normalized (and (stringp result) (not (string-empty-p result)) result)))
    (succeed normalized)))

(defun nskk--wrap-to-first-candidate ()
  "Reset candidate display to the first page.
Resets index to 0, restores `list' phase, and re-fires the show-candidates
hook.  Assumes `nskk-current-state' is bound."
  (let ((candidates (nskk-state-candidates nskk-current-state)))
    (setf (nskk-state-current-index nskk-current-state) 0)
    (setq nskk--henkan-count nskk-henkan-show-candidates-nth)
    (nskk-state-set-henkan-phase nskk-current-state 'list)
    (run-hook-with-args 'nskk-henkan-show-candidates-functions candidates 0)
    (setq nskk--henkan-candidate-list-active t)))

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
        (let ((reading
               (if (nskk-with-current-state
                     (nskk-state-get-metadata nskk-current-state 'okurigana-in-progress))
                   (let* ((query (nskk-with-current-state
                                   (nskk-state-get-metadata nskk-current-state 'okurigana-query)))
                          (okuri-kana (buffer-substring-no-properties
                                       (overlay-end nskk--conversion-overlay) (point)))
                          (stem (substring query 0 (- (length query) 1))))
                     (concat stem nskk-okurigana-marker okuri-kana))
                 text)))
          (nskk-start-registration/k reading
            (lambda (registered)
              (if registered
                  (progn
                    (nskk-delete-overlay nskk--conversion-overlay)
                    (nskk--insert-registered-and-reset registered start (lambda ())))
                ;; Registration cancelled: wrap back to first candidate page.
                (nskk--wrap-to-first-candidate)))
            #'ignore))
      ;; No preedit text: wrap back to first candidate page.
      (nskk--wrap-to-first-candidate))))

;;;; Dynamic Completion (動的補完)

(defun nskk--dcomp-search-prefix (prefix)
  "Search for dictionary keys with PREFIX for dynamic completion.
Returns a sorted list of reading strings that start with PREFIX,
excluding PREFIX itself.  Returns nil when no strict prefix matches
exist.  Searches both `user-dict-entry' and `system-dict-entry'
Prolog trie indexes; deduplicates keys present in both."
  (let ((keys nil))
    ;; Search user-dict-entry via Prolog trie
    (dolist (pair (nskk-prolog-trie-prefix-search 'user-dict-entry 2 prefix))
      (let ((key (car pair)))
        (when (and key (not (equal key prefix)))
          (push key keys))))
    ;; Search system-dict-entry via Prolog trie if available
    (dolist (pair (nskk-prolog-trie-prefix-search 'system-dict-entry 2 prefix))
      (let ((key (car pair)))
        (when (and key (not (equal key prefix)) (not (member key keys)))
          (push key keys))))
    (sort keys #'string<)))

(defun nskk--dcomp-replace-preedit (new-text)
  "Replace the current preedit text with NEW-TEXT for dynamic completion."
  (let ((start (nskk--get-conversion-start)))
    (when start
      (let ((text-start (nskk--skip-marker-pos start nskk-henkan-on-marker-regexp)))
        (delete-region text-start (point))
        (goto-char text-start)
        (insert new-text)))))

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
              (cand (cdr pair))
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
  "Display dcomp multiple candidate list inline below preedit.
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
matching candidates inline below the preedit text."
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
    ;; Separating this as a fact table (data) rather than an inline memq (logic)
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
