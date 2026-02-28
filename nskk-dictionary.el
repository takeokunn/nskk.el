;;; nskk-dictionary.el --- Dictionary module for NSKK -*- lexical-binding: t; -*-

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
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Dictionary loading and lookup for NSKK (Layer 1: Core Engine).
;;
;; Layer position: L1 (Core Engine) -- depends only on nskk-prolog.
;;
;; Provides loading, lookup, registration, and persistence of SKK dictionaries.
;; Supports both user dictionaries (read/write) and system dictionaries
;; (read-only, e.g. SKK-JISYO.L from the skktools package).
;;
;; Dictionary sources are identified by source symbols (`user', `system')
;; mapped to Prolog predicates via `dict-source/2' facts.  All dictionary
;; data lives in the global Prolog database as two predicates:
;;
;;   `user-dict-entry/2'   --- (user-dict-entry READING CANDIDATES-LIST)
;;   `system-dict-entry/2' --- (system-dict-entry READING CANDIDATES-LIST)
;;
;; Lookup is O(1) for exact matches via Prolog hash index, O(k + n) for
;; prefix matches via Prolog trie index.  User dictionary entries take
;; priority over system entries via Prolog clause ordering in `dict-entry/2'.
;;
;; Prolog predicates maintained by this module:
;; - `dict-source/2'        -- maps source symbol to predicate name
;; - `user-dict-entry/2'    -- trie-indexed user dictionary entries
;; - `system-dict-entry/2'  -- trie-indexed system dictionary entries
;; - `dict-entry/2'         -- bridge rule (user then system lookup)
;; - `member/2'             -- list membership helper
;; - `dict-register/2'      -- assertz/retract-based registration rule
;;
;; Key public API:
;; - `nskk-dict-lookup'                   -- look up a reading key
;; - `nskk-dict-register-word'            -- register a new word
;; - `nskk-dict-load-user-dictionary'     -- load user dictionary from file
;; - `nskk-dict-load-system-dictionaries' -- load system dictionaries
;; - `nskk-dict-save-user-dictionary'     -- persist user dictionary to file
;; - `nskk-dict-initialize'               -- initialize all dictionaries

;;; Code:

(require 'cl-lib)
(require 'nskk-prolog)

(declare-function nskk-prolog-trie-bulk-assert "nskk-prolog")

(defgroup nskk-dictionary nil
  "Dictionary and search settings."
  :prefix "nskk-dict-"
  :group 'nskk)

(defcustom nskk-dict-user-dictionary-file
  (expand-file-name "~/.skk/jisyo")
  "Path to the user dictionary file."
  :type 'file
  :group 'nskk-dictionary)

(defcustom nskk-dict-system-dictionary-files nil
  "List of system dictionary files to load.
When nil, NSKK auto-detects dictionary paths from nix profiles
and common system locations."
  :type '(repeat file)
  :group 'nskk-dictionary)

(defcustom nskk-dict-cache-enabled t
  "Whether to enable dictionary caching."
  :type 'boolean
  :group 'nskk-dictionary)

(defcustom nskk-large-dictionary nil
  "Path to large SKK dictionary file."
  :type '(choice file (const nil))
  :group 'nskk-dictionary)

(defvar nskk-jisyo-update-hook nil
  "Hook run when dictionary is updated.
DDSKK equivalent: skk-jisyo-update-hook")

;;; Section 1: Error types

(define-error 'nskk-dict-error "Dictionary error")

;;; Section 2: Prolog infrastructure

;; Dictionary source facts: (dict-source source-symbol predicate-name)
;; These map source symbols to their Prolog predicate names
(nskk-prolog-set-index 'dict-source 2 :hash)
(nskk-prolog-<- (dict-source user user-dict-entry))
(nskk-prolog-<- (dict-source system system-dict-entry))

;; Bridge rule: unified lookup across all dictionary sources
;; User dictionary has priority (first clause wins on first solution)
(nskk-prolog-<- (dict-entry \?k \?c) (user-dict-entry \?k \?c))
(nskk-prolog-<- (dict-entry \?k \?c) (system-dict-entry \?k \?c))

;; List membership helper (needed for dict-register rule)
(nskk-prolog-<- (member \?x (\?x . \?_)))
(nskk-prolog-<- (member \?x (\?_ . \?rest)) (member \?x \?rest))

;; Dictionary registration rule using assertz/retract builtins
;; Clause 1: update existing entry, prepend word if not already present
(nskk-prolog-<- (dict-register \?reading \?word)
  (user-dict-entry \?reading \?existing)
  (not (member \?word \?existing))
  (retract (user-dict-entry \?reading \?existing))
  (assertz (user-dict-entry \?reading (\?word . \?existing))))
;; Clause 2: word already exists in entry, no-op success
(nskk-prolog-<- (dict-register \?reading \?word)
  (user-dict-entry \?reading \?existing)
  (member \?word \?existing))
;; Clause 3: no entry exists yet, create new one
(nskk-prolog-<- (dict-register \?reading \?word)
  (not (user-dict-entry \?reading \?_))
  (assertz (user-dict-entry \?reading (\?word))))

;; Cache source-file validity predicate.
;; Succeeds when stored source file list equals current file list.
;; Both arguments must be pre-sorted by the ELisp caller (string< order).
;; (dict-cache-source-valid StoredFiles CurrentFiles)
(nskk-prolog-<- (dict-cache-source-valid \?files \?files))

;; Load strategy routing predicate.
;; Returns ?Strategy = :cache when cache is both enabled and mtime-valid,
;; or :parse as fallback.  ELisp caller passes boolean atoms (t or nil).
;; Use nskk-prolog-query-value for first-solution semantics (first clause wins).
;; Index must be :list so the fallback clause is always reachable.
;; (dict-load-strategy CacheEnabled MtimeValid ?Strategy)
(nskk-prolog-set-index 'dict-load-strategy 3 :list)
(nskk-prolog-<- (dict-load-strategy t t :cache))
(nskk-prolog-<- (dict-load-strategy \?cache-enabled \?mtime-valid :parse))

;; Error/fallback action rules for dictionary loading.
;; ELisp catches errors, maps them to a reason atom, then queries this predicate
;; to obtain the declarative recovery action.
;; (dict-load-fallback Reason ?Action)
(nskk-prolog-set-index 'dict-load-fallback 2 :hash)
(nskk-prolog-<- (dict-load-fallback cache-read-failed :reparse))
(nskk-prolog-<- (dict-load-fallback source-unreadable :skip))
(nskk-prolog-<- (dict-load-fallback no-files-found :warn-and-nil))

;;; Section 3: Data structures

(cl-defstruct nskk-dict-entry
  "Dictionary entry structure."
  (key nil)
  (candidates nil)
  (okuri nil))

(cl-defstruct nskk-dict-index
  "Dictionary index structure.
Lookup is performed via the Prolog database using PREDICATE.
PREDICATE is a symbol naming the Prolog predicate (e.g., \\='system-dict-entry)
with arity 2: (predicate key candidates-list)."
  (predicate nil)
  (by-freq nil))

(defun nskk-dict--struct-lookup (index query &optional _okuri-type)
  "Look up QUERY in dictionary INDEX via Prolog.
Returns `nskk-dict-entry' or nil if not found."
  (when (and (nskk-dict-index-p index) (stringp query))
    (let* ((pred (nskk-dict-index-predicate index))
           (candidates (when pred
                         (nskk-prolog-query-value
                          `(,pred ,query \?candidates) '\?candidates))))
      (when candidates
        (make-nskk-dict-entry :key query :candidates candidates)))))

(defun nskk-dict--struct-entry-count (index _okuri-type)
  "Return count of entries in INDEX."
  (let ((pred (nskk-dict-index-predicate index)))
    (if pred
        (length (gethash (format "%s/2" pred) nskk-prolog--database))
      0)))

(defun nskk-dict-source-p (source)
  "Return non-nil if SOURCE is a valid dictionary source symbol."
  (not (null (nskk-prolog-query-one `(dict-source ,source \?_)))))

(defun nskk-dict--entry-count (source _okuri-type)
  "Return count of entries for dictionary SOURCE."
  (let* ((pred (nskk-prolog-query-value `(dict-source ,source \?pred) '\?pred))
         (key (when pred (nskk-prolog--clause-key pred 2)))
         (trie (when key (gethash key nskk-prolog--trie-indices))))
    (cond
     ((null pred) 0)
     (trie (nskk-prolog--trie-size trie))
     (key (length (gethash key nskk-prolog--database)))
     (t 0))))

;;; Section 4: I/O and lifecycle

;;; Dictionary Parsing

(defun nskk-dict-parse-line (line)
  "Parse a single SKK dictionary LINE.
Returns (key . candidates-list) or nil for comments/invalid lines."
  ;; Skip comment lines and empty lines
  (when (and (stringp line)
             (> (length line) 0)
             (not (string-prefix-p ";;" line)))
    (let ((space-pos (string-search " " line)))
      (when (and space-pos
                 (> space-pos 0)
                 (> (length line) (+ space-pos 2))
                 (= (aref line (1+ space-pos)) ?/))
        (let* ((key (substring line 0 space-pos))
               (candidates-str (substring line (1+ space-pos)))
               (candidates (nskk-dict--parse-candidates candidates-str)))
          (when candidates
            (cons key candidates)))))))

(defun nskk-dict--parse-candidates (str)
  "Parse candidates from STR like \"/candidate1/candidate2/...\"."
  (when (and (stringp str) (> (length str) 1) (= (aref str 0) ?/))
    (let ((parts (split-string (substring str 1) "/" t)))
      ;; Strip annotations (e.g., "漢字;annotation" -> "漢字")
      (mapcar (lambda (c)
                (let ((semi (string-search ";" c)))
                  (if semi (substring c 0 semi) c)))
              parts))))

;;; Dictionary Loading

(defun nskk-dict--parse-file-to-entries (file &optional coding-system)
  "Parse SKK dictionary FILE into a list of (key . candidates-list) pairs.
CODING-SYSTEM defaults to nil (auto-detect).
Returns the parsed entries list, or nil if FILE is not readable.
Does not modify the Prolog database."
  (when (and (stringp file) (file-readable-p file))
    (let ((entries nil)
          (coding coding-system))
      (with-temp-buffer
        (let ((coding-system-for-read coding))
          (insert-file-contents file))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((parsed (nskk-dict-parse-line
                         (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))))
            (when parsed
              (push parsed entries)))
          (forward-line 1)))
      (nreverse entries))))

(defun nskk-dict-load-file (file &optional coding-system predicate-name)
  "Load SKK dictionary from FILE into Prolog as PREDICATE-NAME/2 facts.
PREDICATE-NAME defaults to \\='system-dict-entry.
CODING-SYSTEM defaults to nil which lets Emacs auto-detect encoding.
Returns PREDICATE-NAME symbol on success, or nil on failure."
  (when (and (stringp file) (file-readable-p file))
    (let* ((pred (or predicate-name 'system-dict-entry))
           (coding coding-system))
      ;; Set up trie index for this predicate (arity 2: key + candidates)
      (nskk-prolog-set-index pred 2 :trie)
      (with-temp-buffer
        (let ((coding-system-for-read coding))
          (insert-file-contents file))
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (parsed (nskk-dict-parse-line line)))
            (when parsed
              (let ((key (car parsed))
                    (candidates (cdr parsed)))
                (nskk-prolog-assert (list (list pred key candidates))))))
          (forward-line 1)))
      ;; Return predicate symbol indicating successful load
      pred)))

(defun nskk-dict-load-system-dictionaries ()
  "Load all system dictionaries configured in `nskk-dict-system-dictionary-files'.
Asserts all entries as \\='system-dict-entry/2 Prolog facts via trie index.
When `nskk-dict-cache-enabled' is non-nil, uses an on-disk cache to avoid
re-parsing dictionary files on subsequent Emacs sessions.
Returns \\='system if any entries loaded, or nil if none found."
  (let* ((dict-files nskk-dict-system-dictionary-files)
         (loaded 0))
    ;; Clear any previously loaded system dict facts
    (nskk-prolog-retract-all 'system-dict-entry 2)
    ;; Ensure trie index exists after retract-all
    (nskk-prolog-set-index 'system-dict-entry 2 :trie)
    (let ((strategy (nskk-prolog-query-value
                     `(dict-load-strategy
                       ,(if nskk-dict-cache-enabled t nil)
                       ,(if (nskk-dict--cache-valid-p dict-files) t nil)
                       \?strategy)
                     '\?strategy)))
      (if (eq strategy :cache)
          ;; Fast path: cache is enabled and mtime-valid; load from cache.
          ;; nskk-dict--load-system-dict-from-cache also validates source files.
          (let ((entries (nskk-dict--load-system-dict-from-cache)))
            (when entries
              (nskk-prolog-trie-bulk-assert 'system-dict-entry 2 entries)
              (setq loaded (length entries))
              (message "NSKK: Loaded %d entries from cache" loaded)))
        ;; Parse path: cache disabled, stale, or source-file mismatch detected.
        (let ((all-entries nil))
          (dolist (file dict-files)
            (when (file-readable-p file)
              (let ((entries (nskk-dict--parse-file-to-entries file)))
                (when entries
                  (setq all-entries (nconc all-entries entries))
                  (cl-incf loaded)))))
          (when all-entries
            (nskk-prolog-trie-bulk-assert 'system-dict-entry 2 all-entries)
            (when nskk-dict-cache-enabled
              (nskk-dict--save-system-dict-cache all-entries dict-files)))
          (setq loaded (length all-entries)))))
    (if (> loaded 0)
        (progn
          (message "NSKK: Dictionary initialization complete (%d entries)" loaded)
          'system)
      (message "NSKK: No system dictionaries found")
      nil)))

(defun nskk-dict-load-user-dictionary ()
  "Load user dictionary from `nskk-dict-user-dictionary-file'.
Returns \\='user if loaded, or nil if not found."
  (when (and nskk-dict-user-dictionary-file
             (file-readable-p nskk-dict-user-dictionary-file))
    (message "NSKK: Loading user dictionary from %s"
             nskk-dict-user-dictionary-file)
    ;; Clear any previously loaded user dict facts
    (nskk-prolog-retract-all 'user-dict-entry 2)
    (when (nskk-dict-load-file nskk-dict-user-dictionary-file nil 'user-dict-entry)
      'user)))

;;; On-disk cache for system dictionaries

(defun nskk-dict--cache-file-path ()
  "Return the path to the on-disk system dictionary cache."
  (expand-file-name "nskk/dict-cache.eld" user-emacs-directory))

(defun nskk-dict--cache-valid-p (dict-files)
  "Return non-nil if the cache file exists and is newer than all DICT-FILES."
  (let ((cache-path (nskk-dict--cache-file-path)))
    (and dict-files
         (file-readable-p cache-path)
         (let ((cache-mtime (file-attribute-modification-time
                             (file-attributes cache-path))))
           (cl-every (lambda (f)
                       (let ((attr (file-attributes f)))
                         (and attr
                              (time-less-p
                               (file-attribute-modification-time attr)
                               cache-mtime))))
                     dict-files)))))

(defun nskk-dict--save-system-dict-cache (entries dict-files)
  "Serialize ENTRIES to the on-disk cache.
ENTRIES is a list of (kana . candidates-list) pairs.
DICT-FILES is the list of source files used to build the cache."
  (let ((cache-path (nskk-dict--cache-file-path)))
    (make-directory (file-name-directory cache-path) t)
    (with-temp-file cache-path
      (prin1 (list :version 1
                   :source-files dict-files
                   :entries entries)
             (current-buffer)))
    (message "NSKK: Cached %d entries to %s" (length entries) cache-path)))

(defun nskk-dict--load-system-dict-from-cache ()
  "Load system dictionary entries from the on-disk cache.
Returns a list of (kana . candidates-list) pairs, or nil on failure
or if the stored source files no longer match the current configuration.

Validates `:source-files' stored in the cache against
`nskk-dict-system-dictionary-files' using the `dict-cache-source-valid/2'
Prolog predicate.  Both lists are sorted before comparison so that
reordering of dictionary paths does not invalidate the cache (FR-009)."
  (let ((cache-path (nskk-dict--cache-file-path)))
    (condition-case err
      (with-temp-buffer
        (insert-file-contents cache-path)
        (let* ((data (read (current-buffer)))
               (version (plist-get data :version))
               (stored-files (plist-get data :source-files))
               (entries (plist-get data :entries)))
          (unless (eql version 1)
            (error "Unknown cache version: %s" version))
          ;; Validate stored source files match current configuration (FR-005).
          ;; Sort both lists for order-insensitive comparison (FR-009).
          (let ((sorted-stored (sort (copy-sequence (or stored-files nil)) #'string<))
                (sorted-current (sort (copy-sequence
                                       (or nskk-dict-system-dictionary-files nil))
                                      #'string<)))
            (if (nskk-prolog-holds-p
                 `(dict-cache-source-valid ,sorted-stored ,sorted-current))
                entries
              (message "NSKK: Cache stale (source files changed), reloading from source")
              nil))))
      (error
       (message "NSKK: Cache read failed (%s), reloading from source"
                (error-message-string err))
       nil))))

;;; Global Dictionary State

(defvar nskk--system-dict-index nil
  "Non-nil when system dictionary is loaded.
Value is the source symbol \\='system.")

(defvar nskk--user-dict-index nil
  "Non-nil when user dictionary is loaded.
Value is the source symbol \\='user.")

(defun nskk-dict--detect-system-dictionaries ()
  "Auto-detect system dictionary files.
Probes nix profiles, common system paths, and homebrew locations.
Also checks `nskk-large-dictionary' as a fallback.
Returns a list of readable dictionary file paths."
  (let ((candidates
         (append
          ;; Nix profile paths
          (list (expand-file-name "~/.nix-profile/share/skk/SKK-JISYO.L")
                "/run/current-system/sw/share/skk/SKK-JISYO.L")
          ;; NIX_PROFILES env var paths
          (let ((nix-profiles (getenv "NIX_PROFILES")))
            (when nix-profiles
              (mapcar (lambda (p) (expand-file-name "share/skk/SKK-JISYO.L" p))
                      (split-string nix-profiles))))
          ;; Common system paths
          (list "/usr/share/skk/SKK-JISYO.L"
                "/usr/local/share/skk/SKK-JISYO.L")
          ;; Homebrew paths
          (list "/opt/homebrew/share/skk/SKK-JISYO.L")
          ;; nskk-large-dictionary fallback
          (when nskk-large-dictionary
            (list nskk-large-dictionary))))
        (found nil))
    (dolist (path candidates)
      (when (and (stringp path) (file-readable-p path))
        (push path found)))
    (nreverse found)))

(defun nskk-dict-initialize ()
  "Initialize dictionaries by loading system and user dictionaries.
When `nskk-dict-system-dictionary-files' is nil, auto-detects
dictionary paths from nix profiles and common system locations.

Calling this function interactively allows manual retry: it retracts
the \\='(dict-initialized) Prolog fact first, then reinitializes."
  (interactive)
  ;; Allow manual retry: retract previous initialization marker
  (nskk-prolog-retract-all 'dict-initialized 0)
  (let ((dict-files (or nskk-dict-system-dictionary-files
                        (nskk-dict--detect-system-dictionaries))))
    (when dict-files
      (let ((nskk-dict-system-dictionary-files dict-files))
        (setq nskk--system-dict-index (nskk-dict-load-system-dictionaries)))))
  (setq nskk--user-dict-index (nskk-dict-load-user-dictionary))
  ;; Mark initialization complete (whether or not system dict was found).
  ;; This prevents repeated re-initialization across buffer enables.
  (nskk-prolog-assert '((dict-initialized)))
  (message "NSKK: Dictionary initialization complete"))

(defun nskk-dict-lookup (key)
  "Look up KEY in loaded dictionaries via Prolog bridge rule.
Returns list of candidates or nil.
User dictionary results take priority via clause ordering."
  (let* ((solutions (nskk-prolog-query `(dict-entry ,key \?c)))
         (all-candidate-lists
          (mapcar (lambda (sol) (nskk-prolog-walk '\?c sol))
                  solutions)))
    (when all-candidate-lists
      (cl-reduce (lambda (acc lst) (cl-union acc lst :test #'equal))
                 all-candidate-lists
                 :initial-value nil))))

;;; User Dictionary Modification

(defvar nskk-dict-modified nil
  "Non-nil when the user dictionary has unsaved modifications.")

(defun nskk-dict-register-word (reading word)
  "Register WORD as a conversion candidate for READING in user dictionary.
Uses the Prolog dict-register rule which handles both new entries
and updates to existing entries via assertz/retract builtins."
  (when (and (stringp reading) (stringp word)
             (not (string-empty-p reading))
             (not (string-empty-p word)))
    ;; Ensure user dict index exists before registering
    (unless nskk--user-dict-index
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (setq nskk--user-dict-index 'user))
    (when (nskk-prolog-query-one `(dict-register ,reading ,word))
      (setq nskk-dict-modified t)
      (condition-case err
          (run-hooks 'nskk-jisyo-update-hook)
        (error (message "NSKK: jisyo-update-hook error: %s" (error-message-string err))))
      (message "NSKK: Registered %s -> %s" reading word))))

;;; User Dictionary Save

(defun nskk-dict-save-user-dictionary ()
  "Save user dictionary to `nskk-dict-user-dictionary-file'."
  (interactive)
  (when (and nskk-dict-user-dictionary-file nskk--user-dict-index)
    (let ((dir (file-name-directory nskk-dict-user-dictionary-file)))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    (with-temp-file nskk-dict-user-dictionary-file
      (insert ";; -*- mode: fundamental; coding: utf-8 -*-\n")
      (insert ";; NSKK user dictionary\n")
      (insert ";; okuri-nasi entries.\n")
      ;; Single query fetches all key+candidates pairs (eliminates N+1 pattern)
      (let ((solutions (nskk-prolog-query '(user-dict-entry \?k \?c))))
        (dolist (sol solutions)
          (let ((key (nskk-prolog-walk '\?k sol))
                (candidates (nskk-prolog-walk '\?c sol)))
            (when (and key candidates)
              (insert (format "%s /%s/\n"
                              key
                              (mapconcat #'identity candidates "/"))))))))
    (message "NSKK: User dictionary saved to %s"
             nskk-dict-user-dictionary-file)
    (setq nskk-dict-modified nil)))

(defun nskk-dict--maybe-save ()
  "Save user dictionary if it has unsaved modifications.
Called from `kill-emacs-hook' to persist registrations on Emacs exit."
  (when nskk-dict-modified
    (condition-case err
        (nskk-dict-save-user-dictionary)
      (error (message "NSKK: failed to save user dictionary: %s"
                      (error-message-string err))))))

(provide 'nskk-dictionary)

;;; nskk-dictionary.el ends here
