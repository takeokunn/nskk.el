;;; nskk-dictionary.el --- Dictionary module for NSKK -*- lexical-binding: t; -*-

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

;; Dictionary loading and lookup for NSKK (Layer 1: Core Engine).
;;
;; Layer position: L1 (Core Engine) -- depends on nskk-prolog and nskk-cps-macros.
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
;; Lookup is O(k) for exact matches via Prolog trie index (where k is the
;; key length), O(k + n) for prefix matches.  User dictionary entries take
;; priority over system entries via Prolog clause ordering in `dict-entry/2'.
;;
;; Prolog predicates maintained by this module:
;; - `dict-source/2'             -- maps source symbol to predicate name
;; - `user-dict-entry/2'         -- trie-indexed user dictionary entries
;; - `system-dict-entry/2'       -- trie-indexed system dictionary entries
;; - `dict-entry/2'              -- bridge rule (user then system lookup)
;; - `member/2'                  -- list membership helper
;; - `dict-register/2'           -- assertz/retract-based registration rule
;; - `dict-initialized/0'        -- idempotency marker (asserted after init)
;; - `okuri-consonant/1'         -- set of valid okurigana consonant characters,
;;                                  queried by `nskk--dict-lookup-okuri-ari'
;;                                  (arity 1, :hash index)
;;
;; Key public API:
;; - `nskk-dict-lookup'                   -- look up a reading key
;; - `nskk-dict-load-file'                 -- load any SKK file as Prolog facts
;; - `nskk-dict-register-word'            -- register a new word
;; - `nskk-dict-load-user-dictionary'     -- load user dictionary from file
;; - `nskk-dict-load-system-dictionaries' -- load system dictionaries
;; - `nskk-dict-load-ja-dic'              -- load Emacs built-in ja-dic data
;; - `nskk-dict-save-user-dictionary'     -- persist user dictionary to file
;; - `nskk-dict-initialize'               -- initialize all dictionaries

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nskk-prolog)
(require 'nskk-cps-macros)

(declare-function nskk-prolog-trie-bulk-assert "nskk-prolog")
;; Optional: annotation support
(declare-function nskk-annotation-initialize "nskk-annotation")
(declare-function nskk--annotation-load-from-candidates "nskk-annotation")

(defgroup nskk-dictionary nil
  "Dictionary and search settings."
  :prefix "nskk-dict-"
  :group 'nskk)

(defcustom nskk-dict-user-dictionary-file
  (expand-file-name "~/.nskk/jisyo")
  "Path to the user dictionary file for storing registered words."
  :type 'file
  :package-version '(nskk . "0.1.0")
  :group 'nskk-dictionary)

(defcustom nskk-dict-system-dictionary-files nil
  "List of system dictionary files to load.
When nil, NSKK auto-detects dictionary paths from nix profiles
and common system locations."
  :type '(repeat file)
  :package-version '(nskk . "0.1.0")
  :group 'nskk-dictionary)

(defcustom nskk-dict-cache-enabled t
  "When non-nil, enable on-disk caching for system dictionaries."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-dictionary)

(defcustom nskk-dict-use-ja-dic 'auto
  "Control whether Emacs's built-in ja-dic is used as the system dictionary.
Only consulted when `nskk-dict-system-dictionary-files' is nil.

Possible values:
  `auto' (default) -- auto-detect SKK-JISYO files from system paths first;
                      fall back to ja-dic only if no files are found.
  t               -- always use ja-dic, skipping auto-detection entirely.
  nil             -- never use ja-dic; only auto-detected or explicitly
                      configured SKK-JISYO files are loaded."
  :type '(choice (const :tag "Auto-detect first, ja-dic fallback" auto)
                 (const :tag "Always use ja-dic" t)
                 (const :tag "Never use ja-dic" nil))
  :safe (lambda (v) (memq v '(auto t nil)))
  :package-version '(nskk . "0.2.0")
  :group 'nskk-dictionary)

(defcustom nskk-large-dictionary nil
  "Path to large SKK dictionary file, or nil to disable."
  :type '(choice file (const nil))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-dictionary)

(defvar nskk-jisyo-update-hook nil
  "Hook run when dictionary is updated.")

;;; Section 1: Error types

(define-error 'nskk-dict-error "Dictionary error")

;;; Section 2: Prolog infrastructure

;; Dictionary source facts: (dict-source source-symbol predicate-name)
;; These map source symbols to their Prolog predicate names
(nskk-prolog-define-fact-table dict-source (:arity 2 :index :hash)
  (user user-dict-entry)
  (system system-dict-entry)
  (kakutei kakutei-dict-entry))

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

;; Dictionary unregistration rule using retract/assertz builtins
;; Clause 1: word is the sole candidate, retract entire entry
(nskk-prolog-<- (dict-unregister \?reading \?word)
  (user-dict-entry \?reading (\?word))
  (retract (user-dict-entry \?reading (\?word))))
;; Clause 2: remove word from multi-candidate entry (keep remaining)
(nskk-prolog-<- (dict-unregister \?reading \?word)
  (user-dict-entry \?reading \?existing)
  (member \?word \?existing)
  (retract (user-dict-entry \?reading \?existing))
  (remove-element \?word \?existing \?rest)
  (assertz (user-dict-entry \?reading \?rest)))

;; List element removal helper (needed for dict-unregister rule)
(nskk-prolog-<- (remove-element \?x (\?x . \?rest) \?rest))
(nskk-prolog-<- (remove-element \?x (\?y . \?tail) (\?y . \?result))
  (remove-element \?x \?tail \?result))

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
  (predicate nil))

;;; Section 4: Elisp helpers (replacing removed Prolog predicates)

(defun nskk--dict-collect-candidates (solutions)
  "Collect and deduplicate candidates from Prolog SOLUTIONS.
SOLUTIONS is a list of substitution environments from `nskk-prolog-query'.
Returns a deduplicated list of candidate strings."
  (cl-loop for sol in solutions
           for cands = (nskk-prolog-walk '\?c sol)
           when cands append cands into acc
           finally return (delete-dups acc)))

(defun nskk--dict-cache-source-valid-p (stored-files)
  "Return non-nil if STORED-FILES match current system dictionary configuration.
Compares sorted STORED-FILES against sorted `nskk-dict-system-dictionary-files'
so that reordering of dictionary paths does not invalidate the cache."
  (equal (sort (copy-sequence stored-files) #'string<)
         (sort (copy-sequence nskk-dict-system-dictionary-files) #'string<)))

(defun nskk--dict-run-update-hook ()
  "Run `nskk-jisyo-update-hook' with error protection."
  (condition-case err
      (run-hooks 'nskk-jisyo-update-hook)
    (error (message "NSKK: jisyo-update-hook error: %s"
                    (error-message-string err)))))

(defun nskk--dict-load-from-cache ()
  "Load system dictionary from on-disk cache into Prolog.
Returns entry count on success, or 0 if cache is unavailable."
  (let ((entries (nskk--dict-load-system-dict-from-cache)))
    (if entries
        (progn
          (nskk-prolog-trie-bulk-assert 'system-dict-entry 2 entries)
          (message "NSKK: Loaded %d entries from cache" (length entries))
          (length entries))
      0)))

(defun nskk--dict-load-from-files (dict-files)
  "Parse DICT-FILES and load all entries into Prolog as system-dict-entry/2.
Returns entry count (0 if no readable files or no entries found)."
  (let ((all-entries (cl-loop for file in dict-files
                               when (file-readable-p file)
                               append (nskk--dict-parse-file-to-entries file))))
    (when all-entries
      (nskk-prolog-trie-bulk-assert 'system-dict-entry 2 all-entries)
    (when nskk-dict-cache-enabled
      (nskk--dict-save-system-dict-cache all-entries dict-files)))
    (length all-entries)))

(defvar nskk--dict-ja-dic-code-table nil
  "Hash table mapping ja-dic compact kana codes to Emacs characters.")

(defun nskk--dict-ja-dic-decode-key (codes)
  "Decode ja-dic compact key CODES into an NSKK reading string."
  (unless nskk--dict-ja-dic-code-table
    (setq nskk--dict-ja-dic-code-table (make-hash-table :test #'eql))
    (cl-loop for ch from #x3041 to #x3096
             for jis = (encode-char ch 'japanese-jisx0208)
             when jis do (puthash (- (logand jis #xFF) 32) ch nskk--dict-ja-dic-code-table)))
  (apply #'string
         (mapcar (lambda (code)
                   (cond ((zerop code) ?ー)
                         ((< code 0) (- code))
                         (t (or (gethash code nskk--dict-ja-dic-code-table)
                                (error "NSKK: Unknown ja-dic compact code %S" code)))))
                 codes)))

(defun nskk--dict-ja-dic-flatten-node (node prefix)
  "Recursively flatten ja-dic NODE using PREFIX compact codes.
Candidates at each leaf are stored as-is from the ja-dic tree.
For `skkdic-okuri-nasi', the stored order matches SKK-JISYO.L order.
For `skkdic-okuri-ari', `skkdic-extract-conversion-data' reverses
candidates via cons-accumulation; callers must reverse them back."
  (let* ((code (car node))
         (rest (cdr node))
         (path (append prefix (list code)))
         (cands (car rest))
         (entries nil))
    (when (and (listp cands) (stringp (car cands)))
      (push (cons (nskk--dict-ja-dic-decode-key path) cands) entries)
      (setq rest (cdr rest)))
    (when (eq (car rest) t) (setq rest (cdr rest)))
    (dolist (child rest)
      (when (consp child)
        (setq entries (nconc entries (nskk--dict-ja-dic-flatten-node child path)))))
    entries))

(defun nskk--dict-ja-dic-flatten-tree (tree &optional reverse-candidates)
  "Flatten ja-dic TREE into a list of (key . candidates) entries.
When REVERSE-CANDIDATES is non-nil, reverse each entry's candidate list.
This is needed for `skkdic-okuri-ari' where `skkdic-extract-conversion-data'
stores candidates in reversed order via cons-accumulation."
  (let ((entries (cl-loop for node in (cdr tree)
                          when (consp node)
                          nconc (nskk--dict-ja-dic-flatten-node node nil))))
    (if reverse-candidates
        (mapcar (lambda (entry)
                  (cons (car entry) (reverse (cdr entry))))
                entries)
      entries)))

(defun nskk-dict-load-ja-dic ()
  "Load Emacs built-in `ja-dic' data as `system-dict-entry' facts.
Returns `system' when entries were loaded successfully, or nil otherwise."
  (condition-case err
      (when (load-library "ja-dic/ja-dic")
        (let ((entries (append (when (boundp 'skkdic-okuri-nasi)
                                 (nskk--dict-ja-dic-flatten-tree skkdic-okuri-nasi))
                               (when (boundp 'skkdic-okuri-ari)
                                 (nskk--dict-ja-dic-flatten-tree skkdic-okuri-ari t)))))
          (when entries
            (nskk-prolog-retract-all 'system-dict-entry 2)
            (nskk-prolog-set-index 'system-dict-entry 2 :trie)
            (nskk-prolog-trie-bulk-assert 'system-dict-entry 2 entries)
            (message "NSKK: Loaded ja-dic system dictionary (%d entries)"
                     (length entries))
            'system)))
    (error
     (message "NSKK: Could not load ja-dic (%s)"
              (error-message-string err))
     nil)))

;;; Section 5: I/O and lifecycle

;;; Dictionary Parsing

(defun nskk-dict-parse-line (line)
  "Parse a single SKK dictionary LINE.
Returns (key . candidates-list) or nil for comments/invalid lines.
When `nskk-show-annotation' is non-nil and nskk-annotation is loaded,
also registers any candidate annotations found in the line."
  ;; Skip comment lines and empty lines
  (when (and (stringp line)
             (not (string-empty-p line))
             (not (string-prefix-p ";;" line)))
    (when-let* ((space-pos (string-search " " line))
                ((> space-pos 0))
                ((> (length line) (+ space-pos 2)))
                ((= (aref line (1+ space-pos)) ?/)))
      (let* ((key            (substring line 0 space-pos))
             (candidates-str (substring line (1+ space-pos)))
             (candidates     (nskk--dict-parse-candidates candidates-str)))
        ;; Register annotations when annotation support is enabled
        (when (and candidates
                   (boundp 'nskk-show-annotation)
                   nskk-show-annotation
                   (fboundp 'nskk--annotation-load-from-candidates))
          (let ((with-annots (nskk--dict-parse-candidates-with-annotations
                              candidates-str)))
            (nskk--annotation-load-from-candidates key with-annots)))
        (when candidates
          (cons key candidates))))))

(defun nskk--dict-parse-candidates (str)
  "Parse candidates from STR like \"/candidate1/candidate2/...\"."
  (when (and (stringp str) (> (length str) 1) (= (aref str 0) ?/))
    (let ((parts (split-string (substring str 1) "/" t)))
      ;; Strip annotations (e.g., "漢字;annotation" -> "漢字")
      (mapcar (lambda (c)
                (let ((semi (string-search ";" c)))
                  (if semi (substring c 0 semi) c)))
              parts))))

(defun nskk--dict-split-candidate-annotation (candidate-str)
  "Split CANDIDATE-STR into (candidate . annotation) cons cell.
If CANDIDATE-STR contains ';', returns (text-before-semi . text-after-semi).
Otherwise returns (CANDIDATE-STR . nil)."
  (let ((semi (string-search ";" candidate-str)))
    (if semi
        (cons (substring candidate-str 0 semi)
              (substring candidate-str (1+ semi)))
      (cons candidate-str nil))))

(defun nskk--dict-parse-candidates-with-annotations (str)
  "Parse candidates from STR, preserving annotations.
Returns a list of (candidate . annotation-or-nil) cons cells.
For \"/漢字;a kanji/感じ/\", returns:
  ((\"漢字\" . \"a kanji\") (\"感じ\" . nil))"
  (when (and (stringp str) (> (length str) 1) (= (aref str 0) ?/))
    (let ((parts (split-string (substring str 1) "/" t)))
      (mapcar #'nskk--dict-split-candidate-annotation parts))))

;;; Dictionary Loading

(defun nskk--dict-parse-file-to-entries (file &optional coding-system)
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

Uses `nskk-prolog-assert' per entry (not bulk-assert) so that the flat
clause database is also populated.  This is required for variable-key
enumeration (e.g. `nskk-dict-save-user-dictionary' queries
\\='(user-dict-entry ?k ?c)') to work correctly.  For large read-only
system dictionaries, use `nskk-prolog-trie-bulk-assert' directly after
`nskk--dict-parse-file-to-entries'.

Returns PREDICATE-NAME symbol on success, or nil when FILE has no valid
entries or cannot be read."
  (when (and (stringp file) (file-readable-p file))
    (let* ((pred (or predicate-name 'system-dict-entry))
           (entries (nskk--dict-parse-file-to-entries file coding-system)))
      (when entries
        (nskk-prolog-set-index pred 2 :trie)
        (dolist (entry entries)
          (nskk-prolog-assert (list (list pred (car entry) (cdr entry)))))
        pred))))

(defun/k nskk-dict-load-system-dictionaries ()
  "Load system dictionaries from `nskk-dict-system-dictionary-files'.
Asserts all entries as \\='system-dict-entry/2 Prolog facts via trie index.
When `nskk-dict-cache-enabled' is non-nil and the cache is valid, loads from
the on-disk cache to avoid re-parsing dictionary files.
Calls ON-FOUND with \\='system if entries loaded; ON-NOT-FOUND otherwise."
  (let ((dict-files nskk-dict-system-dictionary-files))
    ;; Clear any previously loaded system dict facts
    (nskk-prolog-retract-all 'system-dict-entry 2)
    ;; Ensure trie index exists after retract-all
    (nskk-prolog-set-index 'system-dict-entry 2 :trie)
    (let ((loaded (if (and nskk-dict-cache-enabled
                           (nskk--dict-cache-valid-p dict-files))
                      (nskk--dict-load-from-cache)
                    (nskk--dict-load-from-files dict-files))))
      (if (> loaded 0)
          (progn
            (message "NSKK: Dictionary initialization is complete (%d entries)" loaded)
            (succeed 'system))
        (message "NSKK: No system dictionaries found")
        (fail)))))

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

(defun nskk--dict-cache-file-path ()
  "Return the path to the on-disk system dictionary cache."
  (expand-file-name "nskk/dict-cache.eld" user-emacs-directory))

(defun nskk--dict-file-older-than (file cache-mtime)
  "Return non-nil if FILE's mtime is strictly older than CACHE-MTIME.
Returns nil both when FILE cannot be stat'd (missing, unreadable, or broken
symlink) and when FILE is newer than or equal to CACHE-MTIME."
  (let ((attr (file-attributes file)))
    (and attr
         (time-less-p
          (file-attribute-modification-time attr)
          cache-mtime))))

(defun nskk--dict-cache-valid-p (dict-files)
  "Return non-nil if the cache file exists and is newer than all DICT-FILES."
  (let ((cache-path (nskk--dict-cache-file-path)))
    (and dict-files
         (let ((cache-attr (file-attributes cache-path)))
           (and cache-attr
                (file-readable-p cache-path)
                (let ((cache-mtime (file-attribute-modification-time cache-attr)))
                  (cl-every (lambda (f) (nskk--dict-file-older-than f cache-mtime))
                            dict-files)))))))

(defun nskk--dict-save-system-dict-cache (entries dict-files)
  "Serialize ENTRIES to the on-disk cache.
ENTRIES is a list of (kana . candidates-list) pairs.
DICT-FILES is the list of source files used to build the cache."
  (let ((cache-path (nskk--dict-cache-file-path)))
    (make-directory (file-name-directory cache-path) t)
    (with-temp-file cache-path
      (prin1 (list :version 1
                   :source-files dict-files
                   :entries entries)
             (current-buffer)))
    (message "NSKK: Cached %d entries to %s" (length entries) cache-path)))

(defun nskk--dict-load-system-dict-from-cache ()
  "Load system dictionary entries from the on-disk cache.
Returns a list of (kana . candidates-list) pairs, or nil on failure
or if the stored source files no longer match the current configuration.

Validates `:source-files' stored in the cache against
`nskk-dict-system-dictionary-files' using `nskk--dict-cache-source-valid-p'.
Both lists are sorted before comparison so that reordering of dictionary
paths does not invalidate the cache (FR-009)."
  (let ((cache-path (nskk--dict-cache-file-path)))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents cache-path)
          (let* ((data (read (current-buffer)))
                 (version (plist-get data :version))
                 (stored (plist-get data :source-files))
                 (entries (plist-get data :entries)))
            (when (and (eql version 1)
                       stored
                       entries
                       (nskk--dict-cache-source-valid-p stored))
              entries)))
      (error
       (message "NSKK: Cache read failed (%s), reloading from source"
                (error-message-string err))
       nil))))

;;; Global Dictionary State

(defvar nskk--system-dict-index nil
  "Non-nil when system dictionary is loaded.
Value is the source symbol \\='system.")

(defsubst nskk-dict-system-index ()
  "Return the system dictionary index, or nil if not initialized."
  nskk--system-dict-index)

(defvar nskk--user-dict-index nil
  "Non-nil when user dictionary is loaded.
Value is the source symbol \\='user.")

(defconst nskk--dict-system-probe-paths
  (list (expand-file-name "~/.nix-profile/share/skk/SKK-JISYO.L")
        "/run/current-system/sw/share/skk/SKK-JISYO.L"
        "/usr/share/skk/SKK-JISYO.L"
        "/usr/local/share/skk/SKK-JISYO.L"
        "/opt/homebrew/share/skk/SKK-JISYO.L")
  "Candidate paths probed for system SKK dictionary files.
All readable paths from this list are returned by
`nskk--dict-detect-system-dictionaries'.")

(defun nskk--dict-detect-system-dictionaries ()
  "Auto-detect system dictionary files.
Probes nix profiles, common system paths, and homebrew locations.
Also includes `nskk-large-dictionary' when non-nil.
Returns a list of readable dictionary file paths."
  (let* ((nix-profiles (getenv "NIX_PROFILES"))
         (nix-profile-paths
          (when nix-profiles
            (mapcar (lambda (p) (expand-file-name "share/skk/SKK-JISYO.L" p))
                    (split-string nix-profiles))))
         (large-dict-paths
          (when nskk-large-dictionary
            (list nskk-large-dictionary)))
         (candidates
          (append nskk--dict-system-probe-paths
                  nix-profile-paths
                  large-dict-paths)))
    (delete-dups (cl-remove-if-not (lambda (p) (and (stringp p) (file-readable-p p))) candidates))))

(defvar nskk--dict-okuri-consonants nil
  "Cached list of okuri-ari consonant character codes.
Populated by `nskk-dict-initialize' from the `okuri-consonant/1' Prolog table.")

(defun nskk--dict-initialize-system-dictionary ()
  "Initialize the system dictionary using configured files or built-in ja-dic.
Priority (first match wins):
  1. Explicit `nskk-dict-system-dictionary-files' -- load them.
  2. `nskk-dict-use-ja-dic' is t -- force ja-dic unconditionally.
  3. Auto-detect SKK-JISYO files from system paths.
  4. ja-dic as last resort (unless `nskk-dict-use-ja-dic' is nil)."
  (or ;; 1. Explicit dictionary files configured by user.
      (when nskk-dict-system-dictionary-files
        (nskk-dict-load-system-dictionaries))
      ;; 2. Force ja-dic when explicitly requested.
      (when (eq nskk-dict-use-ja-dic t)
        (nskk-dict-load-ja-dic))
      ;; 3. Auto-detect SKK-JISYO files from system paths.
      (let ((dict-files (nskk--dict-detect-system-dictionaries)))
        (when dict-files
          (let ((nskk-dict-system-dictionary-files dict-files))
            (nskk-dict-load-system-dictionaries))))
      ;; 4. ja-dic as last resort (skipped when nskk-dict-use-ja-dic is nil).
      (when nskk-dict-use-ja-dic
        (nskk-dict-load-ja-dic))))

;;;###autoload
(defun nskk-dict-initialize ()
  "Initialize dictionaries by loading system and user dictionaries.
When `nskk-dict-system-dictionary-files' is nil, auto-detects
dictionary paths from nix profiles and common system locations.
See `nskk-dict-use-ja-dic' for the auto-detect vs ja-dic priority.

Calling this function interactively allows manual retry: it retracts
the \\='(dict-initialized) Prolog fact first, then reinitializes."
  (interactive)
  ;; Allow manual retry: retract previous initialization marker
  (nskk-prolog-retract-all 'dict-initialized 0)
  ;; Define okuri-consonant/1 fact table (inside guard so it survives
  ;; nskk-prolog-clear-database and is re-asserted on re-initialization)
  (nskk-prolog-define-fact-table okuri-consonant (:arity 1 :index :hash)
    (?k) (?s) (?t) (?n) (?h) (?m) (?y) (?r) (?w)
    (?g) (?z) (?d) (?b) (?p))
  ;; Populate the module-level cache used by nskk--dict-lookup-okuri-ari
  (setq nskk--dict-okuri-consonants
        (nskk-prolog-query-all-values '(okuri-consonant \?c) '\?c))
  (setq nskk--system-dict-index (nskk--dict-initialize-system-dictionary))
  (setq nskk--user-dict-index (nskk-dict-load-user-dictionary))
  ;; Load confirmed dictionary if configured
  (nskk-dict-load-kakutei-dictionary)
  ;; Mark initialization complete (whether or not system dict was found).
  ;; This prevents repeated re-initialization across buffer enables.
  (nskk-prolog-assert '((dict-initialized)))
  (message "NSKK: Dictionary initialization is complete"))

(defun/k nskk--dict-lookup-okuri-ari (key)
  "Look up KEY for okuri-ari entries by appending each okuri consonant.
Returns combined unique candidates from all matching entries via on-found,
or calls on-not-found if no candidates are found."
  (let* ((consonants nskk--dict-okuri-consonants)
         (all-candidates
          (cl-loop for c in consonants
                   for okuri-key = (concat key (string c))
                   for solutions = (nskk-prolog-query `(dict-entry ,okuri-key \?cands))
                   when solutions
                   nconc (cl-loop for sol in solutions
                                  for cands = (nskk-prolog-walk '\?cands sol)
                                  when cands append cands))))
    (if all-candidates
        (succeed (cl-remove-duplicates all-candidates :test #'equal))
      (fail))))

(defun/k nskk--dict-do-lookup (key)
  "Internal: perform the actual Prolog lookup for KEY.
User dictionary results take priority via clause ordering.

When the length of KEY is greater than 1, also searches for okuri-ari
entries by appending all okurigana consonants to KEY in turn.
Results from both searches are combined via `cl-union'.
Calls ON-FOUND with candidates when non-empty; ON-NOT-FOUND otherwise."
  (let* ((okuri-nasi (nskk--dict-collect-candidates
                       (nskk-prolog-query `(dict-entry ,key \?c))))
         (candidates (if (> (length key) 1)
                         (cl-union okuri-nasi
                                   (nskk--dict-lookup-okuri-ari key)
                                   :test #'equal)
                       okuri-nasi)))
    (if candidates
        (succeed candidates)
      (fail))))

(defun/k nskk-dict-lookup (key)
  "Look up KEY in loaded dictionaries via Prolog bridge rule.
Returns list of candidates or nil.
User dictionary results take priority via clause ordering.

When KEY has no explicit okurigana marker (no trailing lowercase consonant),
also searches for okuri-ari entries by trying all possible okurigana
consonants appended to KEY.  Results from both searches are combined."
  (<- candidates nskk--dict-do-lookup key)
  (succeed candidates))

;;; User Dictionary Modification

(defvar nskk-dict-modified nil
  "Non-nil when the user dictionary has unsaved modifications.")

(defun nskk--dict-register-impl (reading word)
  "Attempt to register WORD for READING in the Prolog user dictionary.
Returns t on success (Prolog dict-register/2 succeeded), nil on failure."
  (unless nskk--user-dict-index
    (nskk-prolog-set-index 'user-dict-entry 2 :trie)
    (setq nskk--user-dict-index 'user))
  (when (nskk-prolog-holds-p `(dict-register ,reading ,word))
    (setq nskk-dict-modified t)
    (nskk--dict-run-update-hook)
    (message "NSKK: Registered %s -> %s" reading word)
    t))

(defun/k nskk-dict-register-word (reading word)
  "Register WORD as a conversion candidate for READING in user dictionary.
Uses the Prolog dict-register rule which handles both new entries
and updates to existing entries via assertz/retract builtins.
Returns non-nil (t) on success; calls on-not-found when READING or WORD
are empty/invalid or when the Prolog registration query fails."
  (if (and (stringp reading) (not (string-empty-p reading))
           (stringp word)   (not (string-empty-p word))
           (nskk--dict-register-impl reading word))
      (succeed t)
    (fail)))

(defun nskk--dict-unregister-impl (reading word)
  "Attempt to unregister WORD for READING from the Prolog user dictionary.
Returns t on success (Prolog dict-unregister/2 succeeded), nil on failure."
  (when (and nskk--user-dict-index
             (nskk-prolog-holds-p `(dict-unregister ,reading ,word)))
    (setq nskk-dict-modified t)
    (nskk--dict-run-update-hook)
    (message "NSKK: Unregistered %s -> %s" reading word)
    t))

(defun/k nskk-dict-unregister-word (reading word)
  "Unregister WORD as a conversion candidate for READING from user dictionary.
Uses the Prolog dict-unregister rule which removes the word from an
existing entry (or retracts the entire entry if it was the sole candidate).
Returns non-nil (t) on success; calls on-not-found when READING or WORD
are empty/invalid or when the Prolog unregistration query fails."
  (if (and (stringp reading) (not (string-empty-p reading))
           (stringp word)   (not (string-empty-p word))
           (nskk--dict-unregister-impl reading word))
      (succeed t)
    (fail)))

;;; User Dictionary Save

;;;###autoload
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
      (dolist (binding (nskk-prolog-query-bindings '(user-dict-entry \?k \?c) '(\?k \?c)))
        (let ((key        (car binding))
              (candidates (cadr binding)))
          (when (and key candidates)
            (let ((safe-cands (seq-remove
                               (lambda (c) (string-match-p "[\n\r/]" c))
                               candidates)))
              (when (and (not (string-match-p "[\n\r]" key))
                         safe-cands)
                (insert (format "%s /%s/\n"
                                key
                                (string-join safe-cands "/")))))))))
    (message "NSKK: User dictionary saved to %s"
             nskk-dict-user-dictionary-file)
    (setq nskk-dict-modified nil)))

(defun nskk--dict-maybe-save ()
  "Save user dictionary if it has unsaved modifications.
Called from `kill-emacs-hook' to persist registrations on Emacs exit."
  (when nskk-dict-modified
    (condition-case err
        (nskk-dict-save-user-dictionary)
      (error (message "NSKK: Failed to save user dictionary: %s"
                      (error-message-string err))))))

;;;; Confirmed Dictionary (確定辞書) Support

(defvar nskk--kakutei-dict-loaded nil
  "Non-nil when the confirmed (kakutei) dictionary has been loaded.")

(defun nskk-dict-load-kakutei-dictionary ()
  "Load the confirmed dictionary from `nskk-kakutei-jisyo' if configured.
The confirmed dictionary contains entries that are committed immediately
without candidate selection.  Entries are loaded as \\='kakutei-dict-entry/2
Prolog facts with trie indexing.
Returns \\='kakutei if loaded, nil otherwise."
  (when (and (boundp 'nskk-kakutei-jisyo)
             nskk-kakutei-jisyo
             (file-readable-p nskk-kakutei-jisyo))
    (message "NSKK: Loading kakutei dictionary from %s" nskk-kakutei-jisyo)
    (nskk-prolog-retract-all 'kakutei-dict-entry 2)
    (nskk-prolog-set-index 'kakutei-dict-entry 2 :trie)
    (when-let* ((entries (nskk--dict-parse-file-to-entries nskk-kakutei-jisyo)))
      (dolist (entry entries)
        (nskk-prolog-assert (list (list 'kakutei-dict-entry
                                       (car entry) (cdr entry)))))
      (setq nskk--kakutei-dict-loaded t)
      'kakutei)))

(defun/k nskk-dict-lookup-kakutei (reading)
  "Look up READING in the confirmed dictionary.
Returns the single candidate string when a unique match is found.
Calls on-not-found when the kakutei dictionary is not loaded or has
no entry, or when the entry has multiple candidates (not a confirmed entry).
Entries with exactly one candidate are treated as confirmed; others are
treated as regular entries (returned via on-not-found to allow normal
candidate selection to proceed)."
  (if (and nskk--kakutei-dict-loaded (stringp reading))
      (let ((result (nskk-prolog-query-value
                     `(kakutei-dict-entry ,reading \?c) '\?c)))
        (if (and result
                 (listp result)
                 (= (length result) 1))
            ;; Single candidate: confirm immediately
            (succeed (car result))
          ;; Multiple candidates or no entry: fall through
          (fail)))
    (fail)))

(provide 'nskk-dictionary)

;;; nskk-dictionary.el ends here
