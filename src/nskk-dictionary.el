;;; nskk-dictionary.el --- Dictionary module for NSKK -*- lexical-binding: t; -*-
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
(require 'nskk-dict-transaction)

(require 'nskk-cps-macros)
(declare-function nskk-search-cache-snapshots "nskk-search")
(declare-function nskk-search-restore-cache-snapshot "nskk-search" (snapshot))

(declare-function nskk-prolog-trie-bulk-assert "nskk-prolog")

;; Optional: annotation support
(declare-function nskk-annotation-initialize "nskk-annotation")

(declare-function nskk-annotation-load-from-candidates "nskk-annotation")

(defgroup
  nskk-dictionary
  nil
  "Dictionary and search settings."
  :prefix
  "nskk-dict-"
  :group
  'nskk)

(defcustom
  nskk-dict-user-dictionary-file
  (expand-file-name "~/.nskk/jisyo")
  "Path to the user dictionary file for storing registered words."
  :type
  'file
  :package-version
  '(nskk . "0.1.0")
  :group
  'nskk-dictionary)

(defcustom
  nskk-dict-system-dictionary-files
  nil
  "List of system dictionary files to load.
When nil, NSKK auto-detects dictionary paths from nix profiles
and common system locations."
  :type
  '(repeat file)
  :package-version
  '(nskk . "0.1.0")
  :group
  'nskk-dictionary)

(defcustom
  nskk-dict-cache-enabled
  t
  "When non-nil, enable on-disk caching for system dictionaries."
  :type
  'boolean
  :safe
  #'booleanp
  :package-version
  '(nskk . "0.1.0")
  :group
  'nskk-dictionary)

(defcustom
  nskk-dict-use-ja-dic
  'auto
  "Control whether Emacs's built-in ja-dic is used as the system dictionary.
Only consulted when `nskk-dict-system-dictionary-files' is nil.

Possible values:
  `auto' (default) -- auto-detect SKK-JISYO files from system paths first;
                      fall back to ja-dic only if no files are found.
  t               -- always use ja-dic, skipping auto-detection entirely.
  nil             -- never use ja-dic; only auto-detected or explicitly
                      configured SKK-JISYO files are loaded."
  :type
  '(choice
    (const :tag "Auto-detect first, ja-dic fallback" auto)
    (const :tag "Always use ja-dic" t)
    (const :tag "Never use ja-dic" nil))
  :safe
  (lambda (v)
    (memq v '(auto t nil)))
  :package-version
  '(nskk . "0.2.0")
  :group
  'nskk-dictionary)

(defcustom
  nskk-large-dictionary
  nil
  "Path to large SKK dictionary file, or nil to disable."
  :type
  '(choice file (const nil))
  :package-version
  '(nskk . "0.1.0")
  :group
  'nskk-dictionary)

(defvar nskk-jisyo-update-hook nil
  "Hook run while a dictionary update crosses its publication boundary.
User registration is transactional: the first `error' or `quit' stops later
observers, rolls back NSKK's internal registration state, and propagates the
original condition.  Best-effort update paths may instead report ordinary
errors per observer.  Hook functions must manage their own external effects;
NSKK can roll back only its internal dictionary and search-cache state.")

;;; Atomic file writing

(defun nskk--dict-file-identifier (path)
  "Return the device and inode identifier for PATH, or nil if unavailable."
  (when-let* ((attributes (file-attributes path 'integer)))
    (file-attribute-file-identifier attributes)))

(defun nskk--dict-call-with-atomic-file (path writer &optional on-commit)
  "Call WRITER in a temporary buffer and atomically replace PATH.
WRITER fills the temporary buffer.  Its complete contents are passed to
\`make-temp-file' so creation and writing do not reopen a named temporary
file.  ON-COMMIT, when non-nil, runs after the rename commits and before quits
are re-enabled.  A condition signaled after the underlying rename is
recognized by comparing file identifiers, so the callback still runs before
the original condition is re-signaled."
  (when (file-symlink-p path)
    (signal 'file-error (list "Refusing to replace symbolic link" path)))
  (when (and (file-exists-p path) (not (file-regular-p path)))
    (signal 'file-error (list "Refusing to replace non-regular file" path)))
  (let* ((old-modes (and (file-exists-p path) (file-modes path)))
         (prefix (concat (expand-file-name path) "."))
         (temp nil)
         (temp-identifier nil)
         (primary-condition nil)
         (cleanup-condition nil)
         result)
    (unwind-protect
        (condition-case condition
            (progn
              (with-temp-buffer
                (funcall writer)
                (setq temp
                      (with-file-modes #o600
                        (make-temp-file prefix nil nil (buffer-string)))))
              (setq temp-identifier (nskk--dict-file-identifier temp))
              (unless temp-identifier
                (signal 'file-error
                        (list "Cannot identify atomic temporary file" temp)))
              (when old-modes
                (set-file-modes temp old-modes))
              (unless (equal temp-identifier
                             (nskk--dict-file-identifier temp))
                (signal 'file-error
                        (list "Atomic temporary file identity changed" temp)))
              (let ((rename-condition nil)
                    rename-result)
                (let ((inhibit-quit t))
                  (condition-case condition
                      (setq rename-result (rename-file temp path t))
                    ((error quit)
                     (setq rename-condition condition)))
                  (if (or (null rename-condition)
                          (condition-case nil
                              (equal temp-identifier
                                     (nskk--dict-file-identifier path))
                            ((error quit) nil)))
                      (progn
                        (setq result rename-result)
                        (when on-commit
                          (funcall on-commit))
                        (when rename-condition
                          (signal (car rename-condition)
                                  (cdr rename-condition))))
                    (signal (car rename-condition)
                            (cdr rename-condition))))))
          ((error quit)
           (setq primary-condition condition)))
      (let ((inhibit-quit t))
        (condition-case condition
            (when (and temp-identifier
                       (equal temp-identifier
                              (nskk--dict-file-identifier temp)))
              (delete-file temp))
          ((error quit)
           (setq cleanup-condition condition)))))
    (cond
     (primary-condition
      (signal (car primary-condition) (cdr primary-condition)))
     (cleanup-condition
      (signal (car cleanup-condition) (cdr cleanup-condition)))
     (t result))))

(defmacro nskk-dict-with-atomic-file (path &rest body)
  "Write BODY output to PATH atomically and privately.
Like \`with-temp-file', but the complete content is written while
\`make-temp-file' exclusively creates a same-directory temporary file, then
renamed over PATH.  Existing target modes are preserved.  Refuses to replace
a symbolic link or an existing non-regular file."
  (declare (indent 1)
           (debug (form body)))
  `(nskk--dict-call-with-atomic-file
    ,path
    (lambda ()
      ,@body)))

(defun nskk-dict-serialize-solutions (goal variables)
  "Query GOAL and return one list of VARIABLES' bindings per solution.
The result is the serialized form shared by the persistence files: a
proper list of proper lists, in solution order."
  (mapcar (lambda (solution)
            (mapcar (lambda (variable)
                      (nskk-prolog-walk variable solution))
                    variables))
          (nskk-prolog-query goal)))

(defun nskk-dict-write-private-file (file payload)
  "Write PAYLOAD to FILE atomically, creating FILE's directory privately.
The persistence files record the user's own conversion history, so a
directory this code has to create is created unreadable by anyone else.
An existing directory keeps whatever modes it already has."
  (let ((directory (file-name-directory file)))
    (unless (file-directory-p directory)
      (with-file-modes #o700
        (make-directory directory t))))
  (nskk-dict-with-atomic-file file
    (prin1 payload (current-buffer))))

;; Dictionary source facts: (dict-source source-symbol predicate-name)
;; These map source symbols to their Prolog predicate names
(nskk-prolog-define-fact-table
    dict-source
    (:arity 2 :index :hash)
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
(nskk-prolog-<-
  (dict-register \?reading \?word)
  (user-dict-entry \?reading \?existing)
  (not (member \?word \?existing))
  (retract (user-dict-entry \?reading \?existing))
  (assertz (user-dict-entry \?reading (\?word . \?existing))))

;; Clause 2: word already exists in entry, no-op success
(nskk-prolog-<-
  (dict-register \?reading \?word)
  (user-dict-entry \?reading \?existing)
  (member \?word \?existing))

;; Clause 3: no entry exists yet, create new one
(nskk-prolog-<-
  (dict-register \?reading \?word)
  (not (user-dict-entry \?reading \?_))
  (assertz (user-dict-entry \?reading (\?word))))

;; Dictionary unregistration rule using retract/assertz builtins
;; Clause 1: word is the sole candidate, retract entire entry
(nskk-prolog-<-
  (dict-unregister \?reading \?word)
  (user-dict-entry \?reading (\?word))
  (retract (user-dict-entry \?reading (\?word))))

;; Clause 2: remove word from multi-candidate entry (keep remaining)
(nskk-prolog-<-
  (dict-unregister \?reading \?word)
  (user-dict-entry \?reading \?existing)
  (member \?word \?existing)
  (retract (user-dict-entry \?reading \?existing))
  (remove-element \?word \?existing \?rest)
  (assertz (user-dict-entry \?reading \?rest)))

;; List element removal helper (needed for dict-unregister rule)
(nskk-prolog-<- (remove-element \?x (\?x . \?rest) \?rest))

(nskk-prolog-<-
  (remove-element \?x (\?y . \?tail) (\?y . \?result))
  (remove-element \?x \?tail \?result))

(cl-defstruct
  nskk-dict-entry
  "Dictionary entry structure."
  (key nil)
  (candidates nil)
  (okuri nil))

(cl-defstruct
  nskk-dict-index
  "Dictionary index structure.
Lookup is performed via the Prolog database using PREDICATE.
PREDICATE is a symbol naming the Prolog predicate (e.g., \\='system-dict-entry)
with arity 2: (predicate key candidates-list)."
  (predicate nil))

(defun nskk--dict-merge-candidate-lists (candidate-lists)
  "Merge CANDIDATE-LISTS in order, removing `equal' duplicates.
The first equal candidate object is retained and input lists are not modified."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (candidates candidate-lists)
      (dolist (candidate candidates)
        (unless (gethash candidate seen)
          (puthash candidate t seen)
          (push candidate result))))
    (nreverse result)))

(defun nskk--dict-collect-candidates (solutions)
  "Collect and deduplicate candidates from Prolog SOLUTIONS.
SOLUTIONS is a list of substitution environments from `nskk-prolog-query'.
Returns candidates in solution order, retaining the first equal object."
  (nskk--dict-merge-candidate-lists
    (cl-loop
      for
      sol
      in
      solutions
      for
      candidates
      =
      (nskk-prolog-walk '\?c sol)
      when
      candidates
      collect
      candidates)))

(defun nskk--dict-cache-source-valid-p (stored-files)
  "Return non-nil if STORED-FILES match current system dictionary configuration.
Compares sorted STORED-FILES against sorted `nskk-dict-system-dictionary-files'
so that reordering of dictionary paths does not invalidate the cache."
  (equal
    (sort (copy-sequence stored-files) #'string<)
    (sort (copy-sequence nskk-dict-system-dictionary-files) #'string<)))

(defun nskk--dict-run-notification-hook (hook label)
  "Run notification HOOK, reporting each ordinary error with LABEL.
Each hook function is invoked separately so one failure does not block later
observers.  A `quit' condition is deliberately allowed to escape unchanged."
  (run-hook-wrapped
    hook
    (lambda (function)
      (condition-case
        err
        (funcall function)
        (error (message "NSKK: %s error: %s" label (error-message-string err))))
      nil)))

(defun nskk--dict-run-update-hook ()
  "Run `nskk-jisyo-update-hook' as an isolated notification boundary."
  (nskk--dict-run-notification-hook 'nskk-jisyo-update-hook "jisyo-update-hook"))

(defun nskk--dict-stage-predicate-entries (predicate entries &optional existing-clauses)
  "Build PREDICATE facts for ENTRIES in isolated Prolog storage.
EXISTING-CLAUSES are asserted before ENTRIES for append-style loads."
  (nskk-prolog-with-database-fields
      ((database (make-hash-table :test #'equal))
       (database-tails (make-hash-table :test #'equal))
       (index-config (make-hash-table :test #'equal))
       (hash-indices (make-hash-table :test #'equal))
       (trie-indices (make-hash-table :test #'equal))
       (index-bucket-tail-cache (make-hash-table :test #'equal)))
    (nskk-prolog-set-index predicate 2 :trie)
    (dolist (clause existing-clauses)
      (nskk-prolog-assert clause))
    (nskk-prolog-trie-bulk-assert predicate 2 entries)
    (nskk-dict-transaction-predicate-snapshot (nskk-prolog-clause-key predicate 2))))

(defun nskk--dict-publish-staged-predicate (staged)
  "Publish STAGED predicate storage."
  (nskk-dict-transaction-apply-predicate-snapshot staged))

(defun nskk--dict-commit-staged-predicate (staged &optional prepare)
  "Run PREPARE and atomically publish STAGED predicate storage.
Restore the previous predicate storage if either step signals an error or quit."
  (let* ((key (aref staged 1))
         (owner (list 'nskk--dict-commit-staged-predicate key)))
    (nskk-dict-transaction-ensure-rollback-complete owner)
    (let ((previous (nskk-dict-transaction-predicate-snapshot key)))
      (condition-case condition
          (prog1
              (progn
                (when prepare
                  (funcall prepare))
                (nskk--dict-publish-staged-predicate staged))
            (nskk-dict-transaction-clear-pending-rollback owner))
        ((error quit)
         (nskk-dict-transaction-rollback-and-resignal
          owner
          condition
          (list
           (cons
            'predicate
            (lambda ()
              (nskk-dict-transaction-apply-predicate-snapshot previous))))))))))

(defun nskk--dict-replace-predicate-entries (predicate entries)
  "Atomically replace PREDICATE/2 with ENTRIES."
  (nskk--dict-commit-staged-predicate
    (nskk--dict-stage-predicate-entries predicate entries)))

(defun nskk--dict-append-predicate-entries (predicate entries)
  "Atomically append ENTRIES to PREDICATE/2.
Database and warm index-bucket appends run in O(length ENTRIES).  A bucket
created outside this function pays a one-time tail discovery cost.  A fresh
predicate receives a trie index; existing index strategy is retained."
  (let* ((key (nskk-prolog-clause-key predicate 2))
         (missing nskk-dict-transaction--storage-missing)
         (previous (nskk-dict-transaction-predicate-snapshot key))
         (old-database-head (gethash key (nskk-prolog-database)))
         (old-database-tail (gethash key (nskk-prolog-database-tails)))
         (old-database-tail-cdr (and old-database-tail (cdr old-database-tail)))
         (clauses nil)
         (clauses-tail nil)
         (groups (make-hash-table :test #'equal))
         (group-order nil)
         (database-append-tail nil)
         (index-splices nil)
         (cache-changes nil)
         (committed nil))
    (unwind-protect (progn
        (dolist (entry entries)
          (when (stringp (car entry))
            (let* ((clause (list (list predicate (car entry) (cdr entry))))
                   (database-cell (list clause))
                   (first-arg (car entry))
                   (group (gethash first-arg groups)))
              (if clauses-tail (setcdr clauses-tail database-cell)
                (setq clauses database-cell))
              (setq clauses-tail database-cell)
              (if group (let ((index-cell (list clause)))
                  (setcdr (aref group 1) index-cell)
                  (aset group 1 index-cell))
                (let ((index-cell (list clause)))
                  (puthash first-arg (vector index-cell index-cell) groups)
                  (push first-arg group-order))))))
        (let ((inhibit-quit t))
          (unless (or (gethash key (nskk-prolog-index-config)) old-database-head)
            (nskk-prolog-set-index predicate 2 :trie))
          (let* ((type (gethash key (nskk-prolog-index-config)))
                 (indexed-p (memq type '(:hash :trie)))
                 (index (and indexed-p (nskk-prolog-transaction-index key type)))
                 (cache-entry
                (and indexed-p (gethash key (nskk-prolog-index-bucket-tail-cache) missing)))
                 (cache-buckets
                (and
                  (vectorp cache-entry)
                  (= (length cache-entry) 3)
                  (eq (aref cache-entry 0) type)
                  (eq (aref cache-entry 1) index)
                  (hash-table-p (aref cache-entry 2))
                  (aref cache-entry 2))))
            (when clauses
              (if old-database-head (progn
                  (unless old-database-tail
                    (error "Missing database tail for %s" key))
                  (setq database-append-tail old-database-tail)
                  (setcdr old-database-tail clauses))
                (puthash key clauses (nskk-prolog-database)))
              (puthash key clauses-tail (nskk-prolog-database-tails)))
            (when indexed-p
              (dolist (first-arg group-order)
                (let* ((group (gethash first-arg groups))
                       (group-head (aref group 0))
                       (group-tail (aref group 1))
                       (old-bucket (nskk-prolog-transaction-index-bucket type index first-arg))
                       (old-tail-info (and cache-buckets (gethash first-arg cache-buckets missing)))
                       (old-tail-info-present-p (and cache-buckets (not (eq old-tail-info missing)))))
                  (when cache-buckets
                    (push
                      (vector cache-buckets first-arg old-tail-info-present-p old-tail-info)
                      cache-changes))
                  (let* ((old-tail (nskk-prolog-index-bucket-tail key type index first-arg old-bucket))
                         (old-tail-cdr (and old-tail (cdr old-tail)))
                         (new-bucket (or old-bucket group-head)))
                    (push
                      (vector type index first-arg old-bucket old-tail old-tail-cdr)
                      index-splices)
                    (when old-tail
                      (setcdr old-tail group-head))
                    (nskk-prolog-transaction-set-index-bucket type index first-arg new-bucket)
                    (nskk-prolog-index-cache-set-bucket
                      key
                      type
                      index
                      first-arg
                      new-bucket
                      group-tail))))
              (unless indexed-p
                (remhash key (nskk-prolog-index-bucket-tail-cache))))
            (when quit-flag
              (signal 'quit nil)))
          (setq committed t)))
      (let ((inhibit-quit t))
        (if committed (setq previous nil
                database-append-tail nil
                index-splices nil
                cache-changes nil)
          (dolist (change cache-changes)
            (let ((buckets (aref change 0))
                  (first-arg (aref change 1)))
              (if (aref change 2) (puthash first-arg (aref change 3) buckets)
                (remhash first-arg buckets))))
          (dolist (splice index-splices)
            (when (aref splice 4)
              (setcdr (aref splice 4) (aref splice 5)))
            (nskk-prolog-transaction-set-index-bucket
              (aref splice 0)
              (aref splice 1)
              (aref splice 2)
              (aref splice 3)))
          (when database-append-tail
            (setcdr database-append-tail old-database-tail-cdr))
          (nskk-dict-transaction-apply-predicate-snapshot previous))))))

(defun nskk--dict-load-from-cache ()
  "Replace system dictionary facts from a fully validated on-disk cache.
Returns entry count on success, or 0 if cache is unavailable or invalid."
  (let ((entries (nskk--dict-load-system-dict-from-cache)))
    (if entries (progn
        (nskk--dict-replace-predicate-entries 'system-dict-entry entries)
        (message "NSKK: Loaded %d entries from cache" (length entries))
        (length entries))
      0)))

(defun nskk--dict-load-from-files (dict-files)
  "Transactionally replace system facts from DICT-FILES.
Return the entry count, or 0 without changing facts when any read, staging,
cache publication, or predicate publication step fails."
  (condition-case
    err
    (let ((all-entries
          (cl-loop
            for
            file
            in
            dict-files
            append
            (nskk--dict-parse-file-to-entries-strict file))))
      (when all-entries
        (let ((staged (nskk--dict-stage-predicate-entries 'system-dict-entry all-entries)))
          (nskk--dict-commit-staged-predicate
            staged
            (when nskk-dict-cache-enabled
              (lambda ()
                (nskk--dict-save-system-dict-cache all-entries dict-files))))))
      (length all-entries))
    (error
      (message "NSKK: Dictionary load failed (%s)" (error-message-string err))
      0)))

(defvar nskk--dict-ja-dic-code-table nil
  "Hash table mapping ja-dic compact kana codes to Emacs characters.")

(defun nskk--dict-ja-dic-decode-key (codes)
  "Decode ja-dic compact key CODES into an NSKK reading string."
  (unless nskk--dict-ja-dic-code-table
    (setq nskk--dict-ja-dic-code-table (make-hash-table :test #'eql))
    (cl-loop
      for
      ch
      from
      #x3041
      to
      #x3096
      for
      jis
      =
      (encode-char ch 'japanese-jisx0208)
      when
      jis
      do
      (puthash (- (logand jis #xFF) 32) ch nskk--dict-ja-dic-code-table)))
  (apply
    #'string
    (mapcar
      (lambda (code)
        (cond
          ((zerop code) ?ー)
          ((< code 0) (- code))
          (t
            (or
              (gethash code nskk--dict-ja-dic-code-table)
              (error "NSKK: Unknown ja-dic compact code %S" code)))))
      codes)))

(defun nskk--dict-ja-dic-flatten-node (node prefix)
  "Recursively flatten ja-dic NODE using PREFIX compact codes.
Candidates at each leaf are stored as-is from the ja-dic tree.
For `skkdic-okuri-nasi', the stored order matches SKK-JISYO.L order.
For `skkdic-okuri-ari', `skkdic-extract-conversion-data' reverses
candidates via cons-accumulation; callers must reverse them back."
  (let (entries)
    (cl-labels ((walk (current current-prefix)
                  (let* ((code (car current))
                 (rest (cdr current))
                 (path (append current-prefix (list code)))
                 (candidates (car rest)))
            (when (and (listp candidates) (stringp (car candidates)))
              (push (cons (nskk--dict-ja-dic-decode-key path) candidates) entries)
              (setq rest (cdr rest)))
            (when (eq (car rest) t)
              (setq rest (cdr rest)))
            (dolist (child rest)
              (when (consp child)
                (walk child path))))))
      (walk node prefix))
    (nreverse entries)))

(defun nskk--dict-ja-dic-flatten-tree (tree &optional reverse-candidates)
  "Flatten ja-dic TREE into a list of (key . candidates) entries.
When REVERSE-CANDIDATES is non-nil, reverse each entry's candidate list.
This is needed for `skkdic-okuri-ari' where `skkdic-extract-conversion-data'
stores candidates in reversed order via cons-accumulation."
  (let (entries)
    (dolist (node (cdr tree))
      (when (consp node)
        (dolist (entry (nskk--dict-ja-dic-flatten-node node nil))
          (push entry entries))))
    (setq entries (nreverse entries))
    (if reverse-candidates (mapcar
        (lambda (entry)
          (cons (car entry) (reverse (cdr entry))))
        entries)
      entries)))

(defun nskk-dict-load-ja-dic ()
  "Load Emacs built-in `ja-dic' data as `system-dict-entry' facts.
Returns `system' when entries were loaded successfully, or nil otherwise."
  (condition-case
    err
    (when (load-library "ja-dic/ja-dic")
      (let ((entries
            (append
              (when (boundp 'skkdic-okuri-nasi)
                (nskk--dict-ja-dic-flatten-tree skkdic-okuri-nasi))
              (when (boundp 'skkdic-okuri-ari)
                (nskk--dict-ja-dic-flatten-tree skkdic-okuri-ari t)))))
        (when entries
          (nskk--dict-replace-predicate-entries 'system-dict-entry entries)
          (message "NSKK: Loaded ja-dic system dictionary (%d entries)" (length entries))
          'system)))
    (error
      (message "NSKK: Could not load ja-dic (%s)" (error-message-string err))
      nil)))

;;; Dictionary Parsing
(defun nskk-dict-parse-line (line)
  "Parse a single SKK dictionary LINE.
Returns (key . candidates-list) or nil for comments/invalid lines.
When `nskk-show-annotation' is non-nil and nskk-annotation is loaded,
also registers any candidate annotations found in the line."
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
        (when (and candidates
                   (boundp 'nskk-show-annotation)
                   nskk-show-annotation
                   (fboundp 'nskk-annotation-load-from-candidates))
          (let ((with-annots (nskk--dict-parse-candidates-with-annotations
                              candidates-str)))
            (nskk-annotation-load-from-candidates key with-annots)))
        (when candidates
          (cons key candidates))))))

(defun nskk--dict-parse-candidates (str)
  "Parse candidates from STR like \"/candidate1/candidate2/...\"."
  (when (and (stringp str) (> (length str) 1) (= (aref str 0) ?/))
    (let ((parts (split-string (substring str 1) "/" t)))
      (mapcar (lambda (c)
                (let ((semi (string-search ";" c)))
                  (if semi (substring c 0 semi) c)))
              parts))))

(defun nskk--dict-split-candidate-annotation (candidate-str)
  "Split CANDIDATE-STR into (candidate . annotation) cons cell.
If CANDIDATE-STR contains ';', returns (text-before-semi . text-after-semi).
Otherwise returns (CANDIDATE-STR . nil)."
  (let ((semi (string-search ";" candidate-str)))
    (if semi (cons (substring candidate-str 0 semi) (substring candidate-str (1+ semi)))
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
(defconst
  nskk--dict-cache-max-bytes
  (* 128 1024 1024)
  "Maximum accepted dictionary cache size in bytes.")

(defun nskk--dict-insert-file-contents-bounded (file coding-system)
    "Insert regular FILE into the current empty buffer within the byte limit.
CODING-SYSTEM nil means auto-detection.  Symbolic links to regular files
are supported.  Pin the resolved target before the bounded read so path
replacement cannot redirect the read to a FIFO or another file."
    (unless (file-regular-p file)
      (error "NSKK: Dictionary file is not a regular file"))
    (let* ((resolved-file (file-truename file))
           (attributes (file-attributes resolved-file 'integer)))
      (nskk-dict-transaction-insert-file-contents-pinned
       file resolved-file attributes nskk--dict-cache-max-bytes t)
      (set-buffer-multibyte t)
      (decode-coding-region
       (point-min)
       (point-max)
       (or coding-system 'undecided))))

(defun nskk--dict-parse-file-to-entries-strict (file &optional coding-system)
  "Parse FILE to entries using CODING-SYSTEM.
Signal any validation or I/O error."
  (unless (and (stringp file) (file-readable-p file))
    (error "NSKK: Dictionary file is not readable"))
  (let ((entries nil))
    (with-temp-buffer
      (nskk--dict-insert-file-contents-bounded file coding-system)
      (goto-char (point-min))
      (while
        (not (eobp))
        (let ((parsed
              (nskk-dict-parse-line
                (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
          (when parsed
            (push parsed entries)))
        (forward-line 1)))
    (nreverse entries)))

(defun nskk--dict-parse-file-to-entries (file &optional coding-system)
  "Parse SKK dictionary FILE using CODING-SYSTEM.
Do not modify the Prolog database.  Return parsed entries, or nil when FILE is
unreadable, too large, or invalid."
  (condition-case
    nil
    (nskk--dict-parse-file-to-entries-strict file coding-system)
    (error nil)))

(defun nskk-dict-load-file (file &optional coding-system predicate-name)
  "Load SKK dictionary from FILE into Prolog as PREDICATE-NAME/2 facts.
PREDICATE-NAME defaults to system-dict-entry.
CODING-SYSTEM defaults to nil which lets Emacs auto-detect encoding.
Returns PREDICATE-NAME on success, or nil when FILE is invalid, too large,
unreadable, or contains no valid entries."
  (when (and (stringp file) (file-readable-p file))
    (condition-case
      nil
      (let* ((pred (or predicate-name 'system-dict-entry))
             (entries (nskk--dict-parse-file-to-entries-strict file coding-system)))
        (when entries
          (nskk--dict-append-predicate-entries pred entries)
          pred))
      (error nil))))

(defun/k
  nskk-dict-load-system-dictionaries
  ()
  "Load system dictionaries from configured files or a validated cache.
Existing system facts are preserved when neither source yields entries.
Calls ON-FOUND with the symbol system if entries loaded; ON-NOT-FOUND otherwise."
  (let* ((dict-files nskk-dict-system-dictionary-files)
         (loaded
        (if (and nskk-dict-cache-enabled (nskk--dict-cache-valid-p dict-files)) (nskk--dict-load-from-cache)
          (nskk--dict-load-from-files dict-files))))
    (if (> loaded 0) (progn
        (message "NSKK: Dictionary initialization is complete (%d entries)" loaded)
        (succeed (quote system)))
      (message "NSKK: No system dictionaries found")
      (fail))))

;;; On-disk cache for system dictionaries
(defun nskk--dict-parse-user-file-to-entries (file)
  "Parse and validate all user dictionary lines in FILE.
Return entries only when FILE contains at least one valid entry and no
invalid data lines.  Comments and blank lines are ignored."
  (when (and (stringp file) (file-readable-p file))
    (condition-case
      nil
      (with-temp-buffer
        (nskk--dict-insert-file-contents-bounded file nil)
        (let ((entries nil)
              (valid t))
          (goto-char (point-min))
          (while
            (and valid (not (eobp)))
            (let ((line
                  (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (unless (or (string-empty-p (string-trim line)) (string-prefix-p ";;" line))
                (let ((entry (nskk-dict-parse-line line)))
                  (if entry (push entry entries)
                    (setq valid nil)))))
            (forward-line 1))
          (when (and valid entries)
            (nreverse entries))))
      (error nil))))

(defun nskk-dict-load-user-dictionary ()
  "Load the configured user dictionary.
Returns the symbol user if loaded, or nil if not found or invalid.
Existing user dictionary facts are preserved unless the entire file
validates and contains at least one entry."
  (when (and
      nskk-dict-user-dictionary-file
      (file-readable-p nskk-dict-user-dictionary-file))
    (message "NSKK: Loading user dictionary from %s" nskk-dict-user-dictionary-file)
    (let ((entries (nskk--dict-parse-user-file-to-entries nskk-dict-user-dictionary-file)))
      (when entries
        (nskk--dict-replace-predicate-entries 'user-dict-entry entries)
        'user))))

;;; On-disk cache for system dictionaries
(defun nskk--dict-cache-file-path ()
  "Return the path to the on-disk system dictionary cache."
  (expand-file-name "nskk/dict-cache.eld" user-emacs-directory))

(defun nskk--dict-file-older-than (file cache-mtime)
  "Return non-nil if FILE's mtime is strictly older than CACHE-MTIME.
Returns nil both when FILE cannot be stat'd (missing, unreadable, or broken
symlink) and when FILE is newer than or equal to CACHE-MTIME."
  (let ((attr (file-attributes file)))
    (and attr (time-less-p (file-attribute-modification-time attr) cache-mtime))))

(defun nskk--dict-cache-valid-p (dict-files)
  "Return non-nil if the cache file exists and is newer than all DICT-FILES."
  (let ((cache-path (nskk--dict-cache-file-path)))
    (and
      dict-files
      (let ((cache-attr (file-attributes cache-path)))
        (and
          cache-attr
          (file-readable-p cache-path)
          (let ((cache-mtime (file-attribute-modification-time cache-attr)))
            (cl-every
              (lambda (f)
                (nskk--dict-file-older-than f cache-mtime))
              dict-files)))))))

(defun nskk--dict-save-system-dict-cache (entries dict-files)
  "Serialize ENTRIES to the on-disk cache.
ENTRIES is a list of (kana . candidates-list) pairs.
DICT-FILES is the list of source files used to build the cache."
  (let ((cache-path (nskk--dict-cache-file-path)))
    (make-directory (file-name-directory cache-path) t)
    (nskk-dict-with-atomic-file
      cache-path
      (prin1
        (list :version 1 :source-files dict-files :entries entries)
        (current-buffer)))
    (message "NSKK: Cached %d entries to %s" (length entries) cache-path)))

(defun nskk--dict-cache-entry-p (entry)
  "Return non-nil when ENTRY has the serialized cache entry schema."
  (and
    (consp entry)
    (stringp (car entry))
    (not (string-empty-p (car entry)))
    (proper-list-p (cdr entry))
    (consp (cdr entry))
    (cl-every
      (lambda (candidate)
        (and (stringp candidate) (not (string-empty-p candidate))))
      (cdr entry))))

(defun nskk--dict-cache-data-entries (data)
  "Return validated entries from cache DATA, or nil."
  (when (and (proper-list-p data) (= (length data) 6))
    (let ((keys (cl-loop for (key _value) on data by (function cddr) collect key))
          (version (plist-get data :version))
          (stored (plist-get data :source-files))
          (entries (plist-get data :entries)))
      (when (and
          (= (length (delete-dups (copy-sequence keys))) 3)
          (cl-every
            (lambda (key)
              (memq key (quote (:version :source-files :entries))))
            keys)
          (eql version 1)
          (proper-list-p stored)
          (consp stored)
          (cl-every (function stringp) stored)
          (proper-list-p entries)
          (consp entries)
          (cl-every (function nskk--dict-cache-entry-p) entries)
          (nskk--dict-cache-source-valid-p stored))
        entries))))

(defun nskk--dict-load-system-dict-from-cache ()
  "Load and fully validate system dictionary entries from disk cache.
Returns a list of entry pairs, or nil on any size, syntax, schema, or
source configuration failure."
  (let ((cache-path (nskk--dict-cache-file-path)))
    (condition-case
      err
      (let* ((attributes (file-attributes cache-path))
             (size (and attributes (file-attribute-size attributes))))
        (when (and size (<= size nskk--dict-cache-max-bytes))
          (with-temp-buffer
            (nskk--dict-insert-file-contents-bounded cache-path nil)
            (let ((read-circle nil))
              (goto-char (point-min))
              (let ((data (read (current-buffer))))
                (skip-chars-forward " \\t\\r\\n")
                (when (eobp)
                  (nskk--dict-cache-data-entries data)))))))
      (error
        (message
          "NSKK: Cache read failed (%s), reloading from source"
          (error-message-string err))
        nil))))

;;; Global Dictionary State
(defvar nskk-dict-initialize-hook nil
  "Hook run after dictionary initialization is published successfully.
An ordinary error is reported per function without blocking later observers;
a `quit' condition propagates unchanged and stops the remaining functions.")

(defvar nskk--system-dict-index nil
  "Non-nil when system dictionary is loaded.
Value is the source symbol system.")

(defsubst
  nskk-dict-system-index
  ()
  "Return the system dictionary index, or nil if not initialized."
  nskk--system-dict-index)

(defun nskk-dict-set-system-index (value)
  "Set the system dictionary index to VALUE and return VALUE."
  (setq nskk--system-dict-index value))

(defvar nskk--user-dict-index nil
  "Non-nil when user dictionary is loaded.
Value is the source symbol \\='user.")

(defsubst nskk-dict-user-index ()
  "Return the user dictionary index, or nil if not initialized."
  nskk--user-dict-index)

(defun nskk-dict-set-user-index (value)
  "Set the user dictionary index to VALUE and return VALUE."
  (setq nskk--user-dict-index value))

(defconst nskk-dict-index-variables
  '(nskk--user-dict-index nskk--system-dict-index)
  "Symbols of the two dictionary-index state variables.
For code that needs symbol-level access to this state -- e.g. generic
save/restore machinery that operates on a list of symbols -- rather
than the getter/setter accessors above.")

(defconst
  nskk--dict-system-probe-paths
  (list
    (expand-file-name "~/.nix-profile/share/skk/SKK-JISYO.L")
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
          (mapcar
            (lambda (p)
              (expand-file-name "share/skk/SKK-JISYO.L" p))
            (split-string nix-profiles))))
         (large-dict-paths
        (when nskk-large-dictionary
          (list nskk-large-dictionary)))
         (candidates
        (append nskk--dict-system-probe-paths nix-profile-paths large-dict-paths)))
    (delete-dups
      (cl-remove-if-not
        (lambda (p)
          (and (stringp p) (file-readable-p p)))
        candidates))))

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
  (or
      (when nskk-dict-system-dictionary-files
        (nskk-dict-load-system-dictionaries))
      (when (eq nskk-dict-use-ja-dic t)
        (nskk-dict-load-ja-dic))
      (let ((dict-files (nskk--dict-detect-system-dictionaries)))
        (when dict-files
          (let ((nskk-dict-system-dictionary-files dict-files))
            (nskk-dict-load-system-dictionaries))))
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
  (nskk-prolog-retract-all 'dict-initialized 0)
  ;; Define okuri-consonant/1 fact table (inside guard so it survives
  ;; nskk-prolog-clear-database and is re-asserted on re-initialization)
  (nskk-prolog-define-fact-table okuri-consonant (:arity 1 :index :hash)
    (?k) (?s) (?t) (?n) (?h) (?m) (?y) (?r) (?w)
    (?g) (?z) (?d) (?b) (?p))
  (setq nskk--dict-okuri-consonants
        (nskk-prolog-query-all-values '(okuri-consonant \?c) '\?c))
  (setq nskk--system-dict-index (nskk--dict-initialize-system-dictionary))
  (setq nskk--user-dict-index (nskk-dict-load-user-dictionary))
  (nskk-dict-load-kakutei-dictionary)
  ;; Mark initialization complete (whether or not system dict was found).
  ;; This prevents repeated re-initialization across buffer enables.
  (progn
    (nskk-prolog-assert '((dict-initialized)))
    (nskk--dict-run-notification-hook
     'nskk-dict-initialize-hook "dict-initialize-hook"))
  (message "NSKK: Dictionary initialization is complete"))

(defun/k
  nskk--dict-lookup-okuri-ari
  (key)
  "Look up KEY for okuri-ari entries by appending each okuri consonant.
Returns candidates in consonant and solution order, retaining the first equal
candidate object, or calls on-not-found if no candidates are found."
  (let (candidate-lists)
    (dolist (consonant nskk--dict-okuri-consonants)
      (let* ((okuri-key (concat key (string consonant)))
             (solutions (nskk-prolog-query `(dict-entry ,okuri-key \?cands))))
        (dolist (solution solutions)
          (let ((candidates (nskk-prolog-walk '\?cands solution)))
            (when candidates
              (push candidates candidate-lists))))))
    (let ((candidates (nskk--dict-merge-candidate-lists (nreverse candidate-lists))))
      (if candidates (succeed candidates)
        (fail)))))

(defun/k
  nskk--dict-do-lookup
  (key)
  "Internal: perform the actual Prolog lookup for KEY.
User dictionary results take priority via clause ordering.
For keys without an explicit trailing lowercase okuri marker, also try
okuri-ari entries by appending each configured consonant."
  (let* ((okuri-nasi
        (nskk--dict-collect-candidates (nskk-prolog-query `(dict-entry ,key \?c))))
         (candidates
        (if (and (> (length key) 1) (not (string-match-p "[a-z]\\'" key))) (nskk--dict-merge-candidate-lists
            (list okuri-nasi (nskk--dict-lookup-okuri-ari key)))
          okuri-nasi)))
    (if candidates (succeed candidates)
      (fail))))

(defun/k
  nskk-dict-lookup
  (key)
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
  "Attempt to register WORD for READING as one atomic publication.
Returns t when Prolog dict-register/2, every update hook, and the success
message complete.  Returns nil only when the valid Prolog query has no
solution.  An `error' or `quit' from lazy loading, Prolog mutation, hooks, or
the message boundary restores the exact internal predicate, index marker,
dirty flag, and registered search caches before propagating the condition."
  (let* ((key (nskk-prolog-clause-key 'user-dict-entry 2))
         (owner (list 'nskk--dict-register-impl key)))
    (nskk-dict-transaction-ensure-rollback-complete owner)
    (let* ((previous-key-state (nskk-prolog-capture-key-state key reading t))
           (previous-user-index nskk--user-dict-index)
           (previous-modified nskk-dict-modified)
           (cache-snapshots (nskk-search-cache-snapshots)))
      (condition-case condition
          (prog1
              (progn
                (nskk-prolog-prepare-key-state-index-tail
                 previous-key-state)
                (unless nskk--user-dict-index
                  ;; Loading is part of this transaction: a later failure must
                  ;; not publish either the loaded facts or its index marker.
                  (setq nskk--user-dict-index
                        (nskk-dict-load-user-dictionary))
                  (unless nskk--user-dict-index
                    (nskk-prolog-set-index 'user-dict-entry 2 :trie)
                    (setq nskk--user-dict-index 'user)))
                (when (nskk-prolog-holds-p
                       `(dict-register ,reading ,word))
                  (setq nskk-dict-modified t)
                  ;; Registration hooks are the publication boundary.  Unlike
                  ;; best-effort notifications, the first failure aborts the
                  ;; transaction and prevents later hooks and the message.
                  (run-hooks 'nskk-jisyo-update-hook)
                  (message "NSKK: Registered %s -> %s"
                           (substring-no-properties reading)
                           (substring-no-properties word))
                  t))
            (nskk-dict-transaction-clear-pending-rollback owner))
        ((error quit)
         (nskk-dict-transaction-rollback-and-resignal
          owner condition
          (list
           (cons 'user-dict-predicate
                 (lambda ()
                   (nskk-prolog-restore-key-state previous-key-state)))
           (cons 'user-dict-index
                 (lambda ()
                   (setq nskk--user-dict-index previous-user-index)))
           (cons 'modified
                 (lambda ()
                   (setq nskk-dict-modified previous-modified)))
           (cons 'search-caches
                 (lambda ()
                   (dolist (snapshot cache-snapshots)
                     (nskk-search-restore-cache-snapshot snapshot)))))))))))

(progn
  (defconst nskk--dict-invalid-entry-message
    "Invalid user dictionary entry"
    "Fixed safe message used when a dictionary entry cannot be serialized.")

  (defun nskk--dict-valid-field-p (value allow-space)
    "Return non-nil when VALUE is safe for one SKK dictionary field.
ALLOW-SPACE permits ordinary spaces, as required for candidate words."
    (and (stringp value)
         (not (string-empty-p value))
         (cl-loop for character across value
                  always (and (> character 31)
                              (/= character 127)
                              (or allow-space (/= character 32))
                              (not (memq character (list ?/ ?\; ?▽ ?▼)))))))

  (defun nskk--dict-valid-key-p (key)
    "Return non-nil when KEY is representable as one SKK dictionary key."
    (nskk--dict-valid-field-p key nil))

  (defun nskk--dict-valid-word-p (word)
    "Return non-nil when WORD is representable as one SKK candidate.
Candidates may contain ordinary spaces but not slash, semicolon, preedit
markers, ASCII controls U+0000 through U+001F, or U+007F."
    (nskk--dict-valid-field-p word t)))

(defun/k nskk-dict-register-word (reading word)
  "Register WORD as a conversion candidate for READING in user dictionary.
Signals `nskk-dict-error` with a fixed safe message before any dictionary
state is observed or changed when READING or WORD cannot be serialized.
Otherwise uses the Prolog dict-register rule and returns non-nil on success;
calls on-not-found only when the valid registration query has no solution."
  (unless (and (nskk--dict-valid-key-p reading)
               (nskk--dict-valid-word-p word))
    (signal (quote nskk-dict-error)
            (list nskk--dict-invalid-entry-message)))
  (if (nskk--dict-register-impl reading word)
      (succeed t)
    (fail)))

(defun nskk--dict-unregister-impl (reading word)
  "Attempt to unregister WORD for READING from the Prolog user dictionary.
Returns t on success (Prolog dict-unregister/2 succeeded), nil on failure."
  (when (and
      nskk--user-dict-index
      (nskk-prolog-holds-p `(dict-unregister ,reading ,word)))
    (setq nskk-dict-modified t)
    (nskk--dict-run-update-hook)
    (message "NSKK: Unregistered %s -> %s"
             (substring-no-properties reading)
             (substring-no-properties word))
    t))

(defun/k
  nskk-dict-unregister-word
  (reading word)
  "Unregister WORD as a conversion candidate for READING from user dictionary.
Uses the Prolog dict-unregister rule which removes the word from an
existing entry (or retracts the entire entry if it was the sole candidate).
Returns non-nil (t) on success; calls on-not-found when READING or WORD
are empty/invalid or when the Prolog unregistration query fails."
  (if (and
      (stringp reading)
      (not (string-empty-p reading))
      (stringp word)
      (not (string-empty-p word))
      (nskk--dict-unregister-impl reading word)) (succeed t)
    (fail)))

;;; User Dictionary Save
(defvar nskk--persistence-inhibited nil
  "Non-nil while all NSKK persistence writes are temporarily inhibited.")

(defvar nskk--dict-save-inhibited nil
  "Non-nil while saving the user dictionary is temporarily inhibited.
The tutorial sets this while its mini dictionary replaces the real
`user-dict-entry' Prolog facts; saving during that window would
overwrite the personal dictionary file with tutorial data.")

;;;###autoload
(defun nskk-dict-save-user-dictionary ()
  "Save user dictionary to `nskk-dict-user-dictionary-file'.
Does nothing while NSKK persistence is inhibited."
  (interactive)
  (if (or nskk--dict-save-inhibited nskk--persistence-inhibited) (message "NSKK: User dictionary save inhibited (tutorial active)")
    (nskk--dict-save-user-dictionary-1)))

(defun nskk--dict-save-user-dictionary-1 ()
  "Write the current user-dictionary facts to disk unconditionally.
The complete snapshot is validated before any directory, temporary file, or
output file is created.  Invalid entries signal `nskk-dict-error` with a
fixed safe message while preserving the dirty state and stored snapshot."
  (when (and nskk-dict-user-dictionary-file nskk--user-dict-index)
    (let ((bindings
           (nskk-prolog-query-bindings
            (quote (user-dict-entry \?k \?c)) (quote (\?k \?c)))))
      ;; Validate the complete snapshot before creating a directory or file.
      (dolist (binding bindings)
        (let ((key (car binding))
              (candidates (cadr binding)))
          (unless (and (nskk--dict-valid-key-p key)
                       (proper-list-p candidates)
                       candidates
                       (seq-every-p (function nskk--dict-valid-word-p)
                                    candidates))
            (signal (quote nskk-dict-error)
                    (list nskk--dict-invalid-entry-message)))))
      ;; The personal dictionary records what the user types; keep newly
      ;; created files and directories private (existing modes are kept).
      (let ((dir (file-name-directory nskk-dict-user-dictionary-file)))
        (unless (file-directory-p dir)
          (with-file-modes #o700
            (make-directory dir t))))
      (nskk--dict-call-with-atomic-file
       nskk-dict-user-dictionary-file
       (lambda ()
         (insert ";; -*- mode: fundamental; coding: utf-8 -*-\n")
         (insert ";; NSKK user dictionary\n")
         (insert ";; okuri-nasi entries.\n")
         (dolist (binding bindings)
           (let ((key (car binding))
                 (candidates (cadr binding)))
             (insert (format "%s /%s/\n"
                             key
                             (string-join candidates "/"))))))
       (lambda ()
         (setq nskk-dict-modified nil)))
      (message "NSKK: User dictionary saved to %s"
               nskk-dict-user-dictionary-file))))

(defun nskk--dict-maybe-save ()
  "Save user dictionary if it has unsaved modifications.
Called from `kill-emacs-hook' to persist registrations on Emacs exit."
  (when nskk-dict-modified
    (condition-case
      err
      (nskk-dict-save-user-dictionary)
      (error
        (message "NSKK: Failed to save user dictionary: %s" (error-message-string err))))))

;;;; Confirmed Dictionary (確定辞書) Support
(defvar nskk--kakutei-dict-loaded nil
  "Non-nil when the confirmed (kakutei) dictionary has been loaded.")

(defun nskk-dict-load-kakutei-dictionary ()
  "Load the confirmed dictionary from `nskk-kakutei-jisyo' if configured.
The confirmed dictionary contains entries that are committed immediately
without candidate selection.  Entries are loaded as \\='kakutei-dict-entry/2
Prolog facts with trie indexing.
Returns \\='kakutei if loaded, nil otherwise."
  (when (and
      (boundp 'nskk-kakutei-jisyo)
      nskk-kakutei-jisyo
      (file-readable-p nskk-kakutei-jisyo))
    (message "NSKK: Loading kakutei dictionary from %s" nskk-kakutei-jisyo)
    (condition-case
      condition
      (when-let*
        ((entries (nskk--dict-parse-file-to-entries-strict nskk-kakutei-jisyo)))
        (let* ((key (nskk-prolog-clause-key 'kakutei-dict-entry 2))
               (previous (nskk-dict-transaction-predicate-snapshot key))
               (previous-loaded nskk--kakutei-dict-loaded)
               (committed nil))
          (unwind-protect (let ((staged
                  (nskk--dict-stage-predicate-entries
                    'kakutei-dict-entry
                    nil
                    (mapcar
                      (lambda (entry)
                        (list (list 'kakutei-dict-entry (car entry) (cdr entry))))
                      entries))))
              (let ((inhibit-quit t))
                (nskk--dict-publish-staged-predicate staged)
                (setq nskk--kakutei-dict-loaded t))
              (setq committed t)
              'kakutei)
            (unless committed
              (let ((inhibit-quit t))
                (nskk-dict-transaction-apply-predicate-snapshot previous)
                (setq nskk--kakutei-dict-loaded previous-loaded))))))
      (error nil)
      (quit (signal (car condition) (cdr condition))))))

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
            (succeed (car result))
          (fail)))
    (fail)))

(provide 'nskk-dictionary)

;;; nskk-dictionary.el ends here
