;;; nskk-search.el --- Dictionary search algorithms for NSKK -*- lexical-binding: t; -*-

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

;; Prefix and partial dictionary search for NSKK, plus persistence of the
;; learning scores that order their results.  Exact lookup lives in
;; nskk-dictionary.el; `nskk-core-search' in nskk-henkan.el is the entry
;; point that dispatches between them.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nskk-cps-macros)
(require 'nskk-dictionary)
(require 'nskk-dict-transaction)
(require 'nskk-prolog)
(require 'nskk-debug nil t)

;; nskk.el is the top-level orchestrator and requires this file (transitively
;; via nskk-henkan), so this file cannot `require' nskk.el back without a
;; circular dependency.  By the time these are actually called at runtime,
;; nskk.el has always finished loading; `declare-function' only silences the
;; byte-compiler, it does not load anything.
(declare-function nskk-learning-loaded "nskk" ())
(declare-function nskk-set-learning-loaded "nskk" (value))

;;;; Customization

(defgroup nskk-search nil
  "SKK dictionary search customization."
  :group 'nskk
  :prefix "nskk-search-")

(defcustom nskk-search-sort-method 'frequency
  "Sort method for search results."
  :type '(choice (const :tag "Frequency order" frequency)
                 (const :tag "Kana order" kana)
                 (const :tag "No sorting" none))
  :safe (lambda (v) (memq v '(frequency kana none)))
  :package-version '(nskk . "0.1.0")
  :group 'nskk-search)

(defcustom nskk-search-learning-file
  (expand-file-name "nskk/learning.dat" user-emacs-directory)
  "File path for persisting learning data."
  :type 'file
  :package-version '(nskk . "0.1.0")
  :group 'nskk-search)

(defcustom nskk-search-merge-user-dict-with-server nil
  "When non-nil, merge user dictionary candidates with skkserv results.

By default (nil), skkserv takes priority: when a server lookup succeeds
its candidates are used as-is and the local user dictionary is not
consulted, matching the historical NSKK search behavior.

When non-nil, the local dictionary (user-registered and learned words)
is searched first and its candidates are merged ahead of the server's,
with duplicates removed.  This is closer to ddskk, where the personal
dictionary is merged before the system/server dictionaries so that
registered and learned words always appear among the top candidates.

Only affects exact dictionary lookup (`nskk-core-search')."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-search)

(defcustom nskk-search-auto-save-learning t
  "When non-nil, persist learning data across Emacs sessions.
When enabled, learning scores are loaded from `nskk-search-learning-file'
the first time NSKK is enabled, and saved back on Emacs exit via
`kill-emacs-hook'.  If the optional study feature (`nskk-study') is
loaded, study association data is loaded and saved as well.

When nil, learning is kept in memory only and lost when Emacs exits."
  :type 'boolean
  :safe #'booleanp
  :package-version '(nskk . "0.1.0")
  :group 'nskk-search)

(defconst nskk--search-learning-max-file-size (* 10 1024 1024)
  "Maximum number of bytes accepted from the learning data file.")

(defvar nskk-save-history-hook nil
  "Hook run after learning data is successfully saved.
The save is performed by `nskk-search-save-learning-data'.
Each function is called with no arguments.  The hook fires only on successful
save; I/O errors suppress both the save and this hook.  An ordinary hook error
is reported per function without blocking later observers; a `quit' condition
propagates unchanged and stops the remaining functions.")

;;;; Prolog Search Strategy Facts

;; Learning score facts: (learning-score reading candidate score)
;; Hash-indexed on first arg (reading) for O(1) lookup by reading
(nskk-prolog-set-index 'learning-score 3 :hash)

;; Okuri-type matching rules: (okuri-type-matches filter-type entry-okuri-type)
;; okuri-ari matches only entries that have okurigana.
;; okuri-nasi matches only entries without okurigana.
;; any (nil caller-side) matches all entry types.
(nskk-prolog-define-fact-table okuri-type-matches (:arity 2 :index :hash)
  (okuri-ari  okuri-ari)
  (okuri-nasi okuri-nasi)
  (any        okuri-ari)
  (any        okuri-nasi))

;; Valid sort method membership: (search-sort-method METHOD)
(nskk-prolog-define-fact-table search-sort-method (:arity 1 :index :hash)
  (frequency) (kana) (none))

;; Derived rule: entry-matches-okuri-filter/2
;; (entry-matches-okuri-filter Filter OkuriSym) succeeds when OkuriSym
;; satisfies Filter.  The entry-okuri string is converted to a symbol
;; (okuri-ari / okuri-nasi) by Elisp before calling this rule so that
;; all filter-dispatch logic lives in the Prolog knowledge base.
(nskk-prolog-<- (entry-matches-okuri-filter \?filter \?okuri-sym)
  (okuri-type-matches \?filter \?okuri-sym))

;;; Notification hooks

(defun nskk--search-run-notification-hook (hook label)
  "Run notification HOOK, reporting each ordinary error with LABEL.
Each hook function is invoked separately so one failure does not block later
observers.  A `quit' condition is deliberately allowed to escape unchanged."
  (run-hook-wrapped
   hook
   (lambda (function)
     (condition-case err
         (funcall function)
       (error
        (message "NSKK: %s error: %s"
                 label (error-message-string err))))
     nil)))

;;; Okuri-type filtering

(defun nskk--search-derive-okuri (key)
  "Return the okurigana suffix of KEY, or nil when KEY is okuri-nasi.
An SKK okuri-ari key ends with a single ASCII lower-case letter that
directly follows a non-ASCII kana character (e.g. \"わるi\" -> \"i\",
\"うごk\" -> \"k\").  Keys shorter than two characters, keys whose final
character is not in [a-z], or keys whose penultimate character is ASCII
are treated as okuri-nasi and yield nil.

The returned string populates the `okuri' slot of `nskk-dict-entry' so
that `nskk--search-match-okuri-type-p' can classify entries.  The
classifier only distinguishes empty/nil (okuri-nasi) from non-empty
\(okuri-ari), so the exact suffix string is informational."
  (let ((len (length key)))
    (and (>= len 2)
         (let ((last (aref key (1- len)))
               (prev (aref key (- len 2))))
           (and (<= ?a last ?z)
                (>= prev 128)
                (substring key (1- len)))))))

(defun nskk--search-match-okuri-type-p (okuri-type entry-okuri)
  "Return non-nil if ENTRY-OKURI matches OKURI-TYPE filter.
OKURI-TYPE is \\='okuri-ari, \\='okuri-nasi, or nil (match all).
Converts ENTRY-OKURI string to a symbol, then delegates to the Prolog
`entry-matches-okuri-filter/2' rule (which wraps `okuri-type-matches/2')."
  (let ((filter    (or okuri-type 'any))
        (entry-sym (if (and entry-okuri (not (string= entry-okuri "")))
                       'okuri-ari
                     'okuri-nasi)))
    (nskk-prolog-holds-p `(entry-matches-okuri-filter ,filter ,entry-sym))))

;;; Prefix match search

(defun nskk--search-post-process-results (results okuri-type limit)
  "Apply standard post-processing to RESULTS.
Filters by OKURI-TYPE via Prolog entry-matches-okuri-filter/2, removes
duplicates, sorts by nskk-search-sort-method, and then applies LIMIT.
Returns the processed result list."
  (let* ((filtered (if okuri-type
                       (cl-remove-if-not
                        (lambda (result)
                          (nskk--search-match-okuri-type-p
                           okuri-type (nskk-dict-entry-okuri (cdr result))))
                        results)
                     results))
         (unique (nskk--search-dedup filtered)))
    (cond
     ((null limit) (nskk--search-sort-results unique))
     ((<= limit 0) nil)
     ((>= limit (length unique)) (nskk--search-sort-results unique))
     (t (nskk--search-top-results unique limit)))))

(defun/k nskk-search-prefix (index query okuri-type limit)
  "Perform prefix match search in INDEX for QUERY.
OKURI-TYPE specifies okurigana filtering: \\='okuri-ari, \\='okuri-nasi, or nil.
LIMIT is the maximum number of results.
Returns a list of (KEY . `nskk-dict-entry') pairs in the sync wrapper,
or nil when no results remain after filtering.
The /k variant calls ON-FOUND with that list, ON-NOT-FOUND otherwise."
  (let* ((pred (nskk-dict-index-predicate index))
         (raw-results (when pred (nskk-prolog-trie-prefix-search pred 2 query)))
         (results (mapcar (lambda (pair)
                            (cons (car pair)
                                  (make-nskk-dict-entry
                                   :key (car pair)
                                   :candidates (cdr pair)
                                   :okuri (nskk--search-derive-okuri (car pair)))))
                          raw-results))
         (processed (nskk--search-post-process-results results okuri-type limit)))
    (nskk-debug-log "[SEARCH] prefix: query=%s results=%d" query (length results))
    (if processed
        (succeed processed)
      (fail))))

(defun nskk--search-dedup (results)
  "Remove duplicate keys from RESULTS, keeping the first occurrence of each."
  (let ((seen (make-hash-table :test 'equal))
        (acc  nil))
    (dolist (item results)
      (let ((key (car item)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push item acc))))
    (nreverse acc)))

;;; Partial match search

(defun/k nskk-search-partial (index query okuri-type limit)
  "Perform partial match search in INDEX for QUERY.
OKURI-TYPE specifies okurigana filtering: okuri-ari, okuri-nasi, or nil.
LIMIT is the maximum number of results.
Returns a list of (KEY . nskk-dict-entry) pairs in the sync wrapper,
or nil when no results remain after filtering.
The /k variant calls ON-FOUND with that list, ON-NOT-FOUND otherwise."
  (let* ((pred (nskk-dict-index-predicate index))
         (results
          (when pred
            (cl-loop for sol in (nskk-prolog-query `(,pred \?k \?candidates))
                     for key = (nskk-prolog-walk (quote \?k) sol)
                     for cands = (nskk-prolog-walk (quote \?candidates) sol)
                     when (string-search query key)
                     collect (cons key
                                   (make-nskk-dict-entry
                                    :key key
                                    :candidates cands
                                    :okuri (nskk--search-derive-okuri key))))))
         (processed (nskk--search-post-process-results results okuri-type limit)))
    (if processed (succeed processed) (fail))))

;;; Sort functions

(defun nskk--search-effective-sort-method ()
  "Return the validated search sort method."
  (if (nskk-prolog-holds-p `(search-sort-method ,nskk-search-sort-method))
      nskk-search-sort-method
    'none))

(defun nskk--search-rank-key (method result)
  "Return the ordering key of RESULT under METHOD."
  (if (eq method 'frequency)
      (nskk--search-reading-score (car result) (cdr result))
    (car result)))

(defun nskk--search-ranked-better-p (method left right)
  "Return non-nil when ranked entry LEFT outranks RIGHT under METHOD.
LEFT and RIGHT are [KEY INDEX RESULT] vectors.  Equal keys are broken by
the original input INDEX, which keeps the ordering stable."
  (let ((left-key    (aref left 0))
        (left-index  (aref left 1))
        (right-key   (aref right 0))
        (right-index (aref right 1)))
    (pcase method
      ('frequency
       (or (> left-key right-key)
           (and (= left-key right-key)
                (< left-index right-index))))
      ('kana
       (or (string< left-key right-key)
           (and (string= left-key right-key)
                (< left-index right-index)))))))

(defun nskk--search-heap-swap (heap left right)
  "Exchange the HEAP entries at positions LEFT and RIGHT."
  (let ((value (aref heap left)))
    (aset heap left (aref heap right))
    (aset heap right value)))

(defun nskk--search-heap-sift-up (heap position method)
  "Move the HEAP entry at POSITION up while its parent outranks it.
METHOD selects the ordering."
  (while (> position 0)
    (let ((parent (/ (1- position) 2)))
      (cond
       ((nskk--search-ranked-better-p method (aref heap parent) (aref heap position))
        (nskk--search-heap-swap heap position parent)
        (setq position parent))
       (t (setq position 0))))))

(defun nskk--search-heap-sift-down (heap size position method)
  "Move the HEAP entry at POSITION down while it outranks a child.
SIZE is the number of occupied slots and METHOD selects the ordering."
  (let ((continue t))
    (while continue
      (let ((left (1+ (* 2 position))))
        (if (>= left size)
            (setq continue nil)
          (let* ((right (1+ left))
                 (worst (if (and (< right size)
                                 (nskk--search-ranked-better-p
                                  method (aref heap left) (aref heap right)))
                            right
                          left)))
            (cond
             ((nskk--search-ranked-better-p method (aref heap position) (aref heap worst))
              (nskk--search-heap-swap heap position worst)
              (setq position worst))
             (t (setq continue nil)))))))))

(defun nskk--search-best-ranked (results limit method)
  "Return the LIMIT best RESULTS under METHOD, best first.
The worst retained entry is kept at the heap root, so replacing it costs
O(log LIMIT) instead of re-sorting the whole input."
  (let ((heap (make-vector limit nil))
        (size 0))
    (cl-loop
     for result in results
     for index from 0
     for ranked = (vector (nskk--search-rank-key method result) index result)
     do (cond
         ((< size limit)
          (aset heap size ranked)
          (nskk--search-heap-sift-up heap size method)
          (cl-incf size))
         ((nskk--search-ranked-better-p method ranked (aref heap 0))
          (aset heap 0 ranked)
          (nskk--search-heap-sift-down heap size 0 method))))
    (let (selected)
      (dotimes (index size)
        (push (aref heap index) selected))
      (mapcar (lambda (ranked) (aref ranked 2))
              (sort selected
                    (lambda (left right)
                      (nskk--search-ranked-better-p method left right)))))))

(defun nskk--search-top-results (results limit)
  "Return the best LIMIT RESULTS in stable full-sort order."
  (let ((method (nskk--search-effective-sort-method)))
    (if (eq method 'none)
        (seq-take results limit)
      (nskk--search-best-ranked results limit method))))

(defun nskk--search-sort-results (results)
  "Sort search RESULTS according to the configured method."
  (pcase (nskk--search-effective-sort-method)
    ('frequency (nskk--search-sort-prefix-results results))
    ('kana      (nskk-search-sort-by-kana-order results))
    (_          results)))

(defun nskk-search-sort-by-kana-order (results)
  "Sort RESULTS in Japanese kana order."
  (sort results
        (lambda (a b)
          (string< (car a) (car b)))))

;;; Learning-based sorting

(defun nskk--search-sort-prefix-results (results)
  "Sort prefix search RESULTS by learning scores in descending order."
  (let ((scored (mapcar (lambda (item)
                          (cons (nskk--search-reading-score (car item) (cdr item))
                                item))
                        results)))
    (mapcar #'cdr
            (sort scored (lambda (a b)
                           (> (car a) (car b)))))))

(defun nskk--search-reading-score (reading entry)
  "Return the maximum learning score for READING and ENTRY."
  (if (nskk-dict-entry-p entry)
      (cl-loop for cand in (nskk-dict-entry-candidates entry)
               maximize (nskk--search-candidate-score reading cand))
    0))

(defun nskk--search-candidate-score (reading candidate)
  "Return learning score for CANDIDATE given READING from Prolog database."
  (let ((word (nskk--search-candidate-word candidate)))
    (or (and word (nskk-prolog-query-value
                   `(learning-score ,reading ,word \?s) '\?s))
        0)))

(defun nskk--search-candidate-word (candidate)
  "Extract the word string from CANDIDATE."
  (pcase candidate
    ((pred stringp) candidate)
    (`(,(pred stringp) . ,_) (car candidate))
    (_ nil)))


;;; Learning data management

(defun nskk--search-write-learning-file ()
  "Serialize every learning score to `nskk-search-learning-file'.
Return non-nil once the atomic rename has published the new file, so the
caller only announces a save that actually happened."
  (nskk-dict-write-private-file
   nskk-search-learning-file
   (nskk-dict-serialize-solutions '(learning-score \?r \?c \?s)
                                  '(\?r \?c \?s)))
  t)

;;;###autoload
(defun nskk-search-save-learning-data ()
  "Save learning data from Prolog facts to `nskk-search-learning-file'."
  (interactive)
  (if nskk--persistence-inhibited
      (message "NSKK: Learning data save inhibited (tutorial active)")
    (when
        (condition-case err
            (nskk--search-write-learning-file)
          (error
           (message "NSKK: Failed to save learning data: %s"
                    (error-message-string err))
           nil))
      (message "NSKK: Learning data saved")
      (nskk--search-run-notification-hook 'nskk-save-history-hook "save-history-hook"))))

;;;###autoload
(defun nskk-search-load-learning-data ()
  "Load learning data from `nskk-search-learning-file'.
Restore the `learning-score' Prolog facts serialized by
`nskk-search-save-learning-data'.  Reject non-regular or unstable files,
stage and validate every record, then publish the result transactionally."
  (interactive)
  (let ((file nskk-search-learning-file)
        (owner 'nskk-search-load-learning-data))
    (condition-case err
        (nskk--search-load-learning-file owner file)
      (error
       (message "NSKK: Failed to load learning data: %s"
                (error-message-string err))))))

(defun/done nskk--search-load-learning-file (owner file)
  "Replace the stored learning scores with FILE's contents for OWNER.
A file that is absent or unreadable leaves the existing scores in place."
  (nskk-dict-transaction-ensure-rollback-complete owner)
  (when-let* ((result (nskk--search-read-learning-facts file)))
    (nskk--search-publish-learning-facts owner (cdr result))))

(defun nskk--search-parse-learning-entry (entry)
  "Convert serialized learning ENTRY to a Prolog fact."
  (pcase entry
    (`(,(and (pred stringp) reading)
       ,(and (pred stringp) word)
       ,(and (pred integerp) score))
     `(learning-score ,reading ,word ,score))
    (_ (error "Invalid learning entry: %S" entry))))

(defun nskk--search-read-learning-facts (file)
  "Validate FILE and return its learning facts wrapped in a cons.
Return nil when FILE does not exist or is not readable.  Wrapping the
facts distinguishes a valid empty file from a skipped file.  An
oversized file is signaled rather than skipped, because a truncated
learning set would silently replace a complete one."
  (nskk-dict-transaction-load-entries
   file
   nskk--search-learning-max-file-size
   #'nskk--search-parse-learning-entry))

(defun nskk--search-learning-rollback-actions (loaded-value)
  "Build rollback actions restoring LOADED-VALUE.
The clause-store restore is contributed by
`nskk-dict-transaction-publish-facts'; this covers the search state that
lives outside it."
  (list
   (cons 'loaded-binding
         (lambda ()
           (nskk-set-learning-loaded loaded-value)))))

(defun nskk--search-publish-learning-facts (owner facts)
  "Replace learning facts with FACTS transactionally for OWNER."
  (let ((loaded-value (nskk-learning-loaded)))
    (nskk-dict-transaction-publish-facts
     owner 'learning-score 3 facts
     nil
     (nskk--search-learning-rollback-actions loaded-value))))

(defun/done nskk-search-learn (query candidate &optional context)
  "Record that CANDIDATE was selected for QUERY.
_CONTEXT is reserved for future use.
Stores learning score as a Prolog learning-score/3 fact.

Candidates marked with the nskk-no-learn text property are skipped."
  (ignore context)
  (let ((word (if (stringp candidate) candidate (car candidate))))
    (when (and word (not (get-text-property 0 'nskk-no-learn word)))
      (let* ((old-score (nskk-prolog-query-value
                         (list 'learning-score query word '\?s) '\?s))
             (old-fact (and old-score
                            (list 'learning-score query word old-score)))
             (new-score (1+ (or old-score 0)))
             (new-fact (list 'learning-score query word new-score)))
        (nskk-prolog-replace-clause-transaction
         old-fact (list new-fact)
         (lambda ()
           (nskk-debug-log
            "[SEARCH] learn: query=%s word=%s new-score=%d"
            query word new-score)
           nil))))))

(provide 'nskk-search)

;;; nskk-search.el ends here
