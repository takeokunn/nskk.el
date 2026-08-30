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

;; Dictionary search algorithms for NSKK (Layer 2: Domain).
;;
;; Layer position: L2 (Domain) -- depends on nskk-dictionary, nskk-cache,
;;   nskk-prolog, and nskk-custom.
;;
;; Provides an integrated search engine over the Prolog-backed dictionary
;; with four search modes, learning-based candidate ranking, and optional
;; result caching.
;;
;; Supported search types:
;; - Exact match (exact):   O(1) average via Prolog hash index
;; - Prefix match (prefix): O(k + n) via Prolog trie index
;; - Partial match (partial): O(n) substring scan
;; - Fuzzy match (fuzzy):   O(n * m) Levenshtein distance
;;
;; Performance targets:
;; - Exact match: < 0.1ms
;; - Prefix match (100 results): < 1ms
;; - Partial match (1000 entries): < 50ms
;; - Fuzzy match (1000 entries): < 100ms
;;
;; The search engine supports okurigana type filtering and customizable
;; sort order for candidate ranking.  Learning data is persisted via
;; Prolog `learning-score/3' facts and serialized to
;; `nskk-search-learning-file'.
;;
;; Prolog predicates maintained by this module:
;; - `search-strategy/1'              -- valid search type membership
;; - `search-sort-method/1'           -- valid sort method membership (frequency kana none)
;; - `learning-score/3'               -- (reading candidate score) usage frequency
;; - `okuri-type-matches/2'           -- (filter-type entry-type) okurigana match rules
;; - `entry-matches-okuri-filter/2'   -- derived rule; (filter okuri-sym) wraps
;;                                       okuri-type-matches/2 for filter dispatch
;;
;; Key public API:
;; - `nskk-search'              -- unified search dispatcher
;; - `nskk-search-exact'        -- exact match search
;; - `nskk-search-prefix'       -- prefix match search
;; - `nskk-search-partial'      -- partial (substring) match search
;; - `nskk-search-fuzzy'        -- fuzzy (Levenshtein) match search
;; - `nskk-search-with-cache'   -- cache-backed search
;; - `nskk-search-learn'        -- record candidate selection for learning
;; - `nskk-search-save-learning-data' -- persist learning data to file
;; - `nskk-search-jisyo-hook'         -- hook run after each successful search
;; - `nskk-save-history-hook'         -- hook run after each successful save
;;
;; Usage examples:
;;   (nskk-search index "かんじ" 'exact)
;;   ;; => nskk-dict-entry
;;
;;   (nskk-search index "かん" 'prefix nil 10)
;;   ;; => (("かん" . entry1) ("かんじ" . entry2) ...)
;;
;;   (nskk-search index "かんじ" 'fuzzy nil 5)
;;   ;; => (("かんじ" entry1 . 0) ("かんき" entry2 . 1) ...)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nskk-cps-macros)
(require 'nskk-dictionary)
(require 'nskk-dict-transaction)
(require 'nskk-cache)
(require 'nskk-prolog)
(require 'nskk-custom)
(require 'nskk-debug nil t)

;; nskk.el is the top-level orchestrator and requires this file (transitively
;; via nskk-henkan), so this file cannot `require' nskk.el back without a
;; circular dependency.  By the time these are actually called at runtime,
;; nskk.el has always finished loading; `declare-function' only silences the
;; byte-compiler, it does not load anything.
(declare-function nskk-learning-loaded "nskk" ())
(declare-function nskk-set-learning-loaded "nskk" (value))

(defconst nskk--search-learning-max-file-size (* 10 1024 1024)
  "Maximum number of bytes accepted from the learning data file.")

(defvar nskk-search-jisyo-hook nil
  "Hook run after a successful dictionary search via `nskk-search'.
Each function is called with no arguments.  The hook fires only on
success (when a result is found); failed searches and direct calls to
`nskk-search-exact', `nskk-search-prefix', etc. do not trigger it.
The hook does not fire on cache hits via `nskk-search-with-cache'.
An ordinary error is reported per function without blocking later observers;
a `quit' condition propagates unchanged and stops the remaining functions.")

(defvar nskk-save-history-hook nil
  "Hook run after learning data is successfully saved.
The save is performed by `nskk-search-save-learning-data'.
Each function is called with no arguments.  The hook fires only on successful
save; I/O errors suppress both the save and this hook.  An ordinary hook error
is reported per function without blocking later observers; a `quit' condition
propagates unchanged and stops the remaining functions.")

;;; Error definitions

(define-error 'nskk-dict-search-error
  "Dictionary search error"
  'nskk-dict-error)

(define-error 'nskk-dict-search-invalid-query
  "Invalid search query"
  'nskk-dict-search-error)

(define-error 'nskk-dict-search-invalid-index
  "Invalid dictionary index"
  'nskk-dict-search-error)

;;;; Prolog Search Strategy Facts

;; Search type dispatch rules
(nskk-prolog-define-fact-table search-strategy (:arity 1 :index :hash)
  (exact)
  (prefix)
  (partial)
  (fuzzy))

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

;;; Unified search interface

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

  (defun nskk--search-run-post-hook ()
    "Fire `nskk-search-jisyo-hook' after a successful search.
Ordinary hook errors are reported per function; `quit' propagates unchanged."
    (nskk--search-run-notification-hook
     'nskk-search-jisyo-hook "search-jisyo-hook"))

(defun/k nskk-search (index query &optional search-type okuri-type limit)
  "Search dictionary INDEX for QUERY.
SEARCH-TYPE is `exact', `prefix', `partial', or `fuzzy'.
OKURI-TYPE is `okuri-ari', `okuri-nasi', or nil (ignored for `fuzzy' searches).
LIMIT is the maximum number of results.
Sync wrapper return value is search-type dependent:
`nskk-dict-entry' for exact search, or a result list for prefix/partial/fuzzy.
The /k variant calls ON-FOUND on success and ON-NOT-FOUND when no result exists.

NOTE: The generated `nskk-search/k' variant places ON-FOUND and ON-NOT-FOUND
after the &optional parameters.  Callers MUST always pass both continuation
arguments explicitly; omitting them causes a silent nil-continuation crash,
because Emacs Lisp `&optional' applies to all parameters after the first."
  (unless (nskk-dict-index-p index)
    (signal 'nskk-dict-search-invalid-index (list index)))
  (unless (and (stringp query) (not (string-empty-p query)))
    (signal 'nskk-dict-search-invalid-query (list query)))

  ;; Default search type
  (setq search-type (or search-type 'exact))
  (nskk-debug-log "[SEARCH] search: query=%s type=%s" query search-type)

  ;; Validate against the Prolog search-strategy/1 fact table
  (unless (nskk-prolog-holds-p `(search-strategy ,search-type))
    (signal 'nskk-dict-search-invalid-query
            (list (format "Unknown search type: %s" search-type))))

  ;; Dispatch via sync wrappers (bindings are not CPS-transformed, so succeed/fail
  ;; appears exactly once below, eliminating the 4-branch duplication).
  (let ((result (pcase search-type
                  ('exact   (nskk-search-exact   index query okuri-type))
                  ('prefix  (nskk-search-prefix  index query okuri-type limit))
                  ('partial (nskk-search-partial index query okuri-type limit))
                  ('fuzzy   (nskk-search-fuzzy   index query limit)))))
    (if result
        (progn (nskk--search-run-post-hook) (succeed result))
      (fail))))

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
           ;; Final char ASCII lower-case AND preceding char non-ASCII kana.
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

;;; Exact match search

(defun/k nskk-search-exact (index query okuri-type)
  "Perform exact match search in INDEX for QUERY.
OKURI-TYPE specifies okurigana filtering: \\='okuri-ari, \\='okuri-nasi, or nil.
Returns an `nskk-dict-entry' in the sync wrapper, or nil when not found.
The /k variant calls ON-FOUND with the entry, ON-NOT-FOUND otherwise."
  (let* ((pred       (nskk-dict-index-predicate index))
         (candidates (when pred
                       (nskk-prolog-query-value
                        `(,pred ,query \?candidates) '\?candidates)))
         (entry      (when candidates
                       (make-nskk-dict-entry :key query :candidates candidates
                                             :okuri (nskk--search-derive-okuri query)))))
    (nskk-debug-log "[SEARCH] exact: query=%s found=%s" query (and candidates t))
    (if (and entry
             (nskk--search-match-okuri-type-p okuri-type (nskk-dict-entry-okuri entry)))
        (succeed entry)
      (fail))))

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
         ;; Convert raw (key . candidates) pairs to (key . nskk-dict-entry) pairs
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

(defun nskk--search-dedup (results &optional key-fn merge-fn)
  "Remove duplicates from RESULTS, keeping one item per key.
KEY-FN extracts the dedup key from an item (default: `car').
MERGE-FN is called as (MERGE-FN EXISTING NEW) and should return non-nil
when NEW should replace EXISTING; default keeps the first occurrence.

Used for both ordinary and fuzzy results:
  Ordinary: (nskk--search-dedup results)
  Fuzzy:    (nskk--search-dedup results #\\='car
            (lambda (e n) (< (cddr n) (cddr e))))"
  (let ((key-fn   (or key-fn #'car))
        (seen     (make-hash-table :test 'equal))
        (acc      nil))
    (dolist (item results)
      (let* ((key      (funcall key-fn item))
             (existing (gethash key seen)))
        (cond
         ((null existing)
          (puthash key item seen)
          (push item acc))
         ((and merge-fn (funcall merge-fn existing item))
          (puthash key item seen)
          (setq acc (cons item (delete existing acc)))))))
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

;;; Fuzzy search

(defun/k nskk-search-fuzzy (index query limit)
  "Perform fuzzy search in INDEX for QUERY using bounded Levenshtein distance.
LIMIT is the maximum number of results.
Returns a list in the sync wrapper, or nil when no match is within
`nskk-search-fuzzy-threshold'.  The /k variant calls ON-FOUND with the list
or ON-NOT-FOUND when no candidate qualifies.

Each element in the results list has the shape (KEY ENTRY . DISTANCE)
where KEY is a string, ENTRY is an `nskk-dict-entry', and DISTANCE is
the integer Levenshtein distance from QUERY."
  (let* ((pred (nskk-dict-index-predicate index))
         (raw
          (when pred
            (cl-loop for sol      in (nskk-prolog-query `(,pred \?k \?candidates))
                     for key       = (nskk-prolog-walk '\?k sol)
                     for cands     = (nskk-prolog-walk '\?candidates sol)
                     for distance  = (nskk--search-levenshtein-distance-bounded query key nskk-search-fuzzy-threshold)
                     when (<= distance nskk-search-fuzzy-threshold)
                       collect (cons key (cons (make-nskk-dict-entry
                                                :key key :candidates cands
                                                :okuri (nskk--search-derive-okuri key))
                                               distance)))))
         ;; Pipeline: deduplicate (keep closest) → sort by distance → limit
         (deduped (nskk--search-dedup raw #'car
                                      (lambda (existing new)
                                        (< (cddr new) (cddr existing)))))
         (sorted  (sort deduped (lambda (a b) (< (cddr a) (cddr b)))))
         (results (if (and limit (> (length sorted) limit))
                      (seq-take sorted limit)
                    sorted)))
    (if results (succeed results) (fail))))

(defun nskk--search-levenshtein-distance-bounded (s1 s2 max-distance)
  "Return the edit distance between S1 and S2 up to MAX-DISTANCE.
Return one more than MAX-DISTANCE when the exact distance exceeds the bound.
Use two banded rows and reject impossible length differences before allocation."
  (if (< max-distance 0)
      (1+ max-distance)
    (let* ((a (if (< (length s1) (length s2)) s2 s1))
           (b (if (< (length s1) (length s2)) s1 s2))
           (len-a (length a))
           (len-b (length b))
           (sentinel (1+ max-distance)))
      (if (> (- len-a len-b) max-distance)
          sentinel
        (let ((previous (make-vector (1+ len-b) sentinel))
              (current (make-vector (1+ len-b) sentinel)))
          (dotimes (j (1+ (min len-b max-distance)))
            (aset previous j j))
          (dotimes (i len-a)
            (let* ((row (1+ i))
                   (start (max 1 (- row max-distance)))
                   (end (min len-b (+ row max-distance)))
                   (previous-end (min len-b (+ i max-distance))))
              (aset current (1- start) (if (= start 1) row sentinel))
              (when (> end previous-end)
                (aset previous end sentinel))
              (cl-loop for j from start to end
                       for cost = (if (char-equal (aref a i) (aref b (1- j))) 0 1)
                       do (aset current j
                                (min (1+ (aref previous j))
                                     (1+ (aref current (1- j)))
                                     (+ (aref previous (1- j)) cost)))))
            (let ((swap previous))
              (setq previous current
                    current swap)))
          (min sentinel (aref previous len-b)))))))

  (defun nskk--search-levenshtein-distance (s1 s2)
  "Compute the exact Levenshtein distance between S1 and S2 with two rows."
  (let* ((a
          (if (< (length s1) (length s2)) s2
            s1))
         (b
          (if (< (length s1) (length s2)) s1
            s2))
         (len-a (length a))
         (len-b (length b))
         (previous (make-vector (1+ len-b) 0))
         (current (make-vector (1+ len-b) 0)))
    (dotimes (j (1+ len-b))
      (aset previous j j))
    (dotimes (i len-a)
      (aset current 0 (1+ i))
      (dotimes (j len-b)
        (let ((cost
               (if (char-equal (aref a i) (aref b j)) 0
                 1)))
          (aset
           current
           (1+ j)
           (min
            (1+ (aref previous (1+ j)))
            (1+ (aref current j))
            (+ (aref previous j) cost)))))
      (let ((swap previous))
        (setq previous current
              current swap)))
    (aref previous len-b)))

;;; Sort functions

(defun nskk--search-effective-sort-method ()
    "Return the validated search sort method."
    (if (nskk-prolog-holds-p `(search-sort-method ,nskk-search-sort-method))
        nskk-search-sort-method
      'none))

  (defun nskk--search-top-results (results limit)
  "Return the best LIMIT RESULTS in stable full-sort order."
  (let ((method (nskk--search-effective-sort-method)))
    (if (eq method 'none)
        (seq-take results limit)
      (let ((heap (make-vector limit nil))
            (size 0))
        (cl-labels
            ((candidate-better-p
              (key index ranked)
              (let ((right-key (aref ranked 0))
                    (right-index (aref ranked 1)))
                (pcase method
                  ('frequency
                   (or (> key right-key)
                       (and (= key right-key)
                            (< index right-index))))
                  ('kana
                   (or (string< key right-key)
                       (and (string= key right-key)
                            (< index right-index)))))))
             (better-p
              (left right)
              (candidate-better-p (aref left 0) (aref left 1) right))
             (worse-p (left right) (better-p right left))
             (swap (left right)
               (let ((value (aref heap left)))
                 (aset heap left (aref heap right))
                 (aset heap right value)))
             (sift-up
              (position)
              (while (> position 0)
                (let ((parent (/ (1- position) 2)))
                  (if (worse-p (aref heap position) (aref heap parent))
                      (progn
                        (swap position parent)
                        (setq position parent))
                    (setq position 0)))))
             (sift-down
              (position)
              (let ((continue t))
                (while continue
                  (let ((left (1+ (* 2 position))))
                    (if (>= left size)
                        (setq continue nil)
                      (let* ((right (1+ left))
                             (worst
                              (if (and (< right size)
                                       (worse-p (aref heap right)
                                                (aref heap left)))
                                  right
                                left)))
                        (if (worse-p (aref heap worst)
                                     (aref heap position))
                            (progn
                              (swap position worst)
                              (setq position worst))
                          (setq continue nil)))))))))
          ;; Keep the worst retained result at the root for O(log LIMIT) replacement.
          (cl-loop
           for result in results
           for index from 0
           for key = (if (eq method 'frequency)
                         (nskk--search-reading-score (car result) (cdr result))
                       (car result))
           do
           (if (< size limit)
               (progn
                 (aset heap size (vector key index result))
                 (sift-up size)
                 (cl-incf size))
             (when (candidate-better-p key index (aref heap 0))
               (aset heap 0 (vector key index result))
               (sift-down 0))))
          (let (selected)
            (dotimes (index size)
              (push (aref heap index) selected))
            (mapcar (lambda (ranked) (aref ranked 2))
                    (sort selected #'better-p))))))))

  (defun/k nskk--search-sort-results (results)
    "Sort search RESULTS according to the configured method."
    (pcase (nskk--search-effective-sort-method)
      ('frequency (succeed (nskk--search-sort-prefix-results results)))
      ('kana      (succeed (nskk-search-sort-by-kana-order results)))
      (_          (succeed results))))

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


;;; Search cache invalidation

;; The cache passed to `nskk-search-with-cache' is caller-owned; this module
;; keeps no singleton.  To flush every live cache when the dictionary changes
;; we register each cache the first time it is used.  Weak keys ensure a cache
;; drops out of the registry once the caller stops referencing it, so the
;; registry never keeps a cache alive on its own.
(defvar nskk--search-registered-caches
  (make-hash-table :test 'eq :weakness 'key)
  "Weak registry of caches passed to `nskk-search-with-cache'.
Keys are cache objects; values are unused.  Iterated by
`nskk--search-flush-caches' to invalidate every live search cache when
the dictionary or learning data changes.")

(defun nskk--search-register-cache (cache)
  "Record CACHE in `nskk--search-registered-caches' for later flushing."
  (puthash cache t nskk--search-registered-caches))

(defun nskk--search-flush-caches ()
  "Clear every registered search cache.
Installed on `nskk-jisyo-update-hook' at load time and also invoked after
`nskk-search-learn', so dictionary mutations (word registration or
unregistration) and learning-score updates never return stale cached
candidates.  A full clear is used because it is cheap relative to the
cost of a stale hit."
  (maphash (lambda (cache _)
             (when (nskk-cache-p cache)
               (nskk-cache-clear cache)))
           nskk--search-registered-caches))

;; Dictionary mutations run `nskk-jisyo-update-hook' (see nskk-dictionary.el).
(add-hook 'nskk-jisyo-update-hook #'nskk--search-flush-caches)
  (add-hook 'nskk-dict-initialize-hook #'nskk--search-flush-caches)

  (defun nskk--search-hash-table-snapshot (table)
    "Return TABLE and its exact key/value entries for rollback."
    (let (entries)
      (maphash (lambda (key value)
                 (push (cons key value) entries))
               table)
      (cons table entries)))

  (defun nskk--search-restore-hash-table-snapshot (snapshot)
    "Restore the hash table recorded in SNAPSHOT in place."
    (let ((table (car snapshot)))
      (clrhash table)
      (dolist (entry (cdr snapshot))
        (puthash (car entry) (cdr entry) table))))

  (defun nskk--search-cache-snapshot (cache)
    "Return an exact rollback snapshot for CACHE."
    (cond
     ((nskk-cache-lru-p cache)
      (let ((head (nskk-cache-lru-head cache))
            (tail (nskk-cache-lru-tail cache)))
        (vector 'lru
                cache
                (nskk-cache-lru-capacity cache)
                (nskk-cache-lru-size cache)
                (nskk--search-hash-table-snapshot
                 (nskk-cache-lru-hash cache))
                head
                tail
                (nskk-cache-lru-node-next head)
                (nskk-cache-lru-node-prev tail)
                (nskk-cache-lru-hits cache)
                (nskk-cache-lru-misses cache))))
     ((nskk-cache-lfu-p cache)
      (vector 'lfu
              cache
              (nskk-cache-lfu-capacity cache)
              (nskk-cache-lfu-size cache)
              (nskk--search-hash-table-snapshot
               (nskk-cache-lfu-hash cache))
              (nskk--search-hash-table-snapshot
               (nskk-cache-lfu-freq cache))
              (nskk-cache-lfu-min-freq cache)
              (nskk-cache-lfu-hits cache)
              (nskk-cache-lfu-misses cache)))))

  (defun nskk-search-restore-cache-snapshot (snapshot)
    "Restore CACHE state recorded in SNAPSHOT in place."
    (pcase (aref snapshot 0)
      ('lru
       (let ((cache (aref snapshot 1))
             (head (aref snapshot 5))
             (tail (aref snapshot 6)))
         (nskk--search-restore-hash-table-snapshot (aref snapshot 4))
         (setf (nskk-cache-lru-capacity cache) (aref snapshot 2)
               (nskk-cache-lru-size cache) (aref snapshot 3)
               (nskk-cache-lru-hash cache) (car (aref snapshot 4))
               (nskk-cache-lru-head cache) head
               (nskk-cache-lru-tail cache) tail
               (nskk-cache-lru-node-next head) (aref snapshot 7)
               (nskk-cache-lru-node-prev tail) (aref snapshot 8)
               (nskk-cache-lru-hits cache) (aref snapshot 9)
               (nskk-cache-lru-misses cache) (aref snapshot 10))))
      ('lfu
       (let ((cache (aref snapshot 1)))
         (nskk--search-restore-hash-table-snapshot (aref snapshot 4))
         (nskk--search-restore-hash-table-snapshot (aref snapshot 5))
         (setf (nskk-cache-lfu-capacity cache) (aref snapshot 2)
               (nskk-cache-lfu-size cache) (aref snapshot 3)
               (nskk-cache-lfu-hash cache) (car (aref snapshot 4))
               (nskk-cache-lfu-freq cache) (car (aref snapshot 5))
               (nskk-cache-lfu-min-freq cache) (aref snapshot 6)
               (nskk-cache-lfu-hits cache) (aref snapshot 7)
               (nskk-cache-lfu-misses cache) (aref snapshot 8))))))

  (defun nskk-search-cache-snapshots ()
    "Snapshot every registered search cache for transactional rollback."
    (let (snapshots)
      (maphash (lambda (cache _)
                 (when-let* ((snapshot
                              (nskk--search-cache-snapshot cache)))
                   (push snapshot snapshots)))
               nskk--search-registered-caches)
      snapshots))

;;; Learning data management

;;;###autoload
(defun nskk-search-save-learning-data ()
  "Save learning data from Prolog facts to `nskk-search-learning-file'."
  (interactive)
  (if nskk--persistence-inhibited
      (message "NSKK: Learning data save inhibited (tutorial active)")
    (when
        (condition-case err
            (progn
              (let ((dir (file-name-directory nskk-search-learning-file)))
                (unless (file-directory-p dir)
                  ;; Learning scores record the user's conversion history; keep
                  ;; the directory private when this code has to create it.
                  (with-file-modes #o700
                    (make-directory dir t))))
              (nskk-dict-with-atomic-file nskk-search-learning-file
                (let ((solutions (nskk-prolog-query '(learning-score \?r \?c \?s))))
                  (prin1
                   (mapcar (lambda (sol)
                             (list (nskk-prolog-walk '\?r sol)
                                   (nskk-prolog-walk '\?c sol)
                                   (nskk-prolog-walk '\?s sol)))
                           solutions)
                   (current-buffer))))
              t)
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
        (progn
          (nskk-dict-transaction-ensure-rollback-complete owner)
          (cond
           ((file-symlink-p file)
            (error "Refusing symbolic-link learning file: %s" file))
           ((not (file-exists-p file)) nil)
           ((not (file-regular-p file))
            (error "Learning data is not a regular file: %s" file))
           ((not (file-readable-p file)) nil)
           (t
            (let* ((attributes (file-attributes file 'integer))
                   (size (and attributes
                              (file-attribute-size attributes))))
              (unless attributes
                (error "Cannot inspect learning file: %s" file))
              (unless (integerp size)
                (error "Invalid learning file size: %S" size))
              (when (> size nskk--search-learning-max-file-size)
                (error "Learning file exceeds %d-byte limit"
                       nskk--search-learning-max-file-size))
              (let* ((facts
                      (nskk-dict-transaction-read-entries
                       file (file-truename file) attributes
                       nskk--search-learning-max-file-size
                       (lambda (entry)
                         (pcase entry
                           (`(,(and (pred stringp) reading)
                              ,(and (pred stringp) word)
                              ,(and (pred integerp) score))
                            `(learning-score ,reading ,word ,score))
                           (_ (error "Invalid learning entry: %S" entry))))))
                     (key (nskk-prolog-clause-key 'learning-score 3))
                     (previous (nskk-dict-transaction-predicate-snapshot key))
                     (cache-snapshots (nskk-search-cache-snapshots))
                     (loaded-value (nskk-learning-loaded)))
                (condition-case condition
                    (prog1
                        (progn
                          (nskk-prolog-retract-all 'learning-score 3)
                          (dolist (fact facts)
                            (nskk-prolog-assert (list fact)))
                          (nskk--search-flush-caches))
                      (nskk-dict-transaction-clear-pending-rollback owner))
                  ((error quit)
                   (let ((index 0))
                     (nskk-dict-transaction-rollback-and-resignal
                      owner
                      condition
                      (append
                       (list
                        (cons
                         'predicate
                         (lambda ()
                           (nskk-dict-transaction-apply-predicate-snapshot previous))))
                       (mapcar
                        (lambda (snapshot)
                          (prog1
                              (cons
                               (list 'cache index)
                               (lambda ()
                                 (nskk-search-restore-cache-snapshot snapshot)))
                            (setq index (1+ index))))
                        cache-snapshots)
                       (list
                        (cons
                         'loaded-binding
                         (lambda ()
                           (nskk-set-learning-loaded loaded-value))))))))))))))
      (error
       (message "NSKK: Failed to load learning data: %s"
                (error-message-string err))))))

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
           (nskk--search-flush-caches)
           nil))))))

;;; Cache-backed search

(defun nskk--search-cache-key (index query search-type okuri-type &optional limit)
  "Generate a cache key for INDEX, QUERY, SEARCH-TYPE, OKURI-TYPE, and LIMIT."
  (let ((type (or search-type (quote exact))))
    (list :predicate (nskk-dict-index-predicate index)
          :query query
          :search-type type
          :okuri-type (or okuri-type (quote none))
          :limit (or limit (quote none))
          :sort-method nskk-search-sort-method
          :fuzzy-threshold (and (eq type (quote fuzzy))
                                nskk-search-fuzzy-threshold))))

(defun nskk--search-copy-cache-value (value)
    "Return a detached copy of cache VALUE.
Signal wrong-type-argument when VALUE contains a hash table.  The traversal
is cycle-safe and includes string text-property values."
    (let ((seen (make-hash-table :test (function eq)))
          (pending (list value)))
      (while pending
        (let ((current (pop pending)))
          (unless (gethash current seen)
            (puthash current t seen)
            (cond
             ((hash-table-p current)
              (signal (quote wrong-type-argument)
                      (list (quote nskk-search-cache-value-without-hash-tables-p)
                            current)))
             ((functionp current) nil)
             ((consp current)
              (push (car current) pending)
              (push (cdr current) pending))
             ((stringp current)
              (let ((position 0)
                    (limit (length current)))
                (while (< position limit)
                  (let ((properties (text-properties-at position current)))
                    (while properties
                      (setq properties (cdr properties))
                      (push (car properties) pending)
                      (setq properties (cdr properties))))
                  (setq position
                        (next-property-change position current limit)))))
             ((bool-vector-p current) nil)
             ((recordp current)
              (let ((index 1))
                (while (< index (length current))
                  (push (aref current index) pending)
                  (setq index (1+ index)))))
             ((vectorp current)
              (let ((index 0))
                (while (< index (length current))
                  (push (aref current index) pending)
                  (setq index (1+ index)))))))))
      (nskk-prolog-copy-term value)))

  (defun/k nskk-search-with-cache (cache index query &optional search-type okuri-type limit)
    "Search INDEX for QUERY using CACHE for result caching.
Returns the cached or fresh result via ON-FOUND when candidates exist,
or calls ON-NOT-FOUND when no candidates are found.
SEARCH-TYPE, OKURI-TYPE, and LIMIT are passed to the underlying search on
cache miss.  The sync wrapper returns the same value shape.

The jisyo hook fires on cache misses, but does not fire on cache hits."
    (unless (nskk-cache-p cache)
      (signal (quote wrong-type-argument) (list (quote nskk-cache-p) cache)))
    (nskk--search-register-cache cache)
    (let ((cache-key (nskk--search-cache-key index query search-type okuri-type limit)))
      (<-or cached nskk-cache-get-prepared cache cache-key
            (function nskk--search-copy-cache-value)
        :found (progn
                 (nskk-debug-log "[SEARCH] cache-hit: key=%s" cache-key)
                 (succeed cached))
        :fail (progn
                (nskk-debug-log "[SEARCH] cache-miss: key=%s" cache-key)
                (<-seq [result (nskk-search index query search-type okuri-type limit)]
                  (let* ((canonical-result (nskk--search-copy-cache-value result)) (public-result (nskk--search-copy-cache-value canonical-result))) (nskk-cache-put cache cache-key canonical-result) (succeed public-result)))))))

(provide 'nskk-search)

;;; nskk-search.el ends here
