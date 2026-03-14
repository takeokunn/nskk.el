;;; nskk-search.el --- Dictionary search algorithms for NSKK -*- lexical-binding: t; -*-

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
(require 'nskk-cps-macros)
(require 'nskk-dictionary)
(require 'nskk-cache)
(require 'nskk-prolog)
(require 'nskk-custom)
(require 'nskk-debug nil t)

(defvar nskk-search-jisyo-hook nil
  "Hook run after a successful dictionary search via `nskk-search'.
Each function is called with no arguments.  The hook fires only on
success (when a result is found); failed searches and direct calls to
`nskk-search-exact', `nskk-search-prefix', etc. do not trigger it.
The hook does not fire on cache hits via `nskk-search-with-cache'.
Hook functions must not signal errors; errors are silently suppressed
to protect the CPS continuation chain.")

(defvar nskk-save-history-hook nil
  "Hook run after learning data is successfully saved by
`nskk-search-save-learning-data'.  Each function is called with no
arguments.  The hook fires only on successful save; I/O errors
suppress both the save and this hook.")

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

(defun nskk--search-run-post-hook ()
  "Fire `nskk-search-jisyo-hook' after a successful search.
Errors in hook functions are suppressed to protect the CPS chain."
  (when nskk-search-jisyo-hook
    (ignore-errors (run-hooks 'nskk-search-jisyo-hook))))

;;;###autoload
(defun/k nskk-search (index query &optional search-type okuri-type limit)
  "Search dictionary INDEX for QUERY.
SEARCH-TYPE is `exact', `prefix', `partial', or `fuzzy'.
OKURI-TYPE is `okuri-ari', `okuri-nasi', or nil (ignored for `fuzzy' searches).
LIMIT is the maximum number of results.

NOTE: The generated `nskk-search/k' variant places ON-FOUND and ON-NOT-FOUND
after the &optional parameters.  Callers MUST always pass both continuation
arguments explicitly; omitting them causes a silent nil-continuation crash,
because Emacs Lisp `&optional' applies to all parameters after the first."
  (unless (nskk-dict-index-p index)
    (signal 'nskk-dict-search-invalid-index (list index)))
  (unless (and (stringp query) (> (length query) 0))
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
OKURI-TYPE specifies okurigana filtering: \\='okuri-ari, \\='okuri-nasi, or nil."
  (let* ((pred       (nskk-dict-index-predicate index))
         (candidates (when pred
                       (nskk-prolog-query-value
                        `(,pred ,query \?candidates) '\?candidates)))
         (entry      (when candidates
                       (make-nskk-dict-entry :key query :candidates candidates))))
    (nskk-debug-log "[SEARCH] exact: query=%s found=%s" query (if candidates t nil))
    (if (and entry
             (nskk--search-match-okuri-type-p okuri-type (nskk-dict-entry-okuri entry)))
        (succeed entry)
      (fail))))

;;; Prefix match search

(defun nskk--search-post-process-results (results okuri-type limit)
  "Apply standard post-processing to RESULTS.
Filters by OKURI-TYPE via Prolog `entry-matches-okuri-filter/2', removes
duplicates, applies LIMIT, and sorts by `nskk-search-sort-method'.
Returns the processed result list."
  (let* ((filtered (if okuri-type
                       (cl-remove-if-not
                        (lambda (result)
                          (nskk--search-match-okuri-type-p
                           okuri-type (nskk-dict-entry-okuri (cdr result))))
                        results)
                     results))
         (unique  (nskk--search-dedup filtered))
         (limited (if (and limit (> (length unique) limit))
                      (seq-take unique limit)
                    unique)))
    (nskk--search-sort-results limited)))

(defun/k nskk-search-prefix (index query okuri-type limit)
  "Perform prefix match search in INDEX for QUERY.
OKURI-TYPE specifies okurigana filtering: \\='okuri-ari, \\='okuri-nasi, or nil.
LIMIT is the maximum number of results."
  (let* ((pred (nskk-dict-index-predicate index))
         (raw-results (when pred (nskk-prolog-trie-prefix-search pred 2 query)))
         ;; Convert raw (key . candidates) pairs to (key . nskk-dict-entry) pairs
         (results (mapcar (lambda (pair)
                            (cons (car pair)
                                  (make-nskk-dict-entry
                                   :key (car pair)
                                   :candidates (cdr pair))))
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
OKURI-TYPE specifies okurigana filtering: \\='okuri-ari, \\='okuri-nasi, or nil.
LIMIT is the maximum number of results."
  (let* ((pred (nskk-dict-index-predicate index))
         ;; Collect all substring matches, then delegate filtering / dedup /
         ;; limit / sort to the shared post-process pipeline (consistent with
         ;; prefix and exact search paths).
         (results
          (when pred
            (cl-loop for sol   in (nskk-prolog-query `(,pred \?k \?candidates))
                     for key    = (nskk-prolog-walk '\?k sol)
                     for cands  = (nskk-prolog-walk '\?candidates sol)
                     for entry  = (make-nskk-dict-entry :key key :candidates cands)
                     when (string-search query key)
                       collect (cons key entry))))
         (processed (nskk--search-post-process-results results okuri-type limit)))
    (if processed (succeed processed) (fail))))

;;; Fuzzy search

(defun/k nskk-search-fuzzy (index query limit)
  "Perform fuzzy search in INDEX for QUERY using Levenshtein distance.
LIMIT is the maximum number of results.

Each element in the results list has the shape (KEY ENTRY . DISTANCE)
where KEY is a string, ENTRY is an `nskk-dict-entry', and DISTANCE is
the integer Levenshtein distance from QUERY."
  (let* ((pred (nskk-dict-index-predicate index))
         (raw
          (when pred
            (cl-loop for sol      in (nskk-prolog-query `(,pred \?k \?candidates))
                     for key       = (nskk-prolog-walk '\?k sol)
                     for cands     = (nskk-prolog-walk '\?candidates sol)
                     for distance  = (nskk--search-levenshtein-distance query key)
                     when (<= distance nskk-search-fuzzy-threshold)
                       collect (cons key (cons (make-nskk-dict-entry
                                                :key key :candidates cands)
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

(defun nskk--search-levenshtein-distance (s1 s2)
  "Compute Levenshtein distance between S1 and S2."
  (let* ((len1 (length s1))
         (len2 (length s2))
         ;; Dynamic programming table: dp[i][j] = edit distance for s1[0..i) vs s2[0..j)
         (dp (make-vector (1+ len1) nil)))

    ;; Initialize table rows
    (dotimes (i (1+ len1))
      (aset dp i (make-vector (1+ len2) 0)))

    ;; Base cases: empty prefix distances
    (dotimes (i (1+ len1))
      (aset (aref dp i) 0 i))
    (dotimes (j (1+ len2))
      (aset (aref dp 0) j j))

    ;; Fill via recurrence: min(insertion, deletion, substitution)
    (dotimes (i len1)
      (dotimes (j len2)
        (let ((cost (if (char-equal (aref s1 i) (aref s2 j)) 0 1)))
          (aset (aref dp (1+ i)) (1+ j)
                (min
                 (1+ (aref (aref dp i) (1+ j)))      ; insertion
                 (1+ (aref (aref dp (1+ i)) j))       ; deletion
                 (+ (aref (aref dp i) j) cost))))))    ; substitution

    ;; Return final distance
    (aref (aref dp len1) len2)))

;;; Sort functions

(defun/k nskk--search-sort-results (results)
  "Sort search RESULTS according to `nskk-search-sort-method'."
  (let ((method (if (nskk-prolog-holds-p `(search-sort-method ,nskk-search-sort-method))
                    nskk-search-sort-method
                  'none)))
    (pcase method
      ('frequency (succeed (nskk--search-sort-prefix-results results)))
      ('kana      (succeed (nskk-search-sort-by-kana-order results)))
      (_          (succeed results)))))

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
  (cond
   ((stringp candidate) candidate)
   ((and (consp candidate)
         (stringp (car candidate)))
    (car candidate))
   (t nil)))


;;; Learning data management

;;;###autoload
(defun nskk-search-save-learning-data ()
  "Save learning data from Prolog facts to `nskk-search-learning-file'."
  (interactive)
  (condition-case err
      (progn
        (let ((dir (file-name-directory nskk-search-learning-file)))
          (unless (file-directory-p dir)
            (make-directory dir t)))
        (with-temp-file nskk-search-learning-file
          ;; Query all learning scores and serialize as (reading candidate score) tuples
          (let ((solutions (nskk-prolog-query '(learning-score \?r \?c \?s))))
            (prin1
             (mapcar (lambda (sol)
                       (list (nskk-prolog-walk '\?r sol)
                             (nskk-prolog-walk '\?c sol)
                             (nskk-prolog-walk '\?s sol)))
                     solutions)
             (current-buffer)))
          (message "NSKK: Learning data saved")
          (run-hooks 'nskk-save-history-hook)))
    (error
     (message "NSKK: Failed to save learning data: %s" (error-message-string err)))))

;;;###autoload
(defun/done nskk-search-learn (query candidate &optional context)
  "Record that CANDIDATE was selected for QUERY.
_CONTEXT is reserved for future use.
Stores learning score as a Prolog learning-score/3 fact."
  (ignore context)
  (let* ((word (if (stringp candidate) candidate (car candidate)))
         (old-score (when word
                      (nskk-prolog-query-value
                       `(learning-score ,query ,word \?s) '\?s)))
         (new-score (1+ (or old-score 0))))
    (when word
      (nskk-debug-log "[SEARCH] learn: query=%s word=%s new-score=%d" query word new-score)
      (when old-score
        (nskk-prolog-retract `(learning-score ,query ,word ,old-score)))
      (nskk-prolog-assert (list `(learning-score ,query ,word ,new-score))))))

;;; Cache-backed search

(defun nskk--search-cache-key (query search-type okuri-type)
  "Generate cache key string from QUERY, SEARCH-TYPE, and OKURI-TYPE."
  (format "%s:%s:%s"
          query
          (or search-type 'exact)
          (or okuri-type 'none)))

;;;###autoload
(defun/k nskk-search-with-cache (cache index query &optional search-type okuri-type limit)
  "Search INDEX for QUERY using CACHE for result caching.
Returns the cached or fresh result via ON-FOUND when candidates exist,
or calls ON-NOT-FOUND when no candidates are found.
SEARCH-TYPE, OKURI-TYPE, and LIMIT are passed to `nskk-search' on cache miss.

NOTE: `nskk-search-jisyo-hook' fires on cache misses (when `nskk-search' is
invoked) but does NOT fire on cache hits.  Consumers expecting the hook
to fire on every returned result should query `nskk-search' directly."
  (unless (nskk-cache-p cache)
    (signal 'wrong-type-argument (list 'nskk-cache-p cache)))
  (let ((cache-key (nskk--search-cache-key query search-type okuri-type)))
    (<-or cached nskk-cache-get cache cache-key
      :found (progn
               (nskk-debug-log "[SEARCH] cache-hit: key=%s" cache-key)
               (succeed cached))
      :fail  (progn
               (nskk-debug-log "[SEARCH] cache-miss: key=%s" cache-key)
               (<-seq [result (nskk-search index query search-type okuri-type limit)]
                 (progn
                   (nskk-cache-put cache cache-key result)
                   (succeed result)))))))

(provide 'nskk-search)

;;; nskk-search.el ends here
