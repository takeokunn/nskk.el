;;; nskk-search-test.el --- Tests for nskk-search.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-search.el covering:
;; - Exact search
;; - Prefix search
;; - Partial search
;; - Fuzzy search (Levenshtein distance)
;; - Result sorting
;; - Duplicate removal
;; - Cache integration
;; - Error handling (invalid query, invalid index)

;;; Code:

(require 'ert)
(require 'nskk-search)
(require 'nskk-dictionary)
(require 'nskk-prolog)
(require 'nskk-cache)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)

;;;
;;; Test Helpers
;;;

(defun nskk-search-test--make-index (&optional entries-alist trie-entries
                                               pred-name)
  "Create a test dict-index backed by Prolog facts.
ENTRIES-ALIST is ((key . candidates-list) ...) for exact-match entries.
TRIE-ENTRIES is ((key . candidates-list) ...) for trie-indexed entries.
PRED-NAME is the Prolog predicate symbol (defaults to a unique generated symbol)."
  (let* ((pred (or pred-name (intern (format "test-dict-%d" (abs (random))))))
         (all-entries (append entries-alist trie-entries)))
    (nskk-prolog-set-index pred 2 :trie)
    (dolist (pair all-entries)
      (let ((key (car pair))
            (val (if (listp (cdr pair)) (cdr pair) (list (cdr pair)))))
        (nskk-prolog-assert (list (list pred key val)))))
    (make-nskk-dict-index :predicate pred)))

;;;
;;; nskk-search exact match
;;;

(nskk-describe "nskk-search exact match"
  (nskk-it "returns candidates for an existing key"
    (nskk-with-prolog-entries ((test-exact-dict "かんじ" ("漢字" "感じ")))
      (let* ((index (make-nskk-dict-index :predicate 'test-exact-dict))
             (result (nskk-search index "かんじ" 'exact)))
        (nskk-should-candidates '("漢字" "感じ") result))))

  (nskk-it "returns nil for a non-existing key"
    (nskk-with-prolog-entries ((test-nonexist-dict "abc" ("value")))
      (let* ((index (make-nskk-dict-index :predicate 'test-nonexist-dict))
             (result (nskk-search index "xyz" 'exact)))
        (should (null result)))))

  (nskk-it "defaults to exact type and returns a dict-entry"
    (nskk-with-prolog-entries ((test-default-dict "key" ("value")))
      (let* ((index (make-nskk-dict-index :predicate 'test-default-dict))
             (result (nskk-search index "key")))
        (nskk-should-candidates '("value") result)))))

;;;
;;; nskk-search prefix match
;;;

(nskk-describe "nskk-search prefix match"
  (nskk-it "returns all entries matching the prefix"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    nil
                    '(("かん" . ("缶")) ("かんじ" . ("漢字")) ("かんたん" . ("簡単")) ("きん" . ("金"))))))
        (let ((results (nskk-search index "かん" 'prefix)))
          (should (listp results))
          (should (>= (length results) 3))
          (should (assoc "かん" results))
          (should (assoc "かんじ" results))
          (should (assoc "かんたん" results))))))

  (nskk-it "returns nil when no entries match the prefix"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    nil
                    '(("abc" . ("1")) ("abd" . ("2"))))))
        (let ((results (nskk-search index "xyz" 'prefix)))
          (should (null results))))))

  (nskk-it "respects the limit parameter"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    nil
                    '(("aa" . ("1")) ("ab" . ("2")) ("ac" . ("3")) ("ad" . ("4"))))))
        (let ((results (nskk-search index "a" 'prefix nil 2)))
          (should (<= (length results) 2))))))

  (nskk-it "returns nil when predicate has no trie index"
    (nskk-prolog-test-with-isolated-db
      (let ((index (make-nskk-dict-index :predicate nil)))
        (let ((results (nskk-search index "test" 'prefix)))
          (should (null results)))))))

;;;
;;; nskk-search partial match
;;;

(nskk-describe "nskk-search partial match"
  (nskk-it "returns entries containing the substring"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("abcdef" . ("v1")) ("xyzabc" . ("v2")) ("hello" . ("v3"))))))
        (let ((results (nskk-search index "abc" 'partial)))
          (should (listp results))
          (should (= (length results) 2))
          (should (assoc "abcdef" results))
          (should (assoc "xyzabc" results))))))

  (nskk-it "returns nil when no entries contain the substring"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("hello" . ("v1")) ("world" . ("v2"))))))
        (let ((results (nskk-search index "xyz" 'partial)))
          (should (null results))))))

  (nskk-it "respects the limit parameter"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("abc1" . ("v1")) ("abc2" . ("v2")) ("abc3" . ("v3")) ("abc4" . ("v4"))))))
        (let ((results (nskk-search index "abc" 'partial nil 2)))
          (should (= (length results) 2))))))

  (nskk-it "works with Japanese substring queries"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("にほんご" . ("Japanese"))
                      ("にほん" . ("Japan"))
                      ("せかい" . ("World"))))))
        (let ((results (nskk-search index "にほん" 'partial)))
          (should (= (length results) 2))
          (should (assoc "にほんご" results))
          (should (assoc "にほん" results)))))))

;;;
;;; nskk-search fuzzy match
;;;

(nskk-describe "nskk-search fuzzy match"
  (nskk-context "fuzzy search"
    (nskk-it "finds exact matches (distance 0) first"
      (nskk-prolog-test-with-isolated-db
        (let ((index (nskk-search-test--make-index
                      '(("abc" . ("v1")) ("xyz" . ("v2"))))))
          (let ((results (nskk-search index "abc" 'fuzzy)))
            (should (listp results))
            (should (> (length results) 0))
            ;; The exact match should be first (distance 0)
            (let ((first-result (car results)))
              (should (equal (car first-result) "abc")))))))

    (nskk-it "finds close matches within threshold"
      (nskk-prolog-test-with-isolated-db
        (let ((nskk-search-fuzzy-threshold 2)
              (index (nskk-search-test--make-index
                      '(("abc" . ("v1")) ("abd" . ("v2")) ("xyz" . ("v3"))))))
          (let ((results (nskk-search index "abc" 'fuzzy)))
            (should (listp results))
            ;; "abc" (dist=0) and "abd" (dist=1) should match, "xyz" (dist=3) may not
            (should (>= (length results) 2))))))

    (nskk-it "respects the limit parameter"
      (nskk-prolog-test-with-isolated-db
        (let ((nskk-search-fuzzy-threshold 3)
              (index (nskk-search-test--make-index
                      '(("aaa" . ("1")) ("aab" . ("2")) ("aac" . ("3")) ("aad" . ("4"))))))
          (let ((results (nskk-search index "aaa" 'fuzzy nil 2)))
            (should (<= (length results) 2))))))

    (nskk-it "returns results sorted by distance (non-decreasing)"
      (nskk-prolog-test-with-isolated-db
        (let ((nskk-search-fuzzy-threshold 3)
              (index (nskk-search-test--make-index
                      '(("abc" . ("1")) ("abx" . ("2")) ("axx" . ("3")) ("xxx" . ("4"))))))
          (let ((results (nskk-search index "abc" 'fuzzy)))
            (when (> (length results) 1)
              ;; Verify distances are non-decreasing
              (let ((prev-dist -1))
                (dolist (r results)
                  (let ((dist (cddr r)))
                    (should (>= dist prev-dist))
                    (setq prev-dist dist))))))))))

  (nskk-context "nskk--search-dedup (fuzzy: keep-closer merge)"
    (nskk-it "returns all entries when no duplicates"
      (let ((results `(("a" entry1 . 0) ("b" entry2 . 1) ("c" entry3 . 2))))
        (should (= (length (nskk--search-dedup results #'car
                                               (lambda (e n) (< (cddr n) (cddr e)))))
                   3))))

    (nskk-it "keeps the entry with the smallest distance"
      (let* ((far   `("漢字" entry-far  . 3))
             (close `("漢字" entry-close . 1))
             (results (list far close)))
        (let ((deduped (nskk--search-dedup results #'car
                                           (lambda (e n) (< (cddr n) (cddr e))))))
          (should (= (length deduped) 1))
          ;; The entry with smaller distance should be kept
          (should (= (cddr (car deduped)) 1)))))

    (nskk-it "keeps first entry when distances are equal (no merge-fn trigger)"
      (let* ((first  `("同じ" entry-first  . 2))
             (second `("同じ" entry-second . 2))
             (results (list first second)))
        (let ((deduped (nskk--search-dedup results #'car
                                           (lambda (e n) (< (cddr n) (cddr e))))))
          (should (= (length deduped) 1)))))

    (nskk-it "returns nil for empty input"
      (should (null (nskk--search-dedup nil))))))

;;;
;;; Levenshtein Distance Tests
;;;

;; Table-driven: covers all canonical edit-distance cases in one declaration
(nskk-deftest-table levenshtein-known-distances
  :columns (s1 s2 expected label)
  :rows (("abc"    "abc"      0  "identical strings")
         (""       ""         0  "both empty")
         ("abc"    ""         3  "deletion to empty")
         (""       "abc"      3  "insertion from empty")
         ("abc"    "abcd"     1  "single insertion")
         ("abcd"   "abc"      1  "single deletion")
         ("abc"    "axc"      1  "single substitution")
         ("kitten" "sitting"  3  "kitten->sitting")
         ("かんじ" "かんじ"   0  "identical Japanese")
         ("かんじ" "かんき"   1  "Japanese single substitution")
         ("にほん" "にほんご" 1  "Japanese single insertion"))
  :body (should (= (nskk--search-levenshtein-distance s1 s2) expected)))

;;;
;;; CPS variant direct tests
;;;

(nskk-describe "nskk-search/k CPS callbacks"
  (nskk-it "calls on-found with the matching entry"
    (nskk-with-prolog-entries ((test-cps-found-dict "かんじ" ("漢字")))
      (let* ((index (make-nskk-dict-index :predicate 'test-cps-found-dict))
             (found nil))
        (nskk-search/k index "かんじ" 'exact nil nil
                       (lambda (r) (setq found r))
                       (lambda () nil))
        (should (nskk-dict-entry-p found))
        (should (equal (nskk-dict-entry-candidates found) '("漢字"))))))

  (nskk-it "calls on-not-found when key is absent"
    (nskk-with-prolog-entries ((test-cps-miss-dict "abc" ("val")))
      (let* ((index (make-nskk-dict-index :predicate 'test-cps-miss-dict))
             (missed nil))
        (nskk-search/k index "xyz" 'exact nil nil
                       #'identity
                       (lambda () (setq missed t)))
        (should missed))))

  (nskk-it "on-found receives prefix results list"
    (nskk-prolog-test-with-isolated-db
      (let* ((index (nskk-search-test--make-index
                     nil '(("かん" . ("缶")) ("かんじ" . ("漢字")))))
             (found nil))
        (nskk-search/k index "かん" 'prefix nil nil
                       (lambda (r) (setq found r))
                       (lambda () nil))
        (should (listp found))
        (should (>= (length found) 2)))))

  (nskk-it "on-not-found is called for prefix search with no matches"
    (nskk-prolog-test-with-isolated-db
      (let* ((index (nskk-search-test--make-index
                     nil '(("abc" . ("1")))))
             (missed nil))
        (nskk-search/k index "xyz" 'prefix nil nil
                       #'identity
                       (lambda () (setq missed t)))
        (should missed)))))

;;;
;;; Okuri-type filter tests
;;;

;; Table-driven: all combinations of okuri-type × entry-okuri
(nskk-deftest-table match-okuri-type-p-cases
  :columns (okuri-type entry-okuri matches-p label)
  :rows ((okuri-ari  "し"  t   "ari: non-empty okuri matches")
         (okuri-ari  nil   nil "ari: nil okuri does not match")
         (okuri-nasi nil   t   "nasi: nil okuri matches")
         (okuri-nasi ""    t   "nasi: empty string matches")
         (okuri-nasi "し"  nil "nasi: non-empty okuri does not match")
         (nil        "し"  t   "nil filter: matches any okuri")
         (nil        nil   t   "nil filter: matches nil okuri"))
  :body (if matches-p
            (should (nskk--search-match-okuri-type-p okuri-type entry-okuri))
          (should-not (nskk--search-match-okuri-type-p okuri-type entry-okuri))))

;;;
;;; nskk-search empty/nil handling
;;;

(nskk-describe "nskk-search empty/nil handling"
  (nskk-context "error conditions"
    (nskk-it "signals nskk-dict-search-invalid-query for nil query"
      (nskk-prolog-test-with-isolated-db
        (let ((index (nskk-search-test--make-index '(("a" . ("1")))))
              (caught nil))
          (condition-case _err
              (nskk-search index nil 'exact)
            (nskk-dict-search-invalid-query (setq caught t)))
          (should caught))))

    (nskk-it "signals nskk-dict-search-invalid-query for empty string query"
      (nskk-prolog-test-with-isolated-db
        (let ((index (nskk-search-test--make-index '(("a" . ("1")))))
              (caught nil))
          (condition-case _err
              (nskk-search index "" 'exact)
            (nskk-dict-search-invalid-query (setq caught t)))
          (should caught))))

    (nskk-it "signals nskk-dict-search-invalid-index for a non-index value"
      (let ((caught nil))
        (condition-case _err
            (nskk-search "not-an-index" "query" 'exact)
          (nskk-dict-search-invalid-index (setq caught t)))
        (should caught)))

    (nskk-it "signals nskk-dict-search-invalid-query for invalid search type"
      (nskk-prolog-test-with-isolated-db
        (let ((index (nskk-search-test--make-index '(("a" . ("1")))))
              (caught nil))
          (condition-case _err
              (nskk-search index "a" 'invalid-type)
            (nskk-dict-search-invalid-query (setq caught t)))
          (should caught)))))

  (nskk-context "nskk--search-dedup (ordinary: first-wins)"
    (nskk-it "returns all entries when there are no duplicates"
      (let ((results '(("a" . 1) ("b" . 2) ("c" . 3))))
        (let ((unique (nskk--search-dedup results)))
          (should (= (length unique) 3)))))

    (nskk-it "deduplicates keeping the first occurrence"
      (let ((results '(("a" . 1) ("b" . 2) ("a" . 3) ("c" . 4) ("b" . 5))))
        (let ((unique (nskk--search-dedup results)))
          (should (= (length unique) 3))
          ;; First occurrence should be kept (no merge-fn → first-wins)
          (should (equal (cdr (assoc "a" unique)) 1))
          (should (equal (cdr (assoc "b" unique)) 2)))))

    (nskk-it "returns nil for empty input"
      (should (null (nskk--search-dedup nil))))))

;;;
;;; Sort Tests
;;;

(nskk-describe "nskk-search sort"
  (nskk-it "sort-by-kana-order sorts in kana order"
    (let ((results '(("さ" . 3) ("あ" . 1) ("か" . 2))))
      (let ((sorted (nskk-search-sort-by-kana-order results)))
        (should (equal (car (nth 0 sorted)) "あ"))
        (should (equal (car (nth 1 sorted)) "か"))
        (should (equal (car (nth 2 sorted)) "さ")))))

  (nskk-it "sort method 'frequency' ranks higher-scored entries first"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "あ" "亜" 1)))
      (nskk-prolog-assert '((learning-score "か" "家" 10)))
      (let* ((nskk-search-sort-method 'frequency)
             (e-a (make-nskk-dict-entry :key "あ" :candidates '("亜")))
             (e-k (make-nskk-dict-entry :key "か" :candidates '("家")))
             (results `(("あ" . ,e-a) ("か" . ,e-k)))
             (sorted (nskk--search-sort-results results)))
        ;; Higher score (か=10) should come before lower score (あ=1)
        (should (equal (car (car sorted)) "か")))))

  (nskk-it "sort method 'none' returns results unchanged"
    (let ((nskk-search-sort-method 'none)
          (results '(("c" . 3) ("a" . 1) ("b" . 2))))
      (let ((sorted (nskk--search-sort-results results)))
        (should (equal sorted results)))))

  (nskk-it "sort method 'kana' sorts results"
    (let ((nskk-search-sort-method 'kana)
          (results '(("さ" . 3) ("あ" . 1) ("か" . 2))))
      (let ((sorted (nskk--search-sort-results results)))
        (should (equal (car (nth 0 sorted)) "あ")))))

  (nskk-it "sort-by-kana-order returns nil for empty input"
    (should (null (nskk-search-sort-by-kana-order nil)))))

;;;
;;; nskk--search-post-process-results direct tests
;;;

(nskk-describe "nskk--search-post-process-results"
  (nskk-it "deduplicates results keeping first occurrence"
    (let* ((e1 (make-nskk-dict-entry :key "a" :candidates '("v1")))
           (e2 (make-nskk-dict-entry :key "b" :candidates '("v2")))
           (e3 (make-nskk-dict-entry :key "a" :candidates '("v3")))
           (results (list (cons "a" e1) (cons "b" e2) (cons "a" e3)))
           (processed (nskk--search-post-process-results results nil nil)))
      (should (= (length processed) 2))
      (should (assoc "a" processed))
      (should (assoc "b" processed))))

  (nskk-it "applies limit after deduplication"
    (let* ((entries (mapcar (lambda (k)
                              (cons k (make-nskk-dict-entry :key k :candidates (list k))))
                            '("a" "b" "c" "d" "e")))
           (processed (nskk--search-post-process-results entries nil 3)))
      (should (= (length processed) 3))))

  (nskk-it "returns all results when limit is nil"
    (let* ((entries (mapcar (lambda (k)
                              (cons k (make-nskk-dict-entry :key k :candidates (list k))))
                            '("a" "b" "c")))
           (processed (nskk--search-post-process-results entries nil nil)))
      (should (= (length processed) 3))))

  (nskk-it "filters by okuri-nasi type"
    (let* ((e-plain (make-nskk-dict-entry :key "a" :candidates '("v1")))
           (e-okuri (make-nskk-dict-entry :key "b" :candidates '("v2") :okuri "し"))
           (results (list (cons "a" e-plain) (cons "b" e-okuri)))
           (processed (nskk--search-post-process-results results 'okuri-nasi nil)))
      (should (= (length processed) 1))
      (should (assoc "a" processed))))

  (nskk-it "returns nil for empty input"
    (should (null (nskk--search-post-process-results nil nil nil)))))

;;;
;;; Cache Key Generation Tests
;;;

(defconst nskk-search-test--all-cache-key-queries
  '(("query1" exact   nil)
    ("query1" prefix  nil)
    ("query1" exact   okuri-ari)
    ("query1" exact   okuri-nasi)
    ("query2" exact   nil)
    ("query1" fuzzy   nil)
    ("query1" partial okuri-ari))
  "All (QUERY TYPE OKURI-TYPE) triples used by PBT-004 to verify cache key
uniqueness.  Each triple represents a distinct parameter combination; the
test asserts that every triple produces a key different from all others
in this list.")

(nskk-describe "nskk--search-cache-key"
  (nskk-it "generates distinct string keys for different parameter combinations"
    (let ((key1 (nskk--search-cache-key "query" 'exact nil))
          (key2 (nskk--search-cache-key "query" 'prefix nil))
          (key3 (nskk--search-cache-key "query" 'exact 'okuri-ari)))
      (should (stringp key1))
      (should (stringp key2))
      (should (stringp key3))
      ;; Different parameters should produce different keys
      (should (not (equal key1 key2)))
      (should (not (equal key1 key3)))))

  (nskk-it "generates a string containing 'exact' and 'none' for nil parameters"
    (let ((key (nskk--search-cache-key "query" nil nil)))
      (should (stringp key))
      (should (string-match-p "exact" key))
      (should (string-match-p "none" key))))

  ;; PBT-004 -- Cache key uniqueness (table-driven)
  (nskk-deftest-table search-cache-key-uniqueness
    :columns (query type okuri label)
    :rows (("query1" exact   nil         "exact-no-okuri")
           ("query1" prefix  nil         "prefix-no-okuri")
           ("query1" exact   okuri-ari   "exact-okuri-ari")
           ("query1" exact   okuri-nasi  "exact-okuri-nasi")
           ("query2" exact   nil         "different-query")
           ("query1" fuzzy   nil         "fuzzy-no-okuri")
           ("query1" partial okuri-ari   "partial-okuri-ari"))
    :body (let* ((this-key (nskk--search-cache-key query type okuri))
                 (other-keys (mapcar (lambda (row)
                                       (nskk--search-cache-key (nth 0 row)
                                                                (nth 1 row)
                                                                (nth 2 row)))
                                     (cl-remove-if (lambda (row)
                                                     (and (equal (nth 0 row) query)
                                                          (eq (nth 1 row) type)
                                                          (eq (nth 2 row) okuri)))
                                                   nskk-search-test--all-cache-key-queries))))
            (should (stringp this-key))
            (dolist (other other-keys)
              (should (not (equal this-key other)))))))

;;;
;;; Cache Integration Tests
;;;

(nskk-describe "nskk-search-with-cache"
  (nskk-it "returns correct result on both cache miss and cache hit"
    ;; Not wrapped in nskk-prolog-test-with-isolated-db because the cache
    ;; dispatch system (cache-dispatch-fn/3 facts) must remain intact.
    ;; Each call to nskk-search-test--make-index uses a unique predicate name,
    ;; so there is no test-pollution risk.
    (let* ((cache (nskk-cache-lru-create 100))
           (index (nskk-search-test--make-index '(("test" . ("value"))))))
      ;; First search (cache miss)
      (let ((result (nskk-search-with-cache cache index "test" 'exact)))
        (nskk-should-candidates '("value") result))
      ;; Second search (cache hit)
      (let ((result (nskk-search-with-cache cache index "test" 'exact)))
        (nskk-should-candidates '("value") result))
      ;; Verify cache statistics
      (let ((stats (nskk-cache-stats cache)))
        (should (= (plist-get stats :hits) 1))
        (should (= (plist-get stats :size) 1)))))

  (nskk-it "works with a Prolog trie dict index"
    ;; Not wrapped in nskk-prolog-test-with-isolated-db because the cache
    ;; dispatch system (cache-dispatch-fn/3 facts) must remain intact.
    ;; The predicate 'cache-test-dict is unique to this test and is
    ;; re-asserted on each run, so there is no test-pollution risk.
    (let* ((cache (nskk-cache-lru-create 100))
           (pred 'cache-test-dict)
           (index (progn
                    (nskk-prolog-set-index pred 2 :trie)
                    (nskk-prolog-assert `((,pred "key" ("trie-value"))))
                    (make-nskk-dict-index :predicate pred))))
      (let ((result (nskk-search-with-cache cache index "key" 'exact)))
        (nskk-should-candidates '("trie-value") result))))

  (nskk-it "signals wrong-type-argument for an invalid cache"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index '(("a" . ("1"))))))
        (should-error (nskk-search-with-cache "not-a-cache" index "a")
                      :type 'wrong-type-argument)))))

;;;
;;; Candidate Word Extraction Tests
;;;

(nskk-describe "nskk--search-candidate-word"
  (nskk-it "returns a string candidate as-is"
    (should (equal (nskk--search-candidate-word "漢字") "漢字")))

  (nskk-it "extracts the car of a cons cell candidate"
    (should (equal (nskk--search-candidate-word '("漢字" . "okurigana")) "漢字")))

  (nskk-it "returns nil for non-string, non-cons values"
    (nskk-then
      (should (null (nskk--search-candidate-word 42)))
      (should (null (nskk--search-candidate-word nil)))
      (should (null (nskk--search-candidate-word '(42 . "x")))))))

;;;
;;; Candidate Score Tests
;;;

(nskk-describe "nskk--search-candidate-score"
  (nskk-it "returns 0 when no learning data exists"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (should (= (nskk--search-candidate-score "かんじ" "漢字") 0))))

  (nskk-it "returns value from Prolog learning-score/3"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 5)))
      (should (= (nskk--search-candidate-score "かんじ" "漢字") 5))))

  (nskk-it "works with cons cell candidates"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 3)))
      (should (= (nskk--search-candidate-score "かんじ" '("漢字" . "ji")) 3))))

  (nskk-it "is reading-specific (different readings return different scores)"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 10)))
      (nskk-prolog-assert '((learning-score "もじ" "漢字" 2)))
      (should (= (nskk--search-candidate-score "かんじ" "漢字") 10))
      (should (= (nskk--search-candidate-score "もじ" "漢字") 2)))))

;;;
;;; Reading Score Tests
;;;

(nskk-describe "nskk--search-reading-score"
  (nskk-it "returns 0 for a non-entry value"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (should (= (nskk--search-reading-score "かんじ" "not-an-entry") 0))))

  (nskk-it "returns maximum score across all candidates"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 3)))
      (nskk-prolog-assert '((learning-score "かんじ" "感じ" 7)))
      (let ((entry (make-nskk-dict-entry :key "かんじ" :candidates '("漢字" "感じ"))))
        (should (= (nskk--search-reading-score "かんじ" entry) 7)))))

  (nskk-it "returns 0 when no learning data exists for any candidate"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((entry (make-nskk-dict-entry :key "かんじ" :candidates '("漢字" "感じ"))))
        (should (= (nskk--search-reading-score "かんじ" entry) 0))))))

;;;
;;; Sort Entry by Learning Tests
;;;

(nskk-describe "nskk--search-sort-entry-by-learning"
  (nskk-it "reorders candidates by Prolog score descending"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 1)))
      (nskk-prolog-assert '((learning-score "かんじ" "感じ" 5)))
      (let ((entry (make-nskk-dict-entry :key "かんじ" :candidates '("漢字" "感じ"))))
        (nskk--search-sort-entry-by-learning entry)
        ;; Higher score (感じ=5) should come first
        (should (equal (car (nskk-dict-entry-candidates entry)) "感じ")))))

  (nskk-it "preserves original order when scores are equal"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((entry (make-nskk-dict-entry :key "かんじ" :candidates '("漢字" "感じ"))))
        (let ((result (nskk--search-sort-entry-by-learning entry)))
          ;; Returns the entry itself
          (should (eq result entry))
          ;; All candidates still present
          (should (= (length (nskk-dict-entry-candidates entry)) 2))))))

  (nskk-it "returns nil gracefully for nil input"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (should (null (nskk--search-sort-entry-by-learning nil))))))

;;;
;;; Sort Prefix Results Tests
;;;

(nskk-describe "nskk--search-sort-prefix-results"
  (nskk-it "orders entries by maximum learning score descending"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "あ" "亜" 1)))
      (nskk-prolog-assert '((learning-score "か" "家" 10)))
      (let* ((entry-a (make-nskk-dict-entry :key "あ" :candidates '("亜")))
             (entry-k (make-nskk-dict-entry :key "か" :candidates '("家")))
             (results `(("あ" . ,entry-a) ("か" . ,entry-k))))
        (let ((sorted (nskk--search-sort-prefix-results results)))
          ;; Higher score entry (か=10) should come first
          (should (equal (car (car sorted)) "か"))))))

  (nskk-it "handles an empty list"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (should (null (nskk--search-sort-prefix-results nil)))))

  (nskk-it "returns all results when no learning data exists"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let* ((e1 (make-nskk-dict-entry :key "あ" :candidates '("亜")))
             (e2 (make-nskk-dict-entry :key "い" :candidates '("意")))
             (results `(("あ" . ,e1) ("い" . ,e2))))
        ;; All scores are 0, so original order is preserved by stable sort
        (let ((sorted (nskk--search-sort-prefix-results results)))
          (should (= (length sorted) 2)))))))


;;;
;;; Learning Data: nskk-search-learn Tests
;;;

(nskk-describe "nskk-search-learn"
  (nskk-it "initializes score to 1 for a new candidate"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((nskk--search-dirty-flag nil))
        (nskk-search-learn "かんじ" "漢字")
        (should (= (nskk-prolog-query-value
                    '(learning-score "かんじ" "漢字" \?s) '\?s)
                   1))
        ;; Dirty flag must be set
        (should nskk--search-dirty-flag))))

  (nskk-it "increments an existing score by 1"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 3)))
      (nskk-search-learn "かんじ" "漢字")
      (should (= (nskk-prolog-query-value
                  '(learning-score "かんじ" "漢字" \?s) '\?s)
                 4))))

  (nskk-it "works with cons cell candidates (extracts car)"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-search-learn "かんじ" '("漢字" . "ji"))
      (should (= (nskk-prolog-query-value
                  '(learning-score "かんじ" "漢字" \?s) '\?s)
                 1))))

  (nskk-it "does nothing when candidate is nil"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((nskk--search-dirty-flag nil))
        (nskk-search-learn "かんじ" nil)
        ;; No learning-score fact should be created
        (should-not (nskk-prolog-query-value
                     '(learning-score "かんじ" \?c \?s) '\?s))
        ;; Dirty flag must remain unset
        (should-not nskk--search-dirty-flag))))

  (nskk-it "retracts old score before asserting new one (no duplicates)"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 2)))
      (nskk-search-learn "かんじ" "漢字")
      ;; Only one score fact should exist for this reading/candidate pair
      (let ((all-scores (nskk-prolog-query-all-values
                         '(learning-score "かんじ" "漢字" \?s) '\?s)))
        (should (= (length all-scores) 1))
        (should (= (car all-scores) 3))))))

;;;
;;; Learning Data: Save / Load Tests
;;;

(nskk-describe "nskk-search learning data persistence"
  (nskk-it "save sets nskk--search-dirty-flag to nil"
    (nskk-prolog-test-with-isolated-db
      (let* ((tmp-file (make-temp-file "nskk-learning-test" nil ".dat"))
             (nskk-search-learning-file tmp-file)
             (nskk--search-dirty-flag t))
        (unwind-protect
            (progn
              (nskk-search-save-learning-data)
              (should-not nskk--search-dirty-flag))
          (when (file-exists-p tmp-file)
            (delete-file tmp-file))))))

  (nskk-it "handles write errors gracefully without signaling"
    (nskk-prolog-test-with-isolated-db
      (let* ((nskk-search-learning-file "/nonexistent/dir/learning.dat")
             (messages nil))
        (nskk-with-mocks ((message (lambda (fmt &rest args)
                                     (push (apply #'format fmt args) messages))))
          ;; Should NOT signal an error
          (should-not (condition-case _err
                          (progn (nskk-search-save-learning-data) nil)
                        (error t)))
          ;; Should have emitted a failure message
          (should (cl-some (lambda (m) (string-match-p "Failed" m)) messages)))))))


;;;
;;; nskk-core-search integration
;;;

(nskk-describe "nskk-search combined search and entry-count operations"
  (nskk-it "supports exact prefix partial and fuzzy search on a shared index"
    (nskk-prolog-test-with-isolated-db
      (let* ((entries '(("かんじ" . ("漢字"))
                        ("かんたん" . ("簡単"))
                        ("かん" . ("缶"))
                        ("きんし" . ("禁止"))
                        ("きんぎょ" . ("金魚"))))
             (index (nskk-search-test--make-index
                     entries
                     entries)))
        ;; Exact search returns a dict-entry
        (let ((result (nskk-search index "かんじ" 'exact)))
          (nskk-should-candidates '("漢字") result))

        ;; Prefix search
        (let ((results (nskk-search index "かん" 'prefix)))
          (should (>= (length results) 3)))

        ;; Partial search
        (let ((results (nskk-search index "かん" 'partial)))
          (should (>= (length results) 3)))

        ;; Fuzzy search
        (let ((nskk-search-fuzzy-threshold 2)
              (results (nskk-search index "かんじ" 'fuzzy)))
          (should (> (length results) 0))))))

)

;;;
;;; Property-Based Tests
;;;

;; PBT-001 — Levenshtein symmetry (seeded PBT, 50 runs)
(nskk-property-test-seeded search-levenshtein-symmetry
  ((a romaji-basic)
   (b romaji-basic))
  (= (nskk--search-levenshtein-distance a b)
     (nskk--search-levenshtein-distance b a))
  50
  7)

;; PBT-002 — Levenshtein identity (seeded PBT, 50 runs)
(nskk-property-test-seeded search-levenshtein-identity
  ((input romaji-basic))
  (= (nskk--search-levenshtein-distance input input) 0)
  50
  13)

;; PBT-003 — Levenshtein triangle inequality (seeded PBT, 30 runs)
(nskk-property-test-seeded search-levenshtein-triangle-inequality
  ((a romaji-basic)
   (b romaji-basic)
   (c romaji-basic))
  (<= (nskk--search-levenshtein-distance a c)
      (+ (nskk--search-levenshtein-distance a b)
         (nskk--search-levenshtein-distance b c)))
  30
  17)

;; PBT-005 — nskk--search-dedup idempotency: applying it twice yields the same result
(nskk-property-test-seeded search-dedup-idempotency
  ((a romaji-basic)
   (b romaji-basic)
   (c romaji-basic))
  (let* ((items (list (cons a 1) (cons b 2) (cons c 3) (cons a 4)))
         (once  (nskk--search-dedup items))
         (twice (nskk--search-dedup once)))
    (equal once twice))
  30
  23)

;; PBT-006 — nskk--search-dedup never increases length
(nskk-property-test-seeded search-dedup-length-monotone
  ((a romaji-basic)
   (b romaji-basic))
  (let* ((items (list (cons a 1) (cons b 2) (cons a 3)))
         (result (nskk--search-dedup items)))
    (<= (length result) (length items)))
  30
  29)

;;;
;;; Additional Property-Based Tests
;;;

;; 1. Table-driven: nskk--search-candidate-word for known inputs
(nskk-deftest-cases search-candidate-word-known
  (("漢字"          . "漢字")
    (("漢字" . "ji") . "漢字")
    (nil             . nil)
    (42              . nil))
  :description "nskk--search-candidate-word extracts word string"
  :body (should (equal expected (nskk--search-candidate-word input))))

;; PBT-007 — Levenshtein distance is always non-negative
(nskk-property-test search-levenshtein-non-negative
  ((a romaji-basic)
   (b romaji-basic))
  (>= (nskk--search-levenshtein-distance a b) 0)
  50)

;; PBT-008 — Levenshtein distance <= max(len(a), len(b))
(nskk-property-test search-levenshtein-bounded-by-max-length
  ((a romaji-basic)
   (b romaji-basic))
  (<= (nskk--search-levenshtein-distance a b)
      (max (length a) (length b)))
  50)

;; PBT-009 — cache key is always a non-empty string for any query
(nskk-property-test search-cache-key-always-string
  ((q search-query))
  (let ((key (nskk--search-cache-key q 'exact nil)))
    (and (stringp key) (> (length key) 0)))
  30)

;; PBT-010 — cache key contains the query string
(nskk-property-test search-cache-key-contains-query
  ((q search-query))
  (let ((key (nskk--search-cache-key q 'exact nil)))
    (string-match-p (regexp-quote q) key))
  30)

;; PBT-011 — nskk-search-learn always increments the score by 1
(nskk-property-test search-learn-increments-score
  ((q search-query))
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-retract-all 'learning-score 3)
    (let ((initial-score (or (nskk-prolog-query-value
                              `(learning-score ,q "テスト" \?s) '\?s)
                             0)))
      (nskk-search-learn q "テスト")
      (let ((new-score (nskk-prolog-query-value
                        `(learning-score ,q "テスト" \?s) '\?s)))
        (= new-score (1+ initial-score)))))
  20)

;; PBT-012 — nskk-search-learn always sets dirty flag for non-nil string candidates
(nskk-property-test search-learn-sets-dirty-flag
  ((q search-query))
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-retract-all 'learning-score 3)
    (let ((nskk--search-dirty-flag nil))
      (nskk-search-learn q "テスト")
      nskk--search-dirty-flag))
  20)

;; PBT-013 — nskk-search/k calls exactly one callback (mutual exclusion)
(nskk-property-test search-exact-k-mutual-exclusion
  ((q search-query))
  (nskk-prolog-test-with-isolated-db
    (let* ((index (nskk-search-test--make-index '(("かんじ" . ("漢字")))))
           (found-count 0)
           (not-found-count 0))
      (nskk-search/k index q 'exact nil nil
                     (lambda (_r) (cl-incf found-count))
                     (lambda () (cl-incf not-found-count)))
      ;; Exactly one callback must have fired
      (= 1 (+ found-count not-found-count))))
  30)

(nskk-describe "Search property: learning score monotonicity"
  (nskk-it "applying learn N times yields score N from zero"
    (dotimes (_ 15)
      (nskk-for-all ((q search-query))
        (nskk-prolog-test-with-isolated-db
          (nskk-prolog-retract-all 'learning-score 3)
          (nskk-search-learn q "テスト")
          (nskk-search-learn q "テスト")
          (nskk-search-learn q "テスト")
          (let ((score (nskk-prolog-query-value
                        `(learning-score ,q "テスト" \?s) '\?s)))
            (should (= score 3))))))))

(nskk-describe "Search property: cache key format"
  (nskk-it "all four search types produce distinct keys for the same query"
    (dotimes (_ 20)
      (nskk-for-all ((q search-query))
        (let ((keys (mapcar (lambda (type)
                              (nskk--search-cache-key q type nil))
                            '(exact prefix partial fuzzy))))
          (should (= (length keys) (length (cl-remove-duplicates keys :test #'equal)))))))))

;; PBT-014 — nskk-search-sort-by-kana-order: sorted output is ordered by string<
;;
;; Invariant: for any list of (key . value) pairs, after sorting by kana order
;; every adjacent pair of keys satisfies (not (string< key[n+1] key[n])),
;; i.e., the sequence of keys is non-decreasing under `string<'.
(nskk-deftest-unit search-sort-order-invariant-pbt
  "nskk-search-sort-by-kana-order always produces a non-decreasingly ordered list
of keys under `string<' regardless of the input order."
  (nskk-property-test-seeded search-sort-order-invariant
    ((k1 search-query)
     (k2 search-query)
     (k3 search-query)
     (k4 search-query)
     (k5 search-query))
    (let* ((pairs (list (cons k1 1) (cons k2 2)
                        (cons k3 3) (cons k4 4) (cons k5 5)))
           (sorted (nskk-search-sort-by-kana-order pairs))
           (keys   (mapcar #'car sorted)))
      ;; Every adjacent pair of keys must satisfy (not (string< next prev)),
      ;; which means the list is in non-decreasing kana order.
      (cl-loop for (prev . rest) on keys
               while rest
               always (not (string< (car rest) prev))))
    40
    31))

;;;
;;; Direct API: nskk-search-exact
;;;

(nskk-describe "nskk-search-exact"
  (nskk-it "returns an nskk-dict-entry when the key exists"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("かんじ" . ("漢字" "感じ"))))))
        (let ((result (nskk-search-exact index "かんじ" nil)))
          (should (nskk-dict-entry-p result))
          (should (equal (nskk-dict-entry-candidates result) '("漢字" "感じ")))))))

  (nskk-it "returns nil when the key is absent"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("abc" . ("value"))))))
        (should (null (nskk-search-exact index "xyz" nil))))))

  (nskk-it "filters out entries that do not match okuri-ari type"
    (nskk-prolog-test-with-isolated-db
      ;; An entry whose candidates list contains no okurigana marker is okuri-nasi.
      (let ((index (nskk-search-test--make-index
                    '(("かんじ" . ("漢字"))))))
        ;; Searching with okuri-ari filter should exclude a plain okuri-nasi entry.
        (should (null (nskk-search-exact index "かんじ" 'okuri-ari))))))

  (nskk-it "returns nil when index predicate is nil"
    (nskk-prolog-test-with-isolated-db
      (let ((index (make-nskk-dict-index :predicate nil)))
        (should (null (nskk-search-exact index "key" nil)))))))

;;;
;;; Direct API: nskk-search-prefix
;;;

(nskk-describe "nskk-search-prefix"
  (nskk-it "returns an alist of (key . entry) pairs for prefix matches"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    nil
                    '(("かん" . ("缶")) ("かんじ" . ("漢字")) ("きん" . ("金"))))))
        (let ((results (nskk-search-prefix index "かん" nil nil)))
          (should (listp results))
          (should (>= (length results) 2))
          ;; Each element is a (string . nskk-dict-entry) pair
          (should (stringp (car (car results))))
          (should (nskk-dict-entry-p (cdr (car results))))))))

  (nskk-it "returns nil when no prefix matches"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    nil
                    '(("abc" . ("1"))))))
        (should (null (nskk-search-prefix index "xyz" nil nil))))))

  (nskk-it "respects the limit argument"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    nil
                    '(("aa" . ("1")) ("ab" . ("2")) ("ac" . ("3")) ("ad" . ("4"))))))
        (let ((results (nskk-search-prefix index "a" nil 2)))
          (should (<= (length results) 2))))))

  (nskk-it "returns nil when predicate is nil"
    (nskk-prolog-test-with-isolated-db
      (let ((index (make-nskk-dict-index :predicate nil)))
        (should (null (nskk-search-prefix index "test" nil nil)))))))

;;;
;;; Direct API: nskk-search-partial
;;;

(nskk-describe "nskk-search-partial"
  (nskk-it "returns pairs for entries containing the substring"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("abcdef" . ("v1")) ("xyzabc" . ("v2")) ("hello" . ("v3"))))))
        (let ((results (nskk-search-partial index "abc" nil nil)))
          (should (listp results))
          (should (= (length results) 2))
          (should (assoc "abcdef" results))
          (should (assoc "xyzabc" results))
          ;; Values are nskk-dict-entry structs
          (should (nskk-dict-entry-p (cdr (assoc "abcdef" results))))))))

  (nskk-it "returns nil when no entries contain the substring"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("hello" . ("world"))))))
        (should (null (nskk-search-partial index "xyz" nil nil))))))

  (nskk-it "respects the limit argument"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("abc1" . ("v1")) ("abc2" . ("v2")) ("abc3" . ("v3"))))))
        (let ((results (nskk-search-partial index "abc" nil 2)))
          (should (= (length results) 2))))))

  (nskk-it "returns nil when predicate is nil"
    (nskk-prolog-test-with-isolated-db
      (let ((index (make-nskk-dict-index :predicate nil)))
        (should (null (nskk-search-partial index "abc" nil nil)))))))

;;;
;;; Direct API: nskk-search-fuzzy
;;;

(nskk-describe "nskk-search-fuzzy"
  (nskk-it "returns (key entry . distance) triples"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-search-fuzzy-threshold 1)
            (index (nskk-search-test--make-index
                    '(("abc" . ("value"))))))
        (let ((results (nskk-search-fuzzy index "abc" nil nil)))
          (should (listp results))
          (should (= (length results) 1))
          ;; Shape is (key entry . distance)
          (let ((triple (car results)))
            (should (stringp (car triple)))
            (should (nskk-dict-entry-p (cadr triple)))
            (should (integerp (cddr triple))))))))

  (nskk-it "sorts results by ascending Levenshtein distance"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-search-fuzzy-threshold 3)
            (index (nskk-search-test--make-index
                    '(("abc" . ("v1")) ("abx" . ("v2")) ("xyz" . ("v3"))))))
        (let ((results (nskk-search-fuzzy index "abc" nil nil)))
          ;; Closest match (distance 0) should be first
          (should (= (cddr (car results)) 0))
          ;; Remaining results should have non-decreasing distances
          (cl-loop for (a b) on results
                   while b
                   do (should (<= (cddr a) (cddr b))))))))

  (nskk-it "returns nil when all entries exceed the threshold"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-search-fuzzy-threshold 0)
            (index (nskk-search-test--make-index
                    '(("abc" . ("v1")) ("xyz" . ("v2"))))))
        ;; With threshold 0 only exact matches (distance=0) pass.
        ;; "abc" matches "abc" exactly; query "abc" finds it.
        (let ((results (nskk-search-fuzzy index "def" nil nil)))
          (should (null results))))))

  (nskk-it "returns nil when predicate is nil"
    (nskk-prolog-test-with-isolated-db
      (let ((index (make-nskk-dict-index :predicate nil)))
        (should (null (nskk-search-fuzzy index "abc" nil nil)))))))

;;;
;;; Direct CPS variant tests
;;;

(nskk-describe "nskk-search-exact/k"
  (nskk-it "calls on-found with an nskk-dict-entry when key exists"
    (nskk-with-prolog-entries ((cps-exact-test "かんじ" ("漢字" "感じ")))
      (let ((index (make-nskk-dict-index :predicate 'cps-exact-test))
            found-entry)
        (nskk-search-exact/k index "かんじ" nil
                             (lambda (e) (setq found-entry e))
                             (lambda () (should nil)))
        (should (nskk-dict-entry-p found-entry))
        (should (equal (nskk-dict-entry-candidates found-entry) '("漢字" "感じ"))))))

  (nskk-it "calls on-not-found when key is absent"
    (nskk-with-prolog-entries ((cps-exact-miss-test "かんじ" ("漢字")))
      (let ((index (make-nskk-dict-index :predicate 'cps-exact-miss-test))
            not-found-called)
        (nskk-search-exact/k index "ない" nil
                             (lambda (_) (should nil))
                             (lambda () (setq not-found-called t)))
        (should not-found-called))))

  (nskk-it "calls on-not-found when predicate is nil"
    (nskk-prolog-test-with-isolated-db
      (let ((index (make-nskk-dict-index :predicate nil))
            not-found-called)
        (nskk-search-exact/k index "かんじ" nil
                             (lambda (_) (should nil))
                             (lambda () (setq not-found-called t)))
        (should not-found-called)))))

(nskk-describe "nskk-search-prefix/k"
  (nskk-it "calls on-found with a list of (key . entry) pairs"
    (nskk-with-prolog-entries ((cps-prefix-test "かんじ" ("漢字"))
                               (cps-prefix-test "かんたん" ("簡単")))
      (let ((index (make-nskk-dict-index :predicate 'cps-prefix-test))
            found-results)
        (nskk-search-prefix/k index "かん" nil nil
                              (lambda (r) (setq found-results r))
                              (lambda () (should nil)))
        (should (listp found-results))
        (should (= (length found-results) 2))
        (should (assoc "かんじ" found-results)))))

  (nskk-it "calls on-not-found when no prefix matches"
    (nskk-with-prolog-entries ((cps-prefix-miss-test "にほん" ("日本")))
      (let ((index (make-nskk-dict-index :predicate 'cps-prefix-miss-test))
            not-found-called)
        (nskk-search-prefix/k index "xyz" nil nil
                              (lambda (_) (should nil))
                              (lambda () (setq not-found-called t)))
        (should not-found-called))))

  (nskk-it "respects the limit argument"
    (nskk-with-prolog-entries ((cps-prefix-limit-test "あ" ("亜"))
                               (cps-prefix-limit-test "あい" ("愛"))
                               (cps-prefix-limit-test "あいう" ("合図")))
      (let ((index (make-nskk-dict-index :predicate 'cps-prefix-limit-test))
            found-results)
        (nskk-search-prefix/k index "あ" nil 1
                              (lambda (r) (setq found-results r))
                              (lambda () nil))
        (should (= (length found-results) 1)))))

  (nskk-it "on-found receives alist with nskk-dict-entry values"
    (nskk-with-prolog-entries ((cps-prefix-entry-test "かんじ" ("漢字")))
      (let ((index (make-nskk-dict-index :predicate 'cps-prefix-entry-test))
            found-results)
        (nskk-search-prefix/k index "かん" nil nil
                              (lambda (r) (setq found-results r))
                              (lambda () nil))
        (should found-results)
        (should (nskk-dict-entry-p (cdr (car found-results))))))))

(nskk-describe "nskk-search-partial/k"
  (nskk-it "calls on-found with entries containing the substring"
    (nskk-with-prolog-entries ((cps-partial-test "にほんご" ("日本語"))
                               (cps-partial-test "にほん" ("日本"))
                               (cps-partial-test "ご" ("御")))
      (let ((index (make-nskk-dict-index :predicate 'cps-partial-test))
            found-results)
        (nskk-search-partial/k index "ほん" nil nil
                               (lambda (r) (setq found-results r))
                               (lambda () (should nil)))
        (should (listp found-results))
        (should (= (length found-results) 2)))))

  (nskk-it "calls on-not-found when no entry contains the substring"
    (nskk-with-prolog-entries ((cps-partial-miss-test "かんじ" ("漢字")))
      (let ((index (make-nskk-dict-index :predicate 'cps-partial-miss-test))
            not-found-called)
        (nskk-search-partial/k index "xyz" nil nil
                               (lambda (_) (should nil))
                               (lambda () (setq not-found-called t)))
        (should not-found-called))))

  (nskk-it "respects the limit argument"
    (nskk-with-prolog-entries ((cps-partial-limit-test "あいう" ("哀愁"))
                               (cps-partial-limit-test "あいうえ" ("合図"))
                               (cps-partial-limit-test "あいうえお" ("相合傘")))
      (let ((index (make-nskk-dict-index :predicate 'cps-partial-limit-test))
            found-results)
        (nskk-search-partial/k index "あいう" nil 2
                               (lambda (r) (setq found-results r))
                               (lambda () nil))
        (should (<= (length found-results) 2))))))

(nskk-describe "nskk-search-fuzzy/k"
  (nskk-it "calls on-found with (key entry . distance) triples"
    (nskk-with-prolog-entries ((cps-fuzzy-test "abc" ("ABC")))
      (let ((index (make-nskk-dict-index :predicate 'cps-fuzzy-test))
            (nskk-search-fuzzy-threshold 2)
            found-results)
        (nskk-search-fuzzy/k index "abc" nil nil
                             (lambda (r) (setq found-results r))
                             (lambda () (should nil)))
        (should (listp found-results))
        (should (= (length found-results) 1))
        ;; Shape: (key entry . distance)
        (let ((triple (car found-results)))
          (should (stringp (car triple)))
          (should (nskk-dict-entry-p (cadr triple)))
          (should (integerp (cddr triple)))))))

  (nskk-it "calls on-not-found when all entries exceed the threshold"
    (nskk-with-prolog-entries ((cps-fuzzy-miss-test "xxxxxx" ("X")))
      (let ((index (make-nskk-dict-index :predicate 'cps-fuzzy-miss-test))
            (nskk-search-fuzzy-threshold 1)
            not-found-called)
        (nskk-search-fuzzy/k index "abc" nil nil
                             (lambda (_) (should nil))
                             (lambda () (setq not-found-called t)))
        (should not-found-called))))

  (nskk-it "results are sorted by ascending distance"
    (nskk-with-prolog-entries ((cps-fuzzy-sort-test "abc" ("ABC"))
                               (cps-fuzzy-sort-test "abd" ("ABD"))
                               (cps-fuzzy-sort-test "xyz" ("XYZ")))
      (let ((index (make-nskk-dict-index :predicate 'cps-fuzzy-sort-test))
            (nskk-search-fuzzy-threshold 3)
            found-results)
        (nskk-search-fuzzy/k index "abc" nil nil
                             (lambda (r) (setq found-results r))
                             (lambda () nil))
        ;; Results must be sorted by distance (non-decreasing)
        (should found-results)
        (let ((distances (mapcar #'cddr found-results)))
          (should (cl-every #'<= distances (cdr distances))))))))

(nskk-describe "nskk-search-with-cache/k"
  (nskk-it "calls on-found on cache miss and on cache hit"
    (nskk-with-prolog-entries ((cps-cache-test "かんじ" ("漢字" "感じ")))
      (let ((index (make-nskk-dict-index :predicate 'cps-cache-test))
            (cache (nskk-cache-create :type 'lru :capacity 10))
            first-result second-result)
        ;; First lookup — cache miss, but result found; on-found fires
        (nskk-search-with-cache/k cache index "かんじ" 'exact nil nil
                                  (lambda (r) (setq first-result r))
                                  (lambda () (should nil)))
        (should first-result)
        ;; Second lookup — cache hit; on-found fires with cached result
        (nskk-search-with-cache/k cache index "かんじ" 'exact nil nil
                                  (lambda (r) (setq second-result r))
                                  (lambda () (should nil)))
        (should second-result)
        (should (equal first-result second-result)))))

  (nskk-it "calls on-not-found when key is absent"
    (nskk-with-prolog-entries ((cps-cache-miss-test "かんじ" ("漢字")))
      (let ((index (make-nskk-dict-index :predicate 'cps-cache-miss-test))
            (cache (nskk-cache-create :type 'lru :capacity 10))
            not-found-called)
        (nskk-search-with-cache/k cache index "ない" 'exact nil nil
                                  (lambda (_r) (should nil))
                                  (lambda () (setq not-found-called t)))
        (should not-found-called))))

  (nskk-it "signals wrong-type-argument for an invalid cache object"
    (nskk-with-prolog-entries ((cps-cache-err-test "かんじ" ("漢字")))
      (let ((index (make-nskk-dict-index :predicate 'cps-cache-err-test)))
        (should-error
         (nskk-search-with-cache/k "not-a-cache" index "かんじ" 'exact nil nil
                                   #'identity #'ignore)
         :type 'wrong-type-argument)))))

;; PBT-015 — nskk-search-with-cache/k calls exactly one callback per invocation
(nskk-property-test search-cache-k-mutual-exclusion
  ((q search-query))
  (nskk-prolog-test-with-isolated-db
    (let* ((index (nskk-search-test--make-index '(("かんじ" . ("漢字")))))
           (cache (nskk-cache-create :type 'lru :capacity 10))
           (found-count 0)
           (not-found-count 0))
      (nskk-search-with-cache/k cache index q 'exact nil nil
                                (lambda (_r) (cl-incf found-count))
                                (lambda () (cl-incf not-found-count)))
      ;; Exactly one callback must have fired
      (= 1 (+ found-count not-found-count))))
  30)

;;;
;;; PBT: post-process pipeline invariants
;;;

;; PBT-015 — post-process output has no duplicate keys (dedup invariant)
;;
;; For any list of (key . entry) pairs (including deliberate duplicates),
;; nskk--search-post-process-results must produce a list where every key
;; appears exactly once.
(nskk-property-test-seeded search-post-process-no-duplicates
  ((a search-query)
   (b search-query))
  (let* ((e1 (make-nskk-dict-entry :key a :candidates (list a)))
         (e2 (make-nskk-dict-entry :key b :candidates (list b)))
         ;; Deliberately insert a duplicate of (a . e1)
         (results (list (cons a e1) (cons b e2) (cons a e1)))
         (processed (nskk--search-post-process-results results nil nil))
         (keys (mapcar #'car processed)))
    ;; No duplicate keys in output
    (= (length keys)
       (length (cl-remove-duplicates keys :test #'equal))))
  40
  37)

;; PBT-016 — post-process respects the LIMIT argument
;;
;; For any non-empty results list and any positive limit L, the output
;; must have at most L elements.
(nskk-property-test-seeded search-post-process-limit-respected
  ((a search-query)
   (b search-query)
   (c search-query)
   (d search-query))
  (let* ((entries (mapcar (lambda (k)
                            (cons k (make-nskk-dict-entry :key k :candidates (list k))))
                          (list a b c d)))
         (limit 2)
         (processed (nskk--search-post-process-results entries nil limit)))
    (<= (length processed) limit))
  40
  41)

;; PBT-017 — post-process with sort=kana produces non-decreasing key order
;;
;; When nskk-search-sort-method is 'kana, every adjacent pair of keys in the
;; output must satisfy (not (string< key[n+1] key[n])).
(nskk-property-test-seeded search-post-process-kana-sort-order
  ((a search-query)
   (b search-query)
   (c search-query))
  (let* ((nskk-search-sort-method 'kana)
         (entries (list (cons a (make-nskk-dict-entry :key a :candidates (list a)))
                        (cons b (make-nskk-dict-entry :key b :candidates (list b)))
                        (cons c (make-nskk-dict-entry :key c :candidates (list c)))))
         (processed (nskk--search-post-process-results entries nil nil))
         (keys (mapcar #'car processed)))
    (cl-loop for (prev . rest) on keys
             while rest
             always (not (string< (car rest) prev))))
  40
  43)

(provide 'nskk-search-test)

;;; nskk-search-test.el ends here
