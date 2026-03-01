;;; nskk-search-test.el --- Tests for nskk-search.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

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

(defun nskk-search-test--make-entry (key candidates)
  "Create a test dict-entry with KEY and CANDIDATES."
  (make-nskk-dict-entry :key key :candidates candidates))

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
;;; nskk-search trie
;;;

(nskk-describe "nskk-search trie"
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

  (nskk-context "dedupe-fuzzy"
    (nskk-it "returns all entries when no duplicates"
      (let ((results `(("a" entry1 . 0) ("b" entry2 . 1) ("c" entry3 . 2))))
        (should (= (length (nskk-search--dedupe-fuzzy results)) 3))))

    (nskk-it "keeps the entry with the smallest distance"
      (let* ((far   `("漢字" entry-far  . 3))
             (close `("漢字" entry-close . 1))
             (results (list far close)))
        (let ((deduped (nskk-search--dedupe-fuzzy results)))
          (should (= (length deduped) 1))
          ;; The entry with smaller distance should be kept
          (should (= (cddr (car deduped)) 1)))))

    (nskk-it "keeps first entry when distances are equal"
      (let* ((first  `("同じ" entry-first  . 2))
             (second `("同じ" entry-second . 2))
             (results (list first second)))
        (let ((deduped (nskk-search--dedupe-fuzzy results)))
          (should (= (length deduped) 1)))))

    (nskk-it "returns nil for empty input"
      (should (null (nskk-search--dedupe-fuzzy nil))))))

;;;
;;; Levenshtein Distance Tests
;;;

(nskk-describe "nskk-search--levenshtein-distance"
  (nskk-it "returns 0 for identical strings"
    (should (= (nskk-search--levenshtein-distance "abc" "abc") 0)))

  (nskk-it "handles empty string cases"
    (nskk-then
      (should (= (nskk-search--levenshtein-distance "" "") 0))
      (should (= (nskk-search--levenshtein-distance "abc" "") 3))
      (should (= (nskk-search--levenshtein-distance "" "abc") 3))))

  (nskk-it "returns 1 for single insertion"
    (should (= (nskk-search--levenshtein-distance "abc" "abcd") 1)))

  (nskk-it "returns 1 for single deletion"
    (should (= (nskk-search--levenshtein-distance "abcd" "abc") 1)))

  (nskk-it "returns 1 for single replacement"
    (should (= (nskk-search--levenshtein-distance "abc" "axc") 1)))

  (nskk-it "returns 3 for kitten -> sitting"
    (should (= (nskk-search--levenshtein-distance "kitten" "sitting") 3)))

  (nskk-it "works correctly with Japanese strings"
    (nskk-then
      (should (= (nskk-search--levenshtein-distance "かんじ" "かんじ") 0))
      (should (= (nskk-search--levenshtein-distance "かんじ" "かんき") 1))
      (should (= (nskk-search--levenshtein-distance "にほん" "にほんご") 1)))))

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

  (nskk-context "duplicate removal"
    (nskk-it "returns all entries when there are no duplicates"
      (let ((results '(("a" . 1) ("b" . 2) ("c" . 3))))
        (let ((unique (nskk-search--remove-duplicates results)))
          (should (= (length unique) 3)))))

    (nskk-it "deduplicates keeping the first occurrence"
      (let ((results '(("a" . 1) ("b" . 2) ("a" . 3) ("c" . 4) ("b" . 5))))
        (let ((unique (nskk-search--remove-duplicates results)))
          (should (= (length unique) 3))
          ;; First occurrence should be kept
          (should (equal (cdr (assoc "a" unique)) 1))
          (should (equal (cdr (assoc "b" unique)) 2)))))

    (nskk-it "returns nil for empty input"
      (should (null (nskk-search--remove-duplicates nil))))))

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

  (nskk-it "sort-by-frequency currently acts as passthrough"
    (let ((results '(("a" . 1) ("b" . 2) ("c" . 3))))
      (let ((sorted (nskk-search-sort-by-frequency results)))
        (should (equal sorted results)))))

  (nskk-it "sort method 'none' returns results unchanged"
    (let ((nskk-search-sort-method 'none)
          (results '(("c" . 3) ("a" . 1) ("b" . 2))))
      (let ((sorted (nskk-search--sort-results results)))
        (should (equal sorted results)))))

  (nskk-it "sort method 'kana' sorts results"
    (let ((nskk-search-sort-method 'kana)
          (results '(("さ" . 3) ("あ" . 1) ("か" . 2))))
      (let ((sorted (nskk-search--sort-results results)))
        (should (equal (car (nth 0 sorted)) "あ"))))))

;;;
;;; Cache Key Generation Tests
;;;

(nskk-describe "nskk-search--cache-key"
  (nskk-it "generates distinct string keys for different parameter combinations"
    (let ((key1 (nskk-search--cache-key "query" 'exact nil))
          (key2 (nskk-search--cache-key "query" 'prefix nil))
          (key3 (nskk-search--cache-key "query" 'exact 'okuri-ari)))
      (should (stringp key1))
      (should (stringp key2))
      (should (stringp key3))
      ;; Different parameters should produce different keys
      (should (not (equal key1 key2)))
      (should (not (equal key1 key3)))))

  (nskk-it "generates a string containing 'exact' and 'none' for nil parameters"
    (let ((key (nskk-search--cache-key "query" nil nil)))
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
    :body (let* ((all-queries '(("query1" exact   nil)
                                ("query1" prefix  nil)
                                ("query1" exact   okuri-ari)
                                ("query1" exact   okuri-nasi)
                                ("query2" exact   nil)
                                ("query1" fuzzy   nil)
                                ("query1" partial okuri-ari)))
                 (this-key (nskk-search--cache-key query type okuri))
                 (other-keys (mapcar (lambda (row)
                                       (nskk-search--cache-key (nth 0 row)
                                                                (nth 1 row)
                                                                (nth 2 row)))
                                     (cl-remove-if (lambda (row)
                                                     (and (equal (nth 0 row) query)
                                                          (eq (nth 1 row) type)
                                                          (eq (nth 2 row) okuri)))
                                                   all-queries))))
            ;; This key must be a string and must differ from all other combinations
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

(nskk-describe "nskk-search--candidate-word"
  (nskk-it "returns a string candidate as-is"
    (should (equal (nskk-search--candidate-word "漢字") "漢字")))

  (nskk-it "extracts the car of a cons cell candidate"
    (should (equal (nskk-search--candidate-word '("漢字" . "okurigana")) "漢字")))

  (nskk-it "returns nil for non-string, non-cons values"
    (nskk-then
      (should (null (nskk-search--candidate-word 42)))
      (should (null (nskk-search--candidate-word nil)))
      (should (null (nskk-search--candidate-word '(42 . "x")))))))

;;;
;;; Candidate Score Tests
;;;

(nskk-describe "nskk-search--candidate-score"
  (nskk-it "returns 0 when no learning data exists"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (should (= (nskk-search--candidate-score "かんじ" "漢字") 0))))

  (nskk-it "returns value from Prolog learning-score/3"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 5)))
      (should (= (nskk-search--candidate-score "かんじ" "漢字") 5))))

  (nskk-it "works with cons cell candidates"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 3)))
      (should (= (nskk-search--candidate-score "かんじ" '("漢字" . "ji")) 3))))

  (nskk-it "is reading-specific (different readings return different scores)"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 10)))
      (nskk-prolog-assert '((learning-score "もじ" "漢字" 2)))
      (should (= (nskk-search--candidate-score "かんじ" "漢字") 10))
      (should (= (nskk-search--candidate-score "もじ" "漢字") 2)))))

;;;
;;; Reading Score Tests
;;;

(nskk-describe "nskk-search--reading-score"
  (nskk-it "returns 0 for a non-entry value"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (should (= (nskk-search--reading-score "かんじ" "not-an-entry") 0))))

  (nskk-it "returns maximum score across all candidates"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 3)))
      (nskk-prolog-assert '((learning-score "かんじ" "感じ" 7)))
      (let ((entry (make-nskk-dict-entry :key "かんじ" :candidates '("漢字" "感じ"))))
        (should (= (nskk-search--reading-score "かんじ" entry) 7)))))

  (nskk-it "returns 0 when no learning data exists for any candidate"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((entry (make-nskk-dict-entry :key "かんじ" :candidates '("漢字" "感じ"))))
        (should (= (nskk-search--reading-score "かんじ" entry) 0))))))

;;;
;;; Sort Entry by Learning Tests
;;;

(nskk-describe "nskk-search--sort-entry-by-learning"
  (nskk-it "reorders candidates by Prolog score descending"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 1)))
      (nskk-prolog-assert '((learning-score "かんじ" "感じ" 5)))
      (let ((entry (make-nskk-dict-entry :key "かんじ" :candidates '("漢字" "感じ"))))
        (nskk-search--sort-entry-by-learning entry)
        ;; Higher score (感じ=5) should come first
        (should (equal (car (nskk-dict-entry-candidates entry)) "感じ")))))

  (nskk-it "preserves original order when scores are equal"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((entry (make-nskk-dict-entry :key "かんじ" :candidates '("漢字" "感じ"))))
        (let ((result (nskk-search--sort-entry-by-learning entry)))
          ;; Returns the entry itself
          (should (eq result entry))
          ;; All candidates still present
          (should (= (length (nskk-dict-entry-candidates entry)) 2))))))

  (nskk-it "returns nil gracefully for nil input"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (should (null (nskk-search--sort-entry-by-learning nil))))))

;;;
;;; Sort Prefix Results Tests
;;;

(nskk-describe "nskk-search--sort-prefix-results"
  (nskk-it "orders entries by maximum learning score descending"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "あ" "亜" 1)))
      (nskk-prolog-assert '((learning-score "か" "家" 10)))
      (let* ((entry-a (make-nskk-dict-entry :key "あ" :candidates '("亜")))
             (entry-k (make-nskk-dict-entry :key "か" :candidates '("家")))
             (results `(("あ" . ,entry-a) ("か" . ,entry-k))))
        (let ((sorted (nskk-search--sort-prefix-results results)))
          ;; Higher score entry (か=10) should come first
          (should (equal (car (car sorted)) "か"))))))

  (nskk-it "handles an empty list"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (should (null (nskk-search--sort-prefix-results nil)))))

  (nskk-it "returns all results when no learning data exists"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let* ((e1 (make-nskk-dict-entry :key "あ" :candidates '("亜")))
             (e2 (make-nskk-dict-entry :key "い" :candidates '("意")))
             (results `(("あ" . ,e1) ("い" . ,e2))))
        ;; All scores are 0, so original order is preserved by stable sort
        (let ((sorted (nskk-search--sort-prefix-results results)))
          (should (= (length sorted) 2)))))))

;;;
;;; Search Entry Count Tests
;;;

(nskk-describe "nskk-search-entry-count"
  (nskk-it "returns total entry count without search type"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((index (nskk-search-test--make-index
                    '(("a" . ("1")) ("b" . ("2")) ("c" . ("3"))))))
        (should (= (nskk-search-entry-count index) 3)))))

  (nskk-it "returns 1 for exact search on an existing key"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("かんじ" . ("漢字")) ("かんたん" . ("簡単"))))))
        (should (= (nskk-search-entry-count index 'exact "かんじ" nil) 1)))))

  (nskk-it "returns 0 for exact search on a missing key"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("かんじ" . ("漢字"))))))
        (should (= (nskk-search-entry-count index 'exact "みつからない" nil) 0)))))

  (nskk-it "returns count of all prefix matches"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    nil
                    '(("かん" . ("缶")) ("かんじ" . ("漢字")) ("かんたん" . ("簡単"))))))
        (should (>= (nskk-search-entry-count index 'prefix "かん" nil) 3))))))

;;;
;;; Learning Data: nskk-search-learn Tests
;;;

(nskk-describe "nskk-search-learn"
  (nskk-it "initializes score to 1 for a new candidate"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((nskk-search--dirty-flag nil))
        (nskk-search-learn "かんじ" "漢字")
        (should (= (nskk-prolog-query-value
                    '(learning-score "かんじ" "漢字" \?s) '\?s)
                   1))
        ;; Dirty flag must be set
        (should nskk-search--dirty-flag))))

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
      (let ((nskk-search--dirty-flag nil))
        (nskk-search-learn "かんじ" nil)
        ;; No learning-score fact should be created
        (should-not (nskk-prolog-query-value
                     '(learning-score "かんじ" \?c \?s) '\?s))
        ;; Dirty flag must remain unset
        (should-not nskk-search--dirty-flag))))

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
  (nskk-it "save followed by load restores all learning-score facts"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let* ((tmp-file (make-temp-file "nskk-learning-test" nil ".dat"))
             (nskk-search-learning-file tmp-file))
        (unwind-protect
            (progn
              ;; Arrange: assert two learning scores
              (nskk-prolog-assert '((learning-score "かんじ" "漢字" 3)))
              (nskk-prolog-assert '((learning-score "かんじ" "感じ" 7)))
              ;; Act: save
              (nskk-search-save-learning-data)
              ;; Clear and reload
              (nskk-prolog-retract-all 'learning-score 3)
              (nskk-prolog-set-index 'learning-score 3 :hash)
              (nskk-search--load-learning-data)
              ;; Assert: both facts restored
              (should (= (nskk-prolog-query-value
                          '(learning-score "かんじ" "漢字" \?s) '\?s) 3))
              (should (= (nskk-prolog-query-value
                          '(learning-score "かんじ" "感じ" \?s) '\?s) 7)))
          (when (file-exists-p tmp-file)
            (delete-file tmp-file))))))

  (nskk-it "save sets nskk-search--dirty-flag to nil"
    (nskk-prolog-test-with-isolated-db
      (let* ((tmp-file (make-temp-file "nskk-learning-test" nil ".dat"))
             (nskk-search-learning-file tmp-file)
             (nskk-search--dirty-flag t))
        (unwind-protect
            (progn
              (nskk-search-save-learning-data)
              (should-not nskk-search--dirty-flag))
          (when (file-exists-p tmp-file)
            (delete-file tmp-file))))))

  (nskk-it "loading a nonexistent file silently loads nothing"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((nskk-search-learning-file "/nonexistent/nskk-test-learning.dat"))
        (nskk-search--load-learning-data)
        ;; No facts should be loaded
        (should-not (nskk-prolog-query '(learning-score \?r \?c \?s))))))

  (nskk-it "load retracts existing facts before loading (replace, not merge)"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let* ((tmp-file (make-temp-file "nskk-learning-test" nil ".dat"))
             (nskk-search-learning-file tmp-file))
        (unwind-protect
            (progn
              ;; Write just one fact to disk
              (nskk-prolog-assert '((learning-score "あ" "亜" 1)))
              (nskk-search-save-learning-data)
              ;; Now add a DIFFERENT fact to the DB (stale)
              (nskk-prolog-retract-all 'learning-score 3)
              (nskk-prolog-set-index 'learning-score 3 :hash)
              (nskk-prolog-assert '((learning-score "stale" "stale" 99)))
              ;; Load should replace, not merge
              (nskk-search--load-learning-data)
              ;; Stale fact should be gone
              (should-not (nskk-prolog-query-value
                           '(learning-score "stale" "stale" \?s) '\?s))
              ;; Original fact should be present
              (should (= (nskk-prolog-query-value
                          '(learning-score "あ" "亜" \?s) '\?s) 1)))
          (when (file-exists-p tmp-file)
            (delete-file tmp-file)))))))

;;;
;;; Auto-Save Timer Tests
;;;

(nskk-describe "nskk-search auto-save timer"
  (nskk-it "start-auto-save creates a repeating timer via run-with-timer"
    (let ((nskk-search--auto-save-timer nil)
          (nskk-search-auto-save-interval 300)
          (created-args nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (delay repeat fn)
                   (setq created-args (list delay repeat fn))
                   'mock-timer)))
        (nskk-search--start-auto-save)
        (should (eq nskk-search--auto-save-timer 'mock-timer))
        (should (= (nth 0 created-args) 300))
        (should (= (nth 1 created-args) 300))
        (should (eq (nth 2 created-args) #'nskk-search--auto-save-handler)))))

  (nskk-it "start-auto-save cancels an existing timer before creating a new one"
    (let ((nskk-search--auto-save-timer 'old-timer)
          (nskk-search-auto-save-interval 300)
          (cancelled nil))
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (timer) (setq cancelled timer)))
                ((symbol-function 'run-with-timer)
                 (lambda (&rest _) 'new-timer)))
        (nskk-search--start-auto-save)
        (should (eq cancelled 'old-timer))
        (should (eq nskk-search--auto-save-timer 'new-timer)))))

  (nskk-it "stop-auto-save cancels the timer and sets the variable to nil"
    (let ((nskk-search--auto-save-timer 'active-timer)
          (cancelled nil))
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (timer) (setq cancelled timer))))
        (nskk-search--stop-auto-save)
        (should (eq cancelled 'active-timer))
        (should (null nskk-search--auto-save-timer)))))

  (nskk-it "stop-auto-save does nothing when no timer is active"
    (let ((nskk-search--auto-save-timer nil)
          (cancelled nil))
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (timer) (setq cancelled timer))))
        (nskk-search--stop-auto-save)
        ;; cancel-timer must NOT have been called
        (should (null cancelled)))))

  (nskk-it "auto-save handler calls nskk-search-save-learning-data when dirty"
    (let ((nskk-search--dirty-flag t)
          (saved nil))
      (cl-letf (((symbol-function 'nskk-search-save-learning-data)
                 (lambda () (setq saved t))))
        (nskk-search--auto-save-handler)
        (should saved))))

  (nskk-it "auto-save handler does not call save when dirty flag is nil"
    (let ((nskk-search--dirty-flag nil)
          (saved nil))
      (cl-letf (((symbol-function 'nskk-search-save-learning-data)
                 (lambda () (setq saved t))))
        (nskk-search--auto-save-handler)
        (should-not saved)))))

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

  (nskk-it "counts all entries and okuri-ari entries in a dict index"
    (nskk-with-prolog-entries ((entry-count-dict "a" ("v1"))
                               (entry-count-dict "b" ("v2"))
                               (entry-count-dict "c" ("v3"))
                               (entry-count-dict "d" ("v4"))
                               (entry-count-dict "e" ("v5")))
      (let ((index (make-nskk-dict-index :predicate 'entry-count-dict)))
        (should (= (nskk-dict--struct-entry-count index nil) 5))
        (should (= (nskk-dict--struct-entry-count index 'okuri-ari) 5))))))

;;;
;;; Property-Based Tests
;;;

;; PBT-001 — Levenshtein symmetry (seeded PBT, 50 runs)
(nskk-property-test-seeded search-levenshtein-symmetry
  ((a romaji-basic)
   (b romaji-basic))
  (= (nskk-search--levenshtein-distance a b)
     (nskk-search--levenshtein-distance b a))
  50
  7)

;; PBT-002 — Levenshtein identity (seeded PBT, 50 runs)
(nskk-property-test-seeded search-levenshtein-identity
  ((input romaji-basic))
  (= (nskk-search--levenshtein-distance input input) 0)
  50
  13)

;; PBT-003 — Levenshtein triangle inequality (seeded PBT, 30 runs)
(nskk-property-test-seeded search-levenshtein-triangle-inequality
  ((a romaji-basic)
   (b romaji-basic)
   (c romaji-basic))
  (<= (nskk-search--levenshtein-distance a c)
      (+ (nskk-search--levenshtein-distance a b)
         (nskk-search--levenshtein-distance b c)))
  30
  17)

(provide 'nskk-search-test)

;;; nskk-search-test.el ends here
