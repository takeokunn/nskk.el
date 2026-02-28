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
(require 'nskk-trie)
(require 'nskk-cache)
(require 'nskk-test-framework)

;;;
;;; Test Helpers
;;;

(defun nskk-search-test--make-index (&optional entries-alist trie-entries)
  "Create a test dict-index.
ENTRIES-ALIST is ((key . value) ...) for the hash table.
TRIE-ENTRIES is ((key . value) ...) for the prefix trie."
  (let ((entries (make-hash-table :test 'equal))
        (trie (when trie-entries (nskk-trie-create))))
    (dolist (pair entries-alist)
      (puthash (car pair) (cdr pair) entries))
    (when trie-entries
      (dolist (pair trie-entries)
        (nskk-trie-insert trie (car pair) (cdr pair))))
    (make-nskk-dict-index :entries entries :by-prefix trie)))

(defun nskk-search-test--make-entry (key candidates)
  "Create a test dict-entry with KEY and CANDIDATES."
  (make-nskk-dict-entry :key key :candidates candidates))

;;;
;;; Exact Search Tests
;;;

(nskk-deftest-unit search-exact-existing-key
  "Test exact search for an existing key."
  (let* ((entry (nskk-search-test--make-entry "かんじ" '("漢字" "感じ")))
         (index (nskk-search-test--make-index
                 `(("かんじ" . ,entry)))))
    (let ((result (nskk-search index "かんじ" 'exact)))
      (should result)
      (should (nskk-dict-entry-p result))
      (should (equal (nskk-dict-entry-candidates result) '("漢字" "感じ"))))))

(nskk-deftest-unit search-exact-non-existing-key
  "Test exact search for a non-existing key."
  (let ((index (nskk-search-test--make-index '(("abc" . "value")))))
    (let ((result (nskk-search index "xyz" 'exact)))
      (should (null result)))))

(nskk-deftest-unit search-exact-default-type
  "Test that search defaults to exact type."
  (let ((index (nskk-search-test--make-index '(("key" . "value")))))
    (let ((result (nskk-search index "key")))
      (should (equal result "value")))))

;;;
;;; Prefix Search Tests
;;;

(nskk-deftest-unit search-prefix-basic
  "Test basic prefix search."
  (let ((index (nskk-search-test--make-index
                nil
                '(("かん" . "缶") ("かんじ" . "漢字") ("かんたん" . "簡単") ("きん" . "金")))))
    (let ((results (nskk-search index "かん" 'prefix)))
      (should (listp results))
      (should (>= (length results) 3))
      (should (assoc "かん" results))
      (should (assoc "かんじ" results))
      (should (assoc "かんたん" results)))))

(nskk-deftest-unit search-prefix-no-match
  "Test prefix search with no matching entries."
  (let ((index (nskk-search-test--make-index
                nil
                '(("abc" . 1) ("abd" . 2)))))
    (let ((results (nskk-search index "xyz" 'prefix)))
      (should (null results)))))

(nskk-deftest-unit search-prefix-with-limit
  "Test prefix search with limit."
  (let ((index (nskk-search-test--make-index
                nil
                '(("aa" . 1) ("ab" . 2) ("ac" . 3) ("ad" . 4)))))
    (let ((results (nskk-search index "a" 'prefix nil 2)))
      (should (<= (length results) 2)))))

(nskk-deftest-unit search-prefix-nil-trie
  "Test prefix search when trie is nil."
  (let ((index (make-nskk-dict-index :entries (make-hash-table :test 'equal)
                                     :by-prefix nil)))
    (let ((results (nskk-search index "test" 'prefix)))
      (should (null results)))))

;;;
;;; Partial Search Tests
;;;

(nskk-deftest-unit search-partial-basic
  "Test basic partial search."
  (let ((index (nskk-search-test--make-index
                '(("abcdef" . "v1") ("xyzabc" . "v2") ("hello" . "v3")))))
    (let ((results (nskk-search index "abc" 'partial)))
      (should (listp results))
      (should (= (length results) 2))
      (should (assoc "abcdef" results))
      (should (assoc "xyzabc" results)))))

(nskk-deftest-unit search-partial-no-match
  "Test partial search with no matches."
  (let ((index (nskk-search-test--make-index
                '(("hello" . "v1") ("world" . "v2")))))
    (let ((results (nskk-search index "xyz" 'partial)))
      (should (null results)))))

(nskk-deftest-unit search-partial-with-limit
  "Test partial search with limit."
  (let ((index (nskk-search-test--make-index
                '(("abc1" . "v1") ("abc2" . "v2") ("abc3" . "v3") ("abc4" . "v4")))))
    (let ((results (nskk-search index "abc" 'partial nil 2)))
      (should (= (length results) 2)))))

(nskk-deftest-unit search-partial-japanese
  "Test partial search with Japanese strings."
  (let ((index (nskk-search-test--make-index
                '(("にほんご" . "Japanese")
                  ("にほん" . "Japan")
                  ("せかい" . "World")))))
    (let ((results (nskk-search index "にほん" 'partial)))
      (should (= (length results) 2))
      (should (assoc "にほんご" results))
      (should (assoc "にほん" results)))))

;;;
;;; Fuzzy Search Tests
;;;

(nskk-deftest-unit search-fuzzy-exact-match
  "Test fuzzy search finds exact matches (distance 0)."
  (let ((index (nskk-search-test--make-index
                '(("abc" . "v1") ("xyz" . "v2")))))
    (let ((results (nskk-search index "abc" 'fuzzy)))
      (should (listp results))
      (should (> (length results) 0))
      ;; The exact match should be first (distance 0)
      (let ((first-result (car results)))
        (should (equal (car first-result) "abc"))))))

(nskk-deftest-unit search-fuzzy-close-match
  "Test fuzzy search finds close matches."
  (let ((nskk-search-fuzzy-threshold 2)
        (index (nskk-search-test--make-index
                '(("abc" . "v1") ("abd" . "v2") ("xyz" . "v3")))))
    (let ((results (nskk-search index "abc" 'fuzzy)))
      (should (listp results))
      ;; "abc" (dist=0) and "abd" (dist=1) should match, "xyz" (dist=3) may not
      (should (>= (length results) 2)))))

(nskk-deftest-unit search-fuzzy-with-limit
  "Test fuzzy search with limit."
  (let ((nskk-search-fuzzy-threshold 3)
        (index (nskk-search-test--make-index
                '(("aaa" . 1) ("aab" . 2) ("aac" . 3) ("aad" . 4)))))
    (let ((results (nskk-search index "aaa" 'fuzzy nil 2)))
      (should (<= (length results) 2)))))

(nskk-deftest-unit search-fuzzy-sorted-by-distance
  "Test fuzzy search results are sorted by distance."
  (let ((nskk-search-fuzzy-threshold 3)
        (index (nskk-search-test--make-index
                '(("abc" . 1) ("abx" . 2) ("axx" . 3) ("xxx" . 4)))))
    (let ((results (nskk-search index "abc" 'fuzzy)))
      (when (> (length results) 1)
        ;; Verify distances are non-decreasing
        (let ((prev-dist -1))
          (dolist (r results)
            (let ((dist (cddr r)))
              (should (>= dist prev-dist))
              (setq prev-dist dist))))))))

;;;
;;; Levenshtein Distance Tests
;;;

(nskk-deftest-unit search-levenshtein-identical
  "Test Levenshtein distance between identical strings."
  (should (= (nskk-search--levenshtein-distance "abc" "abc") 0)))

(nskk-deftest-unit search-levenshtein-empty-strings
  "Test Levenshtein distance with empty strings."
  (should (= (nskk-search--levenshtein-distance "" "") 0))
  (should (= (nskk-search--levenshtein-distance "abc" "") 3))
  (should (= (nskk-search--levenshtein-distance "" "abc") 3)))

(nskk-deftest-unit search-levenshtein-single-insert
  "Test Levenshtein distance for single insertion."
  (should (= (nskk-search--levenshtein-distance "abc" "abcd") 1)))

(nskk-deftest-unit search-levenshtein-single-delete
  "Test Levenshtein distance for single deletion."
  (should (= (nskk-search--levenshtein-distance "abcd" "abc") 1)))

(nskk-deftest-unit search-levenshtein-single-replace
  "Test Levenshtein distance for single replacement."
  (should (= (nskk-search--levenshtein-distance "abc" "axc") 1)))

(nskk-deftest-unit search-levenshtein-multiple-ops
  "Test Levenshtein distance for multiple operations."
  (should (= (nskk-search--levenshtein-distance "kitten" "sitting") 3)))

(nskk-deftest-unit search-levenshtein-japanese
  "Test Levenshtein distance with Japanese strings."
  (should (= (nskk-search--levenshtein-distance "かんじ" "かんじ") 0))
  (should (= (nskk-search--levenshtein-distance "かんじ" "かんき") 1))
  (should (= (nskk-search--levenshtein-distance "にほん" "にほんご") 1)))

;;;
;;; Duplicate Removal Tests
;;;

(nskk-deftest-unit search-remove-duplicates-no-dups
  "Test duplicate removal with no duplicates."
  (let ((results '(("a" . 1) ("b" . 2) ("c" . 3))))
    (let ((unique (nskk-search--remove-duplicates results)))
      (should (= (length unique) 3)))))

(nskk-deftest-unit search-remove-duplicates-with-dups
  "Test duplicate removal with duplicates."
  (let ((results '(("a" . 1) ("b" . 2) ("a" . 3) ("c" . 4) ("b" . 5))))
    (let ((unique (nskk-search--remove-duplicates results)))
      (should (= (length unique) 3))
      ;; First occurrence should be kept
      (should (equal (cdr (assoc "a" unique)) 1))
      (should (equal (cdr (assoc "b" unique)) 2)))))

(nskk-deftest-unit search-remove-duplicates-empty
  "Test duplicate removal with empty list."
  (should (null (nskk-search--remove-duplicates nil))))

;;;
;;; Sort Tests
;;;

(nskk-deftest-unit search-sort-by-kana-order
  "Test sorting by kana order."
  (let ((results '(("さ" . 3) ("あ" . 1) ("か" . 2))))
    (let ((sorted (nskk-search-sort-by-kana-order results)))
      (should (equal (car (nth 0 sorted)) "あ"))
      (should (equal (car (nth 1 sorted)) "か"))
      (should (equal (car (nth 2 sorted)) "さ")))))

(nskk-deftest-unit search-sort-by-frequency-passthrough
  "Test that frequency sort currently acts as passthrough."
  (let ((results '(("a" . 1) ("b" . 2) ("c" . 3))))
    (let ((sorted (nskk-search-sort-by-frequency results)))
      (should (equal sorted results)))))

(nskk-deftest-unit search-sort-method-none
  "Test that sort method 'none' returns results unchanged."
  (let ((nskk-search-sort-method 'none)
        (results '(("c" . 3) ("a" . 1) ("b" . 2))))
    (let ((sorted (nskk-search--sort-results results)))
      (should (equal sorted results)))))

(nskk-deftest-unit search-sort-method-kana
  "Test that sort method 'kana' sorts results."
  (let ((nskk-search-sort-method 'kana)
        (results '(("さ" . 3) ("あ" . 1) ("か" . 2))))
    (let ((sorted (nskk-search--sort-results results)))
      (should (equal (car (nth 0 sorted)) "あ")))))

;;;
;;; Error Handling Tests
;;; Note: nskk-dict-search errors do not inherit from 'error due to
;;; nskk-dict-error not being defined with define-error, so we use
;;; condition-case directly instead of should-error.
;;;

(nskk-deftest-unit search-error-nil-query
  "Test search with nil query signals error."
  (let ((index (nskk-search-test--make-index '(("a" . 1))))
        (caught nil))
    (condition-case _err
        (nskk-search index nil 'exact)
      (nskk-dict-search-invalid-query (setq caught t)))
    (should caught)))

(nskk-deftest-unit search-error-empty-query
  "Test search with empty query signals error."
  (let ((index (nskk-search-test--make-index '(("a" . 1))))
        (caught nil))
    (condition-case _err
        (nskk-search index "" 'exact)
      (nskk-dict-search-invalid-query (setq caught t)))
    (should caught)))

(nskk-deftest-unit search-error-invalid-index
  "Test search with invalid index signals error."
  (let ((caught nil))
    (condition-case _err
        (nskk-search "not-an-index" "query" 'exact)
      (nskk-dict-search-invalid-index (setq caught t)))
    (should caught)))

(nskk-deftest-unit search-error-invalid-search-type
  "Test search with invalid search type signals error."
  (let ((index (nskk-search-test--make-index '(("a" . 1))))
        (caught nil))
    (condition-case _err
        (nskk-search index "a" 'invalid-type)
      (nskk-dict-search-invalid-query (setq caught t)))
    (should caught)))

;;;
;;; Cache Key Generation Tests
;;;

(nskk-deftest-unit search-cache-key-generation
  "Test cache key generation."
  (let ((key1 (nskk-search--cache-key "query" 'exact nil))
        (key2 (nskk-search--cache-key "query" 'prefix nil))
        (key3 (nskk-search--cache-key "query" 'exact 'okuri-ari)))
    (should (stringp key1))
    (should (stringp key2))
    (should (stringp key3))
    ;; Different parameters should produce different keys
    (should (not (equal key1 key2)))
    (should (not (equal key1 key3)))))

(nskk-deftest-unit search-cache-key-defaults
  "Test cache key generation with nil parameters."
  (let ((key (nskk-search--cache-key "query" nil nil)))
    (should (stringp key))
    (should (string-match-p "exact" key))
    (should (string-match-p "none" key))))

;;;
;;; Cache Integration Tests
;;;

(nskk-deftest-unit search-with-cache-basic
  "Test search with cache integration."
  (let* ((cache (nskk-cache-create 'lru 100))
         (index (nskk-search-test--make-index '(("test" . "value")))))
    ;; First search (cache miss)
    (let ((result (nskk-search-with-cache cache index "test" 'exact)))
      (should (equal result "value")))
    ;; Second search (cache hit)
    (let ((result (nskk-search-with-cache cache index "test" 'exact)))
      (should (equal result "value")))
    ;; Verify cache statistics
    (let ((stats (nskk-cache-stats cache)))
      (should (= (plist-get stats :hits) 1))
      (should (= (plist-get stats :size) 1)))))

(nskk-deftest-unit search-with-cache-trie
  "Test search with cache for trie index."
  (let* ((cache (nskk-cache-create 'lru 100))
         (trie (nskk-trie-create)))
    (nskk-trie-insert trie "key" "trie-value")
    ;; Search using trie directly
    (let ((result (nskk-search-with-cache cache trie "key" 'exact)))
      (should (equal result "trie-value")))))

(nskk-deftest-unit search-with-cache-invalid-cache
  "Test search with invalid cache signals error."
  (let ((index (nskk-search-test--make-index '(("a" . 1)))))
    (should-error (nskk-search-with-cache "not-a-cache" index "a")
                  :type 'wrong-type-argument)))

;;;
;;; Integration Tests
;;;

(nskk-deftest-integration search-full-workflow
  "Test full search workflow with all search types."
  (let* ((entries '(("かんじ" . "漢字")
                    ("かんたん" . "簡単")
                    ("かん" . "缶")
                    ("きんし" . "禁止")
                    ("きんぎょ" . "金魚")))
         (index (nskk-search-test--make-index
                 entries
                 entries)))
    ;; Exact search
    (let ((result (nskk-search index "かんじ" 'exact)))
      (should (equal result "漢字")))

    ;; Prefix search
    (let ((results (nskk-search index "かん" 'prefix)))
      (should (>= (length results) 3)))

    ;; Partial search
    (let ((results (nskk-search index "かん" 'partial)))
      (should (>= (length results) 3)))

    ;; Fuzzy search
    (let ((nskk-search-fuzzy-threshold 2)
          (results (nskk-search index "かんじ" 'fuzzy)))
      (should (> (length results) 0)))))

(nskk-deftest-integration search-entry-count
  "Test nskk-dict--struct-entry-count function."
  (let ((index (make-nskk-dict-index :entries '(a b c d e))))
    (should (= (nskk-dict--struct-entry-count index nil) 5))
    (should (= (nskk-dict--struct-entry-count index 'okuri-ari) 5))))

(provide 'nskk-search-test)

;;; nskk-search-test.el ends here
