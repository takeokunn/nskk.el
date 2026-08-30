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
PRED-NAME is the Prolog predicate symbol (defaults to
`nskk-search-test-dict').  Existing facts for the predicate are replaced."
  (let* ((pred (or pred-name (quote nskk-search-test-dict)))
         (all-entries (append entries-alist trie-entries)))
    (nskk-prolog-retract-all pred 2)
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
        (nskk-should-candidates '("value") result))))

  (nskk-it "continues post-search observers after an ordinary error"
    (let ((events nil)
          (messages nil)
          (flushes 0)
          (nskk-search-jisyo-hook (list #'nskk--search-flush-caches)))
      (with-temp-buffer
        (setq-local nskk-search-jisyo-hook
                    (list
                     (lambda ()
                       (push 'local-failure events)
                       (error "post hook failure"))
                     (lambda () (push 'local-observer events))
                     t))
        (nskk-with-mocks
            ((nskk--search-flush-caches
              (lambda ()
                (push 'global-cache-invalidation events)
                (cl-incf flushes)))
             (message
              (lambda (fmt &rest args)
                (push (apply #'format fmt args) messages))))
          (nskk--search-run-post-hook)))
      (should
       (equal (nreverse events)
              '(local-failure local-observer global-cache-invalidation)))
      (should (= 1 flushes))
      (should
       (cl-some
        (lambda (text)
          (string-match-p "search-jisyo-hook error" text))
        messages))))

  (nskk-it "propagates post-search quit before later observers"
    (let ((condition '(quit "post hook quit" payload))
          (events nil)
          (nskk-search-jisyo-hook
           (list (lambda () (push 'global-observer events))))
          caught)
      (with-temp-buffer
        (setq-local nskk-search-jisyo-hook
                    (list
                     (lambda ()
                       (push 'quit events)
                       (signal (car condition) (cdr condition)))
                     (lambda () (push 'local-observer events))
                     t))
        (setq caught
              (condition-case signal-condition
                  (progn
                    (nskk--search-run-post-hook)
                    nil)
                (quit signal-condition))))
      (should (equal caught condition))
      (should (equal events '(quit))))))

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
            (should results)
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
  :columns (s1 s2 expected _label)
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
  :columns (okuri-type entry-okuri matches-p _label)
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
;;; Okuri derivation from key shape
;;;

;; An SKK okuri-ari key ends with a single ASCII lower-case letter directly
;; following a non-ASCII kana; everything else is okuri-nasi (nil).
(nskk-deftest-table derive-okuri-from-key
  :columns (key expected _label)
  :rows (("わるi" "i" "kana + trailing ascii letter -> okuri-ari suffix")
         ("うごk" "k" "kana + trailing ascii letter -> okuri-ari suffix")
         ("かんじ" nil "all kana -> okuri-nasi")
         ("a"      nil "single char is too short -> okuri-nasi")
         ("ab"     nil "ascii penultimate char -> okuri-nasi")
         ("わるA"  nil "trailing upper-case is not okurigana -> okuri-nasi")
         (""       nil "empty key -> okuri-nasi"))
  :body (should (equal (nskk--search-derive-okuri key) expected)))

;;;
;;; Okuri-type filtering end-to-end (populated okuri slot)
;;;

(nskk-describe "okuri-type filtering populates and honors the okuri slot"
  (nskk-it "classifies constructed entries by key shape"
    (nskk-then
      (should (equal (nskk-dict-entry-okuri
                      (make-nskk-dict-entry
                       :key "わるi" :candidates '("悪")
                       :okuri (nskk--search-derive-okuri "わるi")))
                     "i"))
      (should (null (nskk-dict-entry-okuri
                     (make-nskk-dict-entry
                      :key "かんじ" :candidates '("漢字")
                      :okuri (nskk--search-derive-okuri "かんじ")))))))

  (nskk-it "exact okuri-ari filter keeps okuri-ari and excludes okuri-nasi"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("わるi" . ("悪")) ("かんじ" . ("漢字"))))))
        ;; okuri-ari key survives an okuri-ari filter, okuri-nasi key does not.
        (should (nskk-search index "わるi" 'exact 'okuri-ari))
        (should (null (nskk-search index "かんじ" 'exact 'okuri-ari))))))

  (nskk-it "exact okuri-nasi filter keeps okuri-nasi and excludes okuri-ari"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    '(("わるi" . ("悪")) ("かんじ" . ("漢字"))))))
        (should (nskk-search index "かんじ" 'exact 'okuri-nasi))
        (should (null (nskk-search index "わるi" 'exact 'okuri-nasi))))))

  (nskk-it "prefix filter separates okuri-ari and okuri-nasi entries"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    nil
                    '(("うごk" . ("動")) ("うごく" . ("動く"))))))
        (let ((ari (nskk-search index "うご" 'prefix 'okuri-ari)))
          (should (assoc "うごk" ari))
          (should-not (assoc "うごく" ari)))
        (let ((nasi (nskk-search index "うご" 'prefix 'okuri-nasi)))
          (should (assoc "うごく" nasi))
          (should-not (assoc "うごk" nasi)))))))

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
      (should (null (nskk--search-post-process-results nil nil nil))))

    (nskk-it "preserves source order for frequency ties at the cutoff"
      (let ((nskk-search-sort-method 'frequency)
            (entries '(("first") ("second") ("third"))))
        (cl-letf (((symbol-function 'nskk--search-reading-score)
                   (lambda (_reading _entry) 0)))
          (should
           (equal (mapcar #'car
                          (nskk--search-post-process-results entries nil 2))
                  '("first" "second")))))))

  (nskk-property-test-seeded search-post-process-limited-equivalent
      ((k1 search-query)
       (k2 search-query)
       (k3 search-query)
       (k4 search-query)
       (k5 search-query))
      (let ((entries (list (cons (format "%s-0" k1) nil)
                           (cons (format "%s-1" k2) nil)
                           (cons (format "%s-2" k3) nil)
                           (cons (format "%s-3" k4) nil)
                           (cons (format "%s-4" k5) nil))))
        (cl-letf (((symbol-function 'nskk--search-reading-score)
                   (lambda (reading _entry) (mod (length reading) 3))))
          (cl-every
           (lambda (method)
             (let* ((nskk-search-sort-method method)
                    (full (nskk--search-post-process-results entries nil nil)))
               (cl-every
                (lambda (limit)
                  (equal (nskk--search-post-process-results entries nil limit)
                         (seq-take full limit)))
                '(-1 0 1 2 3 4 5 6))))
           '(frequency kana none))))
      40
      47)

;;;
;;; Cache Key Generation Tests
;;;

;; Top-level defconst: shared across multiple nskk-describe blocks below.
;; Defined here (not inside any describe block) so it is interned at load time
;; and visible to all test cases without being re-evaluated per-test.
(nskk-describe "nskk--search-cache-key"
  (nskk-it "includes every result-shaping argument"
    (let* ((index (make-nskk-dict-index :predicate 'cache-key-test))
           (base (nskk--search-cache-key index "query" 'exact nil))
           (variants (list
                      (nskk--search-cache-key index "other" 'exact nil)
                      (nskk--search-cache-key index "query" 'prefix nil)
                      (nskk--search-cache-key index "query" 'exact 'okuri-ari)
                      (nskk--search-cache-key index "query" 'exact nil 2))))
      (should (proper-list-p base))
      (dolist (variant variants)
        (should-not (equal base variant)))))
  (nskk-it "includes dictionary identity and sort method"
    (let ((index-a (make-nskk-dict-index :predicate 'cache-key-a))
          (index-b (make-nskk-dict-index :predicate 'cache-key-b)))
      (should-not
       (equal (nskk--search-cache-key index-a "query" 'exact nil)
              (nskk--search-cache-key index-b "query" 'exact nil)))
      (let ((nskk-search-sort-method 'kana))
        (should-not
         (equal (nskk--search-cache-key index-a "query" 'exact nil)
                (let ((nskk-search-sort-method 'none))
                  (nskk--search-cache-key index-a "query" 'exact nil)))))))
  (nskk-it "includes fuzzy threshold only for fuzzy searches"
    (let ((index (make-nskk-dict-index :predicate 'cache-key-fuzzy)))
      (let ((nskk-search-fuzzy-threshold 1))
        (should-not
         (equal (nskk--search-cache-key index "query" 'fuzzy nil)
                (let ((nskk-search-fuzzy-threshold 2))
                  (nskk--search-cache-key index "query" 'fuzzy nil))))
        (should
         (equal (nskk--search-cache-key index "query" 'exact nil)
                (let ((nskk-search-fuzzy-threshold 2))
                  (nskk--search-cache-key index "query" 'exact nil))))))))

;;;
;;; Cache Integration Tests
;;;

(nskk-describe "nskk-search-with-cache"
  (nskk-it "returns correct result on both cache miss and cache hit"
    (nskk-prolog-test-with-isolated-db
      (let* ((cache (nskk-cache-lru-create 100))
             (index (nskk-search-test--make-index '(("test" . ("value"))))))
        (let ((result (nskk-search-with-cache cache index "test" 'exact)))
          (nskk-should-candidates '("value") result))
        (let ((result (nskk-search-with-cache cache index "test" 'exact)))
          (nskk-should-candidates '("value") result))
        (let ((stats (nskk-cache-stats cache)))
          (should (= (plist-get stats :hits) 1))
          (should (= (plist-get stats :size) 1))))))

  (nskk-it "works with a Prolog trie dict index"
    (nskk-prolog-test-with-isolated-db
      (let* ((cache (nskk-cache-lru-create 100))
             (index
              (nskk-search-test--make-index
               '(("key" . ("trie-value")))
               nil
               'cache-test-dict)))
        (let ((result (nskk-search-with-cache cache index "key" 'exact)))
          (nskk-should-candidates '("trie-value") result)))))

  (nskk-it "signals wrong-type-argument for an invalid cache"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index '(("a" . ("1"))))))
        (should-error (nskk-search-with-cache "not-a-cache" index "a")
                      :type 'wrong-type-argument))))
  (nskk-it "caches distinct results for different limits"
    (nskk-prolog-test-with-isolated-db
      (let* ((cache (nskk-cache-lru-create 100))
             (index (nskk-search-test--make-index
                     nil
                     '(("aa" . ("1")) ("ab" . ("2"))
                       ("ac" . ("3")) ("ad" . ("4")))))
             (limit-2 (nskk-search-with-cache cache index "a" 'prefix nil 2))
             (limit-4 (nskk-search-with-cache cache index "a" 'prefix nil 4)))
        (should (= (length limit-2) 2))
        (should (= (length limit-4) 4))
        (should (= (plist-get (nskk-cache-stats cache) :size) 2)))))
  (nskk-it "owns mutable cache keys and results across misses and hits"
    (let* ((cache (nskk-cache-lru-create 100))
           (index (make-nskk-dict-index :predicate 'cache-alias-test))
           (query (copy-sequence "mutable-query"))
           (fresh-query (copy-sequence query))
           (metadata (list (copy-sequence "metadata")))
           (text (copy-sequence "candidate"))
           (shared (cons nil nil))
           (produced (cons nil nil))
           (search-count 0))
      (add-text-properties 0 (length text)
                           (list 'nskk-test-metadata metadata)
                           text)
      (setcar shared text)
      (setcdr shared shared)
      (setcar produced shared)
      (setcdr produced shared)
      (cl-letf (((symbol-function 'nskk-search/k)
                 (lambda (_index _query _search-type _okuri-type _limit
                                  on-found _on-not-found)
                   (cl-incf search-count)
                   (funcall on-found produced))))
        (let ((miss-result
               (nskk-search-with-cache cache index query 'exact)))
          (should-not (eq miss-result produced))
          (should (= search-count 1))
          (should (= (nskk-cache-size cache) 1)))
        (aset query 0 ?X)
        (aset text 0 ?X)
        (setcar metadata "changed")
        (let ((first-hit
               (nskk-search-with-cache cache index fresh-query 'exact)))
          (should (= search-count 1))
          (should (= (nskk-cache-size cache) 1))
          (should-not (eq first-hit produced))
          (should (eq (car first-hit) (cdr first-hit)))
          (let* ((hit-shared (car first-hit))
                 (hit-text (car hit-shared))
                 (hit-metadata
                  (get-text-property 0 'nskk-test-metadata hit-text)))
            (should (eq (cdr hit-shared) hit-shared))
            (should (string= hit-text "candidate"))
            (should (equal hit-metadata '("metadata")))
            (should-not (eq hit-text text))
            (should-not (eq hit-metadata metadata))
            (aset hit-text 0 ?Y)
            (setcar hit-metadata "hit-changed"))
          (let* ((second-hit
                  (nskk-search-with-cache cache index fresh-query 'exact))
                 (hit-shared (car second-hit))
                 (hit-text (car hit-shared))
                 (hit-metadata
                  (get-text-property 0 'nskk-test-metadata hit-text)))
            (should (= search-count 1))
            (should (= (nskk-cache-size cache) 1))
            (should-not (eq second-hit first-hit))
            (should-not (eq hit-shared (car first-hit)))
            (should (eq hit-shared (cdr second-hit)))
            (should (eq (cdr hit-shared) hit-shared))
            (should (string= hit-text "candidate"))
            (should (equal hit-metadata '("metadata"))))))))
)

;;;
;;; Cache invalidation on dictionary change
;;;

(nskk-describe "nskk-search-with-cache invalidation"
  (nskk-it "returns the new candidate after nskk-jisyo-update-hook fires"
    (nskk-prolog-test-with-isolated-db
      (let* ((cache (nskk-cache-lru-create 100))
             (pred 'nskk-search-flush-test-dict)
             (index
              (nskk-search-test--make-index
               '(("かんじ" . ("旧"))) nil pred)))
        ;; Prime the cache with the current (old) candidate.
        (nskk-should-candidates
         '("旧") (nskk-search-with-cache cache index "かんじ" 'exact))
        ;; The dictionary content changes underneath the cache.
        (nskk-prolog-retract (list pred "かんじ" '("旧")))
        (nskk-prolog-assert (list (list pred "かんじ" '("新"))))
        ;; Before invalidation the stale candidate is still served from cache.
        (nskk-should-candidates
         '("旧") (nskk-search-with-cache cache index "かんじ" 'exact))
        ;; Dictionary mutation hooks flush registered search caches.
        (run-hooks 'nskk-jisyo-update-hook)
        (nskk-should-candidates
         '("新") (nskk-search-with-cache cache index "かんじ" 'exact)))))

  (nskk-it "flushes registered caches after nskk-search-learn"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let* ((cache (nskk-cache-lru-create 100))
             (pred 'nskk-search-learn-flush-test-dict)
             (index
              (nskk-search-test--make-index
               '(("かんじ" . ("漢字"))) nil pred)))
        (nskk-should-candidates
         '("漢字") (nskk-search-with-cache cache index "かんじ" 'exact))
        ;; With the registered cache still populated, changing the underlying
        ;; fact alone would leave the stale candidate in place.
        (nskk-prolog-retract (list pred "かんじ" '("漢字")))
        (nskk-prolog-assert (list (list pred "かんじ" '("新漢字"))))
        (nskk-should-candidates
         '("漢字") (nskk-search-with-cache cache index "かんじ" 'exact))
        ;; Learning a candidate flushes every registered cache.
        (nskk-search-learn "かんじ" "漢字")
        (nskk-should-candidates
         '("新漢字") (nskk-search-with-cache cache index "かんじ" 'exact))))))

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
      (nskk-search-learn "かんじ" "漢字")
      (should (= (nskk-prolog-query-value
                  '(learning-score "かんじ" "漢字" \?s) '\?s)
                 1))))

  (nskk-it "increments an existing score by 1"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 3)))
      (nskk-search-learn "かんじ" "漢字")
      (should (= (nskk-prolog-query-value
                  '(learning-score "かんじ" "漢字" \?s) '\?s)
                 4))))

  (progn
  (nskk-it "does not call public assert or retract while learning"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 3)))
      (cl-letf (((symbol-function 'nskk-prolog-assert)
                 (lambda (&rest _arguments)
                   (error "Public assert must not be called")))
                ((symbol-function 'nskk-prolog-retract)
                 (lambda (&rest _arguments)
                   (error "Public retract must not be called"))))
        (nskk-search-learn "かんじ" "漢字"))
      (should
       (equal
        (nskk-prolog-query-all-values
         '(learning-score "かんじ" "漢字" \?s) '\?s)
        '(4)))))

  (nskk-it "replaces only the first matching duplicate"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 7)))
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 7)))
      (nskk-search-learn "かんじ" "漢字")
      (should
       (equal
        (nskk-prolog-query-all-values
         '(learning-score "かんじ" "漢字" \?s) '\?s)
        '(7 8))))))

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
      (nskk-search-learn "かんじ" nil)
      ;; No learning-score fact should be created
      (should-not (nskk-prolog-query-value
                   '(learning-score "かんじ" \?c \?s) '\?s))))

  (nskk-it "retracts old score before asserting new one (no duplicates)"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 2)))
      (nskk-search-learn "かんじ" "漢字")
      ;; Only one score fact should exist for this reading/candidate pair
      (let ((all-scores (nskk-prolog-query-all-values
                         '(learning-score "かんじ" "漢字" \?s) '\?s)))
        (should (= (length all-scores) 1))
        (should (= (car all-scores) 3)))))

  ;; nskk-no-learn: built-in program dictionary candidates (AquaSKK SetAvoidStudy equiv.)
  (nskk-it "does not record learning for candidates with nskk-no-learn text property"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((no-learn-cand (propertize "2026/03/15(Sun)" 'nskk-no-learn t)))
        (nskk-search-learn "today" no-learn-cand)
        ;; No learning-score fact must be created for a no-learn candidate
        (should-not (nskk-prolog-query-value
                     '(learning-score "today" \?c \?s) '\?s)))))

  (nskk-it "still records learning for candidates WITHOUT nskk-no-learn property"
    ;; Regression guard: normal candidates continue to be learned
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((normal-cand "漢字"))  ; no text properties
        (nskk-search-learn "かんじ" normal-cand)
        (should (= (nskk-prolog-query-value
                    '(learning-score "かんじ" "漢字" \?s) '\?s)
                   1)))))

  (nskk-it "nskk-no-learn=nil is treated the same as no property (learns normally)"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((cand (propertize "漢字" 'nskk-no-learn nil)))
        (nskk-search-learn "かんじ" cand)
        ;; nskk-no-learn=nil should not prevent learning
        (should (nskk-prolog-query-value
                 '(learning-score "かんじ" "漢字" \?s) '\?s))))))

;;;
;;; Learning Data: Save / Load Tests
;;;

(defun nskk-search-test--transaction-index (index-type key)
    (pcase index-type
      (:hash (gethash key (nskk-prolog-hash-indices)))
      (:trie (gethash key (nskk-prolog-trie-indices)))))

  (defun nskk-search-test--transaction-index-bucket
      (index-type index reading)
    (pcase index-type
      (:hash (gethash reading index))
      (:trie (nskk-trie-lookup index reading))))

  (defun nskk-search-test--cons-cells (list)
    (let (cells)
      (while list
        (push list cells)
        (setq list (cdr list)))
      (nreverse cells)))

  (defun nskk-search-test--should-eq-spine (before after)
    (should (= (length before) (length after)))
    (cl-mapc
     (lambda (before-cell after-cell)
       (should (eq before-cell after-cell)))
     before after))

  (defun nskk-search-test--assert-rollback-identity
      (index-type clauses operation expected-condition)
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index
       'learning-score 3 (or index-type :list))
      (nskk-prolog-retract-all 'learning-score 3)
      (dolist (clause clauses)
        (nskk-prolog-assert clause))
      (let* ((key (nskk-prolog-clause-key 'learning-score 3))
             (database-head (gethash key (nskk-prolog-database)))
             (database-tail (gethash key (nskk-prolog-database-tails)))
             (database-cells
              (nskk-search-test--cons-cells database-head))
             (database-copy (copy-tree database-head))
             (index
              (and index-type
                   (nskk-search-test--transaction-index
                    index-type key)))
             (index-bucket
              (and index-type
                   (nskk-search-test--transaction-index-bucket
                    index-type index "query")))
             (index-cells
              (nskk-search-test--cons-cells index-bucket))
             (index-copy (copy-tree index-bucket))
             caught)
        (setq caught
              (condition-case condition
                  (progn (funcall operation) nil)
                ((error quit) condition)))
        (should (equal caught expected-condition))
        (should
         (eq database-head
             (gethash key (nskk-prolog-database))))
        (should
         (eq database-tail
             (gethash key (nskk-prolog-database-tails))))
        (nskk-search-test--should-eq-spine
         database-cells
         (nskk-search-test--cons-cells
          (gethash key (nskk-prolog-database))))
        (should
         (equal database-copy
                (gethash key (nskk-prolog-database))))
        (when index-type
          (let* ((after-index
                  (nskk-search-test--transaction-index
                   index-type key))
                 (after-bucket
                  (nskk-search-test--transaction-index-bucket
                   index-type after-index "query")))
            (should (eq index after-index))
            (should (eq index-bucket after-bucket))
            (nskk-search-test--should-eq-spine
             index-cells
             (nskk-search-test--cons-cells after-bucket))
            (should (equal index-copy after-bucket)))))))

  (nskk-describe "nskk-search-learn rollback"
  (nskk-it "restores every existing position for every index strategy"
    (let ((positions
           (list
            '(((learning-score "query" "target" 7)))
            '(((learning-score "query" "target" 7))
              ((learning-score "query" "later-a" 1))
              ((learning-score "query" "later-b" 2)))
            '(((learning-score "query" "earlier" 1))
              ((learning-score "query" "target" 7))
              ((learning-score "query" "later" 2)))
            '(((learning-score "query" "earlier-a" 1))
              ((learning-score "query" "earlier-b" 2))
              ((learning-score "query" "target" 7))))))
      (dolist (index-type '(nil :hash :trie))
        (dolist (clauses positions)
          (nskk-search-test--assert-rollback-identity
           index-type clauses
           (lambda ()
             (cl-letf
                 (((symbol-function 'nskk--search-flush-caches)
                   (lambda ()
                     (signal
                      'error
                      '("Injected cache flush failure")))))
               (nskk-search-learn "query" "target")))
           '(error "Injected cache flush failure"))))))

  (nskk-it "removes a rolled-back new clause for every index strategy"
    (let ((positions
           (list
            nil
            '(((learning-score "query" "other-a" 1))
              ((learning-score "query" "other-b" 2))))))
      (dolist (index-type '(nil :hash :trie))
        (dolist (clauses positions)
          (nskk-search-test--assert-rollback-identity
           index-type clauses
           (lambda ()
             (cl-letf
                 (((symbol-function 'nskk--search-flush-caches)
                   (lambda ()
                     (signal
                      'error
                      '("Injected new-clause failure")))))
               (nskk-search-learn "query" "target")))
           '(error "Injected new-clause failure"))))))

  (nskk-it "restores identity and re-signals quit after cache mutation"
    (nskk-search-test--assert-rollback-identity
     :hash
     '(((learning-score "query" "earlier" 1))
       ((learning-score "query" "target" 7))
       ((learning-score "query" "later" 2)))
     (lambda ()
       (cl-letf
           (((symbol-function 'nskk--search-flush-caches)
             (lambda ()
               (signal 'quit '(injected-cache-quit)))))
         (nskk-search-learn "query" "target")))
     '(quit injected-cache-quit)))

  (nskk-it "restores identity when debug logging fails"
    (nskk-search-test--assert-rollback-identity
     :trie
     '(((learning-score "query" "earlier" 1))
       ((learning-score "query" "target" 7))
       ((learning-score "query" "later" 2)))
     (lambda ()
       (cl-letf
           (((symbol-function 'nskk-debug-log)
             (lambda (&rest _arguments)
               (signal 'error '(injected-debug-failure)))))
         (nskk-search-learn "query" "target")))
     '(error injected-debug-failure)))

  (nskk-it "rolls back after puthash mutates and then signals"
  (dolist (condition '((error injected-puthash-error)
                       (quit injected-puthash-quit)))
    (let ((original-puthash (symbol-function 'puthash))
          (database-puthash-calls 0))
      (nskk-search-test--assert-rollback-identity
       :hash
       '(((learning-score "query" "earlier" 1))
         ((learning-score "query" "target" 7))
         ((learning-score "query" "later" 2)))
       (lambda ()
         (let ((database (nskk-prolog-database)))
           (cl-letf (((symbol-function 'puthash)
                      (lambda (key value table)
                        (prog1 (funcall original-puthash key value table)
                          (when (eq table database)
                            (cl-incf database-puthash-calls)
                            (when (= database-puthash-calls 1)
                              (signal (car condition)
                                      (cdr condition))))))))
             (nskk-search-learn "query" "target"))))
       condition)
      (should (> database-puthash-calls 1)))))

  (nskk-it "rolls back after an index setter mutates and then signals"
    (dolist (index-type '(:hash :trie))
      (dolist (condition '((error injected-index-setter-error)
                           (quit injected-index-setter-quit)))
        (let ((original-setter
               (symbol-function
                'nskk-prolog-transaction-set-index-bucket))
              (setter-calls 0))
          (nskk-search-test--assert-rollback-identity
           index-type
           '(((learning-score "query" "earlier" 1))
             ((learning-score "query" "target" 7))
             ((learning-score "query" "later" 2)))
           (lambda ()
             (cl-letf
                 (((symbol-function
                    'nskk-prolog-transaction-set-index-bucket)
                   (lambda (type index first-arg bucket)
                     (prog1
                         (funcall original-setter
                                  type index first-arg bucket)
                       (cl-incf setter-calls)
                       (when (= setter-calls 1)
                         (signal (car condition)
                                 (cdr condition)))))))
               (nskk-search-learn "query" "target")))
           condition)
          (should (= setter-calls 2))))))

  (nskk-it "commits with a nil callback without exposing a journal"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'learning-score 3 :hash)
      (nskk-prolog-assert
       '((learning-score "query" "target" 7)))
      (should-not
       (nskk-prolog-replace-clause-transaction
        '(learning-score "query" "target" 7)
        '((learning-score "query" "target" 8))))
      (should
       (= 8
          (nskk-prolog-query-value
           '(learning-score "query" "target" \?score)
           '\?score)))))

  (nskk-it "keeps the committed graph when a wrapper signals after helper return"
    (dolist (condition '((error injected-after-commit-error)
                         (quit injected-after-commit-quit)))
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-set-index 'learning-score 3 :hash)
        (nskk-prolog-retract-all 'learning-score 3)
        (nskk-prolog-assert
         '((learning-score "query" "target" 7)))
        (let* ((key (nskk-prolog-clause-key 'learning-score 3))
               (index (gethash key (nskk-prolog-hash-indices)))
               (old-database-cell
                (gethash key (nskk-prolog-database)))
               (old-index-cell (gethash "query" index))
               (original-helper
                (symbol-function
                 'nskk-prolog-replace-clause-transaction))
               caught)
          (cl-letf
              (((symbol-function
                 'nskk-prolog-replace-clause-transaction)
                (lambda (&rest arguments)
                  (prog1 (apply original-helper arguments)
                    (signal (car condition) (cdr condition))))))
            (setq caught
                  (condition-case signal-condition
                      (progn
                        (nskk-search-learn "query" "target")
                        nil)
                    ((error quit) signal-condition))))
          (should (equal caught condition))
          (should
           (= 8
              (nskk-prolog-query-value
               '(learning-score "query" "target" \?score)
               '\?score)))
          (let ((new-database-cell
                 (gethash key (nskk-prolog-database)))
                (new-index-cell (gethash "query" index)))
            (should-not (eq old-database-cell new-database-cell))
            (should-not (eq old-index-cell new-index-cell))
            (should (eq (car new-database-cell)
                        (car new-index-cell)))
            (should
             (eq new-database-cell
                 (gethash key (nskk-prolog-database-tails))))))))))

  (nskk-describe "nskk-search learning data persistence"
    (nskk-it "handles write errors gracefully without signaling"
      (nskk-prolog-test-with-isolated-db
        (let* ((nskk-search-learning-file
                "/nonexistent/dir/learning.dat")
               (messages nil))
          (nskk-with-mocks
              ((message
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) messages))))
            (should-not
             (condition-case _err
                 (progn
                   (nskk-search-save-learning-data)
                   nil)
               (error t)))
            (should
             (cl-some
              (lambda (message)
                (string-match-p "Failed" message))
              messages))))))

    (nskk-it "runs the save hook after the new file is published"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-retract-all 'learning-score 3)
        (nskk-prolog-assert
         '((learning-score "published-reading" "published-candidate" 7)))
        (let ((nskk-search-learning-file
               (make-temp-file "nskk-learning-published-" nil ".dat"))
              observed)
          (unwind-protect
              (let ((nskk-save-history-hook
                     (list
                      (lambda ()
                        (setq observed
                              (with-temp-buffer
                                (insert-file-contents
                                 nskk-search-learning-file)
                                (read (current-buffer))))))))
                (nskk-search-save-learning-data)
                (should
                 (member '("published-reading" "published-candidate" 7)
                         observed)))
            (when (file-exists-p nskk-search-learning-file)
              (delete-file nskk-search-learning-file))))))

    (nskk-it "reports save hook errors, continues observers, and keeps published data"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-retract-all 'learning-score 3)
        (nskk-prolog-assert
         '((learning-score "error-reading" "error-candidate" 8)))
        (let ((nskk-search-learning-file
               (make-temp-file "nskk-learning-hook-error-" nil ".dat"))
              (messages nil)
              observed)
          (unwind-protect
              (let ((nskk-save-history-hook
                     (list
                      (lambda () (error "history hook failure"))
                      (lambda ()
                        (setq observed
                              (with-temp-buffer
                                (insert-file-contents
                                 nskk-search-learning-file)
                                (read (current-buffer))))))))
                (nskk-with-mocks
                    ((message
                      (lambda (fmt &rest args)
                        (push (apply #'format fmt args) messages))))
                  (nskk-search-save-learning-data))
                (should
                 (member '("error-reading" "error-candidate" 8) observed))
                (should
                 (member
                  '("error-reading" "error-candidate" 8)
                  (with-temp-buffer
                    (insert-file-contents nskk-search-learning-file)
                    (read (current-buffer)))))
                (should
                 (cl-some
                  (lambda (text)
                    (string-match-p "save-history-hook error" text))
                  messages))
                (should-not
                 (cl-some
                  (lambda (text) (string-match-p "Failed" text))
                  messages)))
            (when (file-exists-p nskk-search-learning-file)
              (delete-file nskk-search-learning-file))))))

    (nskk-it "propagates save hook quit and stops later observers"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-retract-all 'learning-score 3)
        (nskk-prolog-assert
         '((learning-score "quit-reading" "quit-candidate" 9)))
        (let ((nskk-search-learning-file
               (make-temp-file "nskk-learning-hook-quit-" nil ".dat"))
              (condition '(quit "history hook quit" payload))
              (observer-called nil)
              caught)
          (unwind-protect
              (let ((nskk-save-history-hook
                     (list
                      (lambda ()
                        (signal (car condition) (cdr condition)))
                      (lambda () (setq observer-called t)))))
                (setq caught
                      (condition-case signal-condition
                          (progn
                            (nskk-search-save-learning-data)
                            nil)
                        (quit signal-condition)))
                (should (equal caught condition))
                (should-not observer-called)
                (should
                 (member
                  '("quit-reading" "quit-candidate" 9)
                  (with-temp-buffer
                    (insert-file-contents nskk-search-learning-file)
                    (read (current-buffer))))))
            (when (file-exists-p nskk-search-learning-file)
              (delete-file nskk-search-learning-file)))))))


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
          (should results)))))

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
(nskk-deftest-table search-candidate-word-known
  :description "nskk--search-candidate-word extracts word string"
  :columns (input expected)
  :rows (("漢字"          "漢字")
         (("漢字" . "ji") "漢字")
         (nil             nil)
         (42              nil))
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
(nskk-property-test search-cache-key-always-structural
  ((q search-query))
  (let ((key (nskk--search-cache-key
              (make-nskk-dict-index :predicate 'pbt-cache-key)
              q 'exact nil)))
    (and (proper-list-p key)
         (equal q (plist-get key :query))))
  30)

;; PBT-010 — cache key contains the query string
(nskk-property-test search-cache-key-distinguishes-dictionaries
  ((q search-query))
  (not
   (equal (nskk--search-cache-key
           (make-nskk-dict-index :predicate 'pbt-cache-key-a)
           q 'exact nil)
          (nskk--search-cache-key
           (make-nskk-dict-index :predicate 'pbt-cache-key-b)
           q 'exact nil)))
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
        (let* ((index (make-nskk-dict-index :predicate 'cache-key-format-test))
               (keys (mapcar (lambda (type)
                               (nskk--search-cache-key index q type nil))
                             '(exact prefix partial fuzzy))))
          (should (= (length keys)
                     (length (cl-remove-duplicates keys :test #'equal)))))))))

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
        (let ((results (nskk-search-fuzzy index "abc" nil)))
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
        (let ((results (nskk-search-fuzzy index "abc" nil)))
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
        (let ((results (nskk-search-fuzzy index "def" nil)))
          (should (null results))))))

  (nskk-it "returns nil when predicate is nil"
    (nskk-prolog-test-with-isolated-db
      (let ((index (make-nskk-dict-index :predicate nil)))
        (should (null (nskk-search-fuzzy index "abc" nil)))))))

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
        (nskk-search-fuzzy/k index "abc" nil
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
        (nskk-search-fuzzy/k index "abc" nil
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
        (nskk-search-fuzzy/k index "abc" nil
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
         :type 'wrong-type-argument))))

  (nskk-context "with cached falsy result"
    (nskk-it "calls on-found (not on-not-found) for cached empty-string result"
      ;; Pre-populate the cache with "" (falsy in many contexts) under a key
      ;; that matches what nskk--search-cache-key would generate.  Then call
      ;; nskk-search-with-cache/k and verify the :found path fires, not :fail.
      (nskk-with-prolog-entries ((cps-cache-falsy-empty-test "あ" ("亜")))
        (let* ((index     (make-nskk-dict-index :predicate 'cps-cache-falsy-empty-test))
               (cache     (nskk-cache-create :type 'lru :capacity 10))
               ;; Compute the same key nskk-search-with-cache uses internally.
               (cache-key (nskk--search-cache-key index "あ" 'exact nil))
               found-val
               not-found-called)
          ;; Store "" as a cached result — a falsy value that must trigger on-found.
          (nskk-cache-put cache cache-key "")
          (nskk-search-with-cache/k cache index "あ" 'exact nil nil
                                    (lambda (v) (setq found-val v))
                                    (lambda ()  (setq not-found-called t)))
          (should-not not-found-called)
          (should (string= found-val "")))))

    (nskk-it "calls on-found (not on-not-found) for cached nil result"
      ;; Same as above but the cached value is nil — the falsiest of falsy values.
      ;; nskk-cache-get/k uses key-presence testing, not value truthiness, so
      ;; on-found must still fire when nil is the stored value.
      (nskk-with-prolog-entries ((cps-cache-falsy-nil-test "い" ("以")))
        (let* ((index     (make-nskk-dict-index :predicate 'cps-cache-falsy-nil-test))
               (cache     (nskk-cache-create :type 'lru :capacity 10))
               (cache-key (nskk--search-cache-key index "い" 'exact nil))
               found-called
               not-found-called)
          (nskk-cache-put cache cache-key nil)
          (nskk-search-with-cache/k cache index "い" 'exact nil nil
                                    (lambda (_v) (setq found-called t))
                                    (lambda ()   (setq not-found-called t)))
          (should found-called)
          (should-not not-found-called)))))
  (nskk-it "copies three times on miss and once on hit"
    (let* ((cache (nskk-cache-lru-create 10))
           (index (make-nskk-dict-index :predicate 'cache-copy-count-test))
           (produced (list (copy-sequence "candidate")))
           (original-copy (symbol-function 'nskk-prolog-copy-term))
           (copy-count 0)
           (put-count 0)
           (search-count 0)
           first-result
           second-result)
      (cl-letf (((symbol-function 'nskk-search/k)
                 (lambda (_index _query _search-type _okuri-type _limit
                                  on-found _on-not-found)
                   (cl-incf search-count)
                   (funcall on-found produced)))
                ((symbol-function 'nskk-prolog-copy-term)
                 (lambda (object)
                   (cl-incf copy-count)
                   (funcall original-copy object)))
                ((symbol-function 'nskk-cache-put)
                 (lambda (target key value)
                   (cl-incf put-count)
                   (nskk-cache-lru-put target key value))))
        (nskk-search-with-cache/k
         cache index "copy-count-query" 'exact nil nil
         (lambda (result) (setq first-result result))
         (lambda () (should nil)))
        (progn (should-not (eq first-result produced)) (should (equal first-result produced)))
        (should (= copy-count 3))
        (should (= search-count 1))
        (should (= (nskk-cache-lru-size cache) 1))
        (should (= put-count 1))
        (let ((copies-after-miss copy-count))
          (nskk-search-with-cache/k
           cache index "copy-count-query" 'exact nil nil
           (lambda (result) (setq second-result result))
           (lambda () (should nil)))
          (should (= (- copy-count copies-after-miss) 1)))
        (should (= copy-count 4))
        (should (= search-count 1))
        (should (= (nskk-cache-lru-size cache) 1))
        (should (= put-count 1))
        (should-not (eq second-result produced))
        (should-not (eq second-result first-result))
        (should (equal second-result produced)))))
  (nskk-it "keeps cache unchanged when canonicalization signals error or quit"
    (dolist (case '((1 (error "copy failure" first))
                    (1 (quit copy-failure first))
                    (2 (error "copy failure" second))
                    (2 (quit copy-failure second))))
      (let* ((fault-at (car case))
             (expected (cadr case))
             (cache (nskk-cache-lru-create 10))
             (index (make-nskk-dict-index :predicate 'cache-copy-fault-test))
             (stable-key (list :stable))
             (stable-value (list (copy-sequence "stable")))
             (produced (list (copy-sequence "candidate")))
             (original-copy (symbol-function 'nskk-prolog-copy-term))
             (copy-count 0)
             (put-count 0)
             (search-count 0)
             received)
        (nskk-cache-put cache stable-key stable-value)
        (let ((stable-before (nskk-cache-get cache stable-key))
              (size-before (nskk-cache-size cache)))
          (should (eq stable-before stable-value))
          (cl-letf (((symbol-function 'nskk-search/k)
                     (lambda (_index _query _search-type _okuri-type _limit
                                      on-found _on-not-found)
                       (cl-incf search-count)
                       (funcall on-found produced)))
                    ((symbol-function 'nskk-prolog-copy-term)
                     (lambda (object)
                       (cl-incf copy-count)
                       (if (= copy-count fault-at)
                           (signal (car expected) (cdr expected))
                         (funcall original-copy object))))
                    ((symbol-function 'nskk-cache-put)
                     (lambda (&rest _args)
                       (cl-incf put-count)
                       (error "cache put called after copy failure"))))
            (setq received
                  (condition-case condition
                      (progn
                        (nskk-search-with-cache/k
                         cache index "fault-query" 'exact nil nil
                         (lambda (_result) (should nil))
                         (lambda () (should nil)))
                        nil)
                    (error condition)
                    (quit condition))))
          (should (equal received expected))
          (should (= copy-count fault-at))
          (should (= put-count 0))
          (should (= search-count 1))
          (should (= (nskk-cache-size cache) size-before))
          (let ((stable-after (nskk-cache-get cache stable-key)))
            (should (eq stable-after stable-before))
            (should (equal stable-after '("stable"))))))))
)

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

;;; Learning data persistence (save / load round-trip)

(nskk-describe "nskk-search-load-learning-data"
  (nskk-it "round-trips learning scores without duplicates on repeated loads"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-search-learning-file (make-temp-file "nskk-learning" nil ".dat")))
        (unwind-protect
            (progn
              (nskk-prolog-retract-all (quote learning-score) 3)
              (nskk-prolog-assert (list (quote (learning-score "ゆき" "優響" 3))))
              (nskk-prolog-assert (list (quote (learning-score "ゆき" "雪" 1))))
              (nskk-search-save-learning-data)
              (nskk-prolog-retract-all (quote learning-score) 3)
              (nskk-search-load-learning-data)
              (nskk-search-load-learning-data)
              (should (= 3 (nskk-prolog-query-value
                            (quote (learning-score "ゆき" "優響" \?s))
                            (quote \?s))))
              (should (= 1 (nskk-prolog-query-value
                            (quote (learning-score "ゆき" "雪" \?s))
                            (quote \?s))))
              (should (= 2 (length
                            (nskk-prolog-query
                             (quote (learning-score \?r \?c \?s)))))))
          (delete-file nskk-search-learning-file)))))

  (nskk-it "preserves existing scores when an entry is malformed"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-search-learning-file
             (make-temp-file "nskk-learning-invalid" nil ".dat")))
        (unwind-protect
            (progn
              (with-temp-file nskk-search-learning-file
                (prin1 (quote (("bad" "entry" "not-an-integer")))
                       (current-buffer)))
              (nskk-prolog-retract-all (quote learning-score) 3)
              (nskk-prolog-assert
               (quote ((learning-score "existing" "candidate" 7))))
              (nskk-search-load-learning-data)
              (should (= 7 (nskk-prolog-query-value
                            (quote (learning-score "existing" "candidate" \?s))
                            (quote \?s))))
              (should (= 1 (length
                            (nskk-prolog-query
                             (quote (learning-score \?r \?c \?s)))))))
          (delete-file nskk-search-learning-file)))))

  (nskk-it "restores existing scores when asserting a loaded fact fails"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-search-learning-file
             (make-temp-file "nskk-learning-assert" nil ".dat")))
        (unwind-protect
            (progn
              (with-temp-file nskk-search-learning-file
                (prin1 '(("new" "candidate" 1)) (current-buffer)))
              (nskk-prolog-retract-all 'learning-score 3)
              (nskk-prolog-assert
               '((learning-score "existing" "candidate" 7)))
              (let ((original-assert (symbol-function 'nskk-prolog-assert))
                    assert-attempted)
                (cl-letf (((symbol-function 'nskk-prolog-assert)
                           (lambda (clauses)
                             (if (equal clauses
                                        '((learning-score "new" "candidate" 1)))
                                 (progn
                                   (setq assert-attempted t)
                                   (error "Injected load assert failure"))
                               (funcall original-assert clauses)))))
                  (nskk-search-load-learning-data))
                (should assert-attempted))
              (should (= 7 (nskk-prolog-query-value
                            '(learning-score "existing" "candidate" \?s)
                            '\?s)))
              (should-not
               (nskk-prolog-holds-p
                '(learning-score "new" "candidate" 1)))
              (should (= 1 (length
                            (nskk-prolog-query
                             '(learning-score \?r \?c \?s))))))
          (delete-file nskk-search-learning-file)))))

  (nskk-it "preserves existing scores when the file grows past the size limit"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-search-learning-file
             (make-temp-file "nskk-learning-growing" nil ".dat"))
            (nskk--search-learning-max-file-size 8))
        (unwind-protect
            (progn
              (with-temp-file nskk-search-learning-file
                (prin1 (quote (("new" "candidate" 1))) (current-buffer)))
              (nskk-prolog-retract-all (quote learning-score) 3)
              (nskk-prolog-assert
               (quote ((learning-score "existing" "candidate" 7))))
              (cl-letf (((symbol-function (quote file-attribute-size))
                         (lambda (_attributes) 1)))
                (nskk-search-load-learning-data))
              (should (= 7 (nskk-prolog-query-value
                            (quote (learning-score "existing" "candidate" \?s))
                            (quote \?s))))
              (should (= 1 (length
                            (nskk-prolog-query
                             (quote (learning-score \?r \?c \?s)))))))
          (delete-file nskk-search-learning-file))))))
(nskk-describe "nskk-search-load-learning-data security boundaries"
  (nskk-it "rejects a symbolic link before attempting to read it"
    (nskk-prolog-test-with-isolated-db
      (let* ((directory (make-temp-file "nskk-learning-link" t))
             (target (expand-file-name "target.dat" directory))
             (nskk-search-learning-file
              (expand-file-name "learning.dat" directory))
             read-attempted)
        (unwind-protect
            (progn
              (with-temp-file target
                (prin1 '(("new" "candidate" 1)) (current-buffer)))
              (make-symbolic-link target nskk-search-learning-file)
              (nskk-prolog-assert
               '((learning-score "existing" "candidate" 7)))
              (cl-letf (((symbol-function 'insert-file-contents)
                         (lambda (&rest _)
                           (setq read-attempted t)
                           (error "Unexpected read"))))
                (nskk-search-load-learning-data))
              (should-not read-attempted)
              (should (= 7 (nskk-prolog-query-value
                            '(learning-score "existing" "candidate" \?s)
                            '\?s))))
          (delete-directory directory t)))))

  (nskk-it "rejects a FIFO without entering a blocking read"
    (unless (executable-find "mkfifo")
      (ert-skip "mkfifo is unavailable"))
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-search-learning-file
             (make-temp-file "nskk-learning-fifo" nil ".dat"))
            read-attempted)
        (unwind-protect
            (progn
              (delete-file nskk-search-learning-file)
              (should (= 0 (call-process "mkfifo" nil nil nil
                                         nskk-search-learning-file)))
              (nskk-prolog-assert
               '((learning-score "existing" "candidate" 7)))
              (cl-letf (((symbol-function 'insert-file-contents)
                         (lambda (&rest _)
                           (setq read-attempted t)
                           (error "Unexpected read"))))
                (nskk-search-load-learning-data))
              (should-not read-attempted)
              (should (= 7 (nskk-prolog-query-value
                            '(learning-score "existing" "candidate" \?s)
                            '\?s))))
          (when (file-exists-p nskk-search-learning-file)
            (delete-file nskk-search-learning-file))))))

  (nskk-it "rejects an oversized file before attempting to read it"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-search-learning-file
             (make-temp-file "nskk-learning-oversize" nil ".dat"))
            (nskk--search-learning-max-file-size 8)
            read-attempted)
        (unwind-protect
            (progn
              (with-temp-file nskk-search-learning-file
                (insert "0123456789"))
              (nskk-prolog-assert
               '((learning-score "existing" "candidate" 7)))
              (cl-letf (((symbol-function 'insert-file-contents)
                         (lambda (&rest _)
                           (setq read-attempted t)
                           (error "Unexpected read"))))
                (nskk-search-load-learning-data))
              (should-not read-attempted)
              (should (= 7 (nskk-prolog-query-value
                            '(learning-score "existing" "candidate" \?s)
                            '\?s))))
          (delete-file nskk-search-learning-file)))))

  (nskk-it "rejects atomic replacement while the file is being read"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-search-learning-file
             (make-temp-file "nskk-learning-replaced" nil ".dat"))
            (replacement
             (make-temp-file "nskk-learning-replacement" nil ".dat")))
        (unwind-protect
            (progn
              (with-temp-file nskk-search-learning-file
                (prin1 '(("staged" "candidate" 1)) (current-buffer)))
              (with-temp-file replacement
                (prin1 '(("replacement" "candidate" 2)) (current-buffer)))
              (nskk-prolog-assert
               '((learning-score "existing" "candidate" 7)))
              (let ((original-insert
                     (symbol-function 'insert-file-contents)))
                (cl-letf (((symbol-function 'insert-file-contents)
                           (lambda (&rest arguments)
                             (prog1 (apply original-insert arguments)
                               (rename-file replacement
                                            nskk-search-learning-file t)))))
                  (nskk-search-load-learning-data)))
              (should (= 7 (nskk-prolog-query-value
                            '(learning-score "existing" "candidate" \?s)
                            '\?s)))
              (should-not
               (nskk-prolog-holds-p
                '(learning-score "staged" "candidate" 1)))
              (should-not
               (nskk-prolog-holds-p
                '(learning-score "replacement" "candidate" 2))))
          (when (file-exists-p nskk-search-learning-file)
            (delete-file nskk-search-learning-file))
          (when (file-exists-p replacement)
            (delete-file replacement))))))

  (nskk-it "rejects stable-path metadata changes during the read"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-search-learning-file
             (make-temp-file "nskk-learning-changing" nil ".dat"))
            (attribute-reads 0))
        (unwind-protect
            (progn
              (with-temp-file nskk-search-learning-file
                (prin1 '(("changed" "candidate" 1)) (current-buffer)))
              (nskk-prolog-assert
               '((learning-score "existing" "candidate" 7)))
              (let ((original-attributes (symbol-function 'file-attributes))
 (expected-file (expand-file-name nskk-search-learning-file))
 (expected-resolved-file (file-truename nskk-search-learning-file)))
                (cl-letf (((symbol-function 'file-attributes)
                           (lambda (filename &optional id-format)
                             (let ((attributes
                                    (funcall original-attributes
                                             filename id-format)))
                               (when (and attributes
                                          (eq id-format 'integer)
                                          (or (equal (expand-file-name filename) expected-file)
    (equal (expand-file-name filename) expected-resolved-file))
                                          (> (cl-incf attribute-reads) 1))
                                 (setq attributes (copy-tree attributes))
                                 (setf (nth 5 attributes)
                                       (time-add (nth 5 attributes) 1)))
                               attributes))))
                  (nskk-search-load-learning-data)))
              (should (= 7 (nskk-prolog-query-value
                            '(learning-score "existing" "candidate" \?s)
                            '\?s)))
              (should-not
               (nskk-prolog-holds-p
                '(learning-score "changed" "candidate" 1))))
          (delete-file nskk-search-learning-file)))))
  (nskk-it "fails closed without source fallback or state changes when pinning fails"
    (nskk-prolog-test-with-isolated-db
      (let* ((nskk-search-learning-file
              (make-temp-file "nskk-learning-unpinnable" nil ".dat"))
             (nskk--search-registered-caches
              (make-hash-table :test 'eq :weakness 'key))
             (cache (nskk-cache-lru-create 4))
             (nskk--learning-loaded 'before-pin-failure)
             read-attempted
             diagnostic)
        (unwind-protect
            (progn
              (with-temp-file nskk-search-learning-file
                (prin1 '(("new" "candidate" 1)) (current-buffer)))
              (nskk-prolog-assert
               '((learning-score "existing" "candidate" 7)))
              (nskk-cache-lru-put cache "sentinel" 'preserved)
              (nskk-cache-lru-get cache "sentinel")
              (nskk--search-register-cache cache)
              (let* ((key (nskk-prolog-clause-key 'learning-score 3))
                     (predicate-before
                      (nskk-dict-transaction-predicate-snapshot key))
                     (cache-hash (nskk-cache-lru-hash cache))
                     (cache-head (nskk-cache-lru-head cache))
                     (cache-tail (nskk-cache-lru-tail cache))
                     (head-next (nskk-cache-lru-node-next cache-head))
                     (tail-prev (nskk-cache-lru-node-prev cache-tail))
                     (cache-node (gethash "sentinel" cache-hash))
                     (cache-size (nskk-cache-lru-size cache))
                     (cache-hits (nskk-cache-lru-hits cache))
                     (cache-misses (nskk-cache-lru-misses cache)))
                (cl-letf (((symbol-function 'add-name-to-file)
                           (lambda (&rest _arguments)
                             (signal 'file-error
                                     '("Invalid cross-device link"))))
                          ((symbol-function 'insert-file-contents)
                           (lambda (&rest _arguments)
                             (setq read-attempted t)
                             (error "Unexpected source read")))
                          ((symbol-function 'message)
                           (lambda (format-string &rest arguments)
                             (setq diagnostic
                                   (apply #'format-message
                                          format-string arguments))
                             nil)))
                  (nskk-search-load-learning-data))
                (should-not read-attempted)
                (should (stringp diagnostic))
                (should (string-match-p
                         "NSKK: Cannot safely read unpinned file"
                         diagnostic))
                (let ((predicate-after
                       (nskk-dict-transaction-predicate-snapshot key)))
                  (dotimes (index (length predicate-before))
                    (should (eq (aref predicate-before index)
                                (aref predicate-after index)))))
                (should (= 7 (nskk-prolog-query-value
                              '(learning-score "existing" "candidate" \?s)
                              '\?s)))
                (should-not
                 (nskk-prolog-holds-p
                  '(learning-score "new" "candidate" 1)))
                (should (eq cache-hash (nskk-cache-lru-hash cache)))
                (should (eq cache-head (nskk-cache-lru-head cache)))
                (should (eq cache-tail (nskk-cache-lru-tail cache)))
                (should (eq head-next
                            (nskk-cache-lru-node-next cache-head)))
                (should (eq tail-prev
                            (nskk-cache-lru-node-prev cache-tail)))
                (should (eq cache-node (gethash "sentinel" cache-hash)))
                (should (= cache-size (nskk-cache-lru-size cache)))
                (should (= cache-hits (nskk-cache-lru-hits cache)))
                (should (= cache-misses (nskk-cache-lru-misses cache)))
                (should (eq 'before-pin-failure
                            (nskk-learning-loaded)))))
          (delete-file nskk-search-learning-file)))))

  (nskk-it "disables reader evaluation despite an ambient read-eval binding"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk-search-learning-file
             (make-temp-file "nskk-learning-read-eval" nil ".dat")))
        (unwind-protect
            (progn
              (with-temp-file nskk-search-learning-file
                (insert "#.(progn (put 'nskk-search-test/read-eval "
                        "'executed t) '((\"evil\" \"candidate\" 1)))"))
              (nskk-prolog-assert
               '((learning-score "existing" "candidate" 7)))
              (let ((read-eval t))
                (nskk-search-load-learning-data))
              (should-not (get 'nskk-search-test/read-eval 'executed))
              (should (= 7 (nskk-prolog-query-value
                            '(learning-score "existing" "candidate" \?s)
                            '\?s)))
              (should-not
               (nskk-prolog-holds-p
                '(learning-score "evil" "candidate" 1))))
          (put 'nskk-search-test/read-eval 'executed nil)
          (delete-file nskk-search-learning-file)))))
  (nskk-it "rejects shared reader syntax despite an ambient read-circle binding"
    (nskk-prolog-test-with-isolated-db
      (let* ((nskk-search-learning-file
              (make-temp-file "nskk-learning-read-circle" nil ".dat"))
             (nskk--search-registered-caches
              (make-hash-table :test 'eq :weakness 'key))
             (cache (nskk-cache-lru-create 4))
             (nskk--learning-loaded 'before-shared-read))
        (unwind-protect
            (progn
              (with-temp-file nskk-search-learning-file
                (insert "(#1=(\"evil\" \"candidate\" 1) #1#)"))
              (nskk-prolog-assert
               '((learning-score "existing" "candidate" 7)))
              (nskk-cache-lru-put cache "sentinel" 'preserved)
              (nskk-cache-lru-get cache "sentinel")
              (nskk--search-register-cache cache)
              (let* ((key (nskk-prolog-clause-key 'learning-score 3))
                     (predicate-before
                      (nskk-dict-transaction-predicate-snapshot key))
                     (cache-hash (nskk-cache-lru-hash cache))
                     (cache-head (nskk-cache-lru-head cache))
                     (cache-tail (nskk-cache-lru-tail cache))
                     (head-next (nskk-cache-lru-node-next cache-head))
                     (tail-prev (nskk-cache-lru-node-prev cache-tail))
                     (cache-node (gethash "sentinel" cache-hash))
                     (cache-size (nskk-cache-lru-size cache))
                     (cache-hits (nskk-cache-lru-hits cache))
                     (cache-misses (nskk-cache-lru-misses cache))
                     (read-circle t))
                (nskk-search-load-learning-data)
                (let ((predicate-after
                       (nskk-dict-transaction-predicate-snapshot key)))
                  (dotimes (index (length predicate-before))
                    (should (eq (aref predicate-before index)
                                (aref predicate-after index)))))
                (should (= 7 (nskk-prolog-query-value
                              '(learning-score "existing" "candidate" \?s)
                              '\?s)))
                (should-not
                 (nskk-prolog-holds-p
                  '(learning-score "evil" "candidate" 1)))
                (should (eq cache-hash (nskk-cache-lru-hash cache)))
                (should (eq cache-head (nskk-cache-lru-head cache)))
                (should (eq cache-tail (nskk-cache-lru-tail cache)))
                (should (eq head-next
                            (nskk-cache-lru-node-next cache-head)))
                (should (eq tail-prev
                            (nskk-cache-lru-node-prev cache-tail)))
                (should (eq cache-node (gethash "sentinel" cache-hash)))
                (should (= cache-size (nskk-cache-lru-size cache)))
                (should (= cache-hits (nskk-cache-lru-hits cache)))
                (should (= cache-misses (nskk-cache-lru-misses cache)))
                (should (eq 'before-shared-read (nskk-learning-loaded)))))
          (delete-file nskk-search-learning-file)))))

  (nskk-it "rolls back exact publication state on error and quit"
    (let ((nskk-search-learning-file
           (make-temp-file "nskk-learning-publication" nil ".dat"))
          (loaded-original-value (nskk-learning-loaded)))
      (unwind-protect
          (progn
            (with-temp-file nskk-search-learning-file
              (prin1 '(("new" "candidate" 1)) (current-buffer)))
            (dolist (fault '(error quit))
              (nskk-prolog-test-with-isolated-db
                (let* ((nskk--search-registered-caches
                        (make-hash-table :test 'eq :weakness 'key))
                       (cache (nskk-cache-lru-create 4)))
                  (nskk-prolog-assert
                   '((learning-score "existing" "candidate" 7)))
                  (nskk-cache-lru-put cache "a" 1)
                  (nskk-cache-lru-put cache "b" 2)
                  (nskk-cache-lru-get cache "a")
                  (nskk--search-register-cache cache)
                  (nskk-set-learning-loaded
                   (if (eq fault 'error)
                       'before-publication-error
                     'before-publication-quit))
                  (let* ((key (nskk-prolog-clause-key 'learning-score 3))
                         (predicate-before
                          (nskk-dict-transaction-predicate-snapshot key))
                         (loaded-value (nskk-learning-loaded))
                         (cache-hash (nskk-cache-lru-hash cache))
                         (cache-head (nskk-cache-lru-head cache))
                         (cache-tail (nskk-cache-lru-tail cache))
                         (head-next
                          (nskk-cache-lru-node-next cache-head))
                         (tail-prev
                          (nskk-cache-lru-node-prev cache-tail))
                         (node-a (gethash "a" cache-hash))
                         (node-b (gethash "b" cache-hash))
                         (cache-size (nskk-cache-lru-size cache))
                         (cache-hits (nskk-cache-lru-hits cache))
                         (cache-misses (nskk-cache-lru-misses cache))
                         (original-flush
                          (symbol-function 'nskk--search-flush-caches)))
                    (cl-letf (((symbol-function 'nskk--search-flush-caches)
                               (lambda ()
                                 (funcall original-flush)
                                 (nskk-set-learning-loaded
                                  'during-publication)
                                 (signal fault
                                         '("Injected publication fault")))))
                      (if (eq fault 'error)
                          (nskk-search-load-learning-data)
                        (should
                         (eq 'quit
                             (condition-case nil
                                 (progn
                                   (nskk-search-load-learning-data)
                                   'returned)
                               (quit 'quit))))))
                    (let ((predicate-after
                           (nskk-dict-transaction-predicate-snapshot key)))
                      (dotimes (index (length predicate-before))
                        (should (eq (aref predicate-before index)
                                    (aref predicate-after index)))))
                    (should (= 7 (nskk-prolog-query-value
                                  '(learning-score
                                    "existing" "candidate" \?s)
                                  '\?s)))
                    (should-not
                     (nskk-prolog-holds-p
                      '(learning-score "new" "candidate" 1)))
                    (should (eq cache-hash (nskk-cache-lru-hash cache)))
                    (should (eq cache-head (nskk-cache-lru-head cache)))
                    (should (eq cache-tail (nskk-cache-lru-tail cache)))
                    (should (eq head-next
                                (nskk-cache-lru-node-next cache-head)))
                    (should (eq tail-prev
                                (nskk-cache-lru-node-prev cache-tail)))
                    (should (eq node-a (gethash "a" cache-hash)))
                    (should (eq node-b (gethash "b" cache-hash)))
                    (should (= cache-size (nskk-cache-lru-size cache)))
                    (should (= cache-hits (nskk-cache-lru-hits cache)))
                    (should (= cache-misses
                               (nskk-cache-lru-misses cache)))
                    (should (eq loaded-value (nskk-learning-loaded))))))))
        (nskk-set-learning-loaded loaded-original-value)
        (delete-file nskk-search-learning-file)))))


(nskk-describe "bounded Levenshtein distance"
    (nskk-it "matches exact distance at and below the boundary"
      (dolist (case (quote (("abc" "abc" 0)
                            ("abc" "axc" 1)
                            ("kitten" "sitting" 3)
                            ("かんじ" "かんき" 1))))
        (let ((actual (nskk--search-levenshtein-distance
                       (nth 0 case) (nth 1 case)))
              (bound (nth 2 case)))
          (should (= actual bound))
          (should (= actual
                     (nskk--search-levenshtein-distance-bounded
                      (nth 0 case) (nth 1 case) bound))))))

    (nskk-it "returns the sentinel above the boundary"
      (should (= 2 (nskk--search-levenshtein-distance-bounded
                    "kitten" "sitting" 1)))
      (should (= 3 (nskk--search-levenshtein-distance-bounded
                    "" "abcdef" 2)))
      (should (= 1 (nskk--search-levenshtein-distance-bounded
                    "abc" "abd" 0))))

    (nskk-it "agrees with exact distance for every sufficient bound"
      (dolist (pair (quote (("" "")
                             ("a" "")
                             ("abc" "yabd")
                             ("Saturday" "Sunday")
                             ("にほんご" "にほん"))))
        (let* ((exact (nskk--search-levenshtein-distance
                       (car pair) (cadr pair)))
               (bounded (nskk--search-levenshtein-distance-bounded
                         (car pair) (cadr pair) exact)))
          (should (= exact bounded))))))

  (nskk-describe "search performance regressions"
    (nskk-it "sorts the full result set before applying limit"
      (let* ((nskk-search-sort-method 'kana)
             (entries (mapcar
                       (lambda (key)
                         (cons key (make-nskk-dict-entry
                                    :key key :candidates (list key))))
                       '("う" "あ" "い")))
             (processed (nskk--search-post-process-results entries nil 2)))
        (should (equal '("あ" "い") (mapcar #'car processed)))))

    (nskk-it "constructs entries only for partial-search matches"
      (nskk-prolog-test-with-isolated-db
        (let* ((index (nskk-search-test--make-index
                       '(("miss-a" . ("A"))
                         ("target-hit" . ("B"))
                         ("miss-c" . ("C")))))
               (constructor (symbol-function 'make-nskk-dict-entry))
               (calls 0))
          (cl-letf (((symbol-function 'make-nskk-dict-entry)
                     (lambda (&rest arguments)
                       (cl-incf calls)
                       (apply constructor arguments))))
            (should (= 1 (length (nskk-search-partial index "target" nil nil))))
            (should (= 1 calls)))))))

  (nskk-describe "search cache invalidation after learning load"
    (nskk-it "flushes registered caches after a successful load"
      (nskk-prolog-test-with-isolated-db
        (let ((nskk-search-learning-file
               (make-temp-file "nskk-learning-valid" nil ".dat"))
              (flushes 0))
          (unwind-protect
              (progn
                (with-temp-file nskk-search-learning-file
                  (prin1 '(("reading" "candidate" 1)) (current-buffer)))
                (cl-letf (((symbol-function 'nskk--search-flush-caches)
                           (lambda () (cl-incf flushes))))
                  (nskk-search-load-learning-data))
                (should (= 1 flushes)))
            (delete-file nskk-search-learning-file)))))

    (nskk-it "does not flush caches when validation fails"
      (nskk-prolog-test-with-isolated-db
        (let ((nskk-search-learning-file
               (make-temp-file "nskk-learning-invalid" nil ".dat"))
              (flushes 0))
          (unwind-protect
              (progn
                (with-temp-file nskk-search-learning-file
                  (prin1 '(("reading" "candidate" invalid)) (current-buffer)))
                (cl-letf (((symbol-function 'nskk--search-flush-caches)
                           (lambda () (cl-incf flushes))))
                  (nskk-search-load-learning-data))
                (should (= 0 flushes)))
            (delete-file nskk-search-learning-file))))))

  (nskk-describe "bounded Levenshtein exhaustive regression"
    (nskk-it "matches exact distance or the boundary sentinel"
      (let ((strings '("" "a" "b" "aa" "ab" "ba" "bb")))
        (dolist (left strings)
          (dolist (right strings)
            (dotimes (bound 4)
              (let* ((exact (nskk--search-levenshtein-distance left right))
                     (expected (if (<= exact bound) exact (1+ bound))))
                (should
                 (= expected
                    (nskk--search-levenshtein-distance-bounded
                     left right bound))))))))))

  (progn
  (defun nskk-search-test--cache-state-asserter (cache)
    "Return a closure that asserts CACHE retains its physical state."
    (cond
     ((nskk-cache-lru-p cache)
      (let* ((capacity (nskk-cache-lru-capacity cache))
             (size (nskk-cache-lru-size cache))
             (hash (nskk-cache-lru-hash cache))
             (head (nskk-cache-lru-head cache))
             (tail (nskk-cache-lru-tail cache))
             (hits (nskk-cache-lru-hits cache))
             (misses (nskk-cache-lru-misses cache))
             (head-state
              (vector (nskk-cache-lru-node-key head)
                      (nskk-cache-lru-node-value head)
                      (nskk-cache-lru-node-prev head)
                      (nskk-cache-lru-node-next head)))
             (tail-state
              (vector (nskk-cache-lru-node-key tail)
                      (nskk-cache-lru-node-value tail)
                      (nskk-cache-lru-node-prev tail)
                      (nskk-cache-lru-node-next tail)))
             rows)
        (maphash
         (lambda (key node)
           (push (vector key node
                         (nskk-cache-lru-node-key node)
                         (nskk-cache-lru-node-value node)
                         (nskk-cache-lru-node-prev node)
                         (nskk-cache-lru-node-next node))
                 rows))
         hash)
        (lambda (&optional hit-delta miss-delta)
          (should (= (nskk-cache-lru-capacity cache) capacity))
          (should (= (nskk-cache-lru-size cache) size))
          (should (eq (nskk-cache-lru-hash cache) hash))
          (should (eq (nskk-cache-lru-head cache) head))
          (should (eq (nskk-cache-lru-tail cache) tail))
          (should (= (nskk-cache-lru-hits cache)
                     (+ hits (or hit-delta 0))))
          (should (= (nskk-cache-lru-misses cache)
                     (+ misses (or miss-delta 0))))
          (should (= (hash-table-count hash) (length rows)))
          (should (eq (nskk-cache-lru-node-key head) (aref head-state 0)))
          (should (eq (nskk-cache-lru-node-value head) (aref head-state 1)))
          (should (eq (nskk-cache-lru-node-prev head) (aref head-state 2)))
          (should (eq (nskk-cache-lru-node-next head) (aref head-state 3)))
          (should (eq (nskk-cache-lru-node-key tail) (aref tail-state 0)))
          (should (eq (nskk-cache-lru-node-value tail) (aref tail-state 1)))
          (should (eq (nskk-cache-lru-node-prev tail) (aref tail-state 2)))
          (should (eq (nskk-cache-lru-node-next tail) (aref tail-state 3)))
          (dolist (row rows)
            (let ((node (aref row 1)))
              (should (eq (gethash (aref row 0) hash) node))
              (should (eq (nskk-cache-lru-node-key node) (aref row 2)))
              (should (eq (nskk-cache-lru-node-value node) (aref row 3)))
              (should (eq (nskk-cache-lru-node-prev node) (aref row 4)))
              (should (eq (nskk-cache-lru-node-next node) (aref row 5))))))))
     ((nskk-cache-lfu-p cache)
      (let* ((capacity (nskk-cache-lfu-capacity cache))
             (size (nskk-cache-lfu-size cache))
             (hash (nskk-cache-lfu-hash cache))
             (freq (nskk-cache-lfu-freq cache))
             (min-freq (nskk-cache-lfu-min-freq cache))
             (hits (nskk-cache-lfu-hits cache))
             (misses (nskk-cache-lfu-misses cache))
             entry-rows
             freq-rows)
        (maphash
         (lambda (key entry)
           (push (vector key entry
                         (nskk-cache-lfu-entry-key entry)
                         (nskk-cache-lfu-entry-value entry)
                         (nskk-cache-lfu-entry-frequency entry))
                 entry-rows))
         hash)
        (maphash
         (lambda (frequency bucket)
           (push (vector frequency bucket) freq-rows))
         freq)
        (lambda (&optional hit-delta miss-delta)
          (should (= (nskk-cache-lfu-capacity cache) capacity))
          (should (= (nskk-cache-lfu-size cache) size))
          (should (eq (nskk-cache-lfu-hash cache) hash))
          (should (eq (nskk-cache-lfu-freq cache) freq))
          (should (= (nskk-cache-lfu-min-freq cache) min-freq))
          (should (= (nskk-cache-lfu-hits cache)
                     (+ hits (or hit-delta 0))))
          (should (= (nskk-cache-lfu-misses cache)
                     (+ misses (or miss-delta 0))))
          (should (= (hash-table-count hash) (length entry-rows)))
          (should (= (hash-table-count freq) (length freq-rows)))
          (dolist (row entry-rows)
            (let ((entry (aref row 1)))
              (should (eq (gethash (aref row 0) hash) entry))
              (should (eq (nskk-cache-lfu-entry-key entry) (aref row 2)))
              (should (eq (nskk-cache-lfu-entry-value entry) (aref row 3)))
              (should (= (nskk-cache-lfu-entry-frequency entry) (aref row 4)))))
          (dolist (row freq-rows)
            (should (eq (gethash (aref row 0) freq) (aref row 1)))))))
     (t
      (error "Unsupported cache type: %S" cache))))

  (defun nskk-search-test--make-supported-cache-value ()
    "Build a cyclic value spanning every supported mutable container."
    (let* ((shared (list (copy-sequence "shared")))
           (cycle (cons shared nil))
           (text (copy-sequence "text"))
           (bits (bool-vector t nil t))
           (vector (vector shared bits nil))
           (record (make-nskk-dict-entry
                    :key text :candidates shared :okuri vector))
           (root (list cycle cycle text bits vector record shared)))
      (setcdr cycle cycle)
      (aset vector 2 record)
      (put-text-property 0 1 'nskk-search-adversarial-property vector text)
      root))

  (defun nskk-search-test--assert-supported-cache-copy (copy original)
    "Assert COPY is a detached topology-preserving copy of ORIGINAL."
    (let* ((copy-cycle (nth 0 copy))
           (copy-text (nth 2 copy))
           (copy-bits (nth 3 copy))
           (copy-vector (nth 4 copy))
           (copy-record (nth 5 copy))
           (copy-shared (nth 6 copy))
           (original-cycle (nth 0 original))
           (original-text (nth 2 original))
           (original-bits (nth 3 original))
           (original-vector (nth 4 original))
           (original-record (nth 5 original))
           (original-shared (nth 6 original)))
      (should-not (eq copy original))
      (should (eq copy-cycle (nth 1 copy)))
      (should (eq (cdr copy-cycle) copy-cycle))
      (should (eq (car copy-cycle) copy-shared))
      (should (eq (aref copy-vector 0) copy-shared))
      (should (eq (aref copy-vector 1) copy-bits))
      (should (eq (aref copy-vector 2) copy-record))
      (should (eq (nskk-dict-entry-key copy-record) copy-text))
      (should (eq (nskk-dict-entry-candidates copy-record) copy-shared))
      (should (eq (nskk-dict-entry-okuri copy-record) copy-vector))
      (should (eq (get-text-property
                   0 'nskk-search-adversarial-property copy-text)
                  copy-vector))
      (should (string= copy-text original-text))
      (should (string= (car copy-shared) (car original-shared)))
      (should (equal copy-bits original-bits))
      (should-not (eq copy-cycle original-cycle))
      (should-not (eq copy-text original-text))
      (should-not (eq copy-bits original-bits))
      (should-not (eq copy-vector original-vector))
      (should-not (eq copy-record original-record))
      (should-not (eq copy-shared original-shared))
      (should-not (eq (car copy-shared) (car original-shared)))))

  (defun nskk-search-test--only-cache-value (cache)
    "Return the value of CACHE's sole entry without recording a hit."
    (let (value)
      (maphash
       (lambda (_key entry)
         (setq value
               (if (nskk-cache-lru-p cache)
                   (nskk-cache-lru-node-value entry)
                 (nskk-cache-lfu-entry-value entry))))
       (if (nskk-cache-lru-p cache)
           (nskk-cache-lru-hash cache)
         (nskk-cache-lfu-hash cache)))
      value))

  (defun nskk-search-test--unsupported-cache-value (shape)
    "Build an unsupported cache value for SHAPE."
    (let ((table (make-hash-table :test (function eq))))
      (puthash 'sentinel 'value table)
      (pcase shape
        ('top table)
        ('nested (list :outer (vector table)))
        ('text-property
         (let ((text (copy-sequence "property")))
           (put-text-property
            0 1 'nskk-search-adversarial-property table text)
           text))
        ('cycle
         (let ((cycle (cons table nil)))
           (setcdr cycle cycle)
           cycle))
        (_ (error "Unsupported shape: %S" shape)))))

  (ert-deftest nskk-search-adversarial-supported-graph-is-fresh-on-every-hit ()
    (dolist (strategy '(lru lfu))
      (let* ((cache (nskk-cache-create :type strategy :capacity 8))
             (index (make-nskk-dict-index
                     :predicate 'nskk-search-adversarial-supported))
             (source (nskk-search-test--make-supported-cache-value))
             (original-put (symbol-function 'nskk-cache-put))
             (search-count 0)
             (put-count 0)
             (callback-count 0)
             miss-result
             first-hit
             second-hit)
        (cl-letf (((symbol-function 'nskk-search/k)
                   (lambda (_index _query _search-type _okuri-type _limit
                                    on-found _on-not-found)
                     (cl-incf search-count)
                     (funcall on-found source)))
                  ((symbol-function 'nskk-cache-put)
                   (lambda (&rest arguments)
                     (cl-incf put-count)
                     (apply original-put arguments))))
          (nskk-search-with-cache/k
           cache index "supported" 'exact nil nil
           (lambda (result)
             (cl-incf callback-count)
             (setq miss-result result))
           (lambda () (should nil)))
          (nskk-search-with-cache/k
           cache index "supported" 'exact nil nil
           (lambda (result)
             (cl-incf callback-count)
             (setq first-hit result))
           (lambda () (should nil)))
          (nskk-search-with-cache/k
           cache index "supported" 'exact nil nil
           (lambda (result)
             (cl-incf callback-count)
             (setq second-hit result))
           (lambda () (should nil))))
        (let ((stored (nskk-search-test--only-cache-value cache))
              (stats (nskk-cache-stats cache)))
          (nskk-search-test--assert-supported-cache-copy miss-result stored)
          (nskk-search-test--assert-supported-cache-copy stored source)
          (nskk-search-test--assert-supported-cache-copy first-hit stored)
          (nskk-search-test--assert-supported-cache-copy second-hit stored)
          (should-not (eq first-hit second-hit))
          (aset (car (nth 6 first-hit)) 0 ?X)
          (aset (nth 2 first-hit) 0 ?X)
          (aset (nth 4 first-hit) 0 :mutated)
          (should (string= (car (nth 6 second-hit)) "shared"))
          (should (string= (nth 2 second-hit) "text"))
          (should (eq (aref (nth 4 second-hit) 0) (nth 6 second-hit)))
          (should (string= (car (nth 6 stored)) "shared"))
          (should (string= (nth 2 stored) "text"))
          (should (string= (car (nth 6 source)) "shared"))
          (should (string= (nth 2 source) "text"))
          (should (= search-count 1))
          (should (= put-count 1))
          (should (= callback-count 3))
          (should (= (plist-get stats :size) 1))
          (should (= (plist-get stats :hits) 2))
          (should (= (plist-get stats :misses) 1))))))

  (ert-deftest nskk-search-adversarial-hit-copy-fault-is-atomic-and-retryable ()
    (dolist (strategy '(lru lfu))
      (dolist (condition-type '(error quit))
        (let* ((cache (nskk-cache-create :type strategy :capacity 8))
               (index (make-nskk-dict-index
                       :predicate 'nskk-search-adversarial-hit-fault))
               (cache-key
                (nskk--search-cache-key index "hit-fault" 'exact nil nil))
               (cached (nskk-search-test--make-supported-cache-value))
               (original-copy (symbol-function 'nskk-prolog-copy-term))
               (search-count 0)
               (put-count 0)
               (callback-count 0)
               retry-result)
          (nskk-cache-put cache cache-key cached)
          (nskk-cache-put cache (list :other strategy condition-type) :stable)
          (let ((assert-state
                 (nskk-search-test--cache-state-asserter cache)))
            (cl-letf (((symbol-function 'nskk-search/k)
                       (lambda (&rest _arguments)
                         (cl-incf search-count)
                         (error "search called on cache hit")))
                      ((symbol-function 'nskk-cache-put)
                       (lambda (&rest _arguments)
                         (cl-incf put-count)
                         (error "put called on cache hit"))))
              (dotimes (_ 3)
                (let (received)
                  (cl-letf (((symbol-function 'nskk-prolog-copy-term)
                             (lambda (object)
                               (if (eq object cached)
                                   (signal condition-type '(injected-hit-copy-fault))
                                 (funcall original-copy object)))))
                    (setq received
                          (condition-case condition
                              (progn
                                (nskk-search-with-cache/k
                                 cache index "hit-fault" 'exact nil nil
                                 (lambda (_result) (cl-incf callback-count))
                                 (lambda () (cl-incf callback-count)))
                                nil)
                            (error condition)
                            (quit condition))))
                  (should (eq (car received) condition-type))
                  (should (= callback-count 0))
                  (funcall assert-state 0 0)))
              (nskk-search-with-cache/k
               cache index "hit-fault" 'exact nil nil
               (lambda (result)
                 (cl-incf callback-count)
                 (setq retry-result result))
               (lambda () (cl-incf callback-count))))
            (should (= search-count 0))
            (should (= put-count 0))
            (should (= callback-count 1))
            (nskk-search-test--assert-supported-cache-copy retry-result cached)
            (let ((stats (nskk-cache-stats cache)))
              (should (= (plist-get stats :hits) 1))
              (should (= (plist-get stats :misses) 0))))))))

  (ert-deftest nskk-search-adversarial-miss-copy-fault-is-atomic-and-retryable () (dolist (strategy (quote (lru lfu))) (dolist (condition-type (quote (error quit))) (dolist (fault-at (quote (1 2))) (let* ((cache (nskk-cache-create :type strategy :capacity 8)) (index (make-nskk-dict-index :predicate (quote nskk-search-adversarial-miss-fault))) (produced (nskk-search-test--make-supported-cache-value)) (assert-state (nskk-search-test--cache-state-asserter cache)) (original-copy (symbol-function (quote nskk-prolog-copy-term))) (original-put (symbol-function (quote nskk-cache-put))) (search-count 0) (copy-count 0) (put-count 0) (callback-count 0) received retry-result) (cl-letf (((symbol-function (quote nskk-search/k)) (lambda (_index _query _search-type _okuri-type _limit on-found _on-not-found) (cl-incf search-count) (funcall on-found produced))) ((symbol-function (quote nskk-prolog-copy-term)) (lambda (object) (cl-incf copy-count) (if (= copy-count fault-at) (signal condition-type (quote (injected-miss-copy-fault))) (funcall original-copy object)))) ((symbol-function (quote nskk-cache-put)) (lambda (&rest arguments) (cl-incf put-count) (apply original-put arguments)))) (setq received (condition-case condition (progn (nskk-search-with-cache/k cache index "miss-fault" (quote exact) nil nil (lambda (_result) (cl-incf callback-count)) (lambda () (cl-incf callback-count))) nil) (error condition) (quit condition)))) (should (eq (car received) condition-type)) (should (= search-count 1)) (should (= copy-count fault-at)) (should (= put-count 0)) (should (= callback-count 0)) (funcall assert-state 0 1) (cl-letf (((symbol-function (quote nskk-search/k)) (lambda (_index _query _search-type _okuri-type _limit on-found _on-not-found) (cl-incf search-count) (funcall on-found produced))) ((symbol-function (quote nskk-cache-put)) (lambda (&rest arguments) (cl-incf put-count) (apply original-put arguments)))) (nskk-search-with-cache/k cache index "miss-fault" (quote exact) nil nil (lambda (result) (cl-incf callback-count) (setq retry-result result)) (lambda () (cl-incf callback-count)))) (should (= search-count 2)) (should (= put-count 1)) (should (= callback-count 1)) (let ((stored (nskk-search-test--only-cache-value cache)) (stats (nskk-cache-stats cache))) (nskk-search-test--assert-supported-cache-copy stored produced) (nskk-search-test--assert-supported-cache-copy retry-result stored) (should (= (plist-get stats :hits) 0)) (should (= (plist-get stats :misses) 2)) (should (= (plist-get stats :size) 1))))))))

  (ert-deftest nskk-search-adversarial-hash-rejection-precedes-publication ()
    (dolist (strategy '(lru lfu))
      (dolist (phase '(hit miss))
        (dolist (shape '(top nested text-property cycle))
          (let* ((cache (nskk-cache-create :type strategy :capacity 8))
                 (index (make-nskk-dict-index
                         :predicate 'nskk-search-adversarial-hash))
                 (query (format "hash-%s-%s" phase shape))
                 (cache-key
                  (nskk--search-cache-key index query 'exact nil nil))
                 (unsupported
                  (nskk-search-test--unsupported-cache-value shape))
                 (original-copy (symbol-function 'nskk-prolog-copy-term))
                 (search-count 0)
                 (copy-count 0)
                 (put-count 0)
                 (callback-count 0)
                 received)
            (when (eq phase 'hit)
              (nskk-cache-put cache cache-key unsupported))
            (let ((assert-state
                   (nskk-search-test--cache-state-asserter cache)))
              (cl-letf (((symbol-function 'nskk-search/k)
                         (lambda (_index _query _search-type _okuri-type _limit
                                          on-found _on-not-found)
                           (cl-incf search-count)
                           (funcall on-found unsupported)))
                        ((symbol-function 'nskk-prolog-copy-term)
                         (lambda (object)
                           (cl-incf copy-count)
                           (funcall original-copy object)))
                        ((symbol-function 'nskk-cache-put)
                         (lambda (&rest _arguments)
                           (cl-incf put-count)
                           (error "put called for unsupported value"))))
                (setq received
                      (condition-case condition
                          (progn
                            (nskk-search-with-cache/k
                             cache index query 'exact nil nil
                             (lambda (_result) (cl-incf callback-count))
                             (lambda () (cl-incf callback-count)))
                            nil)
                        (error condition)
                        (quit condition))))
              (should (eq (car received) 'wrong-type-argument))
              (should (= search-count (if (eq phase 'miss) 1 0)))
              (should (= copy-count 0))
              (should (= put-count 0))
              (should (= callback-count 0))
              (funcall assert-state 0 (if (eq phase 'miss) 1 0))))))))

  (provide (quote nskk-search-test)))

;;; nskk-search-test.el ends here
