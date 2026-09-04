;;; nskk-search-test.el --- Tests for nskk-search.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-search.el.

;;; Code:

(require 'ert)
(require 'nskk-search)
(require 'nskk-dictionary)
(require 'nskk-prolog)
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
;;; Okuri-type filter tests
;;;

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

  (nskk-it "prefix filter separates okuri-ari and okuri-nasi entries"
    (nskk-prolog-test-with-isolated-db
      (let ((index (nskk-search-test--make-index
                    nil
                    '(("うごk" . ("動")) ("うごく" . ("動く"))))))
        (let ((ari (nskk-search-prefix index "うご" 'okuri-ari nil)))
          (should (assoc "うごk" ari))
          (should-not (assoc "うごく" ari)))
        (let ((nasi (nskk-search-prefix index "うご" 'okuri-nasi nil)))
          (should (assoc "うごく" nasi))
          (should-not (assoc "うごk" nasi)))))))

;;;
;;; nskk-search empty/nil handling
;;;

(nskk-describe "nskk-search empty/nil handling"
  (nskk-context "nskk--search-dedup (ordinary: first-wins)"
    (nskk-it "returns all entries when there are no duplicates"
      (let ((results '(("a" . 1) ("b" . 2) ("c" . 3))))
        (let ((unique (nskk--search-dedup results)))
          (should (= (length unique) 3)))))

    (nskk-it "deduplicates keeping the first occurrence"
      (let ((results '(("a" . 1) ("b" . 2) ("a" . 3) ("c" . 4) ("b" . 5))))
        (let ((unique (nskk--search-dedup results)))
          (should (= (length unique) 3))
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
      (should-not (nskk-prolog-query-value
                   '(learning-score "かんじ" \?c \?s) '\?s))))

  (nskk-it "retracts old score before asserting new one (no duplicates)"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (nskk-prolog-assert '((learning-score "かんじ" "漢字" 2)))
      (nskk-search-learn "かんじ" "漢字")
      (let ((all-scores (nskk-prolog-query-all-values
                         '(learning-score "かんじ" "漢字" \?s) '\?s)))
        (should (= (length all-scores) 1))
        (should (= (car all-scores) 3)))))

  (nskk-it "does not record learning for candidates with nskk-no-learn text property"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-retract-all 'learning-score 3)
      (let ((no-learn-cand (propertize "2026/03/15(Sun)" 'nskk-no-learn t)))
        (nskk-search-learn "today" no-learn-cand)
        (should-not (nskk-prolog-query-value
                     '(learning-score "today" \?c \?s) '\?s)))))

  (nskk-it "still records learning for candidates WITHOUT nskk-no-learn property"
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
                 (((symbol-function 'nskk-debug-log)
                   (lambda (&rest _arguments)
                     (signal 'error '("Injected commit-callback failure")))))
               (nskk-search-learn "query" "target")))
           '(error "Injected commit-callback failure"))))))

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
                 (((symbol-function 'nskk-debug-log)
                   (lambda (&rest _arguments)
                     (signal 'error '("Injected new-clause failure")))))
               (nskk-search-learn "query" "target")))
           '(error "Injected new-clause failure"))))))

  (nskk-it "restores identity and re-signals quit from the commit callback"
    (nskk-search-test--assert-rollback-identity
     :hash
     '(((learning-score "query" "earlier" 1))
       ((learning-score "query" "target" 7))
       ((learning-score "query" "later" 2)))
     (lambda ()
       (cl-letf
           (((symbol-function 'nskk-debug-log)
             (lambda (&rest _arguments)
               (signal 'quit '(injected-commit-quit)))))
         (nskk-search-learn "query" "target")))
     '(quit injected-commit-quit)))

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

(nskk-deftest-table search-candidate-word-known
  :description "nskk--search-candidate-word extracts word string"
  :columns (input expected)
  :rows (("漢字"          "漢字")
         (("漢字" . "ji") "漢字")
         (nil             nil)
         (42              nil))
  :body (should (equal expected (nskk--search-candidate-word input))))

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
      (cl-loop for (prev . rest) on keys
               while rest
               always (not (string< (car rest) prev))))
    40
    31))

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
;;; Direct CPS variant tests
;;;

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

;;;
;;; PBT: post-process pipeline invariants
;;;

(nskk-property-test-seeded search-post-process-no-duplicates
  ((a search-query)
   (b search-query))
  (let* ((e1 (make-nskk-dict-entry :key a :candidates (list a)))
         (e2 (make-nskk-dict-entry :key b :candidates (list b)))
         (results (list (cons a e1) (cons b e2) (cons a e1)))
         (processed (nskk--search-post-process-results results nil nil))
         (keys (mapcar #'car processed)))
    (= (length keys)
       (length (cl-remove-duplicates keys :test #'equal))))
  40
  37)

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
             (loaded-before 'before-shared-read))
        (nskk-set-learning-loaded loaded-before)
        (unwind-protect
            (progn
              (with-temp-file nskk-search-learning-file
                (insert "(#1=(\"evil\" \"candidate\" 1) #1#)"))
              (nskk-prolog-assert
               '((learning-score "existing" "candidate" 7)))
              (let* ((key (nskk-prolog-clause-key 'learning-score 3))
                     (predicate-before
                      (nskk-dict-transaction-predicate-snapshot key))
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
                (should (eq loaded-before (nskk-learning-loaded)))))
          (delete-file nskk-search-learning-file)))))

  (nskk-it "fails closed without source fallback or state changes when pinning fails"
    (nskk-prolog-test-with-isolated-db
      (let* ((nskk-search-learning-file
              (make-temp-file "nskk-learning-unpinnable" nil ".dat"))
             (loaded-before 'before-pin-failure)
             read-attempted
             diagnostic)
        (nskk-set-learning-loaded loaded-before)
        (unwind-protect
            (progn
              (with-temp-file nskk-search-learning-file
                (prin1 '(("new" "candidate" 1)) (current-buffer)))
              (nskk-prolog-assert
               '((learning-score "existing" "candidate" 7)))
              (let* ((key (nskk-prolog-clause-key 'learning-score 3))
                     (predicate-before
                      (nskk-dict-transaction-predicate-snapshot key)))
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
                (should (eq loaded-before (nskk-learning-loaded)))))
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
                (nskk-prolog-assert
                 '((learning-score "existing" "candidate" 7)))
                (nskk-set-learning-loaded
                 (if (eq fault 'error)
                     'before-publication-error
                   'before-publication-quit))
                (let* ((key (nskk-prolog-clause-key 'learning-score 3))
                       (predicate-before
                        (nskk-dict-transaction-predicate-snapshot key))
                       (loaded-value (nskk-learning-loaded))
                       (original-clear
                        (symbol-function
                         'nskk-dict-transaction-clear-pending-rollback)))
                  ;; The fault fires after the clause mutation has fully
                  ;; completed but still inside the protected region, which
                  ;; is the only window where a partial publication could
                  ;; become observable.
                  (cl-letf (((symbol-function
                              'nskk-dict-transaction-clear-pending-rollback)
                             (lambda (&rest arguments)
                               (apply original-clear arguments)
                               (nskk-set-learning-loaded 'during-publication)
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
                                '(learning-score "existing" "candidate" \?s)
                                '\?s)))
                  (should-not
                   (nskk-prolog-holds-p
                    '(learning-score "new" "candidate" 1)))
                  (should (eq loaded-value (nskk-learning-loaded)))))))
        (nskk-set-learning-loaded loaded-original-value)
        (delete-file nskk-search-learning-file)))))

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

(provide 'nskk-search-test)

;;; nskk-search-test.el ends here
