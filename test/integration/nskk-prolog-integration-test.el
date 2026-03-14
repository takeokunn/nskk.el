;;; nskk-prolog-integration-test.el --- Integration tests for nskk-prolog  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for nskk-prolog.el (Layer 0: Foundation).
;;
;; Tests verify the public API of the embedded Prolog engine:
;;   - Fact assertion and query (nskk-prolog-assert, nskk-prolog-query*)
;;   - Fact retraction (nskk-prolog-retract-all)
;;   - Trie prefix search (nskk-prolog-set-index :trie, nskk-prolog-trie-prefix-search)
;;   - PBT: trie prefix-result is a subset of all results
;;   - Deftest-cases: known predicate arities
;;
;; Every test uses nskk-prolog-test-with-isolated-db to prevent cross-test
;; Prolog database pollution.  Trie-indexed predicates always call
;; nskk-prolog-set-index BEFORE any nskk-prolog-assert calls.

;;; Code:

(require 'ert)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)

;;;; Group 1: Fact assertion and query

(nskk-describe "nskk-prolog-assert: fact assertion"

  (nskk-it "asserted fact unifies with a query"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-assert '((test-color apple red))))
      (let ((result (nskk-prolog-query-value
                     '(test-color apple \?color) '\?color)))
        (nskk-then
         (should (equal result 'red))))))

  (nskk-it "multiple facts with same predicate are all returned"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-assert '((fruit apple)))
       (nskk-prolog-assert '((fruit banana)))
       (nskk-prolog-assert '((fruit cherry))))
      (let ((results (nskk-prolog-query-all-values '(fruit \?f) '\?f)))
        (nskk-then
         (should (= 3 (length results)))
         (should (member 'apple results))
         (should (member 'banana results))
         (should (member 'cherry results))))))

  (nskk-it "assert/retract-all cycle: fact is gone after retraction"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-assert '((cycle-pred foo bar))))
      ;; Verify it exists first
      (should (nskk-prolog-holds-p '(cycle-pred foo bar)))
      (nskk-when
       (nskk-prolog-retract-all 'cycle-pred 2))
      (nskk-then
       (should-not (nskk-prolog-holds-p '(cycle-pred foo bar))))))

  (nskk-it "arity is respected: arity-2 fact does not match arity-1 query"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-assert '((pair-pred alpha beta))))
      (nskk-then
       ;; Exact match on arity-2 succeeds
       (should (nskk-prolog-holds-p '(pair-pred alpha beta)))
       ;; Arity-1 query for the same predicate name finds no clauses
       (should-not (nskk-prolog-holds-p '(pair-pred alpha))))))

  (nskk-it "query returns nil for predicate with no asserted facts"
    (nskk-prolog-test-with-isolated-db
      (nskk-then
       (should (null (nskk-prolog-query '(never-asserted-pred \?x)))))))

  (nskk-it "zero-arity fact is asserted and queried with holds-p"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-assert '((flag-present))))
      (nskk-then
       (should (nskk-prolog-holds-p '(flag-present))))))

  (nskk-it "query-one returns t for ground zero-arity fact"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-assert '((ground-zero-arity-fact))))
      (nskk-then
       (should (eq t (nskk-prolog-query-one '(ground-zero-arity-fact)))))))

  (nskk-it "query-value extracts binding from variable query"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-assert '((kv-fact "greeting" "hello"))))
      (let ((val (nskk-prolog-query-value
                  '(kv-fact "greeting" \?v) '\?v)))
        (nskk-then
         (should (equal val "hello"))))))

  (nskk-it "query-all-values returns all bindings across multiple solutions"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-assert '((color-set red)))
       (nskk-prolog-assert '((color-set green)))
       (nskk-prolog-assert '((color-set blue))))
      (let ((vals (nskk-prolog-query-all-values '(color-set \?c) '\?c)))
        (nskk-then
         (should (equal (sort (copy-sequence vals) #'string<)
                        (sort '(red green blue) #'string<))))))))

;;;; Group 2: Retraction

(nskk-describe "nskk-prolog-retract-all: fact removal"

  (nskk-it "retract-all removes all facts for a predicate"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-assert '((multi-fact one)))
       (nskk-prolog-assert '((multi-fact two)))
       (nskk-prolog-assert '((multi-fact three))))
      (should (= 3 (length (nskk-prolog-query '(multi-fact \?x)))))
      (nskk-when
       (nskk-prolog-retract-all 'multi-fact 1))
      (nskk-then
       (should (null (nskk-prolog-query '(multi-fact \?x)))))))

  (nskk-it "retract-all on a non-existent predicate does not error"
    (nskk-prolog-test-with-isolated-db
      (nskk-then
       (should-not (condition-case nil
                       (progn (nskk-prolog-retract-all 'does-not-exist 1) nil)
                     (error t))))))

  (nskk-it "retract-all followed by assert makes predicate usable again"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-assert '((reusable-pred original))))
      (nskk-when
       (nskk-prolog-retract-all 'reusable-pred 1)
       (nskk-prolog-assert '((reusable-pred new-value))))
      (nskk-then
       (should (nskk-prolog-holds-p '(reusable-pred new-value)))
       (should-not (nskk-prolog-holds-p '(reusable-pred original))))))

  (nskk-it "retract-all leaves other predicates of same name but different arity intact"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-assert '((shared-name one)))
       (nskk-prolog-assert '((shared-name one two))))
      ;; Retract arity-1 only
      (nskk-when
       (nskk-prolog-retract-all 'shared-name 1))
      (nskk-then
       ;; Arity-1 is gone
       (should-not (nskk-prolog-holds-p '(shared-name one)))
       ;; Arity-2 still present
       (should (nskk-prolog-holds-p '(shared-name one two))))))

  (nskk-it "retract-all on trie-indexed predicate preserves the index strategy"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-set-index 'trie-retract-test 2 :trie)
       (nskk-prolog-assert '((trie-retract-test "key" ("val")))))
      (nskk-when
       (nskk-prolog-retract-all 'trie-retract-test 2))
      ;; Can assert again after retract-all without re-calling set-index
      (nskk-prolog-assert '((trie-retract-test "key2" ("val2"))))
      (nskk-then
       (let ((hits (nskk-prolog-trie-prefix-search 'trie-retract-test 2 "key")))
         (should (= 1 (length hits)))
         (should (equal "key2" (caar hits))))))))

;;;; Group 3: Trie operations

(nskk-describe "nskk-prolog-trie-prefix-search: prefix queries"

  (nskk-it "prefix search returns all entries starting with prefix"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-set-index 'trie-dict 2 :trie)
       (nskk-prolog-assert '((trie-dict "さくら" ("桜"))))
       (nskk-prolog-assert '((trie-dict "さかな" ("魚"))))
       (nskk-prolog-assert '((trie-dict "うみ" ("海")))))
      (let ((results (nskk-prolog-trie-prefix-search 'trie-dict 2 "さ")))
        (nskk-then
         ;; "さくら" and "さかな" both start with "さ"; "うみ" does not
         (should (= 2 (length results)))
         (should (assoc "さくら" results))
         (should (assoc "さかな" results))
         (should-not (assoc "うみ" results))))))

  (nskk-it "prefix search with no matches returns empty list"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-set-index 'trie-empty-prefix 2 :trie)
       (nskk-prolog-assert '((trie-empty-prefix "あめ" ("雨")))))
      (let ((results (nskk-prolog-trie-prefix-search 'trie-empty-prefix 2 "ぜ")))
        (nskk-then
         (should (null results))))))

  (nskk-it "prefix that is a proper prefix of multiple keys returns all of them"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-set-index 'trie-exact 2 :trie)
       (nskk-prolog-assert '((trie-exact "やま" ("山"))))
       (nskk-prolog-assert '((trie-exact "やまと" ("大和")))))
      (let ((results (nskk-prolog-trie-prefix-search 'trie-exact 2 "やま")))
        (nskk-then
         ;; "やま" is a prefix of both "やま" and "やまと"
         (should (= 2 (length results)))
         (should (assoc "やま" results))
         (should (assoc "やまと" results))))))

  (nskk-it "Prolog query on exact key gives the correct candidates"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-set-index 'trie-exact2 2 :trie)
       (nskk-prolog-assert '((trie-exact2 "やま" ("山"))))
       (nskk-prolog-assert '((trie-exact2 "かわ" ("川")))))
      (let ((val (nskk-prolog-query-value '(trie-exact2 "やま" \?c) '\?c)))
        (nskk-then
         (should (equal val '("山")))))))

  (nskk-it "prefix search on empty trie returns nil"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-set-index 'trie-all-empty 2 :trie))
      (nskk-then
       (should (null (nskk-prolog-trie-prefix-search 'trie-all-empty 2 "あ"))))))

  (nskk-it "empty-string prefix returns all entries in trie"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-set-index 'trie-all-entries 2 :trie)
       (nskk-prolog-assert '((trie-all-entries "a" ("A"))))
       (nskk-prolog-assert '((trie-all-entries "b" ("B"))))
       (nskk-prolog-assert '((trie-all-entries "c" ("C")))))
      (let ((results (nskk-prolog-trie-prefix-search 'trie-all-entries 2 "")))
        (nskk-then
         (should (= 3 (length results)))))))

  (nskk-it "trie prefix search returns nil for an unindexed predicate"
    ;; Confirm that without set-index the trie is not consulted —
    ;; nskk-prolog-trie-prefix-search returns nil for an unindexed predicate.
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       ;; No set-index call here — predicate uses default :list strategy
       (nskk-prolog-assert '((unindexed-pred "prefix-key" ("val")))))
      (nskk-then
       (should (null (nskk-prolog-trie-prefix-search 'unindexed-pred 2 "prefix")))))))

;;;; Group 4: PBT — trie prefix subset invariant

;; Property: for any prefix P and any set of keys inserted into a trie,
;; every result returned by prefix_search(P) also appears in the full scan
;; (all asserted facts queried via nskk-prolog-query).

(nskk-property-test trie-prefix-results-subset-of-all
  ((prefix search-query))
  (let ((holds t))
    (nskk-prolog-test-with-isolated-db
      ;; Build a small trie with a handful of search-query keys.
      (nskk-prolog-set-index 'pbt-trie-test 2 :trie)
      ;; Generate 5 additional random keys to populate the trie alongside prefix.
      (let* ((extra-keys (cl-loop repeat 5
                                  collect (nskk-generate 'search-query)))
             (all-keys (cons prefix extra-keys)))
        (dolist (k all-keys)
          (when (and (stringp k) (> (length k) 0))
            (nskk-prolog-assert `((pbt-trie-test ,k ("dummy"))))))
        ;; Collect prefix-search results
        (let* ((prefix-results (when (and (stringp prefix) (> (length prefix) 0))
                                 (nskk-prolog-trie-prefix-search 'pbt-trie-test 2 prefix)))
               ;; Collect all keys via full Prolog query
               (all-result-keys (nskk-prolog-query-all-values
                                 '(pbt-trie-test \?k \?_v) '\?k)))
          ;; Every key from the prefix search must also be in the full results.
          (setq holds (cl-every (lambda (pair)
                                  (member (car pair) all-result-keys))
                                prefix-results)))))
    holds)
  20)

;;;; Group 5: deftest-cases — predicate arity mapping

;; Verify that key predicates asserted by NSKK modules exist and respond
;; correctly to holds-p queries with the expected argument count.
;; Each case is (predicate-name . arity) and the test confirms that querying
;; the predicate with that many variables returns a non-error result
;; (either solutions or empty -- both are valid; the test only checks no crash).

(nskk-deftest-table nskk-prolog-predicate-arity-mapping
  :description "Known predicates can be queried without error at the expected arity"
  :columns (input expected)
  :rows (("valid-mode" 1)
         ("japanese-mode" 1)
         ("romaji-to-kana" 2)
         ("dict-entry" 2)
         ("semicolon-key-action" 2)
         ("henkan-phase" 1))
  :body
  ;; Build a variable list of length `expected' then attempt a query.
  ;; The query may return nil (no solutions) but must not signal an error.
  (let* ((arity expected)
         (vars  (cl-loop for i from 1 to arity
                         collect (intern (format "?v%d" i))))
         (goal  (cons (intern input) vars)))
    (should-not
     (condition-case err
         (progn (nskk-prolog-query goal) nil)
       (error err)))))

(provide 'nskk-prolog-integration-test)

;;; nskk-prolog-integration-test.el ends here
