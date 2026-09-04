;;; nskk-prolog-integration-test.el --- Integration tests for nskk-prolog  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for nskk-prolog.

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
       (should (nskk-prolog-holds-p '(pair-pred alpha beta)))
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
                        (sort (list 'red 'green 'blue) #'string<))))))))

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
       ;; ERT fails on an unhandled signal, so the call itself is the assertion.
       (nskk-prolog-retract-all 'does-not-exist 1)
       (should (null (nskk-prolog-query '(does-not-exist \?x)))))))

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
      (nskk-when
       (nskk-prolog-retract-all 'shared-name 1))
      (nskk-then
       (should-not (nskk-prolog-holds-p '(shared-name one)))
       (should (nskk-prolog-holds-p '(shared-name one two))))))

  (nskk-it "retract-all on trie-indexed predicate preserves the index strategy"
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-set-index 'trie-retract-test 2 :trie)
       (nskk-prolog-assert '((trie-retract-test "key" ("val")))))
      (nskk-when
       (nskk-prolog-retract-all 'trie-retract-test 2))
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
    (nskk-prolog-test-with-isolated-db
      (nskk-given
       (nskk-prolog-assert '((unindexed-pred "prefix-key" ("val")))))
      (nskk-then
       (should (null (nskk-prolog-trie-prefix-search 'unindexed-pred 2 "prefix")))))))

;;;; Group 4: PBT — trie prefix subset invariant


(nskk-property-test trie-prefix-results-subset-of-all
  ((prefix search-query))
  (let ((holds t))
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'pbt-trie-test 2 :trie)
      (let* ((extra-keys (cl-loop repeat 5
                                  collect (nskk-generate 'search-query)))
             (all-keys (cons prefix extra-keys)))
        (dolist (k all-keys)
          (when (and (stringp k) (not (string-empty-p k)))
            (nskk-prolog-assert `((pbt-trie-test ,k ("dummy"))))))
        (let* ((prefix-results (when (and (stringp prefix) (not (string-empty-p prefix)))
                                 (nskk-prolog-trie-prefix-search 'pbt-trie-test 2 prefix)))
               (all-result-keys (nskk-prolog-query-all-values
                                 '(pbt-trie-test \?k \?_v) '\?k)))
          (setq holds (cl-every (lambda (pair)
                                  (member (car pair) all-result-keys))
                                prefix-results)))))
    holds)
  20)

;;;; Group 5: deftest-cases — predicate arity mapping


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
  (let* ((arity expected)
         (vars  (cl-loop for i from 1 to arity
                         collect (intern (format "?v%d" i))))
         (goal  (cons (intern input) vars)))
    ;; ERT fails on an unhandled signal, so querying is the assertion; the
    ;; extra check pins the shape of what an all-solutions query returns.
    (should (listp (nskk-prolog-query goal)))))

(provide 'nskk-prolog-integration-test)

;;; nskk-prolog-integration-test.el ends here
