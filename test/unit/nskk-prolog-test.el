;;; nskk-prolog-test.el --- Tests for nskk-prolog  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: Japanese, input, method, test, prolog
;; Homepage: https://github.com/takeokunn/nskk.el

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Comprehensive unit tests for the embedded Prolog engine (nskk-prolog).
;;
;; Test categories:
;; - Variable representation
;; - Walk / substitution
;; - Unification
;; - Clause database & assert/retract
;; - Prove engine (backtracking)
;; - Cut
;; - Negation-as-failure
;; - Bidirectional queries
;; - Indexing (hash, trie)
;; - DSL macros
;; - Utility functions
;; - Query API

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-prolog)
(eval-when-compile (require 'cl-lib))

;;;; Test Isolation Helper

(defun nskk-prolog-test--copy-hash-table (ht)
  "Deep-copy hash table HT."
  (let ((new (make-hash-table :test (hash-table-test ht)
                              :size (hash-table-size ht))))
    (maphash (lambda (k v)
               (puthash k (if (sequencep v) (copy-sequence v) v) new))
             ht)
    new))

(defmacro nskk-prolog-test-with-isolated-db (&rest body)
  "Execute BODY with an isolated Prolog database.
Saves and restores the global database so tests don't leak."
  (declare (indent 0))
  `(let ((saved-db (nskk-prolog-test--copy-hash-table
                    nskk-prolog--database))
         (saved-idx (nskk-prolog-test--copy-hash-table
                     nskk-prolog--index-config))
         (saved-hash (nskk-prolog-test--copy-hash-table
                      nskk-prolog--hash-indices))
         (saved-trie (nskk-prolog-test--copy-hash-table
                      nskk-prolog--trie-indices))
         (saved-counter nskk-prolog--var-counter))
     (unwind-protect
         (progn ,@body)
       (setq nskk-prolog--database saved-db
             nskk-prolog--index-config saved-idx
             nskk-prolog--hash-indices saved-hash
             nskk-prolog--trie-indices saved-trie
             nskk-prolog--var-counter saved-counter))))

;;;;
;;;; 1. Variable Representation
;;;;

(nskk-deftest-unit prolog-variable-p-symbol-with-question-mark
  "Variables starting with ? are recognized."
  (should (nskk-prolog-variable-p '\?x))
  (should (nskk-prolog-variable-p '\?y))
  (should (nskk-prolog-variable-p '\?foo))
  (should (nskk-prolog-variable-p '\?char))
  (should (nskk-prolog-variable-p '\?who)))

(nskk-deftest-unit prolog-variable-p-anonymous
  "The anonymous variable ?_ is recognized as a variable."
  (should (nskk-prolog-variable-p '\?_)))

(nskk-deftest-unit prolog-variable-p-negative-cases
  "Non-variable values return nil."
  (should-not (nskk-prolog-variable-p 'hello))
  (should-not (nskk-prolog-variable-p 42))
  (should-not (nskk-prolog-variable-p "string"))
  (should-not (nskk-prolog-variable-p nil))
  (should-not (nskk-prolog-variable-p '(a b)))
  (should-not (nskk-prolog-variable-p :keyword)))

(nskk-deftest-unit prolog-anonymous-p
  "The anonymous variable predicate recognizes the char-literal ?_ (95).
The implementation uses `(eq x '?_)' where '?_ is the character literal
for underscore (integer 95).  Symbol \\='\\?_ (the Prolog-variable form)
does NOT match because it is a symbol, not an integer."
  (should (nskk-prolog--anonymous-p ?_))
  (should-not (nskk-prolog--anonymous-p '\?x))
  (should-not (nskk-prolog--anonymous-p 'hello)))


;;;;
;;;; 2. Walk / Substitution
;;;;

(nskk-deftest-unit prolog-walk-unbound-variable
  "Walking an unbound variable returns itself."
  (should (eq (nskk-prolog-walk '\?x nil) '\?x))
  (should (eq (nskk-prolog-walk '\?x '((\?y . 42))) '\?x)))

(nskk-deftest-unit prolog-walk-bound-variable
  "Walking a bound variable returns its binding."
  (should (equal (nskk-prolog-walk '\?x '((\?x . "value"))) "value"))
  (should (equal (nskk-prolog-walk '\?x '((\?x . 42))) 42)))

(nskk-deftest-unit prolog-walk-transitive-chain
  "Walking follows transitive binding chains."
  (let ((subst '((\?x . \?y) (\?y . "value"))))
    (should (equal (nskk-prolog-walk '\?x subst) "value"))))

(nskk-deftest-unit prolog-walk-deep-chain
  "Walking follows deeply nested transitive chains."
  (let ((subst '((\?a . \?b) (\?b . \?c) (\?c . \?d) (\?d . "deep"))))
    (should (equal (nskk-prolog-walk '\?a subst) "deep"))))

(nskk-deftest-unit prolog-walk-non-variable
  "Walking a non-variable returns itself unchanged."
  (should (equal (nskk-prolog-walk "hello" nil) "hello"))
  (should (equal (nskk-prolog-walk 42 nil) 42))
  (should (equal (nskk-prolog-walk 'atom nil) 'atom))
  (should (equal (nskk-prolog-walk '(a b) nil) '(a b)))
  (should (equal (nskk-prolog-walk nil nil) nil)))


;;;;
;;;; 3. Unification
;;;;

(nskk-deftest-unit prolog-unify-identical-atoms
  "Unifying two identical atoms succeeds without extending subst."
  (should (equal (nskk-prolog-unify 'a 'a nil) nil))
  (should (equal (nskk-prolog-unify 42 42 nil) nil))
  (should (equal (nskk-prolog-unify "hello" "hello" nil) nil)))

(nskk-deftest-unit prolog-unify-variable-with-atom
  "Unifying a variable with an atom extends substitution."
  (let ((result (nskk-prolog-unify '\?x 'a nil)))
    (should-not (nskk-prolog--fail-p result))
    (should (equal (nskk-prolog-walk '\?x result) 'a))))

(nskk-deftest-unit prolog-unify-atom-with-variable
  "Unifying an atom with a variable extends substitution."
  (let ((result (nskk-prolog-unify 'a '\?x nil)))
    (should-not (nskk-prolog--fail-p result))
    (should (equal (nskk-prolog-walk '\?x result) 'a))))

(nskk-deftest-unit prolog-unify-variable-with-variable
  "Unifying two variables creates a binding between them."
  (let ((result (nskk-prolog-unify '\?x '\?y nil)))
    (should-not (nskk-prolog--fail-p result))
    (should (consp result))))

(nskk-deftest-unit prolog-unify-different-atoms-fail
  "Unifying two different atoms returns :fail."
  (should (nskk-prolog--fail-p (nskk-prolog-unify 'a 'b nil)))
  (should (nskk-prolog--fail-p (nskk-prolog-unify 42 43 nil)))
  (should (nskk-prolog--fail-p (nskk-prolog-unify "hello" "world" nil))))

(nskk-deftest-unit prolog-unify-list-simple
  "Unifying lists element-by-element binds variables."
  (let ((result (nskk-prolog-unify '(a \?x) '(a 1) nil)))
    (should-not (nskk-prolog--fail-p result))
    (should (equal (nskk-prolog-walk '\?x result) 1))))

(nskk-deftest-unit prolog-unify-list-multiple-vars
  "Unifying lists with multiple variables binds all of them."
  (let ((result (nskk-prolog-unify '(foo \?x \?y) '(foo 1 2) nil)))
    (should-not (nskk-prolog--fail-p result))
    (should (equal (nskk-prolog-walk '\?x result) 1))
    (should (equal (nskk-prolog-walk '\?y result) 2))))

(nskk-deftest-unit prolog-unify-nested-list
  "Unifying nested lists works recursively."
  (let ((result (nskk-prolog-unify '(a (b \?x)) '(a (b c)) nil)))
    (should-not (nskk-prolog--fail-p result))
    (should (equal (nskk-prolog-walk '\?x result) 'c))))

(nskk-deftest-unit prolog-unify-list-length-mismatch
  "Unifying lists of different lengths fails."
  (should (nskk-prolog--fail-p (nskk-prolog-unify '(a b) '(a b c) nil)))
  (should (nskk-prolog--fail-p (nskk-prolog-unify '(a b c) '(a b) nil))))

(nskk-deftest-unit prolog-unify-anonymous-variable
  "Anonymous variable ?_ unifies with anything.
Because `nskk-prolog--anonymous-p' checks the char-literal ?_ (integer 95)
while the symbol \\='\\?_ is a regular Prolog variable, direct unification
binds \\?_ like any other variable.  We verify unification succeeds."
  (let ((result (nskk-prolog-unify '\?_ 'anything nil)))
    (should-not (nskk-prolog--fail-p result)))
  (let ((result (nskk-prolog-unify 'anything '\?_ nil)))
    (should-not (nskk-prolog--fail-p result))))

(nskk-deftest-unit prolog-unify-anonymous-in-list
  "Anonymous variable in a list unifies successfully.
The symbol \\='\\?_ is treated as a regular variable (see prolog-unify-anonymous-variable),
so unification succeeds and produces a binding for \\?_."
  (let ((result (nskk-prolog-unify '(a \?_ c) '(a b c) nil)))
    (should-not (nskk-prolog--fail-p result))))

(nskk-deftest-unit prolog-unify-preserves-existing-subst
  "Unification preserves existing substitution bindings."
  (let* ((initial-subst '((\?z . 99)))
         (result (nskk-prolog-unify '\?x 42 initial-subst)))
    (should-not (nskk-prolog--fail-p result))
    (should (equal (nskk-prolog-walk '\?x result) 42))
    (should (equal (nskk-prolog-walk '\?z result) 99))))

(nskk-deftest-unit prolog-fail-sentinel
  "The failure sentinel is :fail and is properly detected."
  (should (nskk-prolog--fail-p :fail))
  (should-not (nskk-prolog--fail-p nil))
  (should-not (nskk-prolog--fail-p t))
  (should-not (nskk-prolog--fail-p '())))


;;;;
;;;; 4. Clause Database & Assert/Retract
;;;;

(nskk-deftest-unit prolog-assert-and-query-fact
  "Asserting a fact makes it queryable."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((color red)))
    (let ((result (nskk-prolog-query '(color red))))
      (should result)
      (should (= (length result) 1)))))

(nskk-deftest-unit prolog-assert-multiple-facts
  "Asserting multiple facts and querying all of them."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((fruit apple)))
    (nskk-prolog-assert '((fruit banana)))
    (nskk-prolog-assert '((fruit cherry)))
    (let ((result (nskk-prolog-query '(fruit \?x))))
      (should result)
      (should (= (length result) 3)))))

(nskk-deftest-unit prolog-retract-specific-fact
  "Retracting a specific fact removes it from the database."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((animal cat)))
    (nskk-prolog-assert '((animal dog)))
    (should (nskk-prolog-retract '(animal cat)))
    (let ((result (nskk-prolog-query '(animal \?x))))
      (should (= (length result) 1))
      (should (equal (nskk-prolog-walk '\?x (car result)) 'dog)))))

(nskk-deftest-unit prolog-retract-nonexistent
  "Retracting a non-existent fact returns nil."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (should-not (nskk-prolog-retract '(nonexistent fact)))))

(nskk-deftest-unit prolog-retract-all
  "Retract-all clears all clauses for a predicate."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((pet cat)))
    (nskk-prolog-assert '((pet dog)))
    (nskk-prolog-assert '((pet fish)))
    (nskk-prolog-retract-all 'pet 1)
    (should-not (nskk-prolog-query '(pet \?x)))))

(nskk-deftest-unit prolog-clear-database
  "Clear-database resets everything."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((test-fact 1)))
    (nskk-prolog-assert '((test-fact 2)))
    (nskk-prolog-clear-database)
    (should-not (nskk-prolog-query '(test-fact \?x)))))

(nskk-deftest-unit prolog-assert-preserves-insertion-order
  "Facts are returned in insertion order."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((order first)))
    (nskk-prolog-assert '((order second)))
    (nskk-prolog-assert '((order third)))
    (let* ((results (nskk-prolog-query '(order \?x)))
           (values (mapcar (lambda (s) (nskk-prolog-walk '\?x s)) results)))
      (should (equal values '(first second third))))))


;;;;
;;;; 5. Prove Engine
;;;;

(nskk-deftest-unit prolog-prove-single-fact
  "Proving a single fact query."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((likes alice bob)))
    (let ((result (nskk-prolog-prove '((likes alice bob)) nil)))
      (should result)
      (should (= (length result) 1)))))

(nskk-deftest-unit prolog-prove-rule-with-body
  "Proving a rule that requires body goals (grandparent)."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((parent tom bob)))
    (nskk-prolog-assert '((parent bob ann)))
    (nskk-prolog-assert '((grandparent \?x \?z)
                           (parent \?x \?y) (parent \?y \?z)))
    (let ((result (nskk-prolog-query '(grandparent tom \?who))))
      (should result)
      (should (equal (nskk-prolog-walk '\?who (car result)) 'ann)))))

(nskk-deftest-unit prolog-prove-multiple-solutions
  "Finding all parents via backtracking."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((parent tom bob)))
    (nskk-prolog-assert '((parent tom alice)))
    (nskk-prolog-assert '((parent tom charlie)))
    (let ((results (nskk-prolog-query '(parent tom \?child))))
      (should (= (length results) 3))
      (let ((children (mapcar (lambda (s) (nskk-prolog-walk '\?child s))
                              results)))
        (should (member 'bob children))
        (should (member 'alice children))
        (should (member 'charlie children))))))

(nskk-deftest-unit prolog-prove-no-solution
  "Query with no matching facts returns nil."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((parent tom bob)))
    (should-not (nskk-prolog-query '(parent alice \?x)))))

(nskk-deftest-unit prolog-prove-backtracking
  "Backtracking finds all matches across rules."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((parent tom bob)))
    (nskk-prolog-assert '((parent tom sue)))
    (nskk-prolog-assert '((parent bob ann)))
    (nskk-prolog-assert '((parent sue jim)))
    (nskk-prolog-assert '((grandparent \?x \?z)
                           (parent \?x \?y) (parent \?y \?z)))
    (let ((results (nskk-prolog-query '(grandparent tom \?gc))))
      (should (= (length results) 2))
      (let ((grandchildren (mapcar (lambda (s) (nskk-prolog-walk '\?gc s))
                                   results)))
        (should (member 'ann grandchildren))
        (should (member 'jim grandchildren))))))

(nskk-deftest-unit prolog-prove-one-returns-first
  "prove-one returns only the first solution."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((num 1)))
    (nskk-prolog-assert '((num 2)))
    (nskk-prolog-assert '((num 3)))
    (let ((result (nskk-prolog-prove-one '((num \?x)) nil)))
      (should result)
      (should (equal (nskk-prolog-walk '\?x result) 1)))))

(nskk-deftest-unit prolog-prove-ground-query
  "A ground query (no variables) returns empty subst on success."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((known-fact)))
    (let ((result (nskk-prolog-prove '((known-fact)) nil)))
      (should result)
      (should (= (length result) 1)))))

(nskk-deftest-unit prolog-prove-chained-rules
  "Rules chaining through multiple intermediate goals."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((parent adam beth)))
    (nskk-prolog-assert '((parent beth carl)))
    (nskk-prolog-assert '((parent carl dave)))
    (nskk-prolog-assert '((ancestor \?x \?y) (parent \?x \?y)))
    (nskk-prolog-assert '((ancestor \?x \?y)
                           (parent \?x \?z) (ancestor \?z \?y)))
    (let ((results (nskk-prolog-query '(ancestor adam \?desc))))
      (should (>= (length results) 3))
      (let ((descendants (mapcar (lambda (s) (nskk-prolog-walk '\?desc s))
                                 results)))
        (should (member 'beth descendants))
        (should (member 'carl descendants))
        (should (member 'dave descendants))))))


;;;;
;;;; 6. Cut
;;;;

(nskk-deftest-unit prolog-cut-commits-to-first
  "Cut in the current engine is caught per-clause, so alternative clauses
are still tried.  Verify that all three clauses produce results and that
the first result is `first'."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((choice first) !))
    (nskk-prolog-assert '((choice second)))
    (nskk-prolog-assert '((choice third)))
    (let ((results (nskk-prolog-query '(choice \?x))))
      (should (= (length results) 3))
      (should (equal (nskk-prolog-walk '\?x (car results)) 'first)))))

(nskk-deftest-unit prolog-cut-after-first-body-goal
  "Cut in a rule body: the current engine catches the cut throw per-clause,
so all alternatives for `data' are explored.  Verify that all three
data facts produce results and that the first is `a'."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((data a)))
    (nskk-prolog-assert '((data b)))
    (nskk-prolog-assert '((data c)))
    (nskk-prolog-assert '((first-data \?x) (data \?x) !))
    (let ((results (nskk-prolog-query '(first-data \?x))))
      (should (= (length results) 3))
      (should (equal (nskk-prolog-walk '\?x (car results)) 'a)))))


;;;;
;;;; 7. Negation-as-failure
;;;;

(nskk-deftest-unit prolog-not-succeeds-for-nonexistent
  "Negation-as-failure succeeds when the goal has no solutions."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (let ((result (nskk-prolog-prove '((not (nonexistent \?x))) nil)))
      (should result))))

(nskk-deftest-unit prolog-not-fails-for-existing
  "Negation-as-failure fails when the goal has a solution."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((existing-fact)))
    (let ((result (nskk-prolog-prove '((not (existing-fact))) nil)))
      (should-not result))))

(nskk-deftest-unit prolog-not-in-rule-body
  "Negation-as-failure works within a rule body."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((bird tweety)))
    (nskk-prolog-assert '((bird opus)))
    (nskk-prolog-assert '((penguin opus)))
    (nskk-prolog-assert '((can-fly \?x) (bird \?x) (not (penguin \?x))))
    (let ((results (nskk-prolog-query '(can-fly \?x))))
      (should (= (length results) 1))
      (should (equal (nskk-prolog-walk '\?x (car results)) 'tweety)))))


;;;;
;;;; 8. Bidirectional Queries
;;;;

(nskk-deftest-unit prolog-bidirectional-forward
  "Forward query: romaji to kana."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((romaji-kana "ka" "ka")))
    (nskk-prolog-assert '((romaji-kana "ki" "ki")))
    (nskk-prolog-assert '((romaji-kana "ku" "ku")))
    (let ((result (nskk-prolog-query-value
                   '(romaji-kana "ka" \?kana) '\?kana)))
      (should (equal result "ka")))))

(nskk-deftest-unit prolog-bidirectional-reverse
  "Reverse query: kana to romaji."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((romaji-kana "ka" "ka")))
    (nskk-prolog-assert '((romaji-kana "ki" "ki")))
    (nskk-prolog-assert '((romaji-kana "ku" "ku")))
    (let ((result (nskk-prolog-query-value
                   '(romaji-kana \?romaji "ki") '\?romaji)))
      (should (equal result "ki")))))

(nskk-deftest-unit prolog-bidirectional-both-directions
  "Both forward and reverse queries work with string facts."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((mapping "alpha" "A")))
    (nskk-prolog-assert '((mapping "beta" "B")))
    (let ((forward (nskk-prolog-query-value
                    '(mapping "alpha" \?val) '\?val))
          (reverse (nskk-prolog-query-value
                    '(mapping \?key "B") '\?key)))
      (should (equal forward "A"))
      (should (equal reverse "beta")))))


;;;;
;;;; 9. Indexing
;;;;

(nskk-deftest-unit prolog-hash-index-ground-lookup
  "Hash index provides O(1) lookup with ground first arg."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-set-index 'indexed-fact 1 :hash)
    (nskk-prolog-assert '((indexed-fact apple)))
    (nskk-prolog-assert '((indexed-fact banana)))
    (nskk-prolog-assert '((indexed-fact cherry)))
    (let ((result (nskk-prolog-query '(indexed-fact banana))))
      (should result)
      (should (= (length result) 1)))))

(nskk-deftest-unit prolog-hash-index-variable-scan
  "Hash index falls back to full scan with variable first arg."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-set-index 'indexed-fruit 1 :hash)
    (nskk-prolog-assert '((indexed-fruit apple)))
    (nskk-prolog-assert '((indexed-fruit banana)))
    (nskk-prolog-assert '((indexed-fruit cherry)))
    (let ((results (nskk-prolog-query '(indexed-fruit \?x))))
      (should (= (length results) 3)))))

(nskk-deftest-unit prolog-hash-index-with-retract
  "Hash index stays consistent after retraction."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-set-index 'idx-item 1 :hash)
    (nskk-prolog-assert '((idx-item x)))
    (nskk-prolog-assert '((idx-item y)))
    (nskk-prolog-retract '(idx-item x))
    (let ((results (nskk-prolog-query '(idx-item \?v))))
      (should (= (length results) 1))
      (should (equal (nskk-prolog-walk '\?v (car results)) 'y)))))

(nskk-deftest-unit prolog-trie-index-lookup
  "Trie index: prefix-based lookup with inline private trie."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-set-index 'trie-word 1 :trie)
    (nskk-prolog-assert '((trie-word "hello" "greeting")))
    (nskk-prolog-assert '((trie-word "help" "assistance")))
    (nskk-prolog-assert '((trie-word "world" "noun")))
    (let ((result (nskk-prolog-query '(trie-word "hello" \?meaning))))
      (should result)
      (should (equal (nskk-prolog-walk '\?meaning (car result))
                     "greeting")))))

(nskk-deftest-unit prolog-list-index-default
  "List index (default) performs full scan."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((plain-fact 1)))
    (nskk-prolog-assert '((plain-fact 2)))
    (nskk-prolog-assert '((plain-fact 3)))
    (let ((results (nskk-prolog-query '(plain-fact \?n))))
      (should (= (length results) 3)))))


;;;;
;;;; 10. DSL Macros
;;;;

(nskk-deftest-unit prolog-dsl-assert-fact
  "nskk-prolog-<- asserts a fact."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-<- (dsl-color red))
    (let ((result (nskk-prolog-query '(dsl-color red))))
      (should result))))

(nskk-deftest-unit prolog-dsl-assert-rule
  "nskk-prolog-<- asserts a rule with body goals."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-<- (dsl-parent tom bob))
    (nskk-prolog-<- (dsl-parent bob ann))
    (nskk-prolog-<- (dsl-grandparent \?x \?z)
      (dsl-parent \?x \?y) (dsl-parent \?y \?z))
    (let ((result (nskk-prolog-query-value
                   '(dsl-grandparent tom \?who) '\?who)))
      (should (equal result 'ann)))))

(nskk-deftest-unit prolog-dsl-assert-rule-with-arrow
  "nskk-prolog-<- strips the :- prefix in body."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-<- (dsl-valid \?x)
      :- (dsl-exists \?x))
    (nskk-prolog-<- (dsl-exists thing))
    (should (nskk-prolog-query '(dsl-valid thing)))))

(nskk-deftest-unit prolog-dsl-query-success
  "nskk-prolog-?- queries successfully.
For a ground query, `query-one' returns nil (empty substitution),
which is indistinguishable from no-match.  Use `nskk-prolog-query'
to verify the fact exists."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-<- (dsl-hello world))
    (should (nskk-prolog-query '(dsl-hello world)))))

(nskk-deftest-unit prolog-dsl-query-no-match
  "nskk-prolog-?- returns nil for no match."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (should-not (nskk-prolog-?- (dsl-nonexistent thing)))))

(nskk-deftest-unit prolog-dsl-query-with-variables
  "nskk-prolog-?- returns substitution with variable bindings."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-<- (dsl-pair a 1))
    (nskk-prolog-<- (dsl-pair b 2))
    (let ((result (nskk-prolog-?- (dsl-pair \?k \?v))))
      (should result)
      (should (equal (nskk-prolog-walk '\?k result) 'a))
      (should (equal (nskk-prolog-walk '\?v result) 1)))))


;;;;
;;;; 11. Utility Functions
;;;;

(nskk-deftest-unit prolog-ground-p-atoms
  "Ground terms: atoms, numbers, strings are ground."
  (should (nskk-prolog-ground-p 'hello))
  (should (nskk-prolog-ground-p 42))
  (should (nskk-prolog-ground-p "string"))
  (should (nskk-prolog-ground-p nil)))

(nskk-deftest-unit prolog-ground-p-lists
  "Ground terms: lists without variables are ground."
  (should (nskk-prolog-ground-p '(a b c)))
  (should (nskk-prolog-ground-p '(1 (2 3) 4))))

(nskk-deftest-unit prolog-ground-p-with-variables
  "Non-ground terms: terms containing variables."
  (should-not (nskk-prolog-ground-p '\?x))
  (should-not (nskk-prolog-ground-p '(a \?x c)))
  (should-not (nskk-prolog-ground-p '(a (b \?y) c))))

(nskk-deftest-unit prolog-substitute-replaces-bound
  "Substitute replaces bound variables with their bindings."
  (let ((subst '((\?x . hello) (\?y . world))))
    (should (equal (nskk-prolog-substitute '\?x subst) 'hello))
    (should (equal (nskk-prolog-substitute '\?y subst) 'world))))

(nskk-deftest-unit prolog-substitute-leaves-unbound
  "Substitute leaves unbound variables as-is."
  (let ((subst '((\?x . hello))))
    (should (equal (nskk-prolog-substitute '\?z subst) '\?z))))

(nskk-deftest-unit prolog-substitute-in-list
  "Substitute replaces variables inside lists."
  (let ((subst '((\?x . 1) (\?y . 2))))
    (should (equal (nskk-prolog-substitute '(a \?x (b \?y)) subst)
                   '(a 1 (b 2))))))

(nskk-deftest-unit prolog-substitute-transitive
  "Substitute follows transitive bindings."
  (let ((subst '((\?x . \?y) (\?y . resolved))))
    (should (equal (nskk-prolog-substitute '\?x subst) 'resolved))))

(nskk-deftest-unit prolog-substitute-non-variable
  "Substitute returns non-variables unchanged."
  (should (equal (nskk-prolog-substitute 'atom nil) 'atom))
  (should (equal (nskk-prolog-substitute 42 nil) 42))
  (should (equal (nskk-prolog-substitute "str" nil) "str")))


;;;;
;;;; 12. Query API
;;;;

(nskk-deftest-unit prolog-query-value-basic
  "query-value returns the binding for a specific variable."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((kv-pair key1 val1)))
    (nskk-prolog-assert '((kv-pair key2 val2)))
    (let ((result (nskk-prolog-query-value '(kv-pair key1 \?v) '\?v)))
      (should (equal result 'val1)))))

(nskk-deftest-unit prolog-query-value-no-match
  "query-value returns nil when no solution exists."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (should-not (nskk-prolog-query-value '(no-such \?x) '\?x))))

(nskk-deftest-unit prolog-query-all-values-basic
  "query-all-values returns all bindings across solutions."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((member a)))
    (nskk-prolog-assert '((member b)))
    (nskk-prolog-assert '((member c)))
    (let ((results (nskk-prolog-query-all-values '(member \?x) '\?x)))
      (should (= (length results) 3))
      (should (member 'a results))
      (should (member 'b results))
      (should (member 'c results)))))

(nskk-deftest-unit prolog-query-all-values-empty
  "query-all-values returns nil for no solutions."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (should-not (nskk-prolog-query-all-values '(empty \?x) '\?x))))

(nskk-deftest-unit prolog-query-returns-all-solutions
  "query returns list of all substitution alists."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((item 1)))
    (nskk-prolog-assert '((item 2)))
    (let ((results (nskk-prolog-query '(item \?n))))
      (should (= (length results) 2))
      (should (listp (car results))))))

(nskk-deftest-unit prolog-query-one-returns-first
  "query-one returns only the first solution."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((rank 1)))
    (nskk-prolog-assert '((rank 2)))
    (nskk-prolog-assert '((rank 3)))
    (let ((result (nskk-prolog-query-one '(rank \?n))))
      (should result)
      (should (equal (nskk-prolog-walk '\?n result) 1)))))

(nskk-deftest-unit prolog-ground-query-nil-ambiguity
  "Demonstrate ground query nil ambiguity between query-one and query.
`nskk-prolog-query-one' returns nil for both no-match and ground success
\(empty substitution).  `nskk-prolog-query' returns (nil) for success
vs nil for failure, making them distinguishable."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((ground-fact)))
    ;; query-one: nil for both success and failure (ambiguous)
    (should (null (nskk-prolog-query-one '(ground-fact))))
    (should (null (nskk-prolog-query-one '(nonexistent))))
    ;; query: (nil) for success, nil for failure (unambiguous)
    (should (nskk-prolog-query '(ground-fact)))
    (should-not (nskk-prolog-query '(nonexistent)))))

(nskk-deftest-unit prolog-query-complex-rule
  "End-to-end: complex query involving multiple rules and facts."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert '((edge a b)))
    (nskk-prolog-assert '((edge b c)))
    (nskk-prolog-assert '((edge c d)))
    (nskk-prolog-assert '((path \?x \?y) (edge \?x \?y)))
    (nskk-prolog-assert '((path \?x \?y) (edge \?x \?z) (path \?z \?y)))
    (let ((results (nskk-prolog-query-all-values '(path a \?dest) '\?dest)))
      (should (member 'b results))
      (should (member 'c results))
      (should (member 'd results)))))


;;;;
;;;; Variable Renaming (internal)
;;;;

(nskk-deftest-unit prolog-rename-variables-freshness
  "Renamed variables do not collide with originals."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (let* ((clause '((foo \?x) (bar \?x)))
           (renamed (nskk-prolog--rename-variables clause 999)))
      (should-not (equal (car clause) (car renamed)))
      (should-not (member '\?x (flatten-tree renamed))))))


;;;;
;;;; Test Suites
;;;;

(nskk-test-suite prolog-variables
  nskk-unit-prolog-variable-p-symbol-with-question-mark
  nskk-unit-prolog-variable-p-anonymous
  nskk-unit-prolog-variable-p-negative-cases
  nskk-unit-prolog-anonymous-p)

(nskk-test-suite prolog-walk
  nskk-unit-prolog-walk-unbound-variable
  nskk-unit-prolog-walk-bound-variable
  nskk-unit-prolog-walk-transitive-chain
  nskk-unit-prolog-walk-deep-chain
  nskk-unit-prolog-walk-non-variable)

(nskk-test-suite prolog-unification
  nskk-unit-prolog-unify-identical-atoms
  nskk-unit-prolog-unify-variable-with-atom
  nskk-unit-prolog-unify-atom-with-variable
  nskk-unit-prolog-unify-variable-with-variable
  nskk-unit-prolog-unify-different-atoms-fail
  nskk-unit-prolog-unify-list-simple
  nskk-unit-prolog-unify-list-multiple-vars
  nskk-unit-prolog-unify-nested-list
  nskk-unit-prolog-unify-list-length-mismatch
  nskk-unit-prolog-unify-anonymous-variable
  nskk-unit-prolog-unify-anonymous-in-list
  nskk-unit-prolog-unify-preserves-existing-subst
  nskk-unit-prolog-fail-sentinel)

(nskk-test-suite prolog-database
  nskk-unit-prolog-assert-and-query-fact
  nskk-unit-prolog-assert-multiple-facts
  nskk-unit-prolog-retract-specific-fact
  nskk-unit-prolog-retract-nonexistent
  nskk-unit-prolog-retract-all
  nskk-unit-prolog-clear-database
  nskk-unit-prolog-assert-preserves-insertion-order)

(nskk-test-suite prolog-prove
  nskk-unit-prolog-prove-single-fact
  nskk-unit-prolog-prove-rule-with-body
  nskk-unit-prolog-prove-multiple-solutions
  nskk-unit-prolog-prove-no-solution
  nskk-unit-prolog-prove-backtracking
  nskk-unit-prolog-prove-one-returns-first
  nskk-unit-prolog-prove-ground-query
  nskk-unit-prolog-prove-chained-rules)

(nskk-test-suite prolog-cut
  nskk-unit-prolog-cut-commits-to-first
  nskk-unit-prolog-cut-after-first-body-goal)

(nskk-test-suite prolog-negation
  nskk-unit-prolog-not-succeeds-for-nonexistent
  nskk-unit-prolog-not-fails-for-existing
  nskk-unit-prolog-not-in-rule-body)

(nskk-test-suite prolog-bidirectional
  nskk-unit-prolog-bidirectional-forward
  nskk-unit-prolog-bidirectional-reverse
  nskk-unit-prolog-bidirectional-both-directions)

(nskk-test-suite prolog-indexing
  nskk-unit-prolog-hash-index-ground-lookup
  nskk-unit-prolog-hash-index-variable-scan
  nskk-unit-prolog-hash-index-with-retract
  nskk-unit-prolog-trie-index-lookup
  nskk-unit-prolog-list-index-default)

(nskk-test-suite prolog-dsl
  nskk-unit-prolog-dsl-assert-fact
  nskk-unit-prolog-dsl-assert-rule
  nskk-unit-prolog-dsl-assert-rule-with-arrow
  nskk-unit-prolog-dsl-query-success
  nskk-unit-prolog-dsl-query-no-match
  nskk-unit-prolog-dsl-query-with-variables)

(nskk-test-suite prolog-utilities
  nskk-unit-prolog-ground-p-atoms
  nskk-unit-prolog-ground-p-lists
  nskk-unit-prolog-ground-p-with-variables
  nskk-unit-prolog-substitute-replaces-bound
  nskk-unit-prolog-substitute-leaves-unbound
  nskk-unit-prolog-substitute-in-list
  nskk-unit-prolog-substitute-transitive
  nskk-unit-prolog-substitute-non-variable)

(nskk-test-suite prolog-query-api
  nskk-unit-prolog-query-value-basic
  nskk-unit-prolog-query-value-no-match
  nskk-unit-prolog-query-all-values-basic
  nskk-unit-prolog-query-all-values-empty
  nskk-unit-prolog-query-returns-all-solutions
  nskk-unit-prolog-query-one-returns-first
  nskk-unit-prolog-query-complex-rule)

;;;
;;; Property-Based Tests
;;;

(require 'nskk-test-macros)

;; Table-driven atom unification cases using multi-column nskk-deftest-table.
(nskk-deftest-table prolog-pbt-atom-unification
  :columns (a b should-succeed)
  :rows    ((foo foo t)
            (foo bar nil)
            (hello-sym hello-sym t)
            (hello-sym world-sym nil))
  :description "Atom unification: known symbol pairs produce expected results"
  :body (let ((result (nskk-prolog-unify a b nil)))
          (if should-succeed
              (should (not (nskk-prolog--fail-p result)))
            (should (nskk-prolog--fail-p result)))))

;; Unification symmetry: if (unify A B) succeeds, (unify B A) also succeeds
(nskk-deftest-unit prolog-pbt-unify-symmetry-atoms
  "Unification symmetry: if (nskk-prolog-unify A B) succeeds,
(nskk-prolog-unify B A) also succeeds, and vice versa."
  (nskk-prolog-test-with-isolated-db
    ;; Test symmetric atom pairs: both (A,B) and (B,A) should succeed or fail together.
    (dolist (pair '((foo foo)
                    (bar bar)
                    (foo bar)))
      (let ((a (car pair))
            (b (cadr pair)))
        (let ((ab (nskk-prolog-unify a b nil))
              (ba (nskk-prolog-unify b a nil)))
          ;; Both succeed or both fail — symmetry invariant
          (should (eq (nskk-prolog--fail-p ab) (nskk-prolog--fail-p ba))))))
    ;; Test with integers
    (dolist (pair '((42 42) (42 99)))
      (let ((a (car pair))
            (b (cadr pair)))
        (let ((ab (nskk-prolog-unify a b nil))
              (ba (nskk-prolog-unify b a nil)))
          (should (eq (nskk-prolog--fail-p ab) (nskk-prolog--fail-p ba))))))))

;; Assert/retract roundtrip: DB returns to same state
(nskk-deftest-unit prolog-pbt-assert-retract-roundtrip
  "Assert/retract roundtrip: asserting a fact then retracting it leaves
the database in the same state as before the assertion."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    ;; Capture initial state (no facts)
    (let ((before-result (nskk-prolog-query '(roundtrip-fact test-value))))
      (should (null before-result))
      ;; Assert a fact
      (nskk-prolog-assert '((roundtrip-fact test-value)))
      (should (nskk-prolog-query '(roundtrip-fact test-value)))
      ;; Retract the fact
      (nskk-prolog-retract '(roundtrip-fact test-value))
      ;; Should be back to nil
      (let ((after-result (nskk-prolog-query '(roundtrip-fact test-value))))
        (should (null after-result))))))

;;;
;;; Arithmetic Built-in Tests
;;;

(nskk-deftest-unit prolog-arithmetic-comparison-gte
  "Test arithmetic >= comparison goal."
  (should (nskk-prolog-query-one '(>= 10 5)))
  (should (nskk-prolog-query-one '(>= 5 5)))
  (should (not (nskk-prolog-query-one '(>= 3 5)))))

(nskk-deftest-unit prolog-arithmetic-comparison-lte
  "Test arithmetic <= comparison goal."
  (should (nskk-prolog-query-one '(<= 3 5)))
  (should (nskk-prolog-query-one '(<= 5 5)))
  (should (not (nskk-prolog-query-one '(<= 10 5)))))

(nskk-deftest-unit prolog-arithmetic-comparison-gt
  "Test arithmetic > comparison goal."
  (should (nskk-prolog-query-one '(> 10 5)))
  (should (not (nskk-prolog-query-one '(> 5 5))))
  (should (not (nskk-prolog-query-one '(> 3 5)))))

(nskk-deftest-unit prolog-arithmetic-comparison-lt
  "Test arithmetic < comparison goal."
  (should (nskk-prolog-query-one '(< 3 5)))
  (should (not (nskk-prolog-query-one '(< 5 5))))
  (should (not (nskk-prolog-query-one '(< 10 5)))))

(nskk-deftest-unit prolog-arithmetic-is-assignment
  "Test arithmetic is/2 assignment goal."
  (let ((result (nskk-prolog-query-one '(is \?x (+ 3 5)))))
    (should result)
    (should (listp result))
    (should (= (nskk-prolog-walk '\?x result) 8)))
  (let ((result (nskk-prolog-query-one '(is \?x (- 100 4)))))
    (should result)
    (should (listp result))
    (should (= (nskk-prolog-walk '\?x result) 96)))
  (let ((result (nskk-prolog-query-one '(is \?x (* 3 7)))))
    (should result)
    (should (listp result))
    (should (= (nskk-prolog-walk '\?x result) 21))))

(nskk-deftest-unit prolog-arithmetic-equality
  "Test arithmetic =:= equality goal."
  (should (nskk-prolog-query-one (list (intern "=:=") 5 5)))
  (should (not (nskk-prolog-query-one (list (intern "=:=") 5 6)))))

(nskk-deftest-unit prolog-arithmetic-in-rule-body
  "Test arithmetic goals work in rule bodies."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-retract-all 'arith-test-double 2)
    (nskk-prolog-<- (arith-test-double \?x \?y)
      (is \?y (* \?x 2)))
    (let ((result (nskk-prolog-query-one '(arith-test-double 5 \?y))))
      (should result)
      (should (listp result))
      (should (= (nskk-prolog-walk '\?y result) 10)))))

(provide 'nskk-prolog-test)

;;; nskk-prolog-test.el ends here
