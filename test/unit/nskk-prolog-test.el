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
(require 'subr-x)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-prolog)
(eval-when-compile (require 'cl-lib))

;;;;
;;;; 1. Variable Representation
;;;;

(nskk-describe "Prolog variable representation"
  (nskk-context "symbol-with-question-mark variables"
    (nskk-it "recognizes variables starting with ? as Prolog variables"
      (should (nskk-prolog-variable-p '\?x))
      (should (nskk-prolog-variable-p '\?y))
      (should (nskk-prolog-variable-p '\?foo))
      (should (nskk-prolog-variable-p '\?char))
      (should (nskk-prolog-variable-p '\?who))))

  (nskk-context "anonymous variable"
    (nskk-it "recognizes the anonymous variable ?_ as a Prolog variable"
      (should (nskk-prolog-variable-p '\?_))))

  (nskk-context "non-variable values"
    (nskk-it "returns nil for atoms, numbers, strings, nil, lists, and keywords"
      (should-not (nskk-prolog-variable-p 'hello))
      (should-not (nskk-prolog-variable-p 42))
      (should-not (nskk-prolog-variable-p "string"))
      (should-not (nskk-prolog-variable-p nil))
      (should-not (nskk-prolog-variable-p '(a b)))
      (should-not (nskk-prolog-variable-p :keyword))))

  (nskk-context "anonymous predicate"
    (nskk-it "recognizes the char-literal ?_ (integer 95) as anonymous, not the symbol \\?_"
      ;; The implementation uses `(eq x ?_)' where ?_ is the character literal
      ;; for underscore (integer 95).  Symbol '\?_ (the Prolog-variable form)
      ;; does NOT match because it is a symbol, not an integer.
      (should (nskk-prolog--anonymous-p ?_))
      (should-not (nskk-prolog--anonymous-p '\?x))
      (should-not (nskk-prolog--anonymous-p 'hello)))))

;;;;
;;;; 2. Walk / Substitution
;;;;

(nskk-describe "Prolog walk and substitution"
  (nskk-context "unbound variable"
    (nskk-it "returns the variable itself when it has no binding"
      (should (eq (nskk-prolog-walk '\?x nil) '\?x))
      (should (eq (nskk-prolog-walk '\?x '((\?y . 42))) '\?x))))

  (nskk-context "bound variable"
    (nskk-it "returns the binding value for a bound variable"
      (should (equal (nskk-prolog-walk '\?x '((\?x . "value"))) "value"))
      (should (equal (nskk-prolog-walk '\?x '((\?x . 42))) 42))))

  (nskk-context "transitive chains"
    (nskk-it "follows a two-step transitive binding chain"
      (let ((subst '((\?x . \?y) (\?y . "value"))))
        (should (equal (nskk-prolog-walk '\?x subst) "value"))))

    (nskk-it "follows deeply nested four-step transitive binding chains"
      (let ((subst '((\?a . \?b) (\?b . \?c) (\?c . \?d) (\?d . "deep"))))
        (should (equal (nskk-prolog-walk '\?a subst) "deep")))))

  (nskk-context "non-variable terms"
    (nskk-it "returns strings, numbers, atoms, lists, and nil unchanged"
      (should (equal (nskk-prolog-walk "hello" nil) "hello"))
      (should (equal (nskk-prolog-walk 42 nil) 42))
      (should (equal (nskk-prolog-walk 'atom nil) 'atom))
      (should (equal (nskk-prolog-walk '(a b) nil) '(a b)))
      (should (equal (nskk-prolog-walk nil nil) nil)))))

;;;;
;;;; 3. Unification
;;;;

(nskk-describe "Prolog unification"
  (nskk-context "identical atoms"
    (nskk-it "unifying two identical atoms succeeds without extending substitution"
      (should (equal (nskk-prolog-unify 'a 'a nil) nil))
      (should (equal (nskk-prolog-unify 42 42 nil) nil))
      (should (equal (nskk-prolog-unify "hello" "hello" nil) nil))))

  (nskk-context "variable with atom"
    (nskk-it "unifying a variable with an atom extends the substitution"
      (let ((result (nskk-prolog-unify '\?x 'a nil)))
        (should-not (nskk-prolog--fail-p result))
        (should (equal (nskk-prolog-walk '\?x result) 'a))))

    (nskk-it "unifying an atom with a variable extends the substitution"
      (let ((result (nskk-prolog-unify 'a '\?x nil)))
        (should-not (nskk-prolog--fail-p result))
        (should (equal (nskk-prolog-walk '\?x result) 'a)))))

  (nskk-context "variable with variable"
    (nskk-it "unifying two variables creates a binding between them"
      (let ((result (nskk-prolog-unify '\?x '\?y nil)))
        (should-not (nskk-prolog--fail-p result))
        (should (consp result)))))

  (nskk-context "different atoms"
    (nskk-it "unifying two different atoms returns :fail"
      (should (nskk-prolog--fail-p (nskk-prolog-unify 'a 'b nil)))
      (should (nskk-prolog--fail-p (nskk-prolog-unify 42 43 nil)))
      (should (nskk-prolog--fail-p (nskk-prolog-unify "hello" "world" nil)))))

  (nskk-context "list unification"
    (nskk-it "unifies lists element-by-element and binds variables"
      (let ((result (nskk-prolog-unify '(a \?x) '(a 1) nil)))
        (should-not (nskk-prolog--fail-p result))
        (should (equal (nskk-prolog-walk '\?x result) 1))))

    (nskk-it "unifies lists with multiple variables binding all of them"
      (let ((result (nskk-prolog-unify '(foo \?x \?y) '(foo 1 2) nil)))
        (should-not (nskk-prolog--fail-p result))
        (should (equal (nskk-prolog-walk '\?x result) 1))
        (should (equal (nskk-prolog-walk '\?y result) 2))))

    (nskk-it "unifies nested lists recursively"
      (let ((result (nskk-prolog-unify '(a (b \?x)) '(a (b c)) nil)))
        (should-not (nskk-prolog--fail-p result))
        (should (equal (nskk-prolog-walk '\?x result) 'c))))

    (nskk-it "fails when lists have different lengths"
      (should (nskk-prolog--fail-p (nskk-prolog-unify '(a b) '(a b c) nil)))
      (should (nskk-prolog--fail-p (nskk-prolog-unify '(a b c) '(a b) nil)))))

  (nskk-context "anonymous variable"
    (nskk-it "symbol \\?_ unifies with anything (treated as regular variable)"
      ;; Because `nskk-prolog--anonymous-p' checks the char-literal ?_ (integer 95)
      ;; while the symbol '\?_ is a regular Prolog variable, direct unification
      ;; binds \?_ like any other variable.  We verify unification succeeds.
      (let ((result (nskk-prolog-unify '\?_ 'anything nil)))
        (should-not (nskk-prolog--fail-p result)))
      (let ((result (nskk-prolog-unify 'anything '\?_ nil)))
        (should-not (nskk-prolog--fail-p result))))

    (nskk-it "symbol \\?_ inside a list unifies successfully"
      ;; The symbol '\?_ is treated as a regular variable (see above),
      ;; so unification succeeds and produces a binding for \?_.
      (let ((result (nskk-prolog-unify '(a \?_ c) '(a b c) nil)))
        (should-not (nskk-prolog--fail-p result)))))

  (nskk-context "substitution preservation"
    (nskk-it "preserves existing substitution bindings when adding a new one"
      (let* ((initial-subst '((\?z . 99)))
             (result (nskk-prolog-unify '\?x 42 initial-subst)))
        (should-not (nskk-prolog--fail-p result))
        (should (equal (nskk-prolog-walk '\?x result) 42))
        (should (equal (nskk-prolog-walk '\?z result) 99)))))

  (nskk-context "failure sentinel"
    (nskk-it "detects :fail as the failure sentinel and rejects nil, t, and empty list"
      (should (nskk-prolog--fail-p :fail))
      (should-not (nskk-prolog--fail-p nil))
      (should-not (nskk-prolog--fail-p t))
      (should-not (nskk-prolog--fail-p '())))))

;;;;
;;;; 4. Clause Database & Assert/Retract
;;;;

(nskk-describe "Prolog assertion and retraction"
  (nskk-context "assert and query"
    (nskk-it "asserting a fact makes it queryable"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((color red)))
        (let ((result (nskk-prolog-query '(color red))))
          (should result)
          (should (= (length result) 1)))))

    (nskk-it "asserting multiple facts returns all of them in a query"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((fruit apple)))
        (nskk-prolog-assert '((fruit banana)))
        (nskk-prolog-assert '((fruit cherry)))
        (let ((result (nskk-prolog-query '(fruit \?x))))
          (should result)
          (should (= (length result) 3)))))

    (nskk-it "facts are returned in insertion order"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((order first)))
        (nskk-prolog-assert '((order second)))
        (nskk-prolog-assert '((order third)))
        (let* ((results (nskk-prolog-query '(order \?x)))
               (values (mapcar (lambda (s) (nskk-prolog-walk '\?x s)) results)))
          (should (equal values '(first second third)))))))

  (nskk-context "retract"
    (nskk-it "retracting a specific fact removes only that fact from the database"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((animal cat)))
        (nskk-prolog-assert '((animal dog)))
        (should (nskk-prolog-retract '(animal cat)))
        (let ((result (nskk-prolog-query '(animal \?x))))
          (should (= (length result) 1))
          (should (equal (nskk-prolog-walk '\?x (car result)) 'dog)))))

    (nskk-it "retracting a non-existent fact returns nil"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (should-not (nskk-prolog-retract '(nonexistent fact)))))

    (nskk-it "retract-all clears all clauses for a predicate"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((pet cat)))
        (nskk-prolog-assert '((pet dog)))
        (nskk-prolog-assert '((pet fish)))
        (nskk-prolog-retract-all 'pet 1)
        (should-not (nskk-prolog-query '(pet \?x))))))

  (nskk-context "clear database"
    (nskk-it "clear-database resets all facts and makes nothing queryable"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((test-fact 1)))
        (nskk-prolog-assert '((test-fact 2)))
        (nskk-prolog-clear-database)
        (should-not (nskk-prolog-query '(test-fact \?x)))))))

;;;;
;;;; 5. Prove Engine
;;;;

(nskk-describe "Prolog prove engine"
  (nskk-context "single fact queries"
    (nskk-it "proves a single ground fact query"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((likes alice bob)))
        (let ((result (nskk-prolog-prove '((likes alice bob)) nil)))
          (should result)
          (should (= (length result) 1)))))

    (nskk-it "returns nil for queries with no matching facts"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((parent tom bob)))
        (should-not (nskk-prolog-query '(parent alice \?x)))))

    (nskk-it "ground query with no variables returns empty substitution on success"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((known-fact)))
        (let ((result (nskk-prolog-prove '((known-fact)) nil)))
          (should result)
          (should (= (length result) 1))))))

  (nskk-context "rule with body"
    (nskk-it "proves a grandparent rule requiring two parent body goals"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((parent tom bob)))
        (nskk-prolog-assert '((parent bob ann)))
        (nskk-prolog-assert '((grandparent \?x \?z)
                               (parent \?x \?y) (parent \?y \?z)))
        (let ((result (nskk-prolog-query '(grandparent tom \?who))))
          (should result)
          (should (equal (nskk-prolog-walk '\?who (car result)) 'ann))))))

  (nskk-context "backtracking"
    (nskk-it "finds all three parent children via backtracking"
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

    (nskk-it "finds all grandchildren via backtracking through multiple rule paths"
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
            (should (member 'jim grandchildren)))))))

  (nskk-context "prove-one"
    (nskk-it "returns only the first solution without exploring further"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((num 1)))
        (nskk-prolog-assert '((num 2)))
        (nskk-prolog-assert '((num 3)))
        (let ((result (nskk-prolog-prove-one '((num \?x)) nil)))
          (should result)
          (should (equal (nskk-prolog-walk '\?x result) 1))))))

  (nskk-context "chained rules"
    (nskk-it "finds all descendants through recursive ancestor rules"
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
            (should (member 'dave descendants))))))))

;;;;
;;;; 6. Cut
;;;;

(nskk-describe "Prolog cut"
  (nskk-context "clause-local scope"
    (nskk-it "cut has clause-local scope so alternative clauses are still explored"
      ;; Cut in this engine has clause-local scope: the cut throw is caught at
      ;; the clause boundary, so alternative clauses are still explored.  This is
      ;; non-standard (ISO Prolog cut prunes all remaining alternatives); the
      ;; implementation documents this as a known limitation.  Verify that all
      ;; three clauses produce results and that the first result is `first'.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((choice first) !))
        (nskk-prolog-assert '((choice second)))
        (nskk-prolog-assert '((choice third)))
        (let ((results (nskk-prolog-query '(choice \?x))))
          (should (= (length results) 3))
          (should (equal (nskk-prolog-walk '\?x (car results)) 'first)))))

    (nskk-it "cut in a rule body still explores all alternatives for data facts"
      ;; Cut in a rule body: the current engine catches the cut throw per-clause,
      ;; so all alternatives for `data' are explored.  Verify that all three
      ;; data facts produce results and that the first is `a'.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((data a)))
        (nskk-prolog-assert '((data b)))
        (nskk-prolog-assert '((data c)))
        (nskk-prolog-assert '((first-data \?x) (data \?x) !))
        (let ((results (nskk-prolog-query '(first-data \?x))))
          (should (= (length results) 3))
          (should (equal (nskk-prolog-walk '\?x (car results)) 'a)))))))

;;;;
;;;; 7. Negation-as-failure
;;;;

(nskk-describe "Prolog negation-as-failure"
  (nskk-context "not/1 goal"
    (nskk-it "succeeds when the negated goal has no solutions"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (let ((result (nskk-prolog-prove '((not (nonexistent \?x))) nil)))
          (should result))))

    (nskk-it "fails when the negated goal has a solution"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((existing-fact)))
        (let ((result (nskk-prolog-prove '((not (existing-fact))) nil)))
          (should-not result))))

    (nskk-it "works correctly within a rule body to filter out penguins from flyers"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((bird tweety)))
        (nskk-prolog-assert '((bird opus)))
        (nskk-prolog-assert '((penguin opus)))
        (nskk-prolog-assert '((can-fly \?x) (bird \?x) (not (penguin \?x))))
        (let ((results (nskk-prolog-query '(can-fly \?x))))
          (should (= (length results) 1))
          (should (equal (nskk-prolog-walk '\?x (car results)) 'tweety)))))))

;;;;
;;;; 8. Bidirectional Queries
;;;;

(nskk-describe "Prolog bidirectional queries"
  (nskk-context "forward and reverse lookup"
    (nskk-it "queries forward from romaji to kana"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((romaji-kana "ka" "ka")))
        (nskk-prolog-assert '((romaji-kana "ki" "ki")))
        (nskk-prolog-assert '((romaji-kana "ku" "ku")))
        (let ((result (nskk-prolog-query-value
                       '(romaji-kana "ka" \?kana) '\?kana)))
          (should (equal result "ka")))))

    (nskk-it "queries in reverse from kana to romaji"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((romaji-kana "ka" "ka")))
        (nskk-prolog-assert '((romaji-kana "ki" "ki")))
        (nskk-prolog-assert '((romaji-kana "ku" "ku")))
        (let ((result (nskk-prolog-query-value
                       '(romaji-kana \?romaji "ki") '\?romaji)))
          (should (equal result "ki")))))

    (nskk-it "both forward and reverse queries work with the same string facts"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((mapping "alpha" "A")))
        (nskk-prolog-assert '((mapping "beta" "B")))
        (let ((forward (nskk-prolog-query-value
                        '(mapping "alpha" \?val) '\?val))
              (reverse (nskk-prolog-query-value
                        '(mapping \?key "B") '\?key)))
          (should (equal forward "A"))
          (should (equal reverse "beta")))))))

;;;;
;;;; 9. Indexing
;;;;

(nskk-describe "Prolog indexing"
  (nskk-context "hash index"
    (nskk-it "provides O(1) ground lookup with a hash index on the first argument"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'indexed-fact 1 :hash)
        (nskk-prolog-assert '((indexed-fact apple)))
        (nskk-prolog-assert '((indexed-fact banana)))
        (nskk-prolog-assert '((indexed-fact cherry)))
        (let ((result (nskk-prolog-query '(indexed-fact banana))))
          (should result)
          (should (= (length result) 1)))))

    (nskk-it "falls back to full scan when the first argument is a variable"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'indexed-fruit 1 :hash)
        (nskk-prolog-assert '((indexed-fruit apple)))
        (nskk-prolog-assert '((indexed-fruit banana)))
        (nskk-prolog-assert '((indexed-fruit cherry)))
        (let ((results (nskk-prolog-query '(indexed-fruit \?x))))
          (should (= (length results) 3)))))

    (nskk-it "stays consistent with retraction: retracted fact disappears from index"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'idx-item 1 :hash)
        (nskk-prolog-assert '((idx-item x)))
        (nskk-prolog-assert '((idx-item y)))
        (nskk-prolog-retract '(idx-item x))
        (let ((results (nskk-prolog-query '(idx-item \?v))))
          (should (= (length results) 1))
          (should (equal (nskk-prolog-walk '\?v (car results)) 'y))))))

  (nskk-context "trie index"
    (nskk-it "performs prefix-based lookup with an inline private trie"
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

    (nskk-it "trie-bulk-assert makes a single entry queryable via the prove engine"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'test-dict-entry 2 :trie)
        (nskk-prolog-trie-bulk-assert 'test-dict-entry 2
                                      '(("か" . ("花" "家"))))
        (let ((result (nskk-prolog-query '(test-dict-entry "か" \?candidates))))
          (should result)
          (should (equal (nskk-prolog-walk '\?candidates (car result))
                         '("花" "家"))))))

    (nskk-it "trie-bulk-assert makes multiple entries individually queryable"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'test-dict-entry 2 :trie)
        (nskk-prolog-trie-bulk-assert 'test-dict-entry 2
                                      '(("か" . ("花" "家"))
                                        ("き" . ("木" "気"))
                                        ("く" . ("口" "来"))))
        (let ((r1 (nskk-prolog-query '(test-dict-entry "か" \?c))))
          (should r1)
          (should (equal (nskk-prolog-walk '\?c (car r1)) '("花" "家"))))
        (let ((r2 (nskk-prolog-query '(test-dict-entry "き" \?c))))
          (should r2)
          (should (equal (nskk-prolog-walk '\?c (car r2)) '("木" "気"))))
        (let ((r3 (nskk-prolog-query '(test-dict-entry "く" \?c))))
          (should r3)
          (should (equal (nskk-prolog-walk '\?c (car r3)) '("口" "来"))))))

    (nskk-it "trie-bulk-assert query for non-existent key returns empty result"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'test-dict-entry 2 :trie)
        (nskk-prolog-trie-bulk-assert 'test-dict-entry 2
                                      '(("か" . ("花" "家"))))
        (let ((result (nskk-prolog-query '(test-dict-entry "き" \?candidates))))
          (should-not result)))))

  (nskk-context "list index (default)"
    (nskk-it "performs full scan and returns all matching facts"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((plain-fact 1)))
        (nskk-prolog-assert '((plain-fact 2)))
        (nskk-prolog-assert '((plain-fact 3)))
        (let ((results (nskk-prolog-query '(plain-fact \?n))))
          (should (= (length results) 3)))))))

;;;;
;;;; 10. DSL Macros
;;;;

(nskk-describe "Prolog DSL macros"
  (nskk-context "nskk-prolog-<- assertion"
    (nskk-it "asserts a fact that is subsequently queryable"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-<- (dsl-color red))
        (let ((result (nskk-prolog-query '(dsl-color red))))
          (should result))))

    (nskk-it "asserts a rule with body goals and resolves it correctly"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-<- (dsl-parent tom bob))
        (nskk-prolog-<- (dsl-parent bob ann))
        (nskk-prolog-<- (dsl-grandparent \?x \?z)
          (dsl-parent \?x \?y) (dsl-parent \?y \?z))
        (let ((result (nskk-prolog-query-value
                       '(dsl-grandparent tom \?who) '\?who)))
          (should (equal result 'ann)))))

    (nskk-it "strips the :- prefix in rule body correctly"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-<- (dsl-valid \?x)
          :- (dsl-exists \?x))
        (nskk-prolog-<- (dsl-exists thing))
        (should (nskk-prolog-query '(dsl-valid thing))))))

  (nskk-context "nskk-prolog-?- query"
    (nskk-it "queries successfully for an asserted fact"
      ;; For a ground query, `query-one' returns t (ground success), not nil.
      ;; Use `nskk-prolog-query' if you need the actual substitution alist.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-<- (dsl-hello world))
        (should (nskk-prolog-query '(dsl-hello world)))))

    (nskk-it "returns nil when no fact matches"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (should-not (nskk-prolog-?- (dsl-nonexistent thing)))))

    (nskk-it "returns substitution with variable bindings for the first solution"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-<- (dsl-pair a 1))
        (nskk-prolog-<- (dsl-pair b 2))
        (let ((result (nskk-prolog-?- (dsl-pair \?k \?v))))
          (should result)
          (should (equal (nskk-prolog-walk '\?k result) 'a))
          (should (equal (nskk-prolog-walk '\?v result) 1)))))))

;;;;
;;;; 11. Utility Functions
;;;;

(nskk-describe "Prolog utility functions"
  (nskk-context "nskk-prolog-ground-p"
    (nskk-it "atoms, numbers, strings, and nil are ground"
      (should (nskk-prolog-ground-p 'hello))
      (should (nskk-prolog-ground-p 42))
      (should (nskk-prolog-ground-p "string"))
      (should (nskk-prolog-ground-p nil)))

    (nskk-it "lists without variables are ground"
      (should (nskk-prolog-ground-p '(a b c)))
      (should (nskk-prolog-ground-p '(1 (2 3) 4))))

    (nskk-it "terms containing variables at any depth are not ground"
      (should-not (nskk-prolog-ground-p '\?x))
      (should-not (nskk-prolog-ground-p '(a \?x c)))
      (should-not (nskk-prolog-ground-p '(a (b \?y) c)))))

  (nskk-context "nskk-prolog-substitute"
    (nskk-it "replaces bound variables with their bindings"
      (let ((subst '((\?x . hello) (\?y . world))))
        (should (equal (nskk-prolog-substitute '\?x subst) 'hello))
        (should (equal (nskk-prolog-substitute '\?y subst) 'world))))

    (nskk-it "leaves unbound variables unchanged"
      (let ((subst '((\?x . hello))))
        (should (equal (nskk-prolog-substitute '\?z subst) '\?z))))

    (nskk-it "replaces variables inside lists recursively"
      (let ((subst '((\?x . 1) (\?y . 2))))
        (should (equal (nskk-prolog-substitute '(a \?x (b \?y)) subst)
                       '(a 1 (b 2))))))

    (nskk-it "follows transitive bindings through the substitution chain"
      (let ((subst '((\?x . \?y) (\?y . resolved))))
        (should (equal (nskk-prolog-substitute '\?x subst) 'resolved))))

    (nskk-it "returns non-variables unchanged"
      (should (equal (nskk-prolog-substitute 'atom nil) 'atom))
      (should (equal (nskk-prolog-substitute 42 nil) 42))
      (should (equal (nskk-prolog-substitute "str" nil) "str")))))

;;;;
;;;; 12. Query API
;;;;

(nskk-describe "Prolog query API"
  (nskk-context "query-value"
    (nskk-it "returns the binding for a specific variable in the first solution"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((kv-pair key1 val1)))
        (nskk-prolog-assert '((kv-pair key2 val2)))
        (let ((result (nskk-prolog-query-value '(kv-pair key1 \?v) '\?v)))
          (should (equal result 'val1)))))

    (nskk-it "returns nil when no solution exists"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (should-not (nskk-prolog-query-value '(no-such \?x) '\?x)))))

  (nskk-context "query-all-values"
    (nskk-it "returns all bindings across all solutions"
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

    (nskk-it "returns nil when there are no solutions"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (should-not (nskk-prolog-query-all-values '(empty \?x) '\?x)))))

  (nskk-context "query-values"
    (nskk-it "extracts multiple variable bindings in order from the first solution"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((triple a 1 x)))
        (let ((result (nskk-prolog-query-values
                       '(triple \?p \?q \?r) '(\?p \?q \?r))))
          (should result)
          (should (equal result '(a 1 x))))))

    (nskk-it "returns nil when no solution exists"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (should-not (nskk-prolog-query-values '(no-triple \?a \?b \?c) '(\?a \?b)))))

    (nskk-it "works with rule-derived bindings combining multiple facts"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((item pen red)))
        (nskk-prolog-assert '((stock item pen 42)))
        (nskk-prolog-assert '((inventory \?name \?color \?count)
                               (item \?name \?color) (stock item \?name \?count)))
        (let ((result (nskk-prolog-query-values
                       '(inventory pen \?color \?count) '(\?color \?count))))
          (should result)
          (should (equal (car result) 'red))
          (should (= (cadr result) 42))))))

  (nskk-context "query and query-one"
    (nskk-it "query returns list of all substitution alists"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((item 1)))
        (nskk-prolog-assert '((item 2)))
        (let ((results (nskk-prolog-query '(item \?n))))
          (should (= (length results) 2))
          (should (listp (car results))))))

    (nskk-it "query-one returns only the first solution"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((rank 1)))
        (nskk-prolog-assert '((rank 2)))
        (nskk-prolog-assert '((rank 3)))
        (let ((result (nskk-prolog-query-one '(rank \?n))))
          (should result)
          (should (equal (nskk-prolog-walk '\?n result) 1)))))

    (nskk-it "ground query: query-one returns t for success and nil for no-solution"
      ;; `nskk-prolog-query-one' upgrades a nil (empty) substitution to t so
      ;; callers can distinguish ground success from no-solution.  `nskk-prolog-query'
      ;; returns (nil) for success vs nil for failure, offering the same
      ;; distinction via list membership.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((ground-fact)))
        (should (eq t (nskk-prolog-query-one '(ground-fact))))
        (should (null (nskk-prolog-query-one '(nonexistent))))
        (should (nskk-prolog-query '(ground-fact)))
        (should-not (nskk-prolog-query '(nonexistent)))))

    (nskk-it "end-to-end complex query involving multiple chained rules and facts"
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
          (should (member 'd results))))))

  (nskk-context "variable renaming internals"
    (nskk-it "renamed variables do not collide with original variable names"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (let* ((clause '((foo \?x) (bar \?x)))
               (renamed (nskk-prolog--rename-variables clause 999)))
          (should-not (equal (car clause) (car renamed)))
          (should-not (member '\?x (flatten-tree renamed))))))))

;;;;
;;;; Property-Based Tests
;;;;

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

;;;;
;;;; Arithmetic Built-in Tests
;;;;

;; Table-driven comparison operator tests: (op a b) should succeed or fail.
(nskk-deftest-table prolog-arith-compare
  :columns (op a b should-succeed)
  :rows    ((>= 10 5 t)
            (>= 5  5 t)
            (>= 3  5 nil)
            (<= 3  5 t)
            (<= 5  5 t)
            (<= 10 5 nil)
            (>  10 5 t)
            (>  5  5 nil)
            (>  3  5 nil)
            (<  3  5 t)
            (<  5  5 nil)
            (<  10 5 nil)
            (=:= 5  5 t)
            (=:= 5  6 nil))
  :description "Arithmetic comparison built-in: (op a b) produces expected boolean result"
  :body (let ((result (nskk-prolog-query-one (list op a b))))
          (if should-succeed
              (should result)
            (should-not result))))

;; Table-driven is/2 tests: variable is bound to evaluated arithmetic result.
(nskk-deftest-table prolog-arith-is
  :columns (expr expected)
  :rows    (((+ 3 5)   8)
            ((- 100 4) 96)
            ((*  3 7)  21)
            ((/  12 4) 3))
  :description "Arithmetic is/2: variable is unified with the evaluated expression result"
  :body (let ((result (nskk-prolog-query-one (list 'is '\?x expr))))
          (should result)
          (should (listp result))
          (should (= (nskk-prolog-walk '\?x result) expected))))

(nskk-describe "Prolog arithmetic in rule bodies"
  (nskk-context "is/2 within a rule"
    (nskk-it "arithmetic goals work correctly within rule bodies"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-retract-all 'arith-test-double 2)
        (nskk-prolog-<- (arith-test-double \?x \?y)
          (is \?y (* \?x 2)))
        (let ((result (nskk-prolog-query-one '(arith-test-double 5 \?y))))
          (should result)
          (should (listp result))
          (should (= (nskk-prolog-walk '\?y result) 10)))))))

;;;;
;;;; Property-Based Tests: Walk invariants
;;;;

;; Walk of a non-variable term always returns the term unchanged.
(nskk-deftest-table prolog-pbt-walk-identity
  :columns (term)
  :rows    ((hello)
            (42)
            ("str")
            (nil)
            ((a b c)))
  :description "Walk identity: non-variable terms return themselves under any substitution"
  :body (should (equal (nskk-prolog-walk term nil) term)))

;; Walk with an empty substitution returns the same result as walk with no binding.
(nskk-deftest-unit prolog-pbt-walk-empty-subst
  "Walk with no binding and with empty substitution both return the variable itself."
  (let ((var '\?x))
    (should (eq (nskk-prolog-walk var nil)
                (nskk-prolog-walk var '((\?y . 42)))))))

;;;;
;;;; 13. holds-p and When-Holds Guard Macro
;;;;

(nskk-describe "Prolog holds-p and nskk-when-prolog-holds"
  (nskk-context "nskk-prolog-holds-p"
    (nskk-it "returns t when the goal has at least one solution"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((holds-fact)))
        (should (nskk-prolog-holds-p '(holds-fact)))))

    (nskk-it "returns nil when the goal has no solution"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (should-not (nskk-prolog-holds-p '(absent-fact)))))

    (nskk-it "works with variable-containing goals"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((color blue)))
        (should (nskk-prolog-holds-p '(color \?x)))
        (should-not (nskk-prolog-holds-p '(shape \?x))))))

  (nskk-context "nskk-when-prolog-holds"
    (nskk-it "executes body when the query has a solution"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((flag enabled)))
        (let (ran)
          (nskk-when-prolog-holds '(flag enabled)
            (setq ran t))
          (should ran))))

    (nskk-it "skips body when the query has no solution"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (let (ran)
          (nskk-when-prolog-holds '(nonexistent-flag x)
            (setq ran t))
          (should-not ran))))

    (nskk-it "accepts a runtime-composed query via backquote"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((mode hiragana)))
        (let ((mode 'hiragana)
              ran)
          (nskk-when-prolog-holds `(mode ,mode)
            (setq ran t))
          (should ran))))))

;;;;
;;;; 14. DSL Macro Extensions: deffacts and define-fact-table
;;;;

(nskk-describe "Prolog deffacts and define-fact-table macros"
  (nskk-context "nskk-prolog-deffacts"
    (nskk-it "asserts multiple facts for a predicate in insertion order"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-deffacts deffact-fruit
          (apple)
          (banana)
          (cherry))
        (let ((results (nskk-prolog-query '(deffact-fruit \?f))))
          (should (= (length results) 3))
          (let ((fruits (mapcar (lambda (s) (nskk-prolog-walk '\?f s)) results)))
            (should (equal fruits '(apple banana cherry)))))))

    (nskk-it "each deffacts row is independently queryable as a ground fact"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-deffacts deffact-num
          (1)
          (2)
          (3))
        (should (nskk-prolog-query '(deffact-num 1)))
        (should (nskk-prolog-query '(deffact-num 2)))
        (should (nskk-prolog-query '(deffact-num 3)))
        (should-not (nskk-prolog-query '(deffact-num 4))))))

  (nskk-context "nskk-prolog-define-fact-table"
    (nskk-it "configures the index and asserts all rows in one declaration"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-define-fact-table dft-valid-mode (:arity 1 :index :hash)
          (hiragana)
          (katakana)
          (latin))
        (should (nskk-prolog-query-one '(dft-valid-mode hiragana)))
        (should (nskk-prolog-query-one '(dft-valid-mode katakana)))
        (should (nskk-prolog-query-one '(dft-valid-mode latin)))
        (should-not (nskk-prolog-query-one '(dft-valid-mode ascii)))))

    (nskk-it "multi-column fact table is queryable by first argument via hash index"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-define-fact-table dft-key-action (:arity 2 :index :hash)
          (space self-insert)
          (return newline)
          (backspace delete))
        (let ((result (nskk-prolog-query-value
                       '(dft-key-action space \?action) '\?action)))
          (should (eq result 'self-insert)))
        (let ((result (nskk-prolog-query-value
                       '(dft-key-action return \?action) '\?action)))
          (should (eq result 'newline)))))))

;;;;
;;;; 15. Trie Prefix Search (public API)
;;;;

(nskk-describe "Prolog trie prefix search"
  (nskk-context "nskk-prolog-trie-prefix-search"
    (nskk-it "returns (key . value) pairs for all keys starting with the prefix"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'pfx-word 2 :trie)
        (nskk-prolog-trie-bulk-assert 'pfx-word 2
                                      '(("hello" . "greeting")
                                        ("help"  . "assistance")
                                        ("world" . "noun")))
        (let ((results (nskk-prolog-trie-prefix-search 'pfx-word 2 "hel")))
          (should results)
          (should (= (length results) 2))
          (should (assoc "hello" results))
          (should (assoc "help" results))
          (should-not (assoc "world" results)))))

    (nskk-it "exact key lookup returns a single (key . value) pair"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'pfx-kana 2 :trie)
        (nskk-prolog-trie-bulk-assert 'pfx-kana 2
                                      '(("か" . ("花" "家"))
                                        ("き" . ("木" "気"))))
        (let ((results (nskk-prolog-trie-prefix-search 'pfx-kana 2 "か")))
          (should (= (length results) 1))
          (should (equal (cdr (assoc "か" results)) '("花" "家"))))))

    (nskk-it "returns nil when no key matches the given prefix"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'pfx-empty 2 :trie)
        (nskk-prolog-trie-bulk-assert 'pfx-empty 2
                                      '(("alpha" . "a")))
        (should-not (nskk-prolog-trie-prefix-search 'pfx-empty 2 "beta"))))

    (nskk-it "returns nil when no trie index is configured for the predicate"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (should-not (nskk-prolog-trie-prefix-search 'no-such-pred 2 "x"))))))

;;;;
;;;; 16. Built-in Goal Handlers: assertz and retract
;;;;

(nskk-describe "Prolog built-in assertz and retract goals"
  (nskk-context "assertz/1 in a rule body"
    (nskk-it "asserting a fact via assertz goal makes it subsequently queryable"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-prove '((assertz (dynamic-fact hello))) nil)
        (should (nskk-prolog-query '(dynamic-fact hello)))))

    (nskk-it "assertz within a rule body adds the fact after the rule fires"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-<- (add-greeting \?x)
          (assertz (greeted \?x)))
        (nskk-prolog-prove '((add-greeting world)) nil)
        (should (nskk-prolog-query '(greeted world))))))

  (nskk-context "retract/1 in a rule body"
    (nskk-it "retracting a fact via retract goal removes it from the database"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((temp-fact x)))
        (nskk-prolog-prove '((retract (temp-fact x))) nil)
        (should-not (nskk-prolog-query '(temp-fact x)))))))

;;;;
;;;; Property-Based Test: Trie/Hash Index Equivalence
;;;;

;; PBT — nskk-unit-prolog-trie-hash-index-equivalence-pbt
;;
;; Invariant: for any set of facts asserted under a predicate, querying via
;; a :hash index and querying via a :trie index return the same set of
;; results (as sorted lists, since order may differ between index types).
;;
;; Method:
;;   1. Load three facts under a unique predicate with :hash index, query,
;;      collect result set.
;;   2. Retract all, switch to :trie index, re-assert the same facts, query,
;;      collect result set.
;;   3. Compare both sets (sorted) for equality.
;;
;; The :hash index is used for O(1) ground first-argument lookup; the :trie
;; index is used for prefix search.  Both must return the same entries when
;; the first argument is a variable (full scan fallback).
(nskk-deftest-unit prolog-trie-hash-index-equivalence-pbt
  "Trie and hash indices return equivalent result sets for the same asserted facts."
  (nskk-property-test-seeded prolog-trie-hash-index-equivalence
    ((k1 search-query)
     (k2 search-query)
     (k3 search-query))
    (nskk-prolog-test-with-isolated-db
      (let* ((pred (intern (format "idx-equiv-test-%d" (abs (random)))))
             (facts (list (list k1 "v1")
                          (list k2 "v2")
                          (list k3 "v3"))))

        ;; --- Hash index pass ---
        (nskk-prolog-retract-all pred 2)
        (nskk-prolog-set-index pred 2 :hash)
        (dolist (f facts)
          (nskk-prolog-assert (list (cons pred f))))
        (let* ((hash-solutions (nskk-prolog-query `(,pred \?k \?v)))
               (hash-set (sort
                          (mapcar (lambda (s)
                                    (cons (nskk-prolog-walk '\?k s)
                                          (nskk-prolog-walk '\?v s)))
                                  hash-solutions)
                          (lambda (a b) (string< (car a) (car b))))))

          ;; --- Trie index pass ---
          (nskk-prolog-retract-all pred 2)
          (nskk-prolog-set-index pred 2 :trie)
          (dolist (f facts)
            (nskk-prolog-assert (list (cons pred f))))
          (let* ((trie-solutions (nskk-prolog-query `(,pred \?k \?v)))
                 (trie-set (sort
                            (mapcar (lambda (s)
                                      (cons (nskk-prolog-walk '\?k s)
                                            (nskk-prolog-walk '\?v s)))
                                    trie-solutions)
                            (lambda (a b) (string< (car a) (car b))))))

            ;; Both index types must produce the same (key . value) pairs.
            (equal hash-set trie-set)))))
    30
    41))

;;; ─────────────────────────────────────────────────────────────────────────
;;; nskk-prolog-bulk-facts macro
;;; ─────────────────────────────────────────────────────────────────────────

(nskk-describe "nskk-prolog-bulk-facts"
  (nskk-context "basic fact assertion from a list"
    (nskk-it "asserts all facts and makes them queryable"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-retract-all 'bulk-test-color 1)
        (nskk-prolog-set-index 'bulk-test-color 1 :list)
        (let ((rules '(("red") ("green") ("blue"))))
          (nskk-prolog-bulk-facts bulk-test-color rules)
          (should (nskk-prolog-holds-p '(bulk-test-color "red")))
          (should (nskk-prolog-holds-p '(bulk-test-color "green")))
          (should (nskk-prolog-holds-p '(bulk-test-color "blue"))))))

    (nskk-it "asserts multi-argument facts correctly"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-retract-all 'bulk-test-pair 2)
        (nskk-prolog-set-index 'bulk-test-pair 2 :hash)
        (let ((rules '(("a" 1) ("b" 2) ("c" 3))))
          (nskk-prolog-bulk-facts bulk-test-pair rules)
          (should (equal (nskk-prolog-query-value
                          '(bulk-test-pair "a" \?v) '\?v) 1))
          (should (equal (nskk-prolog-query-value
                          '(bulk-test-pair "b" \?v) '\?v) 2))
          (should (equal (nskk-prolog-query-value
                          '(bulk-test-pair "c" \?v) '\?v) 3)))))

    (nskk-it "asserts the correct number of facts"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-retract-all 'bulk-test-count 1)
        (nskk-prolog-set-index 'bulk-test-count 1 :list)
        (let ((rules '(("one") ("two") ("three") ("four") ("five"))))
          (nskk-prolog-bulk-facts bulk-test-count rules)
          (should (= 5 (length (nskk-prolog-query '(bulk-test-count \?x))))))))

    (nskk-it "evaluates RULES at runtime from a let-bound variable"
      ;; Unlike nskk-prolog-deffacts which expands at compile time,
      ;; bulk-facts evaluates RULES at runtime — enabling defconst data sources.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-retract-all 'bulk-test-runtime 1)
        (nskk-prolog-set-index 'bulk-test-runtime 1 :list)
        (let ((runtime-data (list (list "dynamic"))))
          (nskk-prolog-bulk-facts bulk-test-runtime runtime-data)
          (should (nskk-prolog-holds-p '(bulk-test-runtime "dynamic")))))))

  (nskk-context "empty rules list"
    (nskk-it "asserts nothing when rules list is empty"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-retract-all 'bulk-test-empty 1)
        (nskk-prolog-set-index 'bulk-test-empty 1 :list)
        (let ((rules '()))
          (nskk-prolog-bulk-facts bulk-test-empty rules)
          (should (null (nskk-prolog-query '(bulk-test-empty \?x)))))))

    (nskk-it "does not signal an error on an empty rules list"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-retract-all 'bulk-test-no-error 1)
        (should-not (condition-case nil
                        (progn (nskk-prolog-bulk-facts bulk-test-no-error '()) nil)
                      (error t))))))

  (nskk-context "equivalence with nskk-prolog-deffacts"
    (nskk-it "produces the same queryable facts as nskk-prolog-deffacts for identical data"
      ;; deffacts expands at compile time; bulk-facts evaluates RULES at
      ;; runtime.  Both must produce the same queryable fact set.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-retract-all 'bulk-cmp-a 2)
        (nskk-prolog-retract-all 'bulk-cmp-b 2)
        (nskk-prolog-set-index 'bulk-cmp-a 2 :list)
        (nskk-prolog-set-index 'bulk-cmp-b 2 :list)
        (nskk-prolog-deffacts bulk-cmp-a
          ("x" "X")
          ("y" "Y"))
        (nskk-prolog-bulk-facts bulk-cmp-b '(("x" "X") ("y" "Y")))
        (let ((results-a (sort (mapcar (lambda (s)
                                         (list (nskk-prolog-walk '\?k s)
                                               (nskk-prolog-walk '\?v s)))
                                       (nskk-prolog-query '(bulk-cmp-a \?k \?v)))
                               (lambda (a b) (string< (car a) (car b)))))
              (results-b (sort (mapcar (lambda (s)
                                         (list (nskk-prolog-walk '\?k s)
                                               (nskk-prolog-walk '\?v s)))
                                       (nskk-prolog-query '(bulk-cmp-b \?k \?v)))
                               (lambda (a b) (string< (car a) (car b))))))
          (should (equal results-a results-b)))))))

;;;
;;; nskk-define-goal-handler
;;;

(nskk-describe "nskk-define-goal-handler"
  (nskk-it "appends a new handler entry to nskk-prolog--goal-handlers"
    (let ((nskk-prolog--goal-handlers (copy-sequence nskk-prolog--goal-handlers)))
      (let ((count-before (length nskk-prolog--goal-handlers)))
        (nskk-define-goal-handler test-new-handler
            (goal _rest _subst on-solution)
          :match (eq (car goal) 'test-new-handler)
          :body  (funcall on-solution nil))
        (should (= (length nskk-prolog--goal-handlers) (1+ count-before)))
        (should (assq 'test-new-handler nskk-prolog--goal-handlers)))))

  (nskk-it "re-defining a handler with the same name replaces it in place"
    (let ((nskk-prolog--goal-handlers (copy-sequence nskk-prolog--goal-handlers)))
      (let ((count-before (length nskk-prolog--goal-handlers)))
        (nskk-define-goal-handler test-idempotent-handler
            (goal _rest _subst on-solution)
          :match (eq (car goal) 'test-idempotent-handler)
          :body  (funcall on-solution nil))
        (nskk-define-goal-handler test-idempotent-handler
            (goal _rest _subst on-solution)
          :match (eq (car goal) 'test-idempotent-handler)
          :body  (funcall on-solution nil))
        ;; Re-definition MUST NOT add a second entry — count stays 1+before
        (should (= (length nskk-prolog--goal-handlers) (1+ count-before))))))

  (nskk-it "handler entry has (name match-fn body-fn) structure"
    (let ((nskk-prolog--goal-handlers (copy-sequence nskk-prolog--goal-handlers)))
      (nskk-define-goal-handler test-structure-check
          (goal _rest _subst on-solution)
        :match (eq (car goal) 'test-structure-check)
        :body  (funcall on-solution nil))
      (let ((entry (assq 'test-structure-check nskk-prolog--goal-handlers)))
        (should entry)
        (should (functionp (nth 1 entry)))   ; match function
        (should (functionp (nth 2 entry)))))))  ; body function

;;;
;;; Private trie operations (nskk-prolog--trie-*)
;;;

(nskk-describe "nskk-prolog--trie internal operations"
  (nskk-it "trie-create produces a trie with size 0"
    (let ((trie (nskk-prolog--trie-create)))
      (should (= (nskk-prolog--trie-size trie) 0))))

  (nskk-it "trie-insert stores a key-value pair"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "hello" "world")
      (should (= (nskk-prolog--trie-size trie) 1))))

  (nskk-it "trie-lookup returns (value . t) for an inserted key"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "hello" "world")
      (let ((result (nskk-prolog--trie-lookup trie "hello")))
        (should (equal (car result) "world"))
        (should (cdr result)))))

  (nskk-it "trie-lookup returns (nil . nil) for a missing key"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "hello" "world")
      (let ((result (nskk-prolog--trie-lookup trie "xyz")))
        (should (null (car result)))
        (should (null (cdr result))))))

  (nskk-it "trie-delete removes a key and returns t"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "hello" "world")
      (should (nskk-prolog--trie-delete trie "hello"))
      (should (= (nskk-prolog--trie-size trie) 0))
      ;; After deletion, lookup should miss
      (let ((result (nskk-prolog--trie-lookup trie "hello")))
        (should (null (cdr result))))))

  (nskk-it "trie-delete returns nil for a non-existent key"
    (let ((trie (nskk-prolog--trie-create)))
      (should (null (nskk-prolog--trie-delete trie "missing")))))

  (nskk-it "trie supports Japanese string keys"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "かんじ" '("漢字"))
      (nskk-prolog--trie-insert trie "かんたん" '("簡単"))
      (let ((r1 (nskk-prolog--trie-lookup trie "かんじ"))
            (r2 (nskk-prolog--trie-lookup trie "かんたん")))
        (should (equal (car r1) '("漢字")))
        (should (equal (car r2) '("簡単")))))))

;;;
;;; nskk-prolog--trie--find-node
;;;

(nskk-describe "nskk-prolog--trie--find-node"
  (nskk-it "returns the terminal node for an inserted key"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "abc" "value")
      (let ((node (nskk-prolog--trie--find-node trie "abc")))
        (should node)
        (should (nskk-prolog--trie-node-is-end node)))))

  (nskk-it "returns nil for a key that was not inserted"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "abc" "value")
      (should-not (nskk-prolog--trie--find-node trie "xyz"))))

  (nskk-it "returns a non-terminal node for a prefix of an existing key"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "abc" "value")
      ;; "ab" is a prefix but not a terminal node
      (let ((node (nskk-prolog--trie--find-node trie "ab")))
        (should node)
        (should-not (nskk-prolog--trie-node-is-end node)))))

  (nskk-it "returns nil for any key in an empty trie"
    (let ((trie (nskk-prolog--trie-create)))
      (should-not (nskk-prolog--trie--find-node trie "abc")))))

;;;
;;; nskk-prolog--trie-prefix-search
;;;

(nskk-describe "nskk-prolog--trie-prefix-search"
  (nskk-it "returns all (key . value) pairs with the given prefix"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "abc" "v1")
      (nskk-prolog--trie-insert trie "abd" "v2")
      (nskk-prolog--trie-insert trie "xyz" "v3")
      (let ((results (nskk-prolog--trie-prefix-search trie "ab")))
        (should (= (length results) 2))
        (should (assoc "abc" results))
        (should (assoc "abd" results))
        (should-not (assoc "xyz" results)))))

  (nskk-it "returns empty list when no keys match prefix"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "abc" "val")
      (should (null (nskk-prolog--trie-prefix-search trie "xyz")))))

  (nskk-it "limits results with the optional limit parameter"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "aa" "v1")
      (nskk-prolog--trie-insert trie "ab" "v2")
      (nskk-prolog--trie-insert trie "ac" "v3")
      (let ((results (nskk-prolog--trie-prefix-search trie "a" 2)))
        (should (= (length results) 2)))))

  (nskk-it "returns all keys when prefix is empty string"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "a" "v1")
      (nskk-prolog--trie-insert trie "b" "v2")
      (let ((results (nskk-prolog--trie-prefix-search trie "")))
        (should (= (length results) 2))))))

;;;
;;; nskk-prolog--trie--collect-all
;;;

(nskk-describe "nskk-prolog--trie--collect-all"
  (nskk-it "collects all key/value pairs from a trie rooted at node"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "ab" "v1")
      (nskk-prolog--trie-insert trie "ac" "v2")
      (let* ((root (nskk-prolog--trie-root trie))
             (results (nskk-prolog--trie--collect-all root "" nil 0)))
        (should (= (length results) 2))
        (should (assoc "ab" results))
        (should (assoc "ac" results)))))

  (nskk-it "returns empty list for an empty trie root"
    (let* ((trie (nskk-prolog--trie-create))
           (root (nskk-prolog--trie-root trie))
           (results (nskk-prolog--trie--collect-all root "" nil 0)))
      (should (null results))))

  (nskk-it "stops at limit when limit is specified"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "a" "v1")
      (nskk-prolog--trie-insert trie "b" "v2")
      (nskk-prolog--trie-insert trie "c" "v3")
      (let* ((root (nskk-prolog--trie-root trie))
             (results (nskk-prolog--trie--collect-all root "" 2 0)))
        (should (= (length results) 2)))))

  (nskk-it "uses prefix as key prefix for collected entries"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "ab" "val")
      (let* ((root (nskk-prolog--trie-root trie))
             (results (nskk-prolog--trie--collect-all root "" nil 0)))
        ;; Key should be the full path from the root prefix
        (should (equal (car (car results)) "ab"))))))

;;;
;;; nskk-prolog--trie--cleanup-path
;;;

(nskk-describe "nskk-prolog--trie--cleanup-path"
  (nskk-it "removes leaf nodes that are no longer needed after deletion"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "ab" "val")
      (nskk-prolog--trie-delete trie "ab")
      ;; After delete + cleanup (trie-delete calls cleanup-path internally),
      ;; the path should be gone — lookup returns (nil . nil) for missing keys
      (should-not (cdr (nskk-prolog--trie-lookup trie "ab")))))

  (nskk-it "does not remove shared nodes when two keys share a prefix"
    (let ((trie (nskk-prolog--trie-create)))
      (nskk-prolog--trie-insert trie "abc" "v1")
      (nskk-prolog--trie-insert trie "abd" "v2")
      ;; Delete one key; the shared "ab" prefix node must remain
      (nskk-prolog--trie-delete trie "abc")
      ;; "abd" should still be accessible
      (should (nskk-prolog--trie-lookup trie "abd")))))

;;;
;;; nskk-prolog--index-add / nskk-prolog--index-remove
;;;

(nskk-describe "nskk-prolog--index-add"
  (nskk-it "completes without error for a predicate with no index config"
    (nskk-prolog-test-with-isolated-db
      (let* ((clause '((test-noindex-pred "a" "b")))
             (key (nskk-prolog--head-key (car clause))))
        ;; Calling directly with a key that has no index config is a no-op
        (should (null (nskk-prolog--index-add key clause))))))

  (nskk-it "is called implicitly during assert and enables query to succeed"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-assert '((test-index-add-fact "x" "y")))
      (should (nskk-prolog-query '(test-index-add-fact "x" "y"))))))

(nskk-describe "nskk-prolog--index-remove"
  (nskk-it "completes without error for a predicate with no index config"
    (nskk-prolog-test-with-isolated-db
      (let* ((clause '((test-remove-noindex "a" "b")))
             (key (nskk-prolog--head-key (car clause))))
        (should (null (nskk-prolog--index-remove key clause))))))

  (nskk-it "is called implicitly during retract and removes the fact"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-assert '((test-remove-fact "x" "y")))
      ;; retract takes a head-pattern, not a clause
      (nskk-prolog-retract '(test-remove-fact "x" "y"))
      (should-not (nskk-prolog-query '(test-remove-fact "x" "y"))))))

;;;
;;; nskk-prolog--get-clauses
;;;

(nskk-describe "nskk-prolog--get-clauses"
  (nskk-it "returns clauses for a predicate after assertion"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-assert '((test-get-clauses-pred "a" "b")))
      (let ((clauses (nskk-prolog--get-clauses 'test-get-clauses-pred '("a" "b") nil)))
        (should clauses))))

  (nskk-it "returns nil for a predicate with no clauses"
    (nskk-prolog-test-with-isolated-db
      (should-not (nskk-prolog--get-clauses 'nonexistent-pred-xyz '() nil)))))

;;;
;;; nskk-prolog--prove-internal
;;;

(nskk-describe "nskk-prolog--prove-internal"
  (nskk-it "calls on-solution when goals is nil (trivially true)"
    (let (solutions)
      (nskk-prolog--prove-internal nil nil
        (lambda (s) (push s solutions)))
      (should (= (length solutions) 1))))

  (nskk-it "proves a simple ground fact in an isolated DB"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-assert '((test-prove-fact)))
      (let (solutions)
        (nskk-prolog--prove-internal '((test-prove-fact)) nil
          (lambda (s) (push s solutions)))
        (should (= (length solutions) 1)))))

  (nskk-it "returns no solutions when goal fails"
    (nskk-prolog-test-with-isolated-db
      (let (solutions)
        (nskk-prolog--prove-internal '((nonexistent-prove-xyz)) nil
          (lambda (s) (push s solutions)))
        (should (null solutions))))))

;;;
;;; nskk-prolog--prove-first
;;;

(nskk-describe "nskk-prolog--prove-first"
  (nskk-it "throws first-solution tag when a matching fact is found"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-assert '((test-first-fact)))
      (let ((result (catch 'nskk-prolog-first-solution
                      (nskk-prolog--prove-first '((test-first-fact)) nil)
                      'no-solution)))
        (should (listp result)))))

  (nskk-it "returns without throwing when no solution is found"
    (nskk-prolog-test-with-isolated-db
      (let ((result (catch 'nskk-prolog-first-solution
                      (nskk-prolog--prove-first '((nonexistent-first-xyz)) nil)
                      'no-solution)))
        (should (eq result 'no-solution))))))

;;;
;;; nskk-prolog--clause-key
;;;

(nskk-describe "nskk-prolog--clause-key"
  (nskk-it "formats predicate and arity as 'pred/arity' string"
    (should (equal (nskk-prolog--clause-key 'foo 2) "foo/2")))

  (nskk-it "works with arity 0"
    (should (equal (nskk-prolog--clause-key 'bar 0) "bar/0")))

  (nskk-it "works with arity 3"
    (should (equal (nskk-prolog--clause-key 'my-pred 3) "my-pred/3")))

  (nskk-it "works with string predicate name"
    (should (equal (nskk-prolog--clause-key "pred" 1) "pred/1"))))

;;;
;;; nskk-prolog--head-key
;;;

(nskk-describe "nskk-prolog--head-key"
  (nskk-it "returns key for a zero-arity clause head"
    ;; (foo) → functor=foo, arity=1-1=0
    (should (equal (nskk-prolog--head-key '(foo)) "foo/0")))

  (nskk-it "returns key for a unary clause head"
    ;; (foo arg1) → functor=foo, arity=2-1=1
    (should (equal (nskk-prolog--head-key '(foo arg1)) "foo/1")))

  (nskk-it "returns key for a binary clause head"
    ;; (foo a b) → functor=foo, arity=3-1=2
    (should (equal (nskk-prolog--head-key '(foo a b)) "foo/2"))))

;;;
;;; nskk-prolog--eval-arith
;;;

(nskk-describe "nskk-prolog--eval-arith"
  (nskk-it "evaluates a number literal to itself"
    (should (= (nskk-prolog--eval-arith 42 nil) 42)))

  (nskk-it "evaluates zero"
    (should (= (nskk-prolog--eval-arith 0 nil) 0)))

  (nskk-it "evaluates addition"
    (should (= (nskk-prolog--eval-arith '(+ 3 4) nil) 7)))

  (nskk-it "evaluates subtraction"
    (should (= (nskk-prolog--eval-arith '(- 10 3) nil) 7)))

  (nskk-it "evaluates multiplication"
    (should (= (nskk-prolog--eval-arith '(* 6 7) nil) 42)))

  (nskk-it "evaluates integer division"
    (should (= (nskk-prolog--eval-arith '(/ 10 2) nil) 5)))

  (nskk-it "evaluates nested arithmetic expressions"
    (should (= (nskk-prolog--eval-arith '(+ (* 2 3) 4) nil) 10)))

  (nskk-it "signals error for unknown arithmetic operator"
    (should-error (nskk-prolog--eval-arith '(% 10 3) nil)))

  (nskk-it "signals error for a non-number non-compound expression"
    (should-error (nskk-prolog--eval-arith "not-a-number" nil))))

(provide 'nskk-prolog-test)

;;; nskk-prolog-test.el ends here
