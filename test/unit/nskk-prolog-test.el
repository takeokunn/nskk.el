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

;;;;
;;;; Arithmetic Built-in Tests
;;;;

(nskk-describe "Prolog arithmetic built-ins"
  (nskk-context "comparison goals"
    (nskk-it ">= succeeds when left is greater than or equal to right"
      (should (nskk-prolog-query-one '(>= 10 5)))
      (should (nskk-prolog-query-one '(>= 5 5)))
      (should (not (nskk-prolog-query-one '(>= 3 5)))))

    (nskk-it "<= succeeds when left is less than or equal to right"
      (should (nskk-prolog-query-one '(<= 3 5)))
      (should (nskk-prolog-query-one '(<= 5 5)))
      (should (not (nskk-prolog-query-one '(<= 10 5)))))

    (nskk-it "> succeeds only when left is strictly greater than right"
      (should (nskk-prolog-query-one '(> 10 5)))
      (should (not (nskk-prolog-query-one '(> 5 5))))
      (should (not (nskk-prolog-query-one '(> 3 5)))))

    (nskk-it "< succeeds only when left is strictly less than right"
      (should (nskk-prolog-query-one '(< 3 5)))
      (should (not (nskk-prolog-query-one '(< 5 5))))
      (should (not (nskk-prolog-query-one '(< 10 5)))))

    (nskk-it "=:= tests arithmetic equality"
      (should (nskk-prolog-query-one (list (intern "=:=") 5 5)))
      (should (not (nskk-prolog-query-one (list (intern "=:=") 5 6))))))

  (nskk-context "is/2 assignment"
    (nskk-it "evaluates arithmetic expressions and binds the result to a variable"
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

    (nskk-it "arithmetic goals work correctly within rule bodies"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-retract-all 'arith-test-double 2)
        (nskk-prolog-<- (arith-test-double \?x \?y)
          (is \?y (* \?x 2)))
        (let ((result (nskk-prolog-query-one '(arith-test-double 5 \?y))))
          (should result)
          (should (listp result))
          (should (= (nskk-prolog-walk '\?y result) 10)))))))

(provide 'nskk-prolog-test)

;;; nskk-prolog-test.el ends here
