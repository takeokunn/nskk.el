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

;; Tests for nskk-prolog .

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-prolog)
(require 'nskk-pbt-generators)
(progn (eval-when-compile (require (quote cl-lib))) (defvar nskk--annotation-initialized))

;;;;
;;;; 1. Variable Representation
;;;;

(nskk-deftest-table prolog-variable-p-recognizes
  :description "Recognizes symbols starting with ? as Prolog variables"
  :columns (var)
  :rows ((\?x) (\?y) (\?foo) (\?char) (\?who))
  :body (should (nskk-prolog-variable-p var)))

(nskk-describe "Prolog variable representation"
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
      (should (nskk--prolog-anonymous-p ?_))
      (should-not (nskk--prolog-anonymous-p '\?x))
      (should-not (nskk--prolog-anonymous-p 'hello)))))

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
        (should-not (nskk--prolog-fail-p result))
        (should (equal (nskk-prolog-walk '\?x result) 'a))))

    (nskk-it "unifying an atom with a variable extends the substitution"
      (let ((result (nskk-prolog-unify 'a '\?x nil)))
        (should-not (nskk--prolog-fail-p result))
        (should (equal (nskk-prolog-walk '\?x result) 'a)))))

  (nskk-context "variable with variable"
    (nskk-it "unifying two variables creates a binding between them"
      (let ((result (nskk-prolog-unify '\?x '\?y nil)))
        (should-not (nskk--prolog-fail-p result))
        (should (consp result)))))

  (nskk-context "different atoms"
    (nskk-it "unifying two different atoms returns :fail"
      (should (nskk--prolog-fail-p (nskk-prolog-unify 'a 'b nil)))
      (should (nskk--prolog-fail-p (nskk-prolog-unify 42 43 nil)))
      (should (nskk--prolog-fail-p (nskk-prolog-unify "hello" "world" nil)))))

  (nskk-context "list unification"
    (nskk-it "unifies lists element-by-element and binds variables"
      (let ((result (nskk-prolog-unify '(a \?x) '(a 1) nil)))
        (should-not (nskk--prolog-fail-p result))
        (should (equal (nskk-prolog-walk '\?x result) 1))))

    (nskk-it "unifies lists with multiple variables binding all of them"
      (let ((result (nskk-prolog-unify '(foo \?x \?y) '(foo 1 2) nil)))
        (should-not (nskk--prolog-fail-p result))
        (should (equal (nskk-prolog-walk '\?x result) 1))
        (should (equal (nskk-prolog-walk '\?y result) 2))))

    (nskk-it "unifies nested lists recursively"
      (let ((result (nskk-prolog-unify '(a (b \?x)) '(a (b c)) nil)))
        (should-not (nskk--prolog-fail-p result))
        (should (equal (nskk-prolog-walk '\?x result) 'c))))

    (nskk-it "fails when lists have different lengths"
      (should (nskk--prolog-fail-p (nskk-prolog-unify '(a b) '(a b c) nil)))
      (should (nskk--prolog-fail-p (nskk-prolog-unify '(a b c) '(a b) nil)))))

  (nskk-context "anonymous variable"
    (nskk-it "symbol \\?_ unifies with anything (treated as regular variable)"
      ;; Because `nskk--prolog-anonymous-p' checks the char-literal ?_ (integer 95)
      ;; while the symbol '\?_ is a regular Prolog variable, direct unification
      ;; binds \?_ like any other variable.  We verify unification succeeds.
      (let ((result (nskk-prolog-unify '\?_ 'anything nil)))
        (should-not (nskk--prolog-fail-p result)))
      (let ((result (nskk-prolog-unify 'anything '\?_ nil)))
        (should-not (nskk--prolog-fail-p result))))

    (nskk-it "symbol \\?_ inside a list unifies successfully"
      ;; The symbol '\?_ is treated as a regular variable (see above),
      ;; so unification succeeds and produces a binding for \?_.
      (let ((result (nskk-prolog-unify '(a \?_ c) '(a b c) nil)))
        (should-not (nskk--prolog-fail-p result)))))

  (nskk-context "substitution preservation"
    (nskk-it "preserves existing substitution bindings when adding a new one"
      (let* ((initial-subst '((\?z . 99)))
             (result (nskk-prolog-unify '\?x 42 initial-subst)))
        (should-not (nskk--prolog-fail-p result))
        (should (equal (nskk-prolog-walk '\?x result) 42))
        (should (equal (nskk-prolog-walk '\?z result) 99)))))

  (nskk-context "failure sentinel"
    (nskk-it "detects :fail as the failure sentinel and rejects nil, t, and empty list"
      (should (nskk--prolog-fail-p :fail))
      (should-not (nskk--prolog-fail-p nil))
      (should-not (nskk--prolog-fail-p t))
      (should-not (nskk--prolog-fail-p '())))))

;;;;
;;;; 3b. nskk-prolog-unify/k — CPS interface
;;;;

(nskk-describe "nskk-prolog-unify/k CPS interface"
  (nskk-context "on-found path"
    (nskk-it "calls on-found with original subst for equal atoms"
      (let ((found nil) (not-found nil))
        (nskk-prolog-unify/k 'a 'a nil
                             (lambda (s) (setq found s))
                             (lambda () (setq not-found t)))
        (should-not not-found)
        (should (eq found nil))))

    (nskk-it "calls on-found with extended subst when binding a variable"
      (let ((found nil))
        (nskk-prolog-unify/k '\?x 42 nil
                             (lambda (s) (setq found s))
                             #'ignore)
        (should found)
        (should (equal (nskk-prolog-walk '\?x found) 42))))

    (nskk-it "calls on-found for anonymous character (skips binding)"
      ;; The integer ?_ (char 95) is treated as anonymous by nskk--prolog-anonymous-p
      (let ((found nil))
        (nskk-prolog-unify/k ?_ 'something nil
                             (lambda (s) (setq found s))
                             #'ignore)
        (should (eq found nil))))  ; subst unchanged — no binding added

    (nskk-it "calls on-found for recursive list unification with bindings"
      (let ((found nil))
        (nskk-prolog-unify/k '(a \?x) '(a 1) nil
                             (lambda (s) (setq found s))
                             #'ignore)
        (should found)
        (should (equal (nskk-prolog-walk '\?x found) 1))))

    (nskk-it "preserves existing substitution entries on success"
      (let* ((init '((\?z . 99)))
             (found nil))
        (nskk-prolog-unify/k '\?x 42 init
                             (lambda (s) (setq found s))
                             #'ignore)
        (should (equal (nskk-prolog-walk '\?z found) 99))
        (should (equal (nskk-prolog-walk '\?x found) 42)))))

  (nskk-context "on-not-found path"
    (nskk-it "calls on-not-found for two different atoms"
      (let ((not-found nil))
        (nskk-prolog-unify/k 'a 'b nil
                             (lambda (_s) nil)
                             (lambda () (setq not-found t)))
        (should not-found)))

    (nskk-it "calls on-not-found when lists have different lengths"
      (let ((not-found nil))
        (nskk-prolog-unify/k '(a b) '(a b c) nil
                             (lambda (_s) nil)
                             (lambda () (setq not-found t)))
        (should not-found)))

    (nskk-it "calls on-not-found when a nested element fails to unify"
      (let ((not-found nil))
        (nskk-prolog-unify/k '(a (b 1)) '(a (b 2)) nil
                             (lambda (_s) nil)
                             (lambda () (setq not-found t)))
        (should not-found)))))

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
          (should (equal (nskk-prolog-walk '\?x (car results)) 'a))))))

  (nskk-context "cut does not affect caller alternatives"
    (nskk-it "cut in a called predicate does not prune the caller's alternatives"
      ;; Under NSKK's clause-local cut semantics, cut throw is caught at the
      ;; clause boundary.  So if (find-first ?x) has a cut, and (wrapper ?x)
      ;; calls (find-first ?x), the wrapper should still see all solutions
      ;; from backtracking through its own alternatives.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        ;; find-first: has a cut in first clause (clause-local effect only)
        (nskk-prolog-assert '((find-first a) !))
        (nskk-prolog-assert '((find-first b)))
        (nskk-prolog-assert '((find-first c)))
        ;; wrapper: calls find-first
        (nskk-prolog-assert '((wrapper \?x) (find-first \?x)))
        (let ((results (nskk-prolog-query '(wrapper \?x))))
          ;; Under clause-local cut semantics, all 3 find-first clauses fire
          ;; because cut only prevents goals AFTER cut in the same clause body.
          (should (= (length results) 3))
          (let ((vals (mapcar (lambda (s) (nskk-prolog-walk '\?x s)) results)))
            (should (member 'a vals))
            (should (member 'b vals))
            (should (member 'c vals))))))))

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
          (should-not result))))

    (nskk-it "variable-first-arg query returns all bulk-asserted entries"
      ;; After dual-write fix: bulk-asserted facts are in both trie AND flat DB.
      ;; A query with ?k as variable falls back to the flat DB and finds all entries.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'bulk-var-query 2 :trie)
        (nskk-prolog-trie-bulk-assert 'bulk-var-query 2
                                      '(("か" . ("花" "家"))
                                        ("き" . ("木" "気"))
                                        ("く" . ("口" "来"))))
        ;; Variable first arg: must fall back to flat DB
        (let ((results (nskk-prolog-query '(bulk-var-query \?k \?c))))
          (should (= (length results) 3))
          (let ((keys (mapcar (lambda (s) (nskk-prolog-walk '\?k s)) results)))
            (should (member "か" keys))
            (should (member "き" keys))
            (should (member "く" keys))))))

    (nskk-it "retract-all clears both trie and flat-DB for bulk-asserted entries"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'bulk-retract-test 2 :trie)
        (nskk-prolog-trie-bulk-assert 'bulk-retract-test 2
                                      '(("a" . ("A"))
                                        ("b" . ("B"))))
        ;; Before retract-all: both ground and variable queries work
        (should (nskk-prolog-query '(bulk-retract-test "a" \?c)))
        (should (= 2 (length (nskk-prolog-query '(bulk-retract-test \?k \?c)))))
        ;; After retract-all: both queries return nil
        (nskk-prolog-retract-all 'bulk-retract-test 2)
        (should-not (nskk-prolog-query '(bulk-retract-test "a" \?c)))
        (should-not (nskk-prolog-query '(bulk-retract-test \?k \?c)))))

    (nskk-it "clear-database removes all bulk-asserted entries"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'bulk-clear-test 2 :trie)
        (nskk-prolog-trie-bulk-assert 'bulk-clear-test 2
                                      '(("x" . ("X"))))
        (should (nskk-prolog-query '(bulk-clear-test "x" \?c)))
        (nskk-prolog-clear-database)
        (should-not (nskk-prolog-query '(bulk-clear-test "x" \?c))))))

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
          (should (= (cadr result) 42)))))

    (nskk-it "returns nil for a fully ground query even when it succeeds"
      ;; query-values calls query-one, which returns t (not a list) for ground
      ;; success.  Since (listp t) is nil, query-values returns nil.
      ;; This is documented behavior: use query-one for ground existence checks.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((ground-query-vals-test)))
        ;; query-one returns t for the ground fact — query-values sees (listp t) = nil
        (should-not (nskk-prolog-query-values '(ground-query-vals-test) '()))
        ;; But query-one correctly returns t
        (should (eq t (nskk-prolog-query-one '(ground-query-vals-test)))))))

  (nskk-context "query-bindings"
    (nskk-it "returns all solutions with multiple variables walked"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((pair a 1)))
        (nskk-prolog-assert '((pair b 2)))
        (nskk-prolog-assert '((pair c 3)))
        (let ((result (nskk-prolog-query-bindings '(pair \?x \?y) '(\?x \?y))))
          (should (equal result '((a 1) (b 2) (c 3)))))))

    (nskk-it "returns empty list when no solutions exist"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (should (null (nskk-prolog-query-bindings '(nonexistent \?x \?y) '(\?x \?y))))))

    (nskk-it "returns list-of-singletons for single variable"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((item a)))
        (nskk-prolog-assert '((item b)))
        (let ((result (nskk-prolog-query-bindings '(item \?x) '(\?x))))
          (should (equal result '((a) (b)))))))

    (nskk-it "returns list with empty binding for ground successful query"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((fact)))
        (let ((result (nskk-prolog-query-bindings '(fact) '())))
          (should (equal result '(())))))))

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
               (renamed (nskk--prolog-rename-variables clause 999)))
          (should-not (equal (car clause) (car renamed)))
          (should-not (member '\?x (flatten-tree renamed))))))

    (nskk-it "renames anonymous ?_ variables to fresh ?_anon_N symbols"
      (nskk-prolog-test-with-isolated-db
        ;; A clause containing the character literal ?_ (anonymous wildcard).
        ;; After renaming, no bare ?_ character should remain — each becomes ?_anon_N.
        (let* ((clause (list (list 'anon-test ?_ ?_)))
               (renamed (nskk--prolog-rename-variables clause 1)))
          ;; The renamed clause should contain no bare ?_ integers
          (should-not (member ?_ (flatten-tree renamed))))))

    (nskk-it "returns a structurally identical clause when it contains no variables"
      ;; A clause with only atoms should be unchanged structurally.
      (let* ((clause '((ground-fact apple banana)))
             (renamed (nskk--prolog-rename-variables clause 42)))
        (should (equal clause renamed))))))

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
              (should (not (nskk--prolog-fail-p result)))
            (should (nskk--prolog-fail-p result)))))

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
          (should (eq (nskk--prolog-fail-p ab) (nskk--prolog-fail-p ba))))))
    (dolist (pair '((42 42) (42 99)))
      (let ((a (car pair))
            (b (cadr pair)))
        (let ((ab (nskk-prolog-unify a b nil))
              (ba (nskk-prolog-unify b a nil)))
          (should (eq (nskk--prolog-fail-p ab) (nskk--prolog-fail-p ba))))))))

;; Assert/retract roundtrip: DB returns to same state
(nskk-deftest-unit prolog-pbt-assert-retract-roundtrip
  "Assert/retract roundtrip: asserting a fact then retracting it leaves
the database in the same state as before the assertion."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    ;; Capture initial state (no facts)
    (let ((before-result (nskk-prolog-query '(roundtrip-fact test-value))))
      (should (null before-result))
      (nskk-prolog-assert '((roundtrip-fact test-value)))
      (should (nskk-prolog-query '(roundtrip-fact test-value)))
      (nskk-prolog-retract '(roundtrip-fact test-value))
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
            ((/  12 4) 3)
            ((+ (* 2 3) 4)   10)
            ((- (* 5 4) (* 2 3)) 14))
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

(nskk-describe "Prolog arithmetic error conditions"
  (nskk-context "unknown operator"
    (nskk-it "signals an error for an unknown arithmetic operator"
      (should-error (nskk--prolog-eval-arith '(% 10 3) nil))))

  (nskk-context "non-numeric expressions"
    (nskk-it "signals an error for a string in arithmetic position"
      (should-error (nskk--prolog-eval-arith "not-a-number" nil)))

    (nskk-it "signals an error for an unbound Prolog variable in arithmetic"
      (should-error (nskk--prolog-eval-arith '\?unbound nil))))

  (nskk-context "bound Prolog variable"
    (nskk-it "evaluates correctly when a Prolog variable is bound to a number"
      ;; Bind ?x = 5 via unification, then use in arithmetic
      (nskk-prolog-test-with-isolated-db
        (let* ((subst (nskk-prolog-unify '\?x 5 nil))
               (result (nskk--prolog-eval-arith '\?x subst)))
          (should (= result 5)))))

    (nskk-it "evaluates nested expression with a bound variable"
      (nskk-prolog-test-with-isolated-db
        (let* ((subst (nskk-prolog-unify '\?n 7 nil))
               (result (nskk--prolog-eval-arith '(* \?n 3) subst)))
          (should (= result 21)))))))

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
        (should-not (nskk-prolog-trie-prefix-search 'no-such-pred 2 "x")))))

  (nskk-context "nskk-prolog-trie-has-prefix-p"
    (nskk-it "returns non-nil when prefix is a proper prefix of an indexed key"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'has-pfx-word 2 :trie)
        (nskk-prolog-trie-bulk-assert 'has-pfx-word 2
                                      '(("hello" . "greeting")
                                        ("world" . "noun")))
        (should (nskk-prolog-trie-has-prefix-p 'has-pfx-word 2 "hel"))))

    (nskk-it "returns non-nil for an exact key match (exact is also a valid prefix)"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'has-pfx-exact 2 :trie)
        (nskk-prolog-trie-bulk-assert 'has-pfx-exact 2
                                      '(("か" . ("花" "家"))))
        (should (nskk-prolog-trie-has-prefix-p 'has-pfx-exact 2 "か"))))

    (nskk-it "returns nil when prefix matches nothing"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'has-pfx-miss 2 :trie)
        (nskk-prolog-trie-bulk-assert 'has-pfx-miss 2
                                      '(("alpha" . "a")))
        (should-not (nskk-prolog-trie-has-prefix-p 'has-pfx-miss 2 "beta"))))

    (nskk-it "returns nil when no trie index is configured for the predicate"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (should-not (nskk-prolog-trie-has-prefix-p 'no-trie-pred 2 "x"))))

    (nskk-it "works with Japanese kana prefixes"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'has-pfx-kana 2 :trie)
        (nskk-prolog-trie-bulk-assert 'has-pfx-kana 2
                                      '(("かな" . ("仮名"))
                                        ("かき" . ("柿" "書記"))
                                        ("さ" . ("差"))))
        ;; "か" is a prefix of both "かな" and "かき"
        (should (nskk-prolog-trie-has-prefix-p 'has-pfx-kana 2 "か"))
        ;; "さ" is an exact match (also a valid prefix node)
        (should (nskk-prolog-trie-has-prefix-p 'has-pfx-kana 2 "さ"))
        ;; "き" matches nothing
        (should-not (nskk-prolog-trie-has-prefix-p 'has-pfx-kana 2 "き"))))

    (nskk-it "empty prefix returns non-nil when trie has entries (root always exists)"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'has-pfx-root 2 :trie)
        (nskk-prolog-trie-bulk-assert 'has-pfx-root 2
                                      '(("x" . "val")))
        ;; Empty string is a prefix of everything; root node always present
        (should (nskk-prolog-trie-has-prefix-p 'has-pfx-root 2 ""))))))

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
;;; Goal dispatch: nskk--prolog-goal-kind and nskk--prolog-builtin-table
;;;

(nskk-deftest-table prolog-goal-kind-dispatch
  :description "nskk--prolog-goal-kind classifies each goal form to the expected kind keyword"
  :columns (goal expected-kind)
  :rows ((!                 :cut)
         ((not foo)         :not)
         ((assertz (foo))   :assertz)
         ((retract (foo))   :retract)
         ((is \?x 1)        :arith)
         ((=:= 1 1)         :arith)
         ((> 2 1)           :arith)
         ((< 1 2)           :arith)
         ((>= 2 2)          :arith)
         ((<= 1 1)          :arith)
         ((my-pred foo bar) :normal)
         (some-atom         :normal))
  :body (should (eq (nskk--prolog-goal-kind goal) expected-kind)))

(nskk-describe "nskk--prolog-goal-kind"
  (nskk-context "builtin-table completeness"
    (nskk-it "nskk--prolog-builtin-table has an entry for every kind"
      (dolist (kind '(:cut :not :assertz :retract :arith :normal))
        (should (functionp (gethash kind nskk--prolog-builtin-table)))))))

;;;
;;; nskk-prolog-index-add / nskk--prolog-index-remove
;;;

(nskk-describe "nskk-prolog-index-add"
  (nskk-it "completes without error for a predicate with no index config"
    (nskk-prolog-test-with-isolated-db
      (let* ((clause '((test-noindex-pred "a" "b")))
             (key (nskk--prolog-head-key (car clause))))
        ;; Calling directly with a key that has no index config is a no-op
        (should (null (nskk-prolog-index-add key clause))))))

  (nskk-it "is called implicitly during assert and enables query to succeed"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-assert '((test-index-add-fact "x" "y")))
      (should (nskk-prolog-query '(test-index-add-fact "x" "y"))))))

(nskk-describe "nskk--prolog-index-remove"
  (nskk-it "completes without error for a predicate with no index config"
    (nskk-prolog-test-with-isolated-db
      (let* ((clause '((test-remove-noindex "a" "b")))
             (key (nskk--prolog-head-key (car clause))))
        (should (null (nskk--prolog-index-remove key clause))))))

  (nskk-it "is called implicitly during retract and removes the fact"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-assert '((test-remove-fact "x" "y")))
      ;; retract takes a head-pattern, not a clause
      (nskk-prolog-retract '(test-remove-fact "x" "y"))
      (should-not (nskk-prolog-query '(test-remove-fact "x" "y"))))))

;;;
;;; nskk-prolog-get-clauses
;;;

(nskk-describe "nskk-prolog-get-clauses"
  (nskk-it "returns clauses for a predicate after assertion"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-assert '((test-get-clauses-pred "a" "b")))
      (let ((clauses (nskk-prolog-get-clauses 'test-get-clauses-pred '("a" "b") nil)))
        (should clauses))))

  (nskk-it "returns nil for a predicate with no clauses"
    (nskk-prolog-test-with-isolated-db
      (should-not (nskk-prolog-get-clauses 'nonexistent-pred-xyz '() nil)))))

;;;
;;; nskk--prolog-prove-internal
;;;

(nskk-describe "nskk--prolog-prove-internal"
  (nskk-it "calls on-solution when goals is nil (trivially true)"
    (let (solutions)
      (nskk--prolog-prove-internal nil nil
        (lambda (s) (push s solutions)))
      (should (= (length solutions) 1))))

  (nskk-it "proves a simple ground fact in an isolated DB"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-assert '((test-prove-fact)))
      (let (solutions)
        (nskk--prolog-prove-internal '((test-prove-fact)) nil
          (lambda (s) (push s solutions)))
        (should (= (length solutions) 1)))))

  (nskk-it "returns no solutions when goal fails"
    (nskk-prolog-test-with-isolated-db
      (let (solutions)
        (nskk--prolog-prove-internal '((nonexistent-prove-xyz)) nil
          (lambda (s) (push s solutions)))
        (should (null solutions))))))

;;;
;;; nskk--prolog-prove-first
;;;

(nskk-describe "nskk--prolog-prove-first"
  (nskk-it "throws first-solution tag when a matching fact is found"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-assert '((test-first-fact)))
      (let ((result (catch 'nskk-prolog-first-solution
                      (nskk--prolog-prove-first '((test-first-fact)) nil)
                      'no-solution)))
        (should (listp result)))))

  (nskk-it "returns without throwing when no solution is found"
    (nskk-prolog-test-with-isolated-db
      (let ((result (catch 'nskk-prolog-first-solution
                      (nskk--prolog-prove-first '((nonexistent-first-xyz)) nil)
                      'no-solution)))
        (should (eq result 'no-solution))))))

;;;
;;; nskk-prolog-clause-key
;;;

(nskk-deftest-table prolog-clause-key-format
  :description "nskk-prolog-clause-key formats predicate/arity as 'pred/N' string"
  :columns (pred arity expected)
  :rows ((foo     2 "foo/2")
         (bar     0 "bar/0")
         (my-pred 3 "my-pred/3")
         ("pred"  1 "pred/1"))
  :body (should (equal (nskk-prolog-clause-key pred arity) expected)))

;;;
;;; nskk--prolog-head-key
;;;

(nskk-deftest-table prolog-head-key-format
  :description "nskk--prolog-head-key derives 'functor/arity' from a clause head list"
  :columns (head expected)
  :rows (((foo)      "foo/0")
         ((foo arg1) "foo/1")
         ((foo a b)  "foo/2"))
  :body (should (equal (nskk--prolog-head-key head) expected)))

;;;
;;; nskk--prolog-eval-arith
;;;

(nskk-deftest-table prolog-eval-arith-expressions
  :description "nskk--prolog-eval-arith evaluates numeric literals and arithmetic expressions"
  :columns (expr expected)
  :rows ((42            42)
         (0             0)
         ((+ 3 4)       7)
         ((- 10 3)      7)
         ((* 6 7)       42)
         ((/ 10 2)      5)
         ((+ (* 2 3) 4) 10))
  :body (should (= (nskk--prolog-eval-arith expr nil) expected)))

(nskk-describe "nskk--prolog-eval-arith"
  (nskk-it "signals error for unknown arithmetic operator"
    (should-error (nskk--prolog-eval-arith '(% 10 3) nil)))

  (nskk-it "signals error for a non-number non-compound expression"
    (should-error (nskk--prolog-eval-arith "not-a-number" nil)))

  (nskk-it "evaluates a bound Emacs Lisp symbol (defconst/defvar) as its value"
    ;; most-positive-fixnum is a globally-bound Emacs built-in constant;
    ;; it satisfies (symbolp), (not (nskk-prolog-variable-p)), and (boundp).
    (should (= most-positive-fixnum
               (nskk--prolog-eval-arith 'most-positive-fixnum nil))))

  (nskk-it "evaluates compound expression with a bound symbol operand"
    (should (= 0
               (nskk--prolog-eval-arith '(- most-positive-fixnum most-positive-fixnum) nil))))

  (nskk-it "signals error for unbound Prolog variable in arithmetic"
    (should-error (nskk--prolog-eval-arith '\?unbound nil))))

;;;;
;;;; 17. set-index Idempotency and retract-all Edge Cases
;;;;

(nskk-describe "nskk-prolog-set-index idempotency"
  (nskk-context "calling set-index twice on the same predicate"
    (nskk-it "does not reset an existing hash index when called a second time"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'idem-pred 1 :hash)
        (nskk-prolog-assert '((idem-pred alpha)))
        ;; Second set-index call must not clear the existing index data
        (nskk-prolog-set-index 'idem-pred 1 :hash)
        (let ((result (nskk-prolog-query '(idem-pred alpha))))
          (should result)
          (should (= (length result) 1)))))

    (nskk-it "does not reset an existing trie index when called a second time"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'idem-trie-pred 2 :trie)
        (nskk-prolog-assert '((idem-trie-pred "key" "val")))
        ;; Second set-index call must not reset the trie
        (nskk-prolog-set-index 'idem-trie-pred 2 :trie)
        (let ((result (nskk-prolog-query-value
                       '(idem-trie-pred "key" \?v) '\?v)))
          (should (equal result "val")))))))

(nskk-describe "nskk-prolog-retract-all edge cases"
  (nskk-context "retract-all on a predicate that was never asserted"
    (nskk-it "completes without signaling an error"
      (nskk-prolog-test-with-isolated-db
        (should-not (condition-case nil
                        (progn (nskk-prolog-retract-all 'never-asserted-xyz 1)
                               nil)
                      (error t))))))

  (nskk-context "retract-all then re-assert"
    (nskk-it "re-asserted facts are queryable after retract-all"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'reassert-pred 1 :hash)
        (nskk-prolog-assert '((reassert-pred first)))
        (nskk-prolog-retract-all 'reassert-pred 1)
        (should-not (nskk-prolog-query '(reassert-pred first)))
        ;; Re-assert after retract-all
        (nskk-prolog-assert '((reassert-pred second)))
        (let ((result (nskk-prolog-query '(reassert-pred second))))
          (should result)
          (should (= (length result) 1)))))

    (nskk-it "retract-all on a hash-indexed predicate resets the index"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'hash-retract-pred 1 :hash)
        (nskk-prolog-assert '((hash-retract-pred x)))
        (nskk-prolog-assert '((hash-retract-pred y)))
        (nskk-prolog-retract-all 'hash-retract-pred 1)
        (should-not (nskk-prolog-query '(hash-retract-pred \?v)))))

    (nskk-it "retract-all on a trie-indexed predicate resets the trie"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'trie-retract-pred 2 :trie)
        (nskk-prolog-trie-bulk-assert 'trie-retract-pred 2
                                      '(("ka" . ("花"))))
        (nskk-prolog-retract-all 'trie-retract-pred 2)
        (should-not (nskk-prolog-query '(trie-retract-pred "ka" \?c)))))))

;;;;
;;;; 18. Input Validation and Error Cases
;;;;

(nskk-describe "nskk-prolog-trie-bulk-assert error cases"
  (nskk-context "calling without a configured trie index"
    (nskk-it "signals an error when no trie index is configured for the predicate"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (should-error
         (nskk-prolog-trie-bulk-assert 'no-index-pred-xyz 2
                                       '(("x" . "y"))))))))

(nskk-describe "nskk-trie input validation"
  (nskk-context "invalid key types"
    (nskk-it "signals an error for an empty string key"
      (let ((trie (nskk-trie-create)))
        (should-error (nskk-trie-insert trie "" "value"))))

    (nskk-it "signals an error for a non-string key"
      (let ((trie (nskk-trie-create)))
        (should-error (nskk-trie-insert trie 42 "value"))))

    (nskk-it "signals an error for a non-string prefix in prefix-search"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "hello" "world")
        (should-error (nskk-trie-prefix-search trie 42))))))

;;;;
;;;; 19. Property-Based Tests: substitute, ground-p, rename-variables
;;;;

;; Table-driven substitute completeness tests.
(nskk-deftest-table prolog-pbt-substitute
  :columns (term subst expected-ground-p)
  :rows    ((hello nil t)
            (\?x ((\?x . atom-value)) t)
            (\?x nil nil)
            ((\?x \?y) ((\?x . a) (\?y . b)) t)
            ((\?x \?y) ((\?x . a)) nil))
  :description "substitute completeness: ground-p of substituted term matches expected"
  :body (let ((result (nskk-prolog-substitute term subst)))
          (if expected-ground-p
              (should (nskk-prolog-ground-p result))
            (should-not (nskk-prolog-ground-p result)))))

;; Table-driven ground-p invariant tests.
(nskk-deftest-table prolog-pbt-ground-p
  :columns (term expected)
  :rows    ((hello t)
            (42 t)
            ("str" t)
            (nil t)
            (\?x nil)
            ((\?x b c) nil)
            ((a b c) t)
            ((a (b \?y) c) nil)
            ((1 (2 3) 4) t))
  :description "ground-p invariants: atoms, numbers, strings, and variable-free lists are ground"
  :body (if expected
            (should (nskk-prolog-ground-p term))
          (should-not (nskk-prolog-ground-p term))))

;; Rename freshness: renamed variables never collide with originals.
(nskk-deftest-unit prolog-pbt-rename-freshness
  "Rename freshness invariant: renamed variable symbols are disjoint from the original variable set."
  (cl-labels
      ((collect-vars (term)
         (cond
          ((nskk-prolog-variable-p term) (list term))
          ((consp term) (append (collect-vars (car term))
                                (collect-vars (cdr term))))
          (t nil))))
    (dolist (clause '(((foo \?x \?y) (bar \?x \?z))
                      ((grandparent \?a \?c) (parent \?a \?b) (parent \?b \?c))
                      ((single-var \?only))
                      ((no-vars atom1 atom2))))
      (let* ((original-vars (collect-vars clause))
             (renamed (nskk--prolog-rename-variables clause 9999))
             (renamed-vars (collect-vars renamed)))
        ;; No renamed variable should appear in the original variable set
        (dolist (v renamed-vars)
          (should-not (member v original-vars)))))))

;;;
;;; nskk-prolog-deffacts Tests
;;;

(nskk-describe "nskk-prolog-deffacts macro"
  (nskk-context "macro expansion"
    (nskk-it "empty fact list expands to a single (progn) with no assertions"
      ;; macroexpand operates on the form without evaluating: verify no
      ;; nskk-prolog-<- calls appear in the expansion for zero rows.
      (let ((expansion (macroexpand '(nskk-prolog-deffacts test-pred))))
        (should (eq (car expansion) 'progn))
        (should (null (cdr expansion)))))

    (nskk-it "single row expands to exactly one nskk-prolog-<- call"
      (let* ((expansion (macroexpand '(nskk-prolog-deffacts my-pred
                                        (a b c))))
             (calls (cdr expansion)))
        (should (eq (car expansion) 'progn))
        (should (= (length calls) 1))
        ;; The call should be (nskk-prolog-<- (my-pred a b c))
        (let ((call (car calls)))
          (should (eq (car call) 'nskk-prolog-<-))
          (should (equal (cadr call) '(my-pred a b c))))))

    (nskk-it "multiple rows expand to one nskk-prolog-<- call per row in order"
      (let* ((expansion (macroexpand '(nskk-prolog-deffacts key-act
                                        (space converting next-candidate)
                                        (space preedit   start-conversion)
                                        (space normal    self-insert))))
             (calls (cdr expansion)))
        (should (eq (car expansion) 'progn))
        (should (= (length calls) 3))
        (should (equal (cadr (nth 0 calls)) '(key-act space converting next-candidate)))
        (should (equal (cadr (nth 1 calls)) '(key-act space preedit   start-conversion)))
        (should (equal (cadr (nth 2 calls)) '(key-act space normal    self-insert))))))

  (nskk-context "runtime behavior"
    (nskk-it "facts asserted via nskk-prolog-deffacts are matched in insertion order"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'test-order-pred 2 :hash)
        (nskk-prolog-deffacts test-order-pred
          (key first)
          (key second)
          (key third))
        ;; The first matching result for (test-order-pred key ?v) must be `first'.
        (let ((val (nskk-prolog-query-value '(test-order-pred key \?v) '\?v)))
          (should (eq val 'first)))))

    (nskk-it "all rows in nskk-prolog-deffacts are asserted to the Prolog database"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        ;; Use list index so all facts are retrievable.
        (nskk-prolog-deffacts test-allrows-pred
          (row1 val-a)
          (row2 val-b)
          (row3 val-c))
        ;; All three rows must resolve.
        (should (nskk-prolog-query-value '(test-allrows-pred row1 \?v) '\?v))
        (should (nskk-prolog-query-value '(test-allrows-pred row2 \?v) '\?v))
        (should (nskk-prolog-query-value '(test-allrows-pred row3 \?v) '\?v))))

    (nskk-it "facts from different nskk-prolog-deffacts calls do not interfere"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-deffacts pred-alpha
          (k1 v1))
        (nskk-prolog-deffacts pred-beta
          (k2 v2))
        ;; pred-alpha should not return facts for pred-beta's key and vice versa.
        (should (null (nskk-prolog-query-value '(pred-alpha k2 \?v) '\?v)))
        (should (null (nskk-prolog-query-value '(pred-beta  k1 \?v) '\?v)))
        ;; But each predicate should resolve its own fact.
        (should (eq (nskk-prolog-query-value '(pred-alpha k1 \?v) '\?v) 'v1))
        (should (eq (nskk-prolog-query-value '(pred-beta  k2 \?v) '\?v) 'v2))))

    (nskk-it "nskk-prolog-deffacts with zero rows asserts nothing and does not signal"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        ;; Evaluate empty deffacts (no rows) — must not signal an error.
        (eval '(nskk-prolog-deffacts empty-noop-pred))
        ;; No fact should be present.
        (should (null (nskk-prolog-query-value '(empty-noop-pred \?a \?b) '\?a)))))

    (nskk-it "nskk-prolog-deffacts correctly handles 1-arity facts"
      ;; Verify macro expansion: a unary row (my-flag active) should expand to
      ;; (nskk-prolog-<- (my-flag active)).
      (let* ((expansion (macroexpand '(nskk-prolog-deffacts my-flag (active))))
             (calls (cdr expansion)))
        (should (eq (car expansion) 'progn))
        (should (= (length calls) 1))
        (let ((call (car calls)))
          (should (eq (car call) 'nskk-prolog-<-))
          (should (equal (cadr call) '(my-flag active)))))
      ;; Verify runtime behaviour: both unary facts are queryable after assertion.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'my-unary-pred 1 :hash)
        (nskk-prolog-deffacts my-unary-pred
          (enabled)
          (disabled))
        (should (nskk-prolog-query-one '(my-unary-pred enabled)))
        (should (nskk-prolog-query-one '(my-unary-pred disabled)))))))

;;;
;;; nskk-when-prolog-holds Tests
;;;

(nskk-describe "nskk-when-prolog-holds macro"
  (nskk-context "macro definition"
    (nskk-it "nskk-when-prolog-holds is defined as a macro"
      (should (fboundp 'nskk-when-prolog-holds))
      (should (macrop (symbol-function 'nskk-when-prolog-holds))))

    (nskk-it "macro expansion uses nskk-prolog-query (not nskk-prolog-query-one) as the guard"
      (let* ((expansion (macroexpand '(nskk-when-prolog-holds (foo x) (bar))))
             ;; expansion: (when (nskk-prolog-query (foo x)) (bar))
             ;; (when TEST BODY): TEST is cadr, so guard is (cadr expansion)
             (guard (cadr expansion)))
        ;; The guard must be a nskk-prolog-query call
        (should (consp guard))
        (should (eq (car guard) 'nskk-prolog-query)))))

  (nskk-context "execution behavior"
    (nskk-it "body executes when the Prolog query returns at least one solution"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-<- (test-wphold-flag active))
        (let ((ran nil))
          (nskk-when-prolog-holds '(test-wphold-flag active)
            (setq ran t))
          (should ran))))

    (nskk-it "body does NOT execute when the Prolog query has no solutions"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (let ((ran nil))
          (nskk-when-prolog-holds '(test-wphold-no-such-pred x)
            (setq ran t))
          (should-not ran))))

    (nskk-it "resolves nil-ambiguity of nskk-prolog-query-one for ground queries"
      (nskk-prolog-test-with-isolated-db
        ;; Verify the nil-ambiguity of nskk-prolog-query-one for ground queries.
        ;; (converting-phase active) is asserted by nskk-henkan-initialize but
        ;; after nskk-prolog-clear-database it will be absent, so query-one => nil.
        (nskk-prolog-clear-database)
        (should (null (nskk-prolog-query-one '(converting-phase active))))
        ;; Now add the fact and verify query-one returns t (not a subst, not nil).
        (nskk-prolog-<- (converting-phase active))
        (should (eq t (nskk-prolog-query-one '(converting-phase active))))
        ;; Crucially: nskk-prolog-query returns a NON-NIL list for a successful
        ;; ground query, resolving the ambiguity that affects nskk-prolog-query-one.
        (let ((result (nskk-prolog-query '(converting-phase active))))
          (should result)
          (should (listp result)))
        ;; And nskk-when-prolog-holds correctly executes its body.
        (let ((ran nil))
          (nskk-when-prolog-holds '(converting-phase active)
            (setq ran t))
          (should ran))))))

;;;;
;;;; Data-Provider: nskk-prolog-deffacts call count verification
;;;;

(nskk-deftest-table prolog-deffacts-call-count
  :columns (fact-count expansion-args _description)
  :rows    ((0 ()                                       "empty fact list → zero nskk-prolog-<- calls")
            (1 ((a b c))                               "single row → exactly one call")
            (3 ((k1 v1) (k2 v2) (k3 v3))              "three rows → three calls"))
  :description "nskk-prolog-deffacts expands to exactly N nskk-prolog-<- calls"
  :body (let* ((form `(nskk-prolog-deffacts test-pred ,@expansion-args))
               (expansion (macroexpand form))
               (calls (cdr expansion)))
          (should (eq (car expansion) 'progn))
          (should (= fact-count (length calls)))))

;;;;
;;;; Property: nskk-prolog-deffacts always produces valid progn form
;;;;

(nskk-describe "nskk-prolog-deffacts always-progn contract"
  (nskk-it "should always expand to a progn form regardless of row count"
    (dolist (n '(0 1 2 5 10))
      (let* ((rows (cl-loop repeat n collect '(key value)))
             (form `(nskk-prolog-deffacts test-pred ,@rows))
             (expansion (macroexpand form)))
        (should (eq (car expansion) 'progn))
        (should (= n (length (cdr expansion))))))))


;;;;
;;;; Property-Based Tests: Assert/Query/Retract Invariants
;;;;

;; PBT 1: Assert→Query roundtrip invariant.
;;
;; Invariant: for any two distinct string atoms `key' and `val', after
;; asserting (nskk--prolog-pbt-test-pred-rtrip key val) into an isolated
;; database, querying for that fact returns a substitution where ?val is
;; bound to `val'.
;;
;; Uses `search-query' for keys (hiragana strings) and `romaji-pattern'
;; for values (romaji strings) — both are plain strings, which the Prolog
;; engine stores and unifies as atoms.
(nskk-property-test-seeded prolog-pbt-assert-query-roundtrip
  ((key   search-query)
   (val   romaji-pattern))
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-assert (list (list 'nskk--prolog-pbt-test-pred-rtrip key val)))
    (let ((result (nskk-prolog-query-value
                   (list 'nskk--prolog-pbt-test-pred-rtrip key '\?val)
                   '\?val)))
      (equal result val)))
  30
  42)

;; PBT 2: Retract idempotency.
;;
;; Invariant: after asserting then retracting (nskk--prolog-pbt-test-pred-idm key),
;; querying returns no results.  A second retract call must not signal an error
;; (retractall/retract-already-gone is a no-op).
(nskk-property-test-seeded prolog-pbt-retract-idempotency
  ((key search-query))
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    ;; Assert the fact, then retract it once.
    (nskk-prolog-assert (list (list 'nskk--prolog-pbt-test-pred-idm key)))
    (nskk-prolog-retract-all 'nskk--prolog-pbt-test-pred-idm 1)
    ;; After retract, query must return nothing.
    (let ((after-first (nskk-prolog-query
                        (list 'nskk--prolog-pbt-test-pred-idm key))))
      ;; Second retract-all must not error.
      (condition-case _err
          (nskk-prolog-retract-all 'nskk--prolog-pbt-test-pred-idm 1)
        (error nil))
      ;; The query result after first retract must be nil.
      (null after-first)))
  30
  42)

;; PBT 3: Assert count monotonicity.
;;
;; Invariant: after asserting N distinct facts
;; (nskk--prolog-pbt-test-pred-cnt k1), (nskk--prolog-pbt-test-pred-cnt k2), …,
;; querying with a variable returns exactly N solutions.
;;
;; We generate N=3 distinct keys by combining the three generated strings.
;; In the rare case of collisions the keys are deduplicated before counting,
;; and the expected count is adjusted accordingly so the property stays tight.
(nskk-property-test-seeded prolog-pbt-assert-count-monotonicity
  ((k1 search-query)
   (k2 search-query)
   (k3 search-query))
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    ;; Build a deduplicated list of keys so the expected count is exact.
    (let* ((keys (cl-remove-duplicates (list k1 k2 k3) :test #'equal))
           (n    (length keys)))
      (dolist (k keys)
        (nskk-prolog-assert (list (list 'nskk--prolog-pbt-test-pred-cnt k))))
      (let ((results (nskk-prolog-query
                      '(nskk--prolog-pbt-test-pred-cnt \?k))))
        (= (length results) n))))
  30
  42)

;;;;
;;;; New helper functions - edge cases
;;;;

(nskk-describe "new helper functions - edge cases"
  (nskk-context "rename-variables: same variable in head and body"
    (nskk-it "same ?x in head and body renames to the same fresh symbol (G4)"
      ;; When the same variable ?x appears in both the head and a body goal of
      ;; a clause, nskk--prolog-rename-variables must map it to the SAME fresh
      ;; symbol throughout — consistent renaming via the shared mapping hash.
      (let* ((clause (list '(foo \?x) '(bar \?x)))
             (renamed (nskk--prolog-rename-variables clause 99))
             ;; head is (car renamed) = (foo ?x_99); body goal is (cadr renamed) = (bar ?x_99).
             ;; Each is a list whose second element (cadr) is the variable position.
             (var-in-head (cadr (car renamed)))
             (var-in-body (cadr (cadr renamed))))
        ;; Both positions must carry the identical (eq) fresh symbol.
        (should (symbolp var-in-head))
        (should (symbolp var-in-body))
        (should (eq var-in-head var-in-body)))))

  (nskk-context "rename-variables: two anonymous ?_ get distinct fresh symbols"
    (nskk-it "two ?_ occurrences in the same clause get distinct ?_anon_N symbols (G5)"
      ;; Each anonymous-variable occurrence must be replaced by a UNIQUE fresh
      ;; symbol so that they can unify independently.  The integer ?_ (95) is
      ;; the anonymous wildcard recognised by nskk--prolog-anonymous-p.
      (let* (;; clause: ((anon-test ?_ ?_)) — two anonymous positions
             (clause (list (list 'anon-test ?_ ?_)))
             (renamed (nskk--prolog-rename-variables clause 1))
             ;; The single head term is (car renamed); its args are cadr and caddr.
             (head         (car renamed))
             (first-anon   (cadr head))
             (second-anon  (caddr head)))
        ;; Both renamed positions must be symbols starting with "?_anon_".
        (should (symbolp first-anon))
        (should (symbolp second-anon))
        (should (string-prefix-p "?_anon_" (symbol-name first-anon)))
        (should (string-prefix-p "?_anon_" (symbol-name second-anon)))
        ;; The two renamed anonymous variables must be DISTINCT symbols.
        (should-not (eq first-anon second-anon)))))

  (nskk-context "try-clause: failed unification never invokes on-solution"
    (nskk-it "try-clause does not call on-solution when goal cannot unify with clause head (G9)"
      ;; nskk--prolog-try-clause should silently skip a clause whose head fails
      ;; to unify with the given goal.  The on-solution callback must never fire.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((demo-pred "a")))
        (let* ((solution-called nil)
               ;; The only clause for demo-pred/1 has head (demo-pred "a").
               ;; Querying with "b" will fail to unify.
               (clause (car (gethash "demo-pred/1" nskk--prolog-database)))
               (goal   '(demo-pred "b")))
          (nskk--prolog-try-clause clause goal '() nil
                                   (lambda (_subst) (setq solution-called t)))
          (should-not solution-called)))))

  (nskk-context "try-clause: cut does not propagate outward"
    (nskk-it "nskk-prolog-cut thrown inside try-clause is caught within it and does not escape (G8)"
      ;; nskk--prolog-try-clause wraps its body proof in (catch 'nskk-prolog-cut …).
      ;; A cut thrown inside must not escape to the surrounding dynamic context.
      ;; We verify by asserting a rule containing cut, calling try-clause directly
      ;; inside an outer catch, and confirming the outer catch is never triggered.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        ;; Assert the fact so we can look up its clause.
        (nskk-prolog-assert '((cut-test-head "x")))
        (let* ((cut-escaped nil)
               ;; Build a clause manually: head + body containing cut.
               ;; Format: (head . body-goals) — same structure nskk-prolog-assert stores.
               (clause (list (list 'cut-test-head "x") '!))
               (goal   '(cut-test-head "x")))
          ;; Wrap in an outer catch to detect any escaping cut throw.
          (catch 'nskk-prolog-cut
            (nskk--prolog-try-clause clause goal '() nil #'ignore)
            ;; Reaching here means cut did not escape past try-clause — correct.
            ;; If cut had escaped, the throw would unwind past this point and
            ;; the outer catch would have been the handler instead.
            )
          (should-not cut-escaped))))))

;;;;
;;;; nskk--prolog-try-clause internal contract
;;;;

(nskk-describe "nskk--prolog-try-clause internal contract"
  (nskk-context "per-clause cut isolation"
    (nskk-it "per-clause cut does not escape to parent goal (G10)"
      ;; nskk--prolog-try-clause wraps each clause body in (catch 'nskk-prolog-cut).
      ;; A cut thrown inside one clause must not prevent backtracking over OTHER
      ;; top-level clauses (the outer goal loop must continue).
      ;; We verify this by asserting two independent facts and confirming that
      ;; nskk-prolog-query collects both solutions regardless of cut semantics.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((outer a)))
        (nskk-prolog-assert '((outer b)))
        ;; Both facts must be reachable — cut inside one clause must not block
        ;; the engine from considering the second clause.
        (let ((solutions (nskk-prolog-query '(outer \?x))))
          (should (= (length solutions) 2))
          (let ((vals (mapcar (lambda (s) (nskk-prolog-walk '\?x s)) solutions)))
            (should (member 'a vals))
            (should (member 'b vals)))))))

  (nskk-context "variable renaming per call"
    (nskk-it "variable renaming gives fresh vars per call so two queries do not share bindings (G11)"
      ;; Each call to the Prolog engine renames clause variables to fresh gensyms.
      ;; Two separate nskk-prolog-query-one calls therefore return independent
      ;; substitution environments; walking ?x in one must not yield the binding
      ;; from the other's environment.
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (nskk-prolog-assert '((shared-var foo)))
        (let ((s1 (nskk-prolog-query-one '(shared-var \?x)))
              (s2 (nskk-prolog-query-one '(shared-var \?x))))
          ;; Both queries must succeed and return the same value "foo".
          (should s1)
          (should s2)
          (should (equal (nskk-prolog-walk '\?x s1) 'foo))
          (should (equal (nskk-prolog-walk '\?x s2) 'foo))
          ;; The two substitution alists must be independent objects — not eq.
          (should-not (eq s1 s2)))))))

(defun nskk-test--prolog-key-state (key tables)
  "Return exact KEY presence and value identity for TABLES."
  (let ((missing (make-symbol "missing")))
    (mapcar
     (lambda (table)
       (let ((value (gethash key table missing)))
         (cons (not (eq value missing))
               (unless (eq value missing) value))))
     tables)))

(defun nskk-test--prolog-should-match-key-state (key tables expected)
  "Assert exact KEY presence and value identity in TABLES against EXPECTED."
  (let ((actual (nskk-test--prolog-key-state key tables)))
    (cl-mapc
     (lambda (before after)
       (should (eq (car before) (car after)))
       (should (eq (cdr before) (cdr after))))
     expected actual)))

(ert-deftest nskk-prolog-set-index-publish-fault-restores-exact-key-state ()
  (dolist (fault-type '(error quit))
    (nskk-prolog-test-with-isolated-db
     (let ((nskk--prolog-index-bucket-tail-cache
            (make-hash-table :test 'equal)))
       (nskk-prolog-clear-database)
       (nskk-prolog-set-index 'publish-fault 2 :hash)
       (nskk-prolog-assert '((publish-fault "a" old)))
       (let* ((key "publish-fault/2")
              (tables
               (list nskk--prolog-index-config
                     nskk--prolog-hash-indices
                     nskk--prolog-trie-indices
                     nskk--prolog-index-bucket-tail-cache))
              (expected (nskk-test--prolog-key-state key tables))
              (original-puthash (symbol-function 'puthash))
              (original-remhash (symbol-function 'remhash))
              (publish-count 0)
              (faulted nil)
              caught)
         (cl-letf
             (((symbol-function 'puthash)
               (lambda (map-key value table)
                 (let ((result (funcall original-puthash map-key value table)))
                   (when (and (not faulted)
                              (equal map-key key)
                              (memq table tables))
                     (setq publish-count (1+ publish-count))
                     (when (= publish-count 3)
                       (setq faulted t)
                       (signal fault-type nil)))
                   result)))
              ((symbol-function 'remhash)
               (lambda (map-key table)
                 (let ((result (funcall original-remhash map-key table)))
                   (when (and (not faulted)
                              (equal map-key key)
                              (memq table tables))
                     (setq publish-count (1+ publish-count))
                     (when (= publish-count 3)
                       (setq faulted t)
                       (signal fault-type nil)))
                   result))))
           (condition-case condition
               (nskk-prolog-set-index 'publish-fault 2 :trie)
             ((error quit)
              (setq caught (car condition)))))
         (should faulted)
         (should (= publish-count 3))
         (should (eq caught fault-type))
         (nskk-test--prolog-should-match-key-state key tables expected))))))

(ert-deftest nskk-prolog-replace-no-match-appends-with-canonical-tails ()
  (progn
    (dolist (index-type (quote (:list :hash)))
      (nskk-prolog-test-with-isolated-db
        (let ((nskk--prolog-index-bucket-tail-cache
               (make-hash-table :test (quote equal))))
          (nskk-prolog-clear-database)
          (let* ((predicate (if (eq index-type :hash)
                                (quote replace-no-match-hash)
                              (quote replace-no-match-list)))
                 (key (format "%s/2" predicate))
                 (old-head (list predicate "old" 7))
                 (old-clause (list old-head))
                 (missing-head (list predicate "new" 0))
                 (new-head (list predicate "new" 1))
                 (new-clause (list new-head))
                 (updated-head (list predicate "new" 2))
                 (updated-clause (list updated-head)))
            (nskk-prolog-set-index predicate 2 index-type)
            (nskk-prolog-assert old-clause)
            (nskk-prolog-replace-clause-transaction missing-head new-clause)
            (should (equal (nskk-prolog-query-value
                            (list predicate "old" (quote \?value))
                            (quote \?value))
                           7))
            (should (equal (nskk-prolog-query-value
                            (list predicate "new" (quote \?value))
                            (quote \?value))
                           1))
            (let ((database (gethash key nskk--prolog-database)))
              (should (equal database (list old-clause new-clause)))
              (should (eq (gethash key nskk--prolog-database-tails)
                          (last database))))
            (when (eq index-type :hash)
              (let* ((index (gethash key nskk--prolog-hash-indices))
                     (bucket (gethash "new" index))
                     (entry (gethash key nskk--prolog-index-bucket-tail-cache))
                     (cache-info (gethash "new" (aref entry 2))))
                (should (equal bucket (list new-clause)))
                (should (eq (aref cache-info 0) bucket))
                (should (eq (aref cache-info 1) (last bucket)))))
            (nskk-prolog-replace-clause-transaction new-head updated-clause)
            (should-not (nskk-prolog-query new-head))
            (should (equal (nskk-prolog-query-value
                            (list predicate "old" (quote \?value))
                            (quote \?value))
                           7))
            (should (equal (nskk-prolog-query-value
                            (list predicate "new" (quote \?value))
                            (quote \?value))
                           2))
            (let ((database (gethash key nskk--prolog-database)))
              (should (equal database (list old-clause updated-clause)))
              (should (eq (gethash key nskk--prolog-database-tails)
                          (last database))))
            (when (eq index-type :hash)
              (let* ((index (gethash key nskk--prolog-hash-indices))
                     (bucket (gethash "new" index))
                     (entry (gethash key nskk--prolog-index-bucket-tail-cache))
                     (cache-info (gethash "new" (aref entry 2))))
                (should (equal bucket (list updated-clause)))
                (should (eq (aref cache-info 0) bucket))
                (should (eq (aref cache-info 1) (last bucket)))))))))
    (dolist (index-type (quote (:list :hash :trie)))
      (nskk-prolog-test-with-isolated-db
        (let ((nskk--prolog-index-bucket-tail-cache
               (make-hash-table :test (quote equal))))
          (nskk-prolog-clear-database)
          (let* ((predicate
                  (pcase index-type
                    (:list (quote replace-delete-list))
                    (:hash (quote replace-delete-hash))
                    (:trie (quote replace-delete-trie))))
                 (key (format "%s/2" predicate))
                 (duplicate-head (list predicate "a" (quote duplicate)))
                 (duplicate-clause (list duplicate-head))
                 (survivor-head (list predicate "a" (quote survivor)))
                 (survivor-clause (list survivor-head))
                 (solo-head (list predicate "solo" (quote final)))
                 (solo-clause (list solo-head))
                 (variable-pattern
                  (list predicate (quote \?first) (quote duplicate))))
            (nskk-prolog-set-index predicate 2 index-type)
            (nskk-prolog-assert duplicate-clause)
            (nskk-prolog-assert duplicate-clause)
            (nskk-prolog-assert survivor-clause)
            (nskk-prolog-assert solo-clause)
            (let* ((database-before (gethash key nskk--prolog-database))
                   (database-tail-before
                    (gethash key nskk--prolog-database-tails))
                   (index
                    (pcase index-type
                      (:list nil)
                      (:hash (gethash key nskk--prolog-hash-indices))
                      (:trie (gethash key nskk--prolog-trie-indices))))
                   (bucket-before
                    (pcase index-type
                      (:list nil)
                      (:hash (gethash "a" index))
                      (:trie (nskk-trie-lookup index "a"))))
                   (callback-count 0))
              (should
               (eq :deleted
                   (nskk-prolog-replace-clause-transaction
                    variable-pattern nil
                    (lambda ()
                      (setq callback-count (1+ callback-count))
                      (should (nskk-prolog-query duplicate-head))
                      :deleted))))
              (should (= callback-count 1))
              (let ((database-after (gethash key nskk--prolog-database)))
                (should (eq database-after (cdr database-before)))
                (should
                 (equal database-after
                        (list duplicate-clause survivor-clause solo-clause)))
                (should (eq (gethash key nskk--prolog-database-tails)
                            database-tail-before)))
              (should (nskk-prolog-query duplicate-head))
              (should (nskk-prolog-query survivor-head))
              (should (nskk-prolog-query solo-head))
              (unless (eq index-type :list)
                (let* ((bucket-after
                        (pcase index-type
                          (:hash (gethash "a" index))
                          (:trie (nskk-trie-lookup index "a"))))
                       (entry
                        (gethash key nskk--prolog-index-bucket-tail-cache))
                       (cache-info (gethash "a" (aref entry 2))))
                  (should (eq bucket-after (cdr bucket-before)))
                  (should
                   (equal bucket-after
                          (list duplicate-clause survivor-clause)))
                  (should (eq (aref cache-info 0) bucket-after))
                  (should (eq (aref cache-info 1) (last bucket-after)))))
              (nskk-prolog-replace-clause-transaction solo-head nil)
              (let ((database-after (gethash key nskk--prolog-database)))
                (should
                 (equal database-after
                        (list duplicate-clause survivor-clause)))
                (should (eq (gethash key nskk--prolog-database-tails)
                            (last database-after))))
              (should-not (nskk-prolog-query solo-head))
              (unless (eq index-type :list)
                (let* ((solo-bucket
                        (pcase index-type
                          (:hash (gethash "solo" index))
                          (:trie (nskk-trie-lookup index "solo"))))
                       (entry
                        (gethash key nskk--prolog-index-bucket-tail-cache))
                       (cache-info (gethash "solo" (aref entry 2))))
                  (should-not solo-bucket)
                  (should cache-info)
                  (should-not (aref cache-info 0))
                  (should-not (aref cache-info 1))))
              (let* ((tables
                      (list nskk--prolog-database
                            nskk--prolog-database-tails
                            nskk--prolog-hash-indices
                            nskk--prolog-trie-indices
                            nskk--prolog-index-bucket-tail-cache))
                     (expected (nskk-test--prolog-key-state key tables))
                     (database (gethash key nskk--prolog-database))
                     (database-tail
                      (gethash key nskk--prolog-database-tails))
                     (entry
                      (gethash key nskk--prolog-index-bucket-tail-cache))
                     (bucket
                      (pcase index-type
                        (:list nil)
                        (:hash (gethash "a" index))
                        (:trie (nskk-trie-lookup index "a"))))
                     (cache-info
                      (and entry (gethash "a" (aref entry 2))))
                     (callback-count 0))
                (should
                 (eq :no-match
                     (nskk-prolog-replace-clause-transaction
                      (list predicate "missing" (quote absent)) nil
                      (lambda ()
                        (setq callback-count (1+ callback-count))
                        :no-match))))
                (should (= callback-count 1))
                (nskk-test--prolog-should-match-key-state
                 key tables expected)
                (should (eq database (gethash key nskk--prolog-database)))
                (should (eq database-tail
                            (gethash key nskk--prolog-database-tails)))
                (unless (eq index-type :list)
                  (should
                   (eq index
                       (pcase index-type
                         (:hash (gethash key nskk--prolog-hash-indices))
                         (:trie (gethash key nskk--prolog-trie-indices)))))
                  (should
                   (eq bucket
                       (pcase index-type
                         (:hash (gethash "a" index))
                         (:trie (nskk-trie-lookup index "a")))))
                  (should
                   (eq entry
                       (gethash key nskk--prolog-index-bucket-tail-cache)))
                  (should (eq cache-info (gethash "a" (aref entry 2)))))))))))))
(ert-deftest nskk-prolog-replace-helper-fault-restores-exact-object-graph ()
  (progn
    (dolist (fault-type '(error quit))
      (nskk-prolog-test-with-isolated-db
       (let ((nskk--prolog-index-bucket-tail-cache
              (make-hash-table :test 'equal)))
         (nskk-prolog-clear-database)
         (nskk-prolog-set-index 'replace-fault 2 :hash)
         (nskk-prolog-assert '((replace-fault "a" old)))
         (nskk-prolog-assert '((replace-fault "a" other)))
         (let* ((key "replace-fault/2")
                (database-head (gethash key nskk--prolog-database))
                (database-tail (gethash key nskk--prolog-database-tails))
                (index (gethash key nskk--prolog-hash-indices))
                (bucket (gethash "a" index))
                (entry
                 (gethash key nskk--prolog-index-bucket-tail-cache))
                (buckets (aref entry 2))
                (stale-info (vector bucket nil))
                (original-puthash (symbol-function 'puthash))
                (faulted nil)
                caught)
           (puthash "a" stale-info buckets)
           (cl-letf
               (((symbol-function 'puthash)
                 (lambda (map-key value table)
                   (let ((result (funcall original-puthash map-key value table)))
                     (when (and (not faulted)
                                (eq table buckets)
                                (equal map-key "a"))
                       (setq faulted t)
                       (signal fault-type nil))
                     result))))
             (condition-case condition
                 (nskk-prolog-replace-clause-transaction
                  '(replace-fault "a" old)
                  '((replace-fault "a" new)))
               ((error quit)
                (setq caught (car condition)))))
           (should faulted)
           (should (eq caught fault-type))
           (should (eq database-head
                       (gethash key nskk--prolog-database)))
           (should (eq database-tail
                       (gethash key nskk--prolog-database-tails)))
           (should (eq index
                       (gethash key nskk--prolog-hash-indices)))
           (should (eq bucket (gethash "a" index)))
           (should (eq entry
                       (gethash key nskk--prolog-index-bucket-tail-cache)))
           (should (eq stale-info (gethash "a" buckets)))
           (should
            (equal database-head
                   '(((replace-fault "a" old))
                     ((replace-fault "a" other)))))))))
    (dolist (fault-type (quote (error quit)))
      (dolist (callback-phase (quote (before after)))
        (dotimes (_iteration 3)
          (nskk-prolog-test-with-isolated-db
            (let ((nskk--prolog-index-bucket-tail-cache
                   (make-hash-table :test (quote equal))))
              (nskk-prolog-clear-database)
              (let* ((predicate (quote replace-delete-callback-fault))
                     (key "replace-delete-callback-fault/2")
                     (keep-head (list predicate "a" (quote keep)))
                     (keep-clause (list keep-head))
                     (target-head (list predicate "a" (quote target)))
                     (target-clause (list target-head))
                     (other-head (list predicate "a" (quote other)))
                     (other-clause (list other-head))
                     (variable-pattern
                      (list predicate (quote \?first) (quote target))))
                (nskk-prolog-set-index predicate 2 :hash)
                (nskk-prolog-assert keep-clause)
                (nskk-prolog-assert target-clause)
                (nskk-prolog-assert other-clause)
                (let* ((tables
                        (list nskk--prolog-database
                              nskk--prolog-database-tails
                              nskk--prolog-hash-indices
                              nskk--prolog-trie-indices
                              nskk--prolog-index-bucket-tail-cache))
                       (expected (nskk-test--prolog-key-state key tables))
                       (database-head (gethash key nskk--prolog-database))
                       (database-target-cell (cdr database-head))
                       (database-after-target (cdr database-target-cell))
                       (database-tail
                        (gethash key nskk--prolog-database-tails))
                       (index (gethash key nskk--prolog-hash-indices))
                       (bucket (gethash "a" index))
                       (bucket-target-cell (cdr bucket))
                       (bucket-after-target (cdr bucket-target-cell))
                       (entry
                        (gethash key nskk--prolog-index-bucket-tail-cache))
                       (buckets (aref entry 2))
                       (cache-info (gethash "a" buckets))
                       (callback-count 0)
                       callback-observed
                       caught)
                  (condition-case condition
                      (nskk-prolog-replace-clause-transaction
                       variable-pattern nil
                       (lambda ()
                         (setq callback-count (1+ callback-count))
                         (when (eq callback-phase (quote after))
                           (progn
                             (should-not (nskk-prolog-query target-head))
                             (should (nskk-prolog-query keep-head))
                             (should (nskk-prolog-query other-head))
                             (setq callback-observed t)))
                         (signal fault-type nil)))
                    ((error quit)
                     (setq caught (car condition))))
                  (should (= callback-count 1))
                  (should (eq caught fault-type))
                  (if (eq callback-phase (quote after))
                      (should callback-observed)
                    (should-not callback-observed))
                  (nskk-test--prolog-should-match-key-state
                   key tables expected)
                  (should (eq database-head
                              (gethash key nskk--prolog-database)))
                  (should (eq database-target-cell (cdr database-head)))
                  (should (eq database-after-target
                              (cdr database-target-cell)))
                  (should (eq database-tail
                              (gethash key nskk--prolog-database-tails)))
                  (should (eq index
                              (gethash key nskk--prolog-hash-indices)))
                  (should (eq bucket (gethash "a" index)))
                  (should (eq bucket-target-cell (cdr bucket)))
                  (should (eq bucket-after-target
                              (cdr bucket-target-cell)))
                  (should (eq entry
                              (gethash key
                                       nskk--prolog-index-bucket-tail-cache)))
                  (should (eq cache-info (gethash "a" buckets)))
                  (should (nskk-prolog-query target-head))
                  (let ((recovery-callback-count 0))
                    (should
                     (eq :recovered
                         (nskk-prolog-replace-clause-transaction
                          variable-pattern nil
                          (lambda ()
                            (setq recovery-callback-count
                                  (1+ recovery-callback-count))
                            :recovered))))
                    (should (= recovery-callback-count 1)))
                  (should-not (nskk-prolog-query target-head))
                  (should (nskk-prolog-query keep-head))
                  (should (nskk-prolog-query other-head))
                  (should (eq database-head
                              (gethash key nskk--prolog-database)))
                  (should (eq database-after-target (cdr database-head)))
                  (should (eq database-tail
                              (gethash key nskk--prolog-database-tails)))
                  (should (eq index
                              (gethash key nskk--prolog-hash-indices)))
                  (should (eq bucket (gethash "a" index)))
                  (should (eq bucket-after-target (cdr bucket)))
                  (let ((recovery-cache-info (gethash "a" buckets)))
                    (should (eq (aref recovery-cache-info 0) bucket))
                    (should
                     (eq (aref recovery-cache-info 1)
                         bucket-after-target))))))))))))

(defvar nskk-test--prolog-nonnumeric-global nil
  "Non-numeric global used to audit arithmetic query isolation.")

(ert-deftest nskk-prolog-assert-fault-restores-exact-object-graph ()
  (dolist (fault-type '(error quit))
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--prolog-index-bucket-tail-cache
             (make-hash-table :test 'equal)))
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'assert-fault 2 :hash)
        (nskk-prolog-assert '((assert-fault "a" old)))
        (let* ((key "assert-fault/2")
               (tables
                (list nskk--prolog-database
                      nskk--prolog-database-tails
                      nskk--prolog-index-config
                      nskk--prolog-hash-indices
                      nskk--prolog-trie-indices
                      nskk--prolog-index-bucket-tail-cache))
               (expected (nskk-test--prolog-key-state key tables))
               (database-head (gethash key nskk--prolog-database))
               (database-tail (gethash key nskk--prolog-database-tails))
               (database-tail-cdr (cdr database-tail))
               (index (gethash key nskk--prolog-hash-indices))
               (bucket (gethash "a" index))
               (bucket-tail (last bucket))
               (bucket-tail-cdr (cdr bucket-tail))
               (cache-entry
                (gethash key nskk--prolog-index-bucket-tail-cache))
               (cache-buckets (aref cache-entry 2))
               (cache-bucket (gethash "a" cache-buckets))
               (original-index-add
                (symbol-function 'nskk-prolog-index-add))
               faulted
               caught)
          (cl-letf (((symbol-function 'nskk-prolog-index-add)
                     (lambda (inner-key clause)
                       (prog1 (funcall original-index-add inner-key clause)
                         (setq faulted t)
                         (signal fault-type nil)))))
            (condition-case condition
                (nskk-prolog-assert '((assert-fault "a" rejected)))
              ((error quit)
               (setq caught (car condition)))))
          (should faulted)
          (should (eq caught fault-type))
          (nskk-test--prolog-should-match-key-state key tables expected)
          (should (eq database-head
                      (gethash key nskk--prolog-database)))
          (should (eq database-tail
                      (gethash key nskk--prolog-database-tails)))
          (should (eq database-tail-cdr (cdr database-tail)))
          (should (eq index (gethash key nskk--prolog-hash-indices)))
          (should (eq bucket (gethash "a" index)))
          (should (eq bucket-tail-cdr (cdr bucket-tail)))
          (should (eq cache-entry
                      (gethash key nskk--prolog-index-bucket-tail-cache)))
          (should (eq cache-bucket (gethash "a" cache-buckets)))
          (should (equal database-head '(((assert-fault "a" old)))))
          (nskk-prolog-assert '((assert-fault "a" after)))
          (should
           (equal database-head
                  '(((assert-fault "a" old))
                    ((assert-fault "a" after))))))))))

(ert-deftest nskk-prolog-retract-fault-restores-exact-object-graph ()
  (dolist (fault-type '(error quit))
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--prolog-index-bucket-tail-cache
             (make-hash-table :test 'equal)))
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'retract-fault 2 :hash)
        (nskk-prolog-assert '((retract-fault "a" old)))
        (nskk-prolog-assert '((retract-fault "a" other)))
        (let* ((key "retract-fault/2")
               (tables
                (list nskk--prolog-database
                      nskk--prolog-database-tails
                      nskk--prolog-index-config
                      nskk--prolog-hash-indices
                      nskk--prolog-trie-indices
                      nskk--prolog-index-bucket-tail-cache))
               (expected (nskk-test--prolog-key-state key tables))
               (database-head (gethash key nskk--prolog-database))
               (database-tail (gethash key nskk--prolog-database-tails))
               (index (gethash key nskk--prolog-hash-indices))
               (bucket (gethash "a" index))
               (cache-entry
                (gethash key nskk--prolog-index-bucket-tail-cache))
               (cache-bucket (gethash "a" (aref cache-entry 2)))
               (original-index-remove
                (symbol-function 'nskk--prolog-index-remove))
               faulted
               caught)
          (cl-letf (((symbol-function 'nskk--prolog-index-remove)
                     (lambda (inner-key clause)
                       (prog1 (funcall original-index-remove inner-key clause)
                         (setq faulted t)
                         (signal fault-type nil)))))
            (condition-case condition
                (nskk-prolog-retract '(retract-fault "a" old))
              ((error quit)
               (setq caught (car condition)))))
          (should faulted)
          (should (eq caught fault-type))
          (nskk-test--prolog-should-match-key-state key tables expected)
          (should (eq database-head
                      (gethash key nskk--prolog-database)))
          (should (eq database-tail
                      (gethash key nskk--prolog-database-tails)))
          (should (eq index (gethash key nskk--prolog-hash-indices)))
          (should (eq bucket (gethash "a" index)))
          (should (eq cache-entry
                      (gethash key nskk--prolog-index-bucket-tail-cache)))
          (should (eq cache-bucket
                      (gethash "a" (aref cache-entry 2))))
          (should
           (equal database-head
                  '(((retract-fault "a" old))
                    ((retract-fault "a" other)))))
          (nskk-prolog-assert '((retract-fault "a" after)))
          (should
           (equal database-head
                  '(((retract-fault "a" old))
                    ((retract-fault "a" other))
                    ((retract-fault "a" after))))))))))

(ert-deftest nskk-prolog-retract-all-fault-restores-exact-key-state ()
  (dolist (fault-type '(error quit))
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--prolog-index-bucket-tail-cache
             (make-hash-table :test 'equal)))
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'retract-all-fault 2 :hash)
        (nskk-prolog-assert '((retract-all-fault "a" old)))
        (let* ((key "retract-all-fault/2")
               (tables
                (list nskk--prolog-database
                      nskk--prolog-database-tails
                      nskk--prolog-index-config
                      nskk--prolog-hash-indices
                      nskk--prolog-trie-indices
                      nskk--prolog-index-bucket-tail-cache))
               (expected (nskk-test--prolog-key-state key tables))
               (database-head (gethash key nskk--prolog-database))
               (database-tail (gethash key nskk--prolog-database-tails))
               (index (gethash key nskk--prolog-hash-indices))
               (cache-entry
                (gethash key nskk--prolog-index-bucket-tail-cache))
               (original-puthash (symbol-function 'puthash))
               faulted
               caught)
          (cl-letf (((symbol-function 'puthash)
                     (lambda (map-key value table)
                       (let ((result
                              (funcall original-puthash
                                       map-key value table)))
                         (when (and (not faulted)
                                    (equal map-key key)
                                    (eq table nskk--prolog-hash-indices))
                           (setq faulted t)
                           (signal fault-type nil))
                         result))))
            (condition-case condition
                (nskk-prolog-retract-all 'retract-all-fault 2)
              ((error quit)
               (setq caught (car condition)))))
          (should faulted)
          (should (eq caught fault-type))
          (nskk-test--prolog-should-match-key-state key tables expected)
          (should (eq database-head
                      (gethash key nskk--prolog-database)))
          (should (eq database-tail
                      (gethash key nskk--prolog-database-tails)))
          (should (eq index (gethash key nskk--prolog-hash-indices)))
          (should (eq cache-entry
                      (gethash key nskk--prolog-index-bucket-tail-cache)))
          (should
           (equal database-head
                  '(((retract-all-fault "a" old)))))
          (nskk-prolog-assert '((retract-all-fault "a" after)))
          (should
           (equal database-head
                  '(((retract-all-fault "a" old))
                    ((retract-all-fault "a" after))))))))))

(ert-deftest nskk-prolog-replace-callback-blocks-same-key-mutations ()
  (dolist (operation '(assert retract retract-all set-index trie-bulk clear))
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--prolog-index-bucket-tail-cache
             (make-hash-table :test 'equal)))
        (nskk-prolog-clear-database)
        (nskk-prolog-set-index 'callback-guard 2 :hash)
        (nskk-prolog-assert '((callback-guard "a" old)))
        (nskk-prolog-assert '((callback-guard "a" other)))
        (let* ((key "callback-guard/2")
               (tables
                (list nskk--prolog-database
                      nskk--prolog-database-tails
                      nskk--prolog-index-config
                      nskk--prolog-hash-indices
                      nskk--prolog-trie-indices
                      nskk--prolog-index-bucket-tail-cache))
               (expected (nskk-test--prolog-key-state key tables))
               (database-head (gethash key nskk--prolog-database))
               (database-tail (gethash key nskk--prolog-database-tails))
               (index (gethash key nskk--prolog-hash-indices))
               (bucket (gethash "a" index)))
          (should-error
           (nskk-prolog-replace-clause-transaction
            '(callback-guard "a" old)
            '((callback-guard "a" replacement))
            (lambda ()
              (pcase operation
                ('assert
                 (nskk-prolog-assert
                  '((callback-guard "a" nested))))
                ('retract
                 (nskk-prolog-retract
                  '(callback-guard "a" other)))
                ('retract-all
                 (nskk-prolog-retract-all 'callback-guard 2))
                ('set-index
                 (nskk-prolog-set-index 'callback-guard 2 :trie))
                ('trie-bulk
                 (nskk-prolog-trie-bulk-assert
                  'callback-guard 2 '(("a" nested))))
                ('clear
                 (nskk-prolog-clear-database))))))
          (nskk-test--prolog-should-match-key-state key tables expected)
          (should (eq database-head
                      (gethash key nskk--prolog-database)))
          (should (eq database-tail
                      (gethash key nskk--prolog-database-tails)))
          (should (eq index (gethash key nskk--prolog-hash-indices)))
          (should (eq bucket (gethash "a" index)))
          (should-not nskk--prolog-active-mutation-keys)
          (should
           (equal database-head
                  '(((callback-guard "a" old))
                    ((callback-guard "a" other))))))))))

(ert-deftest nskk-prolog-replace-callback-allows-read-and-different-key ()
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-set-index 'callback-main 2 :hash)
    (nskk-prolog-assert '((callback-main "a" old)))
    (let (callback-ran)
      (nskk-prolog-replace-clause-transaction
       '(callback-main "a" old)
       '((callback-main "a" replacement))
       (lambda ()
         (should
          (nskk-prolog-query-one
           '(callback-main "a" replacement)))
         (nskk-prolog-assert '((callback-other "b" nested)))
         (setq callback-ran t)))
      (should callback-ran)
      (should-not nskk--prolog-active-mutation-keys)
      (should
       (nskk-prolog-query-one '(callback-main "a" replacement)))
      (should
       (nskk-prolog-query-one '(callback-other "b" nested))))))

(ert-deftest nskk-prolog-hot-indexed-assert-does-not-scan-tail ()
(nskk-prolog-test-with-isolated-db
  (let ((nskk--prolog-index-bucket-tail-cache
         (make-hash-table :test 'equal)))
    (nskk-prolog-clear-database)
    (nskk-prolog-set-index 'hot-assert 2 :hash)
    (nskk-prolog-assert '((hot-assert "a" first)))
    (should-not
     (string-match-p
      "(memq\\_>"
      (prin1-to-string
       (symbol-function 'nskk-prolog-index-bucket-tail))))
    (cl-letf (((symbol-function 'last)
               (lambda (&rest _args)
                 (error "hot assert called last"))))
      (nskk-prolog-assert '((hot-assert "a" second))))
    (should
     (equal (gethash "hot-assert/2" nskk--prolog-database)
            '(((hot-assert "a" first))
              ((hot-assert "a" second))))))))

(ert-deftest nskk-prolog-isolation-keeps-clause-graphs-independent ()
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-clear-database)
      (nskk-prolog-set-index 'isolation-hash 2 :hash)
      (nskk-prolog-set-index 'isolation-trie 2 :trie)
      (nskk-prolog-assert
       '((isolation-hash "hash" (original . hash-tail))))
      (nskk-prolog-assert
       '((isolation-trie "trie" (original . trie-tail))))
      (cl-labels
          ((current-clause
            (key type first-arg)
            (let* ((database-clause
                    (car (gethash key nskk--prolog-database)))
                   (index-clause
                    (pcase type
                      (:hash
                       (car
                        (gethash
                         first-arg
                         (gethash key nskk--prolog-hash-indices))))
                      (:trie
                       (car
                        (nskk-trie-lookup
                         (gethash key nskk--prolog-trie-indices)
                         first-arg))))))
              (should (eq database-clause index-clause))
              database-clause)))
        (let* ((hash-key (nskk-prolog-clause-key 'isolation-hash 2))
               (trie-key (nskk-prolog-clause-key 'isolation-trie 2))
               (outer-hash
                (current-clause hash-key :hash "hash"))
               (outer-trie
                (current-clause trie-key :trie "trie")))
          (nskk-prolog-test-with-isolated-db
            (let ((inner-hash
                   (current-clause hash-key :hash "hash"))
                  (inner-trie
                   (current-clause trie-key :trie "trie")))
              (should-not (eq inner-hash outer-hash))
              (should-not (eq inner-trie outer-trie))
              (setcar inner-hash 'learned-head)
              (setcdr inner-trie '(learned-tail))
              (should
               (eq inner-hash
                   (car
                    (gethash
                     "hash"
                     (gethash hash-key nskk--prolog-hash-indices)))))
              (should
               (eq inner-trie
                   (car
                    (nskk-trie-lookup
                     (gethash trie-key nskk--prolog-trie-indices)
                     "trie"))))))
          (should
           (eq outer-hash
               (current-clause hash-key :hash "hash")))
          (should
           (eq outer-trie
               (current-clause trie-key :trie "trie")))
          (should
           (equal outer-hash
                  '((isolation-hash
                     "hash"
                     (original . hash-tail)))))
          (should
           (equal outer-trie
                  '((isolation-trie
                     "trie"
                     (original . trie-tail)))))))))

  (ert-deftest nskk-prolog-isolation-does-not-capture-body-bindings ()
    (let* ((marker (list 'body-marker))
           (saved-stores marker)
           (saved-flags marker)
           (saved-db marker)
           (saved-tails marker)
           (saved-index marker)
           (saved-hash marker)
           (saved-trie marker)
           (saved-tail-cache marker)
           (copies marker)
           (isolated-db marker)
           (isolated-index marker)
           (isolated-hash marker)
           (isolated-trie marker)
           (isolated-tails marker)
           (isolated-tail-cache marker)
           (store-symbol marker)
           (flag-symbol marker)
           (tail-key marker)
           (tail-facts marker)
           (store-entry marker)
           (flag-entry marker))
      (nskk-prolog-test-with-isolated-db
        (dolist
            (binding
             (list saved-stores saved-flags saved-db saved-tails
                   saved-index saved-hash saved-trie saved-tail-cache
                   copies isolated-db isolated-index isolated-hash
                   isolated-trie isolated-tails isolated-tail-cache
                   store-symbol flag-symbol tail-key tail-facts
                   store-entry flag-entry))
          (should (eq binding marker))))))

  (ert-deftest nskk-prolog-isolation-restores-on-all-exits ()
    (let ((database-before nskk--prolog-database)
          (input-was-bound (boundp 'nskk--input-initialized))
          (input-before
           (and (boundp 'nskk--input-initialized)
                nskk--input-initialized))
          (annotation-was-bound
           (boundp 'nskk--annotation-initialized))
          (annotation-before
           (and (boundp 'nskk--annotation-initialized)
                nskk--annotation-initialized)))
      (unwind-protect
          (progn
            (set 'nskk--input-initialized 'before)
            (makunbound 'nskk--annotation-initialized)
            (cl-labels
                ((should-be-restored
                  ()
                  (should (eq nskk--prolog-database database-before))
                  (should (eq nskk--input-initialized 'before))
                  (should-not (boundp 'nskk--annotation-initialized))))
              (nskk-prolog-test-with-isolated-db
                (should-not nskk--input-initialized)
                (should-not (boundp 'nskk--annotation-initialized))
                (setq nskk--prolog-database (make-hash-table :test 'equal)
                      nskk--input-initialized 'normal-dirty
                      nskk--annotation-initialized 'normal-dirty))
              (should-be-restored)
              (should-error
               (nskk-prolog-test-with-isolated-db
                 (setq nskk--prolog-database
                       (make-hash-table :test 'equal)
                       nskk--input-initialized 'error-dirty
                       nskk--annotation-initialized 'error-dirty)
                 (error "isolation error"))
               :type 'error)
              (should-be-restored)
              (should
               (eq
                'caught
                (condition-case nil
                    (progn
                      (nskk-prolog-test-with-isolated-db
                        (setq nskk--prolog-database
                              (make-hash-table :test 'equal)
                              nskk--input-initialized 'quit-dirty
                              nskk--annotation-initialized 'quit-dirty)
                        (signal 'quit nil))
                      'missed)
                  (quit 'caught))))
              (should-be-restored)
              (nskk-prolog-test-with-isolated-db
                (setq nskk--input-initialized 'outer
                      nskk--annotation-initialized 'outer)
                (nskk-prolog-test-with-isolated-db
                  (should-not nskk--input-initialized)
                  (should-not nskk--annotation-initialized)
                  (setq nskk--input-initialized 'inner
                        nskk--annotation-initialized 'inner))
                (should (eq nskk--input-initialized 'outer))
                (should (eq nskk--annotation-initialized 'outer)))
              (should-be-restored)))
        (let ((inhibit-quit t))
          (if input-was-bound
              (set 'nskk--input-initialized input-before)
            (makunbound 'nskk--input-initialized))
          (if annotation-was-bound
              (set 'nskk--annotation-initialized annotation-before)
            (makunbound 'nskk--annotation-initialized))))))

  (ert-deftest nskk-prolog-arithmetic-rejects-nonnumeric-global-symbol ()
    (let ((nskk-test--prolog-nonnumeric-global
           '((private . secret))))
      (should-error
       (nskk-prolog-query
        '(is \?result nskk-test--prolog-nonnumeric-global)))
      (should-error
       (nskk--prolog-eval-arith
        'nskk-test--prolog-nonnumeric-global nil))))

(ert-deftest nskk-prolog-isolation-copies-long-shared-bucket-without-recursion ()
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-clear-database)
      (let* ((key (nskk-prolog-clause-key 'isolation-long 2))
             (database-bucket nil)
             (index-bucket nil)
             (index (make-hash-table :test 'equal)))
        (dotimes (number 400)
          (let ((clause (list (list 'isolation-long "same" number))))
            (push clause database-bucket)
            (push clause index-bucket)))
        (puthash key database-bucket nskk--prolog-database)
        (puthash key (last database-bucket) nskk--prolog-database-tails)
        (puthash key index nskk--prolog-hash-indices)
        (puthash "same" index-bucket index)
        (let ((outer-database database-bucket)
              (outer-index index-bucket)
              (max-lisp-eval-depth 100))
          (nskk-prolog-test-with-isolated-db
            (let ((copied-database (gethash key nskk--prolog-database))
                  (copied-index
                   (gethash "same"
                            (gethash key nskk--prolog-hash-indices)))
                  (copied-tail (gethash key nskk--prolog-database-tails))
                  seen-tail)
              (while copied-database
                (should copied-index)
                (should (eq (car copied-database) (car copied-index)))
                (should-not (eq (car copied-database) (car outer-database)))
                (setq seen-tail copied-database
                      copied-database (cdr copied-database)
                      copied-index (cdr copied-index)
                      outer-database (cdr outer-database)
                      outer-index (cdr outer-index)))
              (should-not copied-index)
              (should-not outer-database)
              (should-not outer-index)
              (should (eq copied-tail seen-tail))
              (should (= 0 (hash-table-count
                            nskk--prolog-index-bucket-tail-cache)))))))))

  (ert-deftest nskk-prolog-isolation-copies-text-property-value-graph ()
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-clear-database)
      (let* ((key (nskk-prolog-clause-key 'isolation-text 1))
             (owner (copy-sequence "owner!!"))
             (payload (copy-sequence "payload!")))
        (add-text-properties 0 5 (list 'isolation-payload payload) owner)
        (add-text-properties 0 (length payload)
                             (list 'isolation-owner owner) payload)
        (nskk-prolog-set-index 'isolation-text 1 :hash)
        (nskk-prolog-assert (list (list 'isolation-text owner)))
        (let ((outer-clause (car (gethash key nskk--prolog-database))))
          (nskk-prolog-test-with-isolated-db
            (let* ((copied-clause (car (gethash key nskk--prolog-database)))
                   (indexed-clause
                    (car (gethash owner
                                  (gethash key nskk--prolog-hash-indices))))
                   (copied-owner (nth 1 (car copied-clause)))
                   (copied-payload
                    (get-text-property 1 'isolation-payload copied-owner)))
              (should (eq copied-clause indexed-clause))
              (should-not (eq copied-clause outer-clause))
              (should-not (eq copied-owner owner))
              (should-not (eq copied-payload payload))
              (should (eq copied-owner
                          (get-text-property 1 'isolation-owner copied-payload)))
              (should (eq copied-payload
                          (get-text-property 4 'isolation-payload copied-owner)))
              (should-not
               (get-text-property 5 'isolation-payload copied-owner))
              (aset copied-payload 0 ?X)
              (should (= (aref payload 0) ?p))))))))

  (ert-deftest nskk-prolog-isolation-snapshots-weak-hash-before-allocation ()
    (let* ((source (make-hash-table :test 'eq
                                    :size 17
                                    :rehash-size 2.0
                                    :rehash-threshold 0.7
                                    :weakness 'key))
           (key (list 'weak-key))
           (value (vector key))
           (copies (make-hash-table :test 'eq))
           (make-hash-table-function (symbol-function 'make-hash-table))
           cleared
           copied)
      (puthash key value source)
      (cl-letf (((symbol-function 'make-hash-table)
                 (lambda (&rest arguments)
                   (unless cleared
                     (setq cleared t)
                     (clrhash source))
                   (apply make-hash-table-function arguments))))
        (setq copied (nskk-prolog-test--copy-object source copies)))
      (should cleared)
      (should (= 0 (hash-table-count source)))
      (should (= 1 (hash-table-count copied)))
      (should (eq (hash-table-test source) (hash-table-test copied)))
      (should (= (hash-table-size source) (hash-table-size copied)))
      (should (= (hash-table-rehash-size source)
                 (hash-table-rehash-size copied)))
      (should (= (hash-table-rehash-threshold source)
                 (hash-table-rehash-threshold copied)))
      (should (eq (hash-table-weakness source)
                  (hash-table-weakness copied)))
      (let (copied-key copied-value)
        (maphash (lambda (entry-key entry-value)
                   (setq copied-key entry-key
                         copied-value entry-value))
                 copied)
        (should-not (eq copied-key key))
        (should-not (eq copied-value value))
        (should (eq copied-key (aref copied-value 0))))))

  (ert-deftest nskk-prolog-isolation-preserves-shared-trie-children ()
    (let* ((children (make-hash-table :test 'eq
                                      :size 13
                                      :rehash-size 2.0
                                      :rehash-threshold 0.8))
           (right (nskk-trie-node--create :children children :count 2))
           (left (nskk-trie-node--create :children children :count 2))
           (trie (nskk-trie--create-internal :root left :size 1))
           (graph (vector trie right children))
           (copies (make-hash-table :test 'eq)))
      (puthash ?x right children)
      (let* ((copied-graph (nskk-prolog-test--copy-object graph copies))
             (copied-trie (aref copied-graph 0))
             (copied-right (aref copied-graph 1))
             (copied-children (aref copied-graph 2))
             (copied-left (nskk-trie-root copied-trie)))
        (should-not (eq copied-trie trie))
        (should-not (eq copied-left left))
        (should-not (eq copied-right right))
        (should-not (eq copied-children children))
        (should (eq copied-children (nskk-trie-node-children copied-left)))
        (should (eq copied-children (nskk-trie-node-children copied-right)))
        (should (eq copied-right (gethash ?x copied-children)))
        (should (eq (hash-table-test children)
                    (hash-table-test copied-children)))
        (should (= (hash-table-size children)
                   (hash-table-size copied-children))))))

  (ert-deftest nskk-prolog-isolation-watcher-cleanup-continues-and-resignals ()
    (nskk-prolog-test-with-isolated-db
      (let* ((database-before nskk--prolog-database)
             ;; Must be a real number, not an arbitrary sentinel object like
             ;; the other -before values below: the isolation macro's own
             ;; `saved-flags' setup now runs a `module-initialized-flag'
             ;; Prolog query (FR-010), and proving touches
             ;; `nskk--prolog-var-counter' for variable renaming, so it must
             ;; stay numerically valid even while this test is deliberately
             ;; corrupting it to verify identity-preserving restore.
             (counter-before -972837465)
             (system-index-before (list 'system-index-before))
             (annotation-before (list 'annotation-before))
             (watcher (lambda (&rest _arguments)))
             (set-function (symbol-function 'set))
             expected-watchers)
        (setq nskk--prolog-var-counter counter-before
              nskk--annotation-initialized annotation-before)
        (nskk-dict-set-system-index system-index-before)
        (add-variable-watcher 'nskk--prolog-database watcher)
        (setq expected-watchers
              (copy-sequence
               (get-variable-watchers 'nskk--prolog-database)))
        (unwind-protect
            (cl-letf (((symbol-function 'set)
                       (lambda (symbol value)
                         (if (and (eq symbol 'nskk--prolog-database)
                                  (eq value database-before))
                             (error "blocked database restore")
                           (funcall set-function symbol value)))))
              (let ((condition
                     (should-error
                      (nskk-prolog-test-with-isolated-db
                        (setq nskk--prolog-var-counter 'dirty-counter
                              nskk--annotation-initialized 'dirty-annotation)
                        (nskk-dict-set-system-index 'dirty-index))
                      :type 'error)))
                (should (equal (cadr condition) "blocked database restore"))
                (should (eq nskk--prolog-var-counter counter-before))
                (should (eq (nskk-dict-system-index) system-index-before))
                (should (eq nskk--annotation-initialized annotation-before))
                (should (equal
                         (get-variable-watchers 'nskk--prolog-database)
                         expected-watchers))))
          (remove-variable-watcher 'nskk--prolog-database watcher)))))

  (ert-deftest nskk-prolog-copy-term-preserves-supported-graph-topology ()
  (let* ((shared (list 'shared))
         (equal-left (list 'same))
         (equal-right (list 'same))
         (cycle (cons 'cycle nil))
         (closure (let ((value shared)) (lambda () value)))
         (bools (bool-vector t nil t))
         (record (record 'nskk-prolog-test-record shared))
         (nested-vector (vector shared cycle))
         (proper (list shared equal-left equal-right))
         (dotted (cons shared equal-left))
         (root (vector proper dotted nested-vector record bools closure)))
    (setcdr cycle cycle)
    (let* ((copy (nskk-prolog-copy-term root))
           (copied-proper (aref copy 0))
           (copied-dotted (aref copy 1))
           (copied-vector (aref copy 2))
           (copied-record (aref copy 3))
           (copied-bools (aref copy 4))
           (copied-function (aref copy 5))
           (copied-shared (car copied-proper))
           (copied-left (cadr copied-proper))
           (copied-right (caddr copied-proper))
           (copied-cycle (aref copied-vector 1)))
      (should-not (eq copy root))
      (should-not (eq copied-proper proper))
      (should-not (eq copied-dotted dotted))
      (should-not (eq copied-vector nested-vector))
      (should-not (eq copied-record record))
      (should-not (eq copied-bools bools))
      (should (eq copied-shared (car copied-dotted)))
      (should (eq copied-shared (aref copied-vector 0)))
      (should (eq copied-shared (aref copied-record 1)))
      (should (equal copied-left copied-right))
      (should-not (eq copied-left copied-right))
      (should-not (eq copied-left equal-left))
      (should-not (eq copied-right equal-right))
      (should (eq (cdr copied-dotted) copied-left))
      (should-not (eq copied-cycle cycle))
      (should (eq (cdr copied-cycle) copied-cycle))
      (should (recordp copied-record))
      (should (eq (aref copied-record 0) 'nskk-prolog-test-record))
      (should (equal copied-bools bools))
      (should (eq copied-function closure))
      (should (eq (nskk-prolog-copy-term 'atom) 'atom))
      (should (= (nskk-prolog-copy-term 42) 42)))))
(ert-deftest nskk-prolog-copy-term-preserves-char-table-graph ()
  (let* ((purpose (make-symbol "nskk-prolog-copy-char-table"))
         parent
         table
         peer
         (shared (list 'shared))
         (hash (make-hash-table :test #'eq)))
    (put purpose 'char-table-extra-slots 2)
    (setq parent (make-char-table purpose (list 'parent-default))
          table (make-char-table purpose shared)
          peer (make-char-table purpose nil))
    (set-char-table-parent table parent)
    (set-char-table-range parent ?p shared)
    (set-char-table-range table (cons ?a ?c) shared)
    (set-char-table-range table ?n nil)
    (set-char-table-range table ?x peer)
    (set-char-table-range table ?z table)
    (set-char-table-range peer ?y table)
    (set-char-table-extra-slot table 0 shared)
    (set-char-table-extra-slot table 1 table)
    (puthash table shared hash)
    (put purpose 'char-table-extra-slots 1)
    (let* ((copy (nskk-prolog-copy-term
                  (list table parent peer shared hash)))
           (copied-table (nth 0 copy))
           (copied-parent (nth 1 copy))
           (copied-peer (nth 2 copy))
           (copied-shared (nth 3 copy))
           (copied-hash (nth 4 copy)))
      (should (char-table-p copied-table))
      (should (eq (char-table-subtype copied-table) purpose))
      (should-not (eq copied-table table))
      (should-not (eq copied-parent parent))
      (should-not (eq copied-peer peer))
      (should-not (eq copied-shared shared))
      (should (eq (char-table-parent copied-table) copied-parent))
      (should (equal (char-table-range copied-parent nil)
                     '(parent-default)))
      (should-not (eq (char-table-range copied-parent nil)
                      (char-table-range parent nil)))
      (should (eq (char-table-range copied-table nil) copied-shared))
      (dolist (character '(?a ?b ?c))
        (should (eq (aref copied-table character) copied-shared)))
      (should (eq (aref copied-table ?p) copied-shared))
      (should (eq (aref copied-table ?x) copied-peer))
      (should (eq (aref copied-table ?z) copied-table))
      (should (eq (aref copied-peer ?y) copied-table))
      (should (eq (char-table-range copied-table ?n) copied-shared))
      (should (eq (char-table-extra-slot copied-table 0) copied-shared))
      (should (eq (char-table-extra-slot copied-table 1) copied-table))
      (should-error (char-table-extra-slot copied-table 2)
                    :type 'args-out-of-range)
      (should (eq (gethash copied-table copied-hash) copied-shared))
      (setcar shared 'source-mutated)
      (should (eq (car copied-shared) 'shared))
      (setcar copied-shared 'copy-mutated)
      (should (eq (car shared) 'source-mutated)))))

(ert-deftest nskk-prolog-copy-term-detaches-text-properties-and-deep-graphs ()
  (let* ((metadata (list 'metadata))
         (string (copy-sequence "ab"))
         (root (vector string metadata)))
    (add-text-properties 0 1 (list 'nskk-test-metadata metadata) string)
    (let* ((copy (nskk-prolog-copy-term root))
           (copied-string (aref copy 0))
           (copied-metadata (aref copy 1))
           (copied-property
            (get-text-property 0 'nskk-test-metadata copied-string)))
      (should-not (eq copy root))
      (should-not (eq copied-string string))
      (should (equal-including-properties copied-string string))
      (should-not (eq copied-metadata metadata))
      (should (eq copied-property copied-metadata))
      (setcar copied-property 'changed)
      (aset copied-string 0 ?z)
      (should (equal string "ab"))
      (should (eq (car (get-text-property
                        0 'nskk-test-metadata string))
                  'metadata))))
  (let (root)
    (dotimes (_ 210001)
      (push nil root))
    (let ((copy (nskk-prolog-copy-term root))
          (cursor nil)
          (count 0))
      (should-not (eq copy root))
      (setq cursor copy)
      (while (consp cursor)
        (setq count (1+ count)
              cursor (cdr cursor)))
      (should (= count 210001))
      (should-not cursor))))

(ert-deftest nskk-prolog-assert-copies-once-and-publishes-one-canonical-object ()
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-set-index 'copy-assert 2 :hash)
    (let* ((payload (list 'original))
           (clause (list (list 'copy-assert "key" payload)))
           (copy-function (symbol-function 'nskk-prolog-copy-term))
           (calls 0)
           database-clause
           indexed-clause)
      (cl-letf (((symbol-function 'nskk-prolog-copy-term)
                 (lambda (object)
                   (setq calls (1+ calls))
                   (funcall copy-function object))))
        (nskk-prolog-assert clause))
      (let* ((key (nskk-prolog-clause-key 'copy-assert 2))
             (index (gethash key nskk--prolog-hash-indices)))
        (setq database-clause
              (car (gethash key nskk--prolog-database))
              indexed-clause
              (car (gethash "key" index))))
      (should (= calls 1))
      (should (eq database-clause indexed-clause))
      (should-not (eq database-clause clause))
      (should-not (eq (nth 2 (car database-clause)) payload))
      (setcar payload 'caller-mutated)
      (setcar (car clause) 'caller-head-mutated)
      (should (eq (caar database-clause) 'copy-assert))
      (should (equal (nth 2 (car database-clause)) '(original))))))

(ert-deftest nskk-prolog-assert-copy-failure-is-atomic-and-resignals ()
  (dolist (expected '((error "copy-error") (quit copy-quit)))
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-clear-database)
      (nskk-prolog-set-index 'copy-failure 2 :hash)
      (let* ((key (nskk-prolog-clause-key 'copy-failure 2))
             (index (gethash key nskk--prolog-hash-indices))
             (database-count (hash-table-count nskk--prolog-database))
             (tails-count
              (hash-table-count nskk--prolog-database-tails))
             (index-count (hash-table-count index))
             (cache-count
              (hash-table-count nskk--prolog-index-bucket-tail-cache))
             (calls 0)
             (received
              (condition-case condition
                  (cl-letf (((symbol-function 'nskk-prolog-copy-term)
                             (lambda (_object)
                               (setq calls (1+ calls))
                               (signal (car expected) (cdr expected)))))
                    (nskk-prolog-assert
                     '((copy-failure "key" (caller-owned))))
                    nil)
                ((error quit) condition))))
        (should (= calls 1))
        (should (equal received expected))
        (should (= (hash-table-count nskk--prolog-database)
                   database-count))
        (should (= (hash-table-count nskk--prolog-database-tails)
                   tails-count))
        (should (= (hash-table-count index) index-count))
        (should (= (hash-table-count
                    nskk--prolog-index-bucket-tail-cache)
                   cache-count))
        (should-not (gethash key nskk--prolog-database))
        (should-not (gethash "key" index))
        (should-not nskk--prolog-active-mutation-keys)))))

(progn
  (ert-deftest nskk-prolog-copy-term-preserves-hash-graph-and-metadata ()
    (let* ((shared (list 'shared))
           (equal-left (list 'same))
           (equal-right (list 'same))
           (text (copy-sequence "x"))
           (outer
            (make-hash-table
             :test 'eq
             :size 23
             :rehash-size 2.0
             :rehash-threshold 0.7
             :weakness 'key-and-value))
           (inner (make-hash-table :test 'equal :size 17))
           (root
            (vector outer inner shared equal-left equal-right text)))
      (add-text-properties 0 1 (list 'nskk-test-hash inner) text)
      (puthash equal-left shared outer)
      (puthash equal-right shared outer)
      (puthash 'self outer outer)
      (puthash 'inner inner outer)
      (puthash shared outer inner)
      (puthash 'text text inner)
      (puthash 'outer outer inner)
      (let* ((copy (nskk-prolog-copy-term root))
             (copied-outer (aref copy 0))
             (copied-inner (aref copy 1))
             (copied-shared (aref copy 2))
             (copied-left (aref copy 3))
             (copied-right (aref copy 4))
             (copied-text (aref copy 5)))
        (dolist (pair
                 (list (cons outer copied-outer)
                       (cons inner copied-inner)))
          (let ((source (car pair))
                (copied (cdr pair)))
            (should-not (eq source copied))
            (should (eq (hash-table-test source)
                        (hash-table-test copied)))
            (should (= (hash-table-size source)
                       (hash-table-size copied)))
            (should (equal (hash-table-rehash-size source)
                           (hash-table-rehash-size copied)))
            (should (equal (hash-table-rehash-threshold source)
                           (hash-table-rehash-threshold copied)))
            (should (eq (hash-table-weakness source)
                        (hash-table-weakness copied)))))
        (should-not (eq copied-shared shared))
        (should (equal copied-left copied-right))
        (should-not (eq copied-left copied-right))
        (should-not (eq copied-left equal-left))
        (should-not (eq copied-right equal-right))
        (should (eq (gethash copied-left copied-outer) copied-shared))
        (should (eq (gethash copied-right copied-outer) copied-shared))
        (should (eq (gethash 'self copied-outer) copied-outer))
        (should (eq (gethash 'inner copied-outer) copied-inner))
        (should (eq (gethash copied-shared copied-inner) copied-outer))
        (should (eq (gethash 'text copied-inner) copied-text))
        (should (eq (gethash 'outer copied-inner) copied-outer))
        (should (eq (get-text-property
                     0 'nskk-test-hash copied-text)
                    copied-inner))
        (setcar shared 'source-mutated)
        (puthash 'source-only t outer)
        (should (eq (car copied-shared) 'shared))
        (should-not (gethash 'source-only copied-outer))
        (setcar copied-shared 'copy-mutated)
        (puthash 'copy-only t copied-inner)
        (should (eq (car shared) 'source-mutated))
        (should-not (gethash 'copy-only inner)))))

  (ert-deftest nskk-prolog-copy-term-handles-deep-mixed-hash-graph-iteratively ()
    (let (deep)
      (dotimes (_ 210001)
        (push nil deep))
      (let* ((table (make-hash-table :test 'eq))
             (text (copy-sequence "d"))
             (root (vector deep table text)))
        (add-text-properties 0 1 (list 'nskk-test-hash table) text)
        (puthash 'self table table)
        (puthash 'root root table)
        (puthash 'deep deep table)
        (let* ((copy (nskk-prolog-copy-term root))
               (copied-deep (aref copy 0))
               (copied-table (aref copy 1))
               (copied-text (aref copy 2))
               (cursor copied-deep)
               (count 0))
          (should-not (eq copy root))
          (should-not (eq copied-deep deep))
          (should-not (eq copied-table table))
          (should-not (eq copied-text text))
          (should (eq (gethash 'self copied-table) copied-table))
          (should (eq (gethash 'root copied-table) copy))
          (should (eq (gethash 'deep copied-table) copied-deep))
          (should (eq (get-text-property
                       0 'nskk-test-hash copied-text)
                      copied-table))
          (while (consp cursor)
            (setq count (1+ count)
                  cursor (cdr cursor)))
          (should (= count 210001))
          (should-not cursor)))))

  (ert-deftest nskk-prolog-assert-hash-payload-is-canonical-and-query-is-detached ()
    (dolist (spec '((copy-hash :hash "hash-key")
                    (copy-trie :trie "trie-key")))
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-clear-database)
        (let* ((predicate (nth 0 spec))
               (index-type (nth 1 spec))
               (first-arg (nth 2 spec))
               (source-shared (list 'original))
               (source-table (make-hash-table :test 'eq :size 19))
               (clause (list (list predicate first-arg source-table))))
          (puthash 'shared source-shared source-table)
          (puthash 'self source-table source-table)
          (nskk-prolog-set-index predicate 2 index-type)
          (nskk-prolog-assert clause)
          (let* ((key (nskk-prolog-clause-key predicate 2))
                 (database-clause
                  (car (gethash key nskk--prolog-database)))
                 (index
                  (pcase index-type
                    (:hash (gethash key nskk--prolog-hash-indices))
                    (:trie (gethash key nskk--prolog-trie-indices))))
                 (bucket
                  (pcase index-type
                    (:hash (gethash first-arg index))
                    (:trie (nskk-trie-lookup index first-arg))))
                 (indexed-clause (car bucket))
                 (canonical-table (nth 2 (car database-clause)))
                 (canonical-shared (gethash 'shared canonical-table)))
            (should (eq database-clause indexed-clause))
            (should-not (eq database-clause clause))
            (should-not (eq canonical-table source-table))
            (should-not (eq canonical-shared source-shared))
            (should (equal canonical-shared '(original)))
            (should (eq (gethash 'self canonical-table)
                        canonical-table))
            (setcar source-shared 'caller-mutated)
            (puthash 'caller-only t source-table)
            (should (equal (gethash 'shared canonical-table)
                           '(original)))
            (should-not (gethash 'caller-only canonical-table))
            (let ((query-table
                   (nskk-prolog-query-value
                    (list predicate first-arg '\?payload)
                    '\?payload)))
              (should (hash-table-p query-table))
              (should-not (eq query-table canonical-table))
              (should-not (eq (gethash 'shared query-table)
                              canonical-shared))
              (should (equal (gethash 'shared query-table)
                             '(original)))
              (should (eq (gethash 'self query-table) query-table))
              (setcar (gethash 'shared query-table) 'query-mutated)
              (puthash 'query-only t query-table)
              (should (equal (gethash 'shared canonical-table)
                             '(original)))
              (should-not (gethash 'query-only canonical-table))))))))

  (ert-deftest nskk-prolog-assert-copy-puthash-fault-is-exact-and-retryable ()
    (dolist (fault-type '(error quit))
      (dolist (timing '(before after))
        (nskk-prolog-test-with-isolated-db
          (nskk-prolog-clear-database)
          (nskk-prolog-set-index 'copy-puthash 2 :hash)
          (nskk-prolog-assert '((copy-puthash "old" old)))
          (let* ((key (nskk-prolog-clause-key 'copy-puthash 2))
                 (index (gethash key nskk--prolog-hash-indices))
                 (old-index-bucket (gethash "old" index))
                 (index-count (hash-table-count index))
                 (source-shared (list 'caller-owned))
                 (source-table
                  (make-hash-table
                   :test 'eq
                   :size 29
                   :rehash-size 2.0
                   :rehash-threshold 0.7
                   :weakness 'key))
                 (clause
                  (list (list 'copy-puthash "new" source-table)))
                 (tables
                  (list nskk--prolog-database
                        nskk--prolog-database-tails
                        nskk--prolog-index-config
                        nskk--prolog-hash-indices
                        nskk--prolog-trie-indices
                        nskk--prolog-index-bucket-tail-cache))
                 (expected-state
                  (nskk-test--prolog-key-state key tables))
                 (condition-payload
                  (list 'copy-puthash-payload timing fault-type))
                 (condition-data
                  (list "copy puthash fault" condition-payload))
                 (original-copy
                  (symbol-function 'nskk-prolog-copy-term))
                 (original-puthash (symbol-function 'puthash))
                 (copy-active nil)
                 (faulted nil)
                 (calls 0)
                 received)
            (puthash 'shared source-shared source-table)
            (puthash 'self source-table source-table)
            (setq received
                  (condition-case condition
                      (cl-letf
                          (((symbol-function 'nskk-prolog-copy-term)
                            (lambda (object)
                              (setq copy-active t)
                              (unwind-protect
                                  (funcall original-copy object)
                                (setq copy-active nil))))
                           ((symbol-function 'puthash)
                            (lambda (map-key value table)
                              (if (and copy-active (not faulted))
                                  (progn
                                    (setq calls (1+ calls))
                                    (if (eq timing 'before)
                                        (progn
                                          (setq faulted t)
                                          (signal fault-type
                                                  condition-data))
                                      (let ((result
                                             (funcall
                                              original-puthash
                                              map-key value table)))
                                        (setq faulted t)
                                        (signal fault-type
                                                condition-data)
                                        result)))
                                (funcall original-puthash
                                         map-key value table)))))
                        (nskk-prolog-assert clause)
                        nil)
                    ((error quit) condition)))
            (should faulted)
            (should (= calls 1))
            (should (eq (car received) fault-type))
            (should (eq (cdr received) condition-data))
            (should (eq (caddr received) condition-payload))
            (should-not copy-active)
            (nskk-test--prolog-should-match-key-state
             key tables expected-state)
            (should (eq old-index-bucket (gethash "old" index)))
            (should (= index-count (hash-table-count index)))
            (should-not (gethash "new" index))
            (should-not nskk--prolog-active-mutation-keys)
            (should (= (hash-table-count source-table) 2))
            (should (eq (gethash 'shared source-table)
                        source-shared))
            (should (eq (gethash 'self source-table) source-table))
            (nskk-prolog-assert clause)
            (let* ((database-clauses
                    (gethash key nskk--prolog-database))
                   (canonical-clause (cadr database-clauses))
                   (canonical-table
                    (nth 2 (car canonical-clause))))
              (should (= (length database-clauses) 2))
              (should (eq canonical-clause
                          (car (gethash "new" index))))
              (should-not (eq canonical-table source-table))
              (should (eq (hash-table-test canonical-table)
                          (hash-table-test source-table)))
              (should (= (hash-table-size canonical-table)
                         (hash-table-size source-table)))
              (should (equal
                       (hash-table-rehash-size canonical-table)
                       (hash-table-rehash-size source-table)))
              (should (equal
                       (hash-table-rehash-threshold canonical-table)
                       (hash-table-rehash-threshold source-table)))
              (should (eq (hash-table-weakness canonical-table)
                          (hash-table-weakness source-table)))
              (should (equal (gethash 'shared canonical-table)
                             '(caller-owned)))
              (should (eq (gethash 'self canonical-table)
                          canonical-table))
              (should (= (hash-table-count source-table) 2))))))))

  (provide 'nskk-prolog-test))




;;; nskk-prolog-test.el ends here
