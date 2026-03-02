;;; nskk-search-strategy-integration-test.el --- Search strategy integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests verifying cross-strategy consistency in nskk-search.el.
;;
;; These tests exercise exact, prefix, and partial search strategies on a
;; shared dictionary index to verify:
;; - Each strategy returns results consistent with its contract.
;; - Prefix search finds all entries that share a common prefix.
;; - Partial search finds all entries whose key contains a given substring.
;; - Cross-strategy consistency: the candidates returned for a given key are
;;   the same regardless of which strategy locates that key.
;; - The :limit parameter caps the number of results for prefix/partial.
;;
;; The shared fixture has a common "かん" prefix across four entries, plus
;; one unrelated entry ("かわ") to verify exclusion logic.
;;
;; Implementation note on fixture choice:
;; nskk-with-prolog-entries is used (not nskk-with-mock-dict) because
;; nskk-search-prefix calls (nskk-prolog-trie-prefix-search pred 2 query),
;; which looks for the trie at DB key "user-dict-entry/2".
;; nskk-with-mock-dict internally calls (nskk-prolog-set-index pred 1 :trie),
;; creating the trie at "user-dict-entry/1" — the wrong key.
;; nskk-with-prolog-entries calls (nskk-prolog-set-index pred 2 :trie),
;; placing the trie at the correct "user-dict-entry/2" key.

;;; Code:

(require 'ert)
(require 'nskk-search)
(require 'nskk-dictionary)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Shared test fixture

(defmacro nskk-search-strategy--with-fixture (&rest body)
  "Run BODY with the shared search strategy test fixture.
Provides five Prolog `user-dict-entry' facts: four entries share the
prefix \"かん\" and one \"かわ\" entry does not.  The trie is indexed at
arity=2 (via `nskk-with-prolog-entries') so that `nskk-search-prefix'
and `nskk-prolog-trie-prefix-search' operate correctly.

Note: `nskk-with-mock-dict' is intentionally avoided here because it
calls (nskk-prolog-set-index pred 1 :trie), placing the trie at the DB
key \\\"user-dict-entry/1\\\".  `nskk-prolog-trie-prefix-search' queries
arity=2, looking for \\\"user-dict-entry/2\\\", so prefix search returns nil
with the mock-dict fixture.  `nskk-with-prolog-entries' calls
(nskk-prolog-set-index pred 2 :trie), creating the trie at the correct
\\\"user-dict-entry/2\\\" key."
  `(nskk-with-prolog-entries
       ((user-dict-entry "かんじ"  ("漢字" "感じ" "幹事"))
        (user-dict-entry "かんき"  ("換気"))
        (user-dict-entry "かんこく" ("韓国"))
        (user-dict-entry "かんが"  ("考え"))
        (user-dict-entry "かわ"    ("川" "河")))
     ,@body))

;;;; Exact search

(nskk-describe "exact search strategy"

  (nskk-it "returns nskk-dict-entry for an existing key"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((result (nskk-search idx "かんじ" 'exact)))
          (should (nskk-dict-entry-p result))
          (should (string= "かんじ" (nskk-dict-entry-key result)))))))

  (nskk-it "returned entry candidates include 漢字"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((result (nskk-search idx "かんじ" 'exact)))
          (should (member "漢字" (nskk-dict-entry-candidates result)))))))

  (nskk-it "returns nil for a key not in the dictionary"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (should (null (nskk-search idx "zzzzz" 'exact))))))

  (nskk-it "かわ is found exactly"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((result (nskk-search idx "かわ" 'exact)))
          (should (nskk-dict-entry-p result))
          (should (member "川" (nskk-dict-entry-candidates result))))))))

;;;; Prefix search

(nskk-describe "prefix search strategy"

  (nskk-it "returns a list for a common prefix"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((results (nskk-search idx "かん" 'prefix)))
          (should (listp results))
          (should (> (length results) 0))))))

  (nskk-it "prefix search for かん finds all four かん-prefixed entries"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((keys (mapcar #'car (nskk-search idx "かん" 'prefix))))
          (should (member "かんじ"  keys))
          (should (member "かんき"  keys))
          (should (member "かんこく" keys))
          (should (member "かんが"  keys))))))

  (nskk-it "prefix search does not return entries lacking the prefix"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((keys (mapcar #'car (nskk-search idx "かん" 'prefix))))
          (should-not (member "かわ" keys))))))

  (nskk-it "prefix search with limit returns at most limit entries"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((results (nskk-search idx "かん" 'prefix nil 2)))
          (should (<= (length results) 2))))))

  (nskk-it "each prefix result value is a nskk-dict-entry"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (dolist (pair (nskk-search idx "かん" 'prefix))
          (should (nskk-dict-entry-p (cdr pair))))))))

;;;; Partial search

(nskk-describe "partial search strategy"

  (nskk-it "returns a list for a substring query"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((results (nskk-search idx "かん" 'partial)))
          (should (listp results))
          (should (> (length results) 0))))))

  (nskk-it "every partial result key contains the query substring"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (dolist (pair (nskk-search idx "かん" 'partial))
          (should (string-match-p "かん" (car pair)))))))

  (nskk-it "partial search does not return entries without the substring"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((keys (mapcar #'car (nskk-search idx "かん" 'partial))))
          (should-not (member "かわ" keys))))))

  (nskk-it "partial search with limit returns at most limit entries"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((results (nskk-search idx "かん" 'partial nil 2)))
          (should (<= (length results) 2)))))))

;;;; Cross-strategy consistency

(nskk-describe "cross-strategy consistency"

  (nskk-it "exact and prefix agree on candidates for the same key"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((exact-entry   (nskk-search idx "かんじ" 'exact))
              (prefix-results (nskk-search idx "かんじ" 'prefix)))
          (let ((prefix-pair (assoc "かんじ" prefix-results)))
            ;; Both strategies must find the key
            (should (nskk-dict-entry-p exact-entry))
            (should prefix-pair)
            ;; Candidates must be identical
            (should (equal (nskk-dict-entry-candidates exact-entry)
                           (nskk-dict-entry-candidates (cdr prefix-pair)))))))))

  (nskk-it "partial search finds the same key as exact search"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((partial-keys (mapcar #'car
                                    (nskk-search idx "かんじ" 'partial))))
          (should (member "かんじ" partial-keys))))))

  (nskk-it "prefix search for an exact key includes that key's entry"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((prefix-results (nskk-search idx "かんこく" 'prefix)))
          (let ((pair (assoc "かんこく" prefix-results)))
            (should pair)
            (should (member "韓国"
                            (nskk-dict-entry-candidates (cdr pair))))))))))

(provide 'nskk-search-strategy-integration-test)

;;; nskk-search-strategy-integration-test.el ends here
