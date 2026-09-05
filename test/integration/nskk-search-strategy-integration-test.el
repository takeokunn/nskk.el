;;; nskk-search-strategy-integration-test.el --- Search strategy integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Search strategy integration tests.

;;; Code:

(require 'ert)
(require 'nskk-search)
(require 'nskk-dictionary)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Shared test fixture

(defmacro nskk-search-strategy--with-fixture (&rest body)
  (declare (indent 0))
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

;; `nskk-dict-lookup' (nskk-dictionary.el) queries the fixed `dict-entry'
;; predicate, which is bridged from `user-dict-entry' by a Prolog rule
;; (see nskk-dictionary.el's `(nskk-prolog-<- (dict-entry ?k ?c)
;; (user-dict-entry ?k ?c))'), so it transparently sees the facts this
;; fixture asserts under `user-dict-entry'.  Unlike the deleted
;; `nskk-search', it returns a bare candidate list rather than an
;; `nskk-dict-entry' struct.  `nskk-dictionary-test.el' already covers
;; `nskk-dict-lookup' in isolation (found key, unknown key, empty key,
;; okuri-ari); the tests kept here exist to feed the cross-strategy
;; consistency checks below with a result computed independently of
;; `nskk-search-prefix'/`nskk-search-partial'.

(nskk-describe "exact lookup via nskk-dict-lookup"

  (nskk-it "returns candidates for an existing key"
    (nskk-search-strategy--with-fixture
      (let ((result (nskk-dict-lookup "かんじ")))
        (should (listp result))
        (should (member "漢字" result)))))

  (nskk-it "returns nil for a key not in the dictionary"
    (nskk-search-strategy--with-fixture
      (should (null (nskk-dict-lookup "zzzzz")))))

  (nskk-it "かわ is found exactly"
    (nskk-search-strategy--with-fixture
      (should (member "川" (nskk-dict-lookup "かわ"))))))

;;;; Prefix search

(nskk-describe "prefix search strategy"

  (nskk-it "returns a list for a common prefix"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((results (nskk-search-prefix idx "かん" nil nil)))
          (should (listp results))
          (should results)))))

  (nskk-it "prefix search for かん finds all four かん-prefixed entries"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((keys (mapcar #'car (nskk-search-prefix idx "かん" nil nil))))
          (should (member "かんじ"  keys))
          (should (member "かんき"  keys))
          (should (member "かんこく" keys))
          (should (member "かんが"  keys))))))

  (nskk-it "prefix search does not return entries lacking the prefix"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((keys (mapcar #'car (nskk-search-prefix idx "かん" nil nil))))
          (should-not (member "かわ" keys))))))

  (nskk-it "prefix search with limit returns at most limit entries"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((results (nskk-search-prefix idx "かん" nil 2)))
          (should (<= (length results) 2))))))

  (nskk-it "each prefix result value is a nskk-dict-entry"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (dolist (pair (nskk-search-prefix idx "かん" nil nil))
          (should (nskk-dict-entry-p (cdr pair))))))))

;;;; Partial search

(nskk-describe "partial search strategy"

  (nskk-it "returns a list for a substring query"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((results (nskk-search-partial idx "かん" nil nil)))
          (should (listp results))
          (should results)))))

  (nskk-it "every partial result key contains the query substring"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (dolist (pair (nskk-search-partial idx "かん" nil nil))
          (should (string-match-p "かん" (car pair)))))))

  (nskk-it "partial search does not return entries without the substring"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((keys (mapcar #'car (nskk-search-partial idx "かん" nil nil))))
          (should-not (member "かわ" keys))))))

  (nskk-it "partial search with limit returns at most limit entries"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((results (nskk-search-partial idx "かん" nil 2)))
          (should (<= (length results) 2)))))))

;;;; Cross-strategy consistency

(nskk-describe "cross-strategy consistency"

  (nskk-it "nskk-dict-lookup and nskk-search-prefix agree on candidates for the same key"
    (nskk-search-strategy--with-fixture
      (let* ((idx (make-nskk-dict-index :predicate 'user-dict-entry))
             (exact-candidates (nskk-dict-lookup "かんじ"))
             (prefix-results (nskk-search-prefix idx "かんじ" nil nil))
             (prefix-pair (assoc "かんじ" prefix-results)))
        (should exact-candidates)
        (should prefix-pair)
        (should (equal exact-candidates
                       (nskk-dict-entry-candidates (cdr prefix-pair)))))))

  (nskk-it "partial search finds the same key that exact lookup finds"
    (nskk-search-strategy--with-fixture
      (let* ((idx (make-nskk-dict-index :predicate 'user-dict-entry))
             (partial-keys (mapcar #'car
                                   (nskk-search-partial idx "かんじ" nil nil))))
        (should (nskk-dict-lookup "かんじ"))
        (should (member "かんじ" partial-keys)))))

  (nskk-it "prefix search for an exact key includes that key's entry"
    (nskk-search-strategy--with-fixture
      (let ((idx (make-nskk-dict-index :predicate 'user-dict-entry)))
        (let ((prefix-results (nskk-search-prefix idx "かんこく" nil nil)))
          (let ((pair (assoc "かんこく" prefix-results)))
            (should pair)
            (should (member "韓国"
                            (nskk-dict-entry-candidates (cdr pair))))))))))

(provide 'nskk-search-strategy-integration-test)

;;; nskk-search-strategy-integration-test.el ends here
