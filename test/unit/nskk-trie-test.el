;;; nskk-trie-test.el --- Tests for nskk-trie  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: Japanese, input, method, test, trie
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

;; Unit tests for nskk-trie.el trie data structure.
;;
;; Test categories:
;; - create: fresh trie invariants
;; - insert/lookup: round-trip for ASCII and Japanese keys, overwrite, absent key
;; - delete: present/absent key, size decrement, shared-prefix safety, leaf cleanup
;; - prefix-search: exact match, partial prefix, empty prefix, no-match, limit
;; - size tracking: insert, overwrite, delete lifecycle
;; - error cases: non-string key, empty key

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-trie)

;;;;
;;;; 1. nskk-trie-create
;;;;

(nskk-describe "nskk-trie-create"
  (nskk-it "creates an empty trie with size 0"
    (let ((trie (nskk-trie-create)))
      (should (= (nskk-trie-size trie) 0))))

  (nskk-it "creates a trie with a valid root node"
    (let ((trie (nskk-trie-create)))
      (should (nskk-trie-root trie)))))

;;;;
;;;; 2. insert / lookup
;;;;

(nskk-describe "nskk-trie-insert and nskk-trie-lookup"
  (nskk-context "ASCII key round-trip"
    (nskk-it "inserts and looks up a simple ASCII key"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "hello" "world")
        (should (equal (nskk-trie-lookup trie "hello") '("world" . t)))))

    (nskk-it "lookup returns (nil . nil) for an absent ASCII key"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "hello" "world")
        (should (equal (nskk-trie-lookup trie "helo") '(nil . nil)))))

    (nskk-it "lookup returns (nil . nil) when trie is empty"
      (let ((trie (nskk-trie-create)))
        (should (equal (nskk-trie-lookup trie "any") '(nil . nil))))))

  (nskk-context "Japanese kana key round-trip"
    (nskk-it "inserts and looks up a single kana key"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "か" "KA")
        (should (equal (nskk-trie-lookup trie "か") '("KA" . t)))))

    (nskk-it "inserts and looks up a multi-kana key"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "かんじ" '("漢字" "感じ"))
        (should (equal (nskk-trie-lookup trie "かんじ") '(("漢字" "感じ") . t)))))

    (nskk-it "lookup returns (nil . nil) for absent kana key"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "か" "KA")
        (should (equal (nskk-trie-lookup trie "き") '(nil . nil))))))

  (nskk-context "overwrite"
    (nskk-it "overwriting an existing key updates the value"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "key" "first")
        (nskk-trie-insert trie "key" "second")
        (should (equal (nskk-trie-lookup trie "key") '("second" . t)))))

    (nskk-it "overwriting an existing key does not change size"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "key" "first")
        (nskk-trie-insert trie "key" "second")
        (should (= (nskk-trie-size trie) 1)))))

  (nskk-context "multiple distinct keys"
    (nskk-it "all inserted keys are independently retrievable"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "ka"  "か")
        (nskk-trie-insert trie "ki"  "き")
        (nskk-trie-insert trie "ku"  "く")
        (should (equal (nskk-trie-lookup trie "ka") '("か" . t)))
        (should (equal (nskk-trie-lookup trie "ki") '("き" . t)))
        (should (equal (nskk-trie-lookup trie "ku") '("く" . t)))))

    (nskk-it "keys with shared prefix do not collide"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "ka"  "short")
        (nskk-trie-insert trie "kan" "medium")
        (nskk-trie-insert trie "kanji" "long")
        (should (equal (nskk-trie-lookup trie "ka")    '("short"  . t)))
        (should (equal (nskk-trie-lookup trie "kan")   '("medium" . t)))
        (should (equal (nskk-trie-lookup trie "kanji") '("long"   . t))))))

  (nskk-context "falsy stored values"
    (nskk-it "stores nil as a value and returns (nil . t)"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "nilkey" nil)
        (should (equal (nskk-trie-lookup trie "nilkey") '(nil . t)))))

    (nskk-it "stores 0 as a value and returns (0 . t)"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "zero" 0)
        (should (equal (nskk-trie-lookup trie "zero") '(0 . t)))))

    (nskk-it "stores empty string as a value and returns (\"\" . t)"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "emptyval" "")
        (should (equal (nskk-trie-lookup trie "emptyval") '("" . t)))))))

;;;;
;;;; 3. delete
;;;;

(nskk-describe "nskk-trie-delete"
  (nskk-context "deleting a present key"
    (nskk-it "returns t when the key exists"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "foo" "bar")
        (should (eq (nskk-trie-delete trie "foo") t))))

    (nskk-it "decrements size by 1"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "foo" "bar")
        (nskk-trie-insert trie "baz" "qux")
        (nskk-trie-delete trie "foo")
        (should (= (nskk-trie-size trie) 1))))

    (nskk-it "lookup returns (nil . nil) after deletion"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "foo" "bar")
        (nskk-trie-delete trie "foo")
        (should (equal (nskk-trie-lookup trie "foo") '(nil . nil))))))

  (nskk-context "deleting an absent key"
    (nskk-it "returns nil when the key is not present"
      (let ((trie (nskk-trie-create)))
        (should (null (nskk-trie-delete trie "ghost")))))

    (nskk-it "does not change size when deleting an absent key"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "real" "value")
        (nskk-trie-delete trie "ghost")
        (should (= (nskk-trie-size trie) 1)))))

  (nskk-context "shared-prefix safety"
    (nskk-it "deleting a longer key does not affect the shorter prefix key"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "ka"  "か")
        (nskk-trie-insert trie "kan" "かん")
        (nskk-trie-delete trie "kan")
        (should (equal (nskk-trie-lookup trie "ka") '("か" . t)))))

    (nskk-it "deleting a shorter key does not affect the longer key sharing that prefix"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "ka"  "か")
        (nskk-trie-insert trie "kan" "かん")
        (nskk-trie-delete trie "ka")
        (should (equal (nskk-trie-lookup trie "kan") '("かん" . t)))))

    (nskk-it "after deleting shorter key, lookup of it returns (nil . nil)"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "ka"  "か")
        (nskk-trie-insert trie "kan" "かん")
        (nskk-trie-delete trie "ka")
        (should (equal (nskk-trie-lookup trie "ka") '(nil . nil))))))

  (nskk-context "leaf node cleanup"
    (nskk-it "inserting then deleting the only key yields size 0"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "solo" "alone")
        (nskk-trie-delete trie "solo")
        (should (= (nskk-trie-size trie) 0))))))

;;;;
;;;; 4. prefix-search
;;;;

(nskk-describe "nskk-trie-prefix-search"
  (nskk-context "exact key as prefix"
    (nskk-it "prefix-search with the exact key returns a list containing that entry"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "ka" "か")
        (let ((results (nskk-trie-prefix-search trie "ka")))
          (should (member (cons "ka" "か") results))))))

  (nskk-context "partial prefix"
    (nskk-it "prefix か matches both か and かな but not き"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "か"   "ka-val")
        (nskk-trie-insert trie "かな" "kana-val")
        (nskk-trie-insert trie "き"   "ki-val")
        (let ((results (nskk-trie-prefix-search trie "か")))
          (should (member (cons "か"   "ka-val")   results))
          (should (member (cons "かな" "kana-val") results))
          (should-not (member (cons "き" "ki-val") results)))))

    (nskk-it "prefix ka matches ka, kan, kanji but not ki"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "ka"    "v1")
        (nskk-trie-insert trie "kan"   "v2")
        (nskk-trie-insert trie "kanji" "v3")
        (nskk-trie-insert trie "ki"    "v4")
        (let ((results (nskk-trie-prefix-search trie "ka")))
          (should (member (cons "ka"    "v1") results))
          (should (member (cons "kan"   "v2") results))
          (should (member (cons "kanji" "v3") results))
          (should-not (member (cons "ki" "v4") results))))))

  (nskk-context "empty prefix"
    (nskk-it "empty prefix returns all entries"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "a" 1)
        (nskk-trie-insert trie "b" 2)
        (nskk-trie-insert trie "c" 3)
        (let ((results (nskk-trie-prefix-search trie "")))
          (should (= (length results) 3))
          (should (member (cons "a" 1) results))
          (should (member (cons "b" 2) results))
          (should (member (cons "c" 3) results)))))

    (nskk-it "empty prefix on empty trie returns nil"
      (let ((trie (nskk-trie-create)))
        (should (null (nskk-trie-prefix-search trie ""))))))

  (nskk-context "no-match prefix"
    (nskk-it "prefix with no matching keys returns nil"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "alpha" "v1")
        (nskk-trie-insert trie "beta"  "v2")
        (should (null (nskk-trie-prefix-search trie "gamma"))))))

  (nskk-context "limit parameter"
    (nskk-it "limit caps the number of returned results"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "prefix-a" 1)
        (nskk-trie-insert trie "prefix-b" 2)
        (nskk-trie-insert trie "prefix-c" 3)
        (nskk-trie-insert trie "prefix-d" 4)
        (nskk-trie-insert trie "prefix-e" 5)
        (let ((results (nskk-trie-prefix-search trie "prefix-" 3)))
          (should (= (length results) 3)))))

    (nskk-it "limit larger than available results returns all matching entries"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "match-x" "v1")
        (nskk-trie-insert trie "match-y" "v2")
        (let ((results (nskk-trie-prefix-search trie "match-" 100)))
          (should (= (length results) 2)))))

    (nskk-it "limit of 0 returns nil"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "foo" "bar")
        (should (null (nskk-trie-prefix-search trie "foo" 0)))))))

;;;;
;;;; 5. size tracking
;;;;

(nskk-describe "nskk-trie-size"
  (nskk-it "fresh trie has size 0"
    (let ((trie (nskk-trie-create)))
      (should (= (nskk-trie-size trie) 0))))

  (nskk-it "size equals the number of distinct keys inserted"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "one"   1)
      (nskk-trie-insert trie "two"   2)
      (nskk-trie-insert trie "three" 3)
      (should (= (nskk-trie-size trie) 3))))

  (nskk-it "overwriting the same key keeps size unchanged"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "one" 1)
      (nskk-trie-insert trie "two" 2)
      (nskk-trie-insert trie "three" 3)
      (nskk-trie-insert trie "two" 99)
      (should (= (nskk-trie-size trie) 3))))

  (nskk-it "size decrements after a delete"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "one"   1)
      (nskk-trie-insert trie "two"   2)
      (nskk-trie-insert trie "three" 3)
      (nskk-trie-delete trie "two")
      (should (= (nskk-trie-size trie) 2))))

  (nskk-it "size is not decremented when deleting an absent key"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "one" 1)
      (nskk-trie-delete trie "none")
      (should (= (nskk-trie-size trie) 1)))))

;;;;
;;;; 6. error cases
;;;;

(nskk-describe "nskk-trie error cases"
  (nskk-context "non-string key"
    (nskk-it "signals an error when inserting with an integer key"
      (let ((trie (nskk-trie-create)))
        (should-error (nskk-trie-insert trie 42 "val"))))

    (nskk-it "signals an error when inserting with a symbol key"
      (let ((trie (nskk-trie-create)))
        (should-error (nskk-trie-insert trie 'sym "val"))))

    (nskk-it "signals an error when inserting with a list key"
      (let ((trie (nskk-trie-create)))
        (should-error (nskk-trie-insert trie '("a" "b") "val"))))

    (nskk-it "signals an error when inserting with nil as key"
      (let ((trie (nskk-trie-create)))
        (should-error (nskk-trie-insert trie nil "val")))))

  (nskk-context "empty string key"
    (nskk-it "signals an error when inserting with an empty string key"
      (let ((trie (nskk-trie-create)))
        (should-error (nskk-trie-insert trie "" "val"))))))

(provide 'nskk-trie-test)
;;; nskk-trie-test.el ends here
