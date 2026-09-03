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

;; Tests for nskk-trie .

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
        (should (equal (nskk-trie-lookup trie "hello") "world"))))

    (nskk-it "lookup returns nil for an absent ASCII key"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "hello" "world")
        (should (null (nskk-trie-lookup trie "helo")))))

    (nskk-it "lookup returns nil when trie is empty"
      (let ((trie (nskk-trie-create)))
        (should (null (nskk-trie-lookup trie "any"))))))

  (nskk-context "Japanese kana key round-trip"
    (nskk-it "inserts and looks up a single kana key"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "か" "KA")
        (should (equal (nskk-trie-lookup trie "か") "KA"))))

    (nskk-it "inserts and looks up a multi-kana key"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "かんじ" '("漢字" "感じ"))
        (should (equal (nskk-trie-lookup trie "かんじ") '("漢字" "感じ")))))

    (nskk-it "lookup returns nil for absent kana key"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "か" "KA")
        (should (null (nskk-trie-lookup trie "き"))))))

  (nskk-context "overwrite"
    (nskk-it "overwriting an existing key updates the value"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "key" "first")
        (nskk-trie-insert trie "key" "second")
        (should (equal (nskk-trie-lookup trie "key") "second"))))

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
        (should (equal (nskk-trie-lookup trie "ka") "か"))
        (should (equal (nskk-trie-lookup trie "ki") "き"))
        (should (equal (nskk-trie-lookup trie "ku") "く"))))

    (nskk-it "keys with shared prefix do not collide"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "ka"  "short")
        (nskk-trie-insert trie "kan" "medium")
        (nskk-trie-insert trie "kanji" "long")
        (should (equal (nskk-trie-lookup trie "ka")    "short"))
        (should (equal (nskk-trie-lookup trie "kan")   "medium"))
        (should (equal (nskk-trie-lookup trie "kanji") "long")))))

  (nskk-context "falsy stored values"
    (nskk-it "stores nil — /k calls on-found (not on-not-found)"
      (let* ((trie (nskk-trie-create))
             (found nil)
             (not-found nil))
        (nskk-trie-insert trie "nilkey" nil)
        (nskk-trie-lookup/k trie "nilkey"
                            (lambda (v) (setq found (cons v t)))
                            (lambda () (setq not-found t)))
        (should found)
        (should-not not-found)))

    (nskk-it "stores 0 as a value and returns 0"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "zero" 0)
        (should (equal (nskk-trie-lookup trie "zero") 0))))

    (nskk-it "stores empty string as a value and returns it"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "emptyval" "")
        (should (equal (nskk-trie-lookup trie "emptyval") ""))))))

;;;;
;;;; 3. delete
;;;;

(nskk-describe "nskk-trie-delete"
 (nskk-context "cyclic path atomicity"
   (nskk-it "rejects a self-cycle unchanged and deletes normally after repair"
     (let* ((trie (nskk-trie-create))
            (root (nskk-trie-root trie))
            (root-children (make-hash-table :test #'eql))
            (node-children (make-hash-table :test #'eql))
            (node (nskk-trie-node--create
                   :children node-children
                   :value :kept
                   :is-end t
                   :count 1)))
       (setf (nskk-trie-node-children root) root-children
             (nskk-trie-size trie) 1)
       (puthash ?a node root-children)
       (puthash ?a node node-children)
       (should-error (nskk-trie-delete trie "aa") :type 'error)
       (should (= (nskk-trie-size trie) 1))
       (should (eq (nskk-trie-root trie) root))
       (should (eq (nskk-trie-node-children root) root-children))
       (should (= (nskk-trie-node-count root) 0))
       (should-not (nskk-trie-node-is-end root))
       (should-not (nskk-trie-node-value root))
       (should (eq (gethash ?a root-children) node))
       (should (eq (nskk-trie-node-children node) node-children))
       (should (= (nskk-trie-node-count node) 1))
       (should (nskk-trie-node-is-end node))
       (should (eq (nskk-trie-node-value node) :kept))
       (should (eq (gethash ?a node-children) node))
       (should (eq (nskk-trie-lookup trie "aa") :kept))
       (let ((leaf (nskk-trie-node--create
                    :value :replacement
                    :is-end t
                    :count 1)))
         (setf (nskk-trie-node-is-end node) nil
               (nskk-trie-node-value node) nil)
         (puthash ?a leaf node-children)
         (should (eq (nskk-trie-delete trie "aa") t))
         (should (= (nskk-trie-size trie) 0))
         (should-not (gethash ?a root-children)))))
   (nskk-it "rejects a root back-edge without changing nodes or edges"
     (let* ((trie (nskk-trie-create))
            (root (nskk-trie-root trie))
            (root-children (make-hash-table :test #'eql))
            (node-children (make-hash-table :test #'eql))
            (node (nskk-trie-node--create
                   :children node-children
                   :value :node-kept
                   :is-end nil
                   :count 1)))
       (setf (nskk-trie-node-children root) root-children
             (nskk-trie-node-value root) :root-kept
             (nskk-trie-node-is-end root) t
             (nskk-trie-node-count root) 1
             (nskk-trie-size trie) 1)
       (puthash ?a node root-children)
       (puthash ?a root node-children)
       (should-error (nskk-trie-delete trie "aa") :type 'error)
       (should (= (nskk-trie-size trie) 1))
       (should (eq (nskk-trie-root trie) root))
       (should (eq (nskk-trie-node-children root) root-children))
       (should (= (nskk-trie-node-count root) 1))
       (should (nskk-trie-node-is-end root))
       (should (eq (nskk-trie-node-value root) :root-kept))
       (should (eq (gethash ?a root-children) node))
       (should (eq (nskk-trie-node-children node) node-children))
       (should (= (nskk-trie-node-count node) 1))
       (should-not (nskk-trie-node-is-end node))
       (should (eq (nskk-trie-node-value node) :node-kept))
       (should (eq (gethash ?a node-children) root))
       (should (eq (nskk-trie-lookup trie "aa") :root-kept)))))
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

    (nskk-it "lookup returns nil after deletion"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "foo" "bar")
        (nskk-trie-delete trie "foo")
        (should (null (nskk-trie-lookup trie "foo"))))))

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
    (nskk-it "deleting a longer key preserves shorter values and exact counts"
      (let* ((trie (nskk-trie-create))
             (root (nskk-trie-root trie)))
        (nskk-trie-insert trie "ka" :short)
        (nskk-trie-insert trie "kan" :long)
        (let* ((k-node (gethash ?k (nskk-trie-node-children root)))
               (a-node (gethash ?a (nskk-trie-node-children k-node)))
               (n-node (gethash ?n (nskk-trie-node-children a-node))))
          (should (= (nskk-trie-node-count k-node) 2))
          (should (= (nskk-trie-node-count a-node) 2))
          (should (= (nskk-trie-node-count n-node) 1))
          (should (eq (nskk-trie-delete trie "kan") t))
          (should (= (nskk-trie-size trie) 1))
          (should (eq (nskk-trie-lookup trie "ka") :short))
          (should (null (nskk-trie-lookup trie "kan")))
          (should (= (nskk-trie-node-count k-node) 1))
          (should (= (nskk-trie-node-count a-node) 1))
          (should (nskk-trie-node-is-end a-node))
          (should (eq (nskk-trie-node-value a-node) :short))
          (should (null (gethash ?n (nskk-trie-node-children a-node)))))))

    (nskk-it "deleting a shorter key preserves longer values and exact counts"
      (let* ((trie (nskk-trie-create))
             (root (nskk-trie-root trie)))
        (nskk-trie-insert trie "ka" :short)
        (nskk-trie-insert trie "kan" :long)
        (let* ((k-node (gethash ?k (nskk-trie-node-children root)))
               (a-node (gethash ?a (nskk-trie-node-children k-node)))
               (n-node (gethash ?n (nskk-trie-node-children a-node))))
          (should (eq (nskk-trie-delete trie "ka") t))
          (should (= (nskk-trie-size trie) 1))
          (should (null (nskk-trie-lookup trie "ka")))
          (should (eq (nskk-trie-lookup trie "kan") :long))
          (should (= (nskk-trie-node-count k-node) 1))
          (should (= (nskk-trie-node-count a-node) 1))
          (should (= (nskk-trie-node-count n-node) 1))
          (should-not (nskk-trie-node-is-end a-node))
          (should (null (nskk-trie-node-value a-node)))
          (should (nskk-trie-node-is-end n-node))
          (should (eq (nskk-trie-node-value n-node) :long)))))

    (nskk-it "after deleting shorter key, lookup of it returns nil"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "ka" "short")
        (nskk-trie-insert trie "kan" "long")
        (nskk-trie-delete trie "ka")
        (should (null (nskk-trie-lookup trie "ka"))))))

  (nskk-context "leaf node cleanup"
    (nskk-it "inserting then deleting the only key yields size 0"
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie "solo" "alone")
        (nskk-trie-delete trie "solo")
        (should (= (nskk-trie-size trie) 0)))))

  (nskk-context "deep iterative deletion"
    (nskk-it "deletes and reuses a 5000-character key without orphaned prefixes"
      (let* ((key (make-string 5000 ?x))
             (trie (nskk-trie-create))
             (root (nskk-trie-root trie)))
        (nskk-trie-insert trie key :first)
        (should (gethash ?x (nskk-trie-node-children root)))
        (should (eq (nskk-trie-delete trie key) t))
        (should (= (nskk-trie-size trie) 0))
        (should (null (nskk-trie-lookup trie key)))
        (should (null (gethash ?x (nskk-trie-node-children root))))
        (nskk-trie-insert trie key :second)
        (should (= (nskk-trie-size trie) 1))
        (should (eq (nskk-trie-lookup trie key) :second))
        (should (= (nskk-trie-node-count
                    (gethash ?x (nskk-trie-node-children root)))
                   1))
        (should (eq (nskk-trie-delete trie key) t))
        (should (= (nskk-trie-size trie) 0))
        (should (null (gethash ?x (nskk-trie-node-children root)))))))

  (nskk-context "atomic path preparation"
    (nskk-it "leaves the complete trie unchanged when preparation signals and retries normally"
      (let ((original-make-vector (symbol-function 'make-vector)))
        (dolist (condition (quote (error quit)))
          (let* ((key "atomic")
                 (trie (nskk-trie-create))
                 (root (nskk-trie-root trie))
                 (nodes (make-vector (1+ (length key)) nil))
                 (index 0)
                 (calls 0)
                 (caught nil)
                 (states nil)
                 (size-before nil))
            (nskk-trie-insert trie key :value)
            (aset nodes 0 root)
            (while (< index (length key))
              (aset nodes
                    (1+ index)
                    (gethash (aref key index)
                             (nskk-trie-node-children (aref nodes index))))
              (cl-incf index))
            (setq size-before (nskk-trie-size trie))
            (setq states
                  (mapcar
                   (lambda (node)
                     (list (nskk-trie-node-children node)
                           (nskk-trie-node-count node)
                           (nskk-trie-node-is-end node)
                           (nskk-trie-node-value node)))
                   (append nodes nil)))
            (cl-letf (((symbol-function 'make-vector)
                       (lambda (size initial-element)
                         (cl-incf calls)
                         (if (= calls 2)
                             (signal condition nil)
                           (funcall original-make-vector
                                    size initial-element)))))
              (setq caught
                    (condition-case err
                        (progn
                          (nskk-trie-delete trie key)
                          :no-signal)
                      ((error quit) (car err)))))
            (should (eq caught condition))
            (should (= calls 2))
            (should (= (nskk-trie-size trie) size-before))
            (should (eq (nskk-trie-root trie) root))
            (should (eq (nskk-trie-lookup trie key) :value))
            (dotimes (node-index (length nodes))
              (let ((node (aref nodes node-index))
                    (state (nth node-index states)))
                (should (eq (nskk-trie-node-children node)
                            (nth 0 state)))
                (should (= (nskk-trie-node-count node)
                           (nth 1 state)))
                (should (eq (nskk-trie-node-is-end node)
                            (nth 2 state)))
                (should (equal (nskk-trie-node-value node)
                               (nth 3 state)))))
            (should (eq (nskk-trie-delete trie key) t))
            (should (= (nskk-trie-size trie) 0))
            (should-not (nskk-trie-lookup trie key))
            (should-not (gethash ?a (nskk-trie-node-children root)))))))))

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

  ;; Table-driven: each trie query operation signals an error for non-string input.
  ;; All five operations share the same contract: key/prefix/input must be a string.
  (nskk-deftest-table trie-non-string-input-signals-error
    :description "Each trie operation signals an error when given a non-string argument"
    :columns (operation)
    :rows ((nskk-trie-lookup) (nskk-trie-delete) (nskk-trie-prefix-search)
           (nskk-trie-longest-match) (nskk-trie-has-prefix-p))
    :body (let ((trie (nskk-trie-create)))
            (should-error (funcall operation trie 42))))

  (nskk-context "empty string key"
    (nskk-it "signals an error when inserting with an empty string key"
      (let ((trie (nskk-trie-create)))
        (should-error (nskk-trie-insert trie "" "val"))))))

;;;;
;;;; 7. longest-match
;;;;

(nskk-describe "nskk-trie-longest-match"
  (nskk-it "returns (value . consumed-length) for single-char match"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "a" "A")
      (should (equal (nskk-trie-longest-match trie "abc") '("A" . 1)))))

  (nskk-it "returns the longest matching prefix"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "a" "short")
      (nskk-trie-insert trie "ab" "medium")
      (nskk-trie-insert trie "abc" "long")
      (should (equal (nskk-trie-longest-match trie "abcd") '("long" . 3)))))

  (nskk-it "returns nil when no prefix matches"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "xyz" "val")
      (should (null (nskk-trie-longest-match trie "abc")))))

  (nskk-it "skips non-terminal prefix nodes"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "abc" "full")
      (should (null (nskk-trie-longest-match trie "ab")))))

  (nskk-it "returns match when input is exactly the key"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "abc" "exact")
      (should (equal (nskk-trie-longest-match trie "abc") '("exact" . 3)))))

  (nskk-it "works with Japanese kana keys"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "か" "ka")
      (nskk-trie-insert trie "かん" "kan")
      (should (equal (nskk-trie-longest-match trie "かんじ") '("kan" . 2))))))

;;;;
;;;; 8. has-prefix-p
;;;;

(nskk-describe "nskk-trie-has-prefix-p"
  (nskk-it "returns non-nil for an exact key"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "abc" "val")
      (should (nskk-trie-has-prefix-p trie "abc"))))

  (nskk-it "returns non-nil for a proper prefix of a key"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "abcdef" "val")
      (should (nskk-trie-has-prefix-p trie "abc"))))

  (nskk-it "returns nil for a non-existent prefix"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "abc" "val")
      (should-not (nskk-trie-has-prefix-p trie "xyz"))))

  (nskk-it "returns non-nil for empty prefix (root always exists)"
    (let ((trie (nskk-trie-create)))
      (nskk-trie-insert trie "abc" "val")
      (should (nskk-trie-has-prefix-p trie "")))))

;;;;
;;;; 9. lookup/k CPS behavior
;;;;

(nskk-describe "nskk-trie-lookup/k"
  (nskk-it "calls on-found with value when key exists"
    (let* ((trie (nskk-trie-create))
           (result nil))
      (nskk-trie-insert trie "key" "val")
      (nskk-trie-lookup/k trie "key"
                          (lambda (v) (setq result v))
                          #'ignore)
      (should (equal result "val"))))

  (nskk-it "calls on-not-found when key is absent"
    (let* ((trie (nskk-trie-create))
           (not-found nil))
      (nskk-trie-lookup/k trie "missing"
                          #'ignore
                          (lambda () (setq not-found t)))
      (should not-found)))

  (nskk-it "calls on-found for falsy stored nil value"
    (let* ((trie (nskk-trie-create))
           (found-called nil)
           (not-found-called nil))
      (nskk-trie-insert trie "nilkey" nil)
      (nskk-trie-lookup/k trie "nilkey"
                          (lambda (v) (setq found-called (cons v t)))
                          (lambda () (setq not-found-called t)))
      (should found-called)
      (should-not not-found-called))))

;;;;
;;;; 10. longest-match/k CPS behavior
;;;;

(nskk-describe "nskk-trie-longest-match/k"
  (nskk-it "calls on-found with (value . length) on match"
    (let* ((trie (nskk-trie-create))
           (result nil))
      (nskk-trie-insert trie "ab" "AB")
      (nskk-trie-longest-match/k trie "abc"
                                 (lambda (v) (setq result v))
                                 #'ignore)
      (should (equal result '("AB" . 2)))))

  (nskk-it "calls on-not-found when no prefix matches"
    (let* ((trie (nskk-trie-create))
           (not-found nil))
      (nskk-trie-insert trie "xyz" "val")
      (nskk-trie-longest-match/k trie "abc"
                                 #'ignore
                                 (lambda () (setq not-found t)))
      (should not-found))))

;;;;
;;;; PBT: Trie invariants
;;;;

(nskk-describe "PBT: trie invariants"
  (nskk-it "trie-insert-lookup-roundtrip"
    (nskk-property-test trie-insert-lookup-roundtrip
      ((key romaji-basic))
      (let* ((trie (nskk-trie-create))
             (key-x (concat key "x"))
             (key-y (concat key "y")))
        (nskk-trie-insert trie key-x 42)
        (nskk-trie-insert trie key-y 99)
        (and (equal (nskk-trie-lookup trie key-x) 42)
             (equal (nskk-trie-lookup trie key-y) 99)))
      50))

  (nskk-it "trie-delete-removes-key"
    (nskk-property-test trie-delete-removes-key
      ((key romaji-basic))
      (let* ((trie (nskk-trie-create))
             (key-x (concat key "x"))
             (key-y (concat key "y")))
        (nskk-trie-insert trie key-x 42)
        (nskk-trie-insert trie key-y 99)
        (nskk-trie-delete trie key-x)
        (and (null (nskk-trie-lookup trie key-x))
             (equal (nskk-trie-lookup trie key-y) 99)))
      50))

  (nskk-it "insert-then-lookup always returns the stored value"
    (nskk-property-test trie-pbt-insert-lookup
      ((key   romaji-string)
       (value romaji-string))
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie key value)
        (equal (nskk-trie-lookup trie key) value))
      50))

  (nskk-it "size is 0 for a fresh trie regardless of lookup"
    (nskk-property-test trie-pbt-empty-lookup-nil
      ((key romaji-string))
      (let ((trie (nskk-trie-create)))
        (and (null (nskk-trie-lookup trie key))
             (= (nskk-trie-size trie) 0)))
      50))

  (nskk-it "insert increments size by 1 for a new key"
    (nskk-property-test trie-pbt-size-after-insert
      ((key romaji-string))
      (let ((trie (nskk-trie-create)))
        (let ((size-before (nskk-trie-size trie)))
          (nskk-trie-insert trie key "v")
          (= (nskk-trie-size trie) (1+ size-before))))
      50))

  (nskk-it "delete after insert reduces size back to 0"
    (nskk-property-test trie-pbt-delete-removes-key
      ((key romaji-string))
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie key "v")
        (nskk-trie-delete trie key)
        (and (null (nskk-trie-lookup trie key))
             (= (nskk-trie-size trie) 0)))
      50))

  (nskk-it "has-prefix-p is true for the key itself after insert"
    (nskk-property-test trie-pbt-has-prefix-after-insert
      ((key romaji-string))
      (let ((trie (nskk-trie-create)))
        (nskk-trie-insert trie key "v")
        (nskk-trie-has-prefix-p trie key))
      50)))

(provide 'nskk-trie-test)
;;; nskk-trie-test.el ends here
