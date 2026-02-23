;;; nskk-dict-struct-test.el --- Tests for nskk-dict-struct.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-dict-struct.el covering:
;; - nskk-dict-entry struct: creation, accessors, predicates
;; - nskk-dict-index struct: creation, accessors, predicates
;; - nskk-dict-struct-entry-count function
;; - nskk-search-reset-stats function

;;; Code:

(require 'ert)
(require 'nskk-dict-struct)
(require 'nskk-test-framework)

;;;
;;; dict-entry Creation Tests
;;;

(nskk-deftest-unit dict-entry-create-default
  "Test creating a dict-entry with default values."
  (let ((entry (make-nskk-dict-entry)))
    (should (nskk-dict-entry-p entry))
    (should (null (nskk-dict-entry-key entry)))
    (should (null (nskk-dict-entry-candidates entry)))
    (should (null (nskk-dict-entry-okuri entry)))))

(nskk-deftest-unit dict-entry-create-with-key
  "Test creating a dict-entry with a key."
  (let ((entry (make-nskk-dict-entry :key "かんじ")))
    (should (nskk-dict-entry-p entry))
    (should (equal (nskk-dict-entry-key entry) "かんじ"))))

(nskk-deftest-unit dict-entry-create-with-all-fields
  "Test creating a dict-entry with all fields."
  (let ((entry (make-nskk-dict-entry
                :key "かんじ"
                :candidates '("漢字" "感じ" "幹事")
                :okuri "i")))
    (should (nskk-dict-entry-p entry))
    (should (equal (nskk-dict-entry-key entry) "かんじ"))
    (should (equal (nskk-dict-entry-candidates entry) '("漢字" "感じ" "幹事")))
    (should (equal (nskk-dict-entry-okuri entry) "i"))))

(nskk-deftest-unit dict-entry-create-with-nil-candidates
  "Test creating a dict-entry with nil candidates."
  (let ((entry (make-nskk-dict-entry :key "test" :candidates nil)))
    (should (nskk-dict-entry-p entry))
    (should (null (nskk-dict-entry-candidates entry)))))

;;;
;;; dict-entry Predicate Tests
;;;

(nskk-deftest-unit dict-entry-p-valid
  "Test nskk-dict-entry-p with a valid entry."
  (let ((entry (make-nskk-dict-entry :key "test")))
    (should (nskk-dict-entry-p entry))))

(nskk-deftest-unit dict-entry-p-nil
  "Test nskk-dict-entry-p with nil."
  (should (not (nskk-dict-entry-p nil))))

(nskk-deftest-unit dict-entry-p-non-entry
  "Test nskk-dict-entry-p with non-entry values."
  (should (not (nskk-dict-entry-p "string")))
  (should (not (nskk-dict-entry-p 123)))
  (should (not (nskk-dict-entry-p '(a b c))))
  (should (not (nskk-dict-entry-p (make-hash-table)))))

;;;
;;; dict-entry Accessor Tests
;;;

(nskk-deftest-unit dict-entry-accessor-key
  "Test nskk-dict-entry-key accessor."
  (let ((entry (make-nskk-dict-entry :key "あいう")))
    (should (equal (nskk-dict-entry-key entry) "あいう"))))

(nskk-deftest-unit dict-entry-accessor-candidates
  "Test nskk-dict-entry-candidates accessor."
  (let ((entry (make-nskk-dict-entry :candidates '("候補1" "候補2"))))
    (should (equal (nskk-dict-entry-candidates entry) '("候補1" "候補2")))))

(nskk-deftest-unit dict-entry-accessor-okuri
  "Test nskk-dict-entry-okuri accessor."
  (let ((entry (make-nskk-dict-entry :okuri "k")))
    (should (equal (nskk-dict-entry-okuri entry) "k"))))

(nskk-deftest-unit dict-entry-setf-key
  "Test setting dict-entry key with setf."
  (let ((entry (make-nskk-dict-entry)))
    (setf (nskk-dict-entry-key entry) "new-key")
    (should (equal (nskk-dict-entry-key entry) "new-key"))))

(nskk-deftest-unit dict-entry-setf-candidates
  "Test setting dict-entry candidates with setf."
  (let ((entry (make-nskk-dict-entry)))
    (setf (nskk-dict-entry-candidates entry) '("a" "b"))
    (should (equal (nskk-dict-entry-candidates entry) '("a" "b")))))

(nskk-deftest-unit dict-entry-setf-okuri
  "Test setting dict-entry okuri with setf."
  (let ((entry (make-nskk-dict-entry)))
    (setf (nskk-dict-entry-okuri entry) "t")
    (should (equal (nskk-dict-entry-okuri entry) "t"))))

;;;
;;; dict-index Creation Tests
;;;

(nskk-deftest-unit dict-index-create-default
  "Test creating a dict-index with default values."
  (let ((index (make-nskk-dict-index)))
    (should (nskk-dict-index-p index))
    (should (null (nskk-dict-index-entries index)))
    (should (null (nskk-dict-index-by-prefix index)))
    (should (null (nskk-dict-index-by-freq index)))))

(nskk-deftest-unit dict-index-create-with-hash-entries
  "Test creating a dict-index with hash table entries."
  (let ((entries (make-hash-table :test 'equal)))
    (puthash "key1" (make-nskk-dict-entry :key "key1") entries)
    (puthash "key2" (make-nskk-dict-entry :key "key2") entries)
    (let ((index (make-nskk-dict-index :entries entries)))
      (should (nskk-dict-index-p index))
      (should (hash-table-p (nskk-dict-index-entries index)))
      (should (= (hash-table-count (nskk-dict-index-entries index)) 2)))))

(nskk-deftest-unit dict-index-create-with-prefix-trie
  "Test creating a dict-index with a prefix trie."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "test" "value")
    (let ((index (make-nskk-dict-index :by-prefix trie)))
      (should (nskk-dict-index-p index))
      (should (nskk-trie-p (nskk-dict-index-by-prefix index))))))

;;;
;;; dict-index Predicate Tests
;;;

(nskk-deftest-unit dict-index-p-valid
  "Test nskk-dict-index-p with a valid index."
  (let ((index (make-nskk-dict-index)))
    (should (nskk-dict-index-p index))))

(nskk-deftest-unit dict-index-p-nil
  "Test nskk-dict-index-p with nil."
  (should (not (nskk-dict-index-p nil))))

(nskk-deftest-unit dict-index-p-non-index
  "Test nskk-dict-index-p with non-index values."
  (should (not (nskk-dict-index-p "string")))
  (should (not (nskk-dict-index-p 123)))
  (should (not (nskk-dict-index-p (make-nskk-dict-entry)))))

;;;
;;; dict-index Accessor Tests
;;;

(nskk-deftest-unit dict-index-accessor-entries
  "Test nskk-dict-index-entries accessor."
  (let ((entries '(("a" . "entry-a") ("b" . "entry-b")))
        (index (make-nskk-dict-index)))
    (setf (nskk-dict-index-entries index) entries)
    (should (equal (nskk-dict-index-entries index) entries))))

(nskk-deftest-unit dict-index-accessor-by-prefix
  "Test nskk-dict-index-by-prefix accessor."
  (let ((trie (nskk-trie-create))
        (index (make-nskk-dict-index)))
    (setf (nskk-dict-index-by-prefix index) trie)
    (should (eq (nskk-dict-index-by-prefix index) trie))))

(nskk-deftest-unit dict-index-accessor-by-freq
  "Test nskk-dict-index-by-freq accessor."
  (let ((freq-data '((10 . "entry-a") (5 . "entry-b")))
        (index (make-nskk-dict-index)))
    (setf (nskk-dict-index-by-freq index) freq-data)
    (should (equal (nskk-dict-index-by-freq index) freq-data))))

;;;
;;; nskk-dict-struct-entry-count Tests
;;;

(nskk-deftest-unit dict-struct-entry-count-nil-entries
  "Test entry count on index with nil entries."
  (let ((index (make-nskk-dict-index :entries nil)))
    (should (= (nskk-dict-struct-entry-count index nil) 0))))

(nskk-deftest-unit dict-struct-entry-count-list-entries
  "Test entry count on index with list entries."
  (let ((index (make-nskk-dict-index :entries '(a b c d e))))
    (should (= (nskk-dict-struct-entry-count index nil) 5))))

(nskk-deftest-unit dict-struct-entry-count-ignores-okuri-type
  "Test that entry count ignores okuri-type parameter."
  (let ((index (make-nskk-dict-index :entries '(a b c))))
    (should (= (nskk-dict-struct-entry-count index 'okuri-ari) 3))
    (should (= (nskk-dict-struct-entry-count index 'okuri-nasi) 3))
    (should (= (nskk-dict-struct-entry-count index nil) 3))))

;;;
;;; nskk-search-reset-stats Tests
;;;

(nskk-deftest-unit search-reset-stats
  "Test nskk-search-reset-stats returns nil."
  (should (null (nskk-search-reset-stats))))

;;;
;;; Integration Tests
;;;

(nskk-deftest-integration dict-struct-build-index-workflow
  "Test building a dict-index with entries and trie."
  (let ((entries (make-hash-table :test 'equal))
        (trie (nskk-trie-create)))
    ;; Build entries
    (let ((entry1 (make-nskk-dict-entry :key "かんじ" :candidates '("漢字" "感じ")))
          (entry2 (make-nskk-dict-entry :key "にほん" :candidates '("日本")))
          (entry3 (make-nskk-dict-entry :key "にほんご" :candidates '("日本語"))))
      (puthash "かんじ" entry1 entries)
      (puthash "にほん" entry2 entries)
      (puthash "にほんご" entry3 entries)
      (nskk-trie-insert trie "かんじ" entry1)
      (nskk-trie-insert trie "にほん" entry2)
      (nskk-trie-insert trie "にほんご" entry3))

    ;; Build index
    (let ((index (make-nskk-dict-index :entries entries :by-prefix trie)))
      (should (nskk-dict-index-p index))
      (should (= (hash-table-count (nskk-dict-index-entries index)) 3))
      ;; Verify trie prefix search
      (let ((prefix-results (nskk-trie-prefix-search
                             (nskk-dict-index-by-prefix index)
                             "にほん")))
        (should (= (length prefix-results) 2))))))

(nskk-deftest-integration dict-struct-entry-with-okuri
  "Test dict-entry with okurigana information."
  (let ((entry (make-nskk-dict-entry
                :key "うごk"
                :candidates '("動く" "蠢く")
                :okuri "k")))
    (should (nskk-dict-entry-p entry))
    (should (equal (nskk-dict-entry-key entry) "うごk"))
    (should (equal (nskk-dict-entry-okuri entry) "k"))
    (should (= (length (nskk-dict-entry-candidates entry)) 2))))

(provide 'nskk-dict-struct-test)

;;; nskk-dict-struct-test.el ends here
