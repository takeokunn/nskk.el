;;; nskk-trie-test.el --- Tests for nskk-trie.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-trie.el covering:
;; - Trie creation and basic properties
;; - Insertion and lookup
;; - Prefix search
;; - Deletion and cleanup
;; - Edge cases (empty strings, duplicates, large datasets)
;; - Serialization and deserialization
;; - Statistics and utility functions

;;; Code:

(require 'ert)
(require 'nskk-trie)
(require 'nskk-test-framework)

;;;
;;; Trie Creation Tests
;;;

(nskk-deftest-unit trie-create-empty
  "Test creating an empty trie."
  (let ((trie (nskk-trie-create)))
    (should (nskk-trie-p trie))
    (should (= (nskk-trie-size trie) 0))
    (should (nskk-trie-node-p (nskk-trie-root trie)))
    (should (null (nskk-trie-metadata trie)))))

(nskk-deftest-unit trie-empty-p-on-new-trie
  "Test nskk-trie-empty-p on a newly created trie."
  (let ((trie (nskk-trie-create)))
    (should (nskk-trie-empty-p trie))))

(nskk-deftest-unit trie-empty-p-after-insert
  "Test nskk-trie-empty-p after inserting an entry."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "abc" "value")
    (should (not (nskk-trie-empty-p trie)))))

;;;
;;; Insertion Tests
;;;

(nskk-deftest-unit trie-insert-single
  "Test inserting a single key-value pair."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "hello" "world")
    (should (= (nskk-trie-size trie) 1))))

(nskk-deftest-unit trie-insert-returns-trie
  "Test that insert returns the trie itself."
  (let ((trie (nskk-trie-create)))
    (should (eq (nskk-trie-insert trie "key" "val") trie))))

(nskk-deftest-unit trie-insert-multiple
  "Test inserting multiple key-value pairs."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "apple" 1)
    (nskk-trie-insert trie "banana" 2)
    (nskk-trie-insert trie "cherry" 3)
    (should (= (nskk-trie-size trie) 3))))

(nskk-deftest-unit trie-insert-duplicate-key
  "Test inserting duplicate key overwrites value without incrementing size."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "key" "original")
    (should (= (nskk-trie-size trie) 1))
    (nskk-trie-insert trie "key" "updated")
    (should (= (nskk-trie-size trie) 1))
    (let ((result (nskk-trie-lookup trie "key")))
      (should (equal (car result) "updated"))
      (should (cdr result)))))

(nskk-deftest-unit trie-insert-shared-prefix
  "Test inserting keys that share a prefix."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "abc" 1)
    (nskk-trie-insert trie "abd" 2)
    (nskk-trie-insert trie "ab" 3)
    (should (= (nskk-trie-size trie) 3))
    (should (equal (car (nskk-trie-lookup trie "abc")) 1))
    (should (equal (car (nskk-trie-lookup trie "abd")) 2))
    (should (equal (car (nskk-trie-lookup trie "ab")) 3))))

(nskk-deftest-unit trie-insert-japanese
  "Test inserting Japanese keys."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "かんじ" "漢字")
    (nskk-trie-insert trie "かんたん" "簡単")
    (nskk-trie-insert trie "かん" "缶")
    (should (= (nskk-trie-size trie) 3))
    (should (equal (car (nskk-trie-lookup trie "かんじ")) "漢字"))
    (should (equal (car (nskk-trie-lookup trie "かんたん")) "簡単"))
    (should (equal (car (nskk-trie-lookup trie "かん")) "缶"))))

(nskk-deftest-unit trie-insert-error-non-string-key
  "Test that inserting a non-string key signals an error."
  (let ((trie (nskk-trie-create)))
    (should-error (nskk-trie-insert trie 123 "value") :type 'error)
    (should-error (nskk-trie-insert trie nil "value") :type 'error)))

(nskk-deftest-unit trie-insert-error-empty-key
  "Test that inserting an empty string key signals an error."
  (let ((trie (nskk-trie-create)))
    (should-error (nskk-trie-insert trie "" "value") :type 'error)))

(nskk-deftest-unit trie-insert-nil-value
  "Test inserting nil as a value."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "key" nil)
    (should (= (nskk-trie-size trie) 1))
    (let ((result (nskk-trie-lookup trie "key")))
      (should (null (car result)))
      (should (cdr result)))))

(nskk-deftest-unit trie-insert-single-char
  "Test inserting single character keys."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "a" 1)
    (nskk-trie-insert trie "b" 2)
    (should (= (nskk-trie-size trie) 2))
    (should (equal (car (nskk-trie-lookup trie "a")) 1))
    (should (equal (car (nskk-trie-lookup trie "b")) 2))))

;;;
;;; Lookup Tests
;;;

(nskk-deftest-unit trie-lookup-existing-key
  "Test looking up an existing key."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "test" "value")
    (let ((result (nskk-trie-lookup trie "test")))
      (should (equal (car result) "value"))
      (should (eq (cdr result) t)))))

(nskk-deftest-unit trie-lookup-non-existing-key
  "Test looking up a non-existing key."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "test" "value")
    (let ((result (nskk-trie-lookup trie "other")))
      (should (null (car result)))
      (should (null (cdr result))))))

(nskk-deftest-unit trie-lookup-partial-key
  "Test looking up a key that is a prefix of an existing key."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "testing" "value")
    (let ((result (nskk-trie-lookup trie "test")))
      (should (null (car result)))
      (should (null (cdr result))))))

(nskk-deftest-unit trie-lookup-error-non-string
  "Test that looking up a non-string key signals an error."
  (let ((trie (nskk-trie-create)))
    (should-error (nskk-trie-lookup trie 123) :type 'error)))

(nskk-deftest-unit trie-lookup-values-existing
  "Test nskk-trie-lookup-values for existing key."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "key" "value")
    (should (equal (nskk-trie-lookup-values trie "key") "value"))))

(nskk-deftest-unit trie-lookup-values-non-existing
  "Test nskk-trie-lookup-values for non-existing key."
  (let ((trie (nskk-trie-create)))
    (should (null (nskk-trie-lookup-values trie "missing")))))

(nskk-deftest-unit trie-has-key-p-existing
  "Test nskk-trie-has-key-p for existing key."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "key" "value")
    (should (nskk-trie-has-key-p trie "key"))))

(nskk-deftest-unit trie-has-key-p-non-existing
  "Test nskk-trie-has-key-p for non-existing key."
  (let ((trie (nskk-trie-create)))
    (should (not (nskk-trie-has-key-p trie "missing")))))

(nskk-deftest-unit trie-has-key-p-prefix-only
  "Test nskk-trie-has-key-p for prefix that is not a key."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "testing" "value")
    (should (not (nskk-trie-has-key-p trie "test")))))

;;;
;;; Deletion Tests
;;;

(nskk-deftest-unit trie-delete-existing-key
  "Test deleting an existing key."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "key" "value")
    (should (= (nskk-trie-size trie) 1))
    (should (nskk-trie-delete trie "key"))
    (should (= (nskk-trie-size trie) 0))
    (should (not (nskk-trie-has-key-p trie "key")))))

(nskk-deftest-unit trie-delete-non-existing-key
  "Test deleting a non-existing key returns nil."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "key" "value")
    (should (null (nskk-trie-delete trie "other")))
    (should (= (nskk-trie-size trie) 1))))

(nskk-deftest-unit trie-delete-preserves-siblings
  "Test that deleting a key does not affect sibling keys."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "abc" 1)
    (nskk-trie-insert trie "abd" 2)
    (nskk-trie-delete trie "abc")
    (should (= (nskk-trie-size trie) 1))
    (should (not (nskk-trie-has-key-p trie "abc")))
    (should (nskk-trie-has-key-p trie "abd"))
    (should (equal (car (nskk-trie-lookup trie "abd")) 2))))

(nskk-deftest-unit trie-delete-preserves-parent
  "Test that deleting a child key does not affect parent key."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "ab" 1)
    (nskk-trie-insert trie "abc" 2)
    (nskk-trie-delete trie "abc")
    (should (= (nskk-trie-size trie) 1))
    (should (nskk-trie-has-key-p trie "ab"))
    (should (equal (car (nskk-trie-lookup trie "ab")) 1))))

(nskk-deftest-unit trie-delete-preserves-child
  "Test that deleting a parent key does not affect child key."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "ab" 1)
    (nskk-trie-insert trie "abc" 2)
    (nskk-trie-delete trie "ab")
    (should (= (nskk-trie-size trie) 1))
    (should (nskk-trie-has-key-p trie "abc"))
    (should (equal (car (nskk-trie-lookup trie "abc")) 2))))

(nskk-deftest-unit trie-delete-error-non-string
  "Test that deleting a non-string key signals an error."
  (let ((trie (nskk-trie-create)))
    (should-error (nskk-trie-delete trie 123) :type 'error)))

(nskk-deftest-unit trie-delete-all-keys
  "Test deleting all keys empties the trie."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "a" 1)
    (nskk-trie-insert trie "b" 2)
    (nskk-trie-insert trie "c" 3)
    (nskk-trie-delete trie "a")
    (nskk-trie-delete trie "b")
    (nskk-trie-delete trie "c")
    (should (= (nskk-trie-size trie) 0))
    (should (nskk-trie-empty-p trie))))

;;;
;;; Prefix Search Tests
;;;

(nskk-deftest-unit trie-prefix-search-basic
  "Test basic prefix search."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "abc" 1)
    (nskk-trie-insert trie "abd" 2)
    (nskk-trie-insert trie "xyz" 3)
    (let ((results (nskk-trie-prefix-search trie "ab")))
      (should (= (length results) 2))
      (should (assoc "abc" results))
      (should (assoc "abd" results))
      (should (not (assoc "xyz" results))))))

(nskk-deftest-unit trie-prefix-search-exact-match-included
  "Test that prefix search includes the exact prefix match."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "ab" 0)
    (nskk-trie-insert trie "abc" 1)
    (nskk-trie-insert trie "abd" 2)
    (let ((results (nskk-trie-prefix-search trie "ab")))
      (should (= (length results) 3))
      (should (assoc "ab" results)))))

(nskk-deftest-unit trie-prefix-search-empty-prefix
  "Test prefix search with empty prefix returns all entries."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "a" 1)
    (nskk-trie-insert trie "b" 2)
    (nskk-trie-insert trie "c" 3)
    (let ((results (nskk-trie-prefix-search trie "")))
      (should (= (length results) 3)))))

(nskk-deftest-unit trie-prefix-search-no-match
  "Test prefix search with no matching entries."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "abc" 1)
    (let ((results (nskk-trie-prefix-search trie "xyz")))
      (should (null results)))))

(nskk-deftest-unit trie-prefix-search-with-limit
  "Test prefix search with limit."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "aa" 1)
    (nskk-trie-insert trie "ab" 2)
    (nskk-trie-insert trie "ac" 3)
    (nskk-trie-insert trie "ad" 4)
    (let ((results (nskk-trie-prefix-search trie "a" 2)))
      (should (= (length results) 2)))))

(nskk-deftest-unit trie-prefix-search-japanese
  "Test prefix search with Japanese keys."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "かんじ" "漢字")
    (nskk-trie-insert trie "かんたん" "簡単")
    (nskk-trie-insert trie "かん" "缶")
    (nskk-trie-insert trie "きんし" "禁止")
    (let ((results (nskk-trie-prefix-search trie "かん")))
      (should (= (length results) 3))
      (should (assoc "かんじ" results))
      (should (assoc "かんたん" results))
      (should (assoc "かん" results))
      (should (not (assoc "きんし" results))))))

(nskk-deftest-unit trie-prefix-search-error-non-string
  "Test that prefix search with non-string signals an error."
  (let ((trie (nskk-trie-create)))
    (should-error (nskk-trie-prefix-search trie 123) :type 'error)))

;;;
;;; Clear and Keys Tests
;;;

(nskk-deftest-unit trie-clear
  "Test clearing a trie."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "a" 1)
    (nskk-trie-insert trie "b" 2)
    (nskk-trie-insert trie "c" 3)
    (should (= (nskk-trie-size trie) 3))
    (nskk-trie-clear trie)
    (should (= (nskk-trie-size trie) 0))
    (should (nskk-trie-empty-p trie))
    (should (not (nskk-trie-has-key-p trie "a")))))

(nskk-deftest-unit trie-clear-returns-trie
  "Test that clear returns the trie itself."
  (let ((trie (nskk-trie-create)))
    (should (eq (nskk-trie-clear trie) trie))))

(nskk-deftest-unit trie-keys-basic
  "Test nskk-trie-keys returns all keys."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "apple" 1)
    (nskk-trie-insert trie "banana" 2)
    (nskk-trie-insert trie "cherry" 3)
    (let ((keys (nskk-trie-keys trie)))
      (should (= (length keys) 3))
      (should (member "apple" keys))
      (should (member "banana" keys))
      (should (member "cherry" keys)))))

(nskk-deftest-unit trie-keys-empty
  "Test nskk-trie-keys on empty trie."
  (let ((trie (nskk-trie-create)))
    (should (null (nskk-trie-keys trie)))))

;;;
;;; Statistics Tests
;;;

(nskk-deftest-unit trie-statistics-empty
  "Test statistics on empty trie."
  (let* ((trie (nskk-trie-create))
         (stats (nskk-trie-statistics trie)))
    (should (= (plist-get stats :size) 0))
    (should (= (plist-get stats :node-count) 1))
    (should (= (plist-get stats :max-depth) 0))
    (should (= (plist-get stats :avg-depth) 0.0))))

(nskk-deftest-unit trie-statistics-with-data
  "Test statistics on trie with data."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "a" 1)
    (nskk-trie-insert trie "ab" 2)
    (nskk-trie-insert trie "abc" 3)
    (let ((stats (nskk-trie-statistics trie)))
      (should (= (plist-get stats :size) 3))
      (should (> (plist-get stats :node-count) 3))
      (should (= (plist-get stats :max-depth) 3))
      (should (> (plist-get stats :memory-usage) 0)))))

;;;
;;; Serialization Tests
;;;

(nskk-deftest-unit trie-serialize-empty
  "Test serializing an empty trie."
  (let* ((trie (nskk-trie-create))
         (data (nskk-trie-serialize trie)))
    (should (equal (plist-get data :version) "1.0"))
    (should (= (plist-get data :size) 0))
    (should (null (plist-get data :entries)))))

(nskk-deftest-unit trie-serialize-with-data
  "Test serializing a trie with data."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "key1" "val1")
    (nskk-trie-insert trie "key2" "val2")
    (let ((data (nskk-trie-serialize trie)))
      (should (equal (plist-get data :version) "1.0"))
      (should (= (plist-get data :size) 2))
      (should (= (length (plist-get data :entries)) 2)))))

(nskk-deftest-unit trie-deserialize-roundtrip
  "Test that serialize/deserialize preserves data."
  (let ((trie (nskk-trie-create)))
    (nskk-trie-insert trie "apple" "りんご")
    (nskk-trie-insert trie "banana" "バナナ")
    (nskk-trie-insert trie "cherry" "さくらんぼ")
    (let* ((data (nskk-trie-serialize trie))
           (restored (nskk-trie-deserialize data)))
      (should (= (nskk-trie-size restored) 3))
      (should (equal (car (nskk-trie-lookup restored "apple")) "りんご"))
      (should (equal (car (nskk-trie-lookup restored "banana")) "バナナ"))
      (should (equal (car (nskk-trie-lookup restored "cherry")) "さくらんぼ")))))

(nskk-deftest-unit trie-deserialize-invalid-version
  "Test deserializing with invalid version signals an error."
  (should-error (nskk-trie-deserialize '(:version "2.0" :entries nil))
                :type 'error))

(nskk-deftest-unit trie-serialize-with-metadata
  "Test that serialization preserves metadata."
  (let ((trie (nskk-trie-create)))
    (setf (nskk-trie-metadata trie) '(:source "test" :date "2025"))
    (nskk-trie-insert trie "key" "val")
    (let* ((data (nskk-trie-serialize trie))
           (restored (nskk-trie-deserialize data)))
      (should (equal (nskk-trie-metadata restored) '(:source "test" :date "2025"))))))

;;;
;;; Large Dataset Tests
;;;

(nskk-deftest-unit trie-large-dataset
  "Test trie with a large number of entries."
  (let ((trie (nskk-trie-create))
        (n 500))
    (dotimes (i n)
      (nskk-trie-insert trie (format "key-%04d" i) i))
    (should (= (nskk-trie-size trie) n))
    ;; Verify random lookups
    (should (equal (car (nskk-trie-lookup trie "key-0000")) 0))
    (should (equal (car (nskk-trie-lookup trie "key-0250")) 250))
    (should (equal (car (nskk-trie-lookup trie "key-0499")) 499))
    (should (not (cdr (nskk-trie-lookup trie "key-0500"))))))

(nskk-deftest-unit trie-prefix-search-large-dataset
  "Test prefix search on large dataset."
  (let ((trie (nskk-trie-create)))
    (dotimes (i 100)
      (nskk-trie-insert trie (format "aa-%02d" i) i))
    (dotimes (i 100)
      (nskk-trie-insert trie (format "bb-%02d" i) i))
    (let ((results (nskk-trie-prefix-search trie "aa-")))
      (should (= (length results) 100)))))

;;;
;;; Integration: Insert -> Lookup -> Delete -> Verify
;;;

(nskk-deftest-integration trie-full-lifecycle
  "Test full trie lifecycle: create, insert, lookup, delete, verify."
  (let ((trie (nskk-trie-create)))
    ;; Insert
    (nskk-trie-insert trie "hello" "world")
    (nskk-trie-insert trie "help" "me")
    (nskk-trie-insert trie "world" "peace")
    (should (= (nskk-trie-size trie) 3))

    ;; Lookup
    (should (equal (car (nskk-trie-lookup trie "hello")) "world"))
    (should (equal (car (nskk-trie-lookup trie "help")) "me"))
    (should (nskk-trie-has-key-p trie "world"))

    ;; Prefix search
    (let ((results (nskk-trie-prefix-search trie "hel")))
      (should (= (length results) 2)))

    ;; Delete
    (should (nskk-trie-delete trie "hello"))
    (should (= (nskk-trie-size trie) 2))
    (should (not (nskk-trie-has-key-p trie "hello")))
    (should (nskk-trie-has-key-p trie "help"))

    ;; Verify prefix search after delete
    (let ((results (nskk-trie-prefix-search trie "hel")))
      (should (= (length results) 1))
      (should (equal (car (car results)) "help")))

    ;; Clear
    (nskk-trie-clear trie)
    (should (nskk-trie-empty-p trie))))

(nskk-deftest-integration trie-japanese-workflow
  "Test typical Japanese dictionary workflow."
  (let ((trie (nskk-trie-create)))
    ;; Build a small dictionary
    (nskk-trie-insert trie "あ" '("亜" "阿"))
    (nskk-trie-insert trie "あい" '("愛" "藍"))
    (nskk-trie-insert trie "あいう" '("相生"))
    (nskk-trie-insert trie "あお" '("青" "蒼"))
    (nskk-trie-insert trie "いう" '("言う"))

    ;; Prefix search for "あ" should return all "あ*" entries
    (let ((results (nskk-trie-prefix-search trie "あ")))
      (should (= (length results) 4))
      (should (assoc "あ" results))
      (should (assoc "あい" results))
      (should (assoc "あいう" results))
      (should (assoc "あお" results)))

    ;; Prefix search for "あい" should return 2 entries
    (let ((results (nskk-trie-prefix-search trie "あい")))
      (should (= (length results) 2)))

    ;; Lookup specific entry
    (let ((result (nskk-trie-lookup trie "あい")))
      (should (equal (car result) '("愛" "藍")))
      (should (cdr result)))))

(provide 'nskk-trie-test)

;;; nskk-trie-test.el ends here
