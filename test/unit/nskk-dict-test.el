;;; nskk-dict-test.el --- Tests for nskk-dictionary.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-dictionary.el covering:
;; - Error types defined in nskk-search.el
;; - nskk-dict-entry struct: creation, accessors, predicates
;; - nskk-dict-index struct: creation, accessors, predicates
;; - nskk-dict--struct-entry-count function
;; - Module loading and feature provision
;; - Trie file I/O and trie save/load functions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-dictionary)
(require 'nskk-search)
(require 'nskk-trie)
(require 'nskk-test-framework)

;;; Section 1: Error type tests

;;;
;;; Module Loading Tests
;;;

(nskk-deftest-unit dict-errors-feature-provided
  "Test that nskk-dict-errors feature is provided."
  (should (featurep 'nskk-dictionary)))

(nskk-deftest-unit dict-errors-require-idempotent
  "Test that requiring nskk-dict-errors multiple times is safe."
  (require 'nskk-dictionary)
  (require 'nskk-dictionary)
  (should (featurep 'nskk-dictionary)))

;;;
;;; Customization Group Tests
;;;

(nskk-deftest-unit dict-errors-customization-group-exists
  "Test that nskk-dict-errors customization group has documentation."
  (should (get 'nskk-dictionary 'group-documentation)))

;;;
;;; Error Condition Chain Tests
;;;

(nskk-deftest-unit dict-errors-search-error-conditions
  "Test that nskk-dict-search-error has correct error conditions."
  (let ((conditions (get 'nskk-dict-search-error 'error-conditions)))
    (should (listp conditions))
    (should (memq 'nskk-dict-search-error conditions))))

(nskk-deftest-unit dict-errors-search-invalid-query-conditions
  "Test that nskk-dict-search-invalid-query has correct error conditions."
  (let ((conditions (get 'nskk-dict-search-invalid-query 'error-conditions)))
    (should (listp conditions))
    (should (memq 'nskk-dict-search-invalid-query conditions))
    (should (memq 'nskk-dict-search-error conditions))))

(nskk-deftest-unit dict-errors-search-invalid-index-conditions
  "Test that nskk-dict-search-invalid-index has correct error conditions."
  (let ((conditions (get 'nskk-dict-search-invalid-index 'error-conditions)))
    (should (listp conditions))
    (should (memq 'nskk-dict-search-invalid-index conditions))
    (should (memq 'nskk-dict-search-error conditions))))

;;;
;;; Error Signaling Tests (using condition-case directly)
;;;

(nskk-deftest-unit dict-errors-search-error-signal
  "Test signaling nskk-dict-search-error."
  (let ((caught nil))
    (condition-case _err
        (signal 'nskk-dict-search-error '("test error"))
      (nskk-dict-search-error (setq caught t)))
    (should caught)))

(nskk-deftest-unit dict-errors-search-invalid-query-signal
  "Test signaling nskk-dict-search-invalid-query."
  (let ((caught nil))
    (condition-case _err
        (signal 'nskk-dict-search-invalid-query '("bad query"))
      (nskk-dict-search-invalid-query (setq caught t)))
    (should caught)))

(nskk-deftest-unit dict-errors-search-invalid-index-signal
  "Test signaling nskk-dict-search-invalid-index."
  (let ((caught nil))
    (condition-case _err
        (signal 'nskk-dict-search-invalid-index '("bad index"))
      (nskk-dict-search-invalid-index (setq caught t)))
    (should caught)))

(nskk-deftest-unit dict-errors-search-invalid-query-is-search-error
  "Test that nskk-dict-search-invalid-query is caught by nskk-dict-search-error handler."
  (let ((caught nil))
    (condition-case _err
        (signal 'nskk-dict-search-invalid-query '("bad query"))
      (nskk-dict-search-error (setq caught t)))
    (should caught)))

(nskk-deftest-unit dict-errors-search-invalid-index-is-search-error
  "Test that nskk-dict-search-invalid-index is caught by nskk-dict-search-error handler."
  (let ((caught nil))
    (condition-case _err
        (signal 'nskk-dict-search-invalid-index '("bad index"))
      (nskk-dict-search-error (setq caught t)))
    (should caught)))

;;;
;;; Error Data Preservation Tests
;;;

(nskk-deftest-unit dict-errors-search-error-data
  "Test that error data is preserved when signaling search errors."
  (condition-case err
      (signal 'nskk-dict-search-invalid-query '("test data"))
    (nskk-dict-search-invalid-query
     (should (equal (cadr err) "test data")))))

(nskk-deftest-unit dict-errors-search-error-data-list
  "Test that error data list is preserved."
  (condition-case err
      (signal 'nskk-dict-search-error '("msg" extra-data))
    (nskk-dict-search-error
     (should (equal (cadr err) "msg"))
     (should (eq (caddr err) 'extra-data)))))

(nskk-deftest-unit dict-errors-search-error-catch-with-condition-case
  "Test catching search errors with condition-case."
  (let ((caught nil))
    (condition-case _err
        (signal 'nskk-dict-search-error '("test"))
      (nskk-dict-search-error (setq caught t)))
    (should caught)))

;;;
;;; Error Message Tests
;;;

(nskk-deftest-unit dict-errors-search-error-message
  "Test error message for search errors."
  (let ((msg (get 'nskk-dict-search-error 'error-message)))
    (should (stringp msg))
    (should (string-match-p "search" (downcase msg)))))

(nskk-deftest-unit dict-errors-search-invalid-query-message
  "Test error message for invalid query errors."
  (let ((msg (get 'nskk-dict-search-invalid-query 'error-message)))
    (should (stringp msg))
    (should (string-match-p "query" (downcase msg)))))

(nskk-deftest-unit dict-errors-search-invalid-index-message
  "Test error message for invalid index errors."
  (let ((msg (get 'nskk-dict-search-invalid-index 'error-message)))
    (should (stringp msg))
    (should (string-match-p "index" (downcase msg)))))

;;;
;;; Error Type Differentiation Tests
;;;

(nskk-deftest-unit dict-errors-different-error-types-distinguishable
  "Test that different error types can be distinguished."
  (let ((query-caught nil)
        (index-caught nil))
    ;; Test query error
    (condition-case _err
        (signal 'nskk-dict-search-invalid-query '("test"))
      (nskk-dict-search-invalid-query (setq query-caught t))
      (nskk-dict-search-invalid-index (setq index-caught t)))
    (should query-caught)
    (should (not index-caught))

    ;; Test index error
    (setq query-caught nil)
    (setq index-caught nil)
    (condition-case _err
        (signal 'nskk-dict-search-invalid-index '("test"))
      (nskk-dict-search-invalid-query (setq query-caught t))
      (nskk-dict-search-invalid-index (setq index-caught t)))
    (should (not query-caught))
    (should index-caught)))

;;; Section 2: Data structure tests

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
;;; nskk-dict--struct-entry-count Tests
;;;

(nskk-deftest-unit dict-struct-entry-count-nil-entries
  "Test entry count on index with nil entries."
  (let ((index (make-nskk-dict-index :entries nil)))
    (should (= (nskk-dict--struct-entry-count index nil) 0))))

(nskk-deftest-unit dict-struct-entry-count-list-entries
  "Test entry count on index with list entries."
  (let ((index (make-nskk-dict-index :entries '(a b c d e))))
    (should (= (nskk-dict--struct-entry-count index nil) 5))))

(nskk-deftest-unit dict-struct-entry-count-ignores-okuri-type
  "Test that entry count ignores okuri-type parameter."
  (let ((index (make-nskk-dict-index :entries '(a b c))))
    (should (= (nskk-dict--struct-entry-count index 'okuri-ari) 3))
    (should (= (nskk-dict--struct-entry-count index 'okuri-nasi) 3))
    (should (= (nskk-dict--struct-entry-count index nil) 3))))

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

;;; Section 3: I/O tests

;;;
;;; Module Loading Tests
;;;

(nskk-deftest-unit dict-io-feature-provided
  "Test that nskk-dict-io feature is provided."
  (should (featurep 'nskk-dictionary)))

(nskk-deftest-unit dict-io-require-idempotent
  "Test that requiring nskk-dict-io multiple times is safe."
  (require 'nskk-dictionary)
  (require 'nskk-dictionary)
  (should (featurep 'nskk-dictionary)))

;;;
;;; Trie File I/O Tests (trie provides the actual I/O functions)
;;;

(nskk-deftest-unit dict-io-trie-save-and-load
  "Test saving and loading a trie to/from a file."
  (let ((trie (nskk-trie-create))
        (temp-file (make-temp-file "nskk-trie-test-" nil ".dat")))
    (unwind-protect
        (progn
          ;; Insert data
          (nskk-trie-insert trie "かんじ" "漢字")
          (nskk-trie-insert trie "にほん" "日本")
          (nskk-trie-insert trie "にほんご" "日本語")

          ;; Save to file
          (nskk-trie-save-to-file trie temp-file)

          ;; Verify file exists and has content
          (should (file-exists-p temp-file))
          (should (> (file-attribute-size (file-attributes temp-file)) 0))

          ;; Load from file
          (let ((loaded (nskk-trie-load-from-file temp-file)))
            (should (nskk-trie-p loaded))
            (should (= (nskk-trie-size loaded) 3))
            (should (equal (car (nskk-trie-lookup loaded "かんじ")) "漢字"))
            (should (equal (car (nskk-trie-lookup loaded "にほん")) "日本"))
            (should (equal (car (nskk-trie-lookup loaded "にほんご")) "日本語"))))
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(nskk-deftest-unit dict-io-trie-save-empty
  "Test saving and loading an empty trie."
  (let ((trie (nskk-trie-create))
        (temp-file (make-temp-file "nskk-trie-empty-" nil ".dat")))
    (unwind-protect
        (progn
          (nskk-trie-save-to-file trie temp-file)
          (let ((loaded (nskk-trie-load-from-file temp-file)))
            (should (nskk-trie-p loaded))
            (should (= (nskk-trie-size loaded) 0))
            (should (nskk-trie-empty-p loaded))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(nskk-deftest-unit dict-io-trie-save-with-metadata
  "Test saving and loading a trie with metadata."
  (let ((trie (nskk-trie-create))
        (temp-file (make-temp-file "nskk-trie-meta-" nil ".dat")))
    (unwind-protect
        (progn
          (setf (nskk-trie-metadata trie) '(:source "test-dict" :version "1.0"))
          (nskk-trie-insert trie "key" "value")
          (nskk-trie-save-to-file trie temp-file)

          (let ((loaded (nskk-trie-load-from-file temp-file)))
            (should (equal (nskk-trie-metadata loaded)
                           '(:source "test-dict" :version "1.0")))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(nskk-deftest-unit dict-io-trie-load-nonexistent-file
  "Test loading from nonexistent file signals an error."
  (should-error (nskk-trie-load-from-file "/tmp/nskk-nonexistent-file.dat")
                :type 'file-missing))

(nskk-deftest-unit dict-io-trie-save-load-large
  "Test saving and loading a trie with many entries."
  (let ((trie (nskk-trie-create))
        (temp-file (make-temp-file "nskk-trie-large-" nil ".dat"))
        (n 200))
    (unwind-protect
        (progn
          ;; Insert many entries
          (dotimes (i n)
            (nskk-trie-insert trie (format "key-%03d" i) (format "val-%03d" i)))
          (should (= (nskk-trie-size trie) n))

          ;; Save and load
          (nskk-trie-save-to-file trie temp-file)
          (let ((loaded (nskk-trie-load-from-file temp-file)))
            (should (= (nskk-trie-size loaded) n))
            ;; Verify some entries
            (should (equal (car (nskk-trie-lookup loaded "key-000")) "val-000"))
            (should (equal (car (nskk-trie-lookup loaded "key-100")) "val-100"))
            (should (equal (car (nskk-trie-lookup loaded "key-199")) "val-199"))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(nskk-deftest-unit dict-io-trie-file-content-readable
  "Test that saved trie file contains readable Emacs Lisp."
  (let ((trie (nskk-trie-create))
        (temp-file (make-temp-file "nskk-trie-readable-" nil ".dat")))
    (unwind-protect
        (progn
          (nskk-trie-insert trie "test" "value")
          (nskk-trie-save-to-file trie temp-file)

          ;; Read file and verify it starts with comment
          (with-temp-buffer
            (insert-file-contents temp-file)
            (goto-char (point-min))
            (should (looking-at ";;"))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(nskk-deftest-unit dict-io-trie-save-load-japanese-values
  "Test saving and loading trie with Japanese values."
  (let ((trie (nskk-trie-create))
        (temp-file (make-temp-file "nskk-trie-ja-" nil ".dat")))
    (unwind-protect
        (progn
          (nskk-trie-insert trie "あ" '("亜" "阿" "娃"))
          (nskk-trie-insert trie "い" '("位" "依" "威"))
          (nskk-trie-save-to-file trie temp-file)

          (let ((loaded (nskk-trie-load-from-file temp-file)))
            (should (equal (car (nskk-trie-lookup loaded "あ")) '("亜" "阿" "娃")))
            (should (equal (car (nskk-trie-lookup loaded "い")) '("位" "依" "威")))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;;
;;; Dict-entry Serialization Tests
;;;

(nskk-deftest-unit dict-io-entry-can-be-serialized
  "Test that dict-entry can be written and read back."
  (let ((entry (make-nskk-dict-entry :key "test" :candidates '("a" "b")))
        (temp-file (make-temp-file "nskk-entry-" nil ".dat")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (prin1 entry (current-buffer)))
          (let ((restored (with-temp-buffer
                            (insert-file-contents temp-file)
                            (read (current-buffer)))))
            ;; cl-defstruct records produce readable output
            (should (file-exists-p temp-file))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;;
;;; Integration Tests
;;;

(nskk-deftest-integration dict-io-trie-roundtrip-workflow
  "Test full workflow: create trie, populate, save, load, verify."
  (let ((temp-file (make-temp-file "nskk-trie-workflow-" nil ".dat")))
    (unwind-protect
        (progn
          ;; Phase 1: Create and populate
          (let ((trie (nskk-trie-create)))
            (nskk-trie-insert trie "あいう" "アイウ")
            (nskk-trie-insert trie "あいうえ" "アイウエ")
            (nskk-trie-insert trie "あいうえお" "アイウエオ")
            (nskk-trie-insert trie "かきく" "カキク")
            (nskk-trie-save-to-file trie temp-file))

          ;; Phase 2: Load and verify
          (let ((loaded (nskk-trie-load-from-file temp-file)))
            (should (= (nskk-trie-size loaded) 4))

            ;; Verify prefix search works on loaded trie
            (let ((results (nskk-trie-prefix-search loaded "あいう")))
              (should (= (length results) 3))
              (should (assoc "あいう" results))
              (should (assoc "あいうえ" results))
              (should (assoc "あいうえお" results)))

            ;; Verify exact lookup
            (should (equal (car (nskk-trie-lookup loaded "かきく")) "カキク"))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;;;
;;;; Dictionary Auto-Detection Tests
;;;;

(nskk-deftest-unit dict-detect-returns-empty-when-no-files
  "Auto-detection returns empty list when no dictionary files exist."
  (cl-letf (((symbol-function 'file-readable-p) (lambda (_f) nil)))
    (let ((result (nskk-dict--detect-system-dictionaries)))
      (should (listp result))
      (should (null result)))))

(nskk-deftest-unit dict-detect-finds-nix-profile-dict
  "Auto-detection finds dictionary in nix profile."
  (cl-letf (((symbol-function 'file-readable-p)
             (lambda (f) (string-match-p "nix-profile" f))))
    (let ((result (nskk-dict--detect-system-dictionaries)))
      (should result)
      (should (cl-some (lambda (p) (string-match-p "nix-profile" p)) result)))))

(nskk-deftest-unit dict-detect-finds-system-dict
  "Auto-detection finds dictionary in standard system path."
  (cl-letf (((symbol-function 'file-readable-p)
             (lambda (f) (string= f "/usr/share/skk/SKK-JISYO.L"))))
    (let ((result (nskk-dict--detect-system-dictionaries)))
      (should result)
      (should (member "/usr/share/skk/SKK-JISYO.L" result)))))

(nskk-deftest-unit dict-detect-respects-large-dictionary
  "Auto-detection includes nskk-large-dictionary when set."
  (let ((nskk-large-dictionary "/tmp/test-large-dict"))
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (f) (string= f "/tmp/test-large-dict"))))
      (let ((result (nskk-dict--detect-system-dictionaries)))
        (should result)
        (should (member "/tmp/test-large-dict" result))))))

(nskk-deftest-unit dict-detect-nix-profiles-env
  "Auto-detection uses NIX_PROFILES environment variable."
  (cl-letf (((symbol-function 'getenv)
             (lambda (var) (when (string= var "NIX_PROFILES")
                             "/nix/var/nix/profiles/default /home/user/.nix-profile")))
            ((symbol-function 'file-readable-p)
             (lambda (f) (string-match-p "/nix/var/nix/profiles/default/share/skk" f))))
    (let ((result (nskk-dict--detect-system-dictionaries)))
      (should result)
      (should (cl-some (lambda (p) (string-match-p "profiles/default" p)) result)))))

(nskk-deftest-unit dict-initialize-uses-detection-when-nil
  "nskk-dict-initialize uses auto-detection when config is nil."
  (let ((nskk-dict-system-dictionary-files nil)
        (nskk-dict-user-dictionary-file nil)
        (nskk--system-dict-index nil)
        (nskk--user-dict-index nil)
        (detect-called nil))
    (cl-letf (((symbol-function 'nskk-dict--detect-system-dictionaries)
               (lambda () (setq detect-called t) nil))
              ((symbol-function 'nskk-dict-load-user-dictionary)
               (lambda () nil)))
      (nskk-dict-initialize)
      (should detect-called))))

(nskk-deftest-unit dict-initialize-skips-detection-when-configured
  "nskk-dict-initialize skips auto-detection when files are configured."
  (let ((nskk-dict-system-dictionary-files '("/some/path"))
        (nskk-dict-user-dictionary-file nil)
        (nskk--system-dict-index nil)
        (nskk--user-dict-index nil)
        (detect-called nil))
    (cl-letf (((symbol-function 'nskk-dict--detect-system-dictionaries)
               (lambda () (setq detect-called t) nil))
              ((symbol-function 'nskk-dict-load-system-dictionaries)
               (lambda () nil))
              ((symbol-function 'nskk-dict-load-user-dictionary)
               (lambda () nil)))
      (nskk-dict-initialize)
      (should-not detect-called))))

(provide 'nskk-dict-test)

;;; nskk-dict-test.el ends here
