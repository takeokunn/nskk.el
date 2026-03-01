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
;; - Prolog dictionary facts and I/O.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-dictionary)
(require 'nskk-search)
(require 'nskk-prolog)
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
    (should (null (nskk-dict-index-predicate index)))
    (should (null (nskk-dict-index-by-freq index)))))

(nskk-deftest-unit dict-index-create-with-predicate
  "Test creating a dict-index with a Prolog predicate."
  (nskk-with-prolog-entries ((test-dict "key1" ("val1"))
                             (test-dict "key2" ("val2")))
    (let ((index (make-nskk-dict-index :predicate 'test-dict)))
      (should (nskk-dict-index-p index))
      (should (eq (nskk-dict-index-predicate index) 'test-dict)))))

(nskk-deftest-unit dict-index-create-with-prolog-trie
  "Test creating a dict-index backed by Prolog trie index."
  (nskk-with-prolog-entries ((prefix-dict "test" ("value")))
    (let ((index (make-nskk-dict-index :predicate 'prefix-dict)))
      (should (nskk-dict-index-p index))
      (should (eq (nskk-dict-index-predicate index) 'prefix-dict)))))

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

(nskk-deftest-unit dict-index-accessor-predicate
  "Test nskk-dict-index-predicate accessor."
  (let ((index (make-nskk-dict-index :predicate 'my-dict)))
    (should (eq (nskk-dict-index-predicate index) 'my-dict))))

(nskk-deftest-unit dict-index-accessor-by-freq
  "Test nskk-dict-index-by-freq accessor."
  (let ((freq-data '((10 . "entry-a") (5 . "entry-b")))
        (index (make-nskk-dict-index)))
    (setf (nskk-dict-index-by-freq index) freq-data)
    (should (equal (nskk-dict-index-by-freq index) freq-data))))

;;;
;;; nskk-dict--struct-entry-count Tests
;;;

(nskk-deftest-unit dict-struct-entry-count-empty-prolog
  "Test entry count on empty Prolog predicate."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (let ((index (make-nskk-dict-index :predicate 'empty-dict)))
      (should (= (nskk-dict--struct-entry-count index nil) 0)))))

(nskk-deftest-unit dict-struct-entry-count-with-prolog-facts
  "Test entry count with Prolog facts."
  (nskk-with-prolog-entries ((count-dict "a" ("v1"))
                             (count-dict "b" ("v2"))
                             (count-dict "c" ("v3")))
    (let ((index (make-nskk-dict-index :predicate 'count-dict)))
      (should (= (nskk-dict--struct-entry-count index nil) 3)))))

;;;
;;; Integration Tests
;;;

(nskk-deftest-integration dict-struct-build-index-workflow
  "Test building a dict-index backed by Prolog facts."
  (nskk-with-prolog-entries ((workflow-dict "かんじ" ("漢字" "感じ"))
                             (workflow-dict "にほん" ("日本"))
                             (workflow-dict "にほんご" ("日本語")))
    (let ((index (make-nskk-dict-index :predicate 'workflow-dict)))
      (should (nskk-dict-index-p index))
      (should (= (nskk-dict--struct-entry-count index nil) 3))
      ;; Verify prefix search
      (let ((prefix-results (nskk-prolog-trie-prefix-search 'workflow-dict 2 "にほん")))
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
;;; Prolog Dictionary I/O Tests
;;;

(nskk-deftest-unit dict-io-prolog-assert-and-lookup
  "Test asserting dict entries and looking them up via Prolog."
  (nskk-with-prolog-entries ((io-test-dict "かんじ" ("漢字")))
    (let ((result (nskk-prolog-query-value
                   '(io-test-dict "かんじ" \?c) '\?c)))
      (nskk-should-equal '("漢字") result))))

(nskk-deftest-unit dict-io-prolog-prefix-search
  "Test prefix search over Prolog dict facts."
  (nskk-with-prolog-entries ((prefix-test-dict "かんじ" ("漢字"))
                             (prefix-test-dict "かんたん" ("簡単"))
                             (prefix-test-dict "にほん" ("日本")))
    (let ((results (nskk-prolog-trie-prefix-search 'prefix-test-dict 2 "かん")))
      (should (= (length results) 2))
      (should (assoc "かんじ" results))
      (should (assoc "かんたん" results)))))

(nskk-deftest-unit dict-io-prolog-japanese-values
  "Test Prolog dict with Japanese candidate lists."
  (nskk-with-prolog-entries ((ja-dict "にほん" ("日本" "二本")))
    (let ((result (nskk-prolog-query-value '(ja-dict "にほん" \?c) '\?c)))
      (nskk-should-equal '("日本" "二本") result))))

(nskk-deftest-unit dict-io-prolog-retract-all
  "Test clearing all entries via retract-all."
  (nskk-with-prolog-entries ((retract-dict "a" ("val1"))
                             (retract-dict "b" ("val2")))
    (nskk-prolog-retract-all 'retract-dict 2)
    (let ((result (nskk-prolog-query-value '(retract-dict "a" \?c) '\?c)))
      (should (null result)))))

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

(nskk-deftest-integration dict-io-prolog-roundtrip-workflow
  "Test full workflow: assert Prolog facts, query, prefix-search, verify."
  (nskk-with-prolog-entries ((workflow-io-dict "あいう" ("アイウ"))
                             (workflow-io-dict "あいうえ" ("アイウエ"))
                             (workflow-io-dict "あいうえお" ("アイウエオ"))
                             (workflow-io-dict "かきく" ("カキク")))
    (let ((index (make-nskk-dict-index :predicate 'workflow-io-dict)))
      (should (= (nskk-dict--struct-entry-count index nil) 4))
      ;; Verify prefix search works
      (let ((results (nskk-prolog-trie-prefix-search 'workflow-io-dict 2 "あいう")))
        (should (= (length results) 3))
        (should (assoc "あいう" results))
        (should (assoc "あいうえ" results))
        (should (assoc "あいうえお" results)))
      ;; Verify exact lookup
      (let ((result (nskk-prolog-query-value
                     '(workflow-io-dict "かきく" \?c) '\?c)))
        (nskk-should-equal '("カキク") result)))))

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

;;;
;;; Property-Based Tests
;;;

(require 'nskk-test-macros)

;; Table-driven dict entry creation
(nskk-deftest-cases dict-pbt-entry-creation
  (("かんじ"   . ("漢字" "感じ" "幹事"))
   ("にほん"   . ("日本" "二本"))
   ("さくら"   . ("桜"))
   ("やま"     . ("山")))
  :description "Dict entry creation with known keys and candidates"
  :body (let ((entry (make-nskk-dict-entry :key input :candidates expected)))
          (should (nskk-dict-entry-p entry))
          (should (equal (nskk-dict-entry-key entry) input))
          (should (equal (nskk-dict-entry-candidates entry) expected))))

;; Search invariant: inserting a key then searching returns non-empty results
(nskk-deftest-unit dict-pbt-search-inserted-key-found
  "Search invariant: searching for a key that was inserted returns a non-empty result."
  (nskk-with-mock-dict '(("てすと" . ("テスト")))
    (let ((result (nskk-prolog-query-value
                   '(user-dict-entry "てすと" \?c) '\?c)))
      (should result)
      (should (= (length result) 1))
      (should (equal (car result) "テスト")))))

;; Empty search: searching for non-existent key returns nil (no crash)
(nskk-deftest-unit dict-pbt-search-nonexistent-key-no-crash
  "Empty search: searching for a non-existent key returns nil without crashing."
  (nskk-with-mock-dict nil
    (nskk-should-not-error
      (let ((result (nskk-prolog-query-value
                     '(user-dict-entry "ぜったいにそんざいしないきー" \?c) '\?c)))
        (should (null result))))))

(provide 'nskk-dict-test)

;;; nskk-dict-test.el ends here
