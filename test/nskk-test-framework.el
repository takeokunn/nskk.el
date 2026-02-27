;;; nskk-test-framework.el --- NSKK Test Framework using ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: Japanese, input, method, test, framework
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

;; This file provides a comprehensive test framework for NSKK using ERT
;; (Emacs Lisp Regression Testing). It includes test helpers, fixtures,
;; and utilities to support TDD (Test-Driven Development) and PBT
;; (Property-Based Testing) strategies.
;;
;; Features:
;; - ERT-based test framework
;; - Test environment setup/teardown
;; - Mock objects and fixtures
;; - Property-based testing support
;; - Performance benchmarking
;; - Coverage measurement integration

;;; Code:

(require 'ert)
(require 'nskk-dict-struct)
(eval-when-compile (require 'cl-lib))


;;;;
;;;; Test Framework Configuration
;;;;

(defgroup nskk-test nil
  "NSKK test framework configuration."
  :prefix "nskk-test-"
  :group 'nskk)

(defcustom nskk-test-verbose nil
  "Enable verbose test output."
  :type 'boolean
  :group 'nskk-test)

(defcustom nskk-test-timeout 5.0
  "Default test timeout in seconds."
  :type 'number
  :group 'nskk-test)

(defcustom nskk-test-property-runs 100
  "Default number of runs for property-based tests."
  :type 'integer
  :group 'nskk-test)


;;;;
;;;; Test State Management
;;;;

(defvar nskk--test-mode nil
  "Non-nil when running in test mode.")

(defvar nskk--test-state nil
  "Current test state information.")

(defvar nskk--test-fixtures nil
  "Active test fixtures.")

(defvar nskk--test-mocks nil
  "Active mock objects.")

(cl-defstruct nskk-test-state
  "Test state container."
  (name nil :read-only t)
  (start-time nil)
  (end-time nil)
  (result nil)
  (error nil)
  (coverage nil)
  (metadata nil :type hash-table))


;;;;
;;;; Test Environment Setup
;;;;

(defun nskk--test-setup ()
  "Setup test environment before each test."
  (setq nskk--test-mode t
        nskk--test-state (make-nskk-test-state
                         :name (or (ert-running-test) 'unknown)
                         :start-time (current-time)
                         :metadata (make-hash-table :test 'equal))
        nskk--test-fixtures nil
        nskk--test-mocks nil)

  (when nskk-test-verbose
    (message "[NSKK Test] Setup: %s" (ert-test-name (ert-running-test)))))

(defun nskk--test-teardown ()
  "Cleanup test environment after each test."
  (when nskk--test-state
    (setf (nskk-test-state-end-time nskk--test-state) (current-time)))

  ;; Cleanup fixtures
  (dolist (fixture nskk--test-fixtures)
    (nskk--fixture-cleanup fixture))

  ;; Cleanup mocks
  (dolist (mock nskk--test-mocks)
    (nskk--mock-restore mock))

  (setq nskk--test-fixtures nil
        nskk--test-mocks nil)

  (when nskk-test-verbose
    (message "[NSKK Test] Teardown: %s" (ert-test-name (ert-running-test)))))


;;;;
;;;; Test Definition Macros
;;;;

(defmacro nskk-deftest (name docstring &rest body)
  "Define an NSKK test with NAME, DOCSTRING, and BODY."
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,(intern (format "nskk-test-%s" name)) ()
     ,docstring
     (let ((nskk--test-mode t)
           (nskk--test-state nil))
       (nskk--test-setup)
       (unwind-protect
           (progn
             ,@body)
         (nskk--test-teardown)))))

(defmacro nskk-deftest-unit (name docstring &rest body)
  "Define a unit test."
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,(intern (format "nskk-unit-%s" name)) ()
     ,docstring
     (let ((nskk--test-mode t))
       (nskk--test-setup)
       (unwind-protect
           (progn
             ,@body)
         (nskk--test-teardown)))))

(defmacro nskk-deftest-integration (name docstring &rest body)
  "Define an integration test."
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,(intern (format "nskk-integration-%s" name)) ()
     ,docstring
     (let ((nskk--test-mode t))
       (nskk--test-setup)
       (unwind-protect
           (progn
             ,@body)
         (nskk--test-teardown)))))

(defmacro nskk-deftest-performance (name docstring &rest body)
  "Define a performance test."
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,(intern (format "nskk-performance-%s" name)) ()
     ,docstring
     (let ((nskk--test-mode t)
           (nskk--test-start-time (current-time)))
       (nskk--test-setup)
       (unwind-protect
           (progn
             ,@body
             (let ((elapsed (float-time
                             (time-subtract (current-time)
                                           nskk--test-start-time))))
               (message "[NSKK Performance] %s: %.3fms" ',name (* 1000 elapsed))))
         (nskk--test-teardown)))))


;;;;
;;;; Test Assertions
;;;;

(defun nskk-assert-approx-equal (a b &optional epsilon)
  "Assert that A and B are approximately equal within EPSILON."
  (let ((eps (or epsilon 0.001)))
    (unless (< (abs (- a b)) eps)
      (ert-fail (format "Not approximately equal: %S vs %S (epsilon: %s)"
                        a b eps)))))

(defun nskk-assert-strings-equal (a b)
  "Assert that strings A and B are equal, with detailed error message."
  (unless (equal a b)
    (ert-fail (format "Strings differ:\nExpected: %S\nActual:   %S" a b))))

(defun nskk-assert-length (collection expected-length)
  "Assert that COLLECTION has EXPECTED-LENGTH."
  (unless (= (length collection) expected-length)
    (ert-fail (format "Length mismatch: expected %d, got %d"
                      expected-length (length collection)))))

(defun nskk-assert-member (item list)
  "Assert that ITEM is a member of LIST."
  (unless (member item list)
    (ert-fail (format "%S not found in %S" item list))))

(defun nskk-assert-type (value expected-type)
  "Assert that VALUE is of EXPECTED-TYPE."
  (unless (funcall expected-type value)
    (ert-fail (format "Type assertion failed: %S is not %s"
                      value expected-type))))


;;;;
;;;; Test Fixtures
;;;;

(cl-defstruct nskk-fixture
  "Test fixture structure."
  name
  data
  cleanup-fn)

(defun nskk--fixture-create (name data cleanup-fn)
  "Create a test fixture."
  (let ((fixture (make-nskk-fixture
                  :name name
                  :data data
                  :cleanup-fn cleanup-fn)))
    (push fixture nskk--test-fixtures)
    fixture))

(defun nskk--fixture-cleanup (fixture)
  "Cleanup a test fixture."
  (when (and fixture (nskk-fixture-cleanup-fn fixture))
    (funcall (nskk-fixture-cleanup-fn fixture))))

(defmacro nskk-with-fixture (name &rest body)
  "Execute BODY with fixture NAME."
  (declare (indent 1))
  `(let ((fixture (nskk--get-fixture ',name)))
     (unwind-protect
         (progn ,@body)
       (nskk--fixture-cleanup fixture))))


;;;;
;;;; Mock Objects
;;;;

(cl-defstruct nskk-mock
  "Mock object structure."
  symbol
  original-value
  mocked-value)

(defun nskk-mock-function (symbol mock-function)
  "Mock FUNCTION with MOCK-FUNCTION for testing."
  (let ((mock (make-nskk-mock
               :symbol symbol
               :original-value (symbol-function symbol)
               :mocked-value mock-function)))
    (push mock nskk--test-mocks)
    (fset symbol mock-function)
    mock))

(defun nskk-mock-variable (symbol mock-value)
  "Mock VARIABLE with MOCK-VALUE for testing."
  (let ((mock (make-nskk-mock
               :symbol symbol
               :original-value (symbol-value symbol)
               :mocked-value mock-value)))
    (push mock nskk--test-mocks)
    (set symbol mock-value)
    mock))

(defun nskk--mock-restore (mock)
  "Restore original value from MOCK."
  (when (nskk-mock-symbol mock)
    (if (functionp (nskk-mock-original-value mock))
        (fset (nskk-mock-symbol mock) (nskk-mock-original-value mock))
      (set (nskk-mock-symbol mock) (nskk-mock-original-value mock)))))


;;;;
;;;; Test Data Generators
;;;;

(defvar nskk--test-generators nil
  "Registry of test data generators.")

(defun nskk-register-generator (name generator)
  "Register a test data generator."
  (setf (alist-get name nskk--test-generators) generator))

(defun nskk-generate (name &rest args)
  "Generate test data using generator NAME."
  (let ((generator (alist-get name nskk--test-generators)))
    (when generator
      (apply generator args))))

;; Predefined generators
(nskk-register-generator 'romaji-string
  (lambda (&optional length)
    (let ((chars '("a" "i" "u" "e" "o"
                   "ka" "ki" "ku" "ke" "ko"
                   "sa" "shi" "su" "se" "so"
                   "ta" "chi" "tsu" "te" "to"
                   "na" "ni" "nu" "ne" "no"))
          (len (or length (+ 1 (random 10)))))
      (mapconcat 'identity
                 (cl-loop repeat len
                          collect (nth (random (length chars)) chars))
                 ""))))

(nskk-register-generator 'hiragana-string
  (lambda (&optional length)
    (let ((chars '("あ" "い" "う" "え" "お"
                   "か" "き" "く" "け" "こ"
                   "さ" "し" "す" "せ" "そ"
                   "た" "ち" "つ" "て" "と"
                   "な" "に" "ぬ" "ね" "の"))
          (len (or length (+ 1 (random 10)))))
      (mapconcat #'identity
                 (cl-loop repeat len
                          collect (nth (random (length chars)) chars))
                 ""))))

(nskk-register-generator 'kanji-string
  (lambda (&optional length)
    (let ((chars '("漢" "字" "変" "換" "日" "本" "語"
                   "入" "力" "シ" "ス" "テ" "ム"))
          (len (or length (+ 1 (random 5)))))
      (mapconcat #'identity
                 (cl-loop repeat len
                          collect (nth (random (length chars)) chars))
                 ""))))


;;;;
;;;; Test Suite Management
;;;;

(defun nskk-test-run-all (&optional selector)
  "Run all NSKK tests matching SELECTOR."
  (interactive)
  (let ((test-selector (or selector "^nskk")))
    (ert-run-tests-batch test-selector)))

(defun nskk-test-run-unit ()
  "Run unit tests only."
  (interactive)
  (nskk-test-run-all "^nskk-unit-"))

(defun nskk-test-run-integration ()
  "Run integration tests only."
  (interactive)
  (nskk-test-run-all "^nskk-integration-"))

(defun nskk-test-run-performance ()
  "Run performance tests only."
  (interactive)
  (nskk-test-run-all "^nskk-performance-"))

(defun nskk-test-list ()
  "List all NSKK tests."
  (interactive)
  (let ((tests (ert-select-tests "^nskk" nil)))
    (with-output-to-temp-buffer "*NSKK Tests*"
      (princ "NSKK Test Suite:\n\n")
      (dolist (test tests)
        (princ (format "  - %s\n" (ert-test-name test)))))))


;;;;
;;;; Test Result Reporting
;;;;

(defun nskk-test-report ()
  "Generate test report."
  (interactive)
  (let ((stats (nskk--test-gather-stats)))
    (with-output-to-temp-buffer "*NSKK Test Report*"
      (princ "NSKK Test Report\n")
      (princ "----------------\n\n")
      (princ (format "Total tests:  %d\n" (plist-get stats :total)))
      (princ (format "Passed:       %d\n" (plist-get stats :passed)))
      (princ (format "Failed:       %d\n" (plist-get stats :failed)))
      (princ (format "Skipped:      %d\n" (plist-get stats :skipped)))
      (princ (format "Success rate: %.1f%%\n"
                     (* 100 (/ (float (plist-get stats :passed))
                              (plist-get stats :total))))))))

(defun nskk--test-gather-stats ()
  "Gather test statistics."
  (let ((tests (ert-select-tests "^nskk" nil))
        (passed 0)
        (failed 0)
        (skipped 0))
    (dolist (test tests)
      (let ((result (ert-test-result test)))
        (pcase (car result)
          ('passed (cl-incf passed))
          ('failed (cl-incf failed))
          ('_ (cl-incf skipped)))))
    (list :total (length tests)
          :passed passed
          :failed failed
          :skipped skipped)))

;;;;
;;;; Mock Dictionary Helpers
;;;;

(defun nskk-test-create-mock-dict (&optional entries)
  "Create a mock dictionary index with ENTRIES.
ENTRIES is an alist of (key . candidates-list).
If nil, uses a default set of common Japanese words."
  (let ((default-entries
         '(("かんじ" . ("漢字" "感じ" "幹事"))
           ("にほんご" . ("日本語"))
           ("にほん" . ("日本" "二本"))
           ("ひらがな" . ("平仮名"))
           ("かたかな" . ("片仮名"))
           ("へんかん" . ("変換"))
           ("にゅうりょく" . ("入力"))
           ("もじ" . ("文字"))
           ("さくら" . ("桜"))
           ("やま" . ("山"))
           ("かわ" . ("川" "河"))
           ("はな" . ("花" "鼻"))
           ("あめ" . ("雨" "飴"))))
        (ht (make-hash-table :test 'equal :size 50)))
    (dolist (entry (or entries default-entries))
      (puthash (car entry) (cdr entry) ht))
    (make-nskk-dict-index
     :entries ht
     :by-prefix nil
     :by-freq nil)))

(defmacro nskk-with-mock-dict (entries &rest body)
  "Execute BODY with a mock dictionary installed.
ENTRIES is an alist of (key . candidates-list) or nil for defaults.
Restores original dictionary state after BODY completes."
  (declare (indent 1))
  `(let ((nskk--system-dict-index (nskk-test-create-mock-dict ,entries))
         (nskk--user-dict-index nil))
     ,@body))

(provide 'nskk-test-framework)

;;; nskk-test-framework.el ends here
