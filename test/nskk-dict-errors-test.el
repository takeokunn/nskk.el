;;; nskk-dict-errors-test.el --- Tests for nskk-dict-errors.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-dict-errors.el covering:
;; - Module loading and feature provision
;; - Customization group definition
;; - Error types defined in nskk-search.el that depend on this module
;;
;; Note: The error hierarchy has nskk-dict-error as a parent that is not
;; itself defined with define-error, so `should-error` cannot catch these
;; errors (it relies on the `error` condition). Tests use condition-case
;; directly instead.

;;; Code:

(require 'ert)
(require 'nskk-dict-errors)
(require 'nskk-search)
(require 'nskk-test-framework)

;;;
;;; Module Loading Tests
;;;

(nskk-deftest-unit dict-errors-feature-provided
  "Test that nskk-dict-errors feature is provided."
  (should (featurep 'nskk-dict-errors)))

(nskk-deftest-unit dict-errors-require-idempotent
  "Test that requiring nskk-dict-errors multiple times is safe."
  (require 'nskk-dict-errors)
  (require 'nskk-dict-errors)
  (should (featurep 'nskk-dict-errors)))

;;;
;;; Customization Group Tests
;;;

(nskk-deftest-unit dict-errors-customization-group-exists
  "Test that nskk-dict-errors customization group has documentation."
  (should (get 'nskk-dict-errors 'group-documentation)))

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

(provide 'nskk-dict-errors-test)

;;; nskk-dict-errors-test.el ends here
