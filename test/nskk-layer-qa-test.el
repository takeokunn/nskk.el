;;; nskk-layer-qa-test.el --- Tests for nskk-layer-qa.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-layer-qa.el covering:
;; - QA state management
;; - QA metrics struct
;; - Session management
;; - Coverage measurement helpers
;; - Performance monitoring helpers
;; - Report generation
;; - Quality gates
;; - Source file listing
;; - Count helpers

;;; Code:

(require 'ert)
(require 'nskk-layer-qa)
(require 'nskk-test-framework)

;;;
;;; QA State Struct Tests
;;;

(nskk-deftest-unit layer-qa-state-struct-exists
  "Test that nskk-qa-state struct is defined."
  (should (fboundp 'nskk-qa-state-p)))

(nskk-deftest-unit layer-qa-state-create
  "Test creating a QA state."
  (let ((state (make-nskk-qa-state
                :session-id "test-session"
                :start-time (current-time)
                :test-results (make-hash-table :test 'equal)
                :coverage-data (make-hash-table :test 'equal)
                :performance-data (make-hash-table :test 'equal)
                :metadata (make-hash-table :test 'equal))))
    (should (nskk-qa-state-p state))
    (should (string= (nskk-qa-state-session-id state) "test-session"))
    (should (nskk-qa-state-start-time state))
    (should (null (nskk-qa-state-end-time state)))
    (should (hash-table-p (nskk-qa-state-test-results state)))
    (should (hash-table-p (nskk-qa-state-coverage-data state)))
    (should (hash-table-p (nskk-qa-state-performance-data state)))
    (should (hash-table-p (nskk-qa-state-metadata state)))))

;;;
;;; QA Metrics Struct Tests
;;;

(nskk-deftest-unit layer-qa-metrics-struct-exists
  "Test that nskk-qa-metrics struct is defined."
  (should (fboundp 'nskk-qa-metrics-p)))

(nskk-deftest-unit layer-qa-metrics-create-defaults
  "Test creating QA metrics with defaults."
  (let ((metrics (make-nskk-qa-metrics)))
    (should (nskk-qa-metrics-p metrics))
    (should (= (nskk-qa-metrics-test-count metrics) 0))
    (should (= (nskk-qa-metrics-pass-count metrics) 0))
    (should (= (nskk-qa-metrics-fail-count metrics) 0))
    (should (= (nskk-qa-metrics-skip-count metrics) 0))
    (should (= (nskk-qa-metrics-coverage-percent metrics) 0.0))
    (should (= (nskk-qa-metrics-avg-response-time-ms metrics) 0.0))
    (should (= (nskk-qa-metrics-max-response-time-ms metrics) 0.0))
    (should (= (nskk-qa-metrics-memory-usage-bytes metrics) 0))))

(nskk-deftest-unit layer-qa-metrics-set-values
  "Test setting QA metrics values."
  (let ((metrics (make-nskk-qa-metrics)))
    (setf (nskk-qa-metrics-test-count metrics) 100)
    (setf (nskk-qa-metrics-pass-count metrics) 95)
    (setf (nskk-qa-metrics-fail-count metrics) 3)
    (setf (nskk-qa-metrics-skip-count metrics) 2)
    (setf (nskk-qa-metrics-coverage-percent metrics) 85.5)
    (should (= (nskk-qa-metrics-test-count metrics) 100))
    (should (= (nskk-qa-metrics-pass-count metrics) 95))
    (should (= (nskk-qa-metrics-fail-count metrics) 3))
    (should (= (nskk-qa-metrics-skip-count metrics) 2))
    (should (= (nskk-qa-metrics-coverage-percent metrics) 85.5))))

;;;
;;; Session Management Tests
;;;

(nskk-deftest-unit layer-qa-start-session
  "Test starting a QA session."
  (let ((nskk--qa-state nil))
    (let ((state (nskk-qa-start-session)))
      (should (nskk-qa-state-p state))
      (should (string-prefix-p "nskk-qa-" (nskk-qa-state-session-id state)))
      (should (nskk-qa-state-start-time state))
      (should (null (nskk-qa-state-end-time state))))))

(nskk-deftest-unit layer-qa-end-session
  "Test ending a QA session."
  (let ((nskk--qa-state nil)
        (nskk--qa-metrics nil))
    (nskk-qa-start-session)
    (let ((metrics (nskk-qa-end-session)))
      (should (nskk-qa-metrics-p metrics))
      (should (nskk-qa-state-end-time nskk--qa-state)))))

;;;
;;; Source File Listing Tests
;;;

(nskk-deftest-unit layer-qa-get-source-files
  "Test getting source files."
  (let ((files (nskk--get-source-files)))
    (should (listp files))
    ;; Should find at least some source files
    (should (> (length files) 0))
    ;; Should not include test files
    (dolist (file files)
      (should (not (string-match-p "-test\\.el\\'" file))))))

;;;
;;; Count Helper Tests
;;;

(nskk-deftest-unit layer-qa-count-tests-returns-number
  "Test that count-tests returns a number."
  (should (numberp (nskk--count-tests nil))))

(nskk-deftest-unit layer-qa-count-passed-returns-number
  "Test that count-passed returns a number."
  (should (numberp (nskk--count-passed nil))))

(nskk-deftest-unit layer-qa-count-failed-returns-number
  "Test that count-failed returns a number."
  (should (numberp (nskk--count-failed nil))))

(nskk-deftest-unit layer-qa-count-skipped-returns-number
  "Test that count-skipped returns a number."
  (should (numberp (nskk--count-skipped nil))))

;;;
;;; Benchmark Helper Tests
;;;

(nskk-deftest-unit layer-qa-benchmark-search-returns-plist
  "Test that benchmark-search returns a plist."
  (let ((result (nskk--benchmark-search)))
    (should (listp result))
    (should (numberp (plist-get result :mean)))
    (should (numberp (plist-get result :min)))
    (should (numberp (plist-get result :max)))))

(nskk-deftest-unit layer-qa-benchmark-cache-returns-plist
  "Test that benchmark-cache returns a plist."
  (let ((result (nskk--benchmark-cache)))
    (should (listp result))
    (should (numberp (plist-get result :mean)))
    (should (numberp (plist-get result :min)))
    (should (numberp (plist-get result :max)))))

;;;
;;; Configuration Tests
;;;

(nskk-deftest-unit layer-qa-custom-group-exists
  "Test that the nskk-qa custom group exists."
  (should (get 'nskk-qa 'custom-group)))

(nskk-deftest-unit layer-qa-coverage-enabled-default
  "Test that coverage is enabled by default."
  (should (eq (default-value 'nskk-qa-enable-coverage) t)))

(nskk-deftest-unit layer-qa-benchmark-enabled-default
  "Test that benchmark is enabled by default."
  (should (eq (default-value 'nskk-qa-enable-benchmark) t)))

(nskk-deftest-unit layer-qa-coverage-target-default
  "Test that coverage target has a sensible default."
  (should (integerp (default-value 'nskk-qa-coverage-target)))
  (should (> (default-value 'nskk-qa-coverage-target) 0))
  (should (<= (default-value 'nskk-qa-coverage-target) 100)))

(nskk-deftest-unit layer-qa-performance-threshold-default
  "Test that performance threshold has a sensible default."
  (should (numberp (default-value 'nskk-qa-performance-threshold-ms)))
  (should (> (default-value 'nskk-qa-performance-threshold-ms) 0)))

;;;
;;; Function Existence Tests
;;;

(nskk-deftest-unit layer-qa-public-functions-exist
  "Test that all public QA functions exist."
  (should (fboundp 'nskk-qa-start-session))
  (should (fboundp 'nskk-qa-end-session))
  (should (fboundp 'nskk-qa-run-tests))
  (should (fboundp 'nskk-qa-run-unit-tests))
  (should (fboundp 'nskk-qa-run-integration-tests))
  (should (fboundp 'nskk-qa-run-performance-tests))
  (should (fboundp 'nskk-qa-run-property-tests))
  (should (fboundp 'nskk-qa-run-all-tests))
  (should (fboundp 'nskk-qa-measure-coverage))
  (should (fboundp 'nskk-qa-measure-performance))
  (should (fboundp 'nskk-qa-calculate-metrics))
  (should (fboundp 'nskk-qa-generate-report))
  (should (fboundp 'nskk-qa-generate-json-report))
  (should (fboundp 'nskk-qa-check-gate))
  (should (fboundp 'nskk-qa-check-coverage-threshold))
  (should (fboundp 'nskk-qa-show-stats))
  (should (fboundp 'nskk-qa-dump-state))
  (should (fboundp 'nskk-qa-layer-init)))

;;;
;;; Module Feature Tests
;;;

(nskk-deftest-unit layer-qa-provides-feature
  "Test that nskk-layer-qa provides its feature."
  (should (featurep 'nskk-layer-qa)))

(provide 'nskk-layer-qa-test)

;;; nskk-layer-qa-test.el ends here
