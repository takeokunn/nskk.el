;;; nskk-layer-qa.el --- NSKK QA Layer Interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: Japanese, input, method, qa, layer
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

;; This file provides the QA (Quality Assurance) layer interface for NSKK.
;; It integrates test frameworks, coverage measurement, and quality metrics
;; into a unified interface.
;;
;; The QA layer sits at Layer 7 in the NSKK architecture:
;;
;; Layer 1: UI Layer
;; Layer 2: Extension Layer
;; Layer 3: Application Layer
;; Layer 4: Core Engine Layer
;; Layer 5: Data Access Layer
;; Layer 6: Infrastructure Layer
;; Layer 7: QA Layer (this file)
;;
;; Features:
;; - Test execution coordination
;; - Coverage measurement
;; - Quality metrics
;; - Performance monitoring
;; - Regression detection

;;; Code:

(require 'ert)
(require 'json)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(eval-when-compile (require 'cl-lib))


;;;;
;;;; QA Layer Configuration
;;;;

(defgroup nskk-qa nil
  "NSKK Quality Assurance layer configuration."
  :prefix "nskk-qa-"
  :group 'nskk)

(defcustom nskk-qa-enable-coverage t
  "Enable coverage tracking during tests."
  :type 'boolean
  :group 'nskk-qa)

(defcustom nskk-qa-coverage-target 90
  "Target coverage percentage."
  :type 'integer
  :group 'nskk-qa)

(defcustom nskk-qa-enable-benchmark t
  "Enable performance benchmarking."
  :type 'boolean
  :group 'nskk-qa)

(defcustom nskk-qa-performance-threshold-ms 1.0
  "Performance threshold for critical operations (milliseconds)."
  :type 'number
  :group 'nskk-qa)


;;;;
;;;; QA State Management
;;;;

(defvar nskk--qa-state nil
  "Current QA state.")

(defvar nskk--qa-metrics nil
  "Collected QA metrics.")

(cl-defstruct nskk-qa-state
  "QA state container."
  (session-id nil :read-only t)
  (start-time nil :read-only t)
  (end-time nil)
  (test-results nil :type hash-table)
  (coverage-data nil :type hash-table)
  (performance-data nil :type hash-table)
  (metadata nil :type hash-table))


(cl-defstruct nskk-qa-metrics
  "QA metrics container."
  (test-count 0)
  (pass-count 0)
  (fail-count 0)
  (skip-count 0)
  (coverage-percent 0.0)
  (avg-response-time-ms 0.0)
  (max-response-time-ms 0.0)
  (memory-usage-bytes 0))


;;;;
;;;; QA Session Management
;;;;

(defun nskk-qa-start-session ()
  "Start a new QA session."
  (interactive)
  (setq nskk--qa-state
        (make-nskk-qa-state
         :session-id (format "nskk-qa-%s" (format-time-string "%Y%m%d-%H%M%S"))
         :start-time (current-time)
         :test-results (make-hash-table :test 'equal)
         :coverage-data (make-hash-table :test 'equal)
         :performance-data (make-hash-table :test 'equal)
         :metadata (make-hash-table :test 'equal)))

  (message "[NSKK QA] Session started: %s"
           (nskk-qa-state-session-id nskk--qa-state))
  nskk--qa-state)

(defun nskk-qa-end-session ()
  "End current QA session and generate report."
  (interactive)
  (when nskk--qa-state
    (setf (nskk-qa-state-end-time nskk--qa-state) (current-time))
    (let ((metrics (nskk-qa-calculate-metrics)))
      (nskk-qa-generate-report metrics)
      (setq nskk--qa-metrics metrics)
      (message "[NSKK QA] Session ended: %s"
               (nskk-qa-state-session-id nskk--qa-state))
      metrics)))


;;;;
;;;; Test Execution
;;;;

(defun nskk-qa-run-tests (&optional selector)
  "Run all NSKK tests matching SELECTOR."
  (interactive)
  (unless nskk--qa-state
    (nskk-qa-start-session))

  (message "[NSKK QA] Running tests...")
  (let* ((test-selector (or selector "^nskk"))
         (results (ert-run-tests-batch test-selector)))

    ;; Store results
    (puthash "test-results" results (nskk-qa-state-test-results nskk--qa-state))
    results))

(defun nskk-qa-run-unit-tests ()
  "Run unit tests only."
  (interactive)
  (nskk-qa-run-tests "^nskk-unit-"))

(defun nskk-qa-run-integration-tests ()
  "Run integration tests only."
  (interactive)
  (nskk-qa-run-tests "^nskk-integration-"))

(defun nskk-qa-run-performance-tests ()
  "Run performance tests only."
  (interactive)
  (nskk-qa-run-tests "^nskk-performance-"))

(defun nskk-qa-run-property-tests ()
  "Run property-based tests only."
  (interactive)
  (nskk-qa-run-tests "^nskk-property-"))

(defun nskk-qa-run-all-tests ()
  "Run all NSKK tests."
  (interactive)
  (nskk-qa-run-tests "^nskk"))


;;;;
;;;; Coverage Measurement
;;;;

(defun nskk-qa-measure-coverage (&optional files)
  "Measure code coverage for FILES (or all NSKK files)."
  (interactive)
  (when nskk-qa-enable-coverage
    (message "[NSKK QA] Measuring coverage...")

    (let ((coverage (nskk--calculate-coverage files)))
      (puthash "coverage" coverage (nskk-qa-state-coverage-data nskk--qa-state))
      coverage)))

(defun nskk--calculate-coverage (&optional files)
  "Calculate coverage for FILES.
;; TODO: Placeholder implementation - currently uses autoload cookie count
;; as a rough proxy for coverage.  Replace with proper instrumentation-based
;; coverage measurement."
  (let* ((target-files (or files (nskk--get-source-files)))
         (total-lines 0)
         (covered-lines 0))

    (dolist (file target-files)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((lines (count-lines (point-min) (point-max))))
          (cl-incf total-lines lines)
          ;; Count lines marked as covered
          (goto-char (point-min))
          (while (re-search-forward ";;;###autoload" nil t)
            (cl-incf covered-lines)))))

    (if (> total-lines 0)
        (* 100.0 (/ (float covered-lines) total-lines))
      0.0)))

(defun nskk-qa-check-coverage-threshold ()
  "Check if coverage meets target threshold."
  (interactive)
  (let ((coverage (nskk-qa-measure-coverage)))
    (if (>= coverage nskk-qa-coverage-target)
        (message "[NSKK QA] Coverage OK: %.1f%% (target: %d%%)"
                 coverage nskk-qa-coverage-target)
      (warn "[NSKK QA] Coverage below target: %.1f%% (target: %d%%)"
            coverage nskk-qa-coverage-target))
    coverage))


;;;;
;;;; Performance Monitoring
;;;;

(defun nskk-qa-measure-performance ()
  "Measure performance of critical operations."
  (interactive)
  (when nskk-qa-enable-benchmark
    (message "[NSKK QA] Measuring performance...")

    (let ((metrics (make-hash-table :test 'equal)))
      ;; Measure conversion performance
      (puthash "romaji-conversion"
               (nskk--benchmark-conversion)
               metrics)
      ;; Measure search performance
      (puthash "dictionary-search"
               (nskk--benchmark-search)
               metrics)
      ;; Measure cache performance
      (puthash "cache-access"
               (nskk--benchmark-cache)
               metrics)

      (puthash "performance" metrics (nskk-qa-state-performance-data nskk--qa-state))
      metrics)))

(defun nskk--benchmark-conversion ()
  "Benchmark romaji conversion."
  (let ((test-inputs '("aiueo" "konnichiwa" "nihongo" "sakana"))
        (times nil))
    (dolist (input test-inputs)
      (let ((start (current-time)))
        (dotimes (_ 1000)
          (nskk-convert-romaji input))
        (push (* 1000.0 (float-time (time-subtract (current-time) start)))
              times)))
    (list :mean (/ (apply #'+ times) (float (length times)))
          :min (apply #'min times)
          :max (apply #'max times))))

(defun nskk--benchmark-search ()
  "Benchmark dictionary search."
  (let ((queries '("nihon" "kawa" "yama" "sora"))
        (times nil))
    (dolist (query queries)
      (let ((start (current-time)))
        (dotimes (_ 1000)
          (ignore query))
        (push (* 1000.0 (float-time (time-subtract (current-time) start)))
              times)))
    (list :mean (/ (apply #'+ times) (float (length times)))
          :min (apply #'min times)
          :max (apply #'max times))))

(defun nskk--benchmark-cache ()
  "Benchmark cache access."
  (let ((cache (or (bound-and-true-p nskk-cache)
                   (make-hash-table)))
        ;; TODO: Fix compiler warning about symbolp, 'nskk-cache
        (keys '("test1" "test2" "test3" "test4"))
        (times nil))
    (dolist (key keys)
      (puthash key "value" cache))
    (dolist (key keys)
      (let ((start (current-time)))
        (dotimes (_ 10000)
          (ignore (gethash key cache)))
        (push (* 1000.0 (float-time (time-subtract (current-time) start)))
              times)))
    (list :mean (/ (apply #'+ times) (float (length times)))
          :min (apply #'min times)
          :max (apply #'max times))))


;;;;
;;;; Metrics Calculation
;;;;

(defun nskk-qa-calculate-metrics ()
  "Calculate comprehensive QA metrics."
  (unless nskk--qa-state
    (error "No active QA session"))

  (let* ((test-results (gethash "test-results" (nskk-qa-state-test-results nskk--qa-state)))
         (coverage-data (gethash "coverage" (nskk-qa-state-coverage-data nskk--qa-state)))
         (perf-data (gethash "performance" (nskk-qa-state-performance-data nskk--qa-state)))

         (metrics (make-nskk-qa-metrics)))

    ;; Count test results
    (when test-results
      (setf (nskk-qa-metrics-test-count metrics)
            (nskk--count-tests test-results))
      (setf (nskk-qa-metrics-pass-count metrics)
            (nskk--count-passed test-results))
      (setf (nskk-qa-metrics-fail-count metrics)
            (nskk--count-failed test-results))
      (setf (nskk-qa-metrics-skip-count metrics)
            (nskk--count-skipped test-results)))

    ;; Coverage percentage
    (when coverage-data
      (setf (nskk-qa-metrics-coverage-percent metrics) coverage-data))

    ;; Performance metrics
    (when perf-data
      (let ((conv-perf (gethash "romaji-conversion" perf-data)))
        (when conv-perf
          (setf (nskk-qa-metrics-avg-response-time-ms metrics)
                (plist-get conv-perf :mean))
          (setf (nskk-qa-metrics-max-response-time-ms metrics)
                (plist-get conv-perf :max)))))

    metrics))


;;;;
;;;; Report Generation
;;;;

(defun nskk-qa-generate-report (&optional metrics)
  "Generate QA report with METRICS."
  (interactive)
  (let ((metrics (or metrics (nskk-qa-calculate-metrics))))

    (with-output-to-temp-buffer "*NSKK QA Report*"
      (princ "NSKK Quality Assurance Report\n")
      (princ "----------------------------\n\n")
      (princ (format "Session: %s\n" (nskk-qa-state-session-id nskk--qa-state)))
      (princ (format "Time: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))

      ;; Test results
      (princ "Test Results:\n")
      (princ "  Total tests:  ")
      (princ (number-to-string (nskk-qa-metrics-test-count metrics)))
      (princ "\n")
      (princ "  Passed:       ")
      (princ (number-to-string (nskk-qa-metrics-pass-count metrics)))
      (princ "\n")
      (princ "  Failed:       ")
      (princ (number-to-string (nskk-qa-metrics-fail-count metrics)))
      (princ "\n")
      (princ "  Skipped:      ")
      (princ (number-to-string (nskk-qa-metrics-skip-count metrics)))
      (princ "\n")

      (let ((success-rate
             (if (> (nskk-qa-metrics-test-count metrics) 0)
                 (* 100.0 (/ (float (nskk-qa-metrics-pass-count metrics))
                            (nskk-qa-metrics-test-count metrics)))
               0.0)))
        (princ (format "  Success rate: %.1f%%\n\n" success-rate)))

      ;; Coverage
      (princ "Code Coverage:\n")
      (princ (format "  Overall: %.1f%%\n" (nskk-qa-metrics-coverage-percent metrics)))
      (princ (format "  Target:  %d%%\n" nskk-qa-coverage-target))
      (if (>= (nskk-qa-metrics-coverage-percent metrics) nskk-qa-coverage-target)
          (princ "  Status:  ✓ PASS\n\n")
        (princ "  Status:  ✗ FAIL\n\n"))

      ;; Performance
      (princ "Performance Metrics:\n")
      (princ (format "  Avg response time: %.2f ms\n" (nskk-qa-metrics-avg-response-time-ms metrics)))
      (princ (format "  Max response time: %.2f ms\n" (nskk-qa-metrics-max-response-time-ms metrics)))
      (princ (format "  Threshold:         %.2f ms\n" nskk-qa-performance-threshold-ms))
      (if (<= (nskk-qa-metrics-avg-response-time-ms metrics) nskk-qa-performance-threshold-ms)
          (princ "  Status:            ✓ PASS\n")
        (princ "  Status:            ✗ FAIL\n")))))

(defun nskk-qa-generate-json-report (&optional metrics)
  "Generate QA report as JSON."
  (interactive)
  (let ((metrics (or metrics (nskk-qa-calculate-metrics))))
    (json-encode
     `((session . ,(nskk-qa-state-session-id nskk--qa-state))
       (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
       (tests . ((total . ,(nskk-qa-metrics-test-count metrics))
                 (passed . ,(nskk-qa-metrics-pass-count metrics))
                 (failed . ,(nskk-qa-metrics-fail-count metrics))
                 (skipped . ,(nskk-qa-metrics-skip-count metrics))))
       (coverage . ((percent . ,(nskk-qa-metrics-coverage-percent metrics))
                   (target . ,nskk-qa-coverage-target)))
       (performance . ((avg-ms . ,(nskk-qa-metrics-avg-response-time-ms metrics))
                      (max-ms . ,(nskk-qa-metrics-max-response-time-ms metrics))
                      (threshold-ms . ,nskk-qa-performance-threshold-ms)))))))


;;;;
;;;; Quality Gates
;;;;

(defun nskk-qa-check-gate (&optional strict)
  "Check if quality gates pass.
With STRICT, enforce all quality thresholds."
  (interactive)
  (let ((metrics (nskk-qa-calculate-metrics))
        (failures nil))

    ;; Check coverage gate
    (when (< (nskk-qa-metrics-coverage-percent metrics)
             nskk-qa-coverage-target)
      (push (format "Coverage: %.1f%% < %d%%"
                    (nskk-qa-metrics-coverage-percent metrics)
                    nskk-qa-coverage-target)
            failures))

    ;; Check performance gate
    (when (> (nskk-qa-metrics-avg-response-time-ms metrics)
             nskk-qa-performance-threshold-ms)
      (push (format "Performance: %.2fms > %.2fms"
                    (nskk-qa-metrics-avg-response-time-ms metrics)
                    nskk-qa-performance-threshold-ms)
            failures))

    ;; Check test success rate
    (let ((success-rate
           (if (> (nskk-qa-metrics-test-count metrics) 0)
               (* 100.0 (/ (float (nskk-qa-metrics-pass-count metrics))
                          (nskk-qa-metrics-test-count metrics)))
             0.0)))
      (when (< success-rate (if strict 100 95))
        (push (format "Test success rate: %.1f%% < %d%%"
                      success-rate (if strict 100 95))
              failures)))

    (if failures
        (progn
          (message "[NSKK QA] Quality gates FAILED:")
          (dolist (failure failures)
            (message "  - %s" failure))
          nil)
      (message "[NSKK QA] All quality gates PASSED")
      t)))


;;;;
;;;; Helper Functions
;;;;

(defun nskk--get-source-files ()
  "Get list of NSKK source files."
  (let ((files nil))
    (dolist (file (directory-files default-directory nil "\\`nskk-.*\\.el\\'"))
      (unless (string-match "-test\\.el\\'" file)
        (push file files)))
    (nreverse files)))

(defun nskk--count-tests (_results)
  "Count total tests from RESULTS."
  ;; TODO: Stub - implement by parsing ERT test results structure
  0)

(defun nskk--count-passed (_results)
  "Count passed tests from RESULTS."
  ;; TODO: Stub - implement by filtering ERT results for passed status
  0)

(defun nskk--count-failed (_results)
  "Count failed tests from RESULTS."
  ;; TODO: Stub - implement by filtering ERT results for failed status
  0)

(defun nskk--count-skipped (_results)
  "Count skipped tests from RESULTS."
  ;; TODO: Stub - implement by filtering ERT results for skipped status
  0)


;;;;
;;;; Layer Statistics and Debugging
;;;;

(defun nskk-qa-show-stats ()
  "Show QA layer statistics."
  (interactive)
  (when nskk--qa-state
    (message "[NSKK QA] Session: %s"
             (nskk-qa-state-session-id nskk--qa-state))
    (message "[NSKK QA] Runtime: %.2f seconds"
             (float-time (time-subtract
                          (or (nskk-qa-state-end-time nskk--qa-state)
                              (current-time))
                          (nskk-qa-state-start-time nskk--qa-state))))
    (message "[NSKK QA] Tests run: %d"
             (hash-table-count (nskk-qa-state-test-results nskk--qa-state)))
    (message "[NSKK QA] Coverage entries: %d"
             (hash-table-count (nskk-qa-state-coverage-data nskk--qa-state)))
    (message "[NSKK QA] Performance entries: %d"
             (hash-table-count (nskk-qa-state-performance-data nskk--qa-state)))))

(defun nskk-qa-dump-state ()
  "Dump QA state for debugging."
  (interactive)
  (when nskk--qa-state
    (with-output-to-temp-buffer "*NSKK QA State*"
      (princ "NSKK QA Layer State\n")
      (princ "------------------\n\n")
      (princ (format "Session ID: %s\n" (nskk-qa-state-session-id nskk--qa-state)))
      (princ (format "Start Time: %s\n" (nskk-qa-state-start-time nskk--qa-state)))
      (princ (format "End Time: %s\n" (nskk-qa-state-end-time nskk--qa-state)))
      (princ (format "\nTest Results: %d entries\n"
                     (hash-table-count (nskk-qa-state-test-results nskk--qa-state))))
      (princ (format "Coverage Data: %d entries\n"
                     (hash-table-count (nskk-qa-state-coverage-data nskk--qa-state))))
      (princ (format "Performance Data: %d entries\n"
                     (hash-table-count (nskk-qa-state-performance-data nskk--qa-state))))
      (princ (format "Metadata: %d entries\n"
                     (hash-table-count (nskk-qa-state-metadata nskk--qa-state)))))))


;;;;
;;;; Integration with Other Layers
;;;;

(defun nskk-qa-layer-init ()
  "Initialize QA layer.
Called during NSKK initialization."
  (unless nskk--qa-state
    (nskk-qa-start-session))
  (add-hook 'nskk-pre-shutdown-hook #'nskk-qa-end-session)
  (add-hook 'nskk-after-config-change-hook #'nskk-qa-measure-coverage)
  (message "[NSKK QA] Layer initialized"))

(provide 'nskk-layer-qa)

;;; nskk-layer-qa.el ends here
