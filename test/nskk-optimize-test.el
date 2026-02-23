;;; nskk-optimize-test.el --- Tests for nskk-optimize.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-optimize.el covering:
;; - Timing measurement macros
;; - Profiling data recording and display
;; - Benchmark functions
;; - Memory usage measurement
;; - Optimization macros
;; - Performance statistics

;;; Code:

(require 'ert)
(require 'nskk-optimize)
(require 'nskk-test-framework)

;;;
;;; Timing Measurement Tests
;;;

(nskk-deftest-unit optimize-measure-time-returns-cons
  "Test that nskk-measure-time returns a cons cell."
  (let ((result (nskk-measure-time (+ 1 2))))
    (should (consp result))
    (should (numberp (car result)))
    (should (= (cdr result) 3))))

(nskk-deftest-unit optimize-measure-time-measures-elapsed
  "Test that nskk-measure-time measures elapsed time."
  (let ((result (nskk-measure-time (sleep-for 0 10))))
    (should (consp result))
    ;; Time should be positive (in microseconds)
    (should (>= (car result) 0))))

(nskk-deftest-unit optimize-measure-time-body-result
  "Test that nskk-measure-time preserves body result."
  (let ((result (nskk-measure-time (concat "hello" " " "world"))))
    (should (string= (cdr result) "hello world"))))

;;;
;;; Profiling Data Tests
;;;

(nskk-deftest-unit optimize-profile-data-initially-nil
  "Test that profile data starts empty."
  (let ((nskk-optimize--profile-data nil))
    (should (null nskk-optimize--profile-data))))

(nskk-deftest-unit optimize-record-profile-creates-entry
  "Test that recording profile creates an entry."
  (let ((nskk-optimize--profile-data nil))
    (nskk-optimize--record-profile "test-func" (time-subtract (current-time) (current-time)))
    (should (assoc "test-func" nskk-optimize--profile-data))))

(nskk-deftest-unit optimize-record-profile-increments-count
  "Test that recording profile increments count."
  (let ((nskk-optimize--profile-data nil))
    (nskk-optimize--record-profile "test-func" (time-subtract (current-time) (current-time)))
    (nskk-optimize--record-profile "test-func" (time-subtract (current-time) (current-time)))
    (let ((entry (cdr (assoc "test-func" nskk-optimize--profile-data))))
      (should (= (nth 0 entry) 2)))))

(nskk-deftest-unit optimize-reset-profile-clears-data
  "Test that reset clears profiling data."
  (let ((nskk-optimize--profile-data '(("a" 1 1.0 0.5 1.5))))
    (nskk-optimize-reset-profile)
    (should (null nskk-optimize--profile-data))))

(nskk-deftest-unit optimize-show-profile-no-data
  "Test show-profile with no data does not error."
  (let ((nskk-optimize--profile-data nil))
    ;; Should just show a message, not error
    (nskk-optimize-show-profile)))

(nskk-deftest-unit optimize-with-profiling-disabled
  "Test with-profiling when profiling is disabled."
  (let ((nskk-optimize-enable-profiling nil)
        (nskk-optimize--profile-data nil))
    (let ((result (nskk-with-profiling "test" (+ 1 2))))
      (should (= result 3))
      ;; Should NOT record profile data when disabled
      (should (null nskk-optimize--profile-data)))))

(nskk-deftest-unit optimize-with-profiling-enabled
  "Test with-profiling when profiling is enabled."
  (let ((nskk-optimize-enable-profiling t)
        (nskk-optimize--profile-data nil))
    (let ((result (nskk-with-profiling "test" (+ 1 2))))
      (should (= result 3))
      ;; Should record profile data when enabled
      (should (assoc "test" nskk-optimize--profile-data)))))

;;;
;;; Memory Usage Tests
;;;

(nskk-deftest-unit optimize-memory-usage-returns-plist
  "Test that benchmark-memory-usage returns a plist."
  (let ((usage (nskk-benchmark-memory-usage)))
    (should (listp usage))
    (should (plist-get usage :gc-cons-threshold))
    (should (numberp (plist-get usage :gcs-done)))))

(nskk-deftest-unit optimize-measure-memory-delta-returns-plist
  "Test that measure-memory-delta returns a structured plist."
  (let ((result (nskk-measure-memory-delta (lambda () (make-list 100 nil)))))
    (should (listp result))
    (should (plist-get result :before))
    (should (plist-get result :after))
    (should (plist-get result :delta))))

;;;
;;; Optimization Macros Tests
;;;

(nskk-deftest-unit optimize-loop-basic
  "Test nskk-optimize-loop iterates correctly."
  (let ((sum 0))
    (nskk-optimize-loop i 10
      (setq sum (+ sum i)))
    (should (= sum 45))))

(nskk-deftest-unit optimize-loop-zero-iterations
  "Test nskk-optimize-loop with zero iterations."
  (let ((sum 0))
    (nskk-optimize-loop i 0
      (setq sum (+ sum i)))
    (should (= sum 0))))

(nskk-deftest-unit optimize-string-concat-works
  "Test nskk-optimize-string-concat produces correct result."
  (should (string= (nskk-optimize-string-concat "hello" " world") "hello world"))
  (should (string= (nskk-optimize-string-concat "" "test") "test"))
  (should (string= (nskk-optimize-string-concat "a" "") "a")))

;;;
;;; Statistics Tests
;;;

(nskk-deftest-unit optimize-stats-returns-plist
  "Test that nskk-optimize-stats returns a proper plist."
  (let ((stats (nskk-optimize-stats)))
    (should (listp stats))
    (should (plist-member stats :profiling-enabled))
    (should (plist-member stats :benchmark-iterations))
    (should (plist-member stats :profile-entries))
    (should (plist-member stats :baseline-set))))

(nskk-deftest-unit optimize-stats-reflects-settings
  "Test that stats reflect current settings."
  (let ((nskk-optimize-enable-profiling t)
        (nskk-optimize-benchmark-iterations 5000)
        (nskk-optimize--profile-data '(("a" 1 1.0 0.5 1.5)))
        (nskk-optimize--performance-baseline nil))
    (let ((stats (nskk-optimize-stats)))
      (should (eq (plist-get stats :profiling-enabled) t))
      (should (= (plist-get stats :benchmark-iterations) 5000))
      (should (= (plist-get stats :profile-entries) 1))
      (should (eq (plist-get stats :baseline-set) nil)))))

;;;
;;; Customization Tests
;;;

(nskk-deftest-unit optimize-custom-group-exists
  "Test that the nskk-optimize custom group exists."
  (should (get 'nskk-optimize 'custom-group)))

(nskk-deftest-unit optimize-default-iterations
  "Test default benchmark iterations value."
  (should (integerp nskk-optimize-benchmark-iterations))
  (should (> nskk-optimize-benchmark-iterations 0)))

(nskk-deftest-unit optimize-default-profiling-disabled
  "Test that profiling is disabled by default."
  ;; The default value should be nil
  (should (eq (default-value 'nskk-optimize-enable-profiling) nil)))

;;;
;;; Performance Baseline Tests
;;;

(nskk-deftest-unit optimize-baseline-initially-nil
  "Test that performance baseline starts as nil."
  (let ((nskk-optimize--performance-baseline nil))
    (should (null nskk-optimize--performance-baseline))))

(nskk-deftest-unit optimize-check-regression-without-baseline-errors
  "Test that check-regression errors without baseline."
  (let ((nskk-optimize--performance-baseline nil))
    (should-error (nskk-optimize-check-regression))))

(provide 'nskk-optimize-test)

;;; nskk-optimize-test.el ends here
