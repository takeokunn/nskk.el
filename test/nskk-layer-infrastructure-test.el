;;; nskk-layer-infrastructure-test.el --- Tests for nskk-layer-infrastructure.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-layer-infrastructure.el covering:
;; - Feature loading and module availability
;; - Memory management (allocate, free, track usage)
;; - File I/O (read, write)
;; - Timer management (schedule, cancel)
;; - Task submission (synchronous fallback)
;; - Statistics tracking
;; - Debug mode toggle

;;; Code:

(require 'ert)
(require 'nskk-layer-infrastructure)
(require 'nskk-test-framework)

;;;
;;; Helper Macros
;;;

(defmacro nskk-infra-test-with-clean-state (&rest body)
  "Execute BODY with clean infrastructure state."
  (declare (indent 0))
  `(let ((nskk-infrastructure--thread-pool nil)
         (nskk-infrastructure--memory-usage 0)
         (nskk-infrastructure--timers nil)
         (nskk-infrastructure--resource-monitor-timer nil)
         (nskk-infrastructure--statistics (make-hash-table :test 'eq))
         (nskk-infrastructure--debug-enabled nil)
         (nskk-infrastructure-enable-monitoring nil))
     (nskk-infrastructure--initialize-statistics)
     ,@body))

;;;
;;; Feature Loading Tests
;;;

(nskk-deftest-unit layer-infra-provides-feature
  "Test that nskk-layer-infrastructure provides its feature."
  (should (featurep 'nskk-layer-infrastructure)))

(nskk-deftest-unit layer-infra-require-idempotent
  "Test that requiring nskk-layer-infrastructure again is safe."
  (should (require 'nskk-layer-infrastructure)))

;;;
;;; Function Existence Tests
;;;

(nskk-deftest-unit layer-infra-public-functions-exist
  "Test that main public functions are defined."
  (should (fboundp 'nskk-infrastructure-initialize))
  (should (fboundp 'nskk-infrastructure-shutdown))
  (should (fboundp 'nskk-infrastructure-submit-task))
  (should (fboundp 'nskk-infrastructure-allocate-memory))
  (should (fboundp 'nskk-infrastructure-free-memory))
  (should (fboundp 'nskk-infrastructure-get-memory-usage))
  (should (fboundp 'nskk-infrastructure-read-file))
  (should (fboundp 'nskk-infrastructure-write-file))
  (should (fboundp 'nskk-infrastructure-schedule-timer))
  (should (fboundp 'nskk-infrastructure-cancel-timer))
  (should (fboundp 'nskk-infrastructure-get-statistics))
  (should (fboundp 'nskk-infrastructure-health-check)))

;;;
;;; Memory Management Tests
;;;

(nskk-deftest-unit layer-infra-memory-initial-zero
  "Test that memory usage starts at zero."
  (nskk-infra-test-with-clean-state
    (should (= (nskk-infrastructure-get-memory-usage) 0))))

(nskk-deftest-unit layer-infra-allocate-memory-increases-usage
  "Test that allocating memory increases tracked usage."
  (nskk-infra-test-with-clean-state
    (nskk-infrastructure-allocate-memory 1024)
    (should (= (nskk-infrastructure-get-memory-usage) 1024))))

(nskk-deftest-unit layer-infra-allocate-memory-cumulative
  "Test that multiple allocations are cumulative."
  (nskk-infra-test-with-clean-state
    (nskk-infrastructure-allocate-memory 100)
    (nskk-infrastructure-allocate-memory 200)
    (nskk-infrastructure-allocate-memory 300)
    (should (= (nskk-infrastructure-get-memory-usage) 600))))

(nskk-deftest-unit layer-infra-free-memory-decreases-usage
  "Test that freeing memory decreases tracked usage."
  (nskk-infra-test-with-clean-state
    (nskk-infrastructure-allocate-memory 1000)
    (nskk-infrastructure-free-memory 400)
    (should (= (nskk-infrastructure-get-memory-usage) 600))))

(nskk-deftest-unit layer-infra-free-memory-does-not-go-negative
  "Test that freeing more than allocated does not go below zero."
  (nskk-infra-test-with-clean-state
    (nskk-infrastructure-allocate-memory 100)
    (nskk-infrastructure-free-memory 500)
    (should (= (nskk-infrastructure-get-memory-usage) 0))))

;;;
;;; File I/O Tests
;;;

(nskk-deftest-unit layer-infra-write-and-read-file
  "Test that write-file and read-file roundtrip correctly."
  (nskk-infra-test-with-clean-state
    (let ((temp-file (make-temp-file "nskk-infra-test")))
      (unwind-protect
          (progn
            (nskk-infrastructure-write-file temp-file "hello world")
            (let ((content (nskk-infrastructure-read-file temp-file)))
              (should (equal content "hello world"))))
        (delete-file temp-file)))))

(nskk-deftest-unit layer-infra-read-file-nonexistent-errors
  "Test that reading a nonexistent file signals an error."
  (nskk-infra-test-with-clean-state
    (should-error
     (nskk-infrastructure-read-file "/nonexistent/path/file.txt")
     :type 'file-error)))

(nskk-deftest-unit layer-infra-write-file-creates-directory
  "Test that write-file creates parent directory if needed."
  (nskk-infra-test-with-clean-state
    (let* ((temp-dir (make-temp-file "nskk-infra-dir" t))
           (sub-dir (expand-file-name "sub/nested" temp-dir))
           (temp-file (expand-file-name "test.txt" sub-dir)))
      (unwind-protect
          (progn
            (nskk-infrastructure-write-file temp-file "content")
            (should (file-exists-p temp-file))
            (should (equal (nskk-infrastructure-read-file temp-file) "content")))
        (delete-directory temp-dir t)))))

;;;
;;; Task Submission Tests (Synchronous Fallback)
;;;

(nskk-deftest-unit layer-infra-submit-task-runs-inline
  "Test that without a thread pool, task runs inline and returns result."
  (nskk-infra-test-with-clean-state
    (let ((result nil))
      (nskk-infrastructure-submit-task
       (lambda () 42)
       (lambda (r) (setq result r)))
      (should (= result 42)))))

(nskk-deftest-unit layer-infra-submit-task-error-handler
  "Test that task error handler is called on failure."
  (nskk-infra-test-with-clean-state
    (let ((error-caught nil))
      (nskk-infrastructure-submit-task
       (lambda () (error "test error"))
       nil
       (lambda (_err) (setq error-caught t)))
      (should error-caught))))

(nskk-deftest-unit layer-infra-submit-task-requires-function
  "Test that submit-task rejects non-function argument."
  (nskk-infra-test-with-clean-state
    (should-error
     (nskk-infrastructure-submit-task "not-a-function")
     :type 'user-error)))

;;;
;;; Statistics Tests
;;;

(nskk-deftest-unit layer-infra-statistics-initial-values
  "Test that statistics start with correct initial values."
  (nskk-infra-test-with-clean-state
    (should (= (gethash :tasks-submitted nskk-infrastructure--statistics) 0))
    (should (= (gethash :tasks-completed nskk-infrastructure--statistics) 0))
    (should (= (gethash :files-read nskk-infrastructure--statistics) 0))
    (should (= (gethash :files-written nskk-infrastructure--statistics) 0))))

(nskk-deftest-unit layer-infra-statistics-track-tasks
  "Test that statistics track task submissions and completions."
  (nskk-infra-test-with-clean-state
    (nskk-infrastructure-submit-task (lambda () 1))
    (nskk-infrastructure-submit-task (lambda () 2))
    (should (= (gethash :tasks-submitted nskk-infrastructure--statistics) 2))
    (should (= (gethash :tasks-completed nskk-infrastructure--statistics) 2))))

(nskk-deftest-unit layer-infra-statistics-track-file-io
  "Test that statistics track file read/write operations."
  (nskk-infra-test-with-clean-state
    (let ((temp-file (make-temp-file "nskk-infra-stats")))
      (unwind-protect
          (progn
            (nskk-infrastructure-write-file temp-file "data")
            (nskk-infrastructure-read-file temp-file)
            (should (= (gethash :files-written nskk-infrastructure--statistics) 1))
            (should (= (gethash :files-read nskk-infrastructure--statistics) 1)))
        (delete-file temp-file)))))

(nskk-deftest-unit layer-infra-get-statistics-returns-plist
  "Test that get-statistics returns a proper plist."
  (nskk-infra-test-with-clean-state
    (let ((stats (nskk-infrastructure-get-statistics)))
      (should (listp stats))
      (should (numberp (plist-get stats :memory)))
      (should (numberp (plist-get stats :tasks-submitted)))
      (should (numberp (plist-get stats :tasks-completed)))
      (should (numberp (plist-get stats :active-timers))))))

;;;
;;; Timer Management Tests
;;;

(nskk-deftest-unit layer-infra-schedule-timer-returns-timer
  "Test that schedule-timer returns a timer object."
  (nskk-infra-test-with-clean-state
    (let ((timer (nskk-infrastructure-schedule-timer 999 nil #'ignore)))
      (unwind-protect
          (progn
            (should (timerp timer))
            (should (memq timer nskk-infrastructure--timers)))
        (nskk-infrastructure-cancel-timer timer)))))

(nskk-deftest-unit layer-infra-cancel-timer-removes-timer
  "Test that cancel-timer removes timer from the list."
  (nskk-infra-test-with-clean-state
    (let ((timer (nskk-infrastructure-schedule-timer 999 nil #'ignore)))
      (should (memq timer nskk-infrastructure--timers))
      (nskk-infrastructure-cancel-timer timer)
      (should-not (memq timer nskk-infrastructure--timers)))))

(nskk-deftest-unit layer-infra-cleanup-timers-clears-all
  "Test that cleanup-timers cancels and removes all timers."
  (nskk-infra-test-with-clean-state
    (nskk-infrastructure-schedule-timer 999 nil #'ignore)
    (nskk-infrastructure-schedule-timer 999 nil #'ignore)
    (should (= (length nskk-infrastructure--timers) 2))
    (nskk-infrastructure--cleanup-timers)
    (should (null nskk-infrastructure--timers))))

;;;
;;; Debug Mode Tests
;;;

(nskk-deftest-unit layer-infra-debug-enable-disable
  "Test that debug mode can be toggled."
  (let ((nskk-infrastructure--debug-enabled nil))
    (nskk-infrastructure-enable-debug)
    (should nskk-infrastructure--debug-enabled)
    (nskk-infrastructure-disable-debug)
    (should-not nskk-infrastructure--debug-enabled)))

(provide 'nskk-layer-infrastructure-test)

;;; nskk-layer-infrastructure-test.el ends here
