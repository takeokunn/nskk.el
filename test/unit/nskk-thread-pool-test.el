;;; nskk-thread-pool-test.el --- Tests for nskk-thread-pool.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-thread-pool.el covering:
;; - Thread pool availability check
;; - Data structures (thread-pool, thread-task)
;; - CPU count detection
;; - Optimal pool size calculation
;; - Thread pool creation and shutdown
;; - Task submission and execution
;; - Queue management

;;; Code:

(require 'ert)
(require 'nskk-thread-pool)
(require 'nskk-test-framework)

;;;
;;; Availability Tests
;;;

(nskk-deftest-unit thread-pool-available-p-returns-boolean
  "Test that thread pool availability returns a boolean."
  (let ((result (nskk-thread-pool-available-p)))
    (should (or (eq result t) (eq result nil)))))

(nskk-deftest-unit thread-pool-available-checks-functions
  "Test availability checks required threading functions."
  (let ((result (nskk-thread-pool-available-p)))
    (if (and (fboundp 'make-thread)
             (fboundp 'make-mutex)
             (fboundp 'make-condition-variable))
        (should result)
      (should (not result)))))

;;;
;;; Data Structure Tests
;;;

(nskk-deftest-unit thread-pool-struct-exists
  "Test that nskk-thread-pool struct is defined."
  (should (fboundp 'nskk-thread-pool-p)))

(nskk-deftest-unit thread-task-struct-exists
  "Test that nskk-thread-task struct is defined."
  (should (fboundp 'nskk-thread-task-p)))

(nskk-deftest-unit thread-task-create-basic
  "Test creating a thread task."
  (let ((task (nskk-thread-task-create
               :function (lambda () (+ 1 2))
               :callback nil
               :error-handler nil)))
    (should (nskk-thread-task-p task))
    (should (functionp (nskk-thread-task-function task)))
    (should (null (nskk-thread-task-callback task)))
    (should (null (nskk-thread-task-error-handler task)))))

(nskk-deftest-unit thread-task-create-with-callback
  "Test creating a thread task with callback."
  (let ((task (nskk-thread-task-create
               :function (lambda () 42)
               :callback (lambda (r) (message "Got %s" r))
               :error-handler (lambda (e) (message "Error %s" e)))))
    (should (nskk-thread-task-p task))
    (should (functionp (nskk-thread-task-callback task)))
    (should (functionp (nskk-thread-task-error-handler task)))))

;;;
;;; CPU Count Tests
;;;

(nskk-deftest-unit thread-pool-get-cpu-count-positive
  "Test that CPU count returns a positive integer."
  (let ((count (nskk-thread-pool--get-cpu-count)))
    (should (integerp count))
    (should (> count 0))))

;;;
;;; Optimal Size Tests
;;;

(nskk-deftest-unit thread-pool-optimal-size-in-range
  "Test that optimal size is between 2 and 8."
  (let ((size (nskk-thread-pool--optimal-size)))
    (should (integerp size))
    (should (>= size 2))
    (should (<= size 8))))

;;;
;;; Thread Pool Creation and Shutdown Tests
;;;

(nskk-deftest-unit thread-pool-create-sets-size
  "Test that thread pool creation sets pool size."
  (when (nskk-thread-pool-available-p)
    (let ((pool (nskk-thread-pool-create 2)))
      (unwind-protect
          (progn
            (should (nskk-thread-pool-p pool))
            (should (= (nskk-thread-pool-size pool) 2))
            (should (not (nskk-thread-pool-shutdown-flag pool)))
            (should (= (nskk-thread-pool-active-count pool) 0)))
        (nskk-thread-pool-shutdown pool t)))))

(nskk-deftest-unit thread-pool-shutdown-sets-flag
  "Test that shutdown sets the flag."
  (when (nskk-thread-pool-available-p)
    (let ((pool (nskk-thread-pool-create 2)))
      (nskk-thread-pool-shutdown pool t)
      (should (nskk-thread-pool-shutdown-flag pool)))))

(nskk-deftest-unit thread-pool-submit-to-shutdown-errors
  "Test that submitting to a shutdown pool signals error."
  (when (nskk-thread-pool-available-p)
    (let ((pool (nskk-thread-pool-create 2)))
      (nskk-thread-pool-shutdown pool t)
      (should-error (nskk-thread-submit pool (lambda () (+ 1 2)))))))

(nskk-deftest-unit thread-pool-status-function-exists
  "Test that nskk-thread-pool-status function exists."
  (should (fboundp 'nskk-thread-pool-status)))

(nskk-deftest-unit thread-pool-wait-for-completion-exists
  "Test that wait-for-completion function exists."
  (should (fboundp 'nskk-thread-pool-wait-for-completion)))

;;;
;;; Customization Tests
;;;

(nskk-deftest-unit thread-pool-custom-group-exists
  "Test that the nskk-thread-pool custom group exists."
  (should (get 'nskk-thread-pool 'custom-group)))

(nskk-deftest-unit thread-pool-default-size-is-nil
  "Test that default pool size is nil (auto)."
  (should (eq (default-value 'nskk-thread-pool-default-size) nil)))

(nskk-deftest-unit thread-pool-idle-timeout-default
  "Test default idle timeout value."
  (should (integerp nskk-thread-pool-idle-timeout))
  (should (> nskk-thread-pool-idle-timeout 0)))

(nskk-deftest-unit thread-pool-max-queue-size-default
  "Test default max queue size value."
  (should (integerp nskk-thread-pool-max-queue-size))
  (should (> nskk-thread-pool-max-queue-size 0)))

;;;
;;; Module Feature Tests
;;;

(nskk-deftest-unit thread-pool-provides-feature
  "Test that nskk-thread-pool provides its feature."
  (should (featurep 'nskk-thread-pool)))

(provide 'nskk-thread-pool-test)

;;; nskk-thread-pool-test.el ends here
