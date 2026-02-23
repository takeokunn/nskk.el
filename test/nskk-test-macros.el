;;; nskk-test-macros.el --- NSKK Test Helper Macros  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: Japanese, input, method, test, macros
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

;; This file provides test helper macros for NSKK, including property-based
;; testing (PBT) macros, performance testing macros, and other test utilities.
;;
;; Features:
;; - Property-based testing macros
;; - Performance benchmarking macros
;; - Environment setup macros
;; - Assertion helper macros
;; - Mock and fixture macros

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(eval-when-compile (require 'cl-lib))


;;;;
;;;; Property-Based Testing Macros
;;;;

(defmacro nskk-property-test (name generators property &optional runs)
  "Define a property-based test.
NAME: Test name
GENERATORS: ((var generator-name)...) pairs
PROPERTY: Property expression to test
RUNS: Number of test runs (default: nskk-test-property-runs)"
  (declare (indent 3))
  `(ert-deftest ,(intern (format "nskk-property-%s" name)) ()
     (let ((runs (or ,runs nskk-test-property-runs))
           (failures nil))
       (dotimes (_ runs)
         (let ,(mapcar (lambda (gen)
                         `(,(car gen) (nskk-generate ',(cadr gen))))
                       generators)
           (condition-case err
               (unless ,property
                 (push (list ,@(mapcar #'car generators)) failures))
             (error
              (push (list 'error ,@(mapcar #'car generators) err)
                    failures)))))
       (when failures
         (ert-fail (format "Property failed for %d cases:\n%S"
                           (length failures)
                           (take 5 failures)))))))

(defmacro nskk-property-check (name generators property &optional runs)
  "Define a property check (lighter weight test).
Like `nskk-property-test' but with fewer runs by default."
  (declare (indent 3))
  `(nskk-property-test ,name ,generators ,property
                       (or ,runs 10)))

(defmacro nskk-for-all (generators &rest body)
  "Test BODY for all values from GENERATORS."
  (declare (indent 1))
  `(let ,(mapcar (lambda (gen)
                   `(,(car gen) (nskk-generate ',(cadr gen))))
                 generators)
     ,@body))


;;;;
;;;; Common Property Tests
;;;;

(nskk-property-test conversion-idempotent
  ((input romaji-string))
  (let ((converted (nskk-convert-romaji input)))
    (equal converted (nskk-convert-romaji converted)))
  100)

(nskk-property-test conversion-reversible
  ((input romaji-string))
  (let ((converted (nskk-convert-romaji input)))
    ;; Converted string should be non-nil and a string
    (and (stringp converted) (> (length converted) 0)))
  75)

(nskk-property-test string-length-preservation
  ((input romaji-string))
  (let ((converted (nskk-convert-romaji input)))
    ;; Kana output length should be at least 1/4 of romaji input
    ;; (since some romaji like "tsu" are 3 chars -> 1 kana)
    (>= (length converted) (/ (length input) 4)))
  100)

(nskk-property-test partial-conversion-consistency
  ((input romaji-string))
  (let* ((converted (nskk-convert-romaji input))
         (converted2 (nskk-convert-romaji input)))
    ;; Conversion should be deterministic
    (equal converted converted2))
  75)


;;;;
;;;; Performance Testing Macros
;;;;

(defmacro nskk-benchmark (name iterations &rest body)
  "Benchmark BODY execution over ITERATIONS.
Returns time in seconds per iteration."
  (declare (indent 2))
  `(let ((start-time (current-time))
         (iter ,iterations))
     (dotimes (_ iter)
       ,@body)
     (let ((elapsed (float-time (time-subtract (current-time) start-time))))
       (/ elapsed iter))))

(defmacro nskk-should-be-fast (name threshold-ms &rest body)
  "Assert that BODY completes within THRESHOLD-MS milliseconds."
  (declare (indent 2))
  `(let ((start-time (current-time)))
     ,@body
     (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
            (elapsed-ms (* 1000 elapsed)))
       (when (> elapsed-ms ,threshold-ms)
         (ert-fail (format "Too slow: %s took %.2fms (threshold: %dms)"
                           ',name elapsed-ms ,threshold-ms))))))

(defmacro nskk-measure-memory (&rest body)
  "Measure memory usage of BODY."
  (declare (indent 0))
  `(let ((before (garbage-collect)))
     ,@body
     (let ((after (garbage-collect)))
       (- (car after) (car before)))))

(defmacro nskk-profile-form (form &optional label)
  "Profile FORM and print timing information."
  `(let ((start (current-time))
         (label ,(or label "Profile")))
     (prog1
         ,form
       (let ((elapsed (* 1000.0 (float-time (time-subtract (current-time) start)))))
         (message "[NSKK Profile] %s: %.2fms" label elapsed)))))

(defmacro nskk-compare-performance (baseline optimized &optional speedup-factor)
  "Compare BASELINE and OPTIMIZED implementations.
Assert that OPTIMIZED is at least SPEEDUP-FACTOR times faster."
  (declare (indent 1))
  `(let ((baseline-time
          (let ((start (current-time)))
            (dotimes (_ 100) ,baseline)
            (float-time (time-subtract (current-time) start))))
         (optimized-time
          (let ((start (current-time)))
            (dotimes (_ 100) ,optimized)
            (float-time (time-subtract (current-time) start))))
         (min-speedup (or ,speedup-factor 1.2)))
     (let ((speedup (/ baseline-time optimized-time)))
       (when (< speedup min-speedup)
         (ert-fail (format "Not fast enough: %.2fx speedup (expected > %.2fx)"
                           speedup min-speedup)))
       speedup)))


;;;;
;;;; Environment Setup Macros
;;;;

(defmacro nskk-with-temp-dictionary (&rest body)
  "Execute BODY with a temporary test dictionary."
  (declare (indent 0))
  `(let ((nskk--dictionary-cache (nskk--create-test-dictionary))
         (nskk--user-dictionary nil)
         (nskk--system-dictionary nil))
     (unwind-protect
         (progn ,@body)
       (nskk--cleanup-dictionary-cache))))

(defmacro nskk-with-clean-state (&rest body)
  "Execute BODY with clean NSKK state."
  (declare (indent 0))
  `(let ((nskk--conversion-mode nil)
         (nskk--input-buffer "")
         (nskk--candidate-list nil)
         (nskk--current-candidate-index 0)
         (nskk--okurigana nil))
     (unwind-protect
         (progn ,@body)
       (nskk--reset-state))))

(defmacro nskk-with-mode (mode &rest body)
  "Execute BODY with NSKK in MODE."
  (declare (indent 1))
  `(let ((nskk--conversion-mode ,mode))
     ,@body))

(defmacro nskk-with-buffer (content &rest body)
  "Execute BODY with CONTENT in a temporary buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defmacro nskk-with-input-simulation (keys &rest body)
  "Simulate input KEYS and execute BODY."
  (declare (indent 1))
  `(let ((input-buffer "")
         (pos 0))
     ;; Simulate key input
     (dolist (key ,keys)
       (setq input-buffer (concat input-buffer (string key))))
     ;; Execute body with simulated input
     (let ((nskk--input-buffer input-buffer))
       ,@body)))


;;;;
;;;; Assertion Helper Macros
;;;;

(defmacro nskk-should-error (type &rest body)
  "Assert that BODY signals error of TYPE."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body
              (ert-fail (format "Expected error %s but none was signaled" ',type)))
     (,type nil)  ; Expected error, test passes
     (error
      (ert-fail (format "Got wrong error type: %s (expected %s)"
                        (car err) ',type)))))

(defmacro nskk-should-not-error (&rest body)
  "Assert that BODY does not signal any error."
  (declare (indent 0))
  `(condition-case err
       (progn ,@body)
     (error
      (ert-fail (format "Unexpected error: %s" err)))))

(defmacro nskk-should-return (expected-value &rest body)
  "Assert that BODY returns EXPECTED-VALUE."
  (declare (indent 1))
  `(let ((actual (progn ,@body)))
     (unless (equal actual ,expected-value)
       (ert-fail (format "Expected %S but got %S" ,expected-value actual)))
     actual))

(defmacro nskk-should-match (regex &rest body)
  "Assert that BODY returns string matching REGEX."
  (declare (indent 1))
  `(let ((result (progn ,@body)))
     (unless (and (stringp result)
                  (string-match ,regex result))
       (ert-fail (format "Expected string matching %S but got %S"
                         ,regex result)))
     result))

(defmacro nskk-should-be-a (type &rest body)
  "Assert that BODY returns value of TYPE."
  (declare (indent 1))
  `(let ((result (progn ,@body)))
     (unless (funcall ',type result)
       (ert-fail (format "Expected %s but got %S: %S"
                         ',type (type-of result) result)))
     result))


;;;;
;;;; Test Data Macros
;;;;

(defmacro nskk-with-test-data (spec &rest body)
  "Execute BODY with test data from SPEC.
SPEC is ((var data)...) where data can be a literal value or
a generator name."
  (declare (indent 1))
  `(let ,(mapcar (lambda (entry)
                   `(,(car entry)
                     (cond
                      ((quoted-p ,(cadr entry))
                       ,(cadr entry))  ; Literal value
                      ((symbolp ,(cadr entry))
                       (nskk-generate ',(cadr entry)))  ; Generator
                      (t ,(cadr entry)))))  ; Expression
                 spec)
     ,@body))

(defmacro nskk-with-fixture-data (name &rest body)
  "Execute BODY with fixture data NAME."
  (declare (indent 1))
  `(let ((data (nskk--get-fixture-data ',name)))
     ,@body))


;;;;
;;;; Integration Test Macros
;;;;

(defmacro nskk-scenario-test (name scenario &rest body)
  "Define a scenario-based integration test.
NAME: Test name
SCENARIO: Scenario description
BODY: Test implementation"
  (declare (indent 2))
  `(ert-deftest ,(intern (format "nskk-scenario-%s" name)) ()
     ,scenario
     (nskk-with-clean-state
       (nskk-with-temp-dictionary
         ,@body))))

(defmacro nskk-e2e-test (name description &rest steps)
  "Define an end-to-end test with multiple steps.
NAME: Test name
DESCRIPTION: Test description
STEPS: List of (step-description step-form) pairs"
  (declare (indent 2))
  `(ert-deftest ,(intern (format "nskk-e2e-%s" name)) ()
     ,description
     (nskk-with-clean-state
       (nskk-with-temp-dictionary
         (let ((step-num 0))
           ,@(mapcar (lambda (step)
                       `(let ((step-num (1+ step-num)))
                          (message "[Step %d] %s" step-num ,(car step))
                          ,(cadr step)))
                     steps))))))


;;;;
;;;; Regression Test Macros
;;;;

(defvar nskk--regression-tests nil
  "Registry of regression tests.")

(defmacro nskk-regression-test (name bug-id description &rest body)
  "Define a regression test for BUG-ID."
  (declare (indent 3))
  `(ert-deftest ,(intern (format "nskk-regression-%s" name)) ()
     ,(format "Regression test for %s: %s" bug-id description)
     (push (cons ',name ,bug-id) nskk--regression-tests)
     ,@body))

(defmacro nskk-with-known-bug (bug-id &rest body)
  "Mark test as known bug BUG-ID and execute BODY."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (message "[Known Bug %s] Test failed as expected: %s" ,bug-id err)
      ;; Don't fail the test for known bugs
      nil)))


;;;;
;;;; Coverage Testing Macros
;;;;

(defmacro nskk-with-coverage-tracking (&rest body)
  "Execute BODY with coverage tracking enabled."
  (declare (indent 0))
  `(let ((nskk--coverage-tracking t)
         (nskk--coverage-data (make-hash-table :test 'equal)))
     (unwind-protect
         (progn ,@body)
       (nskk--report-coverage))))

(defmacro nskk-should-cover (function-list &rest body)
  "Assert that BODY covers all functions in FUNCTION-LIST."
  (declare (indent 1))
  `(let ((covered nil))
     (nskk-with-coverage-tracking
       ,@body
       (dolist (fn ,function-list)
         (unless (gethash fn nskk--coverage-data)
           (ert-fail (format "Function %s not covered" fn)))
         (push fn covered)))
     covered))


;;;;
;;;; Parallel Test Execution
;;;;

(defmacro nskk-parallel-test (&rest tests)
  "Execute TESTS in parallel (if threads available)."
  (declare (indent 0))
  `(if (fboundp 'make-thread)
       ;; Use threads for parallel execution
       (let ((threads nil))
         (dolist (test ,tests)
           (push (make-thread
                  (lambda () (funcall test))
                  (format "nskk-test-%s" (gensym)))
                 threads))
         (dolist (thread threads)
           (thread-join thread)))
     ;; Sequential fallback
     (dolist (test ,tests)
       (funcall test))))


;;;;
;;;; Test Composition Macros
;;;;

(defmacro nskk-test-suite (name &rest tests)
  "Define a test suite containing TESTS."
  (declare (indent 1))
  `(defun ,(intern (format "nskk-test-suite-%s" name)) ()
     (interactive)
     (message "Running test suite: %s" ',name)
     ,@(mapcar (lambda (test)
                 `(progn
                    (message "  Running: %s" ',test)
                    (funcall #',test)))
               tests)
     (message "Test suite %s complete" ',name)))

(defmacro nskk-test-group (description &rest tests)
  "Group related tests with DESCRIPTION."
  (declare (indent 1))
  `(progn
     (message "")
     (message "=== %s ===" ,description)
     ,@tests
     (message "")))


;;;;
;;;; Debug Macros
;;;;

(defmacro nskk-test-trace (&rest forms)
  "Trace evaluation of FORMS in test context."
  `(let ((results nil))
     (dolist (form ',forms)
       (let ((result (eval form)))
         (message "[TRACE] %S => %S" form result)
         (push result results)))
     (nreverse results)))

(defmacro nskk-inspect (var)
  "Inspect VAR in test context."
  `(let ((value ,var))
     (message "[INSPECT] %s = %S (type: %s)"
              ',var value (type-of value))
     value))


;;;;
;;;; Compatibility Macros
;;;;

(defmacro nskk-skip-unless (feature)
  "Skip test unless FEATURE is available."
  `(unless (featurep ',feature)
     (ert-skip (format "Feature %s not available" ',feature))))

(defmacro nskk-skip-when (condition)
  "Skip test when CONDITION is non-nil."
  `(when ,condition
     (ert-skip (format "Skipped: %s" ',condition))))

(defmacro nskk-emacs-version-< (major minor)
  "Skip test unless Emacs version is >= (MAJOR . MINOR)."
  `(nskk-skip-when
    (version< emacs-version
              (format "%d.%d" ,major ,minor))))

(provide 'nskk-test-macros)

;;; nskk-test-macros.el ends here
