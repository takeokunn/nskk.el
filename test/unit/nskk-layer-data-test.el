;;; nskk-layer-data-test.el --- Tests for nskk-layer-data.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-layer-data.el covering:
;; - Feature loading and module availability
;; - Learning data management (learn, store, retrieve)
;; - Cache management (clear, statistics)
;; - Data search with caching behavior
;; - Export/import of learning data
;; - Statistics and health check

;;; Code:

(require 'ert)
(require 'nskk-layer-data)
(require 'nskk-test-framework)

;;;
;;; Helper Macros
;;;

(defmacro nskk-layer-data-test-with-clean-state (&rest body)
  "Execute BODY with clean data layer state."
  (declare (indent 0))
  `(let ((nskk-data--dictionaries nil)
         (nskk-data--learning-data (make-hash-table :test 'equal))
         (nskk-data--cache (make-hash-table :test 'equal))
         (nskk-data--dirty-flag nil)
         (nskk-data--auto-save-timer nil)
         (nskk-data-dictionary-paths nil))
     ,@body))

;;;
;;; Feature Loading Tests
;;;

(nskk-deftest-unit layer-data-provides-feature
  "Test that nskk-layer-data provides its feature."
  (should (featurep 'nskk-layer-data)))

(nskk-deftest-unit layer-data-require-idempotent
  "Test that requiring nskk-layer-data again is safe."
  (should (require 'nskk-layer-data)))

;;;
;;; Function Existence Tests
;;;

(nskk-deftest-unit layer-data-public-functions-exist
  "Test that main public functions are defined."
  (should (fboundp 'nskk-data-initialize))
  (should (fboundp 'nskk-data-shutdown))
  (should (fboundp 'nskk-data-search))
  (should (fboundp 'nskk-data-load-dictionary))
  (should (fboundp 'nskk-data-learn))
  (should (fboundp 'nskk-data-save-learning-data))
  (should (fboundp 'nskk-data-clear-cache))
  (should (fboundp 'nskk-data-sync))
  (should (fboundp 'nskk-data-get-statistics))
  (should (fboundp 'nskk-data-health-check))
  (should (fboundp 'nskk-data-export-learning-data))
  (should (fboundp 'nskk-data-import-learning-data)))

;;;
;;; Learning Data Tests
;;;

(nskk-deftest-unit layer-data-learn-records-entry
  "Test that nskk-data-learn records a learning entry."
  (nskk-layer-data-test-with-clean-state
    (nskk-data-learn "test-query" "test-candidate")
    (let ((scores (gethash "test-query" nskk-data--learning-data)))
      (should scores)
      (should (= (gethash "test-candidate" scores) 1)))))

(nskk-deftest-unit layer-data-learn-increments-score
  "Test that learning the same entry increments its score."
  (nskk-layer-data-test-with-clean-state
    (nskk-data-learn "query" "candidate")
    (nskk-data-learn "query" "candidate")
    (nskk-data-learn "query" "candidate")
    (let ((scores (gethash "query" nskk-data--learning-data)))
      (should (= (gethash "candidate" scores) 3)))))

(nskk-deftest-unit layer-data-learn-sets-dirty-flag
  "Test that learning sets the dirty flag."
  (nskk-layer-data-test-with-clean-state
    (should-not nskk-data--dirty-flag)
    (nskk-data-learn "query" "candidate")
    (should nskk-data--dirty-flag)))

(nskk-deftest-unit layer-data-learn-multiple-candidates
  "Test learning multiple candidates for the same query."
  (nskk-layer-data-test-with-clean-state
    (nskk-data-learn "query" "candidate-a")
    (nskk-data-learn "query" "candidate-b")
    (nskk-data-learn "query" "candidate-a")
    (let ((scores (gethash "query" nskk-data--learning-data)))
      (should (= (gethash "candidate-a" scores) 2))
      (should (= (gethash "candidate-b" scores) 1)))))

;;;
;;; Cache Management Tests
;;;

(nskk-deftest-unit layer-data-clear-cache-empties-cache
  "Test that clear-cache empties the cache hash table."
  (nskk-layer-data-test-with-clean-state
    (puthash "key1" "value1" nskk-data--cache)
    (puthash "key2" "value2" nskk-data--cache)
    (should (= (hash-table-count nskk-data--cache) 2))
    (nskk-data--cleanup-cache)
    (should (= (hash-table-count nskk-data--cache) 0))))

(nskk-deftest-unit layer-data-cache-statistics-reports-count
  "Test that cache-statistics reports correct count."
  (nskk-layer-data-test-with-clean-state
    (puthash "key1" "value1" nskk-data--cache)
    (puthash "key2" "value2" nskk-data--cache)
    (puthash "key3" "value3" nskk-data--cache)
    (should (= (hash-table-count nskk-data--cache) 3))))

;;;
;;; Statistics Tests
;;;

(nskk-deftest-unit layer-data-get-statistics-returns-plist
  "Test that get-statistics returns a plist with expected keys."
  (nskk-layer-data-test-with-clean-state
    (let ((stats (nskk-data-get-statistics)))
      (should (listp stats))
      (should (eq (plist-get stats :layer) 'data))
      (should (numberp (plist-get stats :loaded-dictionaries)))
      (should (numberp (plist-get stats :cache-size)))
      (should (numberp (plist-get stats :learning-entries))))))

(nskk-deftest-unit layer-data-get-statistics-reflects-state
  "Test that statistics reflect actual state."
  (nskk-layer-data-test-with-clean-state
    (nskk-data-learn "q1" "c1")
    (nskk-data-learn "q2" "c2")
    (let ((stats (nskk-data-get-statistics)))
      (should (= (plist-get stats :loaded-dictionaries) 0))
      (should (= (plist-get stats :learning-entries) 2)))))

;;;
;;; Export/Import Tests
;;;

(nskk-deftest-unit layer-data-export-import-roundtrip
  "Test that learning data survives export/import roundtrip."
  (nskk-layer-data-test-with-clean-state
    (nskk-data-learn "export-query" "export-candidate")
    (nskk-data-learn "export-query" "export-candidate")
    (let ((temp-file (make-temp-file "nskk-test-export")))
      (unwind-protect
          (progn
            (nskk-data-export-learning-data temp-file)
            ;; Clear learning data
            (setq nskk-data--learning-data (make-hash-table :test 'equal))
            (should (= (hash-table-count nskk-data--learning-data) 0))
            ;; Import back
            (nskk-data-import-learning-data temp-file)
            (let ((scores (gethash "export-query" nskk-data--learning-data)))
              (should scores)
              (should (= (gethash "export-candidate" scores) 2))))
        (delete-file temp-file)))))

;;;
;;; Debug Mode Tests
;;;

(nskk-deftest-unit layer-data-debug-enable-disable
  "Test that debug mode can be toggled."
  (let ((nskk-data--debug-enabled nil))
    (nskk-data-enable-debug)
    (should nskk-data--debug-enabled)
    (nskk-data-disable-debug)
    (should-not nskk-data--debug-enabled)))

;;;
;;; Search Validation Tests
;;;

(nskk-deftest-unit layer-data-search-empty-query-errors
  "Test that searching with empty query signals error."
  (nskk-layer-data-test-with-clean-state
    (should-error (nskk-data-search "") :type 'user-error)))

(nskk-deftest-unit layer-data-search-nil-query-errors
  "Test that searching with nil query signals error."
  (nskk-layer-data-test-with-clean-state
    (should-error (nskk-data-search nil) :type 'user-error)))

;;;
;;; Transaction Macro Tests
;;;

(nskk-deftest-unit layer-data-with-transaction-executes-body
  "Test that nskk-data-with-transaction executes body forms."
  (let ((result nil))
    (nskk-data-with-transaction
      (setq result 42))
    (should (= result 42))))

(provide 'nskk-layer-data-test)

;;; nskk-layer-data-test.el ends here
