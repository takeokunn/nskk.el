;;; nskk-memory-optimize-test.el --- Tests for nskk-memory-optimize.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-memory-optimize.el covering:
;; - Module loading (stub implementation)
;; - Feature availability
;; - Stub contract verification
;; - No side effects from loading

;;; Code:

(require 'ert)
(require 'nskk-memory-optimize)
(require 'nskk-test-framework)

;;;
;;; Module Loading Tests
;;;

(nskk-deftest-unit memory-optimize-provides-feature
  "Test that nskk-memory-optimize provides its feature."
  (should (featurep 'nskk-memory-optimize)))

(nskk-deftest-unit memory-optimize-loadable
  "Test that nskk-memory-optimize loads without error."
  (should (require 'nskk-memory-optimize)))

(nskk-deftest-unit memory-optimize-require-idempotent
  "Test that requiring nskk-memory-optimize again is safe."
  (should (require 'nskk-memory-optimize))
  (should (featurep 'nskk-memory-optimize)))

;;;
;;; Stub Contract Tests
;;;

(nskk-deftest-unit memory-optimize-no-public-api-on-load
  "Test that loading the stub module does not define any public API functions."
  (let ((public-funcs nil))
    (mapatoms (lambda (sym)
                (when (and (fboundp sym)
                           (string-prefix-p "nskk-memory-optimize-"
                                            (symbol-name sym)))
                  (push sym public-funcs))))
    ;; Stub should not define any public functions
    (should (null public-funcs))))

(nskk-deftest-unit memory-optimize-feature-is-only-export
  "Test that the only export of the stub is the feature symbol."
  (should (featurep 'nskk-memory-optimize))
  ;; Verify no functions named nskk-memory-optimize-* exist
  (let ((funcs nil))
    (mapatoms (lambda (sym)
                (when (and (fboundp sym)
                           (string-prefix-p "nskk-memory-optimize-"
                                            (symbol-name sym)))
                  (push sym funcs))))
    (should (null funcs))))

(provide 'nskk-memory-optimize-test)

;;; nskk-memory-optimize-test.el ends here
