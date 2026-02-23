;;; nskk-native-compile-test.el --- Tests for nskk-native-compile.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-native-compile.el covering:
;; - Native compilation detection constants
;; - Type definitions
;; - Function property declarations
;; - Benchmark helper functions
;; - Status reporting

;;; Code:

(require 'ert)
(require 'nskk-native-compile)
(require 'nskk-test-framework)

;;;
;;; Native Compilation Detection Tests
;;;

(nskk-deftest-unit native-compile-available-constant
  "Test that nskk-native-compile-available is a boolean constant."
  (should (or (eq nskk-native-compile-available t)
              (eq nskk-native-compile-available nil))))

(nskk-deftest-unit native-compile-enabled-constant
  "Test that nskk-native-compile-enabled is a boolean constant."
  (should (or (eq nskk-native-compile-enabled t)
              (eq nskk-native-compile-enabled nil))))

(nskk-deftest-unit native-compile-enabled-implies-available
  "Test that enabled implies available."
  (when nskk-native-compile-enabled
    (should nskk-native-compile-available)))

;;;
;;; Type Definition Tests
;;;

(nskk-deftest-unit native-compile-type-nskk-string
  "Test nskk-string type is defined."
  (should (cl-typep "hello" 'nskk-string))
  (should (not (cl-typep 42 'nskk-string))))

(nskk-deftest-unit native-compile-type-nskk-character
  "Test nskk-character type is defined."
  (should (cl-typep ?a 'nskk-character))
  (should (cl-typep 0 'nskk-character))
  (should (not (cl-typep -1 'nskk-character))))

(nskk-deftest-unit native-compile-type-nskk-mode
  "Test nskk-mode type is defined."
  (should (cl-typep 'hiragana 'nskk-mode))
  (should (cl-typep 'katakana 'nskk-mode))
  (should (cl-typep 'ascii 'nskk-mode))
  (should (not (cl-typep 'invalid 'nskk-mode))))

;;;
;;; Function Property Tests
;;;

;; Removed tests for phantom function properties (nskk-romaji-char-p,
;; nskk-kana-char-p, etc.) -- these functions were never defined.

(nskk-deftest-unit native-compile-convert-romaji-pure
  "Test nskk-convert-romaji is declared side-effect-free."
  (should (eq (get 'nskk-convert-romaji 'side-effect-free) t)))

(nskk-deftest-unit native-compile-state-get-mode-pure
  "Test nskk-state-get-mode is declared side-effect-free."
  (should (eq (get 'nskk-state-get-mode 'side-effect-free) t)))

(nskk-deftest-unit native-compile-state-set-mode-not-pure
  "Test nskk-state-set-mode is NOT declared side-effect-free."
  (should (not (get 'nskk-state-set-mode 'side-effect-free))))

;;;
;;; Benchmark Helper Tests
;;;

(nskk-deftest-unit native-compile-benchmark-function-exists
  "Test that nskk--benchmark-function exists."
  (should (fboundp 'nskk--benchmark-function)))

(nskk-deftest-unit native-compile-benchmark-function-returns-plist
  "Test that nskk--benchmark-function returns proper statistics."
  (let ((result (nskk--benchmark-function (lambda () (+ 1 2)) 100)))
    (should (listp result))
    (should (numberp (plist-get result :mean)))
    (should (numberp (plist-get result :median)))
    (should (numberp (plist-get result :min)))
    (should (numberp (plist-get result :max)))
    (should (= (plist-get result :iterations) 100))))

(nskk-deftest-unit native-compile-benchmark-function-times-positive
  "Test that benchmark times are non-negative."
  (let ((result (nskk--benchmark-function (lambda () nil) 50)))
    (should (>= (plist-get result :mean) 0))
    (should (>= (plist-get result :min) 0))
    (should (>= (plist-get result :max) 0))
    (should (>= (plist-get result :max) (plist-get result :min)))))

;;;
;;; Async Compilation Tests
;;;

(nskk-deftest-unit native-compile-async-function-exists
  "Test that nskk-native-compile-async function exists."
  (should (fboundp 'nskk-native-compile-async)))

(nskk-deftest-unit native-compile-refresh-function-exists
  "Test that nskk-native-compile-refresh function exists."
  (should (fboundp 'nskk-native-compile-refresh)))

(nskk-deftest-unit native-compile-status-function-exists
  "Test that nskk-native-compile-status function exists."
  (should (fboundp 'nskk-native-compile-status)))

;;;
;;; Initialize Function Tests
;;;

(nskk-deftest-unit native-compile-initialize-function-exists
  "Test that nskk-native-compile-initialize function exists."
  (should (fboundp 'nskk-native-compile-initialize)))

(nskk-deftest-unit native-compile-after-save-function-exists
  "Test that nskk-native-compile-after-save function exists."
  (should (fboundp 'nskk-native-compile-after-save)))

;;;
;;; Module Feature Tests
;;;

(nskk-deftest-unit native-compile-provides-feature
  "Test that nskk-native-compile provides its feature."
  (should (featurep 'nskk-native-compile)))

(provide 'nskk-native-compile-test)

;;; nskk-native-compile-test.el ends here
