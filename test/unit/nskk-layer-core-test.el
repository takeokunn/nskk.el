;;; nskk-layer-core-test.el --- Tests for nskk-layer-core.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-layer-core.el covering:
;; - Conversion API (romaji, hiragana/katakana, zenkaku/hankaku)
;; - Character classification API
;; - Dictionary search API
;; - Performance statistics
;; - Romaji table validation

;;; Code:

(require 'ert)
(require 'nskk-layer-core)
(require 'nskk-test-framework)

;;;
;;; Conversion API Tests
;;;

(nskk-deftest-unit layer-core-convert-romaji-nil-input
  "Test convert-romaji with nil input."
  (should (null (nskk-core-convert-romaji nil))))

(nskk-deftest-unit layer-core-convert-romaji-non-string
  "Test convert-romaji with non-string input."
  (should (null (nskk-core-convert-romaji 42))))

(nskk-deftest-unit layer-core-convert-romaji-empty-string
  "Test convert-romaji with empty string."
  (let ((result (nskk-core-convert-romaji "")))
    ;; Empty string returns nil (no conversion possible)
    (should (null result))))

(nskk-deftest-unit layer-core-convert-romaji-basic
  "Test convert-romaji with basic input."
  (let ((result (nskk-core-convert-romaji "ka")))
    (should result)))

(nskk-deftest-unit layer-core-hiragana-to-katakana-nil
  "Test hiragana-to-katakana with nil input."
  (should (null (nskk-layer-core-hiragana-to-katakana nil))))

(nskk-deftest-unit layer-core-hiragana-to-katakana-non-string
  "Test hiragana-to-katakana with non-string input."
  (should (null (nskk-layer-core-hiragana-to-katakana 42))))

(nskk-deftest-unit layer-core-hiragana-to-katakana-basic
  "Test hiragana-to-katakana basic conversion."
  (should (string= (nskk-layer-core-hiragana-to-katakana "あいうえお") "アイウエオ")))

(nskk-deftest-unit layer-core-katakana-to-hiragana-nil
  "Test katakana-to-hiragana with nil input."
  (should (null (nskk-layer-core-katakana-to-hiragana nil))))

(nskk-deftest-unit layer-core-katakana-to-hiragana-non-string
  "Test katakana-to-hiragana with non-string input."
  (should (null (nskk-layer-core-katakana-to-hiragana 42))))

(nskk-deftest-unit layer-core-katakana-to-hiragana-basic
  "Test katakana-to-hiragana basic conversion."
  (should (string= (nskk-layer-core-katakana-to-hiragana "アイウエオ") "あいうえお")))

(nskk-deftest-unit layer-core-zenkaku-to-hankaku-nil
  "Test zenkaku-to-hankaku with nil input."
  (should (null (nskk-layer-core-zenkaku-to-hankaku nil))))

(nskk-deftest-unit layer-core-hankaku-to-zenkaku-nil
  "Test hankaku-to-zenkaku with nil input."
  (should (null (nskk-layer-core-hankaku-to-zenkaku nil))))

;;;
;;; Character Classification API Tests
;;;

(nskk-deftest-unit layer-core-classify-hiragana
  "Test hiragana classification in layer-core."
  (should (nskk-core-hiragana-p ?あ))
  (should (nskk-core-hiragana-p ?ん))
  (should (not (nskk-core-hiragana-p ?ア)))
  (should (not (nskk-core-hiragana-p ?a))))

(nskk-deftest-unit layer-core-classify-katakana
  "Test katakana classification in layer-core."
  (should (nskk-core-katakana-p ?ア))
  (should (nskk-core-katakana-p ?ン))
  (should (not (nskk-core-katakana-p ?あ)))
  (should (not (nskk-core-katakana-p ?a))))

(nskk-deftest-unit layer-core-classify-han
  "Test han (kanji) classification in layer-core."
  (should (nskk-core-han-p ?漢))
  (should (nskk-core-han-p ?字))
  (should (not (nskk-core-han-p ?あ)))
  (should (not (nskk-core-han-p ?ア)))
  (should (not (nskk-core-han-p ?a))))

(nskk-deftest-unit layer-core-classify-japanese
  "Test Japanese character classification."
  (should (nskk-core-japanese-p ?あ))
  (should (nskk-core-japanese-p ?ア))
  (should (nskk-core-japanese-p ?漢))
  (should (not (nskk-core-japanese-p ?a)))
  (should (not (nskk-core-japanese-p ?1))))

(nskk-deftest-unit layer-core-classify-non-integer
  "Test classification with non-integer input."
  (should (not (nskk-core-hiragana-p nil)))
  (should (not (nskk-core-katakana-p "a")))
  (should (not (nskk-core-han-p 'symbol))))

;;;
;;; Performance Statistics Tests
;;;

(nskk-deftest-unit layer-core-stats-returns-alist
  "Test that core-stats returns an alist."
  (let ((stats (nskk-core-stats)))
    (should (listp stats))
    (should (assq 'romaji-table-size stats))))

(nskk-deftest-unit layer-core-stats-table-size-positive
  "Test that romaji table size is positive."
  (let ((stats (nskk-core-stats)))
    (should (> (cdr (assq 'romaji-table-size stats)) 0))))

(nskk-deftest-unit layer-core-reset-stats-exists
  "Test that reset-stats function exists."
  (should (fboundp 'nskk-core-reset-stats)))

;;;
;;; Romaji Table Validation Tests
;;;

(nskk-deftest-unit layer-core-validate-romaji-table
  "Test romaji table validation."
  (should (nskk-core-validate-romaji-table)))

;;;
;;; Benchmark Function Tests
;;;

(nskk-deftest-unit layer-core-benchmark-exists
  "Test that benchmark function exists."
  (should (fboundp 'nskk-core-benchmark-romaji-conversion)))

(nskk-deftest-unit layer-core-benchmark-runs
  "Test that benchmark executes without error."
  (let ((result (nskk-core-benchmark-romaji-conversion 10)))
    (should (numberp result))
    (should (>= result 0))))

;;;
;;; Module Feature Tests
;;;

(nskk-deftest-unit layer-core-provides-feature
  "Test that nskk-layer-core provides its feature."
  (should (featurep 'nskk-layer-core)))

(provide 'nskk-layer-core-test)

;;; nskk-layer-core-test.el ends here
