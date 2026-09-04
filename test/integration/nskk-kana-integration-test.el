;;; nskk-kana-integration-test.el --- Kana conversion integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Cross-module tests for nskk-kana: its interaction with the Prolog
;; database, and the conversion pipeline other modules reach it through.
;; Single-function value assertions belong in test/unit/nskk-kana-test.el.

;;; Code:

(require 'ert)
(require 'nskk-kana)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)

;;;; Prolog database integration

(nskk-describe "nskk-kana-initialize against the Prolog database"

  (nskk-it "populates kana-conversion/3 in a freshly isolated database"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (should (eq (nskk-prolog-query-value
                   '(kana-conversion katakana insert \?fn) '\?fn)
                  'nskk-kana-string-hiragana-to-katakana))
      (should (eq (nskk-prolog-query-value
                   '(kana-conversion katakana normalize \?fn) '\?fn)
                  'nskk-kana-string-katakana-to-hiragana))))

  (nskk-it "resolves every declared mode in both directions"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (dolist (mode '(hiragana katakana katakana-半角))
        (dolist (direction '(insert normalize))
          (should (nskk-prolog-query-value
                   `(kana-conversion ,mode ,direction \?fn) '\?fn))))))

  ;; `nskk-prolog-assert' does not deduplicate, so the guard in
  ;; `nskk-kana-initialize' is the only thing preventing row growth.
  ;; Measured as a delta: the isolated database inherits the rows asserted
  ;; when nskk-kana was first loaded, so the absolute count is not 1.
  (nskk-it "adds no further rows once initialized"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (let ((after-first (length (nskk-prolog-query-all-values
                                  '(kana-conversion katakana insert \?fn) '\?fn))))
        (nskk-kana-initialize)
        (nskk-kana-initialize)
        (should (= after-first
                   (length (nskk-prolog-query-all-values
                            '(kana-conversion katakana insert \?fn) '\?fn))))))))

;;;; Conversion pipeline reached through the mode table

(nskk-describe "mode-driven conversion pipeline"

  (nskk-it "converts hiragana for each insert mode via the Prolog table"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (should (string= (nskk-kana-convert-for-mode "かんじ" 'hiragana) "かんじ"))
      (should (string= (nskk-kana-convert-for-mode "かんじ" 'katakana) "カンジ"))
      (should (string= (nskk-kana-convert-for-mode "かんじ" 'katakana-半角) "ｶﾝｼﾞ"))))

  (nskk-it "normalizes each script back to hiragana for dictionary lookup"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (should (string= (nskk-kana-normalize-for-lookup "かんじ" 'hiragana) "かんじ"))
      (should (string= (nskk-kana-normalize-for-lookup "カンジ" 'katakana) "かんじ"))
      (should (string= (nskk-kana-normalize-for-lookup "ｶﾝｼﾞ" 'katakana-半角) "かんじ"))))

  ;; insert then normalize is the round trip a dictionary lookup performs
  ;; after the user has typed in a non-hiragana mode.
  (nskk-it "recovers the original reading through insert then normalize"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (dolist (mode '(hiragana katakana katakana-半角))
        (should (string= (nskk-kana-normalize-for-lookup
                          (nskk-kana-convert-for-mode "かんじ" mode)
                          mode)
                         "かんじ"))))))

;;;; CPS pipeline

(nskk-describe "CPS conversion pipeline"

  (nskk-it "chains hiragana->katakana->hiragana through continuations"
    (nskk-kana-string-hiragana-to-katakana/k "ひらがな"
      (lambda (kata)
        (should (string= kata "ヒラガナ"))
        (nskk-kana-string-katakana-to-hiragana/k kata
          (lambda (back) (should (string= back "ひらがな")))
          (lambda () (ert-fail "katakana-to-hiragana failed"))))
      (lambda () (ert-fail "hiragana-to-katakana failed"))))

  (nskk-it "propagates failure to the not-found continuation for non-string input"
    (let ((failed nil))
      (nskk-kana-string-hiragana-to-katakana/k 42
        (lambda (_) (ert-fail "should not succeed for integer input"))
        (lambda () (setq failed t)))
      (should failed))))

;;;; PBT: hiragana-katakana string roundtrip

(nskk-property-test kana-string-hiragana-katakana-roundtrip
  ((h hiragana-string))
  (let ((roundtrip-ok nil))
    (nskk-kana-string-hiragana-to-katakana/k h
      (lambda (kata)
        (nskk-kana-string-katakana-to-hiragana/k kata
          (lambda (back)
            (should (string= back h))
            (setq roundtrip-ok t))
          (lambda () (ert-fail "katakana-to-hiragana failed"))))
      (lambda () (ert-fail "hiragana-to-katakana failed")))
    roundtrip-ok)
  15)

(provide 'nskk-kana-integration-test)

;;; nskk-kana-integration-test.el ends here
