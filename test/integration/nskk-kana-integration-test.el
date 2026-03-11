;;; nskk-kana-integration-test.el --- Kana conversion integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for nskk-kana.el (Layer 1: Core Engine).
;;
;; Tests verify the cross-module call chain between nskk-kana.el and
;; nskk-prolog.el -- specifically that kana conversion facts are correctly
;; initialized and queried.
;;
;; Hiragana <-> katakana conversion uses pure code-point arithmetic and does
;; not depend on Prolog facts; zenkaku <-> hankaku conversion does require
;; Prolog fact initialization via `nskk-kana-initialize'.
;;
;; Zenkaku <-> hankaku tests and the idempotency tests use
;; `nskk-prolog-test-with-isolated-db' to prevent cross-test Prolog DB
;; pollution and to ensure idempotency flags are reset so that
;; `nskk-kana-initialize' repopulates the fresh isolated DB on each test.
;; Hiragana <-> katakana tests do NOT use isolation: they are pure arithmetic
;; and touch no Prolog facts.
;;
;; String-level hiragana <-> katakana tests use
;; `nskk-kana-string-hiragana-to-katakana/k' and
;; `nskk-kana-string-katakana-to-hiragana/k' (the public string-level CPS
;; API).  The lower-level `nskk-kana-hiragana-to-katakana/k' and
;; `nskk-kana-katakana-to-hiragana/k' operate on single character integers
;; and are not tested here.

;;; Code:

(require 'ert)
(require 'nskk-kana)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)

;;;; hiragana to katakana string conversion

(nskk-describe "hiragana to katakana string conversion"

  (nskk-it "converts single hiragana あ to ア"
    (nskk-kana-string-hiragana-to-katakana/k "あ"
      (lambda (result) (should (string= result "ア")))
      (lambda () (ert-fail "conversion failed unexpectedly"))))

  (nskk-it "converts multi-character string ひらがな to ヒラガナ"
    (nskk-kana-string-hiragana-to-katakana/k "ひらがな"
      (lambda (result) (should (string= result "ヒラガナ")))
      (lambda () (ert-fail "conversion failed unexpectedly"))))

  (nskk-it "converts empty string to empty string"
    (nskk-kana-string-hiragana-to-katakana/k ""
      (lambda (result) (should (string= result "")))
      #'ignore))

  (nskk-it "passes through non-hiragana characters unchanged"
    ;; ASCII characters are not in the hiragana range; they pass through.
    (nskk-kana-string-hiragana-to-katakana/k "abc"
      (lambda (result) (should (string= result "abc")))
      (lambda () (ert-fail "conversion failed unexpectedly"))))

  (nskk-it "calls on-not-found for non-string input"
    (let ((failed nil))
      (nskk-kana-string-hiragana-to-katakana/k 42
        (lambda (_result) (ert-fail "should not succeed for integer input"))
        (lambda () (setq failed t)))
      (should failed))))

;;;; katakana to hiragana string conversion

(nskk-describe "katakana to hiragana string conversion"

  (nskk-it "converts single katakana ア to あ"
    (nskk-kana-string-katakana-to-hiragana/k "ア"
      (lambda (result) (should (string= result "あ")))
      (lambda () (ert-fail "conversion failed unexpectedly"))))

  (nskk-it "converts multi-character string カタカナ to かたかな"
    (nskk-kana-string-katakana-to-hiragana/k "カタカナ"
      (lambda (result) (should (string= result "かたかな")))
      (lambda () (ert-fail "conversion failed unexpectedly"))))

  (nskk-it "converts empty string to empty string"
    (nskk-kana-string-katakana-to-hiragana/k ""
      (lambda (result) (should (string= result "")))
      #'ignore))

  (nskk-it "passes through non-katakana characters unchanged"
    (nskk-kana-string-katakana-to-hiragana/k "abc"
      (lambda (result) (should (string= result "abc")))
      (lambda () (ert-fail "conversion failed unexpectedly"))))

  (nskk-it "calls on-not-found for non-string input"
    (let ((failed nil))
      (nskk-kana-string-katakana-to-hiragana/k nil
        (lambda (_result) (ert-fail "should not succeed for nil input"))
        (lambda () (setq failed t)))
      (should failed))))

;;;; zenkaku to hankaku conversion

(nskk-describe "zenkaku to hankaku conversion"

  (nskk-it "converts zenkaku katakana ア to hankaku ｱ"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (nskk-kana-zenkaku-to-hankaku/k "ア"
        (lambda (result) (should (string= result "ｱ")))
        (lambda () (ert-fail "conversion failed unexpectedly")))))

  (nskk-it "converts zenkaku string アイウ to hankaku ｱｲｳ"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (nskk-kana-zenkaku-to-hankaku/k "アイウ"
        (lambda (result) (should (string= result "ｱｲｳ")))
        (lambda () (ert-fail "conversion failed unexpectedly")))))

  (nskk-it "passes through unrecognized characters unchanged"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      ;; ASCII letters are not in the zenkaku katakana table; pass through.
      (nskk-kana-zenkaku-to-hankaku/k "abc"
        (lambda (result) (should (string= result "abc")))
        (lambda () (ert-fail "conversion failed unexpectedly")))))

  (nskk-it "always succeeds for empty string"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (nskk-kana-zenkaku-to-hankaku/k ""
        (lambda (result) (should (string= result "")))
        (lambda () (ert-fail "should always succeed"))))))

;;;; hankaku to zenkaku conversion

(nskk-describe "hankaku to zenkaku conversion"

  (nskk-it "converts hankaku ｱ to zenkaku ア"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (nskk-kana-hankaku-to-zenkaku/k "ｱ"
        (lambda (result) (should (string= result "ア")))
        (lambda () (ert-fail "conversion failed unexpectedly")))))

  (nskk-it "converts hankaku string ｱｲｳ to zenkaku アイウ"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (nskk-kana-hankaku-to-zenkaku/k "ｱｲｳ"
        (lambda (result) (should (string= result "アイウ")))
        (lambda () (ert-fail "conversion failed unexpectedly")))))

  (nskk-it "converts dakuten combination ｶﾞ to voiced ガ"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (nskk-kana-hankaku-to-zenkaku/k "ｶﾞ"
        (lambda (result) (should (string= result "ガ")))
        (lambda () (ert-fail "conversion failed unexpectedly")))))

  (nskk-it "always succeeds for empty string"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (nskk-kana-hankaku-to-zenkaku/k ""
        (lambda (result) (should (string= result "")))
        (lambda () (ert-fail "should always succeed"))))))

;;;; zenkaku-hankaku roundtrip

(nskk-describe "zenkaku hankaku roundtrip"

  (nskk-it "roundtrips single zenkaku katakana char"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (nskk-kana-zenkaku-to-hankaku/k "ア"
        (lambda (hankaku)
          (nskk-kana-hankaku-to-zenkaku/k hankaku
            (lambda (back) (should (string= back "ア")))
            (lambda () (ert-fail "hankaku-to-zenkaku failed"))))
        (lambda () (ert-fail "zenkaku-to-hankaku failed")))))

  (nskk-it "roundtrips five-char zenkaku katakana string"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (nskk-kana-zenkaku-to-hankaku/k "アイウエオ"
        (lambda (hankaku)
          (nskk-kana-hankaku-to-zenkaku/k hankaku
            (lambda (back) (should (string= back "アイウエオ")))
            (lambda () (ert-fail "hankaku-to-zenkaku failed"))))
        (lambda () (ert-fail "zenkaku-to-hankaku failed")))))

  (nskk-it "roundtrips single hankaku katakana char"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (nskk-kana-hankaku-to-zenkaku/k "ｱ"
        (lambda (zenkaku)
          (nskk-kana-zenkaku-to-hankaku/k zenkaku
            (lambda (back) (should (string= back "ｱ")))
            (lambda () (ert-fail "zenkaku-to-hankaku failed"))))
        (lambda () (ert-fail "hankaku-to-zenkaku failed"))))))

;;;; kana-initialize idempotency

(nskk-describe "kana-initialize idempotency"

  (nskk-it "calling nskk-kana-initialize twice does not error"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (should-not (condition-case nil
                      (progn (nskk-kana-initialize) nil)
                    (error t)))))

  (nskk-it "conversion works correctly after double initialization"
    (nskk-prolog-test-with-isolated-db
      (nskk-kana-initialize)
      (nskk-kana-initialize)
      (nskk-kana-zenkaku-to-hankaku/k "ア"
        (lambda (result) (should (string= result "ｱ")))
        (lambda () (ert-fail "conversion failed after double initialize"))))))

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

;;;; PBT: output is always a string

(nskk-property-test kana-string-hiragana-to-katakana-always-string
  ((h hiragana-string))
  (let ((result-is-string nil))
    (nskk-kana-string-hiragana-to-katakana/k h
      (lambda (result)
        (should (stringp result))
        (setq result-is-string t))
      (lambda () (ert-fail "should always succeed for string input")))
    result-is-string)
  15)

;;;; PBT: katakana output length equals hiragana input length

(nskk-property-test kana-string-hiragana-to-katakana-preserves-length
  ((h hiragana-string))
  (let ((length-ok nil))
    (nskk-kana-string-hiragana-to-katakana/k h
      (lambda (kata)
        ;; Each hiragana char maps 1:1 to a katakana char (same codepoint count).
        (should (= (length kata) (length h)))
        (setq length-ok t))
      (lambda () (ert-fail "should always succeed for string input")))
    length-ok)
  15)

(provide 'nskk-kana-integration-test)

;;; nskk-kana-integration-test.el ends here
