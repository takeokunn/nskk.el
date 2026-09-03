;;; nskk-initialization-integration-test.el --- Module initialization chain integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Module initialization chain integration tests.

;;; Code:

(require 'ert)
(require 'nskk-state)
(require 'nskk-kana)
(require 'nskk-henkan)
(require 'nskk-input)
(require 'nskk-converter)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)

;;;; Post-initialization predicate availability (nskk-state-initialize-prolog)

(nskk-describe "state-initialize-prolog predicates"

  (nskk-it "valid-mode/1 holds for hiragana"
    (should (nskk-prolog-holds-p '(valid-mode hiragana))))

  (nskk-it "valid-mode/1 holds for katakana"
    (should (nskk-prolog-holds-p '(valid-mode katakana))))

  (nskk-it "valid-mode/1 holds for ascii"
    (should (nskk-prolog-holds-p '(valid-mode ascii))))

  (nskk-it "mode-properties/5 display string for hiragana is かな"
    (let ((display (nskk-prolog-query-value
                    '(mode-properties hiragana \?s \?f \?h \?c) '\?s)))
      (should (string= display "かな"))))

  (nskk-it "mode-properties/5 display string for katakana is カナ"
    (let ((display (nskk-prolog-query-value
                    '(mode-properties katakana \?s \?f \?h \?c) '\?s)))
      (should (string= display "カナ"))))

  (nskk-it "mode-properties/5 display string for ascii is SKK"
    (let ((display (nskk-prolog-query-value
                    '(mode-properties ascii \?s \?f \?h \?c) '\?s)))
      (should (string= display "SKK")))))

;;;; Post-initialization predicate availability (nskk-henkan-initialize)

(nskk-describe "henkan-initialize predicates"

  (nskk-it "core-search-type maps :exact to dict-lookup"
    (let ((action (nskk-prolog-query-value
                   '(core-search-type :exact \?a) '\?a)))
      (should (eq action 'dict-lookup))))

  (nskk-it "core-search-type maps :prefix to prefix-search"
    (let ((action (nskk-prolog-query-value
                   '(core-search-type :prefix \?a) '\?a)))
      (should (eq action 'prefix-search))))

  (nskk-it "converting-phase/1 holds for active"
    (should (nskk-prolog-holds-p '(converting-phase active))))

  (nskk-it "converting-phase/1 holds for list"
    (should (nskk-prolog-holds-p '(converting-phase list))))

  (nskk-it "converting-phase/1 holds for registration"
    (should (nskk-prolog-holds-p '(converting-phase registration)))))

;;;; Post-initialization predicate availability (nskk-kana-initialize)

(nskk-describe "kana-initialize predicates"

  (nskk-it "kana-hiragana/1 Prolog rule holds for hiragana codepoint"
    (should (nskk-prolog-holds-p `(kana-hiragana ,?あ))))

  (nskk-it "kana-hiragana/1 does not hold for katakana codepoint"
    (should-not (nskk-prolog-holds-p `(kana-hiragana ,?ア))))

  (nskk-it "kana-katakana/1 Prolog rule holds for katakana codepoint"
    (should (nskk-prolog-holds-p `(kana-katakana ,?ア)))))

;;;; Idempotency of init functions

(nskk-describe "initialization idempotency"

  (nskk-it "nskk-state-initialize-prolog is safe to call multiple times"
    (should-not (condition-case nil
                    (progn (nskk-state-initialize-prolog) nil)
                  (error t))))

  (nskk-it "nskk-kana-initialize is safe to call multiple times"
    (should-not (condition-case nil
                    (progn (nskk-kana-initialize) nil)
                  (error t))))

  (nskk-it "nskk-henkan-initialize is safe to call multiple times"
    (should-not (condition-case nil
                    (progn (nskk-henkan-initialize) nil)
                  (error t))))

  (nskk-it "nskk-input-initialize is safe to call multiple times"
    (should-not (condition-case nil
                    (progn (nskk-input-initialize) nil)
                  (error t))))

  (nskk-it "nskk-converter-initialize is safe to call multiple times"
    (should-not (condition-case nil
                    (progn (nskk-converter-initialize) nil)
                  (error t)))))

;;;; Cross-module integration: full session exercises the init chain

(nskk-describe "cross-module init chain integration"

  (nskk-it "initialized system processes hiragana input via converter and state"
    (nskk-integration-with-session 'hiragana
      (nskk--integration-type-char ?a)
      (should (string= "あ" (buffer-string)))))

  (nskk-it "initialized system processes katakana input correctly"
    (nskk-integration-with-session 'katakana
      (nskk--integration-type-char ?a)
      (should (string= "ア" (buffer-string)))))

  (nskk-it "state-init and henkan-init Prolog predicates coexist"
    (should (nskk-prolog-holds-p '(valid-mode hiragana)))
    (should (nskk-prolog-holds-p '(converting-phase active))))

  (nskk-it "kana-init and henkan-init predicates coexist"
    (should (nskk-prolog-holds-p `(kana-hiragana ,?あ)))
    (should (nskk-prolog-holds-p '(converting-phase list)))))

;;;; Initialization idempotency (PBT)

(nskk-describe "initialization idempotency (PBT)"

  (nskk-property-test-seeded n-times-init-leaves-db-consistent
    ()
    (let ((n (+ 2 (random 4))))     ; N in [2, 5]
      (nskk-prolog-test-with-isolated-db
        (dotimes (_ n)
          (nskk-state-initialize-prolog)
          (nskk-kana-initialize)
          (nskk-henkan-initialize)
          (nskk-input-initialize)
          (nskk-converter-initialize))
        (and
         (nskk-prolog-holds-p '(valid-mode hiragana))
         (nskk-prolog-holds-p '(valid-mode katakana))
         (nskk-prolog-holds-p '(valid-mode ascii))
         (nskk-prolog-holds-p `(kana-hiragana ,?あ))
         (nskk-prolog-holds-p `(kana-katakana ,?ア))
         (nskk-prolog-holds-p '(converting-phase active))
         (nskk-prolog-holds-p '(converting-phase list))
         (nskk-prolog-holds-p '(converting-phase registration))
         (let ((action (nskk-prolog-query-value
                        '(core-search-type :exact \?a) '\?a)))
           (eq action 'dict-lookup)))))
    40
    42))


(provide 'nskk-initialization-integration-test)

;;; nskk-initialization-integration-test.el ends here
