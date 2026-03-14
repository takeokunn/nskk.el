;;; nskk-initialization-integration-test.el --- Module initialization chain integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests verifying the module initialization chain.
;;
;; The initialization sequence in nskk--enable is:
;;   nskk-state-initialize-prolog   → mode-properties/5, valid-mode/1, …
;;   nskk-kana-initialize           → kana-hiragana/1, kana-katakana/1, …
;;   nskk-henkan-initialize         → core-search-type/2, converting-phase/1, …
;;   nskk-input-initialize          → input routing rules
;;   nskk-converter-initialize      → romaji-to-kana table
;;
;; Each init function registers Prolog predicates consumed by other modules.
;; Tests verify:
;; 1. Post-initialization Prolog predicates are accessible cross-module.
;; 2. Each init function is idempotent (safe to call multiple times).
;; 3. After all inits, the integrated system processes input correctly.
;;
;; Note: nskk-test-framework.el already calls all five init functions at
;; load time.  These tests verify the post-init database state and
;; cross-module interactions rather than bootstrapping from scratch.

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
    ;; あ = U+3042, within hiragana block U+3040–U+309F
    (should (nskk-prolog-holds-p `(kana-hiragana ,?あ))))

  (nskk-it "kana-hiragana/1 does not hold for katakana codepoint"
    ;; ア = U+30A2, in katakana block — should not match hiragana rule
    (should-not (nskk-prolog-holds-p `(kana-hiragana ,?ア))))

  (nskk-it "kana-katakana/1 Prolog rule holds for katakana codepoint"
    ;; ア = U+30A2, within katakana block U+30A0–U+30FF
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
    ;; Exercises: nskk-converter-initialize (romaji table) +
    ;;            nskk-state-initialize-prolog (mode-properties) +
    ;;            nskk-input-initialize (routing rules)
    (nskk-integration-with-session 'hiragana
      (nskk--integration-type-char ?a)
      (should (string= "あ" (buffer-string)))))

  (nskk-it "initialized system processes katakana input correctly"
    (nskk-integration-with-session 'katakana
      (nskk--integration-type-char ?a)
      (should (string= "ア" (buffer-string)))))

  (nskk-it "state-init and henkan-init Prolog predicates coexist"
    ;; valid-mode (state-init) and converting-phase (henkan-init) both
    ;; live in the same Prolog database and must not conflict.
    (should (nskk-prolog-holds-p '(valid-mode hiragana)))
    (should (nskk-prolog-holds-p '(converting-phase active))))

  (nskk-it "kana-init and henkan-init predicates coexist"
    (should (nskk-prolog-holds-p `(kana-hiragana ,?あ)))
    (should (nskk-prolog-holds-p '(converting-phase list)))))

;;;; Initialization idempotency (PBT)

(nskk-describe "initialization idempotency (PBT)"

  ;; Property: calling all 5 init functions N times (N between 2 and 5) leaves
  ;; the Prolog DB in the same queryable state as calling them once.
  ;;
  ;; After N calls we verify that a representative set of predicates — one from
  ;; each init function — still resolves to the expected values.  All Prolog
  ;; state is isolated so the repeated calls run against a fresh DB on each run.
  ;;
  ;; Predicates verified:
  ;;   valid-mode/1          (nskk-state-initialize-prolog)
  ;;   kana-hiragana/1       (nskk-kana-initialize)
  ;;   converting-phase/1    (nskk-henkan-initialize)
  ;;   core-search-type/2    (nskk-henkan-initialize)
  (nskk-property-test-seeded n-times-init-leaves-db-consistent
    ()
    (let ((n (+ 2 (random 4))))     ; N in [2, 5]
      (nskk-prolog-test-with-isolated-db
        ;; Call all 5 init functions N times; idempotency guards (the
        ;; *-initialized flags) are reset by nskk-prolog-test-with-isolated-db
        ;; at the start of each iteration, so the first call in the loop
        ;; actually populates the fresh DB and subsequent calls are no-ops.
        (dotimes (_ n)
          (nskk-state-initialize-prolog)
          (nskk-kana-initialize)
          (nskk-henkan-initialize)
          (nskk-input-initialize)
          (nskk-converter-initialize))
        ;; Verify predicates from nskk-state-initialize-prolog
        (and
         (nskk-prolog-holds-p '(valid-mode hiragana))
         (nskk-prolog-holds-p '(valid-mode katakana))
         (nskk-prolog-holds-p '(valid-mode ascii))
         ;; Verify predicate from nskk-kana-initialize
         (nskk-prolog-holds-p `(kana-hiragana ,?あ))
         (nskk-prolog-holds-p `(kana-katakana ,?ア))
         ;; Verify predicates from nskk-henkan-initialize
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
