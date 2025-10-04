;;; nskk-test-macros-test.el --- Tests for nskk-test-macros.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-test-macros.el のテストスイート
;; テストマクロ自体をテストする

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;; プロパティベーステストのテスト

(nskk-property-test nskk-test-macros-test-property-basic
  "基本的なプロパティテストの動作を検証"
  :iterations 10
  :tags '(:unit)
  :property (lambda ()
              ;; 常に真となるプロパティ
              (let ((x (random 100)))
                (= x x))))

(nskk-property-test nskk-test-macros-test-property-string-concat
  "文字列連結の結合性を検証"
  :iterations 20
  :tags '(:unit)
  :property (lambda ()
              (let ((a (nskk-test-random-string 3))
                    (b (nskk-test-random-string 3))
                    (c (nskk-test-random-string 3)))
                (equal (concat (concat a b) c)
                       (concat a (concat b c))))))

(nskk-property-test nskk-test-macros-test-property-list-reverse
  "リストreverse の冪等性を検証"
  :iterations 15
  :tags '(:unit)
  :property (lambda ()
              (let ((list (list (random 10) (random 10) (random 10))))
                (equal list (reverse (reverse list))))))

;;; パフォーマンステストのテスト

(nskk-performance-test nskk-test-macros-test-perf-simple
  "シンプルな計算のパフォーマンステスト"
  :max-time 0.001
  :iterations 10
  :warmup 2
  :tags '(:unit)
  (+ 1 1))

(nskk-performance-test nskk-test-macros-test-perf-string-concat
  "文字列連結のパフォーマンステスト"
  :max-time 0.001
  :iterations 10
  :tags '(:unit)
  (concat "a" "b" "c" "d" "e"))

(nskk-performance-test nskk-test-macros-test-perf-list-operations
  "リスト操作のパフォーマンステスト"
  :max-time 0.001
  :iterations 10
  :tags '(:unit)
  (let ((list '(1 2 3 4 5)))
    (reverse (append list list))))

;;; 統合テストのテスト

(nskk-integration-test nskk-test-macros-test-integration-basic
  "基本的な統合テストの動作を検証"
  :setup (setq test-var 42)
  :teardown (setq test-var nil)
  :timeout 1.0
  :tags '(:unit)
  (should (= test-var 42))
  (setq test-var 100)
  (should (= test-var 100)))

(nskk-integration-test nskk-test-macros-test-integration-buffer
  "バッファ操作を含む統合テスト"
  :setup (insert "initial")
  :teardown (erase-buffer)
  :timeout 1.0
  :tags '(:unit)
  (should (equal (buffer-string) "initial"))
  (goto-char (point-max))
  (insert " text")
  (should (equal (buffer-string) "initial text")))

;;; パラメータ化テストのテスト

(nskk-parameterized-test nskk-test-macros-test-param-arithmetic
  "算術演算のパラメータ化テスト"
  ((a . (1 2 3))
   (b . (10 20 30)))
  (should (numberp (+ a b)))
  (should (> (+ a b) a))
  (should (> (+ a b) b)))

(nskk-parameterized-test nskk-test-macros-test-param-string
  "文字列操作のパラメータ化テスト"
  ((str . ("hello" "world" "test")))
  (should (stringp str))
  (should (> (length str) 0)))

;;; ヘルパー関数のテスト

(ert-deftest nskk-test-macros-test-clear-property-failures ()
  "プロパティ失敗ケースのクリアをテスト"
  (push '(:test failure) nskk-test--property-failures)
  (nskk-test-clear-property-failures)
  (should (null nskk-test--property-failures)))

(ert-deftest nskk-test-macros-test-clear-performance-results ()
  "パフォーマンス結果のクリアをテスト"
  (push '(:test result) nskk-test--performance-results)
  (nskk-test-clear-performance-results)
  (should (null nskk-test--performance-results)))

;;; エッジケーステスト

(ert-deftest nskk-test-macros-test-property-no-iterations ()
  "反復回数0のプロパティテストは正常に完了"
  (let ((test-executed nil))
    (eval '(nskk-property-test temp-test
             "Temp test"
             :iterations 0
             :property (lambda ()
                         (setq test-executed t)
                         t)))
    ;; 0回なので実行されない
    (should-not test-executed)))

(nskk-property-test nskk-test-macros-test-property-with-generator
  "ジェネレーター付きプロパティテスト"
  :iterations 5
  :generator (lambda () (random 100))
  :tags '(:unit)
  :property (lambda (n)
              (and (numberp n) (>= n 0) (< n 100))))

;;; 統合シナリオテスト

(ert-deftest nskk-test-macros-test-integration-scenario ()
  "複数のマクロを組み合わせたテスト"
  ;; プロパティテストの実行
  (nskk-test-clear-property-failures)

  ;; パフォーマンステストの実行
  (nskk-test-clear-performance-results)

  ;; テストが正常に実行されることを確認
  (should t))

(provide 'nskk-test-macros-test)

;;; nskk-test-macros-test.el ends here
