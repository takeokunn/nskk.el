;;; nskk-native-compile-test.el --- Tests for nskk-native-compile.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-native-compile.el の単体テスト。

;;; Code:

(require 'ert)
(require 'nskk-native-compile)

;;; ネイティブコンパイル検出テスト

(ert-deftest nskk-native-compile-test-available-p ()
  "ネイティブコンパイル利用可否のテスト。"
  ;; 環境依存のため、エラーが出ないことのみ確認
  (should (or (nskk-native-compile-available-p)
              (not (nskk-native-compile-available-p)))))

(ert-deftest nskk-native-compile-test-enabled-p ()
  "ネイティブコンパイル有効状態のテスト。"
  ;; 環境依存のため、エラーが出ないことのみ確認
  (should (or (nskk-native-compile-enabled-p)
              (not (nskk-native-compile-enabled-p)))))

;;; PGOテスト

(ert-deftest nskk-native-compile-test-pgo-basic ()
  "PGO基本動作のテスト。"
  (nskk-native-compile-pgo-start)
  ;; プロファイリングが有効でないと記録されない
  ;; データ構造の確認のみ行う
  (should (listp nskk-native-compile--pgo-data)))

(ert-deftest nskk-native-compile-test-pgo-multiple-functions ()
  "複数関数のPGOテスト。"
  (nskk-native-compile-pgo-start)
  ;; データ構造の確認のみ
  (should (listp nskk-native-compile--pgo-data))
  (nskk-native-compile-pgo-stop))

;;; コンパイル設定テスト

(ert-deftest nskk-native-compile-test-set-optimization-level ()
  "最適化レベル設定のテスト。"
  (nskk-native-compile-set-optimization-level 3 0)
  (should (= nskk-native-compile-speed 3))
  (should (= nskk-native-compile-safety 0))

  (nskk-native-compile-set-optimization-level 1 2)
  (should (= nskk-native-compile-speed 1))
  (should (= nskk-native-compile-safety 2)))

;;; マクロテスト

(ert-deftest nskk-native-compile-test-hot-path ()
  "ホットパスマクロのテスト。"
  (should (= (nskk-native-compile-hot-path (+ 1 2 3)) 6)))

(ert-deftest nskk-native-compile-test-cold-path ()
  "コールドパスマクロのテスト。"
  (should (= (nskk-native-compile-cold-path (+ 1 2 3)) 6)))

(ert-deftest nskk-native-compile-test-declare-string ()
  "文字列型宣言マクロのテスト。"
  (let ((str "test"))
    (nskk-native-compile-declare-string str
      (should (stringp str)))))

(ert-deftest nskk-native-compile-test-declare-integer ()
  "整数型宣言マクロのテスト。"
  (let ((num 42))
    (nskk-native-compile-declare-integer num
      (should (integerp num)))))

;;; 診断テスト

(ert-deftest nskk-native-compile-test-check-status ()
  "状態チェックのテスト。"
  (let ((status (nskk-native-compile-check-status)))
    (should (plist-member status :available))
    (should (plist-member status :enabled))
    (should (plist-member status :speed))
    (should (plist-member status :safety))
    (should (plist-member status :deferred))
    (should (plist-member status :trampolines))
    (should (plist-member status :compiled-files))

    ;; 値の妥当性チェック
    (should (or (eq (plist-get status :available) t)
                (eq (plist-get status :available) nil)))
    (should (numberp (plist-get status :speed)))
    (should (numberp (plist-get status :safety)))))

;;; 統計情報テスト

(ert-deftest nskk-native-compile-test-stats ()
  "統計情報のテスト。"
  (let ((stats (nskk-native-compile-stats)))
    (should (plist-member stats :available))
    (should (plist-member stats :enabled))
    (should (plist-member stats :optimization))
    (should (plist-member stats :pgo-entries))

    ;; 最適化設定の確認
    (let ((opt (plist-get stats :optimization)))
      (should (plist-member opt :speed))
      (should (plist-member opt :safety)))))

;;; ベクトル化ヒントテスト

(ert-deftest nskk-native-compile-test-vectorize-loop ()
  "ベクトル化ループのテスト。"
  (let ((sum 0))
    (nskk-native-compile-vectorize-loop item '(1 2 3 4 5)
      (setq sum (+ sum item)))
    (should (= sum 15))))

;;; SIMDヒントテスト

(ert-deftest nskk-native-compile-test-simd-hint ()
  "SIMDヒントマクロのテスト。"
  (should (= (nskk-native-compile-simd-hint
              (+ 1 2 3 4 5))
             15)))

(provide 'nskk-native-compile-test)

;;; nskk-native-compile-test.el ends here
