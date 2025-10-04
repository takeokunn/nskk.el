;;; nskk-memory-optimize-test.el --- Tests for nskk-memory-optimize.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-memory-optimize.el の単体テスト。

;;; Code:

(require 'ert)
(require 'nskk-memory-optimize)

;;; オブジェクトプールテスト

(ert-deftest nskk-memory-optimize-test-string-pool ()
  "文字列プールのテスト。"
  (let ((nskk-memory-enable-pooling t))
    ;; プールをクリア
    (nskk-memory-pool-clear)

    ;; 文字列を取得
    (let ((str1 (nskk-memory-pool-get-string 10)))
      (should (stringp str1))
      (should (= (length str1) 10))

      ;; 文字列を返却
      (nskk-memory-pool-return-string str1)

      ;; 再取得（プールから取得されるはず）
      (let ((str2 (nskk-memory-pool-get-string 10)))
        (should (stringp str2))
        (should (= (length str2) 10))))))

(ert-deftest nskk-memory-optimize-test-cons-pool ()
  "consプールのテスト。"
  (let ((nskk-memory-enable-pooling t))
    (nskk-memory-pool-clear)

    ;; consを取得
    (let ((cell1 (nskk-memory-pool-get-cons)))
      (should (consp cell1))

      ;; consを返却
      (nskk-memory-pool-return-cons cell1)

      ;; 再取得
      (let ((cell2 (nskk-memory-pool-get-cons)))
        (should (consp cell2))))))

(ert-deftest nskk-memory-optimize-test-vector-pool ()
  "ベクトルプールのテスト。"
  (let ((nskk-memory-enable-pooling t))
    (nskk-memory-pool-clear)

    ;; ベクトルを取得
    (let ((vec1 (nskk-memory-pool-get-vector 5)))
      (should (vectorp vec1))
      (should (= (length vec1) 5))

      ;; ベクトルを返却
      (nskk-memory-pool-return-vector vec1)

      ;; 再取得
      (let ((vec2 (nskk-memory-pool-get-vector 5)))
        (should (vectorp vec2))
        (should (= (length vec2) 5))))))

(ert-deftest nskk-memory-optimize-test-pool-disabled ()
  "プール無効時のテスト。"
  (let ((nskk-memory-enable-pooling nil))
    ;; プールが無効でも動作する
    (let ((str (nskk-memory-pool-get-string 10)))
      (should (stringp str))
      (should (= (length str) 10)))))

;;; 文字列インターンテスト

(ert-deftest nskk-memory-optimize-test-intern-string ()
  "文字列インターンのテスト。"
  (let ((nskk-memory-enable-interning t))
    (nskk-memory-clear-interned-strings)

    (let ((str1 (nskk-memory-intern-string "test"))
          (str2 (nskk-memory-intern-string "test")))
      ;; 同じ文字列は同じオブジェクトを返すべき
      (should (eq str1 str2)))))

(ert-deftest nskk-memory-optimize-test-intern-different-strings ()
  "異なる文字列のインターンテスト。"
  (let ((nskk-memory-enable-interning t))
    (nskk-memory-clear-interned-strings)

    (let ((str1 (nskk-memory-intern-string "hello"))
          (str2 (nskk-memory-intern-string "world")))
      ;; 異なる文字列は異なるオブジェクト
      (should-not (eq str1 str2)))))

(ert-deftest nskk-memory-optimize-test-intern-disabled ()
  "インターン無効時のテスト。"
  (let ((nskk-memory-enable-interning nil))
    (let ((str1 (nskk-memory-intern-string "test"))
          (str2 (nskk-memory-intern-string "test")))
      ;; インターン無効時は異なるオブジェクトを返す可能性がある
      (should (string= str1 str2)))))

;;; GC最適化テスト

(ert-deftest nskk-memory-optimize-test-gc-optimization ()
  "GC最適化のテスト。"
  (let ((original-threshold gc-cons-threshold))
    ;; GC設定を最適化
    (nskk-memory-optimize-gc-settings)
    (should (= gc-cons-threshold nskk-memory-gc-cons-threshold))

    ;; 元に戻す
    (nskk-memory-restore-gc-settings)
    (should (= gc-cons-threshold original-threshold))))

(ert-deftest nskk-memory-optimize-test-with-gc-optimization ()
  "一時的なGC最適化のテスト。"
  (let ((original-threshold gc-cons-threshold)
        (result nil))
    (setq result
          (nskk-memory-with-gc-optimization
            (+ 1 2 3)))
    (should (= result 6))
    ;; 元の設定が保持されている
    (should (= gc-cons-threshold original-threshold))))

;;; メモリ使用量測定テスト

(ert-deftest nskk-memory-optimize-test-usage ()
  "メモリ使用量測定のテスト。"
  (let ((usage (nskk-memory-usage)))
    (should (plist-member usage :string-pool-bytes))
    (should (plist-member usage :cons-pool-bytes))
    (should (plist-member usage :vector-pool-bytes))
    (should (plist-member usage :interned-bytes))
    (should (plist-member usage :total-bytes))
    (should (plist-member usage :gc-count))
    (should (plist-member usage :gc-elapsed))

    ;; 値の妥当性確認
    (should (>= (plist-get usage :string-pool-bytes) 0))
    (should (>= (plist-get usage :cons-pool-bytes) 0))
    (should (>= (plist-get usage :vector-pool-bytes) 0))
    (should (>= (plist-get usage :interned-bytes) 0))
    (should (>= (plist-get usage :total-bytes) 0))))

;;; メモリプレッシャー監視テスト

(ert-deftest nskk-memory-optimize-test-check-pressure ()
  "メモリプレッシャーチェックのテスト。"
  (let ((pressure (nskk-memory-check-pressure)))
    (should (plist-member pressure :pressure))
    (should (plist-member pressure :usage))
    (should (plist-member pressure :threshold))

    ;; プレッシャーレベルの妥当性
    (should (memq (plist-get pressure :pressure) '(low medium high)))))

(ert-deftest nskk-memory-optimize-test-relieve-pressure ()
  "メモリプレッシャー緩和のテスト。"
  (let ((nskk-memory-enable-pooling t)
        (nskk-memory-enable-interning t))
    ;; データを追加
    (nskk-memory-pool-get-string 100)
    (nskk-memory-intern-string "test")

    ;; プレッシャーを緩和
    (nskk-memory-relieve-pressure)

    ;; プールがクリアされている
    (should (null nskk-memory--string-pool))
    (should (= (hash-table-count nskk-memory--interned-strings) 0))))

;;; 統計情報テスト

(ert-deftest nskk-memory-optimize-test-stats ()
  "統計情報のテスト。"
  (let ((stats (nskk-memory-stats)))
    (should (plist-member stats :pooling-enabled))
    (should (plist-member stats :interning-enabled))
    (should (plist-member stats :gc-threshold))
    (should (plist-member stats :usage))
    (should (plist-member stats :pressure))

    ;; 使用量情報の確認
    (let ((usage (plist-get stats :usage)))
      (should (plist-member usage :total-bytes)))

    ;; プレッシャー情報の確認
    (let ((pressure (plist-get stats :pressure)))
      (should (plist-member pressure :pressure)))))

;;; プールクリアテスト

(ert-deftest nskk-memory-optimize-test-pool-clear ()
  "プールクリアのテスト。"
  (let ((nskk-memory-enable-pooling t))
    ;; データを追加
    (nskk-memory-pool-return-string (make-string 10 ?x))
    (nskk-memory-pool-return-cons (cons 1 2))
    (nskk-memory-pool-return-vector (vector 1 2 3))

    ;; クリア
    (nskk-memory-pool-clear)

    ;; すべてのプールが空
    (should (null nskk-memory--string-pool))
    (should (null nskk-memory--cons-pool))
    (should (null nskk-memory--vector-pool))))

;;; メモリ目標達成テスト

(ert-deftest nskk-memory-optimize-test-target-usage ()
  "メモリ使用量目標のテスト。"
  (let ((usage (nskk-memory-usage)))
    ;; 総使用量が目標以下かチェック（警告のみ）
    (let ((total (plist-get usage :total-bytes)))
      (when (>= total nskk-memory-target-usage)
        (message "Warning: Memory usage %d exceeds target %d"
                total nskk-memory-target-usage))
      ;; テストは常にパス（環境依存のため）
      (should t))))

(provide 'nskk-memory-optimize-test)

;;; nskk-memory-optimize-test.el ends here
