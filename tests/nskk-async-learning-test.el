;;; nskk-async-learning-test.el --- Tests for nskk-async-learning -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:

;; nskk-async-learning.el のテスト

;;; Code:

(require 'ert)
(require 'nskk-async-learning)

;;; 初期化テスト

(ert-deftest nskk-async-learning-test-initialize ()
  "非同期学習初期化のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)
  (should nskk-async-learning--state)
  (should (nskk-async-learning-state-pool nskk-async-learning--state))
  (nskk-async-learning-shutdown))

(ert-deftest nskk-async-learning-test-shutdown ()
  "非同期学習シャットダウンのテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)
  (nskk-async-learning-shutdown)
  (should-not nskk-async-learning--state))

;;; 学習記録テスト

(ert-deftest nskk-async-learning-test-record ()
  "学習記録のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)

  ;; 学習記録
  (nskk-async-learning-record "かんじ" "漢字")

  ;; キューに追加されているか確認
  (should (> (length (nskk-async-learning-state-queue nskk-async-learning--state)) 0))

  (nskk-async-learning-shutdown))

(ert-deftest nskk-async-learning-test-record-with-context ()
  "文脈付き学習記録のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)

  ;; 文脈付き学習記録
  (nskk-async-learning-record "かんじ" "漢字" "これは漢字です")

  (should (> (length (nskk-async-learning-state-queue nskk-async-learning--state)) 0))

  (nskk-async-learning-shutdown))

;;; 頻度更新テスト

(ert-deftest nskk-async-learning-test-frequency-update ()
  "頻度更新のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)

  ;; 学習記録
  (nskk-async-learning-record "かんじ" "漢字")
  (nskk-async-learning-record "かんじ" "漢字")
  (nskk-async-learning-record "かんじ" "漢字")

  ;; バッチ処理待機
  (sleep-for 2.0)

  ;; 頻度確認
  (let ((freq (nskk-async-learning-get-frequency "かんじ" "漢字")))
    (should (= freq 3)))

  (nskk-async-learning-shutdown))

(ert-deftest nskk-async-learning-test-frequency-different-candidates ()
  "異なる候補の頻度更新テスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)

  ;; 異なる候補を学習
  (nskk-async-learning-record "かんじ" "漢字")
  (nskk-async-learning-record "かんじ" "感じ")
  (nskk-async-learning-record "かんじ" "漢字")

  ;; バッチ処理待機
  (sleep-for 2.0)

  ;; 頻度確認
  (should (= (nskk-async-learning-get-frequency "かんじ" "漢字") 2))
  (should (= (nskk-async-learning-get-frequency "かんじ" "感じ") 1))

  (nskk-async-learning-shutdown))

;;; 文脈更新テスト

(ert-deftest nskk-async-learning-test-context-update ()
  "文脈更新のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)

  ;; 文脈付き学習
  (nskk-async-learning-record "かんじ" "漢字" "これは漢字です")
  (nskk-async-learning-record "かんじ" "漢字" "漢字を使う")

  ;; バッチ処理待機
  (sleep-for 2.0)

  ;; 文脈確認
  (let ((contexts (nskk-async-learning-get-contexts "かんじ" "漢字")))
    (should (= (length contexts) 2)))

  (nskk-async-learning-shutdown))

;;; キュー操作テスト

(ert-deftest nskk-async-learning-test-queue-operations ()
  "キュー操作のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)

  ;; 複数記録
  (dotimes (i 5)
    (nskk-async-learning-record (format "key%d" i) (format "value%d" i)))

  ;; キューサイズ確認
  (should (= (length (nskk-async-learning-state-queue nskk-async-learning--state)) 5))

  (nskk-async-learning-shutdown))

(ert-deftest nskk-async-learning-test-max-queue-size ()
  "最大キューサイズのテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((nskk-async-learning-max-queue-size 10))
    (nskk-async-learning-initialize)

    ;; 最大サイズを超えて追加
    (dotimes (i 15)
      (nskk-async-learning-record (format "key%d" i) (format "value%d" i)))

    ;; 最大サイズに制限されている
    (should (<= (length (nskk-async-learning-state-queue nskk-async-learning--state)) 10))

    (nskk-async-learning-shutdown)))

;;; バッチ処理テスト

(ert-deftest nskk-async-learning-test-batch-processing ()
  "バッチ処理のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((nskk-async-learning-batch-size 5)
        (nskk-async-learning-batch-interval 0.5))
    (nskk-async-learning-initialize)

    ;; 10件記録
    (dotimes (i 10)
      (nskk-async-learning-record (format "key%d" i) (format "value%d" i)))

    ;; バッチ処理待機
    (sleep-for 1.5)

    ;; キューが処理されている
    (should (< (length (nskk-async-learning-state-queue nskk-async-learning--state)) 10))

    (nskk-async-learning-shutdown)))

;;; UIブロッキングテスト

(ert-deftest nskk-async-learning-test-non-blocking ()
  "UIブロッキングなしのテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)

  (let ((start (current-time)))
    ;; 大量の学習記録
    (dotimes (i 100)
      (nskk-async-learning-record (format "key%d" i) (format "value%d" i)))

    (let ((elapsed (float-time (time-subtract (current-time) start))))
      ;; 記録自体は即座に完了（ブロッキングなし）
      (should (< elapsed 0.1))))  ; 100ms以内

  (nskk-async-learning-shutdown))

;;; フラッシュテスト

(ert-deftest nskk-async-learning-test-flush-queue ()
  "キューフラッシュのテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)

  ;; 記録追加
  (dotimes (i 10)
    (nskk-async-learning-record (format "key%d" i) (format "value%d" i)))

  ;; フラッシュ
  (nskk-async-learning--flush-queue)

  ;; キューが空
  (should (= (length (nskk-async-learning-state-queue nskk-async-learning--state)) 0))

  (nskk-async-learning-shutdown))

;;; 統計情報テスト

(ert-deftest nskk-async-learning-test-statistics ()
  "統計情報表示のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)
  (should-not (nskk-async-learning-statistics))
  (nskk-async-learning-shutdown))

;;; ヘルパー関数テスト

(ert-deftest nskk-async-learning-test-wait-for-queue-empty ()
  "キュー空待機のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)

  ;; 少量の記録
  (dotimes (i 5)
    (nskk-async-learning-record (format "key%d" i) (format "value%d" i)))

  ;; キュー空待機
  (let ((result (nskk-async-learning-wait-for-queue-empty 5.0)))
    (should result))

  (nskk-async-learning-shutdown))

;;; パフォーマンステスト

(ert-deftest nskk-async-learning-test-performance ()
  "パフォーマンステスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (nskk-async-learning-initialize)

  (let ((start (current-time))
        (count 1000))
    ;; 大量記録
    (dotimes (i count)
      (nskk-async-learning-record (format "key%d" i) (format "value%d" i)))

    (let ((elapsed (float-time (time-subtract (current-time) start))))
      ;; 記録自体は高速（UIブロックなし）
      (should (< elapsed 1.0))  ; 1秒以内

      ;; キュー空待機
      (nskk-async-learning-wait-for-queue-empty 10.0)))

  (nskk-async-learning-shutdown))

(provide 'nskk-async-learning-test)

;;; nskk-async-learning-test.el ends here
