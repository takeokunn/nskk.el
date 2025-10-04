;;; nskk-async-candidates-test.el --- Tests for nskk-async-candidates -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-async-candidates.el

;;; Code:

(require 'ert)
(require 'nskk-async-candidates)

;;; 非同期候補表示テスト

(ert-deftest nskk-async-candidates-test-show-async-small ()
  "小規模候補リストの非同期表示テスト。"
  (let ((candidates '("候補1" "候補2" "候補3"))
        (displayed-count nil)
        (nskk-async-candidates-batch-size 5))

    ;; 候補数がバッチサイズ以下の場合は一括表示
    (nskk-async-candidates-show-async
     candidates
     (lambda (count) (setq displayed-count count)))

    ;; 一括表示の場合は即座に完了
    (should (= displayed-count 3))
    (should-not (nskk-async-candidates-running-p))))

(ert-deftest nskk-async-candidates-test-show-async-large ()
  "大規模候補リストの非同期表示テスト。"
  (let ((candidates (cl-loop for i from 1 to 20 collect (format "候補%d" i)))
        (displayed-count nil)
        (nskk-async-candidates-batch-size 5)
        (nskk-async-candidates-delay 0.001))

    ;; 段階的表示開始
    (nskk-async-candidates-show-async
     candidates
     (lambda (count) (setq displayed-count count)))

    ;; 実行中フラグ確認
    (should (nskk-async-candidates-running-p))

    ;; 進捗確認
    (let ((progress (nskk-async-candidates-progress)))
      (should progress)
      (should (plist-get progress :total))
      (should (= (plist-get progress :total) 20)))

    ;; 完了まで待機（最大1秒）
    (let ((max-wait 1.0)
          (start-time (float-time)))
      (while (and (nskk-async-candidates-running-p)
                  (< (- (float-time) start-time) max-wait))
        (sleep-for 0.01)))

    ;; 完了確認（非同期処理が完了しているはず）
    (should-not (nskk-async-candidates-running-p))
    ;; コールバックが呼ばれていることを確認
    (should displayed-count)
    (should (numberp displayed-count))
    (should (= displayed-count 20))))

(ert-deftest nskk-async-candidates-test-cancel ()
  "キャンセル処理テスト。"
  (let ((candidates (cl-loop for i from 1 to 50 collect (format "候補%d" i)))
        (nskk-async-candidates-batch-size 5)
        (nskk-async-candidates-delay 0.01))

    ;; 表示開始
    (nskk-async-candidates-show-async candidates)

    ;; 実行中確認
    (should (nskk-async-candidates-running-p))

    ;; キャンセル
    (nskk-async-candidates-cancel)

    ;; キャンセル確認
    (should-not (nskk-async-candidates-running-p))))

(ert-deftest nskk-async-candidates-test-progress ()
  "進捗取得テスト。"
  (let ((candidates (cl-loop for i from 1 to 10 collect (format "候補%d" i)))
        (nskk-async-candidates-batch-size 3)
        (nskk-async-candidates-delay 0.01))

    ;; 表示開始
    (nskk-async-candidates-show-async candidates)

    ;; 進捗取得
    (let ((progress (nskk-async-candidates-progress)))
      (should progress)
      (should (numberp (plist-get progress :displayed)))
      (should (numberp (plist-get progress :total)))
      (should (numberp (plist-get progress :percentage)))
      (should (= (plist-get progress :total) 10)))

    ;; クリーンアップ
    (nskk-async-candidates-cancel)))

(ert-deftest nskk-async-candidates-test-stats ()
  "統計情報取得テスト。"
  (let ((candidates (cl-loop for i from 1 to 10 collect (format "候補%d" i)))
        (nskk-async-candidates-batch-size 3))

    ;; 表示開始
    (nskk-async-candidates-show-async candidates)

    ;; 統計取得
    (let ((stats (nskk-async-candidates-stats)))
      (should (plist-get stats :running))
      (should (plist-get stats :progress))
      (should (numberp (plist-get stats :elapsed))))

    ;; クリーンアップ
    (nskk-async-candidates-cancel)))

(ert-deftest nskk-async-candidates-test-presets ()
  "プリセット設定テスト。"
  ;; Fast preset
  (nskk-async-candidates-preset-fast)
  (should (= nskk-async-candidates-batch-size 10))
  (should (= nskk-async-candidates-delay 0.0005))

  ;; Smooth preset
  (nskk-async-candidates-preset-smooth)
  (should (= nskk-async-candidates-batch-size 5))
  (should (= nskk-async-candidates-delay 0.001))

  ;; Responsive preset
  (nskk-async-candidates-preset-responsive)
  (should (= nskk-async-candidates-batch-size 3))
  (should (= nskk-async-candidates-delay 0.0005)))

;;; パフォーマンステスト

(ert-deftest nskk-async-candidates-test-performance ()
  "UIブロッキング0msの検証。"
  (let ((candidates (cl-loop for i from 1 to 100 collect (format "候補%d" i)))
        (nskk-async-candidates-batch-size 5)
        (nskk-async-candidates-delay 0.001)
        (start-time (float-time)))

    ;; 非同期表示開始
    (nskk-async-candidates-show-async candidates)

    ;; 開始時間の検証（1ms以内に制御が戻る）
    (let ((elapsed (- (float-time) start-time)))
      (should (< elapsed 0.001)))

    ;; クリーンアップ
    (nskk-async-candidates-cancel)))

(provide 'nskk-async-candidates-test)

;;; nskk-async-candidates-test.el ends here
