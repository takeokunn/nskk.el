;;; nskk-background-test.el --- Tests for nskk-background -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-background.el

;;; Code:

(require 'ert)
(require 'nskk-background)

;;; タスク登録・管理テスト

(ert-deftest nskk-background-test-register-task ()
  "タスク登録テスト。"
  (let ((executed nil)
        (task-fn (lambda () (setq executed t))))

    ;; タスク登録
    (let ((task (nskk-background-register-task "test-task" task-fn 60)))
      (should task)
      (should (nskk-background-task-p task))
      (should (string= (nskk-background-task-name task) "test-task"))
      (should (= (nskk-background-task-interval task) 60))
      (should (nskk-background-task-enabled task)))

    ;; 登録解除
    (nskk-background-unregister-task "test-task")
    (should-not (nskk-background--find-task "test-task"))))

(ert-deftest nskk-background-test-find-task ()
  "タスク検索テスト。"
  (nskk-background-register-task "task1" (lambda () nil) 60)
  (nskk-background-register-task "task2" (lambda () nil) 120)

  ;; 検索
  (let ((task1 (nskk-background--find-task "task1"))
        (task2 (nskk-background--find-task "task2"))
        (task3 (nskk-background--find-task "task3")))

    (should task1)
    (should (string= (nskk-background-task-name task1) "task1"))

    (should task2)
    (should (string= (nskk-background-task-name task2) "task2"))

    (should-not task3))

  ;; クリーンアップ
  (nskk-background-unregister-task "task1")
  (nskk-background-unregister-task "task2"))

(ert-deftest nskk-background-test-enable-disable-task ()
  "タスク有効化・無効化テスト。"
  (nskk-background-register-task "test-task" (lambda () nil) 60)

  (let ((task (nskk-background--find-task "test-task")))
    ;; 初期状態は有効
    (should (nskk-background-task-enabled task))

    ;; 無効化
    (nskk-background-disable-task "test-task")
    (should-not (nskk-background-task-enabled task))

    ;; 有効化
    (nskk-background-enable-task "test-task")
    (should (nskk-background-task-enabled task)))

  ;; クリーンアップ
  (nskk-background-unregister-task "test-task"))

;;; タスク実行テスト

(ert-deftest nskk-background-test-execute-task ()
  "タスク実行テスト。"
  (let* ((executed nil)
         (task-fn (lambda () (setq executed t)))
         (task (nskk-background-task--create
                :name "test"
                :function task-fn
                :enabled t)))

    ;; タスク実行
    (nskk-background--execute-task task)

    ;; 実行確認
    (should executed)

    ;; 統計確認
    (should (= (nskk-background-task-run-count task) 1))
    (should (> (nskk-background-task-total-time task) 0))
    (should (nskk-background-task-last-run task))))

(ert-deftest nskk-background-test-execute-task-error ()
  "タスク実行エラーテスト。"
  (let ((task-fn (lambda () (error "Test error"))))

    (let ((task (nskk-background-task--create
                 :name "error-task"
                 :function task-fn
                 :enabled t)))

      ;; エラーが発生してもクラッシュしない
      (should-not (condition-case err
                      (progn
                        (nskk-background--execute-task task)
                        nil)
                    (error t)))

      ;; 統計は更新される
      (should (= (nskk-background-task-run-count task) 1)))))

;;; 統計収集テスト

(ert-deftest nskk-background-test-collect-stats ()
  "統計収集テスト。"
  (let ((stats (nskk-background--collect-stats)))
    (should stats)
    (should (nskk-background-stats-p stats))
    (should (numberp (nskk-background-stats-cpu-usage stats)))
    (should (numberp (nskk-background-stats-memory-usage stats)))
    (should (numberp (nskk-background-stats-gc-count stats)))
    (should (numberp (nskk-background-stats-timestamp stats)))))

(ert-deftest nskk-background-test-task-stats ()
  "タスク統計テスト。"
  (nskk-background-register-task "task1" (lambda () (sleep-for 0.01)) 60)

  ;; タスク実行
  (let ((task (nskk-background--find-task "task1")))
    (nskk-background--execute-task task)
    (nskk-background--execute-task task))

  ;; 統計取得
  (let ((stats (nskk-background-task-stats)))
    (should stats)
    (should (listp stats))

    (let ((task1-stats (cl-find "task1" stats
                               :key (lambda (s) (plist-get s :name))
                               :test #'string=)))
      (should task1-stats)
      (should (= (plist-get task1-stats :run-count) 2))
      (should (> (plist-get task1-stats :total-time) 0))
      (should (> (plist-get task1-stats :avg-time) 0))))

  ;; クリーンアップ
  (nskk-background-unregister-task "task1"))

(ert-deftest nskk-background-test-resource-stats ()
  "リソース統計テスト。"
  ;; 統計履歴を追加
  (push (nskk-background-stats--create
         :cpu-usage 10.0
         :memory-usage 20
         :gc-count 5
         :timestamp (float-time))
        nskk-background--stats-history)

  (push (nskk-background-stats--create
         :cpu-usage 15.0
         :memory-usage 25
         :gc-count 6
         :timestamp (float-time))
        nskk-background--stats-history)

  ;; リソース統計取得
  (let ((stats (nskk-background-resource-stats)))
    (should stats)
    (should (plist-get stats :current))
    (should (numberp (plist-get stats :avg-cpu)))
    (should (numberp (plist-get stats :avg-memory)))
    (should (numberp (plist-get stats :max-cpu)))
    (should (numberp (plist-get stats :max-memory))))

  ;; クリーンアップ
  (setq nskk-background--stats-history nil))

;;; リソース監視テスト

(ert-deftest nskk-background-test-suspend-resume ()
  "タスク一時停止・再開テスト。"
  ;; 初期状態
  (should-not nskk-background--suspended)

  ;; 一時停止
  (nskk-background--suspend-tasks "Test reason")
  (should nskk-background--suspended)

  ;; 再開
  (nskk-background--resume-tasks)
  (should-not nskk-background--suspended))

;;; 開始・停止テスト

(ert-deftest nskk-background-test-start-stop ()
  "バックグラウンドタスク開始・停止テスト。"
  (let ((nskk-background-enable t))
    ;; 開始
    (nskk-background-start)
    (should nskk-background--idle-timer)
    (should nskk-background--resource-monitor-timer)
    (should nskk-background--tasks)

    ;; 停止
    (nskk-background-stop)
    (should-not nskk-background--idle-timer)
    (should-not nskk-background--resource-monitor-timer)
    (should-not nskk-background--tasks)))

;;; パフォーマンステスト

(ert-deftest nskk-background-test-low-overhead ()
  "低オーバーヘッド検証。"
  (let ((task-fn (lambda () (sleep-for 0.001)))
        (nskk-background-enable t))

    (nskk-background-register-task "perf-test" task-fn 60)

    ;; タスク実行
    (let ((task (nskk-background--find-task "perf-test"))
          (start-time (float-time)))

      (dotimes (_ 10)
        (nskk-background--execute-task task))

      ;; 平均実行時間が2ms以下であることを確認
      (let ((avg-time (/ (nskk-background-task-total-time task)
                        (nskk-background-task-run-count task))))
        (should (< avg-time 0.002))))

    ;; クリーンアップ
    (nskk-background-unregister-task "perf-test")))

(ert-deftest nskk-background-test-memory-usage ()
  "メモリ使用量テスト。"
  (let ((initial-stats (nskk-background--collect-stats)))

    ;; 大量のタスクを登録
    (dotimes (i 10)
      (nskk-background-register-task
       (format "task-%d" i)
       (lambda () nil)
       60))

    ;; メモリ使用量の増加が適切な範囲内であることを確認
    (let* ((current-stats (nskk-background--collect-stats))
           (memory-increase (- (nskk-background-stats-memory-usage current-stats)
                             (nskk-background-stats-memory-usage initial-stats))))

      ;; 1MB未満の増加であることを確認
      (should (< memory-increase 1)))

    ;; クリーンアップ
    (dotimes (i 10)
      (nskk-background-unregister-task (format "task-%d" i)))))

(provide 'nskk-background-test)

;;; nskk-background-test.el ends here
