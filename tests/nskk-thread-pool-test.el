;;; nskk-thread-pool-test.el --- Tests for nskk-thread-pool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:

;; nskk-thread-pool.el のテスト

;;; Code:

(require 'ert)
(require 'nskk-thread-pool)

;;; 基本機能テスト

(ert-deftest nskk-thread-pool-test-create ()
  "スレッドプール作成のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((pool (nskk-thread-pool-create 4)))
    (should (nskk-thread-pool-p pool))
    (should (= (nskk-thread-pool-size pool) 4))
    (should (= (length (nskk-thread-pool-workers pool)) 4))
    (nskk-thread-pool-shutdown pool)))

(ert-deftest nskk-thread-pool-test-auto-size ()
  "自動サイズ調整のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((pool (nskk-thread-pool-create)))
    (should (nskk-thread-pool-p pool))
    (should (>= (nskk-thread-pool-size pool) 2))
    (should (<= (nskk-thread-pool-size pool) 8))
    (nskk-thread-pool-shutdown pool)))

(ert-deftest nskk-thread-pool-test-cpu-count ()
  "CPU数取得のテスト。"
  (let ((count (nskk-thread-pool--get-cpu-count)))
    (should (integerp count))
    (should (>= count 1))))

;;; タスク実行テスト

(ert-deftest nskk-thread-pool-test-simple-task ()
  "シンプルなタスク実行のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((pool (nskk-thread-pool-create 2))
        (result nil))
    (nskk-thread-submit pool
                        (lambda () (+ 1 2))
                        (lambda (r) (setq result r)))
    (sleep-for 0.5)
    (should (= result 3))
    (nskk-thread-pool-shutdown pool)))

(ert-deftest nskk-thread-pool-test-multiple-tasks ()
  "複数タスク実行のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((pool (nskk-thread-pool-create 4))
        (results nil)
        (mutex (make-mutex)))
    (dotimes (i 10)
      (nskk-thread-submit pool
                          (lambda () (* i i))
                          (lambda (r)
                            (with-mutex mutex
                              (push r results)))))
    (sleep-for 1.0)
    (should (= (length results) 10))
    (nskk-thread-pool-shutdown pool)))

(ert-deftest nskk-thread-pool-test-callback ()
  "コールバック機能のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((pool (nskk-thread-pool-create 2))
        (callback-called nil))
    (nskk-thread-submit pool
                        (lambda () 42)
                        (lambda (result)
                          (setq callback-called result)))
    (sleep-for 0.5)
    (should (= callback-called 42))
    (nskk-thread-pool-shutdown pool)))

;;; エラーハンドリングテスト

(ert-deftest nskk-thread-pool-test-error-handling ()
  "エラーハンドリングのテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((pool (nskk-thread-pool-create 2))
        (error-caught nil))
    (nskk-thread-submit pool
                        (lambda () (error "Test error"))
                        nil
                        (lambda (err)
                          (setq error-caught err)))
    (sleep-for 0.5)
    (should error-caught)
    (nskk-thread-pool-shutdown pool)))

;;; キュー操作テスト

(ert-deftest nskk-thread-pool-test-queue-operations ()
  "キュー操作のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((pool (nskk-thread-pool-create 2)))
    ;; タスクをエンキュー
    (dotimes (i 5)
      (nskk-thread-submit pool
                          (lambda () (sleep-for 0.1))))

    ;; キューに要素があることを確認
    (should (> (length (nskk-thread-pool-task-queue pool)) 0))

    (nskk-thread-pool-shutdown pool)))

(ert-deftest nskk-thread-pool-test-max-queue-size ()
  "最大キューサイズのテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((pool (nskk-thread-pool-create 1))
        (nskk-thread-pool-max-queue-size 5))
    ;; 最大サイズを超えてエンキューを試みる
    (dotimes (i 5)
      (nskk-thread-submit pool
                          (lambda () (sleep-for 1.0))))

    ;; 次のタスクはエラーになるべき
    (should-error
     (nskk-thread-submit pool
                         (lambda () 1)))

    (nskk-thread-pool-shutdown pool)))

;;; シャットダウンテスト

(ert-deftest nskk-thread-pool-test-shutdown ()
  "シャットダウンのテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((pool (nskk-thread-pool-create 2)))
    (nskk-thread-pool-shutdown pool)
    (should (nskk-thread-pool-shutdown-flag pool))

    ;; シャットダウン後のタスク投入はエラー
    (should-error
     (nskk-thread-submit pool (lambda () 1)))))

(ert-deftest nskk-thread-pool-test-shutdown-wait ()
  "待機付きシャットダウンのテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((pool (nskk-thread-pool-create 2))
        (completed 0)
        (mutex (make-mutex)))
    (dotimes (i 3)
      (nskk-thread-submit pool
                          (lambda ()
                            (sleep-for 0.1)
                            (with-mutex mutex
                              (setq completed (1+ completed))))))

    (nskk-thread-pool-shutdown pool t)
    (should (= completed 3))))

;;; パフォーマンステスト

(ert-deftest nskk-thread-pool-test-performance ()
  "パフォーマンステスト（並列化効果）。"
  (skip-unless (nskk-thread-pool-available-p))
  (let* ((pool (nskk-thread-pool-create 4))
         (task-count 100)
         (results nil)
         (mutex (make-mutex))
         (start-time (current-time)))

    (dotimes (i task-count)
      (nskk-thread-submit pool
                          (lambda ()
                            ;; 軽量な計算タスク
                            (let ((sum 0))
                              (dotimes (j 1000)
                                (setq sum (+ sum j)))
                              sum))
                          (lambda (r)
                            (with-mutex mutex
                              (push r results)))))

    ;; 完了待機
    (while (< (length results) task-count)
      (sleep-for 0.01))

    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (should (< elapsed 5.0)))  ; 5秒以内に完了

    (nskk-thread-pool-shutdown pool)))

;;; ステータステスト

(ert-deftest nskk-thread-pool-test-status ()
  "ステータス表示のテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((pool (nskk-thread-pool-create 2)))
    (should-not (nskk-thread-pool-status pool))
    (nskk-thread-pool-shutdown pool)))

;;; 並行性テスト

(ert-deftest nskk-thread-pool-test-concurrent-access ()
  "並行アクセスのテスト。"
  (skip-unless (nskk-thread-pool-available-p))
  (let ((pool (nskk-thread-pool-create 4))
        (counter 0)
        (mutex (make-mutex))
        (task-count 100))

    (dotimes (i task-count)
      (nskk-thread-submit pool
                          (lambda ()
                            (with-mutex mutex
                              (setq counter (1+ counter))))))

    ;; 全タスク完了待機
    (sleep-for 1.0)

    (should (= counter task-count))
    (nskk-thread-pool-shutdown pool)))

;;; 利用可能性チェック

(ert-deftest nskk-thread-pool-test-availability ()
  "スレッド機能の利用可能性チェック。"
  (let ((available (nskk-thread-pool-available-p)))
    (should (booleanp available))))

(provide 'nskk-thread-pool-test)

;;; nskk-thread-pool-test.el ends here
