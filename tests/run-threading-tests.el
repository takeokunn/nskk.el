;;; run-threading-tests.el --- Track N Threading test runner -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:

;; Track N: Threading の統合テスト実行スクリプト

;;; Code:

(add-to-list 'load-path (expand-file-name ".." default-directory))
(add-to-list 'load-path default-directory)

;; 依存モジュール読み込み
(require 'nskk-thread-pool)
(require 'nskk-parallel-search)
(require 'nskk-async-learning)
(require 'nskk-sync-primitives)

;; テスト読み込み
(require 'nskk-thread-pool-test)
(require 'nskk-parallel-search-test)
(require 'nskk-async-learning-test)
(require 'nskk-sync-primitives-test)

;;; 並行性テスト

(defun nskk-threading-test-concurrent-operations ()
  "複数モジュールの並行動作テスト。"
  (message "\n=== Concurrent Operations Test ===")

  (when (nskk-thread-pool-available-p)
    ;; スレッドプール作成
    (let ((pool (nskk-thread-pool-create 4))
          (counter (nskk-atomic-create 0))
          (mutex (make-mutex))
          (results nil))

      ;; 並列検索と学習を同時実行
      (message "Running concurrent search and learning...")

      ;; 100タスクを並列実行
      (dotimes (i 100)
        (nskk-thread-submit
         pool
         (lambda ()
           ;; Atomic操作テスト
           (nskk-atomic-increment counter)

           ;; 非同期学習テスト
           (nskk-async-learning-record
            (format "key%d" i)
            (format "value%d" i))

           ;; 結果記録
           (with-mutex mutex
             (push i results)))
         (lambda (_)
           nil)))

      ;; 完了待機
      (message "Waiting for completion...")
      (sleep-for 3.0)

      ;; 検証
      (let ((final-count (nskk-atomic-get counter)))
        (message "Final counter: %d (expected: 100)" final-count)
        (message "Results count: %d" (length results))

        (if (and (= final-count 100)
                 (= (length results) 100))
            (message "✓ Concurrent operations test PASSED")
          (message "✗ Concurrent operations test FAILED")))

      (nskk-thread-pool-shutdown pool))))

;;; 負荷テスト

(defun nskk-threading-test-stress ()
  "負荷テスト - 大量タスクの並列処理。"
  (message "\n=== Stress Test ===")

  (when (nskk-thread-pool-available-p)
    (let ((pool (nskk-thread-pool-create 8))
          (task-count 1000)
          (completed (nskk-atomic-create 0))
          (start-time (current-time)))

      (message "Submitting %d tasks..." task-count)

      ;; 1000タスクを投入
      (dotimes (i task-count)
        (nskk-thread-submit
         pool
         (lambda ()
           ;; 軽量な計算タスク
           (let ((sum 0))
             (dotimes (j 100)
               (setq sum (+ sum j)))
             sum))
         (lambda (_)
           (nskk-atomic-increment completed))))

      ;; 完了待機
      (message "Waiting for completion...")
      (let ((timeout 30.0)
            (check-interval 0.5))
        (while (and (< (nskk-atomic-get completed) task-count)
                    (< (float-time (time-subtract (current-time) start-time))
                       timeout))
          (sleep-for check-interval)
          (when (= (mod (nskk-atomic-get completed) 100) 0)
            (message "Progress: %d/%d" (nskk-atomic-get completed) task-count))))

      (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
             (final-count (nskk-atomic-get completed))
             (throughput (/ final-count elapsed)))

        (message "Completed: %d/%d tasks in %.2f seconds"
                 final-count task-count elapsed)
        (message "Throughput: %.1f tasks/sec" throughput)

        (if (= final-count task-count)
            (message "✓ Stress test PASSED")
          (message "✗ Stress test FAILED")))

      (nskk-thread-pool-shutdown pool))))

;;; パフォーマンステスト

(defun nskk-threading-test-performance ()
  "並列化による性能向上テスト。"
  (message "\n=== Performance Test ===")

  (when (and (nskk-thread-pool-available-p)
             (fboundp 'nskk-search))
    (message "Testing parallel search speedup...")

    ;; テスト用インデックス作成
    (let ((index (nskk-index-create)))
      ;; 10000エントリ追加
      (message "Creating test index with 10000 entries...")
      (dotimes (i 10000)
        (nskk-index-insert index
                           (format "かん%d" i)
                           (list (format "漢字%d" i))))

      ;; 並列検索ベンチマーク
      (let ((iterations 10))
        (message "Running benchmark with %d iterations..." iterations)

        ;; 通常検索
        (let ((seq-times nil))
          (dotimes (_ iterations)
            (let ((start (current-time)))
              (nskk-search index "かん" 'prefix)
              (push (float-time (time-subtract (current-time) start))
                    seq-times)))

          ;; 並列検索
          (let ((par-times nil))
            (nskk-parallel-search-initialize)

            (dotimes (_ iterations)
              (let ((start (current-time)))
                (nskk-parallel-search index "かん" 'prefix)
                (push (float-time (time-subtract (current-time) start))
                      par-times)))

            (let* ((seq-avg (/ (apply #'+ seq-times) iterations))
                   (par-avg (/ (apply #'+ par-times) iterations))
                   (speedup (/ seq-avg par-avg)))

              (message "Sequential: %.3f ms" (* seq-avg 1000))
              (message "Parallel:   %.3f ms" (* par-avg 1000))
              (message "Speedup:    %.2fx" speedup)

              (if (>= speedup 1.2)
                  (message "✓ Performance test PASSED (speedup: %.2fx)" speedup)
                (message "✗ Performance test FAILED (speedup: %.2fx < 1.2x)" speedup)))

            (nskk-parallel-search-shutdown)))))))

;;; メインテスト実行

(defun nskk-threading-run-all-tests ()
  "全てのスレッディングテストを実行する。"
  (interactive)
  (message "\n╔════════════════════════════════════════╗")
  (message "║  NSKK Track N: Threading Test Suite  ║")
  (message "╚════════════════════════════════════════╝\n")

  ;; 利用可能性チェック
  (if (nskk-thread-pool-available-p)
      (progn
        (message "✓ Thread support available (Emacs %s)" emacs-version)
        (message "  CPU cores: %d" (nskk-thread-pool--get-cpu-count))

        ;; 並行性テスト
        (nskk-threading-test-concurrent-operations)

        ;; 負荷テスト
        (nskk-threading-test-stress)

        ;; パフォーマンステスト
        (when (fboundp 'nskk-search)
          (nskk-threading-test-performance))

        (message "\n╔════════════════════════════════════════╗")
        (message "║        Test Suite Completed           ║")
        (message "╚════════════════════════════════════════╝\n"))

    (message "✗ Thread support NOT available")
    (message "  Skipping threading tests")))

;; バッチモードで実行
(when noninteractive
  (nskk-threading-run-all-tests))

(provide 'run-threading-tests)

;;; run-threading-tests.el ends here
