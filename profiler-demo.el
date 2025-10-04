;;; profiler-demo.el --- Demo script for NSKK profiler -*- lexical-binding: t; -*-

;;; Commentary:
;; Track Pプロファイリング機能のデモスクリプト

;;; Code:

(require 'nskk-profiler)
(require 'nskk-bottleneck-detector)
(require 'nskk-auto-tune)

(defun profiler-demo-simple ()
  "シンプルなプロファイリングデモ。"
  (message "=== Simple Profiling Demo ===")

  ;; プロファイラー初期化
  (nskk-profiler-reset)

  ;; プロファイリング開始
  (message "Starting profiler...")
  (let ((nskk-profiler-auto-report nil))
    (nskk-profile-start)

    ;; テスト処理
    (message "Running test workload...")
    (dotimes (i 100)
      (nskk-profile-function "demo-computation"
        (let ((sum 0))
          (dotimes (j 1000)
            (setq sum (+ sum j)))
          sum)))

    ;; 停止
    (message "Stopping profiler...")
    (nskk-profile-stop))

  ;; ステータス表示
  (let ((status (nskk-profiler-status)))
    (message "Profiler Status:")
    (message "  Active: %s" (plist-get status :active))
    (message "  Memory snapshots: %d" (plist-get status :memory-snapshots))
    (message "  Function stats: %d" (plist-get status :function-stats))
    (message "  GC events: %d" (plist-get status :gc-events)))

  (message "Demo completed. Use (nskk-profile-report) to see details."))

(defun profiler-demo-bottleneck ()
  "ボトルネック検出デモ。"
  (message "=== Bottleneck Detection Demo ===")

  ;; リセット
  (nskk-bottleneck-reset)

  ;; 検出開始
  (message "Starting bottleneck detection...")
  (nskk-bottleneck-detect-start)

  ;; テスト処理（意図的に遅い処理を含む）
  (message "Running workload with intentional bottlenecks...")

  ;; ホットパス（頻繁に呼ばれる）
  (dotimes (i 200)
    (nskk-profile-function "hotpath-function"
      (+ i i)))

  ;; 遅い関数
  (dotimes (i 5)
    (nskk-profile-function "slow-function"
      (sleep-for 0.01)))

  ;; 停止と分析
  (message "Stopping and analyzing...")
  (nskk-bottleneck-detect-stop)

  ;; ステータス表示
  (let ((status (nskk-bottleneck-status)))
    (message "Bottleneck Status:")
    (message "  Hotpaths: %d" (plist-get status :hotpaths))
    (message "  Slow functions: %d" (plist-get status :slow-functions))
    (message "  Memory issues: %d" (plist-get status :memory-issues))
    (message "  GC issues: %d" (plist-get status :gc-issues))
    (message "  Total issues: %d" (plist-get status :total-issues)))

  (message "Demo completed. Use (nskk-bottleneck-report) to see details."))

(defun profiler-demo-autotune ()
  "自動チューニングデモ。"
  (message "=== Auto-Tuning Demo ===")

  ;; リセット
  (nskk-auto-tune-reset)

  ;; 現在のパラメータ表示
  (message "Initial parameters:")
  (message "  gc-cons-threshold: %d" gc-cons-threshold)

  ;; 自動チューニング有効化
  (message "Enabling auto-tuning...")
  (nskk-auto-tune-enable)

  ;; 手動でチューニング実行
  (message "Running tuning cycle...")
  (nskk-auto-tune-run)

  ;; 結果表示
  (message "Tuning completed.")
  (when nskk-auto-tune--best-params
    (message "Best parameters found:")
    (message "  Cache size: %d"
             (plist-get nskk-auto-tune--best-params :cache-size))
    (message "  GC threshold: %d"
             (plist-get nskk-auto-tune--best-params :gc-cons-threshold))
    (message "  Best score: %.3f" nskk-auto-tune--best-score))

  ;; 無効化
  (nskk-auto-tune-disable)

  (message "Demo completed. Use (nskk-auto-tune-report) to see details."))

(defun profiler-demo-all ()
  "全てのデモを順次実行。"
  (interactive)
  (message "\n========================================")
  (message "NSKK Profiler Demo - All Features")
  (message "========================================\n")

  (profiler-demo-simple)
  (message "\n")
  (sit-for 1)

  (profiler-demo-bottleneck)
  (message "\n")
  (sit-for 1)

  (profiler-demo-autotune)
  (message "\n")

  (message "========================================")
  (message "All demos completed!")
  (message "========================================"))

;; 個別デモ実行用の便利関数
;;;###autoload
(defun nskk-demo-profiler ()
  "プロファイラーデモを実行。"
  (interactive)
  (profiler-demo-simple))

;;;###autoload
(defun nskk-demo-bottleneck ()
  "ボトルネック検出デモを実行。"
  (interactive)
  (profiler-demo-bottleneck))

;;;###autoload
(defun nskk-demo-autotune ()
  "自動チューニングデモを実行。"
  (interactive)
  (profiler-demo-autotune))

(provide 'profiler-demo)

;;; profiler-demo.el ends here
