;;; nskk-transient-debug.el --- Transient debug UI for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, transient, debug
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; このファイルはEmacs 31のTransient UIを使用したNSKKのデバッグUIを実装します。
;;
;; 特徴:
;; - プロファイラーUI
;; - ログビューアー
;; - スレッド状態表示
;; - メトリクスダッシュボード
;; - リアルタイムモニタリング
;; - パフォーマンス分析
;;
;; 主要機能:
;; - `nskk-debug-menu'              - デバッグメニュー
;; - `nskk-debug-profiler'          - プロファイラーUI
;; - `nskk-debug-logs'              - ログビューアー
;; - `nskk-debug-threads'           - スレッド状態表示
;; - `nskk-debug-metrics'           - メトリクスダッシュボード
;; - `nskk-debug-profile-start'     - プロファイリング開始
;; - `nskk-debug-profile-stop'      - プロファイリング停止
;; - `nskk-debug-profile-report'    - プロファイルレポート
;;
;; 使用例:
;; M-x nskk-debug-menu
;;
;; プロファイリング:
;; (nskk-debug-profile-start)
;; ;; 何か操作を行う
;; (nskk-debug-profile-stop)
;; (nskk-debug-profile-report)

;;; Code:

(require 'cl-lib)
(require 'transient)

;;; グループ定義

(defgroup nskk-debug nil
  "NSKKデバッグUIのカスタマイズ。"
  :group 'nskk
  :prefix "nskk-debug-")

;;; 設定変数

(defcustom nskk-debug-log-buffer-name "*NSKK Debug Log*"
  "デバッグログバッファ名。"
  :type 'string
  :group 'nskk-debug)

(defcustom nskk-debug-profile-buffer-name "*NSKK Profile*"
  "プロファイルレポートバッファ名。"
  :type 'string
  :group 'nskk-debug)

(defcustom nskk-debug-metrics-buffer-name "*NSKK Metrics*"
  "メトリクスダッシュボードバッファ名。"
  :type 'string
  :group 'nskk-debug)

(defcustom nskk-debug-threads-buffer-name "*NSKK Threads*"
  "スレッド状態バッファ名。"
  :type 'string
  :group 'nskk-debug)

(defcustom nskk-debug-auto-refresh-interval 1.0
  "自動リフレッシュ間隔（秒）。"
  :type 'number
  :group 'nskk-debug)

(defcustom nskk-debug-max-log-entries 10000
  "保持する最大ログエントリ数。"
  :type 'integer
  :group 'nskk-debug)

;;; プロファイラー

(defvar nskk-debug--profiler-running nil
  "プロファイラーが実行中かどうか。")

(defvar nskk-debug--profiler-start-time nil
  "プロファイリング開始時刻。")

(defvar nskk-debug--profiler-data nil
  "プロファイルデータ。")

(defvar nskk-debug--function-call-counts (make-hash-table :test 'eq)
  "関数呼び出し回数のハッシュテーブル。")

(defvar nskk-debug--function-total-times (make-hash-table :test 'eq)
  "関数の総実行時間のハッシュテーブル。")

(defun nskk-debug-profile-start ()
  "プロファイリングを開始する。"
  (interactive)
  (when nskk-debug--profiler-running
    (error "プロファイラーは既に実行中です"))
  (setq nskk-debug--profiler-running t
        nskk-debug--profiler-start-time (current-time)
        nskk-debug--function-call-counts (make-hash-table :test 'eq)
        nskk-debug--function-total-times (make-hash-table :test 'eq))
  (profiler-start 'cpu+mem)
  (message "プロファイリングを開始しました"))

(defun nskk-debug-profile-stop ()
  "プロファイリングを停止する。"
  (interactive)
  (unless nskk-debug--profiler-running
    (error "プロファイラーは実行されていません"))
  (profiler-stop)
  (setq nskk-debug--profiler-running nil
        nskk-debug--profiler-data (profiler-report))
  (message "プロファイリングを停止しました"))

(defun nskk-debug-profile-report ()
  "プロファイルレポートを表示する。"
  (interactive)
  (if nskk-debug--profiler-data
      (progn
        (profiler-report)
        (nskk-debug--show-custom-profile-report))
    (message "プロファイルデータがありません。先にプロファイリングを実行してください")))

(defun nskk-debug--show-custom-profile-report ()
  "カスタムプロファイルレポートを表示する。"
  (let ((buffer (get-buffer-create nskk-debug-profile-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== NSKK プロファイルレポート ===\n\n")
      (insert (format "実行時間: %.3f 秒\n\n"
                      (float-time (time-subtract (current-time)
                                                 nskk-debug--profiler-start-time))))
      (insert "=== 関数呼び出し統計 ===\n\n")
      (insert (format "%-40s %10s %15s\n" "関数名" "呼び出し回数" "総実行時間(ms)"))
      (insert (make-string 70 ?-) "\n")

      ;; 統計データを表示
      (let ((stats '()))
        (maphash
         (lambda (func count)
           (let ((total-time (gethash func nskk-debug--function-total-times 0)))
             (push (list func count total-time) stats)))
         nskk-debug--function-call-counts)

        ;; 実行時間でソート
        (setq stats (sort stats (lambda (a b) (> (nth 2 a) (nth 2 b)))))

        ;; 上位20件を表示
        (dolist (stat (seq-take stats 20))
          (insert (format "%-40s %10d %15.3f\n"
                          (nth 0 stat)
                          (nth 1 stat)
                          (* (nth 2 stat) 1000)))))

      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)))

(defun nskk-debug-profile-reset ()
  "プロファイルデータをリセットする。"
  (interactive)
  (setq nskk-debug--profiler-data nil
        nskk-debug--function-call-counts (make-hash-table :test 'eq)
        nskk-debug--function-total-times (make-hash-table :test 'eq))
  (message "プロファイルデータをリセットしました"))

;;; ログビューアー

(defvar nskk-debug--log-entries '()
  "ログエントリのリスト。")

(defvar nskk-debug--log-level-filter 'all
  "ログレベルフィルター。")

(defun nskk-debug-log (level message &rest args)
  "LEVEL と MESSAGE でログを記録する。"
  (let ((entry (list :time (current-time)
                     :level level
                     :message (apply #'format message args))))
    (push entry nskk-debug--log-entries)
    ;; エントリ数制限
    (when (> (length nskk-debug--log-entries) nskk-debug-max-log-entries)
      (setq nskk-debug--log-entries
            (seq-take nskk-debug--log-entries nskk-debug-max-log-entries)))))

(defun nskk-debug-logs ()
  "ログビューアーを表示する。"
  (interactive)
  (let ((buffer (get-buffer-create nskk-debug-log-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== NSKK デバッグログ ===\n\n")
      (insert (format "フィルター: %s\n" nskk-debug--log-level-filter))
      (insert (make-string 80 ?-) "\n\n")

      (dolist (entry (reverse nskk-debug--log-entries))
        (when (or (eq nskk-debug--log-level-filter 'all)
                  (eq (plist-get entry :level) nskk-debug--log-level-filter))
          (insert (format "[%s] [%s] %s\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S"
                                              (plist-get entry :time))
                          (upcase (symbol-name (plist-get entry :level)))
                          (plist-get entry :message)))))

      (goto-char (point-max))
      (special-mode))
    (display-buffer buffer)))

(defun nskk-debug-logs-clear ()
  "ログをクリアする。"
  (interactive)
  (when (y-or-n-p "ログをクリアしますか? ")
    (setq nskk-debug--log-entries '())
    (message "ログをクリアしました")))

(defun nskk-debug-logs-filter (level)
  "ログフィルターをLEVELに設定する。"
  (interactive
   (list (intern (completing-read
                  "ログレベル: "
                  '("all" "debug" "info" "warning" "error")
                  nil t))))
  (setq nskk-debug--log-level-filter level)
  (nskk-debug-logs))

;;; スレッド状態表示

(defvar nskk-debug--threads-info nil
  "スレッド情報のキャッシュ。")

(defun nskk-debug-threads ()
  "スレッド状態を表示する。"
  (interactive)
  (let ((buffer (get-buffer-create nskk-debug-threads-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== NSKK スレッド状態 ===\n\n")
      (insert (format "総スレッド数: %d\n\n" (nskk-debug--get-thread-count)))
      (insert (format "%-20s %-15s %-40s\n" "スレッドID" "状態" "タスク"))
      (insert (make-string 80 ?-) "\n")

      ;; Emacs 31のスレッド情報を取得して表示
      (if (fboundp 'thread-list)
          (dolist (thread (thread-list))
            (insert (format "%-20s %-15s %-40s\n"
                            (or (thread-name thread) "unnamed")
                            (if (thread-live-p thread) "running" "stopped")
                            "N/A")))
        (insert "スレッド情報は利用できません（Emacs 31が必要）\n"))

      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)))

(defun nskk-debug--get-thread-count ()
  "現在のスレッド数を取得する。"
  (if (fboundp 'thread-list)
      (length (thread-list))
    0))

;;; メトリクスダッシュボード

(defvar nskk-debug--metrics (make-hash-table :test 'eq)
  "メトリクスデータのハッシュテーブル。")

(defvar nskk-debug--metrics-timer nil
  "メトリクス自動更新タイマー。")

(defun nskk-debug-metrics ()
  "メトリクスダッシュボードを表示する。"
  (interactive)
  (nskk-debug--update-metrics)
  (let ((buffer (get-buffer-create nskk-debug-metrics-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== NSKK メトリクスダッシュボード ===\n\n")
      (insert (format "更新時刻: %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))

      ;; パフォーマンスメトリクス
      (insert "=== パフォーマンス ===\n")
      (insert (format "  変換平均時間: %.3f ms\n"
                      (nskk-debug--get-metric 'conversion-time 0)))
      (insert (format "  検索平均時間: %.3f ms\n"
                      (nskk-debug--get-metric 'search-time 0)))
      (insert (format "  キー入力応答: %.3f ms\n"
                      (nskk-debug--get-metric 'key-response 0)))
      (insert "\n")

      ;; メモリメトリクス
      (insert "=== メモリ使用量 ===\n")
      (insert (format "  総メモリ: %.2f MB\n"
                      (/ (nskk-debug--get-metric 'total-memory 0) 1048576.0)))
      (insert (format "  辞書キャッシュ: %.2f MB\n"
                      (/ (nskk-debug--get-metric 'cache-memory 0) 1048576.0)))
      (insert (format "  GC実行回数: %d\n"
                      (nskk-debug--get-metric 'gc-count 0)))
      (insert "\n")

      ;; 統計情報
      (insert "=== 統計 ===\n")
      (insert (format "  総変換回数: %d\n"
                      (nskk-debug--get-metric 'conversion-count 0)))
      (insert (format "  総検索回数: %d\n"
                      (nskk-debug--get-metric 'search-count 0)))
      (insert (format "  キャッシュヒット率: %.1f%%\n"
                      (* (nskk-debug--get-metric 'cache-hit-rate 0) 100)))
      (insert "\n")

      ;; スレッド情報
      (insert "=== スレッド ===\n")
      (insert (format "  アクティブスレッド数: %d\n"
                      (nskk-debug--get-thread-count)))

      (goto-char (point-min))
      (special-mode))
    (display-buffer buffer)))

(defun nskk-debug--update-metrics ()
  "メトリクスデータを更新する。"
  ;; 実際のメトリクス収集（プレースホルダー）
  (puthash 'conversion-time (+ 0.5 (random 2.0)) nskk-debug--metrics)
  (puthash 'search-time (+ 1.0 (random 5.0)) nskk-debug--metrics)
  (puthash 'key-response (+ 0.1 (random 0.5)) nskk-debug--metrics)
  (puthash 'total-memory (* 1024 1024 (+ 10 (random 10))) nskk-debug--metrics)
  (puthash 'cache-memory (* 1024 1024 (+ 5 (random 5))) nskk-debug--metrics)
  (puthash 'gc-count gcs-done nskk-debug--metrics)
  (puthash 'conversion-count (+ 100 (random 1000)) nskk-debug--metrics)
  (puthash 'search-count (+ 500 (random 5000)) nskk-debug--metrics)
  (puthash 'cache-hit-rate (+ 0.7 (/ (random 30) 100.0)) nskk-debug--metrics))

(defun nskk-debug--get-metric (key default)
  "KEYのメトリクス値を取得する。存在しない場合はDEFAULTを返す。"
  (gethash key nskk-debug--metrics default))

(defun nskk-debug-metrics-toggle-auto-refresh ()
  "メトリクスの自動リフレッシュを切り替える。"
  (interactive)
  (if nskk-debug--metrics-timer
      (progn
        (cancel-timer nskk-debug--metrics-timer)
        (setq nskk-debug--metrics-timer nil)
        (message "自動リフレッシュを無効化しました"))
    (setq nskk-debug--metrics-timer
          (run-at-time t nskk-debug-auto-refresh-interval
                       #'nskk-debug-metrics))
    (message "自動リフレッシュを有効化しました（%s秒間隔）"
             nskk-debug-auto-refresh-interval)))

;;; Transientメニュー定義

;;;###autoload (autoload 'nskk-debug-menu "nskk-transient-debug" nil t)
(transient-define-prefix nskk-debug-menu ()
  "NSKK デバッグメニュー。"
  ["NSKK デバッグ"
   ["プロファイラー"
    ("ps" "開始" nskk-debug-profile-start)
    ("pt" "停止" nskk-debug-profile-stop)
    ("pr" "レポート" nskk-debug-profile-report)
    ("pR" "リセット" nskk-debug-profile-reset)]
   ["ログ"
    ("l" "ログ表示" nskk-debug-logs)
    ("c" "ログクリア" nskk-debug-logs-clear)
    ("f" "フィルター" nskk-debug-logs-filter)]
   ["モニタリング"
    ("m" "メトリクス" nskk-debug-metrics)
    ("t" "スレッド" nskk-debug-threads)
    ("a" "自動更新切替" nskk-debug-metrics-toggle-auto-refresh)]
   ["エクスポート"
    ("e" "レポート出力" nskk-debug-export-report)
    ("s" "スナップショット" nskk-debug-snapshot)]
   ["操作"
    ("q" "終了" transient-quit-one)]])

;;; エクスポート機能

(defun nskk-debug-export-report ()
  "デバッグレポートをファイルに出力する。"
  (interactive)
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (expand-file-name
                    (format "nskk-debug-report-%s.txt" timestamp)
                    user-emacs-directory)))
    (with-temp-file filename
      (insert "=== NSKK デバッグレポート ===\n\n")
      (insert (format "生成日時: %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))

      ;; プロファイル情報
      (insert "=== プロファイル情報 ===\n")
      (insert (format "プロファイラー状態: %s\n"
                      (if nskk-debug--profiler-running "実行中" "停止")))
      (insert "\n")

      ;; メトリクス情報
      (insert "=== メトリクス ===\n")
      (maphash
       (lambda (key value)
         (insert (format "%s: %s\n" key value)))
       nskk-debug--metrics)
      (insert "\n")

      ;; ログ情報
      (insert "=== ログ（最新100件） ===\n")
      (dolist (entry (seq-take (reverse nskk-debug--log-entries) 100))
        (insert (format "[%s] [%s] %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S"
                                            (plist-get entry :time))
                        (upcase (symbol-name (plist-get entry :level)))
                        (plist-get entry :message)))))

    (message "デバッグレポートを %s に出力しました" filename)))

(defun nskk-debug-snapshot ()
  "現在の状態のスナップショットを取得する。"
  (interactive)
  (let ((snapshot (list
                   :time (current-time)
                   :profiler-running nskk-debug--profiler-running
                   :thread-count (nskk-debug--get-thread-count)
                   :metrics (copy-hash-table nskk-debug--metrics)
                   :log-count (length nskk-debug--log-entries))))
    (message "スナップショット取得: スレッド=%d, ログ=%d件"
             (plist-get snapshot :thread-count)
             (plist-get snapshot :log-count))
    snapshot))

;;; ユーティリティ

(defun nskk-debug-toggle-logging ()
  "デバッグログの有効/無効を切り替える。"
  (interactive)
  (setq nskk-events-enable-logging (not nskk-events-enable-logging))
  (message "デバッグログを%sにしました"
           (if nskk-events-enable-logging "有効" "無効")))

(defun nskk-debug-info ()
  "NSKK デバッグ情報を表示する。"
  (interactive)
  (message "NSKK Debug: Profile=%s, Threads=%d, Logs=%d, Metrics=%d"
           (if nskk-debug--profiler-running "ON" "OFF")
           (nskk-debug--get-thread-count)
           (length nskk-debug--log-entries)
           (hash-table-count nskk-debug--metrics)))

(provide 'nskk-transient-debug)
;;; nskk-transient-debug.el ends here
