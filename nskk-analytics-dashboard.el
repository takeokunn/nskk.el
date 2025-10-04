;;; nskk-analytics-dashboard.el --- Interactive dashboard for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, analytics, dashboard
;; Version: 1.0.0
;; Package-Requires: ((emacs "31.0") (transient "0.5.0"))

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

;; このファイルはNSKKの対話的分析ダッシュボードを提供します。
;;
;; 主な機能:
;; 1. リアルタイムメトリクス - 現在のパフォーマンス指標を表示
;; 2. 対話的分析 - Transient UIによる直感的な操作
;; 3. カスタムビュー - ユーザー定義のダッシュボードビュー
;; 4. クイックアクション - ワンクリックで分析・最適化実行
;;
;; 使用例:
;; (require 'nskk-analytics-dashboard)
;;
;; ;; ダッシュボードを開く
;; M-x nskk-analytics-dashboard
;;
;; ;; カスタムビューを作成
;; (nskk-analytics-dashboard-create-view "My View"
;;   '((metrics . t) (patterns . t) (trends . nil)))

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'nskk-analytics-pattern)
(require 'nskk-analytics-optimize)
(require 'nskk-analytics-report)

;;; カスタマイズ変数

(defgroup nskk-analytics-dashboard nil
  "NSKK analytics dashboard settings."
  :group 'nskk
  :prefix "nskk-analytics-dashboard-")

(defcustom nskk-analytics-dashboard-refresh-interval 5
  "ダッシュボードの自動更新間隔（秒）。0で無効化。"
  :type 'integer
  :group 'nskk-analytics-dashboard)

(defcustom nskk-analytics-dashboard-compact-mode nil
  "非nilの場合、コンパクト表示モードを使用する。"
  :type 'boolean
  :group 'nskk-analytics-dashboard)

;;; 内部変数

(defvar nskk-analytics-dashboard--refresh-timer nil
  "自動更新タイマー。")

(defvar nskk-analytics-dashboard--current-view 'default
  "現在のビュー。")

(defvar nskk-analytics-dashboard--custom-views nil
  "カスタムビューの定義。")

(defvar nskk-analytics-dashboard--live-metrics nil
  "リアルタイムメトリクス。")

;;; Transientダッシュボード

;;;###autoload (autoload 'nskk-analytics-dashboard "nskk-analytics-dashboard" nil t)
(transient-define-prefix nskk-analytics-dashboard ()
  "NSKK Analytics Dashboard - 対話的分析ダッシュボード"
  :refresh-suffixes t
  [:description
   (lambda ()
     (nskk-analytics-dashboard--build-header))
   :class transient-columns]
  ["Metrics"
   :class transient-row
   ("m" "View Metrics" nskk-analytics-dashboard-show-metrics)
   ("p" "Patterns" nskk-analytics-dashboard-show-patterns)
   ("t" "Trends" nskk-analytics-dashboard-show-trends)]
  ["Analysis"
   :class transient-row
   ("d" "Detect Patterns" nskk-analytics-detect-patterns)
   ("a" "Analyze Trends" nskk-analytics-analyze-trends)
   ("s" "Suggest Optimizations" nskk-analytics-suggest-optimizations)]
  ["Actions"
   :class transient-row
   ("r" "Generate Report" nskk-analytics-generate-html-report)
   ("o" "Auto Optimize" nskk-analytics-auto-optimize)
   ("A" "Run A/B Test" nskk-analytics-dashboard-run-ab-test)]
  ["Export"
   :class transient-row
   ("h" "HTML Export" nskk-analytics-generate-html-report)
   ("P" "PDF Export" nskk-analytics-export-pdf)]
  ["View"
   :class transient-row
   ("v" "Switch View" nskk-analytics-dashboard-switch-view)
   ("c" "Create Custom View" nskk-analytics-dashboard-create-view)
   ("R" "Refresh" nskk-analytics-dashboard-refresh)]
  ["Settings"
   :class transient-row
   ("C" "Clear Data" nskk-analytics-pattern-clear-data)
   ("T" "Toggle Auto-refresh" nskk-analytics-dashboard-toggle-refresh)
   ("q" "Quit" transient-quit-one)])

;;; ヘッダー構築

(defun nskk-analytics-dashboard--build-header ()
  "ダッシュボードヘッダーを構築する。

戻り値: フォーマット済み文字列"
  (let* ((metrics (or nskk-analytics-dashboard--live-metrics
                      (nskk-analytics-collect-metrics)))
         (patterns (or nskk-analytics-pattern--patterns
                       (nskk-analytics-detect-patterns))))

    (setq nskk-analytics-dashboard--live-metrics metrics)

    (concat
     (propertize "═══ NSKK Analytics Dashboard ═══\n" 'face 'bold)
     (format "\n%s\n"
             (propertize "Real-time Metrics" 'face 'underline))
     (format "  Conversions: %s | Keystrokes: %s | Accuracy: %s | Speed: %s\n"
             (propertize (format "%d" (nskk-analytics-metrics-conversions metrics))
                         'face 'success)
             (propertize (format "%d" (nskk-analytics-metrics-keystrokes metrics))
                         'face 'success)
             (propertize (format "%.1f%%"
                                 (* (nskk-analytics-metrics-accuracy metrics) 100))
                         'face (if (> (nskk-analytics-metrics-accuracy metrics) 0.9)
                                   'success
                                 'warning))
             (propertize (format "%.3fs" (nskk-analytics-metrics-speed metrics))
                         'face (if (< (nskk-analytics-metrics-speed metrics) 0.1)
                                   'success
                                 'warning)))
     (format "\n%s\n"
             (propertize "Quick Stats" 'face 'underline))
     (format "  Usage Intensity: %s events/day | Efficiency Score: %s/100\n"
             (propertize (format "%.1f"
                                 (or (alist-get 'usage-intensity patterns) 0.0))
                         'face 'info)
             (propertize (format "%d"
                                 (or (alist-get 'efficiency-score patterns) 0))
                         'face (let ((score (or (alist-get 'efficiency-score patterns) 0)))
                                 (cond
                                  ((>= score 80) 'success)
                                  ((>= score 60) 'warning)
                                  (t 'error)))))
     (format "\nView: %s | Last Update: %s\n"
             (propertize (symbol-name nskk-analytics-dashboard--current-view)
                         'face 'italic)
             (format-time-string "%H:%M:%S"))
     (propertize "────────────────────────────────────────\n" 'face 'shadow))))

;;; メトリクス表示

(defun nskk-analytics-dashboard-show-metrics ()
  "詳細メトリクスを表示する。"
  (interactive)
  (let ((metrics (nskk-analytics-collect-metrics)))
    (with-current-buffer (get-buffer-create "*NSKK Metrics*")
      (erase-buffer)
      (insert (nskk-analytics-dashboard--format-metrics metrics))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun nskk-analytics-dashboard--format-metrics (metrics)
  "メトリクスを整形する。

METRICS: メトリクス構造体
戻り値: フォーマット済み文字列"
  (format "=== NSKK Detailed Metrics ===

Total Conversions: %d
Total Keystrokes:  %d
Accuracy:          %.2f%%
Average Speed:     %.4f seconds

Patterns Found:    %d
Generated:         %s
"
          (nskk-analytics-metrics-conversions metrics)
          (nskk-analytics-metrics-keystrokes metrics)
          (* (nskk-analytics-metrics-accuracy metrics) 100)
          (nskk-analytics-metrics-speed metrics)
          (length (nskk-analytics-metrics-patterns metrics))
          (format-time-string "%Y-%m-%d %H:%M:%S"
                              (nskk-analytics-metrics-timestamp metrics))))

;;; パターン表示

(defun nskk-analytics-dashboard-show-patterns ()
  "使用パターンを表示する。"
  (interactive)
  (nskk-analytics-visualize-patterns))

;;; トレンド表示

(defun nskk-analytics-dashboard-show-trends ()
  "トレンドを表示する。"
  (interactive)
  (let ((trends (nskk-analytics-analyze-trends)))
    (with-current-buffer (get-buffer-create "*NSKK Trends*")
      (erase-buffer)
      (insert (nskk-analytics-dashboard--format-trends trends))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun nskk-analytics-dashboard--format-trends (trends)
  "トレンドを整形する。

TRENDS: トレンドのalist
戻り値: フォーマット済み文字列"
  (format "=== NSKK Trends Analysis ===

Conversions Trend: %s %s
Accuracy Trend:    %s %s
Speed Trend:       %s %s

Legend:
  ↑ Increasing
  ↓ Decreasing
  → Stable
"
          (nskk-analytics-dashboard--trend-indicator (alist-get 'conversions trends))
          (or (alist-get 'conversions trends) 'stable)
          (nskk-analytics-dashboard--trend-indicator (alist-get 'accuracy trends))
          (or (alist-get 'accuracy trends) 'stable)
          (nskk-analytics-dashboard--trend-indicator (alist-get 'speed trends))
          (or (alist-get 'speed trends) 'stable)))

(defun nskk-analytics-dashboard--trend-indicator (trend)
  "トレンドのインジケーターを返す。

TREND: トレンド（'increasing/'decreasing/'stable）
戻り値: インジケーター文字列"
  (pcase trend
    ('increasing "↑")
    ('decreasing "↓")
    (_ "→")))

;;; A/Bテスト

(defun nskk-analytics-dashboard-run-ab-test ()
  "A/Bテストを対話的に設定・実行する。"
  (interactive)
  (let* ((name (read-string "Test name: "))
         (param (intern (completing-read "Parameter to test: "
                                         '("nskk-cache-size"
                                           "nskk-thread-pool-size"
                                           "nskk-completion-limit"))))
         (value-a (read-number (format "%s value A: " param)))
         (value-b (read-number (format "%s value B: " param)))
         (duration (read-number "Duration (seconds): " 3600)))
    (nskk-analytics-run-ab-test
     name
     `((,param . ,value-a))
     `((,param . ,value-b))
     duration)))

;;; ビュー管理

(defun nskk-analytics-dashboard-switch-view ()
  "ビューを切り替える。"
  (interactive)
  (let ((view (intern (completing-read "View: "
                                       (cons 'default
                                             (mapcar #'car nskk-analytics-dashboard--custom-views))))))
    (setq nskk-analytics-dashboard--current-view view)
    (message "Switched to view: %s" view)))

(defun nskk-analytics-dashboard-create-view (name config)
  "カスタムビューを作成する。

NAME: ビュー名
CONFIG: ビュー設定（plist）"
  (interactive
   (list (read-string "View name: ")
         (read--expression "Config (plist): ")))
  (push (cons (intern name) config) nskk-analytics-dashboard--custom-views)
  (message "Created custom view: %s" name))

;;; 自動更新

(defun nskk-analytics-dashboard-toggle-refresh ()
  "自動更新を切り替える。"
  (interactive)
  (if nskk-analytics-dashboard--refresh-timer
      (progn
        (cancel-timer nskk-analytics-dashboard--refresh-timer)
        (setq nskk-analytics-dashboard--refresh-timer nil)
        (message "Auto-refresh disabled"))
    (when (> nskk-analytics-dashboard-refresh-interval 0)
      (setq nskk-analytics-dashboard--refresh-timer
            (run-at-time nskk-analytics-dashboard-refresh-interval
                         nskk-analytics-dashboard-refresh-interval
                         #'nskk-analytics-dashboard-refresh))
      (message "Auto-refresh enabled (interval: %ds)"
               nskk-analytics-dashboard-refresh-interval))))

(defun nskk-analytics-dashboard-refresh ()
  "ダッシュボードを更新する。"
  (interactive)
  (setq nskk-analytics-dashboard--live-metrics nil)
  ;; Transientが開いている場合は再描画
  (when (and (boundp 'transient-current-command)
             transient-current-command)
    (transient-setup 'nskk-analytics-dashboard)))

;;; ヘルパー関数

(defun nskk-analytics-dashboard--format-number (num)
  "数値を読みやすい形式にフォーマットする。

NUM: 数値
戻り値: フォーマット済み文字列"
  (cond
   ((>= num 1000000)
    (format "%.1fM" (/ num 1000000.0)))
   ((>= num 1000)
    (format "%.1fK" (/ num 1000.0)))
   (t
    (format "%d" num))))

(defun nskk-analytics-dashboard--create-sparkline (data &optional width)
  "簡易スパークラインを作成する。

DATA: データポイントのリスト
WIDTH: 幅（デフォルト: 20）
戻り値: スパークライン文字列"
  (let* ((w (or width 20))
         (max-val (apply #'max data))
         (min-val (apply #'min data))
         (range (- max-val min-val))
         (chars '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█")))
    (mapconcat
     (lambda (val)
       (let ((index (if (zerop range)
                        0
                      (min 7 (floor (* 8 (/ (- val min-val) (float range))))))))
         (nth index chars)))
     data
     "")))

;;; 統計ウィジェット

(defun nskk-analytics-dashboard--create-gauge (value max-value &optional label)
  "ゲージウィジェットを作成する。

VALUE: 現在値
MAX-VALUE: 最大値
LABEL: ラベル（オプション）
戻り値: フォーマット済み文字列"
  (let* ((percentage (/ (float value) max-value))
         (filled (floor (* 20 percentage)))
         (empty (- 20 filled)))
    (format "%s [%s%s] %.0f%%"
            (or label "")
            (make-string filled ?█)
            (make-string empty ?░)
            (* percentage 100))))

(provide 'nskk-analytics-dashboard)

;;; nskk-analytics-dashboard.el ends here
