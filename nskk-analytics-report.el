;;; nskk-analytics-report.el --- Report generation for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, analytics, report
;; Version: 1.0.0
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

;; このファイルはNSKKのレポート生成機能を提供します。
;;
;; 主な機能:
;; 1. HTMLレポート - ブラウザで閲覧可能なレポート生成
;; 2. PDFエクスポート - PDFファイルへのエクスポート（要外部ツール）
;; 3. グラフ生成 - SVG/PNG形式のグラフ生成
;; 4. 定期レポート - スケジュール実行によるレポート自動生成
;;
;; 使用例:
;; (require 'nskk-analytics-report)
;;
;; ;; HTMLレポート生成
;; (nskk-analytics-generate-html-report)
;;
;; ;; 定期レポート設定（毎日午後11時）
;; (nskk-analytics-schedule-report "0 23 * * *")

;;; Code:

(require 'cl-lib)
(require 'nskk-analytics-pattern)
(require 'nskk-analytics-optimize)

;;; カスタマイズ変数

(defgroup nskk-analytics-report nil
  "NSKK report generation settings."
  :group 'nskk
  :prefix "nskk-analytics-report-")

(defcustom nskk-analytics-report-directory
  (expand-file-name "nskk-reports" user-emacs-directory)
  "レポート保存ディレクトリ。"
  :type 'directory
  :group 'nskk-analytics-report)

(defcustom nskk-analytics-report-auto-open t
  "非nilの場合、レポート生成後に自動で開く。"
  :type 'boolean
  :group 'nskk-analytics-report)

(defcustom nskk-analytics-report-include-graphs t
  "非nilの場合、レポートにグラフを含める。"
  :type 'boolean
  :group 'nskk-analytics-report)

(defcustom nskk-analytics-report-style 'modern
  "レポートのスタイル（'modern, 'classic, 'minimal）。"
  :type '(choice (const :tag "Modern" modern)
                 (const :tag "Classic" classic)
                 (const :tag "Minimal" minimal))
  :group 'nskk-analytics-report)

;;; データ構造

(cl-defstruct (nskk-analytics-report
               (:constructor nskk-analytics-report-create)
               (:copier nskk-analytics-report-copy))
  "レポート構造体。

スロット:
  title        - レポートタイトル
  period       - 対象期間（plist）
  metrics      - 主要メトリクス
  charts       - チャートデータのリスト
  insights     - 自動生成された洞察
  timestamp    - 生成タイムスタンプ"
  (title "NSKK Analytics Report")
  (period nil)
  (metrics nil)
  (charts nil)
  (insights nil)
  (timestamp (current-time)))

;;; 内部変数

(defvar nskk-analytics-report--scheduled-timer nil
  "定期レポート生成のタイマー。")

(defvar nskk-analytics-report--last-report nil
  "最後に生成されたレポート。")

;;; HTMLレポート生成

;;;###autoload
(defun nskk-analytics-generate-html-report (&optional data output-file)
  "HTMLレポートを生成する。

DATA: レポートデータ（nilの場合は自動収集）
OUTPUT-FILE: 出力ファイルパス（nilの場合は自動生成）
戻り値: 生成されたファイルパス"
  (interactive)
  (let* ((report-data (or data (nskk-analytics-report--collect-data)))
         (output (or output-file
                     (nskk-analytics-report--generate-filename "html")))
         (html (nskk-analytics-report--build-html report-data)))

    ;; ディレクトリ作成
    (unless (file-directory-p nskk-analytics-report-directory)
      (make-directory nskk-analytics-report-directory t))

    ;; HTML書き込み
    (with-temp-file output
      (insert html))

    (setq nskk-analytics-report--last-report report-data)

    (message "HTML report generated: %s" output)

    ;; 自動で開く
    (when (and nskk-analytics-report-auto-open
               (called-interactively-p 'interactive))
      (browse-url-of-file output))

    output))

(defun nskk-analytics-report--collect-data ()
  "レポートデータを収集する。"
  (let ((metrics (nskk-analytics-collect-metrics))
        (patterns (nskk-analytics-detect-patterns))
        (trends (nskk-analytics-analyze-trends))
        (optimizations (nskk-analytics-suggest-optimizations)))

    (nskk-analytics-report-create
     :title "NSKK Analytics Report"
     :period `(:start ,(nskk-analytics-report--get-period-start)
               :end ,(current-time))
     :metrics metrics
     :charts (when nskk-analytics-report-include-graphs
               (nskk-analytics-report--generate-charts metrics patterns))
     :insights (nskk-analytics-report--generate-insights
                metrics patterns trends optimizations))))

(defun nskk-analytics-report--get-period-start ()
  "レポート対象期間の開始時刻を取得する。"
  (if (null nskk-analytics-pattern--events)
      (current-time)
    (seconds-to-time
     (nskk-analytics-event-timestamp
      (car (last nskk-analytics-pattern--events))))))

(defun nskk-analytics-report--build-html (report)
  "HTMLレポートを構築する。

REPORT: レポート構造体
戻り値: HTML文字列"
  (concat
   (nskk-analytics-report--html-header report)
   (nskk-analytics-report--html-summary report)
   (nskk-analytics-report--html-metrics report)
   (nskk-analytics-report--html-patterns report)
   (nskk-analytics-report--html-trends report)
   (nskk-analytics-report--html-insights report)
   (when nskk-analytics-report-include-graphs
     (nskk-analytics-report--html-charts report))
   (nskk-analytics-report--html-footer)))

(defun nskk-analytics-report--html-header (report)
  "HTMLヘッダーを生成する。

REPORT: レポート構造体
戻り値: HTML文字列"
  (format "<!DOCTYPE html>
<html lang=\"ja\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>%s</title>
    <style>
        %s
    </style>
</head>
<body>
    <div class=\"container\">
        <header>
            <h1>%s</h1>
            <p class=\"timestamp\">Generated: %s</p>
        </header>
"
          (nskk-analytics-report-title report)
          (nskk-analytics-report--css-styles)
          (nskk-analytics-report-title report)
          (format-time-string "%Y-%m-%d %H:%M:%S"
                              (nskk-analytics-report-timestamp report))))

(defun nskk-analytics-report--css-styles ()
  "CSSスタイルを返す。"
  (pcase nskk-analytics-report-style
    ('modern "
        body { font-family: 'Segoe UI', Tahoma, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }
        .container { max-width: 1200px; margin: 0 auto; background: white; padding: 40px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }
        h2 { color: #34495e; margin-top: 30px; }
        .metric-card { display: inline-block; margin: 10px; padding: 20px; background: #ecf0f1; border-radius: 5px; min-width: 200px; }
        .metric-value { font-size: 2em; font-weight: bold; color: #3498db; }
        .metric-label { color: #7f8c8d; font-size: 0.9em; }
        table { width: 100%; border-collapse: collapse; margin: 20px 0; }
        th, td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }
        th { background: #3498db; color: white; }
        .insight { background: #e8f4f8; padding: 15px; margin: 10px 0; border-left: 4px solid #3498db; }
        .chart { margin: 20px 0; }
        footer { margin-top: 40px; padding-top: 20px; border-top: 1px solid #ddd; color: #7f8c8d; text-align: center; }
    ")
    ('classic "
        body { font-family: Georgia, serif; margin: 0; padding: 20px; background: white; }
        .container { max-width: 800px; margin: 0 auto; }
        h1 { color: #000; border-bottom: 2px solid #000; }
        h2 { color: #333; }
        .metric-card { margin: 10px 0; padding: 10px; border: 1px solid #000; }
        table { width: 100%; border-collapse: collapse; }
        th, td { padding: 8px; border: 1px solid #000; }
        th { background: #ddd; }
    ")
    (_ "
        body { font-family: monospace; margin: 20px; }
        h1, h2 { border-bottom: 1px solid #000; }
        .metric-card { margin: 10px 0; }
        table { border-collapse: collapse; }
        th, td { padding: 5px; border: 1px solid #000; }
    ")))

(defun nskk-analytics-report--html-summary (report)
  "サマリーセクションを生成する。

REPORT: レポート構造体
戻り値: HTML文字列"
  (let* ((period (nskk-analytics-report-period report))
         (start (plist-get period :start))
         (end (plist-get period :end)))
    (format "
        <section class=\"summary\">
            <h2>Summary</h2>
            <p><strong>Period:</strong> %s to %s</p>
            <p><strong>Duration:</strong> %d days</p>
        </section>
"
            (format-time-string "%Y-%m-%d" start)
            (format-time-string "%Y-%m-%d" end)
            (/ (float-time (time-subtract end start)) 86400.0))))

(defun nskk-analytics-report--html-metrics (report)
  "メトリクスセクションを生成する。

REPORT: レポート構造体
戻り値: HTML文字列"
  (let ((metrics (nskk-analytics-report-metrics report)))
    (format "
        <section class=\"metrics\">
            <h2>Key Metrics</h2>
            <div class=\"metric-card\">
                <div class=\"metric-value\">%d</div>
                <div class=\"metric-label\">Total Conversions</div>
            </div>
            <div class=\"metric-card\">
                <div class=\"metric-value\">%d</div>
                <div class=\"metric-label\">Total Keystrokes</div>
            </div>
            <div class=\"metric-card\">
                <div class=\"metric-value\">%.1f%%</div>
                <div class=\"metric-label\">Accuracy</div>
            </div>
            <div class=\"metric-card\">
                <div class=\"metric-value\">%.3fs</div>
                <div class=\"metric-label\">Avg Speed</div>
            </div>
        </section>
"
            (nskk-analytics-metrics-conversions metrics)
            (nskk-analytics-metrics-keystrokes metrics)
            (* (nskk-analytics-metrics-accuracy metrics) 100)
            (nskk-analytics-metrics-speed metrics))))

(defun nskk-analytics-report--html-patterns (report)
  "パターンセクションを生成する。

REPORT: レポート構造体
戻り値: HTML文字列"
  (let* ((metrics (nskk-analytics-report-metrics report))
         (patterns (nskk-analytics-metrics-patterns metrics))
         (top-conv (alist-get 'top-conversions patterns)))
    (concat "
        <section class=\"patterns\">
            <h2>Usage Patterns</h2>
            <h3>Top Conversions</h3>
            <table>
                <thead>
                    <tr><th>Rank</th><th>Input</th><th>Count</th></tr>
                </thead>
                <tbody>
"
            (mapconcat
             (lambda (item)
               (format "                    <tr><td>%d</td><td>%s</td><td>%d</td></tr>\n"
                       (1+ (cl-position item top-conv))
                       (car item)
                       (cdr item)))
             (cl-subseq top-conv 0 (min 10 (length top-conv)))
             "")
            "
                </tbody>
            </table>
        </section>
")))

(defun nskk-analytics-report--html-trends (report)
  "トレンドセクションを生成する。

REPORT: レポート構造体
戻り値: HTML文字列"
  (let ((trends (or nskk-analytics-pattern--trends
                    (nskk-analytics-analyze-trends))))
    (format "
        <section class=\"trends\">
            <h2>Trends</h2>
            <ul>
                <li><strong>Conversions:</strong> %s</li>
                <li><strong>Accuracy:</strong> %s</li>
                <li><strong>Speed:</strong> %s</li>
            </ul>
        </section>
"
            (or (alist-get 'conversions trends) 'stable)
            (or (alist-get 'accuracy trends) 'stable)
            (or (alist-get 'speed trends) 'stable))))

(defun nskk-analytics-report--html-insights (report)
  "洞察セクションを生成する。

REPORT: レポート構造体
戻り値: HTML文字列"
  (let ((insights (nskk-analytics-report-insights report)))
    (concat "
        <section class=\"insights\">
            <h2>Insights & Recommendations</h2>
"
            (mapconcat
             (lambda (insight)
               (format "            <div class=\"insight\">%s</div>\n" insight))
             insights
             "")
            "
        </section>
")))

(defun nskk-analytics-report--html-charts (report)
  "チャートセクションを生成する。

REPORT: レポート構造体
戻り値: HTML文字列"
  (let ((charts (nskk-analytics-report-charts report)))
    (concat "
        <section class=\"charts\">
            <h2>Visualizations</h2>
"
            (mapconcat
             (lambda (chart)
               (format "            <div class=\"chart\">%s</div>\n" chart))
             charts
             "")
            "
        </section>
")))

(defun nskk-analytics-report--html-footer ()
  "HTMLフッターを生成する。

戻り値: HTML文字列"
  "
        <footer>
            <p>Generated by NSKK Analytics v1.0.0</p>
            <p>© 2024 NSKK Development Team</p>
        </footer>
    </div>
</body>
</html>
")

;;; グラフ生成

(defun nskk-analytics-report--generate-charts (metrics patterns)
  "チャートデータを生成する。

METRICS: メトリクス構造体
PATTERNS: パターンのalist
戻り値: チャートデータのリスト"
  (list
   (nskk-analytics-report--generate-bar-chart
    "Hourly Usage"
    (alist-get 'hourly patterns))
   (nskk-analytics-report--generate-pie-chart
    "Accuracy"
    `(("Correct" . ,(* (nskk-analytics-metrics-accuracy metrics) 100))
      ("Errors" . ,(* (- 1 (nskk-analytics-metrics-accuracy metrics)) 100))))))

(defun nskk-analytics-report--generate-bar-chart (title data)
  "棒グラフのSVGを生成する。

TITLE: グラフタイトル
DATA: データ配列
戻り値: SVG文字列"
  (when (> (length data) 0)
    (let* ((max-val (apply #'max data))
           (width 800)
           (height 400)
           (bar-width (/ width (float (length data))))
           (scale (if (> max-val 0) (/ height (float max-val)) 1)))
      (concat
       (format "<svg width=\"%d\" height=\"%d\" xmlns=\"http://www.w3.org/2000/svg\">\n" width height)
       (format "  <text x=\"%d\" y=\"20\" font-size=\"16\" font-weight=\"bold\">%s</text>\n"
               (/ width 2) title)
       (let ((x 0)
             (result ""))
         (dotimes (i (length data))
           (let* ((current-val (nth i data))
                  (bar-height (* current-val scale)))
             (setq result
                   (concat result
                           (format "  <rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"#3498db\"/>\n"
                                   x (- height bar-height) (- bar-width 2) bar-height)
                           (format "  <text x=\"%d\" y=\"%d\" font-size=\"10\" text-anchor=\"middle\">%d</text>\n"
                                   (+ x (/ bar-width 2)) (- height 5) i)))
             (setq x (+ x bar-width))))
         result)
       "</svg>"))))

(defun nskk-analytics-report--generate-pie-chart (title data)
  "円グラフのSVGを生成する。

TITLE: グラフタイトル
DATA: データのalist
戻り値: SVG文字列"
  (let* ((total (apply #'+ (mapcar #'cdr data)))
         (cx 200)
         (cy 200)
         (radius 150)
         (start-angle 0))
    (concat
     "<svg width=\"400\" height=\"400\" xmlns=\"http://www.w3.org/2000/svg\">\n"
     (format "  <text x=\"200\" y=\"30\" font-size=\"16\" font-weight=\"bold\" text-anchor=\"middle\">%s</text>\n"
             title)
     (mapconcat
      (lambda (item)
        (let* ((value (cdr item))
               (percentage (/ value total))
               (angle (* percentage 360))
               (end-angle (+ start-angle angle))
               (x1 (+ cx (* radius (cos (* start-angle (/ float-pi 180))))))
               (y1 (+ cy (* radius (sin (* start-angle (/ float-pi 180))))))
               (x2 (+ cx (* radius (cos (* end-angle (/ float-pi 180))))))
               (y2 (+ cy (* radius (sin (* end-angle (/ float-pi 180))))))
               (large-arc (if (> angle 180) 1 0)))
          (prog1
              (format "  <path d=\"M %d,%d L %d,%d A %d,%d 0 %d,1 %d,%d Z\" fill=\"%s\"/>\n"
                      cx cy x1 y1 radius radius large-arc x2 y2
                      (if (string= (car item) "Correct") "#2ecc71" "#e74c3c"))
            (setq start-angle end-angle))))
      data
      "")
     "</svg>")))

;;; 洞察生成

(defun nskk-analytics-report--generate-insights (metrics patterns trends optimizations)
  "自動で洞察を生成する。

METRICS: メトリクス構造体
PATTERNS: パターンのalist
TRENDS: トレンドのalist
OPTIMIZATIONS: 最適化提案のリスト
戻り値: 洞察文字列のリスト"
  (let ((insights nil))

    ;; 精度に関する洞察
    (let ((accuracy (nskk-analytics-metrics-accuracy metrics)))
      (cond
       ((> accuracy 0.95)
        (push "Excellent conversion accuracy! Keep up the good work." insights))
       ((> accuracy 0.8)
        (push "Good accuracy, but there's room for improvement. Review error patterns." insights))
       (t
        (push "Accuracy is low. Consider reviewing your dictionary or input method settings." insights))))

    ;; 速度に関する洞察
    (let ((speed (nskk-analytics-metrics-speed metrics)))
      (cond
       ((< speed 0.05)
        (push "Lightning-fast conversions! Performance is optimal." insights))
       ((< speed 0.1)
        (push "Good conversion speed. Consider cache optimization for further improvement." insights))
       (t
        (push "Conversion speed could be improved. Check for optimization suggestions." insights))))

    ;; トレンドに関する洞察
    (when trends
      (let ((acc-trend (alist-get 'accuracy trends)))
        (pcase acc-trend
          ('increasing
           (push "Your accuracy is improving over time. Great progress!" insights))
          ('decreasing
           (push "Warning: Accuracy is declining. Review recent changes." insights)))))

    ;; 最適化提案
    (when (> (length optimizations) 0)
      (push (format "%d optimization suggestions available. Review them to improve performance."
                    (length optimizations))
            insights))

    (nreverse insights)))

;;; PDFエクスポート

;;;###autoload
(defun nskk-analytics-export-pdf (&optional html-file output-file)
  "PDFファイルにエクスポートする（要wkhtmltopdf）。

HTML-FILE: 入力HTMLファイル（nilの場合は新規生成）
OUTPUT-FILE: 出力PDFファイル（nilの場合は自動生成）
戻り値: 生成されたPDFファイルパス"
  (interactive)
  (let* ((html (or html-file (nskk-analytics-generate-html-report)))
         (pdf (or output-file
                  (concat (file-name-sans-extension html) ".pdf"))))

    (if (executable-find "wkhtmltopdf")
        (progn
          (call-process "wkhtmltopdf" nil nil nil html pdf)
          (message "PDF exported: %s" pdf)
          pdf)
      (error "wkhtmltopdf not found. Please install it to export PDF"))))

;;; 定期レポート

;;;###autoload
(defun nskk-analytics-schedule-report (cron-expression)
  "定期レポート生成をスケジュールする。

CRON-EXPRESSION: cron形式の実行スケジュール（簡易実装）"
  (interactive "sCron expression (e.g., '0 23 * * *'): ")
  ;; 簡易実装：毎日の特定時刻に実行
  (when nskk-analytics-report--scheduled-timer
    (cancel-timer nskk-analytics-report--scheduled-timer))

  (let ((seconds-until-next (* 24 60 60)))  ;; 24時間後
    (setq nskk-analytics-report--scheduled-timer
          (run-at-time seconds-until-next seconds-until-next
                       #'nskk-analytics-generate-html-report))
    (message "Scheduled daily report generation")))

;;;###autoload
(defun nskk-analytics-cancel-scheduled-report ()
  "定期レポート生成をキャンセルする。"
  (interactive)
  (when nskk-analytics-report--scheduled-timer
    (cancel-timer nskk-analytics-report--scheduled-timer)
    (setq nskk-analytics-report--scheduled-timer nil)
    (message "Cancelled scheduled report generation")))

;;; ヘルパー関数

(defun nskk-analytics-report--generate-filename (extension)
  "レポートファイル名を生成する。

EXTENSION: ファイル拡張子
戻り値: フルパス"
  (expand-file-name
   (format "nskk-report-%s.%s"
           (format-time-string "%Y%m%d-%H%M%S")
           extension)
   nskk-analytics-report-directory))

(provide 'nskk-analytics-report)

;;; nskk-analytics-report.el ends here
