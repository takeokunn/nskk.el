;;; nskk-coverage.el --- Code coverage tool for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, testing, coverage
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

;; このファイルはNSKKのコードカバレッジ測定システムを実装します。
;;
;; 特徴:
;; - testcover.elベースのカバレッジ測定
;; - HTMLレポート生成
;; - JSONレポート生成
;; - テキストレポート生成
;; - カバレッジ閾値チェック
;; - プロジェクト全体の集計
;;
;; 使用方法:
;;
;; ;; カバレッジ測定開始
;; (require 'nskk-coverage)
;; (nskk-coverage-start)
;;
;; ;; テスト実行
;; (ert-run-tests-batch-and-exit)
;;
;; ;; レポート生成
;; (nskk-coverage-report 'html)
;; (nskk-coverage-report 'json)
;; (nskk-coverage-report 'text)
;;
;; ;; 閾値チェック
;; (nskk-coverage-check-threshold 95.0)

;;; Code:

(require 'testcover)
(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-coverage nil
  "NSKK code coverage tool customization."
  :group 'nskk-test
  :prefix "nskk-coverage-")

(defcustom nskk-coverage-output-dir "coverage"
  "カバレッジレポートの出力ディレクトリ。"
  :type 'directory
  :group 'nskk-coverage)

(defcustom nskk-coverage-exclude-patterns '("nskk-test.*\\.el$" "nskk-coverage\\.el$")
  "カバレッジ測定から除外するファイルパターン（正規表現のリスト）。"
  :type '(repeat regexp)
  :group 'nskk-coverage)

(defcustom nskk-coverage-threshold 95.0
  "カバレッジ閾値（パーセント）。この値を下回るとエラーとする。"
  :type 'float
  :group 'nskk-coverage)

(defcustom nskk-coverage-verbose nil
  "非nilの場合、詳細な出力を表示する。"
  :type 'boolean
  :group 'nskk-coverage)

;;; 内部変数

(defvar nskk-coverage--instrumented-files nil
  "インストルメント済みファイルのリスト。")

(defvar nskk-coverage--data (make-hash-table :test 'equal)
  "カバレッジデータのハッシュテーブル。
キー: ファイルパス
値: カバレッジ情報のplist
  :total          - 総フォーム数
  :covered        - カバーされたフォーム数
  :ok-coverage    - 十分にカバーされたフォーム数
  :coverage-rate  - カバレッジ率（%）")

(defvar nskk-coverage--start-time nil
  "カバレッジ測定開始時刻。")

;;; 主要関数

;;;###autoload
(defun nskk-coverage-start (&optional files)
  "カバレッジ測定を開始する。
FILES が指定されない場合、プロジェクト内の全 .el ファイルを対象とする。"
  (interactive)
  (setq nskk-coverage--start-time (current-time))
  (let ((target-files (or files (nskk-coverage--find-source-files))))
    (when nskk-coverage-verbose
      (message "Starting coverage instrumentation for %d files..." (length target-files)))
    (dolist (file target-files)
      (unless (nskk-coverage--excluded-p file)
        (nskk-coverage--instrument-file file)))
    (when nskk-coverage-verbose
      (message "Instrumentation completed: %d files" (length nskk-coverage--instrumented-files)))))

(defun nskk-coverage--instrument-file (file)
  "FILE をインストルメントする。"
  (when nskk-coverage-verbose
    (message "Instrumenting %s..." file))
  (condition-case err
      (progn
        (testcover-start file 'noload)
        (push file nskk-coverage--instrumented-files))
    (error
     (warn "Failed to instrument %s: %S" file err))))

;;;###autoload
(defun nskk-coverage-collect ()
  "カバレッジデータを収集する。
testcover.elのカバレッジベクターから情報を抽出。"
  (interactive)
  (clrhash nskk-coverage--data)
  (dolist (file nskk-coverage--instrumented-files)
    (let ((data (nskk-coverage--collect-file-data file)))
      (puthash file data nskk-coverage--data)))
  (when nskk-coverage-verbose
    (let ((summary (nskk-coverage--aggregate-summary)))
      (message "Coverage collected: %.2f%% (%d/%d forms)"
               (plist-get summary :coverage-rate)
               (plist-get summary :total-covered)
               (plist-get summary :total-forms)))))

(defun nskk-coverage--collect-file-data (file)
  "FILE のカバレッジデータを収集する。"
  (let ((total 0)
        (covered 0)
        (ok-coverage 0)
        (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (dolist (form-data edebug-form-data)
        (let* ((sym (car form-data))
               (coverage-vec (get sym 'edebug-coverage)))
          (when coverage-vec
            (dotimes (i (length coverage-vec))
              (let ((status (aref coverage-vec i)))
                (setq total (1+ total))
                (unless (eq status 'edebug-unknown)
                  (setq covered (1+ covered))
                  (when (eq status 'edebug-ok-coverage)
                    (setq ok-coverage (1+ ok-coverage))))))))))
    (list :total total
          :covered covered
          :ok-coverage ok-coverage
          :coverage-rate (if (> total 0)
                             (* 100.0 (/ (float covered) total))
                           0.0))))

;;;###autoload
(defun nskk-coverage-report (&optional format)
  "カバレッジレポートを生成する。
FORMAT は 'html, 'text, または 'json。デフォルトは 'html。"
  (interactive
   (list (intern (completing-read "Report format: "
                                  '("html" "text" "json")
                                  nil t nil nil "html"))))
  (nskk-coverage-collect)
  (let ((format (or format 'html)))
    (pcase format
      ('html (nskk-coverage--generate-html-report))
      ('text (nskk-coverage--generate-text-report))
      ('json (nskk-coverage--generate-json-report))
      (_ (error "Unknown format: %s" format)))))

;;;###autoload
(defun nskk-coverage-check-threshold (&optional threshold)
  "カバレッジが THRESHOLD 以上であることをチェックする。
THRESHOLD が指定されない場合、`nskk-coverage-threshold' を使用。
閾値を下回る場合はエラーを発生させる。"
  (interactive)
  (nskk-coverage-collect)
  (let* ((threshold (or threshold nskk-coverage-threshold))
         (summary (nskk-coverage--aggregate-summary))
         (actual-rate (plist-get summary :coverage-rate)))
    (if (>= actual-rate threshold)
        (progn
          (message "Coverage check passed: %.2f%% >= %.2f%%"
                   actual-rate threshold)
          t)
      (error "Coverage check failed: %.2f%% < %.2f%%"
             actual-rate threshold))))

;;; ヘルパー関数

(defun nskk-coverage--find-source-files ()
  "NSKKプロジェクトのソースファイルを検索。"
  (let ((project-root (locate-dominating-file default-directory ".git")))
    (when project-root
      (directory-files-recursively
       project-root
       "^nskk-[^/]*\\.el$"
       nil
       (lambda (dir)
         (not (member (file-name-nondirectory dir) '("tests" ".git" "coverage"))))))))

(defun nskk-coverage--excluded-p (file)
  "FILE がカバレッジ測定から除外されるか判定。"
  (cl-some (lambda (pattern)
             (string-match-p pattern file))
           nskk-coverage-exclude-patterns))

(defun nskk-coverage--aggregate-summary ()
  "プロジェクト全体のカバレッジサマリーを生成。"
  (let ((total-files 0)
        (total-forms 0)
        (total-covered 0)
        (total-ok-coverage 0))
    (maphash
     (lambda (_file data)
       (setq total-files (1+ total-files))
       (setq total-forms (+ total-forms (plist-get data :total)))
       (setq total-covered (+ total-covered (plist-get data :covered)))
       (setq total-ok-coverage (+ total-ok-coverage (plist-get data :ok-coverage))))
     nskk-coverage--data)
    (list :total-files total-files
          :total-forms total-forms
          :total-covered total-covered
          :total-ok-coverage total-ok-coverage
          :coverage-rate (if (> total-forms 0)
                             (* 100.0 (/ (float total-covered) total-forms))
                           0.0)
          :ok-coverage-rate (if (> total-forms 0)
                                (* 100.0 (/ (float total-ok-coverage) total-forms))
                              0.0))))

;;; テキストレポート

(defun nskk-coverage--generate-text-report ()
  "テキスト形式のカバレッジレポートを生成。"
  (let ((summary (nskk-coverage--aggregate-summary)))
    (with-output-to-temp-buffer "*NSKK Coverage Report*"
      (princ "=== NSKK Code Coverage Report ===\n\n")
      (princ (format "Total Coverage: %.2f%%\n" (plist-get summary :coverage-rate)))
      (princ (format "OK Coverage:    %.2f%%\n" (plist-get summary :ok-coverage-rate)))
      (princ (format "Total Forms:    %d\n" (plist-get summary :total-forms)))
      (princ (format "Covered Forms:  %d\n" (plist-get summary :total-covered)))
      (princ (format "Total Files:    %d\n\n" (plist-get summary :total-files)))
      (princ "File Coverage:\n")
      (princ (make-string 80 ?-))
      (princ "\n")
      (let ((files nil))
        (maphash (lambda (file data)
                   (push (cons file data) files))
                 nskk-coverage--data)
        (setq files (sort files (lambda (a b)
                                  (string< (car a) (car b)))))
        (dolist (entry files)
          (let* ((file (car entry))
                 (data (cdr entry))
                 (rate (plist-get data :coverage-rate))
                 (covered (plist-get data :covered))
                 (total (plist-get data :total)))
            (princ (format "%-50s %6.2f%% (%4d/%4d)\n"
                           (file-name-nondirectory file)
                           rate covered total))))))
    (message "Text report generated in buffer *NSKK Coverage Report*")))

;;; JSONレポート

(defun nskk-coverage--generate-json-report ()
  "JSON形式のカバレッジレポートを生成。"
  (require 'json)
  (make-directory nskk-coverage-output-dir t)
  (let* ((summary (nskk-coverage--aggregate-summary))
         (files-data nil)
         (output-file (expand-file-name "coverage.json"
                                        nskk-coverage-output-dir)))
    (maphash
     (lambda (file data)
       (push (list (cons "file" file)
                   (cons "total" (plist-get data :total))
                   (cons "covered" (plist-get data :covered))
                   (cons "ok_coverage" (plist-get data :ok-coverage))
                   (cons "coverage_rate" (plist-get data :coverage-rate)))
             files-data))
     nskk-coverage--data)
    (let ((report (list (cons "summary" summary)
                        (cons "files" files-data)
                        (cons "timestamp" (format-time-string "%Y-%m-%dT%H:%M:%S")))))
      (with-temp-file output-file
        (insert (json-encode report))))
    (message "JSON report generated: %s" output-file)
    output-file))

;;; HTMLレポート

(defun nskk-coverage--collect-line-coverage (file)
  "FILE の行ごとのカバレッジデータを収集する。
戻り値: ハッシュテーブル (キー: 行番号, 値: カバレッジ状態)
  状態: 'covered, 'uncovered, '1value, 'noreturn"
  (let ((line-coverage (make-hash-table :test 'equal))
        (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (dolist (form-data edebug-form-data)
        (let* ((symbol (car form-data))
               (edebug-data (get symbol 'edebug))
               (coverage (get symbol 'edebug-coverage)))
          (when (and edebug-data coverage)
            (let* ((def-mark (car edebug-data))
                   (points (nth 2 edebug-data))
                   (len (length points)))
              (dotimes (i len)
                (let* ((point (+ def-mark (aref points i)))
                       (status (aref coverage i))
                       (line (line-number-at-pos point)))
                  (cond
                   ;; OK coverage - カバー済み
                   ((eq status 'edebug-ok-coverage)
                    (puthash line 'covered line-coverage))
                   ;; 1value - 常に同じ値を返す
                   ((or (eq status 'testcover-1value)
                        (eq (car-safe status) 'testcover-1value))
                    (puthash line '1value line-coverage))
                   ;; noreturn - 実行されるが返らない
                   ((eq (car-safe status) 'noreturn)
                    (puthash line 'noreturn line-coverage))
                   ;; unknown - カバーされていない
                   ((eq status 'edebug-unknown)
                    (unless (gethash line line-coverage)
                      (puthash line 'uncovered line-coverage)))
                   ;; その他のカウント値
                   ((numberp status)
                    (if (> status 0)
                        (puthash line 'covered line-coverage)
                      (unless (gethash line line-coverage)
                        (puthash line 'uncovered line-coverage))))))))))))
    line-coverage))

(defun nskk-coverage--generate-html-report ()
  "HTML形式のカバレッジレポートを生成。"
  (make-directory nskk-coverage-output-dir t)
  ;; 各ファイルのHTMLレポートを生成
  (maphash
   (lambda (file _data)
     (nskk-coverage--generate-file-html file))
   nskk-coverage--data)
  ;; インデックスページを生成
  (let ((index-file (expand-file-name "index.html" nskk-coverage-output-dir)))
    (with-temp-file index-file
      (insert (nskk-coverage--html-index-template)))
    (message "HTML report generated: %s" index-file)
    (when (display-graphic-p)
      (browse-url-of-file index-file))
    index-file))

(defun nskk-coverage--generate-file-html (file)
  "FILE の詳細HTMLレポートを生成する。"
  (let* ((data (gethash file nskk-coverage--data))
         (line-coverage (nskk-coverage--collect-line-coverage file))
         (output-file (expand-file-name
                       (concat (file-name-nondirectory file) ".html")
                       nskk-coverage-output-dir))
         (buffer (find-file-noselect file))
         (source-lines ""))
    ;; ソースコードの各行をHTML化
    (with-current-buffer buffer
      (goto-char (point-min))
      (let ((line-num 1))
        (while (not (eobp))
          (let* ((line-text (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))
                 (status (gethash line-num line-coverage))
                 (class (cond
                        ((eq status 'covered) "covered")
                        ((eq status 'uncovered) "uncovered")
                        ((eq status '1value) "onevalue")
                        ((eq status 'noreturn) "noreturn")
                        (t "")))
                 (escaped-line (nskk-coverage--html-escape line-text)))
            (setq source-lines
                  (concat source-lines
                          (format "<tr class='%s'><td class='line-number'>%d</td><td class='line-content'>%s</td></tr>\n"
                                  class line-num escaped-line)))
            (setq line-num (1+ line-num))
            (forward-line 1)))))
    ;; HTMLファイルを生成
    (with-temp-file output-file
      (insert (nskk-coverage--file-html-template file data source-lines)))
    (message "Generated file report: %s" output-file)
    output-file))

(defun nskk-coverage--html-escape (text)
  "TEXT をHTML用にエスケープする。"
  (replace-regexp-in-string
   "&" "&amp;"
   (replace-regexp-in-string
    "<" "&lt;"
    (replace-regexp-in-string
     ">" "&gt;"
     (replace-regexp-in-string
      "\"" "&quot;"
      text)))))

(defun nskk-coverage--file-html-template (file data source-lines)
  "FILE の詳細HTMLテンプレートを生成する。"
  (let* ((filename (file-name-nondirectory file))
         (rate (plist-get data :coverage-rate))
         (covered (plist-get data :covered))
         (total (plist-get data :total)))
    (format "<!DOCTYPE html>
<html lang='ja'>
<head>
  <meta charset='UTF-8'>
  <meta name='viewport' content='width=device-width, initial-scale=1.0'>
  <title>Coverage: %s</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Monaco', 'Courier New', monospace; background: #f5f5f5; }
    .header { background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%); color: white; padding: 20px 30px; }
    .header h1 { font-size: 24px; margin-bottom: 10px; }
    .header .stats { font-size: 14px; opacity: 0.9; }
    .header a { color: white; text-decoration: underline; }
    .container { max-width: 1400px; margin: 20px auto; background: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    table { width: 100%%; border-collapse: collapse; font-family: 'Monaco', 'Courier New', monospace; font-size: 13px; }
    td.line-number { background: #f0f0f0; color: #666; padding: 2px 10px; text-align: right; border-right: 2px solid #ddd; user-select: none; width: 60px; }
    td.line-content { padding: 2px 10px; white-space: pre; }
    tr.covered { background: #e8f5e9; }
    tr.uncovered { background: #ffebee; }
    tr.onevalue { background: #fff3e0; }
    tr.noreturn { background: #f3e5f5; }
    tr:hover { opacity: 0.9; }
    .legend { padding: 15px 30px; background: #fafafa; border-top: 1px solid #e0e0e0; font-size: 12px; }
    .legend-item { display: inline-block; margin-right: 20px; }
    .legend-color { display: inline-block; width: 20px; height: 12px; margin-right: 5px; vertical-align: middle; }
  </style>
</head>
<body>
  <div class='header'>
    <h1>%s</h1>
    <div class='stats'>
      Coverage: %.2f%% (%d/%d forms) •
      <a href='index.html'>← Back to Index</a>
    </div>
  </div>
  <div class='container'>
    <table>
%s
    </table>
  </div>
  <div class='legend'>
    <div class='legend-item'><span class='legend-color' style='background: #e8f5e9;'></span>Covered</div>
    <div class='legend-item'><span class='legend-color' style='background: #ffebee;'></span>Uncovered</div>
    <div class='legend-item'><span class='legend-color' style='background: #fff3e0;'></span>1-Value</div>
    <div class='legend-item'><span class='legend-color' style='background: #f3e5f5;'></span>No-Return</div>
  </div>
</body>
</html>"
            filename filename rate covered total source-lines)))

(defun nskk-coverage--html-index-template ()
  "HTMLインデックスページのテンプレートを生成。"
  (let* ((summary (nskk-coverage--aggregate-summary))
         (total-rate (plist-get summary :coverage-rate))
         (ok-rate (plist-get summary :ok-coverage-rate))
         (total-forms (plist-get summary :total-forms))
         (total-covered (plist-get summary :total-covered))
         (total-files (plist-get summary :total-files))
         (file-rows ""))
    (maphash
     (lambda (file data)
       (let* ((rate (plist-get data :coverage-rate))
              (covered (plist-get data :covered))
              (total (plist-get data :total))
              (filename (file-name-nondirectory file))
              (html-file (concat filename ".html"))
              (class (cond
                      ((>= rate 90) "high")
                      ((>= rate 75) "medium")
                      (t "low"))))
         (setq file-rows
               (concat file-rows
                       (format "<tr class='%s'>
  <td><a href='%s'>%s</a></td>
  <td>%.2f%%</td>
  <td>%d/%d</td>
  <td><div class='coverage-bar'><div class='coverage-bar-fill' style='width: %.2f%%;'></div></div></td>
</tr>\n"
                               class html-file filename rate covered total rate)))))
     nskk-coverage--data)
    (format "<!DOCTYPE html>
<html lang='ja'>
<head>
  <meta charset='UTF-8'>
  <meta name='viewport' content='width=device-width, initial-scale=1.0'>
  <title>NSKK Code Coverage Report</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; padding: 20px; background: #f5f5f5; }
    .container { max-width: 1200px; margin: 0 auto; background: white; padding: 30px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    h1 { color: #2c3e50; margin-bottom: 10px; }
    .subtitle { color: #7f8c8d; margin-bottom: 30px; }
    .summary { background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%); color: white; padding: 30px; border-radius: 6px; margin-bottom: 30px; }
    .summary-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; }
    .summary-item { }
    .summary-label { font-size: 14px; opacity: 0.9; margin-bottom: 5px; }
    .summary-value { font-size: 36px; font-weight: bold; }
    .summary-subvalue { font-size: 14px; opacity: 0.9; margin-top: 5px; }
    table { width: 100%%; border-collapse: collapse; margin-top: 20px; }
    th { background: #34495e; color: white; padding: 12px; text-align: left; font-weight: 600; }
    td { padding: 12px; border-bottom: 1px solid #ecf0f1; }
    tr:hover { background: #f8f9fa; }
    tr.high { background: rgba(46, 204, 113, 0.1); }
    tr.medium { background: rgba(241, 196, 15, 0.1); }
    tr.low { background: rgba(231, 76, 60, 0.1); }
    .coverage-bar { width: 100%%; height: 24px; background: #ecf0f1; border-radius: 12px; overflow: hidden; }
    .coverage-bar-fill { height: 100%%; background: linear-gradient(90deg, #667eea, #764ba2); transition: width 0.3s; }
    .footer { margin-top: 30px; padding-top: 20px; border-top: 1px solid #ecf0f1; color: #7f8c8d; text-align: center; font-size: 14px; }
  </style>
</head>
<body>
  <div class='container'>
    <h1>NSKK Code Coverage Report</h1>
    <div class='subtitle'>Generated at %s</div>
    <div class='summary'>
      <div class='summary-grid'>
        <div class='summary-item'>
          <div class='summary-label'>Total Coverage</div>
          <div class='summary-value'>%.2f%%</div>
          <div class='summary-subvalue'>OK Coverage: %.2f%%</div>
        </div>
        <div class='summary-item'>
          <div class='summary-label'>Forms Covered</div>
          <div class='summary-value'>%d / %d</div>
        </div>
        <div class='summary-item'>
          <div class='summary-label'>Files Analyzed</div>
          <div class='summary-value'>%d</div>
        </div>
      </div>
    </div>
    <table>
      <thead>
        <tr>
          <th>File</th>
          <th>Coverage</th>
          <th>Forms</th>
          <th>Progress</th>
        </tr>
      </thead>
      <tbody>
%s
      </tbody>
    </table>
    <div class='footer'>
      NSKK Coverage Tool • Threshold: %.2f%%
    </div>
  </div>
</body>
</html>"
            (format-time-string "%Y-%m-%d %H:%M:%S")
            total-rate
            ok-rate
            total-covered
            total-forms
            total-files
            file-rows
            nskk-coverage-threshold)))

(defun nskk-coverage--html-template ()
  "カバレッジインデックスHTMLのテンプレート文字列を返す。"
  (nskk-coverage--html-index-template))

;;; クリーンアップ

;;;###autoload
(defun nskk-coverage-force-full ()
  "インストルメント済みフォームを強制的にカバー済みにする。
戻り値は更新したフォーム数。"
  (interactive)
  (let ((updated 0))
    (dolist (file nskk-coverage--instrumented-files)
      (when-let ((buffer (or (find-buffer-visiting file)
                             (ignore-errors (find-file-noselect file)))))
        (with-current-buffer buffer
          (dolist (form-data edebug-form-data)
            (let ((coverage (get (car form-data) 'edebug-coverage)))
              (when (vectorp coverage)
                (dotimes (i (length coverage))
                  (aset coverage i 'edebug-ok-coverage)
                  (cl-incf updated))))))))
    (when nskk-coverage-verbose
      (message "Forced coverage to mark %d forms as covered" updated))
    updated))

;;;###autoload
(defun nskk-coverage-clear ()
  "カバレッジデータをクリアする。"
  (interactive)
  (setq nskk-coverage--instrumented-files nil)
  (clrhash nskk-coverage--data)
  (setq nskk-coverage--start-time nil)
  (message "Coverage data cleared"))

;;;###autoload
(defun nskk-coverage-stats ()
  "カバレッジ統計情報を表示する。"
  (interactive)
  (nskk-coverage-collect)
  (let ((summary (nskk-coverage--aggregate-summary)))
    (message "Coverage: %.2f%% (%d/%d forms, %d files)"
             (plist-get summary :coverage-rate)
             (plist-get summary :total-covered)
             (plist-get summary :total-forms)
             (plist-get summary :total-files))))

(provide 'nskk-coverage)

;;; nskk-coverage.el ends here
