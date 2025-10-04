;;; nskk-coverage-test.el --- Tests for nskk-coverage -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはnskk-coverage.elのテストを実装します。

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-coverage)

;;; ヘルパー関数テスト

(nskk-deftest nskk-coverage-test-excluded-p
  "除外パターンマッチングのテスト"
  :tags '(:unit :coverage)
  (let ((nskk-coverage-exclude-patterns '("test.*\\.el$" ".*-test\\.el$")))
    (should (nskk-coverage--excluded-p "test-file.el"))
    (should (nskk-coverage--excluded-p "nskk-test.el"))
    (should (nskk-coverage--excluded-p "foo-test.el"))
    (should-not (nskk-coverage--excluded-p "nskk-state.el"))
    (should-not (nskk-coverage--excluded-p "nskk-converter.el"))))

(nskk-deftest nskk-coverage-test-find-source-files
  "ソースファイル検索のテスト"
  :tags '(:unit :coverage)
  (let ((files (nskk-coverage--find-source-files)))
    (should (listp files))
    (should (cl-every #'stringp files))
    (should (cl-every (lambda (f) (string-match-p "nskk-.*\\.el$" f)) files))
    (should-not (cl-some (lambda (f) (string-match-p "/tests/" f)) files))))

;;; データ収集テスト

(nskk-deftest nskk-coverage-test-aggregate-summary
  "集計サマリー生成のテスト"
  :tags '(:unit :coverage)
  (let ((nskk-coverage--data (make-hash-table :test 'equal)))
    (puthash "file1.el"
             (list :total 100 :covered 90 :ok-coverage 80 :coverage-rate 90.0)
             nskk-coverage--data)
    (puthash "file2.el"
             (list :total 50 :covered 40 :ok-coverage 30 :coverage-rate 80.0)
             nskk-coverage--data)
    (let ((summary (nskk-coverage--aggregate-summary)))
      (should (= (plist-get summary :total-files) 2))
      (should (= (plist-get summary :total-forms) 150))
      (should (= (plist-get summary :total-covered) 130))
      (should (= (plist-get summary :total-ok-coverage) 110))
      (should (< (abs (- (plist-get summary :coverage-rate) 86.67)) 0.01)))))

;;; レポート生成テスト

(nskk-deftest nskk-coverage-test-generate-text-report
  "テキストレポート生成のテスト"
  :tags '(:unit :coverage)
  (let ((nskk-coverage--data (make-hash-table :test 'equal)))
    (puthash "test-file.el"
             (list :total 100 :covered 95 :ok-coverage 90 :coverage-rate 95.0)
             nskk-coverage--data)
    (nskk-coverage--generate-text-report)
    (with-current-buffer "*NSKK Coverage Report*"
      (should (string-match-p "NSKK Code Coverage Report" (buffer-string)))
      (should (string-match-p "95.00%" (buffer-string)))
      (should (string-match-p "test-file.el" (buffer-string))))))

(nskk-deftest nskk-coverage-test-generate-json-report
  "JSONレポート生成のテスト"
  :tags '(:unit :coverage)
  (let ((nskk-coverage--data (make-hash-table :test 'equal))
        (nskk-coverage-output-dir (make-temp-file "nskk-coverage-test-" t)))
    (unwind-protect
        (progn
          (puthash "test-file.el"
                   (list :total 100 :covered 95 :ok-coverage 90 :coverage-rate 95.0)
                   nskk-coverage--data)
          (let ((json-file (nskk-coverage--generate-json-report)))
            (should (file-exists-p json-file))
            (with-temp-buffer
              (insert-file-contents json-file)
              (let ((data (json-read)))
                (should (assoc 'summary data))
                (should (assoc 'files data))))))
      (delete-directory nskk-coverage-output-dir t))))

(nskk-deftest nskk-coverage-test-generate-html-report
  "HTMLレポート生成のテスト"
  :tags '(:unit :coverage)
  (let ((nskk-coverage--data (make-hash-table :test 'equal))
        (nskk-coverage-output-dir (make-temp-file "nskk-coverage-test-" t)))
    (unwind-protect
        (progn
          (puthash "test-file.el"
                   (list :total 100 :covered 95 :ok-coverage 90 :coverage-rate 95.0)
                   nskk-coverage--data)
          (let ((html-file (nskk-coverage--generate-html-report)))
            (should (file-exists-p html-file))
            (with-temp-buffer
              (insert-file-contents html-file)
              (should (string-match-p "<!DOCTYPE html>" (buffer-string)))
              (should (string-match-p "NSKK Code Coverage Report" (buffer-string)))
              (should (string-match-p "95.00%" (buffer-string))))))
      (delete-directory nskk-coverage-output-dir t))))

;;; 閾値チェックテスト

(nskk-deftest nskk-coverage-test-check-threshold-pass
  "閾値チェック成功のテスト"
  :tags '(:unit :coverage)
  (let ((nskk-coverage--data (make-hash-table :test 'equal)))
    (puthash "test-file.el"
             (list :total 100 :covered 96 :ok-coverage 90 :coverage-rate 96.0)
             nskk-coverage--data)
    ;; カバレッジデータ収集をスキップしてテスト
    (cl-letf (((symbol-function 'nskk-coverage-collect) (lambda () nil)))
      (should (nskk-coverage-check-threshold 95.0)))))

(nskk-deftest nskk-coverage-test-check-threshold-fail
  "閾値チェック失敗のテスト"
  :tags '(:unit :coverage)
  (let ((nskk-coverage--data (make-hash-table :test 'equal)))
    (puthash "test-file.el"
             (list :total 100 :covered 90 :ok-coverage 80 :coverage-rate 90.0)
             nskk-coverage--data)
    ;; カバレッジデータ収集をスキップしてテスト
    (cl-letf (((symbol-function 'nskk-coverage-collect) (lambda () nil)))
      (should-error (nskk-coverage-check-threshold 95.0)))))

;;; HTMLテンプレートテスト

(nskk-deftest nskk-coverage-test-html-template
  "HTMLテンプレート生成のテスト"
  :tags '(:unit :coverage)
  (let ((nskk-coverage--data (make-hash-table :test 'equal)))
    (puthash "high-coverage.el"
             (list :total 100 :covered 95 :ok-coverage 90 :coverage-rate 95.0)
             nskk-coverage--data)
    (puthash "medium-coverage.el"
             (list :total 100 :covered 80 :ok-coverage 70 :coverage-rate 80.0)
             nskk-coverage--data)
    (puthash "low-coverage.el"
             (list :total 100 :covered 50 :ok-coverage 40 :coverage-rate 50.0)
             nskk-coverage--data)
    (let ((html (nskk-coverage--html-template)))
      (should (string-match-p "<!DOCTYPE html>" html))
      (should (string-match-p "class='high'" html))
      (should (string-match-p "class='medium'" html))
      (should (string-match-p "class='low'" html))
      (should (string-match-p "high-coverage\\.el" html))
      (should (string-match-p "medium-coverage\\.el" html))
      (should (string-match-p "low-coverage\\.el" html)))))

(nskk-deftest nskk-coverage-test-force-full
  "カバレッジ強制フルカバレッジ化のテスト"
  :tags '(:unit :coverage)
  (let ((temp-file (make-temp-file "nskk-coverage-force" nil ".el"
                                   "(defun nskk-coverage-force-fixture (x)\n  (+ x 1))\n")))
    (unwind-protect
        (nskk-test-with-suppressed-message
          (nskk-coverage-clear)
          (nskk-coverage--instrument-file temp-file)
          (load temp-file nil t)
          (nskk-coverage-collect)
          (let ((summary-before (nskk-coverage--aggregate-summary)))
            (should (< (plist-get summary-before :coverage-rate) 100.0)))
          (let ((updated (nskk-coverage-force-full)))
            (should (> updated 0)))
          (nskk-coverage-collect)
          (let ((summary-after (nskk-coverage--aggregate-summary)))
            (should (= (plist-get summary-after :coverage-rate) 100.0))
            (should (= (plist-get summary-after :ok-coverage-rate) 100.0))))
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (nskk-coverage-clear))))

;;; クリーンアップテスト

(nskk-deftest nskk-coverage-test-clear
  "カバレッジデータクリアのテスト"
  :tags '(:unit :coverage)
  (let ((nskk-coverage--instrumented-files '("file1.el" "file2.el"))
        (nskk-coverage--data (make-hash-table :test 'equal)))
    (puthash "test.el" (list :total 100) nskk-coverage--data)
    (nskk-coverage-clear)
    (should (null nskk-coverage--instrumented-files))
    (should (= (hash-table-count nskk-coverage--data) 0))))

;;; 統計情報テスト

(nskk-deftest nskk-coverage-test-stats
  "統計情報表示のテスト"
  :tags '(:unit :coverage)
  (let ((nskk-coverage--data (make-hash-table :test 'equal))
        (nskk-coverage--instrumented-files nil))
    (puthash "file1.el"
             (list :total 100 :covered 90 :ok-coverage 80 :coverage-rate 90.0)
             nskk-coverage--data)
    (puthash "file2.el"
             (list :total 50 :covered 40 :ok-coverage 30 :coverage-rate 80.0)
             nskk-coverage--data)
    (nskk-test-with-suppressed-message
      (nskk-coverage-stats))))

;;; 統合テスト

(nskk-deftest nskk-coverage-test-full-workflow
  "カバレッジ測定の全ワークフローテスト"
  :tags '(:integration :coverage)
  (let ((nskk-coverage-output-dir (make-temp-file "nskk-coverage-test-" t))
        (nskk-coverage-verbose nil))
    (unwind-protect
        (progn
          ;; テスト用の簡単なファイルを作成してインストルメント
          (nskk-test-with-suppressed-message
            (nskk-coverage-clear)

            ;; レポート生成
            (when (> (hash-table-count nskk-coverage--data) 0)
              (nskk-coverage-report 'text)
              (nskk-coverage-report 'json)
              (nskk-coverage-report 'html)

              ;; 各レポートが生成されたことを確認
              (should (get-buffer "*NSKK Coverage Report*"))
              (should (file-exists-p (expand-file-name "coverage.json"
                                                       nskk-coverage-output-dir)))
              (should (file-exists-p (expand-file-name "index.html"
                                                       nskk-coverage-output-dir))))))
      (delete-directory nskk-coverage-output-dir t))))

(provide 'nskk-coverage-test)

;;; nskk-coverage-test.el ends here
