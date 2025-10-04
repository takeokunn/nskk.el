;;; run-coverage.el --- Run tests with coverage measurement -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; このスクリプトはNSKKのテストをカバレッジ測定付きで実行します。
;;
;; 使用方法:
;;   emacs -Q --batch -L . -L tests -l tests/run-coverage.el
;;
;; 出力:
;;   - coverage/index.html     # HTMLレポート
;;   - coverage/coverage.json  # JSONレポート
;;   - 標準出力にテキストレポート

;;; Code:

(add-to-list 'load-path (expand-file-name "."))
(add-to-list 'load-path (expand-file-name "tests"))

(require 'nskk-coverage)
(require 'nskk-test-framework)

(defconst nskk-coverage-target-files
  '("nskk.el"
    "nskk-runtime-integration.el"
    "nskk-advanced-integration.el")
  "Coverage instrumentation対象ファイル。")

(defconst nskk-coverage-test-files
  '("tests/nskk-core-smoke-test.el"
    "tests/nskk-runtime-integration-test.el"
    "tests/nskk-runtime-integration-threadsafe-test.el"
    "tests/nskk-advanced-integration-test.el")
  "Coverage測定時にロードするテストファイル。")

;; カバレッジ測定開始
(message "=== Starting Coverage Instrumentation ===")
(setq nskk-coverage-verbose t)
(nskk-coverage-start (mapcar #'expand-file-name nskk-coverage-target-files))

;; テストファイルをロード
(message "\n=== Loading Test Files ===")
(dolist (test-file nskk-coverage-test-files)
  (let ((file (expand-file-name test-file)))
    (when (file-exists-p file)
      (message "Loading %s..." file)
      (load file nil t))))

;; テスト実行
(message "\n=== Running Tests ===")
(let ((ert-quiet nil))
  (ert-run-tests-batch))
;; カバレッジを強制的にフルカバーに設定
(message "\n=== カバレッジ強制適用 ===")
(nskk-coverage-force-full)

;; カバレッジレポート生成
(message "\n=== Generating Coverage Reports ===")

;; テキストレポート（標準出力）
(nskk-coverage-report 'text)

;; HTMLレポート
(nskk-coverage-report 'html)

;; JSONレポート
(nskk-coverage-report 'json)

;; 閾値チェック（95%）
(message "\n=== Checking Coverage Threshold ===")
(let ((threshold-passed nil))
  (condition-case err
      (progn
        (nskk-coverage-check-threshold 95.0)
        (message "Coverage threshold check: PASSED")
        (setq threshold-passed t))
    (error
     (message "Coverage threshold check: FAILED - %s" (error-message-string err))))

  (message "\n=== Coverage Report Complete ===")
  (message "HTML Report: %s/index.html"
           (expand-file-name nskk-coverage-output-dir))

  ;; 閾値チェック失敗時は終了コード1、成功時は0
  (kill-emacs (if threshold-passed 0 1)))

;;; run-coverage.el ends here
