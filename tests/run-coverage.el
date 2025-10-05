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
  (mapcar (lambda (file) (expand-file-name file))
          '(;; Phase 1: Core Engine (最優先)
            "nskk-romaji-tables.el"
            "nskk-converter.el"
            "nskk-special-chars.el"
            "nskk-optimize.el"
            "nskk-state.el"
            "nskk-mode-switch.el"
            "nskk-buffer.el"
            "nskk-events.el"
            "nskk-dict-parser.el"
            "nskk-dict-struct.el"
            "nskk-dict-io.el"
            "nskk-dict-errors.el"
            ;; Phase 2: Search & UI
            "nskk-trie.el"
            "nskk-search.el"
            "nskk-cache.el"
            "nskk-keymap.el"
            "nskk-candidate-window.el"
            "nskk-minibuffer.el"
            "nskk-modeline.el"
            ;; Phase 3: Learning & Advanced Features
            "nskk-learning-frequency.el"
            "nskk-learning-persist.el"
            "nskk-learning-context.el"
            "nskk-history.el"
            "nskk-verb-conjugation.el"
            "nskk-adjective-conjugation.el"
            "nskk-complex-conjugation.el"
            "nskk-conjugation-tables.el"
            "nskk-annotation-parser.el"
            "nskk-annotation-display.el"
            "nskk-custom-annotation.el"
            ;; Integration files
            "nskk.el"
            "nskk-runtime-integration.el"
            "nskk-advanced-integration.el"))
  "Coverage instrumentation対象ファイル（テスト済みコアファイル33個）。")

(defconst nskk-coverage-test-files
  (mapcar (lambda (file) (expand-file-name file "tests"))
          '(;; Phase 1: Core Engine
            "nskk-romaji-tables-test.el"
            "nskk-converter-test.el"
            "nskk-special-chars-test.el"
            "nskk-optimize-test.el"
            "nskk-state-test.el"
            "nskk-mode-switch-test.el"
            "nskk-buffer-test.el"
            "nskk-events-test.el"
            "nskk-dict-parser-test.el"
            "nskk-dict-struct-test.el"
            "nskk-dict-io-test.el"
            "nskk-dict-errors-test.el"
            ;; Phase 2: Search & UI
            "nskk-trie-test.el"
            "nskk-search-test.el"
            "nskk-cache-test.el"
            "nskk-keymap-test.el"
            "nskk-candidate-window-test.el"
            "nskk-minibuffer-test.el"
            "nskk-modeline-test.el"
            ;; Phase 3: Learning & Advanced Features
            "nskk-learning-frequency-test.el"
            "nskk-learning-persist-test.el"
            "nskk-learning-context-test.el"
            "nskk-history-test.el"
            "nskk-verb-conjugation-test.el"
            "nskk-adjective-conjugation-test.el"
            "nskk-complex-conjugation-test.el"
            "nskk-conjugation-tables-test.el"
            "nskk-annotation-parser-test.el"
            "nskk-annotation-display-test.el"
            "nskk-custom-annotation-test.el"
            ;; Integration files
            "nskk-test.el"
            "nskk-runtime-integration-test.el"
            "nskk-advanced-integration-test.el"))
  "Coverage測定時にロードするテストファイル（テスト済みコアファイル対応）。")

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

;; カバレッジレポート生成
(message "\n=== Generating Coverage Reports ===")

;; テキストレポート（標準出力）
(nskk-coverage-report 'text)

;; HTMLレポート
(nskk-coverage-report 'html)

;; JSONレポート
(nskk-coverage-report 'json)

;; 閾値チェック（段階的に向上）
(message "\n=== Checking Coverage Threshold ===")
(let ((threshold-passed nil)
      (target-threshold 0.0))  ;; まず現状を把握
  (condition-case err
      (progn
        (nskk-coverage-check-threshold target-threshold)
        (message "Coverage threshold check: PASSED (>= %.2f%%)" target-threshold)
        (setq threshold-passed t))
    (error
     (message "Coverage threshold check: FAILED - %s" (error-message-string err))))

  (message "\n=== Coverage Report Complete ===")
  (message "HTML Report: %s/index.html"
           (expand-file-name nskk-coverage-output-dir))

  ;; 閾値チェック失敗時は終了コード1、成功時は0
  (kill-emacs (if threshold-passed 0 1)))

;;; run-coverage.el ends here
