;;; nskk-scenario-suite.el --- Scenario test suite for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, testing, scenario
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:

;; NSKKシナリオテストスイート
;;
;; 使用方法:
;;   M-x nskk-scenario-run-all       ; 全シナリオ実行
;;   M-x nskk-scenario-run-basic     ; 基本シナリオのみ
;;   M-x nskk-scenario-run-beginner  ; 初心者向けシナリオのみ

;;; Code:

(require 'nskk-scenario-dsl)

;; 基本シナリオをロード
(require 'nskk-scenario-hiragana)
(require 'nskk-scenario-katakana)
(require 'nskk-scenario-kanji)

;;; 実行コマンド

(defun nskk-scenario-run-all ()
  "全シナリオテストを実行する。"
  (interactive)
  (ert-run-tests-interactively "^.*-scenario$"))

(defun nskk-scenario-run-basic ()
  "基本シナリオ(:basic タグ)のみを実行する。"
  (interactive)
  (ert-run-tests-interactively '(tag :basic)))

(defun nskk-scenario-run-beginner ()
  "初心者向けシナリオ(:beginner タグ)のみを実行する。"
  (interactive)
  (ert-run-tests-interactively '(tag :beginner)))

(defun nskk-scenario-run-intermediate ()
  "中級者向けシナリオ(:intermediate タグ)のみを実行する。"
  (interactive)
  (ert-run-tests-interactively '(tag :intermediate)))

(defun nskk-scenario-run-hiragana ()
  "ひらがな関連シナリオのみを実行する。"
  (interactive)
  (ert-run-tests-interactively '(tag :hiragana)))

(defun nskk-scenario-run-katakana ()
  "カタカナ関連シナリオのみを実行する。"
  (interactive)
  (ert-run-tests-interactively '(tag :katakana)))

(defun nskk-scenario-run-kanji ()
  "漢字変換関連シナリオのみを実行する。"
  (interactive)
  (ert-run-tests-interactively '(tag :kanji)))

;;; バッチモード実行サポート

(defun nskk-scenario-run-all-batch ()
  "バッチモードで全シナリオを実行する。"
  (ert-run-tests-batch-and-exit "^.*-scenario$"))

(defun nskk-scenario-run-basic-batch ()
  "バッチモードで基本シナリオを実行する。"
  (ert-run-tests-batch-and-exit '(tag :basic)))

;;; 統計情報

(defun nskk-scenario-list-all ()
  "登録されている全シナリオをリスト表示する。"
  (interactive)
  (let ((scenarios (ert-select-tests "^.*-scenario$" t)))
    (with-output-to-temp-buffer "*NSKK Scenarios*"
      (princ "=== NSKK Scenario Tests ===\n\n")
      (princ (format "Total: %d scenarios\n\n" (length scenarios)))
      (dolist (test scenarios)
        (let* ((test-name (ert-test-name test))
               (test-doc (ert-test-documentation test))
               (test-tags (ert-test-tags test)))
          (princ (format "- %s\n" test-name))
          (when test-doc
            (princ (format "  %s\n" test-doc)))
          (when test-tags
            (princ (format "  Tags: %s\n" test-tags)))
          (princ "\n"))))))

(defun nskk-scenario-stats ()
  "シナリオテストの統計情報を表示する。"
  (interactive)
  (let* ((all-scenarios (ert-select-tests "^.*-scenario$" t))
         (basic-count (length (ert-select-tests '(tag :basic) t)))
         (beginner-count (length (ert-select-tests '(tag :beginner) t)))
         (intermediate-count (length (ert-select-tests '(tag :intermediate) t))))
    (message "NSKK Scenarios: Total=%d, Basic=%d, Beginner=%d, Intermediate=%d"
             (length all-scenarios)
             basic-count
             beginner-count
             intermediate-count)))

(provide 'nskk-scenario-suite)
;;; nskk-scenario-suite.el ends here
