;;; nskk-diagnostic-report-test.el --- Tests for nskk-diagnostic-report -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input-method, skk, test

;;; Commentary:

;; nskk-diagnostic-report関数のテストスイート

;;; Code:

(require 'ert)
(require 'nskk)

(ert-deftest nskk-diagnostic-report-test/function-exists ()
  "nskk-diagnostic-report関数が存在することを確認する。"
  (should (fboundp 'nskk-diagnostic-report))
  (should (commandp 'nskk-diagnostic-report)))

(ert-deftest nskk-diagnostic-report-test/returns-plist ()
  "nskk-diagnostic-reportが正しい形式のplistを返すことを確認する。"
  (let ((result (nskk-diagnostic-report)))
    (should (listp result))
    (should (plist-get result :loaded))
    (should (plist-get result :total))
    (should (numberp (plist-get result :errors)))
    (should (numberp (plist-get result :warnings)))))

(ert-deftest nskk-diagnostic-report-test/module-count ()
  "モジュール数が正しくカウントされることを確認する。"
  (let ((result (nskk-diagnostic-report)))
    (should (= (plist-get result :total) 40))
    (should (<= (plist-get result :loaded) 40))
    (should (>= (plist-get result :loaded) 0))))

(ert-deftest nskk-diagnostic-report-test/creates-buffer ()
  "診断レポートバッファが作成されることを確認する。"
  (nskk-diagnostic-report)
  (should (get-buffer "*NSKK Diagnostic Report*")))

(ert-deftest nskk-diagnostic-report-test/buffer-content ()
  "診断レポートバッファに必要なセクションが含まれることを確認する。"
  (nskk-diagnostic-report)
  (with-current-buffer "*NSKK Diagnostic Report*"
    (let ((content (buffer-string)))
      (should (string-match-p "SYSTEM INFORMATION" content))
      (should (string-match-p "MODULE STATUS" content))
      (should (string-match-p "DICTIONARY FILES" content))
      (should (string-match-p "CONFIGURATION" content))
      (should (string-match-p "INPUT METHOD" content))
      (should (string-match-p "KEYMAP STATUS" content))
      (should (string-match-p "APPLICATION LAYER" content))
      (should (string-match-p "DIAGNOSIS" content)))))

(ert-deftest nskk-diagnostic-report-test/system-info ()
  "システム情報が正しく表示されることを確認する。"
  (nskk-diagnostic-report)
  (with-current-buffer "*NSKK Diagnostic Report*"
    (let ((content (buffer-string)))
      (should (string-match-p "Emacs Version:" content))
      (should (string-match-p "System Type:" content))
      (should (string-match-p "NSKK Version:" content)))))

(ert-deftest nskk-diagnostic-report-test/input-method-registered ()
  "入力メソッドの登録状態が正しく表示されることを確認する。"
  (nskk-diagnostic-report)
  (with-current-buffer "*NSKK Diagnostic Report*"
    (let ((content (buffer-string)))
      (should (string-match-p "\"nskk\" registered" content)))))

(ert-deftest nskk-diagnostic-report-test/keymap-initialized ()
  "キーマップの初期化状態が正しく表示されることを確認する。"
  (nskk-diagnostic-report)
  (with-current-buffer "*NSKK Diagnostic Report*"
    (let ((content (buffer-string)))
      (should (string-match-p "nskk-mode-map initialized" content)))))

(ert-deftest nskk-diagnostic-report-test/config-values ()
  "設定値が正しく表示されることを確認する。"
  (nskk-diagnostic-report)
  (with-current-buffer "*NSKK Diagnostic Report*"
    (let ((content (buffer-string)))
      (should (string-match-p "nskk-dictionary-path:" content))
      (should (string-match-p "nskk-user-dictionary-path:" content))
      (should (string-match-p "nskk-enable-completion:" content))
      (should (string-match-p "nskk-candidate-display-count:" content))
      (should (string-match-p "nskk-debug-mode:" content)))))

(ert-deftest nskk-diagnostic-report-test/no-errors-on-clean-state ()
  "クリーンな状態ではエラーが0件であることを確認する。"
  (let ((result (nskk-diagnostic-report)))
    ;; モジュールが全てロードされている場合、エラーは0件
    (when (= (plist-get result :loaded) 40)
      (should (= (plist-get result :errors) 0)))))

(provide 'nskk-diagnostic-report-test)

;;; nskk-diagnostic-report-test.el ends here
