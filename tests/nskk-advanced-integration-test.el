;;; nskk-advanced-integration-test.el --- Tests for advanced integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'nskk-advanced-integration)

(ert-deftest nskk-advanced-integration-test-feature-loaded ()
  "モジュールがロードされていることを確認する。"
  (should (featurep 'nskk-advanced-integration)))

(ert-deftest nskk-advanced-integration-test-initialize-disable-all ()
  "全機能を無効化した初期化が成功することを確認する。"
  (let ((nskk-advanced-integration-enable-ai nil)
        (nskk-advanced-integration-enable-sync nil)
        (nskk-advanced-integration-enable-analytics nil))
    (should-not (nskk-advanced-integration-initialized-p))
    (nskk-advanced-integration-initialize)
    (should (nskk-advanced-integration-initialized-p))
    (should-not (nskk-advanced-integration-ai-enabled-p))
    (should-not (nskk-advanced-integration-sync-enabled-p))
    (should-not (nskk-advanced-integration-analytics-enabled-p))
    (nskk-advanced-integration-shutdown)
    (should-not (nskk-advanced-integration-initialized-p))))

(ert-deftest nskk-advanced-integration-test-status-message ()
  "ステータス表示が文字列として返るかを確認する。"
  (let ((nskk-advanced-integration-enable-ai t)
        (nskk-advanced-integration-enable-sync nil)
        (nskk-advanced-integration-enable-analytics t))
    (nskk-advanced-integration-initialize)
    (unwind-protect
        (should (stringp (nskk-advanced-integration-status)))
      (nskk-advanced-integration-shutdown))))

(ert-deftest nskk-advanced-integration-test-health-check ()
  "ヘルスチェックがbooleanを返すことを確認する。"
  (should (booleanp (nskk-advanced-integration-health-check))))

(ert-deftest nskk-advanced-integration-test-integration-ready ()
  "統合テスト準備関数がbooleanを返すことを確認する。"
  (should (booleanp (nskk-advanced-integration-integration-test-ready-p))))

(ert-deftest nskk-advanced-integration-test-sync-guard ()
  "同期URLを設定しない場合に同期が有効化されないことを確認する。"
  (let ((nskk-advanced-integration-enable-ai nil)
        (nskk-advanced-integration-enable-sync t)
        (nskk-advanced-integration-sync-server-url nil)
        (nskk-advanced-integration-enable-analytics nil))
    (nskk-advanced-integration-initialize)
    (unwind-protect
        (should-not (nskk-advanced-integration-sync-enabled-p))
      (nskk-advanced-integration-shutdown))))

;;; AI機能のテスト

(ert-deftest nskk-advanced-integration-test-ai-only ()
  "AI機能のみ有効化するテスト"
  (let ((nskk-advanced-integration-enable-ai t)
        (nskk-advanced-integration-enable-sync nil)
        (nskk-advanced-integration-enable-analytics nil))
    (nskk-advanced-integration-initialize)
    (unwind-protect
        (progn
          (should (nskk-advanced-integration-ai-enabled-p))
          (should-not (nskk-advanced-integration-sync-enabled-p))
          (should-not (nskk-advanced-integration-analytics-enabled-p)))
      (nskk-advanced-integration-shutdown))))

;;; Analytics機能のテスト

(ert-deftest nskk-advanced-integration-test-analytics-only ()
  "Analytics機能のみ有効化するテスト"
  (let ((nskk-advanced-integration-enable-ai nil)
        (nskk-advanced-integration-enable-sync nil)
        (nskk-advanced-integration-enable-analytics t))
    (nskk-advanced-integration-initialize)
    (unwind-protect
        (progn
          (should-not (nskk-advanced-integration-ai-enabled-p))
          (should-not (nskk-advanced-integration-sync-enabled-p))
          (should (nskk-advanced-integration-analytics-enabled-p)))
      (nskk-advanced-integration-shutdown))))

;;; 複数回初期化のテスト

(ert-deftest nskk-advanced-integration-test-double-initialize ()
  "複数回初期化しても問題ないことを確認する。"
  (let ((nskk-advanced-integration-enable-ai nil)
        (nskk-advanced-integration-enable-sync nil)
        (nskk-advanced-integration-enable-analytics nil))
    (nskk-advanced-integration-initialize)
    (should (nskk-advanced-integration-initialized-p))
    ;; 2回目の初期化
    (nskk-advanced-integration-initialize)
    (should (nskk-advanced-integration-initialized-p))
    (nskk-advanced-integration-shutdown)
    (should-not (nskk-advanced-integration-initialized-p))))

;;; シャットダウンのテスト

(ert-deftest nskk-advanced-integration-test-shutdown-idempotent ()
  "未初期化状態でシャットダウンしてもエラーにならないことを確認する。"
  ;; 初期化していない状態でシャットダウン
  (nskk-advanced-integration-shutdown)
  (should-not (nskk-advanced-integration-initialized-p)))

;;; 便利関数のテスト

(ert-deftest nskk-advanced-integration-test-show-dashboard-unavailable ()
  "Analytics無効時のダッシュボード表示テスト"
  (let ((nskk-advanced-integration-enable-analytics nil))
    (nskk-advanced-integration-initialize)
    (unwind-protect
        ;; エラーなく実行できることを確認
        (should-not (nskk-advanced-integration-show-dashboard))
      (nskk-advanced-integration-shutdown))))

(ert-deftest nskk-advanced-integration-test-generate-report-unavailable ()
  "Analytics無効時のレポート生成テスト"
  (let ((nskk-advanced-integration-enable-analytics nil))
    (nskk-advanced-integration-initialize)
    (unwind-protect
        ;; エラーなく実行できることを確認
        (should-not (nskk-advanced-integration-generate-report))
      (nskk-advanced-integration-shutdown))))

(ert-deftest nskk-advanced-integration-test-sync-now-unavailable ()
  "Sync無効時の同期実行テスト"
  (let ((nskk-advanced-integration-enable-sync nil))
    (nskk-advanced-integration-initialize)
    (unwind-protect
        ;; エラーなく実行できることを確認
        (should-not (nskk-advanced-integration-sync-now))
      (nskk-advanced-integration-shutdown))))

;;; ステータス表示のテスト

(ert-deftest nskk-advanced-integration-test-status-display ()
  "ステータス表示が文字列を返すことを確認する。"
  (let ((nskk-advanced-integration-enable-ai t)
        (nskk-advanced-integration-enable-sync nil)
        (nskk-advanced-integration-enable-analytics nil))
    (nskk-advanced-integration-initialize)
    (unwind-protect
        (let ((status (nskk-advanced-integration-status)))
          (should (stringp status))
          (should (string-match-p "initialized" status)))
      (nskk-advanced-integration-shutdown))))

;;; 統合テスト準備チェックのテスト

(ert-deftest nskk-advanced-integration-test-integration-test-ready ()
  "統合テスト準備チェックがbooleanを返すことを確認する。"
  (let ((result (nskk-advanced-integration-integration-test-ready-p)))
    (should (booleanp result))))

;;; 全機能有効化のテスト

(ert-deftest nskk-advanced-integration-test-all-features-enabled ()
  "全機能を有効化した場合のテスト"
  (let ((nskk-advanced-integration-enable-ai t)
        (nskk-advanced-integration-enable-sync nil) ;; URLがないので無効
        (nskk-advanced-integration-sync-server-url nil)
        (nskk-advanced-integration-enable-analytics t))
    (nskk-advanced-integration-initialize)
    (unwind-protect
        (progn
          (should (nskk-advanced-integration-initialized-p))
          (should (nskk-advanced-integration-ai-enabled-p))
          (should-not (nskk-advanced-integration-sync-enabled-p))
          (should (nskk-advanced-integration-analytics-enabled-p)))
      (nskk-advanced-integration-shutdown))))

(provide 'nskk-advanced-integration-test)

;;; nskk-advanced-integration-test.el ends here
