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

(provide 'nskk-advanced-integration-test)

;;; nskk-advanced-integration-test.el ends here
