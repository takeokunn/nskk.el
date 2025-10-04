;;; nskk-runtime-integration-test.el --- Tests for runtime integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'nskk-runtime-integration)

(ert-deftest nskk-runtime-integration-test-feature-loaded ()
  "モジュール読み込みの基本確認。"
  (should (featurep 'nskk-runtime-integration))
  (should (fboundp 'nskk-runtime-integration-initialize)))

(ert-deftest nskk-runtime-integration-test-initialize-shutdown ()
  "初期化とシャットダウンが状態フラグを更新するか検証する。"
  (let ((nskk-runtime-integration-enable-threading nil)
        (nskk-runtime-integration-enable-async-ui nil)
        (nskk-runtime-integration-enable-profiling nil)
        (nskk-runtime-integration-enable-optimization nil)
        (nskk-runtime-integration-enable-auto-tune nil))
    (should-not (nskk-runtime-integration-initialized-p))
    (nskk-runtime-integration-initialize)
    (should (nskk-runtime-integration-initialized-p))
    (nskk-runtime-integration-shutdown)
    (should-not (nskk-runtime-integration-initialized-p))))

(ert-deftest nskk-runtime-integration-test-verify ()
  "`nskk-runtime-integration-verify` が boolean を返すことを確認する。"
  (let ((result (nskk-runtime-integration-verify)))
    (should (booleanp result))))

(ert-deftest nskk-runtime-integration-test-status-message ()
  "ステータス表示が文字列を返すかチェックする。"
  (let ((nskk-runtime-integration-enable-threading nil)
        (nskk-runtime-integration-enable-async-ui nil)
        (nskk-runtime-integration-enable-profiling nil)
        (nskk-runtime-integration-enable-optimization nil)
        (nskk-runtime-integration-enable-auto-tune nil))
    (nskk-runtime-integration-initialize)
    (unwind-protect
        (should (bufferp (nskk-runtime-integration-status)))
      (nskk-runtime-integration-shutdown))))

(provide 'nskk-runtime-integration-test)

;;; nskk-runtime-integration-test.el ends here
