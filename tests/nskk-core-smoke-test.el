;;; nskk-core-smoke-test.el --- Minimal smoke tests for NSKK -*- lexical-binding: t; -*-

(require 'ert)
(require 'nskk)

(ert-deftest nskk-core-smoke-test-require ()
  "`nskk` がロードできることを検証する。"
  (should (featurep 'nskk))
  (should (boundp 'nskk-version))
  (should (stringp nskk-version)))

(ert-deftest nskk-core-smoke-test-mode-toggle ()
  "`nskk-mode` のオン・オフが出来ることを確認する。"
  (with-temp-buffer
    (let ((nskk-runtime-integration-enable-threading nil)
          (nskk-runtime-integration-enable-async-ui nil)
          (nskk-runtime-integration-enable-optimization nil)
          (nskk-runtime-integration-enable-auto-tune nil)
          (nskk-advanced-integration-enable-ai nil)
          (nskk-advanced-integration-enable-sync nil)
          (nskk-advanced-integration-enable-analytics nil))
      (nskk-mode 1)
      (should nskk-mode)
      (nskk-mode -1)
      (should-not nskk-mode))))

(ert-deftest nskk-core-smoke-test-initialize-shutdown ()
  "`nskk-initialize` と `nskk-shutdown` が例外を出さないことを確認する。"
  (let ((nskk-runtime-integration-enable-threading nil)
        (nskk-runtime-integration-enable-async-ui nil)
        (nskk-runtime-integration-enable-optimization nil)
        (nskk-runtime-integration-enable-auto-tune nil)
        (nskk-advanced-integration-enable-ai nil)
        (nskk-advanced-integration-enable-sync nil)
        (nskk-advanced-integration-enable-analytics nil))
    (nskk-initialize)
    (should (nskk-runtime-integration-initialized-p))
    (nskk-shutdown)
    (should-not (nskk-runtime-integration-initialized-p))))

(provide 'nskk-core-smoke-test)

;;; nskk-core-smoke-test.el ends here
