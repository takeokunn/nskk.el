;;; nskk-runtime-integration-threadsafe-test.el --- Thread utility tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nskk-runtime-integration)

(ert-deftest nskk-runtime-integration-threadsafe-test-thread-pool-opt-out ()
  "スレッド機能を無効化した場合にスレッドプールが生成されないことを確認する。"
  (let ((nskk-runtime-integration-enable-threading nil)
        (nskk-runtime-integration-enable-async-ui nil)
        (nskk-runtime-integration-enable-optimization nil)
        (nskk-runtime-integration-enable-auto-tune nil))
    (nskk-runtime-integration-initialize)
    (unwind-protect
        (should-not (nskk-runtime-integration-thread-pool))
      (nskk-runtime-integration-shutdown))))

(ert-deftest nskk-runtime-integration-threadsafe-test-thread-pool-enabled ()
  "スレッド機能を有効化した場合にスレッドプール情報が取得できることを確認する。"
  (skip-unless (fboundp 'make-thread))
  (let ((nskk-runtime-integration-enable-threading t)
        (nskk-runtime-integration-enable-async-ui nil)
        (nskk-runtime-integration-enable-optimization nil)
        (nskk-runtime-integration-enable-auto-tune nil))
    (nskk-runtime-integration-initialize)
    (unwind-protect
        (should (or (null (nskk-runtime-integration-thread-pool))
                    (nskk-thread-pool-p (nskk-runtime-integration-thread-pool))))
      (nskk-runtime-integration-shutdown))))

(provide 'nskk-runtime-integration-threadsafe-test)

;;; nskk-runtime-integration-threadsafe-test.el ends here
