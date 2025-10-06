;;; run-e2e-test.el --- E2E test runner with proper load-path

;; load-pathを設定
(add-to-list 'load-path ".")

;; NSKKをロード
(load "nskk.el")

;; E2Eテストを実行
(load "test-e2e-setter.el")

(provide 'run-e2e-test)
;;; run-e2e-test.el ends here
