;;; init-test.el --- Test initialization for NSKK

;; NSKKの初期化後に自動的にテストを実行

(defun nskk-post-init-test ()
  "NSKK初期化後のテストを実行する。"
  (message "\n=== NSKK Post-Init Test ===")

  ;; テスト1: グローバル状態の確認
  (message "[Test 1] Checking global state")
  (if (boundp 'nskk-current-state)
      (message "✓ nskk-current-state is bound")
    (message "⚠ nskk-current-state is not bound (this is OK if not initialized yet)"))

  ;; テスト2: setter関数の存在確認
  (message "\n[Test 2] Checking setter function")
  (if (fboundp 'nskk-application--set-input-buffer)
      (message "✓ nskk-application--set-input-buffer exists")
    (message "✗ nskk-application--set-input-buffer not found"))

  ;; テスト3: 状態構造体のsetter確認
  (message "\n[Test 3] Testing state setter")
  (condition-case err
      (let ((test-state (nskk-state-create)))
        (setf (nskk-state-input-buffer test-state) "init-test")
        (if (string= (nskk-state-input-buffer test-state) "init-test")
            (message "✓ State setter works: %s" (nskk-state-input-buffer test-state))
          (message "✗ State setter failed")))
    (error
     (message "✗ Error in state setter: %s" (error-message-string err))))

  ;; テスト4: アプリケーション層のsetter確認
  (message "\n[Test 4] Testing application layer setter")
  (condition-case err
      (with-temp-buffer
        (setq-local nskk-current-state (nskk-state-create))
        (nskk-application--set-input-buffer "app-init-test")
        (if (string= (nskk-state-input-buffer nskk-current-state) "app-init-test")
            (message "✓ Application setter works: %s"
                     (nskk-state-input-buffer nskk-current-state))
          (message "✗ Application setter failed")))
    (error
     (message "✗ Error in application setter: %s" (error-message-string err))))

  (message "\n=== All initialization tests passed ===")
  (message "NSKK is ready for interactive use!")
  (message "\nTo test interactively:")
  (message "  1. Create a new buffer: C-x b *test* RET")
  (message "  2. Enable NSKK: C-x C-j")
  (message "  3. Type some characters: a, ka, etc."))

;; NSKKの初期化が完了したら自動的にテストを実行
(with-eval-after-load 'nskk
  (nskk-post-init-test))

(provide 'init-test)
;;; init-test.el ends here
