;;; test-cli-input.el --- CLI input test for NSKK

;; テスト用の自動入力シミュレーション

(defun nskk-test-cli-input ()
  "CLI環境でのNSKK入力をテストする。"
  (interactive)
  (message "=== NSKK CLI Input Test Starting ===")

  ;; バッファを作成
  (switch-to-buffer "*nskk-test*")
  (erase-buffer)

  ;; NSKKモードを起動
  (message "Activating NSKK mode...")
  (nskk-mode 1)

  ;; 少し待機
  (sit-for 0.5)

  ;; テスト1: 基本的な文字入力
  (message "Test 1: Basic input 'a'")
  (condition-case err
      (progn
        (execute-kbd-macro (kbd "a"))
        (message "✓ Test 1 passed: No error on 'a' input")
        (sit-for 0.3))
    (error
     (message "✗ Test 1 failed: %s" (error-message-string err))))

  ;; テスト2: ひらがな入力
  (message "Test 2: Hiragana input 'ka'")
  (condition-case err
      (progn
        (execute-kbd-macro (kbd "k a"))
        (message "✓ Test 2 passed: No error on 'ka' input")
        (sit-for 0.3))
    (error
     (message "✗ Test 2 failed: %s" (error-message-string err))))

  ;; テスト3: setter動作確認
  (message "Test 3: Setter functionality")
  (condition-case err
      (let ((state (nskk-state-create)))
        (setf (nskk-state-input-buffer state) "test")
        (if (string= (nskk-state-input-buffer state) "test")
            (message "✓ Test 3 passed: Setter works correctly")
          (message "✗ Test 3 failed: Setter returned wrong value")))
    (error
     (message "✗ Test 3 failed: %s" (error-message-string err))))

  ;; テスト4: アプリケーション層のsetter
  (message "Test 4: Application layer setter")
  (condition-case err
      (progn
        (nskk-application--set-input-buffer "test-input")
        (message "✓ Test 4 passed: Application setter works")
        (sit-for 0.3))
    (error
     (message "✗ Test 4 failed: %s" (error-message-string err))))

  (message "=== NSKK CLI Input Test Complete ===")
  (message "Check buffer content and messages above")

  ;; 5秒後に自動終了
  (run-at-time 5 nil #'save-buffers-kill-emacs))

;; Emacsが起動したら自動的にテストを実行
(add-hook 'emacs-startup-hook #'nskk-test-cli-input)

(provide 'test-cli-input)
;;; test-cli-input.el ends here
