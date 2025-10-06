;;; verify-nix-package.el --- Verify the built nix package

;; nskk パッケージをロード（nixでビルドされたもの）
(require 'nskk)

(message "\n=== Verifying Nix Built Package ===\n")

;; Test 1: Setter関数の存在確認
(message "[Test 1] Checking if setter is available")
(condition-case err
    (let ((state (nskk-state-create)))
      (setf (nskk-state-input-buffer state) "nix-package-test")
      (message "✓ PASS: Setter works in nix package (got: %s)"
               (nskk-state-input-buffer state)))
  (error
   (message "✗ FAIL: %s" (error-message-string err))
   (message "This means the setter is NOT in the built package!")))

;; Test 2: 全スロットのsetter確認
(message "\n[Test 2] Testing all slot setters")
(condition-case err
    (let ((state (nskk-state-create)))
      (setf (nskk-state-mode state) 'katakana)
      (setf (nskk-state-submode state) 'okuri-ari)
      (setf (nskk-state-input-buffer state) "test")
      (setf (nskk-state-conversion-buffer state) "conv")
      (setf (nskk-state-candidates state) '("a" "b"))
      (setf (nskk-state-candidate-index state) 1)
      (message "✓ PASS: All setters work in nix package"))
  (error
   (message "✗ FAIL: %s" (error-message-string err))))

;; Test 3: アプリケーション層
(message "\n[Test 3] Testing application layer")
(condition-case err
    (with-temp-buffer
      (setq-local nskk-current-state (nskk-state-create))
      (nskk-application--set-input-buffer "app-nix-test")
      (message "✓ PASS: Application layer works (got: %s)"
               (nskk-state-input-buffer nskk-current-state)))
  (error
   (message "✗ FAIL: %s" (error-message-string err))))

(message "\n=== Verification Complete ===\n")
(message "If all tests passed, 'nix run .' should work!")

;;; verify-nix-package.el ends here
