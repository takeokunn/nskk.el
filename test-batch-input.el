;;; test-batch-input.el --- Batch mode test for NSKK

;; NSKKの全モジュールをロード
(add-to-list 'load-path ".")
(load "nskk.el")

(message "=== NSKK Batch Mode Test ===")

;; テスト1: nskk-stateのsetter
(message "\n[Test 1] nskk-state setter test")
(condition-case err
    (let ((state (nskk-state-create)))
      (setf (nskk-state-input-buffer state) "test-value")
      (let ((result (nskk-state-input-buffer state)))
        (if (string= result "test-value")
            (message "✓ PASS: Setter works correctly (got: %s)" result)
          (message "✗ FAIL: Setter returned wrong value (expected: test-value, got: %s)" result))))
  (error
   (message "✗ FAIL: %s" (error-message-string err))
   (message "Stack trace: %S" err)))

;; テスト2: 全スロットのsetter
(message "\n[Test 2] All slots setter test")
(condition-case err
    (let ((state (nskk-state-create)))
      (setf (nskk-state-mode state) 'katakana)
      (setf (nskk-state-submode state) 'okuri-ari)
      (setf (nskk-state-input-buffer state) "input")
      (setf (nskk-state-conversion-buffer state) "conv")
      (setf (nskk-state-candidates state) '("cand1" "cand2"))
      (setf (nskk-state-candidate-index state) 1)
      (setf (nskk-state-okuri-char state) ?a)
      (message "✓ PASS: All setters executed without error")
      (message "  mode=%s submode=%s input-buffer=%s"
               (nskk-state-mode state)
               (nskk-state-submode state)
               (nskk-state-input-buffer state)))
  (error
   (message "✗ FAIL: %s" (error-message-string err))
   (message "Stack trace: %S" err)))

;; テスト3: アプリケーション層のsetter
(message "\n[Test 3] Application layer setter test")
(condition-case err
    (progn
      ;; グローバル状態を初期化
      (setq nskk-current-state (nskk-state-create))
      (nskk-application--set-input-buffer "app-test")
      (let ((result (nskk-state-input-buffer nskk-current-state)))
        (if (string= result "app-test")
            (message "✓ PASS: Application setter works (got: %s)" result)
          (message "✗ FAIL: Application setter failed (expected: app-test, got: %s)" result))))
  (error
   (message "✗ FAIL: %s" (error-message-string err))
   (message "Stack trace: %S" err)))

;; テスト4: バッファコンテキストでの動作
(message "\n[Test 4] Buffer context test")
(condition-case err
    (with-temp-buffer
      (setq-local nskk-current-state (nskk-state-create))
      (nskk-application--set-input-buffer "buffer-test")
      (let ((result (nskk-state-input-buffer nskk-current-state)))
        (if (string= result "buffer-test")
            (message "✓ PASS: Buffer-local setter works (got: %s)" result)
          (message "✗ FAIL: Buffer-local setter failed (expected: buffer-test, got: %s)" result))))
  (error
   (message "✗ FAIL: %s" (error-message-string err))
   (message "Stack trace: %S" err)))

;; テスト5: 繰り返しsetter呼び出し
(message "\n[Test 5] Repeated setter calls test")
(condition-case err
    (let ((state (nskk-state-create)))
      (dotimes (i 10)
        (setf (nskk-state-input-buffer state) (format "test-%d" i)))
      (let ((result (nskk-state-input-buffer state)))
        (if (string= result "test-9")
            (message "✓ PASS: Repeated setters work (got: %s)" result)
          (message "✗ FAIL: Repeated setters failed (expected: test-9, got: %s)" result))))
  (error
   (message "✗ FAIL: %s" (error-message-string err))
   (message "Stack trace: %S" err)))

(message "\n=== Test Complete ===")

;;; test-batch-input.el ends here
