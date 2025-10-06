;;; test-key-simulation.el --- Simulate actual key input for NSKK

;; NSKKの全モジュールをロード
(add-to-list 'load-path ".")
(load "nskk.el")

(message "=== NSKK Key Input Simulation Test ===")

;; テスト用バッファを作成
(with-temp-buffer
  (message "\n[Setup] Creating test buffer")

  ;; バッファローカルな状態を初期化
  (setq-local nskk-current-state (nskk-state-create))

  (message "✓ State created: %S" nskk-current-state)

  ;; NSKKモードを有効化（簡易版）
  (message "\n[Test 1] Simulating NSKK mode activation")
  (condition-case err
      (progn
        ;; キーマップの初期化
        (when (boundp 'nskk-mode-map)
          (use-local-map nskk-mode-map))
        (message "✓ PASS: Mode activation simulated"))
    (error
     (message "✗ FAIL: %s" (error-message-string err))))

  ;; 入力バッファへの文字追加をシミュレート
  (message "\n[Test 2] Simulating character input 'a'")
  (condition-case err
      (progn
        (nskk-application--set-input-buffer "a")
        (let ((result (nskk-state-input-buffer nskk-current-state)))
          (if (string= result "a")
              (message "✓ PASS: Character 'a' added to input buffer (got: %s)" result)
            (message "✗ FAIL: Wrong buffer content (expected: a, got: %s)" result))))
    (error
     (message "✗ FAIL: %s" (error-message-string err))
     (message "Stack trace: %S" err)))

  ;; 複数文字の入力をシミュレート
  (message "\n[Test 3] Simulating multiple character input 'ka'")
  (condition-case err
      (progn
        (nskk-application--set-input-buffer "")
        (nskk-application--append-input "k")
        (nskk-application--append-input "a")
        (let ((result (nskk-state-input-buffer nskk-current-state)))
          (if (string= result "ka")
              (message "✓ PASS: Characters 'ka' added to input buffer (got: %s)" result)
            (message "✗ FAIL: Wrong buffer content (expected: ka, got: %s)" result))))
    (error
     (message "✗ FAIL: %s" (error-message-string err))
     (message "Stack trace: %S" err)))

  ;; 入力バッファのリセットをシミュレート
  (message "\n[Test 4] Simulating input buffer reset")
  (condition-case err
      (progn
        (nskk-application--set-input-buffer "")
        (let ((result (nskk-state-input-buffer nskk-current-state)))
          (if (string= result "")
              (message "✓ PASS: Input buffer reset (got: %s)" (if (string= result "") "empty" result))
            (message "✗ FAIL: Buffer not empty (expected: empty, got: %s)" result))))
    (error
     (message "✗ FAIL: %s" (error-message-string err))
     (message "Stack trace: %S" err)))

  ;; 状態遷移をシミュレート
  (message "\n[Test 5] Simulating mode transition")
  (condition-case err
      (progn
        (setf (nskk-state-mode nskk-current-state) 'hiragana)
        (nskk-application--set-input-buffer "a")
        (setf (nskk-state-mode nskk-current-state) 'katakana)
        (let ((mode (nskk-state-mode nskk-current-state))
              (input (nskk-state-input-buffer nskk-current-state)))
          (if (and (eq mode 'katakana) (string= input "a"))
              (message "✓ PASS: Mode transition successful (mode=%s, input=%s)" mode input)
            (message "✗ FAIL: Mode transition failed (mode=%s, input=%s)" mode input))))
    (error
     (message "✗ FAIL: %s" (error-message-string err))
     (message "Stack trace: %S" err))))

(message "\n=== Simulation Test Complete ===")

;;; test-key-simulation.el ends here
