;;; test-e2e-setter.el --- End-to-end setter test for NSKK

;; このテストは実際のNSKKのロード後に実行される完全なE2Eテスト

(require 'nskk)

(message "\n========================================")
(message "NSKK End-to-End Setter Test")
(message "========================================\n")

(defvar nskk-e2e-test-results '())

(defun nskk-e2e-record-result (test-name passed message)
  "テスト結果を記録する。"
  (push (list test-name passed message) nskk-e2e-test-results)
  (message "%s [%s] %s"
           (if passed "✓" "✗")
           test-name
           message))

(defun nskk-e2e-run-tests ()
  "E2Eテストを実行する。"

  ;; Test 1: 基本的なsetter
  (condition-case err
      (let ((state (nskk-state-create)))
        (setf (nskk-state-input-buffer state) "test1")
        (nskk-e2e-record-result
         "Basic Setter"
         (string= (nskk-state-input-buffer state) "test1")
         (format "Got: %s" (nskk-state-input-buffer state))))
    (error
     (nskk-e2e-record-result
      "Basic Setter"
      nil
      (format "Error: %s" (error-message-string err)))))

  ;; Test 2: 全スロットのsetter
  (condition-case err
      (let ((state (nskk-state-create))
            (all-ok t))
        (setf (nskk-state-mode state) 'katakana)
        (setf (nskk-state-submode state) 'okuri-ari)
        (setf (nskk-state-input-buffer state) "abc")
        (setf (nskk-state-conversion-buffer state) "def")
        (setf (nskk-state-candidates state) '("x" "y"))
        (setf (nskk-state-candidate-index state) 5)

        (unless (eq (nskk-state-mode state) 'katakana) (setq all-ok nil))
        (unless (eq (nskk-state-submode state) 'okuri-ari) (setq all-ok nil))
        (unless (string= (nskk-state-input-buffer state) "abc") (setq all-ok nil))
        (unless (string= (nskk-state-conversion-buffer state) "def") (setq all-ok nil))
        (unless (equal (nskk-state-candidates state) '("x" "y")) (setq all-ok nil))
        (unless (= (nskk-state-candidate-index state) 5) (setq all-ok nil))

        (nskk-e2e-record-result
         "All Slots"
         all-ok
         (format "All %d slots tested" 6)))
    (error
     (nskk-e2e-record-result
      "All Slots"
      nil
      (format "Error: %s" (error-message-string err)))))

  ;; Test 3: アプリケーション層のsetter
  (condition-case err
      (with-temp-buffer
        (setq-local nskk-current-state (nskk-state-create))
        (nskk-application--set-input-buffer "app-test")
        (nskk-e2e-record-result
         "Application Layer"
         (string= (nskk-state-input-buffer nskk-current-state) "app-test")
         (format "Got: %s" (nskk-state-input-buffer nskk-current-state))))
    (error
     (nskk-e2e-record-result
      "Application Layer"
      nil
      (format "Error: %s" (error-message-string err)))))

  ;; Test 4: append操作
  (condition-case err
      (with-temp-buffer
        (setq-local nskk-current-state (nskk-state-create))
        (setq-local nskk-application--input-buffer "")
        (nskk-application--append-input "k")
        (nskk-application--append-input "a")
        (nskk-e2e-record-result
         "Append Operation"
         (string= (nskk-state-input-buffer nskk-current-state) "ka")
         (format "Got: %s" (nskk-state-input-buffer nskk-current-state))))
    (error
     (nskk-e2e-record-result
      "Append Operation"
      nil
      (format "Error: %s" (error-message-string err)))))

  ;; Test 5: reset操作
  (condition-case err
      (with-temp-buffer
        (setq-local nskk-current-state (nskk-state-create))
        (setq-local nskk-application--input-buffer "something")
        (nskk-application--set-input-buffer "")
        (nskk-e2e-record-result
         "Reset Operation"
         (string= (nskk-state-input-buffer nskk-current-state) "")
         "Buffer reset successful"))
    (error
     (nskk-e2e-record-result
      "Reset Operation"
      nil
      (format "Error: %s" (error-message-string err)))))

  ;; 結果のサマリー
  (message "\n========================================")
  (message "Test Summary")
  (message "========================================")
  (let ((total (length nskk-e2e-test-results))
        (passed (cl-count-if (lambda (r) (nth 1 r)) nskk-e2e-test-results)))
    (message "Total:  %d" total)
    (message "Passed: %d" passed)
    (message "Failed: %d" (- total passed))
    (message "\n%s"
             (if (= passed total)
                 "✓ ALL TESTS PASSED"
               "✗ SOME TESTS FAILED")))
  (message "========================================\n"))

;; テストを実行
(nskk-e2e-run-tests)

(provide 'test-e2e-setter)
;;; test-e2e-setter.el ends here
