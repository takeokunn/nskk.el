;;; nskk-test-fixtures-test.el --- Tests for nskk-test-fixtures.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-test-fixtures.el のテストスイート
;; フィクスチャとモックシステム自体をテストする

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-fixtures)

;;; テストデータファクトリーのテスト

(ert-deftest nskk-test-fixtures-test-create-state ()
  "状態オブジェクト生成をテストする"
  (let ((state (nskk-fixture-create-state :mode 'katakana)))
    (should (plist-get state :mode))
    (should (eq (plist-get state :mode) 'katakana))
    (should (equal (plist-get state :input-buffer) ""))
    (should (listp (plist-get state :timestamp)))))

(ert-deftest nskk-test-fixtures-test-create-dict-entry ()
  "辞書エントリ生成をテストする"
  (let ((entry (nskk-fixture-create-dict-entry "test" '("テスト" "試験"))))
    (should (consp entry))
    (should (equal (car entry) "test"))
    (should (equal (cdr entry) '("テスト" "試験")))))

(ert-deftest nskk-test-fixtures-test-create-dict-entry-random ()
  "ランダム辞書エントリ生成をテストする"
  (let ((entry (nskk-fixture-create-dict-entry "あ")))
    (should (consp entry))
    (should (equal (car entry) "あ"))
    (should (listp (cdr entry)))
    (should (= (length (cdr entry)) 2))))

(ert-deftest nskk-test-fixtures-test-create-dict-entries ()
  "複数辞書エントリ生成をテストする"
  (let ((entries (nskk-fixture-create-dict-entries 5)))
    (should (listp entries))
    (should (= (length entries) 5))
    (should (cl-every #'consp entries))))

(ert-deftest nskk-test-fixtures-test-create-candidate-list ()
  "候補リスト生成をテストする"
  (let ((candidates (nskk-fixture-create-candidate-list 3)))
    (should (listp candidates))
    (should (= (length candidates) 3))
    (should (cl-every #'stringp candidates))))

(ert-deftest nskk-test-fixtures-test-create-buffer-content ()
  "バッファ内容生成をテストする"
  (should (equal (nskk-fixture-create-buffer-content 'empty) ""))
  (should (stringp (nskk-fixture-create-buffer-content 'simple)))
  (should (string-match-p "\n" (nskk-fixture-create-buffer-content 'complex))))

;;; モック関数のテスト

(ert-deftest nskk-test-fixtures-test-mock-function ()
  "モック関数の基本動作をテストする"
  (nskk-fixture-clear-mock-calls)
  (nskk-with-mock-function 'upcase
    (lambda (&rest args) "MOCKED")
    (should (equal (upcase "test") "MOCKED"))
    (should (equal (upcase "another") "MOCKED")))
  ;; 元の関数が復元される
  (should (equal (upcase "test") "TEST")))

(ert-deftest nskk-test-fixtures-test-mock-call-recording ()
  "モック呼び出し記録をテストする"
  (nskk-fixture-clear-mock-calls)
  (nskk-with-mock-function 'identity
    (lambda (&rest args) args)
    (identity 1 2 3)
    (should (nskk-fixture-was-called-p 'identity))
    (should (= (nskk-fixture-call-count 'identity) 1))
    (should (equal (nskk-fixture-get-call-args 'identity) '(1 2 3)))))

(ert-deftest nskk-test-fixtures-test-mock-multiple-calls ()
  "複数回のモック呼び出しをテストする"
  (nskk-fixture-clear-mock-calls)
  (nskk-with-mock-function 'string-to-number
    (lambda (a) 42)
    (string-to-number "1")
    (string-to-number "3")
    (string-to-number "5")
    (should (= (nskk-fixture-call-count 'string-to-number) 3))
    (should (equal (nskk-fixture-get-call-args 'string-to-number 0) '("1")))
    (should (equal (nskk-fixture-get-call-args 'string-to-number 1) '("3")))
    (should (equal (nskk-fixture-get-call-args 'string-to-number 2) '("5")))))

(ert-deftest nskk-test-fixtures-test-mock-functions ()
  "複数モック関数をテストする"
  (nskk-fixture-clear-mock-calls)
  (nskk-with-mock-functions
      ((upcase (lambda (&rest _) "UP"))
       (downcase (lambda (&rest _) "down")))
    (should (equal (upcase "test") "UP"))
    (should (equal (downcase "TEST") "down"))
    (should (nskk-fixture-was-called-p 'upcase))
    (should (nskk-fixture-was-called-p 'downcase))))

;;; スタブ関数のテスト

(ert-deftest nskk-test-fixtures-test-stub ()
  "スタブ関数の基本動作をテストする"
  (nskk-with-stub 'identity t
    (should (identity nil))
    (should (identity nil)))
  ;; 元の動作に戻る
  (should-not (identity nil)))

(ert-deftest nskk-test-fixtures-test-stubs ()
  "複数スタブ関数をテストする"
  (nskk-with-stubs
      ((upcase "STUB")
       (downcase "stub"))
    (should (equal (upcase "anything") "STUB"))
    (should (equal (downcase "ANYTHING") "stub"))))

;;; 呼び出し検証のテスト

(ert-deftest nskk-test-fixtures-test-assert-called ()
  "呼び出し検証をテストする"
  (nskk-fixture-clear-mock-calls)
  (nskk-with-mock-function 'identity
    (lambda (x) x)
    (identity 42)
    (nskk-fixture-assert-called 'identity)))

(ert-deftest nskk-test-fixtures-test-assert-called-times ()
  "呼び出し回数検証をテストする"
  (nskk-fixture-clear-mock-calls)
  (nskk-with-mock-function 'identity
    (lambda (x) x)
    (identity 1)
    (identity 2)
    (identity 3)
    (nskk-fixture-assert-called-times 'identity 3)))

(ert-deftest nskk-test-fixtures-test-assert-called-with ()
  "引数検証をテストする"
  (nskk-fixture-clear-mock-calls)
  (nskk-with-mock-function 'upcase
    (lambda (&rest args) "RESULT")
    (upcase "test" 'extra 'args)
    (nskk-fixture-assert-called-with 'upcase "test" 'extra 'args)))

(ert-deftest nskk-test-fixtures-test-assert-not-called-fails ()
  "未呼び出し検証の失敗をテストする"
  (nskk-fixture-clear-mock-calls)
  (nskk-with-mock-function 'identity
    (lambda (x) x)
    ;; identityを呼ばない
    (should-error (nskk-fixture-assert-called 'identity))))

;;; フィクスチャ管理のテスト

(ert-deftest nskk-test-fixtures-test-register-fixture ()
  "フィクスチャ登録をテストする"
  (nskk-fixture-register 'test-fixture
    (lambda () "test data"))
  (let ((fixture (nskk-fixture-get 'test-fixture)))
    (should (nskk-fixture-p fixture))
    (should (eq (nskk-fixture-name fixture) 'test-fixture))
    (should (functionp (nskk-fixture-setup fixture)))))

(ert-deftest nskk-test-fixtures-test-with-fixture ()
  "フィクスチャ使用をテストする"
  (nskk-fixture-register 'temp-fixture
    (lambda () (list :value 42)))
  (nskk-with-fixture temp-fixture
    (should (equal fixture-data (list :value 42)))))

(ert-deftest nskk-test-fixtures-test-fixture-teardown ()
  "フィクスチャティアダウンをテストする"
  (let ((torn-down nil))
    (nskk-fixture-register 'cleanup-fixture
      (lambda () "data")
      (lambda (_data) (setq torn-down t)))
    (nskk-with-fixture cleanup-fixture
      (should fixture-data))
    (should torn-down)))

(ert-deftest nskk-test-fixtures-test-standard-fixtures ()
  "標準フィクスチャをテストする"
  (nskk-with-fixture empty-buffer
    (should (bufferp fixture-data))
    (should (buffer-live-p fixture-data)))

  (nskk-with-fixture simple-state
    (should (plist-get fixture-data :mode)))

  (nskk-with-fixture dict-entries
    (should (listp fixture-data))
    (should (= (length fixture-data) 10))))

;;; ヘルパー関数のテスト

(ert-deftest nskk-test-fixtures-test-active-mocks ()
  "アクティブモック一覧取得をテストする"
  (nskk-with-mock-function 'identity
    (lambda (x) x)
    (should (memq 'identity (nskk-fixture-active-mocks))))
  (should-not (memq 'identity (nskk-fixture-active-mocks))))

(ert-deftest nskk-test-fixtures-test-mock-calls-summary ()
  "モック呼び出し要約をテストする"
  (nskk-fixture-clear-mock-calls)
  (nskk-with-mock-functions
      ((upcase (lambda (&rest _) "MOCK"))
       (downcase (lambda (&rest _) "mock")))
    (upcase "a")
    (upcase "b")
    (downcase "C")
    (let ((summary (nskk-fixture-mock-calls-summary)))
      (should (= (cdr (assq 'upcase summary)) 2))
      (should (= (cdr (assq 'downcase summary)) 1)))))

;;; 統合テスト

(ert-deftest nskk-test-fixtures-test-integration ()
  "フィクスチャシステム全体の統合テスト"
  (nskk-fixture-clear-mock-calls)

  ;; フィクスチャ登録
  (nskk-fixture-register 'integration-fixture
    (lambda ()
      (list :state (nskk-fixture-create-state :mode 'katakana)
            :entries (nskk-fixture-create-dict-entries 3))))

  ;; モックとフィクスチャの併用
  (nskk-with-fixture integration-fixture
    (nskk-with-mock-function 'user-error
      (lambda (&rest _) nil)
      (user-error "test")
      (should (nskk-fixture-was-called-p 'user-error))
      (should (plist-get (plist-get fixture-data :state) :mode))
      (should (= (length (plist-get fixture-data :entries)) 3)))))

(provide 'nskk-test-fixtures-test)

;;; nskk-test-fixtures-test.el ends here
