;;; nskk-test-framework-test.el --- Tests for nskk-test-framework.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-test-framework.el のテストスイート
;; テストフレームワーク自体をテストする

;;; Code:

(require 'ert)
(require 'nskk-test-framework)

;;; nskk-deftest マクロテスト

(nskk-deftest nskk-test-framework-test-deftest-basic
  "基本的な nskk-deftest の動作をテストする"
  :tags '(:unit :framework)
  (should t))

(nskk-deftest nskk-test-framework-test-deftest-with-assertion
  "アサーションを含む nskk-deftest をテストする"
  :tags '(:unit :framework)
  (should (= 1 1))
  (should (equal "test" "test")))

(ert-deftest nskk-test-framework-test-performance-tracking ()
  "パフォーマンストラッキングをテストする"
  (nskk-test-clear-performance-data)
  (let ((nskk-test-report-performance t))
    ;; nskk-deftest を使ったテストを実行すると、パフォーマンスデータが記録される
    ;; ここでは直接テストせず、既存のテストが記録されることを確認
    (should (listp nskk-test--performance-data))))

;;; テストユーティリティテスト

(ert-deftest nskk-test-framework-test-with-temp-buffer ()
  "一時バッファユーティリティをテストする"
  (let ((buf nil))
    (nskk-test-with-temp-buffer
      (setq buf (current-buffer))
      (should (bufferp buf))
      (should (buffer-live-p buf))
      (insert "test content")
      (should (equal (buffer-string) "test content")))
    ;; バッファは自動的に削除されている
    (should-not (buffer-live-p buf))))

(ert-deftest nskk-test-framework-test-with-mock-input-string ()
  "文字列によるモック入力をテストする"
  (nskk-test-with-mock-input "abc"
    (should (= (read-char) ?a))
    (should (= (read-char) ?b))
    (should (= (read-char) ?c))
    (should-error (read-char))))

(ert-deftest nskk-test-framework-test-with-mock-input-list ()
  "リストによるモック入力をテストする"
  (nskk-test-with-mock-input '(?x ?y ?z)
    (should (= (read-char) ?x))
    (should (= (read-char) ?y))
    (should (= (read-char) ?z))))

(ert-deftest nskk-test-framework-test-with-suppressed-message ()
  "メッセージ抑制ユーティリティをテストする"
  (let ((message-called nil))
    (nskk-test-with-suppressed-message
      (message "This should be suppressed")
      ;; messageが呼ばれても何も出力されない
      (should t))))

(ert-deftest nskk-test-framework-test-measure-time ()
  "時間測定ユーティリティをテストする"
  (let ((result (nskk-test-measure-time
                  (sleep-for 0.01)
                  42)))
    (should (consp result))
    (should (= (car result) 42))
    (should (numberp (cdr result)))
    (should (>= (cdr result) 0.01))))

;;; アサーションマクロテスト

(ert-deftest nskk-test-framework-test-should-equal-string ()
  "文字列等価アサーションをテストする"
  (nskk-should-equal-string "test" "test")
  (should-error (nskk-should-equal-string "test" "TEST")))

(ert-deftest nskk-test-framework-test-should-match-regexp ()
  "正規表現マッチアサーションをテストする"
  (nskk-should-match-regexp "hello world" "hello")
  (nskk-should-match-regexp "test123" "[0-9]+")
  (should-error (nskk-should-match-regexp "hello" "xyz")))

(ert-deftest nskk-test-framework-test-should-buffer-string ()
  "バッファ文字列アサーションをテストする"
  (with-temp-buffer
    (insert "test content")
    (nskk-should-buffer-string "test content")
    (should-error (nskk-should-buffer-string "wrong"))))

(ert-deftest nskk-test-framework-test-should-point ()
  "ポイント位置アサーションをテストする"
  (with-temp-buffer
    (insert "test")
    (nskk-should-point 5)
    (goto-char 1)
    (nskk-should-point 1)
    (should-error (nskk-should-point 10))))

;;; テストデータ生成テスト

(ert-deftest nskk-test-framework-test-random-string ()
  "ランダム文字列生成をテストする"
  (let ((str (nskk-test-random-string 10)))
    (should (stringp str))
    (should (= (length str) 10))
    (should (string-match-p "^[a-zA-Z0-9]+$" str))))

(ert-deftest nskk-test-framework-test-random-string-with-charset ()
  "文字セット指定のランダム文字列生成をテストする"
  (let ((str (nskk-test-random-string 5 "abc")))
    (should (= (length str) 5))
    (should (string-match-p "^[abc]+$" str))))

(ert-deftest nskk-test-framework-test-random-hiragana ()
  "ランダムひらがな生成をテストする"
  (let ((str (nskk-test-random-hiragana 5)))
    (should (= (length str) 5))
    ;; ひらがなの範囲: U+3040-U+309F
    (should (cl-every (lambda (c) (<= #x3040 c #x309F))
                     (string-to-list str)))))

(ert-deftest nskk-test-framework-test-random-katakana ()
  "ランダムカタカナ生成をテストする"
  (let ((str (nskk-test-random-katakana 5)))
    (should (= (length str) 5))
    ;; カタカナの範囲: U+30A0-U+30FF
    (should (cl-every (lambda (c) (<= #x30A0 c #x30FF))
                     (string-to-list str)))))

(ert-deftest nskk-test-framework-test-random-int ()
  "ランダム整数生成をテストする"
  (dotimes (_ 10)
    (let ((n (nskk-test-random-int 5 10)))
      (should (>= n 5))
      (should (< n 10)))))

(ert-deftest nskk-test-framework-test-random-choice ()
  "ランダム選択をテストする"
  (let ((choices '(a b c)))
    (dotimes (_ 10)
      (should (memq (nskk-test-random-choice choices) choices)))))

;;; パフォーマンス分析テスト

(ert-deftest nskk-test-framework-test-performance-report ()
  "パフォーマンスレポート生成をテストする"
  (nskk-test-clear-performance-data)
  (push (cons 'test1 0.001) nskk-test--performance-data)
  (push (cons 'test2 0.002) nskk-test--performance-data)
  (let ((report (nskk-test-performance-report)))
    (should (listp report))
    (should (= (length report) 2))
    ;; ソート済み（遅い順）
    (should (equal (caar report) 'test2))))

(ert-deftest nskk-test-framework-test-clear-performance-data ()
  "パフォーマンスデータクリアをテストする"
  (push (cons 'test 0.001) nskk-test--performance-data)
  (nskk-test-clear-performance-data)
  (should (null nskk-test--performance-data)))

;;; ヘルパー関数テスト

(ert-deftest nskk-test-framework-test-string-diff ()
  "文字列差分表示をテストする"
  (let ((diff (nskk-test--string-diff "hello" "hallo")))
    (should (stringp diff))
    (should (string-match-p "position 1" diff))))

(ert-deftest nskk-test-framework-test-buffer-contents-equal-p ()
  "バッファ内容比較をテストする"
  (let ((buf1 (generate-new-buffer " *test1*"))
        (buf2 (generate-new-buffer " *test2*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1 (insert "test"))
          (with-current-buffer buf2 (insert "test"))
          (should (nskk-test-buffer-contents-equal-p buf1 buf2))

          (with-current-buffer buf2 (insert "more"))
          (should-not (nskk-test-buffer-contents-equal-p buf1 buf2)))
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest nskk-test-framework-test-list-tests-by-tag ()
  "タグによるテスト一覧取得をテストする"
  (let ((tests (nskk-test-list-tests-by-tag :framework)))
    (should (listp tests))
    ;; このファイルで定義したテストが含まれているはず
    (should (memq 'nskk-test-framework-test-deftest-basic tests))))

;;; 統計情報テスト

(ert-deftest nskk-test-framework-test-stats ()
  "統計情報取得をテストする"
  (let ((stats (nskk-test-stats)))
    (should (plist-get stats :total-tests))
    (should (numberp (plist-get stats :total-tests)))
    (should (>= (plist-get stats :total-tests) 0))
    (should (listp (plist-get stats :tagged-tests)))
    (should (numberp (plist-get stats :performance-data-count)))
    (should (numberp (plist-get stats :temp-buffers-count)))))

;;; クリーンアップテスト

(ert-deftest nskk-test-framework-test-cleanup ()
  "クリーンアップ処理をテストする"
  (let ((buf (generate-new-buffer " *nskk-test-cleanup*")))
    (push buf nskk-test--temp-buffers)
    (push (cons 'test 0.001) nskk-test--performance-data)

    (nskk-test-cleanup)

    (should-not (buffer-live-p buf))
    (should (null nskk-test--performance-data))
    (should (null nskk-test--temp-buffers))))

;;; 統合テスト

(ert-deftest nskk-test-framework-test-integration ()
  "フレームワーク全体の統合テストを実行する"
  (nskk-test-with-temp-buffer
    ;; テストデータ生成
    (let ((random-str (nskk-test-random-string 10))
          (random-hiragana (nskk-test-random-hiragana 3)))

      ;; バッファ操作
      (insert random-str)
      (nskk-should-buffer-string random-str)

      (erase-buffer)
      (insert random-hiragana)
      (nskk-should-buffer-string random-hiragana)

      ;; 時間測定
      (let ((result (nskk-test-measure-time
                      (dotimes (i 100)
                        (insert "x")
                        (delete-char -1))
                      "done")))
        (should (equal (car result) "done"))
        (should (numberp (cdr result)))))))

(provide 'nskk-test-framework-test)

;;; nskk-test-framework-test.el ends here
