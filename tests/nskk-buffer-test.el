;;; nskk-buffer-test.el --- Tests for nskk-buffer.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-buffer.el のテストスイート
;; ERTフレームワークを使用した包括的なテストを実装

;;; Code:

(require 'ert)
(require 'nskk-state)
(require 'nskk-buffer)

;;; テスト用ヘルパー

(defmacro nskk-buffer-test-with-temp-buffer (&rest body)
  "テスト用の一時バッファで BODY を実行する。"
  `(with-temp-buffer
     (nskk-state-init)
     ,@body
     (nskk-buffer-cleanup)))

;;; 基本的な挿入・削除テスト

(ert-deftest nskk-buffer-test-insert ()
  "文字列挿入をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あ")
   (should (equal (buffer-string) "あ"))
   (should (= (point) 2))))

(ert-deftest nskk-buffer-test-multiple-insert ()
  "複数回の挿入をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あ")
   (nskk-buffer-insert "い")
   (nskk-buffer-insert "う")
   (should (equal (buffer-string) "あいう"))
   (should (= (point) 4))))

(ert-deftest nskk-buffer-test-delete-backward-char ()
  "後方削除をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (should (nskk-buffer-delete-backward-char 1))
   (should (equal (buffer-string) "あい"))
   (should (= (point) 3))))

(ert-deftest nskk-buffer-test-delete-backward-multiple ()
  "複数文字の後方削除をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (should (nskk-buffer-delete-backward-char 2))
   (should (equal (buffer-string) "あ"))
   (should (= (point) 2))))

(ert-deftest nskk-buffer-test-delete-backward-beyond-start ()
  "開始位置を超える削除をテストする（失敗するべき）。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あ")
   (should-not (nskk-buffer-delete-backward-char 2))
   (should (equal (buffer-string) "あ"))))

(ert-deftest nskk-buffer-test-delete-forward-char ()
  "前方削除をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (goto-char 1)
   (should (nskk-buffer-delete-forward-char 1))
   (should (equal (buffer-string) "いう"))))

;;; バッファクリアテスト

(ert-deftest nskk-buffer-test-clear ()
  "バッファクリアをテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (should (equal (buffer-string) "あいう"))
   (nskk-buffer-clear)
   (should (equal (buffer-string) ""))
   (should (= (point) 1))))

(ert-deftest nskk-buffer-test-clear-markers ()
  "クリア後のマーカー状態をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (should (nskk-state-marker-start nskk-current-state))
   (nskk-buffer-clear)
   (should-not (nskk-state-marker-start nskk-current-state))
   (should-not (nskk-state-marker-end nskk-current-state))))

;;; 確定処理テスト

(ert-deftest nskk-buffer-test-commit ()
  "確定処理をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (nskk-buffer-commit "漢字")
   (should (equal (buffer-string) "漢字"))
   (should (= (point) 3))))

(ert-deftest nskk-buffer-test-commit-clears-markers ()
  "確定後のマーカークリアをテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (nskk-buffer-commit "漢字")
   (should-not (nskk-state-marker-start nskk-current-state))))

;;; 未確定入力状態テスト

(ert-deftest nskk-buffer-test-has-pending-input-p ()
  "未確定入力の有無判定をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (should-not (nskk-buffer-has-pending-input-p))
   (nskk-buffer-insert "あ")
   (should (nskk-buffer-has-pending-input-p))
   (nskk-buffer-clear)
   (should-not (nskk-buffer-has-pending-input-p))))

(ert-deftest nskk-buffer-test-pending-text ()
  "未確定テキスト取得をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (should-not (nskk-buffer-pending-text))
   (nskk-buffer-insert "あいう")
   (should (equal (nskk-buffer-pending-text) "あいう"))
   (nskk-buffer-delete-backward-char 1)
   (should (equal (nskk-buffer-pending-text) "あい"))))

(ert-deftest nskk-buffer-test-pending-length ()
  "未確定入力の長さ取得をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (should (= (nskk-buffer-pending-length) 0))
   (nskk-buffer-insert "あいう")
   (should (= (nskk-buffer-pending-length) 3))
   (nskk-buffer-delete-backward-char 1)
   (should (= (nskk-buffer-pending-length) 2))))

;;; アンドゥ・リドゥテスト

(ert-deftest nskk-buffer-test-undo-insert ()
  "挿入のアンドゥをテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あ")
   (should (equal (buffer-string) "あ"))
   (should (nskk-buffer-undo))
   (should (equal (buffer-string) ""))))

(ert-deftest nskk-buffer-test-undo-multiple ()
  "複数回のアンドゥをテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あ")
   (nskk-buffer-insert "い")
   (nskk-buffer-insert "う")
   (should (nskk-buffer-undo))
   (should (equal (buffer-string) "あい"))
   (should (nskk-buffer-undo))
   (should (equal (buffer-string) "あ"))
   (should (nskk-buffer-undo))
   (should (equal (buffer-string) ""))))

(ert-deftest nskk-buffer-test-undo-delete ()
  "削除のアンドゥをテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (nskk-buffer-delete-backward-char 1)
   (should (equal (buffer-string) "あい"))
   (should (nskk-buffer-undo))
   (should (equal (buffer-string) "あいう"))))

(ert-deftest nskk-buffer-test-undo-no-history ()
  "履歴がない場合のアンドゥをテストする。"
  (nskk-buffer-test-with-temp-buffer
   (should-not (nskk-buffer-undo))))

(ert-deftest nskk-buffer-test-redo ()
  "リドゥをテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あ")
   (nskk-buffer-undo)
   (should (equal (buffer-string) ""))
   (should (nskk-buffer-redo))
   (should (equal (buffer-string) "あ"))))

(ert-deftest nskk-buffer-test-redo-multiple ()
  "複数回のリドゥをテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "a")
   (nskk-buffer-insert "b")
   (nskk-buffer-insert "c")

   ;; 全てアンドゥ
   (nskk-buffer-undo)
   (nskk-buffer-undo)
   (nskk-buffer-undo)
   (should (equal (buffer-string) ""))

   ;; 全てリドゥ
   (should (nskk-buffer-redo))
   (should (equal (buffer-string) "a"))
   (should (nskk-buffer-redo))
   (should (equal (buffer-string) "ab"))
   (should (nskk-buffer-redo))
   (should (equal (buffer-string) "abc"))))

(ert-deftest nskk-buffer-test-redo-cleared-by-new-operation ()
  "新しい操作でリドゥ履歴がクリアされることをテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あ")
   (nskk-buffer-undo)
   (nskk-buffer-insert "い")
   (should-not (nskk-buffer-redo))))

;;; マーカー管理テスト

(ert-deftest nskk-buffer-test-markers-created ()
  "挿入時にマーカーが作成されることをテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あ")
   (should (nskk-state-marker-start nskk-current-state))
   (should (nskk-state-marker-end nskk-current-state))
   (should (markerp (nskk-state-marker-start nskk-current-state)))
   (should (markerp (nskk-state-marker-end nskk-current-state)))))

(ert-deftest nskk-buffer-test-marker-positions ()
  "マーカー位置の正確性をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (should (= (marker-position (nskk-state-marker-start nskk-current-state)) 1))
   (should (= (marker-position (nskk-state-marker-end nskk-current-state)) 4))))

(ert-deftest nskk-buffer-test-marker-update-on-delete ()
  "削除時のマーカー更新をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (nskk-buffer-delete-backward-char 1)
   (should (= (marker-position (nskk-state-marker-start nskk-current-state)) 1))
   (should (= (marker-position (nskk-state-marker-end nskk-current-state)) 3))))

;;; 統計情報テスト

(ert-deftest nskk-buffer-test-stats ()
  "統計情報取得をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (let ((stats (nskk-buffer-stats)))
     (should-not (plist-get stats :has-pending))
     (should (= (plist-get stats :pending-length) 0))
     (should (= (plist-get stats :undo-stack-size) 0)))

   (nskk-buffer-insert "あいう")

   (let ((stats (nskk-buffer-stats)))
     (should (plist-get stats :has-pending))
     (should (= (plist-get stats :pending-length) 3))
     (should (= (plist-get stats :undo-stack-size) 1)))))

(ert-deftest nskk-buffer-test-stats-after-undo ()
  "アンドゥ後の統計情報をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (nskk-buffer-insert "あ")
   (nskk-buffer-insert "い")

   (let ((stats (nskk-buffer-stats)))
     (should (= (plist-get stats :undo-stack-size) 2))
     (should (= (plist-get stats :redo-stack-size) 0)))

   (nskk-buffer-undo)

   (let ((stats (nskk-buffer-stats)))
     (should (= (plist-get stats :undo-stack-size) 1))
     (should (= (plist-get stats :redo-stack-size) 1)))))

;;; 複合操作テスト

(ert-deftest nskk-buffer-test-complex-scenario ()
  "複雑な操作シナリオをテストする。"
  (nskk-buffer-test-with-temp-buffer
   ;; 入力
   (nskk-buffer-insert "あ")
   (nskk-buffer-insert "い")
   (nskk-buffer-insert "う")
   (should (equal (buffer-string) "あいう"))

   ;; 削除
   (nskk-buffer-delete-backward-char 1)
   (should (equal (buffer-string) "あい"))

   ;; さらに入力
   (nskk-buffer-insert "え")
   (should (equal (buffer-string) "あいえ"))

   ;; アンドゥ
   (nskk-buffer-undo)
   (should (equal (buffer-string) "あい"))

   ;; アンドゥ
   (nskk-buffer-undo)
   (should (equal (buffer-string) "あいう"))

   ;; リドゥ
   (nskk-buffer-redo)
   (should (equal (buffer-string) "あい"))))

(ert-deftest nskk-buffer-test-insert-commit-scenario ()
  "入力と確定のシナリオをテストする。"
  (nskk-buffer-test-with-temp-buffer
   ;; 入力
   (nskk-buffer-insert "かんじ")
   (should (equal (buffer-string) "かんじ"))
   (should (nskk-buffer-has-pending-input-p))

   ;; 確定
   (nskk-buffer-commit "漢字")
   (should (equal (buffer-string) "漢字"))
   (should-not (nskk-buffer-has-pending-input-p))

   ;; 続けて入力
   (nskk-buffer-insert "にゅうりょく")
   (should (equal (buffer-string) "漢字にゅうりょく"))
   (should (nskk-buffer-has-pending-input-p))

   ;; 確定
   (nskk-buffer-commit "入力")
   (should (equal (buffer-string) "漢字入力"))
   (should-not (nskk-buffer-has-pending-input-p))))

(ert-deftest nskk-buffer-test-undo-limit ()
  "アンドゥ履歴の上限をテストする。"
  (nskk-buffer-test-with-temp-buffer
   (let ((nskk-buffer-undo-limit 3))
     ;; 4回挿入（上限を超える）
     (nskk-buffer-insert "あ")
     (nskk-buffer-insert "い")
     (nskk-buffer-insert "う")
     (nskk-buffer-insert "え")

     ;; 履歴は最大3件まで
     (let ((stats (nskk-buffer-stats)))
       (should (<= (plist-get stats :undo-stack-size) 3))))))

(ert-deftest nskk-buffer-test-cleanup ()
  "クリーンアップ処理をテストする。"
  (with-temp-buffer
    (nskk-state-init)
    (nskk-buffer-insert "あいう")

    ;; クリーンアップ前
    (should (nskk-state-marker-start nskk-current-state))
    (should nskk-buffer--undo-stack)

    ;; クリーンアップ
    (nskk-buffer-cleanup)

    ;; クリーンアップ後
    (should-not (nskk-state-marker-start nskk-current-state))
    (should-not nskk-buffer--undo-stack)
    (should-not nskk-buffer--redo-stack)))

(provide 'nskk-buffer-test)

;;; nskk-buffer-test.el ends here
