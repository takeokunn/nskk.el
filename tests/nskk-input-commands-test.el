;;; nskk-input-commands-test.el --- Tests for nskk-input-commands.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-input-commands.el のテストスイート
;; ERTフレームワークを使用した包括的なテストを実装

;;; Code:

(require 'ert)
(require 'nskk-state)
(require 'nskk-buffer)
(require 'nskk-input-commands)

;;; テスト用ヘルパー

(defmacro nskk-input-commands-test-with-temp-buffer (&rest body)
  "テスト用の一時バッファで BODY を実行する。"
  `(with-temp-buffer
     (nskk-state-init)
     ,@body
     (nskk-buffer-cleanup)))

;;; Interactive属性のテスト

(ert-deftest nskk-input-commands-test-all-interactive ()
  "全ての入力コマンドがinteractiveであることをテストする。"
  (should (commandp 'nskk-input-delete-backward-char))
  (should (commandp 'nskk-input-delete-forward-char))
  (should (commandp 'nskk-input-kill-line))
  (should (commandp 'nskk-input-kill-whole-line))
  (should (commandp 'nskk-input-kill-region))
  (should (commandp 'nskk-input-yank)))

;;; 削除コマンドのテスト

(ert-deftest nskk-input-commands-test-delete-backward-char ()
  "nskk-input-delete-backward-char の動作をテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (call-interactively 'nskk-input-delete-backward-char)
   (should (equal (buffer-string) "あい"))
   (should (= (point) 3))))

(ert-deftest nskk-input-commands-test-delete-backward-char-with-arg ()
  "nskk-input-delete-backward-char の引数付き呼び出しをテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (nskk-buffer-insert "あいうえお")
   (nskk-input-delete-backward-char 2)
   (should (equal (buffer-string) "あいう"))
   (should (= (point) 4))))

(ert-deftest nskk-input-commands-test-delete-forward-char ()
  "nskk-input-delete-forward-char の動作をテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (goto-char 1)
   (call-interactively 'nskk-input-delete-forward-char)
   (should (equal (buffer-string) "いう"))
   (should (= (point) 1))))

(ert-deftest nskk-input-commands-test-delete-forward-char-with-arg ()
  "nskk-input-delete-forward-char の引数付き呼び出しをテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (nskk-buffer-insert "あいうえお")
   (goto-char 1)
   (nskk-input-delete-forward-char 3)
   (should (equal (buffer-string) "えお"))
   (should (= (point) 1))))

;;; kill-lineコマンドのテスト

(ert-deftest nskk-input-commands-test-kill-line ()
  "nskk-input-kill-line の動作をテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (nskk-buffer-insert "あいうえお")
   (goto-char 3)
   (call-interactively 'nskk-input-kill-line)
   (should (equal (buffer-string) "あい"))
   (should (= (point) 3))))

(ert-deftest nskk-input-commands-test-kill-line-at-end ()
  "行末でnskk-input-kill-lineを呼び出した場合のテスト。"
  (nskk-input-commands-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (goto-char (point-max))
   (call-interactively 'nskk-input-kill-line)
   (should (equal (buffer-string) "あいう"))))

(ert-deftest nskk-input-commands-test-kill-line-at-beginning ()
  "行頭でnskk-input-kill-lineを呼び出した場合のテスト。"
  (nskk-input-commands-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (goto-char 1)
   (call-interactively 'nskk-input-kill-line)
   (should (equal (buffer-string) ""))))

;;; kill-whole-lineコマンドのテスト

(ert-deftest nskk-input-commands-test-kill-whole-line ()
  "nskk-input-kill-whole-line の動作をテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (goto-char 3)
   (call-interactively 'nskk-input-kill-whole-line)
   (should (equal (buffer-string) ""))
   (should (= (point) 1))))

(ert-deftest nskk-input-commands-test-kill-whole-line-clears-state ()
  "nskk-input-kill-whole-line が状態をクリアすることをテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (should (nskk-buffer-has-pending-input-p))
   (call-interactively 'nskk-input-kill-whole-line)
   (should-not (nskk-buffer-has-pending-input-p))))

;;; kill-regionコマンドのテスト

(ert-deftest nskk-input-commands-test-kill-region ()
  "nskk-input-kill-region の動作をテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (nskk-buffer-insert "あいうえお")
   (goto-char 2)
   (set-mark (point))
   (goto-char 5)
   (transient-mark-mode 1)
   (activate-mark)
   (call-interactively 'nskk-input-kill-region)
   (should (equal (buffer-string) "あお"))
   (should-not (region-active-p))))

(ert-deftest nskk-input-commands-test-kill-region-no-region ()
  "リージョンが無効な場合のnskk-input-kill-regionをテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (nskk-buffer-insert "あいう")
   (deactivate-mark)
   (call-interactively 'nskk-input-kill-region)
   (should (equal (buffer-string) "あいう"))))

;;; yankコマンドのテスト

(ert-deftest nskk-input-commands-test-yank ()
  "nskk-input-yank の動作をテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (kill-new "test")
   (call-interactively 'nskk-input-yank)
   (should (equal (buffer-string) "test"))
   (should (= (point) 5))))

(ert-deftest nskk-input-commands-test-yank-multiple ()
  "複数回のyankをテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (kill-new "テスト")
   (call-interactively 'nskk-input-yank)
   (should (equal (buffer-string) "テスト"))
   (call-interactively 'nskk-input-yank)
   (should (equal (buffer-string) "テストテスト"))))

(ert-deftest nskk-input-commands-test-yank-japanese ()
  "日本語テキストのyankをテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (kill-new "こんにちは")
   (call-interactively 'nskk-input-yank)
   (should (equal (buffer-string) "こんにちは"))
   (should (= (point) 6))))

;;; エッジケースのテスト

(ert-deftest nskk-input-commands-test-delete-empty-buffer ()
  "空バッファでの削除操作をテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (call-interactively 'nskk-input-delete-backward-char)
   (should (equal (buffer-string) ""))
   (call-interactively 'nskk-input-delete-forward-char)
   (should (equal (buffer-string) ""))))

(ert-deftest nskk-input-commands-test-kill-line-empty-buffer ()
  "空バッファでのkill-lineをテストする。"
  (nskk-input-commands-test-with-temp-buffer
   (call-interactively 'nskk-input-kill-line)
   (should (equal (buffer-string) ""))))

;;; 統合テスト

(ert-deftest nskk-input-commands-test-combined-operations ()
  "複数の入力コマンドを組み合わせたテスト。"
  (nskk-input-commands-test-with-temp-buffer
   (kill-new "挿入")
   (call-interactively 'nskk-input-yank)
   (should (equal (buffer-string) "挿入"))

   (call-interactively 'nskk-input-delete-backward-char)
   (should (equal (buffer-string) "挿"))

   (goto-char 1)
   (call-interactively 'nskk-input-kill-line)
   (should (equal (buffer-string) ""))

   (call-interactively 'nskk-input-yank)
   (call-interactively 'nskk-input-kill-whole-line)
   (should (equal (buffer-string) ""))))

(provide 'nskk-input-commands-test)

;;; nskk-input-commands-test.el ends here
