;;; nskk-mode-switch-test.el --- Tests for nskk-mode-switch.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-mode-switch.el のテストスイート
;; ERTフレームワークを使用した包括的なテストを実装

;;; Code:

(require 'ert)
(require 'nskk-state)
(require 'nskk-mode-switch)

;;; テスト用ヘルパー

(defmacro nskk-mode-switch-test-with-state (&rest body)
  "テスト用の新しい状態で BODY を実行する。"
  `(let ((nskk-current-state (nskk-state-create)))
     ,@body))

;;; 基本的なモード切り替えテスト

(ert-deftest nskk-mode-switch-test-basic-switch ()
  "基本的なモード切り替えをテストする。"
  (nskk-mode-switch-test-with-state
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
   (nskk-mode-switch 'katakana)
   (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(ert-deftest nskk-mode-switch-test-to-all-modes ()
  "全てのモードへの切り替えをテストする。"
  (nskk-mode-switch-test-with-state
   (dolist (mode nskk-state-modes)
     (nskk-mode-switch mode)
     (should (eq (nskk-state-mode nskk-current-state) mode)))))

(ert-deftest nskk-mode-switch-test-same-mode ()
  "同じモードへの切り替えをテストする。"
  (nskk-mode-switch-test-with-state
   (nskk-mode-switch 'hiragana)
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
   (should (null (nskk-state-previous-mode nskk-current-state)))))

(ert-deftest nskk-mode-switch-test-previous-mode ()
  "前回のモード記録をテストする。"
  (nskk-mode-switch-test-with-state
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
   (nskk-mode-switch 'katakana)
   (should (eq (nskk-state-previous-mode nskk-current-state) 'hiragana))
   (nskk-mode-switch 'latin)
   (should (eq (nskk-state-previous-mode nskk-current-state) 'katakana))))

;;; インタラクティブコマンドテスト

(ert-deftest nskk-mode-switch-test-to-hiragana ()
  "ひらがなモード切り替えコマンドをテストする。"
  (nskk-mode-switch-test-with-state
   (nskk-mode-switch 'katakana)
   (nskk-mode-switch-to-hiragana)
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(ert-deftest nskk-mode-switch-test-to-katakana ()
  "カタカナモード切り替えコマンドをテストする。"
  (nskk-mode-switch-test-with-state
   (nskk-mode-switch-to-katakana)
   (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(ert-deftest nskk-mode-switch-test-to-latin ()
  "半角英数モード切り替えコマンドをテストする。"
  (nskk-mode-switch-test-with-state
   (nskk-mode-switch-to-latin)
   (should (eq (nskk-state-mode nskk-current-state) 'latin))))

(ert-deftest nskk-mode-switch-test-to-zenkaku-latin ()
  "全角英数モード切り替えコマンドをテストする。"
  (nskk-mode-switch-test-with-state
   (nskk-mode-switch-to-zenkaku-latin)
   (should (eq (nskk-state-mode nskk-current-state) 'zenkaku-latin))))

(ert-deftest nskk-mode-switch-test-to-abbrev ()
  "abbrevモード切り替えコマンドをテストする。"
  (nskk-mode-switch-test-with-state
   (nskk-mode-switch-to-abbrev)
   (should (eq (nskk-state-mode nskk-current-state) 'abbrev))))

;;; トグルコマンドテスト

(ert-deftest nskk-mode-switch-test-toggle-kana-hiragana-to-katakana ()
  "ひらがな→カタカナのトグルをテストする。"
  (nskk-mode-switch-test-with-state
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
   (nskk-mode-toggle-kana)
   (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(ert-deftest nskk-mode-switch-test-toggle-kana-katakana-to-hiragana ()
  "カタカナ→ひらがなのトグルをテストする。"
  (nskk-mode-switch-test-with-state
   (nskk-mode-switch 'katakana)
   (nskk-mode-toggle-kana)
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(ert-deftest nskk-mode-switch-test-toggle-kana-from-latin ()
  "英数モードからのかなトグルをテストする。"
  (nskk-mode-switch-test-with-state
   (nskk-mode-switch 'latin)
   (nskk-mode-toggle-kana)
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(ert-deftest nskk-mode-switch-test-toggle-latin-from-hiragana ()
  "ひらがな→英数のトグルをテストする。"
  (nskk-mode-switch-test-with-state
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
   (nskk-mode-toggle-latin)
   (should (eq (nskk-state-mode nskk-current-state) 'latin))))

(ert-deftest nskk-mode-switch-test-toggle-latin-back-to-hiragana ()
  "英数→ひらがなへの復帰をテストする。"
  (nskk-mode-switch-test-with-state
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
   (nskk-mode-toggle-latin)
   (should (eq (nskk-state-mode nskk-current-state) 'latin))
   (nskk-mode-toggle-latin)
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(ert-deftest nskk-mode-switch-test-toggle-latin-back-to-katakana ()
  "英数→カタカナへの復帰をテストする。"
  (nskk-mode-switch-test-with-state
   (nskk-mode-switch 'katakana)
   (nskk-mode-toggle-latin)
   (should (eq (nskk-state-mode nskk-current-state) 'latin))
   (nskk-mode-toggle-latin)
   (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(ert-deftest nskk-mode-switch-test-toggle-zenkaku-latin ()
  "全角/半角英数のトグルをテストする。"
  (nskk-mode-switch-test-with-state
   (nskk-mode-switch 'latin)
   (nskk-mode-toggle-zenkaku-latin)
   (should (eq (nskk-state-mode nskk-current-state) 'zenkaku-latin))
   (nskk-mode-toggle-zenkaku-latin)
   (should (eq (nskk-state-mode nskk-current-state) 'latin))))

;;; 循環切り替えテスト

(ert-deftest nskk-mode-switch-test-cycle-forward ()
  "順方向循環切り替えをテストする。"
  (nskk-mode-switch-test-with-state
   ;; ひらがな → カタカナ
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
   (nskk-mode-cycle-forward)
   (should (eq (nskk-state-mode nskk-current-state) 'katakana))

   ;; カタカナ → 全角英数
   (nskk-mode-cycle-forward)
   (should (eq (nskk-state-mode nskk-current-state) 'zenkaku-latin))

   ;; 全角英数 → 半角英数
   (nskk-mode-cycle-forward)
   (should (eq (nskk-state-mode nskk-current-state) 'latin))

   ;; 半角英数 → ひらがな
   (nskk-mode-cycle-forward)
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(ert-deftest nskk-mode-switch-test-cycle-backward ()
  "逆方向循環切り替えをテストする。"
  (nskk-mode-switch-test-with-state
   ;; ひらがな → 半角英数
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
   (nskk-mode-cycle-backward)
   (should (eq (nskk-state-mode nskk-current-state) 'latin))

   ;; 半角英数 → 全角英数
   (nskk-mode-cycle-backward)
   (should (eq (nskk-state-mode nskk-current-state) 'zenkaku-latin))

   ;; 全角英数 → カタカナ
   (nskk-mode-cycle-backward)
   (should (eq (nskk-state-mode nskk-current-state) 'katakana))

   ;; カタカナ → ひらがな
   (nskk-mode-cycle-backward)
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

;;; 前回モード復帰テスト

(ert-deftest nskk-mode-switch-test-restore-previous ()
  "前回モード復帰をテストする。"
  (nskk-mode-switch-test-with-state
   (nskk-mode-switch 'katakana)
   (nskk-mode-switch 'latin)
   (should (eq (nskk-state-mode nskk-current-state) 'latin))
   (nskk-mode-restore-previous)
   (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(ert-deftest nskk-mode-switch-test-restore-previous-no-history ()
  "前回モード履歴がない場合の復帰をテストする。"
  (nskk-mode-switch-test-with-state
   (setf (nskk-state-previous-mode nskk-current-state) nil)
   (nskk-mode-restore-previous)
   (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

;;; モード問い合わせテスト

(ert-deftest nskk-mode-switch-test-current-mode ()
  "現在のモード取得をテストする。"
  (nskk-mode-switch-test-with-state
   (should (eq (nskk-mode-current-mode) 'hiragana))
   (nskk-mode-switch 'katakana)
   (should (eq (nskk-mode-current-mode) 'katakana))))

(ert-deftest nskk-mode-switch-test-current-mode-string ()
  "現在のモード説明文字列取得をテストする。"
  (nskk-mode-switch-test-with-state
   (should (equal (nskk-mode-current-mode-string) "ひらがな"))
   (nskk-mode-switch 'katakana)
   (should (equal (nskk-mode-current-mode-string) "カタカナ"))))

(ert-deftest nskk-mode-switch-test-current-indicator ()
  "現在のモードインジケーター取得をテストする。"
  (nskk-mode-switch-test-with-state
   (should (equal (nskk-mode-current-indicator) "あ"))
   (nskk-mode-switch 'katakana)
   (should (equal (nskk-mode-current-indicator) "ア"))
   (nskk-mode-switch 'latin)
   (should (equal (nskk-mode-current-indicator) "A"))))

;;; フックテスト

(ert-deftest nskk-mode-switch-test-before-hook ()
  "before フックの実行をテストする。"
  (nskk-mode-switch-test-with-state
   (let ((hook-called nil)
         (from-mode-arg nil)
         (to-mode-arg nil))
     (add-hook 'nskk-mode-switch-before-hook
               (lambda (from to)
                 (setq hook-called t
                       from-mode-arg from
                       to-mode-arg to)))

     (nskk-mode-switch 'katakana)

     (should hook-called)
     (should (eq from-mode-arg 'hiragana))
     (should (eq to-mode-arg 'katakana))

     (remove-hook 'nskk-mode-switch-before-hook
                  (lambda (from to)
                    (setq hook-called t
                          from-mode-arg from
                          to-mode-arg to))))))

(ert-deftest nskk-mode-switch-test-after-hook ()
  "after フックの実行をテストする。"
  (nskk-mode-switch-test-with-state
   (let ((hook-called nil)
         (from-mode-arg nil)
         (to-mode-arg nil))
     (add-hook 'nskk-mode-switch-after-hook
               (lambda (from to)
                 (setq hook-called t
                       from-mode-arg from
                       to-mode-arg to)))

     (nskk-mode-switch 'katakana)

     (should hook-called)
     (should (eq from-mode-arg 'hiragana))
     (should (eq to-mode-arg 'katakana))

     (remove-hook 'nskk-mode-switch-after-hook
                  (lambda (from to)
                    (setq hook-called t
                          from-mode-arg from
                          to-mode-arg to))))))

(ert-deftest nskk-mode-switch-test-mode-specific-hook ()
  "モード別フックの実行をテストする。"
  (nskk-mode-switch-test-with-state
   (let ((hook-called nil))
     (add-hook 'nskk-mode-switch-to-katakana-hook
               (lambda () (setq hook-called t)))

     (nskk-mode-switch 'katakana)
     (should hook-called)

     (remove-hook 'nskk-mode-switch-to-katakana-hook
                  (lambda () (setq hook-called t))))))

(ert-deftest nskk-mode-switch-test-no-hooks-flag ()
  "NO-HOOKS フラグによるフックスキップをテストする。"
  (nskk-mode-switch-test-with-state
   (let ((hook-called nil))
     (add-hook 'nskk-mode-switch-after-hook
               (lambda (from to) (setq hook-called t)))

     (nskk-mode-switch 'katakana nil t)
     (should-not hook-called)

     (remove-hook 'nskk-mode-switch-after-hook
                  (lambda (from to) (setq hook-called t))))))

;;; 入力バッファクリアテスト

(ert-deftest nskk-mode-switch-test-clear-input-on-switch ()
  "モード切り替え時の入力バッファクリアをテストする。"
  (nskk-mode-switch-test-with-state
   (let ((nskk-mode-switch-clear-input-on-mode-change t))
     (nskk-state-append-input nskk-current-state ?k)
     (nskk-state-append-input nskk-current-state ?a)
     (should (equal (nskk-state-input-buffer nskk-current-state) "ka"))

     (nskk-mode-switch 'katakana)
     (should (string-empty-p (nskk-state-input-buffer nskk-current-state))))))

(ert-deftest nskk-mode-switch-test-no-clear-input ()
  "入力バッファクリアなしの切り替えをテストする。"
  (nskk-mode-switch-test-with-state
   (let ((nskk-mode-switch-clear-input-on-mode-change nil))
     (nskk-state-append-input nskk-current-state ?k)
     (nskk-state-append-input nskk-current-state ?a)
     (should (equal (nskk-state-input-buffer nskk-current-state) "ka"))

     (nskk-mode-switch 'katakana)
     (should (equal (nskk-state-input-buffer nskk-current-state) "ka")))))

;;; 統計情報テスト

(ert-deftest nskk-mode-switch-test-stats ()
  "統計情報取得をテストする。"
  (nskk-mode-switch-test-with-state
   (let ((stats (nskk-mode-switch-stats)))
     (should (eq (plist-get stats :current-mode) 'hiragana))
     (should (null (plist-get stats :previous-mode)))
     (should (= (plist-get stats :available-modes) 6))

     (nskk-mode-switch 'katakana)
     (setq stats (nskk-mode-switch-stats))
     (should (eq (plist-get stats :current-mode) 'katakana))
     (should (eq (plist-get stats :previous-mode) 'hiragana)))))

;;; 複合操作テスト

(ert-deftest nskk-mode-switch-test-complex-scenario ()
  "複雑なモード切り替えシナリオをテストする。"
  (nskk-mode-switch-test-with-state
   ;; ひらがなで開始
   (should (eq (nskk-mode-current-mode) 'hiragana))

   ;; カタカナに切り替え
   (nskk-mode-toggle-kana)
   (should (eq (nskk-mode-current-mode) 'katakana))

   ;; 英数に切り替え
   (nskk-mode-toggle-latin)
   (should (eq (nskk-mode-current-mode) 'latin))

   ;; カタカナに戻る
   (nskk-mode-toggle-latin)
   (should (eq (nskk-mode-current-mode) 'katakana))

   ;; 全角英数に切り替え
   (nskk-mode-switch 'zenkaku-latin)
   (should (eq (nskk-mode-current-mode) 'zenkaku-latin))

   ;; 前回モード(カタカナ)に戻る
   (nskk-mode-restore-previous)
   (should (eq (nskk-mode-current-mode) 'katakana))))

(provide 'nskk-mode-switch-test)

;;; nskk-mode-switch-test.el ends here
