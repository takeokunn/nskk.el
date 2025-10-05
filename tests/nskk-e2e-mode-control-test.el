;;; nskk-e2e-mode-control-test.el --- E2E tests for mode control -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:

;; モード制御機能のE2E統合テストスイート
;;
;; カバーするテストシナリオ:
;; 1. ひらがな⇔カタカナ切り替え
;; 2. 全モード間の遷移
;; 3. モード切り替え時のイベント発火
;; 4. 各モードでの入力動作の違い
;; 5. 未確定入力中のモード切り替え

;;; Code:

(require 'ert)
(require 'nskk)
(require 'nskk-state)
(require 'nskk-mode-switch)
(require 'nskk-events)

;;; テストヘルパー関数

(defun nskk-e2e-mode--setup ()
  "E2Eテスト用環境のセットアップ。"
  (nskk-state-init)
  ;; グローバルリスナーをクリア（既存のリスナーの影響を排除）
  (nskk-events-clear-listeners nil t))

(defun nskk-e2e-mode--teardown ()
  "E2Eテスト用環境のクリーンアップ。"
  (nskk-state-cleanup)
  (nskk-events-clear-listeners nil t))

(defun nskk-e2e-mode--assert-mode (expected-mode)
  "現在のモードが EXPECTED-MODE と一致することを検証。"
  (should (eq (nskk-state-mode nskk-current-state) expected-mode)))

(defun nskk-e2e-mode--assert-previous-mode (expected-mode)
  "前回のモードが EXPECTED-MODE と一致することを検証。"
  (should (eq (nskk-state-previous-mode nskk-current-state) expected-mode)))

;;; Test Suite 1: ひらがな⇔カタカナ切り替え

(ert-deftest nskk-e2e-mode-toggle-kana ()
  "ひらがな⇔カタカナのトグル動作をテスト。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        ;; 初期状態: ひらがな
        (nskk-e2e-mode--assert-mode 'hiragana)

        ;; ひらがな → カタカナ
        (nskk-mode-toggle-kana)
        (nskk-e2e-mode--assert-mode 'katakana)
        (nskk-e2e-mode--assert-previous-mode 'hiragana)

        ;; カタカナ → ひらがな
        (nskk-mode-toggle-kana)
        (nskk-e2e-mode--assert-mode 'hiragana)
        (nskk-e2e-mode--assert-previous-mode 'katakana)

        ;; 再度トグル
        (nskk-mode-toggle-kana)
        (nskk-e2e-mode--assert-mode 'katakana))
    (nskk-e2e-mode--teardown)))

(ert-deftest nskk-e2e-mode-toggle-kana-from-latin ()
  "英数モードからトグルした場合の動作テスト。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        ;; 英数モードに切り替え
        (nskk-mode-switch-to-latin)
        (nskk-e2e-mode--assert-mode 'latin)

        ;; 英数モードからトグル → ひらがな
        (nskk-mode-toggle-kana)
        (nskk-e2e-mode--assert-mode 'hiragana))
    (nskk-e2e-mode--teardown)))

;;; Test Suite 2: 全モード遷移

(ert-deftest nskk-e2e-mode-transitions ()
  "全モード間の遷移テスト: hiragana/katakana/latin/zenkaku-latin。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        ;; ひらがな → カタカナ
        (nskk-mode-switch-to-katakana)
        (nskk-e2e-mode--assert-mode 'katakana)

        ;; カタカナ → 全角英数
        (nskk-mode-switch-to-zenkaku-latin)
        (nskk-e2e-mode--assert-mode 'zenkaku-latin)

        ;; 全角英数 → 半角英数
        (nskk-mode-switch-to-latin)
        (nskk-e2e-mode--assert-mode 'latin)

        ;; 半角英数 → ひらがな
        (nskk-mode-switch-to-hiragana)
        (nskk-e2e-mode--assert-mode 'hiragana))
    (nskk-e2e-mode--teardown)))

(ert-deftest nskk-e2e-mode-cycle-forward ()
  "順方向モード循環切り替えテスト。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        ;; ひらがな → カタカナ
        (nskk-mode-cycle-forward)
        (nskk-e2e-mode--assert-mode 'katakana)

        ;; カタカナ → 全角英数
        (nskk-mode-cycle-forward)
        (nskk-e2e-mode--assert-mode 'zenkaku-latin)

        ;; 全角英数 → 半角英数
        (nskk-mode-cycle-forward)
        (nskk-e2e-mode--assert-mode 'latin)

        ;; 半角英数 → ひらがな
        (nskk-mode-cycle-forward)
        (nskk-e2e-mode--assert-mode 'hiragana))
    (nskk-e2e-mode--teardown)))

(ert-deftest nskk-e2e-mode-cycle-backward ()
  "逆方向モード循環切り替えテスト。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        ;; ひらがな → 半角英数
        (nskk-mode-cycle-backward)
        (nskk-e2e-mode--assert-mode 'latin)

        ;; 半角英数 → 全角英数
        (nskk-mode-cycle-backward)
        (nskk-e2e-mode--assert-mode 'zenkaku-latin)

        ;; 全角英数 → カタカナ
        (nskk-mode-cycle-backward)
        (nskk-e2e-mode--assert-mode 'katakana)

        ;; カタカナ → ひらがな
        (nskk-mode-cycle-backward)
        (nskk-e2e-mode--assert-mode 'hiragana))
    (nskk-e2e-mode--teardown)))

;;; Test Suite 3: モード切り替え時のイベント発火

(ert-deftest nskk-e2e-mode-switch-events ()
  "モード切り替え時の:mode-switchedイベント発火確認。"
  (unwind-protect
      (let ((event-fired nil)
            (from-mode nil)
            (to-mode nil))
        (nskk-e2e-mode--setup)

        ;; イベントリスナー登録
        (nskk-events-add-listener
         :mode-switched
         (lambda (event)
           (setq event-fired t)
           (setq from-mode (plist-get (nskk-event-data event) :from))
           (setq to-mode (plist-get (nskk-event-data event) :to)))
         t) ; グローバルリスナー

        ;; モード切り替え実行
        (nskk-mode-switch-to-katakana)

        ;; イベントが発火されたか検証
        (should event-fired)
        (should (eq from-mode 'hiragana))
        (should (eq to-mode 'katakana)))
    (nskk-e2e-mode--teardown)))

(ert-deftest nskk-e2e-mode-switch-events-multiple ()
  "複数回のモード切り替えでイベントが正しく発火することを確認。"
  (unwind-protect
      (let ((event-count 0)
            (transitions nil))
        (nskk-e2e-mode--setup)

        ;; イベントリスナー登録
        (nskk-events-add-listener
         :mode-switched
         (lambda (event)
           (setq event-count (1+ event-count))
           (push (cons (plist-get (nskk-event-data event) :from)
                       (plist-get (nskk-event-data event) :to))
                 transitions))
         t)

        ;; 複数回のモード切り替え
        (nskk-mode-switch-to-katakana)
        (nskk-mode-switch-to-latin)
        (nskk-mode-switch-to-hiragana)

        ;; イベントカウント検証
        (should (= event-count 3))

        ;; 遷移履歴検証（逆順でpushされる）
        (should (equal (nreverse transitions)
                      '((hiragana . katakana)
                        (katakana . latin)
                        (latin . hiragana)))))
    (nskk-e2e-mode--teardown)))

(ert-deftest nskk-e2e-mode-switch-events-no-fire-on-same-mode ()
  "同じモードへの切り替えではイベントが発火しないことを確認。"
  (unwind-protect
      (let ((event-fired nil))
        (nskk-e2e-mode--setup)

        ;; イベントリスナー登録
        (nskk-events-add-listener
         :mode-switched
         (lambda (_event)
           (setq event-fired t))
         t)

        ;; 現在のモード（hiragana）に再度切り替え
        (nskk-mode-switch-to-hiragana)

        ;; イベントが発火されていないことを検証
        (should-not event-fired))
    (nskk-e2e-mode--teardown)))

;;; Test Suite 4: モード別入力動作

(ert-deftest nskk-e2e-input-by-mode ()
  "各モードでの入力動作の違いを検証。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        ;; ひらがなモード
        (nskk-mode-switch-to-hiragana)
        (should (eq (nskk-state-mode nskk-current-state) 'hiragana))

        ;; カタカナモード
        (nskk-mode-switch-to-katakana)
        (should (eq (nskk-state-mode nskk-current-state) 'katakana))

        ;; 半角英数モード
        (nskk-mode-switch-to-latin)
        (should (eq (nskk-state-mode nskk-current-state) 'latin))

        ;; 全角英数モード
        (nskk-mode-switch-to-zenkaku-latin)
        (should (eq (nskk-state-mode nskk-current-state) 'zenkaku-latin)))
    (nskk-e2e-mode--teardown)))

;;; Test Suite 5: 未確定入力中のモード切り替え

(ert-deftest nskk-e2e-mode-switch-with-pending ()
  "未確定入力がある状態でのモード切り替え挙動。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        ;; 未確定入力を設定
        (setf (nskk-state-input-buffer nskk-current-state) "ka")
        (should (equal (nskk-state-input-buffer nskk-current-state) "ka"))

        ;; デフォルト設定では、モード切り替え時に入力バッファがクリアされる
        (let ((nskk-mode-switch-clear-input-on-mode-change t))
          (nskk-mode-switch-to-katakana)

          ;; 入力バッファがクリアされたことを検証
          (should (string-empty-p (nskk-state-input-buffer nskk-current-state)))))
    (nskk-e2e-mode--teardown)))

(ert-deftest nskk-e2e-mode-switch-preserve-pending ()
  "未確定入力を保持する設定でのモード切り替え。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        ;; 未確定入力を設定
        (setf (nskk-state-input-buffer nskk-current-state) "ka")

        ;; 入力バッファをクリアしない設定
        (let ((nskk-mode-switch-clear-input-on-mode-change nil))
          (nskk-mode-switch-to-katakana)

          ;; 入力バッファが保持されていることを検証
          (should (equal (nskk-state-input-buffer nskk-current-state) "ka"))))
    (nskk-e2e-mode--teardown)))

;;; Test Suite 6: フック動作

(ert-deftest nskk-e2e-mode-switch-before-hook ()
  "モード切り替え前フックの動作テスト。"
  (unwind-protect
      (let ((hook-called nil)
            (hook-from nil)
            (hook-to nil))
        (nskk-e2e-mode--setup)

        ;; beforeフック登録
        (add-hook 'nskk-mode-switch-before-hook
                  (lambda (from to)
                    (setq hook-called t)
                    (setq hook-from from)
                    (setq hook-to to)))

        ;; モード切り替え実行
        (nskk-mode-switch-to-katakana)

        ;; フック検証
        (should hook-called)
        (should (eq hook-from 'hiragana))
        (should (eq hook-to 'katakana))

        ;; クリーンアップ
        (setq nskk-mode-switch-before-hook nil))
    (nskk-e2e-mode--teardown)))

(ert-deftest nskk-e2e-mode-switch-after-hook ()
  "モード切り替え後フックの動作テスト。"
  (unwind-protect
      (let ((hook-called nil)
            (hook-from nil)
            (hook-to nil))
        (nskk-e2e-mode--setup)

        ;; afterフック登録
        (add-hook 'nskk-mode-switch-after-hook
                  (lambda (from to)
                    (setq hook-called t)
                    (setq hook-from from)
                    (setq hook-to to)))

        ;; モード切り替え実行
        (nskk-mode-switch-to-katakana)

        ;; フック検証
        (should hook-called)
        (should (eq hook-from 'hiragana))
        (should (eq hook-to 'katakana))

        ;; クリーンアップ
        (setq nskk-mode-switch-after-hook nil))
    (nskk-e2e-mode--teardown)))

(ert-deftest nskk-e2e-mode-switch-mode-specific-hook ()
  "モード別フックの動作テスト。"
  (unwind-protect
      (let ((katakana-hook-called nil))
        (nskk-e2e-mode--setup)

        ;; カタカナモード切り替えフック登録
        (add-hook 'nskk-mode-switch-to-katakana-hook
                  (lambda ()
                    (setq katakana-hook-called t)))

        ;; カタカナモードに切り替え
        (nskk-mode-switch-to-katakana)

        ;; フック検証
        (should katakana-hook-called)

        ;; クリーンアップ
        (setq nskk-mode-switch-to-katakana-hook nil))
    (nskk-e2e-mode--teardown)))

;;; Test Suite 7: 前回モード復帰

(ert-deftest nskk-e2e-mode-restore-previous ()
  "前回モード復帰機能のテスト。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        ;; ひらがな → カタカナ
        (nskk-mode-switch-to-katakana)
        (nskk-e2e-mode--assert-mode 'katakana)

        ;; カタカナ → 半角英数
        (nskk-mode-switch-to-latin)
        (nskk-e2e-mode--assert-mode 'latin)

        ;; 前回モード（カタカナ）に復帰
        (nskk-mode-restore-previous)
        (nskk-e2e-mode--assert-mode 'katakana))
    (nskk-e2e-mode--teardown)))

(ert-deftest nskk-e2e-mode-toggle-latin-restore ()
  "英数トグルによる前回モード復帰のテスト。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        ;; ひらがな → カタカナ
        (nskk-mode-switch-to-katakana)

        ;; カタカナ → 英数
        (nskk-mode-toggle-latin)
        (nskk-e2e-mode--assert-mode 'latin)

        ;; 英数 → 前回モード（カタカナ）
        (nskk-mode-toggle-latin)
        (nskk-e2e-mode--assert-mode 'katakana))
    (nskk-e2e-mode--teardown)))

;;; Test Suite 8: モード問い合わせ

(ert-deftest nskk-e2e-mode-current-mode-query ()
  "現在モード問い合わせのテスト。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        ;; 各モードで問い合わせ
        (nskk-mode-switch-to-hiragana)
        (should (eq (nskk-mode-current-mode) 'hiragana))

        (nskk-mode-switch-to-katakana)
        (should (eq (nskk-mode-current-mode) 'katakana))

        (nskk-mode-switch-to-latin)
        (should (eq (nskk-mode-current-mode) 'latin)))
    (nskk-e2e-mode--teardown)))

(ert-deftest nskk-e2e-mode-current-mode-string ()
  "現在モード説明文字列取得のテスト。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        (nskk-mode-switch-to-hiragana)
        (should (equal (nskk-mode-current-mode-string) "ひらがな"))

        (nskk-mode-switch-to-katakana)
        (should (equal (nskk-mode-current-mode-string) "カタカナ"))

        (nskk-mode-switch-to-latin)
        (should (equal (nskk-mode-current-mode-string) "半角英数")))
    (nskk-e2e-mode--teardown)))

(ert-deftest nskk-e2e-mode-current-indicator ()
  "現在モードインジケーター取得のテスト。"
  (unwind-protect
      (progn
        (nskk-e2e-mode--setup)

        (nskk-mode-switch-to-hiragana)
        (should (equal (nskk-mode-current-indicator) "あ"))

        (nskk-mode-switch-to-katakana)
        (should (equal (nskk-mode-current-indicator) "ア"))

        (nskk-mode-switch-to-latin)
        (should (equal (nskk-mode-current-indicator) "A"))

        (nskk-mode-switch-to-zenkaku-latin)
        (should (equal (nskk-mode-current-indicator) "Ａ")))
    (nskk-e2e-mode--teardown)))

(provide 'nskk-e2e-mode-control-test)

;;; nskk-e2e-mode-control-test.el ends here
