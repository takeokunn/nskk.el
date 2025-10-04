;;; nskk-minibuffer-test.el --- Tests for nskk-minibuffer.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; nskk-minibuffer.el のテストスイート
;; ERTフレームワークを使用した包括的なテストを実装

;;; Code:

(require 'ert)
(require 'nskk-state)
(require 'nskk-minibuffer)

;;; テスト用ヘルパー

(defmacro nskk-minibuffer-test-with-temp-buffer (&rest body)
  "テスト用の一時バッファで BODY を実行する。"
  `(with-temp-buffer
     (nskk-state-init)
     ,@body
     (nskk-minibuffer-cleanup)))

;;; 初期化・クリーンアップテスト

(ert-deftest nskk-minibuffer-test-init ()
  "ミニバッファの初期化をテストする。"
  (nskk-minibuffer-cleanup)
  (nskk-minibuffer-init)
  (should nskk-minibuffer--state)
  (should (nskk-minibuffer-state-p nskk-minibuffer--state))
  (should-not (nskk-minibuffer-state-visible nskk-minibuffer--state))
  (nskk-minibuffer-cleanup))

(ert-deftest nskk-minibuffer-test-cleanup ()
  "クリーンアップをテストする。"
  (nskk-minibuffer-init)
  (nskk-minibuffer-cleanup)
  (should-not nskk-minibuffer--state))

(ert-deftest nskk-minibuffer-test-multiple-init ()
  "複数回の初期化をテストする（冪等性）。"
  (nskk-minibuffer-cleanup)
  (nskk-minibuffer-init)
  (let ((state1 nskk-minibuffer--state))
    (nskk-minibuffer-init)
    (should (eq state1 nskk-minibuffer--state)))
  (nskk-minibuffer-cleanup))

;;; 基本的な表示テスト

(ert-deftest nskk-minibuffer-test-show ()
  "基本的な表示をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (should (nskk-minibuffer-visible-p))
   (should (equal (nskk-minibuffer-current-text) "あ"))))

(ert-deftest nskk-minibuffer-test-show-with-face ()
  "フェイス指定での表示をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ" 'highlight)
   (should (nskk-minibuffer-visible-p))
   (should (equal (nskk-minibuffer-current-text) "あ"))))

(ert-deftest nskk-minibuffer-test-show-with-mode ()
  "モード指定での表示をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "ア" nil 'katakana)
   (should (nskk-minibuffer-visible-p))
   (should (equal (nskk-minibuffer-current-text) "ア"))
   (should (eq (nskk-minibuffer-state-mode nskk-minibuffer--state) 'katakana))))

(ert-deftest nskk-minibuffer-test-hide ()
  "非表示化をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (should (nskk-minibuffer-visible-p))
   (nskk-minibuffer-hide)
   (should-not (nskk-minibuffer-visible-p))
   (should-not (nskk-minibuffer-current-text))))

(ert-deftest nskk-minibuffer-test-update ()
  "テキスト更新をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (should (nskk-minibuffer-update "あい"))
   (should (equal (nskk-minibuffer-current-text) "あい"))))

(ert-deftest nskk-minibuffer-test-update-when-hidden ()
  "非表示時の更新をテストする（失敗するべき）。"
  (nskk-minibuffer-test-with-temp-buffer
   (should-not (nskk-minibuffer-update "あ"))))

;;; インライン候補テスト

(ert-deftest nskk-minibuffer-test-show-inline-candidate ()
  "インライン候補の表示をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (nskk-minibuffer-show-inline-candidate "愛")
   (should (nskk-minibuffer-has-inline-candidate-p))
   (should (equal (nskk-minibuffer-current-inline-candidate) "愛"))))

(ert-deftest nskk-minibuffer-test-hide-inline-candidate ()
  "インライン候補の非表示化をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (nskk-minibuffer-show-inline-candidate "愛")
   (should (nskk-minibuffer-has-inline-candidate-p))
   (nskk-minibuffer-hide-inline-candidate)
   (should-not (nskk-minibuffer-has-inline-candidate-p))
   (should-not (nskk-minibuffer-current-inline-candidate))))

(ert-deftest nskk-minibuffer-test-update-inline-candidate ()
  "インライン候補の更新をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (should (nskk-minibuffer-update-inline-candidate "愛"))
   (should (equal (nskk-minibuffer-current-inline-candidate) "愛"))
   (should (nskk-minibuffer-update-inline-candidate "会"))
   (should (equal (nskk-minibuffer-current-inline-candidate) "会"))))

(ert-deftest nskk-minibuffer-test-update-inline-candidate-empty ()
  "空のインライン候補での更新をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (nskk-minibuffer-show-inline-candidate "愛")
   (should (nskk-minibuffer-update-inline-candidate ""))
   (should-not (nskk-minibuffer-has-inline-candidate-p))))

(ert-deftest nskk-minibuffer-test-update-inline-candidate-nil ()
  "nilのインライン候補での更新をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (nskk-minibuffer-show-inline-candidate "愛")
   (should (nskk-minibuffer-update-inline-candidate nil))
   (should-not (nskk-minibuffer-has-inline-candidate-p))))

(ert-deftest nskk-minibuffer-test-accept-inline-candidate ()
  "インライン候補の確定をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (nskk-minibuffer-show-inline-candidate "愛")
   (let ((result (nskk-minibuffer-accept-inline-candidate)))
     (should (equal result "愛"))
     (should-not (nskk-minibuffer-has-inline-candidate-p)))))

(ert-deftest nskk-minibuffer-test-accept-inline-candidate-when-none ()
  "インライン候補がない時の確定をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (should-not (nskk-minibuffer-accept-inline-candidate))))

(ert-deftest nskk-minibuffer-test-inline-disabled ()
  "インライン候補が無効な時のテスト。"
  (nskk-minibuffer-test-with-temp-buffer
   (let ((nskk-minibuffer-enable-inline nil))
     (nskk-minibuffer-show "あ")
     (nskk-minibuffer-show-inline-candidate "愛")
     ;; 無効化されているため表示されない
     (should-not (nskk-minibuffer-has-inline-candidate-p)))))

;;; プロンプトテスト

(ert-deftest nskk-minibuffer-test-set-prompt ()
  "プロンプト設定をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-set-prompt "▽")
   (should (equal (nskk-minibuffer-state-prompt nskk-minibuffer--state) "▽"))))

(ert-deftest nskk-minibuffer-test-set-prompt-with-face ()
  "フェイス付きプロンプト設定をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-set-prompt "▽" 'minibuffer-prompt)
   (let ((prompt (nskk-minibuffer-state-prompt nskk-minibuffer--state)))
     (should (equal (substring-no-properties prompt) "▽"))
     (should (get-text-property 0 'face prompt)))))

(ert-deftest nskk-minibuffer-test-update-prompt ()
  "モード別プロンプト更新をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-update-prompt 'hiragana)
   (should (nskk-minibuffer-state-prompt nskk-minibuffer--state))
   (nskk-minibuffer-update-prompt 'conversion)
   (should (nskk-minibuffer-state-prompt nskk-minibuffer--state))))

(ert-deftest nskk-minibuffer-test-prompt-with-mode-indicator-disabled ()
  "モードインジケーター無効時のテスト。"
  (nskk-minibuffer-test-with-temp-buffer
   (let ((nskk-minibuffer-show-mode-indicator nil))
     (nskk-minibuffer-show "あ" nil 'hiragana)
     ;; インジケーター無効時はプロンプトが空
     (should (equal (nskk-minibuffer-state-prompt nskk-minibuffer--state) "")))))

;;; カーソル位置管理テスト

(ert-deftest nskk-minibuffer-test-set-cursor-pos ()
  "カーソル位置設定をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-set-cursor-pos 5)
   (should (= (nskk-minibuffer-get-cursor-pos) 5))))

(ert-deftest nskk-minibuffer-test-cursor-pos-default ()
  "デフォルトのカーソル位置をテストする。"
  (nskk-minibuffer-cleanup)
  (should (= (nskk-minibuffer-get-cursor-pos) 0)))

(ert-deftest nskk-minibuffer-test-cursor-pos-update ()
  "カーソル位置の更新をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-set-cursor-pos 3)
   (should (= (nskk-minibuffer-get-cursor-pos) 3))
   (nskk-minibuffer-set-cursor-pos 7)
   (should (= (nskk-minibuffer-get-cursor-pos) 7))))

;;; 統合関数テスト

(ert-deftest nskk-minibuffer-test-show-with-candidate ()
  "テキストと候補の同時表示をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show-with-candidate "あ" "愛")
   (should (nskk-minibuffer-visible-p))
   (should (equal (nskk-minibuffer-current-text) "あ"))
   (should (equal (nskk-minibuffer-current-inline-candidate) "愛"))))

(ert-deftest nskk-minibuffer-test-show-with-candidate-empty ()
  "空の候補での同時表示をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show-with-candidate "あ" "")
   (should (nskk-minibuffer-visible-p))
   (should (equal (nskk-minibuffer-current-text) "あ"))
   (should-not (nskk-minibuffer-has-inline-candidate-p))))

(ert-deftest nskk-minibuffer-test-show-with-candidate-nil ()
  "nilの候補での同時表示をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show-with-candidate "あ" nil)
   (should (nskk-minibuffer-visible-p))
   (should (equal (nskk-minibuffer-current-text) "あ"))
   (should-not (nskk-minibuffer-has-inline-candidate-p))))

(ert-deftest nskk-minibuffer-test-show-with-candidate-and-mode ()
  "モード指定での同時表示をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show-with-candidate "ア" "愛" 'katakana)
   (should (nskk-minibuffer-visible-p))
   (should (equal (nskk-minibuffer-current-text) "ア"))
   (should (equal (nskk-minibuffer-current-inline-candidate) "愛"))
   (should (eq (nskk-minibuffer-state-mode nskk-minibuffer--state) 'katakana))))

;;; 状態問い合わせテスト

(ert-deftest nskk-minibuffer-test-visible-p ()
  "表示状態の判定をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (should-not (nskk-minibuffer-visible-p))
   (nskk-minibuffer-show "あ")
   (should (nskk-minibuffer-visible-p))
   (nskk-minibuffer-hide)
   (should-not (nskk-minibuffer-visible-p))))

(ert-deftest nskk-minibuffer-test-has-inline-candidate-p ()
  "インライン候補の有無判定をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (should-not (nskk-minibuffer-has-inline-candidate-p))
   (nskk-minibuffer-show-inline-candidate "愛")
   (should (nskk-minibuffer-has-inline-candidate-p))
   (nskk-minibuffer-hide-inline-candidate)
   (should-not (nskk-minibuffer-has-inline-candidate-p))))

(ert-deftest nskk-minibuffer-test-current-text ()
  "現在のテキスト取得をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (should-not (nskk-minibuffer-current-text))
   (nskk-minibuffer-show "あいう")
   (should (equal (nskk-minibuffer-current-text) "あいう"))
   (nskk-minibuffer-hide)
   (should-not (nskk-minibuffer-current-text))))

;;; オーバーレイ管理テスト

(ert-deftest nskk-minibuffer-test-overlay-creation ()
  "オーバーレイの作成をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (when nskk-minibuffer-use-overlay
     (should (nskk-minibuffer-state-overlay nskk-minibuffer--state))
     (should (overlay-buffer (nskk-minibuffer-state-overlay nskk-minibuffer--state))))))

(ert-deftest nskk-minibuffer-test-overlay-deletion ()
  "オーバーレイの削除をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (when nskk-minibuffer-use-overlay
     (let ((ov (nskk-minibuffer-state-overlay nskk-minibuffer--state)))
       (should ov)
       (nskk-minibuffer-hide)
       (should-not (overlay-buffer ov))))))

(ert-deftest nskk-minibuffer-test-inline-overlay-creation ()
  "インライン候補オーバーレイの作成をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (nskk-minibuffer-show-inline-candidate "愛")
   (should (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state))
   (should (overlay-buffer (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state)))))

(ert-deftest nskk-minibuffer-test-inline-overlay-deletion ()
  "インライン候補オーバーレイの削除をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (nskk-minibuffer-show-inline-candidate "愛")
   (let ((ov (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state)))
     (should ov)
     (nskk-minibuffer-hide-inline-candidate)
     (should-not (overlay-buffer ov)))))

;;; 複雑なシナリオテスト

(ert-deftest nskk-minibuffer-test-multiple-updates ()
  "複数回の更新をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (should (equal (nskk-minibuffer-current-text) "あ"))
   (nskk-minibuffer-update "あい")
   (should (equal (nskk-minibuffer-current-text) "あい"))
   (nskk-minibuffer-update "あいう")
   (should (equal (nskk-minibuffer-current-text) "あいう"))))

(ert-deftest nskk-minibuffer-test-candidate-change ()
  "候補の切り替えをテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (nskk-minibuffer-show-inline-candidate "愛")
   (should (equal (nskk-minibuffer-current-inline-candidate) "愛"))
   (nskk-minibuffer-show-inline-candidate "会")
   (should (equal (nskk-minibuffer-current-inline-candidate) "会"))
   (nskk-minibuffer-show-inline-candidate "合")
   (should (equal (nskk-minibuffer-current-inline-candidate) "合"))))

(ert-deftest nskk-minibuffer-test-mode-change ()
  "モード変更をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ" nil 'hiragana)
   (should (eq (nskk-minibuffer-state-mode nskk-minibuffer--state) 'hiragana))
   (nskk-minibuffer-update "ア" nil 'katakana)
   (should (eq (nskk-minibuffer-state-mode nskk-minibuffer--state) 'katakana))
   (nskk-minibuffer-update "あ" nil 'conversion)
   (should (eq (nskk-minibuffer-state-mode nskk-minibuffer--state) 'conversion))))

(ert-deftest nskk-minibuffer-test-show-hide-cycle ()
  "表示・非表示のサイクルをテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (dotimes (_ 3)
     (nskk-minibuffer-show "あ")
     (should (nskk-minibuffer-visible-p))
     (nskk-minibuffer-hide)
     (should-not (nskk-minibuffer-visible-p)))))

(ert-deftest nskk-minibuffer-test-inline-candidate-cycle ()
  "インライン候補の表示・非表示サイクルをテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   (dotimes (_ 3)
     (nskk-minibuffer-show-inline-candidate "愛")
     (should (nskk-minibuffer-has-inline-candidate-p))
     (nskk-minibuffer-hide-inline-candidate)
     (should-not (nskk-minibuffer-has-inline-candidate-p)))))

;;; 統計・デバッグテスト

(ert-deftest nskk-minibuffer-test-stats ()
  "統計情報の取得をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ" nil 'hiragana)
   (nskk-minibuffer-show-inline-candidate "愛")
   (let ((stats (nskk-minibuffer-stats)))
     (should (plist-get stats :visible))
     (should (equal (plist-get stats :text) "あ"))
     (should (equal (plist-get stats :inline-candidate) "愛"))
     (should (eq (plist-get stats :mode) 'hiragana)))))

(ert-deftest nskk-minibuffer-test-stats-when-hidden ()
  "非表示時の統計情報をテストする。"
  (nskk-minibuffer-cleanup)
  (let ((stats (nskk-minibuffer-stats)))
    (should-not (plist-get stats :visible))))

(ert-deftest nskk-minibuffer-test-describe ()
  "describe関数をテストする（エラーが出ないことを確認）。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ")
   ;; describe関数はmessageを呼び出すため、値を返す
   ;; エラーが出ないことだけを確認
   (should (stringp (nskk-minibuffer-describe)))))

;;; エッジケーステスト

(ert-deftest nskk-minibuffer-test-empty-text ()
  "空文字列の表示をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "")
   (should (nskk-minibuffer-visible-p))
   (should (equal (nskk-minibuffer-current-text) ""))))

(ert-deftest nskk-minibuffer-test-long-text ()
  "長い文字列の表示をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (let ((long-text (make-string 100 ?あ)))
     (nskk-minibuffer-show long-text)
     (should (equal (nskk-minibuffer-current-text) long-text)))))

(ert-deftest nskk-minibuffer-test-multibyte-text ()
  "マルチバイト文字列をテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ漢字𠮷")
   (should (equal (nskk-minibuffer-current-text) "あ漢字𠮷"))))

(ert-deftest nskk-minibuffer-test-special-characters ()
  "特殊文字を含むテキストをテストする。"
  (nskk-minibuffer-test-with-temp-buffer
   (nskk-minibuffer-show "あ\nい\tう")
   (should (equal (nskk-minibuffer-current-text) "あ\nい\tう"))))

(provide 'nskk-minibuffer-test)

;;; nskk-minibuffer-test.el ends here
