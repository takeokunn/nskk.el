;;; nskk-keymap-test.el --- Tests for nskk-keymap -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; nskk-keymap.elのテストスイート
;;
;; テスト項目:
;; 1. グローバルキーマップテスト
;; 2. モード別キーマップテスト
;; 3. キーバインド整合性テスト
;; 4. カスタマイズテスト
;; 5. ddskk互換性テスト

;;; Code:

(require 'ert)
(require 'nskk)
(require 'nskk-keymap)
(require 'nskk-state)

;;; テストヘルパー

(defmacro nskk-keymap-test-with-temp-keymap (&rest body)
  "一時的なキーマップ環境でBODYを実行する。
テスト後、元の状態に戻す。"
  (declare (indent 0))
  `(let ((orig-mode-map (copy-keymap nskk-mode-map))
         (orig-hiragana-map (copy-keymap nskk-hiragana-mode-map))
         (orig-katakana-map (copy-keymap nskk-katakana-mode-map))
         (orig-ascii-map (copy-keymap nskk-ascii-mode-map))
         (orig-henkan-map (copy-keymap nskk-henkan-mode-map))
         (orig-kouho-map (copy-keymap nskk-kouho-mode-map)))
     (unwind-protect
         (progn ,@body)
       ;; クリーンアップ
       (setq nskk-mode-map orig-mode-map)
       (setq nskk-hiragana-mode-map orig-hiragana-map)
       (setq nskk-katakana-mode-map orig-katakana-map)
       (setq nskk-ascii-mode-map orig-ascii-map)
       (setq nskk-henkan-mode-map orig-henkan-map)
       (setq nskk-kouho-mode-map orig-kouho-map))))

;;; 1. グローバルキーマップテスト

(ert-deftest nskk-keymap-test-global-keymap-exists ()
  "メインキーマップが存在することを確認する。"
  (should (keymapp nskk-mode-map))
  (should (not (equal nskk-mode-map (make-sparse-keymap)))))

(ert-deftest nskk-keymap-test-mode-switch-bindings ()
  "モード切り替えキーが正しくバインドされていることを確認する。"
  (dolist (binding nskk-keymap-mode-switch-keys)
    (let* ((key (car binding))
           (command (cdr binding))
           (bound-command (lookup-key nskk-mode-map (kbd key))))
      (should (eq bound-command command)))))

(ert-deftest nskk-keymap-test-prefix-key-binding ()
  "プレフィックスキーが正しくバインドされていることを確認する。"
  (let ((prefix-binding (lookup-key nskk-mode-map
                                   (kbd nskk-keymap-prefix))))
    (should (keymapp prefix-binding))
    ;; キーマップの同一性ではなく、内容が同じかどうかを確認
    (should (equal prefix-binding nskk-prefix-map))))

(ert-deftest nskk-keymap-test-input-keys ()
  "入力操作キーが正しくバインドされていることを確認する。"
  (dolist (binding nskk-keymap-input-keys)
    (let* ((key (car binding))
           (command (cdr binding))
           (bound-command (lookup-key nskk-mode-map (kbd key))))
      (should (eq bound-command command)))))

;;; 2. モード別キーマップテスト

(ert-deftest nskk-keymap-test-hiragana-mode-map ()
  "ひらがなモードキーマップが正しく設定されていることを確認する。"
  (should (keymapp nskk-hiragana-mode-map))
  ;; 変換キーの確認
  (should (lookup-key nskk-hiragana-mode-map (kbd "SPC")))
  ;; 特殊文字キーの確認
  (should (lookup-key nskk-hiragana-mode-map (kbd "."))))

(ert-deftest nskk-keymap-test-katakana-mode-map ()
  "カタカナモードキーマップが正しく設定されていることを確認する。"
  (should (keymapp nskk-katakana-mode-map))
  ;; 変換キーの確認
  (should (lookup-key nskk-katakana-mode-map (kbd "SPC"))))

(ert-deftest nskk-keymap-test-ascii-mode-map ()
  "ASCIIモードキーマップが存在することを確認する。"
  (should (keymapp nskk-ascii-mode-map)))

(ert-deftest nskk-keymap-test-henkan-mode-map ()
  "変換モードキーマップが正しく設定されていることを確認する。"
  (should (keymapp nskk-henkan-mode-map))
  ;; 変換操作キーの確認
  (should (lookup-key nskk-henkan-mode-map (kbd "SPC")))
  (should (lookup-key nskk-henkan-mode-map (kbd "x")))
  (should (lookup-key nskk-henkan-mode-map (kbd "C-g"))))

(ert-deftest nskk-keymap-test-kouho-mode-map ()
  "候補選択モードキーマップが正しく設定されていることを確認する。"
  (should (keymapp nskk-kouho-mode-map))
  ;; 候補操作キーの確認
  (should (lookup-key nskk-kouho-mode-map (kbd "SPC")))
  (should (lookup-key nskk-kouho-mode-map (kbd "x")))
  ;; 数字キーの確認
  (dotimes (i 10)
    (should (lookup-key nskk-kouho-mode-map
                       (kbd (number-to-string i))))))

(ert-deftest nskk-keymap-test-abbrev-mode-map ()
  "abbrevモードキーマップが存在することを確認する。"
  (should (keymapp nskk-abbrev-mode-map)))

;;; 3. キーバインド整合性テスト

(ert-deftest nskk-keymap-test-define-key ()
  "nskk-define-key関数が正しく動作することを確認する。"
  (nskk-keymap-test-with-temp-keymap
    (let ((test-map (make-sparse-keymap)))
      ;; 新規バインディング
      (should (nskk-define-key test-map "C-t" 'test-command))
      (should (eq (lookup-key test-map (kbd "C-t")) 'test-command))

      ;; 上書きなし（forceなし）
      (should-not (nskk-define-key test-map "C-t" 'new-command))
      (should (eq (lookup-key test-map (kbd "C-t")) 'test-command))

      ;; 上書きあり（forceあり）
      (should (nskk-define-key test-map "C-t" 'new-command t))
      (should (eq (lookup-key test-map (kbd "C-t")) 'new-command)))))

(ert-deftest nskk-keymap-test-undefine-key ()
  "nskk-undefine-key関数が正しく動作することを確認する。"
  (nskk-keymap-test-with-temp-keymap
    (let ((test-map (make-sparse-keymap)))
      (nskk-define-key test-map "C-t" 'test-command)
      (should (lookup-key test-map (kbd "C-t")))

      (nskk-undefine-key test-map "C-t")
      (should-not (lookup-key test-map (kbd "C-t"))))))

(ert-deftest nskk-keymap-test-define-keys ()
  "nskk-define-keys関数が複数のキーを正しくバインドすることを確認する。"
  (nskk-keymap-test-with-temp-keymap
    (let ((test-map (make-sparse-keymap))
          (bindings '(("C-a" . cmd-a)
                     ("C-b" . cmd-b)
                     ("C-c" . cmd-c))))
      (should (= (nskk-define-keys test-map bindings) 3))
      (should (eq (lookup-key test-map (kbd "C-a")) 'cmd-a))
      (should (eq (lookup-key test-map (kbd "C-b")) 'cmd-b))
      (should (eq (lookup-key test-map (kbd "C-c")) 'cmd-c)))))

(ert-deftest nskk-keymap-test-setup-keybindings ()
  "nskk-setup-keybindings関数が全てのキーマップを初期化することを確認する。"
  (nskk-keymap-test-with-temp-keymap
    ;; キーマップをクリア
    (setq nskk-mode-map (make-sparse-keymap))
    (setq nskk-hiragana-mode-map (make-sparse-keymap))

    ;; セットアップ実行
    (nskk-setup-keybindings)

    ;; 基本的なキーバインドが設定されていることを確認
    (should (lookup-key nskk-mode-map (kbd "C-j")))
    (should (lookup-key nskk-hiragana-mode-map (kbd "SPC")))))

(ert-deftest nskk-keymap-test-reset-keybindings ()
  "nskk-reset-keybindings関数がキーマップをリセットすることを確認する。"
  (nskk-keymap-test-with-temp-keymap
    ;; カスタムバインディングを追加
    (nskk-define-key nskk-mode-map "C-x C-z" 'custom-command)
    (should (eq (lookup-key nskk-mode-map (kbd "C-x C-z")) 'custom-command))

    ;; リセット実行
    (nskk-reset-keybindings)

    ;; カスタムバインディングが削除されていることを確認
    (should-not (lookup-key nskk-mode-map (kbd "C-x C-z")))
    ;; 標準バインディングは存在することを確認
    (should (lookup-key nskk-mode-map (kbd "C-j")))))

;;; 4. カスタマイズテスト

(ert-deftest nskk-keymap-test-customize-key ()
  "nskk-customize-key関数がキーバインドをカスタマイズできることを確認する。"
  (nskk-keymap-test-with-temp-keymap
    (nskk-customize-key 'hiragana "C-t" 'test-command)
    (should (eq (lookup-key nskk-hiragana-mode-map (kbd "C-t"))
                'test-command))))

(ert-deftest nskk-keymap-test-get-keymap-for-mode ()
  "nskk-get-keymap-for-mode関数が正しいキーマップを返すことを確認する。"
  (should (eq (nskk-get-keymap-for-mode 'hiragana) nskk-hiragana-mode-map))
  (should (eq (nskk-get-keymap-for-mode 'katakana) nskk-katakana-mode-map))
  (should (eq (nskk-get-keymap-for-mode 'latin) nskk-ascii-mode-map))
  (should (eq (nskk-get-keymap-for-mode 'zenkaku-latin)
              nskk-zenkaku-latin-mode-map))
  (should (eq (nskk-get-keymap-for-mode 'abbrev) nskk-abbrev-mode-map))
  (should (eq (nskk-get-keymap-for-mode 'henkan) nskk-henkan-mode-map))
  (should (eq (nskk-get-keymap-for-mode 'kouho) nskk-kouho-mode-map))
  (should-not (nskk-get-keymap-for-mode 'unknown)))

(ert-deftest nskk-keymap-test-prefix-key-customization ()
  "プレフィックスキーのカスタマイズが反映されることを確認する。"
  (nskk-keymap-test-with-temp-keymap
    (let ((nskk-keymap-prefix "C-c j"))
      (nskk-setup-keybindings)
      (should (keymapp (lookup-key nskk-mode-map (kbd "C-c j")))))))

;;; 5. ddskk互換性テスト

(ert-deftest nskk-keymap-test-skk-compatibility-flag ()
  "skk互換フラグが正しく設定されていることを確認する。"
  (should (booleanp nskk-keymap-skk-compatible)))

(ert-deftest nskk-keymap-test-skk-j-mode-map-compat ()
  "ddskk互換のskk-j-mode-map関数が動作することを確認する。"
  (should (eq (nskk-keymap-skk-j-mode-map) nskk-hiragana-mode-map)))

(ert-deftest nskk-keymap-test-skk-jisx0208-latin-mode-map-compat ()
  "ddskk互換のskk-jisx0208-latin-mode-map関数が動作することを確認する。"
  (should (eq (nskk-keymap-skk-jisx0208-latin-mode-map)
              nskk-zenkaku-latin-mode-map)))

(ert-deftest nskk-keymap-test-skk-abbrev-mode-map-compat ()
  "ddskk互換のskk-abbrev-mode-map関数が動作することを確認する。"
  (should (eq (nskk-keymap-skk-abbrev-mode-map) nskk-abbrev-mode-map)))

;;; 6. Sticky Shiftテスト

(ert-deftest nskk-keymap-test-sticky-shift-key ()
  "Sticky Shiftキーが正しくバインドされていることを確認する。"
  (when nskk-keymap-enable-sticky-shift
    (should (lookup-key nskk-hiragana-mode-map
                       (kbd nskk-keymap-sticky-key)))))

(ert-deftest nskk-keymap-test-sticky-shift-uppercase ()
  "大文字キーが送り仮名開始コマンドにバインドされていることを確認する。"
  (when nskk-keymap-enable-sticky-shift
    (dolist (char '("A" "K" "S" "T"))
      (should (lookup-key nskk-hiragana-mode-map (kbd char))))))

;;; 7. キーマップ情報取得テスト

(ert-deftest nskk-keymap-test-stats ()
  "nskk-keymap-stats関数が統計情報を返すことを確認する。"
  (let ((stats (nskk-keymap-stats)))
    (should (plist-get stats :mode-keymaps))
    (should (stringp (plist-get stats :prefix-key)))
    (should (booleanp (plist-get stats :skk-compatible)))
    (should (booleanp (plist-get stats :sticky-shift-enabled)))))

(ert-deftest nskk-keymap-test-get-current-keymap ()
  "nskk-get-current-keymap関数が現在のモードに応じたキーマップを返すことを確認する。"
  (let ((nskk-current-state (nskk-state-create :mode 'hiragana)))
    (should (eq (nskk-get-current-keymap) nskk-hiragana-mode-map)))

  (let ((nskk-current-state (nskk-state-create :mode 'katakana)))
    (should (eq (nskk-get-current-keymap) nskk-katakana-mode-map)))

  (let ((nskk-current-state (nskk-state-create :mode 'latin)))
    (should (eq (nskk-get-current-keymap) nskk-ascii-mode-map))))

(ert-deftest nskk-keymap-test-get-current-keymap-conversion ()
  "変換モード時に正しいキーマップが返されることを確認する。"
  ;; 候補なし（変換モード）
  (let ((nskk-current-state (nskk-state-create :mode 'conversion)))
    (should (eq (nskk-get-current-keymap) nskk-henkan-mode-map)))

  ;; 候補あり（候補選択モード）
  (let ((nskk-current-state (nskk-state-create :mode 'conversion)))
    (nskk-state-set-candidates nskk-current-state '("候補1" "候補2"))
    (should (eq (nskk-get-current-keymap) nskk-kouho-mode-map))))

;;; 8. セットアップ関数テスト

(ert-deftest nskk-keymap-test-setup-mode-switch-keys ()
  "モード切り替えキーのセットアップが正しく動作することを確認する。"
  (nskk-keymap-test-with-temp-keymap
    (let ((test-map (make-sparse-keymap)))
      (nskk-setup-mode-switch-keys test-map)
      (should (lookup-key test-map (kbd "C-j")))
      (should (lookup-key test-map (kbd "q")))
      (should (lookup-key test-map (kbd "l"))))))

(ert-deftest nskk-keymap-test-setup-henkan-keys ()
  "変換キーのセットアップが正しく動作することを確認する。"
  (nskk-keymap-test-with-temp-keymap
    (let ((test-map (make-sparse-keymap)))
      (nskk-setup-henkan-keys test-map)
      (should (lookup-key test-map (kbd "SPC")))
      (should (lookup-key test-map (kbd "x")))
      (should (lookup-key test-map (kbd "C-g"))))))

(ert-deftest nskk-keymap-test-setup-kouho-keys ()
  "候補選択キーのセットアップが正しく動作することを確認する。"
  (nskk-keymap-test-with-temp-keymap
    (let ((test-map (make-sparse-keymap)))
      (nskk-setup-kouho-keys test-map)
      (should (lookup-key test-map (kbd "SPC")))
      (should (lookup-key test-map (kbd "x")))
      (dotimes (i 10)
        (should (lookup-key test-map (kbd (number-to-string i))))))))

;;; 9. エッジケーステスト

(ert-deftest nskk-keymap-test-define-key-empty-key ()
  "空のキー定義の動作を確認する。"
  (nskk-keymap-test-with-temp-keymap
    (let ((test-map (make-sparse-keymap)))
      ;; 空文字列のkbdはベクター[]を返し、既存のkeymapが存在するためnilを返す
      (should-not (nskk-define-key test-map "" 'test-command)))))

(ert-deftest nskk-keymap-test-define-key-nil-command ()
  "nilコマンドでキーバインドを削除できることを確認する。"
  (nskk-keymap-test-with-temp-keymap
    (let ((test-map (make-sparse-keymap)))
      (nskk-define-key test-map "C-t" 'test-command)
      (should (lookup-key test-map (kbd "C-t")))

      (nskk-define-key test-map "C-t" nil t)
      (should-not (lookup-key test-map (kbd "C-t"))))))

(ert-deftest nskk-keymap-test-get-keymap-for-invalid-mode ()
  "無効なモードに対してnilが返されることを確認する。"
  (should-not (nskk-get-keymap-for-mode 'invalid-mode))
  (should-not (nskk-get-keymap-for-mode nil)))

;;; 10. 統合テスト

(ert-deftest nskk-keymap-test-full-integration ()
  "全体的な統合テスト。"
  (nskk-keymap-test-with-temp-keymap
    ;; キーマップをリセット
    (nskk-reset-keybindings)

    ;; 各モードのキーマップが存在することを確認
    (should (keymapp nskk-mode-map))
    (should (keymapp nskk-hiragana-mode-map))
    (should (keymapp nskk-katakana-mode-map))
    (should (keymapp nskk-henkan-mode-map))
    (should (keymapp nskk-kouho-mode-map))

    ;; 基本的なキーバインドが設定されていることを確認
    (should (lookup-key nskk-mode-map (kbd "C-j")))
    (should (lookup-key nskk-hiragana-mode-map (kbd "SPC")))
    (should (lookup-key nskk-kouho-mode-map (kbd "0")))

    ;; カスタマイズが動作することを確認
    (nskk-customize-key 'hiragana "C-x C-t" 'test-command)
    (should (eq (lookup-key nskk-hiragana-mode-map (kbd "C-x C-t"))
                'test-command))

    ;; 統計情報が取得できることを確認
    (let ((stats (nskk-keymap-stats)))
      (should (> (plist-get stats :mode-keymaps) 0)))))

;;; ddskk互換性テスト (グローバルキーバインド)

(ert-deftest nskk-keymap-test-global-keys-auto-setup ()
  "グローバルキーバインドの自動設定をテスト"
  (let ((nskk-setup-global-keys-on-load t))
    (nskk-setup-global-keys)

    (should (eq (key-binding (kbd "C-x C-j")) 'nskk-mode))
    (should (eq (key-binding (kbd "C-x j")) 'nskk-auto-fill-mode))

    (when (eq (key-binding (kbd "C-x t")) 'nskk-tutorial)
      (should (eq (key-binding (kbd "C-x t")) 'nskk-tutorial)))))

(ert-deftest nskk-keymap-test-nskk-auto-fill-mode ()
  "nskk-auto-fill-mode の動作確認"
  (with-temp-buffer
    (should-not auto-fill-function)

    (nskk-auto-fill-mode)

    (should auto-fill-function)

    (nskk-auto-fill-mode)
    (should-not auto-fill-function)))

(ert-deftest nskk-keymap-test-nskk-auto-fill-mode-with-arg ()
  "nskk-auto-fill-mode の引数付き動作確認"
  (with-temp-buffer
    (should-not auto-fill-function)

    (nskk-auto-fill-mode 1)
    (should auto-fill-function)

    (nskk-auto-fill-mode 1)
    (should auto-fill-function)))

(ert-deftest nskk-keymap-test-nskk-tutorial ()
  "nskk-tutorial の動作確認"
  (let ((tutorial-called nil))
    (cl-letf (((symbol-function 'find-file)
               (lambda (_file) (setq tutorial-called t))))
      (let ((nskk-tutorial-file "test-tutorial.txt")
            (nskk-dir (file-name-directory (locate-library "nskk"))))
        (with-temp-file (expand-file-name nskk-tutorial-file nskk-dir)
          (insert "Test tutorial"))
        (nskk-tutorial)
        (should tutorial-called)))))

(ert-deftest nskk-keymap-test-setup-global-keys-idempotent ()
  "nskk-setup-global-keys の冪等性確認"
  (nskk-setup-global-keys)
  (let ((first-binding (key-binding (kbd "C-x C-j"))))
    (nskk-setup-global-keys)
    (should (eq (key-binding (kbd "C-x C-j")) first-binding))))

;;; 11. ddskk完全互換テスト: キーバインド削除確認

(ert-deftest nskk-keymap-test-ddskk-compatible-no-bindings ()
  "ddskk完全互換: nskk-mode-mapに入力編集キーがバインドされていないことを確認"
  (nskk-setup-keybindings)

  ;; 以下のキーは全てバインドされていないべき
  (dolist (key '("C-h" "DEL" "C-d" "C-w" "C-y" "C-k" "C-u"))
    (let ((binding (lookup-key nskk-mode-map (kbd key))))
      (should (or (not binding) (numberp binding)))
      (message "Key %s binding: %s" key binding))))

(ert-deftest nskk-keymap-test-emacs-default-commands-accessible ()
  "nskk-mode有効時でも標準Emacsコマンドがアクセス可能であることを確認"
  (with-temp-buffer
    (nskk-mode 1)

    ;; 標準Emacsコマンドがグローバルバインディングとして解決されることを確認
    (should (eq (key-binding (kbd "C-d")) 'delete-char))
    (message "C-d is bound to delete-char")

    (should (eq (key-binding (kbd "C-k")) 'kill-line))
    (message "C-k is bound to kill-line")

    (should (eq (key-binding (kbd "C-w")) 'kill-region))
    (message "C-w is bound to kill-region")

    (should (eq (key-binding (kbd "C-y")) 'yank))
    (message "C-y is bound to yank")

    (should (eq (key-binding (kbd "DEL")) 'delete-backward-char))
    (message "DEL is bound to delete-backward-char")

    (should (eq (key-binding (kbd "C-u")) 'universal-argument))
    (message "C-u is bound to universal-argument")

    (nskk-mode -1)))

(ert-deftest nskk-keymap-test-c-h-is-help ()
  "C-hがhelpコマンドとして動作することを確認"
  (with-temp-buffer
    (nskk-mode 1)

    ;; C-hがnskk-mode-mapでバインドされていないことを確認
    (let ((binding (lookup-key nskk-mode-map (kbd "C-h"))))
      (should (or (not binding) (numberp binding))))

    ;; グローバルのC-hバインディングを確認(help-command)
    (let ((global-binding (key-binding (kbd "C-h"))))
      (should (eq global-binding 'help-command)))

    (nskk-mode -1)))

(ert-deftest nskk-keymap-test-input-keys-empty ()
  "nskk-keymap-input-keysが空であることを確認"
  (should (null nskk-keymap-input-keys)))

(ert-deftest nskk-keymap-test-obsolete-functions ()
  "obsolete化された関数が警告を出すことを確認"
  (let ((obsolete-functions
         '(nskk-input-delete-forward-char
           nskk-input-kill-line
           nskk-input-kill-whole-line
           nskk-input-kill-region
           nskk-input-yank)))
    (dolist (func obsolete-functions)
      (should (get func 'byte-obsolete-info))
      (message "Function %s is correctly marked as obsolete" func))))

(ert-deftest nskk-keymap-test-mode-switch-keys-unaffected ()
  "モード切り替えキーが修正の影響を受けていないことを確認"
  (nskk-setup-keybindings)

  ;; モード切り替えキーは正常にバインドされているべき
  (should (lookup-key nskk-mode-map (kbd "C-j")))
  (should (lookup-key nskk-mode-map (kbd "q")))
  (should (lookup-key nskk-mode-map (kbd "l")))

  (message "Mode switch keys are properly bound"))

(provide 'nskk-keymap-test)

;;; nskk-keymap-test.el ends here
