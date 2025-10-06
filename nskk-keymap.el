;;; nskk-keymap.el --- Keymap definitions for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

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

;; このファイルはNSKKのキーマップ定義を実装します。
;;
;; 特徴:
;; - ddskk完全互換のキーバインド設計
;; - 標準Emacsコマンドを上書きしない
;; - モード別キーマップの提供
;; - カスタマイズ可能なキーバインド設定
;; - プレフィックスキーによるコマンド体系
;;
;; キーマップ構造:
;; - nskk-mode-map           - メインキーマップ
;; - nskk-hiragana-mode-map  - ひらがなモード用
;; - nskk-katakana-mode-map  - カタカナモード用
;; - nskk-ascii-mode-map     - ASCII モード用
;; - nskk-henkan-mode-map    - 変換モード用（▽）
;; - nskk-kouho-mode-map     - 候補選択モード用（▼）
;;
;; 使用例:
;; (nskk-setup-keybindings)
;; (nskk-define-key nskk-mode-map "C-j" 'nskk-mode-switch-to-hiragana)

;;; Code:

(require 'cl-lib)

;;; カスタマイズグループ

(defgroup nskk-keymap nil
  "NSKK keymap customization."
  :group 'nskk
  :prefix "nskk-keymap-")

;;; カスタマイズ変数

(defcustom nskk-keymap-prefix "C-x j"
  "NSKKコマンドのプレフィックスキー。
このキーを押した後にNSKK固有のコマンドを実行できる。"
  :type 'string
  :group 'nskk-keymap)

(defcustom nskk-keymap-skk-compatible t
  "非nilの場合、ddskk互換のキーバインドを使用する。
nilの場合、NSKKオリジナルのキーバインドを使用する。"
  :type 'boolean
  :group 'nskk-keymap)

(defcustom nskk-keymap-use-jisx0201-input-method nil
  "非nilの場合、半角カナ入力を有効化する。
ddskkの`skk-use-jisx0201-input-method'互換。"
  :type 'boolean
  :group 'nskk-keymap)

(defcustom nskk-keymap-sticky-key ";"
  "Sticky Shift用のキー。
このキーを押すとShiftを押したのと同じ動作をする。"
  :type 'string
  :group 'nskk-keymap)

(defcustom nskk-keymap-enable-sticky-shift t
  "非nilの場合、Sticky Shift機能を有効化する。"
  :type 'boolean
  :group 'nskk-keymap)

;;; キーマップ変数

(defvar nskk-mode-map
  (make-sparse-keymap)
  "NSKK minor modeのメインキーマップ。
全てのNSKK機能へのエントリーポイント。")

(defvar nskk-hiragana-mode-map
  (make-sparse-keymap)
  "ひらがな入力モード用のキーマップ。
ローマ字からひらがなへの変換処理を行う。")

(defvar nskk-katakana-mode-map
  (make-sparse-keymap)
  "カタカナ入力モード用のキーマップ。
ローマ字からカタカナへの変換処理を行う。")

(defvar nskk-ascii-mode-map
  (make-sparse-keymap)
  "ASCII入力モード用のキーマップ。
半角英数字を直接入力する。")

(defvar nskk-zenkaku-latin-mode-map
  (make-sparse-keymap)
  "全角英数入力モード用のキーマップ。
全角英数字を入力する。")

(defvar nskk-henkan-mode-map
  (make-sparse-keymap)
  "変換モード（▽モード）用のキーマップ。
変換対象文字列を入力中の状態。")

(defvar nskk-kouho-mode-map
  (make-sparse-keymap)
  "候補選択モード（▼モード）用のキーマップ。
変換候補から選択する状態。")

(defvar nskk-abbrev-mode-map
  (make-sparse-keymap)
  "Abbrevモード用のキーマップ。
英語の略語展開を行う。")

;;; プレフィックスキーマップ

(defvar nskk-prefix-map
  (make-sparse-keymap)
  "NSKKプレフィックスキー用のキーマップ。
`nskk-keymap-prefix'に続けて押すコマンド群。")

;;; キーマップ設定データ構造

(cl-defstruct (nskk-keymap-config
               (:constructor nskk-keymap-config-create)
               (:copier nil))
  "キーマップ設定を表す構造体。

スロット:
  global-map     - グローバルキーマップ
  mode-maps      - モード別キーマップのalist
  prefix-key     - プレフィックスキー
  skk-compatible - ddskk互換フラグ"
  (global-map nil)
  (mode-maps nil :type list)
  (prefix-key "C-x j" :type string)
  (skk-compatible t :type boolean))

;;; キーバインド定義テーブル

(defconst nskk-keymap-mode-switch-keys
  '(("C-j"   . nskk-mode-switch-to-hiragana)  ; ひらがなモード切り替え
    ("q"     . nskk-mode-toggle-kana)         ; カナモード切り替え
    ("Q"     . nskk-mode-switch-to-katakana)  ; カタカナモード
    ("l"     . nskk-mode-toggle-latin)        ; 英数モード切り替え
    ("L"     . nskk-mode-switch-to-latin)     ; 半角英数モード
    ("\\"    . nskk-mode-toggle-zenkaku-latin); 全角英数切り替え
    ("/"     . nskk-mode-switch-to-abbrev))   ; abbrevモード
  "モード切り替え用のキーバインド定義。
各要素は (キー . コマンド) の形式。")

(defconst nskk-keymap-henkan-keys
  '((" "     . nskk-henkan-start-or-next)     ; 変換開始・次候補
    ("SPC"   . nskk-henkan-start-or-next)     ; 変換開始・次候補（明示的）
    ("x"     . nskk-henkan-previous)          ; 前候補
    ("C-g"   . nskk-henkan-cancel)            ; 変換中止
    ("RET"   . nskk-henkan-commit)            ; 確定
    ("C-m"   . nskk-henkan-commit)            ; 確定
    ("C-n"   . nskk-henkan-next-candidate)    ; 次候補（リスト表示時）
    ("C-p"   . nskk-henkan-prev-candidate)    ; 前候補（リスト表示時）
    ("TAB"   . nskk-henkan-complete)          ; 補完
    ("C-i"   . nskk-henkan-complete))         ; 補完
  "変換操作用のキーバインド定義。")

;; ddskk互換: 直接キーバインドは行わない
;; 将来的にエミュレーション機構を実装する場合のプレースホルダー
(defconst nskk-keymap-input-keys nil
  "入力操作用のキーバインド定義。
ddskk完全互換のため、現在は空リスト。
標準Emacsコマンド(C-d, C-w, C-h等)を上書きしない。

将来的にddskkのエミュレーション機構と同等の実装を行う場合、
`nskk-setup-emulation-commands'関数を通じて設定する。")

;; 後方互換性のため、古い定義をobsolete化
(make-obsolete-variable 'nskk-keymap-input-keys nil "1.0.0")

(defconst nskk-keymap-special-keys
  '(("C-q"   . nskk-input-toggle-kuten)         ; 句点入力モード切り替え
    ("."     . nskk-input-insert-period)        ; 句点入力
    (","     . nskk-input-insert-comma)         ; 読点入力
    ("?"     . nskk-input-insert-question)      ; 疑問符入力
    ("!"     . nskk-input-insert-exclamation))  ; 感嘆符入力
  "特殊文字入力用のキーバインド定義。")

(defconst nskk-keymap-kouho-keys
  '((" "     . nskk-kouho-next)                 ; 次候補
    ("SPC"   . nskk-kouho-next)                 ; 次候補（明示的）
    ("x"     . nskk-kouho-previous)             ; 前候補
    ("C-n"   . nskk-kouho-next-group)           ; 次候補グループ
    ("C-p"   . nskk-kouho-prev-group)           ; 前候補グループ
    ("C-g"   . nskk-kouho-cancel)               ; キャンセル
    ("RET"   . nskk-kouho-commit)               ; 確定
    ("C-m"   . nskk-kouho-commit)               ; 確定
    ("0"     . nskk-kouho-select-0)             ; 候補0を選択
    ("1"     . nskk-kouho-select-1)             ; 候補1を選択
    ("2"     . nskk-kouho-select-2)             ; 候補2を選択
    ("3"     . nskk-kouho-select-3)             ; 候補3を選択
    ("4"     . nskk-kouho-select-4)             ; 候補4を選択
    ("5"     . nskk-kouho-select-5)             ; 候補5を選択
    ("6"     . nskk-kouho-select-6)             ; 候補6を選択
    ("7"     . nskk-kouho-select-7)             ; 候補7を選択
    ("8"     . nskk-kouho-select-8)             ; 候補8を選択
    ("9"     . nskk-kouho-select-9))            ; 候補9を選択
  "候補選択モード用のキーバインド定義。")

(defconst nskk-keymap-prefix-keys
  '(("s"     . nskk-save-jisyo)                 ; 辞書保存
    ("r"     . nskk-reload-jisyo)               ; 辞書再読み込み
    ("v"     . nskk-show-version)               ; バージョン表示
    ("h"     . nskk-help)                       ; ヘルプ表示
    ("d"     . nskk-toggle-debug-mode)          ; デバッグモード切り替え
    ("t"     . nskk-tutorial))                  ; チュートリアル起動
  "プレフィックスキー配下のコマンド定義。")

;;; Sticky Shift関連

(defconst nskk-keymap-sticky-shift-triggers
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
  "Sticky Shiftが有効な文字のリスト。")

;;; キーバインド設定関数

(defun nskk-define-key (keymap key command &optional force)
  "KEYMAPにKEYとCOMANDのバインディングを定義する。

引数:
  KEYMAP  - キーマップ
  KEY     - キー定義（文字列またはベクター）
  COMMAND - コマンド（シンボルまたは関数）
  FORCE   - 非nilの場合、既存のバインディングを上書き

戻り値: t（成功）、nil（既存バインディングあり&forceがnil）"
  (let* ((key-sequence (if (stringp key) (kbd key) key))
         (existing (lookup-key keymap key-sequence)))
    (when (or force (not existing) (numberp existing))
      (define-key keymap key-sequence command)
      t)))

(defun nskk-undefine-key (keymap key)
  "KEYMAPからKEYのバインディングを削除する。

引数:
  KEYMAP - キーマップ
  KEY    - キー定義（文字列またはベクター）"
  (let ((key-sequence (if (stringp key) (kbd key) key)))
    (define-key keymap key-sequence nil)))

(defun nskk-define-keys (keymap bindings &optional force)
  "KEYMAPに複数のキーバインディングを一括定義する。

引数:
  KEYMAP   - キーマップ
  BINDINGS - ((key . command) ...) 形式のリスト
  FORCE    - 非nilの場合、既存のバインディングを上書き

戻り値: 成功したバインディング数"
  (let ((count 0))
    (dolist (binding bindings)
      (when (nskk-define-key keymap (car binding) (cdr binding) force)
        (setq count (1+ count))))
    count))

;;; キーマップセットアップ

(defun nskk-setup-mode-switch-keys (keymap)
  "KEYMAPにモード切り替え用のキーバインドを設定する。"
  (nskk-define-keys keymap nskk-keymap-mode-switch-keys t))

(defun nskk-setup-henkan-keys (keymap)
  "KEYMAPに変換操作用のキーバインドを設定する。"
  (nskk-define-keys keymap nskk-keymap-henkan-keys t))

(defun nskk-setup-input-keys (keymap)
  "KEYMAPに入力操作用のキーバインドを設定する。
ddskk完全互換のため、現在は何も設定しない。"
  ;; ddskk互換: 直接キーバインドを設定しない
  ;; 将来的にエミュレーション機構を実装する場合はここに追加
  (ignore keymap))

(defun nskk-setup-special-keys (keymap)
  "KEYMAPに特殊文字入力用のキーバインドを設定する。"
  (nskk-define-keys keymap nskk-keymap-special-keys t))

(defun nskk-setup-kouho-keys (keymap)
  "KEYMAPに候補選択用のキーバインドを設定する。"
  (nskk-define-keys keymap nskk-keymap-kouho-keys t))

(defun nskk-setup-prefix-keys (keymap)
  "KEYMAPにプレフィックスキー配下のキーバインドを設定する。"
  (nskk-define-keys keymap nskk-keymap-prefix-keys t))

(defun nskk-setup-sticky-shift (keymap)
  "KEYMAPにSticky Shift用のキーバインドを設定する。"
  (when nskk-keymap-enable-sticky-shift
    ;; Sticky Shiftトリガーキー
    (nskk-define-key keymap
                     nskk-keymap-sticky-key
                     'nskk-input-sticky-shift
                     t)
    ;; 大文字入力時の送り仮名処理
    (dolist (char nskk-keymap-sticky-shift-triggers)
      (let ((upper (upcase char)))
        (nskk-define-key keymap
                        upper
                        'nskk-input-start-okuri
                        t)))))

;;; メインセットアップ関数

(defun nskk-setup-keybindings ()
  "全てのNSKKキーマップを初期化・設定する。
この関数はnskk-modeの初期化時に呼び出される。"
  (interactive)

  ;; メインキーマップ
  (nskk-setup-mode-switch-keys nskk-mode-map)
  (nskk-setup-input-keys nskk-mode-map)

  ;; プレフィックスキーマップ
  (nskk-setup-prefix-keys nskk-prefix-map)
  (nskk-define-key nskk-mode-map nskk-keymap-prefix nskk-prefix-map t)

  ;; ひらがなモード
  (nskk-setup-henkan-keys nskk-hiragana-mode-map)
  (nskk-setup-special-keys nskk-hiragana-mode-map)
  (nskk-setup-sticky-shift nskk-hiragana-mode-map)

  ;; カタカナモード（ひらがなと同じ）
  (nskk-setup-henkan-keys nskk-katakana-mode-map)
  (nskk-setup-special-keys nskk-katakana-mode-map)
  (nskk-setup-sticky-shift nskk-katakana-mode-map)

  ;; 変換モード
  (nskk-setup-henkan-keys nskk-henkan-mode-map)
  (nskk-setup-sticky-shift nskk-henkan-mode-map)

  ;; 候補選択モード
  (nskk-setup-kouho-keys nskk-kouho-mode-map)

  ;; abbrevモード
  (nskk-setup-henkan-keys nskk-abbrev-mode-map)
  (nskk-setup-special-keys nskk-abbrev-mode-map)

  (message "NSKK keybindings initialized"))

(defalias 'nskk-keymap-setup #'nskk-setup-keybindings)

(defun nskk-reset-keybindings ()
  "全てのNSKKキーマップをリセットする。
既存のキーバインドを全て削除し、初期状態に戻す。"
  (interactive)

  ;; 各キーマップをクリア
  (setq nskk-mode-map (make-sparse-keymap))
  (setq nskk-hiragana-mode-map (make-sparse-keymap))
  (setq nskk-katakana-mode-map (make-sparse-keymap))
  (setq nskk-ascii-mode-map (make-sparse-keymap))
  (setq nskk-zenkaku-latin-mode-map (make-sparse-keymap))
  (setq nskk-henkan-mode-map (make-sparse-keymap))
  (setq nskk-kouho-mode-map (make-sparse-keymap))
  (setq nskk-abbrev-mode-map (make-sparse-keymap))
  (setq nskk-prefix-map (make-sparse-keymap))

  ;; 再セットアップ
  (nskk-setup-keybindings)

  (message "NSKK keybindings reset"))

;;; モード別キーマップ取得

(defun nskk-get-current-keymap ()
  "現在のNSKK入力モードに対応するキーマップを返す。
モードが初期化されていない場合はnskk-mode-mapを返す。"
  (if (bound-and-true-p nskk-current-state)
      (let ((mode (nskk-state-mode nskk-current-state)))
        (cond
         ((eq mode 'hiragana) nskk-hiragana-mode-map)
         ((eq mode 'katakana) nskk-katakana-mode-map)
         ((eq mode 'latin) nskk-ascii-mode-map)
         ((eq mode 'zenkaku-latin) nskk-zenkaku-latin-mode-map)
         ((eq mode 'abbrev) nskk-abbrev-mode-map)
         ((eq mode 'conversion)
          (if (nskk-state-has-candidates-p nskk-current-state)
              nskk-kouho-mode-map
            nskk-henkan-mode-map))
         (t nskk-mode-map)))
    nskk-mode-map))

(defun nskk-get-keymap-for-mode (mode)
  "指定されたMODEに対応するキーマップを返す。

引数:
  MODE - モードシンボル（hiragana, katakana等）

戻り値:
  対応するキーマップ、または不明な場合はnil"
  (cond
   ((eq mode 'hiragana) nskk-hiragana-mode-map)
   ((eq mode 'katakana) nskk-katakana-mode-map)
   ((eq mode 'latin) nskk-ascii-mode-map)
   ((eq mode 'zenkaku-latin) nskk-zenkaku-latin-mode-map)
   ((eq mode 'abbrev) nskk-abbrev-mode-map)
   ((eq mode 'henkan) nskk-henkan-mode-map)
   ((eq mode 'kouho) nskk-kouho-mode-map)
   (t nil)))

;;; キーバインドカスタマイズ

(defun nskk-customize-key (mode key command)
  "指定されたモードのキーバインドをカスタマイズする。

引数:
  MODE    - モードシンボル
  KEY     - キー定義（文字列）
  COMMAND - バインドするコマンド

例:
  (nskk-customize-key 'hiragana \"C-j\" 'my-custom-command)"
  (interactive
   (list
    (intern (completing-read "Mode: "
                            '(hiragana katakana latin zenkaku-latin
                              abbrev henkan kouho)))
    (read-string "Key: ")
    (read-command "Command: ")))
  (when-let ((keymap (nskk-get-keymap-for-mode mode)))
    (nskk-define-key keymap key command t)
    (message "Customized key binding: %s in %s mode -> %s"
             key mode command)))

;;; キーマップ情報取得

(defun nskk-describe-keymap (&optional mode)
  "指定されたMODEのキーマップ情報を表示する。
MODEが省略された場合は現在のモードのキーマップを表示。"
  (interactive)
  (let* ((target-mode (or mode
                         (when (bound-and-true-p nskk-current-state)
                           (nskk-state-mode nskk-current-state))
                         'hiragana))
         (keymap (nskk-get-keymap-for-mode target-mode)))
    (if keymap
        (with-help-window "*NSKK Keymap*"
          (princ (format "NSKK Keymap for %s mode\n\n" target-mode))
          (princ (substitute-command-keys "\\{keymap}")))
      (message "No keymap found for mode: %s" target-mode))))

(defun nskk-list-all-keybindings ()
  "全てのNSKKキーバインドをリスト表示する。"
  (interactive)
  (with-help-window "*NSKK Keybindings*"
    (princ "NSKK Key Bindings\n")
    (princ "=================\n\n")

    (princ "Mode Switch Keys:\n")
    (dolist (binding nskk-keymap-mode-switch-keys)
      (princ (format "  %s\t-> %s\n" (car binding) (cdr binding))))

    (princ "\nConversion Keys:\n")
    (dolist (binding nskk-keymap-henkan-keys)
      (princ (format "  %s\t-> %s\n" (car binding) (cdr binding))))

    (princ "\nCandidate Selection Keys:\n")
    (dolist (binding nskk-keymap-kouho-keys)
      (princ (format "  %s\t-> %s\n" (car binding) (cdr binding))))

    (princ "\nPrefix Keys:\n")
    (princ (format "  Prefix: %s\n" nskk-keymap-prefix))
    (dolist (binding nskk-keymap-prefix-keys)
      (princ (format "  %s %s\t-> %s\n"
                    nskk-keymap-prefix
                    (car binding)
                    (cdr binding))))))

;;; デバッグ・統計

(defun nskk-keymap-stats ()
  "キーマップシステムの統計情報を返す。

戻り値: 統計情報のplist
  :mode-keymaps        - モード別キーマップ数
  :prefix-key          - プレフィックスキー
  :skk-compatible      - ddskk互換フラグ
  :sticky-shift-enabled - Sticky Shift有効フラグ"
  (list :mode-keymaps 7
        :prefix-key nskk-keymap-prefix
        :skk-compatible nskk-keymap-skk-compatible
        :sticky-shift-enabled nskk-keymap-enable-sticky-shift))

;;; 互換性関数（ddskk用）

(defun nskk-keymap-skk-j-mode-map ()
  "ddskk互換のskk-j-mode-mapを返す。
ひらがなモードマップを返す。"
  nskk-hiragana-mode-map)

(defun nskk-keymap-skk-jisx0208-latin-mode-map ()
  "ddskk互換のskk-jisx0208-latin-mode-mapを返す。
全角英数モードマップを返す。"
  nskk-zenkaku-latin-mode-map)

(defun nskk-keymap-skk-abbrev-mode-map ()
  "ddskk互換のskk-abbrev-mode-mapを返す。
abbrevモードマップを返す。"
  nskk-abbrev-mode-map)

;;; 初期化

;; ライブラリロード時に基本的なキーマップをセットアップ
(nskk-setup-keybindings)

(provide 'nskk-keymap)

;;; nskk-keymap.el ends here
