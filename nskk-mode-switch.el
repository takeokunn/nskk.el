;;; nskk-mode-switch.el --- Mode switching for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.0"))

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

;; このファイルはNSKKのモード切り替え機能を実装します。
;;
;; 特徴:
;; - インタラクティブなモード切り替えコマンド
;; - モード切り替え前後のフック実行
;; - モードライン表示の自動更新
;; - キーバインドによる直接切り替え
;;
;; 使用例:
;; (nskk-mode-switch 'katakana)
;; (nskk-mode-switch-to-hiragana)
;; (nskk-mode-toggle-kana)

;;; Code:

(require 'cl-lib)
(require 'nskk-state)

;;; カスタマイズ変数

(defgroup nskk-mode-switch nil
  "NSKK mode switching settings."
  :group 'nskk
  :prefix "nskk-mode-switch-")

(defcustom nskk-mode-switch-show-message t
  "非nilの場合、モード切り替え時にメッセージを表示する。"
  :type 'boolean
  :group 'nskk-mode-switch)

(defcustom nskk-mode-switch-update-modeline t
  "非nilの場合、モード切り替え時にモードラインを自動更新する。"
  :type 'boolean
  :group 'nskk-mode-switch)

(defcustom nskk-mode-switch-clear-input-on-mode-change t
  "非nilの場合、モード切り替え時に未確定入力をクリアする。"
  :type 'boolean
  :group 'nskk-mode-switch)

;;; フック定義

(defvar nskk-mode-switch-before-hook nil
  "モード切り替え前に実行されるフック。
フック関数は2つの引数を受け取る:
  FROM-MODE - 切り替え前のモード
  TO-MODE   - 切り替え後のモード

フック関数内で `nskk-current-state' を参照できる。")

(defvar nskk-mode-switch-after-hook nil
  "モード切り替え後に実行されるフック。
フック関数は2つの引数を受け取る:
  FROM-MODE - 切り替え前のモード
  TO-MODE   - 切り替え後のモード

フック関数内で `nskk-current-state' を参照できる。")

;;; モード別フック

(defvar nskk-mode-switch-to-hiragana-hook nil
  "ひらがなモードへの切り替え後に実行されるフック。")

(defvar nskk-mode-switch-to-katakana-hook nil
  "カタカナモードへの切り替え後に実行されるフック。")

(defvar nskk-mode-switch-to-latin-hook nil
  "半角英数モードへの切り替え後に実行されるフック。")

(defvar nskk-mode-switch-to-zenkaku-latin-hook nil
  "全角英数モードへの切り替え後に実行されるフック。")

(defvar nskk-mode-switch-to-abbrev-hook nil
  "abbrevモードへの切り替え後に実行されるフック。")

;;; 内部変数

(defvar nskk-mode-switch--in-progress nil
  "モード切り替え中かどうかを示すフラグ。
再帰的な切り替えを防止するために使用する。")

;;; 補助関数

(defun nskk-mode-switch--get-mode-hook (mode)
  "MODE に対応するモード別フックシンボルを返す。"
  (intern (format "nskk-mode-switch-to-%s-hook" mode)))

(defun nskk-mode-switch--update-modeline ()
  "モードラインを更新する。"
  (when nskk-mode-switch-update-modeline
    (force-mode-line-update)))

(defun nskk-mode-switch--show-message (mode)
  "MODE への切り替えメッセージを表示する。"
  (when nskk-mode-switch-show-message
    (message "NSKK: %s" (nskk-state-mode-description mode))))

(defun nskk-mode-switch--clear-input-if-needed (state)
  "設定に応じて STATE の入力バッファをクリアする。"
  (when nskk-mode-switch-clear-input-on-mode-change
    (nskk-state-clear-input state)))

;;; コア関数

(defun nskk-mode-switch (to-mode &optional force no-hooks)
  "現在のNSKK状態を TO-MODE に切り替える。

引数:
  TO-MODE  - 切り替え先のモード（`nskk-state-modes' のいずれか）
  FORCE    - 非nilの場合、遷移可能性チェックをスキップ
  NO-HOOKS - 非nilの場合、フックの実行をスキップ

返り値:
  切り替えに成功した場合は t、失敗した場合は nil

副作用:
  - `nskk-current-state' のモードを変更
  - 未確定入力のクリア（設定による）
  - フックの実行
  - モードライン更新
  - メッセージ表示"
  (unless nskk-current-state
    (nskk-state-init))

  ;; 再帰的な切り替えを防止
  (when nskk-mode-switch--in-progress
    (error "Mode switch already in progress"))

  (let ((from-mode (nskk-state-mode nskk-current-state))
        (nskk-mode-switch--in-progress t))

    (unwind-protect
        (progn
          ;; 同じモードへの切り替えはスキップ
          (if (eq from-mode to-mode)
              t
            ;; before フック実行
            (unless no-hooks
              (run-hook-with-args 'nskk-mode-switch-before-hook
                                  from-mode to-mode))

            ;; 状態遷移を実行
            (if (nskk-state-transition nskk-current-state to-mode force)
                (progn
                  ;; 入力バッファクリア
                  (nskk-mode-switch--clear-input-if-needed nskk-current-state)

                  ;; after フック実行
                  (unless no-hooks
                    (run-hook-with-args 'nskk-mode-switch-after-hook
                                        from-mode to-mode)
                    ;; モード別フック実行
                    (run-hooks (nskk-mode-switch--get-mode-hook to-mode)))

                  ;; UI更新
                  (nskk-mode-switch--update-modeline)
                  (nskk-mode-switch--show-message to-mode)
                  t)
              nil)))

      ;; cleanup
      (setq nskk-mode-switch--in-progress nil))))

;;; インタラクティブコマンド

(defun nskk-mode-switch-to-hiragana ()
  "ひらがな入力モードに切り替える。"
  (interactive)
  (nskk-mode-switch 'hiragana))

(defun nskk-mode-switch-to-katakana ()
  "カタカナ入力モードに切り替える。"
  (interactive)
  (nskk-mode-switch 'katakana))

(defun nskk-mode-switch-to-latin ()
  "半角英数モードに切り替える。"
  (interactive)
  (nskk-mode-switch 'latin))

(defun nskk-mode-switch-to-zenkaku-latin ()
  "全角英数モードに切り替える。"
  (interactive)
  (nskk-mode-switch 'zenkaku-latin))

(defun nskk-mode-switch-to-abbrev ()
  "abbrevモードに切り替える。"
  (interactive)
  (nskk-mode-switch 'abbrev))

;;; トグルコマンド

(defun nskk-mode-toggle-kana ()
  "ひらがなモードとカタカナモードを切り替える。"
  (interactive)
  (unless nskk-current-state
    (nskk-state-init))

  (let ((current-mode (nskk-state-mode nskk-current-state)))
    (cond
     ((eq current-mode 'hiragana)
      (nskk-mode-switch 'katakana))
     ((eq current-mode 'katakana)
      (nskk-mode-switch 'hiragana))
     (t
      (nskk-mode-switch 'hiragana)))))

(defun nskk-mode-toggle-latin ()
  "かなモードと英数モードを切り替える。
現在のモードに応じて最適なモードに切り替える。"
  (interactive)
  (unless nskk-current-state
    (nskk-state-init))

  (let ((current-mode (nskk-state-mode nskk-current-state)))
    (cond
     ((memq current-mode '(hiragana katakana abbrev))
      (nskk-mode-switch 'latin))
     ((eq current-mode 'latin)
      ;; 前回のかなモードに戻る、なければひらがな
      (let ((prev-mode (nskk-state-previous-mode nskk-current-state)))
        (if (memq prev-mode '(hiragana katakana))
            (nskk-mode-switch prev-mode)
          (nskk-mode-switch 'hiragana))))
     (t
      (nskk-mode-switch 'latin)))))

(defun nskk-mode-toggle-zenkaku-latin ()
  "全角英数モードと半角英数モードを切り替える。"
  (interactive)
  (unless nskk-current-state
    (nskk-state-init))

  (let ((current-mode (nskk-state-mode nskk-current-state)))
    (cond
     ((eq current-mode 'latin)
      (nskk-mode-switch 'zenkaku-latin))
     ((eq current-mode 'zenkaku-latin)
      (nskk-mode-switch 'latin))
     (t
      (nskk-mode-switch 'zenkaku-latin)))))

;;; 循環切り替え

(defun nskk-mode-cycle-forward ()
  "モードを順方向に循環切り替えする。
ひらがな → カタカナ → 全角英数 → 半角英数 → ひらがな"
  (interactive)
  (unless nskk-current-state
    (nskk-state-init))

  (let ((current-mode (nskk-state-mode nskk-current-state)))
    (cond
     ((eq current-mode 'hiragana) (nskk-mode-switch 'katakana))
     ((eq current-mode 'katakana) (nskk-mode-switch 'zenkaku-latin))
     ((eq current-mode 'zenkaku-latin) (nskk-mode-switch 'latin))
     ((eq current-mode 'latin) (nskk-mode-switch 'hiragana))
     (t (nskk-mode-switch 'hiragana)))))

(defun nskk-mode-cycle-backward ()
  "モードを逆方向に循環切り替えする。
ひらがな → 半角英数 → 全角英数 → カタカナ → ひらがな"
  (interactive)
  (unless nskk-current-state
    (nskk-state-init))

  (let ((current-mode (nskk-state-mode nskk-current-state)))
    (cond
     ((eq current-mode 'hiragana) (nskk-mode-switch 'latin))
     ((eq current-mode 'latin) (nskk-mode-switch 'zenkaku-latin))
     ((eq current-mode 'zenkaku-latin) (nskk-mode-switch 'katakana))
     ((eq current-mode 'katakana) (nskk-mode-switch 'hiragana))
     (t (nskk-mode-switch 'hiragana)))))

;;; 前回モード復帰

(defun nskk-mode-restore-previous ()
  "前回のモードに復帰する。"
  (interactive)
  (unless nskk-current-state
    (nskk-state-init))

  (let ((prev-mode (or (nskk-state-previous-mode nskk-current-state)
                       'hiragana)))
    (nskk-mode-switch prev-mode)))

;;; モード問い合わせ

(defun nskk-mode-current-mode ()
  "現在のNSKK入力モードを返す。
NSKKが初期化されていない場合は nil を返す。"
  (when nskk-current-state
    (nskk-state-mode nskk-current-state)))

(defun nskk-mode-current-mode-string ()
  "現在のNSKK入力モードの説明文字列を返す。"
  (if-let ((mode (nskk-mode-current-mode)))
      (nskk-state-mode-description mode)
    "未初期化"))

(defun nskk-mode-current-indicator ()
  "現在のNSKK入力モードのインジケーター文字列を返す。
モードライン表示に使用する。"
  (if-let ((mode (nskk-mode-current-mode)))
      (nskk-state-mode-indicator mode)
    ""))

;;; キーバインド定義

(defvar nskk-mode-switch-key-bindings
  '(("C-j"   . nskk-mode-switch-to-hiragana)
    ("q"     . nskk-mode-toggle-kana)
    ("Q"     . nskk-mode-switch-to-katakana)
    ("l"     . nskk-mode-toggle-latin)
    ("L"     . nskk-mode-switch-to-latin)
    ("\\"    . nskk-mode-toggle-zenkaku-latin)
    ("/"     . nskk-mode-switch-to-abbrev)
    ("C-M-j" . nskk-mode-restore-previous))
  "NSKKモード切り替え用のデフォルトキーバインド。
各要素は (キー . コマンド) の形式。")

(defun nskk-mode-switch-setup-keys (keymap)
  "KEYMAP にモード切り替え用のキーバインドを設定する。
`nskk-mode-switch-key-bindings' で定義されたキーバインドを適用する。"
  (dolist (binding nskk-mode-switch-key-bindings)
    (let ((key (car binding))
          (command (cdr binding)))
      (define-key keymap (kbd key) command))))

;;; デバッグ機能

(defun nskk-mode-switch-describe-current ()
  "現在のモード切り替え状態を表示する。"
  (interactive)
  (if nskk-current-state
      (let ((mode (nskk-state-mode nskk-current-state))
            (prev-mode (nskk-state-previous-mode nskk-current-state)))
        (message "Current mode: %s (%s), Previous: %s"
                 mode
                 (nskk-state-mode-description mode)
                 (if prev-mode
                     (format "%s (%s)" prev-mode
                             (nskk-state-mode-description prev-mode))
                   "none")))
    (message "NSKK not initialized")))

(defun nskk-mode-switch-list-modes ()
  "利用可能な全てのモードを表示する。"
  (interactive)
  (let ((modes-info
         (mapcar (lambda (mode)
                   (format "  %s - %s (%s)"
                           mode
                           (nskk-state-mode-description mode)
                           (nskk-state-mode-indicator mode)))
                 nskk-state-modes)))
    (message "Available modes:\n%s" (string-join modes-info "\n"))))

;;; 統計・情報取得

(defun nskk-mode-switch-stats ()
  "モード切り替えシステムの統計情報を返す。

返り値は以下の要素を含むplist:
  :current-mode        - 現在のモード
  :previous-mode       - 前回のモード
  :available-modes     - 利用可能なモード数
  :clear-on-switch     - モード切り替え時の入力クリア設定
  :show-message        - メッセージ表示設定"
  (list :current-mode (nskk-mode-current-mode)
        :previous-mode (when nskk-current-state
                         (nskk-state-previous-mode nskk-current-state))
        :available-modes (length nskk-state-modes)
        :clear-on-switch nskk-mode-switch-clear-input-on-mode-change
        :show-message nskk-mode-switch-show-message))

(provide 'nskk-mode-switch)

;;; nskk-mode-switch.el ends here
