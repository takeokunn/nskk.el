;;; nskk-modeline.el --- Modeline display for NSKK -*- lexical-binding: t; -*-

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

;; このファイルはNSKKのモードライン表示機能を実装します。
;;
;; 特徴:
;; - モード別の色分け表示
;; - 変換状態インジケーター（▽▼）
;; - カスタマイズ可能なフォーマット
;; - 辞書状態表示
;; - エラー表示
;;
;; モードライン表示例:
;; - ひらがなモード: [あ] (緑)
;; - カタカナモード: [カ] (青)
;; - 英数モード:     [A]  (赤)
;; - 変換中:         [あ▽] (緑+黄)
;; - 候補選択中:     [あ▼] (緑+赤)
;;
;; 使用例:
;; (nskk-modeline-update)
;; (nskk-modeline-format 'hiragana 'converting)
;; (nskk-modeline-propertize "text" 'nskk-modeline-hiragana-face)

;;; Code:

(require 'cl-lib)
(require 'nskk-state)

;;; カスタマイズグループ

(defgroup nskk-modeline nil
  "NSKK modeline display settings."
  :group 'nskk
  :prefix "nskk-modeline-")

;;; カスタマイズ変数

(defcustom nskk-modeline-format "[%m%s]"
  "モードライン表示フォーマット。
以下のプレースホルダーが使用可能:
  %m - モード表示（例: あ, カ, A）
  %s - 状態インジケーター（例: ▽, ▼）
  %d - 辞書状態（例: 読, !）
  %% - リテラルの%"
  :type 'string
  :group 'nskk-modeline)

(defcustom nskk-modeline-use-color t
  "非nilの場合、モード別に色分けを行う。
nilの場合は色を使用せず、プレーンテキストで表示する。"
  :type 'boolean
  :group 'nskk-modeline)

(defcustom nskk-modeline-use-icons nil
  "非nilの場合、テキストインジケーターの代わりにアイコンを使用する。
現在のバージョンではサポートされていません（将来の拡張用）。"
  :type 'boolean
  :group 'nskk-modeline)

(defcustom nskk-modeline-show-state-indicator t
  "非nilの場合、変換状態インジケーター（▽▼）を表示する。"
  :type 'boolean
  :group 'nskk-modeline)

(defcustom nskk-modeline-show-dict-status nil
  "非nilの場合、辞書の状態を表示する。
辞書読み込み中や辞書エラー時に情報を表示する。"
  :type 'boolean
  :group 'nskk-modeline)

(defcustom nskk-modeline-position 'mode-line-position
  "モードライン内での表示位置。
mode-line-format 内のシンボルを指定する。"
  :type 'symbol
  :group 'nskk-modeline)

;;; フェイス定義

(defface nskk-modeline-hiragana-face
  '((t :foreground "green" :weight bold))
  "ひらがなモード用のフェイス。"
  :group 'nskk-modeline)

(defface nskk-modeline-katakana-face
  '((t :foreground "blue" :weight bold))
  "カタカナモード用のフェイス。"
  :group 'nskk-modeline)

(defface nskk-modeline-latin-face
  '((t :foreground "red" :weight bold))
  "半角英数モード用のフェイス。"
  :group 'nskk-modeline)

(defface nskk-modeline-zenkaku-latin-face
  '((t :foreground "orange" :weight bold))
  "全角英数モード用のフェイス。"
  :group 'nskk-modeline)

(defface nskk-modeline-abbrev-face
  '((t :foreground "purple" :weight bold))
  "Abbrevモード用のフェイス。"
  :group 'nskk-modeline)

(defface nskk-modeline-conversion-face
  '((t :foreground "cyan" :weight bold))
  "変換モード用のフェイス。"
  :group 'nskk-modeline)

(defface nskk-modeline-converting-indicator-face
  '((t :foreground "yellow" :weight bold))
  "変換中インジケーター（▽）用のフェイス。"
  :group 'nskk-modeline)

(defface nskk-modeline-selecting-indicator-face
  '((t :foreground "red" :weight bold))
  "候補選択中インジケーター（▼）用のフェイス。"
  :group 'nskk-modeline)

(defface nskk-modeline-dict-loading-face
  '((t :foreground "gray" :slant italic))
  "辞書読み込み中の状態表示用フェイス。"
  :group 'nskk-modeline)

(defface nskk-modeline-error-face
  '((t :foreground "red" :weight bold :background "yellow"))
  "エラー表示用のフェイス。"
  :group 'nskk-modeline)

;;; 状態定義

(cl-defstruct (nskk-modeline-state
               (:constructor nskk-modeline-state--create)
               (:copier nskk-modeline-state-copy))
  "モードライン表示状態を表す構造体。

スロット:
  mode      - 現在の入力モード（symbol）
  state     - 入力状態（'normal, 'converting, 'selecting）
  indicator - 追加のインジケーター文字列
  dict-status - 辞書の状態（nil, 'loading, 'error）
  error-message - エラーメッセージ"
  (mode 'hiragana
        :type symbol
        :documentation "現在の入力モード")
  (state 'normal
         :type symbol
         :documentation "入力状態（normal/converting/selecting）")
  (indicator ""
             :type string
             :documentation "追加のインジケーター文字列")
  (dict-status nil
               :type (or null symbol)
               :documentation "辞書の状態")
  (error-message nil
                 :type (or null string)
                 :documentation "エラーメッセージ"))

(defun nskk-modeline-state-create (&rest args)
  "新しいモードライン状態を生成する。

引数:
  ARGS - cl-defstructのキーワード引数

例:
  (nskk-modeline-state-create)
  (nskk-modeline-state-create :mode 'katakana :state 'converting)"
  (apply #'nskk-modeline-state--create args))

;;; モード別フェイス取得

(defconst nskk-modeline-mode-faces
  '((hiragana . nskk-modeline-hiragana-face)
    (katakana . nskk-modeline-katakana-face)
    (latin . nskk-modeline-latin-face)
    (zenkaku-latin . nskk-modeline-zenkaku-latin-face)
    (abbrev . nskk-modeline-abbrev-face)
    (conversion . nskk-modeline-conversion-face))
  "各モードに対応するフェイスのalist。")

(defun nskk-modeline-get-mode-face (mode)
  "MODE に対応するフェイスを取得する。
対応するフェイスがない場合は default フェイスを返す。"
  (or (alist-get mode nskk-modeline-mode-faces)
      'default))

;;; 状態インジケーター

(defconst nskk-modeline-state-indicators
  '((normal . "")
    (converting . "▽")
    (selecting . "▼"))
  "各状態に対応するインジケーター文字列。")

(defun nskk-modeline-get-state-indicator (state)
  "STATE に対応するインジケーター文字列を取得する。"
  (if nskk-modeline-show-state-indicator
      (or (alist-get state nskk-modeline-state-indicators) "")
    ""))

(defun nskk-modeline-get-state-indicator-face (state)
  "STATE に対応するインジケーター用フェイスを取得する。"
  (pcase state
    ('converting 'nskk-modeline-converting-indicator-face)
    ('selecting 'nskk-modeline-selecting-indicator-face)
    (_ 'default)))

;;; 辞書状態インジケーター

(defconst nskk-modeline-dict-status-indicators
  '((loading . "読")
    (error . "!"))
  "辞書状態に対応するインジケーター文字列。")

(defun nskk-modeline-get-dict-status-indicator (dict-status)
  "DICT-STATUS に対応するインジケーター文字列を取得する。"
  (if (and nskk-modeline-show-dict-status dict-status)
      (or (alist-get dict-status nskk-modeline-dict-status-indicators) "")
    ""))

(defun nskk-modeline-get-dict-status-face (dict-status)
  "DICT-STATUS に対応するフェイスを取得する。"
  (pcase dict-status
    ('loading 'nskk-modeline-dict-loading-face)
    ('error 'nskk-modeline-error-face)
    (_ 'default)))

;;; テキスト装飾

(defun nskk-modeline-propertize (text face)
  "TEXT に FACE を適用してプロパティ付き文字列を返す。
`nskk-modeline-use-color' が nil の場合は TEXT をそのまま返す。

引数:
  TEXT - 装飾する文字列
  FACE - 適用するフェイス（シンボルまたはフェイス属性のリスト）

返り値:
  プロパティ付き文字列"
  (if nskk-modeline-use-color
      (propertize text 'face face)
    text))

;;; フォーマット展開

(defun nskk-modeline-expand-format (format-string mode state dict-status)
  "FORMAT-STRING 内のプレースホルダーを展開する。

引数:
  FORMAT-STRING - フォーマット文字列
  MODE          - 入力モード
  STATE         - 入力状態
  DICT-STATUS   - 辞書状態

返り値:
  展開された文字列"
  (let ((mode-indicator (nskk-state-mode-indicator mode))
        (state-indicator (nskk-modeline-get-state-indicator state))
        (dict-indicator (nskk-modeline-get-dict-status-indicator dict-status))
        (result format-string))
    ;; まず %% を一時的な文字列に置換
    (setq result (replace-regexp-in-string "%%" "\x00PERCENT\x00" result t t))
    ;; プレースホルダーを順番に置換
    (setq result (replace-regexp-in-string "%m" mode-indicator result t t))
    (setq result (replace-regexp-in-string "%s" state-indicator result t t))
    (setq result (replace-regexp-in-string "%d" dict-indicator result t t))
    ;; 一時文字列を % に戻す
    (setq result (replace-regexp-in-string "\x00PERCENT\x00" "%" result t t))
    result))

;;; モードライン文字列生成

(defun nskk-modeline-format (mode state &optional dict-status)
  "モードライン表示用の文字列を生成する。

引数:
  MODE        - 入力モード（'hiragana, 'katakana, 'latin など）
  STATE       - 入力状態（'normal, 'converting, 'selecting）
  DICT-STATUS - 辞書状態（nil, 'loading, 'error）

返り値:
  装飾されたモードライン文字列"
  (let* ((mode-indicator (nskk-state-mode-indicator mode))
         (state-indicator (nskk-modeline-get-state-indicator state))
         (dict-indicator (nskk-modeline-get-dict-status-indicator dict-status))

         ;; 各要素を装飾
         (mode-text (nskk-modeline-propertize
                     mode-indicator
                     (nskk-modeline-get-mode-face mode)))
         (state-text (if (string-empty-p state-indicator)
                         ""
                       (nskk-modeline-propertize
                        state-indicator
                        (nskk-modeline-get-state-indicator-face state))))
         (dict-text (if (string-empty-p dict-indicator)
                        ""
                      (nskk-modeline-propertize
                       dict-indicator
                       (nskk-modeline-get-dict-status-face dict-status))))

         ;; フォーマット文字列を処理
         (format-str nskk-modeline-format)
         (result format-str))

    ;; プレースホルダーを装飾されたテキストで置換
    ;; まず %% を一時的な文字列に置換
    (setq result (replace-regexp-in-string "%%" "\x00PERCENT\x00" result t t))
    ;; プレースホルダーを順番に置換（装飾されたテキストを使用）
    (setq result (replace-regexp-in-string "%m" mode-text result t t))
    (setq result (replace-regexp-in-string "%s" state-text result t t))
    (setq result (replace-regexp-in-string "%d" dict-text result t t))
    ;; 一時文字列を % に戻す
    (setq result (replace-regexp-in-string "\x00PERCENT\x00" "%" result t t))
    result))

(defun nskk-modeline-format-from-state (ml-state)
  "nskk-modeline-state 構造体からモードライン文字列を生成する。

引数:
  ML-STATE - nskk-modeline-state 構造体

返り値:
  装飾されたモードライン文字列"
  (nskk-modeline-format
   (nskk-modeline-state-mode ml-state)
   (nskk-modeline-state-state ml-state)
   (nskk-modeline-state-dict-status ml-state)))

;;; NSKK状態からの変換

(defun nskk-modeline-state-from-nskk-state (nskk-state)
  "NSKK-STATE から nskk-modeline-state を生成する。

引数:
  NSKK-STATE - nskk-state 構造体

返り値:
  nskk-modeline-state 構造体"
  (let* ((mode (nskk-state-mode nskk-state))
         (has-candidates (nskk-state-has-candidates-p nskk-state))
         (has-conversion-buffer (not (string-empty-p
                                      (nskk-state-conversion-buffer nskk-state))))
         ;; 状態を判定
         (state (cond
                 ((and has-candidates (eq mode 'conversion))
                  'selecting)
                 (has-conversion-buffer
                  'converting)
                 (t
                  'normal))))
    (nskk-modeline-state-create
     :mode mode
     :state state)))

;;; モードライン更新

(defvar-local nskk-modeline-string ""
  "現在のバッファのモードライン表示文字列。")

(defun nskk-modeline-update (&optional mode state dict-status)
  "モードライン表示を更新する。

引数:
  MODE        - 入力モード（省略時は現在の状態から取得）
  STATE       - 入力状態（省略時は現在の状態から判定）
  DICT-STATUS - 辞書状態（省略時は nil）

副作用:
  `nskk-modeline-string' を更新し、モードラインを再描画する。"
  (interactive)
  (let* ((nskk-state (or nskk-current-state
                         (nskk-state-create)))
         (actual-mode (or mode (nskk-state-mode nskk-state)))
         (ml-state (if (and (null mode) (null state))
                       ;; 引数が省略された場合は状態から自動判定
                       (nskk-modeline-state-from-nskk-state nskk-state)
                     ;; 引数が指定された場合はそれを使用
                     (nskk-modeline-state-create
                      :mode actual-mode
                      :state (or state 'normal)
                      :dict-status dict-status))))

    (setq nskk-modeline-string
          (nskk-modeline-format-from-state ml-state))

    ;; モードラインを強制再描画
    (force-mode-line-update)))

;;; モードライン統合

(defun nskk-modeline-install ()
  "NSKKモードライン表示を mode-line-format に組み込む。"
  (unless (memq 'nskk-modeline-string mode-line-format)
    ;; mode-line-position の前に挿入
    (let ((pos (cl-position nskk-modeline-position mode-line-format)))
      (if pos
          (setq mode-line-format
                (append (cl-subseq mode-line-format 0 pos)
                        '(nskk-modeline-string)
                        (cl-subseq mode-line-format pos)))
        ;; 見つからない場合は末尾に追加
        (setq mode-line-format
              (append mode-line-format '(nskk-modeline-string)))))))

(defun nskk-modeline-uninstall ()
  "NSKKモードライン表示を mode-line-format から削除する。"
  (setq mode-line-format
        (delq 'nskk-modeline-string mode-line-format))
  (setq nskk-modeline-string ""))

;;; フック関数

(defun nskk-modeline-on-mode-switch (from-mode to-mode)
  "モード切り替え時のモードライン更新フック関数。

引数:
  FROM-MODE - 切り替え前のモード
  TO-MODE   - 切り替え後のモード"
  (nskk-modeline-update to-mode 'normal))

;;; デバッグ・ユーティリティ

(defun nskk-modeline-describe-current ()
  "現在のモードライン状態を表示する。"
  (interactive)
  (message "NSKK Modeline: %s"
           (substring-no-properties nskk-modeline-string)))

(defun nskk-modeline-preview-all-modes ()
  "全てのモードのモードライン表示をプレビューする。"
  (interactive)
  (let ((modes '(hiragana katakana latin zenkaku-latin abbrev conversion))
        (states '(normal converting selecting))
        (preview-buffer (get-buffer-create "*NSKK Modeline Preview*")))
    (with-current-buffer preview-buffer
      (erase-buffer)
      (insert "NSKK Modeline Preview\n")
      (insert "=====================\n\n")

      ;; 各モードと状態の組み合わせを表示
      (dolist (mode modes)
        (insert (format "Mode: %s\n" mode))
        (dolist (state states)
          (let ((ml-string (nskk-modeline-format mode state nil)))
            (insert (format "  %s: %s\n" state ml-string))))
        (insert "\n"))

      ;; 辞書状態付き
      (insert "Dictionary Status:\n")
      (insert (format "  Loading: %s\n"
                      (nskk-modeline-format 'hiragana 'normal 'loading)))
      (insert (format "  Error:   %s\n"
                      (nskk-modeline-format 'hiragana 'normal 'error)))
      (insert "\n")

      (goto-char (point-min))
      (special-mode))
    (display-buffer preview-buffer)))

;;; 統計情報

(defun nskk-modeline-stats ()
  "モードライン表示システムの統計情報を返す。

返り値:
  以下の要素を含むplist:
    :format          - 現在のフォーマット文字列
    :use-color       - 色使用設定
    :show-indicator  - 状態インジケーター表示設定
    :current-string  - 現在の表示文字列"
  (list :format nskk-modeline-format
        :use-color nskk-modeline-use-color
        :show-indicator nskk-modeline-show-state-indicator
        :current-string (substring-no-properties nskk-modeline-string)))

(provide 'nskk-modeline)

;;; nskk-modeline.el ends here
