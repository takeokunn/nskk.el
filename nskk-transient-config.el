;;; nskk-transient-config.el --- Transient configuration menu for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, transient
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

;; このファイルはEmacs 31のTransient UIを使用したNSKKの設定メニューを実装します。
;;
;; 特徴:
;; - setopt統合による安全な設定変更
;; - 階層的な設定メニュー
;; - リアルタイムプレビュー機能
;; - プリセット管理機能
;;
;; 主要機能:
;; - `nskk-config-menu'        - メイン設定メニュー
;; - `nskk-config-core'        - コア設定
;; - `nskk-config-input'       - 入力方式設定
;; - `nskk-config-learning'    - 学習設定
;; - `nskk-config-ui'          - UI設定
;; - `nskk-config-advanced'    - 高度な設定
;; - `nskk-config-preset-save' - プリセット保存
;; - `nskk-config-preset-load' - プリセット読み込み
;;
;; 使用例:
;; M-x nskk-config-menu
;;
;; プリセット管理:
;; (nskk-config-preset-save "my-preset")
;; (nskk-config-preset-load "my-preset")

;;; Code:

(require 'cl-lib)
(require 'transient)

;;; グループ定義

(defgroup nskk-config nil
  "NSKK設定メニューのカスタマイズ。"
  :group 'nskk
  :prefix "nskk-config-")

;;; 設定変数

(defcustom nskk-config-preset-directory
  (expand-file-name "presets" user-emacs-directory)
  "設定プリセットの保存ディレクトリ。"
  :type 'directory
  :group 'nskk-config)

(defcustom nskk-config-enable-preview t
  "設定変更時にプレビューを表示するか。"
  :type 'boolean
  :group 'nskk-config)

;;; プリセット管理

(defvar nskk-config--preset-alist nil
  "プリセット設定の連想リスト。")

(defvar nskk-config--current-values nil
  "現在の設定値を保持するハッシュテーブル。")

(defun nskk-config--ensure-preset-directory ()
  "プリセットディレクトリを作成する。"
  (unless (file-exists-p nskk-config-preset-directory)
    (make-directory nskk-config-preset-directory t)))

(defun nskk-config-preset-save (name)
  "現在の設定をNAMEでプリセットとして保存する。"
  (interactive "sプリセット名: ")
  (nskk-config--ensure-preset-directory)
  (let ((preset-file (expand-file-name
                      (concat name ".el")
                      nskk-config-preset-directory))
        (preset-data (nskk-config--collect-current-values)))
    (with-temp-file preset-file
      (insert ";;; NSKK Configuration Preset\n")
      (insert (format ";;; Name: %s\n" name))
      (insert (format ";;; Created: %s\n\n" (current-time-string)))
      (pp preset-data (current-buffer)))
    (message "プリセット '%s' を保存しました" name)))

(defun nskk-config-preset-load (name)
  "NAMEのプリセットを読み込む。"
  (interactive
   (list (completing-read
          "プリセット: "
          (nskk-config--list-presets)
          nil t)))
  (let ((preset-file (expand-file-name
                      (concat name ".el")
                      nskk-config-preset-directory)))
    (if (file-exists-p preset-file)
        (progn
          (load preset-file)
          (message "プリセット '%s' を読み込みました" name))
      (error "プリセット '%s' が見つかりません" name))))

(defun nskk-config--list-presets ()
  "利用可能なプリセットのリストを取得する。"
  (when (file-exists-p nskk-config-preset-directory)
    (mapcar
     (lambda (file)
       (file-name-sans-extension (file-name-nondirectory file)))
     (directory-files nskk-config-preset-directory nil "\\.el$"))))

(defun nskk-config--collect-current-values ()
  "現在の全設定値を収集する。"
  (list
   ;; コア設定
   :mode-switch-show-message nskk-mode-switch-show-message
   :mode-switch-update-modeline nskk-mode-switch-update-modeline
   :mode-switch-clear-input-on-mode-change nskk-mode-switch-clear-input-on-mode-change

   ;; 学習設定
   :frequency-algorithm nskk-frequency-algorithm
   :frequency-decay-enabled nskk-frequency-decay-enabled
   :frequency-decay-rate nskk-frequency-decay-rate
   :frequency-lru-weight nskk-frequency-lru-weight
   :frequency-lfu-weight nskk-frequency-lfu-weight

   ;; 履歴設定
   :history-enabled nskk-history-enabled
   :history-anonymize nskk-history-anonymize
   :history-max-entries nskk-history-max-entries
   :history-retention-days nskk-history-retention-days

   ;; イベント設定
   :events-enable-logging nskk-events-enable-logging
   :events-max-log-entries nskk-events-max-log-entries

   ;; 永続化設定
   :persist-auto-save nskk-persist-auto-save
   :persist-auto-save-interval nskk-persist-auto-save-interval
   :persist-compression nskk-persist-compression))

;;; プレビュー機能

(defun nskk-config--preview-value (variable value)
  "VARIABLEにVALUEを設定した場合のプレビューを表示する。"
  (when nskk-config-enable-preview
    (message "プレビュー: %s = %S" variable value)))

(defun nskk-config--set-value (variable value)
  "VARIABLEにVALUEを安全に設定する（setoptを使用）。"
  (condition-case err
      (progn
        (setopt variable value)
        (message "%s を %S に設定しました" variable value))
    (error
     (message "設定エラー: %s" (error-message-string err)))))

;;; Transientメニュー定義

;;;###autoload (autoload 'nskk-config-menu "nskk-transient-config" nil t)
(transient-define-prefix nskk-config-menu ()
  "NSKK設定メニュー。"
  ["NSKK 設定"
   ["カテゴリ"
    ("c" "コア設定" nskk-config-core)
    ("i" "入力設定" nskk-config-input)
    ("l" "学習設定" nskk-config-learning)
    ("u" "UI設定" nskk-config-ui)
    ("a" "高度な設定" nskk-config-advanced)]
   ["プリセット"
    ("s" "保存" nskk-config-preset-save-transient)
    ("L" "読み込み" nskk-config-preset-load-transient)
    ("d" "削除" nskk-config-preset-delete)]
   ["操作"
    ("r" "リセット" nskk-config-reset)
    ("q" "終了" transient-quit-one)]])

;;;###autoload (autoload 'nskk-config-core "nskk-transient-config" nil t)
(transient-define-prefix nskk-config-core ()
  "コア設定メニュー。"
  ["コア設定"
   ["モード切り替え"
    ("-m" "メッセージ表示" "nskk-mode-switch-show-message"
     :class transient-option
     :choices ("t" "nil")
     :reader (lambda (_prompt _initial-input _history)
               (if (y-or-n-p "メッセージを表示しますか? ") "t" "nil")))
    ("-u" "モードライン更新" "nskk-mode-switch-update-modeline"
     :class transient-option
     :choices ("t" "nil")
     :reader (lambda (_prompt _initial-input _history)
               (if (y-or-n-p "モードラインを更新しますか? ") "t" "nil")))
    ("-c" "入力クリア" "nskk-mode-switch-clear-input-on-mode-change"
     :class transient-option
     :choices ("t" "nil")
     :reader (lambda (_prompt _initial-input _history)
               (if (y-or-n-p "モード変更時に入力をクリアしますか? ") "t" "nil")))]
   ["操作"
    ("a" "適用" nskk-config-core-apply)
    ("b" "戻る" transient-quit-one)]])

;;;###autoload (autoload 'nskk-config-input "nskk-transient-config" nil t)
(transient-define-prefix nskk-config-input ()
  "入力方式設定メニュー。"
  ["入力方式設定"
   ["入力方式"
    ("q" "QWERTY" nskk-config-input-qwerty)
    ("a" "AZIK" nskk-config-input-azik)
    ("d" "Dvorak" nskk-config-input-dvorak)
    ("c" "Colemak" nskk-config-input-colemak)
    ("k" "かな入力" nskk-config-input-kana)
    ("n" "NICOLA" nskk-config-input-nicola)]
   ["操作"
    ("b" "戻る" transient-quit-one)]])

;;;###autoload (autoload 'nskk-config-learning "nskk-transient-config" nil t)
(transient-define-prefix nskk-config-learning ()
  "学習設定メニュー。"
  ["学習設定"
   ["頻度学習"
    ("-a" "アルゴリズム" "nskk-frequency-algorithm"
     :class transient-option
     :choices ("lru" "lfu" "hybrid")
     :reader (lambda (_prompt _initial-input _history)
               (completing-read "アルゴリズム: " '("lru" "lfu" "hybrid"))))
    ("-d" "減衰有効化" "nskk-frequency-decay-enabled"
     :class transient-option
     :choices ("t" "nil")
     :reader (lambda (_prompt _initial-input _history)
               (if (y-or-n-p "頻度減衰を有効化しますか? ") "t" "nil")))
    ("-r" "減衰率" "nskk-frequency-decay-rate"
     :class transient-option
     :reader (lambda (prompt _initial-input _history)
               (read-number prompt 0.95)))
    ("-l" "LRU重み" "nskk-frequency-lru-weight"
     :class transient-option
     :reader (lambda (prompt _initial-input _history)
               (read-number prompt 0.3)))
    ("-f" "LFU重み" "nskk-frequency-lfu-weight"
     :class transient-option
     :reader (lambda (prompt _initial-input _history)
               (read-number prompt 0.7)))]
   ["履歴"
    ("-h" "履歴有効化" "nskk-history-enabled"
     :class transient-option
     :choices ("t" "nil")
     :reader (lambda (_prompt _initial-input _history)
               (if (y-or-n-p "履歴を有効化しますか? ") "t" "nil")))
    ("-n" "匿名化" "nskk-history-anonymize"
     :class transient-option
     :choices ("t" "nil")
     :reader (lambda (_prompt _initial-input _history)
               (if (y-or-n-p "履歴を匿名化しますか? ") "t" "nil")))
    ("-m" "最大エントリ数" "nskk-history-max-entries"
     :class transient-option
     :reader (lambda (prompt _initial-input _history)
               (read-number prompt 10000)))]
   ["操作"
    ("a" "適用" nskk-config-learning-apply)
    ("b" "戻る" transient-quit-one)]])

;;;###autoload (autoload 'nskk-config-ui "nskk-transient-config" nil t)
(transient-define-prefix nskk-config-ui ()
  "UI設定メニュー。"
  ["UI設定"
   ["イベント"
    ("-l" "ログ有効化" "nskk-events-enable-logging"
     :class transient-option
     :choices ("t" "nil")
     :reader (lambda (_prompt _initial-input _history)
               (if (y-or-n-p "イベントログを有効化しますか? ") "t" "nil")))
    ("-m" "最大ログエントリ数" "nskk-events-max-log-entries"
     :class transient-option
     :reader (lambda (prompt _initial-input _history)
               (read-number prompt 1000)))]
   ["プレビュー"
    ("-p" "プレビュー有効化" "nskk-config-enable-preview"
     :class transient-option
     :choices ("t" "nil")
     :reader (lambda (_prompt _initial-input _history)
               (if (y-or-n-p "設定プレビューを有効化しますか? ") "t" "nil")))]
   ["操作"
    ("a" "適用" nskk-config-ui-apply)
    ("b" "戻る" transient-quit-one)]])

;;;###autoload (autoload 'nskk-config-advanced "nskk-transient-config" nil t)
(transient-define-prefix nskk-config-advanced ()
  "高度な設定メニュー。"
  ["高度な設定"
   ["永続化"
    ("-a" "自動保存" "nskk-persist-auto-save"
     :class transient-option
     :choices ("t" "nil")
     :reader (lambda (_prompt _initial-input _history)
               (if (y-or-n-p "自動保存を有効化しますか? ") "t" "nil")))
    ("-i" "保存間隔(秒)" "nskk-persist-auto-save-interval"
     :class transient-option
     :reader (lambda (prompt _initial-input _history)
               (read-number prompt 300)))
    ("-c" "圧縮" "nskk-persist-compression"
     :class transient-option
     :choices ("t" "nil")
     :reader (lambda (_prompt _initial-input _history)
               (if (y-or-n-p "圧縮を有効化しますか? ") "t" "nil")))]
   ["サーバー"
    ("-p" "プロトコルバージョン" "nskk-server-protocol-version"
     :class transient-option
     :choices ("auto" "1.0" "2.0")
     :reader (lambda (_prompt _initial-input _history)
               (completing-read "プロトコル: " '("auto" "1.0" "2.0"))))
    ("-e" "エンコーディング" "nskk-server-protocol-encoding"
     :class transient-option
     :choices ("euc-jp" "utf-8")
     :reader (lambda (_prompt _initial-input _history)
               (completing-read "エンコーディング: " '("euc-jp" "utf-8"))))]
   ["操作"
    ("a" "適用" nskk-config-advanced-apply)
    ("b" "戻る" transient-quit-one)]])

;;; アクション関数

(defun nskk-config-core-apply (&optional args)
  "コア設定を適用する。"
  (interactive
   (list (transient-args 'nskk-config-core)))
  (nskk-config--apply-args args)
  (message "コア設定を適用しました"))

(defun nskk-config-learning-apply (&optional args)
  "学習設定を適用する。"
  (interactive
   (list (transient-args 'nskk-config-learning)))
  (nskk-config--apply-args args)
  (message "学習設定を適用しました"))

(defun nskk-config-ui-apply (&optional args)
  "UI設定を適用する。"
  (interactive
   (list (transient-args 'nskk-config-ui)))
  (nskk-config--apply-args args)
  (message "UI設定を適用しました"))

(defun nskk-config-advanced-apply (&optional args)
  "高度な設定を適用する。"
  (interactive
   (list (transient-args 'nskk-config-advanced)))
  (nskk-config--apply-args args)
  (message "高度な設定を適用しました"))

(defun nskk-config--apply-args (args)
  "ARGSから設定を抽出して適用する。"
  (dolist (arg args)
    (when (string-match "^\\([^=]+\\)=\\(.+\\)$" arg)
      (let* ((var-name (match-string 1 arg))
             (value-str (match-string 2 arg))
             (var-symbol (intern var-name))
             (value (nskk-config--parse-value value-str)))
        (nskk-config--set-value var-symbol value)))))

(defun nskk-config--parse-value (value-str)
  "VALUE-STRを適切な型に変換する。"
  (cond
   ((string= value-str "t") t)
   ((string= value-str "nil") nil)
   ((string-match "^[0-9.]+$" value-str)
    (string-to-number value-str))
   (t (intern value-str))))

(defun nskk-config-preset-save-transient ()
  "Transient経由でプリセットを保存する。"
  (interactive)
  (call-interactively #'nskk-config-preset-save))

(defun nskk-config-preset-load-transient ()
  "Transient経由でプリセットを読み込む。"
  (interactive)
  (call-interactively #'nskk-config-preset-load))

(defun nskk-config-preset-delete ()
  "プリセットを削除する。"
  (interactive)
  (let ((preset-name (completing-read
                      "削除するプリセット: "
                      (nskk-config--list-presets)
                      nil t)))
    (when (y-or-n-p (format "プリセット '%s' を削除しますか? " preset-name))
      (let ((preset-file (expand-file-name
                          (concat preset-name ".el")
                          nskk-config-preset-directory)))
        (when (file-exists-p preset-file)
          (delete-file preset-file)
          (message "プリセット '%s' を削除しました" preset-name))))))

(defun nskk-config-reset ()
  "全設定をデフォルト値にリセットする。"
  (interactive)
  (when (y-or-n-p "全設定をリセットしますか? ")
    ;; 各変数をデフォルト値にリセット
    (setopt nskk-mode-switch-show-message t)
    (setopt nskk-mode-switch-update-modeline t)
    (setopt nskk-mode-switch-clear-input-on-mode-change t)
    (setopt nskk-frequency-algorithm 'hybrid)
    (setopt nskk-frequency-decay-enabled t)
    (setopt nskk-frequency-decay-rate 0.95)
    (setopt nskk-history-enabled t)
    (setopt nskk-history-anonymize t)
    (message "設定をリセットしました")))

;; 入力方式選択関数（プレースホルダー）
(defun nskk-config-input-qwerty ()
  "QWERTY入力方式を選択する。"
  (interactive)
  (message "QWERTY入力方式を選択しました"))

(defun nskk-config-input-azik ()
  "AZIK入力方式を選択する。"
  (interactive)
  (message "AZIK入力方式を選択しました"))

(defun nskk-config-input-dvorak ()
  "Dvorak入力方式を選択する。"
  (interactive)
  (message "Dvorak入力方式を選択しました"))

(defun nskk-config-input-colemak ()
  "Colemak入力方式を選択する。"
  (interactive)
  (message "Colemak入力方式を選択しました"))

(defun nskk-config-input-kana ()
  "かな入力方式を選択する。"
  (interactive)
  (message "かな入力方式を選択しました"))

(defun nskk-config-input-nicola ()
  "NICOLA入力方式を選択する。"
  (interactive)
  (message "NICOLA入力方式を選択しました"))

(provide 'nskk-transient-config)
;;; nskk-transient-config.el ends here
