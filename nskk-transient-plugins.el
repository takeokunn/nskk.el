;;; nskk-transient-plugins.el --- Transient plugin management UI for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, transient, plugins
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

;; このファイルはEmacs 31のTransient UIを使用したNSKKの拡張管理UIを実装します。
;;
;; 特徴:
;; - プラグイン一覧表示
;; - インストール/アンインストール機能
;; - 有効/無効切り替え
;; - プラグイン設定画面
;; - 依存関係管理
;; - バージョン管理
;;
;; 主要機能:
;; - `nskk-plugins-menu'           - プラグイン管理メニュー
;; - `nskk-plugin-install'         - プラグインインストール
;; - `nskk-plugin-uninstall'       - プラグインアンインストール
;; - `nskk-plugin-enable'          - プラグイン有効化
;; - `nskk-plugin-disable'         - プラグイン無効化
;; - `nskk-plugin-configure'       - プラグイン設定
;; - `nskk-plugin-update'          - プラグイン更新
;; - `nskk-plugin-list-available'  - 利用可能プラグイン一覧
;;
;; 使用例:
;; M-x nskk-plugins-menu
;;
;; プラグイン構造:
;; (defstruct nskk-plugin
;;   name version description author
;;   dependencies enabled-p config-function)

;;; Code:

(require 'cl-lib)
(require 'transient)

;;; グループ定義

(defgroup nskk-plugins nil
  "NSKKプラグイン管理のカスタマイズ。"
  :group 'nskk
  :prefix "nskk-plugins-")

;;; 設定変数

(defcustom nskk-plugins-directory
  (expand-file-name "nskk-plugins" user-emacs-directory)
  "プラグインのインストールディレクトリ。"
  :type 'directory
  :group 'nskk-plugins)

(defcustom nskk-plugins-repository-url
  "https://raw.githubusercontent.com/nskk/plugins/main/registry.json"
  "プラグインレジストリのURL。"
  :type 'string
  :group 'nskk-plugins)

(defcustom nskk-plugins-auto-update nil
  "起動時に自動的にプラグインを更新するか。"
  :type 'boolean
  :group 'nskk-plugins)

(defcustom nskk-plugins-check-dependencies t
  "プラグインインストール時に依存関係をチェックするか。"
  :type 'boolean
  :group 'nskk-plugins)

;;; データ構造

(cl-defstruct nskk-plugin
  "NSKKプラグインの構造体。"
  (name nil :read-only t)
  (version "0.0.0")
  (description "")
  (author "")
  (url "")
  (dependencies '())
  (enabled-p nil)
  (installed-p nil)
  (config-function nil)
  (install-date nil)
  (update-date nil))

;;; プラグインレジストリ

(defvar nskk-plugins--registry nil
  "プラグインレジストリのキャッシュ。")

(defvar nskk-plugins--installed nil
  "インストール済みプラグインのハッシュテーブル。
キーはプラグイン名（シンボル）、値はnskk-plugin構造体。")

(defvar nskk-plugins--enabled nil
  "有効化されたプラグインのリスト。")

;;; 初期化

(defun nskk-plugins--init ()
  "プラグインシステムを初期化する。"
  (unless nskk-plugins--installed
    (setq nskk-plugins--installed (make-hash-table :test 'eq)))
  (nskk-plugins--ensure-directory)
  (nskk-plugins--load-installed-plugins))

(defun nskk-plugins--ensure-directory ()
  "プラグインディレクトリを作成する。"
  (unless (file-exists-p nskk-plugins-directory)
    (make-directory nskk-plugins-directory t)))

(defun nskk-plugins--load-installed-plugins ()
  "インストール済みプラグインを読み込む。"
  (when (file-exists-p nskk-plugins-directory)
    (dolist (dir (directory-files nskk-plugins-directory t "^[^.]"))
      (when (file-directory-p dir)
        (let* ((plugin-name (intern (file-name-nondirectory dir)))
               (plugin-file (expand-file-name "plugin.el" dir)))
          (when (file-exists-p plugin-file)
            (load plugin-file t)
            (let ((plugin (gethash plugin-name nskk-plugins--installed)))
              (when plugin
                (setf (nskk-plugin-installed-p plugin) t)))))))))

;;; レジストリ管理

(defun nskk-plugins--fetch-registry ()
  "プラグインレジストリを取得する。"
  (condition-case err
      (with-temp-buffer
        (url-insert-file-contents nskk-plugins-repository-url)
        (goto-char (point-min))
        (setq nskk-plugins--registry (json-read)))
    (error
     (message "レジストリの取得に失敗しました: %s" (error-message-string err))
     nil)))

(defun nskk-plugins--get-registry ()
  "レジストリを取得する（キャッシュ使用）。"
  (or nskk-plugins--registry
      (nskk-plugins--fetch-registry)))

(defun nskk-plugins--list-available-plugins ()
  "利用可能なプラグインのリストを取得する。"
  (let ((registry (nskk-plugins--get-registry))
        (plugins '()))
    (when registry
      (maphash
       (lambda (name info)
         (push (cons name info) plugins))
       registry))
    (nreverse plugins)))

;;; プラグイン操作

(defun nskk-plugin-install (plugin-name)
  "PLUGIN-NAMEのプラグインをインストールする。"
  (interactive
   (list (intern (completing-read
                  "インストールするプラグイン: "
                  (mapcar (lambda (p) (symbol-name (car p)))
                          (nskk-plugins--list-available-plugins))
                  nil t))))
  (nskk-plugins--init)
  (let* ((registry (nskk-plugins--get-registry))
         (plugin-info (gethash plugin-name registry)))
    (if plugin-info
        (progn
          (message "プラグイン %s をインストール中..." plugin-name)
          ;; 依存関係チェック
          (when nskk-plugins-check-dependencies
            (nskk-plugins--check-dependencies plugin-info))
          ;; インストール処理
          (nskk-plugins--do-install plugin-name plugin-info)
          (message "プラグイン %s をインストールしました" plugin-name))
      (error "プラグイン %s が見つかりません" plugin-name))))

(defun nskk-plugins--do-install (plugin-name plugin-info)
  "PLUGIN-NAMEをPLUGIN-INFOに基づいてインストールする。"
  (let* ((plugin-dir (expand-file-name (symbol-name plugin-name)
                                       nskk-plugins-directory))
         (url (plist-get plugin-info :url))
         (plugin (make-nskk-plugin
                  :name plugin-name
                  :version (plist-get plugin-info :version)
                  :description (plist-get plugin-info :description)
                  :author (plist-get plugin-info :author)
                  :url url
                  :dependencies (plist-get plugin-info :dependencies)
                  :installed-p t
                  :enabled-p nil
                  :install-date (current-time))))
    ;; ディレクトリ作成
    (make-directory plugin-dir t)
    ;; プラグインファイルをダウンロード（簡略版）
    (with-temp-file (expand-file-name "plugin.el" plugin-dir)
      (insert (format ";;; %s plugin\n" plugin-name))
      (insert (format "(provide '%s)\n" plugin-name)))
    ;; レジストリに追加
    (puthash plugin-name plugin nskk-plugins--installed)))

(defun nskk-plugin-uninstall (plugin-name)
  "PLUGIN-NAMEのプラグインをアンインストールする。"
  (interactive
   (list (intern (completing-read
                  "アンインストールするプラグイン: "
                  (nskk-plugins--list-installed-plugins)
                  nil t))))
  (nskk-plugins--init)
  (when (y-or-n-p (format "プラグイン %s をアンインストールしますか? " plugin-name))
    (let ((plugin-dir (expand-file-name (symbol-name plugin-name)
                                        nskk-plugins-directory)))
      ;; ディレクトリ削除
      (when (file-exists-p plugin-dir)
        (delete-directory plugin-dir t))
      ;; レジストリから削除
      (remhash plugin-name nskk-plugins--installed)
      (message "プラグイン %s をアンインストールしました" plugin-name))))

(defun nskk-plugin-enable (plugin-name)
  "PLUGIN-NAMEのプラグインを有効化する。"
  (interactive
   (list (intern (completing-read
                  "有効化するプラグイン: "
                  (nskk-plugins--list-disabled-plugins)
                  nil t))))
  (nskk-plugins--init)
  (let ((plugin (gethash plugin-name nskk-plugins--installed)))
    (if plugin
        (progn
          (setf (nskk-plugin-enabled-p plugin) t)
          (push plugin-name nskk-plugins--enabled)
          ;; プラグインの初期化関数を実行
          (let ((init-func (intern (format "%s-init" plugin-name))))
            (when (fboundp init-func)
              (funcall init-func)))
          (message "プラグイン %s を有効化しました" plugin-name))
      (error "プラグイン %s がインストールされていません" plugin-name))))

(defun nskk-plugin-disable (plugin-name)
  "PLUGIN-NAMEのプラグインを無効化する。"
  (interactive
   (list (intern (completing-read
                  "無効化するプラグイン: "
                  (nskk-plugins--list-enabled-plugins)
                  nil t))))
  (nskk-plugins--init)
  (let ((plugin (gethash plugin-name nskk-plugins--installed)))
    (if plugin
        (progn
          (setf (nskk-plugin-enabled-p plugin) nil)
          (setq nskk-plugins--enabled (delq plugin-name nskk-plugins--enabled))
          ;; プラグインのクリーンアップ関数を実行
          (let ((cleanup-func (intern (format "%s-cleanup" plugin-name))))
            (when (fboundp cleanup-func)
              (funcall cleanup-func)))
          (message "プラグイン %s を無効化しました" plugin-name))
      (error "プラグイン %s がインストールされていません" plugin-name))))

(defun nskk-plugin-update (plugin-name)
  "PLUGIN-NAMEのプラグインを更新する。"
  (interactive
   (list (intern (completing-read
                  "更新するプラグイン: "
                  (nskk-plugins--list-installed-plugins)
                  nil t))))
  (nskk-plugins--init)
  (message "プラグイン %s を更新中..." plugin-name)
  ;; 一旦アンインストールして再インストール
  (nskk-plugin-uninstall plugin-name)
  (nskk-plugin-install plugin-name)
  (message "プラグイン %s を更新しました" plugin-name))

(defun nskk-plugin-configure (plugin-name)
  "PLUGIN-NAMEのプラグインを設定する。"
  (interactive
   (list (intern (completing-read
                  "設定するプラグイン: "
                  (nskk-plugins--list-installed-plugins)
                  nil t))))
  (nskk-plugins--init)
  (let ((plugin (gethash plugin-name nskk-plugins--installed)))
    (if plugin
        (let ((config-func (nskk-plugin-config-function plugin)))
          (if config-func
              (funcall config-func)
            (message "プラグイン %s には設定画面がありません" plugin-name)))
      (error "プラグイン %s がインストールされていません" plugin-name))))

;;; ヘルパー関数

(defun nskk-plugins--list-installed-plugins ()
  "インストール済みプラグインのリストを取得する。"
  (let ((plugins '()))
    (maphash
     (lambda (name _plugin)
       (push (symbol-name name) plugins))
     nskk-plugins--installed)
    (nreverse plugins)))

(defun nskk-plugins--list-enabled-plugins ()
  "有効化されたプラグインのリストを取得する。"
  (mapcar #'symbol-name nskk-plugins--enabled))

(defun nskk-plugins--list-disabled-plugins ()
  "無効化されたプラグインのリストを取得する。"
  (let ((plugins '()))
    (maphash
     (lambda (name plugin)
       (unless (nskk-plugin-enabled-p plugin)
         (push (symbol-name name) plugins)))
     nskk-plugins--installed)
    (nreverse plugins)))

(defun nskk-plugins--check-dependencies (plugin-info)
  "PLUGIN-INFOの依存関係をチェックする。"
  (let ((deps (plist-get plugin-info :dependencies)))
    (dolist (dep deps)
      (unless (gethash dep nskk-plugins--installed)
        (when (y-or-n-p (format "依存プラグイン %s をインストールしますか? " dep))
          (nskk-plugin-install dep))))))

(defun nskk-plugins--format-plugin-info (plugin)
  "PLUGINの情報を整形して返す。"
  (format "%s (v%s) - %s [%s]"
          (nskk-plugin-name plugin)
          (nskk-plugin-version plugin)
          (nskk-plugin-description plugin)
          (if (nskk-plugin-enabled-p plugin) "有効" "無効")))

;;; Transientメニュー定義

;;;###autoload (autoload 'nskk-plugins-menu "nskk-transient-plugins" nil t)
(transient-define-prefix nskk-plugins-menu ()
  "NSKK プラグイン管理メニュー。"
  ["NSKK プラグイン管理"
   ["プラグイン操作"
    ("i" "インストール" nskk-plugin-install)
    ("u" "アンインストール" nskk-plugin-uninstall)
    ("e" "有効化" nskk-plugin-enable)
    ("d" "無効化" nskk-plugin-disable)
    ("U" "更新" nskk-plugin-update)
    ("c" "設定" nskk-plugin-configure)]
   ["表示"
    ("l" "インストール済み一覧" nskk-plugins-list-installed)
    ("a" "利用可能一覧" nskk-plugins-list-available)
    ("E" "有効化済み一覧" nskk-plugins-list-enabled)]
   ["レジストリ"
    ("r" "更新" nskk-plugins-refresh-registry)
    ("s" "検索" nskk-plugins-search)]
   ["操作"
    ("q" "終了" transient-quit-one)]])

;;; 一覧表示機能

(defun nskk-plugins-list-installed ()
  "インストール済みプラグインを一覧表示する。"
  (interactive)
  (nskk-plugins--init)
  (let ((buffer (get-buffer-create "*NSKK Installed Plugins*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== インストール済みプラグイン ===\n\n")
      (maphash
       (lambda (_name plugin)
         (insert (format "  - %s\n" (nskk-plugins--format-plugin-info plugin))))
       nskk-plugins--installed)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun nskk-plugins-list-available ()
  "利用可能なプラグインを一覧表示する。"
  (interactive)
  (let ((buffer (get-buffer-create "*NSKK Available Plugins*"))
        (plugins (nskk-plugins--list-available-plugins)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== 利用可能なプラグイン ===\n\n")
      (dolist (plugin plugins)
        (insert (format "  - %s: %s\n"
                        (car plugin)
                        (plist-get (cdr plugin) :description))))
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun nskk-plugins-list-enabled ()
  "有効化されたプラグインを一覧表示する。"
  (interactive)
  (nskk-plugins--init)
  (let ((buffer (get-buffer-create "*NSKK Enabled Plugins*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== 有効化されたプラグイン ===\n\n")
      (dolist (name nskk-plugins--enabled)
        (let ((plugin (gethash name nskk-plugins--installed)))
          (when plugin
            (insert (format "  - %s\n" (nskk-plugins--format-plugin-info plugin))))))
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun nskk-plugins-refresh-registry ()
  "プラグインレジストリを更新する。"
  (interactive)
  (message "レジストリを更新中...")
  (setq nskk-plugins--registry nil)
  (if (nskk-plugins--fetch-registry)
      (message "レジストリを更新しました")
    (message "レジストリの更新に失敗しました")))

(defun nskk-plugins-search ()
  "プラグインを検索する。"
  (interactive)
  (let* ((query (read-string "検索キーワード: "))
         (plugins (nskk-plugins--list-available-plugins))
         (results '()))
    (dolist (plugin plugins)
      (when (or (string-match-p query (symbol-name (car plugin)))
                (string-match-p query (plist-get (cdr plugin) :description)))
        (push plugin results)))
    (if results
        (let ((buffer (get-buffer-create "*NSKK Plugin Search*")))
          (with-current-buffer buffer
            (erase-buffer)
            (insert (format "=== 検索結果: '%s' ===\n\n" query))
            (dolist (plugin results)
              (insert (format "  - %s: %s\n"
                              (car plugin)
                              (plist-get (cdr plugin) :description))))
            (goto-char (point-min)))
          (display-buffer buffer))
      (message "該当するプラグインが見つかりませんでした"))))

(provide 'nskk-transient-plugins)
;;; nskk-transient-plugins.el ends here
