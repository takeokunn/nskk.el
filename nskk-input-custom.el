;;; nskk-input-custom.el --- Custom input method framework for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, custom
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;; This file is part of NSKK.

;;; Commentary:

;; カスタム入力方式フレームワークの実装。
;;
;; ユーザーが独自の入力方式を定義・登録できるフレームワーク。
;; 既存の入力方式をベースにカスタマイズしたり、
;; 完全に新しい入力方式を作成することが可能。
;;
;; 主な機能:
;; - カスタム変換テーブルの定義
;; - 既存入力方式のカスタマイズ
;; - 複数のカスタム入力方式の管理
;; - 入力方式のインポート/エクスポート
;;
;; 使用例:
;; (require 'nskk-input-custom)
;;
;; ;; カスタム入力方式の定義
;; (nskk-input-custom-define
;;  'my-custom
;;  '(("dh" . "で")
;;    ("kk" . "っか"))
;;  :base 'azik
;;  :description "My custom AZIK extension")
;;
;; ;; カスタム入力方式の使用
;; (nskk-register-input-method 'my-custom)

;;; Code:

(require 'cl-lib)

;;; カスタム入力方式の定義

(cl-defstruct (nskk-input-custom-method
               (:constructor nskk-input-custom-method-create)
               (:copier nil))
  "カスタム入力方式の構造体。"
  name                ; 入力方式の名前（シンボル）
  table               ; 変換テーブル（連想リスト）
  base                ; ベースとなる入力方式（シンボル、オプション）
  description         ; 説明文字列
  hash-table          ; 高速検索用ハッシュテーブル
  priority)           ; 優先度（数値、大きいほど優先）

;;; グローバル変数

(defvar nskk-input-custom-methods nil
  "登録されているカスタム入力方式のリスト。
各要素は `nskk-input-custom-method' 構造体。")

;;; カスタム入力方式の定義

(defun nskk-input-custom-define (name table &rest options)
  "カスタム入力方式を定義する。

NAME は入力方式の名前（シンボル）。
TABLE は変換テーブル（連想リスト）。
OPTIONS はキーワード引数:
  :base SYMBOL        - ベースとなる入力方式
  :description STRING - 説明文字列
  :priority NUMBER    - 優先度（デフォルト: 100）

使用例:
  (nskk-input-custom-define
   'my-azik
   '((\"dh\" . \"で\") (\"kk\" . \"っか\"))
   :base 'azik
   :description \"My AZIK extension\"
   :priority 150)"
  (let* ((base (plist-get options :base))
         (description (plist-get options :description))
         (priority (or (plist-get options :priority) 100))
         (method (nskk-input-custom-method-create
                  :name name
                  :table table
                  :base base
                  :description description
                  :priority priority)))

    ;; ハッシュテーブルの初期化
    (nskk-input-custom--init-hash-table method)

    ;; 既存の定義を削除
    (setq nskk-input-custom-methods
          (cl-remove-if (lambda (m)
                          (eq (nskk-input-custom-method-name m) name))
                        nskk-input-custom-methods))

    ;; 新しい定義を追加
    (push method nskk-input-custom-methods)

    ;; 優先度順にソート
    (setq nskk-input-custom-methods
          (cl-sort nskk-input-custom-methods #'>
                   :key #'nskk-input-custom-method-priority))

    method))

;;; ハッシュテーブル管理

(defun nskk-input-custom--init-hash-table (method)
  "METHODのハッシュテーブルを初期化する。"
  (let ((hash-table (make-hash-table :test 'equal
                                     :size (max 100 (* 2 (length (nskk-input-custom-method-table method)))))))
    ;; ベース入力方式がある場合、その変換テーブルを含める
    (when (nskk-input-custom-method-base method)
      (let ((base-table (nskk-input-custom--get-base-table
                         (nskk-input-custom-method-base method))))
        (dolist (entry base-table)
          (puthash (car entry) (cdr entry) hash-table))))

    ;; カスタムテーブルを追加（上書き）
    (dolist (entry (nskk-input-custom-method-table method))
      (puthash (car entry) (cdr entry) hash-table))

    (setf (nskk-input-custom-method-hash-table method) hash-table)))

(defun nskk-input-custom--get-base-table (base-name)
  "BASE-NAMEの変換テーブルを取得する。"
  (cond
   ((eq base-name 'azik)
    (require 'nskk-input-azik)
    nskk-input-azik-table)
   ((eq base-name 'act)
    (require 'nskk-input-act)
    nskk-input-act-table)
   ((eq base-name 'tutcode)
    (require 'nskk-input-tutcode)
    nskk-input-tutcode-table)
   ((eq base-name 'qwerty)
    (require 'nskk-romaji-tables)
    nskk-romaji-table)
   (t
    (warn "Unknown base input method: %s" base-name)
    nil)))

;;; 検索関数

(defun nskk-input-custom-lookup (name key)
  "NAMEのカスタム入力方式でKEYを変換する。

使用例:
  (nskk-input-custom-lookup 'my-custom \"dh\")  ;; => \"で\""
  (let ((method (nskk-input-custom-get-method name)))
    (when method
      (gethash key (nskk-input-custom-method-hash-table method)))))

(defun nskk-input-custom-get-method (name)
  "NAMEのカスタム入力方式を取得する。"
  (cl-find name nskk-input-custom-methods
           :key #'nskk-input-custom-method-name))

(defun nskk-input-custom-get-candidates (name prefix)
  "NAMEのカスタム入力方式でPREFIXで始まる候補を取得する。"
  (let ((method (nskk-input-custom-get-method name)))
    (when method
      (let ((candidates nil)
            (hash-table (nskk-input-custom-method-hash-table method)))
        (maphash (lambda (key _value)
                   (when (string-prefix-p prefix key)
                     (push key candidates)))
                 hash-table)
        (nreverse candidates)))))

;;; 入力方式の管理

(defun nskk-input-custom-list-methods ()
  "登録されている全カスタム入力方式のリストを返す。"
  (mapcar #'nskk-input-custom-method-name nskk-input-custom-methods))

(defun nskk-input-custom-remove-method (name)
  "NAMEのカスタム入力方式を削除する。"
  (setq nskk-input-custom-methods
        (cl-remove-if (lambda (m)
                        (eq (nskk-input-custom-method-name m) name))
                      nskk-input-custom-methods)))

(defun nskk-input-custom-describe-method (name)
  "NAMEのカスタム入力方式の情報を表示する。"
  (interactive (list (intern (completing-read "Input method: "
                                               (nskk-input-custom-list-methods)))))
  (let ((method (nskk-input-custom-get-method name)))
    (if method
        (message "Name: %s\nBase: %s\nEntries: %d\nPriority: %d\nDescription: %s"
                 (nskk-input-custom-method-name method)
                 (or (nskk-input-custom-method-base method) "none")
                 (hash-table-count (nskk-input-custom-method-hash-table method))
                 (nskk-input-custom-method-priority method)
                 (or (nskk-input-custom-method-description method) ""))
      (message "No such custom input method: %s" name))))

;;; インポート/エクスポート

(defun nskk-input-custom-export-method (name file)
  "NAMEのカスタム入力方式をFILEにエクスポートする。"
  (interactive (list (intern (completing-read "Input method: "
                                               (nskk-input-custom-list-methods)))
                     (read-file-name "Export to: ")))
  (let ((method (nskk-input-custom-get-method name)))
    (if method
        (with-temp-file file
          (prin1 `(nskk-input-custom-define
                   ',(nskk-input-custom-method-name method)
                   ',(nskk-input-custom-method-table method)
                   :base ',(nskk-input-custom-method-base method)
                   :description ,(nskk-input-custom-method-description method)
                   :priority ,(nskk-input-custom-method-priority method))
                 (current-buffer))
          (message "Exported to %s" file))
      (error "No such custom input method: %s" name))))

(defun nskk-input-custom-import-method (file)
  "FILEからカスタム入力方式をインポートする。"
  (interactive "fImport from: ")
  (load-file file)
  (message "Imported from %s" file))

;;; 統計情報

(defun nskk-input-custom-stats (name)
  "NAMEのカスタム入力方式の統計情報を返す。"
  (let ((method (nskk-input-custom-get-method name)))
    (when method
      (list :name name
            :total (hash-table-count (nskk-input-custom-method-hash-table method))
            :custom-entries (length (nskk-input-custom-method-table method))
            :base (nskk-input-custom-method-base method)
            :priority (nskk-input-custom-method-priority method)))))

;;; パフォーマンス最適化

(defsubst nskk-input-custom-lookup-fast (name key)
  "NAMEのカスタム入力方式でKEYを高速変換する。"
  (let ((method (nskk-input-custom-get-method name)))
    (when method
      (gethash key (nskk-input-custom-method-hash-table method)))))

(provide 'nskk-input-custom)

;;; nskk-input-custom.el ends here
