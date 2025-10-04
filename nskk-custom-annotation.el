;;; nskk-custom-annotation.el --- Custom annotation system for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, annotation
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

;; このファイルはユーザー定義注釈システムを実装します。
;;
;; 特徴:
;; - ユーザー定義注釈の管理
;; - 注釈の追加・編集・削除
;; - 注釈辞書の保存・読み込み
;; - 注釈テンプレート
;; - AI生成注釈準備（拡張統合連携用フック）
;; - 注釈変換フック
;;
;; 使用例:
;;
;;   (require 'nskk-custom-annotation)
;;
;;   ;; カスタム注釈を追加
;;   (nskk-custom-annotation-add "かんじ" "漢字" "Chinese characters")
;;
;;   ;; カスタム注釈を取得
;;   (nskk-custom-annotation-get "かんじ" "漢字")
;;   ;; => "Chinese characters"
;;
;;   ;; カスタム注釈を編集
;;   (nskk-custom-annotation-edit "かんじ" "漢字" "Japanese Kanji")
;;
;;   ;; カスタム注釈を削除
;;   (nskk-custom-annotation-remove "かんじ" "漢字")
;;
;;   ;; 注釈ファイルを保存
;;   (nskk-custom-annotation-save)
;;
;;   ;; AI生成注釈フックを登録
;;   (add-hook 'nskk-custom-annotation-generate-hooks
;;             #'my-ai-annotation-generator)

;;; Code:

(require 'cl-lib)
(require 'nskk-annotation-parser)

;;; カスタマイズ変数

(defgroup nskk-custom-annotation nil
  "NSKK custom annotation settings."
  :group 'nskk
  :prefix "nskk-custom-annotation-")

(defcustom nskk-custom-annotation-file
  (expand-file-name "~/.skk/custom-annotations")
  "カスタム注釈を保存するファイルパス。"
  :type 'file
  :group 'nskk-custom-annotation)

(defcustom nskk-custom-annotation-auto-save t
  "非nilの場合、注釈を追加・編集時に自動保存する。"
  :type 'boolean
  :group 'nskk-custom-annotation)

(defcustom nskk-custom-annotation-enable-templates t
  "非nilの場合、注釈テンプレート機能を有効にする。"
  :type 'boolean
  :group 'nskk-custom-annotation)

(defcustom nskk-custom-annotation-templates
  '((english . "en:%s")
    (reading . "読み:%s")
    (definition . "定義:%s")
    (example . "例:%s")
    (note . "Note:%s"))
  "注釈テンプレートのalist。
各要素は (テンプレート名 . フォーマット文字列) の形式。"
  :type '(alist :key-type symbol :value-type string)
  :group 'nskk-custom-annotation)

;;; 内部変数

(defvar nskk-custom-annotation--table (make-hash-table :test 'equal)
  "カスタム注釈を保存するハッシュテーブル。
キー: (midashi . candidate) のcons
値: 注釈文字列")

(defvar nskk-custom-annotation--modified nil
  "カスタム注釈が変更されたかどうかのフラグ。")

;;; フック

(defvar nskk-custom-annotation-generate-hooks nil
  "注釈を自動生成するためのフック。
各フック関数は (midashi candidate) を引数に取り、注釈文字列を返す。
拡張統合でAI統合時に使用予定。")

(defvar nskk-custom-annotation-before-add-hook nil
  "注釈を追加する前に呼ばれるフック。
引数: (midashi candidate annotation)")

(defvar nskk-custom-annotation-after-add-hook nil
  "注釈を追加した後に呼ばれるフック。
引数: (midashi candidate annotation)")

(defvar nskk-custom-annotation-before-remove-hook nil
  "注釈を削除する前に呼ばれるフック。
引数: (midashi candidate)")

(defvar nskk-custom-annotation-after-remove-hook nil
  "注釈を削除した後に呼ばれるフック。
引数: (midashi candidate)")

;;; カスタム注釈管理

;;;###autoload
(defun nskk-custom-annotation-add (midashi candidate annotation)
  "MIDASHI の CANDIDATE に対して ANNOTATION を追加する。"
  (interactive
   (list (read-string "見出し語: ")
         (read-string "候補: ")
         (read-string "注釈: ")))

  (run-hook-with-args 'nskk-custom-annotation-before-add-hook
                     midashi candidate annotation)

  (let ((key (cons midashi candidate)))
    (puthash key annotation nskk-custom-annotation--table)
    (setq nskk-custom-annotation--modified t))

  (run-hook-with-args 'nskk-custom-annotation-after-add-hook
                     midashi candidate annotation)

  (when nskk-custom-annotation-auto-save
    (nskk-custom-annotation-save))

  (message "Added annotation for %s/%s: %s" midashi candidate annotation))

;;;###autoload
(defun nskk-custom-annotation-get (midashi candidate)
  "MIDASHI の CANDIDATE に対する注釈を取得する。
注釈がない場合は nil を返す。"
  (let ((key (cons midashi candidate)))
    (gethash key nskk-custom-annotation--table)))

;;;###autoload
(defun nskk-custom-annotation-edit (midashi candidate new-annotation)
  "MIDASHI の CANDIDATE の注釈を NEW-ANNOTATION に変更する。"
  (interactive
   (let* ((midashi (read-string "見出し語: "))
          (candidate (read-string "候補: "))
          (current (nskk-custom-annotation-get midashi candidate))
          (new (read-string "新しい注釈: " current)))
     (list midashi candidate new)))

  (if (nskk-custom-annotation-get midashi candidate)
      (progn
        (nskk-custom-annotation-add midashi candidate new-annotation)
        (message "Updated annotation for %s/%s" midashi candidate))
    (message "No annotation found for %s/%s" midashi candidate)))

;;;###autoload
(defun nskk-custom-annotation-remove (midashi candidate)
  "MIDASHI の CANDIDATE から注釈を削除する。"
  (interactive
   (list (read-string "見出し語: ")
         (read-string "候補: ")))

  (run-hook-with-args 'nskk-custom-annotation-before-remove-hook
                     midashi candidate)

  (let ((key (cons midashi candidate)))
    (when (gethash key nskk-custom-annotation--table)
      (remhash key nskk-custom-annotation--table)
      (setq nskk-custom-annotation--modified t)
      (message "Removed annotation for %s/%s" midashi candidate)

      (run-hook-with-args 'nskk-custom-annotation-after-remove-hook
                         midashi candidate)

      (when nskk-custom-annotation-auto-save
        (nskk-custom-annotation-save))
      t)))

;;;###autoload
(defun nskk-custom-annotation-has-p (midashi candidate)
  "MIDASHI の CANDIDATE が注釈を持つかどうかを判定する。"
  (let ((key (cons midashi candidate)))
    (and (gethash key nskk-custom-annotation--table) t)))

;;; 注釈テンプレート

;;;###autoload
(defun nskk-custom-annotation-add-from-template (midashi candidate template-name text)
  "MIDASHI の CANDIDATE に対して、TEMPLATE-NAME を使用して注釈を追加する。"
  (interactive
   (list (read-string "見出し語: ")
         (read-string "候補: ")
         (intern (completing-read "テンプレート: "
                                 (mapcar #'car nskk-custom-annotation-templates)))
         (read-string "内容: ")))

  (when nskk-custom-annotation-enable-templates
    (if-let ((template (alist-get template-name nskk-custom-annotation-templates)))
        (let ((annotation (format template text)))
          (nskk-custom-annotation-add midashi candidate annotation))
      (error "Template not found: %s" template-name))))

;;;###autoload
(defun nskk-custom-annotation-list-templates ()
  "利用可能な注釈テンプレートのリストを表示する。"
  (interactive)
  (message "Available templates:\n%s"
           (mapconcat (lambda (tmpl)
                       (format "  %s: %s" (car tmpl) (cdr tmpl)))
                     nskk-custom-annotation-templates
                     "\n")))

;;; AI生成注釈（拡張統合準備）

;;;###autoload
(defun nskk-custom-annotation-generate (midashi candidate)
  "MIDASHI の CANDIDATE に対して注釈を自動生成する。
`nskk-custom-annotation-generate-hooks' に登録された関数を順次実行し、
最初に非nilを返した注釈を使用する。"
  (interactive
   (list (read-string "見出し語: ")
         (read-string "候補: ")))

  (let ((annotation nil))
    (run-hook-with-args-until-success
     'nskk-custom-annotation-generate-hooks
     midashi candidate)

    (when annotation
      (nskk-custom-annotation-add midashi candidate annotation)
      annotation)))

;;; ファイルI/O

;;;###autoload
(defun nskk-custom-annotation-save (&optional file)
  "カスタム注釈を FILE に保存する。
FILE が指定されない場合は `nskk-custom-annotation-file' を使用する。"
  (interactive)
  (let ((file (or file nskk-custom-annotation-file)))
    ;; ディレクトリを作成
    (let ((dir (file-name-directory file)))
      (unless (file-directory-p dir)
        (make-directory dir t)))

    ;; ファイルに書き込み
    (with-temp-buffer
      (insert ";; -*- mode: emacs-lisp; coding: utf-8 -*-\n")
      (insert ";; NSKK Custom Annotations\n")
      (insert (format ";; Saved: %s\n\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S")))

      (insert "(\n")
      (maphash (lambda (key value)
                (let ((midashi (car key))
                      (candidate (cdr key)))
                  (insert (format "  ((%S . %S) . %S)\n"
                                 midashi candidate value))))
              nskk-custom-annotation--table)
      (insert ")\n")

      (write-region (point-min) (point-max) file nil 'silent))

    (setq nskk-custom-annotation--modified nil)
    (message "Saved %d annotations to %s"
             (hash-table-count nskk-custom-annotation--table)
             file)))

;;;###autoload
(defun nskk-custom-annotation-load (&optional file)
  "FILE からカスタム注釈を読み込む。
FILE が指定されない場合は `nskk-custom-annotation-file' を使用する。"
  (interactive)
  (let ((file (or file nskk-custom-annotation-file)))
    (when (file-exists-p file)
      (let ((data (with-temp-buffer
                   (insert-file-contents file)
                   (goto-char (point-min))
                   ;; コメント行をスキップ
                   (while (looking-at "^;;")
                     (forward-line 1))
                   (read (current-buffer)))))

        ;; ハッシュテーブルをクリア
        (clrhash nskk-custom-annotation--table)

        ;; データを読み込み
        (dolist (entry data)
          (let* ((key (car entry))
                 (value (cdr entry))
                 (midashi (car key))
                 (candidate (cdr key)))
            (puthash key value nskk-custom-annotation--table)))

        (setq nskk-custom-annotation--modified nil)
        (message "Loaded %d annotations from %s"
                 (hash-table-count nskk-custom-annotation--table)
                 file)))))

;;;###autoload
(defun nskk-custom-annotation-save-if-modified ()
  "変更がある場合のみカスタム注釈を保存する。"
  (when nskk-custom-annotation--modified
    (nskk-custom-annotation-save)))

;;; 一括操作

;;;###autoload
(defun nskk-custom-annotation-clear ()
  "全てのカスタム注釈を削除する。"
  (interactive)
  (when (yes-or-no-p "本当に全ての注釈を削除しますか? ")
    (clrhash nskk-custom-annotation--table)
    (setq nskk-custom-annotation--modified t)
    (message "Cleared all custom annotations")))

;;;###autoload
(defun nskk-custom-annotation-export (file)
  "カスタム注釈を FILE にエクスポートする（SKK辞書形式）。"
  (interactive "FExport to: ")
  (with-temp-buffer
    (insert ";; -*- coding: utf-8 -*-\n")
    (insert ";; NSKK Custom Annotations (SKK format)\n\n")

    ;; 見出し語ごとにグループ化
    (let ((grouped (make-hash-table :test 'equal)))
      (maphash (lambda (key value)
                (let ((midashi (car key))
                      (candidate (cdr key)))
                  (push (cons candidate value)
                        (gethash midashi grouped))))
              nskk-custom-annotation--table)

      ;; SKK形式で出力
      (maphash (lambda (midashi candidates)
                (insert midashi " /")
                (dolist (cand (nreverse candidates))
                  (insert (car cand))
                  (when (cdr cand)
                    (insert ";" (cdr cand)))
                  (insert "/"))
                (insert "\n"))
              grouped))

    (write-region (point-min) (point-max) file))
  (message "Exported annotations to %s" file))

;;; 統計・デバッグ

;;;###autoload
(defun nskk-custom-annotation-statistics ()
  "カスタム注釈の統計情報を返す。"
  (let ((total (hash-table-count nskk-custom-annotation--table))
        (midashi-count (make-hash-table :test 'equal))
        (avg-length 0))

    ;; 見出し語数をカウント
    (maphash (lambda (key _value)
              (let ((midashi (car key)))
                (puthash midashi t midashi-count)))
            nskk-custom-annotation--table)

    ;; 平均注釈長を計算
    (when (> total 0)
      (let ((total-length 0))
        (maphash (lambda (_key value)
                  (setq total-length (+ total-length (length value))))
                nskk-custom-annotation--table)
        (setq avg-length (/ total-length total))))

    (list :total total
          :unique-midashi (hash-table-count midashi-count)
          :avg-length avg-length
          :modified nskk-custom-annotation--modified)))

;;;###autoload
(defun nskk-custom-annotation-describe ()
  "カスタム注釈の統計情報を表示する。"
  (interactive)
  (let ((stats (nskk-custom-annotation-statistics)))
    (message "NSKK Custom Annotations:
  Total annotations: %d
  Unique midashi: %d
  Average length: %d
  Modified: %s
  File: %s"
             (plist-get stats :total)
             (plist-get stats :unique-midashi)
             (plist-get stats :avg-length)
             (if (plist-get stats :modified) "yes" "no")
             nskk-custom-annotation-file)))

;;; 初期化

;;;###autoload
(defun nskk-custom-annotation-init ()
  "カスタム注釈システムを初期化する。"
  (when (file-exists-p nskk-custom-annotation-file)
    (nskk-custom-annotation-load)))

;;; クリーンアップ

(defun nskk-custom-annotation-cleanup ()
  "カスタム注釈システムをクリーンアップする。"
  (nskk-custom-annotation-save-if-modified))

;; Emacsを終了する時に自動保存
(add-hook 'kill-emacs-hook #'nskk-custom-annotation-cleanup)

(provide 'nskk-custom-annotation)

;;; nskk-custom-annotation.el ends here
