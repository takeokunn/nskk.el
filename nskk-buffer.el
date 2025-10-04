;;; nskk-buffer.el --- Buffer management for NSKK -*- lexical-binding: t; -*-

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

;; このファイルはNSKKのバッファ管理機能を実装します。
;;
;; 特徴:
;; - 入力バッファの管理（挿入・削除・クリア）
;; - アンドゥ・リドゥ機能（履歴管理）
;; - マーカーベースのバッファ位置管理
;; - オーバーレイによる視覚的フィードバック
;;
;; 使用例:
;; (nskk-buffer-insert "あ")
;; (nskk-buffer-delete-backward-char)
;; (nskk-buffer-undo)

;;; Code:

(require 'cl-lib)
(require 'nskk-state)

;;; カスタマイズ変数

(defgroup nskk-buffer nil
  "NSKK buffer management settings."
  :group 'nskk
  :prefix "nskk-buffer-")

(defcustom nskk-buffer-undo-limit 100
  "アンドゥ履歴の最大保持数。"
  :type 'integer
  :group 'nskk-buffer)

(defcustom nskk-buffer-show-pending-input t
  "非nilの場合、未確定入力をバッファに表示する。"
  :type 'boolean
  :group 'nskk-buffer)

(defcustom nskk-buffer-pending-face 'underline
  "未確定入力の表示に使用するface。"
  :type 'face
  :group 'nskk-buffer)

(defcustom nskk-buffer-conversion-face 'highlight
  "変換中テキストの表示に使用するface。"
  :type 'face
  :group 'nskk-buffer)

;;; 内部変数

(defvar-local nskk-buffer--overlay nil
  "未確定入力表示用のオーバーレイ。")

(defvar-local nskk-buffer--undo-stack nil
  "アンドゥ用の履歴スタック。
各要素は (operation . data) の形式。")

(defvar-local nskk-buffer--redo-stack nil
  "リドゥ用の履歴スタック。")

(defvar-local nskk-buffer--in-undo-redo nil
  "アンドゥ・リドゥ実行中かどうかのフラグ。
履歴保存をスキップするために使用。")

;;; アンドゥ履歴管理

(cl-defstruct (nskk-buffer-history-entry
               (:constructor nskk-buffer-history-entry-create)
               (:copier nil))
  "バッファ操作履歴のエントリ。

スロット:
  operation  - 操作種別（'insert, 'delete, 'clear 等）
  data       - 操作に関連するデータ
  state      - 操作時の状態スナップショット"
  (operation nil :type symbol :read-only t)
  (data nil :read-only t)
  (state nil :read-only t))

(defun nskk-buffer--save-history (operation data)
  "履歴スタックに OPERATION と DATA を保存する。"
  (unless nskk-buffer--in-undo-redo
    (when (< (length nskk-buffer--undo-stack) nskk-buffer-undo-limit)
      (let ((entry (nskk-buffer-history-entry-create
                    :operation operation
                    :data data
                    :state (when nskk-current-state
                            (nskk-state-copy nskk-current-state)))))
        (push entry nskk-buffer--undo-stack)
        ;; リドゥスタックをクリア（新しい操作が行われたため）
        (setq nskk-buffer--redo-stack nil)))))

(defun nskk-buffer--clear-history ()
  "アンドゥ・リドゥ履歴をクリアする。"
  (setq nskk-buffer--undo-stack nil)
  (setq nskk-buffer--redo-stack nil))

;;; オーバーレイ管理

(defun nskk-buffer--ensure-overlay ()
  "オーバーレイが存在することを確認し、なければ作成する。"
  (unless (and nskk-buffer--overlay
               (overlay-buffer nskk-buffer--overlay))
    (setq nskk-buffer--overlay (make-overlay (point) (point)))
    (overlay-put nskk-buffer--overlay 'face nskk-buffer-pending-face)
    (overlay-put nskk-buffer--overlay 'nskk-overlay t)))

(defun nskk-buffer--delete-overlay ()
  "オーバーレイを削除する。"
  (when (and nskk-buffer--overlay
             (overlay-buffer nskk-buffer--overlay))
    (delete-overlay nskk-buffer--overlay)
    (setq nskk-buffer--overlay nil)))

(defun nskk-buffer--update-overlay (start end face)
  "オーバーレイの位置と表示を更新する。"
  (nskk-buffer--ensure-overlay)
  (move-overlay nskk-buffer--overlay start end)
  (overlay-put nskk-buffer--overlay 'face face))

;;; バッファ挿入・削除

(defun nskk-buffer-insert (string)
  "STRING を現在位置に挿入する。

引数:
  STRING - 挿入する文字列

副作用:
  - バッファへの文字列挿入
  - マーカー位置の更新
  - アンドゥ履歴への記録"
  (unless nskk-current-state
    (nskk-state-init))

  (let ((start (point)))
    ;; バッファに挿入
    (insert string)

    ;; マーカー更新
    (unless (nskk-state-marker-start nskk-current-state)
      (nskk-state-set-markers nskk-current-state start (point)))
    (when (nskk-state-marker-end nskk-current-state)
      (set-marker (nskk-state-marker-end nskk-current-state) (point)))

    ;; オーバーレイ更新
    (when nskk-buffer-show-pending-input
      (nskk-buffer--update-overlay
       start (point)
       (if (nskk-state-in-conversion-p nskk-current-state)
           nskk-buffer-conversion-face
         nskk-buffer-pending-face)))

    ;; 履歴保存
    (nskk-buffer--save-history 'insert string)))

(defun nskk-buffer-delete-region (start end)
  "START から END までの領域を削除する。

引数:
  START - 削除開始位置
  END   - 削除終了位置

副作用:
  - バッファからのテキスト削除
  - アンドゥ履歴への記録"
  (let ((deleted-text (buffer-substring-no-properties start end)))
    (delete-region start end)

    ;; 履歴保存
    (nskk-buffer--save-history 'delete
                               (list :text deleted-text
                                     :start start
                                     :end end))))

(defun nskk-buffer-delete-backward-char (&optional n)
  "カーソルの前の N 文字を削除する。

引数:
  N - 削除する文字数（デフォルト: 1）

返り値:
  削除に成功した場合は t、失敗した場合は nil"
  (interactive "p")
  (setq n (or n 1))

  (when (and nskk-current-state
             (nskk-state-marker-start nskk-current-state))
    (let ((start (marker-position (nskk-state-marker-start nskk-current-state)))
          (current (point)))
      (when (>= (- current start) n)
        (nskk-buffer-delete-region (- current n) current)

        ;; オーバーレイ更新
        (when nskk-buffer-show-pending-input
          (if (= (point) start)
              (nskk-buffer--delete-overlay)
            (nskk-buffer--update-overlay
             start (point)
             (if (nskk-state-in-conversion-p nskk-current-state)
                 nskk-buffer-conversion-face
               nskk-buffer-pending-face))))
        t))))

(defun nskk-buffer-delete-forward-char (&optional n)
  "カーソルの後の N 文字を削除する。

引数:
  N - 削除する文字数（デフォルト: 1）

返り値:
  削除に成功した場合は t、失敗した場合は nil"
  (interactive "p")
  (setq n (or n 1))

  (when (and nskk-current-state
             (nskk-state-marker-end nskk-current-state))
    (let ((end (marker-position (nskk-state-marker-end nskk-current-state)))
          (current (point)))
      (when (<= (+ current n) end)
        (nskk-buffer-delete-region current (+ current n))
        t))))

;;; バッファクリア

(defun nskk-buffer-clear ()
  "未確定入力を全てクリアする。

副作用:
  - バッファから未確定テキストを削除
  - マーカーのクリア
  - オーバーレイの削除
  - 状態のクリア"
  (interactive)

  (when (and nskk-current-state
             (nskk-state-marker-start nskk-current-state)
             (nskk-state-marker-end nskk-current-state))
    (let ((start (marker-position (nskk-state-marker-start nskk-current-state)))
          (end (marker-position (nskk-state-marker-end nskk-current-state))))

      ;; バッファから削除
      (when (< start end)
        (nskk-buffer-delete-region start end))

      ;; 状態クリア
      (nskk-state-clear-all nskk-current-state)
      (nskk-state-clear-markers nskk-current-state)

      ;; オーバーレイ削除
      (nskk-buffer--delete-overlay)

      ;; 履歴保存
      (nskk-buffer--save-history 'clear nil))))

;;; 確定処理

(defun nskk-buffer-commit (text)
  "TEXT を確定してバッファに挿入する。

引数:
  TEXT - 確定するテキスト

副作用:
  - 未確定入力の削除
  - 確定テキストの挿入
  - 状態のクリア"
  (when (and nskk-current-state
             (nskk-state-marker-start nskk-current-state))
    (let ((start (marker-position (nskk-state-marker-start nskk-current-state))))

      ;; 未確定入力をクリア
      (nskk-buffer-clear)

      ;; 確定テキストを挿入（オーバーレイなし）
      (goto-char start)
      (insert text)

      ;; 履歴保存
      (nskk-buffer--save-history 'commit text))))

;;; アンドゥ・リドゥ

(defun nskk-buffer-undo ()
  "最後のバッファ操作をアンドゥする。

返り値:
  アンドゥに成功した場合は t、履歴がない場合は nil"
  (interactive)

  (if-let ((entry (pop nskk-buffer--undo-stack)))
      (progn
        ;; リドゥ用に保存
        (push entry nskk-buffer--redo-stack)

        ;; 操作に応じてアンドゥ処理
        (let ((operation (nskk-buffer-history-entry-operation entry))
              (data (nskk-buffer-history-entry-data entry))
              (saved-state (nskk-buffer-history-entry-state entry)))

          (pcase operation
            ('insert
             ;; 挿入のアンドゥ: 削除
             (when (and nskk-current-state
                        (nskk-state-marker-end nskk-current-state))
               (let ((end (marker-position (nskk-state-marker-end nskk-current-state)))
                     (len (length data)))
                 (delete-region (- end len) end))))

            ('delete
             ;; 削除のアンドゥ: 挿入
             (let ((text (plist-get data :text))
                   (start (plist-get data :start)))
               (goto-char start)
               (insert text)))

            ('clear
             ;; クリアのアンドゥ: 状態復元
             (when saved-state
               (setq nskk-current-state (nskk-state-copy saved-state))))

            ('commit
             ;; 確定のアンドゥ: テキスト削除
             (let ((len (length data)))
               (delete-region (- (point) len) (point))))))

        t)
    nil))

(defun nskk-buffer-redo ()
  "アンドゥした操作をリドゥする。

返り値:
  リドゥに成功した場合は t、履歴がない場合は nil"
  (interactive)

  (if-let ((entry (pop nskk-buffer--redo-stack)))
      (let ((nskk-buffer--in-undo-redo t))
        ;; アンドゥ用に保存
        (push entry nskk-buffer--undo-stack)

        ;; 操作に応じてリドゥ処理
        (let ((operation (nskk-buffer-history-entry-operation entry))
              (data (nskk-buffer-history-entry-data entry)))

          (pcase operation
            ('insert
             ;; 挿入のリドゥ
             (nskk-buffer-insert data))

            ('delete
             ;; 削除のリドゥ
             (let ((start (plist-get data :start))
                   (end (plist-get data :end)))
               (nskk-buffer-delete-region start end)))

            ('clear
             ;; クリアのリドゥ
             (nskk-buffer-clear))

            ('commit
             ;; 確定のリドゥ
             (nskk-buffer-commit data))))

        t)
    nil))

;;; バッファ状態問い合わせ

(defun nskk-buffer-has-pending-input-p ()
  "未確定入力が存在するかどうかを判定する。"
  (and nskk-current-state
       (nskk-state-marker-start nskk-current-state)
       (nskk-state-marker-end nskk-current-state)
       (< (marker-position (nskk-state-marker-start nskk-current-state))
          (marker-position (nskk-state-marker-end nskk-current-state)))))

(defun nskk-buffer-pending-text ()
  "未確定入力のテキストを取得する。
未確定入力がない場合は nil を返す。"
  (when (nskk-buffer-has-pending-input-p)
    (buffer-substring-no-properties
     (marker-position (nskk-state-marker-start nskk-current-state))
     (marker-position (nskk-state-marker-end nskk-current-state)))))

(defun nskk-buffer-pending-length ()
  "未確定入力の文字数を取得する。"
  (if-let ((text (nskk-buffer-pending-text)))
      (length text)
    0))

;;; クリーンアップ

(defun nskk-buffer-cleanup ()
  "バッファ管理に関連するリソースをクリーンアップする。"
  (nskk-buffer--delete-overlay)
  (nskk-buffer--clear-history)
  (when nskk-current-state
    (nskk-state-clear-markers nskk-current-state)))

;;; 統計・デバッグ

(defun nskk-buffer-stats ()
  "バッファ管理の統計情報を返す。

返り値は以下の要素を含むplist:
  :has-pending     - 未確定入力の有無
  :pending-length  - 未確定入力の文字数
  :undo-stack-size - アンドゥ履歴の数
  :redo-stack-size - リドゥ履歴の数
  :overlay-active  - オーバーレイの有無"
  (list :has-pending (nskk-buffer-has-pending-input-p)
        :pending-length (nskk-buffer-pending-length)
        :undo-stack-size (length nskk-buffer--undo-stack)
        :redo-stack-size (length nskk-buffer--redo-stack)
        :overlay-active (and nskk-buffer--overlay
                             (overlay-buffer nskk-buffer--overlay))))

(defun nskk-buffer-describe ()
  "バッファの現在状態を表示する。"
  (interactive)
  (let ((stats (nskk-buffer-stats)))
    (message "NSKK Buffer:
  Pending input: %s
  Pending length: %d
  Undo stack: %d
  Redo stack: %d
  Overlay: %s"
             (if (plist-get stats :has-pending)
                 (format "\"%s\"" (nskk-buffer-pending-text))
               "none")
             (plist-get stats :pending-length)
             (plist-get stats :undo-stack-size)
             (plist-get stats :redo-stack-size)
             (if (plist-get stats :overlay-active) "active" "inactive"))))

(provide 'nskk-buffer)

;;; nskk-buffer.el ends here
