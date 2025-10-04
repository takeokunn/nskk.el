;;; nskk-events.el --- Event system for NSKK -*- lexical-binding: t; -*-

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

;; このファイルはNSKKのイベントシステムを実装します。
;;
;; 特徴:
;; - イベント駆動アーキテクチャ
;; - グローバル・バッファローカルイベントリスナー
;; - 型安全なイベントデータ構造
;; - エラーハンドリング機構
;; - デバッグ・ロギング機能
;;
;; イベントタイプ:
;; - :state-changed        - 状態変更イベント
;; - :mode-switched        - モード切り替えイベント
;; - :buffer-modified      - バッファ変更イベント
;; - :conversion-started   - 変換開始イベント
;; - :conversion-committed - 変換確定イベント
;; - :input-received       - 入力受信イベント
;; - :error                - エラーイベント
;;
;; 使用例:
;; (nskk-events-add-listener :mode-switched #'my-handler)
;; (nskk-events-emit :mode-switched :from 'hiragana :to 'katakana)
;; (nskk-events-remove-listener :mode-switched #'my-handler)

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-events nil
  "NSKK event system customization."
  :group 'nskk
  :prefix "nskk-events-")

(defcustom nskk-events-enable-logging nil
  "非nilの場合、イベントのロギングを有効化する。
デバッグ用途に使用する。"
  :type 'boolean
  :group 'nskk-events)

(defcustom nskk-events-log-buffer-name "*NSKK Events Log*"
  "イベントログを出力するバッファ名。"
  :type 'string
  :group 'nskk-events)

(defcustom nskk-events-max-log-entries 1000
  "保持する最大ログエントリ数。
この数を超えた場合、古いエントリから削除される。"
  :type 'integer
  :group 'nskk-events)

(defcustom nskk-events-error-handler nil
  "イベントハンドラー内でエラーが発生した場合の処理関数。
nilの場合、エラーメッセージを表示するのみ。
関数の場合、(funcall handler event-type error-data)として呼び出される。"
  :type '(choice (const :tag "Default (message only)" nil)
                 (function :tag "Custom error handler"))
  :group 'nskk-events)

;;; イベントタイプ定義

(defconst nskk-events-types
  '(:state-changed
    :mode-switched
    :buffer-modified
    :conversion-started
    :conversion-committed
    :conversion-canceled
    :candidate-selected
    :input-received
    :error)
  "NSKKで使用可能な全てのイベントタイプ。")

;;; 内部変数

(defvar-local nskk-events--listeners (make-hash-table :test 'eq)
  "バッファローカルなイベントリスナーテーブル。
キー: イベントタイプ（キーワード）
値: リスナー関数のリスト")

(defvar nskk-events--global-listeners (make-hash-table :test 'eq)
  "グローバルなイベントリスナーテーブル。
キー: イベントタイプ（キーワード）
値: リスナー関数のリスト")

(defvar nskk-events--log-entries nil
  "イベントログエントリのリスト。
各エントリは (timestamp event-type data) の形式。")

(defvar nskk-events--emitting nil
  "イベント発行中かどうかのフラグ。
再帰的なイベント発行を検出するために使用。")

;;; イベントデータ構造

(cl-defstruct (nskk-event
               (:constructor nskk-event-create)
               (:copier nil))
  "イベントデータを表す構造体。

スロット:
  type       - イベントタイプ（`nskk-events-types' のいずれか）
  timestamp  - イベント発生時刻
  buffer     - イベントが発生したバッファ
  data       - イベントデータ（plist形式）"
  (type nil :type keyword)
  (timestamp (current-time) :type list)
  (buffer (current-buffer) :type buffer)
  (data nil :type list))

;;; リスナー管理

(defun nskk-events-add-listener (event-type listener &optional global)
  "EVENT-TYPE に対して LISTENER を登録する。

GLOBAL が非nilの場合、グローバルリスナーとして登録。
nilの場合、現在のバッファのローカルリスナーとして登録。

LISTENER は (lambda (event) ...) 形式の関数で、
EVENT は `nskk-event' 構造体。

戻り値: t（成功）、nil（失敗）"
  (unless (memq event-type nskk-events-types)
    (error "Unknown event type: %s" event-type))
  (unless (functionp listener)
    (error "Listener must be a function: %s" listener))

  (let ((table (if global
                   nskk-events--global-listeners
                 ;; バッファローカル変数を確実に初期化
                 (progn
                   (unless (hash-table-p nskk-events--listeners)
                     (setq-local nskk-events--listeners (make-hash-table :test 'eq)))
                   nskk-events--listeners))))
    (let ((listeners (gethash event-type table)))
      (unless (memq listener listeners)
        (puthash event-type
                 (append listeners (list listener))
                 table)))
    t))

(defun nskk-events-remove-listener (event-type listener &optional global)
  "EVENT-TYPE から LISTENER を削除する。

GLOBAL が非nilの場合、グローバルリスナーから削除。
nilの場合、現在のバッファのローカルリスナーから削除。

戻り値: t（削除成功）、nil（リスナーが見つからない）"
  (unless (memq event-type nskk-events-types)
    (error "Unknown event type: %s" event-type))

  (let ((table (if global
                   nskk-events--global-listeners
                 (when (hash-table-p nskk-events--listeners)
                   nskk-events--listeners))))
    (when table
      (let ((listeners (gethash event-type table)))
        (when (memq listener listeners)
          (puthash event-type
                   (delq listener listeners)
                   table)
          t)))))

(defun nskk-events-clear-listeners (&optional event-type global)
  "リスナーをクリアする。

EVENT-TYPE が指定された場合、そのイベントタイプのリスナーのみクリア。
nilの場合、全てのリスナーをクリア。

GLOBAL が非nilの場合、グローバルリスナーをクリア。
nilの場合、現在のバッファのローカルリスナーをクリア。"
  (let ((table (if global
                   nskk-events--global-listeners
                 (when (hash-table-p nskk-events--listeners)
                   nskk-events--listeners))))
    (when table
      (if event-type
          (puthash event-type nil table)
        (clrhash table)))))

(defun nskk-events-get-listeners (event-type &optional include-global)
  "EVENT-TYPE のリスナーリストを取得する。

INCLUDE-GLOBAL が非nilの場合、グローバルリスナーも含める。

戻り値: リスナー関数のリスト"
  (let ((local-listeners
         (when (hash-table-p nskk-events--listeners)
           (gethash event-type nskk-events--listeners)))
        (global-listeners (when include-global
                           (gethash event-type nskk-events--global-listeners))))
    (append local-listeners global-listeners)))

;;; イベント発行

(defun nskk-events-emit (event-type &rest data)
  "EVENT-TYPE イベントを発行する。

DATA はイベントデータで、plist形式で指定する。
例: (nskk-events-emit :mode-switched :from 'hiragana :to 'katakana)

戻り値: 実行されたリスナー数"
  (unless (memq event-type nskk-events-types)
    (error "Unknown event type: %s" event-type))

  ;; 再帰的なイベント発行を検出
  (when nskk-events--emitting
    (nskk-events--log event-type
                      (append data (list :recursive t))
                      'warning)
    (when nskk-events-enable-logging
      (message "Warning: Recursive event emission detected for %s" event-type)))

  (let ((nskk-events--emitting t)
        (event (nskk-event-create
                :type event-type
                :timestamp (current-time)
                :buffer (current-buffer)
                :data data))
        (count 0))

    (nskk-events--log event-type data)

    ;; ローカルリスナーとグローバルリスナーを取得
    (let ((listeners (nskk-events-get-listeners event-type t)))
      (dolist (listener listeners)
        (condition-case err
            (progn
              (funcall listener event)
              (setq count (1+ count)))
          (error
           (nskk-events--handle-error event-type listener err)))))

    count))

;;; エラーハンドリング

(defun nskk-events--handle-error (event-type listener error-data)
  "イベントハンドラー内のエラーを処理する。

EVENT-TYPE: エラーが発生したイベントタイプ
LISTENER: エラーが発生したリスナー関数
ERROR-DATA: エラーデータ"
  (let ((error-info (list :event-type event-type
                         :listener listener
                         :error error-data)))
    (nskk-events--log :error error-info 'error)

    (if nskk-events-error-handler
        (condition-case err
            (funcall nskk-events-error-handler event-type error-info)
          (error
           (message "Error in error handler: %s" err)))
      (message "NSKK event handler error [%s]: %s"
               event-type (error-message-string error-data)))))

;;; ロギング

(defun nskk-events--log (event-type data &optional level)
  "イベントをログに記録する。

EVENT-TYPE: イベントタイプ
DATA: イベントデータ
LEVEL: ログレベル（'info, 'warning, 'error）デフォルトは'info"
  (when nskk-events-enable-logging
    (let ((entry (list :timestamp (current-time)
                      :level (or level 'info)
                      :event-type event-type
                      :data data)))
      (push entry nskk-events--log-entries)

      ;; 最大エントリ数を超えた場合、古いエントリを削除
      (when (> (length nskk-events--log-entries)
               nskk-events-max-log-entries)
        (setq nskk-events--log-entries
              (seq-take nskk-events--log-entries
                       nskk-events-max-log-entries)))

      ;; ログバッファに出力
      (nskk-events--write-log-buffer entry))))

(defun nskk-events--write-log-buffer (entry)
  "ログエントリをログバッファに書き込む。

ENTRY: ログエントリ（plist形式）"
  (with-current-buffer (get-buffer-create nskk-events-log-buffer-name)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (format "[%s] [%s] %s: %s\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S"
                                       (plist-get entry :timestamp))
                     (upcase (symbol-name (plist-get entry :level)))
                     (plist-get entry :event-type)
                     (plist-get entry :data))))))

;;; ログビューア

(defun nskk-events-show-log ()
  "イベントログバッファを表示する。"
  (interactive)
  (let ((buffer (get-buffer-create nskk-events-log-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'nskk-events-log-mode)
        (nskk-events-log-mode)))
    (switch-to-buffer-other-window buffer)))

(defun nskk-events-clear-log ()
  "イベントログをクリアする。"
  (interactive)
  (setq nskk-events--log-entries nil)
  (when (get-buffer nskk-events-log-buffer-name)
    (with-current-buffer nskk-events-log-buffer-name
      (let ((inhibit-read-only t))
        (erase-buffer))))
  (message "NSKK event log cleared"))

(define-derived-mode nskk-events-log-mode special-mode "NSKK-Events-Log"
  "NSKKイベントログ表示用のメジャーモード。"
  (setq buffer-read-only t))

;;; 統計情報

(defun nskk-events-stats ()
  "イベントシステムの統計情報を返す。

戻り値: 統計情報のplist
  :local-listeners-count  - ローカルリスナー総数
  :global-listeners-count - グローバルリスナー総数
  :log-entries-count      - ログエントリ総数
  :event-types-count      - 登録されているイベントタイプ数"
  (let ((local-count 0)
        (global-count 0))
    (when (hash-table-p nskk-events--listeners)
      (maphash (lambda (_type listeners)
                (setq local-count (+ local-count (length listeners))))
              nskk-events--listeners))
    (maphash (lambda (_type listeners)
              (setq global-count (+ global-count (length listeners))))
            nskk-events--global-listeners)

    (list :local-listeners-count local-count
          :global-listeners-count global-count
          :log-entries-count (length nskk-events--log-entries)
          :event-types-count (length nskk-events-types))))

;;; ヘルパー関数

(defun nskk-events-has-listener-p (event-type &optional global)
  "EVENT-TYPE に対するリスナーが存在するか確認する。

GLOBAL が非nilの場合、グローバルリスナーを確認。
nilの場合、ローカルリスナーを確認。

戻り値: リスナーが存在する場合t、存在しない場合nil"
  (let ((table (if global
                   nskk-events--global-listeners
                 (when (hash-table-p nskk-events--listeners)
                   nskk-events--listeners))))
    (when table
      (let ((listeners (gethash event-type table)))
        (and listeners (> (length listeners) 0))))))

;;; クリーンアップ

(defun nskk-events-cleanup ()
  "イベントシステムをクリーンアップする。
全てのローカルリスナーとログをクリア。"
  (nskk-events-clear-listeners nil nil)  ; ローカルリスナークリア
  (when nskk-events-enable-logging
    (setq nskk-events--log-entries nil)))

(provide 'nskk-events)

;;; nskk-events.el ends here
