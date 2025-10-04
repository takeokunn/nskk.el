;;; nskk-server-async.el --- Async SKK server communication for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, server, async
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

;; このファイルはSKK辞書サーバーとの非同期通信機能を実装します。
;;
;; 特徴:
;; - make-network-processを使用した非同期通信
;; - コールバックベースのAPI
;; - タイムアウト処理
;; - 並列リクエスト対応
;; - コネクションプーリング
;; - 自動再接続
;;
;; パフォーマンス目標:
;; - リクエスト応答時間: < 100ms（ローカルサーバー）
;; - 並列リクエスト: 10件同時処理可能
;; - タイムアウト: デフォルト3秒
;;
;; 使用例:
;;
;;   (require 'nskk-server-async)
;;
;;   ;; 非同期検索
;;   (nskk-server-async-search
;;     "localhost" 1178 "かんじ" 'okuri-nasi
;;     (lambda (response)
;;       (message "Found: %S"
;;         (nskk-server-response-candidates response)))
;;     (lambda (error)
;;       (message "Error: %s" error)))

;;; Code:

(require 'cl-lib)
(require 'nskk-server-protocol)

;;; カスタマイズ変数

(defgroup nskk-server-async nil
  "Async SKK server communication customization."
  :group 'nskk
  :prefix "nskk-server-async-")

(defcustom nskk-server-async-timeout 3.0
  "サーバーリクエストのタイムアウト時間（秒）。"
  :type 'float
  :group 'nskk-server-async)

(defcustom nskk-server-async-max-connections 5
  "最大同時接続数。"
  :type 'integer
  :group 'nskk-server-async)

(defcustom nskk-server-async-connection-timeout 5.0
  "接続タイムアウト時間（秒）。"
  :type 'float
  :group 'nskk-server-async)

(defcustom nskk-server-async-keepalive t
  "非nilの場合、接続を維持する（コネクションプーリング）。"
  :type 'boolean
  :group 'nskk-server-async)

(defcustom nskk-server-async-verbose nil
  "非nilの場合、詳細なログを出力する。"
  :type 'boolean
  :group 'nskk-server-async)

;;; エラー定義

(define-error 'nskk-server-async-error
  "SKK server async error")

(define-error 'nskk-server-async-timeout
  "SKK server request timeout"
  'nskk-server-async-error)

(define-error 'nskk-server-async-connection-failed
  "SKK server connection failed"
  'nskk-server-async-error)

(define-error 'nskk-server-async-connection-lost
  "SKK server connection lost"
  'nskk-server-async-error)

;;; データ構造

(cl-defstruct (nskk-server-connection
               (:constructor nskk-server-connection--create)
               (:copier nil))
  "SKKサーバー接続。

スロット:
  host         - ホスト名
  port         - ポート番号
  process      - ネットワークプロセス
  state        - 接続状態（'connecting/'connected/'closed）
  encoding     - エンコーディング
  buffer       - 受信バッファ（文字列）
  pending      - 保留中のリクエストキュー
  created-time - 接続作成時刻
  last-used    - 最終使用時刻"
  (host nil :type (or null string))
  (port nil :type (or null integer))
  (process nil :type (or null process))
  (state 'closed :type symbol)
  (encoding 'euc-jp :type symbol)
  (buffer "" :type string)
  (pending nil :type list)
  (created-time nil :type (or null number))
  (last-used nil :type (or null number)))

(cl-defstruct (nskk-server-request-context
               (:constructor nskk-server-request-context--create)
               (:copier nil))
  "リクエストコンテキスト。

スロット:
  request      - リクエスト文字列
  callback     - 成功コールバック（response -> void）
  error-callback - エラーコールバック（error-message -> void）
  timeout      - タイムアウト時刻（float-time）
  timer        - タイムアウトタイマー"
  (request nil :type (or null string))
  (callback nil :type (or null function))
  (error-callback nil :type (or null function))
  (timeout nil :type (or null number))
  (timer nil :type (or null timer)))

;;; コネクションプール

(defvar nskk-server-async--connection-pool (make-hash-table :test 'equal)
  "コネクションプール（ホスト:ポート → コネクション）")

(defun nskk-server-async--pool-key (host port)
  "コネクションプールのキーを生成する。

引数:
  HOST - ホスト名
  PORT - ポート番号

戻り値:
  キー文字列（\"host:port\"形式）"
  (format "%s:%d" host port))

(defun nskk-server-async--get-connection (host port)
  "コネクションプールから接続を取得する。

引数:
  HOST - ホスト名
  PORT - ポート番号

戻り値:
  nskk-server-connection構造体、または nil（接続がない場合）"
  (when nskk-server-async-keepalive
    (let* ((key (nskk-server-async--pool-key host port))
           (conn (gethash key nskk-server-async--connection-pool)))
      (when (and conn (nskk-server-async--connection-alive-p conn))
        (setf (nskk-server-connection-last-used conn) (float-time))
        conn))))

(defun nskk-server-async--put-connection (conn)
  "コネクションプールに接続を格納する。

引数:
  CONN - nskk-server-connection構造体"
  (when nskk-server-async-keepalive
    (let ((key (nskk-server-async--pool-key
                (nskk-server-connection-host conn)
                (nskk-server-connection-port conn))))
      (puthash key conn nskk-server-async--connection-pool))))

(defun nskk-server-async--remove-connection (conn)
  "コネクションプールから接続を削除する。

引数:
  CONN - nskk-server-connection構造体"
  (let ((key (nskk-server-async--pool-key
              (nskk-server-connection-host conn)
              (nskk-server-connection-port conn))))
    (remhash key nskk-server-async--connection-pool)))

(defun nskk-server-async--connection-alive-p (conn)
  "接続が有効か判定する。

引数:
  CONN - nskk-server-connection構造体

戻り値:
  有効な場合t、無効な場合nil"
  (and conn
       (eq (nskk-server-connection-state conn) 'connected)
       (nskk-server-connection-process conn)
       (process-live-p (nskk-server-connection-process conn))))

;;; 非同期検索

;;;###autoload
(defun nskk-server-async-search (host port key okuri-type
                                      success-callback error-callback
                                      &optional encoding timeout)
  "SKKサーバーに非同期検索リクエストを送信する。

引数:
  HOST             - サーバーホスト名
  PORT             - サーバーポート番号
  KEY              - 検索キー
  OKURI-TYPE       - 送り仮名タイプ（'okuri-ari/'okuri-nasi）
  SUCCESS-CALLBACK - 成功時のコールバック（nskk-server-response -> void）
  ERROR-CALLBACK   - エラー時のコールバック（error-message -> void）
  ENCODING         - エンコーディング（省略時はnskk-server-protocol-encoding）
  TIMEOUT          - タイムアウト時間（秒、省略時はnskk-server-async-timeout）

処理フロー:
  1. リクエスト文字列生成
  2. コネクション取得（プールまたは新規作成）
  3. リクエストキューに追加
  4. タイムアウトタイマー設定
  5. 送信

戻り値:
  なし（非同期処理）"
  (let* ((encoding (or encoding nskk-server-protocol-encoding))
         (timeout (or timeout nskk-server-async-timeout))
         (request-str (nskk-server-protocol-make-request key okuri-type encoding))
         (conn (or (nskk-server-async--get-connection host port)
                  (nskk-server-async--create-connection host port encoding))))

    (when nskk-server-async-verbose
      (message "Async search: %s@%s:%d" key host port))

    ;; リクエストコンテキスト作成
    (let* ((timeout-time (+ (float-time) timeout))
           (context (nskk-server-request-context--create
                    :request request-str
                    :callback success-callback
                    :error-callback error-callback
                    :timeout timeout-time)))

      ;; タイムアウトタイマー設定
      (setf (nskk-server-request-context-timer context)
            (run-at-time timeout nil
                        (lambda ()
                          (nskk-server-async--handle-timeout conn context))))

      ;; リクエストキューに追加
      (push context (nskk-server-connection-pending conn))

      ;; 接続済みの場合は即座に送信
      (when (eq (nskk-server-connection-state conn) 'connected)
        (nskk-server-async--send-next-request conn)))))

(defun nskk-server-async--create-connection (host port encoding)
  "新しいサーバー接続を作成する。

引数:
  HOST     - ホスト名
  PORT     - ポート番号
  ENCODING - エンコーディング

戻り値:
  nskk-server-connection構造体"
  (when nskk-server-async-verbose
    (message "Creating connection to %s:%d" host port))

  (let ((conn (nskk-server-connection--create
               :host host
               :port port
               :state 'connecting
               :encoding encoding
               :created-time (float-time)
               :last-used (float-time))))

    ;; ネットワークプロセス作成
    (condition-case err
        (let ((process (make-network-process
                       :name (format "nskk-server-%s:%d" host port)
                       :host host
                       :service port
                       :coding encoding
                       :filter (lambda (proc string)
                                (nskk-server-async--filter conn proc string))
                       :sentinel (lambda (proc event)
                                  (nskk-server-async--sentinel conn proc event))
                       :nowait t)))
          (setf (nskk-server-connection-process conn) process)
          (nskk-server-async--put-connection conn)
          conn)
      (error
       (when nskk-server-async-verbose
         (message "Connection failed: %s" (error-message-string err)))
       (signal 'nskk-server-async-connection-failed
               (list (error-message-string err)))))))

(defun nskk-server-async--filter (conn _proc string)
  "ネットワークプロセスのフィルタ関数。

引数:
  CONN   - nskk-server-connection構造体
  _PROC  - プロセス（未使用）
  STRING - 受信文字列"
  (when nskk-server-async-verbose
    (message "Received: %S" string))

  ;; バッファに追加
  (setf (nskk-server-connection-buffer conn)
        (concat (nskk-server-connection-buffer conn) string))

  ;; 改行を含む場合、レスポンスを処理
  (when (string-match-p "\n" (nskk-server-connection-buffer conn))
    (nskk-server-async--process-response conn)))

(defun nskk-server-async--sentinel (conn _proc event)
  "ネットワークプロセスのセンチネル関数。

引数:
  CONN  - nskk-server-connection構造体
  _PROC - プロセス（未使用）
  EVENT - イベント文字列"
  (when nskk-server-async-verbose
    (message "Connection event: %s" event))

  (cond
   ;; 接続成功
   ((string-prefix-p "open" event)
    (setf (nskk-server-connection-state conn) 'connected)
    (when nskk-server-async-verbose
      (message "Connection established: %s:%d"
               (nskk-server-connection-host conn)
               (nskk-server-connection-port conn)))
    ;; 保留中のリクエストを送信
    (nskk-server-async--send-next-request conn))

   ;; 接続失敗・切断
   (t
    (setf (nskk-server-connection-state conn) 'closed)
    (nskk-server-async--remove-connection conn)
    ;; 保留中のリクエストにエラー通知
    (dolist (context (nskk-server-connection-pending conn))
      (nskk-server-async--call-error-callback
       context
       (format "Connection lost: %s" event)))
    (setf (nskk-server-connection-pending conn) nil))))

(defun nskk-server-async--send-next-request (conn)
  "次のリクエストを送信する。

引数:
  CONN - nskk-server-connection構造体"
  (when (and (eq (nskk-server-connection-state conn) 'connected)
             (nskk-server-connection-pending conn))
    (let* ((contexts (reverse (nskk-server-connection-pending conn)))
           (context (car contexts)))
      (when context
        (let ((request (nskk-server-request-context-request context)))
          (when nskk-server-async-verbose
            (message "Sending request: %S" request))
          (process-send-string (nskk-server-connection-process conn) request))))))

(defun nskk-server-async--process-response (conn)
  "受信したレスポンスを処理する。

引数:
  CONN - nskk-server-connection構造体"
  (let* ((buffer (nskk-server-connection-buffer conn))
         (lines (split-string buffer "\n")))

    ;; 改行で区切られた各レスポンスを処理
    (dolist (line lines)
      (when (> (length line) 0)
        (nskk-server-async--handle-response conn line)))

    ;; バッファをクリア（未完了データを保持）
    (setf (nskk-server-connection-buffer conn)
          (if (string-suffix-p "\n" buffer)
              ""
            (car (last lines))))))

(defun nskk-server-async--handle-response (conn response-str)
  "レスポンスを処理してコールバックを呼び出す。

引数:
  CONN         - nskk-server-connection構造体
  RESPONSE-STR - レスポンス文字列"
  (let* ((contexts (reverse (nskk-server-connection-pending conn)))
         (context (car contexts)))

    (when context
      ;; タイムアウトタイマーキャンセル
      (when (nskk-server-request-context-timer context)
        (cancel-timer (nskk-server-request-context-timer context)))

      ;; コンテキストをキューから削除
      (setf (nskk-server-connection-pending conn) (cdr contexts))

      ;; レスポンスパース
      (condition-case err
          (let ((response (nskk-server-protocol-parse-response
                          response-str
                          (nskk-server-connection-encoding conn))))
            (nskk-server-async--call-success-callback context response))
        (error
         (nskk-server-async--call-error-callback
          context
          (format "Parse error: %s" (error-message-string err))))))

    ;; 次のリクエストを送信
    (nskk-server-async--send-next-request conn)))

(defun nskk-server-async--handle-timeout (conn context)
  "タイムアウトを処理する。

引数:
  CONN    - nskk-server-connection構造体
  CONTEXT - nskk-server-request-context構造体"
  (when nskk-server-async-verbose
    (message "Request timeout: %s:%d"
             (nskk-server-connection-host conn)
             (nskk-server-connection-port conn)))

  ;; コンテキストをキューから削除
  (setf (nskk-server-connection-pending conn)
        (delq context (nskk-server-connection-pending conn)))

  ;; エラーコールバック呼び出し
  (nskk-server-async--call-error-callback context "Request timeout"))

(defun nskk-server-async--call-success-callback (context response)
  "成功コールバックを呼び出す。

引数:
  CONTEXT  - nskk-server-request-context構造体
  RESPONSE - nskk-server-response構造体"
  (when (nskk-server-request-context-callback context)
    (funcall (nskk-server-request-context-callback context) response)))

(defun nskk-server-async--call-error-callback (context error-message)
  "エラーコールバックを呼び出す。

引数:
  CONTEXT       - nskk-server-request-context構造体
  ERROR-MESSAGE - エラーメッセージ文字列"
  (when (nskk-server-request-context-error-callback context)
    (funcall (nskk-server-request-context-error-callback context) error-message)))

;;; 接続管理

;;;###autoload
(defun nskk-server-async-close-all-connections ()
  "すべてのサーバー接続を閉じる。"
  (interactive)
  (maphash (lambda (_key conn)
             (nskk-server-async-close-connection conn))
           nskk-server-async--connection-pool)
  (clrhash nskk-server-async--connection-pool))

(defun nskk-server-async-close-connection (conn)
  "サーバー接続を閉じる。

引数:
  CONN - nskk-server-connection構造体"
  (when (nskk-server-connection-process conn)
    (delete-process (nskk-server-connection-process conn)))
  (setf (nskk-server-connection-state conn) 'closed)
  (nskk-server-async--remove-connection conn))

;;; ユーティリティ関数

(defun nskk-server-async-connection-count ()
  "現在のコネクション数を取得する。

戻り値:
  接続数（整数）"
  (hash-table-count nskk-server-async--connection-pool))

(defun nskk-server-async-list-connections ()
  "すべての接続のリストを取得する。

戻り値:
  nskk-server-connection構造体のリスト"
  (let ((connections nil))
    (maphash (lambda (_key conn)
               (push conn connections))
             nskk-server-async--connection-pool)
    connections))

(provide 'nskk-server-async)

;;; nskk-server-async.el ends here
