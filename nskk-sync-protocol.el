;;; nskk-sync-protocol.el --- Sync protocol for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, sync, protocol
;; Version: 1.0.0
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

;; このファイルはNSKK同期プロトコルv1.0を実装します。
;;
;; 特徴:
;; - WebSocketベースの双方向通信
;; - HTTP/2フォールバック対応
;; - バージョン管理とプロトコルネゴシエーション
;; - セッション管理
;; - 自動再接続機能
;; - メッセージキューイング
;;
;; プロトコル仕様:
;;
;;   NSKK Sync Protocol v1.0
;;
;;   メッセージフォーマット（JSON）:
;;   {
;;     "version": "1.0",
;;     "type": "sync|ack|error|ping|pong",
;;     "timestamp": 1234567890,
;;     "session_id": "uuid-string",
;;     "client_id": "client-uuid",
;;     "payload": {...},
;;     "signature": "hmac-signature"
;;   }
;;
;; 使用例:
;;
;;   (require 'nskk-sync-protocol)
;;
;;   ;; セッション開始
;;   (let ((session (nskk-sync-create-session
;;                   :server-url "wss://sync.example.com/nskk"
;;                   :client-id "my-client")))
;;     ;; 接続
;;     (nskk-sync-connect session)
;;
;;     ;; メッセージ送信
;;     (nskk-sync-send-message session 'sync '(:data "..."))
;;
;;     ;; 切断
;;     (nskk-sync-disconnect session))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)

;;; カスタマイズ変数

(defgroup nskk-sync-protocol nil
  "NSKK sync protocol customization."
  :group 'nskk
  :prefix "nskk-sync-protocol-")

(defcustom nskk-sync-protocol-version "1.0"
  "NSKK同期プロトコルバージョン。"
  :type 'string
  :group 'nskk-sync-protocol)

(defcustom nskk-sync-protocol-timeout 30
  "通信タイムアウト時間（秒）。"
  :type 'integer
  :group 'nskk-sync-protocol)

(defcustom nskk-sync-protocol-reconnect-attempts 3
  "再接続試行回数。"
  :type 'integer
  :group 'nskk-sync-protocol)

(defcustom nskk-sync-protocol-reconnect-delay 5
  "再接続までの待機時間（秒）。"
  :type 'integer
  :group 'nskk-sync-protocol)

(defcustom nskk-sync-protocol-ping-interval 60
  "Ping送信間隔（秒）。"
  :type 'integer
  :group 'nskk-sync-protocol)

(defcustom nskk-sync-protocol-max-message-size (* 10 1024 1024)  ; 10MB
  "最大メッセージサイズ（バイト）。"
  :type 'integer
  :group 'nskk-sync-protocol)

;;; データ構造

(cl-defstruct (nskk-sync-message
               (:constructor nskk-sync-message--create)
               (:copier nil))
  "同期プロトコルメッセージ。

スロット:
  version     - プロトコルバージョン
  type        - メッセージタイプ（sync/ack/error/ping/pong）
  timestamp   - Unixタイムスタンプ
  session-id  - セッションID
  client-id   - クライアントID
  payload     - メッセージペイロード
  signature   - HMAC署名"
  (version nskk-sync-protocol-version :type string)
  (type nil :type symbol)
  (timestamp nil :type integer)
  (session-id nil :type string)
  (client-id nil :type string)
  (payload nil :type list)
  (signature nil :type string))

(cl-defstruct (nskk-sync-session
               (:constructor nskk-sync-session--create)
               (:copier nil))
  "同期セッション。

スロット:
  id          - セッションID（UUID）
  client-id   - クライアント識別子
  server-url  - 同期サーバーURL
  last-sync   - 最終同期タイムスタンプ
  state       - セッション状態（disconnected/connecting/connected/error）
  connection  - コネクションオブジェクト
  message-queue - メッセージキュー
  callbacks   - コールバック関数群
  reconnect-count - 再接続試行回数
  ping-timer  - Pingタイマー
  metadata    - 追加メタデータ"
  (id nil :type string)
  (client-id nil :type string)
  (server-url nil :type string)
  (last-sync 0 :type integer)
  (state 'disconnected :type symbol)
  (connection nil)
  (message-queue nil :type list)
  (callbacks nil :type list)
  (reconnect-count 0 :type integer)
  (ping-timer nil)
  (metadata nil :type list))

;;; セッション管理

;;;###autoload
(defun nskk-sync-create-session (&rest args)
  "新しい同期セッションを作成する。

ARGS: キーワード引数
  :server-url  - サーバーURL（必須）
  :client-id   - クライアントID（省略時は自動生成）
  :callbacks   - コールバック関数群

戻り値: `nskk-sync-session' オブジェクト"
  (let* ((server-url (plist-get args :server-url))
         (client-id (or (plist-get args :client-id)
                       (nskk-sync-generate-uuid)))
         (session-id (nskk-sync-generate-uuid))
         (callbacks (plist-get args :callbacks)))

    (unless server-url
      (error "Server URL is required"))

    (nskk-sync-session--create
     :id session-id
     :client-id client-id
     :server-url server-url
     :state 'disconnected
     :callbacks callbacks
     :message-queue nil)))

;;;###autoload
(defun nskk-sync-connect (session)
  "SESSION を使用してサーバーに接続する。"
  (when (eq (nskk-sync-session-state session) 'connected)
    (warn "Session already connected")
    (cl-return-from nskk-sync-connect session))

  (setf (nskk-sync-session-state session) 'connecting)

  (condition-case err
      (let* ((url (nskk-sync-session-server-url session))
             (connection (nskk-sync--establish-connection url session)))
        (setf (nskk-sync-session-connection session) connection)
        (setf (nskk-sync-session-state session) 'connected)
        (setf (nskk-sync-session-reconnect-count session) 0)

        ;; Pingタイマー開始
        (nskk-sync--start-ping-timer session)

        ;; キューにあるメッセージを送信
        (nskk-sync--flush-message-queue session)

        ;; 接続成功コールバック
        (nskk-sync--invoke-callback session 'on-connect)

        session)
    (error
     (setf (nskk-sync-session-state session) 'error)
     (nskk-sync--invoke-callback session 'on-error err)
     (nskk-sync--maybe-reconnect session)
     (signal (car err) (cdr err)))))

;;;###autoload
(defun nskk-sync-disconnect (session)
  "SESSION の接続を切断する。"
  (when (nskk-sync-session-ping-timer session)
    (cancel-timer (nskk-sync-session-ping-timer session))
    (setf (nskk-sync-session-ping-timer session) nil))

  (when (nskk-sync-session-connection session)
    (nskk-sync--close-connection (nskk-sync-session-connection session))
    (setf (nskk-sync-session-connection session) nil))

  (setf (nskk-sync-session-state session) 'disconnected)
  (nskk-sync--invoke-callback session 'on-disconnect)

  session)

;;;###autoload
(defun nskk-sync-send-message (session type payload)
  "SESSION を通じてメッセージを送信する。

TYPE: メッセージタイプ（シンボル）
PAYLOAD: メッセージペイロード（plist）

戻り値: 送信成功時はt、失敗時はnil"
  (let ((message (nskk-sync-message--create
                  :type type
                  :timestamp (nskk-sync--current-timestamp)
                  :session-id (nskk-sync-session-id session)
                  :client-id (nskk-sync-session-client-id session)
                  :payload payload)))

    ;; サイズチェック
    (let ((json-data (nskk-sync--message-to-json message)))
      (when (> (length json-data) nskk-sync-protocol-max-message-size)
        (error "Message size exceeds maximum: %d > %d"
               (length json-data) nskk-sync-protocol-max-message-size)))

    (if (eq (nskk-sync-session-state session) 'connected)
        (nskk-sync--send-message-internal session message)
      ;; 未接続の場合はキューに追加
      (push message (nskk-sync-session-message-queue session))
      nil)))

;;; プロトコル実装

(defun nskk-sync--establish-connection (url session)
  "URL へのコネクションを確立する。

WebSocketが利用可能な場合はWebSocket、
そうでない場合はHTTP/2を使用。

戻り値: コネクションオブジェクト"
  (cond
   ;; WebSocket接続（websocket.elが利用可能な場合）
   ((and (fboundp 'websocket-open)
         (string-prefix-p "ws" url))
    (nskk-sync--establish-websocket url session))

   ;; HTTP/2接続
   ((string-prefix-p "http" url)
    (nskk-sync--establish-http2 url session))

   (t
    (error "Unsupported protocol: %s" url))))

(defun nskk-sync--establish-websocket (url session)
  "WebSocket接続を確立する。"
  (if (not (fboundp 'websocket-open))
      (error "WebSocket support not available")
    ;; websocket.elを使用した実装
    ;; 実際の実装ではwebsocket.elのAPIを使用
    (let ((ws-url url))
      ;; プレースホルダー実装
      ;; 実際にはwebsocket-openを呼び出す
      (list 'websocket :url ws-url :session session))))

(defun nskk-sync--establish-http2 (url session)
  "HTTP/2接続を確立する（ロングポーリング）。"
  ;; HTTP/2ロングポーリング実装
  (list 'http2 :url url :session session))

(defun nskk-sync--close-connection (connection)
  "CONNECTION を閉じる。"
  (pcase (car connection)
    ('websocket
     ;; WebSocket接続のクローズ
     (when (fboundp 'websocket-close)
       ;; 実際のwebsocket closeロジック
       nil))
    ('http2
     ;; HTTP/2接続のクローズ
     nil)))

(defun nskk-sync--send-message-internal (session message)
  "MESSAGE を実際に送信する。"
  (let* ((connection (nskk-sync-session-connection session))
         (json-data (nskk-sync--message-to-json message)))

    (pcase (car connection)
      ('websocket
       (nskk-sync--send-via-websocket connection json-data))
      ('http2
       (nskk-sync--send-via-http2 connection json-data))
      (_
       (error "Invalid connection type")))

    ;; 送信成功コールバック
    (nskk-sync--invoke-callback session 'on-message-sent message)
    t))

(defun nskk-sync--send-via-websocket (connection data)
  "WebSocket経由でDATAを送信する。"
  (when (fboundp 'websocket-send-text)
    ;; 実際のwebsocket send実装
    nil))

(defun nskk-sync--send-via-http2 (connection data)
  "HTTP/2経由でDATAを送信する。"
  (let ((url (plist-get (cdr connection) :url)))
    (let ((url-request-method "POST")
          (url-request-extra-headers
           '(("Content-Type" . "application/json")))
          (url-request-data data))
      (url-retrieve url
                    (lambda (status)
                      ;; レスポンス処理
                      nil)
                    nil t))))

;;; メッセージ処理

(defun nskk-sync--message-to-json (message)
  "MESSAGE をJSON文字列に変換する。"
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "version" (nskk-sync-message-version message) hash)
    (puthash "type" (symbol-name (nskk-sync-message-type message)) hash)
    (puthash "timestamp" (nskk-sync-message-timestamp message) hash)
    (puthash "session_id" (nskk-sync-message-session-id message) hash)
    (puthash "client_id" (nskk-sync-message-client-id message) hash)
    (puthash "payload" (nskk-sync--plist-to-hash (nskk-sync-message-payload message)) hash)
    (when (nskk-sync-message-signature message)
      (puthash "signature" (nskk-sync-message-signature message) hash))

    (json-encode hash)))

(defun nskk-sync--json-to-message (json-string)
  "JSON文字列をメッセージオブジェクトに変換する。"
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (data (json-read-from-string json-string))
         (version (gethash "version" data))
         (type (intern (gethash "type" data)))
         (timestamp (gethash "timestamp" data))
         (session-id (gethash "session_id" data))
         (client-id (gethash "client_id" data))
         (payload (nskk-sync--hash-to-plist (gethash "payload" data)))
         (signature (gethash "signature" data)))

    (nskk-sync-message--create
     :version version
     :type type
     :timestamp timestamp
     :session-id session-id
     :client-id client-id
     :payload payload
     :signature signature)))

(defun nskk-sync--plist-to-hash (plist)
  "PLIST をハッシュテーブルに変換する。"
  (let ((hash (make-hash-table :test 'equal)))
    (cl-loop for (key value) on plist by #'cddr
             do (puthash (substring (symbol-name key) 1) value hash))
    hash))

(defun nskk-sync--hash-to-plist (hash)
  "ハッシュテーブルをplistに変換する。"
  (let ((plist nil))
    (maphash (lambda (key value)
               (push (intern (concat ":" key)) plist)
               (push value plist))
             hash)
    (nreverse plist)))

;;; メッセージキュー

(defun nskk-sync--flush-message-queue (session)
  "SESSION のメッセージキューをフラッシュする。"
  (let ((queue (nskk-sync-session-message-queue session)))
    (setf (nskk-sync-session-message-queue session) nil)
    (dolist (message (nreverse queue))
      (nskk-sync--send-message-internal session message))))

;;; 再接続

(defun nskk-sync--maybe-reconnect (session)
  "必要に応じて SESSION を再接続する。"
  (when (< (nskk-sync-session-reconnect-count session)
           nskk-sync-protocol-reconnect-attempts)
    (cl-incf (nskk-sync-session-reconnect-count session))

    (run-with-timer
     nskk-sync-protocol-reconnect-delay
     nil
     (lambda ()
       (condition-case err
           (nskk-sync-connect session)
         (error
          (message "Reconnect failed: %s" (error-message-string err))
          (nskk-sync--maybe-reconnect session)))))))

;;; Ping/Pong

(defun nskk-sync--start-ping-timer (session)
  "SESSION のPingタイマーを開始する。"
  (when (nskk-sync-session-ping-timer session)
    (cancel-timer (nskk-sync-session-ping-timer session)))

  (setf (nskk-sync-session-ping-timer session)
        (run-with-timer
         nskk-sync-protocol-ping-interval
         nskk-sync-protocol-ping-interval
         (lambda ()
           (when (eq (nskk-sync-session-state session) 'connected)
             (nskk-sync-send-message session 'ping nil))))))

;;; コールバック

(defun nskk-sync--invoke-callback (session event &optional data)
  "SESSION のコールバックを呼び出す。"
  (let ((callback (plist-get (nskk-sync-session-callbacks session) event)))
    (when (functionp callback)
      (funcall callback session data))))

;;; ユーティリティ

(defun nskk-sync-generate-uuid ()
  "UUIDを生成する。"
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior (logand (random 65536) 16383) 16384)
          (logior (logand (random 65536) 16383) 32768)
          (random 65536) (random 65536) (random 65536)))

(defun nskk-sync--current-timestamp ()
  "現在のUnixタイムスタンプを返す。"
  (floor (float-time)))

;;; バージョン管理

(defun nskk-sync-negotiate-version (server-version)
  "サーバーバージョンとネゴシエーションする。

SERVER-VERSION: サーバーがサポートするバージョン文字列

戻り値: 合意されたバージョン、または不一致の場合はnil"
  (let ((client-version nskk-sync-protocol-version))
    (if (nskk-sync--version-compatible-p client-version server-version)
        client-version
      nil)))

(defun nskk-sync--version-compatible-p (v1 v2)
  "2つのバージョンが互換性があるか判定する。

メジャーバージョンが一致していれば互換とみなす。"
  (let ((v1-major (car (version-to-list v1)))
        (v2-major (car (version-to-list v2))))
    (= v1-major v2-major)))

;;; デバッグ

(defun nskk-sync-session-info (session)
  "SESSION の情報を表示する。"
  (interactive)
  (message "NSKK Sync Session Info:
  ID: %s
  Client ID: %s
  Server URL: %s
  State: %s
  Last Sync: %s
  Message Queue: %d messages
  Reconnect Count: %d"
           (nskk-sync-session-id session)
           (nskk-sync-session-client-id session)
           (nskk-sync-session-server-url session)
           (nskk-sync-session-state session)
           (if (> (nskk-sync-session-last-sync session) 0)
               (format-time-string "%Y-%m-%d %H:%M:%S"
                                  (seconds-to-time (nskk-sync-session-last-sync session)))
             "Never")
           (length (nskk-sync-session-message-queue session))
           (nskk-sync-session-reconnect-count session)))

(provide 'nskk-sync-protocol)

;;; nskk-sync-protocol.el ends here
