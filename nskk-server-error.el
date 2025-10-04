;;; nskk-server-error.el --- SKK server error handling for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, server, error
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

;; このファイルはSKK辞書サーバーのエラー処理を実装します。
;;
;; 特徴:
;; - エラー検出と分類
;; - リトライロジック（指数バックオフ）
;; - フォールバック機能（ローカル辞書）
;; - エラーログ記録
;; - サーキットブレーカーパターン
;; - エラー統計収集
;;
;; エラー分類:
;; - 接続エラー（connection-error）
;; - タイムアウト（timeout）
;; - プロトコルエラー（protocol-error）
;; - サーバーエラー（server-error）
;;
;; 使用例:
;;
;;   (require 'nskk-server-error)
;;
;;   ;; リトライ付き検索
;;   (nskk-server-error-with-retry
;;     (lambda (success error)
;;       (nskk-server-async-search
;;         "localhost" 1178 "かんじ" 'okuri-nasi
;;         success error))
;;     (lambda (response) (message "Success: %S" response))
;;     (lambda (error) (message "Failed: %s" error)))

;;; Code:

(require 'cl-lib)
(require 'nskk-server-protocol)
(require 'nskk-server-async)

;;; カスタマイズ変数

(defgroup nskk-server-error nil
  "SKK server error handling customization."
  :group 'nskk
  :prefix "nskk-server-error-")

(defcustom nskk-server-error-max-retries 3
  "最大リトライ回数。"
  :type 'integer
  :group 'nskk-server-error)

(defcustom nskk-server-error-retry-delay 0.5
  "初回リトライまでの遅延時間（秒）。"
  :type 'float
  :group 'nskk-server-error)

(defcustom nskk-server-error-retry-backoff 2.0
  "リトライ遅延のバックオフ係数。"
  :type 'float
  :group 'nskk-server-error)

(defcustom nskk-server-error-fallback-enabled t
  "非nilの場合、サーバーエラー時にローカル辞書へフォールバックする。"
  :type 'boolean
  :group 'nskk-server-error)

(defcustom nskk-server-error-circuit-breaker-enabled t
  "非nilの場合、サーキットブレーカーを有効にする。"
  :type 'boolean
  :group 'nskk-server-error)

(defcustom nskk-server-error-circuit-breaker-threshold 5
  "サーキットブレーカーが開くエラー回数の閾値。"
  :type 'integer
  :group 'nskk-server-error)

(defcustom nskk-server-error-circuit-breaker-timeout 60.0
  "サーキットブレーカーがクローズを試みるまでの時間（秒）。"
  :type 'float
  :group 'nskk-server-error)

(defcustom nskk-server-error-log-enabled t
  "非nilの場合、エラーログを記録する。"
  :type 'boolean
  :group 'nskk-server-error)

(defcustom nskk-server-error-log-max-entries 100
  "エラーログの最大エントリ数。"
  :type 'integer
  :group 'nskk-server-error)

(defcustom nskk-server-error-verbose nil
  "非nilの場合、詳細なログを出力する。"
  :type 'boolean
  :group 'nskk-server-error)

;;; エラーログ

(defvar nskk-server-error--log-entries nil
  "エラーログエントリのリスト。")

(cl-defstruct (nskk-server-error-log-entry
               (:constructor nskk-server-error-log-entry--create)
               (:copier nil))
  "エラーログエントリ。

スロット:
  timestamp    - タイムスタンプ
  error-type   - エラータイプ
  message      - エラーメッセージ
  host         - ホスト名
  port         - ポート番号
  request      - リクエスト内容
  stack-trace  - スタックトレース（あれば）"
  (timestamp nil :type (or null number))
  (error-type nil :type (or null symbol))
  (message nil :type (or null string))
  (host nil :type (or null string))
  (port nil :type (or null integer))
  (request nil :type (or null string))
  (stack-trace nil :type (or null string)))

(defun nskk-server-error--log (error-type message &optional host port request)
  "エラーをログに記録する。

引数:
  ERROR-TYPE - エラータイプ
  MESSAGE    - エラーメッセージ
  HOST       - ホスト名
  PORT       - ポート番号
  REQUEST    - リクエスト内容"
  (when nskk-server-error-log-enabled
    (let ((entry (nskk-server-error-log-entry--create
                 :timestamp (float-time)
                 :error-type error-type
                 :message message
                 :host host
                 :port port
                 :request request)))
      (push entry nskk-server-error--log-entries)
      ;; ログサイズを制限
      (when (> (length nskk-server-error--log-entries)
               nskk-server-error-log-max-entries)
        (setq nskk-server-error--log-entries
              (seq-take nskk-server-error--log-entries
                       nskk-server-error-log-max-entries)))
      (when nskk-server-error-verbose
        (message "[NSKK Server Error] %s: %s" error-type message)))))

;;;###autoload
(defun nskk-server-error-get-log (&optional error-type)
  "エラーログを取得する。

引数:
  ERROR-TYPE - フィルタするエラータイプ（省略時は全て）

戻り値:
  エラーログエントリのリスト"
  (if error-type
      (seq-filter (lambda (entry)
                   (eq (nskk-server-error-log-entry-error-type entry)
                       error-type))
                 nskk-server-error--log-entries)
    nskk-server-error--log-entries))

;;;###autoload
(defun nskk-server-error-clear-log ()
  "エラーログをクリアする。"
  (interactive)
  (setq nskk-server-error--log-entries nil))

;;; サーキットブレーカー

(defvar nskk-server-error--circuit-breakers (make-hash-table :test 'equal)
  "サーキットブレーカー（ホスト:ポート → 状態）")

(cl-defstruct (nskk-server-circuit-breaker
               (:constructor nskk-server-circuit-breaker--create)
               (:copier nil))
  "サーキットブレーカー状態。

スロット:
  state          - 状態（'closed/'open/'half-open）
  error-count    - エラーカウント
  last-error     - 最後のエラー時刻
  open-time      - オープンした時刻"
  (state 'closed :type symbol)
  (error-count 0 :type integer)
  (last-error nil :type (or null number))
  (open-time nil :type (or null number)))

(defun nskk-server-error--get-circuit-breaker (host port)
  "サーキットブレーカーを取得する。

引数:
  HOST - ホスト名
  PORT - ポート番号

戻り値:
  nskk-server-circuit-breaker構造体"
  (let* ((key (format "%s:%d" host port))
         (breaker (gethash key nskk-server-error--circuit-breakers)))
    (unless breaker
      (setq breaker (nskk-server-circuit-breaker--create))
      (puthash key breaker nskk-server-error--circuit-breakers))
    breaker))

(defun nskk-server-error--circuit-breaker-allow-request-p (host port)
  "サーキットブレーカーがリクエストを許可するか判定する。

引数:
  HOST - ホスト名
  PORT - ポート番号

戻り値:
  許可する場合t、拒否する場合nil"
  (unless nskk-server-error-circuit-breaker-enabled
    (cl-return-from nskk-server-error--circuit-breaker-allow-request-p t))

  (let ((breaker (nskk-server-error--get-circuit-breaker host port)))
    (pcase (nskk-server-circuit-breaker-state breaker)
      ('closed t)
      ('open
       ;; タイムアウト経過後はhalf-openに遷移
       (when (and (nskk-server-circuit-breaker-open-time breaker)
                  (> (- (float-time) (nskk-server-circuit-breaker-open-time breaker))
                     nskk-server-error-circuit-breaker-timeout))
         (setf (nskk-server-circuit-breaker-state breaker) 'half-open)
         (when nskk-server-error-verbose
           (message "Circuit breaker half-open: %s:%d" host port))
         t))
      ('half-open t))))

(defun nskk-server-error--circuit-breaker-record-success (host port)
  "成功をサーキットブレーカーに記録する。

引数:
  HOST - ホスト名
  PORT - ポート番号"
  (when nskk-server-error-circuit-breaker-enabled
    (let ((breaker (nskk-server-error--get-circuit-breaker host port)))
      (when (eq (nskk-server-circuit-breaker-state breaker) 'half-open)
        (setf (nskk-server-circuit-breaker-state breaker) 'closed)
        (setf (nskk-server-circuit-breaker-error-count breaker) 0)
        (when nskk-server-error-verbose
          (message "Circuit breaker closed: %s:%d" host port))))))

(defun nskk-server-error--circuit-breaker-record-error (host port)
  "エラーをサーキットブレーカーに記録する。

引数:
  HOST - ホスト名
  PORT - ポート番号"
  (when nskk-server-error-circuit-breaker-enabled
    (let ((breaker (nskk-server-error--get-circuit-breaker host port)))
      (cl-incf (nskk-server-circuit-breaker-error-count breaker))
      (setf (nskk-server-circuit-breaker-last-error breaker) (float-time))

      ;; 閾値を超えたらオープン
      (when (>= (nskk-server-circuit-breaker-error-count breaker)
                nskk-server-error-circuit-breaker-threshold)
        (setf (nskk-server-circuit-breaker-state breaker) 'open)
        (setf (nskk-server-circuit-breaker-open-time breaker) (float-time))
        (when nskk-server-error-verbose
          (message "Circuit breaker opened: %s:%d" host port))))))

;;; リトライロジック

;;;###autoload
(defun nskk-server-error-with-retry (request-fn success-callback error-callback
                                               &optional max-retries)
  "リトライロジック付きでサーバーリクエストを実行する。

引数:
  REQUEST-FN       - リクエスト関数（success-callback, error-callback -> void）
  SUCCESS-CALLBACK - 成功コールバック
  ERROR-CALLBACK   - エラーコールバック（最終的に失敗した場合）
  MAX-RETRIES      - 最大リトライ回数（省略時はnskk-server-error-max-retries）

処理フロー:
  1. リクエスト実行
  2. 失敗時、指数バックオフでリトライ
  3. 最大リトライ回数到達で最終エラーコールバック呼び出し"
  (let ((max-retries (or max-retries nskk-server-error-max-retries))
        (retry-count 0))
    (cl-labels ((retry ()
                  (funcall request-fn
                          ;; 成功コールバック
                          success-callback
                          ;; エラーコールバック（リトライ付き）
                          (lambda (error-msg)
                            (if (< retry-count max-retries)
                                (let ((delay (* nskk-server-error-retry-delay
                                              (expt nskk-server-error-retry-backoff
                                                   retry-count))))
                                  (cl-incf retry-count)
                                  (when nskk-server-error-verbose
                                    (message "Retrying (%d/%d) after %.2fs: %s"
                                            retry-count max-retries delay error-msg))
                                  (run-at-time delay nil #'retry))
                              ;; 最大リトライ回数到達
                              (when nskk-server-error-verbose
                                (message "Max retries reached: %s" error-msg))
                              (funcall error-callback error-msg))))))
      (retry))))

;;; フォールバック機能

;;;###autoload
(defun nskk-server-error-with-fallback (host port key okuri-type
                                             success-callback error-callback
                                             local-dict-index)
  "ローカル辞書フォールバック付きでサーバー検索を実行する。

引数:
  HOST             - サーバーホスト
  PORT             - サーバーポート
  KEY              - 検索キー
  OKURI-TYPE       - 送り仮名タイプ
  SUCCESS-CALLBACK - 成功コールバック
  ERROR-CALLBACK   - エラーコールバック
  LOCAL-DICT-INDEX - ローカル辞書インデックス（フォールバック用）

処理フロー:
  1. サーキットブレーカー確認
  2. サーバー検索実行（リトライ付き）
  3. 失敗時、ローカル辞書で検索"
  ;; サーキットブレーカー確認
  (unless (nskk-server-error--circuit-breaker-allow-request-p host port)
    (when nskk-server-error-verbose
      (message "Circuit breaker open, using fallback: %s:%d" host port))
    (nskk-server-error--fallback-to-local-dict
     key okuri-type success-callback error-callback local-dict-index)
    (cl-return-from nskk-server-error-with-fallback))

  ;; サーバー検索（リトライ付き）
  (nskk-server-error-with-retry
   (lambda (success error)
     (nskk-server-async-search host port key okuri-type success error))
   ;; 成功コールバック
   (lambda (response)
     (nskk-server-error--circuit-breaker-record-success host port)
     (funcall success-callback response))
   ;; エラーコールバック（フォールバック付き）
   (lambda (error-msg)
     (nskk-server-error--log 'server-error error-msg host port key)
     (nskk-server-error--circuit-breaker-record-error host port)
     (if nskk-server-error-fallback-enabled
         (progn
           (when nskk-server-error-verbose
             (message "Falling back to local dict: %s" error-msg))
           (nskk-server-error--fallback-to-local-dict
            key okuri-type success-callback error-callback local-dict-index))
       (funcall error-callback error-msg)))))

(defun nskk-server-error--fallback-to-local-dict (key okuri-type
                                                      success-callback
                                                      error-callback
                                                      local-dict-index)
  "ローカル辞書にフォールバックする。

引数:
  KEY              - 検索キー
  OKURI-TYPE       - 送り仮名タイプ
  SUCCESS-CALLBACK - 成功コールバック
  ERROR-CALLBACK   - エラーコールバック
  LOCAL-DICT-INDEX - ローカル辞書インデックス"
  (require 'nskk-search)
  (condition-case err
      (let ((entry (nskk-search local-dict-index key 'exact okuri-type)))
        (if entry
            ;; ローカル辞書で見つかった場合、サーバーレスポンス形式に変換
            (let ((response (nskk-server-response--create
                           :status 'found
                           :candidates (nskk-dict-entry-candidates entry))))
              (funcall success-callback response))
          ;; 見つからない
          (let ((response (nskk-server-response--create :status 'not-found)))
            (funcall success-callback response))))
    (error
     (funcall error-callback (error-message-string err)))))

;;; エラー分類

(defun nskk-server-error-classify (error-message)
  "エラーメッセージからエラータイプを分類する。

引数:
  ERROR-MESSAGE - エラーメッセージ文字列

戻り値:
  エラータイプシンボル（'connection-error/'timeout/'protocol-error/'server-error）"
  (cond
   ((string-match-p "timeout\\|timed out" error-message) 'timeout)
   ((string-match-p "connection\\|connect" error-message) 'connection-error)
   ((string-match-p "parse\\|protocol" error-message) 'protocol-error)
   (t 'server-error)))

;;; 統計情報

(defvar nskk-server-error--statistics (make-hash-table :test 'equal)
  "エラー統計情報（ホスト:ポート → 統計）")

(cl-defstruct (nskk-server-error-statistics
               (:constructor nskk-server-error-statistics--create)
               (:copier nil))
  "エラー統計。

スロット:
  total-requests   - 総リクエスト数
  total-errors     - 総エラー数
  error-by-type    - タイプ別エラー数（ハッシュテーブル）
  last-error-time  - 最後のエラー時刻"
  (total-requests 0 :type integer)
  (total-errors 0 :type integer)
  (error-by-type (make-hash-table :test 'eq) :type hash-table)
  (last-error-time nil :type (or null number)))

(defun nskk-server-error--get-statistics (host port)
  "統計情報を取得する。

引数:
  HOST - ホスト名
  PORT - ポート番号

戻り値:
  nskk-server-error-statistics構造体"
  (let* ((key (format "%s:%d" host port))
         (stats (gethash key nskk-server-error--statistics)))
    (unless stats
      (setq stats (nskk-server-error-statistics--create))
      (puthash key stats nskk-server-error--statistics))
    stats))

(defun nskk-server-error--record-error-statistics (host port error-type)
  "エラー統計を記録する。

引数:
  HOST       - ホスト名
  PORT       - ポート番号
  ERROR-TYPE - エラータイプ"
  (let ((stats (nskk-server-error--get-statistics host port)))
    (cl-incf (nskk-server-error-statistics-total-errors stats))
    (setf (nskk-server-error-statistics-last-error-time stats) (float-time))
    (let ((count (gethash error-type
                         (nskk-server-error-statistics-error-by-type stats)
                         0)))
      (puthash error-type (1+ count)
              (nskk-server-error-statistics-error-by-type stats)))))

;;;###autoload
(defun nskk-server-error-get-statistics (host port)
  "エラー統計を取得する。

引数:
  HOST - ホスト名
  PORT - ポート番号

戻り値:
  統計情報のplist"
  (let ((stats (nskk-server-error--get-statistics host port)))
    (list :total-requests (nskk-server-error-statistics-total-requests stats)
          :total-errors (nskk-server-error-statistics-total-errors stats)
          :error-rate (if (> (nskk-server-error-statistics-total-requests stats) 0)
                         (/ (float (nskk-server-error-statistics-total-errors stats))
                            (nskk-server-error-statistics-total-requests stats))
                       0.0)
          :last-error-time (nskk-server-error-statistics-last-error-time stats))))

(provide 'nskk-server-error)

;;; nskk-server-error.el ends here
