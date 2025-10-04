;;; nskk-server-protocol.el --- SKK server protocol for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, server, protocol
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

;; このファイルはSKK辞書サーバープロトコル（skkserv）を実装します。
;;
;; 特徴:
;; - SKKサーバープロトコル1.0/2.0/3.0対応
;; - リクエスト/レスポンスパース
;; - プロトコルバージョン自動検出
;; - エンコーディング処理（EUC-JP/UTF-8）
;; - 拡張コマンド対応
;;
;; SKKサーバープロトコル:
;;
;;   基本コマンド:
;;   - '1<key> ': 見出し検索（送り仮名なし）
;;   - '4<key> ': 見出し検索（送り仮名あり）
;;   - '0': サーバー情報要求
;;   - '2': プロトコルバージョン要求
;;   - '3': ホスト名要求
;;
;;   応答フォーマット:
;;   - '1/<candidate1>/<candidate2>/.../' : 成功
;;   - '4' : 見つからない
;;
;; 使用例:
;;
;;   (require 'nskk-server-protocol)
;;
;;   ;; リクエスト作成
;;   (nskk-server-protocol-make-request "かんじ" 'okuri-nasi)
;;   ;; => "1かんじ "
;;
;;   ;; レスポンスパース
;;   (nskk-server-protocol-parse-response "1/漢字/幹事/")
;;   ;; => (("漢字" . nil) ("幹事" . nil))

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-server-protocol nil
  "SKK server protocol customization."
  :group 'nskk
  :prefix "nskk-server-protocol-")

(defcustom nskk-server-protocol-version 'auto
  "使用するプロトコルバージョン。
- 'auto: 自動検出
- 1: バージョン1.0（基本）
- 2: バージョン2.0（補完対応）
- 3: バージョン3.0（UTF-8対応）"
  :type '(choice (const :tag "Auto detect" auto)
                 (const :tag "Version 1.0" 1)
                 (const :tag "Version 2.0" 2)
                 (const :tag "Version 3.0" 3))
  :group 'nskk-server-protocol)

(defcustom nskk-server-protocol-encoding 'euc-jp
  "サーバー通信のデフォルトエンコーディング。
- 'euc-jp: EUC-JP（プロトコル1.0/2.0）
- 'utf-8: UTF-8（プロトコル3.0）"
  :type '(choice (const :tag "EUC-JP" euc-jp)
                 (const :tag "UTF-8" utf-8))
  :group 'nskk-server-protocol)

(defcustom nskk-server-protocol-verbose nil
  "非nilの場合、詳細なログを出力する。"
  :type 'boolean
  :group 'nskk-server-protocol)

;;; プロトコル定数

(defconst nskk-server-protocol-command-search-okuri-nasi ?1
  "送り仮名なし検索コマンド。")

(defconst nskk-server-protocol-command-version ?2
  "バージョン要求コマンド。")

(defconst nskk-server-protocol-command-host ?3
  "ホスト名要求コマンド。")

(defconst nskk-server-protocol-command-search-okuri-ari ?4
  "送り仮名あり検索コマンド。")

(defconst nskk-server-protocol-command-server-info ?0
  "サーバー情報要求コマンド。")

(defconst nskk-server-protocol-response-not-found ?4
  "検索結果なし応答。")

(defconst nskk-server-protocol-response-found ?1
  "検索結果あり応答。")

(defconst nskk-server-protocol-space ?\s
  "リクエスト終端文字（スペース）。")

(defconst nskk-server-protocol-newline ?\n
  "レスポンス終端文字（改行）。")

;;; エラー定義

(define-error 'nskk-server-protocol-error
  "SKK server protocol error")

(define-error 'nskk-server-protocol-parse-error
  "SKK server protocol parse error"
  'nskk-server-protocol-error)

(define-error 'nskk-server-protocol-invalid-request
  "Invalid SKK server request"
  'nskk-server-protocol-error)

(define-error 'nskk-server-protocol-invalid-response
  "Invalid SKK server response"
  'nskk-server-protocol-error)

;;; データ構造

(cl-defstruct (nskk-server-request
               (:constructor nskk-server-request--create)
               (:copier nil))
  "SKKサーバーリクエスト。

スロット:
  command   - コマンド文字（?1, ?4, ?0, ?2, ?3）
  key       - 検索キー（検索コマンドの場合）
  encoding  - エンコーディング"
  (command nil :type (or null character))
  (key nil :type (or null string))
  (encoding 'euc-jp :type symbol))

(cl-defstruct (nskk-server-response
               (:constructor nskk-server-response--create)
               (:copier nil))
  "SKKサーバーレスポンス。

スロット:
  status      - ステータス（'found/'not-found/'info）
  candidates  - 候補リスト（(word . annotation)のリスト）
  info        - サーバー情報文字列"
  (status nil :type (or null symbol))
  (candidates nil :type list)
  (info nil :type (or null string)))

;;; リクエスト生成

;;;###autoload
(defun nskk-server-protocol-make-request (key okuri-type &optional encoding)
  "SKKサーバーリクエストを生成する。

引数:
  KEY        - 検索キー
  OKURI-TYPE - 送り仮名タイプ（'okuri-ari/'okuri-nasi）
  ENCODING   - エンコーディング（省略時はnskk-server-protocol-encoding）

戻り値:
  リクエスト文字列（バイトシーケンス）

例:
  (nskk-server-protocol-make-request \"かんじ\" 'okuri-nasi)
  => \"1かんじ \""
  (unless (and (stringp key) (> (length key) 0))
    (signal 'nskk-server-protocol-invalid-request
            (list "Key must be a non-empty string")))

  (let* ((encoding (or encoding nskk-server-protocol-encoding))
         (command (pcase okuri-type
                   ('okuri-nasi nskk-server-protocol-command-search-okuri-nasi)
                   ('okuri-ari nskk-server-protocol-command-search-okuri-ari)
                   (_ (signal 'nskk-server-protocol-invalid-request
                             (list (format "Invalid okuri-type: %s" okuri-type))))))
         (request (nskk-server-request--create
                  :command command
                  :key key
                  :encoding encoding)))
    (nskk-server-protocol-serialize-request request)))

(defun nskk-server-protocol-serialize-request (request)
  "リクエスト構造体を文字列に変換する。

引数:
  REQUEST - nskk-server-request構造体

戻り値:
  リクエスト文字列（エンコーディング済み）"
  (let* ((command (nskk-server-request-command request))
         (key (nskk-server-request-key request))
         (encoding (nskk-server-request-encoding request))
         (request-str (if key
                         (format "%c%s%c"
                                command
                                key
                                nskk-server-protocol-space)
                       (format "%c" command))))
    ;; エンコーディング変換
    (encode-coding-string request-str encoding)))

;;;###autoload
(defun nskk-server-protocol-make-version-request ()
  "プロトコルバージョン要求リクエストを生成する。

戻り値:
  リクエスト文字列"
  (string nskk-server-protocol-command-version))

;;;###autoload
(defun nskk-server-protocol-make-host-request ()
  "ホスト名要求リクエストを生成する。

戻り値:
  リクエスト文字列"
  (string nskk-server-protocol-command-host))

;;;###autoload
(defun nskk-server-protocol-make-server-info-request ()
  "サーバー情報要求リクエストを生成する。

戻り値:
  リクエスト文字列"
  (string nskk-server-protocol-command-server-info))

;;; レスポンスパース

;;;###autoload
(defun nskk-server-protocol-parse-response (response-str &optional encoding)
  "サーバーレスポンスをパースする。

引数:
  RESPONSE-STR - レスポンス文字列（バイトシーケンス）
  ENCODING     - エンコーディング（省略時はnskk-server-protocol-encoding）

戻り値:
  nskk-server-response構造体

レスポンス形式:
  - '1/<candidate1>/<candidate2>/.../' : 見つかった
  - '4' : 見つからない
  - '<info-text>' : サーバー情報"
  (unless (and (stringp response-str) (> (length response-str) 0))
    (signal 'nskk-server-protocol-invalid-response
            (list "Response must be a non-empty string")))

  (let* ((encoding (or encoding nskk-server-protocol-encoding))
         ;; 改行を除去
         (response (string-trim-right response-str "[\r\n]+"))
         ;; デコード
         (decoded (decode-coding-string response encoding)))

    (when nskk-server-protocol-verbose
      (message "Parsing response: %s" decoded))

    (cond
     ;; 見つからない
     ((string= decoded "4")
      (nskk-server-response--create :status 'not-found))

     ;; 検索結果あり
     ((string-prefix-p "1/" decoded)
      (let ((candidates (nskk-server-protocol--parse-candidates decoded)))
        (nskk-server-response--create
         :status 'found
         :candidates candidates)))

     ;; サーバー情報
     (t
      (nskk-server-response--create
       :status 'info
       :info decoded)))))

(defun nskk-server-protocol--parse-candidates (response-str)
  "候補リストをパースする（内部関数）。

引数:
  RESPONSE-STR - レスポンス文字列（\"1/...\" 形式）

戻り値:
  候補リスト（(word . annotation)のリスト）

形式:
  1/候補1/候補2;注釈2/.../"
  (unless (string-prefix-p "1/" response-str)
    (signal 'nskk-server-protocol-parse-error
            (list (format "Invalid response format: %s" response-str))))

  ;; "1/" を除去して候補部分を取得
  (let* ((candidate-part (substring response-str 2))
         (candidates nil))

    ;; 末尾の "/" を除去
    (when (string-suffix-p "/" candidate-part)
      (setq candidate-part (substring candidate-part 0 -1)))

    ;; "/" で分割
    (when (> (length candidate-part) 0)
      (let ((raw-candidates (split-string candidate-part "/" t)))
        (dolist (raw-cand raw-candidates)
          ;; 注釈をパース（候補;注釈 形式）
          (if (string-match "^\\([^;]+\\);\\(.+\\)$" raw-cand)
              (push (cons (match-string 1 raw-cand)
                         (match-string 2 raw-cand))
                    candidates)
            (push (cons raw-cand nil) candidates)))))

    (nreverse candidates)))

;;; プロトコルバージョン検出

;;;###autoload
(defun nskk-server-protocol-detect-version (server-info)
  "サーバー情報からプロトコルバージョンを検出する。

引数:
  SERVER-INFO - サーバー情報文字列

戻り値:
  プロトコルバージョン（1, 2, 3）

検出ロジック:
  - \"UTF-8\" を含む場合: バージョン3
  - \"completion\" を含む場合: バージョン2
  - それ以外: バージョン1"
  (cond
   ((string-match-p "UTF-8\\|utf-8" server-info) 3)
   ((string-match-p "completion\\|COMPLETION" server-info) 2)
   (t 1)))

;;;###autoload
(defun nskk-server-protocol-recommend-encoding (version)
  "プロトコルバージョンから推奨エンコーディングを取得する。

引数:
  VERSION - プロトコルバージョン（1, 2, 3）

戻り値:
  推奨エンコーディング（'euc-jp/'utf-8）"
  (if (>= version 3)
      'utf-8
    'euc-jp))

;;; ユーティリティ関数

(defun nskk-server-protocol-validate-request (request)
  "リクエストの妥当性を検証する。

引数:
  REQUEST - nskk-server-request構造体

戻り値:
  エラーメッセージのリスト（空リストの場合は妥当）"
  (let ((errors nil)
        (command (nskk-server-request-command request))
        (key (nskk-server-request-key request)))

    ;; コマンド検証
    (unless (memq command
                  (list nskk-server-protocol-command-search-okuri-nasi
                        nskk-server-protocol-command-search-okuri-ari
                        nskk-server-protocol-command-version
                        nskk-server-protocol-command-host
                        nskk-server-protocol-command-server-info))
      (push (format "Invalid command: %s" command) errors))

    ;; 検索コマンドの場合はキーが必須
    (when (memq command
                (list nskk-server-protocol-command-search-okuri-nasi
                      nskk-server-protocol-command-search-okuri-ari))
      (unless (and (stringp key) (> (length key) 0))
        (push "Search command requires a non-empty key" errors)))

    errors))

(defun nskk-server-protocol-response-success-p (response)
  "レスポンスが成功か判定する。

引数:
  RESPONSE - nskk-server-response構造体

戻り値:
  成功の場合t、それ以外nil"
  (eq (nskk-server-response-status response) 'found))

(defun nskk-server-protocol-format-response (response)
  "レスポンスを人間が読める形式に整形する。

引数:
  RESPONSE - nskk-server-response構造体

戻り値:
  整形された文字列"
  (pcase (nskk-server-response-status response)
    ('found
     (format "Found %d candidates: %s"
             (length (nskk-server-response-candidates response))
             (mapconcat (lambda (cand)
                         (if (cdr cand)
                             (format "%s;%s" (car cand) (cdr cand))
                           (car cand)))
                       (nskk-server-response-candidates response)
                       ", ")))
    ('not-found
     "Not found")
    ('info
     (format "Server info: %s" (nskk-server-response-info response)))
    (_
     "Unknown status")))

(provide 'nskk-server-protocol)

;;; nskk-server-protocol.el ends here
