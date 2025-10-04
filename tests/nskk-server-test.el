;;; nskk-server-test.el --- Tests for NSKK server modules -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはNSKKサーバー機能（protocol/async/error）のテストを実装します。

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-server-protocol)
(require 'nskk-server-async)
(require 'nskk-server-error)

;;; nskk-server-protocol テスト

(nskk-deftest nskk-server-protocol-test-make-request-okuri-nasi
  "送り仮名なし検索リクエストの生成"
  :tags '(:unit :server-protocol)
  (let ((request (nskk-server-protocol-make-request "かんじ" 'okuri-nasi)))
    (should (stringp request))
    ;; "1かんじ " の形式
    (should (string-prefix-p "1" request))
    (should (string-suffix-p " " request))))

(nskk-deftest nskk-server-protocol-test-make-request-okuri-ari
  "送り仮名あり検索リクエストの生成"
  :tags '(:unit :server-protocol)
  (let ((request (nskk-server-protocol-make-request "わたr" 'okuri-ari)))
    (should (stringp request))
    ;; "4わたr " の形式
    (should (string-prefix-p "4" request))
    (should (string-suffix-p " " request))))

(nskk-deftest nskk-server-protocol-test-make-request-error
  "不正なリクエスト生成"
  :tags '(:unit :server-protocol)
  (should-error (nskk-server-protocol-make-request "" 'okuri-nasi))
  (should-error (nskk-server-protocol-make-request "かんじ" 'invalid)))

(nskk-deftest nskk-server-protocol-test-make-version-request
  "バージョン要求リクエストの生成"
  :tags '(:unit :server-protocol)
  (let ((request (nskk-server-protocol-make-version-request)))
    (should (equal request "2"))))

(nskk-deftest nskk-server-protocol-test-make-host-request
  "ホスト名要求リクエストの生成"
  :tags '(:unit :server-protocol)
  (let ((request (nskk-server-protocol-make-host-request)))
    (should (equal request "3"))))

(nskk-deftest nskk-server-protocol-test-make-server-info-request
  "サーバー情報要求リクエストの生成"
  :tags '(:unit :server-protocol)
  (let ((request (nskk-server-protocol-make-server-info-request)))
    (should (equal request "0"))))

(nskk-deftest nskk-server-protocol-test-parse-response-found
  "検索成功レスポンスのパース"
  :tags '(:unit :server-protocol)
  (let ((response (nskk-server-protocol-parse-response "1/漢字/幹事/\n")))
    (should (nskk-server-response-p response))
    (should (eq (nskk-server-response-status response) 'found))
    (should (= (length (nskk-server-response-candidates response)) 2))
    (should (equal (car (nskk-server-response-candidates response))
                   '("漢字" . nil)))
    (should (equal (cadr (nskk-server-response-candidates response))
                   '("幹事" . nil)))))

(nskk-deftest nskk-server-protocol-test-parse-response-found-with-annotation
  "注釈付き検索成功レスポンスのパース"
  :tags '(:unit :server-protocol)
  (let ((response (nskk-server-protocol-parse-response "1/愛;love/哀;sorrow/\n")))
    (should (eq (nskk-server-response-status response) 'found))
    (should (= (length (nskk-server-response-candidates response)) 2))
    (should (equal (car (nskk-server-response-candidates response))
                   '("愛" . "love")))
    (should (equal (cadr (nskk-server-response-candidates response))
                   '("哀" . "sorrow")))))

(nskk-deftest nskk-server-protocol-test-parse-response-not-found
  "検索失敗レスポンスのパース"
  :tags '(:unit :server-protocol)
  (let ((response (nskk-server-protocol-parse-response "4\n")))
    (should (nskk-server-response-p response))
    (should (eq (nskk-server-response-status response) 'not-found))
    (should (null (nskk-server-response-candidates response)))))

(nskk-deftest nskk-server-protocol-test-parse-response-info
  "サーバー情報レスポンスのパース"
  :tags '(:unit :server-protocol)
  (let ((response (nskk-server-protocol-parse-response "skkserv version 1.0\n")))
    (should (eq (nskk-server-response-status response) 'info))
    (should (equal (nskk-server-response-info response) "skkserv version 1.0"))))

(nskk-deftest nskk-server-protocol-test-parse-response-error
  "不正なレスポンス"
  :tags '(:unit :server-protocol)
  (should-error (nskk-server-protocol-parse-response "")))

(nskk-deftest nskk-server-protocol-test-detect-version
  "プロトコルバージョン検出"
  :tags '(:unit :server-protocol)
  (should (= (nskk-server-protocol-detect-version "skkserv UTF-8") 3))
  (should (= (nskk-server-protocol-detect-version "skkserv completion") 2))
  (should (= (nskk-server-protocol-detect-version "skkserv 1.0") 1)))

(nskk-deftest nskk-server-protocol-test-recommend-encoding
  "推奨エンコーディング取得"
  :tags '(:unit :server-protocol)
  (should (eq (nskk-server-protocol-recommend-encoding 3) 'utf-8))
  (should (eq (nskk-server-protocol-recommend-encoding 2) 'euc-jp))
  (should (eq (nskk-server-protocol-recommend-encoding 1) 'euc-jp)))

(nskk-deftest nskk-server-protocol-test-validate-request
  "リクエスト検証"
  :tags '(:unit :server-protocol)
  (let ((valid-request (nskk-server-request--create
                       :command nskk-server-protocol-command-search-okuri-nasi
                       :key "かんじ"))
        (invalid-request (nskk-server-request--create
                         :command ?X
                         :key "かんじ")))
    (should (null (nskk-server-protocol-validate-request valid-request)))
    (should (nskk-server-protocol-validate-request invalid-request))))

(nskk-deftest nskk-server-protocol-test-response-success-p
  "レスポンス成功判定"
  :tags '(:unit :server-protocol)
  (let ((found-response (nskk-server-response--create :status 'found))
        (not-found-response (nskk-server-response--create :status 'not-found)))
    (should (nskk-server-protocol-response-success-p found-response))
    (should-not (nskk-server-protocol-response-success-p not-found-response))))

(nskk-deftest nskk-server-protocol-test-format-response
  "レスポンス整形"
  :tags '(:unit :server-protocol)
  (let ((response (nskk-server-response--create
                  :status 'found
                  :candidates '(("漢字" . nil) ("幹事" . nil)))))
    (should (string-match-p "Found 2 candidates"
                           (nskk-server-protocol-format-response response)))))

;;; nskk-server-async テスト

(nskk-deftest nskk-server-async-test-pool-key
  "コネクションプールキー生成"
  :tags '(:unit :server-async)
  (should (equal (nskk-server-async--pool-key "localhost" 1178)
                 "localhost:1178")))

(nskk-deftest nskk-server-async-test-connection-alive-p
  "接続有効性判定"
  :tags '(:unit :server-async)
  (let ((conn (nskk-server-connection--create
              :state 'connected
              :process nil)))
    ;; プロセスがnilなので無効
    (should-not (nskk-server-async--connection-alive-p conn)))
  (let ((conn (nskk-server-connection--create
              :state 'closed
              :process t)))
    ;; 状態がclosedなので無効
    (should-not (nskk-server-async--connection-alive-p conn))))

(nskk-deftest nskk-server-async-test-connection-count
  "コネクション数取得"
  :tags '(:unit :server-async)
  (let ((nskk-server-async--connection-pool (make-hash-table :test 'equal)))
    (should (= (nskk-server-async-connection-count) 0))
    (puthash "localhost:1178"
            (nskk-server-connection--create)
            nskk-server-async--connection-pool)
    (should (= (nskk-server-async-connection-count) 1))))

(nskk-deftest nskk-server-async-test-list-connections
  "接続リスト取得"
  :tags '(:unit :server-async)
  (let ((nskk-server-async--connection-pool (make-hash-table :test 'equal)))
    (should (null (nskk-server-async-list-connections)))
    (puthash "localhost:1178"
            (nskk-server-connection--create)
            nskk-server-async--connection-pool)
    (should (= (length (nskk-server-async-list-connections)) 1))))

;;; nskk-server-error テスト

(nskk-deftest nskk-server-error-test-log
  "エラーログ記録"
  :tags '(:unit :server-error)
  (let ((nskk-server-error--log-entries nil)
        (nskk-server-error-log-enabled t))
    (nskk-server-error--log 'timeout "Request timeout" "localhost" 1178 "かんじ")
    (should (= (length nskk-server-error--log-entries) 1))
    (let ((entry (car nskk-server-error--log-entries)))
      (should (eq (nskk-server-error-log-entry-error-type entry) 'timeout))
      (should (equal (nskk-server-error-log-entry-message entry) "Request timeout"))
      (should (equal (nskk-server-error-log-entry-host entry) "localhost"))
      (should (= (nskk-server-error-log-entry-port entry) 1178)))))

(nskk-deftest nskk-server-error-test-get-log
  "エラーログ取得"
  :tags '(:unit :server-error)
  (let ((nskk-server-error--log-entries
         (list (nskk-server-error-log-entry--create :error-type 'timeout)
               (nskk-server-error-log-entry--create :error-type 'connection-error)
               (nskk-server-error-log-entry--create :error-type 'timeout))))
    (should (= (length (nskk-server-error-get-log)) 3))
    (should (= (length (nskk-server-error-get-log 'timeout)) 2))
    (should (= (length (nskk-server-error-get-log 'connection-error)) 1))))

(nskk-deftest nskk-server-error-test-clear-log
  "エラーログクリア"
  :tags '(:unit :server-error)
  (let ((nskk-server-error--log-entries
         (list (nskk-server-error-log-entry--create))))
    (should (> (length nskk-server-error--log-entries) 0))
    (nskk-server-error-clear-log)
    (should (= (length nskk-server-error--log-entries) 0))))

(nskk-deftest nskk-server-error-test-circuit-breaker-allow-request-p
  "サーキットブレーカー許可判定"
  :tags '(:unit :server-error)
  (let ((nskk-server-error--circuit-breakers (make-hash-table :test 'equal))
        (nskk-server-error-circuit-breaker-enabled t))
    ;; 初期状態（closed）は許可
    (should (nskk-server-error--circuit-breaker-allow-request-p "localhost" 1178))
    ;; オープン状態
    (let ((breaker (nskk-server-error--get-circuit-breaker "localhost" 1178)))
      (setf (nskk-server-circuit-breaker-state breaker) 'open)
      (setf (nskk-server-circuit-breaker-open-time breaker) (float-time))
      (should-not (nskk-server-error--circuit-breaker-allow-request-p "localhost" 1178)))))

(nskk-deftest nskk-server-error-test-circuit-breaker-record-success
  "サーキットブレーカー成功記録"
  :tags '(:unit :server-error)
  (let ((nskk-server-error--circuit-breakers (make-hash-table :test 'equal))
        (nskk-server-error-circuit-breaker-enabled t))
    (let ((breaker (nskk-server-error--get-circuit-breaker "localhost" 1178)))
      (setf (nskk-server-circuit-breaker-state breaker) 'half-open)
      (nskk-server-error--circuit-breaker-record-success "localhost" 1178)
      (should (eq (nskk-server-circuit-breaker-state breaker) 'closed))
      (should (= (nskk-server-circuit-breaker-error-count breaker) 0)))))

(nskk-deftest nskk-server-error-test-circuit-breaker-record-error
  "サーキットブレーカーエラー記録"
  :tags '(:unit :server-error)
  (let ((nskk-server-error--circuit-breakers (make-hash-table :test 'equal))
        (nskk-server-error-circuit-breaker-enabled t)
        (nskk-server-error-circuit-breaker-threshold 3))
    (let ((breaker (nskk-server-error--get-circuit-breaker "localhost" 1178)))
      ;; 閾値未満
      (nskk-server-error--circuit-breaker-record-error "localhost" 1178)
      (should (eq (nskk-server-circuit-breaker-state breaker) 'closed))
      (should (= (nskk-server-circuit-breaker-error-count breaker) 1))
      ;; 閾値到達
      (nskk-server-error--circuit-breaker-record-error "localhost" 1178)
      (nskk-server-error--circuit-breaker-record-error "localhost" 1178)
      (should (eq (nskk-server-circuit-breaker-state breaker) 'open)))))

(nskk-deftest nskk-server-error-test-classify
  "エラー分類"
  :tags '(:unit :server-error)
  (should (eq (nskk-server-error-classify "Request timeout") 'timeout))
  (should (eq (nskk-server-error-classify "Connection failed") 'connection-error))
  (should (eq (nskk-server-error-classify "Parse error") 'protocol-error))
  (should (eq (nskk-server-error-classify "Unknown error") 'server-error)))

(nskk-deftest nskk-server-error-test-get-statistics
  "エラー統計取得"
  :tags '(:unit :server-error)
  (let ((nskk-server-error--statistics (make-hash-table :test 'equal)))
    (nskk-server-error--record-error-statistics "localhost" 1178 'timeout)
    (nskk-server-error--record-error-statistics "localhost" 1178 'timeout)
    (let ((stats (nskk-server-error-get-statistics "localhost" 1178)))
      (should (= (plist-get stats :total-errors) 2)))))

;;; 統合テスト

(nskk-deftest nskk-server-test-integration-protocol-request-response
  "プロトコル：リクエスト生成→レスポンスパース"
  :tags '(:integration :server)
  (let* ((request (nskk-server-protocol-make-request "かんじ" 'okuri-nasi))
         ;; 模擬サーバーレスポンス
         (response-str "1/漢字/幹事/\n")
         (response (nskk-server-protocol-parse-response response-str)))
    (should (stringp request))
    (should (nskk-server-response-p response))
    (should (eq (nskk-server-response-status response) 'found))
    (should (= (length (nskk-server-response-candidates response)) 2))))

(nskk-deftest nskk-server-test-integration-error-log
  "エラー処理：ログ記録と取得"
  :tags '(:integration :server)
  (let ((nskk-server-error--log-entries nil)
        (nskk-server-error-log-enabled t))
    ;; エラーログ記録
    (nskk-server-error--log 'timeout "Test timeout" "localhost" 1178 "かんじ")
    (nskk-server-error--log 'connection-error "Test connection" "localhost" 1178 nil)
    ;; ログ取得
    (should (= (length (nskk-server-error-get-log)) 2))
    (should (= (length (nskk-server-error-get-log 'timeout)) 1))
    ;; ログクリア
    (nskk-server-error-clear-log)
    (should (= (length (nskk-server-error-get-log)) 0))))

(nskk-deftest nskk-server-test-integration-circuit-breaker-flow
  "サーキットブレーカー：エラー蓄積→オープン→クローズ"
  :tags '(:integration :server)
  (let ((nskk-server-error--circuit-breakers (make-hash-table :test 'equal))
        (nskk-server-error-circuit-breaker-enabled t)
        (nskk-server-error-circuit-breaker-threshold 3))
    ;; 初期状態：リクエスト許可
    (should (nskk-server-error--circuit-breaker-allow-request-p "localhost" 1178))
    ;; エラー蓄積
    (dotimes (_ 3)
      (nskk-server-error--circuit-breaker-record-error "localhost" 1178))
    ;; オープン状態：リクエスト拒否
    (should-not (nskk-server-error--circuit-breaker-allow-request-p "localhost" 1178))
    ;; 成功記録でクローズ（half-open経由）
    (let ((breaker (nskk-server-error--get-circuit-breaker "localhost" 1178)))
      (setf (nskk-server-circuit-breaker-state breaker) 'half-open))
    (nskk-server-error--circuit-breaker-record-success "localhost" 1178)
    (should (nskk-server-error--circuit-breaker-allow-request-p "localhost" 1178))))

(provide 'nskk-server-test)

;;; nskk-server-test.el ends here
