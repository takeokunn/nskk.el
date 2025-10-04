;;; nskk-dict-errors-test.el --- Tests for nskk-dict-errors -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, dictionary, tests

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

;; nskk-dict-errors.el のテストスイート。
;;
;; テスト対象:
;; - カスタムエラー型定義
;; - エラーリカバリー機構
;; - フォールバック辞書機能
;; - エラーロギング
;; - ユーザー通知

;;; Code:

(require 'ert)
(require 'nskk-dict-errors)
(require 'nskk-dict-struct)
(require 'nskk-dict-parser)
(require 'nskk-dict-io)

;;; テストヘルパー

(defvar nskk-dict-errors-test--temp-dir nil
  "テスト用一時ディレクトリ。")

(defun nskk-dict-errors-test--setup ()
  "テスト環境をセットアップする。"
  (setq nskk-dict-errors-test--temp-dir
       (make-temp-file "nskk-errors-test-" t))
  ;; ログをクリア
  (setq nskk-dict-errors--log-entries nil)
  (setq nskk-dict-errors--fallback-active nil)
  ;; キャッシュをクリア
  (nskk-clear-dictionary-cache))

(defun nskk-dict-errors-test--teardown ()
  "テスト環境をクリーンアップする。"
  (when (and nskk-dict-errors-test--temp-dir
            (file-directory-p nskk-dict-errors-test--temp-dir))
    (delete-directory nskk-dict-errors-test--temp-dir t))
  (setq nskk-dict-errors--log-entries nil)
  (setq nskk-dict-errors--fallback-active nil)
  (nskk-clear-dictionary-cache))

(defun nskk-dict-errors-test--create-sample-dict (path)
  "サンプル辞書ファイルを作成する。

引数:
  PATH - 作成するファイルパス"
  (with-temp-file path
    (insert ";; -*- coding: utf-8 -*-\n")
    (insert ";; okuri-ari entries.\n")
    (insert "わたr /渡/航/\n")
    (insert "\n")
    (insert ";; okuri-nasi entries.\n")
    (insert "かんじ /漢字/幹事/\n")
    (insert "にほん /日本/\n")
    (insert "じしょ /辞書/\n")))

(defun nskk-dict-errors-test--create-invalid-dict (path)
  "不正な辞書ファイルを作成する。

引数:
  PATH - 作成するファイルパス"
  (with-temp-file path
    (insert ";; -*- coding: utf-8 -*-\n")
    (insert ";; okuri-nasi entries.\n")
    (insert "invalid line without slash\n")
    (insert "かんじ /漢字/\n")))

;;; エラー型定義テスト

(ert-deftest nskk-dict-errors-test-error-hierarchy ()
  "エラー型の階層構造をテストする。"
  ;; ベースエラー
  (should (get 'nskk-dict-error 'error-conditions))
  (should (memq 'error (get 'nskk-dict-error 'error-conditions)))

  ;; I/Oエラー
  (should (memq 'nskk-dict-error (get 'nskk-dict-io-error 'error-conditions)))
  (should (memq 'nskk-dict-io-error
               (get 'nskk-dict-io-file-not-found 'error-conditions)))
  (should (memq 'nskk-dict-io-error
               (get 'nskk-dict-io-permission-denied 'error-conditions)))
  (should (memq 'nskk-dict-io-error
               (get 'nskk-dict-io-checksum-mismatch 'error-conditions)))

  ;; パースエラー
  (should (memq 'nskk-dict-error (get 'nskk-dict-parse-error 'error-conditions)))
  (should (memq 'nskk-dict-parse-error
               (get 'nskk-dict-parse-invalid-format 'error-conditions)))
  (should (memq 'nskk-dict-parse-error
               (get 'nskk-dict-parse-encoding-error 'error-conditions)))

  ;; 構造エラー
  (should (memq 'nskk-dict-error (get 'nskk-dict-struct-error 'error-conditions)))
  (should (memq 'nskk-dict-struct-error
               (get 'nskk-dict-struct-invalid-entry 'error-conditions))))

(ert-deftest nskk-dict-errors-test-error-signaling ()
  "エラーのシグナリングをテストする。"
  (should-error
   (signal 'nskk-dict-error '("test error"))
   :type 'nskk-dict-error)

  (should-error
   (signal 'nskk-dict-io-file-not-found '("/path/to/file"))
   :type 'nskk-dict-io-file-not-found)

  ;; 親エラー型でキャッチできるか確認
  (should-error
   (signal 'nskk-dict-io-file-not-found '("/path/to/file"))
   :type 'nskk-dict-io-error)

  (should-error
   (signal 'nskk-dict-io-file-not-found '("/path/to/file"))
   :type 'nskk-dict-error))

;;; エラー情報構造テスト

(ert-deftest nskk-dict-errors-test-error-info-creation ()
  "エラー情報オブジェクトの作成をテストする。"
  (let ((error-info (nskk-dict-errors-create
                     'nskk-dict-io-file-not-found
                     "Test error"
                     '("/path/to/file"))))
    (should (nskk-dict-error-info-p error-info))
    (should (eq (nskk-dict-error-info-type error-info)
               'nskk-dict-io-file-not-found))
    (should (string= (nskk-dict-error-info-message error-info) "Test error"))
    (should (equal (nskk-dict-error-info-data error-info) '("/path/to/file")))
    (should (numberp (nskk-dict-error-info-timestamp error-info)))))

(ert-deftest nskk-dict-errors-test-user-message-formatting ()
  "ユーザー向けメッセージのフォーマットをテストする。"
  ;; ファイルなし
  (let ((msg (nskk-dict-errors-format-user-message
             'nskk-dict-io-file-not-found)))
    (should (string-match-p "見つかりません" msg))
    (should (string-match-p "フォールバック" msg)))

  ;; 権限エラー
  (let ((msg (nskk-dict-errors-format-user-message
             'nskk-dict-io-permission-denied)))
    (should (string-match-p "権限" msg))
    (should (string-match-p "読み取り専用" msg)))

  ;; チェックサム不一致
  (let ((msg (nskk-dict-errors-format-user-message
             'nskk-dict-io-checksum-mismatch)))
    (should (string-match-p "チェックサム" msg))
    (should (string-match-p "破損" msg)))

  ;; エラー情報オブジェクトからのフォーマット
  (let* ((error-info (nskk-dict-errors-create
                     'nskk-dict-io-file-not-found
                     "Test error"
                     '("/test/path")))
         (msg (nskk-dict-errors-format-user-message error-info)))
    (should (string-match-p "/test/path" msg))))

;;; ログ機能テスト

(ert-deftest nskk-dict-errors-test-logging ()
  "ログ記録機能をテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (progn
        (let ((nskk-dict-errors-enable-logging t))
          ;; ログエントリを追加
          (nskk-dict-errors-log 'error 'test-error "Test message" '(data1 data2))

          ;; ログが記録されたか確認
          (should (= (length nskk-dict-errors--log-entries) 1))

          (let ((entry (car nskk-dict-errors--log-entries)))
            (should (eq (plist-get entry :severity) 'error))
            (should (eq (plist-get entry :type) 'test-error))
            (should (string= (plist-get entry :message) "Test message"))
            (should (equal (plist-get entry :data) '(data1 data2)))
            (should (numberp (plist-get entry :timestamp)))))

        ;; 複数ログエントリ
        (nskk-dict-errors-log 'warning 'test-warning "Warning message")
        (should (= (length nskk-dict-errors--log-entries) 2)))
    (nskk-dict-errors-test--teardown)))

(ert-deftest nskk-dict-errors-test-log-clear ()
  "ログクリア機能をテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let ((nskk-dict-errors-enable-logging t))
        ;; ログを追加
        (nskk-dict-errors-log 'error 'test-error "Test")
        (should (> (length nskk-dict-errors--log-entries) 0))

        ;; ログをクリア
        (nskk-dict-errors-clear-log)
        (should (= (length nskk-dict-errors--log-entries) 0)))
    (nskk-dict-errors-test--teardown)))

;;; フォールバック辞書テスト

(ert-deftest nskk-dict-errors-test-builtin-dict-structure ()
  "組み込み辞書のデータ構造をテストする。"
  (should (listp nskk-dict-errors--builtin-dict))
  (should (> (length nskk-dict-errors--builtin-dict) 0))

  ;; 各エントリの構造を確認
  (dolist (entry nskk-dict-errors--builtin-dict)
    (should (listp entry))
    (should (= (length entry) 2))
    (should (stringp (car entry)))   ; 見出し語
    (should (listp (cadr entry)))    ; 候補リスト
    (should (> (length (cadr entry)) 0))))

(ert-deftest nskk-dict-errors-test-create-builtin-index ()
  "組み込み辞書からのインデックス作成をテストする。"
  (let ((index (nskk-dict-errors--create-builtin-index)))
    (should (nskk-dict-index-p index))
    (should (hash-table-p (nskk-dict-index-okuri-nasi-table index)))
    (should (> (hash-table-count (nskk-dict-index-okuri-nasi-table index)) 0))

    ;; 特定のエントリを確認
    (let ((entry (gethash "かんじ" (nskk-dict-index-okuri-nasi-table index))))
      (should (nskk-dict-entry-p entry))
      (should (string= (nskk-dict-entry-midashi entry) "かんじ"))
      (should (> (length (nskk-dict-entry-candidates entry)) 0)))))

(ert-deftest nskk-dict-errors-test-create-fallback-index ()
  "フォールバック辞書作成をテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let ((nskk-dict-errors-fallback-dict-path nil))
        (let ((index (nskk-dict-errors-create-fallback-index)))
          (should (nskk-dict-index-p index))
          (should nskk-dict-errors--fallback-active)
          (should (> (hash-table-count
                     (nskk-dict-index-okuri-nasi-table index)) 0))))
    (nskk-dict-errors-test--teardown)))

(ert-deftest nskk-dict-errors-test-custom-fallback-dict ()
  "カスタムフォールバック辞書の読み込みをテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let* ((custom-dict-path (expand-file-name "custom-fallback.dict"
                                                nskk-dict-errors-test--temp-dir))
             (nskk-dict-errors-fallback-dict-path custom-dict-path))
        ;; カスタム辞書を作成
        (nskk-dict-errors-test--create-sample-dict custom-dict-path)

        ;; カスタム辞書が読み込まれるか確認
        (let ((index (nskk-dict-errors-create-fallback-index)))
          (should (nskk-dict-index-p index))
          (should nskk-dict-errors--fallback-active)

          ;; カスタム辞書のエントリが含まれているか確認
          (let ((entry (gethash "にほん"
                               (nskk-dict-index-okuri-nasi-table index))))
            (should (nskk-dict-entry-p entry))
            (should (string= (nskk-dict-entry-midashi entry) "にほん")))))
    (nskk-dict-errors-test--teardown)))

(ert-deftest nskk-dict-errors-test-fallback-active-check ()
  "フォールバックモード判定をテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (progn
        ;; 初期状態は非アクティブ
        (should-not (nskk-dict-errors-is-fallback-active-p))

        ;; フォールバック作成後はアクティブ
        (nskk-dict-errors-create-fallback-index)
        (should (nskk-dict-errors-is-fallback-active-p)))
    (nskk-dict-errors-test--teardown)))

;;; リカバリー機構テスト

(ert-deftest nskk-dict-errors-test-recover-from-backup ()
  "バックアップからのリカバリーをテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let* ((dict-path (expand-file-name "test.dict"
                                         nskk-dict-errors-test--temp-dir))
             (nskk-dict-io-backup-dir (expand-file-name "backups"
                                                        nskk-dict-errors-test--temp-dir))
             (nskk-dict-io-backup-enabled t))

        ;; 辞書ファイルとバックアップを作成
        (nskk-dict-errors-test--create-sample-dict dict-path)
        (nskk-backup-dictionary dict-path)

        ;; 辞書ファイルを破損
        (with-temp-file dict-path
          (insert "corrupted data"))

        ;; リカバリー実行
        (let ((result (nskk-dict-errors--recover-from-backup dict-path)))
          (should (eq (car result) 'success))

          ;; ファイルが復旧されているか確認
          (should (file-exists-p dict-path))
          (with-temp-buffer
            (insert-file-contents dict-path)
            (goto-char (point-min))
            (should (search-forward "okuri-nasi entries" nil t)))))
    (nskk-dict-errors-test--teardown)))

(ert-deftest nskk-dict-errors-test-recover-no-backup ()
  "バックアップなしでのリカバリーをテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let* ((dict-path (expand-file-name "test.dict"
                                         nskk-dict-errors-test--temp-dir))
             (nskk-dict-io-backup-dir (expand-file-name "backups"
                                                        nskk-dict-errors-test--temp-dir)))

        ;; バックアップなしでリカバリー試行
        (let ((result (nskk-dict-errors--recover-from-backup dict-path)))
          (should (eq (car result) 'failure))
          (should (nskk-dict-error-info-p (cdr result)))
          (should (eq (nskk-dict-error-info-type (cdr result))
                     'nskk-dict-io-backup-failed))))
    (nskk-dict-errors-test--teardown)))

(ert-deftest nskk-dict-errors-test-retry-with-encoding ()
  "エンコーディング再試行をテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let ((dict-path (expand-file-name "test.dict"
                                        nskk-dict-errors-test--temp-dir)))
        ;; UTF-8辞書を作成
        (nskk-dict-errors-test--create-sample-dict dict-path)

        ;; UTF-8で読み込み
        (let ((result (nskk-dict-errors--retry-with-encoding dict-path 'utf-8)))
          (should (eq (car result) 'success))
          (should (nskk-dict-index-p (cdr result)))))
    (nskk-dict-errors-test--teardown)))

(ert-deftest nskk-dict-errors-test-auto-recover-file-not-found ()
  "ファイルなしエラーの自動リカバリーをテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let ((nskk-dict-errors-auto-recovery t))
        (let ((result (nskk-dict-errors-auto-recover
                      'nskk-dict-io-file-not-found
                      "/nonexistent/path")))
          (should (eq (car result) 'success))
          (should (nskk-dict-index-p (cdr result)))
          (should (nskk-dict-errors-is-fallback-active-p))))
    (nskk-dict-errors-test--teardown)))

(ert-deftest nskk-dict-errors-test-auto-recover-parse-error ()
  "パースエラーの自動リカバリーをテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let* ((dict-path (expand-file-name "test.dict"
                                         nskk-dict-errors-test--temp-dir))
             (nskk-dict-io-backup-dir (expand-file-name "backups"
                                                        nskk-dict-errors-test--temp-dir))
             (nskk-dict-io-backup-enabled t)
             (nskk-dict-errors-auto-recovery t))

        ;; 正常な辞書でバックアップ作成
        (nskk-dict-errors-test--create-sample-dict dict-path)
        (nskk-backup-dictionary dict-path)

        ;; 辞書を破損
        (nskk-dict-errors-test--create-invalid-dict dict-path)

        ;; 自動リカバリー実行
        (let ((result (nskk-dict-errors-auto-recover
                      'nskk-dict-parse-error
                      dict-path)))
          (should (eq (car result) 'success))
          (should (nskk-dict-index-p (cdr result)))))
    (nskk-dict-errors-test--teardown)))

;;; 統合テスト

(ert-deftest nskk-dict-errors-test-load-with-recovery-success ()
  "リカバリー付き読み込み（成功ケース）をテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let ((dict-path (expand-file-name "test.dict"
                                        nskk-dict-errors-test--temp-dir)))
        ;; 正常な辞書を作成
        (nskk-dict-errors-test--create-sample-dict dict-path)

        ;; 読み込み
        (let ((index (nskk-dict-errors-load-with-recovery dict-path)))
          (should (nskk-dict-index-p index))
          (should-not (nskk-dict-errors-is-fallback-active-p))

          ;; エントリを確認
          (let ((entry (gethash "かんじ"
                               (nskk-dict-index-okuri-nasi-table index))))
            (should (nskk-dict-entry-p entry))
            (should (string= (nskk-dict-entry-midashi entry) "かんじ")))))
    (nskk-dict-errors-test--teardown)))

(ert-deftest nskk-dict-errors-test-load-with-recovery-file-not-found ()
  "リカバリー付き読み込み（ファイルなし）をテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let ((nskk-dict-errors-auto-recovery t))
        ;; 存在しないファイルを読み込み
        (let ((index (nskk-dict-errors-load-with-recovery
                     "/nonexistent/path.dict")))
          (should (nskk-dict-index-p index))
          (should (nskk-dict-errors-is-fallback-active-p))

          ;; フォールバック辞書が使われているか確認
          (should (> (hash-table-count
                     (nskk-dict-index-okuri-nasi-table index)) 0))))
    (nskk-dict-errors-test--teardown)))

(ert-deftest nskk-dict-errors-test-load-with-recovery-parse-error ()
  "リカバリー付き読み込み（パースエラー）をテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let* ((dict-path (expand-file-name "test.dict"
                                         nskk-dict-errors-test--temp-dir))
             (nskk-dict-io-backup-dir (expand-file-name "backups"
                                                        nskk-dict-errors-test--temp-dir))
             (nskk-dict-io-backup-enabled t)
             (nskk-dict-errors-auto-recovery t)
             (nskk-dict-parser-error-strategy 'strict))

        ;; 正常な辞書でバックアップ作成
        (nskk-dict-errors-test--create-sample-dict dict-path)
        (let ((index (nskk-load-dictionary dict-path)))
          (nskk-save-dictionary index dict-path))

        ;; 辞書を破損
        (nskk-dict-errors-test--create-invalid-dict dict-path)

        ;; リカバリー付き読み込み
        (let ((index (nskk-dict-errors-load-with-recovery dict-path)))
          (should (nskk-dict-index-p index))

          ;; バックアップから復旧されているか、フォールバックが使われているか
          (should (> (hash-table-count
                     (nskk-dict-index-okuri-nasi-table index)) 0))))
    (nskk-dict-errors-test--teardown)))

(ert-deftest nskk-dict-errors-test-load-without-auto-recovery ()
  "自動リカバリー無効時の動作をテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let ((nskk-dict-errors-auto-recovery nil))
        ;; 存在しないファイルを読み込み → エラーがシグナルされるべき
        (should-error
         (nskk-dict-errors-load-with-recovery "/nonexistent/path.dict")
         :type 'nskk-dict-io-file-not-found))
    (nskk-dict-errors-test--teardown)))

;;; パフォーマンステスト

(ert-deftest nskk-dict-errors-test-fallback-creation-performance ()
  "フォールバック辞書作成のパフォーマンスをテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let ((start-time (float-time)))
        (nskk-dict-errors-create-fallback-index)
        (let ((elapsed (- (float-time) start-time)))
          ;; 10ms以内に完了すること
          (should (< elapsed 0.01))
          (message "Fallback dictionary creation time: %.3f ms" (* elapsed 1000))))
    (nskk-dict-errors-test--teardown)))

(ert-deftest nskk-dict-errors-test-logging-performance ()
  "ログ記録のパフォーマンスをテストする。"
  (nskk-dict-errors-test--setup)
  (unwind-protect
      (let ((nskk-dict-errors-enable-logging t)
            (nskk-dict-errors-log-file nil) ; ファイル書き込みを無効化
            (start-time (float-time)))
        ;; 1000回ログ記録
        (dotimes (_ 1000)
          (nskk-dict-errors-log 'error 'test-error "Test message"))
        (let ((elapsed (- (float-time) start-time)))
          ;; 50ms以内に完了すること
          (should (< elapsed 0.05))
          (message "1000 log entries time: %.3f ms" (* elapsed 1000))))
    (nskk-dict-errors-test--teardown)))

(provide 'nskk-dict-errors-test)

;;; nskk-dict-errors-test.el ends here
