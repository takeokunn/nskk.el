;;; nskk-learning-persist-test.el --- Tests for nskk-learning-persist -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; Tests for learning data persistence

;;; Code:

(require 'ert)
(require 'nskk-learning-persist)
(require 'nskk-test-framework)

;;; テスト用一時ディレクトリ

(defvar nskk-persist-test-dir nil
  "テスト用一時ディレクトリ。")

(defun nskk-persist-test-setup ()
  "テスト用一時ディレクトリをセットアップする。"
  (setq nskk-persist-test-dir (make-temp-file "nskk-persist-test-" t))
  (setq nskk-persist-directory nskk-persist-test-dir))

(defun nskk-persist-test-teardown ()
  "テスト用一時ディレクトリをクリーンアップする。"
  (when (and nskk-persist-test-dir
             (file-directory-p nskk-persist-test-dir))
    (delete-directory nskk-persist-test-dir t)))

;;; 頻度データの保存/読み込みテスト

(nskk-deftest nskk-persist-frequency-save-load-test
  "頻度データの保存/読み込みテスト"
  :tags '(:unit)

  (nskk-persist-test-setup)
  (unwind-protect
      (let ((nskk-persist-compression nil))  ; 圧縮無効（テスト高速化）

        ;; テストデータ作成
        (nskk-frequency-clear)
        (nskk-update-frequency "かんじ" "漢字")
        (nskk-update-frequency "かんじ" "幹事")
        (nskk-update-frequency "test" "テスト")

        (let ((original-count (hash-table-count nskk-frequency-table)))

          ;; 保存
          (nskk-persist-save-frequency)

          ;; クリア
          (nskk-frequency-clear)
          (should (= (hash-table-count nskk-frequency-table) 0))

          ;; 読み込み
          (nskk-persist-load-frequency)

          ;; 復元確認
          (should (= (hash-table-count nskk-frequency-table) original-count))
          (should (> (nskk-get-frequency-score "かんじ" "漢字") 0.0))))

    (nskk-persist-test-teardown)))

;;; 文脈データの保存/読み込みテスト

(nskk-deftest nskk-persist-context-save-load-test
  "文脈データの保存/読み込みテスト"
  :tags '(:unit)

  (nskk-persist-test-setup)
  (unwind-protect
      (let ((nskk-persist-compression nil))

        ;; テストデータ作成
        (nskk-context-clear)
        (nskk-learn-context nil "私")
        (nskk-learn-context "私" "は")
        (nskk-learn-context "は" "学生")

        (let ((original-bigrams (hash-table-count nskk-context-bigram-table))
              (original-unigrams (hash-table-count nskk-context-unigram-table)))

          ;; 保存
          (nskk-persist-save-context)

          ;; クリア
          (nskk-context-clear)
          (should (= (hash-table-count nskk-context-bigram-table) 0))

          ;; 読み込み
          (nskk-persist-load-context)

          ;; 復元確認
          (should (= (hash-table-count nskk-context-bigram-table) original-bigrams))
          (should (= (hash-table-count nskk-context-unigram-table) original-unigrams))))

    (nskk-persist-test-teardown)))

;;; 履歴データの保存/読み込みテスト

(nskk-deftest nskk-persist-history-save-load-test
  "履歴データの保存/読み込みテスト"
  :tags '(:unit)

  (nskk-persist-test-setup)
  (unwind-protect
      (let ((nskk-persist-compression nil))

        ;; テストデータ作成
        (setq nskk-history-entries nil)
        (dotimes (i 5)
          (nskk-record-history `(:midashi ,(format "test%d" i)
                                :candidate "候補")))

        (let ((original-count (length nskk-history-entries)))

          ;; 保存
          (nskk-persist-save-history)

          ;; クリア
          (setq nskk-history-entries nil)
          (should (= (length nskk-history-entries) 0))

          ;; 読み込み
          (nskk-persist-load-history)

          ;; 復元確認
          (should (= (length nskk-history-entries) original-count))))

    (nskk-persist-test-teardown)))

;;; 一括保存/読み込みテスト

(nskk-deftest nskk-persist-save-load-all-test
  "全データの一括保存/読み込みテスト"
  :tags '(:unit)

  (nskk-persist-test-setup)
  (unwind-protect
      (let ((nskk-persist-compression nil))

        ;; テストデータ作成
        (nskk-frequency-clear)
        (nskk-context-clear)
        (setq nskk-history-entries nil)

        (nskk-update-frequency "test" "テスト")
        (nskk-learn-context "私" "は")
        (nskk-record-history '(:midashi "test" :candidate "テスト"))

        ;; 一括保存
        (nskk-persist-save-all)

        ;; クリア
        (nskk-frequency-clear)
        (nskk-context-clear)
        (setq nskk-history-entries nil)

        ;; 一括読み込み
        (nskk-persist-load-all)

        ;; 復元確認
        (should (> (hash-table-count nskk-frequency-table) 0))
        (should (> (hash-table-count nskk-context-bigram-table) 0))
        (should (> (length nskk-history-entries) 0)))

    (nskk-persist-test-teardown)))

;;; バックアップテスト

(nskk-deftest nskk-persist-backup-test
  "バックアップ機能のテスト"
  :tags '(:unit)

  (nskk-persist-test-setup)
  (unwind-protect
      (let ((nskk-persist-compression nil)
            (nskk-persist-backup-count 2))

        ;; 初回保存
        (nskk-frequency-clear)
        (nskk-update-frequency "test1" "テスト1")
        (nskk-persist-save-frequency)

        (let ((filepath (nskk-persist--get-file-path nskk-persist-frequency-file)))
          (should (file-exists-p filepath))

          ;; 2回目の保存（バックアップが作成されるはず）
          (nskk-update-frequency "test2" "テスト2")
          (nskk-persist-save-frequency)

          ;; バックアップファイルの存在確認
          (should (file-exists-p (concat filepath ".bak.1")))))

    (nskk-persist-test-teardown)))

;;; 圧縮テスト

(nskk-deftest nskk-persist-compression-test
  "圧縮機能のテスト"
  :tags '(:unit)

  (skip-unless (executable-find "gzip"))

  (nskk-persist-test-setup)
  (unwind-protect
      (let ((nskk-persist-compression t))

        ;; テストデータ作成
        (nskk-frequency-clear)
        (dotimes (i 100)
          (nskk-update-frequency (format "test%d" i) "テスト"))

        ;; 保存（圧縮）
        (nskk-persist-save-frequency)

        (let* ((filepath (nskk-persist--get-file-path nskk-persist-frequency-file))
               (filepath-gz (concat filepath ".gz")))

          ;; 圧縮ファイルの存在確認
          (should (file-exists-p filepath-gz))

          ;; 読み込み
          (nskk-frequency-clear)
          (nskk-persist-load-frequency)

          ;; 復元確認
          (should (> (hash-table-count nskk-frequency-table) 0))))

    (nskk-persist-test-teardown)))

;;; 統計情報テスト

(nskk-deftest nskk-persist-statistics-test
  "統計情報のテスト"
  :tags '(:unit)

  (nskk-persist-test-setup)
  (unwind-protect
      (let ((nskk-persist-compression nil))

        ;; データ保存
        (nskk-frequency-clear)
        (nskk-update-frequency "test" "テスト")
        (nskk-persist-save-frequency)

        (let ((stats (nskk-persist-statistics)))
          (should (plist-get stats :frequency-exists))
          (should (> (plist-get stats :frequency-size) 0))))

    (nskk-persist-test-teardown)))

;;; エラー処理テスト

(nskk-deftest nskk-persist-invalid-version-test
  "不正なバージョンのエラー処理テスト"
  :tags '(:unit)

  (nskk-persist-test-setup)
  (unwind-protect
      (let ((nskk-persist-compression nil)
            (filepath (nskk-persist--get-file-path nskk-persist-frequency-file)))

        ;; 不正なバージョンのデータを作成
        (with-temp-file filepath
          (prin1 '(:version 999 :table nil) (current-buffer)))

        ;; 読み込みエラーを確認
        (should-error (nskk-persist-load-frequency)))

    (nskk-persist-test-teardown)))

(provide 'nskk-learning-persist-test)

;;; nskk-learning-persist-test.el ends here
