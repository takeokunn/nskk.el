;;; nskk-dict-io-test.el --- Tests for nskk-dict-io -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, dictionary, io, test

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

;; このファイルは nskk-dict-io.el のテストスイートです。
;;
;; テスト項目:
;; - 辞書読み込み機能
;; - 辞書書き込み機能
;; - 増分更新機能
;; - バックアップ機能
;; - キャッシュ管理
;; - チェックサム計算
;; - パフォーマンス測定

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-test-framework)
(require 'nskk-dict-io)
(require 'nskk-dict-parser)
(require 'nskk-dict-struct)

;;; テストヘルパー関数

(defun nskk-dict-io-test--create-temp-dict-file (entries &optional encoding)
  "テスト用の一時辞書ファイルを作成する。

引数:
  ENTRIES  - エントリのリスト ((midashi . candidates) ...)
             candidates: ((word . annotation) ...)
  ENCODING - エンコーディング（デフォルト: utf-8）

戻り値:
  一時ファイルのパス"
  (let* ((temp-file (make-temp-file "nskk-test-dict-" nil ".jisyo"))
         (encoding (or encoding 'utf-8)))
    (with-temp-file temp-file
      (let ((coding-system-for-write encoding))
        ;; ヘッダー
        (insert (format ";; -*- coding: %s -*-\n" encoding))
        (insert ";; SKK-JISYO test dictionary\n\n")

        ;; 送り仮名ありエントリ
        (insert ";; okuri-ari entries.\n")
        (dolist (entry entries)
          (when (nskk-dict-io-test--is-okuri-ari-p (car entry))
            (insert (nskk-dict-io-test--format-entry entry))
            (insert "\n")))
        (insert "\n")

        ;; 送り仮名なしエントリ
        (insert ";; okuri-nasi entries.\n")
        (dolist (entry entries)
          (unless (nskk-dict-io-test--is-okuri-ari-p (car entry))
            (insert (nskk-dict-io-test--format-entry entry))
            (insert "\n")))))
    temp-file))

(defun nskk-dict-io-test--is-okuri-ari-p (midashi)
  "見出し語が送り仮名ありか判定する。"
  (and (> (length midashi) 0)
       (let ((last-char (aref midashi (1- (length midashi)))))
         (and (>= last-char ?a) (<= last-char ?z)))))

(defun nskk-dict-io-test--format-entry (entry)
  "エントリをSKK形式に変換する。

引数:
  ENTRY - (midashi . candidates)形式
          candidates: ((word . annotation) ...)"
  (let ((midashi (car entry))
        (candidates (cdr entry)))
    (concat midashi
            " /"
            (mapconcat (lambda (cand)
                        (if (cdr cand)
                            (format "%s;%s" (car cand) (cdr cand))
                          (car cand)))
                      candidates
                      "/")
            "/")))

(defun nskk-dict-io-test--create-test-index ()
  "テスト用のインデックス構造を作成する。"
  (let* ((okuri-ari-table (make-hash-table :test 'equal))
         (okuri-nasi-table (make-hash-table :test 'equal))
         ;; 送り仮名ありエントリ
         (okuri-ari-entries
          (list
           (nskk-dict-entry-create "わたr" '(("渡" . nil) ("航" . nil)) 'okuri-ari)
           (nskk-dict-entry-create "かえr" '(("帰" . nil) ("返" . nil)) 'okuri-ari)))
         ;; 送り仮名なしエントリ
         (okuri-nasi-entries
          (list
           (nskk-dict-entry-create "かんじ" '(("漢字" . nil) ("幹事" . nil)) 'okuri-nasi)
           (nskk-dict-entry-create "あい" '(("愛" . "love") ("哀" . "sorrow")) 'okuri-nasi))))

    ;; ハッシュテーブルに登録
    (dolist (entry okuri-ari-entries)
      (puthash (nskk-dict-entry-midashi entry) entry okuri-ari-table))
    (dolist (entry okuri-nasi-entries)
      (puthash (nskk-dict-entry-midashi entry) entry okuri-nasi-table))

    ;; インデックス構造作成
    (nskk-dict-index--create
     :okuri-ari-table okuri-ari-table
     :okuri-nasi-table okuri-nasi-table)))

;;; チェックサム計算テスト

(nskk-deftest nskk-dict-io-test-checksum-md5
  "MD5チェックサムの計算"
  :tags '(:unit :dict-io)
  (let* ((temp-file (make-temp-file "nskk-test-"))
         (nskk-dict-io-checksum-method 'md5))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "test content"))
          (let ((checksum (nskk-dict-io--compute-checksum temp-file)))
            (should (stringp checksum))
            (should (= (length checksum) 32))))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-io-test-checksum-sha256
  "SHA-256チェックサムの計算"
  :tags '(:unit :dict-io)
  (let* ((temp-file (make-temp-file "nskk-test-"))
         (nskk-dict-io-checksum-method 'sha256))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "test content"))
          (let ((checksum (nskk-dict-io--compute-checksum temp-file)))
            (should (stringp checksum))
            (should (= (length checksum) 64))))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-io-test-checksum-consistency
  "同じファイルで同じチェックサムが得られることを確認"
  :tags '(:unit :dict-io)
  (let ((temp-file (make-temp-file "nskk-test-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "test content"))
          (let ((checksum1 (nskk-dict-io--compute-checksum temp-file))
                (checksum2 (nskk-dict-io--compute-checksum temp-file)))
            (should (equal checksum1 checksum2))))
      (delete-file temp-file))))

;;; 辞書読み込みテスト

(nskk-deftest nskk-dict-io-test-load-dictionary-basic
  "基本的な辞書読み込み"
  :tags '(:unit :dict-io)
  (let ((temp-file (nskk-dict-io-test--create-temp-dict-file
                    '(("かんじ" . (("漢字" . nil) ("幹事" . nil)))
                      ("あい" . (("愛" . "love") ("哀" . "sorrow")))))))
    (unwind-protect
        (let ((index (nskk-load-dictionary temp-file)))
          (should (nskk-dict-index-p index))
          ;; 基本検索
          (let ((entry (nskk-dict-struct-lookup index "かんじ")))
            (should (nskk-dict-entry-p entry))
            (should (equal (nskk-dict-entry-midashi entry) "かんじ"))
            (should (= (length (nskk-dict-entry-candidates entry)) 2))))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-io-test-load-dictionary-cache
  "辞書キャッシュの動作確認"
  :tags '(:unit :dict-io)
  (let ((temp-file (nskk-dict-io-test--create-temp-dict-file
                    '(("かんじ" . (("漢字" . nil))))))
        (nskk-dict-io-cache-enabled t))
    (unwind-protect
        (progn
          ;; 初回読み込み
          (let ((index1 (nskk-load-dictionary temp-file)))
            (should (nskk-dict-index-p index1))
            ;; 2回目はキャッシュから
            (let ((index2 (nskk-load-dictionary temp-file)))
              (should (eq index1 index2)))))
      (nskk-clear-dictionary-cache temp-file)
      (delete-file temp-file))))

(nskk-deftest nskk-dict-io-test-load-dictionary-force-reload
  "強制再読み込み"
  :tags '(:unit :dict-io)
  (let ((temp-file (nskk-dict-io-test--create-temp-dict-file
                    '(("かんじ" . (("漢字" . nil))))))
        (nskk-dict-io-cache-enabled t))
    (unwind-protect
        (progn
          ;; 初回読み込み
          (let ((index1 (nskk-load-dictionary temp-file)))
            (should (nskk-dict-index-p index1))
            ;; 強制再読み込み
            (let ((index2 (nskk-load-dictionary temp-file t)))
              (should (nskk-dict-index-p index2))
              ;; 異なるオブジェクトであることを確認
              (should-not (eq index1 index2)))))
      (nskk-clear-dictionary-cache temp-file)
      (delete-file temp-file))))

(nskk-deftest nskk-dict-io-test-load-dictionary-not-found
  "存在しないファイルの読み込み"
  :tags '(:unit :dict-io)
  (should-error (nskk-load-dictionary "/nonexistent/path.jisyo")))

;;; 辞書書き込みテスト

(nskk-deftest nskk-dict-io-test-save-dictionary-basic
  "基本的な辞書書き込み"
  :tags '(:unit :dict-io)
  (let* ((temp-file (make-temp-file "nskk-test-dict-" nil ".jisyo"))
         (index (nskk-dict-io-test--create-test-index))
         (nskk-dict-io-backup-enabled nil))
    (unwind-protect
        (progn
          ;; 書き込み
          (nskk-save-dictionary index temp-file)
          (should (file-exists-p temp-file))
          ;; 読み込んで確認
          (let ((loaded-index (nskk-load-dictionary temp-file)))
            (should (nskk-dict-index-p loaded-index))
            ;; エントリ数確認
            (should (= (nskk-dict-struct-entry-count loaded-index 'okuri-ari) 2))
            (should (= (nskk-dict-struct-entry-count loaded-index 'okuri-nasi) 2))))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-io-test-save-dictionary-encoding
  "エンコーディング指定での書き込み"
  :tags '(:unit :dict-io)
  (let* ((temp-file (make-temp-file "nskk-test-dict-" nil ".jisyo"))
         (index (nskk-dict-io-test--create-test-index))
         (nskk-dict-io-backup-enabled nil))
    (unwind-protect
        (progn
          ;; UTF-8で書き込み
          (nskk-save-dictionary index temp-file 'utf-8)
          (should (file-exists-p temp-file))
          ;; エンコーディング確認
          (with-temp-buffer
            (insert-file-contents temp-file)
            (goto-char (point-min))
            (should (re-search-forward "coding: utf-8" nil t))))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-io-test-save-dictionary-round-trip
  "読み込み→書き込み→読み込みのラウンドトリップ"
  :tags '(:unit :dict-io)
  (let* ((temp-file1 (nskk-dict-io-test--create-temp-dict-file
                      '(("かんじ" . (("漢字" . nil) ("幹事" . nil)))
                        ("あい" . (("愛" . "love") ("哀" . "sorrow")))
                        ("わたr" . (("渡" . nil) ("航" . nil))))))
         (temp-file2 (make-temp-file "nskk-test-dict2-" nil ".jisyo"))
         (nskk-dict-io-backup-enabled nil))
    (unwind-protect
        (progn
          ;; 読み込み
          (let ((index1 (nskk-load-dictionary temp-file1)))
            ;; 書き込み
            (nskk-save-dictionary index1 temp-file2)
            ;; 再読み込み
            (let ((index2 (nskk-load-dictionary temp-file2)))
              ;; エントリ数が一致することを確認
              (should (= (nskk-dict-struct-entry-count index1)
                        (nskk-dict-struct-entry-count index2)))
              ;; 内容確認
              (let ((entry1 (nskk-dict-struct-lookup index1 "かんじ"))
                    (entry2 (nskk-dict-struct-lookup index2 "かんじ")))
                (should (equal (nskk-dict-entry-midashi entry1)
                              (nskk-dict-entry-midashi entry2)))
                (should (= (length (nskk-dict-entry-candidates entry1))
                          (length (nskk-dict-entry-candidates entry2))))))))
      (delete-file temp-file1)
      (delete-file temp-file2))))

;;; 増分更新テスト

(nskk-deftest nskk-dict-io-test-incremental-update-new-entry
  "新規エントリの追加"
  :tags '(:unit :dict-io)
  (let* ((temp-file (make-temp-file "nskk-test-dict-" nil ".jisyo"))
         (index (nskk-dict-io-test--create-test-index))
         (new-entries (list
                      (nskk-dict-entry-create "あたらしい"
                                             '(("新しい" . nil))
                                             'okuri-nasi)))
         (nskk-dict-io-backup-enabled nil))
    (unwind-protect
        (progn
          ;; 初期書き込み
          (nskk-save-dictionary index temp-file)
          ;; 増分更新
          (nskk-update-dictionary-incremental index new-entries temp-file)
          ;; 読み込んで確認
          (let ((loaded-index (nskk-load-dictionary temp-file t)))
            (let ((entry (nskk-dict-struct-lookup loaded-index "あたらしい")))
              (should (nskk-dict-entry-p entry))
              (should (equal (nskk-dict-entry-midashi entry) "あたらしい")))))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-io-test-incremental-update-merge
  "既存エントリのマージ"
  :tags '(:unit :dict-io)
  (let* ((temp-file (make-temp-file "nskk-test-dict-" nil ".jisyo"))
         (index (nskk-dict-io-test--create-test-index))
         ;; 既存の「かんじ」エントリに新しい候補を追加
         (new-entries (list
                      (nskk-dict-entry-create "かんじ"
                                             '(("感じ" . nil))
                                             'okuri-nasi)))
         (nskk-dict-io-backup-enabled nil))
    (unwind-protect
        (progn
          ;; 初期書き込み
          (nskk-save-dictionary index temp-file)
          ;; 増分更新
          (nskk-update-dictionary-incremental index new-entries temp-file)
          ;; 読み込んで確認
          (let* ((loaded-index (nskk-load-dictionary temp-file t))
                 (entry (nskk-dict-struct-lookup loaded-index "かんじ"))
                 (candidates (nskk-dict-entry-candidates entry)))
            (should (nskk-dict-entry-p entry))
            ;; 新しい候補が含まれることを確認
            (should (cl-find "感じ" candidates
                            :key #'nskk-dict-candidate-word
                            :test #'equal))
            ;; 既存の候補も保持されることを確認
            (should (cl-find "漢字" candidates
                            :key #'nskk-dict-candidate-word
                            :test #'equal))))
      (delete-file temp-file))))

;;; バックアップテスト

(nskk-deftest nskk-dict-io-test-backup-basic
  "基本的なバックアップ機能"
  :tags '(:unit :dict-io)
  (let* ((temp-file (make-temp-file "nskk-test-dict-" nil ".jisyo"))
         (backup-dir (make-temp-file "nskk-test-backup-" t)))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "test content"))
          (let ((backup-file (nskk-backup-dictionary temp-file backup-dir)))
            (should (file-exists-p backup-file))
            (should (string-prefix-p backup-dir backup-file))
            ;; バックアップ内容の確認
            (with-temp-buffer
              (insert-file-contents backup-file)
              (should (equal (buffer-string) "test content")))))
      (delete-file temp-file)
      (delete-directory backup-dir t))))

(nskk-deftest nskk-dict-io-test-backup-cleanup
  "古いバックアップの削除"
  :tags '(:unit :dict-io)
  (let* ((temp-file (make-temp-file "nskk-test-dict-" nil ".jisyo"))
         (backup-dir (make-temp-file "nskk-test-backup-" t))
         (nskk-dict-io-backup-keep-count 3))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "test content"))
          ;; 5個のバックアップを作成
          (dotimes (i 5)
            (sleep-for 0.01) ; タイムスタンプを異ならせる
            (nskk-backup-dictionary temp-file backup-dir))
          ;; バックアップファイル数を確認
          (let ((backup-files (directory-files backup-dir nil "\\.bak$")))
            (should (<= (length backup-files) nskk-dict-io-backup-keep-count))))
      (delete-file temp-file)
      (delete-directory backup-dir t))))

(nskk-deftest nskk-dict-io-test-backup-auto
  "自動バックアップの動作確認"
  :tags '(:unit :dict-io)
  (let* ((temp-file (make-temp-file "nskk-test-dict-" nil ".jisyo"))
         (backup-dir (make-temp-file "nskk-test-backup-" t))
         (nskk-dict-io-backup-enabled t)
         (nskk-dict-io-backup-dir backup-dir)
         (index (nskk-dict-io-test--create-test-index)))
    (unwind-protect
        (progn
          ;; 初回書き込み（バックアップなし）
          (nskk-save-dictionary index temp-file)
          (should (file-exists-p temp-file))
          ;; 2回目の書き込み（バックアップあり）
          (sleep-for 0.01)
          (nskk-save-dictionary index temp-file)
          ;; バックアップが作成されたことを確認
          (let ((backup-files (directory-files backup-dir nil "\\.bak$")))
            (should (> (length backup-files) 0))))
      (delete-file temp-file)
      (delete-directory backup-dir t))))

;;; キャッシュ管理テスト

(nskk-deftest nskk-dict-io-test-cache-clear-specific
  "特定ファイルのキャッシュクリア"
  :tags '(:unit :dict-io)
  (let ((temp-file (nskk-dict-io-test--create-temp-dict-file
                    '(("かんじ" . (("漢字" . nil))))))
        (nskk-dict-io-cache-enabled t))
    (unwind-protect
        (progn
          ;; キャッシュに読み込み
          (nskk-load-dictionary temp-file)
          ;; キャッシュクリア
          (nskk-clear-dictionary-cache temp-file)
          ;; 再読み込み（キャッシュなし）
          (let ((index (nskk-load-dictionary temp-file)))
            (should (nskk-dict-index-p index))))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-io-test-cache-clear-all
  "全キャッシュクリア"
  :tags '(:unit :dict-io)
  (let ((temp-file1 (nskk-dict-io-test--create-temp-dict-file
                     '(("かんじ" . (("漢字" . nil))))))
        (temp-file2 (nskk-dict-io-test--create-temp-dict-file
                     '(("あい" . (("愛" . nil))))))
        (nskk-dict-io-cache-enabled t))
    (unwind-protect
        (progn
          ;; 両方をキャッシュに読み込み
          (nskk-load-dictionary temp-file1)
          (nskk-load-dictionary temp-file2)
          ;; 全キャッシュクリア
          (nskk-clear-dictionary-cache)
          ;; 両方とも再読み込み
          (let ((index1 (nskk-load-dictionary temp-file1))
                (index2 (nskk-load-dictionary temp-file2)))
            (should (nskk-dict-index-p index1))
            (should (nskk-dict-index-p index2))))
      (delete-file temp-file1)
      (delete-file temp-file2))))

;;; パフォーマンステスト

(nskk-deftest nskk-dict-io-test-performance-load
  "辞書読み込みパフォーマンス（10000エントリ）"
  :tags '(:performance :slow :dict-io)
  (let* ((entries nil))
    ;; 10000エントリ生成
    (dotimes (i 10000)
      (push (cons (format "test%05d" i)
                 (list (cons (format "テスト%d" i) nil)))
           entries))
    (let ((temp-file (nskk-dict-io-test--create-temp-dict-file entries)))
      (unwind-protect
          (let ((start-time (float-time)))
            (nskk-load-dictionary temp-file)
            (let ((elapsed (- (float-time) start-time)))
              (message "Load time for 10000 entries: %.3f sec" elapsed)
              ;; 1秒以内に完了することを確認
              (should (< elapsed 1.0))))
        (delete-file temp-file)))))

(nskk-deftest nskk-dict-io-test-performance-save
  "辞書書き込みパフォーマンス（10000エントリ）"
  :tags '(:performance :slow :dict-io)
  (let* ((temp-file (make-temp-file "nskk-test-dict-" nil ".jisyo"))
         (index (nskk-dict-index--create
                :okuri-ari-table (make-hash-table :test 'equal)
                :okuri-nasi-table (make-hash-table :test 'equal)))
         (nskk-dict-io-backup-enabled nil))
    ;; 10000エントリを追加
    (dotimes (i 10000)
      (let ((entry (nskk-dict-entry-create
                   (format "test%05d" i)
                   (list (cons (format "テスト%d" i) nil))
                   'okuri-nasi)))
        (puthash (nskk-dict-entry-midashi entry)
                entry
                (nskk-dict-index-okuri-nasi-table index))))
    (unwind-protect
        (let ((start-time (float-time)))
          (nskk-save-dictionary index temp-file)
          (let ((elapsed (- (float-time) start-time)))
            (message "Save time for 10000 entries: %.3f sec" elapsed)
            ;; 2秒以内に完了することを確認
            (should (< elapsed 2.0))))
      (delete-file temp-file))))

(nskk-deftest nskk-dict-io-test-performance-incremental
  "増分更新パフォーマンス（100エントリ追加）"
  :tags '(:performance :dict-io)
  (let* ((temp-file (make-temp-file "nskk-test-dict-" nil ".jisyo"))
         (index (nskk-dict-io-test--create-test-index))
         (new-entries nil)
         (nskk-dict-io-backup-enabled nil))
    ;; 100エントリ生成
    (dotimes (i 100)
      (push (nskk-dict-entry-create
            (format "new%03d" i)
            (list (cons (format "新%d" i) nil))
            'okuri-nasi)
           new-entries))
    (unwind-protect
        (progn
          ;; 初期書き込み
          (nskk-save-dictionary index temp-file)
          ;; 増分更新のパフォーマンス測定
          (let ((start-time (float-time)))
            (nskk-update-dictionary-incremental index new-entries temp-file)
            (let ((elapsed (- (float-time) start-time)))
              (message "Incremental update time for 100 entries: %.3f sec" elapsed)
              ;; 100ms以内に完了することを確認
              (should (< elapsed 0.1)))))
      (delete-file temp-file))))

;;; エラーハンドリングテスト

(nskk-deftest nskk-dict-io-test-error-directory-not-exists
  "出力ディレクトリが存在しない場合のエラー"
  :tags '(:unit :dict-io)
  (let ((index (nskk-dict-io-test--create-test-index)))
    (should-error (nskk-save-dictionary index "/nonexistent/dir/test.jisyo"))))

(nskk-deftest nskk-dict-io-test-statistics
  "辞書統計情報の取得"
  :tags '(:unit :dict-io)
  (let ((temp-file (nskk-dict-io-test--create-temp-dict-file
                    '(("かんじ" . (("漢字" . nil)))))))
    (unwind-protect
        (let ((stats (nskk-dict-io-statistics temp-file)))
          (should (plist-get stats :file-path))
          (should (plist-get stats :file-size))
          (should (plist-get stats :checksum))
          (should (plist-get stats :mtime)))
      (delete-file temp-file))))

(provide 'nskk-dict-io-test)

;;; nskk-dict-io-test.el ends here
