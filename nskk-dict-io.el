;;; nskk-dict-io.el --- Dictionary file I/O for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, dictionary, io
;; Version: 0.1.0
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

;; このファイルはSKK辞書ファイルのI/O機能を実装します。
;;
;; 特徴:
;; - 辞書の安全な読み込み・書き込み
;; - チェックサムベースのキャッシュ管理
;; - 増分更新によるパフォーマンス最適化
;; - 自動バックアップ機能
;; - アトミックな書き込み（一時ファイル経由）
;; - エンコーディング処理（UTF-8/EUC-JP）
;;
;; パフォーマンス目標:
;; - 辞書読み込み: 10万エントリで < 1秒
;; - 辞書書き込み: 10万エントリで < 2秒
;; - 増分更新: 100エントリ追加で < 100ms
;; - バックアップ: < 50ms
;;
;; 使用例:
;;
;;   (require 'nskk-dict-io)
;;
;;   ;; 辞書を読み込み
;;   (let ((index (nskk-load-dictionary "~/.skk/jisyo")))
;;     ;; 検索、編集など...
;;
;;     ;; 辞書を保存
;;     (nskk-save-dictionary index "~/.skk/jisyo"))
;;
;;   ;; 増分更新
;;   (nskk-update-dictionary-incremental
;;     existing-index
;;     new-entries
;;     "~/.skk/jisyo")

;;; Code:

(require 'cl-lib)
(require 'nskk-dict-parser)
(require 'nskk-dict-struct)
(require 'nskk-dict-errors)

;;; カスタマイズ変数

(defgroup nskk-dict-io nil
  "Dictionary file I/O customization."
  :group 'nskk
  :prefix "nskk-dict-io-")

(defcustom nskk-dict-io-backup-enabled t
  "非nilの場合、辞書書き込み前に自動バックアップする。"
  :type 'boolean
  :group 'nskk-dict-io)

(defcustom nskk-dict-io-backup-keep-count 10
  "保持するバックアップ数。"
  :type 'integer
  :group 'nskk-dict-io)

(defcustom nskk-dict-io-backup-dir "~/.nskk/backups"
  "バックアップディレクトリのパス。"
  :type 'string
  :group 'nskk-dict-io)

(defcustom nskk-dict-io-default-encoding 'utf-8
  "デフォルトエンコーディング。"
  :type '(choice (const utf-8) (const euc-jp))
  :group 'nskk-dict-io)

(defcustom nskk-dict-io-cache-enabled t
  "非nilの場合、辞書読み込みをキャッシュする。"
  :type 'boolean
  :group 'nskk-dict-io)

(defcustom nskk-dict-io-checksum-method 'md5
  "チェックサム計算方法。'md5 または 'sha256。"
  :type '(choice (const md5) (const sha256))
  :group 'nskk-dict-io)

(defcustom nskk-dict-io-verbose nil
  "非nilの場合、詳細なログを出力する。"
  :type 'boolean
  :group 'nskk-dict-io)

;;; キャッシュ管理

(defvar nskk-dict-io--cache (make-hash-table :test 'equal)
  "辞書キャッシュ（パス→(インデックス構造 . メタデータ)）")

(cl-defstruct (nskk-dict-io-cache-metadata
               (:constructor nskk-dict-io-cache-metadata--create)
               (:copier nil))
  "キャッシュメタデータ。

スロット:
  checksum      - ファイルのチェックサム
  mtime         - ファイルの最終更新時刻
  cached-time   - キャッシュした時刻"
  (checksum nil :type (or null string))
  (mtime nil :type (or null number))
  (cached-time nil :type number))

;;; チェックサム計算

(defun nskk-dict-io--compute-checksum (file-path)
  "FILE-PATH のチェックサムを計算する。

戻り値:
  チェックサム文字列"
  (if (eq nskk-dict-io-checksum-method 'sha256)
      (nskk-dict-io--compute-sha256 file-path)
    (nskk-dict-io--compute-md5 file-path)))

(defun nskk-dict-io--compute-md5 (file-path)
  "FILE-PATH のMD5チェックサムを計算する。"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file-path)
    (secure-hash 'md5 (current-buffer))))

(defun nskk-dict-io--compute-sha256 (file-path)
  "FILE-PATH のSHA-256チェックサムを計算する。"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file-path)
    (secure-hash 'sha256 (current-buffer))))

(defun nskk-dict-io--get-file-mtime (file-path)
  "FILE-PATH の最終更新時刻を取得する。

戻り値:
  float-time形式の時刻"
  (float-time (file-attribute-modification-time (file-attributes file-path))))

;;; キャッシュ管理関数

(defun nskk-dict-io--cache-key (file-path)
  "キャッシュキーを生成する。

引数:
  FILE-PATH - ファイルパス

戻り値:
  キャッシュキー（正規化されたファイルパス）"
  (expand-file-name file-path))

(defun nskk-dict-io--cache-valid-p (file-path cached-metadata)
  "キャッシュが有効か判定する。

引数:
  FILE-PATH        - ファイルパス
  CACHED-METADATA  - キャッシュメタデータ

戻り値:
  キャッシュが有効な場合t、無効な場合nil

判定基準:
  1. ファイルの最終更新時刻が一致
  2. チェックサムが一致（高速化のため更新時刻チェック後に実行）"
  (let ((current-mtime (nskk-dict-io--get-file-mtime file-path))
        (cached-mtime (nskk-dict-io-cache-metadata-mtime cached-metadata)))
    (and (equal current-mtime cached-mtime)
         ;; 念のためチェックサムも確認
         (equal (nskk-dict-io--compute-checksum file-path)
                (nskk-dict-io-cache-metadata-checksum cached-metadata)))))

(defun nskk-clear-dictionary-cache (&optional file-path)
  "辞書キャッシュをクリアする。

引数:
  FILE-PATH - 指定した場合、そのファイルのキャッシュのみクリア
              未指定の場合、全キャッシュをクリア"
  (interactive)
  (if file-path
      (let ((cache-key (nskk-dict-io--cache-key file-path)))
        (remhash cache-key nskk-dict-io--cache)
        (when nskk-dict-io-verbose
          (message "Cleared cache for: %s" file-path)))
    (clrhash nskk-dict-io--cache)
    (when nskk-dict-io-verbose
      (message "Cleared all dictionary cache"))))

;;; 辞書読み込み

;;;###autoload
(defun nskk-load-dictionary (path &optional force-reload)
  "辞書ファイルPATHを読み込み、インデックス構造を返す。

引数:
  PATH         - 辞書ファイルパス
  FORCE-RELOAD - 非nilの場合、キャッシュを無視して再読み込み

処理フロー:
  1. ファイル存在確認
  2. メタデータ確認（チェックサム、最終更新時刻）
  3. キャッシュ確認（変更なければキャッシュ返却）
  4. パーサーで読み込み (nskk-parse-dictionary)
  5. データ構造に変換 (nskk-dict-struct-from-parser)
  6. キャッシュに保存
  7. インデックス構造を返す

戻り値:
  nskk-dict-index 構造体

エラー:
  ファイルが存在しない場合、エラーをシグナル"
  (unless (file-exists-p path)
    (signal 'nskk-dict-io-file-not-found (list path)))

  (unless (file-readable-p path)
    (signal 'nskk-dict-io-permission-denied (list path)))

  (let* ((cache-key (nskk-dict-io--cache-key path))
         (cached-entry (gethash cache-key nskk-dict-io--cache))
         (use-cache (and nskk-dict-io-cache-enabled
                        (not force-reload)
                        cached-entry
                        (nskk-dict-io--cache-valid-p path (cdr cached-entry)))))

    (if use-cache
        (progn
          (when nskk-dict-io-verbose
            (message "Using cached dictionary: %s" path))
          (car cached-entry))

      ;; キャッシュが無効なので新規読み込み
      (when nskk-dict-io-verbose
        (message "Loading dictionary: %s" path))

      (let* ((start-time (float-time))
             ;; パーサーで読み込み
             (parsed (nskk-parse-dictionary path))
             ;; データ構造に変換
             (index (nskk-dict-struct-from-parser parsed))
             ;; メタデータ作成
             (checksum (nskk-dict-io--compute-checksum path))
             (mtime (nskk-dict-io--get-file-mtime path))
             (cache-metadata (nskk-dict-io-cache-metadata--create
                             :checksum checksum
                             :mtime mtime
                             :cached-time (float-time)))
             (load-time (- (float-time) start-time)))

        (when nskk-dict-io-verbose
          (message "Dictionary loaded in %.3f sec: %s" load-time path))

        ;; キャッシュに保存
        (when nskk-dict-io-cache-enabled
          (puthash cache-key (cons index cache-metadata) nskk-dict-io--cache))

        index))))

;;; 辞書書き込み

(defun nskk-dict-io--write-header (encoding)
  "辞書ヘッダーを書き込む。

引数:
  ENCODING - エンコーディング ('utf-8 または 'euc-jp)

現在のバッファに書き込む。"
  (let ((coding-str (pcase encoding
                     ('utf-8 "utf-8")
                     ('euc-jp "euc-jp")
                     (_ "utf-8"))))
    (insert (format ";; -*- coding: %s -*-\n" coding-str))
    (insert ";; SKK-JISYO generated by NSKK\n")))

(defun nskk-dict-io--serialize-entry (entry)
  "エントリをSKK形式の文字列に変換する。

引数:
  ENTRY - nskk-dict-entry構造体

戻り値:
  SKK形式の文字列（改行なし）"
  (let ((midashi (nskk-dict-entry-midashi entry))
        (candidates (nskk-dict-entry-candidates entry)))
    (concat midashi
            " /"
            (mapconcat
             (lambda (cand)
               (let ((word (nskk-dict-candidate-word cand))
                     (annotation (nskk-dict-candidate-annotation cand)))
                 (if annotation
                     (format "%s;%s" word annotation)
                   word)))
             candidates
             "/")
            "/")))

(defun nskk-dict-io--collect-all-entries (index okuri-type)
  "インデックスから指定タイプの全エントリを収集する。

引数:
  INDEX      - nskk-dict-index構造体
  OKURI-TYPE - 'okuri-ari または 'okuri-nasi

戻り値:
  nskk-dict-entry構造体のリスト（見出し語でソート済み）"
  (let ((table (if (eq okuri-type 'okuri-ari)
                  (nskk-dict-index-okuri-ari-table index)
                (nskk-dict-index-okuri-nasi-table index)))
        (entries nil))
    (maphash (lambda (_key entry)
              (push entry entries))
            table)
    ;; 見出し語でソート
    (sort entries
          (lambda (a b)
            (string< (nskk-dict-entry-midashi a)
                    (nskk-dict-entry-midashi b))))))

;;;###autoload
(defun nskk-save-dictionary (index output-path &optional encoding)
  "インデックス構造INDEXを辞書ファイルOUTPUT-PATHに書き込む。

引数:
  INDEX       - nskk-dict-index構造体
  OUTPUT-PATH - 出力ファイルパス
  ENCODING    - エンコーディング（未指定の場合はnskk-dict-io-default-encodingを使用）

処理フロー:
  1. バックアップ作成（既存ファイルがあれば）
  2. 一時ファイルに書き込み
  3. エンコーディング指定
  4. SKK辞書形式でシリアライズ
  5. アトミックリネーム（一時ファイル→本ファイル）
  6. パーミッション保持

戻り値:
  OUTPUT-PATH

エラー:
  書き込み権限がない場合、エラーをシグナル"
  (let* ((output-path (expand-file-name output-path))
         (encoding (or encoding nskk-dict-io-default-encoding))
         (file-exists (file-exists-p output-path)))

    ;; ディレクトリの存在確認
    (let ((output-dir (file-name-directory output-path)))
      (unless (file-directory-p output-dir)
        (signal 'nskk-dict-io-error (list (format "Output directory does not exist: %s" output-dir)))))

    ;; 書き込み権限確認
    (when (and file-exists (not (file-writable-p output-path)))
      (signal 'nskk-dict-io-permission-denied (list output-path)))

    ;; バックアップ作成
    (when (and nskk-dict-io-backup-enabled file-exists)
      (nskk-backup-dictionary output-path))

    (when nskk-dict-io-verbose
      (message "Saving dictionary: %s" output-path))

    (let* ((start-time (float-time))
           ;; 一時ファイル作成（同じディレクトリに作成してアトミックリネームを保証）
           (temp-file (let ((default-directory (file-name-directory output-path)))
                       (make-temp-file
                        (concat (file-name-nondirectory output-path) ".")
                        nil
                        ".tmp")))
           ;; 既存ファイルのパーミッション保存
           (original-modes (when file-exists
                            (file-modes output-path))))

      (unwind-protect
          (progn
            ;; 一時ファイルに書き込み
            (with-temp-buffer
              (let ((coding-system-for-write encoding))
                ;; ヘッダー書き込み
                (nskk-dict-io--write-header encoding)
                (insert "\n")

                ;; 送り仮名ありエントリ
                (insert ";; okuri-ari entries.\n")
                (let ((okuri-ari-entries (nskk-dict-io--collect-all-entries index 'okuri-ari)))
                  (dolist (entry okuri-ari-entries)
                    (insert (nskk-dict-io--serialize-entry entry))
                    (insert "\n")))
                (insert "\n")

                ;; 送り仮名なしエントリ
                (insert ";; okuri-nasi entries.\n")
                (let ((okuri-nasi-entries (nskk-dict-io--collect-all-entries index 'okuri-nasi)))
                  (dolist (entry okuri-nasi-entries)
                    (insert (nskk-dict-io--serialize-entry entry))
                    (insert "\n")))

                ;; 一時ファイルに書き込み
                (write-region (point-min) (point-max) temp-file nil 'silent)))

            ;; アトミックリネーム
            (rename-file temp-file output-path t)

            ;; パーミッション復元
            (when original-modes
              (set-file-modes output-path original-modes))

            (let ((save-time (- (float-time) start-time)))
              (when nskk-dict-io-verbose
                (message "Dictionary saved in %.3f sec: %s" save-time output-path))))

        ;; エラー時に一時ファイルを削除
        (when (file-exists-p temp-file)
          (delete-file temp-file)))

      ;; キャッシュをクリア（ファイルが更新されたため）
      (nskk-clear-dictionary-cache output-path)

      output-path)))

;;; 増分更新

(defun nskk-dict-io--merge-entries (existing new)
  "エントリをマージする（重複排除、頻度統合）。

引数:
  EXISTING - 既存のnskk-dict-entry構造体
  NEW      - 新しいnskk-dict-entry構造体

戻り値:
  マージされたnskk-dict-entry構造体

マージロジック:
  - 候補は重複排除（単語が同じものは1つに）
  - 新しい候補を既存候補の前に配置（学習順を反映）
  - 頻度情報を統合"
  (let* ((existing-candidates (nskk-dict-entry-candidates existing))
         (new-candidates (nskk-dict-entry-candidates new))
         ;; 既存候補を単語→候補のマップに変換
         (candidate-map (make-hash-table :test 'equal))
         (merged-candidates nil))

    ;; 既存候補をマップに登録
    (dolist (cand existing-candidates)
      (puthash (nskk-dict-candidate-word cand) cand candidate-map))

    ;; 新しい候補を処理（重複があれば上書き）
    (dolist (cand new-candidates)
      (puthash (nskk-dict-candidate-word cand) cand candidate-map))

    ;; 新しい候補を優先してマージ（学習順）
    (dolist (cand new-candidates)
      (let ((word (nskk-dict-candidate-word cand)))
        (push (gethash word candidate-map) merged-candidates)))

    ;; 既存候補で新規に含まれないものを追加
    (dolist (cand existing-candidates)
      (let ((word (nskk-dict-candidate-word cand)))
        (unless (cl-find word new-candidates
                        :key #'nskk-dict-candidate-word
                        :test #'equal)
          (push cand merged-candidates))))

    ;; マージされたエントリを作成
    (nskk-dict-entry--create
     :midashi (nskk-dict-entry-midashi existing)
     :candidates (nreverse merged-candidates)
     :frequency (+ (nskk-dict-entry-frequency existing)
                  (nskk-dict-entry-frequency new))
     :last-used (or (nskk-dict-entry-last-used new)
                   (nskk-dict-entry-last-used existing))
     :okuri-type (nskk-dict-entry-okuri-type existing)
     :metadata (nskk-dict-entry-metadata existing))))

;;;###autoload
(defun nskk-update-dictionary-incremental (index new-entries output-path)
  "既存辞書INDEXにNEW-ENTRIESをマージして保存。

引数:
  INDEX       - 既存のnskk-dict-index構造体
  NEW-ENTRIES - 新しいエントリのリスト（nskk-dict-entryのリスト）
  OUTPUT-PATH - 出力ファイルパス

処理フロー:
  1. 既存エントリとマージ
  2. 重複チェック
  3. 頻度情報の統合
  4. ソート（見出し語順）
  5. 書き込み

戻り値:
  更新されたnskk-dict-index構造体"
  (when nskk-dict-io-verbose
    (message "Incremental update: %d new entries" (length new-entries)))

  (let ((start-time (float-time)))
    ;; 新しいエントリをインデックスにマージ
    (dolist (new-entry new-entries)
      (let* ((midashi (nskk-dict-entry-midashi new-entry))
             (okuri-type (nskk-dict-entry-okuri-type new-entry))
             (table (if (eq okuri-type 'okuri-ari)
                       (nskk-dict-index-okuri-ari-table index)
                     (nskk-dict-index-okuri-nasi-table index)))
             (existing-entry (gethash midashi table)))

        (if existing-entry
            ;; 既存エントリとマージ
            (let ((merged (nskk-dict-io--merge-entries existing-entry new-entry)))
              (puthash midashi merged table))
          ;; 新規エントリとして追加
          (puthash midashi new-entry table))))

    ;; 辞書を保存
    (nskk-save-dictionary index output-path)

    (let ((update-time (- (float-time) start-time)))
      (when nskk-dict-io-verbose
        (message "Incremental update completed in %.3f sec" update-time)))

    index))

;;; バックアップ機能

(defun nskk-dict-io--cleanup-old-backups (backup-dir keep-count)
  "古いバックアップを削除する。

引数:
  BACKUP-DIR - バックアップディレクトリ
  KEEP-COUNT - 保持するバックアップ数

保持数を超えるバックアップを古い順に削除する。"
  (let* ((backup-files (directory-files backup-dir t "\\.bak$"))
         ;; 最終更新時刻でソート（新しい順）
         (sorted-files (sort backup-files
                            (lambda (a b)
                              (time-less-p (file-attribute-modification-time
                                           (file-attributes b))
                                          (file-attribute-modification-time
                                           (file-attributes a))))))
         ;; 削除対象ファイル
         (files-to-delete (nthcdr keep-count sorted-files)))

    (dolist (file files-to-delete)
      (when nskk-dict-io-verbose
        (message "Deleting old backup: %s" file))
      (delete-file file))))

;;;###autoload
(defun nskk-backup-dictionary (file-path &optional backup-dir)
  "辞書ファイルをバックアップする。

引数:
  FILE-PATH  - バックアップ対象のファイルパス
  BACKUP-DIR - バックアップディレクトリ（未指定の場合はnskk-dict-io-backup-dirを使用）

バックアップ命名:
  <basename>.YYYYMMDD-HHMMSS.bak

処理:
  1. バックアップディレクトリ作成
  2. タイムスタンプ付きコピー
  3. 古いバックアップの自動削除（設定可能）

戻り値:
  バックアップファイルパス"
  (unless (file-exists-p file-path)
    (signal 'nskk-dict-io-file-not-found (list file-path)))

  (let* ((backup-dir (expand-file-name (or backup-dir nskk-dict-io-backup-dir)))
         (basename (file-name-nondirectory file-path))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (backup-name (format "%s.%s.bak" basename timestamp))
         (backup-path (expand-file-name backup-name backup-dir)))

    ;; バックアップディレクトリ作成
    (unless (file-directory-p backup-dir)
      (make-directory backup-dir t))

    ;; ファイルをコピー
    (copy-file file-path backup-path t)

    (when nskk-dict-io-verbose
      (message "Backup created: %s" backup-path))

    ;; 古いバックアップを削除
    (nskk-dict-io--cleanup-old-backups backup-dir nskk-dict-io-backup-keep-count)

    backup-path))

;;; ユーティリティ関数

(defun nskk-dict-io-statistics (file-path)
  "辞書ファイルの統計情報を返す。

引数:
  FILE-PATH - 辞書ファイルパス

戻り値:
  plist形式の統計情報
    :file-path     - ファイルパス
    :file-size     - ファイルサイズ（バイト）
    :checksum      - チェックサム
    :mtime         - 最終更新時刻
    :cached        - キャッシュされているか"
  (unless (file-exists-p file-path)
    (signal 'nskk-dict-io-file-not-found (list file-path)))

  (let* ((cache-key (nskk-dict-io--cache-key file-path))
         (cached-entry (gethash cache-key nskk-dict-io--cache))
         (file-attrs (file-attributes file-path)))
    (list :file-path file-path
          :file-size (file-attribute-size file-attrs)
          :checksum (nskk-dict-io--compute-checksum file-path)
          :mtime (float-time (file-attribute-modification-time file-attrs))
          :cached (not (null cached-entry)))))

(provide 'nskk-dict-io)

;;; nskk-dict-io.el ends here
