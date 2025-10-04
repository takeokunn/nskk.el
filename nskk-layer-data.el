;;; nskk-layer-data.el --- Data Access Layer for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, architecture
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

;; Data Access Layer - データ永続化と辞書アクセス
;;
;; 責務:
;; - 辞書データの読み書き
;; - 学習データの永続化
;; - キャッシュ管理
;; - データ同期
;; - トランザクション管理
;;
;; レイヤー依存:
;; - Infrastructure Layer (ファイルI/O、スレッド管理)
;; - 既存のnskk-dict-io, nskk-cacheを統合
;;
;; 主要コンポーネント:
;; - 辞書リポジトリ
;; - 学習データリポジトリ
;; - キャッシュマネージャー
;; - 同期マネージャー
;;
;; 使用例:
;; (nskk-data-load-dictionary "/path/to/dict")
;; (nskk-data-save-learning-data data)
;; (nskk-data-search "query")

;;; Code:

(require 'cl-lib)

;;; カスタマイズ可能変数

(defgroup nskk-data nil
  "Data access layer settings for NSKK."
  :group 'nskk
  :prefix "nskk-data-")

(defcustom nskk-data-dictionary-paths nil
  "辞書ファイルのパスリスト。"
  :type '(repeat file)
  :group 'nskk-data)

(defcustom nskk-data-learning-file "~/.emacs.d/nskk/learning.dat"
  "学習データファイルのパス。"
  :type 'file
  :group 'nskk-data)

(defcustom nskk-data-auto-save t
  "学習データを自動保存するか。"
  :type 'boolean
  :group 'nskk-data)

(defcustom nskk-data-auto-save-interval 300
  "自動保存の間隔（秒）。"
  :type 'integer
  :group 'nskk-data)

;;; 内部変数

(defvar nskk-data--dictionaries nil
  "ロード済み辞書のリスト。")

(defvar nskk-data--learning-data (make-hash-table :test 'equal)
  "学習データ。")

(defvar nskk-data--cache (make-hash-table :test 'equal)
  "検索結果のキャッシュ。")

(defvar nskk-data--auto-save-timer nil
  "自動保存タイマー。")

(defvar nskk-data--dirty-flag nil
  "データが変更されているかのフラグ。")

;;; 初期化・シャットダウン

(defun nskk-data-initialize ()
  "Data Access Layerを初期化する。"
  (nskk-data--load-dictionaries)
  (nskk-data--load-learning-data)
  (when nskk-data-auto-save
    (nskk-data--start-auto-save))
  (nskk-data--log "Data Access Layer initialized"))

(defun nskk-data-shutdown ()
  "Data Access Layerをシャットダウンする。"
  (when nskk-data-auto-save
    (nskk-data--stop-auto-save))
  (when nskk-data--dirty-flag
    (nskk-data-save-learning-data))
  (nskk-data--cleanup-cache)
  (setq nskk-data--dictionaries nil)
  (nskk-data--log "Data Access Layer shutdown"))

;;; 辞書管理

(defun nskk-data--load-dictionaries ()
  "辞書ファイルをロードする。"
  (setq nskk-data--dictionaries nil)
  (dolist (path nskk-data-dictionary-paths)
    (when (file-readable-p path)
      (condition-case err
          (let ((dict (nskk-data--load-dictionary-file path)))
            (push dict nskk-data--dictionaries)
            (nskk-data--log "Loaded dictionary: %s" path))
        (error
         (nskk-data--log "Failed to load dictionary %s: %s" path err))))))

(defun nskk-data--load-dictionary-file (path)
  "辞書ファイルをロードする。
PATHは辞書ファイルのパス。"
  ;; 既存のnskk-dict-ioを利用
  ;; 実装は統合時に完成
  (nskk-data--log "Loading dictionary file: %s" path)
  (make-hash-table :test 'equal))

(defun nskk-data-load-dictionary (path)
  "辞書ファイルをロードして追加する。
PATHは辞書ファイルのパス。"
  (interactive "fDictionary file: ")
  (when (file-readable-p path)
    (let ((dict (nskk-data--load-dictionary-file path)))
      (push dict nskk-data--dictionaries)
      (message "Dictionary loaded: %s" path))))

;;; 辞書検索

(defun nskk-data-search (query &optional options)
  "辞書を検索する。
QUERYは検索クエリ、OPTIONSは検索オプション。"
  ;; キャッシュチェック
  (let ((cache-key (cons query options)))
    (or (gethash cache-key nskk-data--cache)
        (let ((results (nskk-data--search-uncached query options)))
          (puthash cache-key results nskk-data--cache)
          results))))

(defun nskk-data--search-uncached (query options)
  "辞書を検索する（キャッシュなし）。
QUERYは検索クエリ、OPTIONSは検索オプション。"
  (let ((results nil))
    (dolist (dict nskk-data--dictionaries)
      (let ((entries (gethash query dict)))
        (when entries
          (setq results (append results entries)))))
    ;; 学習データでソート
    (nskk-data--sort-by-learning results query)))

(defun nskk-data--sort-by-learning (results query)
  "学習データに基づいて結果をソートする。
RESULTSは検索結果リスト、QUERYは検索クエリ。"
  (let ((learning-scores (gethash query nskk-data--learning-data)))
    (if learning-scores
        (sort results
              (lambda (a b)
                (> (or (gethash a learning-scores) 0)
                   (or (gethash b learning-scores) 0))))
      results)))

;;; 学習データ管理

(defun nskk-data--load-learning-data ()
  "学習データをロードする。"
  (when (file-readable-p nskk-data-learning-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents nskk-data-learning-file)
          (setq nskk-data--learning-data (read (current-buffer)))
          (nskk-data--log "Loaded learning data from %s"
                          nskk-data-learning-file))
      (error
       (nskk-data--log "Failed to load learning data: %s" err)
       (setq nskk-data--learning-data (make-hash-table :test 'equal))))))

(defun nskk-data-save-learning-data ()
  "学習データを保存する。"
  (interactive)
  (let ((dir (file-name-directory nskk-data-learning-file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (condition-case err
      (with-temp-file nskk-data-learning-file
        (prin1 nskk-data--learning-data (current-buffer))
        (setq nskk-data--dirty-flag nil)
        (nskk-data--log "Saved learning data to %s"
                        nskk-data-learning-file)
        (message "Learning data saved"))
    (error
     (nskk-data--log "Failed to save learning data: %s" err)
     (message "Failed to save learning data: %s" err))))

(defun nskk-data-learn (query candidate &optional context)
  "学習データを記録する。
QUERYは検索クエリ、CANDIDATEは選択された候補、
CONTEXTは学習コンテキスト。"
  (let ((scores (or (gethash query nskk-data--learning-data)
                    (make-hash-table :test 'equal))))
    (puthash candidate
             (1+ (or (gethash candidate scores) 0))
             scores)
    (puthash query scores nskk-data--learning-data)
    (setq nskk-data--dirty-flag t)))

;;; キャッシュ管理

(defun nskk-data--cleanup-cache ()
  "キャッシュをクリーンアップする。"
  (clrhash nskk-data--cache))

(defun nskk-data-clear-cache ()
  "検索キャッシュをクリアする。"
  (interactive)
  (nskk-data--cleanup-cache)
  (message "Data cache cleared"))

(defun nskk-data-cache-statistics ()
  "キャッシュ統計を表示する。"
  (interactive)
  (message "Cache entries: %d"
           (hash-table-count nskk-data--cache)))

;;; 自動保存

(defun nskk-data--start-auto-save ()
  "自動保存を開始する。"
  (when nskk-data--auto-save-timer
    (cancel-timer nskk-data--auto-save-timer))
  (setq nskk-data--auto-save-timer
        (run-with-timer nskk-data-auto-save-interval
                        nskk-data-auto-save-interval
                        #'nskk-data--auto-save-handler)))

(defun nskk-data--stop-auto-save ()
  "自動保存を停止する。"
  (when nskk-data--auto-save-timer
    (cancel-timer nskk-data--auto-save-timer)
    (setq nskk-data--auto-save-timer nil)))

(defun nskk-data--auto-save-handler ()
  "自動保存ハンドラー。"
  (when nskk-data--dirty-flag
    (nskk-data-save-learning-data)))

;;; データ同期

(defun nskk-data-sync ()
  "データを同期する。"
  (interactive)
  ;; 学習データを保存
  (when nskk-data--dirty-flag
    (nskk-data-save-learning-data))
  ;; キャッシュをクリア
  (nskk-data-clear-cache)
  (message "Data synchronized"))

;;; トランザクション管理

(defmacro nskk-data-with-transaction (&rest body)
  "トランザクション内で実行する。
BODYは実行する式のリスト。"
  (declare (indent 0))
  `(let ((nskk-data--transaction-active t))
     (unwind-protect
         (progn ,@body)
       (setq nskk-data--transaction-active nil))))

;;; エクスポート・インポート

(defun nskk-data-export-learning-data (file)
  "学習データをエクスポートする。
FILEはエクスポート先ファイルパス。"
  (interactive "FExport to: ")
  (with-temp-file file
    (prin1 nskk-data--learning-data (current-buffer))
    (message "Learning data exported to %s" file)))

(defun nskk-data-import-learning-data (file)
  "学習データをインポートする。
FILEはインポート元ファイルパス。"
  (interactive "fImport from: ")
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((imported-data (read (current-buffer))))
        (setq nskk-data--learning-data imported-data)
        (setq nskk-data--dirty-flag t)
        (message "Learning data imported from %s" file)))))

;;; 統計情報

(defun nskk-data-get-statistics ()
  "Data Access Layerの統計情報を取得する。"
  (interactive)
  (let ((dict-count (length nskk-data--dictionaries))
        (learning-entries (hash-table-count nskk-data--learning-data))
        (cache-entries (hash-table-count nskk-data--cache)))
    (message "Data Layer Statistics:\n  Dictionaries: %d\n  Learning entries: %d\n  Cache entries: %d\n  Dirty: %s"
             dict-count learning-entries cache-entries
             (if nskk-data--dirty-flag "yes" "no"))
    (list :dictionaries dict-count
          :learning-entries learning-entries
          :cache-entries cache-entries
          :dirty nskk-data--dirty-flag)))

;;; デバッグ・ロギング

(defvar nskk-data--debug-enabled nil
  "デバッグモードが有効かどうか。")

(defun nskk-data-enable-debug ()
  "デバッグモードを有効にする。"
  (interactive)
  (setq nskk-data--debug-enabled t)
  (message "NSKK Data Access Layer: Debug mode enabled"))

(defun nskk-data-disable-debug ()
  "デバッグモードを無効にする。"
  (interactive)
  (setq nskk-data--debug-enabled nil)
  (message "NSKK Data Access Layer: Debug mode disabled"))

(defun nskk-data--log (format-string &rest args)
  "デバッグログを出力する。
FORMAT-STRINGはフォーマット文字列、ARGSは引数。"
  (when nskk-data--debug-enabled
    (apply #'message (concat "[NSKK-Data] " format-string) args)))

;;; ヘルスチェック

(defun nskk-data-health-check ()
  "Data Access Layerのヘルスチェックを実行する。"
  (interactive)
  (let ((issues '()))
    ;; 辞書状態チェック
    (when (null nskk-data--dictionaries)
      (push "No dictionaries loaded" issues))
    ;; 学習データチェック
    (unless nskk-data--learning-data
      (push "Learning data not initialized" issues))
    ;; ファイルアクセスチェック
    (let ((dir (file-name-directory nskk-data-learning-file)))
      (unless (file-writable-p dir)
        (push (format "Learning data directory not writable: %s" dir) issues)))
    ;; 結果表示
    (if issues
        (message "Data Layer issues: %s" (string-join issues ", "))
      (message "Data Layer: All systems operational"))))

(provide 'nskk-layer-data)
;;; nskk-layer-data.el ends here
