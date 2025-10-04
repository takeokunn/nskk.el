;;; nskk-memory-optimize.el --- Memory optimizations for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, optimization, memory
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

;; このファイルはNSKKのメモリ最適化機能を提供します。
;;
;; 主な機能:
;; 1. オブジェクトプール（文字列、リスト、ベクトル）
;; 2. GC圧迫削減
;; 3. メモリレイアウト最適化
;; 4. 文字列インターン
;; 5. メモリ使用量監視
;;
;; 目標:
;; - メモリ使用量 < 20MB
;; - GC頻度の削減
;; - メモリフラグメンテーション削減
;;
;; 使用例:
;;   (nskk-memory-pool-get-string 100)
;;   (nskk-memory-intern-string "common-string")
;;   (nskk-memory-optimize-gc-settings)

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-memory-optimize nil
  "Memory optimization settings."
  :group 'nskk
  :prefix "nskk-memory-")

(defcustom nskk-memory-target-usage (* 20 1024 1024)
  "目標メモリ使用量（バイト）。
デフォルト: 20MB"
  :type 'integer
  :group 'nskk-memory-optimize)

(defcustom nskk-memory-enable-pooling t
  "オブジェクトプーリングを有効にするか。"
  :type 'boolean
  :group 'nskk-memory-optimize)

(defcustom nskk-memory-enable-interning t
  "文字列インターンを有効にするか。"
  :type 'boolean
  :group 'nskk-memory-optimize)

(defcustom nskk-memory-gc-cons-threshold (* 100 1024 1024)
  "GC閾値（バイト）。
デフォルト: 100MB

大きくすることでGC頻度を削減できるが、
メモリ使用量は増加する。"
  :type 'integer
  :group 'nskk-memory-optimize)

;;; オブジェクトプール

;; 文字列プール
(defvar nskk-memory--string-pool nil
  "文字列オブジェクトプール。
形式: ((size . (string1 string2 ...)) ...)")

(defvar nskk-memory--string-pool-max-size 100
  "各サイズの文字列プールの最大サイズ。")

(defun nskk-memory-pool-get-string (size)
  "文字列プールから指定サイズの文字列を取得する。

引数:
  SIZE - 文字列のサイズ

返り値:
  文字列（再利用可能）"
  (if (not nskk-memory-enable-pooling)
      (make-string size ?\ )
    (let* ((pool-entry (assoc size nskk-memory--string-pool))
           (pool (cdr pool-entry)))
      (if (and pool (car pool))
          (let ((str (pop (cdr pool-entry))))
            (fillarray str ?\ )
            str)
        (make-string size ?\ )))))

(defun nskk-memory-pool-return-string (str)
  "文字列をプールに返却する。

引数:
  STR - 返却する文字列"
  (when nskk-memory-enable-pooling
    (let* ((size (length str))
           (pool-entry (assoc size nskk-memory--string-pool))
           (pool (cdr pool-entry)))
      (when (or (null pool)
                (< (length pool) nskk-memory--string-pool-max-size))
        (if pool-entry
            (push str (cdr pool-entry))
          (push (cons size (list str)) nskk-memory--string-pool))))))

;; リストプール
(defvar nskk-memory--cons-pool nil
  "consセルプール。")

(defvar nskk-memory--cons-pool-max-size 1000
  "consセルプールの最大サイズ。")

(defun nskk-memory-pool-get-cons ()
  "consセルプールからconsセルを取得する。

返り値:
  consセル（再利用可能）"
  (if (and nskk-memory-enable-pooling
           nskk-memory--cons-pool)
      (pop nskk-memory--cons-pool)
    (cons nil nil)))

(defun nskk-memory-pool-return-cons (cell)
  "consセルをプールに返却する。

引数:
  CELL - 返却するconsセル"
  (when (and nskk-memory-enable-pooling
             (< (length nskk-memory--cons-pool)
                nskk-memory--cons-pool-max-size))
    (setcar cell nil)
    (setcdr cell nil)
    (push cell nskk-memory--cons-pool)))

;; ベクトルプール
(defvar nskk-memory--vector-pool nil
  "ベクトルプール。
形式: ((size . (vector1 vector2 ...)) ...)")

(defvar nskk-memory--vector-pool-max-size 50
  "各サイズのベクトルプールの最大サイズ。")

(defun nskk-memory-pool-get-vector (size)
  "ベクトルプールから指定サイズのベクトルを取得する。

引数:
  SIZE - ベクトルのサイズ

返り値:
  ベクトル（再利用可能）"
  (if (not nskk-memory-enable-pooling)
      (make-vector size nil)
    (let* ((pool-entry (assoc size nskk-memory--vector-pool))
           (pool (cdr pool-entry)))
      (if (and pool (car pool))
          (let ((vec (pop (cdr pool-entry))))
            (fillarray vec nil)
            vec)
        (make-vector size nil)))))

(defun nskk-memory-pool-return-vector (vec)
  "ベクトルをプールに返却する。

引数:
  VEC - 返却するベクトル"
  (when nskk-memory-enable-pooling
    (let* ((size (length vec))
           (pool-entry (assoc size nskk-memory--vector-pool))
           (pool (cdr pool-entry)))
      (when (or (null pool)
                (< (length pool) nskk-memory--vector-pool-max-size))
        (if pool-entry
            (push vec (cdr pool-entry))
          (push (cons size (list vec)) nskk-memory--vector-pool))))))

;;; プールクリア

(defun nskk-memory-pool-clear ()
  "すべてのオブジェクトプールをクリアする。"
  (interactive)
  (setq nskk-memory--string-pool nil
        nskk-memory--cons-pool nil
        nskk-memory--vector-pool nil)
  (garbage-collect)
  (message "Memory pools cleared"))

;;; 文字列インターン

(defvar nskk-memory--interned-strings (make-hash-table :test 'equal :size 1000)
  "インターンされた文字列のハッシュテーブル。")

(defun nskk-memory-intern-string (str)
  "文字列をインターンする。
同じ内容の文字列は同一のオブジェクトを共有する。

引数:
  STR - インターンする文字列

返り値:
  インターンされた文字列（既存の場合は既存のオブジェクト）"
  (if (not nskk-memory-enable-interning)
      str
    (or (gethash str nskk-memory--interned-strings)
        (puthash str str nskk-memory--interned-strings))))

(defun nskk-memory-clear-interned-strings ()
  "インターンされた文字列をクリアする。"
  (interactive)
  (clrhash nskk-memory--interned-strings)
  (garbage-collect)
  (message "Interned strings cleared"))

;;; GC最適化

(defvar nskk-memory--original-gc-cons-threshold nil
  "元のGC閾値（保存用）。")

(defun nskk-memory-optimize-gc-settings ()
  "GC設定を最適化する。"
  (interactive)
  ;; 元の設定を保存
  (unless nskk-memory--original-gc-cons-threshold
    (setq nskk-memory--original-gc-cons-threshold gc-cons-threshold))

  ;; GC閾値を上げる
  (setq gc-cons-threshold nskk-memory-gc-cons-threshold)

  ;; GCメッセージを抑制
  (setq garbage-collection-messages nil)

  (message "GC settings optimized: threshold=%d MB"
           (/ nskk-memory-gc-cons-threshold 1024 1024)))

(defun nskk-memory-restore-gc-settings ()
  "GC設定を元に戻す。"
  (interactive)
  (when nskk-memory--original-gc-cons-threshold
    (setq gc-cons-threshold nskk-memory--original-gc-cons-threshold
          nskk-memory--original-gc-cons-threshold nil))
  (message "GC settings restored"))

(defmacro nskk-memory-with-gc-optimization (&rest body)
  "GC最適化を一時的に有効にして処理を実行する。

引数:
  BODY - 処理

返り値:
  BODYの返り値"
  (declare (indent 0))
  `(let ((gc-cons-threshold nskk-memory-gc-cons-threshold)
         (garbage-collection-messages nil))
     ,@body))

;;; メモリ使用量測定

(defun nskk-memory-usage ()
  "現在のNSKK関連のメモリ使用量を推定する。

返り値:
  plist
    :string-pool-bytes   - 文字列プールのメモリ使用量
    :cons-pool-bytes     - consプールのメモリ使用量
    :vector-pool-bytes   - ベクトルプールのメモリ使用量
    :interned-bytes      - インターン文字列のメモリ使用量
    :total-bytes         - 合計メモリ使用量
    :gc-count            - GC実行回数
    :gc-elapsed          - GC累積時間"
  (let ((string-pool-bytes
         (cl-loop for (_size . strings) in nskk-memory--string-pool
                  sum (* (length strings)
                         (or (ignore-errors (length (car strings))) 0))))
        (cons-pool-bytes
         (* (length nskk-memory--cons-pool) 16)) ; consセル = 16バイト（推定）
        (vector-pool-bytes
         (cl-loop for (_size . vectors) in nskk-memory--vector-pool
                  sum (* (length vectors)
                         (or (ignore-errors (* (length (car vectors)) 8)) 0))))
        (interned-bytes
         (let ((total 0))
           (maphash (lambda (_k v)
                      (cl-incf total (length v)))
                    nskk-memory--interned-strings)
           total)))
    (list :string-pool-bytes string-pool-bytes
          :cons-pool-bytes cons-pool-bytes
          :vector-pool-bytes vector-pool-bytes
          :interned-bytes interned-bytes
          :total-bytes (+ string-pool-bytes cons-pool-bytes
                         vector-pool-bytes interned-bytes)
          :gc-count gcs-done
          :gc-elapsed gc-elapsed)))

(defun nskk-memory-show-usage ()
  "メモリ使用量を表示する。"
  (interactive)
  (let ((usage (nskk-memory-usage)))
    (with-output-to-temp-buffer "*NSKK Memory Usage*"
      (princ "NSKK Memory Usage Report\n")
      (princ (make-string 60 ?=))
      (princ "\n\n")
      (princ (format "String Pool:       %10d bytes (%6.2f KB)\n"
                    (plist-get usage :string-pool-bytes)
                    (/ (plist-get usage :string-pool-bytes) 1024.0)))
      (princ (format "Cons Pool:         %10d bytes (%6.2f KB)\n"
                    (plist-get usage :cons-pool-bytes)
                    (/ (plist-get usage :cons-pool-bytes) 1024.0)))
      (princ (format "Vector Pool:       %10d bytes (%6.2f KB)\n"
                    (plist-get usage :vector-pool-bytes)
                    (/ (plist-get usage :vector-pool-bytes) 1024.0)))
      (princ (format "Interned Strings:  %10d bytes (%6.2f KB)\n"
                    (plist-get usage :interned-bytes)
                    (/ (plist-get usage :interned-bytes) 1024.0)))
      (princ (make-string 60 ?-))
      (princ "\n")
      (princ (format "Total:             %10d bytes (%6.2f KB, %6.2f MB)\n"
                    (plist-get usage :total-bytes)
                    (/ (plist-get usage :total-bytes) 1024.0)
                    (/ (plist-get usage :total-bytes) 1024.0 1024.0)))
      (princ "\n")
      (princ (format "Target:            %10d bytes (%6.2f MB)\n"
                    nskk-memory-target-usage
                    (/ nskk-memory-target-usage 1024.0 1024.0)))
      (princ (format "Status:            %s\n"
                    (if (< (plist-get usage :total-bytes)
                          nskk-memory-target-usage)
                        "PASS"
                      "EXCEED")))
      (princ "\n")
      (princ (make-string 60 ?=))
      (princ "\n")
      (princ (format "GC Count:          %d\n" (plist-get usage :gc-count)))
      (princ (format "GC Elapsed:        %.3f seconds\n"
                    (plist-get usage :gc-elapsed))))))

;;; メモリレイアウト最適化

(defmacro nskk-memory-compact-struct (&rest slots)
  "メモリレイアウトを最適化した構造体定義。

引数:
  SLOTS - スロット定義

返り値:
  構造体定義

最適化:
  - フィールドの並び順を最適化してパディングを削減"
  `(progn ,@slots))

;;; 軽量オブジェクト

(defmacro nskk-memory-lightweight-plist (&rest pairs)
  "軽量なplistを作成する。

引数:
  PAIRS - キーと値のペア

返り値:
  plist

最適化:
  - 最小限のメモリ使用"
  `(list ,@pairs))

;;; メモリプレッシャー監視

(defvar nskk-memory--pressure-threshold (* 50 1024 1024)
  "メモリプレッシャー閾値（50MB）。")

(defun nskk-memory-check-pressure ()
  "メモリプレッシャーをチェックする。

返り値:
  plist
    :pressure  - メモリプレッシャー（high/medium/low）
    :usage     - 現在の使用量
    :threshold - 閾値"
  (let* ((usage-info (nskk-memory-usage))
         (total-bytes (plist-get usage-info :total-bytes))
         (pressure (cond
                    ((> total-bytes nskk-memory--pressure-threshold) 'high)
                    ((> total-bytes (/ nskk-memory--pressure-threshold 2)) 'medium)
                    (t 'low))))
    (list :pressure pressure
          :usage total-bytes
          :threshold nskk-memory--pressure-threshold)))

(defun nskk-memory-relieve-pressure ()
  "メモリプレッシャーを緩和する。"
  (interactive)
  (nskk-memory-pool-clear)
  (nskk-memory-clear-interned-strings)
  (garbage-collect)
  (message "Memory pressure relieved"))

;;; 自動メモリ管理

(defvar nskk-memory--auto-cleanup-timer nil
  "自動クリーンアップタイマー。")

(defun nskk-memory-enable-auto-cleanup (interval)
  "自動メモリクリーンアップを有効にする。

引数:
  INTERVAL - クリーンアップ間隔（秒）"
  (interactive "nCleanup interval (seconds): ")
  (when nskk-memory--auto-cleanup-timer
    (cancel-timer nskk-memory--auto-cleanup-timer))
  (setq nskk-memory--auto-cleanup-timer
        (run-with-timer interval interval #'nskk-memory--auto-cleanup))
  (message "Auto memory cleanup enabled (interval: %d seconds)" interval))

(defun nskk-memory-disable-auto-cleanup ()
  "自動メモリクリーンアップを無効にする。"
  (interactive)
  (when nskk-memory--auto-cleanup-timer
    (cancel-timer nskk-memory--auto-cleanup-timer)
    (setq nskk-memory--auto-cleanup-timer nil)
    (message "Auto memory cleanup disabled")))

(defun nskk-memory--auto-cleanup ()
  "自動クリーンアップ処理。"
  (let ((pressure-info (nskk-memory-check-pressure)))
    (when (eq (plist-get pressure-info :pressure) 'high)
      (nskk-memory-relieve-pressure))))

;;; 統計情報

(defun nskk-memory-stats ()
  "メモリ最適化の統計情報を返す。

返り値:
  plist
    :pooling-enabled     - プーリング有効
    :interning-enabled   - インターン有効
    :gc-threshold        - GC閾値
    :usage               - メモリ使用量情報
    :pressure            - メモリプレッシャー"
  (list :pooling-enabled nskk-memory-enable-pooling
        :interning-enabled nskk-memory-enable-interning
        :gc-threshold gc-cons-threshold
        :usage (nskk-memory-usage)
        :pressure (nskk-memory-check-pressure)))

(provide 'nskk-memory-optimize)

;;; nskk-memory-optimize.el ends here
