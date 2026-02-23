;;; nskk-layer-infrastructure.el --- Infrastructure Layer for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda

;; Author: NSKK Contributors
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

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

;; Infrastructure Layer - 基盤サービスとリソース管理
;;
;; 責務:
;; - スレッド管理
;; - メモリ管理
;; - ファイルI/O
;; - タイマー管理
;; - リソースプール
;;
;; レイヤー依存:
;; - 他のレイヤーから独立（最下層）
;; - Emacs 31のネイティブスレッド機能を活用
;;
;; 主要コンポーネント:
;; - スレッドプール
;; - メモリプール
;; - ファイルI/Oマネージャー
;; - タイマーマネージャー
;; - リソースモニター
;;
;; 使用例:
;; (nskk-infrastructure-submit-task task)
;; (nskk-infrastructure-allocate-memory size)
;; (nskk-infrastructure-read-file path)

;;; Code:

(require 'cl-lib)
(require 'nskk-thread-pool nil t)

;; Provide fallback stubs when thread-pool is unavailable
(unless (featurep 'nskk-thread-pool)
  (defun nskk-thread-pool-available-p () nil)
  (defun nskk-thread-pool-create (_size) nil)
  (defun nskk-thread-pool-shutdown (_pool &optional _force) nil))

;;; カスタマイズ可能変数

(defgroup nskk-infrastructure nil
  "Infrastructure layer settings for NSKK."
  :group 'nskk
  :prefix "nskk-infrastructure-")

(defcustom nskk-infrastructure-thread-pool-size 4
  "スレッドプールのサイズ。"
  :type 'integer
  :group 'nskk-infrastructure)

(defcustom nskk-infrastructure-memory-limit (* 20 1024 1024)
  "メモリ使用量の上限（バイト）。"
  :type 'integer
  :group 'nskk-infrastructure)

(defcustom nskk-infrastructure-enable-monitoring t
  "リソース監視を有効にするか。"
  :type 'boolean
  :group 'nskk-infrastructure)

;;; 内部変数

(defvar nskk-infrastructure--thread-pool nil
  "スレッドプール。")

(defvar nskk-infrastructure--memory-usage 0
  "現在のメモリ使用量（推定値）。")

(defvar nskk-infrastructure--timers nil
  "管理中のタイマーリスト。")

(defvar nskk-infrastructure--resource-monitor-timer nil
  "リソース監視タイマー。")

(defvar nskk-infrastructure--statistics (make-hash-table :test 'eq)
  "統計情報。")

;;; 初期化・シャットダウン

(defun nskk-infrastructure-initialize ()
  "Infrastructure Layerを初期化する。"
  (nskk-infrastructure--initialize-thread-pool)
  (nskk-infrastructure--initialize-memory-management)
  (nskk-infrastructure--initialize-statistics)
  (when nskk-infrastructure-enable-monitoring
    (nskk-infrastructure--start-monitoring))
  (nskk-infrastructure--log "Infrastructure Layer initialized"))

(defun nskk-infrastructure-shutdown ()
  "Infrastructure Layerをシャットダウンする。"
  (when nskk-infrastructure-enable-monitoring
    (nskk-infrastructure--stop-monitoring))
  (nskk-infrastructure--cleanup-timers)
  (nskk-infrastructure--cleanup-thread-pool)
  (nskk-infrastructure--cleanup-memory)
  (nskk-infrastructure--log "Infrastructure Layer shutdown"))

;;; スレッド管理

(defun nskk-infrastructure--initialize-thread-pool ()
  "スレッドプールを初期化する。"
  (when (and (nskk-thread-pool-available-p)
             (> nskk-infrastructure-thread-pool-size 0))
    (setq nskk-infrastructure--thread-pool
          (nskk-thread-pool-create nskk-infrastructure-thread-pool-size))
    (nskk-infrastructure--log "Thread pool initialized with %d workers"
                              nskk-infrastructure-thread-pool-size))
  (unless nskk-infrastructure--thread-pool
    (nskk-infrastructure--log "Thread pool unavailable; falling back to synchronous execution")))

(defun nskk-infrastructure--cleanup-thread-pool ()
  "スレッドプールをクリーンアップする。"
  (when nskk-infrastructure--thread-pool
    (nskk-thread-pool-shutdown nskk-infrastructure--thread-pool t)
    (setq nskk-infrastructure--thread-pool nil)))

(defun nskk-infrastructure-submit-task (task &optional callback error-handler)
  "タスクをスレッドプールに投入する。
TASKは引数なしで呼び出される関数。
CALLBACKはタスク完了時に結果を受け取る関数。
ERROR-HANDLERは例外発生時に呼び出される関数。"
  (unless (functionp task)
    (user-error "TASK must be a function"))
  (nskk-infrastructure--record-task-submitted)
  (let ((pool nskk-infrastructure--thread-pool))
    (if pool
        (nskk-thread-submit
         pool
         task
         (lambda (result)
           (nskk-infrastructure--record-task-completion)
           (when callback
             (funcall callback result)))
         (lambda (err)
           (nskk-infrastructure--record-task-completion)
           (nskk-infrastructure--log "Task error: %S" err)
           (when error-handler
             (funcall error-handler err))))
      (nskk-infrastructure--log "Running task inline (no thread pool)")
      (condition-case err
          (let ((result (funcall task)))
            (nskk-infrastructure--record-task-completion)
            (when callback
              (funcall callback result))
            result)
        (error
         (nskk-infrastructure--record-task-completion)
         (nskk-infrastructure--log "Task error: %S" err)
         (when error-handler
           (funcall error-handler err)))))))

(defun nskk-infrastructure-run-async (function &rest args)
  "関数を非同期実行する。
FUNCTIONは実行する関数、ARGSは引数。"
  (nskk-infrastructure-submit-task
   (lambda () (apply function args))))

;;; メモリ管理

(defun nskk-infrastructure--initialize-memory-management ()
  "メモリ管理を初期化する。"
  (setq nskk-infrastructure--memory-usage 0)
  (nskk-infrastructure--log "Memory management initialized"))

(defun nskk-infrastructure--cleanup-memory ()
  "メモリをクリーンアップする。"
  (garbage-collect)
  (setq nskk-infrastructure--memory-usage 0))

(defun nskk-infrastructure-allocate-memory (size)
  "メモリを確保する。
SIZEは確保するサイズ（バイト）。"
  (when (> (+ nskk-infrastructure--memory-usage size)
           nskk-infrastructure-memory-limit)
    (nskk-infrastructure--log "Memory limit exceeded, triggering GC")
    (garbage-collect))
  (setq nskk-infrastructure--memory-usage
        (+ nskk-infrastructure--memory-usage size)))

(defun nskk-infrastructure-free-memory (size)
  "メモリを解放する。
SIZEは解放するサイズ（バイト）。"
  (setq nskk-infrastructure--memory-usage
        (max 0 (- nskk-infrastructure--memory-usage size))))

(defun nskk-infrastructure-get-memory-usage ()
  "現在のメモリ使用量を取得する。"
  nskk-infrastructure--memory-usage)

;;; ファイルI/O

(defun nskk-infrastructure-read-file (path)
  "ファイルを読み込む。
PATHはファイルパス。"
  (unless (file-readable-p path)
    (signal 'file-error (list "File not readable" path)))
  (with-temp-buffer
    (insert-file-contents path)
    (nskk-infrastructure--inc-stat :files-read 1)
    (buffer-string)))

(defun nskk-infrastructure-write-file (path content)
  "ファイルに書き込む。
PATHはファイルパス、CONTENTは書き込む内容。"
  (let ((dir (file-name-directory path)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-temp-file path
    (insert content))
  (nskk-infrastructure--inc-stat :files-written 1))

(defun nskk-infrastructure-read-file-async (path callback &optional error-handler)
  "ファイルを非同期読み込みする。
PATHはファイルパス、CALLBACKは完了時のコールバック。
ERROR-HANDLERはエラー時のハンドラー。"
  (nskk-infrastructure-submit-task
   (lambda () (nskk-infrastructure-read-file path))
   callback
   error-handler))

(defun nskk-infrastructure-write-file-async (path content callback &optional error-handler)
  "ファイルに非同期書き込みする。
PATHはファイルパス、CONTENTは書き込む内容、
CALLBACKは完了時のコールバック。"
  (nskk-infrastructure-submit-task
   (lambda ()
     (nskk-infrastructure-write-file path content)
     t)
   (when callback
     (lambda (_result)
       (funcall callback)))
   error-handler))

;;; タイマー管理

(defun nskk-infrastructure--cleanup-timers ()
  "すべてのタイマーをクリーンアップする。"
  (dolist (timer nskk-infrastructure--timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq nskk-infrastructure--timers nil))

(defun nskk-infrastructure-schedule-timer (secs repeat function &rest args)
  "タイマーをスケジュールする。
SECSは初回実行までの秒数、REPEATは繰り返し間隔（nilなら1回のみ）、
FUNCTIONは実行する関数、ARGSは引数。"
  (let ((timer (apply #'run-with-timer secs repeat function args)))
    (push timer nskk-infrastructure--timers)
    timer))

(defun nskk-infrastructure-cancel-timer (timer)
  "タイマーをキャンセルする。
TIMERはタイマーオブジェクト。"
  (when (timerp timer)
    (cancel-timer timer)
    (setq nskk-infrastructure--timers
          (delq timer nskk-infrastructure--timers))))

;;; リソース監視

(defun nskk-infrastructure--start-monitoring ()
  "リソース監視を開始する。"
  (setq nskk-infrastructure--resource-monitor-timer
        (run-with-timer 60 60 #'nskk-infrastructure--monitor-resources)))

(defun nskk-infrastructure--stop-monitoring ()
  "リソース監視を停止する。"
  (when nskk-infrastructure--resource-monitor-timer
    (cancel-timer nskk-infrastructure--resource-monitor-timer)
    (setq nskk-infrastructure--resource-monitor-timer nil)))

(defun nskk-infrastructure--monitor-resources ()
  "リソースを監視する。"
  ;; メモリ使用量チェック
  (when (> nskk-infrastructure--memory-usage
           (* 0.9 nskk-infrastructure-memory-limit))
    (nskk-infrastructure--log "Memory usage high: %d/%d bytes"
                              nskk-infrastructure--memory-usage
                              nskk-infrastructure-memory-limit)
    (garbage-collect))
  ;; 統計更新
  (nskk-infrastructure--update-statistics))

;;; 統計情報

(defun nskk-infrastructure--initialize-statistics ()
  "統計情報を初期化する。"
  (puthash :start-time (float-time) nskk-infrastructure--statistics)
  (puthash :tasks-submitted 0 nskk-infrastructure--statistics)
  (puthash :tasks-completed 0 nskk-infrastructure--statistics)
  (puthash :files-read 0 nskk-infrastructure--statistics)
  (puthash :files-written 0 nskk-infrastructure--statistics))

(defun nskk-infrastructure--update-statistics ()
  "統計情報を更新する。"
  (puthash :uptime (- (float-time)
                      (gethash :start-time nskk-infrastructure--statistics))
           nskk-infrastructure--statistics)
  (puthash :memory-usage nskk-infrastructure--memory-usage
           nskk-infrastructure--statistics))

(defun nskk-infrastructure--inc-stat (key delta)
  "統計情報KEYをDELTAだけ増加させる。"
  (puthash key (+ (or (gethash key nskk-infrastructure--statistics) 0)
                  delta)
           nskk-infrastructure--statistics))

(defun nskk-infrastructure--record-task-submitted ()
  "タスク投入数を更新する。"
  (nskk-infrastructure--inc-stat :tasks-submitted 1))

(defun nskk-infrastructure--record-task-completion ()
  "タスク完了数を更新する。"
  (nskk-infrastructure--inc-stat :tasks-completed 1))

(defun nskk-infrastructure-get-statistics ()
  "統計情報を取得する。"
  (interactive)
  (nskk-infrastructure--update-statistics)
  (let ((uptime (gethash :uptime nskk-infrastructure--statistics))
        (memory (gethash :memory-usage nskk-infrastructure--statistics))
        (tasks-submitted (gethash :tasks-submitted nskk-infrastructure--statistics))
        (tasks-completed (gethash :tasks-completed nskk-infrastructure--statistics)))
    (message "Infrastructure Statistics:\n  Uptime: %.1f seconds\n  Memory: %d bytes\n  Tasks: %d/%d (submitted/completed)\n  Timers: %d"
             uptime memory tasks-submitted tasks-completed
             (length nskk-infrastructure--timers))
    (list :uptime uptime
          :memory memory
          :tasks-submitted tasks-submitted
          :tasks-completed tasks-completed
          :active-timers (length nskk-infrastructure--timers))))

;;; デバッグ・ロギング

(defvar nskk-infrastructure--debug-enabled nil
  "デバッグモードが有効かどうか。")

(defun nskk-infrastructure-enable-debug ()
  "デバッグモードを有効にする。"
  (interactive)
  (setq nskk-infrastructure--debug-enabled t)
  (message "NSKK Infrastructure Layer: Debug mode enabled"))

(defun nskk-infrastructure-disable-debug ()
  "デバッグモードを無効にする。"
  (interactive)
  (setq nskk-infrastructure--debug-enabled nil)
  (message "NSKK Infrastructure Layer: Debug mode disabled"))

(defun nskk-infrastructure--log (format-string &rest args)
  "デバッグログを出力する。
FORMAT-STRINGはフォーマット文字列、ARGSは引数。"
  (when nskk-infrastructure--debug-enabled
    (apply #'message (concat "[NSKK-Infrastructure] " format-string) args)))

;;; ヘルスチェック

(defun nskk-infrastructure-health-check ()
  "Infrastructure Layerのヘルスチェックを実行する。"
  (interactive)
  (let ((issues '()))
    ;; メモリチェック
    (when (> nskk-infrastructure--memory-usage
             nskk-infrastructure-memory-limit)
      (push "Memory limit exceeded" issues))
    ;; スレッドプールチェック
    (unless nskk-infrastructure--thread-pool
      (push "Thread pool not initialized" issues))
    ;; 結果表示
    (if issues
        (message "Infrastructure Layer issues: %s" (string-join issues ", "))
      (message "Infrastructure Layer: All systems operational"))))

(provide 'nskk-layer-infrastructure)
;;; nskk-layer-infrastructure.el ends here
