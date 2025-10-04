;;; nskk-background.el --- Background tasks for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, background
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

;; このファイルはバックグラウンド処理機能を実装します。
;;
;; 特徴:
;; - インデックス更新
;; - 辞書同期
;; - 統計収集
;; - リソース使用量監視
;;
;; 使用例:
;;
;;   (require 'nskk-background)
;;
;;   ;; バックグラウンドタスク開始
;;   (nskk-background-start)
;;
;;   ;; インデックス更新をスケジュール
;;   (nskk-background-schedule-index-update)
;;
;;   ;; バックグラウンドタスク停止
;;   (nskk-background-stop)

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-background nil
  "Background tasks customization."
  :group 'nskk
  :prefix "nskk-background-")

(defcustom nskk-background-enable t
  "非nilの場合、バックグラウンドタスクを有効にする。"
  :type 'boolean
  :group 'nskk-background)

(defcustom nskk-background-index-update-interval 300
  "インデックス更新の間隔（秒）。
デフォルトは5分。"
  :type 'integer
  :group 'nskk-background)

(defcustom nskk-background-dict-sync-interval 600
  "辞書同期の間隔（秒）。
デフォルトは10分。"
  :type 'integer
  :group 'nskk-background)

(defcustom nskk-background-stats-collection-interval 60
  "統計収集の間隔（秒）。
デフォルトは1分。"
  :type 'integer
  :group 'nskk-background)

(defcustom nskk-background-idle-time 5
  "バックグラウンドタスクを開始するアイドル時間（秒）。"
  :type 'integer
  :group 'nskk-background)

(defcustom nskk-background-max-cpu-usage 10.0
  "バックグラウンドタスクの最大CPU使用率（％）。
この値を超える場合、タスクを一時停止する。"
  :type 'float
  :group 'nskk-background)

(defcustom nskk-background-max-memory-usage 50
  "バックグラウンドタスクの最大メモリ使用量（MB）。
この値を超える場合、タスクを一時停止する。"
  :type 'integer
  :group 'nskk-background)

(defcustom nskk-background-verbose nil
  "非nilの場合、詳細なログを出力する。"
  :type 'boolean
  :group 'nskk-background)

;;; データ構造

(cl-defstruct (nskk-background-task
               (:constructor nskk-background-task--create)
               (:copier nil))
  "バックグラウンドタスク。

スロット:
  name          - タスク名
  function      - 実行する関数
  interval      - 実行間隔（秒）
  timer         - タイマー
  last-run      - 最終実行時刻（float-time）
  run-count     - 実行回数
  total-time    - 累計実行時間（秒）
  enabled       - 有効/無効フラグ"
  (name nil :type (or null string))
  (function nil :type (or null function))
  (interval 60 :type integer)
  (timer nil :type (or null timer))
  (last-run nil :type (or null number))
  (run-count 0 :type integer)
  (total-time 0.0 :type float)
  (enabled t :type boolean))

(cl-defstruct (nskk-background-stats
               (:constructor nskk-background-stats--create)
               (:copier nil))
  "リソース統計。

スロット:
  cpu-usage     - CPU使用率（％）
  memory-usage  - メモリ使用量（MB）
  gc-count      - GC実行回数
  timestamp     - タイムスタンプ（float-time）"
  (cpu-usage 0.0 :type float)
  (memory-usage 0 :type integer)
  (gc-count 0 :type integer)
  (timestamp nil :type (or null number)))

;;; 内部変数

(defvar nskk-background--tasks nil
  "登録されているバックグラウンドタスクのリスト。")

(defvar nskk-background--idle-timer nil
  "アイドルタイマー。")

(defvar nskk-background--resource-monitor-timer nil
  "リソース監視タイマー。")

(defvar nskk-background--stats-history nil
  "リソース統計履歴（最大100件）。")

(defvar nskk-background--suspended nil
  "リソース制限によりタスクが一時停止されているかどうか。")

;;; バックグラウンドタスク管理

;;;###autoload
(defun nskk-background-start ()
  "バックグラウンドタスクを開始する。"
  (interactive)
  (when nskk-background-enable
    (when nskk-background-verbose
      (message "Starting background tasks"))

    ;; デフォルトタスクを登録
    (nskk-background--register-default-tasks)

    ;; アイドルタイマー開始
    (unless nskk-background--idle-timer
      (setq nskk-background--idle-timer
            (run-with-idle-timer nskk-background-idle-time t
                                #'nskk-background--on-idle)))

    ;; リソース監視開始
    (nskk-background--start-resource-monitor)))

;;;###autoload
(defun nskk-background-stop ()
  "バックグラウンドタスクを停止する。"
  (interactive)
  (when nskk-background-verbose
    (message "Stopping background tasks"))

  ;; すべてのタスクタイマーをキャンセル
  (dolist (task nskk-background--tasks)
    (when (nskk-background-task-timer task)
      (cancel-timer (nskk-background-task-timer task))))

  ;; アイドルタイマーキャンセル
  (when nskk-background--idle-timer
    (cancel-timer nskk-background--idle-timer)
    (setq nskk-background--idle-timer nil))

  ;; リソース監視停止
  (nskk-background--stop-resource-monitor)

  ;; タスクリストクリア
  (setq nskk-background--tasks nil))

(defun nskk-background--register-default-tasks ()
  "デフォルトのバックグラウンドタスクを登録する。"
  (nskk-background-register-task
   "index-update"
   #'nskk-background--task-index-update
   nskk-background-index-update-interval)

  (nskk-background-register-task
   "dict-sync"
   #'nskk-background--task-dict-sync
   nskk-background-dict-sync-interval)

  (nskk-background-register-task
   "stats-collection"
   #'nskk-background--task-stats-collection
   nskk-background-stats-collection-interval))

;;;###autoload
(defun nskk-background-register-task (name function interval)
  "バックグラウンドタスクを登録する。

引数:
  NAME     - タスク名（文字列）
  FUNCTION - 実行する関数
  INTERVAL - 実行間隔（秒）

戻り値:
  nskk-background-task構造体"
  (when nskk-background-verbose
    (message "Registering background task: %s (interval: %ds)" name interval))

  ;; 既存のタスクを削除
  (nskk-background-unregister-task name)

  (let ((task (nskk-background-task--create
               :name name
               :function function
               :interval interval)))

    (push task nskk-background--tasks)
    task))

;;;###autoload
(defun nskk-background-unregister-task (name)
  "バックグラウンドタスクを登録解除する。

引数:
  NAME - タスク名（文字列）"
  (let ((task (nskk-background--find-task name)))
    (when task
      (when nskk-background-verbose
        (message "Unregistering background task: %s" name))

      ;; タイマーキャンセル
      (when (nskk-background-task-timer task)
        (cancel-timer (nskk-background-task-timer task)))

      ;; リストから削除
      (setq nskk-background--tasks
            (delq task nskk-background--tasks)))))

(defun nskk-background--find-task (name)
  "タスク名でタスクを検索する。

引数:
  NAME - タスク名（文字列）

戻り値:
  nskk-background-task構造体、または nil"
  (cl-find name nskk-background--tasks
           :key #'nskk-background-task-name
           :test #'string=))

;;; タスク実行

(defun nskk-background--on-idle ()
  "アイドル時に呼び出される。"
  (when (and nskk-background-enable
             (not nskk-background--suspended))
    (nskk-background--schedule-tasks)))

(defun nskk-background--schedule-tasks ()
  "すべてのタスクをスケジュールする。"
  (dolist (task nskk-background--tasks)
    (when (and (nskk-background-task-enabled task)
               (not (nskk-background-task-timer task)))
      (nskk-background--schedule-task task))))

(defun nskk-background--schedule-task (task)
  "タスクをスケジュールする。

引数:
  TASK - nskk-background-task構造体"
  (let ((interval (nskk-background-task-interval task)))

    (when nskk-background-verbose
      (message "Scheduling task: %s (interval: %ds)"
               (nskk-background-task-name task)
               interval))

    (setf (nskk-background-task-timer task)
          (run-at-time interval interval
                      (lambda ()
                        (nskk-background--execute-task task))))))

(defun nskk-background--execute-task (task)
  "タスクを実行する。

引数:
  TASK - nskk-background-task構造体"
  (when (and (nskk-background-task-enabled task)
             (not nskk-background--suspended))

    (when nskk-background-verbose
      (message "Executing background task: %s"
               (nskk-background-task-name task)))

    (let ((start-time (float-time)))

      (condition-case err
          (funcall (nskk-background-task-function task))
        (error
         (message "Background task error [%s]: %s"
                  (nskk-background-task-name task)
                  (error-message-string err))))

      ;; 統計更新
      (let ((elapsed (- (float-time) start-time)))
        (setf (nskk-background-task-last-run task) (float-time))
        (cl-incf (nskk-background-task-run-count task))
        (cl-incf (nskk-background-task-total-time task) elapsed)

        (when nskk-background-verbose
          (message "Background task completed: %s (%.3fs)"
                   (nskk-background-task-name task)
                   elapsed))))))

;;; デフォルトタスク実装

(defun nskk-background--task-index-update ()
  "インデックス更新タスク。"
  (when nskk-background-verbose
    (message "Running index update task"))

  ;; nskk-index のAPIを使用してインデックスを更新
  ;; 実際の実装はnskk-indexに依存
  (when (fboundp 'nskk-index-rebuild-incremental)
    (nskk-index-rebuild-incremental)))

(defun nskk-background--task-dict-sync ()
  "辞書同期タスク。"
  (when nskk-background-verbose
    (message "Running dictionary sync task"))

  ;; nskk-dict-io のAPIを使用して辞書を同期
  ;; 実際の実装はnskk-dict-ioに依存
  (when (fboundp 'nskk-dict-sync)
    (nskk-dict-sync)))

(defun nskk-background--task-stats-collection ()
  "統計収集タスク。"
  (when nskk-background-verbose
    (message "Running statistics collection task"))

  ;; 統計データを収集
  (let ((stats (nskk-background--collect-stats)))
    (push stats nskk-background--stats-history)

    ;; 履歴を100件に制限
    (when (> (length nskk-background--stats-history) 100)
      (setq nskk-background--stats-history
            (cl-subseq nskk-background--stats-history 0 100)))))

;;; リソース監視

(defun nskk-background--start-resource-monitor ()
  "リソース監視を開始する。"
  (unless nskk-background--resource-monitor-timer
    (setq nskk-background--resource-monitor-timer
          (run-at-time 10 10 #'nskk-background--check-resources))))

(defun nskk-background--stop-resource-monitor ()
  "リソース監視を停止する。"
  (when nskk-background--resource-monitor-timer
    (cancel-timer nskk-background--resource-monitor-timer)
    (setq nskk-background--resource-monitor-timer nil)))

(defun nskk-background--check-resources ()
  "リソース使用量をチェックし、必要に応じてタスクを一時停止する。"
  (let ((stats (nskk-background--collect-stats)))

    ;; CPU使用率チェック
    (when (> (nskk-background-stats-cpu-usage stats)
             nskk-background-max-cpu-usage)
      (nskk-background--suspend-tasks
       (format "High CPU usage: %.1f%%"
              (nskk-background-stats-cpu-usage stats))))

    ;; メモリ使用量チェック
    (when (> (nskk-background-stats-memory-usage stats)
             nskk-background-max-memory-usage)
      (nskk-background--suspend-tasks
       (format "High memory usage: %dMB"
              (nskk-background-stats-memory-usage stats))))))

(defun nskk-background--suspend-tasks (reason)
  "リソース制限によりタスクを一時停止する。

引数:
  REASON - 一時停止の理由"
  (unless nskk-background--suspended
    (when nskk-background-verbose
      (message "Suspending background tasks: %s" reason))

    (setq nskk-background--suspended t)

    ;; 一定時間後に再開
    (run-at-time 60 nil #'nskk-background--resume-tasks)))

(defun nskk-background--resume-tasks ()
  "タスクを再開する。"
  (when nskk-background--suspended
    (when nskk-background-verbose
      (message "Resuming background tasks"))

    (setq nskk-background--suspended nil)))

(defun nskk-background--collect-stats ()
  "現在のリソース統計を収集する。

戻り値:
  nskk-background-stats構造体"
  (let* (;; メモリ使用量を簡易的に計算（MB単位）
         ;; memory-use-countsはリストを返すので、最初の要素を使用
         (memory-counts (memory-use-counts))
         (memory-usage (/ (if (listp memory-counts)
                             (car memory-counts)
                           memory-counts)
                         1048576))
         ;; CPU使用率の推定（簡易版）
         (cpu-usage 0.0)  ; 実際にはより詳細な計算が必要
         ;; GC回数（gcs-doneはリストの場合がある）
         (gc-count (cond
                    ((numberp gcs-done) gcs-done)
                    ((listp gcs-done) (if (numberp (car gcs-done))
                                         (car gcs-done)
                                       0))
                    (t 0))))

    (nskk-background-stats--create
     :cpu-usage cpu-usage
     :memory-usage memory-usage
     :gc-count gc-count
     :timestamp (float-time))))

;;; 統計情報取得

;;;###autoload
(defun nskk-background-task-stats ()
  "すべてのタスクの統計情報を取得する。

戻り値:
  タスク統計のリスト（plist形式）
  各要素: (:name NAME :run-count N :total-time T :avg-time A)"
  (mapcar (lambda (task)
            (let* ((run-count (nskk-background-task-run-count task))
                   (total-time (nskk-background-task-total-time task))
                   (avg-time (if (zerop run-count)
                                0.0
                              (/ total-time run-count))))

              (list :name (nskk-background-task-name task)
                    :run-count run-count
                    :total-time total-time
                    :avg-time avg-time
                    :enabled (nskk-background-task-enabled task))))
          nskk-background--tasks))

;;;###autoload
(defun nskk-background-resource-stats ()
  "リソース統計の概要を取得する。

戻り値:
  plist形式の統計情報
  (:current STATS :avg-cpu N :avg-memory M :max-cpu N :max-memory M)"
  (let ((current (nskk-background--collect-stats))
        (history nskk-background--stats-history))

    (if (null history)
        (list :current current
              :avg-cpu 0.0
              :avg-memory 0
              :max-cpu 0.0
              :max-memory 0)

      (let ((avg-cpu (/ (cl-reduce #'+ history
                                   :key #'nskk-background-stats-cpu-usage)
                       (float (length history))))
            (avg-memory (/ (cl-reduce #'+ history
                                     :key #'nskk-background-stats-memory-usage)
                          (length history)))
            (max-cpu (cl-reduce #'max history
                               :key #'nskk-background-stats-cpu-usage))
            (max-memory (cl-reduce #'max history
                                  :key #'nskk-background-stats-memory-usage)))

        (list :current current
              :avg-cpu avg-cpu
              :avg-memory avg-memory
              :max-cpu max-cpu
              :max-memory max-memory)))))

;;;###autoload
(defun nskk-background-show-stats ()
  "統計情報を表示する。"
  (interactive)
  (let ((task-stats (nskk-background-task-stats))
        (resource-stats (nskk-background-resource-stats)))

    (with-output-to-temp-buffer "*NSKK Background Stats*"
      (princ "=== NSKK Background Task Statistics ===\n\n")

      ;; タスク統計
      (princ "Tasks:\n")
      (dolist (stat task-stats)
        (princ (format "  %s: %d runs, %.3fs total, %.4fs avg%s\n"
                      (plist-get stat :name)
                      (plist-get stat :run-count)
                      (plist-get stat :total-time)
                      (plist-get stat :avg-time)
                      (if (plist-get stat :enabled) "" " [DISABLED]"))))

      (princ "\n")

      ;; リソース統計
      (princ "Resource Usage:\n")
      (let ((current (plist-get resource-stats :current)))
        (princ (format "  Current: CPU %.1f%%, Memory %dMB\n"
                      (nskk-background-stats-cpu-usage current)
                      (nskk-background-stats-memory-usage current))))
      (princ (format "  Average: CPU %.1f%%, Memory %dMB\n"
                    (plist-get resource-stats :avg-cpu)
                    (plist-get resource-stats :avg-memory)))
      (princ (format "  Maximum: CPU %.1f%%, Memory %dMB\n"
                    (plist-get resource-stats :max-cpu)
                    (plist-get resource-stats :max-memory)))

      (princ "\n")
      (princ (format "Suspended: %s\n"
                    (if nskk-background--suspended "Yes" "No"))))))

;;; タスク制御

;;;###autoload
(defun nskk-background-enable-task (name)
  "タスクを有効にする。

引数:
  NAME - タスク名（文字列）"
  (interactive (list (completing-read "Task: "
                                     (mapcar #'nskk-background-task-name
                                            nskk-background--tasks))))
  (let ((task (nskk-background--find-task name)))
    (when task
      (setf (nskk-background-task-enabled task) t)
      (message "Task enabled: %s" name))))

;;;###autoload
(defun nskk-background-disable-task (name)
  "タスクを無効にする。

引数:
  NAME - タスク名（文字列）"
  (interactive (list (completing-read "Task: "
                                     (mapcar #'nskk-background-task-name
                                            nskk-background--tasks))))
  (let ((task (nskk-background--find-task name)))
    (when task
      (setf (nskk-background-task-enabled task) nil)
      (message "Task disabled: %s" name))))

;;;###autoload
(defun nskk-background-schedule-index-update ()
  "インデックス更新を即座にスケジュールする。"
  (interactive)
  (nskk-background--task-index-update))

;;;###autoload
(defun nskk-background-schedule-dict-sync ()
  "辞書同期を即座にスケジュールする。"
  (interactive)
  (nskk-background--task-dict-sync))

(provide 'nskk-background)

;;; nskk-background.el ends here
