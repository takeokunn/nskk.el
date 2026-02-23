;;; nskk-thread-pool.el --- Thread pool implementation for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda
;;
;; This file is part of NSKK (Next-generation SKK).
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: NSKK Development Team
;; Maintainer: NSKK Development Team
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

;;; Commentary:

;; このファイルは、Emacs 31のネイティブスレッド機能を活用した
;; スレッドプール実装を提供します。
;;
;; 主な機能:
;; - ワーカースレッド管理
;; - タスクキュー (FIFO)
;; - スレッド生存期管理
;; - CPUコア数自動調整
;;
;; 使用例:
;;   (let ((pool (nskk-thread-pool-create)))
;;     (nskk-thread-submit pool
;;                         (lambda () (+ 1 2))
;;                         (lambda (result) (message "Result: %s" result)))
;;     (nskk-thread-pool-shutdown pool))

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-thread-pool nil
  "NSKK thread pool settings."
  :group 'nskk
  :prefix "nskk-thread-pool-")

(defcustom nskk-thread-pool-default-size nil
  "Default thread pool size.
When nil, automatically set based on CPU core count."
  :type '(choice (const :tag "Auto" nil)
                 (integer :tag "Fixed size"))
  :group 'nskk-thread-pool)

(defcustom nskk-thread-pool-idle-timeout 60
  "Timeout in seconds before idle worker threads are terminated."
  :type 'integer
  :group 'nskk-thread-pool)

(defcustom nskk-thread-pool-max-queue-size 1000
  "Maximum number of tasks in the queue."
  :type 'integer
  :group 'nskk-thread-pool)

;;; データ構造

(cl-defstruct (nskk-thread-pool
               (:constructor nskk-thread-pool--create)
               (:copier nil))
  "スレッドプール構造体。"
  (size 0 :type integer :documentation "プールサイズ")
  (workers nil :type list :documentation "ワーカースレッドリスト")
  (task-queue nil :type list :documentation "タスクキュー")
  (queue-mutex nil :documentation "キューmutex")
  (queue-condition nil :documentation "キュー条件変数")
  (shutdown-flag nil :type boolean :documentation "シャットダウンフラグ")
  (active-count 0 :type integer :documentation "アクティブスレッド数"))

(cl-defstruct (nskk-thread-task
               (:constructor nskk-thread-task-create)
               (:copier nil))
  "スレッドタスク構造体。"
  (function nil :type function :documentation "実行する関数")
  (callback nil :documentation "完了時のコールバック")
  (error-handler nil :documentation "エラーハンドラー"))

;;; ユーティリティ関数

(defun nskk-thread-pool--get-cpu-count ()
  "Return the number of available CPU cores."
  (if (fboundp 'num-processors)
      (num-processors)
    ;; フォールバック: 環境変数から取得を試みる
    (or (ignore-errors
          (string-to-number
           (shell-command-to-string
            (cond ((eq system-type 'darwin)
                   "sysctl -n hw.ncpu")
                  ((eq system-type 'gnu/linux)
                   "nproc")
                  (t "echo 4")))))
        4)))

(defun nskk-thread-pool--optimal-size ()
  "Calculate optimal thread pool size based on CPU core count."
  (let ((cpu-count (nskk-thread-pool--get-cpu-count)))
    ;; CPUコア数と同じか、やや少なめに設定
    (max 2 (min cpu-count 8))))

;;; キュー操作

(defun nskk-thread-pool--enqueue (pool task)
  "Enqueue TASK into POOL."
  (with-mutex (nskk-thread-pool-queue-mutex pool)
    (when (>= (length (nskk-thread-pool-task-queue pool))
              nskk-thread-pool-max-queue-size)
      (error "Thread pool queue is full"))
    (let ((queue (nskk-thread-pool-task-queue pool)))
      (if queue
          (setf (nskk-thread-pool-task-queue pool)
                (nconc queue (list task)))
        (setf (nskk-thread-pool-task-queue pool)
              (list task))))
    ;; 待機中のワーカーに通知
    (condition-notify (nskk-thread-pool-queue-condition pool))))

(defun nskk-thread-pool--dequeue (pool)
  "Dequeue and return the next task from POOL, or nil if empty."
  (let ((queue (nskk-thread-pool-task-queue pool)))
    (when queue
      (let ((task (car queue)))
        (setf (nskk-thread-pool-task-queue pool) (cdr queue))
        task))))

;;; ワーカースレッド

(defun nskk-thread-pool--worker-loop (pool worker-id)
  "Main loop for worker WORKER-ID in POOL."
  (cl-block nil
    (while t
      (let ((task nil)
            (should-exit nil))
        (with-mutex (nskk-thread-pool-queue-mutex pool)
          (while (and (not (nskk-thread-pool-shutdown-flag pool))
                      (null (nskk-thread-pool-task-queue pool)))
            (condition-wait (nskk-thread-pool-queue-condition pool)))
          (when (and (nskk-thread-pool-shutdown-flag pool)
                     (null (nskk-thread-pool-task-queue pool)))
            (setq should-exit t))
          (unless should-exit
            (setq task (nskk-thread-pool--dequeue pool))
            (when task
              (setf (nskk-thread-pool-active-count pool)
                    (1+ (nskk-thread-pool-active-count pool))))))
        (when should-exit
          (cl-return nil))
        (when task
          (condition-case err
              (let ((result (funcall (nskk-thread-task-function task))))
                (when (nskk-thread-task-callback task)
                  (funcall (nskk-thread-task-callback task) result)))
            (error
             (if (nskk-thread-task-error-handler task)
                 (funcall (nskk-thread-task-error-handler task) err)
               (message "Thread pool worker %d error: %S" worker-id err))))
          (with-mutex (nskk-thread-pool-queue-mutex pool)
            (setf (nskk-thread-pool-active-count pool)
                  (max 0 (1- (nskk-thread-pool-active-count pool))))))))))



(defun nskk-thread-pool--create-worker (pool worker-id)
  "Create a worker thread with WORKER-ID for POOL."
  (make-thread
   (lambda ()
     (nskk-thread-pool--worker-loop pool worker-id))
   (format "nskk-worker-%d" worker-id)))

;;; 公開API

;;;###autoload
(defun nskk-thread-pool-create (&optional size)
  "Create a thread pool with SIZE workers.
When SIZE is nil, automatically determine based on CPU core count."
  (interactive)
  (let* ((pool-size (or size
                        nskk-thread-pool-default-size
                        (nskk-thread-pool--optimal-size)))
         (queue-mutex (make-mutex "nskk-thread-pool-queue"))
         (pool (nskk-thread-pool--create
                :size pool-size
                :task-queue nil
                :queue-mutex queue-mutex
                :queue-condition (make-condition-variable queue-mutex)
                :shutdown-flag nil
                :active-count 0)))

    ;; ワーカースレッド生成
    (dotimes (i pool-size)
      (push (nskk-thread-pool--create-worker pool i)
            (nskk-thread-pool-workers pool)))

    (message "Thread pool created with %d workers" pool-size)
    pool))

;;;###autoload
(defun nskk-thread-submit (pool task &optional callback error-handler)
  "Submit TASK to POOL for execution.
CALLBACK is called with the result on completion.
ERROR-HANDLER is called with the error on failure."
  (when (nskk-thread-pool-shutdown-flag pool)
    (error "Cannot submit task to shutdown pool"))

  (let ((thread-task (nskk-thread-task-create
                      :function task
                      :callback callback
                      :error-handler error-handler)))
    (nskk-thread-pool--enqueue pool thread-task)))

;;;###autoload
(defun nskk-thread-pool-shutdown (pool &optional wait)
  "Shut down POOL.
When WAIT is non-nil, wait for all tasks to complete."
  (interactive)
  (setf (nskk-thread-pool-shutdown-flag pool) t)

  ;; 全ワーカーに通知
  (with-mutex (nskk-thread-pool-queue-mutex pool)
    (condition-notify (nskk-thread-pool-queue-condition pool) t))

  ;; 待機が指定されている場合
  (when wait
    (dolist (worker (nskk-thread-pool-workers pool))
      (thread-join worker)))

  (message "Thread pool shutdown"))

;;;###autoload
(defun nskk-thread-pool-status (pool)
  "Display status of POOL."
  (interactive)
  (message "Thread Pool Status:
  Size: %d
  Queue length: %d
  Active: %d
  Shutdown: %s"
           (nskk-thread-pool-size pool)
           (length (nskk-thread-pool-task-queue pool))
           (nskk-thread-pool-active-count pool)
           (if (nskk-thread-pool-shutdown-flag pool) "Yes" "No"))
  nil)

;;; ヘルパー関数

(defun nskk-thread-pool-wait-for-completion (pool timeout)
  "Wait for all tasks in POOL to complete within TIMEOUT seconds.
Return t if completed, nil if timed out."
  (let ((start-time (current-time)))
    (while (and (> (length (nskk-thread-pool-task-queue pool)) 0)
                (< (float-time (time-subtract (current-time) start-time))
                   timeout))
      (sleep-for 0.1))
    (= (length (nskk-thread-pool-task-queue pool)) 0)))

(defun nskk-thread-pool-available-p ()
  "Return non-nil if threading functionality is available."
  (and (fboundp 'make-thread)
       (fboundp 'make-mutex)
       (fboundp 'make-condition-variable)))

(provide 'nskk-thread-pool)

;;; nskk-thread-pool.el ends here
