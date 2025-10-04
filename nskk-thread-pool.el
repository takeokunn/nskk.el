;;; nskk-thread-pool.el --- Thread pool implementation for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Maintainer: NSKK Development Team
;; Keywords: japanese, input-method, skk, threading
;; Package-Requires: ((emacs "31.0"))

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
  "NSKK スレッドプール設定。"
  :group 'nskk
  :prefix "nskk-thread-pool-")

(defcustom nskk-thread-pool-default-size nil
  "デフォルトのスレッドプールサイズ。
nilの場合、CPUコア数に基づいて自動設定される。"
  :type '(choice (const :tag "Auto" nil)
                 (integer :tag "Fixed size"))
  :group 'nskk-thread-pool)

(defcustom nskk-thread-pool-idle-timeout 60
  "アイドル状態のワーカースレッドが終了するまでのタイムアウト秒数。"
  :type 'integer
  :group 'nskk-thread-pool)

(defcustom nskk-thread-pool-max-queue-size 1000
  "タスクキューの最大サイズ。"
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
  "利用可能なCPUコア数を取得する。
Emacs 31以降の`num-processors'を使用する。"
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
  "最適なスレッドプールサイズを計算する。
CPUコア数に基づいて決定する。"
  (let ((cpu-count (nskk-thread-pool--get-cpu-count)))
    ;; CPUコア数と同じか、やや少なめに設定
    (max 2 (min cpu-count 8))))

;;; キュー操作

(defun nskk-thread-pool--enqueue (pool task)
  "POOL にタスク TASK をエンキューする。"
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
  "POOL からタスクをデキューする。
タスクがない場合は nil を返す。"
  (let ((queue (nskk-thread-pool-task-queue pool)))
    (when queue
      (let ((task (car queue)))
        (setf (nskk-thread-pool-task-queue pool) (cdr queue))
        task))))

;;; ワーカースレッド

(defun nskk-thread-pool--worker-loop (pool worker-id)
  "ワーカースレッドのメインループ。
POOL はスレッドプール、WORKER-ID はワーカー識別子。"
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
  "POOL のワーカースレッドを作成する。
WORKER-ID はワーカー識別子。"
  (make-thread
   (lambda ()
     (nskk-thread-pool--worker-loop pool worker-id))
   (format "nskk-worker-%d" worker-id)))

;;; 公開API

;;;###autoload
(defun nskk-thread-pool-create (&optional size)
  "スレッドプールを作成する。
SIZE が指定されている場合はそのサイズ、
そうでない場合はCPUコア数に基づいて自動調整される。"
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
  "POOL にタスク TASK を投入する。
TASK は引数なしの関数。
CALLBACK はタスク完了時に呼ばれる関数（オプション）。
ERROR-HANDLER はエラー発生時に呼ばれる関数（オプション）。"
  (when (nskk-thread-pool-shutdown-flag pool)
    (error "Cannot submit task to shutdown pool"))

  (let ((thread-task (nskk-thread-task-create
                      :function task
                      :callback callback
                      :error-handler error-handler)))
    (nskk-thread-pool--enqueue pool thread-task)))

;;;###autoload
(defun nskk-thread-pool-shutdown (pool &optional wait)
  "POOL をシャットダウンする。
WAIT が non-nil の場合、全タスクの完了を待つ。"
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
  "POOL の状態を表示する。"
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
  "POOL の全タスク完了を TIMEOUT 秒まで待機する。
完了した場合 t を、タイムアウトした場合 nil を返す。"
  (let ((start-time (current-time)))
    (while (and (> (length (nskk-thread-pool-task-queue pool)) 0)
                (< (float-time (time-subtract (current-time) start-time))
                   timeout))
      (sleep-for 0.1))
    (= (length (nskk-thread-pool-task-queue pool)) 0)))

(defun nskk-thread-pool-available-p ()
  "スレッド機能が利用可能かチェックする。"
  (and (fboundp 'make-thread)
       (fboundp 'make-mutex)
       (fboundp 'make-condition-variable)))

(provide 'nskk-thread-pool)

;;; nskk-thread-pool.el ends here
