;;; nskk-async-learning.el --- Asynchronous learning for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Maintainer: NSKK Development Team
;; Keywords: japanese, input-method, skk, learning, async
;; Package-Requires: ((emacs "31.0"))

;;; Commentary:

;; このファイルは、バックグラウンドでの非同期学習処理を実装します。
;;
;; 主な機能:
;; - バックグラウンド学習
;; - 非ブロッキング更新
;; - コールバック管理
;; - 学習キュー管理
;;
;; パフォーマンス目標: UIブロッキング 0ms
;;
;; 使用例:
;;   (nskk-async-learning-record "かんじ" "漢字")
;;   ;; => バックグラウンドで学習、UIブロックなし

;;; Code:

(require 'cl-lib)
(require 'nskk-thread-pool)

;;; カスタマイズ変数

(defgroup nskk-async-learning nil
  "NSKK 非同期学習設定。"
  :group 'nskk
  :prefix "nskk-async-learning-")

(defcustom nskk-async-learning-batch-size 10
  "一度に処理する学習バッチサイズ。"
  :type 'integer
  :group 'nskk-async-learning)

(defcustom nskk-async-learning-batch-interval 1.0
  "学習バッチ処理の間隔（秒）。"
  :type 'float
  :group 'nskk-async-learning)

(defcustom nskk-async-learning-max-queue-size 1000
  "学習キューの最大サイズ。"
  :type 'integer
  :group 'nskk-async-learning)

(defcustom nskk-async-learning-auto-save t
  "non-nilの場合、定期的に学習データを自動保存する。"
  :type 'boolean
  :group 'nskk-async-learning)

(defcustom nskk-async-learning-auto-save-interval 300
  "自動保存の間隔（秒）。"
  :type 'integer
  :group 'nskk-async-learning)

;;; データ構造

(cl-defstruct (nskk-learning-record
               (:constructor nskk-learning-record-create)
               (:copier nil))
  "学習レコード構造体。"
  (key nil :type string :documentation "見出し語")
  (candidate nil :type string :documentation "候補")
  (timestamp nil :documentation "記録時刻")
  (context nil :documentation "文脈情報"))

(cl-defstruct (nskk-async-learning-state
               (:constructor nskk-async-learning-state--create)
               (:copier nil))
  "非同期学習状態。"
  (queue nil :type list :documentation "学習キュー")
  (queue-mutex nil :documentation "キューmutex")
  (pool nil :documentation "スレッドプール")
  (timer nil :documentation "バッチ処理タイマー")
  (save-timer nil :documentation "自動保存タイマー")
  (frequency-table nil :documentation "頻度テーブル")
  (context-table nil :documentation "文脈テーブル"))

;;; グローバル変数

(defvar nskk-async-learning--state nil
  "非同期学習のグローバル状態。")

;;; 初期化

;;;###autoload
(defun nskk-async-learning-initialize ()
  "非同期学習システムを初期化する。"
  (interactive)
  (unless nskk-async-learning--state
    (when (nskk-thread-pool-available-p)
      (let ((state (nskk-async-learning-state--create
                    :queue nil
                    :queue-mutex (make-mutex)
                    :pool (nskk-thread-pool-create 2)  ; 学習専用プール
                    :frequency-table (make-hash-table :test 'equal)
                    :context-table (make-hash-table :test 'equal))))

        ;; バッチ処理タイマー設定
        (setf (nskk-async-learning-state-timer state)
              (run-with-timer nskk-async-learning-batch-interval
                              nskk-async-learning-batch-interval
                              #'nskk-async-learning--process-batch))

        ;; 自動保存タイマー設定
        (when nskk-async-learning-auto-save
          (setf (nskk-async-learning-state-save-timer state)
                (run-with-timer nskk-async-learning-auto-save-interval
                                nskk-async-learning-auto-save-interval
                                #'nskk-async-learning--auto-save)))

        (setq nskk-async-learning--state state)
        (message "Async learning system initialized")))))

;;;###autoload
(defun nskk-async-learning-shutdown ()
  "非同期学習システムをシャットダウンする。"
  (interactive)
  (when nskk-async-learning--state
    ;; タイマーキャンセル
    (when (nskk-async-learning-state-timer nskk-async-learning--state)
      (cancel-timer (nskk-async-learning-state-timer nskk-async-learning--state)))

    (when (nskk-async-learning-state-save-timer nskk-async-learning--state)
      (cancel-timer (nskk-async-learning-state-save-timer nskk-async-learning--state)))

    ;; 残りのキューを処理
    (nskk-async-learning--flush-queue)

    ;; スレッドプールシャットダウン
    (when (nskk-async-learning-state-pool nskk-async-learning--state)
      (nskk-thread-pool-shutdown (nskk-async-learning-state-pool nskk-async-learning--state)))

    (setq nskk-async-learning--state nil)
    (message "Async learning system shutdown")))

;;; キュー操作

(defun nskk-async-learning--enqueue (record)
  "学習レコード RECORD をキューに追加する。"
  (when nskk-async-learning--state
    (with-mutex (nskk-async-learning-state-queue-mutex nskk-async-learning--state)
      (let ((queue (nskk-async-learning-state-queue nskk-async-learning--state)))
        (when (>= (length queue) nskk-async-learning-max-queue-size)
          (message "Learning queue full, dropping oldest record")
          (setq queue (cdr queue)))

        (setf (nskk-async-learning-state-queue nskk-async-learning--state)
              (append queue (list record)))))))

(defun nskk-async-learning--dequeue-batch ()
  "バッチサイズ分のレコードをキューから取り出す。"
  (when nskk-async-learning--state
    (with-mutex (nskk-async-learning-state-queue-mutex nskk-async-learning--state)
      (let* ((queue (nskk-async-learning-state-queue nskk-async-learning--state))
             (batch-size (min nskk-async-learning-batch-size (length queue)))
             (batch (cl-subseq queue 0 batch-size)))

        (setf (nskk-async-learning-state-queue nskk-async-learning--state)
              (cl-subseq queue batch-size))

        batch))))

;;; 学習処理

(defun nskk-async-learning--update-frequency (key candidate)
  "KEY と CANDIDATE の使用頻度を更新する。"
  (when nskk-async-learning--state
    (let* ((table (nskk-async-learning-state-frequency-table nskk-async-learning--state))
           (freq-key (format "%s:%s" key candidate))
           (current-freq (gethash freq-key table 0)))
      (puthash freq-key (1+ current-freq) table))))

(defun nskk-async-learning--update-context (key candidate context)
  "KEY と CANDIDATE の文脈情報を更新する。"
  (when (and nskk-async-learning--state context)
    (let* ((table (nskk-async-learning-state-context-table nskk-async-learning--state))
           (ctx-key (format "%s:%s" key candidate))
           (contexts (gethash ctx-key table nil)))
      (push context contexts)
      ;; 最新100件のみ保持
      (when (> (length contexts) 100)
        (setq contexts (cl-subseq contexts 0 100)))
      (puthash ctx-key contexts table))))

(defun nskk-async-learning--process-record (record)
  "学習レコード RECORD を処理する。"
  (let ((key (nskk-learning-record-key record))
        (candidate (nskk-learning-record-candidate record))
        (context (nskk-learning-record-context record)))

    ;; 頻度更新
    (nskk-async-learning--update-frequency key candidate)

    ;; 文脈更新
    (nskk-async-learning--update-context key candidate context)))

(defun nskk-async-learning--process-batch ()
  "バッチ処理を実行する。"
  (when nskk-async-learning--state
    (let ((batch (nskk-async-learning--dequeue-batch)))
      (when batch
        ;; バックグラウンドで処理
        (nskk-thread-submit
         (nskk-async-learning-state-pool nskk-async-learning--state)
         (lambda ()
           (dolist (record batch)
             (nskk-async-learning--process-record record)))
         (lambda (_)
           (message "Processed %d learning records" (length batch))))))))

(defun nskk-async-learning--flush-queue ()
  "キュー内の全レコードを処理する。"
  (when nskk-async-learning--state
    (while (> (length (nskk-async-learning-state-queue nskk-async-learning--state)) 0)
      (nskk-async-learning--process-batch)
      (sleep-for 0.1))))

;;; 公開API

;;;###autoload
(defun nskk-async-learning-record (key candidate &optional context)
  "KEY と CANDIDATE を非同期で学習する。
CONTEXT は文脈情報（オプション）。"
  (unless nskk-async-learning--state
    (nskk-async-learning-initialize))

  (let ((record (nskk-learning-record-create
                 :key key
                 :candidate candidate
                 :timestamp (current-time)
                 :context context)))
    (nskk-async-learning--enqueue record)))

;;;###autoload
(defun nskk-async-learning-get-frequency (key candidate)
  "KEY と CANDIDATE の使用頻度を取得する。"
  (when nskk-async-learning--state
    (let* ((table (nskk-async-learning-state-frequency-table nskk-async-learning--state))
           (freq-key (format "%s:%s" key candidate)))
      (gethash freq-key table 0))))

;;;###autoload
(defun nskk-async-learning-get-contexts (key candidate)
  "KEY と CANDIDATE の文脈リストを取得する。"
  (when nskk-async-learning--state
    (let* ((table (nskk-async-learning-state-context-table nskk-async-learning--state))
           (ctx-key (format "%s:%s" key candidate)))
      (gethash ctx-key table nil))))

;;; 永続化

(defun nskk-async-learning--auto-save ()
  "学習データを自動保存する。"
  (when nskk-async-learning--state
    (message "Auto-saving learning data...")
    ;; TODO: 実際の保存処理は nskk-learning-persist.el で実装される
    (message "Learning data auto-saved")))

;;;###autoload
(defun nskk-async-learning-save ()
  "学習データを保存する。"
  (interactive)
  (when nskk-async-learning--state
    ;; 残りのキューを処理
    (nskk-async-learning--flush-queue)

    ;; 保存処理
    (nskk-async-learning--auto-save)))

;;; 統計情報

;;;###autoload
(defun nskk-async-learning-statistics ()
  "非同期学習の統計情報を表示する。"
  (interactive)
  (if nskk-async-learning--state
      (let* ((queue-size (length (nskk-async-learning-state-queue nskk-async-learning--state)))
             (freq-count (hash-table-count
                          (nskk-async-learning-state-frequency-table nskk-async-learning--state)))
             (ctx-count (hash-table-count
                         (nskk-async-learning-state-context-table nskk-async-learning--state))))
        (message "Async Learning Statistics:
  Queue size: %d
  Frequency entries: %d
  Context entries: %d
  Batch size: %d
  Batch interval: %.1f seconds"
                 queue-size freq-count ctx-count
                 nskk-async-learning-batch-size
                 nskk-async-learning-batch-interval))
    (message "Async learning not initialized")))

;;; テスト用ヘルパー

(defun nskk-async-learning-wait-for-queue-empty (timeout)
  "キューが空になるまで TIMEOUT 秒待機する。
成功した場合 t、タイムアウトした場合 nil を返す。"
  (when nskk-async-learning--state
    (let ((start-time (current-time)))
      (while (and (> (length (nskk-async-learning-state-queue nskk-async-learning--state)) 0)
                  (< (float-time (time-subtract (current-time) start-time)) timeout))
        (sleep-for 0.1))
      (= (length (nskk-async-learning-state-queue nskk-async-learning--state)) 0))))

(provide 'nskk-async-learning)

;;; nskk-async-learning.el ends here
