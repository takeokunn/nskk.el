;;; nskk-sync-primitives.el --- Synchronization primitives for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Maintainer: NSKK Development Team
;; Keywords: japanese, input-method, skk, threading, synchronization
;; Package-Requires: ((emacs "31.0"))

;;; Commentary:

;; このファイルは、スレッド間同期のためのプリミティブを実装します。
;;
;; 主な機能:
;; - Mutex実装（Emacs 31の`make-mutex`使用）
;; - Condition Variable
;; - Read-Write Lock
;; - Atomic Operations
;; - Semaphore
;;
;; 使用例:
;;   ;; Mutex
;;   (nskk-with-mutex my-mutex
;;     (setq counter (1+ counter)))
;;
;;   ;; Read-Write Lock
;;   (nskk-with-read-lock my-rwlock
;;     (message "Value: %s" shared-value))
;;
;;   ;; Atomic操作
;;   (nskk-atomic-increment counter-atom)

;;; Code:

(require 'cl-lib)

;;; Mutex拡張

;;;###autoload
(defmacro nskk-with-mutex (mutex &rest body)
  "MUTEX をロックして BODY を実行する。
実行後、必ず MUTEX をアンロックする。"
  (declare (indent 1) (debug t))
  `(with-mutex ,mutex
     ,@body))

;;; Read-Write Lock

(cl-defstruct (nskk-rwlock
               (:constructor nskk-rwlock--create)
               (:copier nil))
  "Read-Write Lock 構造体。
複数の読み取りスレッドを許可するが、書き込みは排他的に実行する。"
  (mutex nil :documentation "内部mutex")
  (read-count 0 :type integer :documentation "読み取りスレッド数")
  (write-mutex nil :documentation "書き込み用mutex"))

;;;###autoload
(defun nskk-rwlock-create ()
  "Read-Write Lock を作成する。"
  (nskk-rwlock--create
   :mutex (make-mutex)
   :read-count 0
   :write-mutex (make-mutex)))

;;;###autoload
(defun nskk-rwlock-read-lock (rwlock)
  "RWLOCK の読み取りロックを取得する。"
  (with-mutex (nskk-rwlock-mutex rwlock)
    (setf (nskk-rwlock-read-count rwlock)
          (1+ (nskk-rwlock-read-count rwlock)))

    ;; 最初の読み取りスレッドの場合、書き込みをブロック
    (when (= (nskk-rwlock-read-count rwlock) 1)
      (mutex-lock (nskk-rwlock-write-mutex rwlock)))))

;;;###autoload
(defun nskk-rwlock-read-unlock (rwlock)
  "RWLOCK の読み取りロックを解放する。"
  (with-mutex (nskk-rwlock-mutex rwlock)
    (setf (nskk-rwlock-read-count rwlock)
          (1- (nskk-rwlock-read-count rwlock)))

    ;; 最後の読み取りスレッドの場合、書き込みを許可
    (when (= (nskk-rwlock-read-count rwlock) 0)
      (mutex-unlock (nskk-rwlock-write-mutex rwlock)))))

;;;###autoload
(defun nskk-rwlock-write-lock (rwlock)
  "RWLOCK の書き込みロックを取得する。"
  (mutex-lock (nskk-rwlock-write-mutex rwlock)))

;;;###autoload
(defun nskk-rwlock-write-unlock (rwlock)
  "RWLOCK の書き込みロックを解放する。"
  (mutex-unlock (nskk-rwlock-write-mutex rwlock)))

;;;###autoload
(defmacro nskk-with-read-lock (rwlock &rest body)
  "RWLOCK の読み取りロックを取得して BODY を実行する。"
  (declare (indent 1) (debug t))
  `(progn
     (nskk-rwlock-read-lock ,rwlock)
     (unwind-protect
         (progn ,@body)
       (nskk-rwlock-read-unlock ,rwlock))))

;;;###autoload
(defmacro nskk-with-write-lock (rwlock &rest body)
  "RWLOCK の書き込みロックを取得して BODY を実行する。"
  (declare (indent 1) (debug t))
  `(progn
     (nskk-rwlock-write-lock ,rwlock)
     (unwind-protect
         (progn ,@body)
       (nskk-rwlock-write-unlock ,rwlock))))

;;; Semaphore

(cl-defstruct (nskk-semaphore
               (:constructor nskk-semaphore--create)
               (:copier nil))
  "Semaphore 構造体。"
  (count 0 :type integer :documentation "利用可能リソース数")
  (mutex nil :documentation "内部mutex")
  (condition nil :documentation "条件変数"))

;;;###autoload
(defun nskk-semaphore-create (initial-count)
  "初期カウント INITIAL-COUNT の Semaphore を作成する。"
  (let ((mutex (make-mutex)))
    (nskk-semaphore--create
     :count initial-count
     :mutex mutex
     :condition (make-condition-variable mutex))))

;;;###autoload
(defun nskk-semaphore-wait (semaphore &optional timeout)
  "SEMAPHORE を取得する。
TIMEOUT が指定されている場合、その秒数だけ待機する。
成功した場合 t、タイムアウトした場合 nil を返す。"
  (let ((deadline (when timeout (+ (float-time) timeout))))
    (catch 'nskk-semaphore-done
      (while t
        (with-mutex (nskk-semaphore-mutex semaphore)
          (when (> (nskk-semaphore-count semaphore) 0)
            (setf (nskk-semaphore-count semaphore)
                  (1- (nskk-semaphore-count semaphore)))
            (throw 'nskk-semaphore-done t))
          (unless timeout
            (condition-wait (nskk-semaphore-condition semaphore))))
        (if timeout
            (let ((remaining (- deadline (float-time))))
              (when (<= remaining 0)
                (throw 'nskk-semaphore-done nil))
              (sleep-for (min remaining 0.01)))
          ;; 無制限待機の場合はここに到達しない (condition-wait で待機)
          )))))

;;;###autoload
(defun nskk-semaphore-post (semaphore)
  "SEMAPHORE を解放する。"
  (with-mutex (nskk-semaphore-mutex semaphore)
    (setf (nskk-semaphore-count semaphore)
          (1+ (nskk-semaphore-count semaphore)))
    (condition-notify (nskk-semaphore-condition semaphore))))

;;;###autoload
(defmacro nskk-with-semaphore (semaphore &rest body)
  "SEMAPHORE を取得して BODY を実行する。"
  (declare (indent 1) (debug t))
  `(progn
     (nskk-semaphore-wait ,semaphore)
     (unwind-protect
         (progn ,@body)
       (nskk-semaphore-post ,semaphore))))

;;; Atomic Operations

(cl-defstruct (nskk-atomic
               (:constructor nskk-atomic--create)
               (:copier nil))
  "Atomic 変数構造体。"
  (value nil :documentation "保持する値")
  (mutex nil :documentation "内部mutex"))

;;;###autoload
(defun nskk-atomic-create (initial-value)
  "初期値 INITIAL-VALUE のアトミック変数を作成する。"
  (nskk-atomic--create
   :value initial-value
   :mutex (make-mutex)))

;;;###autoload
(defun nskk-atomic-get (atom)
  "ATOM の値を取得する。"
  (with-mutex (nskk-atomic-mutex atom)
    (nskk-atomic-value atom)))

;;;###autoload
(defun nskk-atomic-set (atom new-value)
  "ATOM に NEW-VALUE を設定する。"
  (with-mutex (nskk-atomic-mutex atom)
    (setf (nskk-atomic-value atom) new-value)))

;;;###autoload
(defun nskk-atomic-compare-and-swap (atom expected new-value)
  "ATOM が EXPECTED と等しい場合、NEW-VALUE に設定する。
成功した場合 t、失敗した場合 nil を返す。"
  (with-mutex (nskk-atomic-mutex atom)
    (if (equal (nskk-atomic-value atom) expected)
        (progn
          (setf (nskk-atomic-value atom) new-value)
          t)
      nil)))

;;;###autoload
(defun nskk-atomic-increment (atom &optional delta)
  "ATOM の値を DELTA だけ増やす（デフォルトは1）。
新しい値を返す。"
  (with-mutex (nskk-atomic-mutex atom)
    (let ((new-value (+ (nskk-atomic-value atom) (or delta 1))))
      (setf (nskk-atomic-value atom) new-value)
      new-value)))

;;;###autoload
(defun nskk-atomic-decrement (atom &optional delta)
  "ATOM の値を DELTA だけ減らす（デフォルトは1）。
新しい値を返す。"
  (nskk-atomic-increment atom (- (or delta 1))))

;;;###autoload
(defun nskk-atomic-update (atom update-fn)
  "UPDATE-FN を使って ATOM の値を更新する。
UPDATE-FN は現在の値を受け取り、新しい値を返す関数。
新しい値を返す。"
  (with-mutex (nskk-atomic-mutex atom)
    (let ((new-value (funcall update-fn (nskk-atomic-value atom))))
      (setf (nskk-atomic-value atom) new-value)
      new-value)))

;;; Barrier

(cl-defstruct (nskk-barrier
               (:constructor nskk-barrier--create)
               (:copier nil))
  "Barrier 構造体。
指定された数のスレッドが到達するまで待機する。"
  (count 0 :type integer :documentation "待機スレッド数")
  (threshold 0 :type integer :documentation "バリア閾値")
  (mutex nil :documentation "内部mutex")
  (condition nil :documentation "条件変数"))

;;;###autoload
(defun nskk-barrier-create (threshold)
  "THRESHOLD 個のスレッドが到達するまで待機する Barrier を作成する。"
  (let ((mutex (make-mutex)))
    (nskk-barrier--create
     :count 0
     :threshold threshold
     :mutex mutex
     :condition (make-condition-variable mutex))))

;;;###autoload
(defun nskk-barrier-wait (barrier)
  "BARRIER に到達し、全スレッドが到達するまで待機する。
最後のスレッドの場合 t、それ以外は nil を返す。"
  (with-mutex (nskk-barrier-mutex barrier)
    (setf (nskk-barrier-count barrier)
          (1+ (nskk-barrier-count barrier)))

    (if (>= (nskk-barrier-count barrier) (nskk-barrier-threshold barrier))
        (progn
          ;; 最後のスレッド: 全員に通知してリセット
          (setf (nskk-barrier-count barrier) 0)
          (condition-notify (nskk-barrier-condition barrier) t)
          t)
      ;; 待機
      (condition-wait (nskk-barrier-condition barrier))
      nil)))

;;; Thread-Local Storage (簡易実装)

(defvar nskk-tls--storage (make-hash-table :test 'equal)
  "スレッドローカルストレージ。
キーは (thread-id . variable-symbol) のペア。")

;;;###autoload
(defun nskk-tls-set (variable value)
  "現在のスレッドで VARIABLE に VALUE を設定する。"
  (let ((key (cons (current-thread) variable)))
    (puthash key value nskk-tls--storage)))

;;;###autoload
(defun nskk-tls-get (variable &optional default)
  "現在のスレッドの VARIABLE の値を取得する。
未設定の場合 DEFAULT を返す。"
  (let ((key (cons (current-thread) variable)))
    (gethash key nskk-tls--storage default)))

;;;###autoload
(defun nskk-tls-clear (variable)
  "現在のスレッドの VARIABLE をクリアする。"
  (let ((key (cons (current-thread) variable)))
    (remhash key nskk-tls--storage)))

;;; ユーティリティ

;;;###autoload
(defun nskk-sync-primitives-available-p ()
  "同期プリミティブが利用可能かチェックする。"
  (and (fboundp 'make-mutex)
       (fboundp 'make-condition-variable)
       (fboundp 'mutex-lock)
       (fboundp 'mutex-unlock)
       (fboundp 'condition-wait)
       (fboundp 'condition-notify)))

(provide 'nskk-sync-primitives)

;;; nskk-sync-primitives.el ends here
