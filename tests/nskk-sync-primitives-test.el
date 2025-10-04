;;; nskk-sync-primitives-test.el --- Tests for nskk-sync-primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:

;; nskk-sync-primitives.el のテスト

;;; Code:

(require 'ert)
(require 'nskk-sync-primitives)

;;; Mutex拡張テスト

(ert-deftest nskk-sync-primitives-test-with-mutex ()
  "nskk-with-mutex マクロのテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((mutex (make-mutex))
        (counter 0))
    (nskk-with-mutex mutex
      (setq counter (1+ counter)))
    (should (= counter 1))))

;;; Read-Write Lock テスト

(ert-deftest nskk-sync-primitives-test-rwlock-create ()
  "Read-Write Lock 作成のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((rwlock (nskk-rwlock-create)))
    (should (nskk-rwlock-p rwlock))
    (should (= (nskk-rwlock-read-count rwlock) 0))))

(ert-deftest nskk-sync-primitives-test-rwlock-read ()
  "読み取りロックのテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((rwlock (nskk-rwlock-create))
        (value 42))
    (nskk-with-read-lock rwlock
      (should (= value 42)))))

(ert-deftest nskk-sync-primitives-test-rwlock-write ()
  "書き込みロックのテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((rwlock (nskk-rwlock-create))
        (value 0))
    (nskk-with-write-lock rwlock
      (setq value 42))
    (should (= value 42))))

(ert-deftest nskk-sync-primitives-test-rwlock-multiple-readers ()
  "複数読み取りスレッドのテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((rwlock (nskk-rwlock-create))
        (shared-value 100)
        (results nil)
        (mutex (make-mutex)))

    ;; 複数の読み取りスレッドを起動
    (dotimes (i 5)
      (make-thread
       (lambda ()
         (nskk-with-read-lock rwlock
           (with-mutex mutex
             (push shared-value results))))))

    (sleep-for 1.0)
    (should (= (length results) 5))
    (should (cl-every (lambda (x) (= x 100)) results))))

;;; Semaphore テスト

(ert-deftest nskk-sync-primitives-test-semaphore-create ()
  "Semaphore 作成のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((sem (nskk-semaphore-create 3)))
    (should (nskk-semaphore-p sem))
    (should (= (nskk-semaphore-count sem) 3))))

(ert-deftest nskk-sync-primitives-test-semaphore-wait-post ()
  "Semaphore wait/post のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((sem (nskk-semaphore-create 1)))
    ;; wait成功
    (should (nskk-semaphore-wait sem))
    (should (= (nskk-semaphore-count sem) 0))

    ;; post
    (nskk-semaphore-post sem)
    (should (= (nskk-semaphore-count sem) 1))))

(ert-deftest nskk-sync-primitives-test-semaphore-timeout ()
  "Semaphore タイムアウトのテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((sem (nskk-semaphore-create 0)))
    ;; タイムアウト
    (should-not (nskk-semaphore-wait sem 0.1))))

(ert-deftest nskk-sync-primitives-test-semaphore-macro ()
  "nskk-with-semaphore マクロのテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((sem (nskk-semaphore-create 1))
        (counter 0))
    (nskk-with-semaphore sem
      (setq counter (1+ counter)))
    (should (= counter 1))
    (should (= (nskk-semaphore-count sem) 1))))

;;; Atomic Operations テスト

(ert-deftest nskk-sync-primitives-test-atomic-create ()
  "Atomic 変数作成のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((atom (nskk-atomic-create 42)))
    (should (nskk-atomic-p atom))
    (should (= (nskk-atomic-get atom) 42))))

(ert-deftest nskk-sync-primitives-test-atomic-get-set ()
  "Atomic get/set のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((atom (nskk-atomic-create 0)))
    (nskk-atomic-set atom 42)
    (should (= (nskk-atomic-get atom) 42))))

(ert-deftest nskk-sync-primitives-test-atomic-cas ()
  "Compare-and-Swap のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((atom (nskk-atomic-create 10)))
    ;; 成功
    (should (nskk-atomic-compare-and-swap atom 10 20))
    (should (= (nskk-atomic-get atom) 20))

    ;; 失敗
    (should-not (nskk-atomic-compare-and-swap atom 10 30))
    (should (= (nskk-atomic-get atom) 20))))

(ert-deftest nskk-sync-primitives-test-atomic-increment ()
  "Atomic increment のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((atom (nskk-atomic-create 0)))
    (should (= (nskk-atomic-increment atom) 1))
    (should (= (nskk-atomic-increment atom 5) 6))
    (should (= (nskk-atomic-get atom) 6))))

(ert-deftest nskk-sync-primitives-test-atomic-decrement ()
  "Atomic decrement のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((atom (nskk-atomic-create 10)))
    (should (= (nskk-atomic-decrement atom) 9))
    (should (= (nskk-atomic-decrement atom 3) 6))
    (should (= (nskk-atomic-get atom) 6))))

(ert-deftest nskk-sync-primitives-test-atomic-update ()
  "Atomic update のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((atom (nskk-atomic-create 5)))
    (nskk-atomic-update atom (lambda (x) (* x 2)))
    (should (= (nskk-atomic-get atom) 10))))

(ert-deftest nskk-sync-primitives-test-atomic-concurrent-increment ()
  "並行incrementのテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((atom (nskk-atomic-create 0))
        (threads nil))

    ;; 10スレッドでそれぞれ100回increment
    (dotimes (i 10)
      (push (make-thread
             (lambda ()
               (dotimes (_ 100)
                 (nskk-atomic-increment atom))))
            threads))

    ;; 全スレッド完了待機
    (dolist (thread threads)
      (thread-join thread))

    ;; 正確に1000回incrementされている
    (should (= (nskk-atomic-get atom) 1000))))

;;; Barrier テスト

(ert-deftest nskk-sync-primitives-test-barrier-create ()
  "Barrier 作成のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((barrier (nskk-barrier-create 3)))
    (should (nskk-barrier-p barrier))
    (should (= (nskk-barrier-threshold barrier) 3))
    (should (= (nskk-barrier-count barrier) 0))))

(ert-deftest nskk-sync-primitives-test-barrier-wait ()
  "Barrier wait のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (let ((barrier (nskk-barrier-create 3))
        (results nil)
        (mutex (make-mutex)))

    ;; 3スレッドを起動
    (dotimes (i 3)
      (make-thread
       (lambda ()
         (let ((is-last (nskk-barrier-wait barrier)))
           (with-mutex mutex
             (push is-last results))))))

    (sleep-for 1.0)

    ;; 全スレッドが通過
    (should (= (length results) 3))
    ;; 1つだけがtを返す
    (should (= (cl-count t results) 1))))

;;; Thread-Local Storage テスト

(ert-deftest nskk-sync-primitives-test-tls-get-set ()
  "TLS get/set のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (nskk-tls-set 'test-var 42)
  (should (= (nskk-tls-get 'test-var) 42))
  (nskk-tls-clear 'test-var))

(ert-deftest nskk-sync-primitives-test-tls-default ()
  "TLS デフォルト値のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (should (= (nskk-tls-get 'nonexistent 99) 99)))

(ert-deftest nskk-sync-primitives-test-tls-clear ()
  "TLS clear のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  (nskk-tls-set 'test-var 42)
  (nskk-tls-clear 'test-var)
  (should (null (nskk-tls-get 'test-var))))

(ert-deftest nskk-sync-primitives-test-tls-thread-isolation ()
  "TLS スレッド分離のテスト。"
  (skip-unless (nskk-sync-primitives-available-p))
  ;; メインスレッドで設定
  (nskk-tls-set 'test-var 1)

  (let ((result nil))
    ;; 別スレッドで取得
    (let ((thread (make-thread
                   (lambda ()
                     ;; 別スレッドでは未設定
                     (setq result (nskk-tls-get 'test-var 999))))))
      (thread-join thread))

    ;; 別スレッドではデフォルト値が返る
    (should (= result 999)))

  (nskk-tls-clear 'test-var))

;;; 利用可能性チェック

(ert-deftest nskk-sync-primitives-test-availability ()
  "同期プリミティブの利用可能性チェック。"
  (let ((available (nskk-sync-primitives-available-p)))
    (should (booleanp available))))

(provide 'nskk-sync-primitives-test)

;;; nskk-sync-primitives-test.el ends here
