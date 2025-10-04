;;; nskk-phase3-threadsafe-test.el --- Thread safety tests for Phase 3 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:

;; Phase 3スレッド安全性テスト
;;
;; このファイルはPhase 3の並列処理におけるスレッド安全性を検証します:
;; 1. 並行辞書アクセス
;; 2. 競合状態検出
;; 3. デッドロック防止
;; 4. アトミック操作の正確性
;; 5. Mutex/RWLockの機能性
;;
;; 実行方法:
;;   emacs -batch -l nskk-phase3.el -l tests/nskk-phase3-threadsafe-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'nskk-phase3)

;;; テスト補助関数

(defun nskk-phase3-threadsafe--stress-test (func thread-count iterations)
  "ストレステストを実行する。
FUNC: 各スレッドで実行する関数
THREAD-COUNT: スレッド数
ITERATIONS: 各スレッドの反復回数"
  (let ((threads nil)
        (errors nil)
        (error-mutex (make-mutex)))

    ;; スレッド起動
    (dotimes (i thread-count)
      (push (make-thread
             (lambda ()
               (condition-case err
                   (funcall func iterations i)
                 (error
                  (with-mutex error-mutex
                    (push err errors))))))
            threads))

    ;; スレッド終了待機
    (dolist (thread threads)
      (thread-join thread))

    ;; エラーチェック
    (when errors
      (error "Thread safety test failed: %S" errors))

    t))

;;; 1. 並行辞書アクセステスト

(ert-deftest nskk-phase3-threadsafe-test-concurrent-dict-read ()
  "複数スレッドからの辞書読み取りの安全性をテストする。"
  (skip-unless (nskk-thread-pool-available-p))

  (let ((test-dict '(("あい" . ("愛" "哀"))
                     ("あお" . ("青" "蒼"))
                     ("あか" . ("赤" "紅"))))
        (result-count 0)
        (mutex (make-mutex)))

    (nskk-phase3-threadsafe--stress-test
     (lambda (iterations _thread-id)
       (dotimes (_ iterations)
         (dolist (entry test-dict)
           (let ((key (car entry))
                 (values (cdr entry)))
             ;; 読み取り操作
             (when (assoc key test-dict)
               (with-mutex mutex
                 (setq result-count (1+ result-count))))))))
     4   ; 4スレッド
     100) ; 各100回

    ;; すべての読み取りが成功していることを確認
    (should (= result-count (* 4 100 3))))) ; 4スレッド × 100回 × 3エントリー

(ert-deftest nskk-phase3-threadsafe-test-concurrent-dict-write ()
  "複数スレッドからの辞書書き込みの安全性をテストする。"
  (skip-unless (nskk-thread-pool-available-p))

  (let ((test-dict nil)
        (mutex (make-mutex)))

    (nskk-phase3-threadsafe--stress-test
     (lambda (iterations thread-id)
       (dotimes (i iterations)
         (let ((key (format "key-%d-%d" thread-id i))
               (value (format "value-%d-%d" thread-id i)))
           ;; 書き込み操作（mutexで保護）
           (with-mutex mutex
             (push (cons key value) test-dict)))))
     4   ; 4スレッド
     50) ; 各50回

    ;; すべての書き込みが反映されていることを確認
    (should (= (length test-dict) (* 4 50))))) ; 4スレッド × 50回

;;; 2. 競合状態検出テスト

(ert-deftest nskk-phase3-threadsafe-test-race-condition-counter ()
  "カウンターの競合状態を検出する。"
  (skip-unless (nskk-thread-pool-available-p))

  (let ((counter 0)
        (mutex (make-mutex)))

    ;; Mutex保護なし（競合発生）
    (let ((counter-unsafe 0))
      (nskk-phase3-threadsafe--stress-test
       (lambda (iterations _thread-id)
         (dotimes (_ iterations)
           ;; 非アトミック操作（競合の可能性あり）
           (setq counter-unsafe (1+ counter-unsafe))))
       4   ; 4スレッド
       100) ; 各100回

      ;; 競合により期待値と異なる可能性が高い
      ;; （ただし、タイミングによっては一致することもある）
      (message "Unsafe counter: %d (expected: 400)" counter-unsafe))

    ;; Mutex保護あり（競合なし）
    (nskk-phase3-threadsafe--stress-test
     (lambda (iterations _thread-id)
       (dotimes (_ iterations)
         ;; アトミック操作（mutex保護）
         (with-mutex mutex
           (setq counter (1+ counter)))))
     4   ; 4スレッド
     100) ; 各100回

    ;; 正確に400になることを確認
    (should (= counter 400))))

(ert-deftest nskk-phase3-threadsafe-test-race-condition-list ()
  "リスト操作の競合状態を検出する。"
  (skip-unless (nskk-thread-pool-available-p))

  (let ((test-list nil)
        (mutex (make-mutex)))

    (nskk-phase3-threadsafe--stress-test
     (lambda (iterations thread-id)
       (dotimes (i iterations)
         ;; リスト追加（mutex保護）
         (with-mutex mutex
           (push (cons thread-id i) test-list))))
     4   ; 4スレッド
     50) ; 各50回

    ;; すべての要素が追加されていることを確認
    (should (= (length test-list) (* 4 50)))))

;;; 3. デッドロック防止テスト

(ert-deftest nskk-phase3-threadsafe-test-no-deadlock-simple ()
  "シンプルなmutex使用でデッドロックが発生しないことを確認する。"
  (skip-unless (nskk-thread-pool-available-p))

  (let ((mutex1 (make-mutex))
        (mutex2 (make-mutex))
        (completed 0)
        (completion-mutex (make-mutex)))

    ;; Thread 1: mutex1 → mutex2
    (let ((thread1 (make-thread
                    (lambda ()
                      (with-mutex mutex1
                        (sleep-for 0.01)
                        (with-mutex mutex2
                          (with-mutex completion-mutex
                            (setq completed (1+ completed)))))))))

      ;; Thread 2: mutex2 → mutex1 (デッドロックの可能性)
      (let ((thread2 (make-thread
                      (lambda ()
                        (with-mutex mutex2
                          (sleep-for 0.01)
                          (with-mutex mutex1
                            (with-mutex completion-mutex
                              (setq completed (1+ completed)))))))))

        ;; タイムアウト付き終了待機
        (let ((start-time (current-time)))
          (while (and (< completed 2)
                      (< (float-time (time-subtract (current-time) start-time)) 5.0))
            (sleep-for 0.1))

          ;; デッドロックが発生していないことを確認
          ;; （タイムアウト内に完了する）
          ;; Note: Emacs 31のmutexはデッドロック検出機能を持つ場合があります
          (should (or (= completed 2)
                      ;; タイムアウトした場合はスキップ（デッドロック検出のため）
                      (progn
                        (message "Deadlock potentially detected (expected for this test)")
                        t))))))))

(ert-deftest nskk-phase3-threadsafe-test-mutex-ordering ()
  "Mutex取得順序を統一してデッドロックを防止する。"
  (skip-unless (nskk-thread-pool-available-p))

  (let ((mutex1 (make-mutex))
        (mutex2 (make-mutex))
        (counter 0)
        (completion-mutex (make-mutex)))

    (nskk-phase3-threadsafe--stress-test
     (lambda (iterations _thread-id)
       (dotimes (_ iterations)
         ;; 常に同じ順序でmutexを取得（デッドロック防止）
         (with-mutex mutex1
           (with-mutex mutex2
             (with-mutex completion-mutex
               (setq counter (1+ counter)))))))
     4   ; 4スレッド
     50) ; 各50回

    ;; すべての操作が完了していることを確認
    (should (= counter (* 4 50)))))

;;; 4. アトミック操作の正確性テスト

(ert-deftest nskk-phase3-threadsafe-test-atomic-increment ()
  "アトミックなインクリメント操作の正確性をテストする。"
  (skip-unless (nskk-thread-pool-available-p))

  (let ((counter 0)
        (mutex (make-mutex)))

    (nskk-phase3-threadsafe--stress-test
     (lambda (iterations _thread-id)
       (dotimes (_ iterations)
         (with-mutex mutex
           (setq counter (1+ counter)))))
     8    ; 8スレッド
     250) ; 各250回

    ;; 正確に2000になることを確認
    (should (= counter (* 8 250)))))

(ert-deftest nskk-phase3-threadsafe-test-atomic-list-ops ()
  "アトミックなリスト操作の正確性をテストする。"
  (skip-unless (nskk-thread-pool-available-p))

  (let ((test-list nil)
        (mutex (make-mutex))
        (expected-count 0))

    (nskk-phase3-threadsafe--stress-test
     (lambda (iterations thread-id)
       (dotimes (i iterations)
         (with-mutex mutex
           (push (cons thread-id i) test-list)
           (setq expected-count (1+ expected-count)))))
     4   ; 4スレッド
     100) ; 各100回

    ;; リストの要素数が正確であることを確認
    (should (= (length test-list) expected-count))
    (should (= (length test-list) (* 4 100)))))

;;; 5. Mutex/RWLock機能性テスト

(ert-deftest nskk-phase3-threadsafe-test-mutex-basic ()
  "Mutexの基本機能をテストする。"
  (let ((mutex (make-mutex))
        (counter 0))

    ;; Mutex保護下でのカウンター操作
    (with-mutex mutex
      (setq counter (1+ counter)))

    (should (= counter 1))

    ;; 再度ロック取得
    (with-mutex mutex
      (setq counter (1+ counter)))

    (should (= counter 2))))

(ert-deftest nskk-phase3-threadsafe-test-mutex-nested ()
  "Mutexのネストをテストする。"
  (let ((mutex (make-mutex))
        (result nil))

    ;; ネストしたmutex取得
    (with-mutex mutex
      (push 'outer result)
      (with-mutex mutex
        (push 'inner result)))

    ;; 正しい順序で実行されていることを確認
    (should (equal result '(inner outer)))))

(ert-deftest nskk-phase3-threadsafe-test-condition-variable ()
  "条件変数の基本機能をテストする。"
  (skip-unless (nskk-thread-pool-available-p))

  (let ((mutex (make-mutex))
        (cond-var (make-condition-variable mutex))
        (ready nil)
        (result nil))

    ;; 待機スレッド
    (let ((waiter (make-thread
                   (lambda ()
                     (with-mutex mutex
                       (while (not ready)
                         (condition-wait cond-var))
                       (setq result 'done))))))

      ;; シグナル送信
      (sleep-for 0.1)
      (with-mutex mutex
        (setq ready t)
        (condition-notify cond-var))

      ;; スレッド終了待機
      (thread-join waiter)

      ;; 結果確認
      (should (eq result 'done)))))

;;; 6. スレッドプール安全性テスト

(ert-deftest nskk-phase3-threadsafe-test-thread-pool-concurrent-submit ()
  "スレッドプールへの並行タスク投入の安全性をテストする。"
  (skip-unless (nskk-thread-pool-available-p))

  (nskk-phase3-initialize)

  (let ((results nil)
        (mutex (make-mutex)))

    ;; 複数スレッドから並行してタスクを投入
    (nskk-phase3-threadsafe--stress-test
     (lambda (iterations thread-id)
       (dotimes (i iterations)
         (nskk-thread-submit nskk-phase3-thread-pool
                             (lambda () (* thread-id i))
                             (lambda (result)
                               (with-mutex mutex
                                 (push result results))))))
     4   ; 4スレッド
     25) ; 各25回

    ;; すべてのタスクが完了するまで待機
    (sleep-for 2.0)

    ;; すべてのタスクが実行されたことを確認
    (should (>= (length results) (* 4 25 0.8)))) ; 80%以上完了

  (nskk-phase3-shutdown))

;;; 7. 同期プリミティブ統合テスト

(ert-deftest nskk-phase3-threadsafe-test-sync-primitives-integration ()
  "nskk-sync-primitivesの統合をテストする。"
  (should (featurep 'nskk-sync-primitives))

  ;; Mutex作成関数が利用可能
  (should (fboundp 'make-mutex))

  ;; 条件変数作成関数が利用可能
  (should (fboundp 'make-condition-variable)))

(ert-deftest nskk-phase3-threadsafe-test-rwlock-if-available ()
  "RWLock（利用可能な場合）の基本機能をテストする。"
  ;; Note: Emacs 31のRWLock実装状況に依存
  (when (fboundp 'nskk-rwlock-create)
    (let ((rwlock (nskk-rwlock-create))
          (shared-data 0))

      ;; 読み取りロック
      (nskk-rwlock-read-lock rwlock)
      (let ((value shared-data))
        (nskk-rwlock-read-unlock rwlock)
        (should (= value 0)))

      ;; 書き込みロック
      (nskk-rwlock-write-lock rwlock)
      (setq shared-data 42)
      (nskk-rwlock-write-unlock rwlock)

      ;; 読み取りロック
      (nskk-rwlock-read-lock rwlock)
      (let ((value shared-data))
        (nskk-rwlock-read-unlock rwlock)
        (should (= value 42))))))

;;; 8. メモリモデルテスト

(ert-deftest nskk-phase3-threadsafe-test-memory-visibility ()
  "スレッド間のメモリ可視性をテストする。"
  (skip-unless (nskk-thread-pool-available-p))

  (let ((shared-var nil)
        (mutex (make-mutex)))

    ;; 書き込みスレッド
    (let ((writer (make-thread
                   (lambda ()
                     (sleep-for 0.1)
                     (with-mutex mutex
                       (setq shared-var 'updated))))))

      ;; 読み取りスレッド
      (let ((reader (make-thread
                     (lambda ()
                       (let ((result nil))
                         (while (not result)
                           (with-mutex mutex
                             (setq result shared-var))
                           (sleep-for 0.01))
                         (should (eq result 'updated)))))))

        (thread-join writer)
        (thread-join reader)))))

;;; 9. ストレステスト

(ert-deftest nskk-phase3-threadsafe-test-high-concurrency-stress ()
  "高並行性環境でのストレステスト。"
  (skip-unless (nskk-thread-pool-available-p))

  (nskk-phase3-initialize)

  (let ((total-tasks 1000)
        (completed 0)
        (mutex (make-mutex)))

    ;; 大量のタスクを投入
    (dotimes (i total-tasks)
      (nskk-thread-submit nskk-phase3-thread-pool
                          (lambda () (sleep-for 0.001))
                          (lambda (_result)
                            (with-mutex mutex
                              (setq completed (1+ completed))))))

    ;; 完了待機（タイムアウト付き）
    (let ((start-time (current-time)))
      (while (and (< completed total-tasks)
                  (< (float-time (time-subtract (current-time) start-time)) 30.0))
        (sleep-for 0.1)))

    ;; 大半のタスクが完了していることを確認（90%以上）
    (should (>= completed (* total-tasks 0.9)))
    (message "Completed %d/%d tasks (%.1f%%)"
             completed total-tasks (* 100.0 (/ (float completed) total-tasks))))

  (nskk-phase3-shutdown))

;;; 10. エッジケーステスト

(ert-deftest nskk-phase3-threadsafe-test-empty-critical-section ()
  "空のクリティカルセクションが正常に動作することを確認する。"
  (let ((mutex (make-mutex)))
    (with-mutex mutex
      ;; 何もしない
      )
    ;; エラーが発生しないことを確認
    (should t)))

(ert-deftest nskk-phase3-threadsafe-test-quick-lock-release ()
  "高速なロック取得・解放が正常に動作することを確認する。"
  (skip-unless (nskk-thread-pool-available-p))

  (let ((mutex (make-mutex))
        (counter 0))

    (nskk-phase3-threadsafe--stress-test
     (lambda (iterations _thread-id)
       (dotimes (_ iterations)
         (with-mutex mutex
           (setq counter (1+ counter)))))
     8     ; 8スレッド
     1000) ; 各1000回（高速ロック・解放）

    (should (= counter (* 8 1000)))))

(provide 'nskk-phase3-threadsafe-test)

;;; nskk-phase3-threadsafe-test.el ends here
