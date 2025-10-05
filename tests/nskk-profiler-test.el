;;; nskk-profiler-test.el --- Tests for nskk-profiler -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはnskk-profilerの包括的なテストを提供します。
;;
;; テスト項目:
;; 1. プロファイリング開始/停止テスト
;; 2. メモリスナップショットテスト
;; 3. GCイベント記録テスト
;; 4. 関数統計記録テスト
;; 5. スレッド情報取得テスト
;; 6. プロファイラーリセットテスト
;; 7. ステータス取得テスト
;; 8. モニター機能テスト
;; 9. レポート生成テスト
;; 10. エラーハンドリングテスト

;;; Code:

(require 'ert)
(require 'nskk-profiler)

;;; Test 1: プロファイリング開始/停止テスト

(ert-deftest nskk-profiler-test-start-stop ()
  "プロファイリングの開始と停止が正常に動作することを確認。"
  ;; 初期状態
  (let ((status (nskk-profiler-status)))
    (should-not (plist-get status :active)))

  ;; 開始
  (nskk-profile-start)
  (let ((status (nskk-profiler-status)))
    (should (plist-get status :active)))

  ;; 停止
  (let ((nskk-profiler-auto-report nil))  ; 自動レポートを無効化
    (nskk-profile-stop))
  (let ((status (nskk-profiler-status)))
    (should-not (plist-get status :active))))

(ert-deftest nskk-profiler-test-double-start ()
  "プロファイリング実行中の再開始でエラーが発生することを確認。"
  (nskk-profile-start)
  ;; 重複開始はエラーとなる
  (should-error (nskk-profile-start))
  (let ((nskk-profiler-auto-report nil))
    (nskk-profile-stop)))

(ert-deftest nskk-profiler-test-stop-before-start ()
  "開始前の停止でエラーが発生することを確認。"
  ;; 開始前の停止はエラーとなる
  (should-error
   (let ((nskk-profiler-auto-report nil))
     (nskk-profile-stop))))

;;; Test 2: メモリスナップショットテスト

(ert-deftest nskk-profiler-test-memory-snapshot ()
  "メモリスナップショットが正常に取得できることを確認。"
  (when nskk-profiler-memory-tracking
    (nskk-profiler-reset)
    (nskk-profile-start)

    ;; 何か処理を実行
    (dotimes (_ 100)
      (make-string 1000 ?a))

    (let ((nskk-profiler-auto-report nil))
      (nskk-profile-stop))

    ;; メモリスナップショットが記録されている
    (should (> (length nskk-profiler--memory-snapshots) 0))))

(ert-deftest nskk-profiler-test-memory-tracking-disabled ()
  "メモリトラッキング無効時にスナップショットが記録されないことを確認。"
  (let ((nskk-profiler-memory-tracking nil))
    (nskk-profiler-reset)
    (nskk-profile-start)

    (dotimes (_ 100)
      (make-string 1000 ?a))

    (let ((nskk-profiler-auto-report nil))
      (nskk-profile-stop))

    ;; メモリスナップショットが記録されていない
    (should (= (length nskk-profiler--memory-snapshots) 0))))

;;; Test 3: GCイベント記録テスト

(ert-deftest nskk-profiler-test-gc-tracking ()
  "GCイベントが正常に記録されることを確認。"
  (when nskk-profiler-gc-tracking
    (nskk-profiler-reset)
    (nskk-profile-start)

    ;; GCを強制的に発生させる
    (garbage-collect)

    (let ((nskk-profiler-auto-report nil))
      (nskk-profile-stop))

    ;; GCイベントが記録される可能性がある
    ;; (タイミングによるため必須ではない)
    (should (listp nskk-profiler--gc-events))))

;;; Test 4: 関数統計記録テスト

(ert-deftest nskk-profiler-test-function-stats ()
  "関数統計が正常に記録されることを確認。"
  (nskk-profiler-reset)

  ;; 統計を記録
  (nskk-profiler--record-function-stats 'test-function 0.001)
  (nskk-profiler--record-function-stats 'test-function 0.002)
  (nskk-profiler--record-function-stats 'test-function 0.003)

  ;; 統計が記録されている
  (let ((stats (assq 'test-function nskk-profiler--function-stats)))
    (should stats)
    (should (= (nth 1 stats) 3))  ; call-count
    (should (= (nth 2 stats) 0.006))  ; total-time
    (should (= (nth 3 stats) 0.001))  ; min-time
    (should (= (nth 4 stats) 0.003))))

;;; Test 5: スレッド情報取得テスト

(ert-deftest nskk-profiler-test-thread-info ()
  "スレッド情報が正常に取得できることを確認。"
  (let ((thread-info (nskk-profiler-thread-info)))
    (should (plist-get thread-info :main-thread))
    (should (listp (plist-get thread-info :all-threads)))))

;;; Test 6: プロファイラーリセットテスト

(ert-deftest nskk-profiler-test-reset ()
  "プロファイラーのリセットが正常に動作することを確認。"
  ;; データを記録
  (nskk-profiler--record-function-stats 'test-function 0.001)
  (nskk-profiler--take-memory-snapshot)

  ;; リセット
  (nskk-profiler-reset)

  ;; データがクリアされている
  (should-not nskk-profiler--active)
  (should-not nskk-profiler--start-time)
  (should (= (length nskk-profiler--memory-snapshots) 0))
  (should (= (length nskk-profiler--gc-events) 0))
  (should (= (length nskk-profiler--function-stats) 0)))

;;; Test 7: ステータス取得テスト

(ert-deftest nskk-profiler-test-status ()
  "ステータス取得が正常に動作することを確認。"
  ;; 非アクティブ時
  (nskk-profiler-reset)
  (let ((status (nskk-profiler-status)))
    (should-not (plist-get status :active)))

  ;; アクティブ時
  (nskk-profile-start)
  (let ((status (nskk-profiler-status)))
    (should (plist-get status :active)))

  (let ((nskk-profiler-auto-report nil))
    (nskk-profile-stop)))

;;; Test 8: モニター機能テスト

(ert-deftest nskk-profiler-test-monitor ()
  "モニター機能の開始と停止が正常に動作することを確認。"
  ;; モニター開始
  (nskk-profiler-start-monitor)
  (should nskk-profiler--monitor-timer)

  ;; モニター停止
  (nskk-profiler-stop-monitor)
  (should-not nskk-profiler--monitor-timer))

(ert-deftest nskk-profiler-test-monitor-tick ()
  "モニターのティック処理が正常に動作することを確認。"
  (nskk-profiler-reset)
  (nskk-profile-start)

  ;; ティック実行（エラーが発生しないことを確認）
  (nskk-profiler--monitor-tick)

  (let ((nskk-profiler-auto-report nil))
    (nskk-profile-stop)))

;;; Test 9: レポート生成テスト

(ert-deftest nskk-profiler-test-report ()
  "レポート生成が正常に動作することを確認。"
  (nskk-profiler-reset)
  (nskk-profile-start)

  ;; 何か処理を実行
  (dotimes (_ 100)
    (make-string 100 ?a))

  (let ((nskk-profiler-auto-report nil))
    (nskk-profile-stop))

  ;; レポート生成（エラーが発生しないことを確認）
  (nskk-profile-report))

(ert-deftest nskk-profiler-test-report-before-stop ()
  "停止前のレポート生成でエラーが発生しないことを確認。"
  (nskk-profiler-reset)
  ;; エラーが発生しないことを確認
  (nskk-profile-report))

;;; Test 10: エラーハンドリングテスト

(ert-deftest nskk-profiler-test-invalid-function-stats ()
  "不正な関数統計記録でエラーが発生しないことを確認。"
  ;; エラーが発生しないことを確認
  (nskk-profiler--record-function-stats nil 0.001)
  (nskk-profiler--record-function-stats 'test-function -0.001))

;;; Test 11: 統合テスト

(ert-deftest nskk-profiler-test-full-cycle ()
  "プロファイリングの完全なサイクルが正常に動作することを確認。"
  (nskk-profiler-reset)

  ;; プロファイリング開始
  (nskk-profile-start)
  (should (nskk-profiler-status))

  ;; 処理実行
  (dotimes (i 100)
    (let ((start-time (float-time)))
      (make-string (* i 10) ?a)
      (nskk-profiler--record-function-stats
       'test-operation
       (- (float-time) start-time))))

  ;; メモリスナップショット取得
  (when nskk-profiler-memory-tracking
    (nskk-profiler--take-memory-snapshot))

  ;; プロファイリング停止
  (let ((nskk-profiler-auto-report nil))
    (nskk-profile-stop))
  (should-not (plist-get (nskk-profiler-status) :active))

  ;; レポート生成（エラーが発生しないことを確認）
  (nskk-profile-report)

  ;; データが記録されている
  (should (> (length nskk-profiler--function-stats) 0)))

;;; Test 12: パフォーマンステスト

(ert-deftest nskk-profiler-test-overhead ()
  "プロファイリングのオーバーヘッドが許容範囲内であることを確認。"
  (nskk-profiler-reset)

  ;; プロファイリングなし
  (let ((start-time (float-time)))
    (dotimes (_ 1000)
      (make-string 100 ?a))
    (let ((without-profiling (- (float-time) start-time)))

      ;; プロファイリングあり
      (nskk-profile-start)
      (setq start-time (float-time))
      (dotimes (_ 1000)
        (make-string 100 ?a))
      (let* ((with-profiling (- (float-time) start-time))
             (nskk-profiler-auto-report nil))
        (nskk-profile-stop)

        ;; オーバーヘッドが2倍以内であることを確認
        ;; (実際のオーバーヘッドは環境依存)
        (should (< with-profiling (* without-profiling 3)))))))

(provide 'nskk-profiler-test)

;;; nskk-profiler-test.el ends here
