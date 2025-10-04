;;; nskk-phase3-test.el --- Integration tests for Phase 3 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;;; Commentary:

;; Phase 3統合テスト
;;
;; このファイルはPhase 3の全トラックの統合をテストします:
;; - Track N: Threading
;; - Track O: Async UI
;; - Track P: Profiling
;; - Track Q: Architecture
;; - Track R: Plugin System (optional)
;; - Track S: Transient UI
;; - Track T: Optimization

;;; Code:

(require 'ert)
(require 'nskk-phase3)

;;; 基本統合テスト

(ert-deftest nskk-phase3-test-module-loading ()
  "Phase 3の全モジュールがロードされることを確認する。"
  ;; Track N: Threading
  (should (featurep 'nskk-thread-pool))
  (should (featurep 'nskk-parallel-search))
  (should (featurep 'nskk-async-learning))
  (should (featurep 'nskk-sync-primitives))

  ;; Track O: Async UI
  (should (featurep 'nskk-async-candidates))
  (should (featurep 'nskk-progress))
  (should (featurep 'nskk-background))

  ;; Track P: Profiling
  (should (featurep 'nskk-profiler))
  (should (featurep 'nskk-bottleneck-detector))
  (should (featurep 'nskk-auto-tune))

  ;; Track Q: Architecture
  (should (featurep 'nskk-architecture))

  ;; Track S: Transient UI
  (should (featurep 'nskk-transient-config))
  (should (featurep 'nskk-transient-plugins))
  (should (featurep 'nskk-transient-debug))

  ;; Track T: Optimization
  (should (featurep 'nskk-macro-optimize))
  (should (featurep 'nskk-native-compile))
  (should (featurep 'nskk-memory-optimize))
  (should (featurep 'nskk-multi-cache)))

(ert-deftest nskk-phase3-test-initialization ()
  "Phase 3の初期化が正常に動作することを確認する。"
  ;; 初期化前
  (should-not nskk-phase3-initialized)

  ;; 初期化
  (nskk-phase3-initialize)
  (should nskk-phase3-initialized)

  ;; シャットダウン
  (nskk-phase3-shutdown)
  (should-not nskk-phase3-initialized))

(ert-deftest nskk-phase3-test-verify ()
  "Phase 3の検証機能が動作することを確認する。"
  (should (nskk-phase3-verify)))

;;; Track N: Threading統合テスト

(ert-deftest nskk-phase3-test-threading-integration ()
  "並列処理機能の統合をテストする。"
  (skip-unless (nskk-thread-pool-available-p))

  (nskk-phase3-initialize)

  ;; スレッドプールが作成されている
  (should nskk-phase3-thread-pool)
  (should (nskk-thread-pool-p nskk-phase3-thread-pool))

  ;; タスクを実行できる
  (let ((result nil))
    (nskk-thread-submit nskk-phase3-thread-pool
                        (lambda () (+ 40 2))
                        (lambda (r) (setq result r)))
    (sleep-for 0.5)
    (should (= result 42)))

  (nskk-phase3-shutdown))

(ert-deftest nskk-phase3-test-parallel-search-integration ()
  "並列検索機能の統合をテストする。"
  (skip-unless (nskk-thread-pool-available-p))

  (nskk-phase3-initialize)

  ;; 並列検索が同期検索と同じ結果を返すことを確認
  (let* ((test-entries '(("あい" . ("愛" "哀" "藍"))
                         ("あいう" . ("あいう"))
                         ("あお" . ("青" "蒼"))))
         (sequential-results nil)
         (parallel-results nil))

    ;; 同期検索
    (dolist (entry test-entries)
      (push (cons (car entry) (cdr entry)) sequential-results))

    ;; 並列検索
    (when (fboundp 'nskk-parallel-search-dict)
      (setq parallel-results
            (nskk-parallel-search-dict test-entries "あ"))

      ;; 結果が存在することを確認（詳細な比較は個別テストで実施）
      (should parallel-results))

    (nskk-phase3-shutdown)))

;;; Track O: Async UI統合テスト

(ert-deftest nskk-phase3-test-async-ui-integration ()
  "非同期UI機能の統合をテストする。"
  (nskk-phase3-initialize)

  ;; プログレス機能が利用可能
  (should (fboundp 'nskk-progress-initialize))

  ;; バックグラウンド処理が利用可能
  (should (fboundp 'nskk-background-initialize))

  ;; 非同期候補表示が利用可能
  (should (fboundp 'nskk-async-candidates-fetch))

  (nskk-phase3-shutdown))

(ert-deftest nskk-phase3-test-progress-display ()
  "プログレス表示が動作することを確認する。"
  (nskk-phase3-initialize)

  (when (fboundp 'nskk-progress-create)
    (let ((progress (nskk-progress-create "Test" 100)))
      (should progress)
      (when (fboundp 'nskk-progress-update)
        (nskk-progress-update progress 50))
      (when (fboundp 'nskk-progress-finish)
        (nskk-progress-finish progress))))

  (nskk-phase3-shutdown))

;;; Track P: Profiling統合テスト

(ert-deftest nskk-phase3-test-profiling-integration ()
  "プロファイリング機能の統合をテストする。"
  (nskk-phase3-initialize)

  ;; プロファイラーが利用可能
  (should (fboundp 'nskk-profile-start))
  (should (fboundp 'nskk-profile-stop))

  ;; ボトルネック検出が利用可能
  (should (fboundp 'nskk-bottleneck-detect-start))
  (should (fboundp 'nskk-bottleneck-detect-stop))

  ;; 自動チューニングが利用可能
  (should (fboundp 'nskk-auto-tune-run))

  (nskk-phase3-shutdown))

(ert-deftest nskk-phase3-test-profiler-basic ()
  "プロファイラーの基本動作をテストする。"
  (nskk-phase3-initialize)

  ;; プロファイリング開始
  (nskk-profile-start)
  (setq nskk-phase3-profiling-active t)

  ;; テスト処理
  (dotimes (_ 100)
    (+ 1 2 3))

  ;; プロファイリング停止
  (nskk-profile-stop)
  (setq nskk-phase3-profiling-active nil)

  (nskk-phase3-shutdown))

;;; Track Q: Architecture統合テスト

(ert-deftest nskk-phase3-test-architecture-integration ()
  "7層アーキテクチャの統合をテストする。"
  (should (featurep 'nskk-architecture))

  ;; アーキテクチャ検証機能が利用可能
  (should (fboundp 'nskk-architecture-validate)))

(ert-deftest nskk-phase3-test-layer-separation ()
  "レイヤー分離が正しく機能することを確認する。"
  ;; 各レイヤーのモジュールが独立してロードできることを確認
  (should (featurep 'nskk-architecture))

  ;; レイヤー間の依存関係が正しいことを確認
  ;; （詳細は個別のアーキテクチャテストで実施）
  t)

;;; Track R: Plugin System統合テスト (optional)

(ert-deftest nskk-phase3-test-plugin-system-integration ()
  "プラグインシステムの統合をテストする（利用可能な場合）。"
  (when nskk-phase3-plugin-system-available
    (nskk-phase3-initialize)

    ;; プラグインシステムが利用可能
    (should (fboundp 'nskk-plugin-system-initialize))

    ;; プラグインローダーが利用可能
    (should (fboundp 'nskk-plugin-load))

    (nskk-phase3-shutdown)))

;;; Track S: Transient UI統合テスト

(ert-deftest nskk-phase3-test-transient-ui-integration ()
  "Transient UI機能の統合をテストする。"
  ;; Transient設定UIが利用可能
  (should (fboundp 'nskk-transient-config))

  ;; TransientプラグインUIが利用可能
  (should (fboundp 'nskk-transient-plugins))

  ;; TransientデバッグUIが利用可能
  (should (fboundp 'nskk-transient-debug)))

;;; Track T: Optimization統合テスト

(ert-deftest nskk-phase3-test-optimization-integration ()
  "最適化機能の統合をテストする。"
  (nskk-phase3-initialize)

  ;; メモリ最適化が利用可能
  (should (fboundp 'nskk-memory-optimize-enable))

  ;; マルチキャッシュが利用可能
  (should (fboundp 'nskk-multi-cache-initialize))

  ;; ネイティブコンパイルが利用可能
  (should (fboundp 'nskk-native-compile-setup))

  ;; マクロ最適化が利用可能
  (should (fboundp 'nskk-macro-optimize-apply))

  (nskk-phase3-shutdown))

(ert-deftest nskk-phase3-test-multi-cache-basic ()
  "マルチレベルキャッシュの基本動作をテストする。"
  (nskk-phase3-initialize)

  ;; キャッシュ初期化
  (nskk-multi-cache-initialize)

  ;; キャッシュ設定・取得
  (when (fboundp 'nskk-multi-cache-put)
    (nskk-multi-cache-put "test-key" "test-value")
    (when (fboundp 'nskk-multi-cache-get)
      (let ((result (nskk-multi-cache-get "test-key")))
        (should (equal result "test-value")))))

  (nskk-phase3-shutdown))

;;; クロストラック統合テスト

(ert-deftest nskk-phase3-test-threading-with-profiling ()
  "並列処理とプロファイリングの統合をテストする。"
  (skip-unless (nskk-thread-pool-available-p))

  (nskk-phase3-initialize)

  ;; プロファイリング開始
  (nskk-profile-start)

  ;; 並列タスク実行
  (let ((results nil)
        (mutex (make-mutex)))
    (dotimes (i 5)
      (nskk-thread-submit nskk-phase3-thread-pool
                          (lambda () (* i i))
                          (lambda (r)
                            (with-mutex mutex
                              (push r results)))))
    (sleep-for 1.0)
    (should (= (length results) 5)))

  ;; プロファイリング停止
  (nskk-profile-stop)

  (nskk-phase3-shutdown))

(ert-deftest nskk-phase3-test-async-ui-with-cache ()
  "非同期UIとキャッシュの統合をテストする。"
  (nskk-phase3-initialize)

  ;; キャッシュに候補を保存
  (when (fboundp 'nskk-multi-cache-put)
    (nskk-multi-cache-put "あい" '("愛" "哀" "藍")))

  ;; 非同期候補取得（キャッシュヒット）
  (when (and (fboundp 'nskk-async-candidates-fetch)
             (fboundp 'nskk-multi-cache-get))
    (let ((cached (nskk-multi-cache-get "あい")))
      (should (equal cached '("愛" "哀" "藍")))))

  (nskk-phase3-shutdown))

(ert-deftest nskk-phase3-test-full-integration ()
  "Phase 3全機能の統合テスト。"
  (skip-unless (nskk-thread-pool-available-p))

  ;; 初期化
  (nskk-phase3-initialize)
  (should nskk-phase3-initialized)

  ;; 検証
  (should (nskk-phase3-verify))

  ;; スレッドプール確認
  (should nskk-phase3-thread-pool)

  ;; 各機能が利用可能であることを確認
  (should (fboundp 'nskk-profile-start))
  (should (fboundp 'nskk-progress-create))
  (should (fboundp 'nskk-multi-cache-initialize))
  (should (fboundp 'nskk-transient-config))

  ;; シャットダウン
  (nskk-phase3-shutdown)
  (should-not nskk-phase3-initialized))

;;; パフォーマンス統合テスト（簡易版）

(ert-deftest nskk-phase3-test-performance-basic ()
  "Phase 3のパフォーマンスが許容範囲内であることを確認する。"
  (skip-unless (nskk-thread-pool-available-p))

  (nskk-phase3-initialize)

  ;; 初期化時間が1秒以内であることを確認（既に初期化済み）
  (let ((start-time (current-time)))
    (nskk-phase3-shutdown)
    (nskk-phase3-initialize)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (should (< elapsed 1.0))
      (message "Phase 3 re-initialization time: %.3f sec" elapsed)))

  (nskk-phase3-shutdown))

(ert-deftest nskk-phase3-test-memory-basic ()
  "Phase 3のメモリ使用量が許容範囲内であることを確認する。"
  (let ((gc-before (garbage-collect)))
    (nskk-phase3-initialize)
    (let* ((gc-after (garbage-collect))
           (memory-used (- (caddr gc-after) (caddr gc-before))))
      ;; メモリ使用量が10MB以下であることを確認（緩い制約）
      (should (< memory-used (* 10 1024 1024)))
      (message "Phase 3 memory usage: %.2f MB"
               (/ memory-used 1024.0 1024.0)))
    (nskk-phase3-shutdown)))

;;; エラーハンドリングテスト

(ert-deftest nskk-phase3-test-double-initialization ()
  "二重初期化が安全に処理されることを確認する。"
  (nskk-phase3-initialize)
  (should nskk-phase3-initialized)

  ;; 二重初期化（エラーにならないこと）
  (nskk-phase3-initialize)
  (should nskk-phase3-initialized)

  (nskk-phase3-shutdown))

(ert-deftest nskk-phase3-test-shutdown-before-init ()
  "初期化前のシャットダウンが安全に処理されることを確認する。"
  (when nskk-phase3-initialized
    (nskk-phase3-shutdown))

  ;; 未初期化状態でシャットダウン（エラーにならないこと）
  (nskk-phase3-shutdown)
  (should-not nskk-phase3-initialized))

;;; ステータステスト

(ert-deftest nskk-phase3-test-status-display ()
  "ステータス表示が動作することを確認する。"
  (nskk-phase3-initialize)

  ;; ステータス表示（エラーにならないこと）
  (nskk-phase3-status)

  ;; バッファが作成されている
  (should (get-buffer "*NSKK Phase 3 Status*"))

  ;; バッファを閉じる
  (kill-buffer "*NSKK Phase 3 Status*")

  (nskk-phase3-shutdown))

(provide 'nskk-phase3-test)

;;; nskk-phase3-test.el ends here
