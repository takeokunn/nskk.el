;;; nskk-phase4-test.el --- Phase 4 integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; Phase 4統合テスト
;;
;; このファイルは、Phase 4の全トラック (U, V, W, X, Y, Z) の
;; 統合テストを提供します。
;;
;; テストカテゴリ:
;; 1. モジュールロードテスト
;; 2. 初期化・シャットダウンテスト
;; 3. AI統合テスト
;; 4. 同期システムテスト
;; 5. 分析システムテスト
;; 6. クロスモジュール統合テスト
;; 7. パフォーマンステスト

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Phase 4モジュールをロード
(require 'nskk-phase4)

;;; 1. モジュールロードテスト

(ert-deftest nskk-phase4-test-track-u-modules-loaded ()
  "Track U (AI統合) の全モジュールがロードされているかテスト。"
  (should (featurep 'nskk-ai-context))
  (should (featurep 'nskk-ai-pattern))
  (should (featurep 'nskk-ai-candidates))
  (should (featurep 'nskk-ai-learning)))

(ert-deftest nskk-phase4-test-track-v-modules-loaded ()
  "Track V (同期システム) の全モジュールがロードされているかテスト。"
  (should (featurep 'nskk-sync-protocol))
  (should (featurep 'nskk-sync-crypto))
  (should (featurep 'nskk-sync-diff))
  (should (featurep 'nskk-sync-conflict)))

(ert-deftest nskk-phase4-test-track-w-modules-loaded ()
  "Track W (分析) の全モジュールがロードされているかテスト。"
  (should (featurep 'nskk-analytics-pattern))
  (should (featurep 'nskk-analytics-optimize))
  (should (featurep 'nskk-analytics-report))
  (should (featurep 'nskk-analytics-dashboard)))

(ert-deftest nskk-phase4-test-all-modules-loaded ()
  "Phase 4の全モジュールがロードされているかテスト。"
  (should (featurep 'nskk-phase4))
  ;; Track U
  (should (featurep 'nskk-ai-context))
  (should (featurep 'nskk-ai-pattern))
  (should (featurep 'nskk-ai-candidates))
  (should (featurep 'nskk-ai-learning))
  ;; Track V
  (should (featurep 'nskk-sync-protocol))
  (should (featurep 'nskk-sync-crypto))
  (should (featurep 'nskk-sync-diff))
  (should (featurep 'nskk-sync-conflict))
  ;; Track W
  (should (featurep 'nskk-analytics-pattern))
  (should (featurep 'nskk-analytics-optimize))
  (should (featurep 'nskk-analytics-report))
  (should (featurep 'nskk-analytics-dashboard)))

;;; 2. 初期化・シャットダウンテスト

(ert-deftest nskk-phase4-test-initialize-shutdown ()
  "Phase 4の初期化とシャットダウンが正しく動作するかテスト。"
  ;; 初期状態確認
  (should-not nskk-phase4--initialized)

  ;; 初期化
  (let ((nskk-phase4-enable-ai t)
        (nskk-phase4-enable-sync nil)  ; サーバー不要のため無効
        (nskk-phase4-enable-analytics t))
    (nskk-phase4-initialize)
    (should nskk-phase4--initialized)
    (should nskk-phase4--ai-enabled)
    (should-not nskk-phase4--sync-enabled)
    (should nskk-phase4--analytics-enabled)

    ;; シャットダウン
    (nskk-phase4-shutdown)
    (should-not nskk-phase4--initialized)
    (should-not nskk-phase4--ai-enabled)
    (should-not nskk-phase4--analytics-enabled)))

(ert-deftest nskk-phase4-test-initialize-ai-only ()
  "AI機能のみを有効化した初期化テスト。"
  (let ((nskk-phase4-enable-ai t)
        (nskk-phase4-enable-sync nil)
        (nskk-phase4-enable-analytics nil))
    (nskk-phase4-initialize)
    (should nskk-phase4--initialized)
    (should nskk-phase4--ai-enabled)
    (should-not nskk-phase4--sync-enabled)
    (should-not nskk-phase4--analytics-enabled)
    (nskk-phase4-shutdown)))

(ert-deftest nskk-phase4-test-initialize-analytics-only ()
  "分析機能のみを有効化した初期化テスト。"
  (let ((nskk-phase4-enable-ai nil)
        (nskk-phase4-enable-sync nil)
        (nskk-phase4-enable-analytics t))
    (nskk-phase4-initialize)
    (should nskk-phase4--initialized)
    (should-not nskk-phase4--ai-enabled)
    (should-not nskk-phase4--sync-enabled)
    (should nskk-phase4--analytics-enabled)
    (nskk-phase4-shutdown)))

(ert-deftest nskk-phase4-test-double-initialize ()
  "二重初期化が安全に処理されるかテスト。"
  (let ((nskk-phase4-enable-ai t)
        (nskk-phase4-enable-sync nil)
        (nskk-phase4-enable-analytics t))
    (nskk-phase4-initialize)
    (should nskk-phase4--initialized)
    ;; 二重初期化を試みる (エラーにならないはず)
    (nskk-phase4-initialize)
    (should nskk-phase4--initialized)
    (nskk-phase4-shutdown)))

;;; 3. AI統合テスト

(ert-deftest nskk-phase4-test-ai-functions-available ()
  "AI関連の関数が利用可能かテスト。"
  (should (fboundp 'nskk-ai-context-analyze))
  (should (fboundp 'nskk-ai-pattern-detect))
  (should (fboundp 'nskk-ai-candidates-rank))
  (should (fboundp 'nskk-ai-learning-update)))

(ert-deftest nskk-phase4-test-ai-context-basic ()
  "AI文脈理解の基本動作テスト。"
  (let ((nskk-phase4-enable-ai t)
        (nskk-phase4-enable-sync nil)
        (nskk-phase4-enable-analytics nil))
    (nskk-phase4-initialize)
    ;; 文脈分析が動作するか
    (when (fboundp 'nskk-ai-context-analyze)
      (let ((context (nskk-ai-context-analyze "テスト文脈")))
        (should context)))
    (nskk-phase4-shutdown)))

(ert-deftest nskk-phase4-test-ai-pattern-basic ()
  "AIパターン認識の基本動作テスト。"
  (let ((nskk-phase4-enable-ai t)
        (nskk-phase4-enable-sync nil)
        (nskk-phase4-enable-analytics nil))
    (nskk-phase4-initialize)
    ;; パターン検出が動作するか
    (when (fboundp 'nskk-ai-pattern-detect)
      (let ((pattern (nskk-ai-pattern-detect '("test1" "test2" "test3"))))
        (should (listp pattern))))
    (nskk-phase4-shutdown)))

(ert-deftest nskk-phase4-test-ai-candidates-basic ()
  "AI候補ランキングの基本動作テスト。"
  (let ((nskk-phase4-enable-ai t)
        (nskk-phase4-enable-sync nil)
        (nskk-phase4-enable-analytics nil))
    (nskk-phase4-initialize)
    ;; 候補ランキングが動作するか
    (when (fboundp 'nskk-ai-candidates-rank)
      (let ((ranked (nskk-ai-candidates-rank "かんじ" '("漢字" "感じ" "幹事"))))
        (should (listp ranked))
        (should (= (length ranked) 3))))
    (nskk-phase4-shutdown)))

;;; 4. 同期システムテスト

(ert-deftest nskk-phase4-test-sync-functions-available ()
  "同期関連の関数が利用可能かテスト。"
  (should (fboundp 'nskk-sync-protocol-connect))
  (should (fboundp 'nskk-sync-crypto-encrypt))
  (should (fboundp 'nskk-sync-diff-compute))
  (should (fboundp 'nskk-sync-conflict-resolve)))

(ert-deftest nskk-phase4-test-sync-crypto-basic ()
  "暗号化の基本動作テスト。"
  (when (fboundp 'nskk-sync-crypto-encrypt)
    (let* ((plaintext "test data")
           (key "test-key-1234567890123456")
           (encrypted (nskk-sync-crypto-encrypt plaintext key)))
      (should encrypted)
      (should (stringp encrypted))
      (should-not (string= plaintext encrypted)))))

(ert-deftest nskk-phase4-test-sync-diff-basic ()
  "差分計算の基本動作テスト。"
  (when (fboundp 'nskk-sync-diff-compute)
    (let* ((old-data '(("key1" . "value1") ("key2" . "value2")))
           (new-data '(("key1" . "value1-new") ("key3" . "value3")))
           (diff (nskk-sync-diff-compute old-data new-data)))
      (should diff)
      (should (listp diff)))))

;;; 5. 分析システムテスト

(ert-deftest nskk-phase4-test-analytics-functions-available ()
  "分析関連の関数が利用可能かテスト。"
  (should (fboundp 'nskk-analytics-pattern-record))
  (should (fboundp 'nskk-analytics-optimize-tune))
  (should (fboundp 'nskk-analytics-report-generate))
  (should (fboundp 'nskk-analytics-dashboard-show)))

(ert-deftest nskk-phase4-test-analytics-pattern-basic ()
  "パターン記録の基本動作テスト。"
  (let ((nskk-phase4-enable-ai nil)
        (nskk-phase4-enable-sync nil)
        (nskk-phase4-enable-analytics t))
    (nskk-phase4-initialize)
    ;; パターン記録が動作するか
    (when (fboundp 'nskk-analytics-pattern-record)
      (should-not (nskk-analytics-pattern-record "test-event" '((key . "value")))))
    (nskk-phase4-shutdown)))

(ert-deftest nskk-phase4-test-analytics-optimize-basic ()
  "最適化の基本動作テスト。"
  (let ((nskk-phase4-enable-ai nil)
        (nskk-phase4-enable-sync nil)
        (nskk-phase4-enable-analytics t))
    (nskk-phase4-initialize)
    ;; 最適化が動作するか
    (when (fboundp 'nskk-analytics-optimize-tune)
      (let ((result (nskk-analytics-optimize-tune)))
        (should (or (null result) (listp result)))))
    (nskk-phase4-shutdown)))

;;; 6. クロスモジュール統合テスト

(ert-deftest nskk-phase4-test-ai-analytics-integration ()
  "AI機能と分析機能の統合テスト。"
  (let ((nskk-phase4-enable-ai t)
        (nskk-phase4-enable-sync nil)
        (nskk-phase4-enable-analytics t))
    (nskk-phase4-initialize)
    (should nskk-phase4--ai-enabled)
    (should nskk-phase4--analytics-enabled)

    ;; AI候補ランキング + 分析記録
    (when (and (fboundp 'nskk-ai-candidates-rank)
               (fboundp 'nskk-analytics-pattern-record))
      (let ((candidates (nskk-ai-candidates-rank "てすと" '("テスト" "test"))))
        (should candidates)
        ;; 分析に記録
        (nskk-analytics-pattern-record "ai-ranking"
                                       `((input . "てすと")
                                         (candidates . ,candidates)))))
    (nskk-phase4-shutdown)))

(ert-deftest nskk-phase4-test-all-features-integration ()
  "全機能を有効化した統合テスト。"
  (let ((nskk-phase4-enable-ai t)
        (nskk-phase4-enable-sync nil)  ; サーバー不要のため無効
        (nskk-phase4-enable-analytics t))
    (nskk-phase4-initialize)
    (should nskk-phase4--initialized)
    (should nskk-phase4--ai-enabled)
    (should nskk-phase4--analytics-enabled)

    ;; 統合ワークフロー
    ;; 1. 文脈分析
    (when (fboundp 'nskk-ai-context-analyze)
      (nskk-ai-context-analyze "統合テスト"))

    ;; 2. 候補ランキング
    (when (fboundp 'nskk-ai-candidates-rank)
      (nskk-ai-candidates-rank "とうごう" '("統合" "東郷")))

    ;; 3. パターン記録
    (when (fboundp 'nskk-analytics-pattern-record)
      (nskk-analytics-pattern-record "integration-test" '((status . "ok"))))

    (nskk-phase4-shutdown)))

;;; 7. パフォーマンステスト

(ert-deftest nskk-phase4-test-ai-performance ()
  "AI機能のパフォーマンステスト。"
  (let ((nskk-phase4-enable-ai t)
        (nskk-phase4-enable-sync nil)
        (nskk-phase4-enable-analytics nil))
    (nskk-phase4-initialize)

    ;; 候補ランキングが5ms以内に完了するか
    (when (fboundp 'nskk-ai-candidates-rank)
      (let* ((start-time (current-time))
             (candidates '("漢字" "感じ" "幹事" "寛治" "感次"))
             (result (nskk-ai-candidates-rank "かんじ" candidates))
             (elapsed (float-time (time-subtract (current-time) start-time))))
        (should result)
        ;; 5ms以内 (0.005秒)
        (should (< elapsed 0.005))))

    (nskk-phase4-shutdown)))

(ert-deftest nskk-phase4-test-analytics-overhead ()
  "分析機能のオーバーヘッドテスト。"
  (let ((nskk-phase4-enable-ai nil)
        (nskk-phase4-enable-sync nil)
        (nskk-phase4-enable-analytics t))
    (nskk-phase4-initialize)

    ;; パターン記録が1ms以内に完了するか
    (when (fboundp 'nskk-analytics-pattern-record)
      (let* ((start-time (current-time))
             (result (nskk-analytics-pattern-record "perf-test" '((data . "test"))))
             (elapsed (float-time (time-subtract (current-time) start-time))))
        (should-not result)  ; 戻り値はnil
        ;; 1ms以内 (0.001秒)
        (should (< elapsed 0.001))))

    (nskk-phase4-shutdown)))

;;; 8. ヘルスチェックテスト

(ert-deftest nskk-phase4-test-health-check ()
  "Phase 4ヘルスチェックが正しく動作するかテスト。"
  (should (nskk-phase4-health-check)))

(ert-deftest nskk-phase4-test-integration-test-ready ()
  "統合テストの準備状態チェックテスト。"
  (should (nskk-phase4-integration-test-ready-p)))

;;; 9. カスタマイズ変数テスト

(ert-deftest nskk-phase4-test-customization-variables ()
  "カスタマイズ変数が正しく定義されているかテスト。"
  (should (boundp 'nskk-phase4-enable-ai))
  (should (boundp 'nskk-phase4-enable-sync))
  (should (boundp 'nskk-phase4-enable-analytics))
  (should (boundp 'nskk-phase4-ai-context-window-size))
  (should (boundp 'nskk-phase4-sync-server-url))
  (should (boundp 'nskk-phase4-analytics-report-interval)))

(ert-deftest nskk-phase4-test-customization-defaults ()
  "カスタマイズ変数のデフォルト値テスト。"
  (should (eq nskk-phase4-enable-ai t))
  (should (eq nskk-phase4-enable-sync nil))
  (should (eq nskk-phase4-enable-analytics t))
  (should (integerp nskk-phase4-ai-context-window-size))
  (should (> nskk-phase4-ai-context-window-size 0)))

;;; 10. エラーハンドリングテスト

(ert-deftest nskk-phase4-test-graceful-ai-failure ()
  "AI初期化失敗時の安全な処理テスト。"
  ;; AI初期化関数を一時的に壊す
  (let ((original-func (symbol-function 'nskk-ai-context-initialize)))
    (unwind-protect
        (progn
          (fset 'nskk-ai-context-initialize
                (lambda () (error "Simulated AI init failure")))
          (let ((nskk-phase4-enable-ai t)
                (nskk-phase4-enable-sync nil)
                (nskk-phase4-enable-analytics nil))
            ;; 初期化してもエラーにならないはず
            (should-not (nskk-phase4-initialize))
            ;; AI機能は無効化されているはず
            (should-not nskk-phase4--ai-enabled)
            (nskk-phase4-shutdown)))
      ;; 復元
      (fset 'nskk-ai-context-initialize original-func))))

;;; ユーティリティ関数テスト

(ert-deftest nskk-phase4-test-status-display ()
  "ステータス表示関数のテスト。"
  (should (fboundp 'nskk-phase4-status))
  ;; エラーなく実行できるか
  (should-not (nskk-phase4-status)))

(ert-deftest nskk-phase4-test-quick-access-functions ()
  "クイックアクセス関数のテスト。"
  (should (fboundp 'nskk-phase4-show-dashboard))
  (should (fboundp 'nskk-phase4-generate-report))
  (should (fboundp 'nskk-phase4-sync-now)))

(provide 'nskk-phase4-test)

;;; nskk-phase4-test.el ends here
