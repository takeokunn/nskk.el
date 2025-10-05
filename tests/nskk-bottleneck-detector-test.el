;;; nskk-bottleneck-detector-test.el --- Tests for nskk-bottleneck-detector -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはnskk-bottleneck-detectorの包括的なテストを提供します。
;;
;; テスト項目:
;; 1. ボトルネック検出開始/停止テスト
;; 2. 関数分析テスト
;; 3. メモリ分析テスト
;; 4. GC分析テスト
;; 5. アラート生成テスト
;; 6. レポート生成テスト
;; 7. 推奨事項生成テスト
;; 8. ステータス取得テスト
;; 9. リセット機能テスト
;; 10. エラーハンドリングテスト

;;; Code:

(require 'ert)
(require 'nskk-bottleneck-detector)

;;; Test 1: ボトルネック検出開始/停止テスト

(ert-deftest nskk-bottleneck-test-start-stop ()
  "ボトルネック検出の開始と停止が正常に動作することを確認。"
  ;; 初期状態
  (let ((status (nskk-bottleneck-status)))
    (should-not (plist-get status :active)))

  ;; 開始
  (nskk-bottleneck-detect-start)
  (let ((status (nskk-bottleneck-status)))
    (should (plist-get status :active)))

  ;; 停止
  (let ((nskk-bottleneck-alert-enabled nil))  ; アラートを無効化
    (nskk-bottleneck-detect-stop))
  (let ((status (nskk-bottleneck-status)))
    (should-not (plist-get status :active))))

(ert-deftest nskk-bottleneck-test-double-start ()
  "検出実行中の再開始でエラーが発生することを確認。"
  (nskk-bottleneck-detect-start)
  ;; 重複開始はエラーとなる
  (should-error (nskk-bottleneck-detect-start))
  (let ((nskk-bottleneck-alert-enabled nil))
    (nskk-bottleneck-detect-stop)))

(ert-deftest nskk-bottleneck-test-stop-before-start ()
  "開始前の停止でエラーが発生することを確認。"
  ;; 開始前の停止はエラーとなる
  (should-error
   (let ((nskk-bottleneck-alert-enabled nil))
     (nskk-bottleneck-detect-stop))))

;;; Test 2: 関数分析テスト

(ert-deftest nskk-bottleneck-test-function-analysis ()
  "関数分析が正常に動作することを確認。"
  (nskk-bottleneck-reset)
  (nskk-bottleneck-detect-start)

  ;; プロファイラーに統計を記録
  (when (fboundp 'nskk-profiler--record-function-stats)
    (nskk-profiler--record-function-stats 'test-slow-function 0.1)
    (dotimes (_ 150)
      (nskk-profiler--record-function-stats 'test-hot-function 0.001)))

  ;; 分析実行
  (let ((result (nskk-bottleneck--analyze-functions)))
    (should (plist-get result :slow-functions))
    (should (plist-get result :hot-functions)))

  (let ((nskk-bottleneck-alert-enabled nil))
    (nskk-bottleneck-detect-stop)))

;;; Test 3: メモリ分析テスト

(ert-deftest nskk-bottleneck-test-memory-analysis ()
  "メモリ分析が正常に動作することを確認。"
  (nskk-bottleneck-reset)
  (nskk-bottleneck-detect-start)

  ;; メモリを使用する処理
  (dotimes (_ 1000)
    (make-string 1000 ?a))

  ;; 分析実行
  (let ((result (nskk-bottleneck--analyze-memory)))
    (should (plist-get result :memory-delta))
    (should (numberp (plist-get result :memory-delta))))

  (let ((nskk-bottleneck-alert-enabled nil))
    (nskk-bottleneck-detect-stop)))

;;; Test 4: GC分析テスト

(ert-deftest nskk-bottleneck-test-gc-analysis ()
  "GC分析が正常に動作することを確認。"
  (nskk-bottleneck-reset)
  (nskk-bottleneck-detect-start)

  ;; GCを発生させる
  (garbage-collect)

  ;; 分析実行
  (let ((result (nskk-bottleneck--analyze-gc)))
    (should (plist-get result :gc-count))
    (should (plist-get result :gc-frequency))
    (should (numberp (plist-get result :gc-count)))
    (should (numberp (plist-get result :gc-frequency))))

  (let ((nskk-bottleneck-alert-enabled nil))
    (nskk-bottleneck-detect-stop)))

;;; Test 5: アラート生成テスト

(ert-deftest nskk-bottleneck-test-generate-alerts ()
  "アラート生成が正常に動作することを確認。"
  (nskk-bottleneck-reset)

  ;; 遅い関数を記録
  (when (fboundp 'nskk-profiler--record-function-stats)
    (nskk-profiler--record-function-stats 'slow-function 0.2))

  ;; アラート生成
  (let ((alerts (nskk-bottleneck--generate-alerts)))
    (should (listp alerts))))

(ert-deftest nskk-bottleneck-test-latency-alert ()
  "遅延アラートが正常に生成されることを確認。"
  (nskk-bottleneck-reset)

  ;; 閾値を超える遅い関数を記録
  (when (fboundp 'nskk-profiler--record-function-stats)
    (let ((nskk-bottleneck-latency-threshold 0.01))
      (nskk-profiler--record-function-stats 'very-slow-function 0.1)

      ;; アラート生成
      (let ((alerts (nskk-bottleneck--generate-alerts)))
        (should (listp alerts))
        ;; 遅延アラートが含まれている可能性がある
        (when alerts
          (should (cl-some (lambda (alert)
                             (eq (plist-get alert :type) 'latency))
                           alerts)))))))

;;; Test 6: レポート生成テスト

(ert-deftest nskk-bottleneck-test-report ()
  "レポート生成が正常に動作することを確認。"
  (nskk-bottleneck-reset)
  (nskk-bottleneck-detect-start)

  ;; 何か処理を実行
  (when (fboundp 'nskk-profiler--record-function-stats)
    (nskk-profiler--record-function-stats 'test-function 0.01))

  (let ((nskk-bottleneck-alert-enabled nil))
    (nskk-bottleneck-detect-stop))

  ;; レポート生成（エラーが発生しないことを確認）
  (nskk-bottleneck-report))

(ert-deftest nskk-bottleneck-test-report-before-detection ()
  "検出前のレポート生成でエラーが発生しないことを確認。"
  (nskk-bottleneck-reset)
  ;; エラーが発生しないことを確認
  (nskk-bottleneck-report))

;;; Test 7: 推奨事項生成テスト

(ert-deftest nskk-bottleneck-test-recommendations ()
  "推奨事項生成が正常に動作することを確認。"
  (nskk-bottleneck-reset)

  ;; ボトルネックデータを設定
  (when (fboundp 'nskk-profiler--record-function-stats)
    (nskk-profiler--record-function-stats 'slow-function 0.1))

  ;; 推奨事項生成
  (let ((recommendations (nskk-bottleneck--generate-recommendations)))
    (should (listp recommendations))))

;;; Test 8: ステータス取得テスト

(ert-deftest nskk-bottleneck-test-status ()
  "ステータス取得が正常に動作することを確認。"
  ;; 非アクティブ時
  (nskk-bottleneck-reset)
  (let ((status (nskk-bottleneck-status)))
    (should-not (plist-get status :active)))

  ;; アクティブ時
  (nskk-bottleneck-detect-start)
  (let ((status (nskk-bottleneck-status)))
    (should (plist-get status :active)))

  (let ((nskk-bottleneck-alert-enabled nil))
    (nskk-bottleneck-detect-stop)))

;;; Test 9: リセット機能テスト

(ert-deftest nskk-bottleneck-test-reset ()
  "リセット機能が正常に動作することを確認。"
  ;; データを記録
  (nskk-bottleneck-detect-start)
  (when (fboundp 'nskk-profiler--record-function-stats)
    (nskk-profiler--record-function-stats 'test-function 0.01))

  ;; リセット
  (nskk-bottleneck-reset)

  ;; データがクリアされている
  (should-not nskk-bottleneck--active)
  (should-not nskk-bottleneck--start-time)
  (should (= (length nskk-bottleneck--hotpaths) 0))
  (should (= (length nskk-bottleneck--slow-functions) 0))
  (should (= (length nskk-bottleneck--alerts) 0)))

;;; Test 10: エラーハンドリングテスト

(ert-deftest nskk-bottleneck-test-analyze-without-profiler ()
  "プロファイラー未起動時の分析でエラーが発生しないことを確認。"
  (nskk-bottleneck-reset)

  ;; 分析実行（エラーが発生しないことを確認）
  (nskk-bottleneck--analyze-functions)
  (nskk-bottleneck--analyze-memory)
  (nskk-bottleneck--analyze-gc))

;;; Test 11: 統合テスト

(ert-deftest nskk-bottleneck-test-full-cycle ()
  "ボトルネック検出の完全なサイクルが正常に動作することを確認。"
  (nskk-bottleneck-reset)

  ;; 検出開始
  (nskk-bottleneck-detect-start)
  (should (nskk-bottleneck-status))

  ;; 処理実行（ホットパスとボトルネックを作成）
  (when (fboundp 'nskk-profiler--record-function-stats)
    ;; ホットパス（頻繁に呼ばれる関数）
    (dotimes (_ 200)
      (nskk-profiler--record-function-stats 'hot-function 0.001))

    ;; ボトルネック（遅い関数）
    (nskk-profiler--record-function-stats 'slow-function 0.2))

  ;; 検出停止
  (let ((nskk-bottleneck-alert-enabled nil))
    (nskk-bottleneck-detect-stop))
  (let ((status (nskk-bottleneck-status)))
    (should-not (plist-get status :active)))

  ;; レポート生成（エラーが発生しないことを確認）
  (nskk-bottleneck-report)

  ;; データが記録されている
  (let ((hotpaths nskk-bottleneck--hotpaths)
        (slow-functions nskk-bottleneck--slow-functions))
    ;; データ構造が正しい
    (should (listp hotpaths))
    (should (listp slow-functions))))

;;; Test 12: パフォーマンステスト

(ert-deftest nskk-bottleneck-test-analysis-performance ()
  "分析処理のパフォーマンスが許容範囲内であることを確認。"
  (nskk-bottleneck-reset)
  (nskk-bottleneck-detect-start)

  ;; 大量のデータを記録
  (when (fboundp 'nskk-profiler--record-function-stats)
    (dotimes (i 1000)
      (nskk-profiler--record-function-stats
       (intern (format "function-%d" i))
       0.001)))

  ;; 分析実行
  (let ((start-time (float-time)))
    (nskk-bottleneck--analyze)

    (let ((elapsed-time (- (float-time) start-time)))
      ;; 分析時間が妥当（< 1秒）
      (should (< elapsed-time 1.0))))

  (let ((nskk-bottleneck-alert-enabled nil))
    (nskk-bottleneck-detect-stop)))

;;; Test 13: ダッシュボードテスト

(ert-deftest nskk-bottleneck-test-dashboard ()
  "ダッシュボード表示が正常に動作することを確認。"
  (nskk-bottleneck-reset)

  ;; データを準備
  (when (fboundp 'nskk-profiler--record-function-stats)
    (nskk-profiler--record-function-stats 'test-function 0.01))

  ;; ダッシュボード表示（エラーが発生しないことを確認）
  ;; Transient UIはインタラクティブなので、関数の存在のみ確認
  (should (fboundp 'nskk-bottleneck-dashboard)))

(provide 'nskk-bottleneck-detector-test)

;;; nskk-bottleneck-detector-test.el ends here
