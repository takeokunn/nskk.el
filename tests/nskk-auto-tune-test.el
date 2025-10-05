;;; nskk-auto-tune-test.el --- Tests for nskk-auto-tune -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; This file is part of NSKK.

;;; Commentary:

;; このファイルはnskk-auto-tuneの包括的なテストを提供します。
;;
;; テスト項目:
;; 1. 自動チューニング有効化/無効化テスト
;; 2. パラメータ初期化テスト
;; 3. パフォーマンス測定テスト
;; 4. スコア計算テスト
;; 5. パラメータ調整テスト
;; 6. 学習データ更新テスト
;; 7. A/Bテスト機能テスト
;; 8. レポート生成テスト
;; 9. リセット機能テスト
;; 10. ベスト設定適用テスト

;;; Code:

(require 'ert)
(require 'nskk-auto-tune)

;;; Test 1: 自動チューニング有効化/無効化テスト

(ert-deftest nskk-auto-tune-test-enable-disable ()
  "自動チューニングの有効化と無効化が正常に動作することを確認。"
  ;; 有効化
  (nskk-auto-tune-enable)
  (should nskk-auto-tune-enabled)
  (should nskk-auto-tune--timer)

  ;; 無効化
  (nskk-auto-tune-disable)
  (should-not nskk-auto-tune-enabled)
  (should-not nskk-auto-tune--timer))

(ert-deftest nskk-auto-tune-test-double-enable ()
  "有効化の重複実行は安全に処理されることを確認。"
  (nskk-auto-tune-enable)
  ;; 重複有効化は安全に処理される（エラーにならない）
  (nskk-auto-tune-enable)
  (nskk-auto-tune-disable))

;;; Test 2: パラメータ初期化テスト

(ert-deftest nskk-auto-tune-test-initialize-params ()
  "パラメータ初期化が正常に動作することを確認。"
  (let ((params (nskk-auto-tune--initialize-params)))
    (should (plist-get params :cache-size))
    (should (plist-get params :gc-cons-threshold))
    (should (plist-get params :thread-pool-size))

    ;; 初期値が範囲内にあることを確認
    (let ((cache-size (plist-get params :cache-size)))
      (should (>= cache-size nskk-auto-tune-cache-size-min))
      (should (<= cache-size nskk-auto-tune-cache-size-max)))))

;;; Test 3: パフォーマンス測定テスト

(ert-deftest nskk-auto-tune-test-measure-performance ()
  "パフォーマンス測定が正常に動作することを確認。"
  (let ((metrics (nskk-auto-tune--measure-performance)))
    (should (plist-get metrics :search-time))
    (should (plist-get metrics :memory-usage))
    (should (plist-get metrics :gc-count))
    (should (plist-get metrics :cache-hit-rate))

    ;; 各メトリクスが妥当な値であることを確認
    (should (>= (plist-get metrics :search-time) 0))
    (should (>= (plist-get metrics :memory-usage) 0))
    (should (>= (plist-get metrics :gc-count) 0))
    (should (>= (plist-get metrics :cache-hit-rate) 0.0))
    (should (<= (plist-get metrics :cache-hit-rate) 1.0))))

;;; Test 4: スコア計算テスト

(ert-deftest nskk-auto-tune-test-calculate-score ()
  "スコア計算が正常に動作することを確認。"
  (let* ((metrics '(:search-time 0.001
                    :memory-usage 1000000
                    :gc-count 1
                    :cache-hit-rate 0.8))
         (score (nskk-auto-tune--calculate-score metrics)))
    (should (numberp score))
    (should (> score 0))))

(ert-deftest nskk-auto-tune-test-score-comparison ()
  "スコアが性能を正しく反映することを確認。"
  (let* ((good-metrics '(:search-time 0.001
                         :memory-usage 1000000
                         :gc-count 1
                         :cache-hit-rate 0.9))
         (bad-metrics '(:search-time 0.1
                        :memory-usage 10000000
                        :gc-count 10
                        :cache-hit-rate 0.5))
         (good-score (nskk-auto-tune--calculate-score good-metrics))
         (bad-score (nskk-auto-tune--calculate-score bad-metrics)))
    ;; 良い性能の方が高いスコアを得る
    (should (> good-score bad-score))))

;;; Test 5: パラメータ調整テスト

(ert-deftest nskk-auto-tune-test-adjust-parameters ()
  "パラメータ調整が正常に動作することを確認。"
  (let* ((metrics '(:search-time 0.01
                    :memory-usage 5000000
                    :gc-count 5
                    :cache-hit-rate 0.7))
         (score (nskk-auto-tune--calculate-score metrics))
         (new-params (nskk-auto-tune--adjust-parameters metrics score)))

    (should (plist-get new-params :cache-size))
    (should (plist-get new-params :gc-cons-threshold))

    ;; 調整後のパラメータが範囲内にあることを確認
    (let ((cache-size (plist-get new-params :cache-size)))
      (should (>= cache-size nskk-auto-tune-cache-size-min))
      (should (<= cache-size nskk-auto-tune-cache-size-max)))))

;;; Test 6: 学習データ更新テスト

(ert-deftest nskk-auto-tune-test-learning-data ()
  "学習データ更新が正常に動作することを確認。"
  (when nskk-auto-tune-learning-enabled
    (nskk-auto-tune-reset)

    (let* ((params '(:cache-size 1000 :gc-cons-threshold 10000000))
           (score 100.0))

      ;; 学習データを更新
      (nskk-auto-tune--update-learning-data params score)

      ;; 学習データが記録されている
      (should (> (length nskk-auto-tune--learning-data) 0))

      ;; データ構造が正しい
      (let ((entry (car nskk-auto-tune--learning-data)))
        (should (plist-get entry :params))
        (should (plist-get entry :score))))))

(ert-deftest nskk-auto-tune-test-apply-learning ()
  "学習ベースのパラメータ適用が正常に動作することを確認。"
  (when nskk-auto-tune-learning-enabled
    (nskk-auto-tune-reset)

    ;; 複数の学習データを追加
    (nskk-auto-tune--update-learning-data
     '(:cache-size 1000 :gc-cons-threshold 10000000) 80.0)
    (nskk-auto-tune--update-learning-data
     '(:cache-size 2000 :gc-cons-threshold 20000000) 90.0)
    (nskk-auto-tune--update-learning-data
     '(:cache-size 3000 :gc-cons-threshold 30000000) 100.0)

    ;; 学習を適用
    (let* ((current-params '(:cache-size 1500 :gc-cons-threshold 15000000))
           (current-score 85.0)
           (adjusted-params (nskk-auto-tune--apply-learning
                             current-params current-score)))

      (should (plist-get adjusted-params :cache-size))
      (should (plist-get adjusted-params :gc-cons-threshold)))))

;;; Test 7: A/Bテスト機能テスト

(ert-deftest nskk-auto-tune-test-ab-test-setup ()
  "A/Bテストのセットアップが正常に動作することを確認。"
  (let ((configs '((:name "config-a" :cache-size 1000)
                   (:name "config-b" :cache-size 2000))))
    (nskk-auto-tune-ab-test-setup configs)

    ;; A/Bテスト設定が保存されている
    (should (= (length nskk-auto-tune--ab-test-configs) 2))
    (should (= (length nskk-auto-tune--ab-test-results) 0))))

(ert-deftest nskk-auto-tune-test-ab-test-run ()
  "A/Bテストの実行が正常に動作することを確認。"
  (let ((configs '((:name "config-a" :cache-size 1000)
                   (:name "config-b" :cache-size 2000))))
    (nskk-auto-tune-ab-test-setup configs)

    ;; A/Bテスト実行（エラーが発生しないことを確認）
    (nskk-auto-tune-ab-test-run)

    ;; 結果が記録されている
    (should (> (length nskk-auto-tune--ab-test-results) 0))))

(ert-deftest nskk-auto-tune-test-ab-test-report ()
  "A/Bテストレポート生成が正常に動作することを確認。"
  (let ((configs '((:name "config-a" :cache-size 1000)
                   (:name "config-b" :cache-size 2000))))
    (nskk-auto-tune-ab-test-setup configs)
    (nskk-auto-tune-ab-test-run)

    ;; レポート生成（エラーが発生しないことを確認）
    (nskk-auto-tune-ab-test-report)))

;;; Test 8: レポート生成テスト

(ert-deftest nskk-auto-tune-test-report ()
  "レポート生成が正常に動作することを確認。"
  (nskk-auto-tune-reset)

  ;; いくつかデータを追加
  (nskk-auto-tune--update-learning-data
   '(:cache-size 1000 :gc-cons-threshold 10000000) 80.0)

  ;; レポート生成（エラーが発生しないことを確認）
  (nskk-auto-tune-report))

;;; Test 9: リセット機能テスト

(ert-deftest nskk-auto-tune-test-reset ()
  "リセット機能が正常に動作することを確認。"
  ;; データを追加
  (nskk-auto-tune--update-learning-data
   '(:cache-size 1000 :gc-cons-threshold 10000000) 80.0)

  ;; リセット
  (nskk-auto-tune-reset)

  ;; データがクリアされている
  (should (= (length nskk-auto-tune--learning-data) 0))
  (should (= (length nskk-auto-tune--ab-test-configs) 0))
  (should (= (length nskk-auto-tune--ab-test-results) 0))
  (should-not nskk-auto-tune--current-params)
  (should-not nskk-auto-tune--best-score))

;;; Test 10: ベスト設定適用テスト

(ert-deftest nskk-auto-tune-test-apply-best ()
  "ベスト設定の適用が正常に動作することを確認。"
  (when nskk-auto-tune-learning-enabled
    (nskk-auto-tune-reset)

    ;; 学習データを追加
    (nskk-auto-tune--update-learning-data
     '(:cache-size 1000 :gc-cons-threshold 10000000) 80.0)
    (nskk-auto-tune--update-learning-data
     '(:cache-size 2000 :gc-cons-threshold 20000000) 100.0)
    (nskk-auto-tune--update-learning-data
     '(:cache-size 3000 :gc-cons-threshold 30000000) 90.0)

    ;; ベスト設定を適用（エラーが発生しないことを確認）
    (nskk-auto-tune-apply-best)

    ;; ベストスコアが更新されている
    (should nskk-auto-tune--best-score)
    (should (= nskk-auto-tune--best-score 100.0))))

;;; Test 11: チューニング実行テスト

(ert-deftest nskk-auto-tune-test-run ()
  "チューニング実行が正常に動作することを確認。"
  (nskk-auto-tune-reset)

  ;; チューニング実行（エラーが発生しないことを確認）
  (nskk-auto-tune-run)

  ;; パラメータが設定されている
  (should nskk-auto-tune--current-params))

;;; Test 12: エラーハンドリングテスト

(ert-deftest nskk-auto-tune-test-invalid-metrics ()
  "不正なメトリクスでエラーが発生しないことを確認。"
  ;; エラーが発生しないことを確認
  (nskk-auto-tune--calculate-score nil)
  (nskk-auto-tune--calculate-score '(:invalid-key 123)))

(ert-deftest nskk-auto-tune-test-invalid-params ()
  "不正なパラメータでエラーが発生しないことを確認。"
  ;; エラーが発生しないことを確認
  (nskk-auto-tune--apply-parameters nil)
  (nskk-auto-tune--apply-parameters '(:invalid-param 123)))

;;; Test 13: 統合テスト

(ert-deftest nskk-auto-tune-test-full-cycle ()
  "自動チューニングの完全なサイクルが正常に動作することを確認。"
  (nskk-auto-tune-reset)

  ;; 有効化
  (nskk-auto-tune-enable)

  ;; チューニング実行
  (nskk-auto-tune-run)

  ;; パラメータが設定されている
  (should nskk-auto-tune--current-params)

  ;; 学習データが記録されている
  (when nskk-auto-tune-learning-enabled
    (should (> (length nskk-auto-tune--learning-data) 0)))

  ;; レポート生成（エラーが発生しないことを確認）
  (nskk-auto-tune-report)

  ;; 無効化
  (nskk-auto-tune-disable))

;;; Test 14: パフォーマンステスト

(ert-deftest nskk-auto-tune-test-overhead ()
  "自動チューニングのオーバーヘッドが許容範囲内であることを確認。"
  (nskk-auto-tune-reset)

  (let ((start-time (float-time)))
    ;; チューニング実行
    (nskk-auto-tune-run)

    (let ((elapsed-time (- (float-time) start-time)))
      ;; チューニング時間が妥当（< 1秒）
      (should (< elapsed-time 1.0)))))

(provide 'nskk-auto-tune-test)

;;; nskk-auto-tune-test.el ends here
