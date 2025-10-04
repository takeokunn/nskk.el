;;; test-profiler.el --- Tests for nskk-profiler -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;;; Commentary:
;; Track Pプロファイリング機能のテスト

;;; Code:

(require 'ert)
(require 'nskk-profiler)
(require 'nskk-bottleneck-detector)
(require 'nskk-auto-tune)

;;; nskk-profiler.el のテスト

(ert-deftest test-nskk-profiler-start-stop ()
  "プロファイラーの開始と停止をテスト。"
  (nskk-profiler-reset)

  ;; 開始
  (nskk-profile-start)
  (should (eq nskk-profiler--active t))
  (should nskk-profiler--start-time)

  ;; 停止
  (let ((nskk-profiler-auto-report nil))
    (nskk-profile-stop))
  (should (eq nskk-profiler--active nil)))

(ert-deftest test-nskk-profiler-memory-snapshot ()
  "メモリスナップショットのテスト。"
  (nskk-profiler-reset)
  (nskk-profile-start)

  ;; スナップショットが取得されているか
  (should (>= (length nskk-profiler--memory-snapshots) 1))

  (let ((nskk-profiler-auto-report nil))
    (nskk-profile-stop))

  ;; 停止後も最終スナップショットが追加されているか
  (should (>= (length nskk-profiler--memory-snapshots) 2)))

(ert-deftest test-nskk-profiler-function-tracking ()
  "関数プロファイリングのテスト。"
  (nskk-profiler-reset)
  (nskk-profile-start)

  ;; テスト関数実行
  (dotimes (i 10)
    (nskk-profile-function "test-func"
      (sleep-for 0.01)))

  (let ((nskk-profiler-auto-report nil))
    (nskk-profile-stop))

  ;; 統計が記録されているか
  (let ((entry (assoc "test-func" nskk-profiler--function-stats)))
    (should entry)
    (should (= (nth 1 entry) 10)))) ; 10回呼ばれた

(ert-deftest test-nskk-profiler-status ()
  "プロファイラーステータスのテスト。"
  (nskk-profiler-reset)
  (let ((status (nskk-profiler-status)))
    (should (eq (plist-get status :active) nil))
    (should (= (plist-get status :memory-snapshots) 0)))

  (nskk-profile-start)
  (let ((status (nskk-profiler-status)))
    (should (eq (plist-get status :active) t))
    (should (> (plist-get status :memory-snapshots) 0)))

  (let ((nskk-profiler-auto-report nil))
    (nskk-profile-stop)))

(ert-deftest test-nskk-profiler-thread-info ()
  "スレッド情報取得のテスト。"
  (let ((info (nskk-profiler-thread-info)))
    (should (plist-get info :main-thread))
    (should (>= (plist-get info :thread-count) 1))
    (should (plist-get info :current-thread))))

;;; nskk-bottleneck-detector.el のテスト

(ert-deftest test-nskk-bottleneck-detector-start-stop ()
  "ボトルネック検出の開始と停止をテスト。"
  (nskk-bottleneck-reset)

  ;; 開始
  (nskk-bottleneck-detect-start)
  (should (eq nskk-bottleneck--active t))

  ;; 停止
  (nskk-bottleneck-detect-stop)
  (should (eq nskk-bottleneck--active nil)))

(ert-deftest test-nskk-bottleneck-detector-analysis ()
  "ボトルネック分析のテスト。"
  (nskk-bottleneck-reset)
  (nskk-bottleneck-detect-start)

  ;; 遅い処理をシミュレート
  (dotimes (i 200)
    (nskk-profile-function "slow-function"
      (sleep-for 0.001)))

  (nskk-bottleneck-detect-stop)

  ;; ホットパスが検出されているか
  (should (> (length nskk-bottleneck--hotpaths) 0))

  ;; ステータス確認
  (let ((status (nskk-bottleneck-status)))
    (should (> (plist-get status :total-issues) 0))))

(ert-deftest test-nskk-bottleneck-detector-thresholds ()
  "閾値設定のテスト。"
  (let ((original-latency nskk-bottleneck-latency-threshold)
        (original-call-count nskk-bottleneck-call-count-threshold))

    ;; 閾値変更
    (setq nskk-bottleneck-latency-threshold 0.1)
    (setq nskk-bottleneck-call-count-threshold 50)

    (should (= nskk-bottleneck-latency-threshold 0.1))
    (should (= nskk-bottleneck-call-count-threshold 50))

    ;; 元に戻す
    (setq nskk-bottleneck-latency-threshold original-latency)
    (setq nskk-bottleneck-call-count-threshold original-call-count)))

;;; nskk-auto-tune.el のテスト

(ert-deftest test-nskk-auto-tune-enable-disable ()
  "自動チューニングの有効化と無効化をテスト。"
  (nskk-auto-tune-reset)

  ;; 有効化
  (nskk-auto-tune-enable)
  (should (eq nskk-auto-tune-enabled t))
  (should nskk-auto-tune--timer)

  ;; 無効化
  (nskk-auto-tune-disable)
  (should (eq nskk-auto-tune-enabled nil))
  (should (null nskk-auto-tune--timer)))

(ert-deftest test-nskk-auto-tune-params ()
  "パラメータ初期化のテスト。"
  (nskk-auto-tune-reset)
  (nskk-auto-tune-enable)

  ;; パラメータが設定されているか
  (should nskk-auto-tune--current-params)
  (should (plist-get nskk-auto-tune--current-params :cache-size))
  (should (plist-get nskk-auto-tune--current-params :gc-cons-threshold))

  (nskk-auto-tune-disable))

(ert-deftest test-nskk-auto-tune-measure-performance ()
  "パフォーマンス測定のテスト。"
  (let ((metrics (nskk-auto-tune--measure-performance)))
    (should metrics)
    (should (plist-get metrics :gc-frequency))
    (should (plist-get metrics :memory-usage))))

(ert-deftest test-nskk-auto-tune-score-calculation ()
  "スコア計算のテスト。"
  (let* ((metrics (list :avg-conversion-time 0.001
                       :gc-frequency 2.0
                       :memory-usage 1000000
                       :cache-hit-rate 0.8))
         (score (nskk-auto-tune--calculate-score metrics)))
    (should (numberp score))
    ;; スコアは正の値のはず
    (should (> score 0))))

(ert-deftest test-nskk-auto-tune-learning-data ()
  "学習データ更新のテスト。"
  (nskk-auto-tune-reset)

  (let ((params (list :cache-size 1000 :gc-cons-threshold 800000))
        (score 5.0))
    (nskk-auto-tune--update-learning-data params score)

    (should (= (length nskk-auto-tune--learning-data) 1))
    (let ((entry (car nskk-auto-tune--learning-data)))
      (should (= (cdr entry) 5.0)))))

(ert-deftest test-nskk-auto-tune-ab-test ()
  "A/Bテストのテスト。"
  (nskk-auto-tune-reset)

  ;; テスト設定
  (let ((configs (list
                  (list :name "Config A" :cache-size 500 :gc-cons-threshold 800000)
                  (list :name "Config B" :cache-size 1000 :gc-cons-threshold 1600000))))
    (nskk-auto-tune-ab-test-setup configs)

    (should (= (length nskk-auto-tune--ab-test-configs) 2))

    ;; A/Bテスト実行
    (nskk-auto-tune-ab-test-run)

    ;; 結果が記録されているか
    (should (= (length nskk-auto-tune--ab-test-results) 2))

    ;; 結果がスコア順にソートされているか
    (let ((first-score (plist-get (car nskk-auto-tune--ab-test-results) :score))
          (second-score (plist-get (cadr nskk-auto-tune--ab-test-results) :score)))
      (should (<= first-score second-score)))))

;;; 統合テスト

(ert-deftest test-profiler-integration ()
  "プロファイラー統合テスト。"
  (nskk-profiler-reset)
  (nskk-bottleneck-reset)

  ;; プロファイラーとボトルネック検出を同時実行
  (nskk-profile-start)
  (nskk-bottleneck-detect-start)

  ;; テスト処理
  (dotimes (i 100)
    (nskk-profile-function "integration-test"
      (+ i i)))

  (nskk-bottleneck-detect-stop)
  (let ((nskk-profiler-auto-report nil))
    (nskk-profile-stop))

  ;; 両方のデータが収集されているか
  (should (> (length nskk-profiler--function-stats) 0))
  (let ((status (nskk-bottleneck-status)))
    (should (>= (plist-get status :total-issues) 0))))

(ert-deftest test-auto-tune-integration ()
  "自動チューニング統合テスト。"
  (nskk-auto-tune-reset)

  ;; 自動チューニング有効化
  (nskk-auto-tune-enable)

  ;; 手動でチューニング実行
  (nskk-auto-tune-run)

  ;; 履歴が記録されているか
  (should (> (length nskk-auto-tune--history) 0))

  ;; ベストパラメータが設定されているか
  (should nskk-auto-tune--best-params)
  (should nskk-auto-tune--best-score)

  (nskk-auto-tune-disable))

(provide 'test-profiler)

;;; test-profiler.el ends here
