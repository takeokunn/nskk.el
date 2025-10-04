;;; nskk-auto-tune.el --- Auto-tuning for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, performance
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.0"))

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; このファイルはNSKKの自動チューニング機能を提供します。
;;
;; 主な機能:
;; 1. 動的パラメータ調整（キャッシュサイズ、GC閾値など）
;; 2. 適応的最適化（使用パターンに基づく調整）
;; 3. 学習ベースチューニング（履歴データから最適値を推定）
;; 4. A/Bテスト（複数設定の比較評価）
;;
;; 使用例:
;; (require 'nskk-auto-tune)
;;
;; ;; 自動チューニング有効化
;; (nskk-auto-tune-enable)
;;
;; ;; 手動チューニング実行
;; (nskk-auto-tune-run)
;;
;; ;; チューニング結果確認
;; (nskk-auto-tune-report)

;;; Code:

(require 'cl-lib)
(require 'nskk-profiler)
(require 'nskk-bottleneck-detector)

;;; カスタマイズ変数

(defgroup nskk-auto-tune nil
  "NSKK auto-tuning settings."
  :group 'nskk
  :prefix "nskk-auto-tune-")

(defcustom nskk-auto-tune-enabled nil
  "非nilの場合、自動チューニングを有効にする。"
  :type 'boolean
  :group 'nskk-auto-tune)

(defcustom nskk-auto-tune-interval 300.0
  "自動チューニングの実行間隔（秒）。"
  :type 'number
  :group 'nskk-auto-tune)

(defcustom nskk-auto-tune-learning-enabled t
  "非nilの場合、学習ベースチューニングを有効にする。"
  :type 'boolean
  :group 'nskk-auto-tune)

(defcustom nskk-auto-tune-aggressive nil
  "非nilの場合、積極的なチューニングを行う。
保守的モード（nil）では安全な範囲内でのみ調整を行う。"
  :type 'boolean
  :group 'nskk-auto-tune)

;;; チューニング対象パラメータ

(defcustom nskk-auto-tune-cache-size-min 100
  "キャッシュサイズの最小値。"
  :type 'integer
  :group 'nskk-auto-tune)

(defcustom nskk-auto-tune-cache-size-max 10000
  "キャッシュサイズの最大値。"
  :type 'integer
  :group 'nskk-auto-tune)

(defcustom nskk-auto-tune-gc-cons-threshold-min 800000
  "gc-cons-thresholdの最小値。"
  :type 'integer
  :group 'nskk-auto-tune)

(defcustom nskk-auto-tune-gc-cons-threshold-max 100000000
  "gc-cons-thresholdの最大値（100MB）。"
  :type 'integer
  :group 'nskk-auto-tune)

;;; 内部変数

(defvar nskk-auto-tune--timer nil
  "自動チューニング用タイマー。")

(defvar nskk-auto-tune--history nil
  "チューニング履歴のリスト。
各要素: (timestamp parameters performance-metrics)")

(defvar nskk-auto-tune--current-params nil
  "現在のパラメータ設定。
plist形式: (:cache-size N :gc-cons-threshold M ...)")

(defvar nskk-auto-tune--best-params nil
  "これまでで最高のパフォーマンスを示したパラメータ設定。")

(defvar nskk-auto-tune--best-score nil
  "最高スコア（小さいほど良い）。")

(defvar nskk-auto-tune--learning-data nil
  "学習データ。
形式: ((params . score) ...)")

;;; 自動チューニング制御

;;;###autoload
(defun nskk-auto-tune-enable ()
  "自動チューニングを有効にする。"
  (interactive)
  (when nskk-auto-tune--timer
    (cancel-timer nskk-auto-tune--timer))

  ;; 初期パラメータ設定
  (nskk-auto-tune--initialize-params)

  ;; タイマー設定
  (setq nskk-auto-tune--timer
        (run-with-timer nskk-auto-tune-interval
                       nskk-auto-tune-interval
                       #'nskk-auto-tune-run))

  (setq nskk-auto-tune-enabled t)

  (message "NSKK auto-tuning enabled (interval: %.0fs)" nskk-auto-tune-interval))

;;;###autoload
(defun nskk-auto-tune-disable ()
  "自動チューニングを無効にする。"
  (interactive)
  (when nskk-auto-tune--timer
    (cancel-timer nskk-auto-tune--timer)
    (setq nskk-auto-tune--timer nil))

  (setq nskk-auto-tune-enabled nil)

  (message "NSKK auto-tuning disabled"))

;;;###autoload
(defun nskk-auto-tune-run ()
  "チューニングを1回実行する。"
  (interactive)
  (message "NSKK auto-tuning: Starting tuning cycle...")

  ;; プロファイリング実行
  (let ((performance-metrics (nskk-auto-tune--measure-performance)))

    ;; スコア計算
    (let ((score (nskk-auto-tune--calculate-score performance-metrics)))

      ;; 履歴に記録
      (push (list (current-time)
                  :params (copy-sequence nskk-auto-tune--current-params)
                  :metrics performance-metrics
                  :score score)
            nskk-auto-tune--history)

      ;; 最高スコア更新チェック
      (when (or (null nskk-auto-tune--best-score)
                (< score nskk-auto-tune--best-score))
        (setq nskk-auto-tune--best-score score
              nskk-auto-tune--best-params (copy-sequence nskk-auto-tune--current-params))
        (message "NSKK auto-tuning: New best score: %.3f" score))

      ;; 学習データ更新
      (when nskk-auto-tune-learning-enabled
        (nskk-auto-tune--update-learning-data nskk-auto-tune--current-params score))

      ;; 次回のパラメータ決定
      (nskk-auto-tune--adjust-parameters performance-metrics score)

      (message "NSKK auto-tuning: Cycle completed (score: %.3f)" score))))

;;; パラメータ初期化と調整

(defun nskk-auto-tune--initialize-params ()
  "パラメータを初期化する。"
  (setq nskk-auto-tune--current-params
        (list :cache-size (if (boundp 'nskk-cache-default-capacity)
                             nskk-cache-default-capacity
                           1000)
              :gc-cons-threshold gc-cons-threshold
              :profiler-sampling-interval (if (boundp 'nskk-profiler-sampling-interval)
                                             nskk-profiler-sampling-interval
                                           1000000))))

(defun nskk-auto-tune--adjust-parameters (metrics score)
  "パフォーマンスメトリクスに基づいてパラメータを調整する。

METRICS: パフォーマンスメトリクス
SCORE: 現在のスコア"
  (let ((new-params (copy-sequence nskk-auto-tune--current-params)))

    ;; GC頻度が高い場合、gc-cons-thresholdを増やす
    (when (> (plist-get metrics :gc-frequency) 5.0)
      (let* ((current-gc-threshold (plist-get new-params :gc-cons-threshold))
             (new-gc-threshold (min (* current-gc-threshold 2)
                                   nskk-auto-tune-gc-cons-threshold-max)))
        (plist-put new-params :gc-cons-threshold new-gc-threshold)
        (message "NSKK auto-tuning: Increasing gc-cons-threshold to %d" new-gc-threshold)))

    ;; GC頻度が低すぎる場合、gc-cons-thresholdを減らす
    (when (< (plist-get metrics :gc-frequency) 0.1)
      (let* ((current-gc-threshold (plist-get new-params :gc-cons-threshold))
             (new-gc-threshold (max (/ current-gc-threshold 2)
                                   nskk-auto-tune-gc-cons-threshold-min)))
        (plist-put new-params :gc-cons-threshold new-gc-threshold)
        (message "NSKK auto-tuning: Decreasing gc-cons-threshold to %d" new-gc-threshold)))

    ;; キャッシュヒット率が低い場合、キャッシュサイズを増やす
    (when (and (plist-get metrics :cache-hit-rate)
               (< (plist-get metrics :cache-hit-rate) 0.7))
      (let* ((current-cache-size (plist-get new-params :cache-size))
             (new-cache-size (min (floor (* current-cache-size 1.5))
                                 nskk-auto-tune-cache-size-max)))
        (plist-put new-params :cache-size new-cache-size)
        (message "NSKK auto-tuning: Increasing cache size to %d" new-cache-size)))

    ;; 学習ベースの調整
    (when nskk-auto-tune-learning-enabled
      (setq new-params (nskk-auto-tune--apply-learning new-params score)))

    ;; パラメータ適用
    (nskk-auto-tune--apply-parameters new-params)
    (setq nskk-auto-tune--current-params new-params)))

(defun nskk-auto-tune--apply-parameters (params)
  "パラメータを実際に適用する。

PARAMS: 適用するパラメータのplist"
  ;; gc-cons-threshold適用
  (let ((gc-threshold (plist-get params :gc-cons-threshold)))
    (when gc-threshold
      (setq gc-cons-threshold gc-threshold)))

  ;; キャッシュサイズ適用（nskk-cacheが存在する場合）
  (when (boundp 'nskk-cache-default-capacity)
    (let ((cache-size (plist-get params :cache-size)))
      (when cache-size
        (setq nskk-cache-default-capacity cache-size))))

  ;; プロファイラーサンプリング間隔適用
  (when (boundp 'nskk-profiler-sampling-interval)
    (let ((sampling-interval (plist-get params :profiler-sampling-interval)))
      (when sampling-interval
        (setq nskk-profiler-sampling-interval sampling-interval)))))

;;; パフォーマンス測定

(defun nskk-auto-tune--measure-performance ()
  "現在のパフォーマンスを測定する。

返り値: plist
  :avg-conversion-time - 平均変換時間（秒）
  :gc-frequency        - GC頻度（回/秒）
  :memory-usage        - メモリ使用量（cons cells）
  :cache-hit-rate      - キャッシュヒット率"
  (let ((metrics nil))

    ;; 変換時間測定（nskk-converterが存在する場合）
    (when (fboundp 'nskk-convert-romaji)
      (let ((total-time 0.0)
            (iterations 100))
        (dotimes (_ iterations)
          (let ((start-time (current-time)))
            (ignore-errors (nskk-convert-romaji "konnnichiha"))
            (setq total-time (+ total-time
                               (float-time (time-subtract (current-time) start-time))))))
        (setq metrics (plist-put metrics :avg-conversion-time (/ total-time iterations)))))

    ;; GC統計
    (setq metrics (plist-put metrics :gc-frequency
                            (/ (float gcs-done)
                              (max 1.0 (float-time (time-since before-init-time))))))

    ;; メモリ使用量
    (let ((mem-counts (memory-use-counts)))
      (setq metrics (plist-put metrics :memory-usage (nth 0 mem-counts))))

    ;; キャッシュヒット率（キャッシュが存在する場合）
    (when (boundp 'nskk-cache-default-capacity)
      ;; 実際のキャッシュインスタンスがあれば、その統計を取得
      ;; ここでは簡易的にデフォルト値を返す
      (setq metrics (plist-put metrics :cache-hit-rate 0.5)))

    metrics))

(defun nskk-auto-tune--calculate-score (metrics)
  "パフォーマンスメトリクスからスコアを計算する。

METRICS: パフォーマンスメトリクス
返り値: スコア（小さいほど良い）"
  (let ((score 0.0))

    ;; 変換時間（重み: 10.0）
    (when (plist-get metrics :avg-conversion-time)
      (setq score (+ score (* 10.0 (plist-get metrics :avg-conversion-time)))))

    ;; GC頻度（重み: 1.0、理想は1-2回/秒）
    (when (plist-get metrics :gc-frequency)
      (let ((gc-freq (plist-get metrics :gc-frequency)))
        (setq score (+ score (* 1.0 (abs (- gc-freq 1.5)))))))

    ;; メモリ使用量（重み: 0.000001、100万cons cellsで1.0）
    (when (plist-get metrics :memory-usage)
      (setq score (+ score (* 0.000001 (plist-get metrics :memory-usage)))))

    ;; キャッシュヒット率（重み: -5.0、高いほど良い）
    (when (plist-get metrics :cache-hit-rate)
      (setq score (+ score (* -5.0 (plist-get metrics :cache-hit-rate)))))

    score))

;;; 学習ベースチューニング

(defun nskk-auto-tune--update-learning-data (params score)
  "学習データを更新する。

PARAMS: パラメータ
SCORE: スコア"
  (push (cons (copy-sequence params) score) nskk-auto-tune--learning-data)

  ;; 履歴サイズ制限（最新100件のみ保持）
  (when (> (length nskk-auto-tune--learning-data) 100)
    (setq nskk-auto-tune--learning-data
          (cl-subseq nskk-auto-tune--learning-data 0 100))))

(defun nskk-auto-tune--apply-learning (params current-score)
  "学習データに基づいてパラメータを調整する。

PARAMS: 現在のパラメータ
CURRENT-SCORE: 現在のスコア
返り値: 調整後のパラメータ"
  (if (< (length nskk-auto-tune--learning-data) 10)
      ;; データ不足の場合は現在のパラメータをそのまま返す
      params

    ;; 良好なスコアを持つパラメータの平均を計算
    (let* ((sorted-data (sort (copy-sequence nskk-auto-tune--learning-data)
                             (lambda (a b) (< (cdr a) (cdr b)))))
           (top-10-percent (cl-subseq sorted-data 0
                                     (max 1 (/ (length sorted-data) 10))))
           (avg-cache-size 0)
           (avg-gc-threshold 0)
           (count (length top-10-percent)))

      ;; 平均計算
      (dolist (entry top-10-percent)
        (let ((p (car entry)))
          (setq avg-cache-size (+ avg-cache-size (plist-get p :cache-size)))
          (setq avg-gc-threshold (+ avg-gc-threshold (plist-get p :gc-cons-threshold)))))

      (setq avg-cache-size (/ avg-cache-size count))
      (setq avg-gc-threshold (/ avg-gc-threshold count))

      ;; 現在の値と学習した平均値を加重平均
      (let ((new-params (copy-sequence params))
            (learning-weight (if nskk-auto-tune-aggressive 0.7 0.3)))
        (plist-put new-params :cache-size
                   (floor (+ (* learning-weight avg-cache-size)
                            (* (- 1.0 learning-weight) (plist-get params :cache-size)))))
        (plist-put new-params :gc-cons-threshold
                   (floor (+ (* learning-weight avg-gc-threshold)
                            (* (- 1.0 learning-weight) (plist-get params :gc-cons-threshold)))))
        new-params))))

;;; A/Bテスト

(defvar nskk-auto-tune--ab-test-configs nil
  "A/Bテスト用の設定リスト。")

(defvar nskk-auto-tune--ab-test-results nil
  "A/Bテストの結果。")

(defun nskk-auto-tune-ab-test-setup (configs)
  "A/Bテストをセットアップする。

CONFIGS: テストする設定のリスト
各設定はplist形式: (:name \"Config A\" :cache-size 1000 ...)"
  (setq nskk-auto-tune--ab-test-configs configs
        nskk-auto-tune--ab-test-results nil))

(defun nskk-auto-tune-ab-test-run ()
  "A/Bテストを実行する。"
  (interactive)
  (unless nskk-auto-tune--ab-test-configs
    (error "No A/B test configs set. Use `nskk-auto-tune-ab-test-setup' first"))

  (message "NSKK auto-tuning: Starting A/B test...")

  (dolist (config nskk-auto-tune--ab-test-configs)
    (let ((name (plist-get config :name)))
      (message "Testing config: %s" name)

      ;; パラメータ適用
      (nskk-auto-tune--apply-parameters config)

      ;; パフォーマンス測定
      (let ((metrics (nskk-auto-tune--measure-performance)))
        (let ((score (nskk-auto-tune--calculate-score metrics)))
          (push (list :name name
                     :config config
                     :metrics metrics
                     :score score)
                nskk-auto-tune--ab-test-results)))))

  ;; 結果をソート（スコアが小さい順）
  (setq nskk-auto-tune--ab-test-results
        (sort nskk-auto-tune--ab-test-results
              (lambda (a b) (< (plist-get a :score)
                             (plist-get b :score)))))

  (message "NSKK auto-tuning: A/B test completed")
  (nskk-auto-tune-ab-test-report))

(defun nskk-auto-tune-ab-test-report ()
  "A/Bテストの結果レポートを表示する。"
  (interactive)
  (unless nskk-auto-tune--ab-test-results
    (error "No A/B test results available"))

  (with-output-to-temp-buffer "*NSKK A/B Test Report*"
    (princ "=======================================================\n")
    (princ "         NSKK Auto-Tuning A/B Test Report\n")
    (princ "=======================================================\n\n")

    (princ (format "%-20s %15s %15s %15s\n"
                   "Config" "Score" "Avg Time(s)" "GC Freq"))
    (princ (make-string 65 ?-))
    (princ "\n")

    (dolist (result nskk-auto-tune--ab-test-results)
      (let ((name (plist-get result :name))
            (score (plist-get result :score))
            (metrics (plist-get result :metrics)))
        (princ (format "%-20s %15.3f %15.6f %15.2f\n"
                      name
                      score
                      (or (plist-get metrics :avg-conversion-time) 0.0)
                      (or (plist-get metrics :gc-frequency) 0.0)))))

    (princ "\n=======================================================\n")
    (princ (format "Winner: %s (score: %.3f)\n"
                   (plist-get (car nskk-auto-tune--ab-test-results) :name)
                   (plist-get (car nskk-auto-tune--ab-test-results) :score)))))

;;; レポート

;;;###autoload
(defun nskk-auto-tune-report ()
  "自動チューニングのレポートを表示する。"
  (interactive)
  (with-output-to-temp-buffer "*NSKK Auto-Tune Report*"
    (princ "=======================================================\n")
    (princ "         NSKK Auto-Tuning Report\n")
    (princ "=======================================================\n\n")

    ;; ステータス
    (princ (format "Status: %s\n" (if nskk-auto-tune-enabled "ENABLED" "DISABLED")))
    (princ (format "Learning: %s\n" (if nskk-auto-tune-learning-enabled "ON" "OFF")))
    (princ (format "Mode: %s\n\n" (if nskk-auto-tune-aggressive "AGGRESSIVE" "CONSERVATIVE")))

    ;; 現在のパラメータ
    (princ "--- Current Parameters ---\n\n")
    (when nskk-auto-tune--current-params
      (princ (format "Cache Size: %d\n"
                     (plist-get nskk-auto-tune--current-params :cache-size)))
      (princ (format "GC Cons Threshold: %d\n"
                     (plist-get nskk-auto-tune--current-params :gc-cons-threshold)))
      (princ (format "Profiler Sampling Interval: %d ns\n\n"
                     (plist-get nskk-auto-tune--current-params :profiler-sampling-interval))))

    ;; 最良のパラメータ
    (when nskk-auto-tune--best-params
      (princ "--- Best Parameters (Score: %.3f) ---\n\n" nskk-auto-tune--best-score)
      (princ (format "Cache Size: %d\n"
                     (plist-get nskk-auto-tune--best-params :cache-size)))
      (princ (format "GC Cons Threshold: %d\n\n"
                     (plist-get nskk-auto-tune--best-params :gc-cons-threshold))))

    ;; 履歴サマリー
    (princ (format "--- History ---\n\n"))
    (princ (format "Total tuning cycles: %d\n" (length nskk-auto-tune--history)))
    (princ (format "Learning data points: %d\n\n" (length nskk-auto-tune--learning-data)))

    (princ "=======================================================\n")))

;;; ユーティリティ

(defun nskk-auto-tune-reset ()
  "自動チューニングの状態をリセットする。"
  (interactive)
  (when nskk-auto-tune-enabled
    (nskk-auto-tune-disable))

  (setq nskk-auto-tune--history nil
        nskk-auto-tune--current-params nil
        nskk-auto-tune--best-params nil
        nskk-auto-tune--best-score nil
        nskk-auto-tune--learning-data nil)

  (message "NSKK auto-tuning reset"))

(defun nskk-auto-tune-apply-best ()
  "最良のパラメータを適用する。"
  (interactive)
  (unless nskk-auto-tune--best-params
    (error "No best parameters available"))

  (nskk-auto-tune--apply-parameters nskk-auto-tune--best-params)
  (setq nskk-auto-tune--current-params nskk-auto-tune--best-params)

  (message "Applied best parameters (score: %.3f)" nskk-auto-tune--best-score))

(provide 'nskk-auto-tune)

;;; nskk-auto-tune.el ends here
