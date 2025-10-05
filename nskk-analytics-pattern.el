;;; nskk-analytics-pattern.el --- Usage pattern analysis for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, analytics, pattern
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.0"))

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

;; このファイルはNSKKの使用パターン分析機能を提供します。
;;
;; 主な機能:
;; 1. 統計収集 - 変換回数、キーストローク、精度、速度を記録
;; 2. パターン可視化 - ASCIIチャート、ヒートマップによる可視化
;; 3. トレンド分析 - 時系列データからトレンドを検出
;; 4. レポート生成 - 使用パターンの詳細レポートを作成
;;
;; 使用例:
;; (require 'nskk-analytics-pattern)
;;
;; ;; イベント記録
;; (nskk-analytics-record-event 'conversion "かんじ" "漢字" 0.05)
;;
;; ;; パターン検出
;; (nskk-analytics-detect-patterns)
;;
;; ;; トレンド分析
;; (nskk-analytics-analyze-trends)

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-analytics-pattern nil
  "NSKK usage pattern analysis settings."
  :group 'nskk
  :prefix "nskk-analytics-pattern-")

(defcustom nskk-analytics-pattern-enabled t
  "非nilの場合、使用パターン分析を有効にする。"
  :type 'boolean
  :group 'nskk-analytics-pattern)

(defcustom nskk-analytics-pattern-max-events 10000
  "記録する最大イベント数。"
  :type 'integer
  :group 'nskk-analytics-pattern)

(defcustom nskk-analytics-pattern-retention-days 30
  "イベントデータの保持期間（日数）。"
  :type 'integer
  :group 'nskk-analytics-pattern)

(defcustom nskk-analytics-pattern-anonymize t
  "非nilの場合、データを匿名化する（GDPR対応）。"
  :type 'boolean
  :group 'nskk-analytics-pattern)

;;; データ構造

(cl-defstruct (nskk-analytics-metrics
               (:constructor nskk-analytics-metrics-create)
               (:copier nskk-analytics-metrics-copy))
  "分析メトリクス構造体。

スロット:
  conversions  - 総変換回数
  keystrokes   - 総キーストローク数
  accuracy     - 変換精度（0.0-1.0）
  speed        - 平均変換速度（秒）
  patterns     - 使用パターン（alist）
  timestamp    - 収集タイムスタンプ"
  (conversions 0)
  (keystrokes 0)
  (accuracy 1.0)
  (speed 0.0)
  (patterns nil)
  (timestamp (current-time)))

(cl-defstruct (nskk-analytics-event
               (:constructor nskk-analytics-event-create)
               (:copier nskk-analytics-event-copy))
  "分析イベント構造体。

スロット:
  type         - イベントタイプ（conversion/error/cancel等）
  timestamp    - Unixタイムスタンプ
  duration     - 操作時間（秒）
  metadata     - 追加メタデータ（plist）"
  (type nil)
  (timestamp (float-time))
  (duration 0.0)
  (metadata nil))

;;; 内部変数

(defvar nskk-analytics-pattern--events nil
  "記録されたイベントのリスト。")

(defvar nskk-analytics-pattern--metrics nil
  "現在のメトリクス。")

(defvar nskk-analytics-pattern--patterns nil
  "検出された使用パターン。")

(defvar nskk-analytics-pattern--trends nil
  "検出されたトレンド。")

;;; イベント記録

;;;###autoload
(defun nskk-analytics-record-event (type &optional metadata duration)
  "イベントを記録する。

TYPE: イベントタイプ（シンボル）
METADATA: メタデータ（plist）
DURATION: 操作時間（秒）"
  (when nskk-analytics-pattern-enabled
    (let ((event (nskk-analytics-event-create
                  :type type
                  :timestamp (float-time)
                  :duration (or duration 0.0)
                  :metadata (when (not nskk-analytics-pattern-anonymize)
                              metadata))))
      ;; イベントリストに追加
      (push event nskk-analytics-pattern--events)

      ;; 最大イベント数を超えた場合は古いものを削除
      (when (> (length nskk-analytics-pattern--events)
               nskk-analytics-pattern-max-events)
        (setq nskk-analytics-pattern--events
              (cl-subseq nskk-analytics-pattern--events
                         0 nskk-analytics-pattern-max-events)))

      ;; 保持期間を超えたイベントを削除
      (nskk-analytics-pattern--cleanup-old-events)

      event)))

(defun nskk-analytics-pattern--cleanup-old-events ()
  "保持期間を超えたイベントを削除する。"
  (let ((cutoff-time (- (float-time)
                        (* nskk-analytics-pattern-retention-days
                           24 60 60))))
    (setq nskk-analytics-pattern--events
          (cl-remove-if (lambda (event)
                          (< (nskk-analytics-event-timestamp event)
                             cutoff-time))
                        nskk-analytics-pattern--events))))

;;; メトリクス収集

;;;###autoload
(defun nskk-analytics-collect-metrics ()
  "現在のメトリクスを収集する。"
  (interactive)
  (let ((conversions 0)
        (keystrokes 0)
        (total-duration 0.0)
        (error-count 0))

    ;; イベントから統計を計算
    (dolist (event nskk-analytics-pattern--events)
      (pcase (nskk-analytics-event-type event)
        ('conversion
         (cl-incf conversions)
         (cl-incf total-duration (nskk-analytics-event-duration event)))
        ('keystroke
         (cl-incf keystrokes))
        ('error
         (cl-incf error-count))))

    ;; メトリクス構造体を作成
    (setq nskk-analytics-pattern--metrics
          (nskk-analytics-metrics-create
           :conversions conversions
           :keystrokes keystrokes
           :accuracy (if (> conversions 0)
                         (/ (float (- conversions error-count))
                            conversions)
                       1.0)
           :speed (if (> conversions 0)
                      (/ total-duration conversions)
                    0.0)
           :patterns (nskk-analytics-pattern--extract-patterns)
           :timestamp (current-time)))

    nskk-analytics-pattern--metrics))

(defun nskk-analytics-pattern--extract-patterns ()
  "イベントから使用パターンを抽出する。"
  (let ((hourly-counts (make-vector 24 0))
        (daily-counts (make-vector 7 0))
        (conversion-freq (make-hash-table :test 'equal)))

    ;; 時間帯別・曜日別カウント
    (dolist (event nskk-analytics-pattern--events)
      (when (eq (nskk-analytics-event-type event) 'conversion)
        (let* ((time (seconds-to-time (nskk-analytics-event-timestamp event)))
               (hour (string-to-number (format-time-string "%H" time)))
               (dow (string-to-number (format-time-string "%w" time)))
               (input (plist-get (nskk-analytics-event-metadata event) :input)))
          (cl-incf (aref hourly-counts hour))
          (cl-incf (aref daily-counts dow))

          ;; 変換頻度カウント（匿名化されていない場合のみ）
          (when (and input (not nskk-analytics-pattern-anonymize))
            (puthash input (1+ (gethash input conversion-freq 0))
                     conversion-freq)))))

    ;; 頻度の高い順にソート
    (let ((freq-list nil))
      (maphash (lambda (k v) (push (cons k v) freq-list))
               conversion-freq)
      (setq freq-list (cl-sort freq-list #'> :key #'cdr))

      `((hourly . ,(append hourly-counts nil))
        (daily . ,(append daily-counts nil))
        (top-conversions . ,(cl-subseq freq-list 0 (min 20 (length freq-list))))))))

;;; パターン検出

;;;###autoload
(defun nskk-analytics-detect-patterns (&optional events)
  "使用パターンを検出する。

EVENTS: 分析対象のイベントリスト（nilの場合は全イベント）
戻り値: 検出されたパターンのalist"
  (interactive)
  (let* ((target-events (or events nskk-analytics-pattern--events))
         (metrics (nskk-analytics-collect-metrics))
         (patterns (nskk-analytics-metrics-patterns metrics)))

    (setq nskk-analytics-pattern--patterns
          `((peak-hours . ,(nskk-analytics-pattern--find-peak-hours
                            (alist-get 'hourly patterns)))
            (peak-days . ,(nskk-analytics-pattern--find-peak-days
                           (alist-get 'daily patterns)))
            (frequent-conversions . ,(alist-get 'top-conversions patterns))
            (usage-intensity . ,(nskk-analytics-pattern--calculate-intensity
                                 target-events))
            (efficiency-score . ,(nskk-analytics-pattern--calculate-efficiency
                                  metrics))))

    (when (called-interactively-p 'interactive)
      (message "Patterns detected: %d peak hours, %d frequent conversions"
               (length (alist-get 'peak-hours nskk-analytics-pattern--patterns))
               (length (alist-get 'frequent-conversions nskk-analytics-pattern--patterns))))

    nskk-analytics-pattern--patterns))

(defun nskk-analytics-pattern--find-peak-hours (hourly-counts)
  "ピーク使用時間帯を検出する。

HOURLY-COUNTS: 時間帯別カウント配列
戻り値: ピーク時間帯のリスト"
  (when hourly-counts
    (let ((avg (/ (apply #'+ hourly-counts) (float (length hourly-counts))))
          (peaks nil))
      (dotimes (i (length hourly-counts))
        (when (> (nth i hourly-counts) (* 1.5 avg))
          (push i peaks)))
      (nreverse peaks))))

(defun nskk-analytics-pattern--find-peak-days (daily-counts)
  "ピーク使用曜日を検出する。

DAILY-COUNTS: 曜日別カウント配列
戻り値: ピーク曜日のリスト"
  (when daily-counts
    (let ((avg (/ (apply #'+ daily-counts) (float (length daily-counts))))
          (peaks nil)
          (day-names '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))
      (dotimes (i (length daily-counts))
        (when (> (nth i daily-counts) (* 1.3 avg))
          (push (nth i day-names) peaks)))
      (nreverse peaks))))

(defun nskk-analytics-pattern--calculate-intensity (events)
  "使用強度を計算する（イベント数/日）。

EVENTS: イベントリスト
戻り値: 使用強度"
  (if (null events)
      0.0
    (let* ((first-time (nskk-analytics-event-timestamp (car (last events))))
           (last-time (nskk-analytics-event-timestamp (car events)))
           (days (/ (- last-time first-time) 86400.0)))
      (if (> days 0)
          (/ (float (length events)) days)
        (float (length events))))))

(defun nskk-analytics-pattern--calculate-efficiency (metrics)
  "変換効率スコアを計算する（0-100）。

METRICS: メトリクス構造体
戻り値: 効率スコア"
  (let* ((accuracy (nskk-analytics-metrics-accuracy metrics))
         (speed (nskk-analytics-metrics-speed metrics))
         ;; 速度スコア（0.1秒未満=100点、1秒以上=0点）
         (speed-score (max 0 (min 100 (* (- 1.0 (/ speed 1.0)) 100))))
         ;; 精度スコア（そのままパーセント）
         (accuracy-score (* accuracy 100))
         ;; 総合スコア（速度60%、精度40%）
         (total-score (+ (* speed-score 0.6) (* accuracy-score 0.4))))
    (round total-score)))

;;; トレンド分析

;;;###autoload
(defun nskk-analytics-analyze-trends (&optional metrics-history)
  "時系列データからトレンドを分析する。

METRICS-HISTORY: メトリクス履歴のリスト
戻り値: 検出されたトレンド"
  (interactive)
  (let* ((history (or metrics-history
                      (nskk-analytics-pattern--build-metrics-history)))
         (trends nil))

    (if (>= (length history) 2)
        (progn
          ;; 変換数のトレンド
          (let ((conversion-trend (nskk-analytics-pattern--calculate-trend
                                   history
                                   #'nskk-analytics-metrics-conversions)))
            (push `(conversions . ,conversion-trend) trends))

          ;; 精度のトレンド
          (let ((accuracy-trend (nskk-analytics-pattern--calculate-trend
                                 history
                                 #'nskk-analytics-metrics-accuracy)))
            (push `(accuracy . ,accuracy-trend) trends))

          ;; 速度のトレンド
          (let ((speed-trend (nskk-analytics-pattern--calculate-trend
                              history
                              #'nskk-analytics-metrics-speed)))
            (push `(speed . ,speed-trend) trends))

          (setq nskk-analytics-pattern--trends (nreverse trends)))
      ;; データが不足している場合は安定トレンドとして扱う
      (let ((stable-trends '((conversions . stable)
                             (accuracy . stable)
                             (speed . stable))))
        (setq trends stable-trends)
        (setq nskk-analytics-pattern--trends stable-trends)))

    (when (called-interactively-p 'interactive)
      (message "Trends analyzed: %d metrics tracked" (length trends)))

    nskk-analytics-pattern--trends))

(defcustom nskk-analytics-pattern-trend-interval 3600
  "トレンド分析で集計する時間間隔（秒）。"
  :type 'integer
  :group 'nskk-analytics-pattern)

(defun nskk-analytics-pattern--bucket-start-time (bucket-index)
  "BUCKET-INDEX に対応する開始時刻(Timeオブジェクト)を返す。"
  (seconds-to-time (* bucket-index nskk-analytics-pattern-trend-interval)))

(defun nskk-analytics-pattern--build-metrics-history ()
  "イベントから集計間隔ごとのメトリクス履歴を構築する。"
  (let ((buckets (make-hash-table :test 'equal)))
    ;; 時間バケットにイベントを割り当て
    (dolist (event nskk-analytics-pattern--events)
      (let* ((timestamp (nskk-analytics-event-timestamp event))
             (bucket (floor timestamp nskk-analytics-pattern-trend-interval)))
        (push event (gethash bucket buckets))))

    ;; 各バケットからメトリクスを作成
    (let ((history nil))
      (maphash
       (lambda (bucket events)
         (let* ((conversions (cl-count 'conversion events :key #'nskk-analytics-event-type))
                (errors (cl-count 'error events :key #'nskk-analytics-event-type))
                (successful (max 0 (- conversions errors)))
                (accuracy (if (> conversions 0)
                              (/ successful (float conversions))
                            1.0))
                (total-duration (cl-loop for ev in events
                                         when (eq (nskk-analytics-event-type ev) 'conversion)
                                         sum (nskk-analytics-event-duration ev)))
                (avg-speed (if (> conversions 0)
                               (/ total-duration conversions)
                             0.0))
                (metrics (nskk-analytics-metrics-create
                          :conversions conversions
                          :accuracy accuracy
                          :speed avg-speed
                          :timestamp (nskk-analytics-pattern--bucket-start-time bucket))))
           (push metrics history)))
       buckets)
      (cl-sort history #'time-less-p
               :key #'nskk-analytics-metrics-timestamp))))

(defun nskk-analytics-pattern--calculate-trend (metrics-list extractor)
  "単純線形回帰でトレンドを計算する。

METRICS-LIST: メトリクスのリスト
EXTRACTOR: メトリクスから値を抽出する関数
戻り値: トレンド（'increasing, 'decreasing, 'stable）"
  (let* ((n (length metrics-list))
         (values (mapcar extractor metrics-list))
         (sum-x (/ (* n (1- n)) 2))
         (sum-y (apply #'+ values))
         (sum-xy 0)
         (sum-x2 0))

    (dotimes (i n)
      (cl-incf sum-xy (* i (nth i values)))
      (cl-incf sum-x2 (* i i)))

    ;; 傾きを計算
    (let ((slope (/ (- (* n sum-xy) (* sum-x sum-y))
                    (float (- (* n sum-x2) (* sum-x sum-x))))))
      (cond
       ((> slope 0.01) 'increasing)
       ((< slope -0.01) 'decreasing)
       (t 'stable)))))

;;; 可視化

;;;###autoload
(defun nskk-analytics-visualize-patterns ()
  "使用パターンをASCIIチャートで可視化する。"
  (interactive)
  (let ((patterns (or nskk-analytics-pattern--patterns
                      (nskk-analytics-detect-patterns))))
    (with-current-buffer (get-buffer-create "*NSKK Analytics*")
      (erase-buffer)
      (insert "=== NSKK Usage Pattern Analysis ===\n\n")

      ;; ピーク時間帯
      (insert "Peak Usage Hours:\n")
      (let ((peak-hours (alist-get 'peak-hours patterns)))
        (if peak-hours
            (dolist (hour peak-hours)
              (insert (format "  %02d:00 - %02d:00\n" hour (1+ hour))))
          (insert "  No peak hours detected\n")))
      (insert "\n")

      ;; 頻繁な変換
      (insert "Most Frequent Conversions:\n")
      (let ((freq-conv (alist-get 'frequent-conversions patterns)))
        (if freq-conv
            (dotimes (i (min 10 (length freq-conv)))
              (let ((item (nth i freq-conv)))
                (insert (format "  %2d. %s (%d times)\n"
                                (1+ i) (car item) (cdr item)))))
          (insert "  No data available\n")))
      (insert "\n")

      ;; 使用強度
      (insert (format "Usage Intensity: %.2f events/day\n"
                      (alist-get 'usage-intensity patterns)))

      ;; 効率スコア
      (insert (format "Efficiency Score: %d/100\n"
                      (alist-get 'efficiency-score patterns)))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;; ヘルパー関数

(defun nskk-analytics-pattern-get-events-by-type (type)
  "指定タイプのイベントを取得する。

TYPE: イベントタイプ
戻り値: イベントのリスト"
  (cl-remove-if-not (lambda (event)
                      (eq (nskk-analytics-event-type event) type))
                    nskk-analytics-pattern--events))

(defun nskk-analytics-pattern-clear-data ()
  "すべての分析データをクリアする。"
  (interactive)
  (when (yes-or-no-p "Clear all analytics data? ")
    (setq nskk-analytics-pattern--events nil
          nskk-analytics-pattern--metrics nil
          nskk-analytics-pattern--patterns nil
          nskk-analytics-pattern--trends nil)
    (message "Analytics data cleared")))

(provide 'nskk-analytics-pattern)

;;; nskk-analytics-pattern.el ends here
