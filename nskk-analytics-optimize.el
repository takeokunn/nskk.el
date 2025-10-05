;;; nskk-analytics-optimize.el --- Auto-optimization for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, analytics, optimization
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

;; このファイルはNSKKの自動最適化システムを提供します。
;;
;; 主な機能:
;; 1. パラメータ自動調整 - キャッシュサイズ、スレッド数等を自動調整
;; 2. A/Bテスト - 複数の設定を比較し最適な設定を特定
;; 3. 最適化提案 - 使用パターンに基づいた改善提案
;; 4. 自動適用 - 最適化を自動で適用（オプション）
;;
;; 使用例:
;; (require 'nskk-analytics-optimize)
;;
;; ;; A/Bテスト実行
;; (nskk-analytics-run-ab-test "cache-size"
;;                             '((nskk-cache-size . 1000))
;;                             '((nskk-cache-size . 5000)))
;;
;; ;; 最適化提案取得
;; (nskk-analytics-suggest-optimizations)
;;
;; ;; 自動最適化実行
;; (nskk-analytics-auto-optimize)

;;; Code:

(require 'cl-lib)
(require 'nskk-analytics-pattern)

;;; カスタマイズ変数

(defgroup nskk-analytics-optimize nil
  "NSKK auto-optimization settings."
  :group 'nskk
  :prefix "nskk-analytics-optimize-")

(defcustom nskk-analytics-optimize-auto-apply nil
  "非nilの場合、最適化を自動で適用する。"
  :type 'boolean
  :group 'nskk-analytics-optimize)

(defcustom nskk-analytics-optimize-min-samples 100
  "最適化判定に必要な最小サンプル数。"
  :type 'integer
  :group 'nskk-analytics-optimize)

(defcustom nskk-analytics-optimize-confidence-level 0.95
  "統計的有意性の信頼水準（0.0-1.0）。"
  :type 'float
  :group 'nskk-analytics-optimize)

;;; データ構造

(cl-defstruct (nskk-analytics-ab-test
               (:constructor nskk-analytics-ab-test-create)
               (:copier nskk-analytics-ab-test-copy))
  "A/Bテスト構造体。

スロット:
  name         - テスト名
  variant-a    - 設定A（alist）
  variant-b    - 設定B（alist）
  metrics      - 追跡するメトリクス（シンボルのリスト）
  duration     - テスト期間（秒）
  results      - テスト結果（plist）
  start-time   - 開始時刻
  active       - アクティブフラグ"
  (name nil)
  (variant-a nil)
  (variant-b nil)
  (metrics '(speed accuracy conversions))
  (duration 3600)
  (results nil)
  (start-time nil)
  (active nil))

(cl-defstruct (nskk-analytics-optimization
               (:constructor nskk-analytics-optimization-create)
               (:copier nskk-analytics-optimization-copy))
  "最適化提案構造体。

スロット:
  parameter    - パラメータ名
  current      - 現在の値
  suggested    - 提案値
  reason       - 提案理由
  impact       - 予想される効果（high/medium/low）
  confidence   - 信頼度（0.0-1.0）"
  (parameter nil)
  (current nil)
  (suggested nil)
  (reason nil)
  (impact 'medium)
  (confidence 0.5))

;;; 内部変数

(defvar nskk-analytics-optimize--tests nil
  "実行中/完了したA/Bテストのリスト。")

(defvar nskk-analytics-optimize--optimizations nil
  "最適化提案のリスト。")

(defvar nskk-analytics-optimize--parameters
  '((nskk-cache-size . (100 10000))
    (nskk-thread-pool-size . (2 16))
    (nskk-completion-limit . (10 100))
    (nskk-history-size . (100 10000)))
  "最適化可能なパラメータとその範囲。")

;;; A/Bテスト

;;;###autoload
(defun nskk-analytics-run-ab-test (name variant-a variant-b &optional duration)
  "A/Bテストを実行する。

NAME: テスト名
VARIANT-A: 設定A（alist）
VARIANT-B: 設定B（alist）
DURATION: テスト期間（秒、デフォルト: 3600）"
  (interactive
   (list (read-string "Test name: ")
         (read--expression "Variant A (alist): ")
         (read--expression "Variant B (alist): ")
         (read-number "Duration (seconds): " 3600)))

  (let ((test (nskk-analytics-ab-test-create
               :name name
               :variant-a variant-a
               :variant-b variant-b
               :duration (or duration 3600)
               :start-time (current-time)
               :active t)))

    ;; テストリストに追加
    (push test nskk-analytics-optimize--tests)

    ;; Variant A を適用
    (nskk-analytics-optimize--apply-variant variant-a)

    ;; タイマーで Variant B に切り替え
    (run-at-time (/ (or duration 3600) 2.0) nil
                 (lambda ()
                   (nskk-analytics-optimize--apply-variant variant-b)))

    ;; タイマーでテスト終了
    (run-at-time (or duration 3600) nil
                 (lambda ()
                   (nskk-analytics-optimize--complete-ab-test test)))

    (message "A/B test '%s' started (duration: %d seconds)" name (or duration 3600))
    test))

(defun nskk-analytics-optimize--apply-variant (variant)
  "設定バリアントを適用する。

VARIANT: 設定のalist"
  (dolist (setting variant)
    (set (car setting) (cdr setting))))

(defun nskk-analytics-optimize--complete-ab-test (test)
  "A/Bテストを完了する。

TEST: A/Bテスト構造体"
  (setf (nskk-analytics-ab-test-active test) nil)

  ;; 結果を分析
  (let* ((metrics (nskk-analytics-collect-metrics))
         (results (nskk-analytics-optimize--analyze-ab-results test metrics)))
    (setf (nskk-analytics-ab-test-results test) results)

    (message "A/B test '%s' completed. Winner: %s"
             (nskk-analytics-ab-test-name test)
             (plist-get results :winner))))

(defun nskk-analytics-optimize--analyze-ab-results (test metrics)
  "A/Bテスト結果を分析する。

TEST: A/Bテスト構造体
METRICS: メトリクス構造体
戻り値: 結果のplist"
  ;; 簡易的な実装（実際にはより詳細な統計分析が必要）
  (let* ((speed (nskk-analytics-metrics-speed metrics))
         (accuracy (nskk-analytics-metrics-accuracy metrics))
         (score (+ (* speed 0.6) (* accuracy 0.4)))
         (winner (if (> score 0.5) 'variant-b 'variant-a))
         (significance (nskk-analytics-ab-significance
                        `((speed . ,speed) (accuracy . ,accuracy))
                        `((speed . ,(+ speed 0.01)) (accuracy . ,(- accuracy 0.01))))))
    `(:winner ,winner
      :score ,score
      :significance ,significance
      :metrics ,metrics)))

;;;###autoload
(defun nskk-analytics-ab-significance (results-a results-b)
  "2つの結果の統計的有意性を計算する。

RESULTS-A: 結果A（alist）
RESULTS-B: 結果B（alist）
戻り値: 有意性（t/nil）"
  ;; t検定の簡易実装（実際には標本サイズと分散も考慮）
  (let ((diff-speed (abs (- (alist-get 'speed results-a)
                            (alist-get 'speed results-b))))
        (diff-accuracy (abs (- (alist-get 'accuracy results-a)
                               (alist-get 'accuracy results-b)))))
    ;; 差が十分大きい場合に有意とみなす
    (or (> diff-speed 0.05)
        (> diff-accuracy 0.1))))

;;; 自動最適化

;;;###autoload
(defun nskk-analytics-suggest-optimizations ()
  "使用パターンに基づいた最適化を提案する。

戻り値: 最適化提案のリスト"
  (interactive)
  (let ((metrics (nskk-analytics-collect-metrics))
        (patterns (nskk-analytics-detect-patterns))
        (suggestions nil))

    ;; キャッシュサイズの最適化
    (let ((cache-opt (nskk-analytics-optimize--suggest-cache-size metrics patterns)))
      (when cache-opt
        (push cache-opt suggestions)))

    ;; スレッドプールサイズの最適化
    (let ((thread-opt (nskk-analytics-optimize--suggest-thread-pool-size patterns)))
      (when thread-opt
        (push thread-opt suggestions)))

    ;; 補完制限の最適化
    (let ((completion-opt (nskk-analytics-optimize--suggest-completion-limit metrics)))
      (when completion-opt
        (push completion-opt suggestions)))

    ;; 履歴サイズの最適化
    (let ((history-opt (nskk-analytics-optimize--suggest-history-size patterns)))
      (when history-opt
        (push history-opt suggestions)))

    (setq nskk-analytics-optimize--optimizations (nreverse suggestions))

    (when (called-interactively-p 'interactive)
      (nskk-analytics-optimize--display-suggestions suggestions))

    suggestions))

(defun nskk-analytics-optimize--suggest-cache-size (metrics patterns)
  "キャッシュサイズの最適化を提案する。

METRICS: メトリクス構造体
PATTERNS: パターンのalist
戻り値: 最適化提案またはnil"
  (let* ((conversions (nskk-analytics-metrics-conversions metrics))
         (current-size (or (bound-and-true-p nskk-cache-size) 1000))
         (suggested-size (min 10000 (* conversions 2))))

    (when (and (> conversions 100)
               (> (abs (- suggested-size current-size)) 500))
      (nskk-analytics-optimization-create
       :parameter 'nskk-cache-size
       :current current-size
       :suggested suggested-size
       :reason (format "Based on %d conversions, larger cache may improve performance"
                       conversions)
       :impact (if (> (- suggested-size current-size) 2000) 'high 'medium)
       :confidence 0.8))))

(defun nskk-analytics-optimize--suggest-thread-pool-size (patterns)
  "スレッドプールサイズの最適化を提案する。

PATTERNS: パターンのalist
戻り値: 最適化提案またはnil"
  (let* ((intensity (alist-get 'usage-intensity patterns))
         (current-size (or (bound-and-true-p nskk-thread-pool-size) 4))
         (suggested-size (cond
                          ((> intensity 1000) 8)
                          ((> intensity 500) 6)
                          ((> intensity 100) 4)
                          (t 2))))

    (when (not (eq suggested-size current-size))
      (nskk-analytics-optimization-create
       :parameter 'nskk-thread-pool-size
       :current current-size
       :suggested suggested-size
       :reason (format "Based on usage intensity (%.0f events/day)" intensity)
       :impact 'medium
       :confidence 0.7))))

(defun nskk-analytics-optimize--suggest-completion-limit (metrics)
  "補完制限の最適化を提案する。

METRICS: メトリクス構造体
戻り値: 最適化提案またはnil"
  (let* ((speed (nskk-analytics-metrics-speed metrics))
         (current-limit (or (bound-and-true-p nskk-completion-limit) 20))
         (suggested-limit (cond
                           ((< speed 0.05) 50)   ;; 速い場合は多めに
                           ((< speed 0.1) 30)
                           ((< speed 0.2) 20)
                           (t 10))))             ;; 遅い場合は少なめに

    (when (not (eq suggested-limit current-limit))
      (nskk-analytics-optimization-create
       :parameter 'nskk-completion-limit
       :current current-limit
       :suggested suggested-limit
       :reason (format "Based on conversion speed (%.3fs)" speed)
       :impact 'low
       :confidence 0.6))))

(defun nskk-analytics-optimize--suggest-history-size (patterns)
  "履歴サイズの最適化を提案する。

PATTERNS: パターンのalist
戻り値: 最適化提案またはnil"
  (let* ((freq-conv (alist-get 'frequent-conversions patterns))
         (current-size (or (bound-and-true-p nskk-history-size) 1000))
         (suggested-size (max 1000 (min 10000 (* (length freq-conv) 50)))))

    (when (> (abs (- suggested-size current-size)) 500)
      (nskk-analytics-optimization-create
       :parameter 'nskk-history-size
       :current current-size
       :suggested suggested-size
       :reason (format "Based on %d frequent conversions" (length freq-conv))
       :impact 'low
       :confidence 0.5))))

(defun nskk-analytics-optimize--display-suggestions (suggestions)
  "最適化提案を表示する。

SUGGESTIONS: 最適化提案のリスト"
  (with-current-buffer (get-buffer-create "*NSKK Optimizations*")
    (erase-buffer)
    (insert "=== NSKK Optimization Suggestions ===\n\n")

    (if (null suggestions)
        (insert "No optimizations suggested at this time.\n")
      (dolist (opt suggestions)
        (insert (format "Parameter: %s\n" (nskk-analytics-optimization-parameter opt)))
        (insert (format "  Current:    %s\n" (nskk-analytics-optimization-current opt)))
        (insert (format "  Suggested:  %s\n" (nskk-analytics-optimization-suggested opt)))
        (insert (format "  Reason:     %s\n" (nskk-analytics-optimization-reason opt)))
        (insert (format "  Impact:     %s\n" (nskk-analytics-optimization-impact opt)))
        (insert (format "  Confidence: %.0f%%\n\n"
                        (* (nskk-analytics-optimization-confidence opt) 100)))))

    (insert "\nTo apply these optimizations, run: M-x nskk-analytics-apply-optimizations\n")

    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;;###autoload
(defun nskk-analytics-apply-optimizations (&optional optimizations)
  "最適化提案を適用する。

OPTIMIZATIONS: 適用する最適化リスト（nilの場合は全提案）"
  (interactive)
  (let ((opts (or optimizations nskk-analytics-optimize--optimizations)))
    (when (null opts)
      (setq opts (nskk-analytics-suggest-optimizations)))

    (if (null opts)
        (message "No optimizations to apply")
      (when (or nskk-analytics-optimize-auto-apply
                (yes-or-no-p (format "Apply %d optimizations? " (length opts))))
        (dolist (opt opts)
          (set (nskk-analytics-optimization-parameter opt)
               (nskk-analytics-optimization-suggested opt))
          (message "Applied: %s = %s"
                   (nskk-analytics-optimization-parameter opt)
                   (nskk-analytics-optimization-suggested opt)))
        (message "Applied %d optimizations" (length opts))))))

;;;###autoload
(defun nskk-analytics-auto-optimize ()
  "自動最適化を実行する（提案→適用）。"
  (interactive)
  (let ((suggestions (nskk-analytics-suggest-optimizations)))
    (when suggestions
      (nskk-analytics-apply-optimizations suggestions))))

;;; パラメータチューニング

(defun nskk-analytics-optimize-parameter (parameter)
  "特定のパラメータを最適化する。

PARAMETER: パラメータ名（シンボル）"
  (interactive
   (list (intern (completing-read "Parameter: "
                                  (mapcar #'car nskk-analytics-optimize--parameters)))))
  (let* ((range (alist-get parameter nskk-analytics-optimize--parameters))
         (current (symbol-value parameter))
         (best-value current)
         (best-score 0.0))

    (when range
      ;; グリッドサーチ（簡易的）
      (let ((min-val (car range))
            (max-val (cadr range))
            (steps 5))
        (dotimes (i steps)
          (let* ((test-value (+ min-val (* (/ (- max-val min-val) (float steps)) i)))
                 (score (nskk-analytics-optimize--evaluate-parameter-value
                         parameter test-value)))
            (when (> score best-score)
              (setq best-value test-value
                    best-score score)))))

      (when (not (eq best-value current))
        (message "Optimal value for %s: %s (current: %s, score: %.2f)"
                 parameter best-value current best-score)
        (when (yes-or-no-p (format "Apply optimization %s = %s? " parameter best-value))
          (set parameter best-value))))))

(defun nskk-analytics-optimize--evaluate-parameter-value (parameter value)
  "パラメータ値を評価する。

PARAMETER: パラメータ名
VALUE: テスト値
戻り値: スコア（0.0-1.0）"
  ;; 実際の実装ではパラメータを一時的に変更して性能測定
  ;; ここでは簡易的なヒューリスティックスコアを返す
  (let ((current (symbol-value parameter)))
    (set parameter value)
    (sleep-for 0.1)  ;; 短時間テスト
    (let ((metrics (nskk-analytics-collect-metrics))
          (score 0.0))
      (setq score (/ 1.0 (1+ (nskk-analytics-metrics-speed metrics))))
      (set parameter current)  ;; 元に戻す
      score)))

(provide 'nskk-analytics-optimize)

;;; nskk-analytics-optimize.el ends here
