;;; nskk-optimize.el --- Performance optimization for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Takeshi Umeda

;; Author: NSKK Contributors
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n

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

;; このファイルはNSKKのパフォーマンス最適化ツールを提供します。
;;
;; 主な機能:
;; 1. ベンチマーク測定（変換速度、メモリ使用量）
;; 2. 最適化マクロ（コンパイル時展開）
;; 3. メモリプロファイリング
;; 4. パフォーマンス回帰テスト
;;
;; 目標:
;; - キー入力応答時間: < 0.1ms (100μs)
;; - 辞書検索時間: < 50ms
;; - メモリ使用量: 最小化
;;
;; 使用例:
;; (nskk-benchmark-romaji-conversion "konnnichiha" 10000)
;; (nskk-benchmark-memory-usage)
;; (nskk-profile-function #'nskk-convert-romaji "test")

;;; Code:

(require 'cl-lib)
(require 'benchmark)

;;; カスタマイズ可能変数

(defgroup nskk-optimize nil
  "NSKK performance optimization settings."
  :group 'nskk
  :prefix "nskk-optimize-")

(defcustom nskk-optimize-enable-profiling nil
  "Enable profiling data collection when non-nil."
  :type 'boolean
  :group 'nskk-optimize)

(defcustom nskk-optimize-benchmark-iterations 10000
  "Number of iterations for benchmarks."
  :type 'integer
  :group 'nskk-optimize)

;;; パフォーマンス測定マクロ

(defmacro nskk-measure-time (&rest body)
  "Measure execution time of BODY and return result in microseconds.
Return value is a cons cell (ELAPSED-MICROSECONDS . BODY-RESULT)."
  (declare (indent 0) (debug t))
  (let ((start-time (make-symbol "start-time"))
        (end-time (make-symbol "end-time"))
        (result (make-symbol "result")))
    `(let ((,start-time (current-time))
           (,result (progn ,@body))
           (,end-time (current-time)))
       (cons (* (float-time (time-subtract ,end-time ,start-time)) 1000000.0)
             ,result))))

(defmacro nskk-with-profiling (name &rest body)
  "Profile execution of BODY and record under NAME.
Only effective when `nskk-optimize-enable-profiling' is non-nil."
  (declare (indent 1) (debug t))
  `(if nskk-optimize-enable-profiling
       (let ((start-time (current-time))
             (result (progn ,@body))
             (end-time (current-time)))
         (nskk-optimize--record-profile
          ,name
          (time-subtract end-time start-time))
         result)
     (progn ,@body)))

;;; プロファイリングデータ管理

(defvar nskk-optimize--profile-data nil
  "Alist holding profiling data.
Format: ((NAME COUNT TOTAL-TIME MIN-TIME MAX-TIME) ...).")

(defun nskk-optimize--record-profile (name elapsed-time)
  "Record profiling data for NAME with ELAPSED-TIME."
  (let* ((elapsed-us (* (float-time elapsed-time) 1000000.0))
         (entry (assoc name nskk-optimize--profile-data))
         (count (if entry (nth 1 entry) 0))
         (total (if entry (nth 2 entry) 0.0))
         (min-time (if entry (min (nth 3 entry) elapsed-us) elapsed-us))
         (max-time (if entry (max (nth 4 entry) elapsed-us) elapsed-us)))
    (setf (alist-get name nskk-optimize--profile-data nil nil #'string=)
          (list (1+ count) (+ total elapsed-us) min-time max-time))))

(defun nskk-optimize-reset-profile ()
  "Reset profiling data."
  (interactive)
  (setq nskk-optimize--profile-data nil))

(defun nskk-optimize-show-profile ()
  "Display profiling data in a buffer."
  (interactive)
  (if (null nskk-optimize--profile-data)
      (message "No profiling data available")
    (with-output-to-temp-buffer "*NSKK Profile*"
      (princ (format "%-30s %10s %15s %15s %15s %15s\n"
                     "Name" "Count" "Total(μs)" "Avg(μs)" "Min(μs)" "Max(μs)"))
      (princ (make-string 110 ?-))
      (princ "\n")
      (dolist (entry (sort (copy-sequence nskk-optimize--profile-data)
                          (lambda (a b) (> (nth 2 (cdr a)) (nth 2 (cdr b))))))
        (let* ((name (car entry))
               (count (nth 1 entry))
               (total (nth 2 entry))
               (min-time (nth 3 entry))
               (max-time (nth 4 entry))
               (avg (/ total count)))
          (princ (format "%-30s %10d %15.2f %15.2f %15.2f %15.2f\n"
                        name count total avg min-time max-time)))))))

;;; ベンチマーク関数

(defun nskk-benchmark-romaji-conversion (input iterations)
  "Benchmark romaji conversion with INPUT string for ITERATIONS times.
Return plist with :total-time, :avg-time, :iterations, and :throughput."
  (require 'nskk-converter)
  (garbage-collect) ; GC の影響を最小化
  (let ((start-time (current-time))
        (result nil))
    (dotimes (_ iterations)
      (setq result (nskk-convert-romaji input)))
    (let* ((end-time (current-time))
           (elapsed (float-time (time-subtract end-time start-time)))
           (avg-us (* (/ elapsed iterations) 1000000.0))
           (throughput (/ iterations elapsed)))
      (list :total-time elapsed
            :avg-time avg-us
            :iterations iterations
            :throughput throughput
            :input input
            :result result))))

(defun nskk-benchmark-hiragana-to-katakana (input iterations)
  "Benchmark hiragana-to-katakana conversion with INPUT for ITERATIONS times."
  (require 'nskk-core)
  (garbage-collect)
  (let ((start-time (current-time)))
    (dotimes (_ iterations)
      (ignore (nskk-core-hiragana-to-katakana input)))
    (let* ((end-time (current-time))
           (elapsed (float-time (time-subtract end-time start-time)))
           (avg-us (* (/ elapsed iterations) 1000000.0))
           (throughput (/ iterations elapsed)))
      (list :total-time elapsed
            :avg-time avg-us
            :iterations iterations
            :throughput throughput
            :input input))))

(defun nskk-benchmark-suite ()
  "Run comprehensive benchmark suite and display results."
  (interactive)
  (require 'nskk-converter)

  (let ((test-cases
         '(;; 短い入力
           ("a" "単一母音")
           ("ka" "単一子音+母音")
           ("kya" "拗音")
           ("kka" "促音")
           ("nn" "撥音")
           ;; 中程度の入力
           ("konnnichiha" "こんにちは")
           ("arigatou" "ありがとう")
           ;; 長い入力
           ("kyouhagakkoude benkyoushimasu" "今日は学校で勉強します")))
        (iterations nskk-optimize-benchmark-iterations))

    (with-output-to-temp-buffer "*NSKK Benchmark*"
      (princ (format "NSKK Benchmark Suite (iterations: %d)\n" iterations))
      (princ (make-string 80 ?=))
      (princ "\n\n")

      ;; ローマ字変換ベンチマーク
      (princ "=== Romaji Conversion ===\n")
      (princ (format "%-30s %15s %15s %15s\n"
                     "Input" "Avg Time(μs)" "Throughput" "Result"))
      (princ (make-string 80 ?-))
      (princ "\n")

      (dolist (test test-cases)
        (let* ((input (car test))
               (desc (cadr test))
               (result (nskk-benchmark-romaji-conversion input iterations)))
          (princ (format "%-30s %15.2f %15.0f %15s\n"
                        (format "%s (%s)" input desc)
                        (plist-get result :avg-time)
                        (plist-get result :throughput)
                        "OK"))))

      (princ "\n")

      ;; ひらがな→カタカナ変換ベンチマーク
      (princ "=== Hiragana to Katakana ===\n")
      (princ (format "%-30s %15s %15s\n"
                     "Input" "Avg Time(μs)" "Throughput"))
      (princ (make-string 80 ?-))
      (princ "\n")

      (let ((hiragana-tests '(("あ" "単一文字")
                             ("あいうえお" "五十音")
                             ("こんにちは" "こんにちは")
                             ("ありがとうございます" "ありがとうございます"))))
        (dolist (test hiragana-tests)
          (let* ((input (car test))
                 (desc (cadr test))
                 (result (nskk-benchmark-hiragana-to-katakana input iterations)))
            (princ (format "%-30s %15.2f %15.0f\n"
                          (format "%s (%s)" input desc)
                          (plist-get result :avg-time)
                          (plist-get result :throughput))))))

      (princ "\n")
      (princ (make-string 80 ?=))
      (princ "\n")
      (princ (format "Target: < 100μs per conversion\n"))
      (princ (format "Status: %s\n"
                    (if (< (plist-get (nskk-benchmark-romaji-conversion "ka" 1000)
                                     :avg-time)
                          100.0)
                        "PASS"
                      "FAIL"))))))

;;; メモリプロファイリング

(defun nskk-benchmark-memory-usage ()
  "Measure and return current memory usage as a plist."
  (list :gc-cons-threshold gc-cons-threshold
        :gc-elapsed gc-elapsed
        :gcs-done gcs-done
        :cons-cells-allocated (cl-fifth (memory-use-counts))
        :floats-allocated (cl-sixth (memory-use-counts))
        :strings-allocated (cl-seventh (memory-use-counts))))

(defun nskk-measure-memory-delta (func)
  "Measure memory usage delta before and after calling FUNC."
  (garbage-collect)
  (let ((before (nskk-benchmark-memory-usage)))
    (funcall func)
    (garbage-collect)
    (let* ((after (nskk-benchmark-memory-usage))
           (delta (list :cons-cells
                       (- (plist-get after :cons-cells-allocated)
                          (plist-get before :cons-cells-allocated))
                       :floats
                       (- (plist-get after :floats-allocated)
                          (plist-get before :floats-allocated))
                       :strings
                       (- (plist-get after :strings-allocated)
                          (plist-get before :strings-allocated)))))
      (list :before before :after after :delta delta))))

;;; パフォーマンス回帰テスト

(defvar nskk-optimize--performance-baseline nil
  "Performance baseline data as a plist.")

(defun nskk-optimize-set-baseline ()
  "Record current performance as the baseline."
  (interactive)
  (setq nskk-optimize--performance-baseline
        (nskk-benchmark-romaji-conversion "konnnichiha" 1000))
  (message "Performance baseline set: %.2fμs"
           (plist-get nskk-optimize--performance-baseline :avg-time)))

(defun nskk-optimize-check-regression ()
  "Check for performance regression against the baseline."
  (interactive)
  (unless nskk-optimize--performance-baseline
    (error "No baseline set.  Run `nskk-optimize-set-baseline' first"))

  (let* ((current (nskk-benchmark-romaji-conversion "konnnichiha" 1000))
         (baseline-time (plist-get nskk-optimize--performance-baseline :avg-time))
         (current-time (plist-get current :avg-time))
         (ratio (/ current-time baseline-time))
         (regression (> ratio 1.1)))

    (let ((result (list :baseline baseline-time
                       :current current-time
                       :ratio ratio
                       :regression regression)))
      (when (called-interactively-p 'any)
        (message "Baseline: %.2fμs, Current: %.2fμs, Ratio: %.2f%s"
                baseline-time current-time ratio
                (if regression " [REGRESSION DETECTED]" "")))
      result)))

;;; 統計情報

(defun nskk-optimize-stats ()
  "Return optimization statistics as a plist."
  (list :profiling-enabled nskk-optimize-enable-profiling
        :benchmark-iterations nskk-optimize-benchmark-iterations
        :profile-entries (length nskk-optimize--profile-data)
        :baseline-set (not (null nskk-optimize--performance-baseline))))

(provide 'nskk-optimize)

;;; nskk-optimize.el ends here
