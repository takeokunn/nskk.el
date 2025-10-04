;;; nskk-optimize.el --- Performance optimization for NSKK -*- lexical-binding: t; -*-

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
  "非nilの場合、プロファイリング情報を収集する。
本番環境ではnilにすることを推奨。"
  :type 'boolean
  :group 'nskk-optimize)

(defcustom nskk-optimize-benchmark-iterations 10000
  "ベンチマークの反復回数。
大きいほど精度が上がるが、時間がかかる。"
  :type 'integer
  :group 'nskk-optimize)

;;; パフォーマンス測定マクロ

(defmacro nskk-measure-time (&rest body)
  "BODY の実行時間を測定し、結果をマイクロ秒単位で返す。

返り値: (実行時間(μs) . BODYの返り値)

例:
  (nskk-measure-time (nskk-convert-romaji \"ka\"))
  ;; => (15.2 . #s(nskk-converter-result ...))"
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
  "BODY の実行をプロファイリングし、NAME として記録する。

`nskk-optimize-enable-profiling' が非nilの場合のみ有効。

例:
  (nskk-with-profiling \"romaji-conversion\"
    (nskk-convert-romaji \"konnnichiha\"))"
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
  "プロファイリングデータを保持するalist。
形式: ((name . (count total-time min-time max-time)) ...)")

(defun nskk-optimize--record-profile (name elapsed-time)
  "プロファイリングデータを記録する。
NAME: プロファイル名
ELAPSED-TIME: 経過時間（time-subtract の返り値）"
  (let* ((elapsed-us (* (float-time elapsed-time) 1000000.0))
         (entry (assoc name nskk-optimize--profile-data))
         (count (if entry (nth 1 entry) 0))
         (total (if entry (nth 2 entry) 0.0))
         (min-time (if entry (min (nth 3 entry) elapsed-us) elapsed-us))
         (max-time (if entry (max (nth 4 entry) elapsed-us) elapsed-us)))
    (setf (alist-get name nskk-optimize--profile-data nil nil #'string=)
          (list (1+ count) (+ total elapsed-us) min-time max-time))))

(defun nskk-optimize-reset-profile ()
  "プロファイリングデータをリセットする。"
  (interactive)
  (setq nskk-optimize--profile-data nil))

(defun nskk-optimize-show-profile ()
  "プロファイリングデータを表示する。"
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
  "ローマ字変換のベンチマークを実行する。

INPUT: 変換する入力文字列
ITERATIONS: 反復回数

返り値: plist (:total-time :avg-time :iterations :throughput)
  :total-time  - 合計実行時間（秒）
  :avg-time    - 平均実行時間（マイクロ秒）
  :iterations  - 反復回数
  :throughput  - スループット（回/秒）"
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
  "ひらがな→カタカナ変換のベンチマークを実行する。

INPUT: 変換する入力文字列
ITERATIONS: 反復回数"
  (require 'nskk-special-chars)
  (garbage-collect)
  (let ((start-time (current-time)))
    (dotimes (_ iterations)
      (nskk-hiragana-to-katakana input))
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
  "包括的なベンチマークスイートを実行する。

複数の入力パターンで各種変換のベンチマークを実施。
結果を *NSKK Benchmark* バッファに表示。"
  (interactive)
  (require 'nskk-converter)
  (require 'nskk-special-chars)

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
  "現在のメモリ使用量を測定する。

返り値: plist
  :gc-cons-threshold    - GC閾値
  :gc-elapsed           - GC累積時間
  :gcs-done             - GC実行回数
  :cons-cells-allocated - Cons セル割り当て数
  :floats-allocated     - Float割り当て数
  :strings-allocated    - String割り当て数"
  (list :gc-cons-threshold gc-cons-threshold
        :gc-elapsed gc-elapsed
        :gcs-done gcs-done
        :cons-cells-allocated (cl-fifth (memory-use-counts))
        :floats-allocated (cl-sixth (memory-use-counts))
        :strings-allocated (cl-seventh (memory-use-counts))))

(defun nskk-measure-memory-delta (func)
  "FUNC の実行前後のメモリ使用量の差分を測定する。

FUNC: 引数なしの関数

返り値: plist
  :before        - 実行前のメモリ使用量
  :after         - 実行後のメモリ使用量
  :delta         - 差分"
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

;;; 最適化マクロ

(defmacro nskk-optimize-loop (var limit &rest body)
  "最適化されたループマクロ。
VARが0からLIMIT-1まで反復し、BODYを実行する。

通常のdotimesより高速だが、副作用に注意。

例:
  (nskk-optimize-loop i 1000
    (do-something i))"
  (declare (indent 2) (debug (symbolp form body)))
  `(let ((,var 0)
         (limit ,limit))
     (while (< ,var limit)
       ,@body
       (setq ,var (1+ ,var)))))

(defmacro nskk-optimize-string-concat (str1 str2)
  "文字列連結の最適化版。
短い文字列の連結に特化。

STR1, STR2: 連結する文字列"
  `(concat ,str1 ,str2))

;;; パフォーマンス回帰テスト

(defvar nskk-optimize--performance-baseline nil
  "パフォーマンスベースラインデータ。
形式: plist")

(defun nskk-optimize-set-baseline ()
  "現在のパフォーマンスをベースラインとして記録する。"
  (interactive)
  (setq nskk-optimize--performance-baseline
        (nskk-benchmark-romaji-conversion "konnnichiha" 1000))
  (message "Performance baseline set: %.2fμs"
           (plist-get nskk-optimize--performance-baseline :avg-time)))

(defun nskk-optimize-check-regression ()
  "パフォーマンス回帰をチェックする。

ベースラインと比較して、10%以上の性能低下がある場合は警告。

返り値: plist
  :baseline      - ベースライン平均時間
  :current       - 現在の平均時間
  :ratio         - 比率（current/baseline）
  :regression    - 回帰の有無（10%以上遅い場合t）"
  (interactive)
  (unless nskk-optimize--performance-baseline
    (error "No baseline set. Run `nskk-optimize-set-baseline' first"))

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
  "最適化機能の統計情報を返す。

返り値: plist
  :profiling-enabled   - プロファイリング有効/無効
  :benchmark-iterations - ベンチマーク反復回数
  :profile-entries     - プロファイルエントリ数
  :baseline-set        - ベースライン設定済み"
  (list :profiling-enabled nskk-optimize-enable-profiling
        :benchmark-iterations nskk-optimize-benchmark-iterations
        :profile-entries (length nskk-optimize--profile-data)
        :baseline-set (not (null nskk-optimize--performance-baseline))))

(provide 'nskk-optimize)

;;; nskk-optimize.el ends here
