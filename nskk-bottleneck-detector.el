;;; nskk-bottleneck-detector.el --- Bottleneck detector for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, performance
;; Version: 0.1.0
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

;; このファイルはNSKKのボトルネック検出機能を提供します。
;;
;; 主な機能:
;; 1. ホットパス特定（最も頻繁に実行されるコードパス）
;; 2. 遅延分析（実行時間が長い関数の特定）
;; 3. アラート生成（閾値超過時の通知）
;; 4. Transient UIダッシュボード
;;
;; 使用例:
;; (require 'nskk-bottleneck-detector)
;;
;; ;; ボトルネック検出開始
;; (nskk-bottleneck-detect-start)
;;
;; ;; 処理実行
;; (dotimes (_ 1000)
;;   (nskk-convert-romaji "konnnichiha"))
;;
;; ;; ボトルネック検出停止
;; (nskk-bottleneck-detect-stop)
;;
;; ;; ダッシュボード表示
;; (nskk-bottleneck-dashboard)

;;; Code:

(require 'cl-lib)
(require 'nskk-profiler)

;;; カスタマイズ変数

(defgroup nskk-bottleneck-detector nil
  "NSKK bottleneck detector settings."
  :group 'nskk
  :prefix "nskk-bottleneck-")

(defcustom nskk-bottleneck-latency-threshold 0.05
  "遅延アラートの閾値（秒）。
この値を超える実行時間の関数をボトルネックとして報告する。"
  :type 'number
  :group 'nskk-bottleneck-detector)

(defcustom nskk-bottleneck-call-count-threshold 100
  "ホットパスの閾値（呼び出し回数）。
この値を超える呼び出し回数の関数をホットパスとして報告する。"
  :type 'integer
  :group 'nskk-bottleneck-detector)

(defcustom nskk-bottleneck-memory-delta-threshold 1000000
  "メモリ増加の閾値（cons cells）。
この値を超えるメモリ増加をボトルネックとして報告する。"
  :type 'integer
  :group 'nskk-bottleneck-detector)

(defcustom nskk-bottleneck-gc-frequency-threshold 5
  "GC頻度の閾値（回/秒）。
この値を超えるGC頻度をボトルネックとして報告する。"
  :type 'number
  :group 'nskk-bottleneck-detector)

(defcustom nskk-bottleneck-alert-enabled t
  "非nilの場合、ボトルネック検出時にアラートを表示する。"
  :type 'boolean
  :group 'nskk-bottleneck-detector)

;;; 内部変数

(defvar nskk-bottleneck--active nil
  "ボトルネック検出が有効な場合はt。")

(defvar nskk-bottleneck--start-time nil
  "検出開始時刻。")

(defvar nskk-bottleneck--hotpaths nil
  "検出されたホットパスのリスト。
各要素: (function-name call-count total-time avg-time)")

(defvar nskk-bottleneck--slow-functions nil
  "検出された遅い関数のリスト。
各要素: (function-name max-time avg-time call-count)")

(defvar nskk-bottleneck--memory-issues nil
  "検出されたメモリ問題のリスト。
各要素: (issue-type description severity)")

(defvar nskk-bottleneck--gc-issues nil
  "検出されたGC問題のリスト。
各要素: (issue-type description severity)")

(defvar nskk-bottleneck--alerts nil
  "直近のボトルネックアラートを保持するリスト。")

;;; ボトルネック検出制御

;;;###autoload
(defun nskk-bottleneck-detect-start ()
  "ボトルネック検出を開始する。"
  (interactive)
  (when nskk-bottleneck--active
    (error "Bottleneck detector is already running"))

  ;; プロファイラー起動
  (unless nskk-profiler--active
    (nskk-profile-start))

  ;; 初期化
  (setq nskk-bottleneck--active t
        nskk-bottleneck--start-time (current-time)
        nskk-bottleneck--hotpaths nil
        nskk-bottleneck--slow-functions nil
        nskk-bottleneck--memory-issues nil
        nskk-bottleneck--gc-issues nil)

  (message "NSKK bottleneck detector started"))

;;;###autoload
(defun nskk-bottleneck-detect-stop ()
  "ボトルネック検出を停止し、分析を実行する。"
  (interactive)
  (unless nskk-bottleneck--active
    (error "Bottleneck detector is not running"))

  ;; プロファイラー停止
  (when nskk-profiler--active
    (nskk-profile-stop))

  ;; 分析実行
  (nskk-bottleneck--analyze)

  (setq nskk-bottleneck--active nil)

  (message "NSKK bottleneck detector stopped. Found %d issues."
           (+ (length nskk-bottleneck--hotpaths)
              (length nskk-bottleneck--slow-functions)
              (length nskk-bottleneck--memory-issues)
              (length nskk-bottleneck--gc-issues))))

;;; 分析ロジック

(defun nskk-bottleneck--analyze ()
  "プロファイリングデータを分析してボトルネックを検出する。"
  ;; 関数レベルのボトルネック検出
  (nskk-bottleneck--analyze-functions)

  ;; メモリボトルネック検出
  (nskk-bottleneck--analyze-memory)

  ;; GCボトルネック検出
  (nskk-bottleneck--analyze-gc)

  ;; アラート生成
  (when nskk-bottleneck-alert-enabled
    (nskk-bottleneck--generate-alerts)))

(defun nskk-bottleneck--analyze-functions ()
  "関数レベルのボトルネックを分析する。"
  (setq nskk-bottleneck--hotpaths nil
        nskk-bottleneck--slow-functions nil)
  (dolist (entry nskk-profiler--function-stats)
    (let* ((name (car entry))
           (count (nth 1 entry))
           (total (nth 2 entry))
           (min-time (nth 3 entry))
           (max-time (nth 4 entry))
           (avg (/ total count)))
      ;; ホットパス検出（呼び出し回数が多い）
      (when (>= count nskk-bottleneck-call-count-threshold)
        (push (list name
                    :call-count count
                    :total-time total
                    :avg-time avg)
              nskk-bottleneck--hotpaths))
      ;; 遅延関数検出（平均実行時間が長い）
      (when (>= avg nskk-bottleneck-latency-threshold)
        (push (list name
                    :max-time max-time
                    :avg-time avg
                    :call-count count)
              nskk-bottleneck--slow-functions))))
  ;; ソート
  (setq nskk-bottleneck--hotpaths
        (sort nskk-bottleneck--hotpaths
              (lambda (a b) (> (plist-get (cdr a) :call-count)
                               (plist-get (cdr b) :call-count)))))
  (setq nskk-bottleneck--slow-functions
        (sort nskk-bottleneck--slow-functions
              (lambda (a b) (> (plist-get (cdr a) :avg-time)
                               (plist-get (cdr b) :avg-time)))))
  (list :hot-functions nskk-bottleneck--hotpaths
        :slow-functions nskk-bottleneck--slow-functions))

(defun nskk-bottleneck--analyze-memory ()
  "メモリ使用量を分析してボトルネックを検出する。"
  (setq nskk-bottleneck--memory-issues nil)
  (let ((memory-delta 0))
    (when (>= (length nskk-profiler--memory-snapshots) 2)
      (let* ((snapshots (reverse nskk-profiler--memory-snapshots))
             (initial (cdr (car snapshots)))
             (final (cdr (car (last snapshots))))
             (cons-delta (- (or (plist-get final :cons-cells) 0)
                            (or (plist-get initial :cons-cells) 0))))
        (setq memory-delta cons-delta)
        ;; Cons cells増加チェック
        (when (>= cons-delta nskk-bottleneck-memory-delta-threshold)
          (push (list :excessive-cons-allocation
                      :description (format "Excessive cons cell allocation: %d cells" cons-delta)
                      :severity 'high)
                nskk-bottleneck--memory-issues))
        ;; その他のメモリ問題チェック
        (dolist (key '(:floats :vectors :strings))
          (let* ((delta (- (or (plist-get final key) 0)
                             (or (plist-get initial key) 0)))
                 (threshold (* nskk-bottleneck-memory-delta-threshold 0.1)))
            (when (>= delta threshold)
              (push (list (intern (format "excessive-%s-allocation" (substring (symbol-name key) 1)))
                          :description (format "High %s allocation: %d"
                                               (substring (symbol-name key) 1)
                                               delta)
                          :severity 'medium)
                    nskk-bottleneck--memory-issues))))))
    (list :memory-delta memory-delta
          :issues nskk-bottleneck--memory-issues)))

(defun nskk-bottleneck--analyze-gc ()
  "GCパフォーマンスを分析してボトルネックを検出する。"
  (setq nskk-bottleneck--gc-issues nil)
  (let ((gc-count 0)
        (gc-frequency 0.0))
    (when (> (length nskk-profiler--gc-events) 0)
      (let* ((elapsed (if nskk-bottleneck--start-time
                          (float-time (time-subtract (current-time)
                                                     nskk-bottleneck--start-time))
                        1.0))
             (events (reverse nskk-profiler--gc-events)))
        (setq gc-count (length events))
        (setq gc-frequency (/ gc-count elapsed))
        ;; GC頻度チェック
        (when (>= gc-frequency nskk-bottleneck-gc-frequency-threshold)
          (push (list :high-gc-frequency
                      :description (format "High GC frequency: %.2f GCs/sec" gc-frequency)
                      :severity 'high)
                nskk-bottleneck--gc-issues))
        ;; GC時間チェック
        (when (> gc-count 0)
          (let* ((first-gc (plist-get (cdr (car events)) :gc-elapsed))
                 (last-gc (plist-get (cdr (car (last events))) :gc-elapsed))
                 (gc-time-delta (- last-gc first-gc))
                 (gc-ratio (/ gc-time-delta elapsed)))
            (when (>= gc-ratio 0.1)
              (push (list :excessive-gc-time
                          :description (format "Excessive GC time: %.2f%% of total"
                                               (* gc-ratio 100))
                          :severity 'high)
                    nskk-bottleneck--gc-issues))))))
    (list :gc-count gc-count
          :gc-frequency gc-frequency
          :issues nskk-bottleneck--gc-issues)))

(defun nskk-bottleneck--generate-alerts ()
  "検出されたボトルネックに対してアラートを生成する。"
  (let ((alerts nil)
        (total-issues (+ (length nskk-bottleneck--hotpaths)
                         (length nskk-bottleneck--slow-functions)
                         (length nskk-bottleneck--memory-issues)
                         (length nskk-bottleneck--gc-issues))))
    (setq nskk-bottleneck--alerts nil)
    (when (> total-issues 0)
      (message "⚠ NSKK Bottleneck Alert: %d issue(s) detected" total-issues)
      ;; メモリアラート
      (dolist (issue nskk-bottleneck--memory-issues)
        (let ((severity (plist-get (cdr issue) :severity))
              (desc (plist-get (cdr issue) :description)))
          (when (eq severity 'high)
            (message "  • Memory: %s" desc)
            (push (list :type 'memory :severity severity :description desc)
                  alerts))))
      ;; GCアラート
      (dolist (issue nskk-bottleneck--gc-issues)
        (let ((severity (plist-get (cdr issue) :severity))
              (desc (plist-get (cdr issue) :description)))
          (when (eq severity 'high)
            (message "  • GC: %s" desc)
            (push (list :type 'gc :severity severity :description desc)
                  alerts))))
      ;; 遅い関数アラート
      (when nskk-bottleneck--slow-functions
        (let* ((slowest (car nskk-bottleneck--slow-functions))
               (fn (car slowest))
               (avg (plist-get (cdr slowest) :avg-time))
               (max (plist-get (cdr slowest) :max-time)))
          (message "  • Slowest function: %s (%.3fs avg)" fn avg)
          (push (list :type 'latency :function fn :avg-time avg :max-time max)
                alerts))))
    (let ((result (nreverse alerts)))
      (setq nskk-bottleneck--alerts result)
      result)))

(defun nskk-bottleneck-report ()
  "ボトルネック検出レポートを表示する。"
  (interactive)
  (with-output-to-temp-buffer "*NSKK Bottleneck Report*"
    (princ "=======================================================\n")
    (princ "         NSKK Bottleneck Detection Report\n")
    (princ "=======================================================\n\n")

    ;; サマリー
    (princ (format "Total Issues Found: %d\n"
                   (+ (length nskk-bottleneck--hotpaths)
                      (length nskk-bottleneck--slow-functions)
                      (length nskk-bottleneck--memory-issues)
                      (length nskk-bottleneck--gc-issues))))
    (princ "\n")

    ;; ホットパス
    (when nskk-bottleneck--hotpaths
      (princ "--- Hot Paths (Frequently Called) ---\n\n")
      (princ (format "%-40s %12s %15s %15s\n"
                     "Function" "Calls" "Total(s)" "Avg(s)"))
      (princ (make-string 82 ?-))
      (princ "\n")
      (dolist (hotpath (cl-subseq nskk-bottleneck--hotpaths 0
                                  (min 10 (length nskk-bottleneck--hotpaths))))
        (let ((name (car hotpath))
              (info (cdr hotpath)))
          (princ (format "%-40s %12d %15.6f %15.6f\n"
                        name
                        (plist-get info :call-count)
                        (plist-get info :total-time)
                        (plist-get info :avg-time)))))
      (princ "\n"))

    ;; 遅い関数
    (when nskk-bottleneck--slow-functions
      (princ "--- Slow Functions (High Latency) ---\n\n")
      (princ (format "%-40s %15s %15s %12s\n"
                     "Function" "Max(s)" "Avg(s)" "Calls"))
      (princ (make-string 82 ?-))
      (princ "\n")
      (dolist (slow (cl-subseq nskk-bottleneck--slow-functions 0
                              (min 10 (length nskk-bottleneck--slow-functions))))
        (let ((name (car slow))
              (info (cdr slow)))
          (princ (format "%-40s %15.6f %15.6f %12d\n"
                        name
                        (plist-get info :max-time)
                        (plist-get info :avg-time)
                        (plist-get info :call-count)))))
      (princ "\n"))

    ;; メモリ問題
    (when nskk-bottleneck--memory-issues
      (princ "--- Memory Issues ---\n\n")
      (dolist (issue nskk-bottleneck--memory-issues)
        (let ((severity (plist-get (cdr issue) :severity))
              (desc (plist-get (cdr issue) :description)))
          (princ (format "[%s] %s\n"
                        (upcase (symbol-name severity))
                        desc))))
      (princ "\n"))

    ;; GC問題
    (when nskk-bottleneck--gc-issues
      (princ "--- Garbage Collection Issues ---\n\n")
      (dolist (issue nskk-bottleneck--gc-issues)
        (let ((severity (plist-get (cdr issue) :severity))
              (desc (plist-get (cdr issue) :description)))
          (princ (format "[%s] %s\n"
                        (upcase (symbol-name severity))
                        desc))))
      (princ "\n"))

    ;; 推奨事項
    (princ "--- Recommendations ---\n\n")
    (let ((recs (nskk-bottleneck--generate-recommendations)))
      (if recs
          (dolist (rec recs)
            (princ (format "%s\n" rec)))
        (princ "No specific recommendations. Performance looks good!\n")))

    (princ "\n=======================================================\n")))

(defun nskk-bottleneck--generate-recommendations ()
  "ボトルネックに対する推奨事項を生成する。"
  (let ((recommendations nil))
    ;; ホットパス最適化
    (when nskk-bottleneck--hotpaths
      (push "• Optimize hot paths: Consider caching or memoization for frequently called functions"
            recommendations))
    ;; 遅延関数最適化
    (when nskk-bottleneck--slow-functions
      (push "• Reduce latency: Profile slow functions with `profiler-report' for detailed analysis"
            recommendations))
    ;; メモリ最適化
    (when nskk-bottleneck--memory-issues
      (push "• Reduce memory allocation: Consider using `cl-loop' instead of `mapcar' for large datasets"
            recommendations)
      (push "• Increase gc-cons-threshold temporarily during heavy operations"
            recommendations))
    ;; GC最適化
    (when nskk-bottleneck--gc-issues
      (push "• Reduce GC pressure: Minimize temporary object creation"
            recommendations)
      (push "• Consider using object pooling for frequently allocated objects"
            recommendations))
    (nreverse recommendations)))

(defun nskk-bottleneck-dashboard ()
  "ボトルネック検出のダッシュボードを表示する。
Transient UI版は別途nskk-bottleneck-dashboard.elで提供。"
  (interactive)
  (let ((status (nskk-bottleneck-status)))
    (message "NSKK Bottleneck Dashboard | Status: %s | Issues: %d (Hotpaths: %d, Slow: %d, Memory: %d, GC: %d) | Commands: (s)tart, (x)stop, (r)eport, (R)eset"
             (if (plist-get status :active) "RUNNING" "STOPPED")
             (plist-get status :total-issues)
             (plist-get status :hotpaths)
             (plist-get status :slow-functions)
             (plist-get status :memory-issues)
             (plist-get status :gc-issues))))

;;; ユーティリティ

(defun nskk-bottleneck-reset ()
  "ボトルネック検出器の状態をリセットする。"
  (interactive)
  (when nskk-bottleneck--active
    (nskk-bottleneck-detect-stop))

  (setq nskk-bottleneck--start-time nil
        nskk-bottleneck--hotpaths nil
        nskk-bottleneck--slow-functions nil
        nskk-bottleneck--memory-issues nil
        nskk-bottleneck--gc-issues nil
        nskk-bottleneck--alerts nil)

  ;; プロファイラーもリセット
  (nskk-profiler-reset)

  (message "NSKK bottleneck detector reset"))

(defun nskk-bottleneck-status ()
  "ボトルネック検出器の状態を返す。

返り値: plist
  :active          - 検出中かどうか
  :hotpaths        - ホットパス数
  :slow-functions  - 遅い関数数
  :memory-issues   - メモリ問題数
  :gc-issues       - GC問題数
  :total-issues    - 総問題数"
  (list :active nskk-bottleneck--active
        :hotpaths (length nskk-bottleneck--hotpaths)
        :slow-functions (length nskk-bottleneck--slow-functions)
        :memory-issues (length nskk-bottleneck--memory-issues)
        :gc-issues (length nskk-bottleneck--gc-issues)
        :total-issues (+ (length nskk-bottleneck--hotpaths)
                        (length nskk-bottleneck--slow-functions)
                        (length nskk-bottleneck--memory-issues)
                        (length nskk-bottleneck--gc-issues))))

(provide 'nskk-bottleneck-detector)

;;; nskk-bottleneck-detector.el ends here
