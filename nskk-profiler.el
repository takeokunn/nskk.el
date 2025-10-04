;;; nskk-profiler.el --- Performance profiler for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, profiling
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

;; このファイルはNSKKのパフォーマンスプロファイラーを提供します。
;;
;; 主な機能:
;; 1. リアルタイム監視（CPU、メモリ、GC）
;; 2. 関数レベルプロファイリング（profiler.el統合）
;; 3. メモリプロファイリング（ヒープ、スタック）
;; 4. スレッドプロファイリング（並列処理監視）
;;
;; 使用例:
;; (require 'nskk-profiler)
;;
;; ;; プロファイリング開始
;; (nskk-profile-start)
;;
;; ;; 処理実行
;; (nskk-convert-romaji "konnnichiha")
;;
;; ;; プロファイリング停止
;; (nskk-profile-stop)
;;
;; ;; レポート表示
;; (nskk-profile-report)

;;; Code:

(require 'cl-lib)
(require 'profiler)

;;; カスタマイズ変数

(defgroup nskk-profiler nil
  "NSKK performance profiler settings."
  :group 'nskk
  :prefix "nskk-profiler-")

(defcustom nskk-profiler-sampling-interval 1000000
  "プロファイリングのサンプリング間隔（ナノ秒）。
デフォルトは1ms（1,000,000ナノ秒）。"
  :type 'integer
  :group 'nskk-profiler)

(defcustom nskk-profiler-memory-tracking t
  "非nilの場合、メモリ使用量を追跡する。"
  :type 'boolean
  :group 'nskk-profiler)

(defcustom nskk-profiler-gc-tracking t
  "非nilの場合、GCイベントを追跡する。"
  :type 'boolean
  :group 'nskk-profiler)

(defcustom nskk-profiler-auto-report t
  "非nilの場合、プロファイリング停止時に自動でレポートを表示する。"
  :type 'boolean
  :group 'nskk-profiler)

;;; 内部変数

(defvar nskk-profiler--active nil
  "プロファイリングが有効な場合はt。")

(defvar nskk-profiler--start-time nil
  "プロファイリング開始時刻。")

(defvar nskk-profiler--memory-snapshots nil
  "メモリスナップショットのリスト。
各要素: (timestamp cons-cells floats strings vectors)")

(defvar nskk-profiler--gc-events nil
  "GCイベントのリスト。
各要素: (timestamp gc-elapsed gcs-done)")

(defvar nskk-profiler--function-stats nil
  "関数ごとの統計情報。
形式: ((function-name . (call-count total-time min-time max-time)) ...)")

(defvar nskk-profiler--gc-hook-added nil
  "GCフックが追加されているかのフラグ。")

;;; プロファイリング制御

;;;###autoload
(defun nskk-profile-start ()
  "プロファイリングを開始する。"
  (interactive)
  (when nskk-profiler--active
    (error "Profiler is already running. Call `nskk-profile-stop' first"))

  ;; 初期化
  (setq nskk-profiler--active t
        nskk-profiler--start-time (current-time)
        nskk-profiler--memory-snapshots nil
        nskk-profiler--gc-events nil
        nskk-profiler--function-stats nil)

  ;; GCフック追加
  (when (and nskk-profiler-gc-tracking
             (not nskk-profiler--gc-hook-added))
    (add-hook 'post-gc-hook #'nskk-profiler--record-gc-event)
    (setq nskk-profiler--gc-hook-added t))

  ;; 初期メモリスナップショット
  (when nskk-profiler-memory-tracking
    (nskk-profiler--take-memory-snapshot))

  ;; Emacsビルトインプロファイラ起動
  (profiler-start 'cpu+mem)

  (message "NSKK profiler started"))

;;;###autoload
(defun nskk-profile-stop ()
  "プロファイリングを停止する。"
  (interactive)
  (unless nskk-profiler--active
    (error "Profiler is not running. Call `nskk-profile-start' first"))

  ;; ビルトインプロファイラ停止
  (profiler-stop)

  ;; 最終メモリスナップショット
  (when nskk-profiler-memory-tracking
    (nskk-profiler--take-memory-snapshot))

  ;; GCフック削除
  (when nskk-profiler--gc-hook-added
    (remove-hook 'post-gc-hook #'nskk-profiler--record-gc-event)
    (setq nskk-profiler--gc-hook-added nil))

  (setq nskk-profiler--active nil)

  (message "NSKK profiler stopped")

  ;; 自動レポート表示
  (when nskk-profiler-auto-report
    (nskk-profile-report)))

;;;###autoload
(defun nskk-profile-report ()
  "プロファイリングレポートを表示する。"
  (interactive)
  (unless nskk-profiler--start-time
    (error "No profiling data available. Run `nskk-profile-start' first"))

  (let ((elapsed (if nskk-profiler--active
                     (float-time (time-subtract (current-time)
                                               nskk-profiler--start-time))
                   (if (and nskk-profiler--memory-snapshots
                           (car (last nskk-profiler--memory-snapshots)))
                       (float-time (time-subtract (car (car (last nskk-profiler--memory-snapshots)))
                                                 nskk-profiler--start-time))
                     0.0))))
    (with-output-to-temp-buffer "*NSKK Profiler Report*"
      ;; ヘッダー
      (princ "=======================================================\n")
      (princ "         NSKK Performance Profiler Report\n")
      (princ "=======================================================\n\n")

      (princ (format "Profiling Duration: %.3f seconds\n" elapsed))
      (princ (format "Status: %s\n\n" (if nskk-profiler--active "RUNNING" "STOPPED")))

      ;; メモリ統計
      (when nskk-profiler-memory-tracking
        (nskk-profiler--report-memory-stats))

      ;; GC統計
      (when nskk-profiler-gc-tracking
        (nskk-profiler--report-gc-stats))

      ;; 関数統計
      (when nskk-profiler--function-stats
        (nskk-profiler--report-function-stats))

      ;; フッター
      (princ "\n=======================================================\n")
      (princ "Use `profiler-report' for detailed CPU/Memory report\n")
      (princ "=======================================================\n"))))

;;; メモリプロファイリング

(defun nskk-profiler--take-memory-snapshot ()
  "現在のメモリ使用量のスナップショットを取得する。"
  (let* ((timestamp (current-time))
         (mem-counts (memory-use-counts))
         (cons-cells (nth 0 mem-counts))
         (floats (nth 1 mem-counts))
         (vectors (nth 2 mem-counts))
         (symbols (nth 3 mem-counts))
         (strings (nth 4 mem-counts))
         (intervals (nth 5 mem-counts))
         (buffers (nth 6 mem-counts)))
    (push (list timestamp
                :cons-cells cons-cells
                :floats floats
                :vectors vectors
                :symbols symbols
                :strings strings
                :intervals intervals
                :buffers buffers)
          nskk-profiler--memory-snapshots)))

(defun nskk-profiler--report-memory-stats ()
  "メモリ統計をレポートバッファに出力する。"
  (princ "--- Memory Statistics ---\n\n")

  (when (>= (length nskk-profiler--memory-snapshots) 2)
    (let* ((snapshots (reverse nskk-profiler--memory-snapshots))
           (initial (cdr (car snapshots)))
           (final (cdr (car (last snapshots)))))

      (princ (format "%-20s %15s %15s %15s\n"
                     "Resource" "Initial" "Final" "Delta"))
      (princ (make-string 65 ?-))
      (princ "\n")

      ;; 各リソースの変化を表示
      (dolist (key '(:cons-cells :floats :vectors :symbols :strings :intervals :buffers))
        (let* ((init-val (plist-get initial key))
               (final-val (plist-get final key))
               (delta (- final-val init-val)))
          (princ (format "%-20s %15d %15d %15d\n"
                        (substring (symbol-name key) 1)
                        init-val
                        final-val
                        delta))))

      (princ "\n"))))

;;; GCプロファイリング

(defun nskk-profiler--record-gc-event ()
  "GCイベントを記録する（post-gc-hookから呼ばれる）。"
  (when nskk-profiler--active
    (push (list (current-time)
                :gc-elapsed gc-elapsed
                :gcs-done gcs-done)
          nskk-profiler--gc-events)))

(defun nskk-profiler--report-gc-stats ()
  "GC統計をレポートバッファに出力する。"
  (princ "--- Garbage Collection Statistics ---\n\n")

  (let ((gc-count (length nskk-profiler--gc-events)))
    (princ (format "Total GC count: %d\n" gc-count))
    (princ (format "Current GC elapsed time: %.3f seconds\n" gc-elapsed))
    (princ (format "Total GCs done: %d\n" gcs-done))

    (when (> gc-count 0)
      (let* ((events (reverse nskk-profiler--gc-events))
             (first-gc (plist-get (cadr (car events)) :gc-elapsed))
             (last-gc (plist-get (cadr (car (last events))) :gc-elapsed))
             (gc-time-delta (- last-gc first-gc)))
        (princ (format "GC time during profiling: %.3f seconds\n" gc-time-delta))))

    (princ "\n")))

;;; 関数プロファイリング

(defmacro nskk-profile-function (name &rest body)
  "関数の実行時間をプロファイリングする。

NAME: 関数名（シンボルまたは文字列）
BODY: 実行するコード

返り値: BODYの返り値

例:
  (nskk-profile-function \"romaji-conversion\"
    (nskk-convert-romaji \"konnnichiha\"))"
  (declare (indent 1) (debug t))
  (let ((func-name (if (stringp name) name (symbol-name name)))
        (start-time (make-symbol "start-time"))
        (end-time (make-symbol "end-time"))
        (result (make-symbol "result"))
        (elapsed (make-symbol "elapsed")))
    `(if nskk-profiler--active
         (let* ((,start-time (current-time))
                (,result (progn ,@body))
                (,end-time (current-time))
                (,elapsed (float-time (time-subtract ,end-time ,start-time))))
           (nskk-profiler--record-function-stats ,func-name ,elapsed)
           ,result)
       (progn ,@body))))

(defun nskk-profiler--record-function-stats (func-name elapsed-time)
  "関数統計を記録する。

FUNC-NAME: 関数名
ELAPSED-TIME: 経過時間（秒）"
  (let* ((entry (assoc func-name nskk-profiler--function-stats))
         (count (if entry (nth 1 entry) 0))
         (total (if entry (nth 2 entry) 0.0))
         (min-time (if entry (min (nth 3 entry) elapsed-time) elapsed-time))
         (max-time (if entry (max (nth 4 entry) elapsed-time) elapsed-time)))
    (setf (alist-get func-name nskk-profiler--function-stats nil nil #'string=)
          (list (1+ count) (+ total elapsed-time) min-time max-time))))

(defun nskk-profiler--report-function-stats ()
  "関数統計をレポートバッファに出力する。"
  (princ "--- Function Statistics ---\n\n")
  (princ (format "%-30s %10s %15s %15s %15s %15s\n"
                 "Function" "Calls" "Total(s)" "Avg(s)" "Min(s)" "Max(s)"))
  (princ (make-string 100 ?-))
  (princ "\n")

  (dolist (entry (sort (copy-sequence nskk-profiler--function-stats)
                      (lambda (a b) (> (nth 2 (cdr a)) (nth 2 (cdr b))))))
    (let* ((name (car entry))
           (count (nth 1 entry))
           (total (nth 2 entry))
           (min-time (nth 3 entry))
           (max-time (nth 4 entry))
           (avg (/ total count)))
      (princ (format "%-30s %10d %15.6f %15.6f %15.6f %15.6f\n"
                    name count total avg min-time max-time))))
  (princ "\n"))

;;; スレッドプロファイリング

(defun nskk-profiler-thread-info ()
  "現在のスレッド情報を取得する。

返り値: plist
  :main-thread     - メインスレッドかどうか
  :thread-count    - スレッド数（Emacs 31以降）
  :current-thread  - 現在のスレッド名"
  (let ((current (current-thread)))
    (list :main-thread (eq current main-thread)
          :thread-count (if (fboundp 'all-threads)
                           (length (all-threads))
                         1)
          :current-thread (cond
                          ((not current) nil)
                          ((fboundp 'thread-name) (thread-name current))
                          (t "main")))))

;;; ユーティリティ

(defun nskk-profiler-reset ()
  "プロファイラーの状態をリセットする。"
  (interactive)
  (when nskk-profiler--active
    (nskk-profile-stop))

  (setq nskk-profiler--start-time nil
        nskk-profiler--memory-snapshots nil
        nskk-profiler--gc-events nil
        nskk-profiler--function-stats nil)

  (message "NSKK profiler reset"))

(defun nskk-profiler-status ()
  "プロファイラーの状態を返す。

返り値: plist
  :active          - プロファイリング中かどうか
  :start-time      - 開始時刻
  :elapsed         - 経過時間
  :memory-snapshots - メモリスナップショット数
  :gc-events       - GCイベント数
  :function-stats  - 関数統計エントリ数"
  (list :active nskk-profiler--active
        :start-time nskk-profiler--start-time
        :elapsed (if nskk-profiler--start-time
                    (float-time (time-subtract (current-time)
                                              nskk-profiler--start-time))
                  0.0)
        :memory-snapshots (length nskk-profiler--memory-snapshots)
        :gc-events (length nskk-profiler--gc-events)
        :function-stats (length nskk-profiler--function-stats)))

;;; リアルタイム監視

(defvar nskk-profiler--monitor-timer nil
  "リアルタイム監視用タイマー。")

(defcustom nskk-profiler-monitor-interval 1.0
  "リアルタイム監視の更新間隔（秒）。"
  :type 'number
  :group 'nskk-profiler)

(defun nskk-profiler-start-monitor ()
  "リアルタイム監視を開始する。"
  (interactive)
  (when nskk-profiler--monitor-timer
    (cancel-timer nskk-profiler--monitor-timer))

  (setq nskk-profiler--monitor-timer
        (run-with-timer 0 nskk-profiler-monitor-interval
                       #'nskk-profiler--monitor-tick))

  (message "NSKK profiler monitor started"))

(defun nskk-profiler-stop-monitor ()
  "リアルタイム監視を停止する。"
  (interactive)
  (when nskk-profiler--monitor-timer
    (cancel-timer nskk-profiler--monitor-timer)
    (setq nskk-profiler--monitor-timer nil)
    (message "NSKK profiler monitor stopped")))

(defun nskk-profiler--monitor-tick ()
  "監視タイマーのティック処理。"
  (when nskk-profiler--active
    ;; メモリスナップショット取得
    (when nskk-profiler-memory-tracking
      (nskk-profiler--take-memory-snapshot))

    ;; 必要に応じて追加の監視処理
    ))

(provide 'nskk-profiler)

;;; nskk-profiler.el ends here
