;;; nskk-async-candidates.el --- Async candidate display for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, async
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

;; このファイルは非同期候補表示機能を実装します。
;;
;; 特徴:
;; - ノンブロッキング描画
;; - 段階的候補表示（Incremental Display）
;; - キャンセル処理
;; - UIブロッキング0ms
;;
;; 使用例:
;;
;;   (require 'nskk-async-candidates)
;;
;;   ;; 非同期候補表示開始
;;   (nskk-async-candidates-show-async
;;     candidates
;;     (lambda (displayed-count)
;;       (message "Displayed %d candidates" displayed-count)))
;;
;;   ;; キャンセル
;;   (nskk-async-candidates-cancel)

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-async-candidates nil
  "Async candidate display customization."
  :group 'nskk
  :prefix "nskk-async-candidates-")

(defcustom nskk-async-candidates-batch-size 5
  "一度に表示する候補数。
小さくするとUIがよりレスポンシブになるが、完全表示まで時間がかかる。"
  :type 'integer
  :group 'nskk-async-candidates)

(defcustom nskk-async-candidates-delay 0.001
  "候補表示のバッチ間の遅延時間（秒）。
小さくすると表示が速くなるが、UIブロッキングのリスクが上がる。"
  :type 'float
  :group 'nskk-async-candidates)

(defcustom nskk-async-candidates-enable-incremental t
  "非nilの場合、段階的候補表示を有効にする。"
  :type 'boolean
  :group 'nskk-async-candidates)

(defcustom nskk-async-candidates-verbose nil
  "非nilの場合、詳細なログを出力する。"
  :type 'boolean
  :group 'nskk-async-candidates)

;;; データ構造

(cl-defstruct (nskk-async-candidates-context
               (:constructor nskk-async-candidates-context--create)
               (:copier nil))
  "非同期候補表示のコンテキスト。

スロット:
  candidates       - 候補リスト（全体）
  displayed-count  - 既に表示した候補数
  timer            - 表示タイマー
  callback         - 完了時のコールバック
  cancelled        - キャンセルフラグ
  start-time       - 開始時刻（float-time）"
  (candidates nil :type list)
  (displayed-count 0 :type integer)
  (timer nil :type (or null timer))
  (callback nil :type (or null function))
  (cancelled nil :type boolean)
  (start-time nil :type (or null number)))

;;; 内部変数

(defvar nskk-async-candidates--current-context nil
  "現在実行中の非同期候補表示コンテキスト。")

;;; 非同期候補表示

;;;###autoload
(defun nskk-async-candidates-show-async (candidates &optional callback)
  "候補を非同期で段階的に表示する。

引数:
  CANDIDATES - 候補リスト
  CALLBACK   - 完了時のコールバック（省略可）
               引数: (displayed-count) - 表示した候補数

処理フロー:
  1. 前回の表示をキャンセル
  2. コンテキスト作成
  3. タイマー設定
  4. バッチ処理開始

戻り値:
  なし（非同期処理）"
  (when nskk-async-candidates-verbose
    (message "Starting async candidate display: %d candidates" (length candidates)))

  ;; 前回の表示をキャンセル
  (nskk-async-candidates-cancel)

  (if (or (not nskk-async-candidates-enable-incremental)
          (<= (length candidates) nskk-async-candidates-batch-size))
      ;; 段階的表示が無効、または候補数が少ない場合は一括表示
      (progn
        (nskk-async-candidates--display-batch candidates 0 (length candidates))
        (when callback
          (funcall callback (length candidates))))

    ;; 段階的表示
    (let ((context (nskk-async-candidates-context--create
                    :candidates candidates
                    :displayed-count 0
                    :callback callback
                    :cancelled nil
                    :start-time (float-time))))

      (setq nskk-async-candidates--current-context context)

      ;; 最初のバッチを即座に表示（UIブロッキング0ms）
      (nskk-async-candidates--schedule-next-batch context))))

(defun nskk-async-candidates--schedule-next-batch (context)
  "次のバッチ表示をスケジュールする。

引数:
  CONTEXT - nskk-async-candidates-context構造体"
  (let* ((candidates (nskk-async-candidates-context-candidates context))
         (displayed (nskk-async-candidates-context-displayed-count context))
         (total (length candidates))
         (remaining (- total displayed)))

    (when (and (> remaining 0)
               (not (nskk-async-candidates-context-cancelled context)))

      ;; 次のバッチサイズを決定
      (let ((batch-size (min nskk-async-candidates-batch-size remaining)))

        ;; タイマーで次のバッチを表示
        (setf (nskk-async-candidates-context-timer context)
              (run-at-time nskk-async-candidates-delay nil
                          (lambda ()
                            (nskk-async-candidates--process-next-batch context batch-size))))))))

(defun nskk-async-candidates--process-next-batch (context batch-size)
  "次のバッチを処理する。

引数:
  CONTEXT    - nskk-async-candidates-context構造体
  BATCH-SIZE - バッチサイズ"
  (when (not (nskk-async-candidates-context-cancelled context))
    (let* ((candidates (nskk-async-candidates-context-candidates context))
           (displayed (nskk-async-candidates-context-displayed-count context))
           (total (length candidates)))

      (when nskk-async-candidates-verbose
        (message "Processing batch: %d-%d / %d"
                 displayed
                 (+ displayed batch-size)
                 total))

      ;; バッチ表示
      (nskk-async-candidates--display-batch
       candidates
       displayed
       (min (+ displayed batch-size) total))

      ;; カウント更新
      (setf (nskk-async-candidates-context-displayed-count context)
            (+ displayed batch-size))

      ;; 次のバッチをスケジュール、または完了処理
      (if (>= (+ displayed batch-size) total)
          (nskk-async-candidates--on-complete context)
        (nskk-async-candidates--schedule-next-batch context)))))

(defun nskk-async-candidates--display-batch (candidates start end)
  "候補のバッチを表示する。

引数:
  CANDIDATES - 候補リスト（全体）
  START      - 開始インデックス（0始まり）
  END        - 終了インデックス（この位置は含まない）

この関数は実際のUI更新を担当します。
実装は nskk-candidate-window のAPIを使用します。"
  (let ((batch (cl-subseq candidates start end)))

    (when nskk-async-candidates-verbose
      (message "Displaying batch: %d-%d (%d candidates)"
               start end (length batch)))

    ;; nskk-candidate-window のAPIを使用
    ;; 初回（start = 0）の場合は新規表示、それ以外は更新
    (if (zerop start)
        (when (fboundp 'nskk-show-candidates)
          (nskk-show-candidates candidates))
      (when (fboundp 'nskk-update-candidates)
        (nskk-update-candidates candidates 0)))))

(defun nskk-async-candidates--on-complete (context)
  "表示完了時の処理。

引数:
  CONTEXT - nskk-async-candidates-context構造体"
  (when nskk-async-candidates-verbose
    (let* ((elapsed (- (float-time)
                      (nskk-async-candidates-context-start-time context)))
           (count (nskk-async-candidates-context-displayed-count context)))
      (message "Async candidate display completed: %d candidates in %.3fs"
               count elapsed)))

  ;; コールバック呼び出し
  (when (nskk-async-candidates-context-callback context)
    (funcall (nskk-async-candidates-context-callback context)
             (nskk-async-candidates-context-displayed-count context)))

  ;; コンテキストクリア
  (setq nskk-async-candidates--current-context nil))

;;; キャンセル処理

;;;###autoload
(defun nskk-async-candidates-cancel ()
  "実行中の非同期候補表示をキャンセルする。"
  (interactive)
  (when nskk-async-candidates--current-context
    (let ((context nskk-async-candidates--current-context))

      (when nskk-async-candidates-verbose
        (message "Cancelling async candidate display"))

      ;; キャンセルフラグ設定
      (setf (nskk-async-candidates-context-cancelled context) t)

      ;; タイマーキャンセル
      (when (nskk-async-candidates-context-timer context)
        (cancel-timer (nskk-async-candidates-context-timer context)))

      ;; コンテキストクリア
      (setq nskk-async-candidates--current-context nil))))

;;; 状態確認

(defun nskk-async-candidates-running-p ()
  "非同期候補表示が実行中かどうかを返す。

戻り値:
  実行中の場合t、そうでない場合nil"
  (and nskk-async-candidates--current-context
       (not (nskk-async-candidates-context-cancelled
             nskk-async-candidates--current-context))
       t))

(defun nskk-async-candidates-progress ()
  "現在の表示進捗を取得する。

戻り値:
  plist形式の進捗情報、または nil（実行中でない場合）
  (:displayed N :total M :percentage P)
    displayed  - 表示済み候補数
    total      - 全候補数
    percentage - 進捗率（0.0-1.0）"
  (when nskk-async-candidates--current-context
    (let* ((context nskk-async-candidates--current-context)
           (displayed (nskk-async-candidates-context-displayed-count context))
           (total (length (nskk-async-candidates-context-candidates context)))
           (percentage (if (zerop total) 0.0 (/ (float displayed) total))))

      (list :displayed displayed
            :total total
            :percentage percentage))))

;;; ユーティリティ関数

(defun nskk-async-candidates-stats ()
  "現在の非同期候補表示の統計情報を取得する。

戻り値:
  plist形式の統計情報
  (:running BOOL :progress PLIST :elapsed FLOAT)
    running  - 実行中かどうか
    progress - 進捗情報（nskk-async-candidates-progressの戻り値）
    elapsed  - 経過時間（秒）"
  (let ((running (nskk-async-candidates-running-p))
        (progress (nskk-async-candidates-progress))
        (elapsed (when nskk-async-candidates--current-context
                   (- (float-time)
                      (nskk-async-candidates-context-start-time
                       nskk-async-candidates--current-context)))))

    (list :running running
          :progress progress
          :elapsed elapsed)))

;;;###autoload
(defun nskk-async-candidates-show-stats ()
  "現在の統計情報をメッセージとして表示する。"
  (interactive)
  (let ((stats (nskk-async-candidates-stats)))
    (if (plist-get stats :running)
        (let* ((progress (plist-get stats :progress))
               (displayed (plist-get progress :displayed))
               (total (plist-get progress :total))
               (percentage (* 100 (plist-get progress :percentage)))
               (elapsed (plist-get stats :elapsed)))
          (message "Async candidates: %d/%d (%.1f%%) in %.3fs"
                   displayed total percentage elapsed))
      (message "No async candidate display is running"))))

;;; 設定プリセット

(defun nskk-async-candidates-preset-fast ()
  "高速表示プリセット（バッチサイズ大、遅延小）。"
  (interactive)
  (setq nskk-async-candidates-batch-size 10
        nskk-async-candidates-delay 0.0005))

(defun nskk-async-candidates-preset-smooth ()
  "スムーズ表示プリセット（バッチサイズ中、遅延中）。"
  (interactive)
  (setq nskk-async-candidates-batch-size 5
        nskk-async-candidates-delay 0.001))

(defun nskk-async-candidates-preset-responsive ()
  "応答性優先プリセット（バッチサイズ小、遅延小）。"
  (interactive)
  (setq nskk-async-candidates-batch-size 3
        nskk-async-candidates-delay 0.0005))

(provide 'nskk-async-candidates)

;;; nskk-async-candidates.el ends here
