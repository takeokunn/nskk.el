;;; nskk-progress.el --- Progress indicator for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, progress
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

;; このファイルはプログレス表示機能を実装します。
;;
;; 特徴:
;; - プログレスバー（make-progress-reporter使用）
;; - スピナー
;; - パーセンテージ表示
;; - ミニバッファ・モードライン統合
;;
;; 使用例:
;;
;;   (require 'nskk-progress)
;;
;;   ;; プログレスレポーター使用
;;   (let ((reporter (nskk-progress-create "辞書読み込み中..." 0 100)))
;;     (dotimes (i 100)
;;       (nskk-progress-update reporter i))
;;     (nskk-progress-done reporter))
;;
;;   ;; スピナー使用
;;   (nskk-progress-spinner-start "処理中...")
;;   ;; ... 処理 ...
;;   (nskk-progress-spinner-stop)

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-progress nil
  "Progress indicator customization."
  :group 'nskk
  :prefix "nskk-progress-")

(defcustom nskk-progress-display-type 'minibuffer
  "プログレス表示の種類。
'minibuffer - ミニバッファに表示
'modeline   - モードラインに表示
'both       - 両方に表示"
  :type '(choice (const :tag "ミニバッファ" minibuffer)
                 (const :tag "モードライン" modeline)
                 (const :tag "両方" both))
  :group 'nskk-progress)

(defcustom nskk-progress-spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "スピナーのフレーム配列。"
  :type '(vector string)
  :group 'nskk-progress)

(defcustom nskk-progress-spinner-delay 0.1
  "スピナーフレームの更新間隔（秒）。"
  :type 'float
  :group 'nskk-progress)

(defcustom nskk-progress-percentage-format " [%3d%%]"
  "パーセンテージ表示のフォーマット。"
  :type 'string
  :group 'nskk-progress)

(defcustom nskk-progress-bar-width 20
  "プログレスバーの幅（文字数）。"
  :type 'integer
  :group 'nskk-progress)

(defcustom nskk-progress-bar-char-filled "■"
  "プログレスバーの塗りつぶし文字。"
  :type 'string
  :group 'nskk-progress)

(defcustom nskk-progress-bar-char-empty "□"
  "プログレスバーの空白文字。"
  :type 'string
  :group 'nskk-progress)

(defcustom nskk-progress-verbose nil
  "非nilの場合、詳細なログを出力する。"
  :type 'boolean
  :group 'nskk-progress)

;;; フェイス定義

(defface nskk-progress-face
  '((t :inherit default))
  "プログレスメッセージのフェイス。"
  :group 'nskk-progress)

(defface nskk-progress-percentage-face
  '((t :inherit font-lock-constant-face))
  "パーセンテージのフェイス。"
  :group 'nskk-progress)

(defface nskk-progress-bar-filled-face
  '((t :inherit success))
  "プログレスバーの塗りつぶし部分のフェイス。"
  :group 'nskk-progress)

(defface nskk-progress-bar-empty-face
  '((t :inherit shadow))
  "プログレスバーの空白部分のフェイス。"
  :group 'nskk-progress)

(defface nskk-progress-spinner-face
  '((t :inherit warning))
  "スピナーのフェイス。"
  :group 'nskk-progress)

;;; データ構造

(cl-defstruct (nskk-progress-reporter
               (:constructor nskk-progress-reporter--create)
               (:copier nil))
  "プログレスレポーター。

スロット:
  message     - メッセージ文字列
  min-value   - 最小値
  max-value   - 最大値
  current     - 現在値
  reporter    - make-progress-reporterの戻り値
  start-time  - 開始時刻（float-time）"
  (message nil :type (or null string))
  (min-value 0 :type number)
  (max-value 100 :type number)
  (current 0 :type number)
  (reporter nil)
  (start-time nil :type (or null number)))

(cl-defstruct (nskk-progress-spinner
               (:constructor nskk-progress-spinner--create)
               (:copier nil))
  "スピナー。

スロット:
  message     - メッセージ文字列
  frames      - フレーム配列
  frame-index - 現在のフレームインデックス
  timer       - 更新タイマー
  start-time  - 開始時刻（float-time）"
  (message nil :type (or null string))
  (frames nskk-progress-spinner-frames :type vector)
  (frame-index 0 :type integer)
  (timer nil :type (or null timer))
  (start-time nil :type (or null number)))

;;; 内部変数

(defvar nskk-progress--current-reporter nil
  "現在アクティブなプログレスレポーター。")

(defvar nskk-progress--current-spinner nil
  "現在アクティブなスピナー。")

(defvar nskk-progress--modeline-string ""
  "モードラインに表示する文字列。")

;;; プログレスレポーター

;;;###autoload
(defun nskk-progress-create (message min-value max-value &optional current-value)
  "プログレスレポーターを作成する。

引数:
  MESSAGE       - メッセージ文字列
  MIN-VALUE     - 最小値
  MAX-VALUE     - 最大値
  CURRENT-VALUE - 現在値（省略時はMIN-VALUE）

戻り値:
  nskk-progress-reporter構造体"
  (when nskk-progress-verbose
    (message "Creating progress reporter: %s [%d-%d]"
             message min-value max-value))

  ;; 既存のプログレスをクリーンアップ
  (nskk-progress-cleanup)

  (let* ((current (or current-value min-value))
         (reporter (make-progress-reporter message min-value max-value))
         (progress (nskk-progress-reporter--create
                    :message message
                    :min-value min-value
                    :max-value max-value
                    :current current
                    :reporter reporter
                    :start-time (float-time))))

    (setq nskk-progress--current-reporter progress)

    ;; 初期表示
    (nskk-progress--update-display progress)

    progress))

;;;###autoload
(defun nskk-progress-update (reporter value)
  "プログレスを更新する。

引数:
  REPORTER - nskk-progress-reporter構造体
  VALUE    - 新しい値"
  (when reporter
    (setf (nskk-progress-reporter-current reporter) value)

    ;; Emacsの標準プログレスレポーター更新
    (progress-reporter-update (nskk-progress-reporter-reporter reporter) value)

    ;; カスタム表示更新
    (nskk-progress--update-display reporter)))

;;;###autoload
(defun nskk-progress-done (reporter)
  "プログレスレポーターを完了する。

引数:
  REPORTER - nskk-progress-reporter構造体"
  (when reporter
    (when nskk-progress-verbose
      (let ((elapsed (- (float-time)
                       (nskk-progress-reporter-start-time reporter))))
        (message "Progress completed: %s in %.3fs"
                 (nskk-progress-reporter-message reporter)
                 elapsed)))

    ;; Emacsの標準プログレスレポーター完了
    (progress-reporter-done (nskk-progress-reporter-reporter reporter))

    ;; クリーンアップ
    (when (eq reporter nskk-progress--current-reporter)
      (setq nskk-progress--current-reporter nil))

    (nskk-progress--clear-display)))

(defun nskk-progress--update-display (reporter)
  "プログレス表示を更新する。

引数:
  REPORTER - nskk-progress-reporter構造体"
  (let* ((message (nskk-progress-reporter-message reporter))
         (current (nskk-progress-reporter-current reporter))
         (min-val (nskk-progress-reporter-min-value reporter))
         (max-val (nskk-progress-reporter-max-value reporter))
         (percentage (nskk-progress--calculate-percentage current min-val max-val))
         (bar (nskk-progress--make-bar percentage))
         (display-text (format "%s %s%s"
                              message
                              bar
                              (propertize (format nskk-progress-percentage-format
                                                (floor percentage))
                                        'face 'nskk-progress-percentage-face))))

    ;; 表示タイプに応じて表示
    (pcase nskk-progress-display-type
      ('minibuffer
       (message "%s" display-text))
      ('modeline
       (setq nskk-progress--modeline-string display-text)
       (force-mode-line-update))
      ('both
       (message "%s" display-text)
       (setq nskk-progress--modeline-string display-text)
       (force-mode-line-update)))))

(defun nskk-progress--calculate-percentage (current min-val max-val)
  "パーセンテージを計算する。

引数:
  CURRENT - 現在値
  MIN-VAL - 最小値
  MAX-VAL - 最大値

戻り値:
  パーセンテージ（0.0-100.0）"
  (if (= min-val max-val)
      100.0
    (* 100.0 (/ (- (float current) min-val)
                (- max-val min-val)))))

(defun nskk-progress--make-bar (percentage)
  "プログレスバーを生成する。

引数:
  PERCENTAGE - パーセンテージ（0.0-100.0）

戻り値:
  プログレスバー文字列"
  (let* ((filled-count (floor (* nskk-progress-bar-width (/ percentage 100.0))))
         (empty-count (- nskk-progress-bar-width filled-count))
         (filled-part (propertize (make-string filled-count
                                              (string-to-char nskk-progress-bar-char-filled))
                                 'face 'nskk-progress-bar-filled-face))
         (empty-part (propertize (make-string empty-count
                                             (string-to-char nskk-progress-bar-char-empty))
                                'face 'nskk-progress-bar-empty-face)))
    (concat "[" filled-part empty-part "]")))

;;; スピナー

;;;###autoload
(defun nskk-progress-spinner-start (message &optional frames delay)
  "スピナーを開始する。

引数:
  MESSAGE - メッセージ文字列
  FRAMES  - フレーム配列（省略時はnskk-progress-spinner-frames）
  DELAY   - 更新間隔（秒、省略時はnskk-progress-spinner-delay）

戻り値:
  nskk-progress-spinner構造体"
  (when nskk-progress-verbose
    (message "Starting spinner: %s" message))

  ;; 既存のスピナーを停止
  (nskk-progress-spinner-stop)

  (let* ((frames (or frames nskk-progress-spinner-frames))
         (delay (or delay nskk-progress-spinner-delay))
         (spinner (nskk-progress-spinner--create
                   :message message
                   :frames frames
                   :frame-index 0
                   :start-time (float-time))))

    (setq nskk-progress--current-spinner spinner)

    ;; タイマー開始
    (setf (nskk-progress-spinner-timer spinner)
          (run-at-time delay delay
                      (lambda ()
                        (nskk-progress--spinner-tick spinner))))

    ;; 初期表示
    (nskk-progress--spinner-tick spinner)

    spinner))

;;;###autoload
(defun nskk-progress-spinner-stop ()
  "スピナーを停止する。"
  (interactive)
  (when nskk-progress--current-spinner
    (let ((spinner nskk-progress--current-spinner))

      (when nskk-progress-verbose
        (let ((elapsed (- (float-time)
                         (nskk-progress-spinner-start-time spinner))))
          (message "Spinner stopped: %s in %.3fs"
                   (nskk-progress-spinner-message spinner)
                   elapsed)))

      ;; タイマー停止
      (when (nskk-progress-spinner-timer spinner)
        (cancel-timer (nskk-progress-spinner-timer spinner)))

      ;; クリーンアップ
      (setq nskk-progress--current-spinner nil)
      (nskk-progress--clear-display))))

(defun nskk-progress--spinner-tick (spinner)
  "スピナーフレームを更新する。

引数:
  SPINNER - nskk-progress-spinner構造体"
  (when spinner
    (let* ((frames (nskk-progress-spinner-frames spinner))
           (frame-count (length frames))
           (index (nskk-progress-spinner-frame-index spinner))
           (frame (aref frames index))
           (message (nskk-progress-spinner-message spinner))
           (display-text (format "%s %s"
                                (propertize frame 'face 'nskk-progress-spinner-face)
                                message)))

      ;; フレームインデックス更新
      (setf (nskk-progress-spinner-frame-index spinner)
            (mod (1+ index) frame-count))

      ;; 表示更新
      (pcase nskk-progress-display-type
        ('minibuffer
         (message "%s" display-text))
        ('modeline
         (setq nskk-progress--modeline-string display-text)
         (force-mode-line-update))
        ('both
         (message "%s" display-text)
         (setq nskk-progress--modeline-string display-text)
         (force-mode-line-update))))))

;;; ユーティリティ

(defun nskk-progress--clear-display ()
  "プログレス表示をクリアする。"
  (setq nskk-progress--modeline-string "")
  (force-mode-line-update))

;;;###autoload
(defun nskk-progress-cleanup ()
  "すべてのプログレス表示をクリーンアップする。"
  (interactive)
  (when nskk-progress--current-reporter
    (nskk-progress-done nskk-progress--current-reporter))
  (when nskk-progress--current-spinner
    (nskk-progress-spinner-stop)))

;;; モードライン統合

;;;###autoload
(defun nskk-progress-mode-line-format ()
  "モードライン用のフォーマット文字列を返す。

モードラインのフォーマット設定に追加することで、
プログレスをモードラインに表示できます:

  (setq-default mode-line-format
    (append mode-line-format
            '((:eval (nskk-progress-mode-line-format)))))"
  (when (> (length nskk-progress--modeline-string) 0)
    (concat " " nskk-progress--modeline-string)))

;;; 便利な高レベルAPI

;;;###autoload
(defmacro nskk-progress-with-reporter (message min max &rest body)
  "プログレスレポーターを使用してBODYを実行する。

使用例:
  (nskk-progress-with-reporter \"処理中...\" 0 100
    (dotimes (i 100)
      (nskk-progress-update nskk-progress--current-reporter i)
      (sleep-for 0.01)))"
  (declare (indent 3))
  `(let ((reporter (nskk-progress-create ,message ,min ,max)))
     (unwind-protect
         (progn ,@body)
       (nskk-progress-done reporter))))

;;;###autoload
(defmacro nskk-progress-with-spinner (message &rest body)
  "スピナーを使用してBODYを実行する。

使用例:
  (nskk-progress-with-spinner \"処理中...\"
    (sleep-for 2))"
  (declare (indent 1))
  `(progn
     (nskk-progress-spinner-start ,message)
     (unwind-protect
         (progn ,@body)
       (nskk-progress-spinner-stop))))

(provide 'nskk-progress)

;;; nskk-progress.el ends here
