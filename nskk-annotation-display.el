;;; nskk-annotation-display.el --- Annotation display system for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, annotation
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

;; このファイルはSKK注釈の表示システムを実装します。
;;
;; 特徴:
;; - ポップアップ表示（tooltip）
;; - インライン表示（候補横）
;; - エコーエリア表示
;; - オーバーレイ表示
;; - 画像・音声対応準備
;; - カスタマイズ可能な表示スタイル
;;
;; 表示方式:
;;
;;   1. tooltip方式（デフォルト）:
;;      マウスポインタ付近にツールチップ表示
;;
;;   2. inline方式:
;;      候補文字列の直後に注釈を表示
;;      例: 「愛 (love)」
;;
;;   3. echo-area方式:
;;      エコーエリアに注釈を表示
;;
;;   4. overlay方式:
;;      バッファ内の候補上にオーバーレイ表示
;;
;;   5. popup方式:
;;      専用ウィンドウでポップアップ表示
;;
;; 使用例:
;;
;;   (require 'nskk-annotation-display)
;;
;;   ;; 注釈を表示
;;   (nskk-show-annotation annotation)
;;
;;   ;; 表示スタイルを変更
;;   (setq nskk-annotation-display-style 'inline)
;;
;;   ;; 注釈を非表示
;;   (nskk-hide-annotation)

;;; Code:

(require 'cl-lib)
(require 'nskk-annotation-parser)

;;; カスタマイズ変数

(defgroup nskk-annotation-display nil
  "NSKK annotation display settings."
  :group 'nskk
  :prefix "nskk-annotation-display-")

(defcustom nskk-annotation-display-style 'tooltip
  "注釈の表示スタイル。
- 'tooltip: ツールチップ表示（デフォルト）
- 'inline: インライン表示
- 'echo-area: エコーエリア表示
- 'overlay: オーバーレイ表示
- 'popup: ポップアップウィンドウ表示
- 'none: 表示しない"
  :type '(choice (const :tag "Tooltip" tooltip)
                 (const :tag "Inline" inline)
                 (const :tag "Echo area" echo-area)
                 (const :tag "Overlay" overlay)
                 (const :tag "Popup window" popup)
                 (const :tag "None" none))
  :group 'nskk-annotation-display)

(defcustom nskk-annotation-display-delay 0.5
  "注釈を表示するまでの遅延時間（秒）。"
  :type 'number
  :group 'nskk-annotation-display)

(defcustom nskk-annotation-display-duration 5.0
  "注釈の表示時間（秒）。
nil の場合は手動で消すまで表示し続ける。"
  :type '(choice number (const :tag "Unlimited" nil))
  :group 'nskk-annotation-display)

(defcustom nskk-annotation-display-max-width 80
  "注釈表示の最大幅（文字数）。"
  :type 'integer
  :group 'nskk-annotation-display)

(defcustom nskk-annotation-display-max-height 10
  "注釈表示の最大高さ（行数）。"
  :type 'integer
  :group 'nskk-annotation-display)

(defcustom nskk-annotation-display-inline-format " (%s)"
  "インライン表示時のフォーマット文字列。
%s が注釈文字列に置き換えられる。"
  :type 'string
  :group 'nskk-annotation-display)

(defcustom nskk-annotation-display-face 'tooltip
  "注釈表示に使用するface。"
  :type 'face
  :group 'nskk-annotation-display)

(defcustom nskk-annotation-display-enable-images t
  "非nilの場合、画像注釈の表示を有効にする。"
  :type 'boolean
  :group 'nskk-annotation-display)

(defcustom nskk-annotation-display-image-max-width 400
  "画像表示の最大幅（ピクセル）。"
  :type 'integer
  :group 'nskk-annotation-display)

(defcustom nskk-annotation-display-image-max-height 300
  "画像表示の最大高さ（ピクセル）。"
  :type 'integer
  :group 'nskk-annotation-display)

;;; 内部変数

(defvar-local nskk-annotation-display--overlay nil
  "現在表示中の注釈オーバーレイ。")

(defvar-local nskk-annotation-display--timer nil
  "注釈表示タイマー。")

(defvar-local nskk-annotation-display--hide-timer nil
  "注釈自動非表示タイマー。")

(defvar-local nskk-annotation-display--current-annotation nil
  "現在表示中の注釈。")

;;; 注釈表示メイン

;;;###autoload
(defun nskk-show-annotation (annotation &optional position)
  "ANNOTATION を表示する。
POSITION が指定された場合、その位置に表示する（デフォルト: ポイント位置）。"
  (when (and annotation
             (not (eq nskk-annotation-display-style 'none)))
    ;; 既存の注釈を非表示
    (nskk-hide-annotation)

    ;; 表示遅延がある場合はタイマーを設定
    (if (and nskk-annotation-display-delay
             (> nskk-annotation-display-delay 0))
        (setq nskk-annotation-display--timer
              (run-with-timer
               nskk-annotation-display-delay nil
               #'nskk-annotation-display--show-internal
               annotation position))
      ;; 遅延なしで即座に表示
      (nskk-annotation-display--show-internal annotation position))))

(defun nskk-annotation-display--show-internal (annotation position)
  "ANNOTATION を実際に表示する内部関数。"
  (setq nskk-annotation-display--current-annotation annotation)

  ;; 表示スタイルに応じて表示
  (pcase nskk-annotation-display-style
    ('tooltip
     (nskk-annotation-display--show-tooltip annotation position))
    ('inline
     (nskk-annotation-display--show-inline annotation position))
    ('echo-area
     (nskk-annotation-display--show-echo-area annotation))
    ('overlay
     (nskk-annotation-display--show-overlay annotation position))
    ('popup
     (nskk-annotation-display--show-popup annotation position)))

  ;; 自動非表示タイマーを設定
  (when nskk-annotation-display-duration
    (setq nskk-annotation-display--hide-timer
          (run-with-timer nskk-annotation-display-duration nil
                         #'nskk-hide-annotation))))

;;;###autoload
(defun nskk-hide-annotation ()
  "表示中の注釈を非表示にする。"
  (interactive)

  ;; タイマーをキャンセル
  (when nskk-annotation-display--timer
    (cancel-timer nskk-annotation-display--timer)
    (setq nskk-annotation-display--timer nil))

  (when nskk-annotation-display--hide-timer
    (cancel-timer nskk-annotation-display--hide-timer)
    (setq nskk-annotation-display--hide-timer nil))

  ;; オーバーレイを削除
  (when nskk-annotation-display--overlay
    (delete-overlay nskk-annotation-display--overlay)
    (setq nskk-annotation-display--overlay nil))

  ;; 状態をクリア
  (setq nskk-annotation-display--current-annotation nil))

;;; 表示スタイル別実装

(defun nskk-annotation-display--show-tooltip (annotation position)
  "ANNOTATION をツールチップで表示する。"
  (let* ((text (nskk-annotation-display--format-text annotation))
         (pos (or position (point))))
    (when text
      (tooltip-show text))))

(defun nskk-annotation-display--show-inline (annotation position)
  "ANNOTATION をインライン表示する。"
  (let* ((text (nskk-annotation-display--format-text annotation))
         (pos (or position (point)))
         (formatted (format nskk-annotation-display-inline-format text)))
    (when (and text (not (string-empty-p text)))
      (setq nskk-annotation-display--overlay
            (make-overlay pos pos))
      (overlay-put nskk-annotation-display--overlay
                   'after-string
                   (propertize formatted 'face nskk-annotation-display-face)))))

(defun nskk-annotation-display--show-echo-area (annotation)
  "ANNOTATION をエコーエリアに表示する。"
  (let ((text (nskk-annotation-display--format-text annotation)))
    (when text
      (message "%s" text))))

(defun nskk-annotation-display--show-overlay (annotation position)
  "ANNOTATION をオーバーレイで表示する。"
  (let* ((text (nskk-annotation-display--format-text annotation))
         (pos (or position (point)))
         (end-pos (min (+ pos 1) (point-max))))
    (when text
      (setq nskk-annotation-display--overlay
            (make-overlay pos end-pos))
      (overlay-put nskk-annotation-display--overlay
                   'before-string
                   (concat (propertize text 'face nskk-annotation-display-face) "\n")))))

(defun nskk-annotation-display--show-popup (annotation position)
  "ANNOTATION をポップアップウィンドウで表示する。"
  ;; ポップアップウィンドウの実装は将来的に追加
  ;; 現時点ではツールチップにフォールバック
  (nskk-annotation-display--show-tooltip annotation position))

;;; テキスト整形

(defun nskk-annotation-display--format-text (annotation)
  "ANNOTATION を表示用テキストに整形する。"
  (when annotation
    (let ((text (nskk-annotation-parser-get-description annotation)))
      ;; 画像注釈の場合は画像を含める（オプション）
      (when (and nskk-annotation-display-enable-images
                 (nskk-annotation-parser-has-media-p annotation)
                 (eq (nskk-annotation-media-type annotation) 'image))
        (let ((image-text (nskk-annotation-display--format-image annotation)))
          (when image-text
            (setq text (concat text "\n" image-text)))))

      ;; 最大幅で折り返し
      (setq text (nskk-annotation-display--wrap-text text
                                                     nskk-annotation-display-max-width))

      ;; 最大高さで切り詰め
      (let ((lines (split-string text "\n")))
        (when (> (length lines) nskk-annotation-display-max-height)
          (setq lines (seq-take lines nskk-annotation-display-max-height))
          (setq text (string-join lines "\n"))))

      text)))

(defun nskk-annotation-display--wrap-text (text max-width)
  "TEXT を MAX-WIDTH で折り返す。"
  (if (<= (length text) max-width)
      text
    (let ((lines nil)
          (current-line "")
          (words (split-string text)))
      (dolist (word words)
        (if (> (+ (length current-line) (length word) 1) max-width)
            (progn
              (push current-line lines)
              (setq current-line word))
          (setq current-line
                (if (string-empty-p current-line)
                    word
                  (concat current-line " " word)))))
      (when (not (string-empty-p current-line))
        (push current-line lines))
      (string-join (nreverse lines) "\n"))))

(defun nskk-annotation-display--format-image (annotation)
  "ANNOTATION から画像を表示用にフォーマットする。"
  (when-let ((path (nskk-annotation-parser-get-media-path annotation)))
    (condition-case err
        (when (and (file-exists-p path)
                   (image-type-available-p (image-type-from-file-name path)))
          ;; 画像を読み込んでサイズ調整
          (let* ((image (create-image path nil nil
                                     :max-width nskk-annotation-display-image-max-width
                                     :max-height nskk-annotation-display-image-max-height))
                 (image-string (propertize " " 'display image)))
            image-string))
      (error
       (format "[Image error: %s]" (error-message-string err))))))

;;; 候補との統合

;;;###autoload
(defun nskk-annotation-display-with-candidate (candidate annotation)
  "CANDIDATE と ANNOTATION を組み合わせて表示用文字列を生成する。"
  (if (and annotation
           (eq nskk-annotation-display-style 'inline))
      (let ((ann-text (nskk-annotation-display--format-text annotation)))
        (concat candidate
                (when (and ann-text (not (string-empty-p ann-text)))
                  (format nskk-annotation-display-inline-format ann-text))))
    candidate))

;;;###autoload
(defun nskk-annotation-display-candidates-with-annotations (candidates)
  "CANDIDATES（(word . annotation)のリスト）を表示用文字列のリストに変換する。"
  (mapcar (lambda (cand)
            (nskk-annotation-display-with-candidate
             (car cand)
             (when (cdr cand)
               (nskk-parse-annotation (cdr cand)))))
          candidates))

;;; ユーティリティ関数

(defun nskk-annotation-display-current-p ()
  "現在注釈が表示されているかどうかを判定する。"
  (and nskk-annotation-display--current-annotation t))

(defun nskk-annotation-display-get-current ()
  "現在表示中の注釈を取得する。"
  nskk-annotation-display--current-annotation)

(defun nskk-annotation-display-toggle-style ()
  "注釈表示スタイルを切り替える。"
  (interactive)
  (setq nskk-annotation-display-style
        (pcase nskk-annotation-display-style
          ('tooltip 'inline)
          ('inline 'echo-area)
          ('echo-area 'overlay)
          ('overlay 'popup)
          ('popup 'none)
          ('none 'tooltip)
          (_ 'tooltip)))
  (message "Annotation display style: %s" nskk-annotation-display-style))

;;; クリーンアップ

(defun nskk-annotation-display-cleanup ()
  "注釈表示に関連するリソースをクリーンアップする。"
  (nskk-hide-annotation))

;;; デバッグ・統計

(defun nskk-annotation-display-describe ()
  "現在の注釈表示状態を表示する。"
  (interactive)
  (message "NSKK Annotation Display:
  Style: %s
  Delay: %s sec
  Duration: %s sec
  Current annotation: %s
  Displaying: %s"
           nskk-annotation-display-style
           nskk-annotation-display-delay
           (or nskk-annotation-display-duration "unlimited")
           (if nskk-annotation-display--current-annotation
               (nskk-annotation-text nskk-annotation-display--current-annotation)
             "none")
           (if (nskk-annotation-display-current-p) "yes" "no")))

(provide 'nskk-annotation-display)

;;; nskk-annotation-display.el ends here
