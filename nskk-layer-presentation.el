;;; nskk-layer-presentation.el --- Presentation Layer for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, architecture
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

;; Presentation Layer - 7層アーキテクチャの最上位層
;;
;; 責務:
;; - UIコンポーネントの統合
;; - イベントハンドリング
;; - ユーザー入力の受付
;; - 視覚的フィードバック
;; - キーバインディング管理
;;
;; レイヤー依存:
;; - Extension Layer (イベント配信)
;; - Application Layer (ビジネスロジック呼び出し)
;;
;; 主要コンポーネント:
;; - キーマップ統合
;; - 候補ウィンドウ制御
;; - ミニバッファUI
;; - モードライン表示
;; - インライン表示
;;
;; 使用例:
;; (nskk-presentation-initialize)
;; (nskk-presentation-handle-key-event ?a)
;; (nskk-presentation-show-candidates candidates)
;; (nskk-presentation-update-mode-line "あ")

;;; Code:

(require 'cl-lib)

;;; カスタマイズ可能変数

(defgroup nskk-presentation nil
  "Presentation layer settings for NSKK."
  :group 'nskk
  :prefix "nskk-presentation-")

(defcustom nskk-presentation-inline-candidates-max 5
  "インライン候補表示の最大数。"
  :type 'integer
  :group 'nskk-presentation)

(defcustom nskk-presentation-candidate-window-position 'cursor
  "候補ウィンドウの表示位置。
'cursor - カーソル位置
'bottom - 画面下部
'top    - 画面上部"
  :type '(choice (const :tag "カーソル位置" cursor)
                 (const :tag "画面下部" bottom)
                 (const :tag "画面上部" top))
  :group 'nskk-presentation)

(defcustom nskk-presentation-enable-mode-line t
  "モードライン表示を有効にするか。"
  :type 'boolean
  :group 'nskk-presentation)

;;; 内部変数

(defvar nskk-presentation--current-overlay nil
  "現在アクティブなオーバーレイ。")

(defvar nskk-presentation--candidate-buffer nil
  "候補表示用バッファ。")

(defvar nskk-presentation--event-handlers (make-hash-table :test 'eq)
  "イベントタイプとハンドラーのマッピング。")

;;; UIコンポーネント統合

(defun nskk-presentation-initialize ()
  "Presentation Layerを初期化する。"
  (nskk-presentation--setup-event-handlers)
  (nskk-presentation--initialize-candidate-buffer)
  (nskk-presentation--setup-overlays)
  (when nskk-presentation-enable-mode-line
    (nskk-presentation--initialize-mode-line)))

(defun nskk-presentation-shutdown ()
  "Presentation Layerをシャットダウンする。"
  (nskk-presentation--cleanup-overlays)
  (nskk-presentation--cleanup-candidate-buffer)
  (nskk-presentation--cleanup-event-handlers))

;;; イベントハンドリング

(defun nskk-presentation--setup-event-handlers ()
  "イベントハンドラーをセットアップする。"
  ;; キー入力イベント
  (puthash :key-pressed #'nskk-presentation--on-key-pressed
           nskk-presentation--event-handlers)
  ;; 候補選択イベント
  (puthash :candidate-selected #'nskk-presentation--on-candidate-selected
           nskk-presentation--event-handlers)
  ;; モード変更イベント
  (puthash :mode-changed #'nskk-presentation--on-mode-changed
           nskk-presentation--event-handlers)
  ;; 変換確定イベント
  (puthash :conversion-committed #'nskk-presentation--on-conversion-committed
           nskk-presentation--event-handlers))

(defun nskk-presentation--cleanup-event-handlers ()
  "イベントハンドラーをクリーンアップする。"
  (clrhash nskk-presentation--event-handlers))

(defun nskk-presentation-handle-event (event-type &rest data)
  "イベントを処理する。
EVENT-TYPEはイベントタイプ、DATAは追加データ。"
  (let ((handler (gethash event-type nskk-presentation--event-handlers)))
    (when handler
      (apply handler data))))

;;; キー入力ハンドリング

(defun nskk-presentation--on-key-pressed (key)
  "キー入力イベントを処理する。
KEYは入力されたキー。"
  ;; Application Layerに処理を委譲
  (nskk-presentation--delegate-to-application :process-key key))

(defun nskk-presentation-handle-key-event (key)
  "キーイベントを処理する。
KEYは入力されたキー文字またはイベント。"
  (interactive)
  (nskk-presentation-handle-event :key-pressed key))

;;; 候補表示

(defun nskk-presentation--initialize-candidate-buffer ()
  "候補表示用バッファを初期化する。"
  (unless (buffer-live-p nskk-presentation--candidate-buffer)
    (setq nskk-presentation--candidate-buffer
          (get-buffer-create " *NSKK Candidates*"))
    (with-current-buffer nskk-presentation--candidate-buffer
      (setq buffer-read-only t)
      (buffer-disable-undo))))

(defun nskk-presentation--cleanup-candidate-buffer ()
  "候補表示用バッファをクリーンアップする。"
  (when (buffer-live-p nskk-presentation--candidate-buffer)
    (kill-buffer nskk-presentation--candidate-buffer)
    (setq nskk-presentation--candidate-buffer nil)))

(defun nskk-presentation-show-candidates (candidates &optional index)
  "候補リストを表示する。
CANDIDATESは候補のリスト、INDEXは現在選択中のインデックス。"
  (if (null candidates)
      (nskk-presentation-hide-candidates)
    (let ((display-candidates (if (> (length candidates)
                                     nskk-presentation-inline-candidates-max)
                                  (seq-take candidates
                                            nskk-presentation-inline-candidates-max)
                                candidates)))
      (pcase nskk-presentation-candidate-window-position
        ('cursor (nskk-presentation--show-inline-candidates display-candidates index))
        ('bottom (nskk-presentation--show-bottom-candidates display-candidates index))
        ('top (nskk-presentation--show-top-candidates display-candidates index))))))

(defun nskk-presentation-hide-candidates ()
  "候補表示を非表示にする。"
  (nskk-presentation--cleanup-overlays))

(defun nskk-presentation--show-inline-candidates (candidates index)
  "カーソル位置に候補をインライン表示する。
CANDIDATESは候補リスト、INDEXは選択インデックス。"
  (nskk-presentation--cleanup-overlays)
  (let* ((pos (point))
         (overlay (make-overlay pos pos)))
    (setq nskk-presentation--current-overlay overlay)
    (overlay-put overlay 'after-string
                 (nskk-presentation--format-inline-candidates candidates index))
    (overlay-put overlay 'nskk-presentation t)))

(defun nskk-presentation--show-bottom-candidates (candidates index)
  "画面下部に候補を表示する。
CANDIDATESは候補リスト、INDEXは選択インデックス。"
  (when (buffer-live-p nskk-presentation--candidate-buffer)
    (with-current-buffer nskk-presentation--candidate-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (nskk-presentation--format-list-candidates candidates index)))
      (display-buffer nskk-presentation--candidate-buffer
                      '((display-buffer-at-bottom))))))

(defun nskk-presentation--show-top-candidates (candidates index)
  "画面上部に候補を表示する。
CANDIDATESは候補リスト、INDEXは選択インデックス。"
  (when (buffer-live-p nskk-presentation--candidate-buffer)
    (with-current-buffer nskk-presentation--candidate-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (nskk-presentation--format-list-candidates candidates index)))
      (display-buffer nskk-presentation--candidate-buffer
                      '((display-buffer-at-top))))))

(defun nskk-presentation--format-inline-candidates (candidates index)
  "インライン表示用に候補をフォーマットする。
CANDIDATESは候補リスト、INDEXは選択インデックス。"
  (let ((formatted
         (mapconcat
          (lambda (i)
            (let ((cand (nth i candidates)))
              (if (= i (or index 0))
                  (propertize (format "[%s]" cand) 'face 'highlight)
                (format " %s " cand))))
          (number-sequence 0 (1- (length candidates)))
          " | ")))
    (propertize (format " {%s}" formatted)
                'face 'shadow)))

(defun nskk-presentation--format-list-candidates (candidates index)
  "リスト表示用に候補をフォーマットする。
CANDIDATESは候補リスト、INDEXは選択インデックス。"
  (mapconcat
   (lambda (i)
     (let ((cand (nth i candidates)))
       (format "%s %d. %s"
               (if (= i (or index 0)) ">" " ")
               (1+ i)
               cand)))
   (number-sequence 0 (1- (length candidates)))
   "\n"))

;;; 候補選択イベント

(defun nskk-presentation--on-candidate-selected (candidate)
  "候補選択イベントを処理する。
CANDIDATEは選択された候補。"
  (nskk-presentation-hide-candidates)
  ;; Application Layerに通知
  (nskk-presentation--delegate-to-application :commit-candidate candidate))

;;; モードライン

(defun nskk-presentation--initialize-mode-line ()
  "モードライン表示を初期化する。"
  ;; モードライン初期化ロジック
  (add-to-list 'mode-line-format
               '(:eval (nskk-presentation--mode-line-string))
               t))

(defun nskk-presentation--mode-line-string ()
  "モードライン表示用の文字列を生成する。"
  ;; Application Layerから現在のモードを取得
  (let ((mode (nskk-presentation--get-from-application :current-mode)))
    (pcase mode
      ('hiragana "[あ]")
      ('katakana "[ア]")
      ('latin "[A]")
      ('zenkaku-latin "[Ａ]")
      (_ "[--]"))))

(defun nskk-presentation-update-mode-line (mode-indicator)
  "モードライン表示を更新する。
MODE-INDICATORは表示するモード表示文字列。"
  (when nskk-presentation-enable-mode-line
    (force-mode-line-update)))

;;; モード変更イベント

(defun nskk-presentation--on-mode-changed (old-mode new-mode)
  "モード変更イベントを処理する。
OLD-MODEは変更前のモード、NEW-MODEは変更後のモード。"
  (nskk-presentation-update-mode-line new-mode)
  ;; UIをリフレッシュ
  (nskk-presentation--refresh-ui))

;;; 変換確定イベント

(defun nskk-presentation--on-conversion-committed (text)
  "変換確定イベントを処理する。
TEXTは確定されたテキスト。"
  (nskk-presentation-hide-candidates)
  ;; オーバーレイをクリーンアップ
  (nskk-presentation--cleanup-overlays))

;;; オーバーレイ管理

(defun nskk-presentation--setup-overlays ()
  "オーバーレイをセットアップする。")

(defun nskk-presentation--cleanup-overlays ()
  "オーバーレイをクリーンアップする。"
  (when nskk-presentation--current-overlay
    (delete-overlay nskk-presentation--current-overlay)
    (setq nskk-presentation--current-overlay nil)))

;;; UI更新

(defun nskk-presentation--refresh-ui ()
  "UI全体をリフレッシュする。"
  (force-mode-line-update)
  ;; その他のUI要素も更新
  )

;;; Application Layerとの通信

(defun nskk-presentation--delegate-to-application (action &rest args)
  "Application Layerに処理を委譲する。
ACTIONは実行するアクション、ARGSは引数。"
  ;; Extension Layer経由でApplication Layerに委譲
  ;; 実装は統合時に完成
  (message "Delegating to application: %s with %S" action args))

(defun nskk-presentation--get-from-application (query)
  "Application Layerから情報を取得する。
QUERYは取得する情報のタイプ。"
  ;; Extension Layer経由で取得
  ;; 実装は統合時に完成
  'hiragana)

;;; デバッグ・ロギング

(defvar nskk-presentation--debug-enabled nil
  "デバッグモードが有効かどうか。")

(defun nskk-presentation-enable-debug ()
  "デバッグモードを有効にする。"
  (interactive)
  (setq nskk-presentation--debug-enabled t)
  (message "NSKK Presentation Layer: Debug mode enabled"))

(defun nskk-presentation-disable-debug ()
  "デバッグモードを無効にする。"
  (interactive)
  (setq nskk-presentation--debug-enabled nil)
  (message "NSKK Presentation Layer: Debug mode disabled"))

(defun nskk-presentation--log (format-string &rest args)
  "デバッグログを出力する。
FORMAT-STRINGはフォーマット文字列、ARGSは引数。"
  (when nskk-presentation--debug-enabled
    (apply #'message (concat "[NSKK-Presentation] " format-string) args)))

;;; ヘルスチェック

(defun nskk-presentation-health-check ()
  "Presentation Layerのヘルスチェックを実行する。"
  (interactive)
  (let ((issues '()))
    ;; バッファ状態チェック
    (unless (buffer-live-p nskk-presentation--candidate-buffer)
      (push "Candidate buffer is not initialized" issues))
    ;; イベントハンドラーチェック
    (when (zerop (hash-table-count nskk-presentation--event-handlers))
      (push "No event handlers registered" issues))
    ;; 結果表示
    (if issues
        (message "Presentation Layer issues: %s" (string-join issues ", "))
      (message "Presentation Layer: All systems operational"))))

(provide 'nskk-layer-presentation)
;;; nskk-layer-presentation.el ends here
