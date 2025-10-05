;;; nskk-completion-ui.el --- Completion UI for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, completion, ui
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

;; このファイルは補完機能のUIを統合します。
;;
;; 表示方式:
;; - インライン表示（現在位置に直接表示）
;; - ポップアップ表示（別ウィンドウ）
;; - ミニバッファ表示
;; - ツールチップ表示
;;
;; 特徴:
;; - リアルタイム補完候補表示
;; - キーボードナビゲーション
;; - 候補のプレビュー
;; - カスタマイズ可能な表示スタイル
;;
;; パフォーマンス目標:
;; - UI更新: < 5ms
;; - 描画: < 10ms
;; - キー応答: < 1ms
;;
;; 使用例:
;;
;;   (require 'nskk-completion-ui)
;;
;;   ;; インライン補完表示
;;   (nskk-completion-ui-show candidates 'inline)
;;
;;   ;; ポップアップ補完表示
;;   (nskk-completion-ui-show candidates 'popup)
;;
;;   ;; 補完を閉じる
;;   (nskk-completion-ui-hide)

;;; Code:

(require 'cl-lib)
(require 'nskk-completion-engine)

;;; カスタマイズ変数

(defgroup nskk-completion-ui nil
  "Completion UI for NSKK."
  :group 'nskk
  :prefix "nskk-completion-ui-")

(defcustom nskk-completion-ui-display-method 'inline
  "補完候補の表示方法。
  - 'inline: インライン表示
  - 'popup: ポップアップウィンドウ
  - 'minibuffer: ミニバッファ
  - 'tooltip: ツールチップ"
  :type '(choice (const :tag "Inline" inline)
                 (const :tag "Popup" popup)
                 (const :tag "Minibuffer" minibuffer)
                 (const :tag "Tooltip" tooltip))
  :group 'nskk-completion-ui)

(defcustom nskk-completion-ui-max-display-items 10
  "表示する補完候補の最大数。"
  :type 'integer
  :group 'nskk-completion-ui)

(defcustom nskk-completion-ui-show-score nil
  "非nilの場合、スコアを表示する。"
  :type 'boolean
  :group 'nskk-completion-ui)

(defcustom nskk-completion-ui-show-rank t
  "非nilの場合、ランクを表示する。"
  :type 'boolean
  :group 'nskk-completion-ui)

(defcustom nskk-completion-ui-highlight-current t
  "非nilの場合、現在の候補をハイライトする。"
  :type 'boolean
  :group 'nskk-completion-ui)

(defcustom nskk-completion-ui-auto-show t
  "非nilの場合、自動的に補完候補を表示する。"
  :type 'boolean
  :group 'nskk-completion-ui)

(defcustom nskk-completion-ui-delay 0.1
  "補完候補表示の遅延時間（秒）。"
  :type 'float
  :group 'nskk-completion-ui)

;;; フェイス定義

(defface nskk-completion-ui-current-face
  '((t (:inherit highlight)))
  "現在選択中の補完候補のフェイス。"
  :group 'nskk-completion-ui)

(defface nskk-completion-ui-rank-face
  '((t (:inherit font-lock-comment-face)))
  "ランク番号のフェイス。"
  :group 'nskk-completion-ui)

(defface nskk-completion-ui-score-face
  '((t (:inherit font-lock-constant-face)))
  "スコアのフェイス。"
  :group 'nskk-completion-ui)

(defface nskk-completion-ui-separator-face
  '((t (:inherit font-lock-comment-face)))
  "区切り文字のフェイス。"
  :group 'nskk-completion-ui)

;;; 内部変数

(defvar nskk-completion-ui--current-candidates nil
  "現在表示中の補完候補。")

(defvar nskk-completion-ui--current-index 0
  "現在選択中の候補インデックス。")

(defvar nskk-completion-ui--overlay nil
  "インライン表示用のオーバーレイ。")

(defvar nskk-completion-ui--popup-buffer nil
  "ポップアップ表示用のバッファ。")

(defvar nskk-completion-ui--popup-window nil
  "ポップアップ表示用のウィンドウ。")

(defvar nskk-completion-ui--timer nil
  "遅延表示用のタイマー。")

;;; メイン関数

;;;###autoload
(defun nskk-completion-ui-show (candidates &optional method)
  "補完候補を表示する。

引数:
  CANDIDATES - 補完候補（nskk-completion-engine-result構造体のリスト）
  METHOD     - 表示方法（省略時はnskk-completion-ui-display-method）

処理:
  指定された表示方法で補完候補を表示"
  (if (null candidates)
      (progn
        (nskk-completion-ui-hide)
        nil)
    (let ((display-method (or method nskk-completion-ui-display-method)))
      (setq nskk-completion-ui--current-candidates candidates)
      (setq nskk-completion-ui--current-index 0)

      (pcase display-method
        ('inline
         (nskk-completion-ui--show-inline candidates))
        ('popup
         (nskk-completion-ui--show-popup candidates))
        ('minibuffer
         (nskk-completion-ui--show-minibuffer candidates))
        ('tooltip
         (nskk-completion-ui--show-tooltip candidates))
        (_
         (nskk-completion-ui--show-inline candidates))))))

;;;###autoload
(defun nskk-completion-ui-hide ()
  "補完候補表示を隠す。"
  (interactive)
  (nskk-completion-ui--hide-inline)
  (nskk-completion-ui--hide-popup)
  (setq nskk-completion-ui--current-candidates nil)
  (setq nskk-completion-ui--current-index 0)
  (when nskk-completion-ui--timer
    (cancel-timer nskk-completion-ui--timer)
    (setq nskk-completion-ui--timer nil)))

;;;###autoload
(defun nskk-completion-ui-show-delayed (candidates &optional method delay)
  "補完候補を遅延表示する。

引数:
  CANDIDATES - 補完候補
  METHOD     - 表示方法
  DELAY      - 遅延時間（秒、省略時はnskk-completion-ui-delay）"
  (when nskk-completion-ui--timer
    (cancel-timer nskk-completion-ui--timer))

  (setq nskk-completion-ui--timer
        (run-with-timer (or delay nskk-completion-ui-delay)
                       nil
                       #'nskk-completion-ui-show
                       candidates
                       method)))

;;; インライン表示

(defun nskk-completion-ui--show-inline (candidates)
  "インライン表示で補完候補を表示する（内部関数）。

引数:
  CANDIDATES - 補完候補"
  (nskk-completion-ui--hide-inline)

  (let ((text (nskk-completion-ui--format-candidates candidates 'inline)))
    (setq nskk-completion-ui--overlay (make-overlay (point) (point)))
    (overlay-put nskk-completion-ui--overlay 'after-string
                (concat " " (propertize text 'face 'nskk-completion-ui-current-face)))))

(defun nskk-completion-ui--hide-inline ()
  "インライン表示を隠す（内部関数）。"
  (when nskk-completion-ui--overlay
    (delete-overlay nskk-completion-ui--overlay)
    (setq nskk-completion-ui--overlay nil)))

;;; ポップアップ表示

(defun nskk-completion-ui--show-popup (candidates)
  "ポップアップ表示で補完候補を表示する（内部関数）。

引数:
  CANDIDATES - 補完候補"
  (nskk-completion-ui--hide-popup)

  (let ((buffer (get-buffer-create "*NSKK Completion*"))
        (text (nskk-completion-ui--format-candidates candidates 'popup)))

    (with-current-buffer buffer
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))

    (setq nskk-completion-ui--popup-buffer buffer)
    (setq nskk-completion-ui--popup-window
          (display-buffer buffer
                         '((display-buffer-at-bottom)
                           (window-height . fit-window-to-buffer))))))

(defun nskk-completion-ui--hide-popup ()
  "ポップアップ表示を隠す（内部関数）。"
  (when nskk-completion-ui--popup-window
    (when (window-live-p nskk-completion-ui--popup-window)
      (delete-window nskk-completion-ui--popup-window))
    (setq nskk-completion-ui--popup-window nil))

  (when nskk-completion-ui--popup-buffer
    (when (buffer-live-p nskk-completion-ui--popup-buffer)
      (kill-buffer nskk-completion-ui--popup-buffer))
    (setq nskk-completion-ui--popup-buffer nil)))

;;; ミニバッファ表示

(defun nskk-completion-ui--show-minibuffer (candidates)
  "ミニバッファ表示で補完候補を表示する（内部関数）。

引数:
  CANDIDATES - 補完候補"
  (let ((text (nskk-completion-ui--format-candidates candidates 'minibuffer)))
    (message "%s" text)))

;;; ツールチップ表示

(defun nskk-completion-ui--show-tooltip (candidates)
  "ツールチップ表示で補完候補を表示する（内部関数）。

引数:
  CANDIDATES - 補完候補"
  (let ((text (nskk-completion-ui--format-candidates candidates 'tooltip)))
    (tooltip-show text)))

;;; フォーマット

(defun nskk-completion-ui--format-candidates (candidates display-method)
  "補完候補をフォーマットする（内部関数）。

引数:
  CANDIDATES     - 補完候補
  DISPLAY-METHOD - 表示方法

戻り値:
  フォーマット済み文字列"
  (let ((display-candidates (seq-take candidates nskk-completion-ui-max-display-items))
        (lines nil))

    (pcase display-method
      ('inline
       ;; インライン: 1行で表示
       (mapconcat
        (lambda (cand)
          (nskk-completion-ui--format-single-candidate cand 'inline))
        display-candidates
        " "))

      ('popup
       ;; ポップアップ: 複数行
       (mapconcat
        (lambda (cand)
          (nskk-completion-ui--format-single-candidate cand 'popup))
        display-candidates
        "\n"))

      ('minibuffer
       ;; ミニバッファ: 1行（コンパクト）
       (concat "補完候補: "
               (mapconcat
                (lambda (cand)
                  (nskk-completion-ui--format-single-candidate cand 'minibuffer))
                display-candidates
                " ")))

      ('tooltip
       ;; ツールチップ: 複数行
       (mapconcat
        (lambda (cand)
          (nskk-completion-ui--format-single-candidate cand 'tooltip))
        display-candidates
        "\n"))

      (_
       ""))))

(defun nskk-completion-ui--format-single-candidate (candidate display-method)
  "単一の補完候補をフォーマットする（内部関数）。

引数:
  CANDIDATE      - 補完候補
  DISPLAY-METHOD - 表示方法

戻り値:
  フォーマット済み文字列"
  (let ((midashi (nskk-completion-engine-result-midashi candidate))
        (rank (nskk-completion-engine-result-rank candidate))
        (score (nskk-completion-engine-result-total-score candidate))
        (parts nil))

    ;; ランク
    (when nskk-completion-ui-show-rank
      (push (propertize (format "%d" rank)
                       'face 'nskk-completion-ui-rank-face)
            parts)
      (push (propertize "." 'face 'nskk-completion-ui-separator-face)
            parts))

    ;; 見出し語
    (push midashi parts)

    ;; スコア
    (when nskk-completion-ui-show-score
      (push (propertize (format " (%.3f)" score)
                       'face 'nskk-completion-ui-score-face)
            parts))

    (apply #'concat (nreverse parts))))

;;; ナビゲーション

;;;###autoload
(defun nskk-completion-ui-next ()
  "次の補完候補を選択する。"
  (interactive)
  (when nskk-completion-ui--current-candidates
    (setq nskk-completion-ui--current-index
          (mod (1+ nskk-completion-ui--current-index)
               (length nskk-completion-ui--current-candidates)))
    (nskk-completion-ui--update-display)))

;;;###autoload
(defun nskk-completion-ui-previous ()
  "前の補完候補を選択する。"
  (interactive)
  (when nskk-completion-ui--current-candidates
    (setq nskk-completion-ui--current-index
          (mod (1- nskk-completion-ui--current-index)
               (length nskk-completion-ui--current-candidates)))
    (nskk-completion-ui--update-display)))

;;;###autoload
(defun nskk-completion-ui-select-current ()
  "現在の補完候補を選択する。

戻り値:
  選択された候補（nskk-completion-engine-result構造体）"
  (interactive)
  (when nskk-completion-ui--current-candidates
    (let ((selected (nth nskk-completion-ui--current-index
                        nskk-completion-ui--current-candidates)))
      (nskk-completion-ui-hide)
      selected)))

(defun nskk-completion-ui--update-display ()
  "表示を更新する（内部関数）。"
  (when nskk-completion-ui--current-candidates
    (let ((saved-index nskk-completion-ui--current-index)
          (candidates nskk-completion-ui--current-candidates)
          (display-method nskk-completion-ui-display-method))
      (pcase display-method
        ('inline
         (nskk-completion-ui--show-inline candidates))
        ('popup
         (nskk-completion-ui--show-popup candidates))
        ('minibuffer
         (nskk-completion-ui--show-minibuffer candidates))
        ('tooltip
         (nskk-completion-ui--show-tooltip candidates))
        (_
         (nskk-completion-ui--show-inline candidates)))
      (setq nskk-completion-ui--current-index saved-index))))

;;; キーマップ

(defvar nskk-completion-ui-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'nskk-completion-ui-next)
    (define-key map (kbd "C-p") #'nskk-completion-ui-previous)
    (define-key map (kbd "<down>") #'nskk-completion-ui-next)
    (define-key map (kbd "<up>") #'nskk-completion-ui-previous)
    (define-key map (kbd "RET") #'nskk-completion-ui-select-current)
    (define-key map (kbd "TAB") #'nskk-completion-ui-select-current)
    (define-key map (kbd "C-g") #'nskk-completion-ui-hide)
    map)
  "補完UI用のキーマップ。")

;;; ユーティリティ関数

;;;###autoload
(defun nskk-completion-ui-visible-p ()
  "補完候補が表示されているか判定する。

戻り値:
  表示されている場合t、そうでない場合nil"
  (not (null nskk-completion-ui--current-candidates)))

(defun nskk-completion-ui-current-candidate ()
  "現在選択中の候補を取得する。

戻り値:
  候補（nskk-completion-engine-result構造体）、表示されていない場合nil"
  (when nskk-completion-ui--current-candidates
    (nth nskk-completion-ui--current-index
         nskk-completion-ui--current-candidates)))

(defun nskk-completion-ui-candidate-count ()
  "補完候補の総数を取得する。

戻り値:
  候補数（整数）"
  (if nskk-completion-ui--current-candidates
      (length nskk-completion-ui--current-candidates)
    0))

;;; 統合関数

;;;###autoload
(defun nskk-completion-ui-complete (index query &rest args)
  "統合補完を実行してUIに表示する。

引数:
  INDEX - 辞書インデックス
  QUERY - 検索クエリ

キーワード引数:
  nskk-completion-engine-searchと同じ

処理:
  1. 補完検索を実行
  2. 結果をUIに表示

戻り値:
  補完候補のリスト"
  (let* ((results (apply #'nskk-completion-engine-search index query args))
         (method (or (plist-get args :display-method)
                    nskk-completion-ui-display-method)))

    (if nskk-completion-ui-auto-show
        (if (> nskk-completion-ui-delay 0)
            (nskk-completion-ui-show-delayed results method)
          (nskk-completion-ui-show results method))
      ;; 自動表示しない場合は結果のみ返す
      nil)

    results))

;;; デバッグ用関数

(defun nskk-completion-ui-debug-info ()
  "補完UIのデバッグ情報を表示する。

戻り値:
  plist形式のデバッグ情報"
  (list :visible (nskk-completion-ui-visible-p)
        :candidate-count (nskk-completion-ui-candidate-count)
        :current-index nskk-completion-ui--current-index
        :display-method nskk-completion-ui-display-method
        :max-display-items nskk-completion-ui-max-display-items
        :auto-show nskk-completion-ui-auto-show))

(provide 'nskk-completion-ui)

;;; nskk-completion-ui.el ends here
