;;; nskk-candidate-window.el --- Candidate window for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk
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

;; このファイルはNSKKの候補ウィンドウ機能を実装します。
;;
;; 特徴:
;; - overlayベースのポップアップ表示
;; - ツールチップ表示サポート
;; - カーソル位置・画面端に応じた自動位置調整
;; - 候補番号付き表示
;; - 注釈表示機能
;; - ページングによる大量候補の効率的表示
;; - 選択候補のハイライト
;; - カスタマイズ可能なフェイス・レイアウト
;;
;; 使用例:
;; (nskk-show-candidates '("候補1" "候補2" "候補3"))
;; (nskk-update-candidates '("候補1" "候補2") 1)
;; (nskk-scroll-candidates 'next)
;; (nskk-hide-candidates)

;;; Code:

(require 'cl-lib)

;;; カスタマイズ変数

(defgroup nskk-candidate-window nil
  "NSKK candidate window settings."
  :group 'nskk
  :prefix "nskk-candidate-")

(defcustom nskk-candidate-window-position 'bottom
  "候補ウィンドウの表示位置。
'bottom - カーソルの下に表示
'tooltip - ツールチップとして表示"
  :type '(choice (const :tag "カーソル下" bottom)
                 (const :tag "ツールチップ" tooltip))
  :group 'nskk-candidate-window)

(defcustom nskk-candidate-show-annotations t
  "非nilの場合、注釈を表示する。"
  :type 'boolean
  :group 'nskk-candidate-window)

(defcustom nskk-candidate-page-size 7
  "1ページに表示する候補数。"
  :type 'integer
  :group 'nskk-candidate-window)

(defcustom nskk-candidate-show-index t
  "非nilの場合、候補番号を表示する。"
  :type 'boolean
  :group 'nskk-candidate-window)

(defcustom nskk-candidate-number-style 'arabic
  "候補番号の表示スタイル。
'arabic - 1, 2, 3...
'alphabet - a, b, c..."
  :type '(choice (const :tag "アラビア数字" arabic)
                 (const :tag "アルファベット" alphabet))
  :group 'nskk-candidate-window)

(defcustom nskk-candidate-separator " | "
  "候補間の区切り文字列。"
  :type 'string
  :group 'nskk-candidate-window)

(defcustom nskk-candidate-annotation-separator ": "
  "候補と注釈の区切り文字列。"
  :type 'string
  :group 'nskk-candidate-window)

(defcustom nskk-candidate-window-margin 1
  "ウィンドウの余白（行数）。"
  :type 'integer
  :group 'nskk-candidate-window)

;;; フェイス定義

(defface nskk-candidate-face
  '((t :inherit default))
  "候補のデフォルトフェイス。"
  :group 'nskk-candidate-window)

(defface nskk-candidate-selected-face
  '((t :inherit highlight :weight bold))
  "選択中候補のフェイス。"
  :group 'nskk-candidate-window)

(defface nskk-candidate-number-face
  '((t :inherit font-lock-constant-face))
  "候補番号のフェイス。"
  :group 'nskk-candidate-window)

(defface nskk-candidate-annotation-face
  '((t :inherit font-lock-comment-face))
  "注釈のフェイス。"
  :group 'nskk-candidate-window)

(defface nskk-candidate-separator-face
  '((t :inherit shadow))
  "区切り文字のフェイス。"
  :group 'nskk-candidate-window)

(defface nskk-candidate-border-face
  '((t :inherit border))
  "枠線のフェイス。"
  :group 'nskk-candidate-window)

;;; データ構造

(cl-defstruct (nskk-candidate-window
               (:constructor nskk-candidate-window--create)
               (:copier nil))
  "候補ウィンドウの状態を保持する構造体。

OVERLAY: 候補表示用のoverlay
CANDIDATES: 候補リスト（文字列またはplist）
SELECTED-INDEX: 現在選択中の候補インデックス（0始まり）
PAGE-SIZE: 1ページあたりの表示候補数
CURRENT-PAGE: 現在表示中のページ番号（0始まり）
POSITION: 表示位置（ポイント位置）"
  (overlay nil)
  (candidates nil :type list)
  (selected-index 0 :type integer)
  (page-size nskk-candidate-page-size :type integer)
  (current-page 0 :type integer)
  (position nil))

;;; 内部変数

(defvar-local nskk-candidate-window--current nil
  "現在表示中の候補ウィンドウ。")

;;; 候補データ処理

(defun nskk-candidate-window--normalize-candidate (candidate)
  "候補データを正規化する。

CANDIDATE: 候補（文字列またはplist）

plist形式の場合:
  (:text \"候補\" :annotation \"注釈\" :priority 0)

文字列を返すか、plistを返す。"
  (cond
   ((stringp candidate)
    ;; 文字列の場合はそのまま
    candidate)
   ((and (listp candidate) (plist-get candidate :text))
    ;; plist形式
    candidate)
   (t
    ;; その他の場合は文字列化
    (format "%s" candidate))))

(defun nskk-candidate-window--get-text (candidate)
  "候補から表示テキストを取得する。

CANDIDATE: 候補データ"
  (if (stringp candidate)
      candidate
    (or (plist-get candidate :text) "")))

(defun nskk-candidate-window--get-annotation (candidate)
  "候補から注釈を取得する。

CANDIDATE: 候補データ"
  (when (listp candidate)
    (when nskk-candidate-show-annotations
      (plist-get candidate :annotation))))

;;; 番号生成

(defun nskk-candidate-window--format-number (index)
  "候補番号をフォーマットする。

INDEX: インデックス（0始まり）"
  (let ((num (1+ index)))
    (pcase nskk-candidate-number-style
      ('arabic (format "%d" num))
      ('alphabet (if (<= num 26)
                    (char-to-string (+ ?a (1- num)))
                  (format "%d" num)))
      (_ (format "%d" num)))))

;;; 候補表示文字列生成

(defun nskk-candidate-window--format-candidate (candidate index selected-p)
  "1つの候補をフォーマットする。

CANDIDATE: 候補データ
INDEX: インデックス（ページ内での0始まり）
SELECTED-P: 選択中かどうか"
  (let* ((text (nskk-candidate-window--get-text candidate))
         (annotation (nskk-candidate-window--get-annotation candidate))
         (number (when nskk-candidate-show-index
                   (nskk-candidate-window--format-number index)))
         (face (if selected-p
                   'nskk-candidate-selected-face
                 'nskk-candidate-face))
         (parts nil))

    ;; 番号
    (when number
      (push (propertize number 'face 'nskk-candidate-number-face) parts)
      (push ":" parts))

    ;; 候補テキスト
    (push (propertize text 'face face) parts)

    ;; 注釈
    (when annotation
      (push (propertize nskk-candidate-annotation-separator
                        'face 'nskk-candidate-separator-face)
            parts)
      (push (propertize annotation 'face 'nskk-candidate-annotation-face)
            parts))

    (apply #'concat (nreverse parts))))

(defun nskk-candidate-window--format-page (window)
  "現在のページの候補を整形する。

WINDOW: 候補ウィンドウ構造体"
  (let* ((page-size (nskk-candidate-window-page-size window))
         (current-page (nskk-candidate-window-current-page window))
         (selected-index (nskk-candidate-window-selected-index window))
         (candidates (nskk-candidate-window-candidates window))
         (start-index (* current-page page-size))
         (end-index (min (+ start-index page-size) (length candidates)))
         (page-candidates (cl-subseq candidates start-index end-index))
         (lines nil))

    ;; 各候補を整形
    (cl-loop for candidate in page-candidates
             for i from start-index
             for page-i from 0
             do (let ((selected-p (= i selected-index)))
                  (push (nskk-candidate-window--format-candidate
                         candidate page-i selected-p)
                        lines)))

    ;; ページ情報を追加
    (let* ((total-pages (ceiling (length candidates) (float page-size)))
           (page-info (when (> total-pages 1)
                        (format "[%d/%d]"
                                (1+ current-page)
                                total-pages))))
      (when page-info
        (push (propertize page-info 'face 'nskk-candidate-number-face)
              lines)))

    (mapconcat #'identity (nreverse lines) "\n")))

;;; 表示位置計算

(defun nskk-candidate-window--calculate-position (position)
  "候補ウィンドウの表示位置を計算する。

POSITION: 基準位置（ポイント位置）

(LINE . COLUMN) の形式で返す。"
  (save-excursion
    (goto-char position)
    (let* ((window (selected-window))
           (window-start (window-start window))
           (window-height (window-height window))
           (current-line (count-lines window-start (point)))
           (current-column (current-column))
           (lines-below (- window-height current-line))
           (margin nskk-candidate-window-margin))

      ;; 下に十分なスペースがあるか確認
      (cons (if (>= lines-below (+ margin 3))
                (1+ current-line)  ;; 下に表示
              (max 0 (- current-line margin 1)))  ;; 上に表示
            current-column))))

;;; overlay操作

(defun nskk-candidate-window--create-overlay (position)
  "候補表示用のoverlayを作成する。

POSITION: 表示位置（ポイント位置）"
  (let* ((pos-info (nskk-candidate-window--calculate-position position))
         (line (car pos-info))
         (overlay-pos (save-excursion
                        (goto-char (window-start))
                        (forward-line line)
                        (point)))
         (overlay (make-overlay overlay-pos overlay-pos)))

    ;; overlay設定
    (overlay-put overlay 'window (selected-window))
    (overlay-put overlay 'priority 1000)
    (overlay-put overlay 'evaporate t)
    overlay))

(defun nskk-candidate-window--update-overlay (overlay content)
  "overlayの表示内容を更新する。

OVERLAY: overlay
CONTENT: 表示する文字列"
  (let* ((before-string (concat content "\n")))
    (overlay-put overlay 'before-string before-string)))

(defun nskk-candidate-window--delete-overlay (overlay)
  "overlayを削除する。

OVERLAY: 削除するoverlay"
  (when (overlayp overlay)
    (delete-overlay overlay)))

;;; 公開API

;;;###autoload
(defun nskk-candidate-window-create (candidates &optional page-size)
  "候補リストから新しい候補ウィンドウ構造を生成する。

CANDIDATES: 候補リスト（文字列またはplist）
PAGE-SIZE: 1ページあたりの表示件数（省略時は `nskk-candidate-page-size'）。"
  (let* ((normalized (mapcar #'nskk-candidate-window--normalize-candidate candidates))
         (size (or page-size nskk-candidate-page-size)))
    (nskk-candidate-window--create
     :candidates normalized
     :selected-index 0
     :page-size size
     :current-page 0
     :position nil)))

;;;###autoload
(defun nskk-candidate-window-select (window index)
  "WINDOW 内で INDEX の候補を選択する。"
  (unless (nskk-candidate-window-p window)
    (error "Not a candidate window: %S" window))
  (let* ((candidates (nskk-candidate-window-candidates window))
         (count (length candidates))
         (clamped (if (> count 0)
                      (max 0 (min index (1- count)))
                    0))
         (page-size (nskk-candidate-window-page-size window))
         (page (if (and (> page-size 0) (> count 0))
                   (/ clamped page-size)
                 0)))
    (setf (nskk-candidate-window-selected-index window) clamped)
    (setf (nskk-candidate-window-current-page window) page)
    window))

;;;###autoload
(defun nskk-candidate-window-current (window)
  "WINDOW で現在選択中の候補の表示テキストを返す。"
  (unless (nskk-candidate-window-p window)
    (error "Not a candidate window: %S" window))
  (let* ((index (nskk-candidate-window-selected-index window))
         (candidates (nskk-candidate-window-candidates window))
         (candidate (nth index candidates)))
    (when candidate
      (nskk-candidate-window--get-text candidate))))

;;;###autoload
(defun nskk-show-candidates (candidates &optional position)
  "候補ウィンドウを表示する。

CANDIDATES: 候補リスト（文字列またはplistのリスト）
POSITION: 表示位置（省略時は現在のポイント位置）"
  (interactive)
  (let* ((pos (or position (point)))
         (normalized-candidates (mapcar #'nskk-candidate-window--normalize-candidate
                                       candidates))
         (window (nskk-candidate-window--create
                  :candidates normalized-candidates
                  :selected-index 0
                  :page-size nskk-candidate-page-size
                  :current-page 0
                  :position pos))
         (overlay (nskk-candidate-window--create-overlay pos))
         (content (nskk-candidate-window--format-page window)))

    (setf (nskk-candidate-window-overlay window) overlay)
    (nskk-candidate-window--update-overlay overlay content)
    (setq nskk-candidate-window--current window)
    window))

;;;###autoload
(defun nskk-hide-candidates ()
  "候補ウィンドウを非表示にする。"
  (interactive)
  (when nskk-candidate-window--current
    (let ((overlay (nskk-candidate-window-overlay
                    nskk-candidate-window--current)))
      (nskk-candidate-window--delete-overlay overlay)
      (setq nskk-candidate-window--current nil))))

;;;###autoload
(defun nskk-update-candidates (candidates selected-index)
  "候補ウィンドウの内容を更新する。

CANDIDATES: 新しい候補リスト
SELECTED-INDEX: 選択中の候補インデックス"
  (interactive)
  (if (not nskk-candidate-window--current)
      (nskk-show-candidates candidates)
    (let* ((window nskk-candidate-window--current)
           (normalized-candidates (mapcar #'nskk-candidate-window--normalize-candidate
                                         candidates))
           (page-size (nskk-candidate-window-page-size window))
           (current-page (/ selected-index page-size))
           (overlay (nskk-candidate-window-overlay window)))

      (setf (nskk-candidate-window-candidates window) normalized-candidates)
      (setf (nskk-candidate-window-selected-index window) selected-index)
      (setf (nskk-candidate-window-current-page window) current-page)

      (let ((content (nskk-candidate-window--format-page window)))
        (nskk-candidate-window--update-overlay overlay content))

      window)))

;;;###autoload
(defun nskk-scroll-candidates (direction)
  "候補ウィンドウをスクロールする。

DIRECTION: スクロール方向
  'next     - 次のページ
  'previous - 前のページ
  'first    - 最初のページ
  'last     - 最後のページ"
  (interactive)
  (unless nskk-candidate-window--current
    (error "候補ウィンドウが表示されていません"))

  (let* ((window nskk-candidate-window--current)
         (candidates (nskk-candidate-window-candidates window))
         (page-size (nskk-candidate-window-page-size window))
         (current-page (nskk-candidate-window-current-page window))
         (total-pages (ceiling (length candidates) (float page-size)))
         (new-page
          (pcase direction
            ('next (min (1+ current-page) (1- total-pages)))
            ('previous (max 0 (1- current-page)))
            ('first 0)
            ('last (1- total-pages))
            (_ current-page)))
         (new-index (* new-page page-size)))

    (when (/= new-page current-page)
      (setf (nskk-candidate-window-current-page window) new-page)
      (setf (nskk-candidate-window-selected-index window) new-index)

      (let* ((overlay (nskk-candidate-window-overlay window))
             (content (nskk-candidate-window--format-page window)))
        (nskk-candidate-window--update-overlay overlay content)))

    window))

;;;###autoload
(defun nskk-select-candidate (index)
  "指定インデックスの候補を選択する。

INDEX: 候補インデックス（0始まり）"
  (interactive)
  (unless nskk-candidate-window--current
    (error "候補ウィンドウが表示されていません"))

  (let* ((window nskk-candidate-window--current)
         (candidates (nskk-candidate-window-candidates window))
         (page-size (nskk-candidate-window-page-size window)))

    (when (and (>= index 0) (< index (length candidates)))
      (let ((new-page (/ index page-size)))
        (setf (nskk-candidate-window-selected-index window) index)
        (setf (nskk-candidate-window-current-page window) new-page)

        (let* ((overlay (nskk-candidate-window-overlay window))
               (content (nskk-candidate-window--format-page window)))
          (nskk-candidate-window--update-overlay overlay content))))

    window))

;;;###autoload
(defun nskk-candidate-window-current-selection ()
  "現在選択中の候補を取得する。

候補データを返す。ウィンドウが表示されていない場合はnil。"
  (when nskk-candidate-window--current
    (let* ((window nskk-candidate-window--current)
           (index (nskk-candidate-window-selected-index window))
           (candidates (nskk-candidate-window-candidates window)))
      (when (< index (length candidates))
        (nth index candidates)))))

;;;###autoload
(defun nskk-candidate-window-visible-p ()
  "候補ウィンドウが表示されているかどうかを返す。"
  (and nskk-candidate-window--current
       (overlayp (nskk-candidate-window-overlay
                  nskk-candidate-window--current))
       t))

;;; ユーティリティ

(defun nskk-candidate-window-next ()
  "次の候補を選択する。"
  (interactive)
  (when nskk-candidate-window--current
    (let* ((window nskk-candidate-window--current)
           (index (nskk-candidate-window-selected-index window))
           (candidates (nskk-candidate-window-candidates window))
           (new-index (min (1+ index) (1- (length candidates)))))
      (nskk-select-candidate new-index))))

(defun nskk-candidate-window-previous ()
  "前の候補を選択する。"
  (interactive)
  (when nskk-candidate-window--current
    (let* ((window nskk-candidate-window--current)
           (index (nskk-candidate-window-selected-index window))
           (new-index (max 0 (1- index))))
      (nskk-select-candidate new-index))))

(defun nskk-candidate-window-page-info ()
  "現在のページ情報を取得する。

(CURRENT-PAGE . TOTAL-PAGES) の形式で返す。"
  (when nskk-candidate-window--current
    (let* ((window nskk-candidate-window--current)
           (page-size (nskk-candidate-window-page-size window))
           (candidates (nskk-candidate-window-candidates window))
           (current-page (nskk-candidate-window-current-page window))
           (total-pages (ceiling (length candidates) (float page-size))))
      (cons (1+ current-page) total-pages))))

(provide 'nskk-candidate-window)
;;; nskk-candidate-window.el ends here
