;;; nskk-minibuffer.el --- Minibuffer UI for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk
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

;; このファイルはNSKKのミニバッファUI機能を実装します。
;;
;; 特徴:
;; - インライン候補表示（グレーアウトで表示）
;; - 入力文字列の装飾（フェイスによる視覚的フィードバック）
;; - カーソル位置管理
;; - モード別プロンプト表示
;; - 状態表示（▽▼等のインジケーター）
;;
;; インライン候補:
;; - 候補をグレー表示してTABで確定可能
;; - リアルタイムで更新
;; - カスタマイズ可能なフェイス
;;
;; 使用例:
;; (nskk-minibuffer-show "あ" 'nskk-minibuffer-hiragana-face)
;; (nskk-minibuffer-show-inline-candidate "愛")
;; (nskk-minibuffer-update "あい")
;; (nskk-minibuffer-hide)

;;; Code:

(require 'cl-lib)
(require 'nskk-state)

;;; カスタマイズグループ

(defgroup nskk-minibuffer nil
  "NSKK minibuffer UI settings."
  :group 'nskk
  :prefix "nskk-minibuffer-")

;;; カスタマイズ変数

(defcustom nskk-minibuffer-inline-face 'shadow
  "インライン候補表示に使用するフェイス。"
  :type 'face
  :group 'nskk-minibuffer)

(defcustom nskk-minibuffer-enable-inline t
  "非nilの場合、インライン候補を表示する。"
  :type 'boolean
  :group 'nskk-minibuffer)

(defcustom nskk-minibuffer-hiragana-face 'default
  "ひらがな入力時のテキストフェイス。"
  :type 'face
  :group 'nskk-minibuffer)

(defcustom nskk-minibuffer-katakana-face 'default
  "カタカナ入力時のテキストフェイス。"
  :type 'face
  :group 'nskk-minibuffer)

(defcustom nskk-minibuffer-conversion-face 'highlight
  "変換中のテキストフェイス。"
  :type 'face
  :group 'nskk-minibuffer)

(defcustom nskk-minibuffer-prompt-face 'minibuffer-prompt
  "プロンプト表示に使用するフェイス。"
  :type 'face
  :group 'nskk-minibuffer)

(defcustom nskk-minibuffer-show-mode-indicator t
  "非nilの場合、モードインジケーターを表示する。"
  :type 'boolean
  :group 'nskk-minibuffer)

(defcustom nskk-minibuffer-mode-indicators
  '((hiragana . "▽")
    (katakana . "▽")
    (zenkaku-latin . "▽")
    (latin . "▽")
    (abbrev . "▽")
    (conversion . "▼"))
  "各モードのミニバッファインジケーター。"
  :type '(alist :key-type symbol :value-type string)
  :group 'nskk-minibuffer)

(defcustom nskk-minibuffer-use-overlay t
  "非nilの場合、overlayを使用してテキストを装飾する。
nilの場合、text propertiesを使用する。"
  :type 'boolean
  :group 'nskk-minibuffer)

;;; ミニバッファ状態データ構造

(cl-defstruct (nskk-minibuffer-state
               (:constructor nskk-minibuffer-state--create)
               (:copier nskk-minibuffer-state-copy))
  "ミニバッファUIの状態を表す構造体。

スロット:
  text              - 表示中のテキスト
  inline-candidate  - インライン候補（nil または文字列）
  prompt            - プロンプト文字列
  overlay           - テキスト装飾用のオーバーレイ
  inline-overlay    - インライン候補用のオーバーレイ
  mode              - 現在のモード（hiragana, katakana, 等）
  cursor-pos        - カーソル位置
  visible           - 表示中かどうか"
  (text "" :type string)
  (inline-candidate nil :type (or null string))
  (prompt "" :type string)
  (overlay nil)
  (inline-overlay nil)
  (mode 'hiragana :type symbol)
  (cursor-pos 0 :type integer)
  (visible nil :type boolean))

;;; グローバル状態

(defvar nskk-minibuffer--state nil
  "現在のミニバッファ状態。")

;;; 初期化・クリーンアップ

(defun nskk-minibuffer-init ()
  "ミニバッファUIを初期化する。"
  (unless nskk-minibuffer--state
    (setq nskk-minibuffer--state (nskk-minibuffer-state--create))))

(defun nskk-minibuffer-cleanup ()
  "ミニバッファUIのリソースをクリーンアップする。"
  (when nskk-minibuffer--state
    ;; オーバーレイの削除
    (when (nskk-minibuffer-state-overlay nskk-minibuffer--state)
      (delete-overlay (nskk-minibuffer-state-overlay nskk-minibuffer--state)))
    (when (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state)
      (delete-overlay (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state)))
    (setq nskk-minibuffer--state nil)))

;;; オーバーレイ管理

(defun nskk-minibuffer--ensure-overlay ()
  "テキスト装飾用のオーバーレイを確保する。"
  (nskk-minibuffer-init)
  (unless (and (nskk-minibuffer-state-overlay nskk-minibuffer--state)
               (overlay-buffer (nskk-minibuffer-state-overlay nskk-minibuffer--state)))
    (let ((ov (make-overlay (point-min) (point-min))))
      (overlay-put ov 'nskk-minibuffer t)
      (setf (nskk-minibuffer-state-overlay nskk-minibuffer--state) ov))))

(defun nskk-minibuffer--ensure-inline-overlay ()
  "インライン候補用のオーバーレイを確保する。"
  (nskk-minibuffer-init)
  (unless (and (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state)
               (overlay-buffer (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state)))
    (let ((ov (make-overlay (point-min) (point-min))))
      (overlay-put ov 'nskk-minibuffer-inline t)
      (setf (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state) ov))))

(defun nskk-minibuffer--delete-overlay (overlay)
  "OVERLAY を安全に削除する。"
  (when (and overlay (overlay-buffer overlay))
    (delete-overlay overlay)))

;;; フェイス選択

(defun nskk-minibuffer--select-face (mode)
  "MODE に応じて適切なフェイスを選択する。"
  (pcase mode
    ('hiragana nskk-minibuffer-hiragana-face)
    ('katakana nskk-minibuffer-katakana-face)
    ('zenkaku-latin nskk-minibuffer-hiragana-face)
    ('latin nskk-minibuffer-hiragana-face)
    ('abbrev nskk-minibuffer-hiragana-face)
    ('conversion nskk-minibuffer-conversion-face)
    (_ 'default)))

;;; プロンプト生成

(defun nskk-minibuffer--make-prompt (mode)
  "MODE に応じたプロンプト文字列を生成する。"
  (when nskk-minibuffer-show-mode-indicator
    (let ((indicator (or (alist-get mode nskk-minibuffer-mode-indicators) "▽")))
      (propertize indicator 'face nskk-minibuffer-prompt-face))))

;;; ミニバッファ表示

(defun nskk-minibuffer-show (text &optional face mode)
  "ミニバッファに TEXT を表示する。

引数:
  TEXT - 表示するテキスト
  FACE - 使用するフェイス（オプション）
  MODE - 現在のモード（オプション）

副作用:
  - ミニバッファへのテキスト挿入
  - オーバーレイの適用
  - 状態の更新"
  (nskk-minibuffer-init)

  (let* ((actual-mode (or mode 'hiragana))
         (actual-face (or face (nskk-minibuffer--select-face actual-mode)))
         (prompt (nskk-minibuffer--make-prompt actual-mode)))

    ;; 状態更新
    (setf (nskk-minibuffer-state-text nskk-minibuffer--state) text)
    (setf (nskk-minibuffer-state-mode nskk-minibuffer--state) actual-mode)
    (setf (nskk-minibuffer-state-prompt nskk-minibuffer--state) (or prompt ""))
    (setf (nskk-minibuffer-state-visible nskk-minibuffer--state) t)

    ;; overlayを使用する場合
    (when nskk-minibuffer-use-overlay
      (nskk-minibuffer--ensure-overlay)
      (let ((ov (nskk-minibuffer-state-overlay nskk-minibuffer--state)))
        ;; オーバーレイ位置の更新は呼び出し側が行う
        (overlay-put ov 'face actual-face)
        (when prompt
          (overlay-put ov 'before-string prompt))))))

(defun nskk-minibuffer-hide ()
  "ミニバッファの表示を隠す。

副作用:
  - オーバーレイの削除
  - 状態のクリア"
  (when nskk-minibuffer--state
    ;; オーバーレイの削除
    (when-let ((ov (nskk-minibuffer-state-overlay nskk-minibuffer--state)))
      (nskk-minibuffer--delete-overlay ov)
      (setf (nskk-minibuffer-state-overlay nskk-minibuffer--state) nil))

    (when-let ((ov (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state)))
      (nskk-minibuffer--delete-overlay ov)
      (setf (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state) nil))

    ;; 状態クリア
    (setf (nskk-minibuffer-state-text nskk-minibuffer--state) "")
    (setf (nskk-minibuffer-state-inline-candidate nskk-minibuffer--state) nil)
    (setf (nskk-minibuffer-state-visible nskk-minibuffer--state) nil)))

(defun nskk-minibuffer-update (text &optional face mode)
  "表示中のテキストを更新する。

引数:
  TEXT - 新しいテキスト
  FACE - 使用するフェイス（オプション）
  MODE - 現在のモード（オプション）

返り値:
  更新に成功した場合は t、ミニバッファが非表示の場合は nil"
  (if (and nskk-minibuffer--state
           (nskk-minibuffer-state-visible nskk-minibuffer--state))
      (progn
        (nskk-minibuffer-show text face mode)
        t)
    nil))

;;; インライン候補表示

(defun nskk-minibuffer-show-inline-candidate (candidate)
  "インライン候補 CANDIDATE を表示する。

引数:
  CANDIDATE - 表示する候補文字列

副作用:
  - インライン候補の表示
  - after-string プロパティの設定

インライン候補はカーソル位置の後にグレー表示される。
TABキーで確定可能。"
  (when (and nskk-minibuffer-enable-inline
             nskk-minibuffer--state
             (nskk-minibuffer-state-visible nskk-minibuffer--state)
             candidate
             (not (string-empty-p candidate)))

    (nskk-minibuffer--ensure-inline-overlay)

    (let ((ov (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state))
          (inline-text (propertize candidate 'face nskk-minibuffer-inline-face)))

      ;; after-stringを使用してインライン候補を表示
      (overlay-put ov 'after-string inline-text)
      (setf (nskk-minibuffer-state-inline-candidate nskk-minibuffer--state) candidate))))

(defun nskk-minibuffer-hide-inline-candidate ()
  "インライン候補の表示を隠す。

副作用:
  - インライン候補オーバーレイの削除
  - 状態のクリア"
  (when nskk-minibuffer--state
    (when-let ((ov (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state)))
      (overlay-put ov 'after-string nil)
      (nskk-minibuffer--delete-overlay ov)
      (setf (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state) nil))

    (setf (nskk-minibuffer-state-inline-candidate nskk-minibuffer--state) nil)))

(defun nskk-minibuffer-update-inline-candidate (candidate)
  "インライン候補を更新する。

引数:
  CANDIDATE - 新しい候補文字列

返り値:
  更新に成功した場合は t、失敗した場合は nil"
  (if (and nskk-minibuffer--state
           (nskk-minibuffer-state-visible nskk-minibuffer--state))
      (progn
        (if (and candidate (not (string-empty-p candidate)))
            (nskk-minibuffer-show-inline-candidate candidate)
          (nskk-minibuffer-hide-inline-candidate))
        t)
    nil))

(defun nskk-minibuffer-accept-inline-candidate ()
  "インライン候補を確定する。

返り値:
  確定した候補文字列、候補がない場合は nil

副作用:
  - インライン候補の隠蔽"
  (when-let ((candidate (and nskk-minibuffer--state
                             (nskk-minibuffer-state-inline-candidate nskk-minibuffer--state))))
    (nskk-minibuffer-hide-inline-candidate)
    candidate))

;;; プロンプト操作

(defun nskk-minibuffer-set-prompt (prompt &optional face)
  "プロンプトを設定する。

引数:
  PROMPT - プロンプト文字列
  FACE   - プロンプトのフェイス（オプション）

副作用:
  - プロンプトの更新
  - オーバーレイのbefore-string更新"
  (nskk-minibuffer-init)

  (let ((prompt-text (if face
                         (propertize prompt 'face face)
                       prompt)))
    (setf (nskk-minibuffer-state-prompt nskk-minibuffer--state) prompt-text)

    (when (and nskk-minibuffer-use-overlay
               (nskk-minibuffer-state-overlay nskk-minibuffer--state))
      (overlay-put (nskk-minibuffer-state-overlay nskk-minibuffer--state)
                   'before-string prompt-text))))

(defun nskk-minibuffer-update-prompt (mode)
  "MODE に基づいてプロンプトを更新する。

引数:
  MODE - 入力モード"
  (let ((prompt (nskk-minibuffer--make-prompt mode)))
    (when prompt
      (nskk-minibuffer-set-prompt prompt))))

;;; カーソル位置管理

(defun nskk-minibuffer-set-cursor-pos (pos)
  "カーソル位置を設定する。

引数:
  POS - カーソル位置（文字数）"
  (nskk-minibuffer-init)
  (setf (nskk-minibuffer-state-cursor-pos nskk-minibuffer--state) pos))

(defun nskk-minibuffer-get-cursor-pos ()
  "現在のカーソル位置を取得する。

返り値:
  カーソル位置（文字数）"
  (if nskk-minibuffer--state
      (nskk-minibuffer-state-cursor-pos nskk-minibuffer--state)
    0))

;;; 状態問い合わせ

(defun nskk-minibuffer-visible-p ()
  "ミニバッファが表示中かどうかを判定する。

返り値:
  表示中の場合は t、そうでない場合は nil"
  (and nskk-minibuffer--state
       (nskk-minibuffer-state-visible nskk-minibuffer--state)))

(defun nskk-minibuffer-has-inline-candidate-p ()
  "インライン候補が表示中かどうかを判定する。

返り値:
  インライン候補がある場合は t、そうでない場合は nil"
  (and nskk-minibuffer--state
       (nskk-minibuffer-state-inline-candidate nskk-minibuffer--state)
       t))

(defun nskk-minibuffer-current-text ()
  "現在表示中のテキストを取得する。

返り値:
  表示中のテキスト、非表示の場合は nil"
  (when (and nskk-minibuffer--state
             (nskk-minibuffer-state-visible nskk-minibuffer--state))
    (nskk-minibuffer-state-text nskk-minibuffer--state)))

(defun nskk-minibuffer-current-inline-candidate ()
  "現在のインライン候補を取得する。

返り値:
  インライン候補文字列、なければ nil"
  (when nskk-minibuffer--state
    (nskk-minibuffer-state-inline-candidate nskk-minibuffer--state)))

;;; 統合関数

(defun nskk-minibuffer-show-with-candidate (text candidate &optional mode)
  "テキストとインライン候補を同時に表示する。

引数:
  TEXT      - 表示するテキスト
  CANDIDATE - インライン候補
  MODE      - 入力モード（オプション）

これはnskk-minibuffer-showとnskk-minibuffer-show-inline-candidateを
まとめて呼び出す便利関数。"
  (nskk-minibuffer-show text nil mode)
  (when (and candidate (not (string-empty-p candidate)))
    (nskk-minibuffer-show-inline-candidate candidate)))

;;; デバッグ・統計

(defun nskk-minibuffer-stats ()
  "ミニバッファUIの統計情報を返す。

返り値:
  統計情報を含むplist:
    :visible            - 表示中かどうか
    :text               - 表示中のテキスト
    :inline-candidate   - インライン候補
    :prompt             - プロンプト
    :mode               - 現在のモード
    :cursor-pos         - カーソル位置
    :overlay-active     - オーバーレイの有無
    :inline-overlay-active - インラインオーバーレイの有無"
  (if nskk-minibuffer--state
      (list :visible (nskk-minibuffer-state-visible nskk-minibuffer--state)
            :text (nskk-minibuffer-state-text nskk-minibuffer--state)
            :inline-candidate (nskk-minibuffer-state-inline-candidate nskk-minibuffer--state)
            :prompt (nskk-minibuffer-state-prompt nskk-minibuffer--state)
            :mode (nskk-minibuffer-state-mode nskk-minibuffer--state)
            :cursor-pos (nskk-minibuffer-state-cursor-pos nskk-minibuffer--state)
            :overlay-active (and (nskk-minibuffer-state-overlay nskk-minibuffer--state)
                                 (overlay-buffer (nskk-minibuffer-state-overlay nskk-minibuffer--state)))
            :inline-overlay-active (and (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state)
                                        (overlay-buffer (nskk-minibuffer-state-inline-overlay nskk-minibuffer--state))))
    (list :visible nil)))

(defun nskk-minibuffer-describe ()
  "ミニバッファUIの現在状態を表示する。"
  (interactive)
  (let ((stats (nskk-minibuffer-stats)))
    (message "NSKK Minibuffer:
  Visible: %s
  Text: %s
  Inline Candidate: %s
  Prompt: %s
  Mode: %s
  Cursor Pos: %d
  Overlay: %s
  Inline Overlay: %s"
             (if (plist-get stats :visible) "yes" "no")
             (or (plist-get stats :text) "")
             (or (plist-get stats :inline-candidate) "none")
             (or (plist-get stats :prompt) "")
             (plist-get stats :mode)
             (plist-get stats :cursor-pos)
             (if (plist-get stats :overlay-active) "active" "inactive")
             (if (plist-get stats :inline-overlay-active) "active" "inactive"))))

(provide 'nskk-minibuffer)

;;; nskk-minibuffer.el ends here
