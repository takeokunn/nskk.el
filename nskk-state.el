;;; nskk-state.el --- State management for NSKK -*- lexical-binding: t; -*-

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

;; このファイルはNSKKの状態管理システムを実装します。
;;
;; 特徴:
;; - cl-defstructによる型安全な状態管理
;; - 明確な状態遷移定義
;; - バッファローカルな状態保持
;; - イベントベースの状態変更
;;
;; 状態の種類:
;; - ひらがな入力モード (hiragana)
;; - カタカナ入力モード (katakana)
;; - 全角英数モード (zenkaku-latin)
;; - 半角英数モード (latin)
;; - abbrevモード (abbrev)
;; - 変換モード (conversion)
;;
;; 使用例:
;; (nskk-state-create :mode 'hiragana)
;; (nskk-state-transition state 'katakana)
;; (nskk-state-current-mode state)

;;; Code:

(require 'cl-lib)

;;; モード定義

(defconst nskk-state-modes
  '(hiragana          ; ひらがな入力モード
    katakana          ; カタカナ入力モード
    zenkaku-latin     ; 全角英数モード
    latin             ; 半角英数モード（ASCII）
    abbrev            ; abbrevモード
    conversion)       ; 変換モード（候補選択中）
  "NSKKで使用可能な全ての入力モード。")

(defconst nskk-state-mode-descriptions
  '((hiragana . "ひらがな")
    (katakana . "カタカナ")
    (zenkaku-latin . "全角英数")
    (latin . "半角英数")
    (abbrev . "Abbrev")
    (conversion . "変換中"))
  "各モードの日本語説明。")

(defconst nskk-state-mode-indicators
  '((hiragana . "あ")
    (katakana . "ア")
    (zenkaku-latin . "Ａ")
    (latin . "A")
    (abbrev . "aあ")
    (conversion . "▼"))
  "各モードのモードライン表示用インジケーター。")

;;; 状態データ構造

(cl-defstruct (nskk-state
               (:constructor nskk-state--create)
               (:copier nskk-state-copy))
  "NSKK入力状態を表す構造体。

スロット:
  mode              - 現在の入力モード（`nskk-state-modes' のいずれか）
  submode           - サブモード（nil または 'okuri-ari, 'okuri-nasi）
  input-buffer      - 未確定入力文字列
  conversion-buffer - 変換対象文字列
  candidates        - 変換候補リスト
  candidate-index   - 現在選択中の候補インデックス
  okuri-char        - 送り仮名の最初の文字
  marker-start      - 入力開始位置（マーカー）
  marker-end        - 入力終了位置（マーカー）
  previous-mode     - 前回のモード（モード復帰用）
  timestamp         - 最終更新時刻
  properties        - 拡張プロパティ（plist形式）"
  (mode 'hiragana
        :type symbol
        :documentation "現在の入力モード")
  (submode nil
           :type (or null symbol)
           :documentation "サブモード（送り仮名の状態）")
  (input-buffer ""
                :type string
                :documentation "未確定のローマ字入力")
  (conversion-buffer ""
                     :type string
                     :documentation "変換対象の文字列")
  (candidates nil
              :type list
              :documentation "変換候補のリスト")
  (candidate-index 0
                   :type integer
                   :documentation "現在選択中の候補インデックス")
  (okuri-char nil
              :type (or null character)
              :documentation "送り仮名の最初の文字")
  (marker-start nil
                :type (or null marker)
                :documentation "入力開始位置のマーカー")
  (marker-end nil
              :type (or null marker)
              :documentation "入力終了位置のマーカー")
  (previous-mode nil
                 :type (or null symbol)
                 :documentation "前回のモード")
  (timestamp (current-time)
             :type list
             :documentation "最終更新時刻")
  (properties nil
              :type list
              :documentation "拡張プロパティ（plist）"))

;;; 状態生成関数

(defun nskk-state-create (&rest args)
  "新しいNSKK状態を生成する。

引数:
  ARGS - cl-defstructのキーワード引数

例:
  (nskk-state-create)
  (nskk-state-create :mode 'katakana)
  (nskk-state-create :mode 'hiragana :input-buffer \"ka\")"
  (let ((mode (or (plist-get args :mode) 'hiragana)))
    (unless (memq mode nskk-state-modes)
      (error "Invalid mode: %s" mode))
    (apply #'nskk-state--create args)))

;;; 状態検査関数

(defun nskk-state-valid-mode-p (mode)
  "MODE が有効なNSKK入力モードかどうかを判定する。"
  (and (symbolp mode)
       (memq mode nskk-state-modes)))

(defun nskk-state-mode-description (mode)
  "MODE の日本語説明を取得する。"
  (or (alist-get mode nskk-state-mode-descriptions)
      (symbol-name mode)))

(defun nskk-state-mode-indicator (mode)
  "MODE のモードライン表示用インジケーターを取得する。"
  (or (alist-get mode nskk-state-mode-indicators)
      "?"))

(defun nskk-state-in-conversion-p (state)
  "STATE が変換モード中かどうかを判定する。"
  (eq (nskk-state-mode state) 'conversion))

(defun nskk-state-has-candidates-p (state)
  "STATE が変換候補を持っているかどうかを判定する。"
  (and (nskk-state-candidates state) t))

(defun nskk-state-current-candidate (state)
  "STATE の現在選択中の候補を取得する。
候補がない場合は nil を返す。"
  (when-let ((candidates (nskk-state-candidates state))
             (index (nskk-state-candidate-index state)))
    (nth index candidates)))

(defun nskk-state-empty-p (state)
  "STATE が空（入力も変換もない）かどうかを判定する。"
  (and (string-empty-p (nskk-state-input-buffer state))
       (string-empty-p (nskk-state-conversion-buffer state))
       (null (nskk-state-candidates state))))

;;; 状態更新関数

(defun nskk-state-update-timestamp (state)
  "STATE のタイムスタンプを現在時刻に更新する。"
  (setf (nskk-state-timestamp state) (current-time))
  state)

(defun nskk-state-set-mode (state mode)
  "STATE のモードを MODE に設定する。
前回のモードを保存し、タイムスタンプを更新する。"
  (unless (nskk-state-valid-mode-p mode)
    (error "Invalid mode: %s" mode))
  (setf (nskk-state-previous-mode state) (nskk-state-mode state))
  (setf (nskk-state-mode state) mode)
  (nskk-state-update-timestamp state))

(defun nskk-state-restore-previous-mode (state)
  "STATE を前回のモードに戻す。
前回のモードがない場合は hiragana モードに戻る。"
  (let ((prev-mode (or (nskk-state-previous-mode state) 'hiragana)))
    (nskk-state-set-mode state prev-mode)))

(defun nskk-state-clear-input (state)
  "STATE の入力バッファをクリアする。"
  (setf (nskk-state-input-buffer state) "")
  (nskk-state-update-timestamp state))

(defun nskk-state-clear-conversion (state)
  "STATE の変換関連データをクリアする。"
  (setf (nskk-state-conversion-buffer state) "")
  (setf (nskk-state-candidates state) nil)
  (setf (nskk-state-candidate-index state) 0)
  (setf (nskk-state-okuri-char state) nil)
  (nskk-state-update-timestamp state))

(defun nskk-state-clear-all (state)
  "STATE の全ての入力・変換データをクリアする。"
  (nskk-state-clear-input state)
  (nskk-state-clear-conversion state))

(defun nskk-state-append-input (state char)
  "STATE の入力バッファに CHAR を追加する。"
  (setf (nskk-state-input-buffer state)
        (concat (nskk-state-input-buffer state)
                (char-to-string char)))
  (nskk-state-update-timestamp state))

(defun nskk-state-set-candidates (state candidates &optional index)
  "STATE に変換候補 CANDIDATES を設定する。
INDEX が指定された場合は、その候補をデフォルト選択にする。"
  (setf (nskk-state-candidates state) candidates)
  (setf (nskk-state-candidate-index state) (or index 0))
  (nskk-state-update-timestamp state))

(defun nskk-state-next-candidate (state)
  "STATE の候補選択を次に進める。
最後の候補の場合は最初に戻る。"
  (when-let ((candidates (nskk-state-candidates state)))
    (let* ((index (nskk-state-candidate-index state))
           (next-index (mod (1+ index) (length candidates))))
      (setf (nskk-state-candidate-index state) next-index)
      (nskk-state-update-timestamp state)
      next-index)))

(defun nskk-state-previous-candidate (state)
  "STATE の候補選択を前に戻す。
最初の候補の場合は最後に戻る。"
  (when-let ((candidates (nskk-state-candidates state)))
    (let* ((index (nskk-state-candidate-index state))
           (prev-index (mod (1- index) (length candidates))))
      (setf (nskk-state-candidate-index state) prev-index)
      (nskk-state-update-timestamp state)
      prev-index)))

;;; 状態遷移定義

(defconst nskk-state-transitions
  '((hiragana . (katakana zenkaku-latin latin abbrev conversion))
    (katakana . (hiragana zenkaku-latin latin abbrev conversion))
    (zenkaku-latin . (hiragana katakana latin abbrev conversion))
    (latin . (hiragana katakana zenkaku-latin abbrev conversion))
    (abbrev . (hiragana katakana zenkaku-latin latin conversion))
    (conversion . (hiragana katakana zenkaku-latin latin abbrev)))
  "各モードから遷移可能なモードのリスト。")

(defun nskk-state-can-transition-p (from-mode to-mode)
  "FROM-MODE から TO-MODE への遷移が可能かどうかを判定する。"
  (and (nskk-state-valid-mode-p from-mode)
       (nskk-state-valid-mode-p to-mode)
       (memq to-mode (alist-get from-mode nskk-state-transitions))))

(defun nskk-state-transition (state to-mode &optional force)
  "STATE を TO-MODE に遷移させる。
FORCE が非nilの場合、遷移可能性チェックをスキップする。

遷移が成功した場合は t を返し、失敗した場合は nil を返す。"
  (let ((from-mode (nskk-state-mode state)))
    (cond
     ;; 同じモードへの遷移は何もしない
     ((eq from-mode to-mode)
      t)

     ;; 強制モードまたは遷移可能な場合
     ((or force (nskk-state-can-transition-p from-mode to-mode))
      (nskk-state-set-mode state to-mode)
      t)

     ;; 遷移不可能
     (t
      (message "Cannot transition from %s to %s" from-mode to-mode)
      nil))))

;;; プロパティ管理

(defun nskk-state-get-property (state key &optional default)
  "STATE の拡張プロパティから KEY の値を取得する。
KEY が存在しない場合は DEFAULT を返す。"
  (or (plist-get (nskk-state-properties state) key)
      default))

(defun nskk-state-set-property (state key value)
  "STATE の拡張プロパティに KEY と VALUE を設定する。"
  (setf (nskk-state-properties state)
        (plist-put (nskk-state-properties state) key value))
  (nskk-state-update-timestamp state))

(defun nskk-state-remove-property (state key)
  "STATE の拡張プロパティから KEY を削除する。"
  (let ((props (nskk-state-properties state)))
    (setf (nskk-state-properties state)
          (cl-loop for (k v) on props by #'cddr
                   unless (eq k key)
                   append (list k v))))
  (nskk-state-update-timestamp state))

;;; マーカー管理

(defun nskk-state-set-markers (state start end)
  "STATE にバッファ位置マーカーを設定する。
START と END は buffer position または marker。"
  (when (nskk-state-marker-start state)
    (set-marker (nskk-state-marker-start state) nil))
  (when (nskk-state-marker-end state)
    (set-marker (nskk-state-marker-end state) nil))

  (setf (nskk-state-marker-start state)
        (if (markerp start) start (copy-marker start)))
  (setf (nskk-state-marker-end state)
        (if (markerp end) end (copy-marker end)))

  (nskk-state-update-timestamp state))

(defun nskk-state-clear-markers (state)
  "STATE のマーカーをクリアする。"
  (when (nskk-state-marker-start state)
    (set-marker (nskk-state-marker-start state) nil)
    (setf (nskk-state-marker-start state) nil))
  (when (nskk-state-marker-end state)
    (set-marker (nskk-state-marker-end state) nil)
    (setf (nskk-state-marker-end state) nil))
  (nskk-state-update-timestamp state))

(defun nskk-state-marker-region (state)
  "STATE のマーカーで示される領域を (start . end) で返す。
マーカーが設定されていない場合は nil を返す。"
  (when (and (nskk-state-marker-start state)
             (nskk-state-marker-end state))
    (cons (marker-position (nskk-state-marker-start state))
          (marker-position (nskk-state-marker-end state)))))

;;; デバッグ・表示機能

(defun nskk-state-to-string (state)
  "STATE を人間が読める文字列に変換する。"
  (format "#<nskk-state mode:%s input:\"%s\" conv:\"%s\" cands:%d>"
          (nskk-state-mode state)
          (nskk-state-input-buffer state)
          (nskk-state-conversion-buffer state)
          (length (nskk-state-candidates state))))

(defun nskk-state-describe (state)
  "STATE の詳細情報を表示する。"
  (message "NSKK State:
  Mode: %s (%s)
  Submode: %s
  Input Buffer: \"%s\"
  Conversion Buffer: \"%s\"
  Candidates: %s
  Current Candidate: %s
  Okuri Char: %s
  Previous Mode: %s
  Timestamp: %s"
           (nskk-state-mode state)
           (nskk-state-mode-description (nskk-state-mode state))
           (nskk-state-submode state)
           (nskk-state-input-buffer state)
           (nskk-state-conversion-buffer state)
           (nskk-state-candidates state)
           (nskk-state-current-candidate state)
           (nskk-state-okuri-char state)
           (nskk-state-previous-mode state)
           (format-time-string "%Y-%m-%d %H:%M:%S"
                               (nskk-state-timestamp state))))

;;; バッファローカル変数

(defvar-local nskk-current-state nil
  "現在のバッファのNSKK入力状態。")

(defun nskk-state-init ()
  "現在のバッファのNSKK状態を初期化する。"
  (unless nskk-current-state
    (setq nskk-current-state (nskk-state-create))))

(defun nskk-state-cleanup ()
  "現在のバッファのNSKK状態をクリーンアップする。"
  (when nskk-current-state
    (nskk-state-clear-markers nskk-current-state)
    (setq nskk-current-state nil)))

(provide 'nskk-state)

;;; nskk-state.el ends here
