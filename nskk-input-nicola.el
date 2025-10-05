;;; nskk-input-nicola.el --- NICOLA (親指シフト) input method for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, nicola, oyayubi
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;; This file is part of NSKK.

;;; Commentary:

;; NICOLA (親指シフト) 入力方式の実装。
;;
;; NICOLAは親指シフトキーボード用の入力方式で、
;; 同時打鍵により効率的な日本語入力を実現する。
;;
;; 主な特徴:
;; - 親指シフトキーとの同時打鍵
;; - 1キー1音の効率的入力
;; - ローマ字入力より少ない打鍵数
;;
;; 注意:
;; この実装は親指シフトキーの同時打鍵をエミュレートします。
;; 物理的な親指シフトキーボードがない環境でも使用可能です。
;;
;; 使用例:
;; (require 'nskk-input-nicola)
;; (nskk-register-input-method 'nicola)

;;; Code:

(require 'nskk-romaji-tables)

;;; NICOLA配列テーブル

;; 無シフト状態
(defconst nskk-input-nicola-base-table
  '(;; 上段
    ("q" . "。")
    ("w" . "か")
    ("e" . "た")
    ("r" . "こ")
    ("t" . "さ")
    ("y" . "ら")
    ("u" . "ち")
    ("i" . "く")
    ("o" . "つ")
    ("p" . "、")

    ;; 中段
    ("a" . "う")
    ("s" . "し")
    ("d" . "て")
    ("f" . "け")
    ("g" . "せ")
    ("h" . "は")
    ("j" . "と")
    ("k" . "き")
    ("l" . "い")
    (";" . "ん")

    ;; 下段
    ("x" . "ひ")
    ("c" . "す")
    ("v" . "ふ")
    ("b" . "へ")
    ("n" . "め")
    ("m" . "そ")
    ("," . "ね")
    ("." . "ほ")
    ("/" . "・"))
  "NICOLA配列の無シフト状態のテーブル。")

;; 左親指シフト状態
(defconst nskk-input-nicola-left-shift-table
  '(;; 上段
    ("q" . "ぁ")
    ("w" . "が")
    ("e" . "だ")
    ("r" . "ご")
    ("t" . "ざ")
    ("y" . "よ")
    ("u" . "に")
    ("i" . "る")
    ("o" . "ま")
    ("p" . "ぇ")

    ;; 中段
    ("a" . "を")
    ("s" . "あ")
    ("d" . "な")
    ("f" . "ゆ")
    ("g" . "も")
    ("h" . "わ")
    ("j" . "ほ")
    ("k" . "れ")
    ("l" . "た")
    (";" . "゛")

    ;; 下段
    ("z" . "ぅ")
    ("x" . "ー")
    ("c" . "ろ")
    ("v" . "や")
    ("b" . "ぃ")
    ("n" . "ぬ")
    ("m" . "ゅ")
    ("," . "む")
    ("." . "ゎ")
    ("/" . "ょ"))
  "NICOLA配列の左親指シフト状態のテーブル。")

;; 右親指シフト状態
(defconst nskk-input-nicola-right-shift-table
  '(;; 上段
    ("q" . "ぉ")
    ("w" . "り")
    ("e" . "の")
    ("r" . "く")
    ("t" . "つ")
    ("y" . "ぱ")
    ("u" . "ぢ")
    ("i" . "ぐ")
    ("o" . "づ")
    ("p" . "ぴ")

    ;; 中段
    ("a" . "ぃ")
    ("s" . "ら")
    ("d" . "か")
    ("f" . "ば")
    ("g" . "ど")
    ("h" . "げ")
    ("j" . "さ")
    ("k" . "ぎ")
    ("l" . "す")
    (";" . "゜")

    ;; 下段
    ("z" . "ぇ")
    ("x" . "じ")
    ("c" . "ぞ")
    ("v" . "ぶ")
    ("b" . "べ")
    ("n" . "ぬ")
    ("m" . "ぺ")
    ("," . "ぼ")
    ("." . "ぽ")
    ("/" . "っ"))
  "NICOLA配列の右親指シフト状態のテーブル。")

;;; 統合テーブル

(defconst nskk-input-nicola-table
  (append nskk-input-nicola-base-table
          nskk-input-nicola-left-shift-table
          nskk-input-nicola-right-shift-table)
  "NICOLA入力方式の全状態を含むテーブル。")

;;; ハッシュテーブル

(defvar nskk-input-nicola-hash-table nil
  "NICOLA入力方式用のハッシュテーブル（無シフト）。")

(defvar nskk-input-nicola-left-shift-hash-table nil
  "NICOLA入力方式用のハッシュテーブル（左シフト）。")

(defvar nskk-input-nicola-right-shift-hash-table nil
  "NICOLA入力方式用のハッシュテーブル（右シフト）。")

(defun nskk-input-nicola-init-hash-tables ()
  "NICOLA入力方式用のハッシュテーブルを初期化する。"
  ;; 無シフト
  (setq nskk-input-nicola-hash-table
        (make-hash-table :test 'equal :size 50))
  (dolist (entry nskk-input-nicola-base-table)
    (puthash (car entry) (cdr entry) nskk-input-nicola-hash-table))

  ;; 左シフト
  (setq nskk-input-nicola-left-shift-hash-table
        (make-hash-table :test 'equal :size 50))
  (dolist (entry nskk-input-nicola-left-shift-table)
    (puthash (car entry) (cdr entry) nskk-input-nicola-left-shift-hash-table))

  ;; 右シフト
  (setq nskk-input-nicola-right-shift-hash-table
        (make-hash-table :test 'equal :size 50))
  (dolist (entry nskk-input-nicola-right-shift-table)
    (puthash (car entry) (cdr entry) nskk-input-nicola-right-shift-hash-table)))

;;; 検索関数

(defun nskk-input-nicola-lookup (key &optional shift-state)
  "NICOLA入力方式でKEYをかなに変換する。
SHIFT-STATEは 'left, 'right, または nil（無シフト）。

使用例:
  (nskk-input-nicola-lookup \"k\" nil)      ;; => \"き\"
  (nskk-input-nicola-lookup \"k\" 'left)   ;; => \"れ\"
  (nskk-input-nicola-lookup \"k\" 'right)  ;; => \"ぎ\""
  (unless nskk-input-nicola-hash-table
    (nskk-input-nicola-init-hash-tables))
  (cond
   ((eq shift-state 'left)
    (gethash key nskk-input-nicola-left-shift-hash-table))
   ((eq shift-state 'right)
    (gethash key nskk-input-nicola-right-shift-hash-table))
   (t
    (gethash key nskk-input-nicola-hash-table))))

(defun nskk-input-nicola-get-candidates (prefix &optional shift-state)
  "PREFIX で始まる全てのNICOLA候補を取得する。
SHIFT-STATEは 'left, 'right, または nil。"
  (let ((table (cond
                ((eq shift-state 'left) nskk-input-nicola-left-shift-table)
                ((eq shift-state 'right) nskk-input-nicola-right-shift-table)
                (t nskk-input-nicola-base-table)))
        (candidates nil))
    (dolist (entry table)
      (when (string-prefix-p prefix (car entry))
        (push (car entry) candidates)))
    (nreverse candidates)))

;;; 入力方式登録

(defun nskk-input-nicola-register ()
  "NICOLA入力方式をNSKKに登録する。"
  (nskk-input-nicola-init-hash-tables))

;;; 同時打鍵判定機能

(defcustom nskk-input-nicola-simultaneous-window 0.1
  "同時打鍵判定ウィンドウ(秒)。
親指シフトキーと通常キーの同時打鍵を判定する時間間隔。
デフォルトは100ms。"
  :type 'float
  :group 'nskk)

(defvar nskk-input-nicola--last-key-time nil
  "最後のキー入力時刻。
`float-time' 形式のタイムスタンプ。")

(defvar nskk-input-nicola--last-key nil
  "最後に入力されたキー。
同時打鍵判定に使用される。")

(defvar nskk-input-nicola--pending-shift nil
  "保留中のシフト状態。
'left, 'right, または nil。")

(defun nskk-input-nicola-process (key &optional shift-state)
  "KEYを処理する。
SHIFT-STATEは 'left, 'right, または nil（無シフト）。

同時打鍵判定:
- 前回のキー入力から `nskk-input-nicola-simultaneous-window' 秒以内の場合、
  同時打鍵として処理される。
- それ以外の場合、通常の打鍵として処理される。

返り値:
- 変換されたかな文字列
- nil（変換不可能な場合）

使用例:
  (nskk-input-nicola-process \"k\")           ;; => \"き\"
  (nskk-input-nicola-process \"k\" 'left)    ;; => \"れ\"
  (nskk-input-nicola-process \"k\" 'right)   ;; => \"ぎ\""
  (let ((current-time (float-time)))
    ;; 同時打鍵判定
    (if (and nskk-input-nicola--last-key-time
             (< (- current-time nskk-input-nicola--last-key-time)
                nskk-input-nicola-simultaneous-window))
        ;; 同時打鍵処理
        (prog1
            (nskk-input-nicola--simultaneous-key key shift-state)
          ;; 同時打鍵後は状態をリセット
          (setq nskk-input-nicola--last-key-time nil
                nskk-input-nicola--last-key nil
                nskk-input-nicola--pending-shift nil))
      ;; 通常打鍵処理
      (prog1
          (nskk-input-nicola--normal-key key shift-state)
        ;; 次の同時打鍵判定のために状態を保存
        (setq nskk-input-nicola--last-key-time current-time
              nskk-input-nicola--last-key key
              nskk-input-nicola--pending-shift shift-state)))))

(defun nskk-input-nicola--simultaneous-key (key shift-state)
  "同時打鍵されたKEYを処理する。
SHIFT-STATEまたは `nskk-input-nicola--pending-shift' に基づいて変換する。"
  (let ((actual-shift (or shift-state nskk-input-nicola--pending-shift)))
    (nskk-input-nicola-lookup key actual-shift)))

(defun nskk-input-nicola--normal-key (key shift-state)
  "通常打鍵されたKEYを処理する。"
  (nskk-input-nicola-lookup key shift-state))

(defun nskk-input-nicola-reset-state ()
  "NICOLA入力の状態をリセットする。
同時打鍵判定のための内部状態をクリアする。"
  (setq nskk-input-nicola--last-key-time nil
        nskk-input-nicola--last-key nil
        nskk-input-nicola--pending-shift nil))

;;; 統計情報

(defun nskk-input-nicola-stats ()
  "NICOLA入力方式の統計情報を返す。"
  (list :total (length nskk-input-nicola-table)
        :base (length nskk-input-nicola-base-table)
        :left-shift (length nskk-input-nicola-left-shift-table)
        :right-shift (length nskk-input-nicola-right-shift-table)
        :simultaneous-window nskk-input-nicola-simultaneous-window))

;;; パフォーマンス最適化

(defsubst nskk-input-nicola-lookup-fast (key shift-state)
  "NICOLA入力方式でKEYを高速変換する。"
  (cond
   ((eq shift-state 'left)
    (gethash key nskk-input-nicola-left-shift-hash-table))
   ((eq shift-state 'right)
    (gethash key nskk-input-nicola-right-shift-hash-table))
   (t
    (gethash key nskk-input-nicola-hash-table))))

(provide 'nskk-input-nicola)

;;; nskk-input-nicola.el ends here
