;;; nskk-input-kana.el --- Kana (かな) direct input method for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, kana
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;; This file is part of NSKK.

;;; Commentary:

;; かな入力方式の実装（JISかな配列）。
;;
;; かな入力はキーボードに刻印されたかな文字を
;; 直接入力する方式。ローマ字入力より打鍵数が少ない。
;;
;; 使用例:
;; (require 'nskk-input-kana)
;; (nskk-register-input-method 'kana)

;;; Code:

(require 'nskk-romaji-tables)

;;; JISかな配列テーブル

(defconst nskk-input-kana-table
  '(;; 数字行
    ("1" . "ぬ")
    ("2" . "ふ")
    ("3" . "あ")
    ("4" . "う")
    ("5" . "え")
    ("6" . "お")
    ("7" . "や")
    ("8" . "ゆ")
    ("9" . "よ")
    ("0" . "わ")
    ("-" . "ほ")
    ("^" . "へ")

    ;; 上段
    ("q" . "た")
    ("w" . "て")
    ("e" . "い")
    ("r" . "す")
    ("t" . "か")
    ("y" . "ん")
    ("u" . "な")
    ("i" . "に")
    ("o" . "ら")
    ("p" . "せ")
    ("@" . "゛")
    ("[" . "゜")

    ;; 中段
    ("a" . "ち")
    ("s" . "と")
    ("d" . "し")
    ("f" . "は")
    ("g" . "き")
    ("h" . "く")
    ("j" . "ま")
    ("k" . "の")
    ("l" . "り")
    (";" . "れ")
    (":" . "け")
    ("]" . "む")

    ;; 下段
    ("z" . "つ")
    ("x" . "さ")
    ("c" . "そ")
    ("v" . "ひ")
    ("b" . "こ")
    ("n" . "み")
    ("m" . "も")
    ("," . "ね")
    ("." . "る")
    ("/" . "め")
    ("\\" . "ろ")

    ;; シフト状態（濁音・半濁音）
    ("!" . "ぬ")  ; Shift+1
    ("\"" . "ふ")  ; Shift+2
    ("#" . "ぁ")   ; Shift+3
    ("$" . "ぅ")   ; Shift+4
    ("%" . "ぇ")   ; Shift+5
    ("&" . "ぉ")   ; Shift+6
    ("'" . "ゃ")   ; Shift+7
    ("(" . "ゅ")   ; Shift+8
    (")" . "ょ")   ; Shift+9
    ("=" . "を")   ; Shift+0
    ("~" . "ー")   ; Shift+-

    ("Q" . "た")  ; 大文字は同じ
    ("W" . "て")
    ("E" . "ぃ")
    ("R" . "す")
    ("T" . "が")
    ("Y" . "ん")
    ("U" . "な")
    ("I" . "に")
    ("O" . "ら")
    ("P" . "ぜ")
    ("`" . "「")
    ("{" . "」")

    ("A" . "ぢ")
    ("S" . "ど")
    ("D" . "じ")
    ("F" . "ば")
    ("G" . "ぎ")
    ("H" . "ぐ")
    ("J" . "ま")
    ("K" . "の")
    ("L" . "り")
    ("+" . "げ")
    ("*" . "け")
    ("}" . "む")

    ("Z" . "っ")
    ("X" . "ざ")
    ("C" . "ぞ")
    ("V" . "び")
    ("B" . "ご")
    ("N" . "み")
    ("M" . "も")
    ("<" . "、")
    (">" . "。")
    ("?" . "・")
    ("_" . "ろ"))
  "JISかな配列のテーブル。")

;;; ハッシュテーブル

(defvar nskk-input-kana-hash-table nil
  "かな入力方式用のハッシュテーブル。")

(defun nskk-input-kana-init-hash-table ()
  "かな入力方式用のハッシュテーブルを初期化する。"
  (setq nskk-input-kana-hash-table
        (make-hash-table :test 'equal :size 150))
  (dolist (entry nskk-input-kana-table)
    (puthash (car entry) (cdr entry) nskk-input-kana-hash-table)))

;;; 検索関数

(defun nskk-input-kana-lookup (key)
  "かな入力方式でKEYをかなに変換する。

使用例:
  (nskk-input-kana-lookup \"k\")   ;; => \"の\"
  (nskk-input-kana-lookup \"a\")   ;; => \"ち\""
  (unless nskk-input-kana-hash-table
    (nskk-input-kana-init-hash-table))
  (gethash key nskk-input-kana-hash-table))

(defun nskk-input-kana-get-candidates (prefix)
  "PREFIX で始まる全てのかな入力候補を取得する。"
  (let ((candidates nil))
    (dolist (entry nskk-input-kana-table)
      (when (string-prefix-p prefix (car entry))
        (push (car entry) candidates)))
    (nreverse candidates)))

;;; 入力方式登録

(defun nskk-input-kana-register ()
  "かな入力方式をNSKKに登録する。"
  (nskk-input-kana-init-hash-table))

;;; 統計情報

(defun nskk-input-kana-stats ()
  "かな入力方式の統計情報を返す。"
  (list :total (length nskk-input-kana-table)))

;;; パフォーマンス最適化

(defsubst nskk-input-kana-lookup-fast (key)
  "かな入力方式でKEYを高速変換する。"
  (gethash key nskk-input-kana-hash-table))

(provide 'nskk-input-kana)

;;; nskk-input-kana.el ends here
