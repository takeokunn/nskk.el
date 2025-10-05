;;; nskk-input-dvorak.el --- Dvorak input method for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, dvorak
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;; This file is part of NSKK.

;;; Commentary:

;; Dvorak配列用の日本語入力方式。
;;
;; Dvorak配列でローマ字入力を行う際の変換テーブル。
;; Dvorak配列のキー配置に最適化されています。
;;
;; 使用例:
;; (require 'nskk-input-dvorak)
;; (nskk-register-input-method 'dvorak)

;;; Code:

(require 'nskk-romaji-tables)

;;; Dvorak配列用の変換テーブル
;; Dvorak配列でのキー位置からローマ字への変換

(defconst nskk-input-dvorak-key-map
  '(;; QWERTYのキーをDvorakのキーにマッピング
    ;; 上段
    ("'" . "q")
    ("," . "w")
    ("." . "e")
    ("p" . "r")
    ("y" . "t")
    ("f" . "y")
    ("g" . "u")
    ("c" . "i")
    ("r" . "o")
    ("l" . "p")

    ;; 中段
    ("a" . "a")
    ("o" . "s")
    ("e" . "d")
    ("u" . "f")
    ("i" . "g")
    ("d" . "h")
    ("h" . "j")
    ("t" . "k")
    ("n" . "l")
    ("s" . ";")

    ;; 下段
    (";" . "z")
    ("q" . "x")
    ("j" . "c")
    ("k" . "v")
    ("x" . "b")
    ("b" . "n")
    ("m" . "m")
    ("w" . ",")
    ("v" . ".")
    ("z" . "/"))
  "Dvorak配列からQWERTY配列へのキーマッピング。")

;;; Dvorak用のローマ字テーブル生成

(defun nskk-input-dvorak-convert-table ()
  "Dvorak配列用のローマ字テーブルを生成する。
QWERTYベースのローマ字テーブルをDvorak配列に変換。"
  (let ((dvorak-table nil))
    (dolist (entry nskk-romaji-table)
      (let ((romaji (car entry))
            (kana (cdr entry))
            (converted ""))
        ;; ローマ字の各文字をDvorakキーに変換
        (dotimes (i (length romaji))
          (let* ((char (substring romaji i (1+ i)))
                 (dvorak-char (or (car (rassoc char nskk-input-dvorak-key-map))
                                  char)))
            (setq converted (concat converted dvorak-char))))
        (push (cons converted kana) dvorak-table)))
    (nreverse dvorak-table)))

(defconst nskk-input-dvorak-table
  (nskk-input-dvorak-convert-table)
  "Dvorak配列用の変換テーブル。")

;;; ハッシュテーブル

(defvar nskk-input-dvorak-hash-table nil
  "Dvorak入力方式用のハッシュテーブル。")

(defun nskk-input-dvorak-init-hash-table ()
  "Dvorak入力方式用のハッシュテーブルを初期化する。"
  (setq nskk-input-dvorak-hash-table
        (make-hash-table :test 'equal :size 500))
  (dolist (entry nskk-input-dvorak-table)
    (puthash (car entry) (cdr entry) nskk-input-dvorak-hash-table)))

;;; 検索関数

(defun nskk-input-dvorak-lookup (dvorak-romaji)
  "Dvorak入力方式でDVORAK-ROMAJIをひらがなに変換する。

使用例:
  (nskk-input-dvorak-lookup \"ta\")   ;; Dvorak配列での入力"
  (unless nskk-input-dvorak-hash-table
    (nskk-input-dvorak-init-hash-table))
  (gethash dvorak-romaji nskk-input-dvorak-hash-table))

(defun nskk-input-dvorak-get-candidates (prefix)
  "PREFIX で始まる全てのDvorakローマ字候補を取得する。"
  (let ((candidates nil))
    (dolist (entry nskk-input-dvorak-table)
      (when (string-prefix-p prefix (car entry))
        (push (car entry) candidates)))
    (nreverse candidates)))

;;; 入力方式登録

(defun nskk-input-dvorak-register ()
  "Dvorak入力方式をNSKKに登録する。"
  (nskk-input-dvorak-init-hash-table))

;;; 統計情報

(defun nskk-input-dvorak-stats ()
  "Dvorak入力方式の統計情報を返す。"
  (list :total (length nskk-input-dvorak-table)))

;;; パフォーマンス最適化

(defsubst nskk-input-dvorak-lookup-fast (dvorak-romaji)
  "Dvorak入力方式でDVORAK-ROMAJIを高速変換する。"
  (gethash dvorak-romaji nskk-input-dvorak-hash-table))

(provide 'nskk-input-dvorak)

;;; nskk-input-dvorak.el ends here
