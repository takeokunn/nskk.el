;;; nskk-input-colemak.el --- Colemak input method for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, colemak
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.0"))

;; This file is part of NSKK.

;;; Commentary:

;; Colemak配列用の日本語入力方式。
;;
;; Colemak配列でローマ字入力を行う際の変換テーブル。
;; Colemak配列のキー配置に最適化されています。
;;
;; 使用例:
;; (require 'nskk-input-colemak)
;; (nskk-register-input-method 'colemak)

;;; Code:

(require 'nskk-romaji-tables)

;;; Colemak配列用の変換テーブル

(defconst nskk-input-colemak-key-map
  '(;; QWERTYのキーをColemakのキーにマッピング
    ;; 上段（QWERTYと同じ）
    ("q" . "q")
    ("w" . "w")
    ("e" . "f")
    ("r" . "p")
    ("t" . "g")
    ("y" . "j")
    ("u" . "l")
    ("i" . "u")
    ("o" . "y")
    ("p" . ";")

    ;; 中段
    ("a" . "a")
    ("s" . "r")
    ("d" . "s")
    ("f" . "t")
    ("g" . "d")
    ("h" . "h")
    ("j" . "n")
    ("k" . "e")
    ("l" . "i")
    (";" . "o")

    ;; 下段
    ("z" . "z")
    ("x" . "x")
    ("c" . "c")
    ("v" . "v")
    ("b" . "b")
    ("n" . "k")
    ("m" . "m"))
  "Colemak配列からQWERTY配列へのキーマッピング。")

;;; Colemak用のローマ字テーブル生成

(defun nskk-input-colemak-convert-table ()
  "Colemak配列用のローマ字テーブルを生成する。
QWERTYベースのローマ字テーブルをColemak配列に変換。"
  (let ((colemak-table nil))
    (dolist (entry nskk-romaji-table)
      (let ((romaji (car entry))
            (kana (cdr entry))
            (converted ""))
        ;; ローマ字の各文字をColemakキーに変換
        (dotimes (i (length romaji))
          (let* ((char (substring romaji i (1+ i)))
                 (colemak-char (or (car (rassoc char nskk-input-colemak-key-map))
                                   char)))
            (setq converted (concat converted colemak-char))))
        (push (cons converted kana) colemak-table)))
    (nreverse colemak-table)))

(defconst nskk-input-colemak-table
  (nskk-input-colemak-convert-table)
  "Colemak配列用の変換テーブル。")

;;; ハッシュテーブル

(defvar nskk-input-colemak-hash-table nil
  "Colemak入力方式用のハッシュテーブル。")

(defun nskk-input-colemak-init-hash-table ()
  "Colemak入力方式用のハッシュテーブルを初期化する。"
  (setq nskk-input-colemak-hash-table
        (make-hash-table :test 'equal :size 500))
  (dolist (entry nskk-input-colemak-table)
    (puthash (car entry) (cdr entry) nskk-input-colemak-hash-table)))

;;; 検索関数

(defun nskk-input-colemak-lookup (colemak-romaji)
  "Colemak入力方式でCOLEMAK-ROMAJIをひらがなに変換する。

使用例:
  (nskk-input-colemak-lookup \"ta\")   ;; Colemak配列での入力"
  (unless nskk-input-colemak-hash-table
    (nskk-input-colemak-init-hash-table))
  (gethash colemak-romaji nskk-input-colemak-hash-table))

(defun nskk-input-colemak-get-candidates (prefix)
  "PREFIX で始まる全てのColemakローマ字候補を取得する。"
  (let ((candidates nil))
    (dolist (entry nskk-input-colemak-table)
      (when (string-prefix-p prefix (car entry))
        (push (car entry) candidates)))
    (nreverse candidates)))

;;; 入力方式登録

(defun nskk-input-colemak-register ()
  "Colemak入力方式をNSKKに登録する。"
  (nskk-input-colemak-init-hash-table))

;;; 統計情報

(defun nskk-input-colemak-stats ()
  "Colemak入力方式の統計情報を返す。"
  (list :total (length nskk-input-colemak-table)))

;;; パフォーマンス最適化

(defsubst nskk-input-colemak-lookup-fast (colemak-romaji)
  "Colemak入力方式でCOLEMAK-ROMAJIを高速変換する。"
  (gethash colemak-romaji nskk-input-colemak-hash-table))

(provide 'nskk-input-colemak)

;;; nskk-input-colemak.el ends here
