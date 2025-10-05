;;; nskk-input-qwerty.el --- QWERTY-JIS input method for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, qwerty
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))

;; This file is part of NSKK.

;;; Commentary:

;; QWERTY-JIS入力方式の実装。
;;
;; これは標準的なローマ字入力方式で、QWERTYキーボード配列を
;; 使用した最も一般的な日本語入力方法です。
;; 既存の `nskk-romaji-tables` をそのまま使用します。
;;
;; 使用例:
;; (require 'nskk-input-qwerty)
;; (nskk-register-input-method 'qwerty)

;;; Code:

(require 'nskk-romaji-tables)

;;; QWERTY入力方式は標準ローマ字テーブルをそのまま使用

(defvar nskk-input-qwerty-hash-table nil
  "QWERTY入力方式用のハッシュテーブル。
実際には `nskk-romaji-hash-table` への参照。")

(defun nskk-input-qwerty-init-hash-table ()
  "QWERTY入力方式用のハッシュテーブルを初期化する。
標準ローマ字テーブルを再利用する。"
  (unless nskk-romaji-hash-table
    (nskk-romaji-init-hash-table))
  (setq nskk-input-qwerty-hash-table nskk-romaji-hash-table))

;;; 検索関数

(defun nskk-input-qwerty-lookup (romaji)
  "QWERTY入力方式でROMAJIをひらがなに変換する。

使用例:
  (nskk-input-qwerty-lookup \"ka\")   ;; => \"か\"
  (nskk-input-qwerty-lookup \"kya\")  ;; => \"きゃ\""
  (unless nskk-input-qwerty-hash-table
    (nskk-input-qwerty-init-hash-table))
  (gethash romaji nskk-input-qwerty-hash-table))

(defun nskk-input-qwerty-get-candidates (prefix)
  "PREFIX で始まる全てのQWERTYローマ字候補を取得する。"
  (nskk-romaji-get-candidates prefix))

;;; 入力方式登録

(defun nskk-input-qwerty-register ()
  "QWERTY入力方式をNSKKに登録する。"
  (nskk-input-qwerty-init-hash-table))

;;; 統計情報

(defun nskk-input-qwerty-stats ()
  "QWERTY入力方式の統計情報を返す。"
  (nskk-romaji-table-stats))

;;; パフォーマンス最適化

(defsubst nskk-input-qwerty-lookup-fast (romaji)
  "QWERTY入力方式でROMAJIを高速変換する。"
  (gethash romaji nskk-input-qwerty-hash-table))

(provide 'nskk-input-qwerty)

;;; nskk-input-qwerty.el ends here
