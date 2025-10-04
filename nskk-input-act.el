;;; nskk-input-act.el --- ACT input method for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, act
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.0"))

;; This file is part of NSKK.

;;; Commentary:

;; ACT (Advanced Compact Table) 入力方式の実装。
;;
;; ACTはAZIKと類似の拡張ローマ字入力方式で、
;; より効率的な日本語入力を実現する。
;;
;; 主な特徴:
;; - 2ストロークでの拗音入力
;; - 独自の短縮ルール
;; - 高い入力効率
;;
;; 使用例:
;; (require 'nskk-input-act)
;; (nskk-register-input-method 'act)

;;; Code:

(require 'nskk-romaji-tables)

;;; ACT拡張テーブル

(defconst nskk-input-act-extension-table
  '(;; 拗音の短縮入力
    ;; きゃ行
    ("kh" . "きゃ")
    ("kj" . "きゅ")
    ("kk" . "きょ")

    ;; しゃ行
    ("sh" . "しゃ")
    ("sj" . "しゅ")
    ("sk" . "しょ")

    ;; ちゃ行
    ("th" . "ちゃ")
    ("tj" . "ちゅ")
    ("tk" . "ちょ")

    ;; にゃ行
    ("nh" . "にゃ")
    ("nj" . "にゅ")
    ("nk" . "にょ")

    ;; ひゃ行
    ("hh" . "ひゃ")
    ("hj" . "ひゅ")
    ("hk" . "ひょ")

    ;; みゃ行
    ("mh" . "みゃ")
    ("mj" . "みゅ")
    ("mk" . "みょ")

    ;; りゃ行
    ("rh" . "りゃ")
    ("rj" . "りゅ")
    ("rk" . "りょ")

    ;; ぎゃ行
    ("gh" . "ぎゃ")
    ("gj" . "ぎゅ")
    ("gk" . "ぎょ")

    ;; じゃ行
    ("zh" . "じゃ")
    ("zj" . "じゅ")
    ("zk" . "じょ")

    ;; びゃ行
    ("bh" . "びゃ")
    ("bj" . "びゅ")
    ("bk" . "びょ")

    ;; ぴゃ行
    ("ph" . "ぴゃ")
    ("pj" . "ぴゅ")
    ("pk" . "ぴょ")

    ;; 二重母音・特殊音
    ("q" . "ん")
    ("dh" . "で")
    ("df" . "だ")
    ("dg" . "ど")

    ;; ファ行
    ("fh" . "ふぁ")
    ("fj" . "ふぃ")
    ("fk" . "ふぇ")
    ("fl" . "ふぉ")

    ;; 促音
    (";" . "っ")
    (":" . "ー")

    ;; 記号
    ("[" . "「")
    ("]" . "」")
    ("@" . "、")
    ("/" . "・"))
  "ACT入力方式の拡張テーブル。")

;;; 統合テーブル

(defconst nskk-input-act-table
  (append nskk-input-act-extension-table
          nskk-romaji-table)
  "ACT入力方式用の完全な変換テーブル。")

;;; ハッシュテーブル

(defvar nskk-input-act-hash-table nil
  "ACT入力方式用のハッシュテーブル。")

(defun nskk-input-act-init-hash-table ()
  "ACT入力方式用のハッシュテーブルを初期化する。"
  (setq nskk-input-act-hash-table
        (make-hash-table :test 'equal :size 600))
  (dolist (entry nskk-input-act-table)
    (puthash (car entry) (cdr entry) nskk-input-act-hash-table)))

;;; 検索関数

(defun nskk-input-act-lookup (romaji)
  "ACT入力方式でROMAJIをひらがなに変換する。

使用例:
  (nskk-input-act-lookup \"kh\")   ;; => \"きゃ\"
  (nskk-input-act-lookup \"ka\")   ;; => \"か\""
  (unless nskk-input-act-hash-table
    (nskk-input-act-init-hash-table))
  (gethash romaji nskk-input-act-hash-table))

(defun nskk-input-act-get-candidates (prefix)
  "PREFIX で始まる全てのACTローマ字候補を取得する。"
  (let ((candidates nil))
    (dolist (entry nskk-input-act-table)
      (when (string-prefix-p prefix (car entry))
        (push (car entry) candidates)))
    (nreverse candidates)))

;;; 入力方式登録

(defun nskk-input-act-register ()
  "ACT入力方式をNSKKに登録する。"
  (nskk-input-act-init-hash-table))

;;; 統計情報

(defun nskk-input-act-stats ()
  "ACT入力方式の統計情報を返す。"
  (list :total (length nskk-input-act-table)
        :act-extension (length nskk-input-act-extension-table)
        :base-romaji (length nskk-romaji-table)))

;;; パフォーマンス最適化

(defsubst nskk-input-act-lookup-fast (romaji)
  "ACT入力方式でROMAJIを高速変換する。"
  (gethash romaji nskk-input-act-hash-table))

(provide 'nskk-input-act)

;;; nskk-input-act.el ends here
