;;; nskk-input-tutcode.el --- TUT-code input method for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, tutcode
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.0"))

;; This file is part of NSKK.

;;; Commentary:

;; TUT-code (2ストロークコード) 入力方式の実装。
;;
;; TUT-codeは、すべてのかな文字を2打鍵で入力できる
;; 効率的な入力方式。キーボード配列に基づいた
;; 体系的なコード割り当てが特徴。
;;
;; 主な特徴:
;; - すべてのかなを2ストロークで入力
;; - 覚えやすい体系的な配列
;; - 高速な入力が可能
;;
;; 使用例:
;; (require 'nskk-input-tutcode)
;; (nskk-register-input-method 'tutcode)

;;; Code:

(require 'nskk-romaji-tables)

;;; TUT-codeテーブル
;; キーボード配列に基づく2ストロークコード

(defconst nskk-input-tutcode-table
  '(;; 第1打鍵: a行 (あ行)
    ("aj" . "あ")
    ("ak" . "い")
    ("al" . "う")
    ("as" . "え")
    ("ad" . "お")

    ;; 第1打鍵: k行 (か行)
    ("kj" . "か")
    ("kk" . "き")
    ("kl" . "く")
    ("ks" . "け")
    ("kd" . "こ")

    ;; 第1打鍵: s行 (さ行)
    ("sj" . "さ")
    ("sk" . "し")
    ("sl" . "す")
    ("ss" . "せ")
    ("sd" . "そ")

    ;; 第1打鍵: t行 (た行)
    ("tj" . "た")
    ("tk" . "ち")
    ("tl" . "つ")
    ("ts" . "て")
    ("td" . "と")

    ;; 第1打鍵: n行 (な行)
    ("nj" . "な")
    ("nk" . "に")
    ("nl" . "ぬ")
    ("ns" . "ね")
    ("nd" . "の")

    ;; 第1打鍵: h行 (は行)
    ("hj" . "は")
    ("hk" . "ひ")
    ("hl" . "ふ")
    ("hs" . "へ")
    ("hd" . "ほ")

    ;; 第1打鍵: m行 (ま行)
    ("mj" . "ま")
    ("mk" . "み")
    ("ml" . "む")
    ("ms" . "め")
    ("md" . "も")

    ;; 第1打鍵: y行 (や行)
    ("yj" . "や")
    ("yk" . "ゆ")
    ("yl" . "よ")

    ;; 第1打鍵: r行 (ら行)
    ("rj" . "ら")
    ("rk" . "り")
    ("rl" . "る")
    ("rs" . "れ")
    ("rd" . "ろ")

    ;; 第1打鍵: w行 (わ行)
    ("wj" . "わ")
    ("wk" . "を")
    ("wl" . "ん")

    ;; 第1打鍵: g行 (が行)
    ("gj" . "が")
    ("gk" . "ぎ")
    ("gl" . "ぐ")
    ("gs" . "げ")
    ("gd" . "ご")

    ;; 第1打鍵: z行 (ざ行)
    ("zj" . "ざ")
    ("zk" . "じ")
    ("zl" . "ず")
    ("zs" . "ぜ")
    ("zd" . "ぞ")

    ;; 第1打鍵: d行 (だ行)
    ("dj" . "だ")
    ("dk" . "ぢ")
    ("dl" . "づ")
    ("ds" . "で")
    ("dd" . "ど")

    ;; 第1打鍵: b行 (ば行)
    ("bj" . "ば")
    ("bk" . "び")
    ("bl" . "ぶ")
    ("bs" . "べ")
    ("bd" . "ぼ")

    ;; 第1打鍵: p行 (ぱ行)
    ("pj" . "ぱ")
    ("pk" . "ぴ")
    ("pl" . "ぷ")
    ("ps" . "ぺ")
    ("pd" . "ぽ")

    ;; 拗音 (きゃ、しゃ等)
    ("kja" . "きゃ")
    ("kju" . "きゅ")
    ("kjo" . "きょ")

    ("sja" . "しゃ")
    ("sju" . "しゅ")
    ("sjo" . "しょ")

    ("tja" . "ちゃ")
    ("tju" . "ちゅ")
    ("tjo" . "ちょ")

    ("nja" . "にゃ")
    ("nju" . "にゅ")
    ("njo" . "にょ")

    ("hja" . "ひゃ")
    ("hju" . "ひゅ")
    ("hjo" . "ひょ")

    ("mja" . "みゃ")
    ("mju" . "みゅ")
    ("mjo" . "みょ")

    ("rja" . "りゃ")
    ("rju" . "りゅ")
    ("rjo" . "りょ")

    ("gja" . "ぎゃ")
    ("gju" . "ぎゅ")
    ("gjo" . "ぎょ")

    ("zja" . "じゃ")
    ("zju" . "じゅ")
    ("zjo" . "じょ")

    ("bja" . "びゃ")
    ("bju" . "びゅ")
    ("bjo" . "びょ")

    ("pja" . "ぴゃ")
    ("pju" . "ぴゅ")
    ("pjo" . "ぴょ")

    ;; 特殊文字
    ("qq" . "っ")
    ("q;" . "ん")
    ("[;" . "「")
    ("];" . "」")
    (",;" . "、")
    (".;" . "。"))
  "TUT-code入力方式の2ストロークテーブル。")

;;; ハッシュテーブル

(defvar nskk-input-tutcode-hash-table nil
  "TUT-code入力方式用のハッシュテーブル。")

(defun nskk-input-tutcode-init-hash-table ()
  "TUT-code入力方式用のハッシュテーブルを初期化する。"
  (setq nskk-input-tutcode-hash-table
        (make-hash-table :test 'equal :size 300))
  (dolist (entry nskk-input-tutcode-table)
    (puthash (car entry) (cdr entry) nskk-input-tutcode-hash-table)))

;;; 検索関数

(defun nskk-input-tutcode-lookup (code)
  "TUT-codeでCODEをひらがなに変換する。

使用例:
  (nskk-input-tutcode-lookup \"kj\")   ;; => \"か\"
  (nskk-input-tutcode-lookup \"kja\")  ;; => \"きゃ\""
  (unless nskk-input-tutcode-hash-table
    (nskk-input-tutcode-init-hash-table))
  (gethash code nskk-input-tutcode-hash-table))

(defun nskk-input-tutcode-get-candidates (prefix)
  "PREFIX で始まる全てのTUT-code候補を取得する。"
  (let ((candidates nil))
    (dolist (entry nskk-input-tutcode-table)
      (when (string-prefix-p prefix (car entry))
        (push (car entry) candidates)))
    (nreverse candidates)))

;;; 入力方式登録

(defun nskk-input-tutcode-register ()
  "TUT-code入力方式をNSKKに登録する。"
  (nskk-input-tutcode-init-hash-table))

;;; 統計情報

(defun nskk-input-tutcode-stats ()
  "TUT-code入力方式の統計情報を返す。"
  (list :total (length nskk-input-tutcode-table)
        :two-stroke (cl-count-if (lambda (e) (= (length (car e)) 2))
                                  nskk-input-tutcode-table)
        :three-stroke (cl-count-if (lambda (e) (= (length (car e)) 3))
                                    nskk-input-tutcode-table)))

;;; パフォーマンス最適化

(defsubst nskk-input-tutcode-lookup-fast (code)
  "TUT-codeでCODEを高速変換する。"
  (gethash code nskk-input-tutcode-hash-table))

(provide 'nskk-input-tutcode)

;;; nskk-input-tutcode.el ends here
