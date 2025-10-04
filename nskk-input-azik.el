;;; nskk-input-azik.el --- AZIK input method for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, azik
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

;; AZIK (エイジック) 入力方式の実装。
;;
;; AZIKは拡張ローマ字入力方式の一つで、従来のローマ字入力を
;; 拡張し、より少ないキー入力で日本語を入力できるように設計されている。
;;
;; 主な特徴:
;; - 子音+母音の2ストロークで拗音入力可能 (例: kj -> きゃ)
;; - 2ストロークで多くの音を入力可能
;; - 標準ローマ字入力との互換性を維持
;;
;; 使用例:
;; (require 'nskk-input-azik)
;; (nskk-register-input-method 'azik)

;;; Code:

(require 'nskk-romaji-tables)

;;; AZIK拡張テーブル

(defconst nskk-input-azik-extension-table
  '(;; 拗音の短縮入力 (子音 + j/k/l)
    ;; きゃ行
    ("kj" . "きゃ")
    ("kl" . "きゅ")
    ("ko" . "きょ")

    ;; しゃ行
    ("sj" . "しゃ")
    ("sl" . "しゅ")
    ("so" . "しょ")

    ;; ちゃ行
    ("tj" . "ちゃ")
    ("tl" . "ちゅ")
    ("to" . "ちょ")

    ;; にゃ行
    ("nj" . "にゃ")
    ("nl" . "にゅ")
    ("no" . "にょ")

    ;; ひゃ行
    ("hj" . "ひゃ")
    ("hl" . "ひゅ")
    ("ho" . "ひょ")

    ;; みゃ行
    ("mj" . "みゃ")
    ("ml" . "みゅ")
    ("mo" . "みょ")

    ;; りゃ行
    ("rj" . "りゃ")
    ("rl" . "りゅ")
    ("ro" . "りょ")

    ;; ぎゃ行
    ("gj" . "ぎゃ")
    ("gl" . "ぎゅ")
    ("go" . "ぎょ")

    ;; じゃ行
    ("zj" . "じゃ")
    ("zl" . "じゅ")
    ("zo" . "じょ")

    ;; びゃ行
    ("bj" . "びゃ")
    ("bl" . "びゅ")
    ("bo" . "びょ")

    ;; ぴゃ行
    ("pj" . "ぴゃ")
    ("pl" . "ぴゅ")
    ("po" . "ぴょ")

    ;; 二重母音の短縮入力
    ("q" . "ん")
    ("dh" . "で")
    ("dk" . "だ")
    ("dl" . "ど")

    ;; ファ行の短縮
    ("fj" . "ふぁ")
    ("fk" . "ふぃ")
    ("fl" . "ふぇ")
    ("fo" . "ふぉ")

    ;; ヴァ行
    ("vj" . "ゔぁ")
    ("vk" . "ゔぃ")
    ("vl" . "ゔぇ")
    ("vo" . "ゔぉ")

    ;; 特殊な組み合わせ
    ("wh" . "う")
    ("xj" . "ゃ")
    ("xk" . "ゅ")
    ("xl" . "ょ")

    ;; 促音の短縮
    (";" . "っ")

    ;; 記号
    (":" . "ー")
    ("@" . "、")
    ("[" . "「")
    ("]" . "」"))
  "AZIK入力方式の拡張テーブル。
標準ローマ字入力に追加される短縮入力ルール。")

;;; 統合テーブルの生成

(defconst nskk-input-azik-table
  (append nskk-input-azik-extension-table
          nskk-romaji-table)
  "AZIK入力方式用の完全な変換テーブル。
AZIK拡張ルールと標準ローマ字ルールを統合したもの。")

;;; ハッシュテーブル

(defvar nskk-input-azik-hash-table nil
  "AZIK入力方式用のハッシュテーブル。
高速検索のために使用される。")

(defun nskk-input-azik-init-hash-table ()
  "AZIK入力方式用のハッシュテーブルを初期化する。"
  (setq nskk-input-azik-hash-table
        (make-hash-table :test 'equal :size 600))
  (dolist (entry nskk-input-azik-table)
    (puthash (car entry) (cdr entry) nskk-input-azik-hash-table)))

;;; 検索関数

(defun nskk-input-azik-lookup (romaji)
  "AZIK入力方式でROMAJIをひらがなに変換する。
変換可能な場合はひらがな文字列を返し、不可能な場合は nil を返す。

使用例:
  (nskk-input-azik-lookup \"kj\")   ;; => \"きゃ\"
  (nskk-input-azik-lookup \"ka\")   ;; => \"か\"
  (nskk-input-azik-lookup \";\")    ;; => \"っ\""
  (unless nskk-input-azik-hash-table
    (nskk-input-azik-init-hash-table))
  (gethash romaji nskk-input-azik-hash-table))

(defun nskk-input-azik-get-candidates (prefix)
  "PREFIX で始まる全てのAZIKローマ字候補を取得する。

返り値はローマ字文字列のリスト。

使用例:
  (nskk-input-azik-get-candidates \"k\")   ;; => (\"ka\" \"ki\" \"kj\" ...)
  (nskk-input-azik-get-candidates \"kj\")  ;; => (\"kj\")"
  (let ((candidates nil))
    (dolist (entry nskk-input-azik-table)
      (when (string-prefix-p prefix (car entry))
        (push (car entry) candidates)))
    (nreverse candidates)))

;;; 入力方式登録

(defun nskk-input-azik-register ()
  "AZIK入力方式をNSKKに登録する。
この関数は `nskk-register-input-method' を通じて呼び出される。"
  (nskk-input-azik-init-hash-table))

;;; 統計情報

(defun nskk-input-azik-stats ()
  "AZIK入力方式の統計情報を返す。

返り値は以下の要素を含むplist:
  :total          - 総エントリ数
  :azik-extension - AZIK拡張エントリ数
  :base-romaji    - 標準ローマ字エントリ数"
  (list :total (length nskk-input-azik-table)
        :azik-extension (length nskk-input-azik-extension-table)
        :base-romaji (length nskk-romaji-table)))

;;; パフォーマンス最適化

;; defsubstによるインライン展開で高速化
(defsubst nskk-input-azik-lookup-fast (romaji)
  "AZIK入力方式でROMAJIを高速変換する。
`nskk-input-azik-lookup' のインライン最適化版。"
  (gethash romaji nskk-input-azik-hash-table))

(provide 'nskk-input-azik)

;;; nskk-input-azik.el ends here
