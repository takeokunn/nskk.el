;;; nskk-romaji-tables.el --- Romaji to Hiragana conversion tables for NSKK -*- lexical-binding: t; -*-

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

;; このファイルはNSKKのローマ字→ひらがな変換テーブルを定義します。
;;
;; 特徴:
;; - defconstによるコンパイル時最適化
;; - 最長一致優先のアルゴリズム対応
;; - 代替表記の完全サポート（shi/si, chi/ti, tsu/tu等）
;; - 拡張入力方式（AZIK、ACT等）への拡張可能な設計
;;
;; 使用例:
;; (nskk-romaji-lookup "ka")   ;; => "か"
;; (nskk-romaji-lookup "kya")  ;; => "きゃ"
;; (nskk-romaji-lookup "nn")   ;; => "ん"

;;; Code:

(require 'cl-lib)

;;; 基本五十音テーブル

(defconst nskk-romaji-base-table
  '(;; あ行
    ("a" . "あ")
    ("i" . "い")
    ("u" . "う")
    ("e" . "え")
    ("o" . "お")

    ;; か行
    ("ka" . "か")
    ("ki" . "き")
    ("ku" . "く")
    ("ke" . "け")
    ("ko" . "こ")

    ;; さ行
    ("sa" . "さ")
    ("si" . "し")
    ("su" . "す")
    ("se" . "せ")
    ("so" . "そ")
    ("shi" . "し")  ; 代替表記

    ;; た行
    ("ta" . "た")
    ("ti" . "ち")
    ("tu" . "つ")
    ("te" . "て")
    ("to" . "と")
    ("chi" . "ち")  ; 代替表記
    ("tsu" . "つ")  ; 代替表記

    ;; な行
    ("na" . "な")
    ("ni" . "に")
    ("nu" . "ぬ")
    ("ne" . "ね")
    ("no" . "の")

    ;; は行
    ("ha" . "は")
    ("hi" . "ひ")
    ("hu" . "ふ")
    ("he" . "へ")
    ("ho" . "ほ")
    ("fu" . "ふ")  ; 代替表記

    ;; ま行
    ("ma" . "ま")
    ("mi" . "み")
    ("mu" . "む")
    ("me" . "め")
    ("mo" . "も")

    ;; や行
    ("ya" . "や")
    ("yi" . "い")
    ("yu" . "ゆ")
    ("ye" . "いぇ")
    ("yo" . "よ")

    ;; ら行
    ("ra" . "ら")
    ("ri" . "り")
    ("ru" . "る")
    ("re" . "れ")
    ("ro" . "ろ")

    ;; わ行
    ("wa" . "わ")
    ("wi" . "うぃ")
    ("wu" . "う")
    ("we" . "うぇ")
    ("wo" . "を")

    ;; ん
    ("nn" . "ん")
    ("n'" . "ん")
    ("xn" . "ん"))
  "基本五十音のローマ字→ひらがな変換テーブル。
最も基本的な変換ルールを定義する。")

;;; 濁音・半濁音テーブル

(defconst nskk-romaji-voiced-table
  '(;; が行
    ("ga" . "が")
    ("gi" . "ぎ")
    ("gu" . "ぐ")
    ("ge" . "げ")
    ("go" . "ご")

    ;; ざ行
    ("za" . "ざ")
    ("zi" . "じ")
    ("zu" . "ず")
    ("ze" . "ぜ")
    ("zo" . "ぞ")
    ("ji" . "じ")  ; 代替表記
    ("zu" . "ず")  ; 代替表記（重複だが明示）

    ;; だ行
    ("da" . "だ")
    ("di" . "ぢ")
    ("du" . "づ")
    ("de" . "で")
    ("do" . "ど")

    ;; ば行
    ("ba" . "ば")
    ("bi" . "び")
    ("bu" . "ぶ")
    ("be" . "べ")
    ("bo" . "ぼ")

    ;; ぱ行
    ("pa" . "ぱ")
    ("pi" . "ぴ")
    ("pu" . "ぷ")
    ("pe" . "ぺ")
    ("po" . "ぽ")

    ;; ヴ
    ("va" . "ゔぁ")
    ("vi" . "ゔぃ")
    ("vu" . "ゔ")
    ("ve" . "ゔぇ")
    ("vo" . "ゔぉ"))
  "濁音・半濁音のローマ字→ひらがな変換テーブル。")

;;; 拗音テーブル

(defconst nskk-romaji-youon-table
  '(;; きゃ行
    ("kya" . "きゃ")
    ("kyi" . "きぃ")
    ("kyu" . "きゅ")
    ("kye" . "きぇ")
    ("kyo" . "きょ")

    ;; ぎゃ行
    ("gya" . "ぎゃ")
    ("gyi" . "ぎぃ")
    ("gyu" . "ぎゅ")
    ("gye" . "ぎぇ")
    ("gyo" . "ぎょ")

    ;; しゃ行
    ("sha" . "しゃ")
    ("shi" . "し")
    ("shu" . "しゅ")
    ("she" . "しぇ")
    ("sho" . "しょ")
    ("sya" . "しゃ")  ; 代替表記
    ("syi" . "しぃ")
    ("syu" . "しゅ")  ; 代替表記
    ("sye" . "しぇ")
    ("syo" . "しょ")  ; 代替表記

    ;; じゃ行
    ("ja" . "じゃ")
    ("ji" . "じ")
    ("ju" . "じゅ")
    ("je" . "じぇ")
    ("jo" . "じょ")
    ("jya" . "じゃ")  ; 代替表記
    ("jyi" . "じぃ")
    ("jyu" . "じゅ")  ; 代替表記
    ("jye" . "じぇ")
    ("jyo" . "じょ")  ; 代替表記
    ("zya" . "じゃ")  ; 代替表記
    ("zyi" . "じぃ")
    ("zyu" . "じゅ")  ; 代替表記
    ("zye" . "じぇ")
    ("zyo" . "じょ")  ; 代替表記

    ;; ちゃ行
    ("cha" . "ちゃ")
    ("chi" . "ち")
    ("chu" . "ちゅ")
    ("che" . "ちぇ")
    ("cho" . "ちょ")
    ("cya" . "ちゃ")  ; 代替表記
    ("cyi" . "ちぃ")
    ("cyu" . "ちゅ")  ; 代替表記
    ("cye" . "ちぇ")
    ("cyo" . "ちょ")  ; 代替表記
    ("tya" . "ちゃ")  ; 代替表記
    ("tyi" . "ちぃ")
    ("tyu" . "ちゅ")  ; 代替表記
    ("tye" . "ちぇ")
    ("tyo" . "ちょ")  ; 代替表記

    ;; ぢゃ行
    ("dya" . "ぢゃ")
    ("dyi" . "ぢぃ")
    ("dyu" . "ぢゅ")
    ("dye" . "ぢぇ")
    ("dyo" . "ぢょ")

    ;; にゃ行
    ("nya" . "にゃ")
    ("nyi" . "にぃ")
    ("nyu" . "にゅ")
    ("nye" . "にぇ")
    ("nyo" . "にょ")

    ;; ひゃ行
    ("hya" . "ひゃ")
    ("hyi" . "ひぃ")
    ("hyu" . "ひゅ")
    ("hye" . "ひぇ")
    ("hyo" . "ひょ")

    ;; びゃ行
    ("bya" . "びゃ")
    ("byi" . "びぃ")
    ("byu" . "びゅ")
    ("bye" . "びぇ")
    ("byo" . "びょ")

    ;; ぴゃ行
    ("pya" . "ぴゃ")
    ("pyi" . "ぴぃ")
    ("pyu" . "ぴゅ")
    ("pye" . "ぴぇ")
    ("pyo" . "ぴょ")

    ;; みゃ行
    ("mya" . "みゃ")
    ("myi" . "みぃ")
    ("myu" . "みゅ")
    ("mye" . "みぇ")
    ("myo" . "みょ")

    ;; りゃ行
    ("rya" . "りゃ")
    ("ryi" . "りぃ")
    ("ryu" . "りゅ")
    ("rye" . "りぇ")
    ("ryo" . "りょ")

    ;; ヴゃ行
    ("vya" . "ゔゃ")
    ("vyi" . "ゔぃ")
    ("vyu" . "ゔゅ")
    ("vye" . "ゔぇ")
    ("vyo" . "ゔょ"))
  "拗音のローマ字→ひらがな変換テーブル。
きゃ、しゃ、ちゃなどの拗音を定義する。")

;;; ファ行・ウィ行等の外来音テーブル

(defconst nskk-romaji-foreign-table
  '(;; ファ行
    ("fa" . "ふぁ")
    ("fi" . "ふぃ")
    ("fu" . "ふ")
    ("fe" . "ふぇ")
    ("fo" . "ふぉ")
    ("fya" . "ふゃ")
    ("fyi" . "ふぃ")
    ("fyu" . "ふゅ")
    ("fye" . "ふぇ")
    ("fyo" . "ふょ")

    ;; ウィ行
    ("wi" . "うぃ")
    ("we" . "うぇ")
    ("wo" . "を")
    ("wha" . "うぁ")
    ("whi" . "うぃ")
    ("whu" . "う")
    ("whe" . "うぇ")
    ("who" . "うぉ")

    ;; ティ・ディ行
    ("tha" . "てぁ")
    ("thi" . "てぃ")
    ("thu" . "てゅ")
    ("the" . "てぇ")
    ("tho" . "てょ")
    ("dha" . "でゃ")
    ("dhi" . "でぃ")
    ("dhu" . "でゅ")
    ("dhe" . "でぇ")
    ("dho" . "でょ")

    ;; トゥ・ドゥ
    ("twu" . "とぅ")
    ("dwu" . "どぅ")

    ;; クァ行
    ("qa" . "くぁ")
    ("qi" . "くぃ")
    ("qu" . "く")
    ("qe" . "くぇ")
    ("qo" . "くぉ")
    ("qya" . "くゃ")
    ("qyu" . "くゅ")
    ("qyo" . "くょ")
    ("qwa" . "くぁ")
    ("qwi" . "くぃ")
    ("qwu" . "くぅ")
    ("qwe" . "くぇ")
    ("qwo" . "くぉ")

    ;; グァ行
    ("gwa" . "ぐぁ")
    ("gwi" . "ぐぃ")
    ("gwu" . "ぐぅ")
    ("gwe" . "ぐぇ")
    ("gwo" . "ぐぉ")

    ;; ツァ行
    ("tsa" . "つぁ")
    ("tsi" . "つぃ")
    ("tse" . "つぇ")
    ("tso" . "つぉ"))
  "外来音のローマ字→ひらがな変換テーブル。
ファ、ウィ、ティなど外来語由来の音を定義する。")

;;; 小書き文字テーブル

(defconst nskk-romaji-small-table
  '(;; 小書きあ行
    ("xa" . "ぁ")
    ("xi" . "ぃ")
    ("xu" . "ぅ")
    ("xe" . "ぇ")
    ("xo" . "ぉ")
    ("la" . "ぁ")  ; 代替表記
    ("li" . "ぃ")  ; 代替表記
    ("lu" . "ぅ")  ; 代替表記
    ("le" . "ぇ")  ; 代替表記
    ("lo" . "ぉ")  ; 代替表記

    ;; 小書きや行
    ("xya" . "ゃ")
    ("xyu" . "ゅ")
    ("xyo" . "ょ")
    ("lya" . "ゃ")  ; 代替表記
    ("lyu" . "ゅ")  ; 代替表記
    ("lyo" . "ょ")  ; 代替表記

    ;; 小書きわ行
    ("xwa" . "ゎ")
    ("lwa" . "ゎ")  ; 代替表記

    ;; 小書きか行
    ("xka" . "ゕ")
    ("xke" . "ゖ")
    ("lka" . "ゕ")  ; 代替表記
    ("lke" . "ゖ")  ; 代替表記

    ;; 促音（小書きつ）
    ("xtu" . "っ")
    ("xtsu" . "っ")
    ("ltu" . "っ")  ; 代替表記
    ("ltsu" . "っ"))  ; 代替表記
  "小書き文字のローマ字→ひらがな変換テーブル。
ぁ、ぃ、っなどの小書き文字を定義する。")

;;; 特殊文字テーブル

(defconst nskk-romaji-special-table
  '(;; 句読点・記号
    ("," . "、")
    ("." . "。")
    ("-" . "ー")
    ("~" . "〜")
    ("/" . "・")

    ;; カギ括弧
    ("[" . "「")
    ("]" . "」")

    ;; 中黒
    ("." . "・"))
  "特殊文字のローマ字→記号変換テーブル。
主に句読点や記号を定義する。")

;;; 統合テーブル

(defconst nskk-romaji-table
  (append nskk-romaji-youon-table      ; 拗音（最長一致優先のため先頭）
          nskk-romaji-foreign-table    ; 外来音
          nskk-romaji-small-table      ; 小書き文字
          nskk-romaji-voiced-table     ; 濁音・半濁音
          nskk-romaji-base-table)      ; 基本五十音
  "統合されたローマ字→ひらがな変換テーブル。
最長一致優先のため、長いパターンを先に配置している。

使用例:
  (assoc \"kya\" nskk-romaji-table)  ;; => (\"kya\" . \"きゃ\")
  (assoc \"ka\" nskk-romaji-table)   ;; => (\"ka\" . \"か\")

注意:
  このテーブルは最長一致アルゴリズムでの使用を想定している。
  例えば \"kya\" を変換する際、\"k\" -> \"y\" -> \"a\" ではなく
  \"kya\" として一括で検索すべき。")

;;; ハッシュテーブル版（高速検索用）

(defvar nskk-romaji-hash-table nil
  "ローマ字→ひらがな変換用のハッシュテーブル。
`nskk-romaji-table' から自動生成される。")

(defun nskk-romaji-init-hash-table ()
  "ローマ字変換用のハッシュテーブルを初期化する。
`nskk-romaji-table' の内容をハッシュテーブルに変換し、
高速な検索を可能にする。"
  (setq nskk-romaji-hash-table (make-hash-table :test 'equal :size 500))
  (dolist (entry nskk-romaji-table)
    (puthash (car entry) (cdr entry) nskk-romaji-hash-table)))

;;; 検索関数

(defun nskk-romaji-lookup (romaji)
  "ROMAJI をひらがなに変換する。
変換可能な場合はひらがな文字列を返し、不可能な場合は nil を返す。

使用例:
  (nskk-romaji-lookup \"ka\")   ;; => \"か\"
  (nskk-romaji-lookup \"kya\")  ;; => \"きゃ\"
  (nskk-romaji-lookup \"xyz\")  ;; => nil"
  (unless nskk-romaji-hash-table
    (nskk-romaji-init-hash-table))
  (gethash romaji nskk-romaji-hash-table))

(defun nskk-romaji-get-candidates (prefix)
  "PREFIX で始まる全てのローマ字候補を取得する。
補完機能や入力予測で使用することを想定。

返り値はローマ字文字列のリスト。

使用例:
  (nskk-romaji-get-candidates \"k\")   ;; => (\"ka\" \"ki\" \"ku\" ...)
  (nskk-romaji-get-candidates \"ky\")  ;; => (\"kya\" \"kyu\" \"kyo\" ...)"
  (let ((candidates nil))
    (dolist (entry nskk-romaji-table)
      (when (string-prefix-p prefix (car entry))
        (push (car entry) candidates)))
    (nreverse candidates)))

(defun nskk-romaji-get-max-length ()
  "ローマ字テーブル内の最大文字列長を返す。
最長一致アルゴリズムの最適化に使用。"
  (let ((max-len 0))
    (dolist (entry nskk-romaji-table)
      (setq max-len (max max-len (length (car entry)))))
    max-len))

;;; 統計情報関数

(defun nskk-romaji-table-stats ()
  "ローマ字テーブルの統計情報を返す。
デバッグやドキュメント作成に使用。

返り値は以下の要素を含むplist:
  :total        - 総エントリ数
  :base         - 基本五十音エントリ数
  :voiced       - 濁音・半濁音エントリ数
  :youon        - 拗音エントリ数
  :foreign      - 外来音エントリ数
  :small        - 小書き文字エントリ数
  :max-length   - 最大ローマ字長"
  (list :total (length nskk-romaji-table)
        :base (length nskk-romaji-base-table)
        :voiced (length nskk-romaji-voiced-table)
        :youon (length nskk-romaji-youon-table)
        :foreign (length nskk-romaji-foreign-table)
        :small (length nskk-romaji-small-table)
        :max-length (nskk-romaji-get-max-length)))

;;; カスタマイズ用フック

(defvar nskk-romaji-table-extend-functions nil
  "ローマ字テーブルを拡張するフック。
AZIK、ACT等の拡張入力方式はこのフックを使用して
カスタムテーブルを追加できる。

各関数は追加のローマ字→ひらがな変換ルールを
連想リストとして返す必要がある。

使用例:
  (add-hook 'nskk-romaji-table-extend-functions
            (lambda () '((\"dh\" . \"で\"))))")

(defun nskk-romaji-get-extended-table ()
  "拡張テーブルを含む完全なローマ字変換テーブルを返す。
`nskk-romaji-table-extend-functions' に登録された関数を
全て実行し、その結果を統合する。"
  (let ((extended-table nskk-romaji-table))
    (dolist (func nskk-romaji-table-extend-functions)
      (when (functionp func)
        (let ((additional-entries (funcall func)))
          (when additional-entries
            (setq extended-table (append additional-entries extended-table))))))
    extended-table))

(provide 'nskk-romaji-tables)

;;; nskk-romaji-tables.el ends here
