;;; nskk-special-chars.el --- Special character handling for NSKK -*- lexical-binding: t; -*-

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

;; このファイルはNSKKの特殊文字処理を実装します。
;;
;; 主な機能:
;; 1. 大文字入力の処理（見出し語入力モードへの遷移）
;; 2. ひらがな→カタカナ変換
;; 3. 特殊な入力パターンの処理
;; 4. エッジケースのハンドリング
;;
;; 使用例:
;; (nskk-hiragana-to-katakana "あいうえお")  ;; => "アイウエオ"
;; (nskk-is-uppercase-input "K")              ;; => t
;; (nskk-process-special-input "Q")           ;; => special handling

;;; Code:

(require 'cl-lib)

;;; カスタマイズ可能変数

(defgroup nskk-special-chars nil
  "NSKK special character handling settings."
  :group 'nskk
  :prefix "nskk-special-chars-")

(defcustom nskk-special-chars-katakana-mode-key "q"
  "カタカナモードに切り替えるキー。
デフォルトは 'q'。このキーが押されると次の変換結果を
カタカナに変換する。"
  :type 'string
  :group 'nskk-special-chars)

(defcustom nskk-special-chars-jisx0201-kana-mode-key "Q"
  "半角カタカナモードに切り替えるキー。
デフォルトは 'Q'。"
  :type 'string
  :group 'nskk-special-chars)

;;; ひらがな↔カタカナ変換

(defconst nskk-special-chars--hiragana-katakana-offset 96
  "ひらがなとカタカナのUnicodeコードポイントのオフセット。
ひらがな「あ」(U+3042) とカタカナ「ア」(U+30A2) の差分。")

(defun nskk-hiragana-to-katakana (str)
  "ひらがな文字列 STR をカタカナに変換する。

ひらがな以外の文字はそのまま返される。

例:
  (nskk-hiragana-to-katakana \"あいうえお\")  ;; => \"アイウエオ\"
  (nskk-hiragana-to-katakana \"こんにちは\")  ;; => \"コンニチハ\"
  (nskk-hiragana-to-katakana \"aBc\")         ;; => \"aBc\" (変換なし)"
  (let ((result ""))
    (dotimes (i (length str))
      (let ((char (aref str i)))
        (if (and (>= char ?ぁ) (<= char ?ん))
            ;; ひらがなの場合、カタカナに変換
            (setq result (concat result
                                (char-to-string
                                 (+ char nskk-special-chars--hiragana-katakana-offset))))
          ;; それ以外はそのまま
          (setq result (concat result (char-to-string char))))))
    result))

(defun nskk-katakana-to-hiragana (str)
  "カタカナ文字列 STR をひらがなに変換する。

カタカナ以外の文字はそのまま返される。

例:
  (nskk-katakana-to-hiragana \"アイウエオ\")  ;; => \"あいうえお\"
  (nskk-katakana-to-hiragana \"コンニチハ\")  ;; => \"こんにちは\""
  (let ((result ""))
    (dotimes (i (length str))
      (let ((char (aref str i)))
        (if (and (>= char ?ァ) (<= char ?ン))
            ;; カタカナの場合、ひらがなに変換
            (setq result (concat result
                                (char-to-string
                                 (- char nskk-special-chars--hiragana-katakana-offset))))
          ;; それ以外はそのまま
          (setq result (concat result (char-to-string char))))))
    result))

;;; 大文字入力処理

(defsubst nskk-is-uppercase-char (char)
  "CHAR が大文字のアルファベットかどうかを判定する。"
  (and char (>= char ?A) (<= char ?Z)))

(defun nskk-is-uppercase-input (input)
  "INPUT の最初の文字が大文字かどうかを判定する。

例:
  (nskk-is-uppercase-input \"K\")   ;; => t
  (nskk-is-uppercase-input \"k\")   ;; => nil
  (nskk-is-uppercase-input \"Ka\")  ;; => t"
  (and (not (string-empty-p input))
       (nskk-is-uppercase-char (aref input 0))))

(defun nskk-lowercase-input (input)
  "INPUT を小文字に変換する。

例:
  (nskk-lowercase-input \"HELLO\")  ;; => \"hello\"
  (nskk-lowercase-input \"World\")  ;; => \"world\""
  (downcase input))

(defun nskk-capitalize-string (str)
  "STR の最初の文字を大文字に変換する。

日本語文字列の場合は何もしない。

例:
  (nskk-capitalize-string \"hello\")      ;; => \"Hello\"
  (nskk-capitalize-string \"こんにちは\")  ;; => \"こんにちは\""
  (if (string-empty-p str)
      str
    (let ((first-char (aref str 0)))
      (if (and (>= first-char ?a) (<= first-char ?z))
          (concat (upcase (substring str 0 1))
                  (substring str 1))
        str))))

;;; 特殊入力モード判定

(defun nskk-is-katakana-mode-key (input)
  "INPUT がカタカナモードキーかどうかを判定する。

例:
  (nskk-is-katakana-mode-key \"q\")  ;; => t (デフォルト設定の場合)"
  (string= input nskk-special-chars-katakana-mode-key))

(defun nskk-is-jisx0201-kana-mode-key (input)
  "INPUT が半角カタカナモードキーかどうかを判定する。

例:
  (nskk-is-jisx0201-kana-mode-key \"Q\")  ;; => t (デフォルト設定の場合)"
  (string= input nskk-special-chars-jisx0201-kana-mode-key))

;;; 半角カタカナ変換（JIS X 0201）

(defconst nskk-special-chars--halfwidth-katakana-map
  '(("ア" . "ｱ") ("イ" . "ｲ") ("ウ" . "ｳ") ("エ" . "ｴ") ("オ" . "ｵ")
    ("カ" . "ｶ") ("キ" . "ｷ") ("ク" . "ｸ") ("ケ" . "ｹ") ("コ" . "ｺ")
    ("サ" . "ｻ") ("シ" . "ｼ") ("ス" . "ｽ") ("セ" . "ｾ") ("ソ" . "ｿ")
    ("タ" . "ﾀ") ("チ" . "ﾁ") ("ツ" . "ﾂ") ("テ" . "ﾃ") ("ト" . "ﾄ")
    ("ナ" . "ﾅ") ("ニ" . "ﾆ") ("ヌ" . "ﾇ") ("ネ" . "ﾈ") ("ノ" . "ﾉ")
    ("ハ" . "ﾊ") ("ヒ" . "ﾋ") ("フ" . "ﾌ") ("ヘ" . "ﾍ") ("ホ" . "ﾎ")
    ("マ" . "ﾏ") ("ミ" . "ﾐ") ("ム" . "ﾑ") ("メ" . "ﾒ") ("モ" . "ﾓ")
    ("ヤ" . "ﾔ") ("ユ" . "ﾕ") ("ヨ" . "ﾖ")
    ("ラ" . "ﾗ") ("リ" . "ﾘ") ("ル" . "ﾙ") ("レ" . "ﾚ") ("ロ" . "ﾛ")
    ("ワ" . "ﾜ") ("ヲ" . "ｦ") ("ン" . "ﾝ")
    ("ガ" . "ｶﾞ") ("ギ" . "ｷﾞ") ("グ" . "ｸﾞ") ("ゲ" . "ｹﾞ") ("ゴ" . "ｺﾞ")
    ("ザ" . "ｻﾞ") ("ジ" . "ｼﾞ") ("ズ" . "ｽﾞ") ("ゼ" . "ｾﾞ") ("ゾ" . "ｿﾞ")
    ("ダ" . "ﾀﾞ") ("ヂ" . "ﾁﾞ") ("ヅ" . "ﾂﾞ") ("デ" . "ﾃﾞ") ("ド" . "ﾄﾞ")
    ("バ" . "ﾊﾞ") ("ビ" . "ﾋﾞ") ("ブ" . "ﾌﾞ") ("ベ" . "ﾍﾞ") ("ボ" . "ﾎﾞ")
    ("パ" . "ﾊﾟ") ("ピ" . "ﾋﾟ") ("プ" . "ﾌﾟ") ("ペ" . "ﾍﾟ") ("ポ" . "ﾎﾟ")
    ("ァ" . "ｧ") ("ィ" . "ｨ") ("ゥ" . "ｩ") ("ェ" . "ｪ") ("ォ" . "ｫ")
    ("ャ" . "ｬ") ("ュ" . "ｭ") ("ョ" . "ｮ")
    ("ッ" . "ｯ")
    ("ー" . "ｰ") ("、" . "､") ("。" . "｡") ("「" . "｢") ("」" . "｣")
    ("・" . "･"))
  "全角カタカナから半角カタカナへの変換マップ。")

(defun nskk-katakana-to-halfwidth (str)
  "全角カタカナ文字列 STR を半角カタカナ（JIS X 0201）に変換する。

濁音・半濁音は2文字（基底文字+濁点/半濁点）に分解される。

例:
  (nskk-katakana-to-halfwidth \"アイウエオ\")  ;; => \"ｱｲｳｴｵ\"
  (nskk-katakana-to-halfwidth \"ガギグ\")      ;; => \"ｶﾞｷﾞｸﾞ\""
  (let ((result ""))
    (dotimes (i (length str))
      (let* ((char (char-to-string (aref str i)))
             (halfwidth (cdr (assoc char nskk-special-chars--halfwidth-katakana-map))))
        (if halfwidth
            (setq result (concat result halfwidth))
          ;; マップにない文字はそのまま
          (setq result (concat result char)))))
    result))

;;; エッジケース処理

(defun nskk-process-repeating-input (input)
  "繰り返し入力パターンを処理する。

例えば、同じ文字の連続入力などの特殊なケースを扱う。
現在は促音処理が主な対象だが、将来的に拡張可能。

例:
  (nskk-process-repeating-input \"kkk\")  ;; 促音の連続など"
  ;; 現状は nskk-converter.el で処理されているため、
  ;; ここでは追加の処理は不要
  ;; 将来的な拡張用のプレースホルダー
  input)

(defun nskk-normalize-input (input)
  "INPUT を正規化する。

全角英数字を半角に変換するなどの前処理を行う。

例:
  (nskk-normalize-input \"ＡＢＣ\")  ;; => \"ABC\""
  ;; 全角英数字を半角に変換
  (let ((result ""))
    (dotimes (i (length input))
      (let ((char (aref input i)))
        (cond
         ;; 全角英字 (A-Z)
         ((and (>= char ?Ａ) (<= char ?Ｚ))
          (setq result (concat result (char-to-string (- char (- ?Ａ ?A))))))
         ;; 全角英字 (a-z)
         ((and (>= char ?ａ) (<= char ?ｚ))
          (setq result (concat result (char-to-string (- char (- ?ａ ?a))))))
         ;; 全角数字 (0-9)
         ((and (>= char ?０) (<= char ?９))
          (setq result (concat result (char-to-string (- char (- ?０ ?0))))))
         ;; その他はそのまま
         (t
          (setq result (concat result (char-to-string char)))))))
    result))

;;; 文字種判定ユーティリティ

(defsubst nskk-is-hiragana-char (char)
  "CHAR がひらがなかどうかを判定する。"
  (and char (>= char ?ぁ) (<= char ?ん)))

(defsubst nskk-is-katakana-char (char)
  "CHAR がカタカナかどうかを判定する。"
  (and char (>= char ?ァ) (<= char ?ン)))

(defsubst nskk-is-japanese-char (char)
  "CHAR が日本語文字（ひらがな・カタカナ・漢字）かどうかを判定する。"
  (or (nskk-is-hiragana-char char)
      (nskk-is-katakana-char char)
      ;; 漢字の範囲（簡易判定）
      (and char (>= char ?一) (<= char ?龿))))

(defun nskk-string-type (str)
  "STR の文字種を判定する。

返り値:
  - `hiragana'  : すべてひらがな
  - `katakana'  : すべてカタカナ
  - `kanji'     : 漢字を含む
  - `ascii'     : すべてASCII
  - `mixed'     : 混在

例:
  (nskk-string-type \"あいうえお\")  ;; => hiragana
  (nskk-string-type \"アイウエオ\")  ;; => katakana
  (nskk-string-type \"漢字\")       ;; => kanji
  (nskk-string-type \"abc\")        ;; => ascii
  (nskk-string-type \"あaい\")      ;; => mixed"
  (if (string-empty-p str)
      'empty
    (let ((has-hiragana nil)
          (has-katakana nil)
          (has-kanji nil)
          (has-ascii nil))
      (dotimes (i (length str))
        (let ((char (aref str i)))
          (cond
           ((nskk-is-hiragana-char char) (setq has-hiragana t))
           ((nskk-is-katakana-char char) (setq has-katakana t))
           ((and (>= char ?一) (<= char ?龿)) (setq has-kanji t))
           ((< char 128) (setq has-ascii t)))))
      (cond
       (has-kanji 'kanji)
       ((and has-hiragana (not has-katakana) (not has-ascii)) 'hiragana)
       ((and has-katakana (not has-hiragana) (not has-ascii)) 'katakana)
       ((and has-ascii (not has-hiragana) (not has-katakana) (not has-kanji)) 'ascii)
       (t 'mixed)))))

;;; 統計情報

(defun nskk-special-chars-stats ()
  "特殊文字処理の統計情報を返す。

返り値は以下の要素を含むplist:
  :halfwidth-map-size  - 半角カタカナマップのエントリ数
  :katakana-mode-key   - カタカナモードキー
  :jisx0201-mode-key   - 半角カタカナモードキー"
  (list :halfwidth-map-size (length nskk-special-chars--halfwidth-katakana-map)
        :katakana-mode-key nskk-special-chars-katakana-mode-key
        :jisx0201-mode-key nskk-special-chars-jisx0201-kana-mode-key))

(provide 'nskk-special-chars)

;;; nskk-special-chars.el ends here
