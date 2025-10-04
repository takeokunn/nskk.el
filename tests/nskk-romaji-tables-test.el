;;; nskk-romaji-tables-test.el --- Tests for nskk-romaji-tables.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 NSKK Development Team

;; Author: NSKK Development Team
;; Keywords: japanese, input method, skk, test

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

;; nskk-romaji-tables.el の単体テスト。
;; ERTフレームワークを使用して全ローマ字パターンを検証する。

;;; Code:

(require 'ert)
(require 'nskk-romaji-tables)

;;; 基本五十音のテスト

(ert-deftest nskk-romaji-test-base-a-line ()
  "あ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "a") "あ"))
  (should (string= (nskk-romaji-lookup "i") "い"))
  (should (string= (nskk-romaji-lookup "u") "う"))
  (should (string= (nskk-romaji-lookup "e") "え"))
  (should (string= (nskk-romaji-lookup "o") "お")))

(ert-deftest nskk-romaji-test-base-ka-line ()
  "か行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "ka") "か"))
  (should (string= (nskk-romaji-lookup "ki") "き"))
  (should (string= (nskk-romaji-lookup "ku") "く"))
  (should (string= (nskk-romaji-lookup "ke") "け"))
  (should (string= (nskk-romaji-lookup "ko") "こ")))

(ert-deftest nskk-romaji-test-base-sa-line ()
  "さ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "sa") "さ"))
  (should (string= (nskk-romaji-lookup "si") "し"))
  (should (string= (nskk-romaji-lookup "su") "す"))
  (should (string= (nskk-romaji-lookup "se") "せ"))
  (should (string= (nskk-romaji-lookup "so") "そ"))
  ;; 代替表記
  (should (string= (nskk-romaji-lookup "shi") "し")))

(ert-deftest nskk-romaji-test-base-ta-line ()
  "た行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "ta") "た"))
  (should (string= (nskk-romaji-lookup "ti") "ち"))
  (should (string= (nskk-romaji-lookup "tu") "つ"))
  (should (string= (nskk-romaji-lookup "te") "て"))
  (should (string= (nskk-romaji-lookup "to") "と"))
  ;; 代替表記
  (should (string= (nskk-romaji-lookup "chi") "ち"))
  (should (string= (nskk-romaji-lookup "tsu") "つ")))

(ert-deftest nskk-romaji-test-base-na-line ()
  "な行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "na") "な"))
  (should (string= (nskk-romaji-lookup "ni") "に"))
  (should (string= (nskk-romaji-lookup "nu") "ぬ"))
  (should (string= (nskk-romaji-lookup "ne") "ね"))
  (should (string= (nskk-romaji-lookup "no") "の")))

(ert-deftest nskk-romaji-test-base-ha-line ()
  "は行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "ha") "は"))
  (should (string= (nskk-romaji-lookup "hi") "ひ"))
  (should (string= (nskk-romaji-lookup "hu") "ふ"))
  (should (string= (nskk-romaji-lookup "he") "へ"))
  (should (string= (nskk-romaji-lookup "ho") "ほ"))
  ;; 代替表記
  (should (string= (nskk-romaji-lookup "fu") "ふ")))

(ert-deftest nskk-romaji-test-base-ma-line ()
  "ま行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "ma") "ま"))
  (should (string= (nskk-romaji-lookup "mi") "み"))
  (should (string= (nskk-romaji-lookup "mu") "む"))
  (should (string= (nskk-romaji-lookup "me") "め"))
  (should (string= (nskk-romaji-lookup "mo") "も")))

(ert-deftest nskk-romaji-test-base-ya-line ()
  "や行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "ya") "や"))
  (should (string= (nskk-romaji-lookup "yi") "い"))
  (should (string= (nskk-romaji-lookup "yu") "ゆ"))
  (should (string= (nskk-romaji-lookup "ye") "いぇ"))
  (should (string= (nskk-romaji-lookup "yo") "よ")))

(ert-deftest nskk-romaji-test-base-ra-line ()
  "ら行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "ra") "ら"))
  (should (string= (nskk-romaji-lookup "ri") "り"))
  (should (string= (nskk-romaji-lookup "ru") "る"))
  (should (string= (nskk-romaji-lookup "re") "れ"))
  (should (string= (nskk-romaji-lookup "ro") "ろ")))

(ert-deftest nskk-romaji-test-base-wa-line ()
  "わ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "wa") "わ"))
  (should (string= (nskk-romaji-lookup "wi") "うぃ"))
  (should (string= (nskk-romaji-lookup "wu") "う"))
  (should (string= (nskk-romaji-lookup "we") "うぇ"))
  (should (string= (nskk-romaji-lookup "wo") "を")))

(ert-deftest nskk-romaji-test-base-n ()
  "ん（撥音）の変換をテストする。"
  (should (string= (nskk-romaji-lookup "nn") "ん"))
  (should (string= (nskk-romaji-lookup "n'") "ん"))
  (should (string= (nskk-romaji-lookup "xn") "ん")))

;;; 濁音・半濁音のテスト

(ert-deftest nskk-romaji-test-voiced-ga-line ()
  "が行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "ga") "が"))
  (should (string= (nskk-romaji-lookup "gi") "ぎ"))
  (should (string= (nskk-romaji-lookup "gu") "ぐ"))
  (should (string= (nskk-romaji-lookup "ge") "げ"))
  (should (string= (nskk-romaji-lookup "go") "ご")))

(ert-deftest nskk-romaji-test-voiced-za-line ()
  "ざ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "za") "ざ"))
  (should (string= (nskk-romaji-lookup "zi") "じ"))
  (should (string= (nskk-romaji-lookup "zu") "ず"))
  (should (string= (nskk-romaji-lookup "ze") "ぜ"))
  (should (string= (nskk-romaji-lookup "zo") "ぞ"))
  ;; 代替表記
  (should (string= (nskk-romaji-lookup "ji") "じ")))

(ert-deftest nskk-romaji-test-voiced-da-line ()
  "だ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "da") "だ"))
  (should (string= (nskk-romaji-lookup "di") "ぢ"))
  (should (string= (nskk-romaji-lookup "du") "づ"))
  (should (string= (nskk-romaji-lookup "de") "で"))
  (should (string= (nskk-romaji-lookup "do") "ど")))

(ert-deftest nskk-romaji-test-voiced-ba-line ()
  "ば行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "ba") "ば"))
  (should (string= (nskk-romaji-lookup "bi") "び"))
  (should (string= (nskk-romaji-lookup "bu") "ぶ"))
  (should (string= (nskk-romaji-lookup "be") "べ"))
  (should (string= (nskk-romaji-lookup "bo") "ぼ")))

(ert-deftest nskk-romaji-test-voiced-pa-line ()
  "ぱ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "pa") "ぱ"))
  (should (string= (nskk-romaji-lookup "pi") "ぴ"))
  (should (string= (nskk-romaji-lookup "pu") "ぷ"))
  (should (string= (nskk-romaji-lookup "pe") "ぺ"))
  (should (string= (nskk-romaji-lookup "po") "ぽ")))

(ert-deftest nskk-romaji-test-voiced-va-line ()
  "ヴァ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "va") "ゔぁ"))
  (should (string= (nskk-romaji-lookup "vi") "ゔぃ"))
  (should (string= (nskk-romaji-lookup "vu") "ゔ"))
  (should (string= (nskk-romaji-lookup "ve") "ゔぇ"))
  (should (string= (nskk-romaji-lookup "vo") "ゔぉ")))

;;; 拗音のテスト

(ert-deftest nskk-romaji-test-youon-kya-line ()
  "きゃ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "kya") "きゃ"))
  (should (string= (nskk-romaji-lookup "kyi") "きぃ"))
  (should (string= (nskk-romaji-lookup "kyu") "きゅ"))
  (should (string= (nskk-romaji-lookup "kye") "きぇ"))
  (should (string= (nskk-romaji-lookup "kyo") "きょ")))

(ert-deftest nskk-romaji-test-youon-gya-line ()
  "ぎゃ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "gya") "ぎゃ"))
  (should (string= (nskk-romaji-lookup "gyi") "ぎぃ"))
  (should (string= (nskk-romaji-lookup "gyu") "ぎゅ"))
  (should (string= (nskk-romaji-lookup "gye") "ぎぇ"))
  (should (string= (nskk-romaji-lookup "gyo") "ぎょ")))

(ert-deftest nskk-romaji-test-youon-sha-line ()
  "しゃ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "sha") "しゃ"))
  (should (string= (nskk-romaji-lookup "shu") "しゅ"))
  (should (string= (nskk-romaji-lookup "she") "しぇ"))
  (should (string= (nskk-romaji-lookup "sho") "しょ"))
  ;; 代替表記
  (should (string= (nskk-romaji-lookup "sya") "しゃ"))
  (should (string= (nskk-romaji-lookup "syu") "しゅ"))
  (should (string= (nskk-romaji-lookup "syo") "しょ")))

(ert-deftest nskk-romaji-test-youon-ja-line ()
  "じゃ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "ja") "じゃ"))
  (should (string= (nskk-romaji-lookup "ju") "じゅ"))
  (should (string= (nskk-romaji-lookup "je") "じぇ"))
  (should (string= (nskk-romaji-lookup "jo") "じょ"))
  ;; 代替表記
  (should (string= (nskk-romaji-lookup "jya") "じゃ"))
  (should (string= (nskk-romaji-lookup "jyu") "じゅ"))
  (should (string= (nskk-romaji-lookup "jyo") "じょ"))
  (should (string= (nskk-romaji-lookup "zya") "じゃ"))
  (should (string= (nskk-romaji-lookup "zyu") "じゅ"))
  (should (string= (nskk-romaji-lookup "zyo") "じょ")))

(ert-deftest nskk-romaji-test-youon-cha-line ()
  "ちゃ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "cha") "ちゃ"))
  (should (string= (nskk-romaji-lookup "chu") "ちゅ"))
  (should (string= (nskk-romaji-lookup "che") "ちぇ"))
  (should (string= (nskk-romaji-lookup "cho") "ちょ"))
  ;; 代替表記
  (should (string= (nskk-romaji-lookup "cya") "ちゃ"))
  (should (string= (nskk-romaji-lookup "cyu") "ちゅ"))
  (should (string= (nskk-romaji-lookup "cyo") "ちょ"))
  (should (string= (nskk-romaji-lookup "tya") "ちゃ"))
  (should (string= (nskk-romaji-lookup "tyu") "ちゅ"))
  (should (string= (nskk-romaji-lookup "tyo") "ちょ")))

(ert-deftest nskk-romaji-test-youon-nya-line ()
  "にゃ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "nya") "にゃ"))
  (should (string= (nskk-romaji-lookup "nyi") "にぃ"))
  (should (string= (nskk-romaji-lookup "nyu") "にゅ"))
  (should (string= (nskk-romaji-lookup "nye") "にぇ"))
  (should (string= (nskk-romaji-lookup "nyo") "にょ")))

(ert-deftest nskk-romaji-test-youon-hya-line ()
  "ひゃ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "hya") "ひゃ"))
  (should (string= (nskk-romaji-lookup "hyi") "ひぃ"))
  (should (string= (nskk-romaji-lookup "hyu") "ひゅ"))
  (should (string= (nskk-romaji-lookup "hye") "ひぇ"))
  (should (string= (nskk-romaji-lookup "hyo") "ひょ")))

(ert-deftest nskk-romaji-test-youon-bya-line ()
  "びゃ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "bya") "びゃ"))
  (should (string= (nskk-romaji-lookup "byi") "びぃ"))
  (should (string= (nskk-romaji-lookup "byu") "びゅ"))
  (should (string= (nskk-romaji-lookup "bye") "びぇ"))
  (should (string= (nskk-romaji-lookup "byo") "びょ")))

(ert-deftest nskk-romaji-test-youon-pya-line ()
  "ぴゃ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "pya") "ぴゃ"))
  (should (string= (nskk-romaji-lookup "pyi") "ぴぃ"))
  (should (string= (nskk-romaji-lookup "pyu") "ぴゅ"))
  (should (string= (nskk-romaji-lookup "pye") "ぴぇ"))
  (should (string= (nskk-romaji-lookup "pyo") "ぴょ")))

(ert-deftest nskk-romaji-test-youon-mya-line ()
  "みゃ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "mya") "みゃ"))
  (should (string= (nskk-romaji-lookup "myi") "みぃ"))
  (should (string= (nskk-romaji-lookup "myu") "みゅ"))
  (should (string= (nskk-romaji-lookup "mye") "みぇ"))
  (should (string= (nskk-romaji-lookup "myo") "みょ")))

(ert-deftest nskk-romaji-test-youon-rya-line ()
  "りゃ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "rya") "りゃ"))
  (should (string= (nskk-romaji-lookup "ryi") "りぃ"))
  (should (string= (nskk-romaji-lookup "ryu") "りゅ"))
  (should (string= (nskk-romaji-lookup "rye") "りぇ"))
  (should (string= (nskk-romaji-lookup "ryo") "りょ")))

;;; 外来音のテスト

(ert-deftest nskk-romaji-test-foreign-fa-line ()
  "ファ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "fa") "ふぁ"))
  (should (string= (nskk-romaji-lookup "fi") "ふぃ"))
  (should (string= (nskk-romaji-lookup "fu") "ふ"))
  (should (string= (nskk-romaji-lookup "fe") "ふぇ"))
  (should (string= (nskk-romaji-lookup "fo") "ふぉ")))

(ert-deftest nskk-romaji-test-foreign-wha-line ()
  "ウァ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "wha") "うぁ"))
  (should (string= (nskk-romaji-lookup "whi") "うぃ"))
  (should (string= (nskk-romaji-lookup "whu") "う"))
  (should (string= (nskk-romaji-lookup "whe") "うぇ"))
  (should (string= (nskk-romaji-lookup "who") "うぉ")))

(ert-deftest nskk-romaji-test-foreign-tha-line ()
  "テァ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "tha") "てぁ"))
  (should (string= (nskk-romaji-lookup "thi") "てぃ"))
  (should (string= (nskk-romaji-lookup "thu") "てゅ"))
  (should (string= (nskk-romaji-lookup "the") "てぇ"))
  (should (string= (nskk-romaji-lookup "tho") "てょ")))

(ert-deftest nskk-romaji-test-foreign-qa-line ()
  "クァ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "qa") "くぁ"))
  (should (string= (nskk-romaji-lookup "qi") "くぃ"))
  (should (string= (nskk-romaji-lookup "qu") "く"))
  (should (string= (nskk-romaji-lookup "qe") "くぇ"))
  (should (string= (nskk-romaji-lookup "qo") "くぉ")))

(ert-deftest nskk-romaji-test-foreign-tsa-line ()
  "ツァ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "tsa") "つぁ"))
  (should (string= (nskk-romaji-lookup "tsi") "つぃ"))
  (should (string= (nskk-romaji-lookup "tse") "つぇ"))
  (should (string= (nskk-romaji-lookup "tso") "つぉ")))

;;; 小書き文字のテスト

(ert-deftest nskk-romaji-test-small-a-line ()
  "小書きあ行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "xa") "ぁ"))
  (should (string= (nskk-romaji-lookup "xi") "ぃ"))
  (should (string= (nskk-romaji-lookup "xu") "ぅ"))
  (should (string= (nskk-romaji-lookup "xe") "ぇ"))
  (should (string= (nskk-romaji-lookup "xo") "ぉ"))
  ;; 代替表記
  (should (string= (nskk-romaji-lookup "la") "ぁ"))
  (should (string= (nskk-romaji-lookup "li") "ぃ"))
  (should (string= (nskk-romaji-lookup "lu") "ぅ"))
  (should (string= (nskk-romaji-lookup "le") "ぇ"))
  (should (string= (nskk-romaji-lookup "lo") "ぉ")))

(ert-deftest nskk-romaji-test-small-ya-line ()
  "小書きや行の変換をテストする。"
  (should (string= (nskk-romaji-lookup "xya") "ゃ"))
  (should (string= (nskk-romaji-lookup "xyu") "ゅ"))
  (should (string= (nskk-romaji-lookup "xyo") "ょ"))
  ;; 代替表記
  (should (string= (nskk-romaji-lookup "lya") "ゃ"))
  (should (string= (nskk-romaji-lookup "lyu") "ゅ"))
  (should (string= (nskk-romaji-lookup "lyo") "ょ")))

(ert-deftest nskk-romaji-test-small-tsu ()
  "促音（小書きつ）の変換をテストする。"
  (should (string= (nskk-romaji-lookup "xtu") "っ"))
  (should (string= (nskk-romaji-lookup "xtsu") "っ"))
  ;; 代替表記
  (should (string= (nskk-romaji-lookup "ltu") "っ"))
  (should (string= (nskk-romaji-lookup "ltsu") "っ")))

;;; エッジケースのテスト

(ert-deftest nskk-romaji-test-edge-cases ()
  "エッジケースの変換をテストする。"
  ;; 存在しないローマ字
  (should (null (nskk-romaji-lookup "xyz")))
  (should (null (nskk-romaji-lookup "qqq")))
  (should (null (nskk-romaji-lookup "")))

  ;; 大文字小文字（現在の実装は小文字のみ対応）
  (should (null (nskk-romaji-lookup "KA")))
  (should (null (nskk-romaji-lookup "Ka"))))

;;; 統計・ユーティリティ関数のテスト

(ert-deftest nskk-romaji-test-get-candidates ()
  "`nskk-romaji-get-candidates' の動作をテストする。"
  ;; k で始まる候補を取得
  (let ((candidates (nskk-romaji-get-candidates "k")))
    (should (member "ka" candidates))
    (should (member "ki" candidates))
    (should (member "kya" candidates)))

  ;; ky で始まる候補を取得
  (let ((candidates (nskk-romaji-get-candidates "ky")))
    (should (member "kya" candidates))
    (should (member "kyu" candidates))
    (should (member "kyo" candidates))
    (should-not (member "ka" candidates)))

  ;; 存在しないプレフィックス
  (let ((candidates (nskk-romaji-get-candidates "xyz")))
    (should (null candidates))))

(ert-deftest nskk-romaji-test-max-length ()
  "`nskk-romaji-get-max-length' の動作をテストする。"
  (let ((max-len (nskk-romaji-get-max-length)))
    ;; 最長は "xtsu", "ltsu" などの4文字
    (should (>= max-len 4))
    (should (<= max-len 10))))

(ert-deftest nskk-romaji-test-table-stats ()
  "`nskk-romaji-table-stats' の動作をテストする。"
  (let ((stats (nskk-romaji-table-stats)))
    ;; 統計情報が正しく返される
    (should (plist-get stats :total))
    (should (plist-get stats :base))
    (should (plist-get stats :voiced))
    (should (plist-get stats :youon))
    (should (plist-get stats :foreign))
    (should (plist-get stats :small))
    (should (plist-get stats :max-length))

    ;; 総エントリ数は各カテゴリの合計以上
    (should (>= (plist-get stats :total)
                (+ (plist-get stats :base)
                   (plist-get stats :voiced)
                   (plist-get stats :youon)
                   (plist-get stats :foreign)
                   (plist-get stats :small))))))

;;; ハッシュテーブル初期化のテスト

(ert-deftest nskk-romaji-test-hash-table-init ()
  "ハッシュテーブル初期化の動作をテストする。"
  ;; ハッシュテーブルを初期化
  (nskk-romaji-init-hash-table)

  ;; ハッシュテーブルが正しく生成されている
  (should (hash-table-p nskk-romaji-hash-table))

  ;; いくつかのエントリが正しく格納されている
  (should (string= (gethash "ka" nskk-romaji-hash-table) "か"))
  (should (string= (gethash "kya" nskk-romaji-hash-table) "きゃ"))
  (should (null (gethash "xyz" nskk-romaji-hash-table))))

;;; 拡張テーブルのテスト

(ert-deftest nskk-romaji-test-extended-table ()
  "拡張テーブルの動作をテストする。"
  ;; カスタム拡張を追加
  (add-hook 'nskk-romaji-table-extend-functions
            (lambda () '(("dh" . "で") ("th" . "て"))))

  ;; 拡張テーブルを取得
  (let ((extended-table (nskk-romaji-get-extended-table)))
    ;; 基本テーブルのエントリが含まれている
    (should (assoc "ka" extended-table))

    ;; 拡張エントリが含まれている
    (should (assoc "dh" extended-table))
    (should (assoc "th" extended-table))

    ;; 拡張エントリが正しい値を持つ
    (should (string= (cdr (assoc "dh" extended-table)) "で"))
    (should (string= (cdr (assoc "th" extended-table)) "て")))

  ;; フックをクリーンアップ
  (setq nskk-romaji-table-extend-functions nil))

;;; パフォーマンステスト

(ert-deftest nskk-romaji-test-performance ()
  "ローマ字変換のパフォーマンスをテストする。
要件: < 1ms の応答時間"
  (nskk-romaji-init-hash-table)

  ;; 1000回の変換を実行
  (let ((start-time (current-time))
        (iterations 1000))
    (dotimes (_ iterations)
      (nskk-romaji-lookup "kya")
      (nskk-romaji-lookup "sha")
      (nskk-romaji-lookup "chu")
      (nskk-romaji-lookup "nn"))

    (let* ((end-time (current-time))
           (elapsed (float-time (time-subtract end-time start-time)))
           (per-lookup (/ elapsed (* iterations 4))))

      ;; 1回の検索が0.001秒（1ms）以内であることを確認
      ;; 注: 実際の目標は0.0001秒（0.1ms）だが、テスト環境を考慮
      (should (< per-lookup 0.001)))))

(provide 'nskk-romaji-tables-test)

;;; nskk-romaji-tables-test.el ends here
