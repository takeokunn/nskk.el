;;; nskk-converter-test.el --- Tests for nskk-converter.el -*- lexical-binding: t; -*-

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

;; nskk-converter.el の単体テスト。
;; ERTフレームワークを使用してローマ字→かな変換の全パターンを検証する。
;;
;; テストカテゴリ:
;; 1. 基本変換テスト（五十音、濁音、拗音）
;; 2. 促音処理テスト（kka → っか）
;; 3. 撥音処理テスト（nn → ん）
;; 4. 未確定入力テスト（プレフィックスマッチング）
;; 5. エッジケーステスト
;; 6. 統合テスト（複雑な入力パターン）

;;; Code:

(require 'ert)
(require 'nskk-converter)
(require 'nskk-romaji-tables)

;;; テストヘルパー関数

(defun nskk-converter-test--result-equal (result expected-converted expected-pending)
  "RESULT が期待する EXPECTED-CONVERTED と EXPECTED-PENDING に等しいかチェックする。"
  (and (string= (nskk-converter-result-converted result) expected-converted)
       (string= (nskk-converter-result-pending result) expected-pending)))

;;; 基本変換テスト

(ert-deftest nskk-converter-test-basic-hiragana ()
  "基本的なひらがな変換をテストする。"
  ;; あ行
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "a") "あ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "i") "い" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "u") "う" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "e") "え" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "o") "お" ""))

  ;; か行
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ka") "か" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ki") "き" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ku") "く" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ke") "け" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ko") "こ" ""))

  ;; さ行
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "sa") "さ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "si") "し" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "shi") "し" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "su") "す" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "se") "せ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "so") "そ" "")))

(ert-deftest nskk-converter-test-voiced-sounds ()
  "濁音・半濁音の変換をテストする。"
  ;; が行
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ga") "が" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "gi") "ぎ" ""))

  ;; ば行
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ba") "ば" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "bi") "び" ""))

  ;; ぱ行
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "pa") "ぱ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "pi") "ぴ" "")))

(ert-deftest nskk-converter-test-youon ()
  "拗音の変換をテストする。"
  ;; きゃ行
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "kya") "きゃ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "kyu") "きゅ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "kyo") "きょ" ""))

  ;; しゃ行（複数表記）
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "sha") "しゃ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "sya") "しゃ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "shu") "しゅ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "syu") "しゅ" ""))

  ;; ちゃ行（複数表記）
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "cha") "ちゃ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "cya") "ちゃ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "tya") "ちゃ" "")))

;;; 促音処理テスト

(ert-deftest nskk-converter-test-sokuon-basic ()
  "基本的な促音処理をテストする。"
  ;; k系
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "kka") "っか" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "kki") "っき" ""))

  ;; t系
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "tta") "った" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "tti") "っち" ""))

  ;; p系
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ppa") "っぱ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ppi") "っぴ" "")))

(ert-deftest nskk-converter-test-sokuon-youon ()
  "促音+拗音の組み合わせをテストする。"
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "kkya") "っきゃ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ppya") "っぴゃ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ssha") "っしゃ" "")))

(ert-deftest nskk-converter-test-sokuon-multiple ()
  "連続する促音処理をテストする。"
  ;; 「さっぱり」
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "sappari") "さっぱり" ""))

  ;; 「がっこう」
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "gakkou") "がっこう" ""))

  ;; 「ロケット」（カタカナ想定だが変換ロジックは同じ）
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "roketto") "ろけっと" "")))

;;; 撥音処理テスト

(ert-deftest nskk-converter-test-hatsuon-explicit ()
  "明示的な撥音処理（nn, n', xn）をテストする。"
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "nn") "ん" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "n'") "ん" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "xn") "ん" "")))

(ert-deftest nskk-converter-test-hatsuon-smart ()
  "スマートモードでの撥音処理をテストする。"
  ;; デフォルトモードはsmart
  (let ((nskk-converter-n-processing-mode 'smart))
    ;; n + 子音 → 「ん」として確定し、残りはpending
    (should (nskk-converter-test--result-equal
             (nskk-convert-romaji "nk") "ん" "k"))
    (should (nskk-converter-test--result-equal
             (nskk-convert-romaji "nt") "ん" "t"))

    ;; n + 母音 → 確定しない（「な」等の可能性）
    ;; "na" は "な" に変換される
    (should (nskk-converter-test--result-equal
             (nskk-convert-romaji "na") "な" ""))
    (should (nskk-converter-test--result-equal
             (nskk-convert-romaji "ni") "に" ""))))

(ert-deftest nskk-converter-test-hatsuon-words ()
  "撥音を含む単語の変換をテストする。"
  ;; 「かんじ」
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "kannji") "かんじ" ""))

  ;; 「さんぽ」
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "sanpo") "さんぽ" ""))

  ;; 「てんき」
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "tenki") "てんき" "")))

;;; 未確定入力テスト（プレフィックスマッチング）

(ert-deftest nskk-converter-test-pending-single-char ()
  "1文字の未確定入力をテストする。"
  ;; "k" は "ka", "ki" 等の候補がある → 未確定
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "k") "" "k"))

  ;; "s" は "sa", "shi" 等の候補がある → 未確定
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "s") "" "s"))

  ;; "n" は特殊処理（smartモードでは未確定）
  (let ((nskk-converter-n-processing-mode 'smart))
    (should (nskk-converter-test--result-equal
             (nskk-convert-romaji "n") "" "n"))))

(ert-deftest nskk-converter-test-pending-two-chars ()
  "2文字の未確定入力をテストする。"
  ;; "ky" は "kya", "kyu", "kyo" の候補がある → 未確定
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ky") "" "ky"))

  ;; "sh" は "sha", "shi", "shu" 等の候補がある → 未確定
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "sh") "" "sh"))

  ;; "kk" は促音の可能性があるが、3文字未満なので促音処理されない
  ;; "kk" 全体にマッチなし → 最初の "k" を出力、残りの "k" は pending
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "kk") "k" "k")))

(ert-deftest nskk-converter-test-no-match ()
  "マッチしない入力のテストする。"
  ;; "x" 単独では多くのパターンがない
  ;; ただし "xa", "xi" 等の小書き文字候補がある → 未確定
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "x") "" "x"))

  ;; "xyz": "x" はプレフィックスマッチ(xa等)あり→未確定、
  ;; しかし "xy" にはマッチなし→ "x" 出力、"y" もプレフィックス(ya等)あり→未確定
  ;; 実際の動作: "x" pending, 内部で "xy" → "x" 出力 + "y" pending
  ;; 最終的に "xy" + "z" pending になる
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "xyz") "xy" "z")))

;;; エッジケーステスト

(ert-deftest nskk-converter-test-empty-input ()
  "空文字列の入力をテストする。"
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "") "" "")))

(ert-deftest nskk-converter-test-long-input ()
  "長い入力文字列の変換をテストする。"
  ;; 「こんにちは」
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "konnnichiha") "こんにちは" ""))

  ;; 「ありがとうございます」
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "arigatougozaimasu") "ありがとうございます" "")))

(ert-deftest nskk-converter-test-mixed-patterns ()
  "複数パターンが混在する入力をテストする。"
  ;; 促音 + 拗音 + 撥音
  ;; 「しゅっぱつ」
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "shuppatsu") "しゅっぱつ" ""))

  ;; 「きょうしつ」
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "kyoushitsu") "きょうしつ" "")))

;;; 簡易API テスト

(ert-deftest nskk-converter-test-simple-api ()
  "`nskk-convert-romaji-simple' の動作をテストする。"
  (should (string= (nskk-convert-romaji-simple "ka") "か"))
  (should (string= (nskk-convert-romaji-simple "kya") "きゃ"))
  (should (string= (nskk-convert-romaji-simple "k") ""))
  (should (string= (nskk-convert-romaji-simple "kka") "っか")))

(ert-deftest nskk-converter-test-pending-api ()
  "`nskk-convert-romaji-pending' の動作をテストする。"
  (should (string= (nskk-convert-romaji-pending "ka") ""))
  (should (string= (nskk-convert-romaji-pending "k") "k"))
  (should (string= (nskk-convert-romaji-pending "ky") "ky"))
  (should (string= (nskk-convert-romaji-pending "kya") "")))

;;; 統計情報テスト

(ert-deftest nskk-converter-test-stats ()
  "`nskk-converter-stats' の動作をテストする。"
  (let ((stats (nskk-converter-stats)))
    (should (plist-member stats :romaji-table-size))
    (should (plist-member stats :max-romaji-length))
    (should (plist-member stats :sokuon-enabled))
    (should (plist-member stats :n-processing-mode))
    (should (> (plist-get stats :romaji-table-size) 0))
    (should (> (plist-get stats :max-romaji-length) 0))))

;;; カスタマイズ可能変数のテスト

(ert-deftest nskk-converter-test-sokuon-disabled ()
  "促音処理を無効化した場合のテストする。"
  (let ((nskk-converter-use-sokuon nil))
    ;; 促音処理が無効 → "kka" は "k" + "k" + "a" として処理
    ;; "k" 単独ではプレフィックスマッチあり→未確定
    ;; "kk" でもマッチなし、"k" 出力して次へ
    ;; 最終的に "k" + "か" になる
    (should (nskk-converter-test--result-equal
             (nskk-convert-romaji "kka") "kか" ""))))

(ert-deftest nskk-converter-test-n-processing-explicit ()
  "撥音処理を明示モードにした場合をテストする。"
  (let ((nskk-converter-n-processing-mode 'explicit))
    ;; n + 子音でも確定しない → "n" はprefixマッチあり、"k" もprefixマッチあり
    ;; "nk" 全体でマッチなし → "n" 出力、"k" pending
    (should (nskk-converter-test--result-equal
             (nskk-convert-romaji "nk") "n" "k"))

    ;; nn のみで確定
    (should (nskk-converter-test--result-equal
             (nskk-convert-romaji "nn") "ん" ""))))

(ert-deftest nskk-converter-test-n-processing-aggressive ()
  "撥音処理を積極モードにした場合をテストする。"
  (let ((nskk-converter-n-processing-mode 'aggressive))
    ;; n 単独でも確定
    (should (nskk-converter-test--result-equal
             (nskk-convert-romaji "n") "ん" ""))))

;;; 特殊ケーステスト

(ert-deftest nskk-converter-test-nn-not-sokuon ()
  "nn が促音ではなく撥音として処理されることをテストする。"
  ;; "nn" は "っん" ではなく "ん"
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "nn") "ん" ""))
  (should-not (nskk-converter-test--result-equal
               (nskk-convert-romaji "nn") "っん" "")))

(ert-deftest nskk-converter-test-small-letters ()
  "小書き文字の変換をテストする。"
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "xa") "ぁ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "xtu") "っ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "ltu") "っ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "xya") "ゃ" "")))

(ert-deftest nskk-converter-test-foreign-sounds ()
  "外来音の変換をテストする。"
  ;; ファ行
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "fa") "ふぁ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "fi") "ふぃ" ""))

  ;; ウィ行
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "wi") "うぃ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "we") "うぇ" ""))

  ;; ティ・ディ
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "thi") "てぃ" ""))
  (should (nskk-converter-test--result-equal
           (nskk-convert-romaji "dhi") "でぃ" "")))

(provide 'nskk-converter-test)

;;; nskk-converter-test.el ends here
