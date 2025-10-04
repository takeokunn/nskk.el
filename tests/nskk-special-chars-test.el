;;; nskk-special-chars-test.el --- Tests for nskk-special-chars.el -*- lexical-binding: t; -*-

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

;; nskk-special-chars.el の単体テスト。
;; ERTフレームワークを使用して特殊文字処理の全パターンを検証する。
;;
;; テストカテゴリ:
;; 1. ひらがな↔カタカナ変換
;; 2. 半角カタカナ変換
;; 3. 大文字入力処理
;; 4. 文字種判定
;; 5. 入力正規化
;; 6. エッジケース

;;; Code:

(require 'ert)
(require 'nskk-special-chars)

;;; ひらがな→カタカナ変換テスト

(ert-deftest nskk-special-chars-test-hiragana-to-katakana-basic ()
  "基本的なひらがな→カタカナ変換をテストする。"
  ;; 五十音
  (should (string= (nskk-hiragana-to-katakana "あいうえお") "アイウエオ"))
  (should (string= (nskk-hiragana-to-katakana "かきくけこ") "カキクケコ"))
  (should (string= (nskk-hiragana-to-katakana "さしすせそ") "サシスセソ"))
  (should (string= (nskk-hiragana-to-katakana "たちつてと") "タチツテト"))
  (should (string= (nskk-hiragana-to-katakana "なにぬねの") "ナニヌネノ"))

  ;; 濁音・半濁音
  (should (string= (nskk-hiragana-to-katakana "がぎぐげご") "ガギグゲゴ"))
  (should (string= (nskk-hiragana-to-katakana "ばびぶべぼ") "バビブベボ"))
  (should (string= (nskk-hiragana-to-katakana "ぱぴぷぺぽ") "パピプペポ"))

  ;; 拗音
  (should (string= (nskk-hiragana-to-katakana "きゃきゅきょ") "キャキュキョ"))
  (should (string= (nskk-hiragana-to-katakana "しゃしゅしょ") "シャシュショ"))

  ;; 促音・撥音
  (should (string= (nskk-hiragana-to-katakana "がっこう") "ガッコウ"))
  (should (string= (nskk-hiragana-to-katakana "さんぽ") "サンポ")))

(ert-deftest nskk-special-chars-test-hiragana-to-katakana-words ()
  "実際の単語でひらがな→カタカナ変換をテストする。"
  (should (string= (nskk-hiragana-to-katakana "こんにちは") "コンニチハ"))
  (should (string= (nskk-hiragana-to-katakana "ありがとう") "アリガトウ"))
  (should (string= (nskk-hiragana-to-katakana "さようなら") "サヨウナラ")))

(ert-deftest nskk-special-chars-test-hiragana-to-katakana-mixed ()
  "混在文字列のひらがな→カタカナ変換をテストする。"
  ;; 英数字混在（英数字はそのまま）
  (should (string= (nskk-hiragana-to-katakana "abc") "abc"))
  (should (string= (nskk-hiragana-to-katakana "123") "123"))
  (should (string= (nskk-hiragana-to-katakana "あaいb") "アaイb")))

(ert-deftest nskk-special-chars-test-hiragana-to-katakana-empty ()
  "空文字列のひらがな→カタカナ変換をテストする。"
  (should (string= (nskk-hiragana-to-katakana "") "")))

;;; カタカナ→ひらがな変換テスト

(ert-deftest nskk-special-chars-test-katakana-to-hiragana-basic ()
  "基本的なカタカナ→ひらがな変換をテストする。"
  (should (string= (nskk-katakana-to-hiragana "アイウエオ") "あいうえお"))
  (should (string= (nskk-katakana-to-hiragana "カキクケコ") "かきくけこ"))
  (should (string= (nskk-katakana-to-hiragana "ガギグゲゴ") "がぎぐげご")))

(ert-deftest nskk-special-chars-test-katakana-to-hiragana-roundtrip ()
  "ひらがな→カタカナ→ひらがなの往復変換をテストする。"
  (let ((original "あいうえお"))
    (should (string= (nskk-katakana-to-hiragana
                     (nskk-hiragana-to-katakana original))
                    original)))

  (let ((original "こんにちは"))
    (should (string= (nskk-katakana-to-hiragana
                     (nskk-hiragana-to-katakana original))
                    original))))

;;; 半角カタカナ変換テスト

(ert-deftest nskk-special-chars-test-katakana-to-halfwidth-basic ()
  "基本的な半角カタカナ変換をテストする。"
  (should (string= (nskk-katakana-to-halfwidth "アイウエオ") "ｱｲｳｴｵ"))
  (should (string= (nskk-katakana-to-halfwidth "カキクケコ") "ｶｷｸｹｺ"))
  (should (string= (nskk-katakana-to-halfwidth "サシスセソ") "ｻｼｽｾｿ")))

(ert-deftest nskk-special-chars-test-katakana-to-halfwidth-voiced ()
  "濁音・半濁音の半角カタカナ変換をテストする。"
  ;; 濁音は2文字（基底文字 + 濁点）になる
  (should (string= (nskk-katakana-to-halfwidth "ガギグゲゴ") "ｶﾞｷﾞｸﾞｹﾞｺﾞ"))
  (should (string= (nskk-katakana-to-halfwidth "ザジズゼゾ") "ｻﾞｼﾞｽﾞｾﾞｿﾞ"))
  (should (string= (nskk-katakana-to-halfwidth "バビブベボ") "ﾊﾞﾋﾞﾌﾞﾍﾞﾎﾞ"))
  (should (string= (nskk-katakana-to-halfwidth "パピプペポ") "ﾊﾟﾋﾟﾌﾟﾍﾟﾎﾟ")))

(ert-deftest nskk-special-chars-test-katakana-to-halfwidth-small ()
  "小書き文字の半角カタカナ変換をテストする。"
  (should (string= (nskk-katakana-to-halfwidth "ァィゥェォ") "ｧｨｩｪｫ"))
  (should (string= (nskk-katakana-to-halfwidth "ャュョ") "ｬｭｮ"))
  (should (string= (nskk-katakana-to-halfwidth "ッ") "ｯ")))

(ert-deftest nskk-special-chars-test-katakana-to-halfwidth-symbols ()
  "記号の半角カタカナ変換をテストする。"
  (should (string= (nskk-katakana-to-halfwidth "ー") "ｰ"))
  (should (string= (nskk-katakana-to-halfwidth "、") "､"))
  (should (string= (nskk-katakana-to-halfwidth "。") "｡"))
  (should (string= (nskk-katakana-to-halfwidth "「」") "｢｣"))
  (should (string= (nskk-katakana-to-halfwidth "・") "･")))

(ert-deftest nskk-special-chars-test-katakana-to-halfwidth-words ()
  "実際の単語で半角カタカナ変換をテストする。"
  (should (string= (nskk-katakana-to-halfwidth "コンニチハ") "ｺﾝﾆﾁﾊ"))
  (should (string= (nskk-katakana-to-halfwidth "アリガトウ") "ｱﾘｶﾞﾄｳ"))
  (should (string= (nskk-katakana-to-halfwidth "サヨウナラ") "ｻﾖｳﾅﾗ")))

;;; 大文字入力処理テスト

(ert-deftest nskk-special-chars-test-uppercase-detection ()
  "大文字入力の検出をテストする。"
  (should (eq (nskk-is-uppercase-input "A") t))
  (should (eq (nskk-is-uppercase-input "Z") t))
  (should (eq (nskk-is-uppercase-input "Ka") t))
  (should (eq (nskk-is-uppercase-input "HELLO") t))

  (should (eq (nskk-is-uppercase-input "a") nil))
  (should (eq (nskk-is-uppercase-input "hello") nil))
  (should (eq (nskk-is-uppercase-input "123") nil)))

(ert-deftest nskk-special-chars-test-lowercase-conversion ()
  "大文字→小文字変換をテストする。"
  (should (string= (nskk-lowercase-input "HELLO") "hello"))
  (should (string= (nskk-lowercase-input "World") "world"))
  (should (string= (nskk-lowercase-input "ABC123") "abc123")))

(ert-deftest nskk-special-chars-test-capitalize ()
  "文字列の先頭大文字化をテストする。"
  (should (string= (nskk-capitalize-string "hello") "Hello"))
  (should (string= (nskk-capitalize-string "world") "World"))
  (should (string= (nskk-capitalize-string "abc") "Abc"))

  ;; 既に大文字の場合
  (should (string= (nskk-capitalize-string "Hello") "Hello"))

  ;; 日本語の場合は何もしない
  (should (string= (nskk-capitalize-string "こんにちは") "こんにちは")))

;;; 特殊モードキー判定テスト

(ert-deftest nskk-special-chars-test-katakana-mode-key ()
  "カタカナモードキーの判定をテストする。"
  ;; デフォルトは "q"
  (should (eq (nskk-is-katakana-mode-key "q") t))
  (should (eq (nskk-is-katakana-mode-key "Q") nil))
  (should (eq (nskk-is-katakana-mode-key "a") nil)))

(ert-deftest nskk-special-chars-test-jisx0201-mode-key ()
  "半角カタカナモードキーの判定をテストする。"
  ;; デフォルトは "Q"
  (should (eq (nskk-is-jisx0201-kana-mode-key "Q") t))
  (should (eq (nskk-is-jisx0201-kana-mode-key "q") nil))
  (should (eq (nskk-is-jisx0201-kana-mode-key "A") nil)))

;;; 入力正規化テスト

(ert-deftest nskk-special-chars-test-normalize-fullwidth-alpha ()
  "全角英字の正規化をテストする。"
  (should (string= (nskk-normalize-input "ＡＢＣ") "ABC"))
  (should (string= (nskk-normalize-input "ａｂｃ") "abc"))
  (should (string= (nskk-normalize-input "ＡｂＣ") "AbC")))

(ert-deftest nskk-special-chars-test-normalize-fullwidth-digit ()
  "全角数字の正規化をテストする。"
  (should (string= (nskk-normalize-input "０１２３") "0123"))
  (should (string= (nskk-normalize-input "４５６７８９") "456789")))

(ert-deftest nskk-special-chars-test-normalize-mixed ()
  "混在文字列の正規化をテストする。"
  (should (string= (nskk-normalize-input "ＡＢＣ１２３") "ABC123"))
  (should (string= (nskk-normalize-input "ａｂｃ４５６") "abc456")))

(ert-deftest nskk-special-chars-test-normalize-japanese ()
  "日本語文字は正規化されないことをテストする。"
  (should (string= (nskk-normalize-input "あいうえお") "あいうえお"))
  (should (string= (nskk-normalize-input "アイウエオ") "アイウエオ")))

;;; 文字種判定テスト

(ert-deftest nskk-special-chars-test-char-type-hiragana ()
  "ひらがな文字の判定をテストする。"
  (should (eq (nskk-is-hiragana-char ?あ) t))
  (should (eq (nskk-is-hiragana-char ?ん) t))
  (should (eq (nskk-is-hiragana-char ?ア) nil))
  (should (eq (nskk-is-hiragana-char ?a) nil)))

(ert-deftest nskk-special-chars-test-char-type-katakana ()
  "カタカナ文字の判定をテストする。"
  (should (eq (nskk-is-katakana-char ?ア) t))
  (should (eq (nskk-is-katakana-char ?ン) t))
  (should (eq (nskk-is-katakana-char ?あ) nil))
  (should (eq (nskk-is-katakana-char ?a) nil)))

(ert-deftest nskk-special-chars-test-char-type-japanese ()
  "日本語文字の判定をテストする。"
  (should (eq (nskk-is-japanese-char ?あ) t))
  (should (eq (nskk-is-japanese-char ?ア) t))
  (should (eq (nskk-is-japanese-char ?漢) t))
  (should (eq (nskk-is-japanese-char ?a) nil))
  (should (eq (nskk-is-japanese-char ?1) nil)))

(ert-deftest nskk-special-chars-test-string-type ()
  "文字列の文字種判定をテストする。"
  (should (eq (nskk-string-type "あいうえお") 'hiragana))
  (should (eq (nskk-string-type "アイウエオ") 'katakana))
  (should (eq (nskk-string-type "漢字") 'kanji))
  (should (eq (nskk-string-type "abc") 'ascii))
  (should (eq (nskk-string-type "あaい") 'mixed))
  (should (eq (nskk-string-type "") 'empty)))

;;; エッジケーステスト

(ert-deftest nskk-special-chars-test-empty-string ()
  "空文字列の処理をテストする。"
  (should (string= (nskk-hiragana-to-katakana "") ""))
  (should (string= (nskk-katakana-to-hiragana "") ""))
  (should (string= (nskk-katakana-to-halfwidth "") ""))
  (should (string= (nskk-normalize-input "") ""))
  (should (string= (nskk-capitalize-string "") "")))

(ert-deftest nskk-special-chars-test-single-char ()
  "単一文字の処理をテストする。"
  (should (string= (nskk-hiragana-to-katakana "あ") "ア"))
  (should (string= (nskk-katakana-to-hiragana "ア") "あ"))
  (should (string= (nskk-katakana-to-halfwidth "ア") "ｱ"))
  (should (string= (nskk-normalize-input "Ａ") "A")))

(ert-deftest nskk-special-chars-test-long-string ()
  "長い文字列の処理をテストする。"
  (let ((long-hiragana (make-string 1000 ?あ))
        (long-katakana (make-string 1000 ?ア)))
    (should (string= (nskk-hiragana-to-katakana long-hiragana)
                    long-katakana))
    (should (string= (nskk-katakana-to-hiragana long-katakana)
                    long-hiragana))))

(ert-deftest nskk-special-chars-test-special-characters ()
  "特殊文字の処理をテストする。"
  ;; 改行、タブなどはそのまま
  (should (string= (nskk-hiragana-to-katakana "あ\nい") "ア\nイ"))
  (should (string= (nskk-hiragana-to-katakana "あ\tい") "ア\tイ")))

;;; 統計情報テスト

(ert-deftest nskk-special-chars-test-stats ()
  "`nskk-special-chars-stats' の動作をテストする。"
  (let ((stats (nskk-special-chars-stats)))
    (should (plist-member stats :halfwidth-map-size))
    (should (plist-member stats :katakana-mode-key))
    (should (plist-member stats :jisx0201-mode-key))
    (should (> (plist-get stats :halfwidth-map-size) 0))
    (should (stringp (plist-get stats :katakana-mode-key)))
    (should (stringp (plist-get stats :jisx0201-mode-key)))))

;;; 統合テスト

(ert-deftest nskk-special-chars-test-hiragana-katakana-halfwidth-chain ()
  "ひらがな→カタカナ→半角カタカナの変換チェーンをテストする。"
  (let* ((hiragana "こんにちは")
         (katakana (nskk-hiragana-to-katakana hiragana))
         (halfwidth (nskk-katakana-to-halfwidth katakana)))
    (should (string= katakana "コンニチハ"))
    (should (string= halfwidth "ｺﾝﾆﾁﾊ"))))

(ert-deftest nskk-special-chars-test-normalize-capitalize-chain ()
  "正規化→大文字化の変換チェーンをテストする。"
  (let* ((input "ｈｅｌｌｏ")
         (normalized (nskk-normalize-input input))
         (capitalized (nskk-capitalize-string normalized)))
    (should (string= normalized "hello"))
    (should (string= capitalized "Hello"))))

(provide 'nskk-special-chars-test)

;;; nskk-special-chars-test.el ends here
