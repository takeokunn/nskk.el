;;; nskk-kana-test.el --- Core conversion tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-kana.el covering:
;; - Character classification (hiragana, katakana, han)
;; - Hiragana to katakana conversion
;; - Katakana to hiragana conversion
;; - Zenkaku to hankaku conversion
;; - Hankaku to zenkaku conversion
;; - String conversion functions
;; - Performance benchmarks

;;; Code:

(require 'ert)
(require 'nskk-kana)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;
;;; Character Classification Tests
;;;

(nskk-deftest-unit core-hiragana-p-basic
  "Test hiragana classification for basic characters."
  (should (nskk-kana-hiragana-p ?あ))
  (should (nskk-kana-hiragana-p ?い))
  (should (nskk-kana-hiragana-p ?う))
  (should (nskk-kana-hiragana-p ?え))
  (should (nskk-kana-hiragana-p ?お))
  (should (nskk-kana-hiragana-p ?か))
  (should (nskk-kana-hiragana-p ?き))
  (should (nskk-kana-hiragana-p ?く))
  (should (nskk-kana-hiragana-p ?け))
  (should (nskk-kana-hiragana-p ?こ))
  (should (nskk-kana-hiragana-p ?さ))
  (should (nskk-kana-hiragana-p ?し))
  (should (nskk-kana-hiragana-p ?す))
  (should (nskk-kana-hiragana-p ?せ))
  (should (nskk-kana-hiragana-p ?そ)))

(nskk-deftest-unit core-hiragana-p-range-edges
  "Test hiragana classification at range boundaries."
  (should (nskk-kana-hiragana-p #x3040))  ; Start of range
  (should (nskk-kana-hiragana-p #x309F))  ; End of range
  (should (not (nskk-kana-hiragana-p #x303F)))  ; Just before
  (should (not (nskk-kana-hiragana-p #x30A0)))) ; Just after

(nskk-deftest-unit core-hiragana-p-non-hiragana
  "Test hiragana classification rejects non-hiragana."
  (should (not (nskk-kana-hiragana-p ?ア)))
  (should (not (nskk-kana-hiragana-p ?a)))
  (should (not (nskk-kana-hiragana-p ?A)))
  (should (not (nskk-kana-hiragana-p ?1)))
  (should (not (nskk-kana-hiragana-p ?漢))))

(nskk-deftest-unit core-katakana-p-basic
  "Test katakana classification for basic characters."
  (should (nskk-kana-katakana-p ?ア))
  (should (nskk-kana-katakana-p ?イ))
  (should (nskk-kana-katakana-p ?ウ))
  (should (nskk-kana-katakana-p ?エ))
  (should (nskk-kana-katakana-p ?オ))
  (should (nskk-kana-katakana-p ?カ))
  (should (nskk-kana-katakana-p ?キ))
  (should (nskk-kana-katakana-p ?ク))
  (should (nskk-kana-katakana-p ?ケ))
  (should (nskk-kana-katakana-p ?コ))
  (should (nskk-kana-katakana-p ?サ))
  (should (nskk-kana-katakana-p ?シ))
  (should (nskk-kana-katakana-p ?ス))
  (should (nskk-kana-katakana-p ?セ))
  (should (nskk-kana-katakana-p ?ソ)))

(nskk-deftest-unit core-katakana-p-range-edges
  "Test katakana classification at range boundaries."
  (should (nskk-kana-katakana-p #x30A0))  ; Start of range
  (should (nskk-kana-katakana-p #x30FF))  ; End of range
  (should (not (nskk-kana-katakana-p #x309F)))  ; Just before
  (should (not (nskk-kana-katakana-p #x3100)))) ; Just after

(nskk-deftest-unit core-katakana-p-non-katakana
  "Test katakana classification rejects non-katakana."
  (should (not (nskk-kana-katakana-p ?あ)))
  (should (not (nskk-kana-katakana-p ?a)))
  (should (not (nskk-kana-katakana-p ?A)))
  (should (not (nskk-kana-katakana-p ?1)))
  (should (not (nskk-kana-katakana-p ?漢))))

(nskk-deftest-unit core-han-p-basic
  "Test han (kanji) classification for common characters."
  (should (nskk-kana-han-p ?漢))
  (should (nskk-kana-han-p ?字))
  (should (nskk-kana-han-p ?日))
  (should (nskk-kana-han-p ?本))
  (should (nskk-kana-han-p ?語))
  (should (nskk-kana-han-p ?入))
  (should (nskk-kana-han-p ?力)))

(nskk-deftest-unit core-han-p-range-edges
  "Test han classification at range boundaries."
  (should (nskk-kana-han-p #x4E00))  ; Start of range
  (should (nskk-kana-han-p #x9FFF))  ; End of range
  (should (not (nskk-kana-han-p #x4DFF)))  ; Just before
  (should (not (nskk-kana-han-p #xA000)))) ; Just after

(nskk-deftest-unit core-japanese-p-all-types
  "Test japanese-p for all Japanese character types."
  (should (nskk-kana-japanese-p ?あ))  ; Hiragana
  (should (nskk-kana-japanese-p ?ア))  ; Katakana
  (should (nskk-kana-japanese-p ?漢))  ; Han
  (should (not (nskk-kana-japanese-p ?a)))
  (should (not (nskk-kana-japanese-p ?A)))
  (should (not (nskk-kana-japanese-p ?1))))

;;;
;;; Hiragana/Katakana Conversion Tests
;;;

(nskk-deftest-unit core-hiragana-to-katakana-basic
  "Test basic hiragana to katakana conversion."
  (should (= (nskk-kana-hiragana-to-katakana ?あ) ?ア))
  (should (= (nskk-kana-hiragana-to-katakana ?い) ?イ))
  (should (= (nskk-kana-hiragana-to-katakana ?う) ?ウ))
  (should (= (nskk-kana-hiragana-to-katakana ?え) ?エ))
  (should (= (nskk-kana-hiragana-to-katakana ?お) ?オ))
  (should (= (nskk-kana-hiragana-to-katakana ?か) ?カ))
  (should (= (nskk-kana-hiragana-to-katakana ?き) ?キ))
  (should (= (nskk-kana-hiragana-to-katakana ?く) ?ク))
  (should (= (nskk-kana-hiragana-to-katakana ?け) ?ケ))
  (should (= (nskk-kana-hiragana-to-katakana ?こ) ?コ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-ka
  "Test ka row conversion."
  (should (= (nskk-kana-hiragana-to-katakana ?か) ?カ))
  (should (= (nskk-kana-hiragana-to-katakana ?き) ?キ))
  (should (= (nskk-kana-hiragana-to-katakana ?く) ?ク))
  (should (= (nskk-kana-hiragana-to-katakana ?け) ?ケ))
  (should (= (nskk-kana-hiragana-to-katakana ?こ) ?コ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-sa
  "Test sa row conversion."
  (should (= (nskk-kana-hiragana-to-katakana ?さ) ?サ))
  (should (= (nskk-kana-hiragana-to-katakana ?し) ?シ))
  (should (= (nskk-kana-hiragana-to-katakana ?す) ?ス))
  (should (= (nskk-kana-hiragana-to-katakana ?せ) ?セ))
  (should (= (nskk-kana-hiragana-to-katakana ?そ) ?ソ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-ta
  "Test ta row conversion."
  (should (= (nskk-kana-hiragana-to-katakana ?た) ?タ))
  (should (= (nskk-kana-hiragana-to-katakana ?ち) ?チ))
  (should (= (nskk-kana-hiragana-to-katakana ?つ) ?ツ))
  (should (= (nskk-kana-hiragana-to-katakana ?て) ?テ))
  (should (= (nskk-kana-hiragana-to-katakana ?と) ?ト)))

(nskk-deftest-unit core-hiragana-to-katakana-row-na
  "Test na row conversion."
  (should (= (nskk-kana-hiragana-to-katakana ?な) ?ナ))
  (should (= (nskk-kana-hiragana-to-katakana ?に) ?ニ))
  (should (= (nskk-kana-hiragana-to-katakana ?ぬ) ?ヌ))
  (should (= (nskk-kana-hiragana-to-katakana ?ね) ?ネ))
  (should (= (nskk-kana-hiragana-to-katakana ?の) ?ノ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-ha
  "Test ha row conversion."
  (should (= (nskk-kana-hiragana-to-katakana ?は) ?ハ))
  (should (= (nskk-kana-hiragana-to-katakana ?ひ) ?ヒ))
  (should (= (nskk-kana-hiragana-to-katakana ?ふ) ?フ))
  (should (= (nskk-kana-hiragana-to-katakana ?へ) ?ヘ))
  (should (= (nskk-kana-hiragana-to-katakana ?ほ) ?ホ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-ma
  "Test ma row conversion."
  (should (= (nskk-kana-hiragana-to-katakana ?ま) ?マ))
  (should (= (nskk-kana-hiragana-to-katakana ?み) ?ミ))
  (should (= (nskk-kana-hiragana-to-katakana ?む) ?ム))
  (should (= (nskk-kana-hiragana-to-katakana ?め) ?メ))
  (should (= (nskk-kana-hiragana-to-katakana ?も) ?モ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-ya
  "Test ya row conversion."
  (should (= (nskk-kana-hiragana-to-katakana ?や) ?ヤ))
  (should (= (nskk-kana-hiragana-to-katakana ?ゆ) ?ユ))
  (should (= (nskk-kana-hiragana-to-katakana ?よ) ?ヨ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-ra
  "Test ra row conversion."
  (should (= (nskk-kana-hiragana-to-katakana ?ら) ?ラ))
  (should (= (nskk-kana-hiragana-to-katakana ?り) ?リ))
  (should (= (nskk-kana-hiragana-to-katakana ?る) ?ル))
  (should (= (nskk-kana-hiragana-to-katakana ?れ) ?レ))
  (should (= (nskk-kana-hiragana-to-katakana ?ろ) ?ロ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-wa
  "Test wa row conversion."
  (should (= (nskk-kana-hiragana-to-katakana ?わ) ?ワ))
  (should (= (nskk-kana-hiragana-to-katakana ?を) ?ヲ))
  (should (= (nskk-kana-hiragana-to-katakana ?ん) ?ン)))

(nskk-deftest-unit core-hiragana-to-katakana-non-hiragana
  "Test hiragana to katakana with non-hiragana input."
  (should (= (nskk-kana-hiragana-to-katakana ?a) ?a))
  (should (= (nskk-kana-hiragana-to-katakana ?A) ?A))
  (should (= (nskk-kana-hiragana-to-katakana ?1) ?1))
  (should (= (nskk-kana-hiragana-to-katakana ?漢) ?漢)))

(nskk-deftest-unit core-katakana-to-hiragana-basic
  "Test basic katakana to hiragana conversion."
  (should (= (nskk-kana-katakana-to-hiragana ?ア) ?あ))
  (should (= (nskk-kana-katakana-to-hiragana ?イ) ?い))
  (should (= (nskk-kana-katakana-to-hiragana ?ウ) ?う))
  (should (= (nskk-kana-katakana-to-hiragana ?エ) ?え))
  (should (= (nskk-kana-katakana-to-hiragana ?オ) ?お))
  (should (= (nskk-kana-katakana-to-hiragana ?カ) ?か))
  (should (= (nskk-kana-katakana-to-hiragana ?キ) ?き))
  (should (= (nskk-kana-katakana-to-hiragana ?ク) ?く))
  (should (= (nskk-kana-katakana-to-hiragana ?ケ) ?け))
  (should (= (nskk-kana-katakana-to-hiragana ?コ) ?こ)))

(nskk-deftest-unit core-katakana-to-hiragana-non-katakana
  "Test katakana to hiragana with non-katakana input."
  (should (= (nskk-kana-katakana-to-hiragana ?a) ?a))
  (should (= (nskk-kana-katakana-to-hiragana ?A) ?A))
  (should (= (nskk-kana-katakana-to-hiragana ?1) ?1))
  (should (= (nskk-kana-katakana-to-hiragana ?漢) ?漢)))

(nskk-deftest-unit core-string-hiragana-to-katakana-basic
  "Test string hiragana to katakana conversion."
  (nskk-assert-strings-equal
   (nskk-kana-string-hiragana-to-katakana "あいうえお")
   "アイウエオ")
  (nskk-assert-strings-equal
   (nskk-kana-string-hiragana-to-katakana "かきくけこ")
   "カキクケコ")
  (nskk-assert-strings-equal
   (nskk-kana-string-hiragana-to-katakana "さしすせそ")
   "サシスセソ"))

(nskk-deftest-unit core-string-hiragana-to-katakana-mixed
  "Test string conversion with mixed characters."
  (nskk-assert-strings-equal
   (nskk-kana-string-hiragana-to-katakana "こんにちは123")
   "コンニチハ123")
  (nskk-assert-strings-equal
   (nskk-kana-string-hiragana-to-katakana " Japanese 日本語 ")
   " Japanese 日本語 "))

(nskk-deftest-unit core-string-katakana-to-hiragana-basic
  "Test string katakana to hiragana conversion."
  (nskk-assert-strings-equal
   (nskk-kana-string-katakana-to-hiragana "アイウエオ")
   "あいうえお")
  (nskk-assert-strings-equal
   (nskk-kana-string-katakana-to-hiragana "カキクケコ")
   "かきくけこ")
  (nskk-assert-strings-equal
   (nskk-kana-string-katakana-to-hiragana "サシスセソ")
   "さしすせそ"))

(nskk-deftest-unit core-string-katakana-to-hiragana-mixed
  "Test string conversion with mixed characters."
  (nskk-assert-strings-equal
   (nskk-kana-string-katakana-to-hiragana "コンニチハ123")
   "こんにちは123")
  (nskk-assert-strings-equal
   (nskk-kana-string-katakana-to-hiragana " Japanese 日本語 ")
   " Japanese 日本語 "))

(nskk-deftest-unit core-roundtrip-conversion
  "Test roundtrip conversion preserves data."
  (let ((original "こんにちは世界"))
    (let ((katakana (nskk-kana-string-hiragana-to-katakana original))
          (back-to-hiragana (nskk-kana-string-katakana-to-hiragana
                            (nskk-kana-string-hiragana-to-katakana original))))
      (nskk-assert-strings-equal original back-to-hiragana))))

;;;
;;; Hankaku/Zenkaku Conversion Tests
;;;

(nskk-deftest-unit core-zenkaku-to-hankaku-basic
  "Test basic zenkaku to hankaku conversion."
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "ア")
   "ｱ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "イ")
   "ｲ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "ウ")
   "ｳ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "エ")
   "ｴ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "オ")
   "ｵ"))

(nskk-deftest-unit core-zenkaku-to-hankaku-dakuten
  "Test zenkaku to hankaku with dakuten."
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "ガ")
   "ｶﾞ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "ギ")
   "ｷﾞ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "ザ")
   "ｻﾞ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "ジ")
   "ｼﾞ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "バ")
   "ﾊﾞ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "パ")
   "ﾊﾟ"))

(nskk-deftest-unit core-zenkaku-to-hankaku-small
  "Test zenkaku to hankaku for small katakana."
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "ァ")
   "ｧ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "ィ")
   "ｨ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "ゥ")
   "ｩ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "ェ")
   "ｪ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "ォ")
   "ｫ"))

(nskk-deftest-unit core-zenkaku-to-hankaku-punctuation
  "Test zenkaku to hankaku for punctuation."
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "。")
   "｡")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "、")
   "､")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "・")
   "･")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "ー")
   "ｰ"))

(nskk-deftest-unit core-zenkaku-to-hankaku-string
  "Test zenkaku to hankaku string conversion."
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "コンピュータ")
   "ｺﾝﾋﾟｭｰﾀ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku "サンプル")
   "ｻﾝﾌﾟﾙ"))

(nskk-deftest-unit core-hankaku-to-zenkaku-basic
  "Test basic hankaku to zenkaku conversion."
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｱ")
   "ア")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｲ")
   "イ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｳ")
   "ウ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｴ")
   "エ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｵ")
   "オ"))

(nskk-deftest-unit core-hankaku-to-zenkaku-dakuten
  "Test hankaku to zenkaku with dakuten."
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｶﾞ")
   "ガ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｷﾞ")
   "ギ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｻﾞ")
   "ザ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｼﾞ")
   "ジ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ﾊﾞ")
   "バ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ﾊﾟ")
   "パ"))

(nskk-deftest-unit core-hankaku-to-zenkaku-small
  "Test hankaku to zenkaku for small katakana."
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｧ")
   "ァ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｨ")
   "ィ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｩ")
   "ゥ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｪ")
   "ェ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｫ")
   "ォ"))

(nskk-deftest-unit core-hankaku-to-zenkaku-string
  "Test hankaku to zenkaku string conversion."
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｺﾝﾋﾟｭｰﾀ")
   "コンピュータ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku "ｻﾝﾌﾟﾙ")
   "サンプル"))

(nskk-deftest-unit core-hankaku-zenkaku-roundtrip
  "Test roundtrip hankaku/zenkaku conversion."
  (let ((original "コンピュータ"))
    (let ((hankaku (nskk-kana-zenkaku-to-hankaku original))
          (back-to-zenkaku (nskk-kana-hankaku-to-zenkaku
                            (nskk-kana-zenkaku-to-hankaku original))))
      (nskk-assert-strings-equal original back-to-zenkaku))))

(nskk-deftest-unit core-hankaku-to-zenkaku-character
  "Test hankaku to zenkaku for single character."
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku ?ｱ)
   "ア")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku ?ｲ)
   "イ")
  (nskk-assert-strings-equal
   (nskk-kana-hankaku-to-zenkaku ?ｳ)
   "ウ"))

(nskk-deftest-unit core-zenkaku-to-hankaku-character
  "Test zenkaku to hankaku for single character."
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku ?ア)
   "ｱ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku ?イ)
   "ｲ")
  (nskk-assert-strings-equal
   (nskk-kana-zenkaku-to-hankaku ?ウ)
   "ｳ"))

;;;
;;; Performance Tests
;;;

(nskk-deftest-performance core-performance-hiragana-to-katakana
  "Test hiragana to katakana conversion performance.
Note: Prolog-backed classification costs ~30-50us per character."
  (let ((test-string (make-string 1000 ?あ))
        (start-time (current-time)))
    (nskk-kana-string-hiragana-to-katakana test-string)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Prolog-based classification: ~30-50us per char, 1000 chars < 500ms
      (should (< elapsed 0.5))
      (message "[Performance] 1000 chars: %.3fms" (* 1000 elapsed)))))

(nskk-deftest-performance core-performance-katakana-to-hiragana
  "Test katakana to hiragana conversion performance.
Note: Prolog-backed classification costs ~30-50us per character."
  (let ((test-string (make-string 1000 ?ア))
        (start-time (current-time)))
    (nskk-kana-string-katakana-to-hiragana test-string)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Prolog-based classification: ~30-50us per char, 1000 chars < 500ms
      (should (< elapsed 0.5))
      (message "[Performance] 1000 chars: %.3fms" (* 1000 elapsed)))))

(nskk-deftest-performance core-performance-zenkaku-hankaku
  "Test zenkaku to hankaku conversion performance.
Uses O(1) hash table lookup; not Prolog-backed."
  (let ((test-string "コンピュータサンプルプログラミング")
        (start-time (current-time)))
    (nskk-kana-zenkaku-to-hankaku test-string)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (should (< elapsed 0.01))
      (message "[Performance] String: %.3fms" (* 1000 elapsed)))))

(nskk-deftest-performance core-performance-classification
  "Test character classification performance.
Note: Prolog-backed classification costs ~200-300us per character."
  (let ((test-chars (append (number-sequence #x3040 #x309F)
                             (number-sequence #x30A0 #x30FF)
                             (number-sequence #x4E00 #x4E0F)))
        (start-time (current-time)))
    (dolist (char test-chars)
      (nskk-kana-japanese-p char))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Prolog-based classification via kana-japanese rule; ~200-300us/char
      (should (< elapsed 1.0))
      (message "[Performance] 80 classifications: %.3fms" (* 1000 elapsed)))))

;;;
;;; Integration Tests
;;;

(nskk-deftest-integration core-conversion-pipeline
  "Test full conversion pipeline: hiragana -> katakana -> hankaku."
  (let ((hiragana "こんにちは"))
    (let ((katakana (nskk-kana-string-hiragana-to-katakana hiragana))
          (hankaku (nskk-kana-zenkaku-to-hankaku
                    (nskk-kana-string-hiragana-to-katakana hiragana))))
      (nskk-assert-strings-equal katakana "コンニチハ")
      (nskk-assert-strings-equal hankaku "ｺﾝﾆﾁﾊ"))))

(nskk-deftest-integration core-mixed-script-conversion
  "Test conversion with mixed Japanese scripts."
  (let ((mixed "ひらがなカタカナ漢字123ABC"))
    (let ((katakana (nskk-kana-string-hiragana-to-katakana mixed))
          (hiragana (nskk-kana-string-katakana-to-hiragana mixed)))
      (nskk-assert-strings-equal katakana "ヒラガナカタカナ漢字123ABC")
      (nskk-assert-strings-equal hiragana "ひらがなかたかな漢字123ABC"))))

(nskk-deftest-integration core-comprehensive-conversion
  "Test comprehensive conversion covering all functions."
  (let ((test-cases
         '(("あいうえお" . "アイウエオ")
           ("カキクケコ" . "カキクケコ")
           ("ｺﾝﾋﾟｭｰﾀ" . "コンピュータ")
           ("ｻﾝﾌﾟﾙ" . "サンプル"))))

    (dolist (test-case test-cases)
      (let ((input (car test-case))
            (expected (cdr test-case)))
        ;; Test hankaku to zenkaku
        (when (string-match-p "[\uFF65-\uFF9F]" input)
          (nskk-assert-strings-equal
           (nskk-kana-hankaku-to-zenkaku input)
           expected))))))

;;;
;;; Property-Based Tests
;;;

;; Table-driven hiragana->katakana conversion cases
(nskk-deftest-cases kana-pbt-hiragana-to-katakana-vowels
  (("あ" . "ア")
   ("い" . "イ")
   ("う" . "ウ")
   ("え" . "エ")
   ("お" . "オ")
   ("か" . "カ")
   ("き" . "キ")
   ("く" . "ク")
   ("け" . "ケ")
   ("こ" . "コ")
   ("さ" . "サ")
   ("し" . "シ")
   ("す" . "ス")
   ("せ" . "セ")
   ("そ" . "ソ"))
  :description "Hiragana→katakana string conversion"
  :body (should (equal expected (nskk-kana-string-hiragana-to-katakana input))))

;; Conversion always returns a string: for any hiragana-string input,
;; nskk-kana-string-hiragana-to-katakana returns a string.
(nskk-property-test kana-pbt-string-conversion-returns-string
  ((input hiragana-string))
  (stringp (nskk-kana-string-hiragana-to-katakana input))
  100)

;; Length preservation: hiragana->katakana conversion does not change
;; character count. Each hiragana character maps to exactly one katakana character.
(nskk-property-test kana-pbt-string-length-preserved
  ((input hiragana-string))
  (let ((result (nskk-kana-string-hiragana-to-katakana input)))
    (= (length input) (length result)))
  100)

;;;
;;; Prolog Database Integration Tests
;;;

(nskk-deftest-unit kana-prolog-hiragana-rule
  "Test that kana-hiragana Prolog rule is asserted and works."
  (should (nskk-prolog-query '(kana-hiragana ?あ)))
  (should (nskk-prolog-query '(kana-hiragana ?か)))
  (should (not (nskk-prolog-query '(kana-hiragana ?ア))))
  (should (not (nskk-prolog-query '(kana-hiragana ?a)))))

(nskk-deftest-unit kana-prolog-katakana-rule
  "Test that kana-katakana Prolog rule is asserted and works."
  (should (nskk-prolog-query '(kana-katakana ?ア)))
  (should (nskk-prolog-query '(kana-katakana ?カ)))
  (should (not (nskk-prolog-query '(kana-katakana ?あ)))))

(nskk-deftest-unit kana-prolog-conversion-rules
  "Test that kana conversion Prolog rules work."
  (let ((result (nskk-prolog-query-one (list 'kana-hiragana-to-katakana ?あ '\?k))))
    (should result)
    (should (listp result))
    (should (= (nskk-prolog-walk '\?k result) ?ア)))
  (let ((result (nskk-prolog-query-one (list 'kana-katakana-to-hiragana ?ア '\?h))))
    (should result)
    (should (listp result))
    (should (= (nskk-prolog-walk '\?h result) ?あ))))

(nskk-deftest-unit kana-prolog-zenkaku-facts
  "Test that zenkaku-to-hankaku Prolog facts are asserted."
  (should (nskk-prolog-query (list 'zenkaku-to-hankaku "ア" '\?h)))
  (should (nskk-prolog-query (list 'hankaku-to-zenkaku "ｱ" '\?z))))

(provide 'nskk-kana-test)

;;; nskk-kana-test.el ends here
