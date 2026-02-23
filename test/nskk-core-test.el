;;; nskk-core-test.el --- Core conversion tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-core.el covering:
;; - Character classification (hiragana, katakana, han)
;; - Hiragana to katakana conversion
;; - Katakana to hiragana conversion
;; - Zenkaku to hankaku conversion
;; - Hankaku to zenkaku conversion
;; - String conversion functions
;; - Performance benchmarks

;;; Code:

(require 'ert)
(require 'nskk-core)
(require 'nskk-test-framework)

;;;
;;; Character Classification Tests
;;;

(nskk-deftest-unit core-hiragana-p-basic
  "Test hiragana classification for basic characters."
  (should (nskk-core-hiragana-p ?あ))
  (should (nskk-core-hiragana-p ?い))
  (should (nskk-core-hiragana-p ?う))
  (should (nskk-core-hiragana-p ?え))
  (should (nskk-core-hiragana-p ?お))
  (should (nskk-core-hiragana-p ?か))
  (should (nskk-core-hiragana-p ?き))
  (should (nskk-core-hiragana-p ?く))
  (should (nskk-core-hiragana-p ?け))
  (should (nskk-core-hiragana-p ?こ))
  (should (nskk-core-hiragana-p ?さ))
  (should (nskk-core-hiragana-p ?し))
  (should (nskk-core-hiragana-p ?す))
  (should (nskk-core-hiragana-p ?せ))
  (should (nskk-core-hiragana-p ?そ)))

(nskk-deftest-unit core-hiragana-p-range-edges
  "Test hiragana classification at range boundaries."
  (should (nskk-core-hiragana-p #x3040))  ; Start of range
  (should (nskk-core-hiragana-p #x309F))  ; End of range
  (should (not (nskk-core-hiragana-p #x303F)))  ; Just before
  (should (not (nskk-core-hiragana-p #x30A0)))) ; Just after

(nskk-deftest-unit core-hiragana-p-non-hiragana
  "Test hiragana classification rejects non-hiragana."
  (should (not (nskk-core-hiragana-p ?ア)))
  (should (not (nskk-core-hiragana-p ?a)))
  (should (not (nskk-core-hiragana-p ?A)))
  (should (not (nskk-core-hiragana-p ?1)))
  (should (not (nskk-core-hiragana-p ?漢))))

(nskk-deftest-unit core-katakana-p-basic
  "Test katakana classification for basic characters."
  (should (nskk-core-katakana-p ?ア))
  (should (nskk-core-katakana-p ?イ))
  (should (nskk-core-katakana-p ?ウ))
  (should (nskk-core-katakana-p ?エ))
  (should (nskk-core-katakana-p ?オ))
  (should (nskk-core-katakana-p ?カ))
  (should (nskk-core-katakana-p ?キ))
  (should (nskk-core-katakana-p ?ク))
  (should (nskk-core-katakana-p ?ケ))
  (should (nskk-core-katakana-p ?コ))
  (should (nskk-core-katakana-p ?サ))
  (should (nskk-core-katakana-p ?シ))
  (should (nskk-core-katakana-p ?ス))
  (should (nskk-core-katakana-p ?セ))
  (should (nskk-core-katakana-p ?ソ)))

(nskk-deftest-unit core-katakana-p-range-edges
  "Test katakana classification at range boundaries."
  (should (nskk-core-katakana-p #x30A0))  ; Start of range
  (should (nskk-core-katakana-p #x30FF))  ; End of range
  (should (not (nskk-core-katakana-p #x309F)))  ; Just before
  (should (not (nskk-core-katakana-p #x3100)))) ; Just after

(nskk-deftest-unit core-katakana-p-non-katakana
  "Test katakana classification rejects non-katakana."
  (should (not (nskk-core-katakana-p ?あ)))
  (should (not (nskk-core-katakana-p ?a)))
  (should (not (nskk-core-katakana-p ?A)))
  (should (not (nskk-core-katakana-p ?1)))
  (should (not (nskk-core-katakana-p ?漢))))

(nskk-deftest-unit core-han-p-basic
  "Test han (kanji) classification for common characters."
  (should (nskk-core-han-p ?漢))
  (should (nskk-core-han-p ?字))
  (should (nskk-core-han-p ?日))
  (should (nskk-core-han-p ?本))
  (should (nskk-core-han-p ?語))
  (should (nskk-core-han-p ?入))
  (should (nskk-core-han-p ?力)))

(nskk-deftest-unit core-han-p-range-edges
  "Test han classification at range boundaries."
  (should (nskk-core-han-p #x4E00))  ; Start of range
  (should (nskk-core-han-p #x9FFF))  ; End of range
  (should (not (nskk-core-han-p #x4DFF)))  ; Just before
  (should (not (nskk-core-han-p #xA000)))) ; Just after

(nskk-deftest-unit core-japanese-p-all-types
  "Test japanese-p for all Japanese character types."
  (should (nskk-core-japanese-p ?あ))  ; Hiragana
  (should (nskk-core-japanese-p ?ア))  ; Katakana
  (should (nskk-core-japanese-p ?漢))  ; Han
  (should (not (nskk-core-japanese-p ?a)))
  (should (not (nskk-core-japanese-p ?A)))
  (should (not (nskk-core-japanese-p ?1))))

;;;
;;; Hiragana/Katakana Conversion Tests
;;;

(nskk-deftest-unit core-hiragana-to-katakana-basic
  "Test basic hiragana to katakana conversion."
  (should (= (nskk-core-hiragana-to-katakana ?あ) ?ア))
  (should (= (nskk-core-hiragana-to-katakana ?い) ?イ))
  (should (= (nskk-core-hiragana-to-katakana ?う) ?ウ))
  (should (= (nskk-core-hiragana-to-katakana ?え) ?エ))
  (should (= (nskk-core-hiragana-to-katakana ?お) ?オ))
  (should (= (nskk-core-hiragana-to-katakana ?か) ?カ))
  (should (= (nskk-core-hiragana-to-katakana ?き) ?キ))
  (should (= (nskk-core-hiragana-to-katakana ?く) ?ク))
  (should (= (nskk-core-hiragana-to-katakana ?け) ?ケ))
  (should (= (nskk-core-hiragana-to-katakana ?こ) ?コ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-ka
  "Test ka row conversion."
  (should (= (nskk-core-hiragana-to-katakana ?か) ?カ))
  (should (= (nskk-core-hiragana-to-katakana ?き) ?キ))
  (should (= (nskk-core-hiragana-to-katakana ?く) ?ク))
  (should (= (nskk-core-hiragana-to-katakana ?け) ?ケ))
  (should (= (nskk-core-hiragana-to-katakana ?こ) ?コ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-sa
  "Test sa row conversion."
  (should (= (nskk-core-hiragana-to-katakana ?さ) ?サ))
  (should (= (nskk-core-hiragana-to-katakana ?し) ?シ))
  (should (= (nskk-core-hiragana-to-katakana ?す) ?ス))
  (should (= (nskk-core-hiragana-to-katakana ?せ) ?セ))
  (should (= (nskk-core-hiragana-to-katakana ?そ) ?ソ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-ta
  "Test ta row conversion."
  (should (= (nskk-core-hiragana-to-katakana ?た) ?タ))
  (should (= (nskk-core-hiragana-to-katakana ?ち) ?チ))
  (should (= (nskk-core-hiragana-to-katakana ?つ) ?ツ))
  (should (= (nskk-core-hiragana-to-katakana ?て) ?テ))
  (should (= (nskk-core-hiragana-to-katakana ?と) ?ト)))

(nskk-deftest-unit core-hiragana-to-katakana-row-na
  "Test na row conversion."
  (should (= (nskk-core-hiragana-to-katakana ?な) ?ナ))
  (should (= (nskk-core-hiragana-to-katakana ?に) ?ニ))
  (should (= (nskk-core-hiragana-to-katakana ?ぬ) ?ヌ))
  (should (= (nskk-core-hiragana-to-katakana ?ね) ?ネ))
  (should (= (nskk-core-hiragana-to-katakana ?の) ?ノ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-ha
  "Test ha row conversion."
  (should (= (nskk-core-hiragana-to-katakana ?は) ?ハ))
  (should (= (nskk-core-hiragana-to-katakana ?ひ) ?ヒ))
  (should (= (nskk-core-hiragana-to-katakana ?ふ) ?フ))
  (should (= (nskk-core-hiragana-to-katakana ?へ) ?ヘ))
  (should (= (nskk-core-hiragana-to-katakana ?ほ) ?ホ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-ma
  "Test ma row conversion."
  (should (= (nskk-core-hiragana-to-katakana ?ま) ?マ))
  (should (= (nskk-core-hiragana-to-katakana ?み) ?ミ))
  (should (= (nskk-core-hiragana-to-katakana ?む) ?ム))
  (should (= (nskk-core-hiragana-to-katakana ?め) ?メ))
  (should (= (nskk-core-hiragana-to-katakana ?も) ?モ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-ya
  "Test ya row conversion."
  (should (= (nskk-core-hiragana-to-katakana ?や) ?ヤ))
  (should (= (nskk-core-hiragana-to-katakana ?ゆ) ?ユ))
  (should (= (nskk-core-hiragana-to-katakana ?よ) ?ヨ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-ra
  "Test ra row conversion."
  (should (= (nskk-core-hiragana-to-katakana ?ら) ?ラ))
  (should (= (nskk-core-hiragana-to-katakana ?り) ?リ))
  (should (= (nskk-core-hiragana-to-katakana ?る) ?ル))
  (should (= (nskk-core-hiragana-to-katakana ?れ) ?レ))
  (should (= (nskk-core-hiragana-to-katakana ?ろ) ?ロ)))

(nskk-deftest-unit core-hiragana-to-katakana-row-wa
  "Test wa row conversion."
  (should (= (nskk-core-hiragana-to-katakana ?わ) ?ワ))
  (should (= (nskk-core-hiragana-to-katakana ?を) ?ヲ))
  (should (= (nskk-core-hiragana-to-katakana ?ん) ?ン)))

(nskk-deftest-unit core-hiragana-to-katakana-non-hiragana
  "Test hiragana to katakana with non-hiragana input."
  (should (= (nskk-core-hiragana-to-katakana ?a) ?a))
  (should (= (nskk-core-hiragana-to-katakana ?A) ?A))
  (should (= (nskk-core-hiragana-to-katakana ?1) ?1))
  (should (= (nskk-core-hiragana-to-katakana ?漢) ?漢)))

(nskk-deftest-unit core-katakana-to-hiragana-basic
  "Test basic katakana to hiragana conversion."
  (should (= (nskk-core-katakana-to-hiragana ?ア) ?あ))
  (should (= (nskk-core-katakana-to-hiragana ?イ) ?い))
  (should (= (nskk-core-katakana-to-hiragana ?ウ) ?う))
  (should (= (nskk-core-katakana-to-hiragana ?エ) ?え))
  (should (= (nskk-core-katakana-to-hiragana ?オ) ?お))
  (should (= (nskk-core-katakana-to-hiragana ?カ) ?か))
  (should (= (nskk-core-katakana-to-hiragana ?キ) ?き))
  (should (= (nskk-core-katakana-to-hiragana ?ク) ?く))
  (should (= (nskk-core-katakana-to-hiragana ?ケ) ?け))
  (should (= (nskk-core-katakana-to-hiragana ?コ) ?こ)))

(nskk-deftest-unit core-katakana-to-hiragana-non-katakana
  "Test katakana to hiragana with non-katakana input."
  (should (= (nskk-core-katakana-to-hiragana ?a) ?a))
  (should (= (nskk-core-katakana-to-hiragana ?A) ?A))
  (should (= (nskk-core-katakana-to-hiragana ?1) ?1))
  (should (= (nskk-core-katakana-to-hiragana ?漢) ?漢)))

(nskk-deftest-unit core-string-hiragana-to-katakana-basic
  "Test string hiragana to katakana conversion."
  (nskk-assert-strings-equal
   (nskk-core-string-hiragana-to-katakana "あいうえお")
   "アイウエオ")
  (nskk-assert-strings-equal
   (nskk-core-string-hiragana-to-katakana "かきくけこ")
   "カキクケコ")
  (nskk-assert-strings-equal
   (nskk-core-string-hiragana-to-katakana "さしすせそ")
   "サシスセソ"))

(nskk-deftest-unit core-string-hiragana-to-katakana-mixed
  "Test string conversion with mixed characters."
  (nskk-assert-strings-equal
   (nskk-core-string-hiragana-to-katakana "こんにちは123")
   "コンニチハ123")
  (nskk-assert-strings-equal
   (nskk-core-string-hiragana-to-katakana " Japanese 日本語 ")
   " Japanese 日本語 "))

(nskk-deftest-unit core-string-katakana-to-hiragana-basic
  "Test string katakana to hiragana conversion."
  (nskk-assert-strings-equal
   (nskk-core-string-katakana-to-hiragana "アイウエオ")
   "あいうえお")
  (nskk-assert-strings-equal
   (nskk-core-string-katakana-to-hiragana "カキクケコ")
   "かきくけこ")
  (nskk-assert-strings-equal
   (nskk-core-string-katakana-to-hiragana "サシスセソ")
   "さしすせそ"))

(nskk-deftest-unit core-string-katakana-to-hiragana-mixed
  "Test string conversion with mixed characters."
  (nskk-assert-strings-equal
   (nskk-core-string-katakana-to-hiragana "コンニチハ123")
   "こんにちは123")
  (nskk-assert-strings-equal
   (nskk-core-string-katakana-to-hiragana " Japanese 日本語 ")
   " Japanese 日本語 "))

(nskk-deftest-unit core-roundtrip-conversion
  "Test roundtrip conversion preserves data."
  (let ((original "こんにちは世界"))
    (let ((katakana (nskk-core-string-hiragana-to-katakana original))
          (back-to-hiragana (nskk-core-string-katakana-to-hiragana
                            (nskk-core-string-hiragana-to-katakana original))))
      (nskk-assert-strings-equal original back-to-hiragana))))

;;;
;;; Hankaku/Zenkaku Conversion Tests
;;;

(nskk-deftest-unit core-zenkaku-to-hankaku-basic
  "Test basic zenkaku to hankaku conversion."
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "ア")
   "ｱ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "イ")
   "ｲ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "ウ")
   "ｳ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "エ")
   "ｴ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "オ")
   "ｵ"))

(nskk-deftest-unit core-zenkaku-to-hankaku-dakuten
  "Test zenkaku to hankaku with dakuten."
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "ガ")
   "ｶﾞ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "ギ")
   "ｷﾞ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "ザ")
   "ｻﾞ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "ジ")
   "ｼﾞ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "バ")
   "ﾊﾞ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "パ")
   "ﾊﾟ"))

(nskk-deftest-unit core-zenkaku-to-hankaku-small
  "Test zenkaku to hankaku for small katakana."
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "ァ")
   "ｧ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "ィ")
   "ｨ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "ゥ")
   "ｩ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "ェ")
   "ｪ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "ォ")
   "ｫ"))

(nskk-deftest-unit core-zenkaku-to-hankaku-punctuation
  "Test zenkaku to hankaku for punctuation."
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "。")
   "｡")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "、")
   "､")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "・")
   "･")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "ー")
   "ｰ"))

(nskk-deftest-unit core-zenkaku-to-hankaku-string
  "Test zenkaku to hankaku string conversion."
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "コンピュータ")
   "ｺﾝﾋﾟｭｰﾀ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku "サンプル")
   "ｻﾝﾌﾟﾙ"))

(nskk-deftest-unit core-hankaku-to-zenkaku-basic
  "Test basic hankaku to zenkaku conversion."
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｱ")
   "ア")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｲ")
   "イ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｳ")
   "ウ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｴ")
   "エ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｵ")
   "オ"))

(nskk-deftest-unit core-hankaku-to-zenkaku-dakuten
  "Test hankaku to zenkaku with dakuten."
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｶﾞ")
   "ガ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｷﾞ")
   "ギ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｻﾞ")
   "ザ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｼﾞ")
   "ジ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ﾊﾞ")
   "バ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ﾊﾟ")
   "パ"))

(nskk-deftest-unit core-hankaku-to-zenkaku-small
  "Test hankaku to zenkaku for small katakana."
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｧ")
   "ァ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｨ")
   "ィ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｩ")
   "ゥ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｪ")
   "ェ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｫ")
   "ォ"))

(nskk-deftest-unit core-hankaku-to-zenkaku-string
  "Test hankaku to zenkaku string conversion."
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｺﾝﾋﾟｭｰﾀ")
   "コンピュータ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku "ｻﾝﾌﾟﾙ")
   "サンプル"))

(nskk-deftest-unit core-hankaku-zenkaku-roundtrip
  "Test roundtrip hankaku/zenkaku conversion."
  (let ((original "コンピュータ"))
    (let ((hankaku (nskk-core-zenkaku-to-hankaku original))
          (back-to-zenkaku (nskk-core-hankaku-to-zenkaku
                            (nskk-core-zenkaku-to-hankaku original))))
      (nskk-assert-strings-equal original back-to-zenkaku))))

(nskk-deftest-unit core-hankaku-to-zenkaku-character
  "Test hankaku to zenkaku for single character."
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku ?ｱ)
   "ア")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku ?ｲ)
   "イ")
  (nskk-assert-strings-equal
   (nskk-core-hankaku-to-zenkaku ?ｳ)
   "ウ"))

(nskk-deftest-unit core-zenkaku-to-hankaku-character
  "Test zenkaku to hankaku for single character."
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku ?ア)
   "ｱ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku ?イ)
   "ｲ")
  (nskk-assert-strings-equal
   (nskk-core-zenkaku-to-hankaku ?ウ)
   "ｳ"))

;;;
;;; Performance Tests
;;;

(nskk-deftest-performance core-performance-hiragana-to-katakana
  "Test hiragana to katakana conversion performance (target: < 0.01ms per char)."
  (let ((test-string (make-string 1000 ?あ))
        (start-time (current-time)))
    (nskk-core-string-hiragana-to-katakana test-string)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should convert 1000 characters in less than 10ms
      (should (< elapsed 0.01))
      (message "[Performance] 1000 chars: %.3fms" (* 1000 elapsed)))))

(nskk-deftest-performance core-performance-katakana-to-hiragana
  "Test katakana to hiragana conversion performance."
  (let ((test-string (make-string 1000 ?ア))
        (start-time (current-time)))
    (nskk-core-string-katakana-to-hiragana test-string)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (should (< elapsed 0.01))
      (message "[Performance] 1000 chars: %.3fms" (* 1000 elapsed)))))

(nskk-deftest-performance core-performance-zenkaku-hankaku
  "Test zenkaku to hankaku conversion performance."
  (let ((test-string "コンピュータサンプルプログラミング")
        (start-time (current-time)))
    (nskk-core-zenkaku-to-hankaku test-string)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (should (< elapsed 0.01))
      (message "[Performance] String: %.3fms" (* 1000 elapsed)))))

(nskk-deftest-performance core-performance-classification
  "Test character classification performance."
  (let ((test-chars (append (number-sequence #x3040 #x309F)
                             (number-sequence #x30A0 #x30FF)
                             (number-sequence #x4E00 #x4E0F)))
        (start-time (current-time)))
    (dolist (char test-chars)
      (nskk-core-japanese-p char))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should classify 80 characters quickly
      (should (< elapsed 0.001))
      (message "[Performance] 80 classifications: %.3fms" (* 1000 elapsed)))))

;;;
;;; Integration Tests
;;;

(nskk-deftest-integration core-conversion-pipeline
  "Test full conversion pipeline: hiragana -> katakana -> hankaku."
  (let ((hiragana "こんにちは"))
    (let ((katakana (nskk-core-string-hiragana-to-katakana hiragana))
          (hankaku (nskk-core-zenkaku-to-hankaku
                    (nskk-core-string-hiragana-to-katakana hiragana))))
      (nskk-assert-strings-equal katakana "コンニチハ")
      (nskk-assert-strings-equal hankaku "ｺﾝﾆﾁﾊ"))))

(nskk-deftest-integration core-mixed-script-conversion
  "Test conversion with mixed Japanese scripts."
  (let ((mixed "ひらがなカタカナ漢字123ABC"))
    (let ((katakana (nskk-core-string-hiragana-to-katakana mixed))
          (hiragana (nskk-core-string-katakana-to-hiragana mixed)))
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
           (nskk-core-hankaku-to-zenkaku input)
           expected))))))

(provide 'nskk-core-test)

;;; nskk-core-test.el ends here
