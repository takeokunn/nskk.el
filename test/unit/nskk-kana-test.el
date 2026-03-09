;;; nskk-kana-test.el --- Core conversion tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-kana.el covering:
;;
;; - Character classification (hiragana, katakana, hankaku-katakana, han, japanese)
;; - Boundary and rejection tests for all five predicates
;; - Hiragana <-> katakana single-character conversion
;; - String conversion: hiragana->katakana, katakana->hiragana
;; - CPS (/k) variant success and failure paths
;; - Zenkaku <-> hankaku conversion (basic, dakuten, small forms, punctuation)
;; - Roundtrip properties (hiragana<->katakana, zenkaku<->hankaku)
;; - Property-based tests (PBT): string type, length preservation, roundtrip
;; - Prolog database integration (rule assertions, fact tables)
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

(nskk-describe "hiragana character classification"
  (nskk-context "basic hiragana characters"
    (nskk-it "recognizes representative hiragana across a-, ka-, and sa-rows"
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
      (should (nskk-kana-hiragana-p ?そ))))

  (nskk-context "range boundary detection"
    (nskk-it "accepts characters at the start and end of the hiragana unicode range"
      (should (nskk-kana-hiragana-p #x3040))       ; Start of range
      (should (nskk-kana-hiragana-p #x309F))       ; End of range
      (should (not (nskk-kana-hiragana-p #x303F))) ; Just before
      (should (not (nskk-kana-hiragana-p #x30A0))))) ; Just after

  (nskk-context "non-hiragana rejection"
    (nskk-it "rejects katakana, ascii, digits, kanji, and non-integers"
      (should (not (nskk-kana-hiragana-p ?ア)))
      (should (not (nskk-kana-hiragana-p ?a)))
      (should (not (nskk-kana-hiragana-p ?A)))
      (should (not (nskk-kana-hiragana-p ?1)))
      (should (not (nskk-kana-hiragana-p ?漢)))
      (should (not (nskk-kana-hiragana-p nil)))
      (should (not (nskk-kana-hiragana-p "あ"))))))

(nskk-describe "katakana character classification"
  (nskk-context "basic katakana characters"
    (nskk-it "recognizes representative katakana across a-, ka-, and sa-rows"
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
      (should (nskk-kana-katakana-p ?ソ))))

  (nskk-context "range boundary detection"
    (nskk-it "accepts characters at the start and end of the katakana unicode range"
      (should (nskk-kana-katakana-p #x30A0))       ; Start of range
      (should (nskk-kana-katakana-p #x30FF))       ; End of range
      (should (not (nskk-kana-katakana-p #x309F))) ; Just before
      (should (not (nskk-kana-katakana-p #x3100))))) ; Just after

  (nskk-context "non-katakana rejection"
    (nskk-it "rejects hiragana, ascii, digits, kanji, and non-integers"
      (should (not (nskk-kana-katakana-p ?あ)))
      (should (not (nskk-kana-katakana-p ?a)))
      (should (not (nskk-kana-katakana-p ?A)))
      (should (not (nskk-kana-katakana-p ?1)))
      (should (not (nskk-kana-katakana-p ?漢)))
      (should (not (nskk-kana-katakana-p nil)))
      (should (not (nskk-kana-katakana-p "ア"))))))

(nskk-describe "hankaku katakana character classification"
  (nskk-context "basic hankaku katakana characters"
    (nskk-it "recognizes representative half-width katakana"
      (should (nskk-kana-hankaku-katakana-p ?ｱ))
      (should (nskk-kana-hankaku-katakana-p ?ｲ))
      (should (nskk-kana-hankaku-katakana-p ?ｳ))
      (should (nskk-kana-hankaku-katakana-p ?ｴ))
      (should (nskk-kana-hankaku-katakana-p ?ｵ))
      (should (nskk-kana-hankaku-katakana-p ?ｶ))
      (should (nskk-kana-hankaku-katakana-p ?ﾝ))
      (should (nskk-kana-hankaku-katakana-p ?ｦ))))

  (nskk-context "range boundary detection"
    (nskk-it "accepts characters at the start and end of the hankaku unicode range"
      (should (nskk-kana-hankaku-katakana-p #xFF65))       ; Start of range (｡)
      (should (nskk-kana-hankaku-katakana-p #xFF9F))       ; End of range (ﾟ)
      (should (not (nskk-kana-hankaku-katakana-p #xFF64))) ; Just before
      (should (not (nskk-kana-hankaku-katakana-p #xFFA0))))) ; Just after

  (nskk-context "non-hankaku-katakana rejection"
    (nskk-it "rejects zenkaku katakana, hiragana, and non-integers"
      (should (not (nskk-kana-hankaku-katakana-p ?ア)))
      (should (not (nskk-kana-hankaku-katakana-p ?あ)))
      (should (not (nskk-kana-hankaku-katakana-p ?a)))
      (should (not (nskk-kana-hankaku-katakana-p nil))))))

(nskk-describe "han (kanji) character classification"
  (nskk-context "common kanji characters"
    (nskk-it "recognizes common kanji characters"
      (should (nskk-kana-han-p ?漢))
      (should (nskk-kana-han-p ?字))
      (should (nskk-kana-han-p ?日))
      (should (nskk-kana-han-p ?本))
      (should (nskk-kana-han-p ?語))
      (should (nskk-kana-han-p ?入))
      (should (nskk-kana-han-p ?力))))

  (nskk-context "range boundary detection"
    (nskk-it "accepts characters at the start and end of the han unicode range"
      (should (nskk-kana-han-p #x4E00))           ; Start of CJK range
      (should (nskk-kana-han-p #x9FFF))           ; End of CJK range
      (should (not (nskk-kana-han-p #x4DFF)))     ; Just before
      (should (not (nskk-kana-han-p #xA000)))))   ; Just after

  (nskk-context "non-han rejection"
    (nskk-it "rejects hiragana, katakana, ascii, and non-integers"
      (should (not (nskk-kana-han-p ?あ)))
      (should (not (nskk-kana-han-p ?ア)))
      (should (not (nskk-kana-han-p ?a)))
      (should (not (nskk-kana-han-p nil))))))

(nskk-describe "japanese character classification"
  (nskk-context "all japanese script types"
    (nskk-it "accepts hiragana, katakana, han, and hankaku-katakana"
      (should (nskk-kana-japanese-p ?あ))  ; Hiragana
      (should (nskk-kana-japanese-p ?ア))  ; Katakana
      (should (nskk-kana-japanese-p ?漢))  ; Han
      (should (nskk-kana-japanese-p ?ｱ)))) ; Hankaku katakana

  (nskk-context "non-japanese rejection"
    (nskk-it "rejects ascii letters, digits, and non-integers"
      (should (not (nskk-kana-japanese-p ?a)))
      (should (not (nskk-kana-japanese-p ?A)))
      (should (not (nskk-kana-japanese-p ?1)))
      (should (not (nskk-kana-japanese-p nil))))))

;;;
;;; Hiragana/Katakana Conversion Tests
;;;

(nskk-deftest-table kana-hiragana-to-katakana-chars
  :columns (hiragana katakana)
  :rows ((?あ ?ア)
         (?い ?イ)
         (?う ?ウ)
         (?え ?エ)
         (?お ?オ)
         (?か ?カ)
         (?き ?キ)
         (?く ?ク)
         (?け ?ケ)
         (?こ ?コ)
         (?さ ?サ)
         (?し ?シ)
         (?す ?ス)
         (?せ ?セ)
         (?そ ?ソ)
         (?た ?タ)
         (?ち ?チ)
         (?つ ?ツ)
         (?て ?テ)
         (?と ?ト)
         (?な ?ナ)
         (?に ?ニ)
         (?ぬ ?ヌ)
         (?ね ?ネ)
         (?の ?ノ)
         (?は ?ハ)
         (?ひ ?ヒ)
         (?ふ ?フ)
         (?へ ?ヘ)
         (?ほ ?ホ)
         (?ま ?マ)
         (?み ?ミ)
         (?む ?ム)
         (?め ?メ)
         (?も ?モ)
         (?や ?ヤ)
         (?ゆ ?ユ)
         (?よ ?ヨ)
         (?ら ?ラ)
         (?り ?リ)
         (?る ?ル)
         (?れ ?レ)
         (?ろ ?ロ)
         (?わ ?ワ)
         (?を ?ヲ)
         (?ん ?ン))
  :description "Each hiragana character maps to its katakana equivalent"
  :body (should (= (nskk-kana-hiragana-to-katakana hiragana) katakana)))

(nskk-describe "hiragana to katakana conversion"
  (nskk-context "non-hiragana passthrough"
    (nskk-it "returns non-hiragana characters unchanged"
      (should (= (nskk-kana-hiragana-to-katakana ?a) ?a))
      (should (= (nskk-kana-hiragana-to-katakana ?A) ?A))
      (should (= (nskk-kana-hiragana-to-katakana ?1) ?1))
      (should (= (nskk-kana-hiragana-to-katakana ?漢) ?漢)))))

(nskk-deftest-table kana-katakana-to-hiragana-chars
  :columns (katakana hiragana)
  :rows ((?ア ?あ)
         (?イ ?い)
         (?ウ ?う)
         (?エ ?え)
         (?オ ?お)
         (?カ ?か)
         (?キ ?き)
         (?ク ?く)
         (?ケ ?け)
         (?コ ?こ)
         (?サ ?さ)
         (?シ ?し)
         (?ス ?す)
         (?セ ?せ)
         (?ソ ?そ)
         (?タ ?た)
         (?チ ?ち)
         (?ツ ?つ)
         (?テ ?て)
         (?ト ?と)
         (?ナ ?な)
         (?ニ ?に)
         (?ヌ ?ぬ)
         (?ネ ?ね)
         (?ノ ?の)
         (?ハ ?は)
         (?ヒ ?ひ)
         (?フ ?ふ)
         (?ヘ ?へ)
         (?ホ ?ほ)
         (?マ ?ま)
         (?ミ ?み)
         (?ム ?む)
         (?メ ?め)
         (?モ ?も)
         (?ヤ ?や)
         (?ユ ?ゆ)
         (?ヨ ?よ)
         (?ラ ?ら)
         (?リ ?り)
         (?ル ?る)
         (?レ ?れ)
         (?ロ ?ろ)
         (?ワ ?わ)
         (?ヲ ?を)
         (?ン ?ん))
  :description "Each katakana character maps to its hiragana equivalent"
  :body (should (= (nskk-kana-katakana-to-hiragana katakana) hiragana)))

(nskk-describe "katakana to hiragana conversion"
  (nskk-context "non-katakana passthrough"
    (nskk-it "returns non-katakana characters unchanged"
      (should (= (nskk-kana-katakana-to-hiragana ?a) ?a))
      (should (= (nskk-kana-katakana-to-hiragana ?A) ?A))
      (should (= (nskk-kana-katakana-to-hiragana ?1) ?1))
      (should (= (nskk-kana-katakana-to-hiragana ?漢) ?漢)))))

;;;
;;; String Conversion Tests
;;;

(nskk-deftest-table kana-string-hiragana-to-katakana
  :columns (input expected)
  :rows (("あいうえお" "アイウエオ")
         ("かきくけこ" "カキクケコ")
         ("さしすせそ" "サシスセソ")
         ("たちつてと" "タチツテト")
         ("なにぬねの" "ナニヌネノ"))
  :description "Hiragana strings convert to katakana strings"
  :body (nskk-assert-strings-equal (nskk-kana-string-hiragana-to-katakana input) expected))

(nskk-describe "string hiragana to katakana conversion"
  (nskk-context "mixed character strings"
    (nskk-it "converts hiragana and leaves non-hiragana unchanged"
      (nskk-assert-strings-equal
       (nskk-kana-string-hiragana-to-katakana "こんにちは123")
       "コンニチハ123")
      (nskk-assert-strings-equal
       (nskk-kana-string-hiragana-to-katakana " Japanese 日本語 ")
       " Japanese 日本語 ")))

  (nskk-context "edge cases"
    (nskk-it "returns nil for non-string input"
      (should (null (nskk-kana-string-hiragana-to-katakana nil)))
      (should (null (nskk-kana-string-hiragana-to-katakana 42))))
    (nskk-it "returns an empty string for empty string input"
      (nskk-assert-strings-equal
       (nskk-kana-string-hiragana-to-katakana "")
       "")))

  (nskk-context "roundtrip"
    (nskk-it "hiragana->katakana->hiragana preserves the original string"
      (let ((original "こんにちは世界"))
        (nskk-assert-strings-equal
         original
         (nskk-kana-string-katakana-to-hiragana
          (nskk-kana-string-hiragana-to-katakana original)))))))

(nskk-deftest-table kana-string-katakana-to-hiragana
  :columns (input expected)
  :rows (("アイウエオ" "あいうえお")
         ("カキクケコ" "かきくけこ")
         ("サシスセソ" "さしすせそ")
         ("タチツテト" "たちつてと")
         ("ナニヌネノ" "なにぬねの"))
  :description "Katakana strings convert to hiragana strings"
  :body (nskk-assert-strings-equal (nskk-kana-string-katakana-to-hiragana input) expected))

(nskk-describe "string katakana to hiragana conversion"
  (nskk-context "mixed character strings"
    (nskk-it "converts katakana and leaves non-katakana unchanged"
      (nskk-assert-strings-equal
       (nskk-kana-string-katakana-to-hiragana "コンニチハ123")
       "こんにちは123")
      (nskk-assert-strings-equal
       (nskk-kana-string-katakana-to-hiragana " Japanese 日本語 ")
       " Japanese 日本語 ")))

  (nskk-context "edge cases"
    (nskk-it "returns nil for non-string input"
      (should (null (nskk-kana-string-katakana-to-hiragana nil)))
      (should (null (nskk-kana-string-katakana-to-hiragana 42))))))

;;;
;;; CPS Variant Tests
;;;

(nskk-describe "CPS string hiragana-to-katakana/k"
  (nskk-context "success path"
    (nskk-it "calls on-result with converted string for valid string input"
      (let ((result nil))
        (nskk-kana-string-hiragana-to-katakana/k
         "あいう"
         (lambda (s) (setq result s))
         (lambda () (setq result 'fail)))
        (nskk-assert-strings-equal result "アイウ")))
    (nskk-it "calls on-result with empty string for empty input"
      (let ((result nil))
        (nskk-kana-string-hiragana-to-katakana/k
         ""
         (lambda (s) (setq result s))
         (lambda () (setq result 'fail)))
        (nskk-assert-strings-equal result ""))))

  (nskk-context "failure path"
    (nskk-it "calls on-fail for nil input"
      (let ((failed nil))
        (nskk-kana-string-hiragana-to-katakana/k
         nil
         (lambda (_) nil)
         (lambda () (setq failed t)))
        (should failed)))
    (nskk-it "calls on-fail for integer input"
      (let ((failed nil))
        (nskk-kana-string-hiragana-to-katakana/k
         42
         (lambda (_) nil)
         (lambda () (setq failed t)))
        (should failed)))))

(nskk-describe "CPS string katakana-to-hiragana/k"
  (nskk-context "success path"
    (nskk-it "calls on-result with converted string for valid string input"
      (let ((result nil))
        (nskk-kana-string-katakana-to-hiragana/k
         "アイウ"
         (lambda (s) (setq result s))
         (lambda () (setq result 'fail)))
        (nskk-assert-strings-equal result "あいう"))))

  (nskk-context "failure path"
    (nskk-it "calls on-fail for non-string input"
      (let ((failed nil))
        (nskk-kana-string-katakana-to-hiragana/k
         nil
         (lambda (_) nil)
         (lambda () (setq failed t)))
        (should failed)))))

(nskk-describe "CPS zenkaku-to-hankaku/k"
  (nskk-context "string input"
    (nskk-it "calls on-found with hankaku string"
      (let ((result nil))
        (nskk-kana-zenkaku-to-hankaku/k
         "アイウ"
         (lambda (s) (setq result s))
         #'ignore)
        (nskk-assert-strings-equal result "ｱｲｳ"))))

  (nskk-context "character input"
    (nskk-it "calls on-found with hankaku string for single char"
      (let ((result nil))
        (nskk-kana-zenkaku-to-hankaku/k
         ?ア
         (lambda (s) (setq result s))
         #'ignore)
        (nskk-assert-strings-equal result "ｱ"))))

  (nskk-context "unrecognized input passthrough"
    (nskk-it "passes through unrecognized strings unchanged"
      (let ((result nil))
        (nskk-kana-zenkaku-to-hankaku/k
         "abc"
         (lambda (s) (setq result s))
         #'ignore)
        (nskk-assert-strings-equal result "abc")))))

(nskk-describe "CPS hankaku-to-zenkaku/k"
  (nskk-context "string input"
    (nskk-it "calls on-found with zenkaku string"
      (let ((result nil))
        (nskk-kana-hankaku-to-zenkaku/k
         "ｱｲｳ"
         (lambda (s) (setq result s))
         #'ignore)
        (nskk-assert-strings-equal result "アイウ"))))

  (nskk-context "character input"
    (nskk-it "calls on-found with zenkaku string for single hankaku char"
      (let ((result nil))
        (nskk-kana-hankaku-to-zenkaku/k
         ?ｱ
         (lambda (s) (setq result s))
         #'ignore)
        (nskk-assert-strings-equal result "ア")))))

;;;
;;; Hankaku/Zenkaku Conversion Tests
;;;

(nskk-deftest-table kana-zenkaku-to-hankaku-basic
  :columns (input expected)
  :rows (("ア" "ｱ")
         ("イ" "ｲ")
         ("ウ" "ｳ")
         ("エ" "ｴ")
         ("オ" "ｵ"))
  :description "Basic zenkaku katakana converts to hankaku"
  :body (nskk-assert-strings-equal (nskk-kana-zenkaku-to-hankaku input) expected))

(nskk-deftest-table kana-zenkaku-to-hankaku-dakuten
  :columns (input expected)
  :rows (("ガ" "ｶﾞ")
         ("ギ" "ｷﾞ")
         ("ザ" "ｻﾞ")
         ("ジ" "ｼﾞ")
         ("バ" "ﾊﾞ")
         ("パ" "ﾊﾟ"))
  :description "Zenkaku voiced/semi-voiced katakana converts to hankaku with dakuten"
  :body (nskk-assert-strings-equal (nskk-kana-zenkaku-to-hankaku input) expected))

(nskk-deftest-table kana-zenkaku-to-hankaku-small
  :columns (input expected)
  :rows (("ァ" "ｧ")
         ("ィ" "ｨ")
         ("ゥ" "ｩ")
         ("ェ" "ｪ")
         ("ォ" "ｫ"))
  :description "Small zenkaku katakana converts to hankaku small forms"
  :body (nskk-assert-strings-equal (nskk-kana-zenkaku-to-hankaku input) expected))

(nskk-deftest-table kana-zenkaku-to-hankaku-punctuation
  :columns (input expected)
  :rows (("。" "｡")
         ("、" "､")
         ("・" "･")
         ("ー" "ｰ"))
  :description "Zenkaku Japanese punctuation converts to hankaku"
  :body (nskk-assert-strings-equal (nskk-kana-zenkaku-to-hankaku input) expected))

(nskk-describe "zenkaku to hankaku string conversion"
  (nskk-context "multi-character strings"
    (nskk-it "converts entire katakana strings to hankaku"
      (nskk-assert-strings-equal
       (nskk-kana-zenkaku-to-hankaku "コンピュータ")
       "ｺﾝﾋﾟｭｰﾀ")
      (nskk-assert-strings-equal
       (nskk-kana-zenkaku-to-hankaku "サンプル")
       "ｻﾝﾌﾟﾙ")))

  (nskk-context "single character input"
    (nskk-it "converts a single zenkaku character to hankaku"
      (nskk-assert-strings-equal (nskk-kana-zenkaku-to-hankaku ?ア) "ｱ")
      (nskk-assert-strings-equal (nskk-kana-zenkaku-to-hankaku ?イ) "ｲ")
      (nskk-assert-strings-equal (nskk-kana-zenkaku-to-hankaku ?ウ) "ｳ")))

  (nskk-context "unrecognized input passthrough"
    (nskk-it "passes unrecognized characters through unchanged"
      (nskk-assert-strings-equal (nskk-kana-zenkaku-to-hankaku "abc") "abc"))))

(nskk-deftest-table kana-hankaku-to-zenkaku-basic
  :columns (input expected)
  :rows (("ｱ" "ア")
         ("ｲ" "イ")
         ("ｳ" "ウ")
         ("ｴ" "エ")
         ("ｵ" "オ"))
  :description "Basic hankaku katakana converts to zenkaku"
  :body (nskk-assert-strings-equal (nskk-kana-hankaku-to-zenkaku input) expected))

(nskk-deftest-table kana-hankaku-to-zenkaku-dakuten
  :columns (input expected)
  :rows (("ｶﾞ" "ガ")
         ("ｷﾞ" "ギ")
         ("ｻﾞ" "ザ")
         ("ｼﾞ" "ジ")
         ("ﾊﾞ" "バ")
         ("ﾊﾟ" "パ"))
  :description "Hankaku katakana with dakuten converts to zenkaku voiced forms"
  :body (nskk-assert-strings-equal (nskk-kana-hankaku-to-zenkaku input) expected))

(nskk-deftest-table kana-hankaku-to-zenkaku-small
  :columns (input expected)
  :rows (("ｧ" "ァ")
         ("ｨ" "ィ")
         ("ｩ" "ゥ")
         ("ｪ" "ェ")
         ("ｫ" "ォ"))
  :description "Small hankaku katakana converts to zenkaku small forms"
  :body (nskk-assert-strings-equal (nskk-kana-hankaku-to-zenkaku input) expected))

(nskk-describe "hankaku to zenkaku string conversion"
  (nskk-context "multi-character strings"
    (nskk-it "converts entire hankaku strings to zenkaku"
      (nskk-assert-strings-equal
       (nskk-kana-hankaku-to-zenkaku "ｺﾝﾋﾟｭｰﾀ")
       "コンピュータ")
      (nskk-assert-strings-equal
       (nskk-kana-hankaku-to-zenkaku "ｻﾝﾌﾟﾙ")
       "サンプル")))

  (nskk-context "roundtrip"
    (nskk-it "zenkaku->hankaku->zenkaku preserves the original for pure katakana"
      (let ((original "コンピュータ"))
        (nskk-assert-strings-equal
         original
         (nskk-kana-hankaku-to-zenkaku
          (nskk-kana-zenkaku-to-hankaku original))))))

  (nskk-context "single character input"
    (nskk-it "converts a single hankaku character to zenkaku"
      (nskk-assert-strings-equal (nskk-kana-hankaku-to-zenkaku ?ｱ) "ア")
      (nskk-assert-strings-equal (nskk-kana-hankaku-to-zenkaku ?ｲ) "イ")
      (nskk-assert-strings-equal (nskk-kana-hankaku-to-zenkaku ?ｳ) "ウ"))))

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

(nskk-describe "nskk-kana multi-step conversion pipeline"
  (nskk-it "converts hiragana through the full pipeline to katakana and then hankaku"
    (let* ((hiragana "こんにちは")
           (katakana (nskk-kana-string-hiragana-to-katakana hiragana))
           (hankaku (nskk-kana-zenkaku-to-hankaku katakana)))
      (nskk-assert-strings-equal katakana "コンニチハ")
      (nskk-assert-strings-equal hankaku "ｺﾝﾆﾁﾊ")))

  (nskk-it "converts mixed scripts leaving non-target characters unchanged"
    (let ((mixed "ひらがなカタカナ漢字123ABC"))
      (nskk-assert-strings-equal
       (nskk-kana-string-hiragana-to-katakana mixed)
       "ヒラガナカタカナ漢字123ABC")
      (nskk-assert-strings-equal
       (nskk-kana-string-katakana-to-hiragana mixed)
       "ひらがなかたかな漢字123ABC")))

  (nskk-it "converts hankaku strings to zenkaku across representative test cases"
    (dolist (case '(("ｺﾝﾋﾟｭｰﾀ" . "コンピュータ")
                    ("ｻﾝﾌﾟﾙ"   . "サンプル")))
      (nskk-assert-strings-equal
       (nskk-kana-hankaku-to-zenkaku (car case))
       (cdr case)))))

;;;
;;; Property-Based Tests
;;;

;; Type invariant: string conversion always returns a string for string input.
(nskk-property-test kana-pbt-string-conversion-returns-string
  ((input hiragana-string))
  (stringp (nskk-kana-string-hiragana-to-katakana input))
  100)

;; Length invariant: hiragana->katakana is a bijection; length is preserved.
(nskk-property-test kana-pbt-string-length-preserved
  ((input hiragana-string))
  (let ((result (nskk-kana-string-hiragana-to-katakana input)))
    (= (length input) (length result)))
  100)

;; Roundtrip invariant: hiragana->katakana->hiragana is identity for hiragana strings.
(nskk-property-test kana-pbt-hiragana-katakana-roundtrip
  ((input hiragana-string))
  (equal input
         (nskk-kana-string-katakana-to-hiragana
          (nskk-kana-string-hiragana-to-katakana input)))
  100)

;; Roundtrip invariant (reverse direction): katakana->hiragana->katakana is identity.
;; Generate a katakana string by converting a random hiragana string, then verify
;; that katakana->hiragana->katakana returns the original katakana string.
(nskk-deftest-unit kana-katakana-hiragana-roundtrip-pbt
  "Property: katakana->hiragana->katakana is identity for pure katakana strings.
Seeded PBT: generates random katakana strings via the hiragana generator and
verifies the reverse roundtrip holds for 100 random inputs."
  (let ((test-seed 42)
        (runs 100)
        (failures nil))
    (random (format "nskk-kana-pbt-%d" test-seed))
    (dotimes (_ runs)
      (let* ((hiragana (nskk-generate 'hiragana-string))
             (katakana (nskk-kana-string-hiragana-to-katakana hiragana))
             (roundtripped (nskk-kana-string-hiragana-to-katakana
                            (nskk-kana-string-katakana-to-hiragana katakana))))
        (unless (equal katakana roundtripped)
          (push (list :katakana katakana :roundtripped roundtripped) failures))))
    (when failures
      (ert-fail (format "katakana->hiragana->katakana roundtrip failed for %d cases:\n%S"
                        (length failures) (take 5 failures))))))

;; Roundtrip invariant: zenkaku->hankaku->zenkaku is identity for pure katakana strings.
;; Uses the hiragana generator to produce inputs, converts to katakana (zenkaku), then
;; verifies the zenkaku->hankaku->zenkaku roundtrip holds.
(nskk-deftest-unit kana-zenkaku-hankaku-roundtrip-pbt
  "Property: zenkaku->hankaku->zenkaku is identity for pure zenkaku katakana strings.
Seeded PBT: generates random zenkaku katakana strings via the hiragana generator
and verifies the roundtrip holds for 100 random inputs."
  (let ((test-seed 137)
        (runs 100)
        (failures nil))
    (random (format "nskk-kana-pbt-%d" test-seed))
    (dotimes (_ runs)
      (let* ((hiragana (nskk-generate 'hiragana-string))
             (zenkaku (nskk-kana-string-hiragana-to-katakana hiragana))
             (hankaku (nskk-kana-zenkaku-to-hankaku zenkaku))
             (roundtripped (nskk-kana-hankaku-to-zenkaku hankaku)))
        (unless (equal zenkaku roundtripped)
          (push (list :zenkaku zenkaku :hankaku hankaku :roundtripped roundtripped)
                failures))))
    (when failures
      (ert-fail (format "zenkaku->hankaku->zenkaku roundtrip failed for %d cases:\n%S"
                        (length failures) (take 5 failures))))))

;;;
;;; Prolog Database Integration Tests
;;;

(nskk-describe "nskk-kana Prolog rules"
  (nskk-context "hiragana classification rule"
    (nskk-it "kana-hiragana rule works for hiragana and rejects katakana and ascii"
      (should (nskk-prolog-query '(kana-hiragana ?あ)))
      (should (nskk-prolog-query '(kana-hiragana ?か)))
      (should (not (nskk-prolog-query '(kana-hiragana ?ア))))
      (should (not (nskk-prolog-query '(kana-hiragana ?a))))))

  (nskk-context "katakana classification rule"
    (nskk-it "kana-katakana rule works for katakana and rejects hiragana"
      (should (nskk-prolog-query '(kana-katakana ?ア)))
      (should (nskk-prolog-query '(kana-katakana ?カ)))
      (should (not (nskk-prolog-query '(kana-katakana ?あ))))))

  (nskk-context "conversion API"
    (nskk-it "kana conversion functions work correctly via the elisp API"
      (should (= (nskk-kana-hiragana-to-katakana ?あ) ?ア))
      (should (= (nskk-kana-hiragana-to-katakana ?い) ?イ))
      (should (= (nskk-kana-hiragana-to-katakana ?う) ?ウ))
      (should (= (nskk-kana-katakana-to-hiragana ?ア) ?あ))
      (should (= (nskk-kana-katakana-to-hiragana ?イ) ?い))
      (should (= (nskk-kana-katakana-to-hiragana ?ウ) ?う))))

  (nskk-context "zenkaku/hankaku Prolog facts"
    (nskk-it "zenkaku-to-hankaku and hankaku-to-zenkaku Prolog facts are asserted"
      (should (nskk-prolog-query (list 'zenkaku-to-hankaku "ア" '\?h)))
      (should (nskk-prolog-query (list 'hankaku-to-zenkaku "ｱ" '\?z))))))

;;;
;;; nskk-kana--define-range-predicate
;;;

(nskk-describe "nskk-kana--define-range-predicate"
  (nskk-it "is a macro (not a plain function)"
    (should (macrop 'nskk-kana--define-range-predicate)))

  (nskk-it "generated ELisp predicate returns non-nil for chars in range"
    ;; nskk-kana-hiragana-p is generated by the macro
    (should (nskk-kana-hiragana-p ?あ))
    (should (nskk-kana-hiragana-p ?ん)))

  (nskk-it "generated ELisp predicate returns nil for chars outside range"
    (should (null (nskk-kana-hiragana-p ?ア)))   ; katakana
    (should (null (nskk-kana-hiragana-p ?A))))    ; ASCII

  (nskk-it "also asserts a Prolog range rule queryable via holds-p with char codes"
    ;; The Prolog range rules compare character codes (integers), not strings.
    ;; ?あ evaluates to the char code 12354 (U+3042).
    (should (nskk-prolog-holds-p `(kana-hiragana ,?あ)))
    (should (nskk-prolog-holds-p `(kana-katakana ,?ア)))
    ;; ASCII 'A' (65) should NOT satisfy kana-hiragana
    (should (null (nskk-prolog-holds-p `(kana-hiragana ,?A))))))

;;;
;;; nskk-kana-initialize
;;;

(nskk-describe "nskk-kana-initialize"
  (nskk-it "is idempotent: calling twice does not error"
    ;; nskk-kana-initialize is already called at module load; second call is no-op
    (should (progn (nskk-kana-initialize) t)))

  (nskk-it "populates zenkaku-to-hankaku Prolog facts so conversions work"
    (let ((result (nskk-prolog-query-value
                   '(zenkaku-to-hankaku "ア" \?h) '\?h)))
      (should (stringp result))
      (should (string= result "ｱ"))))

  (nskk-it "populates hankaku-to-zenkaku Prolog facts so conversions work"
    (let ((result (nskk-prolog-query-value
                   '(hankaku-to-zenkaku "ｱ" \?z) '\?z)))
      (should (stringp result))
      (should (string= result "ア")))))

;;;
;;; nskk-kana--map-string-chars/k Tests
;;;

(nskk-describe "nskk-kana--map-string-chars/k"
  (nskk-it "calls on-result with converted string when input is a string"
    (let (result-val)
      (nskk-kana--map-string-chars/k "アイウ" #'nskk-kana-katakana-to-hiragana
                                     (lambda (s) (setq result-val s))
                                     #'ignore)
      (should (equal result-val "あいう"))))

  (nskk-it "calls on-fail when input is not a string"
    (let (fail-called)
      (nskk-kana--map-string-chars/k 42 #'identity
                                     #'ignore
                                     (lambda () (setq fail-called t)))
      (should fail-called)))

  (nskk-it "calls on-fail when input is nil"
    (let (fail-called)
      (nskk-kana--map-string-chars/k nil #'identity
                                     #'ignore
                                     (lambda () (setq fail-called t)))
      (should fail-called)))

  (nskk-it "applies converter to every character in the string"
    (let (result-val)
      (nskk-kana--map-string-chars/k "あいう" #'nskk-kana-hiragana-to-katakana
                                     (lambda (s) (setq result-val s))
                                     #'ignore)
      (should (equal result-val "アイウ")))))

;;;
;;; nskk-kana--zenkaku-string-to-hankaku/k
;;;

(nskk-describe "nskk-kana--zenkaku-string-to-hankaku/k"
  (nskk-it "calls on-result with hankaku string for a zenkaku katakana string"
    (let (result-val)
      (nskk-kana--zenkaku-string-to-hankaku/k "ア"
        (lambda (s) (setq result-val s))
        #'ignore)
      (should (equal result-val "ｱ"))))

  (nskk-it "passes through characters not in the zenkaku table unchanged"
    (let (result-val)
      ;; Hiragana あ is not in the zenkaku-to-hankaku table
      (nskk-kana--zenkaku-string-to-hankaku/k "あ"
        (lambda (s) (setq result-val s))
        #'ignore)
      (should (equal result-val "あ"))))

  (nskk-it "calls on-fail when input is not a string"
    (let (fail-called)
      (nskk-kana--zenkaku-string-to-hankaku/k 42
        #'ignore
        (lambda () (setq fail-called t)))
      (should fail-called)))

  (nskk-it "calls on-fail when input is nil"
    (let (fail-called)
      (nskk-kana--zenkaku-string-to-hankaku/k nil
        #'ignore
        (lambda () (setq fail-called t)))
      (should fail-called))))

;;;
;;; nskk-kana--hankaku-string-to-zenkaku/k
;;;

(nskk-describe "nskk-kana--hankaku-string-to-zenkaku/k"
  (nskk-it "calls on-result with zenkaku string for a hankaku katakana string"
    (let (result-val)
      (nskk-kana--hankaku-string-to-zenkaku/k "ｱ"
        (lambda (s) (setq result-val s))
        #'ignore)
      (should (equal result-val "ア"))))

  (nskk-it "handles two-character dakuten combinations"
    (let (result-val)
      ;; ｶ + ﾞ (dakuten) → ガ
      (nskk-kana--hankaku-string-to-zenkaku/k "ｶﾞ"
        (lambda (s) (setq result-val s))
        #'ignore)
      (should (equal result-val "ガ"))))

  (nskk-it "calls on-fail when input is not a string"
    (let (fail-called)
      (nskk-kana--hankaku-string-to-zenkaku/k 42
        #'ignore
        (lambda () (setq fail-called t)))
      (should fail-called)))

  (nskk-it "calls on-fail when input is nil"
    (let (fail-called)
      (nskk-kana--hankaku-string-to-zenkaku/k nil
        #'ignore
        (lambda () (setq fail-called t)))
      (should fail-called))))

;;;
;;; nskk-kana--fill-hash-table
;;;

(nskk-describe "nskk-kana--fill-hash-table"
  (nskk-it "is a macro"
    (should (macrop 'nskk-kana--fill-hash-table)))

  (nskk-it "inserts all specified key-value pairs into the hash table"
    (let ((tbl (make-hash-table :test 'equal)))
      (nskk-kana--fill-hash-table tbl
        ("a" "あ")
        ("i" "い")
        ("u" "う"))
      (should (equal (gethash "a" tbl) "あ"))
      (should (equal (gethash "i" tbl) "い"))
      (should (equal (gethash "u" tbl) "う"))))

  (nskk-it "returns the table itself"
    (let ((tbl (make-hash-table :test 'equal)))
      (should (eq (nskk-kana--fill-hash-table tbl ("k" "v")) tbl))))

  (nskk-it "overwrites existing entries with new values"
    (let ((tbl (make-hash-table :test 'equal)))
      (puthash "key" "old" tbl)
      (nskk-kana--fill-hash-table tbl ("key" "new"))
      (should (equal (gethash "key" tbl) "new")))))

(provide 'nskk-kana-test)

;;; nskk-kana-test.el ends here
