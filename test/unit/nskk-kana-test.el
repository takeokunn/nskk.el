;;; nskk-kana-test.el --- Core conversion tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Core conversion tests.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'nskk-kana)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;
;;; Character Classification Tests
;;;

(nskk-describe "hiragana character classification"
  (nskk-deftest-table kana-hiragana-basic-chars
    :description "Recognizes representative hiragana across a-, ka-, and sa-rows"
    :columns (char)
    :rows ((?あ) (?い) (?う) (?え) (?お)
           (?か) (?き) (?く) (?け) (?こ)
           (?さ) (?し) (?す) (?せ) (?そ))
    :body (should (nskk-kana-hiragana-p char)))

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
  (nskk-deftest-table kana-katakana-basic-chars
    :description "Recognizes representative katakana across a-, ka-, and sa-rows"
    :columns (char)
    :rows ((?ア) (?イ) (?ウ) (?エ) (?オ)
           (?カ) (?キ) (?ク) (?ケ) (?コ)
           (?サ) (?シ) (?ス) (?セ) (?ソ))
    :body (should (nskk-kana-katakana-p char)))

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
  (nskk-deftest-table kana-hankaku-basic-chars
    :description "Recognizes representative half-width katakana"
    :columns (char)
    :rows ((?ｱ) (?ｲ) (?ｳ) (?ｴ) (?ｵ) (?ｶ) (?ﾝ) (?ｦ))
    :body (should (nskk-kana-hankaku-katakana-p char)))

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
  (nskk-deftest-table kana-han-basic-chars
    :description "Recognizes common kanji characters"
    :columns (char)
    :rows ((?漢) (?字) (?日) (?本) (?語) (?入) (?力))
    :body (should (nskk-kana-han-p char)))

  (nskk-context "range boundary detection"
    (nskk-it "accepts characters at the start and end of both CJK and Extension A ranges"
      (should (nskk-kana-han-p #x4E00))            ; Start of main CJK range
      (should (nskk-kana-han-p #x9FFF))            ; End of main CJK range
      (should (not (nskk-kana-han-p #xA000)))      ; Just after main CJK range
      (should (nskk-kana-han-p #x3400))            ; Start of Extension A
      (should (nskk-kana-han-p #x4DBF))            ; End of Extension A
      (should (not (nskk-kana-han-p #x33FF)))      ; Just before Extension A
      (should (not (nskk-kana-han-p #x4DC0)))      ; Just after Extension A (tight boundary)
      (should (not (nskk-kana-han-p #x4DFF)))))     ; Midpoint in gap

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
      (should (null (nskk-kana-string-katakana-to-hiragana 42))))
    (nskk-it "returns an empty string for empty string input"
      (nskk-assert-strings-equal
       (nskk-kana-string-katakana-to-hiragana "")
       ""))))

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
         ("パ" "ﾊﾟ")
         ("ヴ" "ｳﾞ"))
  :description "Zenkaku voiced/semi-voiced katakana converts to hankaku with dakuten"
  :body (nskk-assert-strings-equal (nskk-kana-zenkaku-to-hankaku input) expected))

(nskk-deftest-table kana-zenkaku-to-hankaku-small
  :columns (input expected)
  :rows (("ァ" "ｧ")
         ("ィ" "ｨ")
         ("ゥ" "ｩ")
         ("ェ" "ｪ")
         ("ォ" "ｫ")
         ("ッ" "ｯ")
         ("ャ" "ｬ")
         ("ュ" "ｭ")
         ("ョ" "ｮ"))
  :description "Small zenkaku katakana converts to hankaku small forms"
  :body (nskk-assert-strings-equal (nskk-kana-zenkaku-to-hankaku input) expected))

(nskk-deftest-table kana-zenkaku-to-hankaku-punctuation
  :columns (input expected)
  :rows (("。" "｡")
         ("、" "､")
         ("・" "･")
         ("ー" "ｰ")
         ("゛" "ﾞ")
         ("゜" "ﾟ"))
  :description "Zenkaku Japanese punctuation and combining marks convert to hankaku"
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
         ("ﾊﾟ" "パ")
         ("ｳﾞ" "ヴ"))
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

(nskk-deftest-table kana-hankaku-to-zenkaku-punctuation
  :columns (input expected)
  :rows (("｡" "。")
         ("､" "、")
         ("･" "・")
         ("ｰ" "ー"))
  :description "Hankaku Japanese punctuation converts to zenkaku"
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
      (should (< elapsed 0.5))
      (message "[Performance] 1000 chars: %.3fms" (* 1000 elapsed)))))

(nskk-deftest-performance core-performance-katakana-to-hiragana
  "Test katakana to hiragana conversion performance.
Note: Prolog-backed classification costs ~30-50us per character."
  (let ((test-string (make-string 1000 ?ア))
        (start-time (current-time)))
    (nskk-kana-string-katakana-to-hiragana test-string)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
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
      (should (< elapsed 1.0))
      (message "[Performance] 208 classifications: %.3fms" (* 1000 elapsed)))))

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
;;;

(nskk-property-test kana-pbt-hiragana-katakana-roundtrip
  ((input hiragana-string))
  (equal input
         (nskk-kana-string-katakana-to-hiragana
          (nskk-kana-string-hiragana-to-katakana input)))
  100)

;; `hiragana-string' draws from あ行-な行 only, so it cannot produce a dakuten,
;; handakuten, ヴ, ん or を.  A round-trip property fed from it therefore never
;; reaches the two-character lookahead in `nskk--kana-hankaku-lookup-at' — the
;; branch most worth exercising here.  These atoms are the hankaku units
;; themselves, single- and two-character alike.
(defconst nskk-kana-test--hankaku-atoms
  '("ｱ" "ｲ" "ｳ" "ｵ" "ｶ" "ﾀ" "ﾅ" "ﾊ" "ﾏ" "ﾔ" "ﾗ" "ﾜ" "ｦ" "ﾝ"
    "ｶﾞ" "ｷﾞ" "ｸﾞ" "ｻﾞ" "ｼﾞ" "ﾀﾞ" "ﾊﾞ" "ﾋﾞ" "ｳﾞ"
    "ﾊﾟ" "ﾋﾟ" "ﾌﾟ" "ﾍﾟ" "ﾎﾟ"
    "ｧ" "ｨ" "ｩ" "ｪ" "ｫ" "ｯ" "ｬ" "ｭ" "ｮ"
    "｡" "､" "･" "ｰ")
  "Hankaku katakana units spanning single chars and dakuten/handakuten pairs.")

(nskk-register-generator 'kana-hankaku-rich-string
  (lambda (&optional length)
    (let ((len (or length (+ 1 (random 6)))))
      (string-join
       (cl-loop repeat len
                collect (nth (random (length nskk-kana-test--hankaku-atoms))
                             nskk-kana-test--hankaku-atoms))
       ""))))

;; Stated hankaku-first because that is the direction that actually holds.
;; zenkaku->hankaku->zenkaku is NOT an identity: see the ヮ case below.
;;
;; A round trip alone is NOT sufficient evidence here: if the two-character
;; lookahead is disabled, "ｶﾞ" decomposes to "カ゛" and converting back
;; re-joins it to "ｶﾞ", so the round trip succeeds on a broken conversion.
;; The composition property below is what actually pins the lookahead —
;; a correct conversion never leaves a standalone combining mark behind,
;; because every ﾞ/ﾟ in the input is attached to a convertible base.
(nskk-property-test kana-pbt-hankaku-zenkaku-roundtrip
  ((hankaku kana-hankaku-rich-string))
  (equal hankaku
         (nskk-kana-zenkaku-to-hankaku
          (nskk-kana-hankaku-to-zenkaku hankaku)))
  100)

(nskk-property-test kana-pbt-hankaku-zenkaku-composes-dakuten
  ((hankaku kana-hankaku-rich-string))
  (let ((zenkaku (nskk-kana-hankaku-to-zenkaku hankaku)))
    (not (or (string-match-p "゛" zenkaku)
             (string-match-p "゜" zenkaku))))
  100)

(nskk-describe "zenkaku to hankaku is not injective"
  ;; JIS X 0201 has no half-width small wa, so ヮ and ワ share the hankaku
  ;; form ﾜ and the round trip cannot recover which one it started from.
  (nskk-it "maps both ヮ and ワ to ﾜ, and ﾜ back to ワ only"
    (nskk-assert-strings-equal (nskk-kana-zenkaku-to-hankaku "ヮ") "ﾜ")
    (nskk-assert-strings-equal (nskk-kana-zenkaku-to-hankaku "ワ") "ﾜ")
    (nskk-assert-strings-equal
     (nskk-kana-hankaku-to-zenkaku (nskk-kana-zenkaku-to-hankaku "ヮ"))
     "ワ")))

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
      (should (= (nskk-kana-katakana-to-hiragana ?ウ) ?う)))))

;;;
;;; Range predicates and their Prolog counterparts
;;;

(nskk-describe "range predicates"
  (nskk-it "returns non-nil for chars in range"
    (should (nskk-kana-hiragana-p ?あ))
    (should (nskk-kana-hiragana-p ?ん)))

  (nskk-it "returns nil for chars outside range"
    (should (null (nskk-kana-hiragana-p ?ア)))   ; katakana
    (should (null (nskk-kana-hiragana-p ?A))))    ; ASCII

  (nskk-it "has a matching Prolog range rule queryable via holds-p with char codes"
    (should (nskk-prolog-holds-p `(kana-hiragana ,?あ)))
    (should (nskk-prolog-holds-p `(kana-katakana ,?ア)))
    (should (null (nskk-prolog-holds-p `(kana-hiragana ,?A))))))

;;;
;;; nskk-kana-initialize
;;;

(nskk-describe "nskk-kana-initialize"
  (nskk-it "sets the initialized flag"
    (nskk-kana-initialize)
    (should nskk--kana-initialized))

  (nskk-it "populates kana-conversion/3 so mode lookup resolves a function"
    (nskk-kana-initialize)
    (should (eq (nskk-prolog-query-value
                 '(kana-conversion katakana insert \?fn) '\?fn)
                'nskk-kana-string-hiragana-to-katakana)))

  ;; The flag is declared once at load time.  Re-declaring it inside
  ;; `nskk-kana-initialize' made `module-initialized-flag' yield the symbol
  ;; twice, because `nskk-prolog-assert' does not deduplicate clauses.
  (nskk-it "contributes exactly one module-initialized-flag solution"
    (nskk-kana-initialize)
    (nskk-kana-initialize)
    (should (= 1 (cl-count 'nskk--kana-initialized
                           (nskk-prolog-query-all-values
                            '(module-initialized-flag \?f) '\?f))))))

;;;
;;; nskk--kana-map-string-chars/k Tests
;;;

(nskk-describe "nskk--kana-map-string-chars/k"
  (nskk-it "calls on-result with converted string when input is a string"
    (let (result-val)
      (nskk--kana-map-string-chars/k "アイウ" #'nskk-kana-katakana-to-hiragana
                                     (lambda (s) (setq result-val s))
                                     #'ignore)
      (should (equal result-val "あいう"))))

  (nskk-it "calls on-fail when input is not a string"
    (let (fail-called)
      (nskk--kana-map-string-chars/k 42 #'identity
                                     #'ignore
                                     (lambda () (setq fail-called t)))
      (should fail-called)))

  (nskk-it "calls on-fail when input is nil"
    (let (fail-called)
      (nskk--kana-map-string-chars/k nil #'identity
                                     #'ignore
                                     (lambda () (setq fail-called t)))
      (should fail-called)))

  (nskk-it "applies converter to every character in the string"
    (let (result-val)
      (nskk--kana-map-string-chars/k "あいう" #'nskk-kana-hiragana-to-katakana
                                     (lambda (s) (setq result-val s))
                                     #'ignore)
      (should (equal result-val "アイウ"))))

  (nskk-it "calls on-result with empty string for empty string input"
    (let (result-val)
      (nskk--kana-map-string-chars/k "" #'identity
                                     (lambda (s) (setq result-val s))
                                     #'ignore)
      (should (equal result-val "")))))

;;;
;;; nskk--kana-zenkaku-string-to-hankaku
;;;

(nskk-describe "nskk--kana-zenkaku-string-to-hankaku"
  (nskk-it "converts a zenkaku katakana string to hankaku"
    (should (equal (nskk--kana-zenkaku-string-to-hankaku "ア") "ｱ")))

  (nskk-it "passes through characters not in the zenkaku table unchanged"
    (should (equal (nskk--kana-zenkaku-string-to-hankaku "あ") "あ"))))

;;;
;;; nskk--kana-hankaku-lookup-at
;;;

(nskk-describe "nskk--kana-hankaku-lookup-at"
  (nskk-context "two-char dakuten match"
    (nskk-it "returns (zenkaku . 2) for a two-char dakuten sequence at position 0"
      (let ((result (nskk--kana-hankaku-lookup-at "ｶﾞ" 0 2)))
        (should (equal (car result) "ガ"))
        (should (= (cdr result) 2))))

    (nskk-it "returns (zenkaku . 2) for a handakuten sequence at position 0"
      (let ((result (nskk--kana-hankaku-lookup-at "ﾊﾟ" 0 2)))
        (should (equal (car result) "パ"))
        (should (= (cdr result) 2)))))

  (nskk-context "single-char fallback"
    (nskk-it "returns (zenkaku . 1) for a plain single-char sequence"
      (let ((result (nskk--kana-hankaku-lookup-at "ｱ" 0 1)))
        (should (equal (car result) "ア"))
        (should (= (cdr result) 1))))

    (nskk-it "falls back to single char when two-char seq is not in table"
      (let ((result (nskk--kana-hankaku-lookup-at "ｱｲ" 0 2)))
        (should (equal (car result) "ア"))
        (should (= (cdr result) 1))))

    (nskk-it "passes through unrecognized chars unchanged with advance 1"
      (let ((result (nskk--kana-hankaku-lookup-at "a" 0 1)))
        (should (equal (car result) "a"))
        (should (= (cdr result) 1))))

    (nskk-it "handles the last character in a string (no pair possible)"
      (let ((result (nskk--kana-hankaku-lookup-at "ｱｲ" 1 2)))
        (should (equal (car result) "イ"))
        (should (= (cdr result) 1))))))

;;;
;;; nskk--kana-hankaku-string-to-zenkaku
;;;

(nskk-describe "nskk--kana-hankaku-string-to-zenkaku"
  (nskk-it "converts a hankaku katakana string to zenkaku"
    (should (equal (nskk--kana-hankaku-string-to-zenkaku "ｱ") "ア")))

  (nskk-it "handles two-character dakuten combinations"
    (should (equal (nskk--kana-hankaku-string-to-zenkaku "ｶﾞ") "ガ"))))

;;;
;;; nskk--kana-fill-hash-table
;;;

(nskk-describe "nskk--kana-fill-hash-table"
  (nskk-it "is a macro"
    (should (macrop 'nskk--kana-fill-hash-table)))

  (nskk-it "inserts all specified key-value pairs into the hash table"
    (let ((tbl (make-hash-table :test 'equal)))
      (nskk--kana-fill-hash-table tbl
        ("a" "あ")
        ("i" "い")
        ("u" "う"))
      (should (equal (gethash "a" tbl) "あ"))
      (should (equal (gethash "i" tbl) "い"))
      (should (equal (gethash "u" tbl) "う"))))

  (nskk-it "returns the table itself"
    (let ((tbl (make-hash-table :test 'equal)))
      (should (eq (nskk--kana-fill-hash-table tbl ("k" "v")) tbl))))

  (nskk-it "overwrites existing entries with new values"
    (let ((tbl (make-hash-table :test 'equal)))
      (puthash "key" "old" tbl)
      (nskk--kana-fill-hash-table tbl ("key" "new"))
      (should (equal (gethash "key" tbl) "new")))))

;;;
;;; nskk--hiragana-to-hankaku
;;;

(nskk-describe "nskk--hiragana-to-hankaku"
  (nskk-context "basic a-row hiragana"
    (nskk-it "converts あ (U+3042) to ｱ (U+FF71)"
      (nskk-assert-strings-equal (nskk--hiragana-to-hankaku "あ") "ｱ"))
    (nskk-it "converts い (U+3044) to ｲ (U+FF72)"
      (nskk-assert-strings-equal (nskk--hiragana-to-hankaku "い") "ｲ"))
    (nskk-it "converts う (U+3046) to ｳ (U+FF73)"
      (nskk-assert-strings-equal (nskk--hiragana-to-hankaku "う") "ｳ")))

  (nskk-context "voiced mora (dakuten)"
    (nskk-it "converts が to ｶﾞ (base char ｶ plus combining dakuten ﾞ)"
      (nskk-assert-strings-equal (nskk--hiragana-to-hankaku "が") "ｶﾞ")))

  (nskk-context "multi-character strings"
    (nskk-it "converts あいう to ｱｲｳ"
      (nskk-assert-strings-equal (nskk--hiragana-to-hankaku "あいう") "ｱｲｳ"))))

;;;
;;; nskk--hankaku-to-hiragana
;;;

(nskk-describe "nskk--hankaku-to-hiragana"
  (nskk-context "basic a-row hankaku katakana"
    (nskk-it "converts ｱ (U+FF71) to あ"
      (nskk-assert-strings-equal (nskk--hankaku-to-hiragana "ｱ") "あ"))
    (nskk-it "converts ｲ (U+FF72) to い"
      (nskk-assert-strings-equal (nskk--hankaku-to-hiragana "ｲ") "い"))
    (nskk-it "converts ｳ (U+FF73) to う"
      (nskk-assert-strings-equal (nskk--hankaku-to-hiragana "ｳ") "う")))

  (nskk-context "voiced mora (dakuten)"
    (nskk-it "converts ｶﾞ (two-char dakuten sequence) to が"
      (nskk-assert-strings-equal (nskk--hankaku-to-hiragana "ｶﾞ") "が")))

  (nskk-context "roundtrip"
    (nskk-it "hankaku->hiragana is the inverse of hiragana->hankaku for あいう"
      (nskk-assert-strings-equal
       (nskk--hankaku-to-hiragana (nskk--hiragana-to-hankaku "あいう"))
       "あいう"))))

;;;
;;; nskk-kana-convert-for-mode
;;;

(nskk-deftest-table kana-convert-for-mode-insert
  :description "nskk-kana-convert-for-mode converts hiragana to the correct script per mode"
  :columns (mode input expected)
  :rows ((hiragana     "あいう" "あいう")
         (katakana     "あいう" "アイウ")
         (katakana-半角 "あいう" "ｱｲｳ"))
  :body (should (equal (nskk-kana-convert-for-mode input mode) expected)))

(nskk-describe "nskk-kana-convert-for-mode"
  (nskk-context "unknown mode falls back to identity"
    (nskk-it "passes text through unchanged when mode has no kana-conversion/3 entry"
      (should (equal (nskk-kana-convert-for-mode "あ" 'ascii) "あ")))))

;;;
;;; nskk-kana-normalize-for-lookup
;;;

(nskk-deftest-table kana-normalize-for-lookup
  :description "nskk-kana-normalize-for-lookup normalizes script to hiragana for dict lookup"
  :columns (mode input expected)
  :rows ((hiragana     "あいう" "あいう")
         (katakana     "アイウ" "あいう")
         (katakana-半角 "ｱｲｳ"   "あいう"))
  :body (should (equal (nskk-kana-normalize-for-lookup input mode) expected)))

(nskk-describe "nskk-kana-normalize-for-lookup"
  (nskk-context "unknown mode falls back to identity"
    (nskk-it "passes text through unchanged when mode has no normalize entry"
      (should (equal (nskk-kana-normalize-for-lookup "test" 'ascii) "test")))))

(nskk-deftest-table kana-jisx0208-latin-char
  :columns (input expected)
  :rows ((?\s ?　)
         (?! ?！)
         (?~ ?～)
         (?_ ?＿)
         (?0 ?０)
         (?\t ?\t)
         (?あ ?あ))
  :body (should (= (nskk-jisx0208-latin-char input) expected)))

(provide 'nskk-kana-test)

;;; nskk-kana-test.el ends here
