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

(nskk-describe "hiragana character classification"
  (nskk-context "basic hiragana characters"
    (nskk-it "recognizes basic hiragana characters in a-row and ka-row and sa-row"
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
      (should (nskk-kana-hiragana-p #x3040))  ; Start of range
      (should (nskk-kana-hiragana-p #x309F))  ; End of range
      (should (not (nskk-kana-hiragana-p #x303F)))  ; Just before
      (should (not (nskk-kana-hiragana-p #x30A0))))) ; Just after

  (nskk-context "non-hiragana rejection"
    (nskk-it "rejects katakana, ascii, digits, and kanji"
      (should (not (nskk-kana-hiragana-p ?ア)))
      (should (not (nskk-kana-hiragana-p ?a)))
      (should (not (nskk-kana-hiragana-p ?A)))
      (should (not (nskk-kana-hiragana-p ?1)))
      (should (not (nskk-kana-hiragana-p ?漢))))))

(nskk-describe "katakana character classification"
  (nskk-context "basic katakana characters"
    (nskk-it "recognizes basic katakana characters in a-row and ka-row and sa-row"
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
      (should (nskk-kana-katakana-p #x30A0))  ; Start of range
      (should (nskk-kana-katakana-p #x30FF))  ; End of range
      (should (not (nskk-kana-katakana-p #x309F)))  ; Just before
      (should (not (nskk-kana-katakana-p #x3100))))) ; Just after

  (nskk-context "non-katakana rejection"
    (nskk-it "rejects hiragana, ascii, digits, and kanji"
      (should (not (nskk-kana-katakana-p ?あ)))
      (should (not (nskk-kana-katakana-p ?a)))
      (should (not (nskk-kana-katakana-p ?A)))
      (should (not (nskk-kana-katakana-p ?1)))
      (should (not (nskk-kana-katakana-p ?漢))))))

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
      (should (nskk-kana-han-p #x4E00))  ; Start of range
      (should (nskk-kana-han-p #x9FFF))  ; End of range
      (should (not (nskk-kana-han-p #x4DFF)))  ; Just before
      (should (not (nskk-kana-han-p #xA000))))) ; Just after

  (nskk-context "japanese-p aggregation"
    (nskk-it "accepts hiragana, katakana, and han but rejects ascii and digits"
      (should (nskk-kana-japanese-p ?あ))  ; Hiragana
      (should (nskk-kana-japanese-p ?ア))  ; Katakana
      (should (nskk-kana-japanese-p ?漢))  ; Han
      (should (not (nskk-kana-japanese-p ?a)))
      (should (not (nskk-kana-japanese-p ?A)))
      (should (not (nskk-kana-japanese-p ?1))))))

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
         (?コ ?こ))
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
         ("さしすせそ" "サシスセソ"))
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

  (nskk-context "roundtrip"
    (nskk-it "roundtrip hiragana->katakana->hiragana preserves the original"
      (let ((original "こんにちは世界"))
        (let ((back-to-hiragana (nskk-kana-string-katakana-to-hiragana
                                 (nskk-kana-string-hiragana-to-katakana original))))
          (nskk-assert-strings-equal original back-to-hiragana))))))

(nskk-deftest-table kana-string-katakana-to-hiragana
  :columns (input expected)
  :rows (("アイウエオ" "あいうえお")
         ("カキクケコ" "かきくけこ")
         ("サシスセソ" "さしすせそ"))
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
       " Japanese 日本語 "))))

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
      (nskk-assert-strings-equal
       (nskk-kana-zenkaku-to-hankaku ?ア)
       "ｱ")
      (nskk-assert-strings-equal
       (nskk-kana-zenkaku-to-hankaku ?イ)
       "ｲ")
      (nskk-assert-strings-equal
       (nskk-kana-zenkaku-to-hankaku ?ウ)
       "ｳ"))))

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
    (nskk-it "roundtrip zenkaku->hankaku->zenkaku preserves the original"
      (let ((original "コンピュータ"))
        (let ((back-to-zenkaku (nskk-kana-hankaku-to-zenkaku
                                (nskk-kana-zenkaku-to-hankaku original))))
          (nskk-assert-strings-equal original back-to-zenkaku)))))

  (nskk-context "single character input"
    (nskk-it "converts a single hankaku character to zenkaku"
      (nskk-assert-strings-equal
       (nskk-kana-hankaku-to-zenkaku ?ｱ)
       "ア")
      (nskk-assert-strings-equal
       (nskk-kana-hankaku-to-zenkaku ?ｲ)
       "イ")
      (nskk-assert-strings-equal
       (nskk-kana-hankaku-to-zenkaku ?ｳ)
       "ウ"))))

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
    (let ((hiragana "こんにちは"))
      (let ((katakana (nskk-kana-string-hiragana-to-katakana hiragana))
            (hankaku (nskk-kana-zenkaku-to-hankaku
                      (nskk-kana-string-hiragana-to-katakana hiragana))))
        (nskk-assert-strings-equal katakana "コンニチハ")
        (nskk-assert-strings-equal hankaku "ｺﾝﾆﾁﾊ"))))

  (nskk-it "converts mixed scripts leaving non-target characters unchanged"
    (let ((mixed "ひらがなカタカナ漢字123ABC"))
      (let ((katakana (nskk-kana-string-hiragana-to-katakana mixed))
            (hiragana (nskk-kana-string-katakana-to-hiragana mixed)))
        (nskk-assert-strings-equal katakana "ヒラガナカタカナ漢字123ABC")
        (nskk-assert-strings-equal hiragana "ひらがなかたかな漢字123ABC"))))

  (nskk-it "converts hankaku strings to zenkaku across all test cases"
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
             expected)))))))

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

(nskk-describe "nskk-kana-classify Prolog rules"
  (nskk-context "hiragana classification rule"
    (nskk-it "kana-hiragana rule is asserted and works for hiragana and rejects katakana and ascii"
      (should (nskk-prolog-query '(kana-hiragana ?あ)))
      (should (nskk-prolog-query '(kana-hiragana ?か)))
      (should (not (nskk-prolog-query '(kana-hiragana ?ア))))
      (should (not (nskk-prolog-query '(kana-hiragana ?a))))))

  (nskk-context "katakana classification rule"
    (nskk-it "kana-katakana rule is asserted and works for katakana and rejects hiragana"
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

(provide 'nskk-kana-test)

;;; nskk-kana-test.el ends here
