;;; nskk-e2e-kana-input.el --- E2E kana input tests for NSKK  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n, testing

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; E2E tests for basic kana input (DDSKK §4.2 Input modes).
;; Covers: ASCII passthrough, hiragana romaji, katakana, compound kana,
;; sokuon, hatsuon, half-width katakana, word sequences.
;;
;; Also includes romaji edge rows (small kana, v-row, foreign extensions,
;; n-apostrophe, long vowel, mode switch clearing pending romaji).
;;
;; Property-based tests (PBT) verify crash-freedom and mode preservation
;; under random romaji input.

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(eval-when-compile (require 'cl-lib))

;;;;
;;;; ASCII Passthrough Tests
;;;;

(nskk-describe "ascii mode passthrough"
  (nskk-it "inserts characters directly"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-assert-mode 'ascii)
      (nskk-e2e-type "hello")
      (nskk-e2e-assert-buffer "hello")))

  (nskk-it "inserts digits directly"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "12345")
      (nskk-e2e-assert-buffer "12345")))

  (nskk-it "inserts symbols directly"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "!@#")
      (nskk-e2e-assert-buffer "!@#"))))

;;;;
;;;; Hiragana Vowel Input Tests
;;;;

(nskk-deftest-table buffer-hiragana-vowels
  :columns (input expected)
  :rows (("a" "あ") ("i" "い") ("u" "う") ("e" "え") ("o" "お"))
  :body (nskk-e2e-with-buffer 'hiragana nil
          (nskk-e2e-type input)
          (nskk-e2e-assert-buffer expected)))

(nskk-describe "hiragana all vowels in sequence"
  (nskk-it "inserts all vowels as a word"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "aiueo")
      (nskk-e2e-assert-buffer "あいうえお"))))

;;;;
;;;; Hiragana Consonant+Vowel (K-row)
;;;;

(nskk-deftest-table hiragana-ka-row
  :columns (romaji expected)
  :rows (("ka" "か") ("ki" "き") ("ku" "く") ("ke" "け") ("ko" "こ"))
  :body
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type romaji)
    (nskk-e2e-assert-buffer expected
                             (format "ka-row: %S → %S" romaji expected))))

;;;;
;;;; Complete Romaji Table Tests (ddskk-equivalent)
;;;;

(nskk-deftest-table romaji-table-comprehensive
  :columns (romaji expected)
  :rows (;; A-row
         ("a" "あ") ("i" "い") ("u" "う") ("e" "え") ("o" "お")
         ;; K-row
         ("ka" "か") ("ki" "き") ("ku" "く") ("ke" "け") ("ko" "こ")
         ;; S-row (standard and alternate spellings)
         ("sa" "さ") ("si" "し") ("su" "す") ("se" "せ") ("so" "そ")
         ("shi" "し")
         ;; T-row
         ("ta" "た") ("ti" "ち") ("tu" "つ") ("te" "て") ("to" "と")
         ("chi" "ち") ("tsu" "つ")
         ;; N-row
         ("na" "な") ("ni" "に") ("nu" "ぬ") ("ne" "ね") ("no" "の")
         ;; H-row
         ("ha" "は") ("hi" "ひ") ("hu" "ふ") ("he" "へ") ("ho" "ほ")
         ("fu" "ふ")
         ;; M-row
         ("ma" "ま") ("mi" "み") ("mu" "む") ("me" "め") ("mo" "も")
         ;; Y-row
         ("ya" "や") ("yu" "ゆ") ("yo" "よ")
         ;; R-row
         ("ra" "ら") ("ri" "り") ("ru" "る") ("re" "れ") ("ro" "ろ")
         ;; W-row
         ("wa" "わ") ("wi" "ゐ") ("we" "ゑ") ("wo" "を")
         ;; G-row (voiced)
         ("ga" "が") ("gi" "ぎ") ("gu" "ぐ") ("ge" "げ") ("go" "ご")
         ;; Z-row (voiced)
         ("za" "ざ") ("zi" "じ") ("zu" "ず") ("ze" "ぜ") ("zo" "ぞ")
         ("ji" "じ")
         ;; D-row (voiced)
         ("da" "だ") ("di" "ぢ") ("du" "づ") ("de" "で") ("do" "ど")
         ;; B-row (voiced)
         ("ba" "ば") ("bi" "び") ("bu" "ぶ") ("be" "べ") ("bo" "ぼ")
         ;; P-row (semi-voiced)
         ("pa" "ぱ") ("pi" "ぴ") ("pu" "ぷ") ("pe" "ぺ") ("po" "ぽ"))
  :body (nskk-e2e-with-buffer 'hiragana nil
          (nskk-e2e-type romaji)
          (nskk-e2e-assert-buffer expected
                                  (format "romaji %S → %S failed" romaji expected))))

;;;;
;;;; Compound Kana (拗音) Tests
;;;;

(nskk-deftest-table compound-kana-romaji
  :columns (romaji expected)
  :rows (;; KY-row
         ("kya" "きゃ") ("kyu" "きゅ") ("kyo" "きょ")
         ;; SH-row
         ("sha" "しゃ") ("shu" "しゅ") ("sho" "しょ")
         ;; CH-row
         ("cha" "ちゃ") ("chu" "ちゅ") ("cho" "ちょ")
         ;; NY-row
         ("nya" "にゃ") ("nyu" "にゅ") ("nyo" "にょ")
         ;; HY-row
         ("hya" "ひゃ") ("hyu" "ひゅ") ("hyo" "ひょ")
         ;; MY-row
         ("mya" "みゃ") ("myu" "みゅ") ("myo" "みょ")
         ;; RY-row
         ("rya" "りゃ") ("ryu" "りゅ") ("ryo" "りょ")
         ;; GY-row
         ("gya" "ぎゃ") ("gyu" "ぎゅ") ("gyo" "ぎょ")
         ;; JY/J-row
         ("ja" "じゃ") ("ju" "じゅ") ("jo" "じょ")
         ;; BY-row
         ("bya" "びゃ") ("byu" "びゅ") ("byo" "びょ")
         ;; PY-row
         ("pya" "ぴゃ") ("pyu" "ぴゅ") ("pyo" "ぴょ"))
  :body (nskk-e2e-with-buffer 'hiragana nil
          (nskk-e2e-type romaji)
          (nskk-e2e-assert-buffer expected
                                  (format "compound romaji %S → %S failed" romaji expected))))

;;;;
;;;; Sokuon (促音 っ) Tests
;;;;

(nskk-describe "sokuon input"
  (nskk-it "converts kka to っか"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "kka")
      (nskk-e2e-assert-buffer "っか")))

  (nskk-it "converts tte to って"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "tte")
      (nskk-e2e-assert-buffer "って")))

  (nskk-it "converts sshi to っし"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "sshi")
      (nskk-e2e-assert-buffer "っし")))

  (nskk-deftest-table sokuon-consonant-combinations
    :columns (romaji expected)
    :rows (("ppa" "っぱ") ("bba" "っば") ("dda" "っだ") ("gga" "っが"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer expected
                                    (format "sokuon %S → %S failed" romaji expected)))))

;;;;
;;;; Hatsuon (撥音 ん) Tests
;;;;

(nskk-describe "hatsuon input"
  (nskk-it "converts nn to ん"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "nn")
      (nskk-e2e-assert-buffer "ん")))

  (nskk-it "emits ん before consonant then processes consonant"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "nka")
      (nskk-e2e-assert-buffer "んか")))

  (nskk-it "treats na as な not hatsuon"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "na")
      (nskk-e2e-assert-buffer "な")))

  (nskk-it "converts nanka to なんか"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "nanka")
      (nskk-e2e-assert-buffer "なんか"))))

;;;;
;;;; Katakana Input Tests
;;;;

(nskk-describe "katakana input"
  (nskk-it "converts vowel sequence aiueo to katakana"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type "aiueo")
      (nskk-e2e-assert-buffer "アイウエオ")))

  (nskk-deftest-table katakana-single-cv
    :columns (romaji expected)
    :rows (("ka"  "カ") ("ki"  "キ") ("ku"  "ク")
           ("shi" "シ") ("tsu" "ツ") ("chi" "チ"))
    :body
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type romaji)
      (nskk-e2e-assert-buffer expected
                               (format "katakana: %S → %S" romaji expected))))

  (nskk-it "converts sokuon to ッ"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type "tte")
      (nskk-e2e-assert-buffer "ッテ")))

  (nskk-deftest-table katakana-representative-entries
    :columns (romaji expected)
    :rows (("a" "ア") ("i" "イ") ("u" "ウ") ("e" "エ") ("o" "オ")
           ("ka" "カ") ("ki" "キ") ("ku" "ク")
           ("sa" "サ") ("shi" "シ") ("su" "ス")
           ("ta" "タ") ("chi" "チ") ("tsu" "ツ")
           ("na" "ナ") ("ni" "ニ") ("nu" "ヌ")
           ("ha" "ハ") ("hi" "ヒ") ("fu" "フ")
           ("ma" "マ") ("mi" "ミ") ("mu" "ム")
           ("ya" "ヤ") ("yu" "ユ") ("yo" "ヨ")
           ("ra" "ラ") ("ri" "リ") ("ru" "ル")
           ("wa" "ワ") ("wo" "ヲ"))
    :body (nskk-e2e-with-buffer 'katakana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer expected
                                    (format "katakana %S → %S failed" romaji expected)))))

;;;;
;;;; Multiple Character Sequence Tests
;;;;

(nskk-describe "hiragana word sequences"
  (nskk-it "types nihongo as にほんご"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "nihongo")
      (nskk-e2e-assert-buffer "にほんご")))

  (nskk-it "types arigatou as ありがとう"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "arigatou")
      (nskk-e2e-assert-buffer "ありがとう")))

  (nskk-it "types konnichiwa as こんにちわ"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "konnichiwa")
      (nskk-e2e-assert-buffer "こんにちわ")))

  (nskk-it "continues typing hiragana after kanji conversion"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "漢字")
      (nskk-e2e-type "no")
      (nskk-e2e-assert-buffer "漢字の"))))

;;;;
;;;; Half-width Katakana (katakana-半角) Character Output Tests
;;;;

(nskk-deftest-table hankaku-katakana-comprehensive
  :columns (romaji expected)
  :rows (;; Vowel row
         ("a" "ｱ") ("i" "ｲ") ("u" "ｳ") ("e" "ｴ") ("o" "ｵ")
         ;; Ka-row
         ("ka" "ｶ") ("ki" "ｷ") ("ku" "ｸ") ("ke" "ｹ") ("ko" "ｺ")
         ;; Sa-row
         ("sa" "ｻ") ("shi" "ｼ") ("su" "ｽ") ("se" "ｾ") ("so" "ｿ")
         ;; Ta-row
         ("ta" "ﾀ") ("chi" "ﾁ") ("tsu" "ﾂ") ("te" "ﾃ") ("to" "ﾄ")
         ;; Na-row
         ("na" "ﾅ") ("ni" "ﾆ") ("nu" "ﾇ") ("ne" "ﾈ") ("no" "ﾉ")
         ;; Ha-row
         ("ha" "ﾊ") ("hi" "ﾋ") ("fu" "ﾌ") ("he" "ﾍ") ("ho" "ﾎ")
         ;; Ma-row
         ("ma" "ﾏ") ("mi" "ﾐ") ("mu" "ﾑ") ("me" "ﾒ") ("mo" "ﾓ")
         ;; Ya-row
         ("ya" "ﾔ") ("yu" "ﾕ") ("yo" "ﾖ")
         ;; Ra-row
         ("ra" "ﾗ") ("ri" "ﾘ") ("ru" "ﾙ") ("re" "ﾚ") ("ro" "ﾛ")
         ;; Wa-row + n
         ("wa" "ﾜ") ("nn" "ﾝ"))
  :body
  (nskk-e2e-with-buffer 'katakana-半角 nil
    (nskk-e2e-type romaji)
    (nskk-e2e-assert-buffer expected
                             (format "hankaku: %S → %S" romaji expected))))

(nskk-describe "half-width katakana (katakana-半角) character output"
  (nskk-it "converts all vowels in sequence to hankaku katakana"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-e2e-type "aiueo")
      (nskk-e2e-assert-buffer "ｱｲｳｴｵ")))

  (nskk-it "converts sokuon (doubled consonant) to ｯ + consonant kana"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-e2e-type "kka")
      (nskk-e2e-assert-buffer "ｯｶ"))))

;;;;
;;;; Property-Based Tests (PBT)
;;;;

(nskk-property-test-seeded romaji-no-crash
  ((romaji romaji-basic))
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e--type-romaji-chars romaji)
    (and (stringp (buffer-string))
         (nskk-state-valid-mode-p (nskk-current-mode))))
  30)

(nskk-property-test-seeded katakana-no-crash
  ((romaji romaji-basic))
  (nskk-e2e-with-buffer 'katakana nil
    (nskk-e2e--type-romaji-chars romaji)
    (eq (nskk-current-mode) 'katakana))
  30)

;; buffer-always-string: buffer-string is always a string after romaji input.
;; (Redundant with romaji-no-crash; kept as a distinct named invariant at 30 runs.)
(nskk-property-test-seeded buffer-always-string
  ((romaji romaji-basic))
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e--type-romaji-chars romaji)
    (stringp (buffer-string)))
  30)

;; mode-preservation: typing lowercase romaji in any mode must not switch mode.
;; Uses nskk-property-test-seeded instead of dotimes+nskk-for-all.
(nskk-property-test-seeded romaji-mode-preserved
  ((mode valid-mode))
  (let ((romaji (nskk-e2e--random-romaji-basic)))
    (nskk-e2e-with-buffer mode nil
      (nskk-e2e--type-romaji-chars romaji)
      (eq (nskk-current-mode) mode)))
  25)

;;;;
;;;; Small Kana (xa/la rows) -- Romaji Edge Cases
;;;;

(nskk-describe "small kana romaji rows"
  (nskk-deftest-table buffer-romaji-small-kana-xa-row
    :columns (romaji kana)
    ;; la/li/lu/le/lo omitted: 'l' is bound to nskk-handle-l (Latin-mode switch)
    ;; and cannot be used as a romaji prefix in E2E key-event tests.
    :rows (("xa" "ぁ") ("xi" "ぃ") ("xu" "ぅ") ("xe" "ぇ") ("xo" "ぉ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana))))

  (nskk-deftest-table buffer-romaji-small-kana-xtsu
    :columns (romaji kana)
    ;; ltsu/ltu omitted: 'l' is a mode-switch key, cannot be used as romaji prefix.
    :rows (("xtsu" "っ") ("xtu" "っ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana))))

  (nskk-deftest-table buffer-romaji-small-kana-xya-row
    :columns (romaji kana)
    ;; lya/lyu/lyo omitted: 'l' is a mode-switch key, cannot be used as romaji prefix.
    :rows (("xya" "ゃ") ("xyu" "ゅ") ("xyo" "ょ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana)))))

;;;;
;;;; V-row (ゔ combinations) -- Romaji Edge Cases
;;;;

(nskk-describe "v-row romaji"
  (nskk-deftest-table buffer-romaji-v-row
    :columns (romaji kana)
    :rows (("va"  "ゔぁ")
           ("vi"  "ゔぃ")
           ("vu"  "ゔ")
           ("ve"  "ゔぇ")
           ("vo"  "ゔぉ")
           ("vya" "ゔゃ")
           ("vyu" "ゔゅ")
           ("vyo" "ゔょ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana)))))

;;;;
;;;; Foreign Extension Rows -- Romaji Edge Cases
;;;;

(nskk-describe "foreign extension romaji rows"
  (nskk-deftest-table buffer-romaji-th-row
    :columns (romaji kana)
    :rows (("tha" "てぁ")
           ("thi" "てぃ")
           ("thu" "てゅ")
           ("the" "てぇ")
           ("tho" "てょ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana))))

  (nskk-deftest-table buffer-romaji-dh-row
    :columns (romaji kana)
    :rows (("dha" "でぁ")
           ("dhi" "でぃ")
           ("dhu" "でゅ")
           ("dhe" "でぇ")
           ("dho" "でょ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana))))

  (nskk-deftest-table buffer-romaji-wh-row
    :columns (romaji kana)
    :rows (("wha" "うぁ")
           ("whi" "うぃ")
           ("whu" "う")
           ("whe" "うぇ")
           ("who" "うぉ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana)))))

;;;;
;;;; N-apostrophe -- Romaji Edge Cases
;;;;

(nskk-describe "n apostrophe romaji"
  (nskk-it "n' produces hatsuon kana"
    ;; n' is a direct rule in the converter table AND handled by
    ;; nskk-convert-n--internal which checks (aref remaining 1) == 39 (ASCII ').
    ;; The apostrophe is consumed and does not appear in the output.
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "n'")
      (nskk-e2e-assert-buffer "ん"
                              "romaji \"n'\" → \"ん\" failed")))

  (nskk-it "n' followed by vowel separates hatsuon from the vowel"
    ;; n' followed by a vowel: apostrophe explicitly separates ん from the vowel.
    ;; Without apostrophe, "na" → "な"; with apostrophe, "n'" → "ん" then "a" → "あ".
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "n'a")
      (nskk-e2e-assert-buffer "んあ"
                              "romaji \"n'a\" → \"んあ\" failed"))))

;;;;
;;;; Long Vowel in Katakana -- Romaji Edge Cases
;;;;

(nskk-describe "long vowel in katakana"
  (nskk-it "hyphen produces long vowel mark in katakana mode"
    ;; The - rule is defined in nskk-converter.el as:
    ;;   (nskk-converter-add-rule "-" "ー")
    ;; It applies in both hiragana and katakana mode (the converter does not
    ;; distinguish modes; katakana conversion uppercases the result).
    ;; We test in katakana mode as that is the primary use case.
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type "-")
      (nskk-e2e-assert-buffer "ー"
                              "romaji \"-\" → \"ー\" failed in katakana mode"))))

(provide 'nskk-e2e-kana-input)

;;; nskk-e2e-kana-input.el ends here
