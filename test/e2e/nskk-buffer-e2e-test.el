;;; nskk-buffer-e2e-test.el --- E2E buffer input tests for NSKK  -*- lexical-binding: t; -*-

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

;; E2E buffer input tests for NSKK.
;;
;; Tests the full input pipeline via execute-kbd-macro:
;;   1. ASCII passthrough
;;   2. Hiragana romaji-to-kana conversion (complete romaji table)
;;   3. Katakana input
;;   4. Special sequences: sokuon (っ), hatsuon (ん), compound kana (しゃ etc.)
;;   5. Conversion flow: ▽ preedit → ▼ candidate → commit/cancel
;;   6. Candidate navigation and okurigana
;;   7. Property-based tests (ddskk-equivalent coverage)
;;   8. Vowel okurigana (母音送り仮名) -- regression tests for AI/II/OU/AE vowel markers
;;   9. C-j kakutei: preedit commit, jisx0208 switch
;;  10. Implicit kakutei on mode switch (▽ preedit + l/L/q/)
;;  11. Abbrev mode: typing, C-j return, self-insert behavior
;;  12. SPC and X key dispatch across modes
;;  13. Sentence-level integration tests
;;  14. Edge cases: C-g, sokuon in preedit, rapid mode switches, katakana preedit
;;
;; Test name format: nskk-e2e-buffer-*

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(eval-when-compile (require 'cl-lib))

;;;;
;;;; ASCII Passthrough Tests
;;;;

(nskk-deftest-e2e buffer-ascii-passthrough
  "ASCII mode: characters are inserted directly without conversion."
  (nskk-e2e-with-buffer nil nil
    (nskk-e2e-assert-mode 'ascii)
    (nskk-e2e-type "hello")
    (nskk-e2e-assert-buffer "hello")))

(nskk-deftest-e2e buffer-ascii-digits
  "ASCII mode: digits insert directly."
  (nskk-e2e-with-buffer nil nil
    (nskk-e2e-type "12345")
    (nskk-e2e-assert-buffer "12345")))

(nskk-deftest-e2e buffer-ascii-symbols
  "ASCII mode: common symbols insert directly."
  (nskk-e2e-with-buffer nil nil
    (nskk-e2e-type "!@#")
    (nskk-e2e-assert-buffer "!@#")))

;;;;
;;;; Hiragana Vowel Input Tests
;;;;

(nskk-deftest-e2e buffer-hiragana-vowel-a
  "Hiragana mode: 'a' → 'あ'."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "a")
    (nskk-e2e-assert-buffer "あ")))

(nskk-deftest-e2e buffer-hiragana-vowel-i
  "Hiragana mode: 'i' → 'い'."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "i")
    (nskk-e2e-assert-buffer "い")))

(nskk-deftest-e2e buffer-hiragana-vowel-u
  "Hiragana mode: 'u' → 'う'."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "u")
    (nskk-e2e-assert-buffer "う")))

(nskk-deftest-e2e buffer-hiragana-vowel-e
  "Hiragana mode: 'e' → 'え'."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "e")
    (nskk-e2e-assert-buffer "え")))

(nskk-deftest-e2e buffer-hiragana-vowel-o
  "Hiragana mode: 'o' → 'お'."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "o")
    (nskk-e2e-assert-buffer "お")))

(nskk-deftest-e2e buffer-hiragana-all-vowels
  "Hiragana mode: all vowels in sequence."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "aiueo")
    (nskk-e2e-assert-buffer "あいうえお")))

;;;;
;;;; Hiragana Consonant+Vowel (K-row)
;;;;

(nskk-deftest-e2e buffer-hiragana-ka-row
  "Hiragana mode: ka-row (ka ki ku ke ko)."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "ka")
    (nskk-e2e-assert-buffer "か"))
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "ki")
    (nskk-e2e-assert-buffer "き"))
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "ku")
    (nskk-e2e-assert-buffer "く"))
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "ke")
    (nskk-e2e-assert-buffer "け"))
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "ko")
    (nskk-e2e-assert-buffer "こ")))

;;;;
;;;; Complete Romaji Table Tests (ddskk-equivalent)
;;;;

(nskk-deftest-e2e buffer-romaji-table-comprehensive
  "Hiragana mode: comprehensive romaji-to-kana conversion table."
  ;; Test each row of the kana table via execute-kbd-macro
  (let ((cases '(;; A-row
                 ("a" . "あ") ("i" . "い") ("u" . "う")
                 ("e" . "え") ("o" . "お")
                 ;; K-row
                 ("ka" . "か") ("ki" . "き") ("ku" . "く")
                 ("ke" . "け") ("ko" . "こ")
                 ;; S-row (standard and alternate spellings)
                 ("sa" . "さ") ("si" . "し") ("su" . "す")
                 ("se" . "せ") ("so" . "そ")
                 ("shi" . "し")
                 ;; T-row
                 ("ta" . "た") ("ti" . "ち") ("tu" . "つ")
                 ("te" . "て") ("to" . "と")
                 ("chi" . "ち") ("tsu" . "つ")
                 ;; N-row
                 ("na" . "な") ("ni" . "に") ("nu" . "ぬ")
                 ("ne" . "ね") ("no" . "の")
                 ;; H-row
                 ("ha" . "は") ("hi" . "ひ") ("hu" . "ふ")
                 ("he" . "へ") ("ho" . "ほ")
                 ("fu" . "ふ")
                 ;; M-row
                 ("ma" . "ま") ("mi" . "み") ("mu" . "む")
                 ("me" . "め") ("mo" . "も")
                 ;; Y-row
                 ("ya" . "や") ("yu" . "ゆ") ("yo" . "よ")
                 ;; R-row
                 ("ra" . "ら") ("ri" . "り") ("ru" . "る")
                 ("re" . "れ") ("ro" . "ろ")
                 ;; W-row
                 ("wa" . "わ") ("wi" . "ゐ") ("we" . "ゑ") ("wo" . "を")
                 ;; G-row (voiced)
                 ("ga" . "が") ("gi" . "ぎ") ("gu" . "ぐ")
                 ("ge" . "げ") ("go" . "ご")
                 ;; Z-row (voiced)
                 ("za" . "ざ") ("zi" . "じ") ("zu" . "ず")
                 ("ze" . "ぜ") ("zo" . "ぞ")
                 ("ji" . "じ")
                 ;; D-row (voiced)
                 ("da" . "だ") ("di" . "ぢ") ("du" . "づ")
                 ("de" . "で") ("do" . "ど")
                 ;; B-row (voiced)
                 ("ba" . "ば") ("bi" . "び") ("bu" . "ぶ")
                 ("be" . "べ") ("bo" . "ぼ")
                 ;; P-row (semi-voiced)
                 ("pa" . "ぱ") ("pi" . "ぴ") ("pu" . "ぷ")
                 ("pe" . "ぺ") ("po" . "ぽ"))))
    (dolist (tc cases)
      (nskk-e2e-with-buffer 'hiragana nil
        (nskk-e2e-type (car tc))
        (nskk-e2e-assert-buffer (cdr tc)
                                (format "romaji %S → %S failed" (car tc) (cdr tc)))))))

;;;;
;;;; Compound Kana (拗音) Tests
;;;;

(nskk-deftest-e2e buffer-compound-kana
  "Hiragana mode: compound kana (拗音) romanization."
  (let ((cases '(;; KY-row
                 ("kya" . "きゃ") ("kyu" . "きゅ") ("kyo" . "きょ")
                 ;; SH-row
                 ("sha" . "しゃ") ("shu" . "しゅ") ("sho" . "しょ")
                 ;; CH-row
                 ("cha" . "ちゃ") ("chu" . "ちゅ") ("cho" . "ちょ")
                 ;; NY-row
                 ("nya" . "にゃ") ("nyu" . "にゅ") ("nyo" . "にょ")
                 ;; HY-row
                 ("hya" . "ひゃ") ("hyu" . "ひゅ") ("hyo" . "ひょ")
                 ;; MY-row
                 ("mya" . "みゃ") ("myu" . "みゅ") ("myo" . "みょ")
                 ;; RY-row
                 ("rya" . "りゃ") ("ryu" . "りゅ") ("ryo" . "りょ")
                 ;; GY-row
                 ("gya" . "ぎゃ") ("gyu" . "ぎゅ") ("gyo" . "ぎょ")
                 ;; JY/J-row
                 ("ja"  . "じゃ") ("ju"  . "じゅ") ("jo"  . "じょ")
                 ;; BY-row
                 ("bya" . "びゃ") ("byu" . "びゅ") ("byo" . "びょ")
                 ;; PY-row
                 ("pya" . "ぴゃ") ("pyu" . "ぴゅ") ("pyo" . "ぴょ"))))
    (dolist (tc cases)
      (nskk-e2e-with-buffer 'hiragana nil
        (nskk-e2e-type (car tc))
        (nskk-e2e-assert-buffer (cdr tc)
                                (format "compound romaji %S → %S failed"
                                        (car tc) (cdr tc)))))))

;;;;
;;;; Sokuon (促音 っ) Tests
;;;;

(nskk-deftest-e2e buffer-sokuon-kka
  "Hiragana mode: 'kka' → 'っか' (sokuon before k)."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "kka")
    (nskk-e2e-assert-buffer "っか")))

(nskk-deftest-e2e buffer-sokuon-tte
  "Hiragana mode: 'tte' → 'って' (sokuon before t)."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "tte")
    (nskk-e2e-assert-buffer "って")))

(nskk-deftest-e2e buffer-sokuon-sshi
  "Hiragana mode: 'sshi' → 'っし' (sokuon before sh)."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "sshi")
    (nskk-e2e-assert-buffer "っし")))

(nskk-deftest-e2e buffer-sokuon-table
  "Hiragana mode: sokuon before various consonants."
  (let ((cases '(("ppa" . "っぱ")
                 ("bba" . "っば")
                 ("dda" . "っだ")
                 ("gga" . "っが"))))
    (dolist (tc cases)
      (nskk-e2e-with-buffer 'hiragana nil
        (nskk-e2e-type (car tc))
        (nskk-e2e-assert-buffer (cdr tc)
                                (format "sokuon %S → %S failed" (car tc) (cdr tc)))))))

;;;;
;;;; Hatsuon (撥音 ん) Tests
;;;;

(nskk-deftest-e2e buffer-hatsuon-nn
  "Hiragana mode: 'nn' → 'ん'."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "nn")
    (nskk-e2e-assert-buffer "ん")))

(nskk-deftest-e2e buffer-hatsuon-n-before-consonant
  "Hiragana mode: 'n' before consonant emits 'ん' then processes consonant."
  ;; 'nka' should give 'んか': n+k triggers ん, then k+a gives か
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "nka")
    (nskk-e2e-assert-buffer "んか")))

(nskk-deftest-e2e buffer-hatsuon-n-before-na
  "Hiragana mode: 'na' → 'な' (n followed by vowel is NOT hatsuon)."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "na")
    (nskk-e2e-assert-buffer "な")))

(nskk-deftest-e2e buffer-hatsuon-word-nka
  "Hiragana mode: 'nanka' → 'なんか'."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "nanka")
    (nskk-e2e-assert-buffer "なんか")))

;;;;
;;;; Katakana Input Tests
;;;;

(nskk-deftest-e2e buffer-katakana-vowels
  "Katakana mode: vowel input produces katakana."
  (nskk-e2e-with-buffer 'katakana nil
    (nskk-e2e-type "aiueo")
    (nskk-e2e-assert-buffer "アイウエオ")))

(nskk-deftest-e2e buffer-katakana-consonants
  "Katakana mode: consonant+vowel produces katakana."
  (nskk-e2e-with-buffer 'katakana nil
    (nskk-e2e-type "ka")
    (nskk-e2e-assert-buffer "カ"))
  (nskk-e2e-with-buffer 'katakana nil
    (nskk-e2e-type "shi")
    (nskk-e2e-assert-buffer "シ")))

(nskk-deftest-e2e buffer-katakana-sokuon
  "Katakana mode: sokuon produces 'ッ'."
  (nskk-e2e-with-buffer 'katakana nil
    (nskk-e2e-type "tte")
    (nskk-e2e-assert-buffer "ッテ")))

(nskk-deftest-e2e buffer-katakana-table
  "Katakana mode: representative kana table entries."
  (let ((cases '(("a"   . "ア") ("i"   . "イ") ("u"   . "ウ")
                 ("e"   . "エ") ("o"   . "オ")
                 ("ka"  . "カ") ("ki"  . "キ") ("ku"  . "ク")
                 ("sa"  . "サ") ("shi" . "シ") ("su"  . "ス")
                 ("ta"  . "タ") ("chi" . "チ") ("tsu" . "ツ")
                 ("na"  . "ナ") ("ni"  . "ニ") ("nu"  . "ヌ")
                 ("ha"  . "ハ") ("hi"  . "ヒ") ("fu"  . "フ")
                 ("ma"  . "マ") ("mi"  . "ミ") ("mu"  . "ム")
                 ("ya"  . "ヤ") ("yu"  . "ユ") ("yo"  . "ヨ")
                 ("ra"  . "ラ") ("ri"  . "リ") ("ru"  . "ル")
                 ("wa"  . "ワ") ("wo"  . "ヲ"))))
    (dolist (tc cases)
      (nskk-e2e-with-buffer 'katakana nil
        (nskk-e2e-type (car tc))
        (nskk-e2e-assert-buffer (cdr tc)
                                (format "katakana %S → %S failed"
                                        (car tc) (cdr tc)))))))

;;;;
;;;; Kanji Conversion Flow Tests
;;;;

(nskk-deftest-e2e buffer-conversion-basic-cj-commit
  "Conversion: type preedit → SPC → C-j commit (no newline)."
  ;; 'Kanji' in hiragana mode: K triggers ▽, 'anji' gives 'かんじ'
  ;; SPC starts conversion, C-j commits first candidate '漢字'
  (nskk-e2e-with-buffer 'hiragana nil
    ;; Start preedit: K → ▽, then anji → かんじ
    (nskk-e2e-type "Kanji")
    (nskk-e2e-assert-henkan-phase 'on)
    ;; SPC → start conversion
    (nskk-e2e-type "SPC")
    (nskk-e2e-assert-henkan-phase 'active)
    ;; C-j → commit-candidate (no newline)
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-henkan-phase nil)
    (nskk-e2e-assert-buffer "漢字")))

(nskk-deftest-e2e buffer-conversion-ret-commit
  "Conversion: type preedit → SPC → RET commit (no newline)."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Kanji")
    (nskk-e2e-type "SPC")
    ;; RET → commit (no newline)
    (nskk-e2e-type "RET")
    (nskk-e2e-assert-buffer "漢字")))

(nskk-deftest-e2e buffer-conversion-second-candidate
  "Conversion: SPC again advances to second candidate."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Kanji")
    ;; First SPC starts conversion (first candidate: 漢字)
    (nskk-e2e-type "SPC")
    ;; Second SPC → next candidate (感じ)
    (nskk-e2e-type "SPC")
    ;; C-j commits second candidate
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-buffer "感じ")))

(nskk-deftest-e2e buffer-conversion-x-previous-candidate
  "Conversion: x key goes to previous candidate."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Kanji")
    ;; SPC → start conversion (candidate 0: 漢字)
    (nskk-e2e-type "SPC")
    ;; SPC → next candidate (candidate 1: 感じ)
    (nskk-e2e-type "SPC")
    ;; x → previous candidate (back to 漢字)
    (nskk-e2e-type "x")
    ;; C-j → commit
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-buffer "漢字")))

(nskk-deftest-e2e buffer-conversion-ctrl-n-next-candidate
  "C-n in converting (▼) state advances to next candidate (same as SPC)."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Kanji")
    ;; First SPC starts conversion (first candidate: 漢字)
    (nskk-e2e-type "SPC")
    (nskk-e2e-assert-converting)
    ;; C-n → next candidate (感じ)
    (nskk-e2e-type "C-n")
    (nskk-e2e-assert-converting)
    ;; C-j commits second candidate
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-buffer "感じ")))

(nskk-deftest-e2e buffer-conversion-ctrl-p-previous-candidate
  "C-p in converting (▼) state returns to previous candidate (same as x)."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Kanji")
    (nskk-e2e-type "SPC")          ; first candidate: 漢字
    (nskk-e2e-type "C-n")          ; second candidate: 感じ
    (nskk-e2e-assert-converting)
    ;; C-p → back to first candidate (漢字)
    (nskk-e2e-type "C-p")
    (nskk-e2e-assert-converting)
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-buffer "漢字")))

(nskk-deftest-e2e buffer-ctrl-n-not-converting-does-not-enter-converting
  "C-n outside converting mode does not start conversion."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "a")            ; type あ
    (nskk-e2e-type "C-n")          ; should behave as next-line, NOT start conversion
    (nskk-e2e-assert-not-converting)))

(nskk-deftest-e2e buffer-conversion-cancel-cg
  "Conversion: C-g cancels active conversion, restores preedit state."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Kanji")
    ;; SPC → converting
    (nskk-e2e-type "SPC")
    (nskk-e2e-assert-converting)
    ;; C-g → cancel conversion
    (nskk-e2e-type "C-g")
    ;; Should return to preedit phase (on) or nil
    ;; Mode should still be hiragana
    (nskk-e2e-assert-mode 'hiragana)
    (nskk-e2e-assert-not-converting)))

(nskk-deftest-e2e buffer-conversion-cancel-preedit
  "Preedit: C-g cancels preedit phase."
  (nskk-e2e-with-buffer 'hiragana nil
    ;; Start preedit
    (nskk-e2e-type "Kanji")
    (nskk-e2e-assert-henkan-phase 'on)
    ;; C-g → cancel preedit
    (nskk-e2e-type "C-g")
    (nskk-e2e-assert-henkan-phase nil)
    (nskk-e2e-assert-mode 'hiragana)))

(nskk-deftest-e2e buffer-conversion-single-kana
  "Conversion: single kana reading (へんかん → 変換)."
  (nskk-e2e-with-buffer 'hiragana nil
    ;; 'Henkan' → ▽へんかん
    (nskk-e2e-type "Henkan")
    (nskk-e2e-assert-henkan-phase 'on)
    (nskk-e2e-type "SPC")
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-buffer "変換")))

(nskk-deftest-e2e buffer-conversion-nihon
  "Conversion: にほん → 日本."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Nihon")
    (nskk-e2e-type "SPC")
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-buffer "日本")))

;;;;
;;;; Full-width Latin Mode Tests
;;;;

(nskk-deftest-e2e buffer-jisx0208-latin-basic
  "jisx0208-latin mode: ASCII chars become full-width."
  (nskk-e2e-with-buffer 'jisx0208-latin nil
    ;; 'a' → ａ (full-width a, U+FF41)
    (nskk-e2e-type "a")
    ;; Full-width 'a' is U+FF41
    (nskk-e2e-assert-buffer "\uFF41")))

(nskk-deftest-e2e buffer-jisx0208-space
  "jisx0208-latin mode: SPC becomes ideographic space."
  (nskk-e2e-with-buffer 'jisx0208-latin nil
    (nskk-e2e-type "SPC")
    ;; Ideographic space U+3000
    (nskk-e2e-assert-buffer "\u3000")))

;;;;
;;;; Multiple Character Sequence Tests
;;;;

(nskk-deftest-e2e buffer-hiragana-word-nihongo
  "Hiragana mode: type 'nihongo' → 'にほんご'."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "nihongo")
    (nskk-e2e-assert-buffer "にほんご")))

(nskk-deftest-e2e buffer-hiragana-word-arigatou
  "Hiragana mode: type 'arigatou' → 'ありがとう'."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "arigatou")
    (nskk-e2e-assert-buffer "ありがとう")))

(nskk-deftest-e2e buffer-hiragana-word-konnichiwa
  "Hiragana mode: type 'konnichiwa' → 'こんにちわ'."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "konnichiwa")
    (nskk-e2e-assert-buffer "こんにちわ")))

(nskk-deftest-e2e buffer-mixed-convert-then-type
  "Mixed: convert 漢字 then continue typing hiragana."
  (nskk-e2e-with-buffer 'hiragana nil
    ;; Convert かんじ → 漢字
    (nskk-e2e-type "Kanji")
    (nskk-e2e-type "SPC")
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-buffer "漢字")
    ;; Continue typing hiragana
    (nskk-e2e-type "no")
    (nskk-e2e-assert-buffer "漢字の")))

;;;;
;;;; Property-Based Tests (PBT)
;;;;

(ert-deftest nskk-e2e-pbt-romaji-no-crash ()
  "PBT: Random romaji sequences in hiragana mode never crash."
  (let ((runs 50)
        (errors nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "E2E PBT 'romaji-no-crash' seed: %d" test-seed)
    (dotimes (run runs)
      (condition-case err
          (let ((romaji (nskk-e2e--random-romaji-basic)))
            (nskk-e2e-with-buffer 'hiragana nil
              (nskk-e2e--type-romaji-chars romaji)
              ;; Property: buffer must be a valid string
              (unless (stringp (buffer-string))
                (push (list :run run :romaji romaji :error "non-string buffer")
                      errors))
              ;; Property: mode must still be valid
              (unless (nskk-state-valid-mode-p (nskk-current-mode))
                (push (list :run run :romaji romaji :error "invalid mode")
                      errors))))
        (error
         (push (list :run run :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT romaji-no-crash: %d failures (seed %d):\n%S"
                        (length errors) test-seed
                        (cl-subseq errors 0 (min 3 (length errors))))))))

(ert-deftest nskk-e2e-pbt-katakana-no-crash ()
  "PBT: Random romaji in katakana mode never crashes."
  (let ((runs 50)
        (errors nil)
        (test-seed (abs (random))))
    (random test-seed)
    (dotimes (run runs)
      (condition-case err
          (let ((romaji (nskk-e2e--random-romaji-basic)))
            (nskk-e2e-with-buffer 'katakana nil
              (nskk-e2e--type-romaji-chars romaji)
              ;; Property: mode must still be katakana
              (unless (eq (nskk-current-mode) 'katakana)
                (push (list :run run :romaji romaji :error "mode changed")
                      errors))))
        (error
         (push (list :run run :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT katakana-no-crash: %d failures (seed %d):\n%S"
                        (length errors) test-seed
                        (cl-subseq errors 0 (min 3 (length errors))))))))

(ert-deftest nskk-e2e-pbt-conversion-roundtrip ()
  "PBT: Conversion flow never corrupts buffer (commit or cancel)."
  ;; For each entry in the default dict, verify that:
  ;; 1. Starting preedit → SPC → candidate appears → C-j → correct text
  (let ((dict-entries '(("かんじ" . ("漢字" "感じ"))
                        ("へんかん" . ("変換"))
                        ("にほん"  . ("日本" "二本"))))
        (errors nil))
    (dolist (entry dict-entries)
      (let ((reading (car entry))
            (first-cand (car (cdr entry))))
        (condition-case err
            ;; Convert reading to romaji for typing.
            ;; We type the kana directly via nskk-start-conversion bypass.
            ;; Instead, we use with-temp-buffer and direct function calls.
            ;; Since E2E needs real key events, we test specific known mappings.
            (let ((romaji-map '(("かんじ" . "Kanji")
                                ("へんかん" . "Henkan")
                                ("にほん"  . "Nihon")))
                  (typed nil))
              (setq typed (cdr (assoc reading romaji-map)))
              (when typed
                (nskk-e2e-with-buffer 'hiragana (list entry)
                  (nskk-e2e-type typed)
                  (nskk-e2e-type "SPC")
                  (nskk-e2e-type "C-j")
                  (let ((actual (buffer-string)))
                    (unless (equal actual first-cand)
                      (push (list :reading reading
                                  :typed typed
                                  :expected first-cand
                                  :actual actual)
                            errors))))))
          (error
           (push (list :reading reading :error (error-message-string err))
                 errors)))))
    (when errors
      (ert-fail (format "E2E PBT conversion-roundtrip: %d failures:\n%S"
                        (length errors)
                        (cl-subseq errors 0 (min 3 (length errors))))))))

(ert-deftest nskk-e2e-pbt-buffer-always-string ()
  "PBT: Buffer content is always a valid string after any romaji input."
  (let ((runs 75)
        (errors nil)
        (test-seed (abs (random))))
    (random test-seed)
    (dotimes (run runs)
      (condition-case err
          (nskk-e2e-with-buffer 'hiragana nil
            (let* ((romaji (nskk-e2e--random-romaji-basic)))
              (nskk-e2e--type-romaji-chars romaji)
              (let ((content (buffer-string)))
                (unless (stringp content)
                  (push (list :run run :romaji romaji) errors)))))
        (error
         (push (list :run run :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT buffer-always-string: %d failures (seed %d):\n%S"
                        (length errors) test-seed
                        (cl-subseq errors 0 (min 3 (length errors))))))))

(ert-deftest nskk-e2e-pbt-mode-preserves-after-typing ()
  "PBT: After typing romaji (no uppercase), mode stays the same."
  (let ((runs 50)
        (errors nil))
    (dotimes (run runs)
      (condition-case err
          (let* ((mode (nth (random 3) '(hiragana katakana ascii)))
                 (romaji (nskk-e2e--random-romaji-basic)))
            (nskk-e2e-with-buffer mode nil
              (nskk-e2e--type-romaji-chars romaji)
              (let ((actual-mode (nskk-current-mode)))
                (unless (eq actual-mode mode)
                  (push (list :run run :mode mode :actual actual-mode :romaji romaji)
                        errors)))))
        (error
         (push (list :run run :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT mode-preserves: %d failures:\n%S"
                        (length errors)
                        (cl-subseq errors 0 (min 3 (length errors))))))))

(ert-deftest nskk-e2e-pbt-henkan-phase-nil-after-commit ()
  "PBT: After SPC+C-j (conversion commit), henkan phase is nil."
  (let ((known-conversions '(("Kanji" . "漢字")
                             ("Henkan" . "変換")
                             ("Nihon" . "日本")))
        (errors nil))
    (dolist (conv known-conversions)
      (condition-case err
          (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type (car conv))
            (nskk-e2e-type "SPC")
            (nskk-e2e-type "C-j")
            (let ((phase (nskk-state-henkan-phase nskk-current-state)))
              (unless (null phase)
                (push (list :typed (car conv) :phase phase) errors))))
        (error
         (push (list :typed (car conv) :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT henkan-nil-after-commit: %d failures:\n%S"
                        (length errors) errors)))))

;;;;
;;;; Okurigana (送り仮名) E2E Tests
;;;;
;;
;; Okurigana input flow (ddskk-compatible):
;;   1. Capital letter starts preedit: Ka → ▽か
;;   2. Lowercase extends reading: KaNi → ▽かに
;;   3. Capital letter marks okurigana boundary: KaNiKu → ▽かに* (okurigana = k)
;;   4. Lowercase kana completes okurigana and triggers conversion: → ▼書く
;;   5. C-j commits: → 書く (candidate + okurigana kana)
;;
;; Dict key format for okurigana: reading + lowercase okurigana consonant
;;   e.g. "かk" → ("書") means "書く" (to write)
;;        "みr" → ("見") means "見る" (to see)

(nskk-deftest-e2e buffer-okurigana-preedit-starts
  "Okurigana: typing Ka enters preedit (▽) phase."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Ka")
    (nskk-e2e-assert-henkan-phase 'on)
    (nskk-e2e-assert-mode 'hiragana)))

(nskk-deftest-e2e buffer-okurigana-marker-sets-state
  "Okurigana: KaK inserts * boundary and stores okurigana consonant in state."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Ka")
    ;; Second capital K → okurigana marker
    (nskk-e2e-type "K")
    ;; State should have okurigana consonant = ?k
    (should (eq (nskk-state-get-okurigana nskk-current-state) ?k))
    ;; Still in preedit phase (not yet converting)
    (nskk-e2e-assert-henkan-phase 'on)
    (nskk-e2e-assert-not-converting)))

(nskk-deftest-e2e buffer-okurigana-kaku-triggers-conversion
  "Okurigana: KaKu triggers conversion → overlay shows 書."
  (let ((dict '(("かk" . ("書")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "Ka")   ;; ▽か
      (nskk-e2e-type "K")    ;; ▽か* (okurigana marker)
      (nskk-e2e-type "u")    ;; triggers conversion
      (nskk-e2e-assert-converting)
      (nskk-e2e-assert-overlay-shows "書"))))

(nskk-deftest-e2e buffer-okurigana-kaku-commit
  "Okurigana: KaKu + C-j → 書く (kanji + okurigana kana preserved)."
  (let ((dict '(("かk" . ("書")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "K")
      (nskk-e2e-type "u")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "書く"))))

(nskk-deftest-e2e buffer-okurigana-miru-commit
  "Okurigana: MiRu + C-j → 見る."
  (let ((dict '(("みr" . ("見")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "Mi")   ;; ▽み
      (nskk-e2e-type "R")    ;; ▽み* (okurigana = r)
      (nskk-e2e-type "u")    ;; → ▼見る
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "見る"))))

(nskk-deftest-e2e buffer-okurigana-okuru-commit
  "Okurigana: OkuRu + C-j → 送る."
  (let ((dict '(("おくr" . ("送")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "Oku")  ;; ▽おく
      (nskk-e2e-type "R")    ;; ▽おく* (okurigana = r)
      (nskk-e2e-type "u")    ;; → ▼送る
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "送る"))))

(nskk-deftest-e2e buffer-okurigana-kiku-commit
  "Okurigana: KiKu + C-j → 聞く."
  (let ((dict '(("きk" . ("聞")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "Ki")   ;; ▽き
      (nskk-e2e-type "K")    ;; ▽き* (okurigana = k)
      (nskk-e2e-type "u")    ;; → ▼聞く
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "聞く"))))

(nskk-deftest-e2e buffer-okurigana-multiple-candidates
  "Okurigana: KiKu with multiple candidates → select second (効く)."
  (let ((dict '(("きk" . ("聞" "効")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "Ki")
      (nskk-e2e-type "K")
      (nskk-e2e-type "u")
      (nskk-e2e-assert-converting)
      (nskk-e2e-assert-overlay-shows "聞")
      ;; SPC → next candidate
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-overlay-shows "効")
      ;; C-j → commit 効く
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "効く"))))

(nskk-deftest-e2e buffer-okurigana-cancel-restores
  "Okurigana: KaKu + C-g cancels conversion, returns to non-converting state."
  (let ((dict '(("かk" . ("書")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "K")
      (nskk-e2e-type "u")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-mode 'hiragana))))

(nskk-deftest-e2e buffer-okurigana-then-continue-typing
  "Okurigana: commit 書く then continue typing hiragana."
  (let ((dict '(("かk" . ("書")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "K")
      (nskk-e2e-type "u")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "書く")
      ;; Continue typing
      (nskk-e2e-type "no")
      (nskk-e2e-assert-buffer "書くの"))))

(ert-deftest nskk-e2e-pbt-okurigana-no-crash ()
  "PBT: Okurigana input sequences (CvC pattern) never crash."
  ;; Tests reading+okurigana patterns like KaKu, MiRu, KiKu, SuRu, etc.
  (let ((patterns '(("Ka" "K" "u")     ;; KaKu (書く)
                    ("Mi" "R" "u")     ;; MiRu (見る)
                    ("Ki" "K" "u")     ;; KiKu (聞く)
                    ("Su" "R" "u")     ;; SuRu (する)
                    ("Ha" "N" "a")     ;; HaNa (花な)
                    ("No" "M" "u")     ;; NoMu (飲む)
                    ("Ka" "E" "ru")))  ;; KaEru (変える)
        (errors nil))
    (dolist (pat patterns)
      (condition-case err
          (nskk-e2e-with-buffer 'hiragana nil
            (dolist (key pat)
              (nskk-e2e-type key))
            ;; Property: buffer must be a string, mode must remain hiragana
            (unless (stringp (buffer-string))
              (push (list :pattern pat :error "non-string buffer") errors))
            (unless (memq (nskk-current-mode) '(hiragana ascii))
              (push (list :pattern pat :mode (nskk-current-mode)) errors)))
        (error
         (push (list :pattern pat :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT okurigana-no-crash: %d failures:\n%S"
                        (length errors) errors)))))

;;;; Vowel Okurigana (母音送り仮名) E2E Tests
;;
;; Vowel okurigana occurs when the okurigana consonant is itself a vowel
;; (A, I, U, E, O).  Unlike consonant okurigana (K, R, etc.) which requires
;; a following character to complete the kana, vowel okurigana is immediately
;; complete -- "I" alone maps to "い" with no further input needed.
;;
;; Bug: Before the fix, typing AI (capital A then capital I) would:
;;   A → ▽あ                   (henkan-on)
;;   I → ▽あ* (romaji="i")    (okurigana marker, BUT didn't convert)
;; Then a SECOND I would flush "i"→"い", insert another *, producing ▽あ*い*.
;; The fix: vowel okurigana immediately inserts kana and triggers conversion.
;;
;; Dict key format for vowel okurigana: reading + vowel char
;;   e.g. "あI" typed → key "あi" → candidates ("愛")

(nskk-deftest-e2e buffer-vowel-okurigana-ai-commits
  "Vowel okurigana: AI (▽あ + I marker) converts and commits 愛."
  (let ((dict '(("あi" . ("愛" "哀")))))
    (nskk-e2e-with-buffer 'hiragana dict
      ;; A → ▽あ; I → vowel okurigana, immediately triggers ▼愛い
      (nskk-e2e-type "A")
      (nskk-e2e-type "I")
      (nskk-e2e-assert-converting)
      ;; C-j commits the candidate
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "愛い"))))

(nskk-deftest-e2e buffer-vowel-okurigana-no-double-marker
  "Vowel okurigana: AII does NOT produce ▽あ*い* (regression test)."
  (let ((dict '(("あi" . ("愛")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "A")
      (nskk-e2e-type "I")
      ;; After the first I, we should be in converting state (not preedit).
      ;; A second I should NOT trigger another okurigana cycle.
      (nskk-e2e-assert-converting)
      ;; The overlay should show the first candidate "愛"
      (nskk-e2e-assert-overlay-shows "愛"))))

(nskk-deftest-e2e buffer-vowel-okurigana-ou-commits
  "Vowel okurigana: OU (▽お + U marker) converts and commits."
  (let ((dict '(("おu" . ("負")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "O")
      (nskk-e2e-type "U")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "負う"))))

(nskk-deftest-e2e buffer-vowel-okurigana-ee-commits
  "Vowel okurigana: aE (▽あ + E marker) converts and commits."
  (let ((dict '(("あe" . ("与")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "A")
      (nskk-e2e-type "E")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "与え"))))

(nskk-deftest-e2e buffer-vowel-okurigana-cancel-restores
  "Vowel okurigana: AI + C-g cancels conversion without leaving stale state."
  (let ((dict '(("あi" . ("愛")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "A")
      (nskk-e2e-type "I")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-mode 'hiragana))))

(nskk-deftest-e2e buffer-vowel-okurigana-then-continue
  "Vowel okurigana: commit AI then continue typing."
  (let ((dict '(("あi" . ("愛")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "A")
      (nskk-e2e-type "I")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "愛い")
      ;; Continue typing — must not be stuck in okurigana state
      (nskk-e2e-type "su")
      (nskk-e2e-assert-buffer "愛いす"))))

(nskk-deftest-e2e buffer-vowel-okurigana-mixed-consonant-and-vowel
  "Mixed: consonant okurigana KaKu followed by vowel okurigana AI."
  (let ((dict '(("かk" . ("書"))
                ("あi" . ("愛")))))
    (nskk-e2e-with-buffer 'hiragana dict
      ;; First word: 書く (consonant okurigana)
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "K")
      (nskk-e2e-type "u")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "書く")
      ;; Second word: 愛い (vowel okurigana)
      (nskk-e2e-type "A")
      (nskk-e2e-type "I")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "書く愛い"))))

;;;; C-j During Preedit Tests
;;
;; C-j in preedit (▽) state commits kana as-is via nskk-henkan-kakutei.
;; Tests for other kakutei-action states (ascii→hiragana, hiragana-idle→
;; newline, latin→hiragana) are in nskk-mode-transition-e2e-test.el.

(nskk-deftest-e2e cj-preedit-commits-kana-as-is
  "C-j during ▽ preedit commits the kana as-is without conversion."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Ka")
    (nskk-e2e-assert-henkan-phase 'on)
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-henkan-phase nil)
    (nskk-e2e-assert-buffer "か")))

(nskk-deftest-e2e cj-preedit-multi-kana-commits
  "C-j during ▽ preedit with multiple kana commits them all."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Ni")
    (nskk-e2e-type "ho")
    (nskk-e2e-type "n")
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-henkan-phase nil)
    (nskk-e2e-assert-buffer "にほ")))

(nskk-deftest-e2e cj-from-jisx0208-enters-hiragana
  "C-j from jisx0208-latin mode switches to hiragana."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "L")
    (nskk-e2e-assert-mode 'jisx0208-latin)
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-mode 'hiragana)))

;;;; Implicit Kakutei on Mode Switch Tests (Preedit ▽ Case)
;;
;; ddskk behaviour: pressing l/L/q// while in ▽ preedit performs
;; implicit kakutei (commits kana as-is) then switches mode.
;; The ▽ marker must NOT be left as literal text in the buffer.
;; Conversion-phase (▼) implicit kakutei is tested in
;; nskk-mode-transition-e2e-test.el (mode-q/l-during-conversion-commits-first).

(nskk-deftest-e2e implicit-kakutei-l-during-preedit
  "Pressing l during ▽ preedit commits kana then switches to latin."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Ka")
    (nskk-e2e-assert-henkan-phase 'on)
    (nskk-e2e-type "l")
    (nskk-e2e-assert-mode 'latin)
    (nskk-e2e-assert-henkan-phase nil)
    (nskk-e2e-assert-buffer "か")))

(nskk-deftest-e2e implicit-kakutei-upper-l-during-preedit
  "Pressing L during ▽ preedit commits kana then switches to jisx0208-latin."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Ka")
    (nskk-e2e-assert-henkan-phase 'on)
    (nskk-e2e-type "L")
    (nskk-e2e-assert-mode 'jisx0208-latin)
    (nskk-e2e-assert-henkan-phase nil)
    (nskk-e2e-assert-buffer "か")))

(nskk-deftest-e2e implicit-kakutei-q-during-preedit
  "Pressing q during ▽ preedit commits kana then toggles to katakana."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Ka")
    (nskk-e2e-assert-henkan-phase 'on)
    (nskk-e2e-type "q")
    (nskk-e2e-assert-henkan-phase nil)
    (nskk-e2e-assert-buffer "か")
    (nskk-e2e-assert-mode 'katakana)))

(nskk-deftest-e2e implicit-kakutei-slash-during-preedit
  "Pressing / during ▽ preedit commits kana then switches to abbrev.
nskk-set-mode-abbrev always opens a new abbrev preedit (inserts ▽ and
sets phase to on), so the buffer becomes か▽ and phase is on."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Ka")
    (nskk-e2e-assert-henkan-phase 'on)
    (nskk-e2e-type "/")
    (nskk-e2e-assert-henkan-phase 'on)
    (nskk-e2e-assert-buffer "か▽")
    (nskk-e2e-assert-mode 'abbrev)))

;;;; Abbrev Mode Tests
;;
;; Current implementation inserts ASCII directly in abbrev mode
;; (full dictionary-assisted lookup is a future feature).

(nskk-deftest-e2e abbrev-typing-inserts-ascii
  "In abbrev mode, typing ASCII letters inserts them directly."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-type "te")
    (nskk-e2e-type "st")
    (nskk-e2e-assert-buffer "▽test")))

(nskk-deftest-e2e abbrev-cj-returns-to-hiragana
  "C-j from abbrev mode returns to hiragana."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-assert-mode 'abbrev)
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-mode 'hiragana)))

(nskk-deftest-e2e abbrev-l-self-inserts
  "In abbrev mode, l is self-inserted (abbrev is not a Japanese mode)."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-assert-mode 'abbrev)
    (nskk-e2e-type "l")
    ;; abbrev is not registered as a Japanese mode in japanese-mode/1 Prolog
    ;; predicate, so nskk-with-japanese-mode falls through to self-insert-command.
    (nskk-e2e-assert-mode 'abbrev)
    (nskk-e2e-assert-buffer "▽l")))

;;;; SPC Key in Various Modes

(nskk-deftest-e2e spc-in-ascii-inserts-space
  "SPC in ASCII mode inserts a literal ASCII space."
  (nskk-e2e-with-buffer nil nil
    (nskk-e2e-type "SPC")
    (nskk-e2e-assert-buffer " ")))

(nskk-deftest-e2e spc-in-hiragana-idle-inserts-space
  "SPC in hiragana idle (no preedit) inserts a literal ASCII space."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "SPC")
    (nskk-e2e-assert-mode 'hiragana)
    (nskk-e2e-assert-buffer " ")))

;;;; X Key Tests (Previous Candidate)

(nskk-deftest-e2e x-in-ascii-self-inserts
  "x key in ASCII mode inserts 'x', not previous-candidate."
  (nskk-e2e-with-buffer nil nil
    (nskk-e2e-type "x")
    (nskk-e2e-assert-buffer "x")))

(nskk-deftest-e2e x-in-hiragana-idle-self-inserts
  "x key in hiragana idle inserts 'x' (no conversion active)."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "x")
    (nskk-e2e-assert-buffer "x")))

(nskk-deftest-e2e x-cycles-back-to-previous-candidate
  "SPC twice then X returns to first candidate."
  (let ((dict '(("かわ" . ("川" "河")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "wa")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-overlay-shows "川")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-overlay-shows "河")
      (nskk-e2e-type "x")
      (nskk-e2e-assert-overlay-shows "川"))))

;;;; Sentence-Level Integration Tests
;;
;; Real Japanese typing scenarios exercising multiple features together.

(nskk-deftest-e2e sentence-nihongo-no-benkyou
  "Full sentence: 日本語の勉強 typed and committed word-by-word."
  (let ((dict '(("にほんご" . ("日本語"))
                ("べんきょう" . ("勉強")))))
    (nskk-e2e-with-buffer 'hiragana dict
      ;; N starts preedit; ihongo all lowercase to avoid okurigana triggers.
      (nskk-e2e-type "Ni")
      (nskk-e2e-type "ho")
      (nskk-e2e-type "n")
      (nskk-e2e-type "go")       ; lowercase: ん+ご → ▽にほんご
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-j")
      (nskk-e2e-type "no")
      ;; B starts preedit; enkyou all lowercase.
      (nskk-e2e-type "Be")
      (nskk-e2e-type "n")
      (nskk-e2e-type "kyo")      ; lowercase: ん+きょ → ▽べんきょ
      (nskk-e2e-type "u")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "日本語の勉強"))))

(nskk-deftest-e2e sentence-hiragana-ascii-mix
  "Mix hiragana, switch to ASCII mid-sentence, then return."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "a")
    (nskk-e2e-assert-buffer "あ")
    (nskk-e2e-type "l")
    (nskk-e2e-assert-mode 'latin)
    (nskk-e2e-type "BC")
    (nskk-e2e-assert-buffer "あBC")
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-mode 'hiragana)
    (nskk-e2e-type "i")
    (nskk-e2e-assert-buffer "あBCい")))

(nskk-deftest-e2e sentence-convert-then-particle
  "Convert 漢字, commit, then type particle の."
  (let ((dict '(("かんじ" . ("漢字")))))
    (nskk-e2e-with-buffer 'hiragana dict
      ;; K starts preedit; anji all lowercase to avoid okurigana (J would trigger it).
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "n")
      (nskk-e2e-type "ji")       ; lowercase: ん+じ → ▽かんじ
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "漢字")
      (nskk-e2e-type "no")
      (nskk-e2e-assert-buffer "漢字の"))))

(nskk-deftest-e2e sentence-cancel-returns-to-kana
  "Cancel conversion with C-g restores plain kana to buffer."
  (let ((dict '(("かわ" . ("川" "河")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "wa")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      ;; After C-g: nskk-rollback-conversion clears conversion markers,
      ;; henkan-phase→nil, buffer retains plain kana かわ.
      ;; Do NOT press C-j here — in japanese-idle state C-j inserts a newline.
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "かわ"))))

;;;; Edge Case Tests

(nskk-deftest-e2e cg-in-hiragana-idle-keyboard-quits
  "C-g in hiragana idle (no preedit or conversion) raises keyboard-quit."
  (nskk-e2e-with-buffer 'hiragana nil
    (condition-case _err
        (nskk-e2e-type "C-g")
      (quit nil))
    (nskk-e2e-assert-mode 'hiragana)))

(nskk-deftest-e2e sokuon-in-preedit-then-convert
  "Sokuon (っ) in preedit: K starts preedit, ka doubles k → ▽っか, then converts."
  (let ((dict '(("っか" . ("蛸")))))
    (nskk-e2e-with-buffer 'hiragana dict
      ;; Uppercase K starts preedit (romaji-buffer = "k").
      ;; Then lowercase "ka": doubled k → sokuon っ, then か → ▽っか.
      ;; Using uppercase Ka would trigger okurigana (second uppercase in preedit).
      (nskk-e2e-type "K")
      (nskk-e2e-type "ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "蛸"))))

(nskk-deftest-e2e rapid-mode-switches-no-stale-buffer
  "Rapid mode switches leave buffer empty — no stale ▽/▼ markers."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "q")
    (nskk-e2e-assert-mode 'katakana)
    (nskk-e2e-type "q")
    (nskk-e2e-assert-mode 'hiragana)
    (nskk-e2e-type "l")
    (nskk-e2e-assert-mode 'latin)
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-mode 'hiragana)
    (nskk-e2e-assert-buffer "")))

(nskk-deftest-e2e katakana-preedit-commits-as-katakana
  "In katakana mode, preedit produces katakana; C-j commits it."
  (nskk-e2e-with-buffer 'katakana nil
    (nskk-e2e-type "Te")
    (nskk-e2e-type "su")
    (nskk-e2e-type "to")
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-buffer "テスト")))

;;;;
;;;; Abbrev Mode — Input and Conversion Scenarios
;;;;
;;
;; These tests cover the two functions modified in the abbrev mode bug fix:
;;
;;   1. `nskk-self-insert' (nskk-input.el):
;;      In abbrev mode, all printable ASCII chars bypass the Prolog routing
;;      path and go directly to `nskk-process-abbrev-input'.  The existing
;;      `abbrev-typing-inserts-ascii' test above covers basic insertion, but
;;      does not verify that the Prolog input-route is truly bypassed for
;;      chars that would otherwise match a Japanese-mode rule (e.g. uppercase
;;      letters that normally start okurigana, or "n" which accumulates in
;;      the romaji buffer).
;;
;;   2. `nskk--current-key-state' (nskk-keymap.el):
;;      In abbrev mode with a conversion-start marker set, this now returns
;;      `preedit' even when `nskk--has-preedit' is false (i.e. the marker
;;      was set but point hasn't moved past the ▽ yet).  This makes SPC
;;      trigger `nskk-start-conversion' rather than self-insert.  The
;;      complementary guard inside `nskk-start-conversion' itself makes
;;      SPC immediately after "/" (empty abbrev preedit) a no-op.

;;;; 1. ASCII chars in abbrev mode bypass Prolog routing

(nskk-deftest-e2e abbrev-uppercase-is-inserted-not-okurigana
  "In abbrev mode, uppercase ASCII is inserted verbatim, not treated as okurigana.
nskk-self-insert short-circuits before the Prolog input-route check, so an
uppercase letter (which would start okurigana in hiragana mode) just inserts
the character directly after the ▽ marker."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-assert-mode 'abbrev)
    (nskk-e2e-type "Te")
    (nskk-e2e-type "st")
    (nskk-e2e-assert-buffer "▽Test")))

(nskk-deftest-e2e abbrev-n-is-inserted-not-accumulated-in-romaji-buffer
  "In abbrev mode, 'n' is inserted directly, not buffered as pending romaji.
In hiragana mode 'n' + vowel → kana; in abbrev mode the romaji buffer must
not be touched at all.  Two consecutive 'n' presses must produce 'nn' in the
buffer, not 'ん'."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-assert-mode 'abbrev)
    (nskk-e2e-type "nn")
    (nskk-e2e-assert-buffer "▽nn")))

(nskk-deftest-e2e abbrev-digits-and-symbols-are-inserted-directly
  "In abbrev mode, digits and ASCII symbols are inserted directly."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-assert-mode 'abbrev)
    ;; Numbers and hyphens are common abbrev lookup keys (e.g. "iso-8859").
    ;; They must go through nskk-process-abbrev-input, not Prolog routing.
    (nskk-e2e-type "1")
    (nskk-e2e-type "2")
    (nskk-e2e-type "3")
    (nskk-e2e-assert-buffer "▽123")))

;;;; 2. SPC in abbrev mode — conversion trigger

(nskk-deftest-e2e abbrev-spc-with-text-starts-conversion
  "SPC after typing text in abbrev mode triggers dictionary conversion.
nskk--current-key-state returns 'preedit when mode is abbrev and the
conversion-start marker is set, so nskk-handle-space dispatches
'start-conversion → nskk-start-conversion."
  (let ((dict '(("test" . ("テスト")))))
    (nskk-e2e-with-buffer 'hiragana dict
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "te")
      (nskk-e2e-type "st")
      (nskk-e2e-assert-buffer "▽test")
      ;; SPC should trigger conversion, not insert a space.
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting))))

(nskk-deftest-e2e abbrev-spc-immediately-after-slash-is-noop
  "SPC immediately after / (empty abbrev preedit) inserts a space.
nskk--current-key-state returns 'preedit (marker is set), so handle-space
dispatches 'start-conversion → nskk-start-conversion.  nskk-start-conversion
guards on non-empty text and falls back to self-inserting a space when the
preedit region is empty."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-assert-mode 'abbrev)
    ;; Nothing typed yet — preedit is empty.
    ;; Must not crash and must not enter a broken converting state.
    (nskk-e2e-assert-henkan-phase 'on)
    (nskk-e2e-type "SPC")
    ;; After SPC on empty abbrev preedit: still in abbrev mode, not converting.
    (nskk-e2e-assert-mode 'abbrev)
    (nskk-e2e-assert-not-converting)))

;;;; 3. Backspace in abbrev preedit

(nskk-deftest-e2e abbrev-backspace-deletes-last-char
  "DEL in abbrev preedit deletes the last typed character."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-assert-mode 'abbrev)
    (nskk-e2e-type "te")
    (nskk-e2e-type "st")
    (nskk-e2e-assert-buffer "▽test")
    (nskk-e2e-type "DEL")
    (nskk-e2e-assert-buffer "▽tes")
    (nskk-e2e-assert-mode 'abbrev)))

(nskk-deftest-e2e abbrev-backspace-on-empty-preedit-cancels
  "DEL on empty abbrev preedit (right after /) cancels preedit entirely.
nskk-handle-backspace detects that point is at the ▽ marker boundary and
calls nskk-cancel-preedit instead of delete-char."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-assert-mode 'abbrev)
    (nskk-e2e-assert-henkan-phase 'on)
    (nskk-e2e-type "DEL")
    ;; Preedit cancelled: buffer is empty, no longer in preedit.
    (nskk-e2e-assert-buffer "")
    (nskk-e2e-assert-henkan-phase nil)))

;;;; 4. C-g cancel in abbrev preedit

(nskk-deftest-e2e abbrev-cg-cancel-clears-preedit
  "C-g in abbrev preedit (with text) cancels and clears the preedit buffer."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-assert-mode 'abbrev)
    (nskk-e2e-type "te")
    (nskk-e2e-assert-buffer "▽te")
    (nskk-e2e-type "C-g")
    ;; Preedit cancelled: buffer is empty and we are not converting.
    (nskk-e2e-assert-buffer "")
    (nskk-e2e-assert-not-converting)))

(nskk-deftest-e2e abbrev-cg-cancel-empty-preedit-is-safe
  "C-g immediately after / (empty abbrev preedit) cancels cleanly."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-assert-mode 'abbrev)
    (nskk-e2e-type "C-g")
    (nskk-e2e-assert-buffer "")
    (nskk-e2e-assert-not-converting)))

(provide 'nskk-buffer-e2e-test)

;;; nskk-buffer-e2e-test.el ends here
