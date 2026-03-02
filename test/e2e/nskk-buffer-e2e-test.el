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

(nskk-describe "hiragana ka-row"
  (nskk-it "converts ka ki ku ke ko"
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
      (nskk-e2e-assert-buffer "こ"))))

;;;;
;;;; Complete Romaji Table Tests (ddskk-equivalent)
;;;;

(nskk-describe "romaji table comprehensive"
  (nskk-it "converts all romaji rows to hiragana"
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
                                  (format "romaji %S → %S failed" (car tc) (cdr tc))))))))

;;;;
;;;; Compound Kana (拗音) Tests
;;;;

(nskk-describe "compound kana romaji"
  (nskk-it "converts all compound kana rows"
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
                                          (car tc) (cdr tc))))))))

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

  (nskk-it "converts sokuon before various consonants"
    (let ((cases '(("ppa" . "っぱ")
                   ("bba" . "っば")
                   ("dda" . "っだ")
                   ("gga" . "っが"))))
      (dolist (tc cases)
        (nskk-e2e-with-buffer 'hiragana nil
          (nskk-e2e-type (car tc))
          (nskk-e2e-assert-buffer (cdr tc)
                                  (format "sokuon %S → %S failed" (car tc) (cdr tc))))))))

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
  (nskk-it "converts vowels to katakana"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type "aiueo")
      (nskk-e2e-assert-buffer "アイウエオ")))

  (nskk-it "converts consonant+vowel to katakana"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type "ka")
      (nskk-e2e-assert-buffer "カ"))
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type "shi")
      (nskk-e2e-assert-buffer "シ")))

  (nskk-it "converts sokuon to ッ"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type "tte")
      (nskk-e2e-assert-buffer "ッテ")))

  (nskk-it "converts representative kana table entries"
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
                                          (car tc) (cdr tc))))))))

;;;;
;;;; Kanji Conversion Flow Tests
;;;;

(nskk-describe "conversion flow"
  (nskk-it "commits via C-j with no newline"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'active)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "漢字")))

  (nskk-it "commits via RET with no newline"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-buffer "漢字")))

  (nskk-it "advances to second candidate with SPC"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "感じ")))

  (nskk-it "returns to previous candidate with x"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "x")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "漢字")))

  (nskk-it "advances to next candidate with C-n"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-n")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "感じ")))

  (nskk-it "returns to previous candidate with C-p"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-n")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-p")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "漢字")))

  (nskk-it "does not enter converting when C-n is used outside conversion"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "a")
      (nskk-e2e-type "C-n")
      (nskk-e2e-assert-not-converting)))

  (nskk-it "cancels active conversion with C-g"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-not-converting)))

  (nskk-it "cancels preedit phase with C-g"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "converts single kana reading へんかん to 変換"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Henkan")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "変換")))

  (nskk-it "converts にほん to 日本"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Nihon")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "日本"))))

;;;;
;;;; Full-width Latin Mode Tests
;;;;

(nskk-describe "jisx0208-latin mode"
  (nskk-it "converts ASCII chars to full-width"
    (nskk-e2e-with-buffer 'jisx0208-latin nil
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "\uFF41")))

  (nskk-it "converts SPC to ideographic space"
    (nskk-e2e-with-buffer 'jisx0208-latin nil
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-buffer "\u3000"))))

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

(nskk-describe "okurigana input"
  (nskk-it "enters preedit phase on Ka"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "sets okurigana consonant state on KaK"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "K")
      (should (eq (nskk-state-get-okurigana nskk-current-state) ?k))
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-not-converting)))

  (nskk-it "triggers conversion on KaKu showing 書"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "commits KaKu to 書く"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く"))))

  (nskk-it "commits MiRu to 見る"
    (let ((dict '(("みr" . ("見")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Mi")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "見る"))))

  (nskk-it "commits OkuRu to 送る"
    (let ((dict '(("おくr" . ("送")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Oku")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "送る"))))

  (nskk-it "commits KiKu to 聞く"
    (let ((dict '(("きk" . ("聞")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ki")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "聞く"))))

  (nskk-it "selects second candidate 効く from KiKu"
    (let ((dict '(("きk" . ("聞" "効")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ki")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "聞")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "効")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "効く"))))

  (nskk-it "cancels KaKu conversion with C-g"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "continues typing after committing 書く"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く")
        (nskk-e2e-type "no")
        (nskk-e2e-assert-buffer "書くの"))))

  (nskk-it "commits DekiRu to 出来る"
    (let ((dict '(("できr" . ("出来")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Deki")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "出来る"))))

  (nskk-it "commits HabikoRu to 蔓延る"
    (let ((dict '(("はびこr" . ("蔓延")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Habiko")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "蔓延る")))))

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

(nskk-describe "vowel okurigana"
  (nskk-it "converts AI to 愛い and commits"
    (let ((dict '(("あi" . ("愛" "哀")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "愛い"))))

  (nskk-it "does not produce double marker on AII"
    (let ((dict '(("あi" . ("愛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "愛"))))

  (nskk-it "converts OU to 負う and commits"
    (let ((dict '(("おu" . ("負")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "O")
        (nskk-e2e-type "U")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "負う"))))

  (nskk-it "converts aE to 与え and commits"
    (let ((dict '(("あe" . ("与")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "E")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "与え"))))

  (nskk-it "converts AU to 買う and commits"
    ;; Vowel okurigana with U: reading "あ" + okurigana vowel "u".
    ;; A starts preedit (reading = "あ"), U triggers vowel okurigana "u".
    ;; Dict key "あu" (hiragana reading + vowel char).  Okurigana kana = "う".
    (let ((dict '(("あu" . ("買")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "U")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "買う"))))

  (nskk-it "cancels AI conversion with C-g without stale state"
    (let ((dict '(("あi" . ("愛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "continues typing after committing AI"
    (let ((dict '(("あi" . ("愛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "愛い")
        (nskk-e2e-type "su")
        (nskk-e2e-assert-buffer "愛いす"))))

  (nskk-it "handles consonant okurigana KaKu followed by vowel okurigana AI"
    (let ((dict '(("かk" . ("書"))
                  ("あi" . ("愛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く")
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く愛い")))))

;;;; Regression Tests: Pending Romaji Discard on Okurigana Trigger
;;
;; Bug (fixed in nskk-henkan.el lines 733-754): when a pending incomplete
;; romaji consonant (e.g. "k", "sh") was in nskk--romaji-buffer at the moment
;; an okurigana trigger (uppercase letter) arrived, the raw consonant was
;; inserted into the buffer before the * okurigana marker, producing
;; e.g. "▽かk*" instead of "▽か*".
;;
;; The fix discards :incomplete romaji results (consonants like "k", "sh") and
;; only emits fully-converted kana or the standalone "n" → "ん" exception.
;;
;; Input sequence for T-E1 (KAkKu):
;;   K   → starts henkan (▽), romaji buffer = ""
;;   A   → romaji "a" completes → "あ" appended to preedit (▽あ), romaji buffer = ""
;;   k   → romaji buffer = "k" (pending incomplete consonant)
;;   K   → okurigana trigger: "k" must be DISCARDED (not inserted before *),
;;          * marker inserted, romaji buffer = "k" (for new okurigana consonant)
;;   u   → romaji "ku" → "く", triggers conversion (▼ state)
;;
;; Input sequence for T-E2 (KAnKu):
;;   K   → starts henkan (▽), romaji buffer = ""
;;   A   → romaji "a" → "あ" in preedit, romaji buffer = ""
;;   n   → romaji buffer = "n" (pending n — special case: converts to ん)
;;   K   → okurigana trigger: "n" must be FLUSHED as "ん" before *, then * inserted
;;   u   → romaji "ku" → "く", triggers conversion (▼ state)

(nskk-describe "okurigana pending romaji discard regression"
  (nskk-it "KAkKu: pending k is discarded before okurigana marker, conversion succeeds"
    ;; T-E1: the lowercase k pending in the romaji buffer when uppercase K fires
    ;; must NOT appear before * in the buffer.  After completing ku → く, the
    ;; conversion should trigger normally (entering active/converting state).
    (let ((dict '(("あk" . ("開")))))
      (nskk-e2e-with-buffer 'hiragana dict
        ;; K starts preedit
        (nskk-e2e-type "K")
        ;; A completes romaji "a" → あ, preedit is now ▽あ
        (nskk-e2e-type "A")
        ;; Lowercase k → pending incomplete consonant in romaji buffer
        (nskk-e2e-type "k")
        ;; Uppercase K fires okurigana trigger while "k" is pending.
        ;; BUG: used to insert raw "k" before *, giving ▽あk*.
        ;; FIX: "k" is discarded (incomplete), giving ▽あ*.
        (nskk-e2e-type "K")
        ;; The buffer content must NOT contain "k" before the * marker.
        ;; After the okurigana trigger the pending-romaji display may briefly
        ;; show "k" for the NEW okurigana consonant, but the buffer proper
        ;; should have no "k" ASCII character adjacent to *.
        (let ((content (buffer-string)))
          (should-not (string-match-p "k\\*" content)))
        ;; Now type u to complete the okurigana ku → く and trigger conversion.
        (nskk-e2e-type "u")
        ;; We should now be in converting (▼) state — conversion was triggered.
        (nskk-e2e-assert-converting)
        ;; The buffer string must not contain a double "k" or "kk" artifact.
        (should-not (string-match-p "kk" (buffer-string))))))

  (nskk-it "KAnKu: pending n is flushed as ん before okurigana marker"
    ;; T-E2: the lowercase n pending when uppercase K fires must be converted
    ;; to ん (the word-boundary n exception) and inserted before *.
    (let ((dict '(("あんk" . ("暗")))))
      (nskk-e2e-with-buffer 'hiragana dict
        ;; K starts preedit
        (nskk-e2e-type "K")
        ;; A completes romaji "a" → あ
        (nskk-e2e-type "A")
        ;; Lowercase n → pending in romaji buffer (not yet ん)
        (nskk-e2e-type "n")
        ;; Uppercase K fires okurigana trigger while "n" is pending.
        ;; The n exception: standalone "n" at word boundary → ん, inserted before *.
        (nskk-e2e-type "K")
        ;; ん must appear in the buffer content (flushed before * marker)
        (let ((content (buffer-string)))
          (should (string-match-p "\u3093" content)))
        ;; Complete the okurigana ku → く and trigger conversion
        (nskk-e2e-type "u")
        ;; Conversion should trigger normally
        (nskk-e2e-assert-converting)))))

;;;; C-j During Preedit Tests
;;
;; C-j in preedit (▽) state commits kana as-is via nskk-henkan-kakutei.
;; Tests for other kakutei-action states (ascii→hiragana, hiragana-idle→
;; newline, latin→hiragana) are in nskk-mode-transition-e2e-test.el.

(nskk-describe "C-j kakutei from preedit"
  (nskk-it "commits kana as-is during preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "か")))

  (nskk-it "commits multiple kana during preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ni")
      (nskk-e2e-type "ho")
      (nskk-e2e-type "n")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "にほ")))

  (nskk-it "switches to hiragana from jisx0208-latin"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "L")
      (nskk-e2e-assert-mode 'jisx0208-latin)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana))))

;;;; Implicit Kakutei on Mode Switch Tests (Preedit ▽ Case)
;;
;; ddskk behaviour: pressing l/L/q// while in ▽ preedit performs
;; implicit kakutei (commits kana as-is) then switches mode.
;; The ▽ marker must NOT be left as literal text in the buffer.
;; Conversion-phase (▼) implicit kakutei is tested in
;; nskk-mode-transition-e2e-test.el (mode-q/l-during-conversion-commits-first).

(nskk-describe "implicit kakutei on mode switch during preedit"
  (nskk-it "l during preedit commits kana then switches to latin"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "l")
      (nskk-e2e-assert-mode 'latin)
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "か")))

  (nskk-it "L during preedit commits kana then switches to jisx0208-latin"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "L")
      (nskk-e2e-assert-mode 'jisx0208-latin)
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "か")))

  (nskk-it "q during preedit commits kana then toggles to katakana"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "q")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "か")
      (nskk-e2e-assert-mode 'katakana)))

  (nskk-it "/ during preedit commits kana then switches to abbrev"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "/")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer "か▽")
      (nskk-e2e-assert-mode 'abbrev))))

;;;; Abbrev Mode Tests
;;
;; Current implementation inserts ASCII directly in abbrev mode
;; (full dictionary-assisted lookup is a future feature).

(nskk-describe "abbrev mode basic behavior"
  (nskk-it "inserts ASCII letters directly in abbrev mode"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-type "te")
      (nskk-e2e-type "st")
      (nskk-e2e-assert-buffer "▽test")))

  (nskk-it "returns to hiragana from abbrev via C-j"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "self-inserts l in abbrev mode"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "l")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-buffer "▽l"))))

;;;; SPC Key in Various Modes

(nskk-describe "SPC key dispatch"
  (nskk-it "inserts literal space in ASCII mode"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-buffer " ")))

  (nskk-it "inserts literal space in hiragana idle"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer " "))))

;;;; X Key Tests (Previous Candidate)

(nskk-describe "x key dispatch"
  (nskk-it "self-inserts x in ASCII mode"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "x")
      (nskk-e2e-assert-buffer "x")))

  (nskk-it "x in hiragana idle accumulates in romaji buffer"
    ;; x is a romaji prefix for small kana (xa→ぁ, xi→ぃ etc.).
    ;; After pressing x alone, the romaji buffer holds the pending x
    ;; and nothing is inserted into the display buffer yet.
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "x")
      (nskk-e2e-assert-buffer "")))

  (nskk-it "cycles back to first candidate after SPC twice then X"
    (let ((dict '(("かわ" . ("川" "河")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "wa")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "川")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "河")
        (nskk-e2e-type "x")
        (nskk-e2e-assert-overlay-shows "川")))))

;;;; Sentence-Level Integration Tests
;;
;; Real Japanese typing scenarios exercising multiple features together.

(nskk-describe "sentence-level integration"
  (nskk-it "types 日本語の勉強 word by word"
    (let ((dict '(("にほんご" . ("日本語"))
                  ("べんきょう" . ("勉強")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ni")
        (nskk-e2e-type "ho")
        (nskk-e2e-type "n")
        (nskk-e2e-type "go")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "C-j")
        (nskk-e2e-type "no")
        (nskk-e2e-type "Be")
        (nskk-e2e-type "n")
        (nskk-e2e-type "kyo")
        (nskk-e2e-type "u")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "日本語の勉強"))))

  (nskk-it "mixes hiragana and ASCII mid-sentence"
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

  (nskk-it "converts kanji then types particle の"
    (let ((dict '(("かんじ" . ("漢字")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "n")
        (nskk-e2e-type "ji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "漢字")
        (nskk-e2e-type "no")
        (nskk-e2e-assert-buffer "漢字の"))))

  (nskk-it "restores plain kana after C-g cancel"
    (let ((dict '(("かわ" . ("川" "河")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "wa")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase nil)
        (nskk-e2e-assert-buffer "かわ")))))

;;;; Edge Case Tests

(nskk-describe "edge cases"
  (nskk-it "C-g in hiragana idle raises keyboard-quit"
    (nskk-e2e-with-buffer 'hiragana nil
      (condition-case _err
          (nskk-e2e-type "C-g")
        (quit nil))
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "sokuon in preedit then converts"
    (let ((dict '(("っか" . ("蛸")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "ka")
        (nskk-e2e-assert-henkan-phase 'on)
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "蛸"))))

  (nskk-it "rapid mode switches leave buffer empty"
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

  (nskk-it "katakana preedit commits as katakana via C-j"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type "Te")
      (nskk-e2e-type "su")
      (nskk-e2e-type "to")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "テスト"))))

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

(nskk-describe "abbrev mode ASCII bypass"
  (nskk-it "inserts uppercase verbatim without triggering okurigana"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "Te")
      (nskk-e2e-type "st")
      (nskk-e2e-assert-buffer "▽Test")))

  (nskk-it "inserts n directly without accumulating in romaji buffer"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "nn")
      (nskk-e2e-assert-buffer "▽nn")))

  (nskk-it "inserts digits and symbols directly"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "1")
      (nskk-e2e-type "2")
      (nskk-e2e-type "3")
      (nskk-e2e-assert-buffer "▽123"))))

;;;; 2. SPC in abbrev mode — conversion trigger

(nskk-describe "abbrev mode SPC conversion"
  (nskk-it "triggers dictionary conversion after typing text"
    (let ((dict '(("test" . ("テスト")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "/")
        (nskk-e2e-assert-mode 'abbrev)
        (nskk-e2e-type "te")
        (nskk-e2e-type "st")
        (nskk-e2e-assert-buffer "▽test")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting))))

  (nskk-it "inserts space when preedit is empty immediately after /"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-not-converting))))

;;;; 3. Backspace in abbrev preedit

(nskk-describe "abbrev mode backspace"
  (nskk-it "deletes last character in abbrev preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "te")
      (nskk-e2e-type "st")
      (nskk-e2e-assert-buffer "▽test")
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽tes")
      (nskk-e2e-assert-mode 'abbrev)))

  (nskk-it "cancels preedit entirely on DEL at empty abbrev preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-assert-henkan-phase nil))))

;;;; 4. C-g cancel in abbrev preedit

(nskk-describe "abbrev mode C-g cancel"
  (nskk-it "cancels and clears preedit buffer with text"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "te")
      (nskk-e2e-assert-buffer "▽te")
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-assert-not-converting)))

  (nskk-it "cancels cleanly on empty abbrev preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-assert-not-converting))))

;;;; 5. q and L in abbrev preedit self-insert

(nskk-describe "abbrev mode self-insert for mode-switch keys"
  ;; q, l, L, and / are bound to nskk-handle-q / nskk-handle-l / nskk-handle-upper-l /
  ;; nskk-handle-slash in the mode map.  Each uses nskk-with-japanese-mode which
  ;; checks japanese-mode/1; abbrev is NOT a Japanese mode, so the macro falls
  ;; through to (self-insert-command 1).  The character therefore lands in the
  ;; buffer verbatim via Emacs's built-in self-insert, bypassing nskk-self-insert.
  (nskk-it "q in abbrev preedit self-inserts q into preedit text"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "q")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-buffer "▽q")))

  (nskk-it "L in abbrev preedit self-inserts L into preedit text"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "L")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-buffer "▽L"))))

;;;; 6. C-j from abbrev idle (no preedit) returns to hiragana

(nskk-describe "abbrev mode C-j from idle"
  ;; When abbrev mode is entered directly (no preedit marker set),
  ;; the kakutei state is direct-idle.  kakutei-action maps direct-idle →
  ;; enter-hiragana, so C-j switches to hiragana and leaves an empty buffer.
  (nskk-it "C-j from abbrev idle returns to hiragana"
    (nskk-e2e-with-buffer 'abbrev nil
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "buffer is empty after C-j from abbrev idle"
    (nskk-e2e-with-buffer 'abbrev nil
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer ""))))

;;;; 7. Abbrev mode conversion via RET and SPC cycling

(nskk-describe "abbrev mode conversion via RET"
  ;; / + "test" + SPC triggers dictionary conversion.
  ;; RET (commit-candidate) commits the first candidate without a newline.
  (nskk-it "commits first candidate with RET after SPC conversion"
    (let ((dict '(("test" . ("テスト" "Test")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "/")
        (nskk-e2e-type "test")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "RET")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "テスト"))))

  (nskk-it "cycles to second candidate with SPC then commits with C-j"
    (let ((dict '(("test" . ("テスト" "Test")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "/")
        (nskk-e2e-type "test")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "Test")))))

;;;; 8. C-g during abbrev conversion cancels back to preedit

(nskk-describe "abbrev mode C-g during conversion"
  ;; After / + "test" + SPC enters conversion (▼), C-g should cancel conversion
  ;; and restore the preedit reading text to the buffer.
  (nskk-it "cancels conversion and restores reading text"
    (let ((dict '(("test" . ("テスト")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "/")
        (nskk-e2e-type "test")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-not-converting)))))

;;;;
;;;; Katakana Mode Okurigana (カタカナ送り仮名) E2E Tests
;;;;
;;
;; In katakana mode the preedit reading accumulates as KATAKANA because
;; nskk-input.el applies nskk-kana-string-hiragana-to-katakana before inserting.
;; Dict lookup keys therefore use katakana + consonant (e.g. "カk", not "あk").
;; After commit the okurigana kana appended is also katakana (ク, イ, ン …).
;;
;; Input sequences follow DDSKK katakana mode conventions:
;;   Ka → preedit reading "カ" (romaji ka → か → katakana → カ)
;;   K  → okurigana trigger; pending romaji "k" registered as okuri char

(nskk-describe "katakana mode okurigana triggers conversion"
  (nskk-it "triggers conversion on KaKu in katakana mode"
    ;; Ka: K starts preedit (uppercase = henkan-start), a completes "ca" → "か" → "カ".
    ;; K (second, uppercase): okurigana trigger; pending romaji "k" → dict key "カk".
    ;; u completes "ku" → "く" → "ク" (appended as okurigana kana).
    ;; Conversion is triggered with reading "カ", okuri char "k".
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "commits KaKu to 書ク in katakana mode"
    ;; Same sequence as above; C-j commits the first candidate.
    ;; Result: kanji 書 replaces the reading "カ", okurigana kana "ク" is appended.
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書ク"))))

  (nskk-it "commits vowel okurigana AI to 愛イ in katakana mode"
    ;; A (uppercase in katakana mode): starts preedit with reading "ア" (vowel henkan-start).
    ;; I (uppercase): okurigana trigger for vowel "i"; okurigana kana "イ" appended.
    ;; Dict key "アi" (katakana reading + vowel char).
    (let ((dict '(("アi" . ("愛")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "愛イ"))))

  (nskk-it "discards pending consonant before okurigana marker in katakana mode (T-E1 analogue)"
    ;; T-E1 katakana analogue: K A k K u
    ;; K (uppercase, empty reading) + A (vowel okurigana with empty reading) →
    ;;   failed conversion (no dict entry for key "a"), cancel; buffer has "ア" reading.
    ;; k (lowercase): accumulates romaji; K (uppercase): okurigana trigger.
    ;; u completes "ku" → okurigana kana "ク"; conversion with dict key "アk".
    ;; Crucially, no stray "k" appears between "ア" and "*" in the preedit display.
    (let ((dict '(("アk" . ("開")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "A")
        (nskk-e2e-type "k")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting))))

  (nskk-it "flushes pending n as ン before okurigana marker in katakana mode (T-E2 analogue)"
    ;; T-E2 katakana analogue: K A n K u
    ;; K + A → reading "ア" (failed vowel okurigana → cancel, reading = "ア").
    ;; n (lowercase): romaji buffer = "n"; K (uppercase): pending "n" flushed as "ン",
    ;;   then okurigana trigger; dict key "アンk".  u → okurigana kana "ク".
    (let ((dict '(("アンk" . ("暗")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "A")
        (nskk-e2e-type "n")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)))))

;;;;
;;;; Sokuon in Okurigana (促音送り仮名) E2E Tests
;;;;
;;
;; Sokuon (っ) appears as the first character of okurigana kana in words like
;; 勝った (katta) and 打った (utta).
;;
;; Mechanism (two-phase):
;;   1. Uppercase T is the okurigana trigger; romaji buffer = "t".
;;   2. Input "ta": first 't' + buffered 't' → sokuon pattern → emits っ → fires
;;      conversion with the accumulated reading and okuri char "t".
;;   3. Remaining 'a' → romaji "ta" → emits "た" into the buffer after っ.
;;   4. C-j commits: kanji replaces the reading, "った" (っ + た) is appended.

(nskk-describe "sokuon in okurigana"
  (nskk-it "commits KaTTa sequence to 勝った"
    ;; Ka: reading = "か" (K starts preedit, a completes romaji).
    ;; T (uppercase): okurigana trigger; romaji buffer = "t".
    ;; t: romaji "t"+"t" → sokuon → emits っ → triggers conversion (key "かt").
    ;; a: romaji "ta" → emits "た" (appended after っ).
    ;; C-j: commits first candidate; buffer = kanji + "った".
    (let ((dict '(("かt" . ("勝")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "勝った"))))

  (nskk-it "commits UTTa sequence to 打った"
    ;; U (uppercase): vowel okurigana start; reading = "う".
    ;; T (uppercase): okurigana trigger; romaji buffer = "t".
    ;; t: romaji "t"+"t" → sokuon → emits っ → triggers conversion (key "うt").
    ;; a: romaji "ta" → emits "た".
    ;; C-j: commits; buffer = kanji + "った".
    (let ((dict '(("うt" . ("打")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "U")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "打った")))))

;;;;
;;;; Half-width Katakana (katakana-半角) Character Output Tests
;;;;

(nskk-deftest-table buffer-hankaku-katakana-vowels
  :columns (input expected)
  :rows (("a" "ｱ") ("i" "ｲ") ("u" "ｳ") ("e" "ｴ") ("o" "ｵ"))
  :body (nskk-e2e-with-buffer 'katakana-半角 nil
          (nskk-e2e-type input)
          (nskk-e2e-assert-buffer expected)))

(nskk-deftest-table buffer-hankaku-katakana-ka-row
  :columns (input expected)
  :rows (("ka" "ｶ") ("ki" "ｷ") ("ku" "ｸ") ("ke" "ｹ") ("ko" "ｺ"))
  :body (nskk-e2e-with-buffer 'katakana-半角 nil
          (nskk-e2e-type input)
          (nskk-e2e-assert-buffer expected)))

(nskk-deftest-table buffer-hankaku-katakana-sa-row
  :columns (input expected)
  :rows (("sa" "ｻ") ("shi" "ｼ") ("su" "ｽ") ("se" "ｾ") ("so" "ｿ"))
  :body (nskk-e2e-with-buffer 'katakana-半角 nil
          (nskk-e2e-type input)
          (nskk-e2e-assert-buffer expected)))

(nskk-deftest-table buffer-hankaku-katakana-ta-row
  :columns (input expected)
  :rows (("ta" "ﾀ") ("chi" "ﾁ") ("tsu" "ﾂ") ("te" "ﾃ") ("to" "ﾄ"))
  :body (nskk-e2e-with-buffer 'katakana-半角 nil
          (nskk-e2e-type input)
          (nskk-e2e-assert-buffer expected)))

(nskk-deftest-table buffer-hankaku-katakana-na-row
  :columns (input expected)
  :rows (("na" "ﾅ") ("ni" "ﾆ") ("nu" "ﾇ") ("ne" "ﾈ") ("no" "ﾉ"))
  :body (nskk-e2e-with-buffer 'katakana-半角 nil
          (nskk-e2e-type input)
          (nskk-e2e-assert-buffer expected)))

(nskk-deftest-table buffer-hankaku-katakana-ha-row
  :columns (input expected)
  :rows (("ha" "ﾊ") ("hi" "ﾋ") ("fu" "ﾌ") ("he" "ﾍ") ("ho" "ﾎ"))
  :body (nskk-e2e-with-buffer 'katakana-半角 nil
          (nskk-e2e-type input)
          (nskk-e2e-assert-buffer expected)))

(nskk-deftest-table buffer-hankaku-katakana-ma-row
  :columns (input expected)
  :rows (("ma" "ﾏ") ("mi" "ﾐ") ("mu" "ﾑ") ("me" "ﾒ") ("mo" "ﾓ"))
  :body (nskk-e2e-with-buffer 'katakana-半角 nil
          (nskk-e2e-type input)
          (nskk-e2e-assert-buffer expected)))

(nskk-deftest-table buffer-hankaku-katakana-ya-row
  :columns (input expected)
  :rows (("ya" "ﾔ") ("yu" "ﾕ") ("yo" "ﾖ"))
  :body (nskk-e2e-with-buffer 'katakana-半角 nil
          (nskk-e2e-type input)
          (nskk-e2e-assert-buffer expected)))

(nskk-deftest-table buffer-hankaku-katakana-ra-row
  :columns (input expected)
  :rows (("ra" "ﾗ") ("ri" "ﾘ") ("ru" "ﾙ") ("re" "ﾚ") ("ro" "ﾛ"))
  :body (nskk-e2e-with-buffer 'katakana-半角 nil
          (nskk-e2e-type input)
          (nskk-e2e-assert-buffer expected)))

(nskk-deftest-table buffer-hankaku-katakana-wa-n
  :columns (input expected)
  :rows (("wa" "ﾜ") ("nn" "ﾝ"))
  :body (nskk-e2e-with-buffer 'katakana-半角 nil
          (nskk-e2e-type input)
          (nskk-e2e-assert-buffer expected)))

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
;;;; Sentence-Level: Consecutive Conversions
;;;;
;;
;; Two kanji words converted back-to-back in the same buffer.
;; Default dict has ("かんじ" . ("漢字" ...)) and ("へんかん" . ("変換")).

(nskk-describe "consecutive conversions in same buffer"
  (nskk-it "produces 漢字変換 from two sequential henkan words"
    ;; Kanji → SPC → C-j commits 漢字; Henkan → SPC → C-j commits 変換.
    ;; Both conversions happen in the same buffer with no mode switch.
    (let ((dict '(("かんじ"  . ("漢字"))
                  ("へんかん" . ("変換")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "n")
        (nskk-e2e-type "ji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "漢字")
        (nskk-e2e-type "He")
        (nskk-e2e-type "n")
        (nskk-e2e-type "ka")
        (nskk-e2e-type "n")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "漢字変換")))))

;;;;
;;;; Sentence-Level: Long Reading Conversion
;;;;
;;
;; Readings of 5+ kana using entries already present in the default dict:
;;   ("ひらがな" . ("平仮名"))
;;   ("にほんご" . ("日本語"))

(nskk-describe "long reading conversion"
  (nskk-it "converts hiragana reading to 平仮名"
    ;; "Hiragana" romaji → ▽ひらがな → SPC → C-j → 平仮名
    ;; The default dict already has ("ひらがな" . ("平仮名")).
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Hi")
      (nskk-e2e-type "ra")
      (nskk-e2e-type "ga")
      (nskk-e2e-type "na")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "平仮名")))

  (nskk-it "converts nihongo reading to 日本語"
    ;; "Nihongo" romaji → ▽にほんご → SPC → C-j → 日本語
    ;; The default dict already has ("にほんご" . ("日本語")).
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ni")
      (nskk-e2e-type "ho")
      (nskk-e2e-type "n")
      (nskk-e2e-type "go")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "日本語"))))

(provide 'nskk-buffer-e2e-test)

;;; nskk-buffer-e2e-test.el ends here
