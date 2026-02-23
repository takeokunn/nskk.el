;;; nskk-converter-test.el --- Example Converter Tests for NSKK  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: Japanese, input, method, test, converter
;; Homepage: https://github.com/takeokunn/nskk.el

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

;; This file provides example tests for the NSKK converter component.
;; It demonstrates unit tests, integration tests, and property-based tests
;; for the conversion engine.
;;
;; Test categories:
;; - Unit tests for basic conversion
;; - Integration tests for full conversion flow
;; - Property-based tests for conversion properties
;; - Performance tests for conversion speed
;; - Edge case tests

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-converter)  ; Assumes converter implementation exists
(eval-when-compile (require 'cl-lib))


;;;;
;;;; Unit Tests: Basic Conversion
;;;;

(nskk-deftest-unit romaji-vowel-conversion
  "Test basic vowel conversion."
  (should (equal (nskk-convert-romaji "a") "あ"))
  (should (equal (nskk-convert-romaji "i") "い"))
  (should (equal (nskk-convert-romaji "u") "う"))
  (should (equal (nskk-convert-romaji "e") "え"))
  (should (equal (nskk-convert-romaji "o") "お")))

(nskk-deftest-unit romaji-consonant-vowel-conversion
  "Test consonant + vowel conversion."
  (should (equal (nskk-convert-romaji "ka") "か"))
  (should (equal (nskk-convert-romaji "ki") "き"))
  (should (equal (nskk-convert-romaji "ku") "く"))
  (should (equal (nskk-convert-romaji "ke") "け"))
  (should (equal (nskk-convert-romaji "ko") "こ"))
  (should (equal (nskk-convert-romaji "sa") "さ"))
  (should (equal (nskk-convert-romaji "shi") "し"))
  (should (equal (nskk-convert-romaji "ta") "た"))
  (should (equal (nskk-convert-romaji "chi") "ち"))
  (should (equal (nskk-convert-romaji "tsu") "つ"))
  (should (equal (nskk-convert-romaji "na") "な"))
  (should (equal (nskk-convert-romaji "ni") "に"))
  (should (equal (nskk-convert-romaji "ha") "は"))
  (should (equal (nskk-convert-romaji "hi") "ひ"))
  (should (equal (nskk-convert-romaji "fu") "ふ"))
  (should (equal (nskk-convert-romaji "he") "へ"))
  (should (equal (nskk-convert-romaji "ho") "ほ"))
  (should (equal (nskk-convert-romaji "ma") "ま"))
  (should (equal (nskk-convert-romaji "mi") "み"))
  (should (equal (nskk-convert-romaji "ya") "や"))
  (should (equal (nskk-convert-romaji "yu") "ゆ"))
  (should (equal (nskk-convert-romaji "yo") "よ"))
  (should (equal (nskk-convert-romaji "ra") "ら"))
  (should (equal (nskk-convert-romaji "ri") "り"))
  (should (equal (nskk-convert-romaji "ru") "る"))
  (should (equal (nskk-convert-romaji "re") "れ"))
  (should (equal (nskk-convert-romaji "ro") "ろ"))
  (should (equal (nskk-convert-romaji "wa") "わ"))
  (should (equal (nskk-convert-romaji "wo") "を"))
  (should (equal (nskk-convert-romaji "n") "ん")))

(nskk-deftest-unit romaji-voiced-consonant-conversion
  "Test voiced consonant conversion."
  (should (equal (nskk-convert-romaji "ga") "が"))
  (should (equal (nskk-convert-romaji "gi") "ぎ"))
  (should (equal (nskk-convert-romaji "gu") "ぐ"))
  (should (equal (nskk-convert-romaji "ge") "げ"))
  (should (equal (nskk-convert-romaji "go") "ご"))
  (should (equal (nskk-convert-romaji "za") "ざ"))
  (should (equal (nskk-convert-romaji "ji") "じ"))
  (should (equal (nskk-convert-romaji "zu") "ず"))
  (should (equal (nskk-convert-romaji "da") "だ"))
  (should (equal (nskk-convert-romaji "ji") "じ"))
  (should (equal (nskk-convert-romaji "ba") "ば"))
  (should (equal (nskk-convert-romaji "bi") "び"))
  (should (equal (nskk-convert-romaji "bu") "ぶ"))
  (should (equal (nskk-convert-romaji "be") "べ"))
  (should (equal (nskk-convert-romaji "bo") "ぼ"))
  (should (equal (nskk-convert-romaji "pa") "ぱ"))
  (should (equal (nskk-convert-romaji "pi") "ぴ"))
  (should (equal (nskk-convert-romaji "pu") "ぷ"))
  (should (equal (nskk-convert-romaji "pe") "ぺ"))
  (should (equal (nskk-convert-romaji "po") "ぽ")))

(nskk-deftest-unit romaji-palatal-consonant-conversion
  "Test palatal consonant conversion."
  (should (equal (nskk-convert-romaji "kya") "きゃ"))
  (should (equal (nskk-convert-romaji "kyu") "きゅ"))
  (should (equal (nskk-convert-romaji "kyo") "きょ"))
  (should (equal (nskk-convert-romaji "sha") "しゃ"))
  (should (equal (nskk-convert-romaji "shu") "しゅ"))
  (should (equal (nskk-convert-romaji "sho") "しょ"))
  (should (equal (nskk-convert-romaji "cha") "ちゃ"))
  (should (equal (nskk-convert-romaji "chu") "ちゅ"))
  (should (equal (nskk-convert-romaji "cho") "ちょ"))
  (should (equal (nskk-convert-romaji "nya") "にゃ"))
  (should (equal (nskk-convert-romaji "nyu") "にゅ"))
  (should (equal (nskk-convert-romaji "nyo") "にょ"))
  (should (equal (nskk-convert-romaji "hya") "ひゃ"))
  (should (equal (nskk-convert-romaji "hyu") "ひゅ"))
  (should (equal (nskk-convert-romaji "hyo") "ひょ"))
  (should (equal (nskk-convert-romaji "mya") "みゃ"))
  (should (equal (nskk-convert-romaji "myu") "みゅ"))
  (should (equal (nskk-convert-romaji "myo") "みょ"))
  (should (equal (nskk-convert-romaji "rya") "りゃ"))
  (should (equal (nskk-convert-romaji "ryu") "りゅ"))
  (should (equal (nskk-convert-romaji "ryo") "りょ")))

(nskk-deftest-unit romaji-special-conversion
  "Test special conversion cases."
  (should (equal (nskk-convert-romaji "nn") "ん"))
  (should (equal (nskk-convert-romaji "n'") "ん"))
  (should (equal (nskk-convert-romaji "kka") "っか"))
  (should (equal (nskk-convert-romaji "sshi") "っし"))
  (should (equal (nskk-convert-romaji "tte") "って"))
  (should (equal (nskk-convert-romaji "ppu") "っぷ"))
  (should (equal (nskk-convert-romaji "xtsu") "っ"))
  (should (equal (nskk-convert-romaji "ya") "や"))
  (should (equal (nskk-convert-romaji "yu") "ゆ"))
  (should (equal (nskk-convert-romaji "yo") "よ")))

(nskk-deftest-unit romaji-conversion-words
  "Test conversion of complete words."
  (should (equal (nskk-convert-romaji "nihongo") "にほんご"))
  (should (equal (nskk-convert-romaji "konnichiwa") "こんにちわ"))
  (should (equal (nskk-convert-romaji "sayounara") "さようなら"))
  (should (equal (nskk-convert-romaji "arigatou") "ありがとう"))
  (should (equal (nskk-convert-romaji "sakana") "さかな"))
  (should (equal (nskk-convert-romaji "yama") "やま"))
  (should (equal (nskk-convert-romaji "kawa") "かわ"))
  (should (equal (nskk-convert-romaji "sora") "そら")))


;;;;
;;;; Unit Tests: Edge Cases
;;;;

(nskk-deftest-unit romaji-conversion-empty
  "Test empty string conversion."
  (should (equal (nskk-convert-romaji "") ""))
  (should (equal (nskk-convert-romaji nil) nil))
  (should (equal (nskk-convert-romaji " ") " ")))

(nskk-deftest-unit romaji-conversion-invalid
  "Test invalid romaji conversion."
  (should (equal (nskk-convert-romaji "xyz") "xyz"))
  (should (equal (nskk-convert-romaji "q") "q"))
  (should (equal (nskk-convert-romaji "123") "123")))

(nskk-deftest-unit romaji-conversion-case-sensitivity
  "Test case sensitivity."
  (should (equal (nskk-convert-romaji "a") "あ"))
  (should (equal (nskk-convert-romaji "A") "あ"))
  (should (equal (nskk-convert-romaji "ka") "か"))
  (should (equal (nskk-convert-romaji "KA") "か"))
  (should (equal (nskk-convert-romaji "Ka") "か")))

(nskk-deftest-unit romaji-conversion-boundary
  "Test boundary cases."
  ;; Consonant followed by 'n' at end
  (should (equal (nskk-convert-romaji "kan") "かん"))
  ;; Double consonant at end (sokuon + remaining consonant)
  (should (equal (nskk-convert-romaji "kk") "っk"))
  ;; Palatal at end
  (should (equal (nskk-convert-romaji "kya") "きゃ")))


;;;;
;;;; Integration Tests: Basic Conversion
;;;;

(nskk-deftest-integration conversion-basic-romaji-to-kana
  "Test basic romaji to kana conversion pipeline."
  (should (equal (nskk-convert-romaji "aiueo") "あいうえお"))
  (should (equal (nskk-convert-romaji "kakikukeko") "かきくけこ"))
  (should (equal (nskk-convert-romaji "sashisuseso") "さしすせそ")))


;;;;
;;;; Property-Based Tests: Conversion Properties
;;;;

(nskk-property-test conversion-idempotent-property
  ((input romaji-string))
  (let ((converted (nskk-convert-romaji input)))
    (equal converted (nskk-convert-romaji converted)))
  100)

(nskk-property-test conversion-length-property
  ((input romaji-string))
  (let ((converted (nskk-convert-romaji input)))
    ;; Kana output length should be between 1/4 and 2x of romaji input
    (and (>= (length converted) (/ (length input) 4))
         (<= (length converted) (* (length input) 2))))
  100)

(nskk-property-test conversion-no-loss-property
  ((input romaji-string))
  (let ((converted (nskk-convert-romaji input)))
    (not (string= converted "")))
  50)

(nskk-property-test conversion-deterministic-property
  ((input romaji-string))
  (let ((result1 (nskk-convert-romaji input))
        (result2 (nskk-convert-romaji input)))
    (equal result1 result2))
  100)


;;;;
;;;; Performance Tests: Conversion Speed
;;;;

(nskk-deftest-performance conversion-basic-performance
  "Test basic conversion performance."
  (let ((test-string "konnichiwa"))
    (nskk-should-be-fast
     basic-conversion 5000
     (dotimes (_ 10000)
       (nskk-convert-romaji test-string)))))

(nskk-deftest-performance conversion-complex-performance
  "Test complex conversion performance."
  (let ((test-string "konyakunishitekyouyakusuru"))
    (nskk-should-be-fast
     complex-conversion 5000
     (dotimes (_ 1000)
       (nskk-convert-romaji test-string)))))

(nskk-deftest-performance conversion-batch-performance
  "Test batch conversion performance."
  (let ((test-strings '("aiueo" "kakikukeko" "sashisuseso"
                        "tachitsuteto" "naninuneno" "hahifuheho"
                        "mamimumemo" "yayuyo" "rariruro" "wawo")))
    (nskk-should-be-fast
     batch-conversion 5000
     (dotimes (_ 1000)
       (dolist (s test-strings)
         (nskk-convert-romaji s))))))

(nskk-deftest-performance conversion-memory-performance
  "Test conversion memory performance."
  (let ((start-time (current-time)))
    (dotimes (_ 1000)
      (nskk-convert-romaji "konnichiwa"))
    (let ((elapsed-ms (* 1000 (float-time (time-subtract (current-time) start-time)))))
      (should (< elapsed-ms 5000)))))


;;;;
;;;; Regression Tests
;;;;

(nskk-regression-test double-consonant
  "double-consonant-001"
  "Fix for double consonant conversion bug."
  (should (equal (nskk-convert-romaji "gakkou") "がっこう"))
  (should (equal (nskk-convert-romaji "zasshi") "ざっし"))
  (should (equal (nskk-convert-romaji "chotto") "ちょっと")))

(nskk-regression-test n-conversion
  "n-conversion-001"
  "Fix for 'n' conversion in various contexts."
  (should (equal (nskk-convert-romaji "nihon") "にほん"))
  (should (equal (nskk-convert-romaji "anna") "あんな"))
  (should (equal (nskk-convert-romaji "san") "さん"))
  (should (equal (nskk-convert-romaji "sensei") "せんせい")))

(nskk-regression-test palatal-conversion
  "palatal-conversion-001"
  "Fix for palatal consonant handling."
  (should (equal (nskk-convert-romaji "toukyou") "とうきょう"))
  (should (equal (nskk-convert-romaji "kyouto") "きょうと"))
  (should (equal (nskk-convert-romaji "sushi") "すし")))


;;;;
;;;; Custom Assertions
;;;;

(nskk-deftest-unit custom-assertion-tests
  "Test custom assertion helpers."
  (nskk-assert-approx-equal 1.0 1.001 0.01)
  (nskk-assert-strings-equal "test" "test")
  (nskk-assert-length '(1 2 3) 3)
  (nskk-assert-member 'b '(a b c))
  (nskk-assert-type "string" #'stringp))


;;;;
;;;; Test Suite Organization
;;;;

(nskk-test-suite converter-basics
  nskk-unit-romaji-vowel-conversion
  nskk-unit-romaji-consonant-vowel-conversion
  nskk-unit-romaji-voiced-consonant-conversion
  nskk-unit-romaji-palatal-consonant-conversion
  nskk-unit-romaji-special-conversion
  nskk-unit-romaji-conversion-words)

(nskk-test-suite converter-edge-cases
  nskk-unit-romaji-conversion-empty
  nskk-unit-romaji-conversion-invalid
  nskk-unit-romaji-conversion-case-sensitivity
  nskk-unit-romaji-conversion-boundary)

(nskk-test-suite converter-integration
  nskk-integration-conversion-basic-romaji-to-kana)

(nskk-test-suite converter-performance
  nskk-performance-conversion-basic-performance
  nskk-performance-conversion-complex-performance
  nskk-performance-conversion-batch-performance
  nskk-performance-conversion-memory-performance)

(provide 'nskk-converter-test)

;;; nskk-converter-test.el ends here
