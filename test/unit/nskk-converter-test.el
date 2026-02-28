;;; nskk-converter-test.el --- Example Converter Tests for NSKK  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
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
(require 'nskk-henkan)
(require 'nskk-state)
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


;;;;
;;;; Unit Tests: Converter Lifecycle Operations
;;;;

(nskk-deftest-unit converter-start-conversion-basic
  "Test basic conversion start."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set state 'input-buffer "かんじ")
    (let ((result (nskk-henkan-start-conversion state '("漢字" "感じ"))))
      (should result)
      (should (nskk-state-henkan-position result))
      (should (equal (nskk-state-candidates result) '("漢字" "感じ")))
      (should (= (nskk-state-current-index result) 0)))))

(nskk-deftest-unit converter-start-conversion-empty-input
  "Test conversion start with empty input buffer."
  (let ((state (nskk-state-create 'hiragana)))
    ;; Empty input buffer — should not start conversion
    (should-not (nskk-henkan-start-conversion state '("漢字")))))

(nskk-deftest-unit converter-start-conversion-nil-state
  "Test conversion start with nil state."
  (should-not (nskk-henkan-start-conversion nil '("漢字"))))

(nskk-deftest-unit converter-start-conversion-no-candidates
  "Test conversion start with no candidates."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set state 'input-buffer "てすと")
    (let ((result (nskk-henkan-start-conversion state)))
      (should result)
      (should (equal (nskk-state-candidates result) '())))))

(nskk-deftest-unit converter-commit-conversion-basic
  "Test basic conversion commit."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set state 'input-buffer "かんじ")
    (nskk-henkan-start-conversion state '("漢字" "感じ"))
    (let ((result (nskk-henkan-commit-conversion state)))
      (should result)
      (should (string-match-p "漢字" (nskk-state-converted-buffer result)))
      (should (equal (nskk-state-input-buffer result) ""))
      (should-not (nskk-state-candidates result))
      (should-not (nskk-state-henkan-position result)))))

(nskk-deftest-unit converter-commit-conversion-no-active
  "Test commit when no conversion is active."
  (let ((state (nskk-state-create 'hiragana)))
    ;; No henkan-position set — should return nil (no-op)
    (should-not (nskk-henkan-commit-conversion state))))

(nskk-deftest-unit converter-commit-conversion-second-candidate
  "Test committing second candidate."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set state 'input-buffer "かんじ")
    (nskk-henkan-start-conversion state '("漢字" "感じ" "幹事"))
    ;; Move to second candidate
    (nskk-state-set state 'current-index 1)
    (let ((result (nskk-henkan-commit-conversion state)))
      (should result)
      (should (string-match-p "感じ" (nskk-state-converted-buffer result))))))

(nskk-deftest-unit converter-cancel-conversion-basic
  "Test basic conversion cancel."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set state 'input-buffer "かんじ")
    (nskk-henkan-start-conversion state '("漢字" "感じ"))
    (let ((result (nskk-henkan-cancel-conversion state "かんじ")))
      (should result)
      (should (equal (nskk-state-input-buffer result) "かんじ"))
      (should-not (nskk-state-candidates result))
      (should-not (nskk-state-henkan-position result)))))

(nskk-deftest-unit converter-cancel-conversion-no-original
  "Test cancel conversion without original input."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set state 'input-buffer "てすと")
    (nskk-henkan-start-conversion state '("テスト"))
    (let ((result (nskk-henkan-cancel-conversion state)))
      (should result)
      (should (equal (nskk-state-input-buffer result) "")))))

(nskk-deftest-unit converter-cancel-conversion-idempotent
  "Test cancel is idempotent when no conversion is active."
  (let ((state (nskk-state-create 'hiragana)))
    (let ((result (nskk-henkan-cancel-conversion state)))
      (should result)
      (should-not (nskk-state-henkan-position result)))))

(nskk-deftest-unit converter-in-conversion-p-basic
  "Test conversion state check."
  (let ((state (nskk-state-create 'hiragana)))
    ;; Not in conversion initially
    (should-not (nskk-henkan-in-conversion-p state))
    ;; Start conversion
    (nskk-state-set state 'input-buffer "かんじ")
    (nskk-henkan-start-conversion state '("漢字"))
    (should (nskk-henkan-in-conversion-p state))
    ;; After commit, no longer in conversion
    (nskk-henkan-commit-conversion state)
    (should-not (nskk-henkan-in-conversion-p state))))

(nskk-deftest-unit converter-has-candidates-p-basic
  "Test candidate availability check."
  (let ((state (nskk-state-create 'hiragana)))
    (should-not (nskk-henkan-has-candidates-p state))
    (nskk-state-set state 'input-buffer "かんじ")
    (nskk-henkan-start-conversion state '("漢字" "感じ"))
    (should (nskk-henkan-has-candidates-p state))
    (nskk-henkan-commit-conversion state)
    (should-not (nskk-henkan-has-candidates-p state))))

(nskk-deftest-unit converter-get-current-candidate-basic
  "Test getting current candidate."
  (let ((state (nskk-state-create 'hiragana)))
    ;; No candidates
    (should-not (nskk-henkan-get-current-candidate state))
    ;; With candidates
    (nskk-state-set state 'input-buffer "かんじ")
    (nskk-henkan-start-conversion state '("漢字" "感じ" "幹事"))
    (should (equal (nskk-henkan-get-current-candidate state) "漢字"))
    ;; After moving index
    (nskk-state-set state 'current-index 2)
    (should (equal (nskk-henkan-get-current-candidate state) "幹事"))))

(nskk-deftest-unit converter-get-current-candidate-nil-state
  "Test getting candidate from nil state."
  (should-not (nskk-henkan-get-current-candidate nil)))


;;;;
;;;; Unit Tests: Converter Convert Function
;;;;

(nskk-deftest-unit converter-convert-basic
  "Test basic converter-convert function."
  (let ((result (nskk-converter-convert "ka")))
    (should result)
    (should (equal (car result) "か"))
    (should (equal (cdr result) ""))))

(nskk-deftest-unit converter-convert-incomplete
  "Test converter-convert with incomplete input."
  (let ((result (nskk-converter-convert "k")))
    (should result)
    (should (eq (car result) :incomplete))))

(nskk-deftest-unit converter-convert-nil
  "Test converter-convert with nil input."
  (should-not (nskk-converter-convert nil)))

(nskk-deftest-unit converter-convert-empty
  "Test converter-convert with empty input."
  (should-not (nskk-converter-convert "")))

(nskk-deftest-unit converter-convert-long-match
  "Test converter-convert with multi-char match."
  (let ((result (nskk-converter-convert "sha")))
    (should result)
    (should (equal (car result) "しゃ"))
    (should (equal (cdr result) ""))))

(nskk-deftest-unit converter-convert-with-remaining
  "Test converter-convert that has remaining input."
  (let ((result (nskk-converter-convert "kak")))
    (should result)
    (should (equal (car result) "か"))
    (should (equal (cdr result) "k"))))


;;;;
;;;; Unit Tests: Possible Completions
;;;;

(nskk-deftest-unit converter-get-completions-basic
  "Test getting possible completions."
  (let ((completions (nskk-converter-get-possible-completions "ka")))
    (should completions)
    (should (cl-some (lambda (c) (equal (car c) "ka")) completions))))

(nskk-deftest-unit converter-get-completions-empty
  "Test completions for nil input."
  (should-not (nskk-converter-get-possible-completions nil)))

(nskk-deftest-unit converter-get-completions-k-prefix
  "Test completions for 'k' prefix."
  (let ((completions (nskk-converter-get-possible-completions "k")))
    (should completions)
    (should (> (length completions) 5))))


;;;;
;;;; Unit Tests: Rule Management
;;;;

(nskk-deftest-unit converter-add-rule-basic
  "Test adding a conversion rule."
  (let ((original (nskk-converter-get-rule "testkey")))
    (unwind-protect
        (progn
          (nskk-converter-add-rule "testkey" "テスト")
          (should (equal (nskk-converter-get-rule "testkey") "テスト")))
      ;; Cleanup
      (if original
          (nskk-converter-add-rule "testkey" original)
        (nskk-converter-remove-rule "testkey")))))

(nskk-deftest-unit converter-remove-rule-basic
  "Test removing a conversion rule."
  (unwind-protect
      (progn
        (nskk-converter-add-rule "tempkey" "テンプ")
        (should (nskk-converter-get-rule "tempkey"))
        (nskk-converter-remove-rule "tempkey")
        (should-not (nskk-converter-get-rule "tempkey")))
    (nskk-converter-remove-rule "tempkey")))

(nskk-deftest-unit converter-get-rule-existing
  "Test getting an existing rule."
  (should (equal (nskk-converter-get-rule "ka") "か")))

(nskk-deftest-unit converter-get-rule-nonexistent
  "Test getting a nonexistent rule."
  (should-not (nskk-converter-get-rule "nonexistent-romaji-key")))

(nskk-deftest-unit converter-add-rule-override
  "Test overriding an existing rule."
  (let ((original (nskk-converter-get-rule "ka")))
    (unwind-protect
        (progn
          (nskk-converter-add-rule "ka" "カ")
          (should (equal (nskk-converter-get-rule "ka") "カ")))
      ;; Restore original
      (nskk-converter-add-rule "ka" original))))


;;;;
;;;; Unit Tests: Style System
;;;;

(nskk-deftest-unit converter-register-style-basic
  "Test registering a new style."
  (let ((test-style-called nil))
    (nskk-converter-register-style 'test-style
      (lambda () (setq test-style-called t)))
    (unwind-protect
        (progn
          (nskk-converter-load-style 'test-style)
          (should test-style-called))
      ;; Restore standard style
      (nskk-converter-load-style 'standard))))

(nskk-deftest-unit converter-load-style-standard
  "Test loading the standard style."
  (should (eq (nskk-converter-load-style 'standard) 'standard))
  ;; Verify basic rules still work
  (should (equal (nskk-converter-get-rule "ka") "か")))

(nskk-deftest-unit converter-load-style-unknown
  "Test loading an unknown style raises error."
  (should-error (nskk-converter-load-style 'nonexistent-style)))

(nskk-deftest-unit converter-register-style-replaces-table
  "Test that loading a style clears and replaces the table."
  (nskk-converter-register-style 'minimal-test
    (lambda ()
      (nskk-converter-add-rule "x" "エックス")))
  (unwind-protect
      (progn
        (nskk-converter-load-style 'minimal-test)
        (should (equal (nskk-converter-get-rule "x") "エックス"))
        ;; Standard rules should be gone
        (should-not (equal (nskk-converter-get-rule "ka") "か")))
    ;; Restore standard
    (nskk-converter-load-style 'standard)))


;;;;
;;;; Unit Tests: Internal Conversion
;;;;

(nskk-deftest-unit converter-internal-simple
  "Test internal conversion for simple input."
  (let ((result (nskk-convert-romaji--internal "ka")))
    (should (equal result "か"))))

(nskk-deftest-unit converter-internal-compound
  "Test internal conversion for compound input."
  (let ((result (nskk-convert-romaji--internal "kanji")))
    (should (equal result "かんじ"))))

(nskk-deftest-unit converter-internal-sokuon
  "Test internal conversion with double consonant."
  (let ((result (nskk-convert-romaji--internal "kka")))
    (should (equal result "っか"))))


(nskk-test-suite converter-lifecycle
  nskk-unit-converter-start-conversion-basic
  nskk-unit-converter-start-conversion-empty-input
  nskk-unit-converter-start-conversion-nil-state
  nskk-unit-converter-commit-conversion-basic
  nskk-unit-converter-commit-conversion-no-active
  nskk-unit-converter-cancel-conversion-basic
  nskk-unit-converter-in-conversion-p-basic
  nskk-unit-converter-has-candidates-p-basic
  nskk-unit-converter-get-current-candidate-basic)

(nskk-test-suite converter-rules
  nskk-unit-converter-add-rule-basic
  nskk-unit-converter-remove-rule-basic
  nskk-unit-converter-get-rule-existing
  nskk-unit-converter-get-rule-nonexistent)

(nskk-test-suite converter-styles
  nskk-unit-converter-register-style-basic
  nskk-unit-converter-load-style-standard
  nskk-unit-converter-load-style-unknown)

(provide 'nskk-converter-test)

;;; nskk-converter-test.el ends here
