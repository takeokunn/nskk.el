;;; nskk-converter-test.el --- Example Converter Tests for NSKK  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

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

;; This file provides unit tests for the NSKK converter component.
;; It covers unit tests, integration tests, and property-based tests
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
(require 'nskk-pbt-generators)
(eval-when-compile (require 'cl-lib))


(nskk-describe "romaji basic conversion"
  (nskk-deftest-cases converter-vowels
    (("a" . "あ")
     ("i" . "い")
     ("u" . "う")
     ("e" . "え")
     ("o" . "お"))
    :description "Converts vowels to hiragana"
    :body (should (equal expected (nskk-convert-romaji input))))

  (nskk-deftest-cases converter-consonant-vowel
    (("ka"  . "か")
     ("ki"  . "き")
     ("ku"  . "く")
     ("ke"  . "け")
     ("ko"  . "こ")
     ("sa"  . "さ")
     ("shi" . "し")
     ("ta"  . "た")
     ("chi" . "ち")
     ("tsu" . "つ")
     ("na"  . "な")
     ("ni"  . "に")
     ("ha"  . "は")
     ("hi"  . "ひ")
     ("fu"  . "ふ")
     ("he"  . "へ")
     ("ho"  . "ほ")
     ("ma"  . "ま")
     ("mi"  . "み")
     ("ya"  . "や")
     ("yu"  . "ゆ")
     ("yo"  . "よ")
     ("ra"  . "ら")
     ("ri"  . "り")
     ("ru"  . "る")
     ("re"  . "れ")
     ("ro"  . "ろ")
     ("wa"  . "わ")
     ("wo"  . "を")
     ("n"   . "ん"))
    :description "Converts consonant + vowel pairs to hiragana"
    :body (should (equal expected (nskk-convert-romaji input))))

  (nskk-deftest-cases converter-voiced-consonants
    (("ga" . "が")
     ("gi" . "ぎ")
     ("gu" . "ぐ")
     ("ge" . "げ")
     ("go" . "ご")
     ("za" . "ざ")
     ("ji" . "じ")
     ("zu" . "ず")
     ("da" . "だ")
     ("du" . "づ")
     ("ba" . "ば")
     ("bi" . "び")
     ("bu" . "ぶ")
     ("be" . "べ")
     ("bo" . "ぼ")
     ("pa" . "ぱ")
     ("pi" . "ぴ")
     ("pu" . "ぷ")
     ("pe" . "ぺ")
     ("po" . "ぽ"))
    :description "Converts voiced and semi-voiced consonants to hiragana"
    :body (should (equal expected (nskk-convert-romaji input))))

  (nskk-deftest-cases converter-palatal-consonants
    (("kya" . "きゃ")
     ("kyu" . "きゅ")
     ("kyo" . "きょ")
     ("sha" . "しゃ")
     ("shu" . "しゅ")
     ("sho" . "しょ")
     ("cha" . "ちゃ")
     ("chu" . "ちゅ")
     ("cho" . "ちょ")
     ("nya" . "にゃ")
     ("nyu" . "にゅ")
     ("nyo" . "にょ")
     ("hya" . "ひゃ")
     ("hyu" . "ひゅ")
     ("hyo" . "ひょ")
     ("mya" . "みゃ")
     ("myu" . "みゅ")
     ("myo" . "みょ")
     ("rya" . "りゃ")
     ("ryu" . "りゅ")
     ("ryo" . "りょ"))
    :description "Converts palatal consonant combinations to hiragana"
    :body (should (equal expected (nskk-convert-romaji input))))

  (nskk-deftest-cases converter-special-sequences
    (("nn"   . "ん")
     ("n'"   . "ん")
     ("kka"  . "っか")
     ("sshi" . "っし")
     ("tte"  . "って")
     ("ppu"  . "っぷ")
     ("xtsu" . "っ")
     ("ya"   . "や")
     ("yu"   . "ゆ")
     ("yo"   . "よ"))
    :description "Converts special romaji sequences to hiragana"
    :body (should (equal expected (nskk-convert-romaji input))))

  (nskk-deftest-cases converter-complete-words
    (("nihongo"    . "にほんご")
     ("konnichiwa" . "こんにちわ")
     ("sayounara"  . "さようなら")
     ("arigatou"   . "ありがとう")
     ("sakana"     . "さかな")
     ("yama"       . "やま")
     ("kawa"       . "かわ")
     ("sora"       . "そら"))
    :description "Converts complete romaji words to hiragana"
    :body (should (equal expected (nskk-convert-romaji input)))))

(nskk-describe "romaji edge cases"
  (nskk-it "handles empty and nil input"
    (should (equal (nskk-convert-romaji "") ""))
    (should (equal (nskk-convert-romaji nil) nil))
    (should (equal (nskk-convert-romaji " ") " ")))

  (nskk-it "passes through invalid romaji unchanged"
    (should (equal (nskk-convert-romaji "xyz") "xyz"))
    (should (equal (nskk-convert-romaji "q") "q"))
    (should (equal (nskk-convert-romaji "123") "123")))

  (nskk-it "is case insensitive"
    (should (equal (nskk-convert-romaji "a") "あ"))
    (should (equal (nskk-convert-romaji "A") "あ"))
    (should (equal (nskk-convert-romaji "ka") "か"))
    (should (equal (nskk-convert-romaji "KA") "か"))
    (should (equal (nskk-convert-romaji "Ka") "か")))

  (nskk-it "handles boundary cases correctly"
    ;; Consonant followed by 'n' at end
    (should (equal (nskk-convert-romaji "kan") "かん"))
    ;; Double consonant at end (sokuon + remaining consonant)
    (should (equal (nskk-convert-romaji "kk") "っk"))
    ;; Palatal at end
    (should (equal (nskk-convert-romaji "kya") "きゃ"))))

(nskk-describe "romaji-to-kana integration"
  (nskk-it "converts complete kana rows"
    (should (equal (nskk-convert-romaji "aiueo") "あいうえお"))
    (should (equal (nskk-convert-romaji "kakikukeko") "かきくけこ"))
    (should (equal (nskk-convert-romaji "sashisuseso") "さしすせそ"))))

;; TR-004: nskk-converter-initialize idempotency
(nskk-describe "converter-initialize"
  (nskk-it "is idempotent: subsequent calls are no-ops"
    (let ((was-initialized nskk--converter-initialized))
      (unwind-protect
          (progn
            ;; Table should still work after multiple initialize calls
            (nskk-converter-initialize)
            (nskk-converter-initialize)
            (should (equal (nskk-converter-get-rule "ka") "か"))
            (should (equal (nskk-convert-romaji "ka") "か")))
        ;; Restore previous initialization state
        (setq nskk--converter-initialized was-initialized)))))


;;;;
;;;; Property-Based Tests: Conversion Properties
;;;;

(nskk-property-test conversion-output-never-expands
  ((input romaji-string))
  ;; Romaji-to-kana conversion never produces more characters than the input.
  ;; Kana is more compact than romaji; incomplete sequences pass through unchanged.
  (<= (length (nskk-convert-romaji input)) (length input))
  100)

;; Minimum compression: kana output is at least 1/4 of romaji input length.
;; The upper bound is already proved by conversion-output-never-expands.
;; Rationale: the longest romaji sequence (e.g. "xtsu" -> "っ") is 4:1,
;; so floor(len/4) is a tight lower bound.
(nskk-property-test conversion-min-compression-ratio
  ((input romaji-string))
  (>= (length (nskk-convert-romaji input)) (/ (length input) 4))
  100)

;; TR-002: Fixed PBT — empty input is valid and returns "" by contract.
(nskk-property-test conversion-non-empty-output
  ((input romaji-string))
  ;; Non-empty romaji input always produces non-empty output.
  ;; Empty input is handled separately (returns "" by contract).
  (or (zerop (length input))
      (> (length (nskk-convert-romaji input)) 0))
  100)

(nskk-deftest-performance conversion-basic-performance
  "Basic romaji-to-kana conversion completes within time budget."
  (let ((test-string "konnichiwa"))
    (nskk-should-be-fast
     basic-conversion 5000
     (dotimes (_ 10000)
       (nskk-convert-romaji test-string)))))

(nskk-deftest-performance conversion-complex-performance
  "Complex romaji-to-kana conversion completes within time budget."
  (let ((test-string "konyakunishitekyouyakusuru"))
    (nskk-should-be-fast
     complex-conversion 5000
     (dotimes (_ 1000)
       (nskk-convert-romaji test-string)))))

(nskk-deftest-performance conversion-batch-performance
  "Batch romaji-to-kana conversion completes within time budget."
  (let ((test-strings '("aiueo" "kakikukeko" "sashisuseso"
                        "tachitsuteto" "naninuneno" "hahifuheho"
                        "mamimumemo" "yayuyo" "rariruro" "wawo")))
    (nskk-should-be-fast
     batch-conversion 5000
     (dotimes (_ 1000)
       (dolist (s test-strings)
         (nskk-convert-romaji s))))))

(nskk-deftest-performance conversion-memory-performance
  "Memory usage during conversion stays within time budget."
  (nskk-should-be-fast
   memory-conversion 5000
   (dotimes (_ 1000)
     (nskk-convert-romaji "konnichiwa"))))


(nskk-describe "regression: double consonant"
  (nskk-it "correctly converts double consonants (double-consonant-001)"
    (should (equal (nskk-convert-romaji "gakkou") "がっこう"))
    (should (equal (nskk-convert-romaji "zasshi") "ざっし"))
    (should (equal (nskk-convert-romaji "chotto") "ちょっと"))))

(nskk-describe "regression: n conversion"
  (nskk-it "correctly converts 'n' in various contexts (n-conversion-001)"
    (should (equal (nskk-convert-romaji "nihon") "にほん"))
    (should (equal (nskk-convert-romaji "anna") "あんな"))
    (should (equal (nskk-convert-romaji "san") "さん"))
    (should (equal (nskk-convert-romaji "sensei") "せんせい"))))

(nskk-describe "regression: palatal conversion"
  (nskk-it "correctly handles palatal consonants (palatal-conversion-001)"
    (should (equal (nskk-convert-romaji "toukyou") "とうきょう"))
    (should (equal (nskk-convert-romaji "kyouto") "きょうと"))
    (should (equal (nskk-convert-romaji "sushi") "すし"))))

(nskk-describe "regression: long string handling"
  (nskk-it "handles long inputs without truncation (internal-long-string-001)"
    ;; 25 syllables — well above previous 100-iteration limit but tests
    ;; that each romaji token advances exactly one step.
    (let ((long-romaji "aiueoaiueoaiueoaiueoaiueo")
          (expected    "あいうえおあいうえおあいうえおあいうえおあいうえお"))
      (should (equal (nskk-convert-romaji long-romaji) expected)))
    ;; 30+ conversion steps via consonant+vowel pairs
    (should (equal (nskk-convert-romaji "kakikukekokakikukekokakikukeko")
                   "かきくけこかきくけこかきくけこ"))))

(nskk-describe "regression: fallback path"
  (nskk-it "appends unconvertible tail verbatim (internal-fallback-001)"
    ;; Pure unknown sequence: returned unchanged
    (should (equal (nskk-convert-romaji "xyz") "xyz"))
    ;; Mixed: known prefix converted, unknown tail appended
    (should (equal (nskk-convert-romaji "kaxyz") "かxyz"))
    ;; Trailing isolated consonant: appended as-is
    (should (equal (nskk-convert-romaji "kak") "かk"))))


(nskk-describe "custom assertion helpers"
  (nskk-it "works with all custom assertion types"
    (nskk-assert-approx-equal 1.0 1.001 0.01)
    (nskk-assert-strings-equal "test" "test")
    (nskk-assert-length '(1 2 3) 3)
    (nskk-assert-member 'b '(a b c))
    (nskk-assert-type "string" #'stringp)))


;;;;
;;;; Test Suite Organization
;;;;

(nskk-test-suite converter-performance
  nskk-performance-conversion-basic-performance
  nskk-performance-conversion-complex-performance
  nskk-performance-conversion-batch-performance
  nskk-performance-conversion-memory-performance)


(nskk-describe "converter-convert function"
  (nskk-it "converts basic input"
    (let ((result (nskk-converter-convert "ka")))
      (should result)
      (should (equal (car result) "か"))
      (should (equal (cdr result) ""))))

  (nskk-it "returns :incomplete for incomplete input"
    (let ((result (nskk-converter-convert "k")))
      (should result)
      (should (eq (car result) :incomplete))))

  (nskk-it "returns nil for nil input"
    (should-not (nskk-converter-convert nil)))

  (nskk-it "returns nil for empty input"
    (should-not (nskk-converter-convert "")))

  (nskk-it "converts multi-char match"
    (let ((result (nskk-converter-convert "sha")))
      (should result)
      (should (equal (car result) "しゃ"))
      (should (equal (cdr result) ""))))

  (nskk-it "returns remaining input after conversion"
    (let ((result (nskk-converter-convert "kak")))
      (should result)
      (should (equal (car result) "か"))
      (should (equal (cdr result) "k")))))

;; TR-007: Direct nskk-converter-lookup tests
(nskk-describe "nskk-converter-lookup"
  (nskk-deftest-cases lookup-complete-match
    (("a"   . "あ")
     ("ka"  . "か")
     ("sha" . "しゃ")
     ("tsu" . "つ")
     ("-"   . "ー"))
    :description "Returns kana string for complete romaji matches"
    :body (should (equal expected (nskk-converter-lookup input))))

  (nskk-deftest-cases lookup-incomplete-prefix
    (("k" . :incomplete)
     ("s" . :incomplete)
     ("sh" . :incomplete)
     ("ky" . :incomplete))
    :description "Returns :incomplete for known romaji prefixes"
    :body (should (eq expected (nskk-converter-lookup input))))

  (nskk-it "returns nil for unknown key"
    (should-not (nskk-converter-lookup "zzzz"))
    (should-not (nskk-converter-lookup "qwerty")))

  (nskk-it "returns nil for non-string input"
    (should-not (nskk-converter-lookup nil))
    (should-not (nskk-converter-lookup 42))
    (should-not (nskk-converter-lookup 'symbol))))

;; TR-001: CPS variant tests
(nskk-describe "nskk-converter-convert/k CPS variant"
  (nskk-it "calls on-match with kana and remaining on complete match"
    (let (got-kana got-remaining)
      (nskk-converter-convert/k "ka"
        (lambda (kana remaining) (setq got-kana kana got-remaining remaining))
        (lambda (_romaji) (should nil))   ; should not reach on-incomplete
        (lambda () (should nil)))         ; should not reach on-fail
      (should (equal got-kana "か"))
      (should (equal got-remaining ""))))

  (nskk-it "calls on-incomplete for known prefix"
    (let (got-romaji)
      (nskk-converter-convert/k "k"
        (lambda (_kana _rem) (should nil)) ; should not reach on-match
        (lambda (romaji) (setq got-romaji romaji))
        (lambda () (should nil)))          ; should not reach on-fail
      (should (equal got-romaji "k"))))

  (nskk-it "calls on-fail for nil input"
    (let (fail-called)
      (nskk-converter-convert/k nil
        (lambda (_k _r) (should nil))
        (lambda (_r) (should nil))
        (lambda () (setq fail-called t)))
      (should fail-called)))

  (nskk-it "calls on-fail for empty input"
    (let (fail-called)
      (nskk-converter-convert/k ""
        (lambda (_k _r) (should nil))
        (lambda (_r) (should nil))
        (lambda () (setq fail-called t)))
      (should fail-called)))

  (nskk-it "calls on-fail for unknown romaji"
    (let (fail-called)
      ;; "2" has no registered rules or prefixes in the standard table
      (nskk-converter-convert/k "2"
        (lambda (_k _r) (should nil))
        (lambda (_r) (should nil))
        (lambda () (setq fail-called t)))
      (should fail-called)))

  (nskk-deftest-cases convert/k-match-cases
    (("sha" . "しゃ") ("tsu" . "つ") ("chi" . "ち") ("a" . "あ"))
    :description "nskk-converter-convert/k on-match path for known syllables"
    :body
    (let (got-kana)
      (nskk-converter-convert/k input
        (lambda (kana _rem) (setq got-kana kana))
        (lambda (_r) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana expected)))))

(nskk-describe "nskk-convert-romaji/k CPS variant"
  (nskk-it "calls continuation with converted kana"
    (let (result)
      (nskk-convert-romaji/k "ka" (lambda (kana) (setq result kana)))
      (should (equal result "か"))))

  (nskk-it "calls continuation with nil for nil input"
    (let (result called)
      (nskk-convert-romaji/k nil (lambda (kana) (setq result kana called t)))
      (should called)
      (should-not result)))

  (nskk-it "calls continuation with empty string for empty input"
    (let (result)
      (nskk-convert-romaji/k "" (lambda (kana) (setq result kana)))
      (should (equal result ""))))

  (nskk-deftest-cases romaji/k-full-conversion
    (("nihongo" . "にほんご")
     ("konnichiwa" . "こんにちわ")
     ("aiueo" . "あいうえお"))
    :description "nskk-convert-romaji/k produces same output as sync wrapper"
    :body
    (let (result)
      (nskk-convert-romaji/k input (lambda (kana) (setq result kana)))
      (should (equal result expected)))))

(nskk-describe "nskk-convert-n--internal/k CPS variant"
  (nskk-it "calls on-hatsuon for standalone n"
    (let (got-kana got-rest)
      (nskk-convert-n--internal/k "n"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should-not got-rest)))

  (nskk-it "calls on-hatsuon for n before consonant"
    (let (got-kana got-rest)
      (nskk-convert-n--internal/k "nb"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should (equal got-rest "b"))))

  (nskk-it "calls on-fallthrough for n before vowel"
    (let (fallthrough-called)
      (nskk-convert-n--internal/k "na"
        (lambda (_k _r) (should nil))
        (lambda () (setq fallthrough-called t)))
      (should fallthrough-called)))

  (nskk-it "calls on-hatsuon for n-quote sequence"
    (let (got-kana got-rest)
      (nskk-convert-n--internal/k "n'"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should-not got-rest))))

(nskk-describe "possible completions"
  (nskk-it "returns completions for a basic prefix"
    (let ((completions (nskk-converter-get-possible-completions "ka")))
      (should completions)
      (should (cl-some (lambda (c) (equal (car c) "ka")) completions))))

  (nskk-it "returns nil for nil input"
    (should-not (nskk-converter-get-possible-completions nil)))

  (nskk-it "returns more than 5 completions for 'k' prefix"
    (let ((completions (nskk-converter-get-possible-completions "k")))
      (should completions)
      (should (> (length completions) 5)))))

(nskk-describe "rule management"
  (nskk-it "adds a conversion rule"
    (let ((original (nskk-converter-get-rule "testkey")))
      (unwind-protect
          (progn
            (nskk-converter-add-rule "testkey" "テスト")
            (should (equal (nskk-converter-get-rule "testkey") "テスト")))
        ;; Cleanup
        (if original
            (nskk-converter-add-rule "testkey" original)
          (nskk-converter-remove-rule "testkey")))))

  (nskk-it "removes a conversion rule"
    (unwind-protect
        (progn
          (nskk-converter-add-rule "tempkey" "テンプ")
          (should (nskk-converter-get-rule "tempkey"))
          (nskk-converter-remove-rule "tempkey")
          (should-not (nskk-converter-get-rule "tempkey")))
      (nskk-converter-remove-rule "tempkey")))

  (nskk-it "gets an existing rule"
    (should (equal (nskk-converter-get-rule "ka") "か")))

  (nskk-it "returns nil for nonexistent rule"
    (should-not (nskk-converter-get-rule "nonexistent-romaji-key")))

  (nskk-it "overrides an existing rule"
    (let ((original (nskk-converter-get-rule "ka")))
      (unwind-protect
          (progn
            (nskk-converter-add-rule "ka" "カ")
            (should (equal (nskk-converter-get-rule "ka") "カ")))
        ;; Restore original
        (nskk-converter-add-rule "ka" original)))))

(nskk-describe "style system"
  (nskk-it "registers and loads a new style"
    (let ((test-style-called nil))
      (nskk-converter-register-style 'test-style
        (lambda () (setq test-style-called t)))
      (unwind-protect
          (progn
            (nskk-converter-load-style 'test-style)
            (should test-style-called))
        ;; Restore standard style
        (nskk-converter-load-style 'standard))))

  (nskk-it "loads the standard style"
    (should (eq (nskk-converter-load-style 'standard) 'standard))
    ;; Verify basic rules still work
    (should (equal (nskk-converter-get-rule "ka") "か")))

  (nskk-it "raises user-error for unknown style"
    (should-error (nskk-converter-load-style 'nonexistent-style) :type 'user-error))

  (nskk-it "clears and replaces the table when loading a style"
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
      (nskk-converter-load-style 'standard))))

;; TR-005: nskk-converter-define-style macro tests
(nskk-describe "nskk-converter-define-style macro"
  (nskk-it "generates an init function and registers the style"
    (nskk-converter-define-style test-define-macro-style
      "Temporary style for macro validation test."
      ("zz" "zzテスト"))
    (unwind-protect
        (progn
          (nskk-converter-load-style 'test-define-macro-style)
          ;; The macro-generated init function should have asserted this rule
          (should (equal (nskk-converter-get-rule "zz") "zzテスト"))
          ;; Standard rules should not be present
          (should-not (equal (nskk-converter-get-rule "ka") "か")))
      ;; Always restore standard style
      (nskk-converter-load-style 'standard)))

  (nskk-it "loads and unloads cleanly"
    (nskk-converter-define-style test-define-clean-style
      "Temporary style for cleanup test."
      ("qq" "クリーン"))
    (unwind-protect
        (progn
          (nskk-converter-load-style 'test-define-clean-style)
          (should (nskk-converter-get-rule "qq")))
      (nskk-converter-load-style 'standard))
    ;; After restoring standard, "qq" should be gone
    (should-not (nskk-converter-get-rule "qq"))))

(nskk-describe "internal conversion"
  (nskk-it "converts simple input"
    (let ((result (nskk-convert-romaji--internal "ka")))
      (should (equal result "か"))))

  (nskk-it "converts compound input"
    (let ((result (nskk-convert-romaji--internal "kanji")))
      (should (equal result "かんじ"))))

  (nskk-it "converts double consonant (sokuon)"
    (let ((result (nskk-convert-romaji--internal "kka")))
      (should (equal result "っか")))))

;; TR-006: Direct nskk-convert-n--internal tests
(nskk-describe "nskk-convert-n--internal"
  (nskk-it "produces ん for standalone n at end of input"
    (let ((result (nskk-convert-n--internal "n")))
      (should result)
      (should (equal (car result) "ん"))
      (should-not (cdr result))))

  (nskk-it "produces ん for nn sequence"
    (let ((result (nskk-convert-n--internal "nn")))
      (should result)
      (should (equal (car result) "ん"))
      (should-not (cdr result))))

  (nskk-it "produces ん for nn with remainder"
    (let ((result (nskk-convert-n--internal "nnk")))
      (should result)
      (should (equal (car result) "ん"))
      (should (equal (cdr result) "nk"))))

  (nskk-it "produces ん for n-quote sequence"
    (let ((result (nskk-convert-n--internal "n'")))
      (should result)
      (should (equal (car result) "ん"))
      (should-not (cdr result))))

  (nskk-it "produces ん for n-quote with remainder"
    (let ((result (nskk-convert-n--internal "n'a")))
      (should result)
      (should (equal (car result) "ん"))
      (should (equal (cdr result) "a"))))

  (nskk-deftest-cases n-internal-before-consonant
    (("nb" . "b") ("nk" . "k") ("nm" . "m") ("np" . "p") ("nt" . "t"))
    :description "n before consonant produces ん with consonant as remainder"
    :body
    (let ((result (nskk-convert-n--internal input)))
      (should result)
      (should (equal (car result) "ん"))
      (should (equal (cdr result) expected))))

  (nskk-it "returns nil for n before vowel (falls through to table)"
    (should-not (nskk-convert-n--internal "na"))
    (should-not (nskk-convert-n--internal "ni"))
    (should-not (nskk-convert-n--internal "nu"))
    (should-not (nskk-convert-n--internal "ne"))
    (should-not (nskk-convert-n--internal "no")))

  (nskk-it "returns nil for n before y (falls through to table)"
    (should-not (nskk-convert-n--internal "nya"))
    (should-not (nskk-convert-n--internal "nyu"))))



;;;
;;; Property-Based Tests
;;;

;; Conversion determinism: same input always produces same output.
(nskk-property-test conversion-pbt-determinism
  ((input romaji-string))
  (let ((result1 (nskk-convert-romaji input))
        (result2 (nskk-convert-romaji input)))
    (equal result1 result2))
  100)

;; Output is always a string: nskk-convert-romaji always returns a string.
(nskk-property-test conversion-pbt-returns-string
  ((input romaji-string))
  (let ((result (nskk-convert-romaji input)))
    (stringp result))
  100)

;; Empty string: converting empty string returns empty string or nil gracefully
;; (no error). Use a fixed empty string — the generator drives the loop.
(nskk-property-test conversion-pbt-empty-string-no-crash
  ((input romaji-string))
  (condition-case nil
      (progn (nskk-convert-romaji input) t)
    (error nil))
  50)

;; Table-driven cases: known romaji->kana mappings
(nskk-deftest-cases conversion-pbt-known-romaji-kana
  (("ka"  . "か")
   ("ki"  . "き")
   ("ku"  . "く")
   ("sa"  . "さ")
   ("shi" . "し")
   ("tsu" . "つ")
   ("chi" . "ち"))
  :description "Known romaji→kana mapping"
  :body (should (equal expected (nskk-convert-romaji input))))

;;;
;;; Seeded Property-Based Tests (new)
;;;

;; Property: nskk-converter-convert returns nil or a cons where car is a string.
(nskk-property-test-seeded converter-pbt-convert-returns-string-or-nil
  ((input romaji-basic))
  (let ((result (nskk-converter-convert input)))
    (or (null result)
        (and (consp result)
             (or (stringp (car result))
                 (eq (car result) :incomplete)))))
  100 1001)

;; Property: nskk-converter-get-possible-completions for "k" always returns a list.
(nskk-property-test-seeded converter-pbt-completions-k-prefix-returns-list
  ((input romaji-basic))
  (let ((completions (nskk-converter-get-possible-completions "k")))
    (listp completions))
  50 1002)

;; Property: convert-is-deterministic — same input always gives same result (seeded).
(nskk-property-test-seeded converter-pbt-convert-is-deterministic
  ((input romaji-basic))
  (let ((result1 (nskk-converter-convert input))
        (result2 (nskk-converter-convert input)))
    (equal result1 result2))
  50 1003)

;; TR-008: Seeded consistency PBTs for CPS vs sync variants.

;; Property: nskk-converter-convert/k is consistent with its sync wrapper.
;; The callbacks mirror what the sync wrapper uses internally.
(nskk-property-test-seeded converter-pbt-convert/k-consistent-with-sync
  ((input romaji-basic))
  (equal (nskk-converter-convert input)
         (nskk-converter-convert/k input
           #'cons                             ; on-match: (cons kana remaining)
           (lambda (r) (cons :incomplete r))  ; on-incomplete: (:incomplete . romaji)
           (lambda () nil)))                  ; on-fail: nil
  50 2001)

;; Property: nskk-convert-romaji/k is consistent with its sync wrapper.
(nskk-property-test-seeded romaji/k-pbt-consistent-with-sync
  ((input romaji-string))
  (equal (nskk-convert-romaji input)
         (nskk-convert-romaji/k input #'identity))
  50 2002)

;;;
;;; Table-driven tests using nskk-should-convert-to
;;;

;; Ten known conversions not already covered by existing tests above.
(nskk-deftest-table converter-should-convert-to-known-cases
  :columns (romaji expected)
  :rows (("ge"  "げ")
         ("gi"  "ぎ")
         ("go"  "ご")
         ("gu"  "ぐ")
         ("ze"  "ぜ")
         ("zo"  "ぞ")
         ("de"  "で")
         ("do"  "ど")
         ("be"  "べ")
         ("pe"  "ぺ"))
  :description "Known romaji->kana conversions via nskk-should-convert-to"
  :body (nskk-should-convert-to romaji expected))

;;;
;;; FR-T-009: nskk--standard-romaji-rules content tests
;;;

(nskk-describe "nskk--standard-romaji-rules content"
  (nskk-it "is a non-empty list"
    (should (listp nskk--standard-romaji-rules))
    (should (> (length nskk--standard-romaji-rules) 0)))

  (nskk-it "contains only (romaji kana) string pairs"
    (dolist (rule nskk--standard-romaji-rules)
      (should (listp rule))
      (should (= (length rule) 2))
      (should (stringp (car rule)))
      (should (stringp (cadr rule)))))

  (nskk-deftest-table standard-romaji-rules-vowels
    :description "Standard rules contain the five Japanese vowels"
    :columns (romaji expected-kana)
    :rows (("a" "あ")
           ("i" "い")
           ("u" "う")
           ("e" "え")
           ("o" "お"))
    :body
    (let ((entry (assoc romaji nskk--standard-romaji-rules)))
      (should entry)
      (should (equal (cadr entry) expected-kana))))

  (nskk-deftest-table standard-romaji-rules-consonant-rows
    :description "Standard rules contain one entry per major consonant row"
    :columns (romaji expected-kana)
    :rows (("ka"  "か")
           ("sa"  "さ")
           ("ta"  "た")
           ("na"  "な")
           ("ha"  "は")
           ("ma"  "ま")
           ("ra"  "ら")
           ("wa"  "わ"))
    :body
    (let ((entry (assoc romaji nskk--standard-romaji-rules)))
      (should entry)
      (should (equal (cadr entry) expected-kana))))

  (nskk-deftest-table standard-romaji-rules-n-sequences
    :description "Standard rules contain hatsuon (ん) sequences"
    :columns (romaji expected-kana)
    :rows (("nn" "ん")
           ("n'" "ん"))
    :body
    (let ((entry (assoc romaji nskk--standard-romaji-rules)))
      (should entry)
      (should (equal (cadr entry) expected-kana))))

  (nskk-deftest-table standard-romaji-rules-special-digraphs
    :description "Standard rules contain common digraph sequences"
    :columns (romaji expected-kana)
    :rows (("shi" "し")
           ("chi" "ち")
           ("tsu" "つ")
           ("fu"  "ふ"))
    :body
    (let ((entry (assoc romaji nskk--standard-romaji-rules)))
      (should entry)
      (should (equal (cadr entry) expected-kana))))

  (nskk-it "contains the long vowel mark rule"
    (let ((entry (assoc "-" nskk--standard-romaji-rules)))
      (should entry)
      (should (equal (cadr entry) "ー"))))

  (nskk-it "has no duplicate romaji keys"
    ;; Each romaji string should appear at most once as the first element.
    ;; Note: some intentional aliases exist (e.g. "shi"/"si" → "し"),
    ;; so we only verify that the total rule count equals the unique key count.
    (let ((keys (mapcar #'car nskk--standard-romaji-rules)))
      (should (= (length keys) (length (cl-remove-duplicates keys :test #'equal))))))

  (nskk-it "all romaji keys contain only ASCII characters"
    (dolist (rule nskk--standard-romaji-rules)
      (let ((romaji (car rule)))
        (should (cl-every (lambda (c) (< c 128)) romaji)))))

  (nskk-it "all kana values contain at least one non-ASCII character"
    (dolist (rule nskk--standard-romaji-rules)
      (let ((kana (cadr rule)))
        (should (cl-some (lambda (c) (>= c 128)) kana))))))

;;;
;;; nskk-converter-define-rules
;;;

(nskk-describe "nskk-converter-define-rules"
  (nskk-it "is a macro (not a plain function)"
    (should (macrop 'nskk-converter-define-rules)))

  (nskk-it "adds multiple rules in one call"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (nskk-converter-define-rules
        ("xt" "てすと1")
        ("xr" "てすと2"))
      (should (equal (nskk-converter-get-rule "xt") "てすと1"))
      (should (equal (nskk-converter-get-rule "xr") "てすと2"))))

  (nskk-it "is equivalent to calling nskk-converter-add-rule for each pair"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      ;; Using the macro
      (nskk-converter-define-rules ("xp" "ぱてすと"))
      ;; Using the function directly
      (nskk-converter-add-rule "xq" "くてすと")
      ;; Both rules should be retrievable the same way
      (should (equal (nskk-converter-get-rule "xp") "ぱてすと"))
      (should (equal (nskk-converter-get-rule "xq") "くてすと"))))

  (nskk-it "with zero pairs expands to a no-op progn"
    ;; Calling with no pairs should not error
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (should (progn (nskk-converter-define-rules) t)))))

;;; nskk-convert-n--internal/k behavior is tested above via CPS variant tests.

;;;
;;; nskk-convert-romaji--internal/k CPS variant
;;;

(nskk-describe "nskk-convert-romaji--internal/k"
  (nskk-it "calls on-done with the converted kana string"
    (let (result)
      (nskk-convert-romaji--internal/k "ka"
                                       (lambda (s) (setq result s)))
      (should (equal result "か"))))

  (nskk-it "on-done receives a string for multi-syllable input"
    (let (result)
      (nskk-convert-romaji--internal/k "kana"
                                       (lambda (s) (setq result s)))
      (should (stringp result))
      (should (string-match-p "か" result))
      (should (string-match-p "な" result))))

  (nskk-it "is consistent with the sync nskk-convert-romaji--internal variant"
    (nskk-deftest-table converter-internal-cps-sync-consistency
      :columns (romaji)
      :rows (("ka") ("shi") ("tsu") ("kka"))
      :body (let (cps-result)
              (nskk-convert-romaji--internal/k romaji
                                               (lambda (s) (setq cps-result s)))
              (should (equal cps-result
                             (nskk-convert-romaji--internal romaji))))))

  (nskk-it "calls on-done exactly once"
    (let ((count 0))
      (nskk-convert-romaji--internal/k "ka"
                                       (lambda (_) (cl-incf count)))
      (should (= count 1)))))

;;;
;;; nskk-converter--populate-incomplete-markers
;;;

(nskk-describe "nskk-converter--populate-incomplete-markers"
  (nskk-it "marks romaji prefixes as :incomplete in the conversion table"
    ;; After initialization, prefixes like \"k\", \"sh\", \"ts\" must be :incomplete.
    ;; These are auto-derived from complete entries like \"ka\" -> \"か\".
    (should (eq (nskk-converter-lookup "k") :incomplete))
    (should (eq (nskk-converter-lookup "sh") :incomplete))
    (should (eq (nskk-converter-lookup "ts") :incomplete)))

  (nskk-it "does not overwrite complete entries with :incomplete"
    ;; Complete entries like \"ka\" -> \"か\" must keep their kana value.
    (should (equal (nskk-converter-lookup "ka") "か"))
    (should (equal (nskk-converter-lookup "shi") "し")))

  (nskk-it "returns nil for a key that is neither complete nor a romaji prefix"
    (should (null (nskk-converter-lookup "zzz")))))

(provide 'nskk-converter-test)

;;; nskk-converter-test.el ends here
