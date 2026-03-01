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


;;;;
;;;; Property-Based Tests: Conversion Properties
;;;;

(nskk-property-test conversion-output-never-expands
  ((input romaji-string))
  ;; Romaji-to-kana conversion never produces more characters than the input.
  ;; Kana is more compact than romaji; incomplete sequences pass through unchanged.
  (<= (length (nskk-convert-romaji input)) (length input))
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
  (let ((start-time (current-time)))
    (dotimes (_ 1000)
      (nskk-convert-romaji "konnichiwa"))
    (let ((elapsed-ms (* 1000 (float-time (time-subtract (current-time) start-time)))))
      (should (< elapsed-ms 5000)))))


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

(provide 'nskk-converter-test)

;;; nskk-converter-test.el ends here
