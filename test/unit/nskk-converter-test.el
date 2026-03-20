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
;;
;; Test categories:
;; - Basic conversion: vowels, consonant rows, palatal, special sequences
;; - Edge cases: nil/empty input, case insensitivity, boundary conditions
;; - Integration: full kana row conversion
;; - CPS variants: nskk-converter-convert/k, nskk-convert-romaji/k,
;;     nskk--convert-step-n/k, nskk-convert-romaji--internal/k,
;;     nskk-converter-load-style/k (on-found/on-not-found),
;;     defun/done /k forms (remove-rule/k, register-style/k, initialize/k)
;; - Internal functions: nskk-convert-romaji--internal, nskk--convert-step-n,
;;     nskk--converter-populate-incomplete-markers
;; - Rule management: add, remove, override, get (with Prolog DB isolation)
;; - Style system: register, load, define-style macro (with registry isolation)
;; - Data validation: nskk--standard-romaji-rules content and structure
;; - Macro validation: nskk-converter-define-rules, nskk-converter-define-style
;; - Property-based tests: determinism, string output, compression ratio, CPS/sync consistency
;; - Seeded PBTs: convert/k, romaji/k, get-rule/k, completions/k, step-n invariant
;; - Regression tests: double consonant, n-conversion, palatal, long strings, fallback path
;; - Performance tests: basic, complex, batch, long-input

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-converter)  ; Assumes converter implementation exists
(require 'nskk-kana)
(require 'nskk-pbt-generators)
(require 'cl-lib)


(nskk-describe "romaji basic conversion"
  (nskk-deftest-table converter-vowels
    :description "Converts vowels to hiragana"
    :columns (input expected)
    :rows (("a" "あ")
           ("i" "い")
           ("u" "う")
           ("e" "え")
           ("o" "お"))
    :body (should (equal expected (nskk-convert-romaji input))))

  (nskk-deftest-table converter-consonant-vowel
    :description "Converts consonant + vowel pairs to hiragana"
    :columns (input expected)
    :rows (("ka"  "か")
           ("ki"  "き")
           ("ku"  "く")
           ("ke"  "け")
           ("ko"  "こ")
           ("sa"  "さ")
           ("shi" "し")
           ("ta"  "た")
           ("chi" "ち")
           ("tsu" "つ")
           ("na"  "な")
           ("ni"  "に")
           ("ha"  "は")
           ("hi"  "ひ")
           ("fu"  "ふ")
           ("he"  "へ")
           ("ho"  "ほ")
           ("ma"  "ま")
           ("mi"  "み")
           ("ya"  "や")
           ("yu"  "ゆ")
           ("yo"  "よ")
           ("ra"  "ら")
           ("ri"  "り")
           ("ru"  "る")
           ("re"  "れ")
           ("ro"  "ろ")
           ("wa"  "わ")
           ("wo"  "を")
           ("n"   "ん"))
    :body (should (equal expected (nskk-convert-romaji input))))

  (nskk-deftest-table converter-voiced-consonants
    :description "Converts voiced and semi-voiced consonants to hiragana"
    :columns (input expected)
    :rows (("ga" "が")
           ("gi" "ぎ")
           ("gu" "ぐ")
           ("ge" "げ")
           ("go" "ご")
           ("za" "ざ")
           ("ji" "じ")
           ("zu" "ず")
           ("da" "だ")
           ("du" "づ")
           ("ba" "ば")
           ("bi" "び")
           ("bu" "ぶ")
           ("be" "べ")
           ("bo" "ぼ")
           ("pa" "ぱ")
           ("pi" "ぴ")
           ("pu" "ぷ")
           ("pe" "ぺ")
           ("po" "ぽ"))
    :body (should (equal expected (nskk-convert-romaji input))))

  (nskk-deftest-table converter-palatal-consonants
    :description "Converts palatal consonant combinations to hiragana"
    :columns (input expected)
    :rows (("kya" "きゃ")
           ("kyu" "きゅ")
           ("kyo" "きょ")
           ("sha" "しゃ")
           ("shu" "しゅ")
           ("sho" "しょ")
           ("cha" "ちゃ")
           ("chu" "ちゅ")
           ("cho" "ちょ")
           ("nya" "にゃ")
           ("nyu" "にゅ")
           ("nyo" "にょ")
           ("hya" "ひゃ")
           ("hyu" "ひゅ")
           ("hyo" "ひょ")
           ("mya" "みゃ")
           ("myu" "みゅ")
           ("myo" "みょ")
           ("rya" "りゃ")
           ("ryu" "りゅ")
           ("ryo" "りょ"))
    :body (should (equal expected (nskk-convert-romaji input))))

  (nskk-deftest-table converter-special-sequences
    :description "Converts special romaji sequences to hiragana"
    :columns (input expected)
    :rows (("nn"   "ん")
           ("n'"   "ん")
           ("kka"  "っか")
           ("sshi" "っし")
           ("tte"  "って")
           ("ppu"  "っぷ")
           ("xtsu" "っ")
           ("ya"   "や")
           ("yu"   "ゆ")
           ("yo"   "よ"))
    :body (should (equal expected (nskk-convert-romaji input))))

  (nskk-deftest-table converter-complete-words
    :description "Converts complete romaji words to hiragana"
    :columns (input expected)
    :rows (("nihongo"    "にほんご")
           ("konnichiwa" "こんにちわ")
           ("sayounara"  "さようなら")
           ("arigatou"   "ありがとう")
           ("sakana"     "さかな")
           ("yama"       "やま")
           ("kawa"       "かわ")
           ("sora"       "そら"))
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
    (nskk-prolog-test-with-isolated-db
      ;; Table should still work after multiple initialize calls
      (nskk-converter-initialize)
      (nskk-converter-initialize)
      (should (equal (nskk-converter-get-rule "ka") "か"))
      (should (equal (nskk-convert-romaji "ka") "か")))))


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
  (or (string-empty-p input)
      (not (string-empty-p (nskk-convert-romaji input))))
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

(nskk-deftest-performance conversion-long-input-performance
  "Long-string romaji-to-kana conversion completes within time budget."
  (let ((test-string "kakikukekokakikukekokakikukekokakikukeko"))
    (nskk-should-be-fast
     long-input-conversion 5000
     (dotimes (_ 1000)
       (nskk-convert-romaji test-string)))))


(nskk-describe "ddskk punctuation rules"
  (nskk-deftest-table converter-basic-punctuation
    :description "Basic punctuation keys convert to Japanese punctuation"
    :columns (input expected)
    :rows (("."  "。")
           (","  "、")
           ("["  "「")
           ("]"  "」"))
    :body (should (equal expected (nskk-convert-romaji input))))

  (nskk-deftest-table converter-z-prefix-symbols
    :description "z-prefix key sequences convert to Japanese symbols"
    :columns (input expected)
    :rows (("z-" "〜")
           ("z." "…")
           ("z," "‥")
           ("z[" "『")
           ("z]" "』")
           ("z/" "・")
           ("zh" "←")
           ("zj" "↓")
           ("zk" "↑")
           ("zl" "→")
           ("z " "　"))
    :body (should (equal expected (nskk-convert-romaji input))))

  (nskk-it "katakana-passthrough: symbols produced by punctuation rules are not altered by hiragana-to-katakana conversion"
    ;; nskk-kana-string-hiragana-to-katakana leaves non-hiragana characters unchanged.
    ;; This confirms punctuation rules behave identically in hiragana and katakana modes.
    (dolist (pair '(("。" . "。") ("、" . "、") ("「" . "「") ("」" . "」")
                    ("〜" . "〜") ("…" . "…") ("‥" . "‥") ("『" . "『") ("』" . "』")
                    ("・" . "・") ("←" . "←") ("↓" . "↓") ("↑" . "↑") ("→" . "→")
                    ("　" . "　")))
      (should (equal (cdr pair)
                     (nskk-kana-string-hiragana-to-katakana (car pair)))))))

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



;;;;
;;;; Test Suite Organization
;;;;

(declare-function nskk-performance-conversion-basic-performance nil)
(declare-function nskk-performance-conversion-complex-performance nil)
(declare-function nskk-performance-conversion-batch-performance nil)
(declare-function nskk-performance-conversion-long-input-performance nil)
(nskk-test-suite converter-performance
  nskk-performance-conversion-basic-performance
  nskk-performance-conversion-complex-performance
  nskk-performance-conversion-batch-performance
  nskk-performance-conversion-long-input-performance)


(nskk-describe "converter-convert function"
  (nskk-it "returns kana and empty remainder for a complete romaji match"
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

  (nskk-it "returns kana and empty remainder for a three-character digraph match"
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
  (nskk-deftest-table lookup-complete-match
    :description "Returns kana string for complete romaji matches"
    :columns (input expected)
    :rows (("a"   "あ")
           ("ka"  "か")
           ("sha" "しゃ")
           ("tsu" "つ")
           ("-"   "ー"))
    :body (should (equal expected (nskk-converter-lookup input))))

  (nskk-deftest-table lookup-incomplete-prefix
    :description "Returns :incomplete for known romaji prefixes"
    :columns (input expected)
    :rows (("k"  :incomplete)
           ("s"  :incomplete)
           ("sh" :incomplete)
           ("ky" :incomplete))
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

  (nskk-deftest-table convert/k-match-cases
    :description "nskk-converter-convert/k on-match path for known syllables"
    :columns (input expected)
    :rows (("sha" "しゃ") ("tsu" "つ") ("chi" "ち") ("a" "あ"))
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
      (nskk-convert-romaji/k "ka" (lambda (kana) (setq result kana)) #'ignore)
      (should (equal result "か"))))

  (nskk-it "calls continuation with nil for nil input"
    (let (result called)
      (nskk-convert-romaji/k nil (lambda (kana) (setq result kana called t)) #'ignore)
      (should called)
      (should-not result)))

  (nskk-it "calls continuation with empty string for empty input"
    (let (result)
      (nskk-convert-romaji/k "" (lambda (kana) (setq result kana)) #'ignore)
      (should (equal result ""))))

  (nskk-deftest-table romaji/k-full-conversion
    :description "nskk-convert-romaji/k produces same output as sync wrapper"
    :columns (input expected)
    :rows (("nihongo" "にほんご")
           ("konnichiwa" "こんにちわ")
           ("aiueo" "あいうえお"))
    :body
    (let (result)
      (nskk-convert-romaji/k input (lambda (kana) (setq result kana)) #'ignore)
      (should (equal result expected)))))

(nskk-describe "nskk--convert-step-n/k CPS variant"
  (nskk-it "calls on-kana for standalone n"
    (let (got-kana got-rest)
      (nskk--convert-step-n/k "n"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_remaining) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should-not got-rest)))

  (nskk-it "calls on-kana for n before consonant"
    (let (got-kana got-rest)
      (nskk--convert-step-n/k "nb"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_remaining) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should (equal got-rest "b"))))

  (nskk-it "calls on-kana for n before vowel via trie delegation"
    (let (got-kana got-rest)
      (nskk--convert-step-n/k "na"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_remaining) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "な"))
      (should (equal got-rest ""))))

  (nskk-it "calls on-kana for n-quote sequence"
    (let (got-kana got-rest)
      (nskk--convert-step-n/k "n'"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_remaining) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should-not got-rest)))

  (nskk-it "calls on-kana for nn with remainder"
    (let (got-kana got-rest)
      (nskk--convert-step-n/k "nnk"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_remaining) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should (equal got-rest "nk")))))

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
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (nskk-converter-add-rule "testkey" "テスト")
      (should (equal (nskk-converter-get-rule "testkey") "テスト"))))

  (nskk-it "removes a conversion rule"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (nskk-converter-add-rule "tempkey" "テンプ")
      (should (nskk-converter-get-rule "tempkey"))
      (nskk-converter-remove-rule "tempkey")
      (should-not (nskk-converter-get-rule "tempkey"))))

  (nskk-it "gets an existing rule"
    (should (equal (nskk-converter-get-rule "ka") "か")))

  (nskk-it "returns nil for nonexistent rule"
    (should-not (nskk-converter-get-rule "nonexistent-romaji-key")))

  (nskk-it "overrides an existing rule"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (nskk-converter-add-rule "ka" "カ")
      (should (equal (nskk-converter-get-rule "ka") "カ")))))

(nskk-describe "style system"
  (nskk-it "registers and loads a new style"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (let ((nskk--style-registry nskk--style-registry)
            (test-style-called nil))
        (nskk-converter-register-style 'test-style
          (lambda () (setq test-style-called t)))
        (nskk-converter-load-style 'test-style)
        (should test-style-called))))

  (nskk-it "loads the standard style"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (should (eq (nskk-converter-load-style 'standard) 'standard))
      ;; Verify basic rules still work
      (should (equal (nskk-converter-get-rule "ka") "か"))))

  (nskk-it "calls on-not-found for unknown style"
    (let (not-found-called)
      (nskk-converter-load-style/k 'nonexistent-style
        (lambda (_s) (should nil))
        (lambda () (setq not-found-called t)))
      (should not-found-called)))

  (nskk-it "calls on-found with style symbol on success"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (let (got-style)
        (nskk-converter-load-style/k 'standard
          (lambda (s) (setq got-style s))
          (lambda () (should nil)))
        (should (eq got-style 'standard)))))

  (nskk-it "clears and replaces the table when loading a style"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (let ((nskk--style-registry nskk--style-registry))
        (nskk-converter-register-style 'minimal-test
          (lambda ()
            (nskk-converter-add-rule "x" "エックス")))
        (nskk-converter-load-style 'minimal-test)
        (should (equal (nskk-converter-get-rule "x") "エックス"))
        ;; Standard rules should be gone
        (should-not (equal (nskk-converter-get-rule "ka") "か"))))))

;; TR-005: nskk-converter-define-style macro tests
(nskk-describe "nskk-converter-define-style macro"
  (nskk-it "generates an init function and registers the style"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (let ((nskk--style-registry nskk--style-registry))
        (nskk-converter-define-style test-define-macro-style
          "Temporary style for macro validation test."
          ("zz" "zzテスト"))
        (nskk-converter-load-style 'test-define-macro-style)
        ;; The macro-generated init function should have asserted this rule
        (should (equal (nskk-converter-get-rule "zz") "zzテスト"))
        ;; Standard rules should not be present
        (should-not (equal (nskk-converter-get-rule "ka") "か")))))

  (nskk-it "loads and unloads cleanly"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (let ((nskk--style-registry nskk--style-registry))
        (nskk-converter-define-style test-define-clean-style
          "Temporary style for cleanup test."
          ("qq" "クリーン"))
        (nskk-converter-load-style 'test-define-clean-style)
        (should (nskk-converter-get-rule "qq"))
        (nskk-converter-load-style 'standard)
        ;; After loading standard, "qq" should be gone
        (should-not (nskk-converter-get-rule "qq"))))))

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

;; TR-006: Direct nskk--convert-step-n tests
(nskk-describe "nskk--convert-step-n sync wrapper"
  (nskk-it "produces ん for standalone n"
    (let (got-kana got-rest)
      (nskk--convert-step-n/k "n"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "ん"))
      (should-not got-rest)))

  (nskk-it "produces ん for nn sequence"
    (let (got-kana got-rest)
      (nskk--convert-step-n/k "nn"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "ん"))
      (should-not got-rest)))

  (nskk-it "produces ん for nn with remainder"
    (let (got-kana got-rest)
      (nskk--convert-step-n/k "nnk"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "ん"))
      (should (equal got-rest "nk"))))

  (nskk-it "produces ん for n-quote sequence"
    (let (got-kana got-rest)
      (nskk--convert-step-n/k "n'"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "ん"))
      (should-not got-rest)))

  (nskk-it "produces ん for n-quote with remainder"
    (let (got-kana got-rest)
      (nskk--convert-step-n/k "n'a"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "ん"))
      (should (equal got-rest "a"))))

  (nskk-deftest-table step-n-before-consonant
    :description "n before consonant calls on-kana with ん and consonant remainder"
    :columns (input expected)
    :rows (("nb" "b") ("nk" "k") ("nm" "m") ("np" "p") ("nt" "t"))
    :body
    (let (got-kana got-rest)
      (nskk--convert-step-n/k input
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "ん"))
      (should (equal got-rest expected))))

  (nskk-it "n before vowel delegates to trie and produces kana"
    (let (got-kana)
      (nskk--convert-step-n/k "na"
        (lambda (kana _rest) (setq got-kana kana))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "な")))
    (let (got-kana)
      (nskk--convert-step-n/k "ni"
        (lambda (kana _rest) (setq got-kana kana))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "に")))
    (let (got-kana)
      (nskk--convert-step-n/k "nu"
        (lambda (kana _rest) (setq got-kana kana))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "ぬ")))
    (let (got-kana)
      (nskk--convert-step-n/k "ne"
        (lambda (kana _rest) (setq got-kana kana))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "ね")))
    (let (got-kana)
      (nskk--convert-step-n/k "no"
        (lambda (kana _rest) (setq got-kana kana))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "の"))))

  (nskk-it "n before y delegates to trie and produces kana"
    (let (got-kana)
      (nskk--convert-step-n/k "nya"
        (lambda (kana _rest) (setq got-kana kana))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "にゃ")))
    (let (got-kana)
      (nskk--convert-step-n/k "nyu"
        (lambda (kana _rest) (setq got-kana kana))
        (lambda (_r) nil)
        (lambda () nil))
      (should (equal got-kana "にゅ")))))



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

;; Crash safety: nskk-convert-romaji never signals for any random romaji input.
;; The generator drives varied inputs; this property verifies no-exception contract.
(nskk-property-test conversion-pbt-no-crash-on-arbitrary-input
  ((input romaji-string))
  (condition-case nil
      (progn (nskk-convert-romaji input) t)
    (error nil))
  50)

;; Table-driven cases: known romaji->kana mappings
(nskk-deftest-table conversion-pbt-known-romaji-kana
  :description "Known romaji→kana mapping"
  :columns (input expected)
  :rows (("ka"  "か")
         ("ki"  "き")
         ("ku"  "く")
         ("sa"  "さ")
         ("shi" "し")
         ("tsu" "つ")
         ("chi" "ち"))
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

;; Property: nskk-converter-get-possible-completions returns nil or a list of cons pairs.
(nskk-property-test-seeded converter-pbt-completions-returns-list-or-nil
  ((input romaji-basic))
  (let ((completions (nskk-converter-get-possible-completions input)))
    (or (null completions)
        (and (listp completions)
             (cl-every #'consp completions))))
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
         (nskk-convert-romaji/k input #'identity (lambda () nil)))
  50 2002)

;; Property: nskk-converter-get-rule/k is consistent with its sync wrapper.
(nskk-property-test-seeded get-rule/k-pbt-consistent-with-sync
  ((input romaji-basic))
  (equal (nskk-converter-get-rule input)
         (nskk-converter-get-rule/k input #'identity (lambda () nil)))
  50 3001)

;; Property: nskk-converter-get-possible-completions/k is consistent with its sync wrapper.
(nskk-property-test-seeded get-possible-completions/k-pbt-consistent-with-sync
  ((input romaji-basic))
  (equal (nskk-converter-get-possible-completions input)
         (nskk-converter-get-possible-completions/k input #'identity (lambda () nil)))
  50 3002)

;; Property: for any string starting with ?n, nskk--convert-step-n/k calls
;; exactly one continuation with appropriate argument types.
(nskk-property-test-seeded step-n-pbt-calls-one-continuation
  ((input romaji-basic))
  (let ((s (concat "n" input))
        (call-count 0))
    (nskk--convert-step-n/k s
      (lambda (kana rest)
        (cl-incf call-count)
        (and (stringp kana)
             (or (null rest) (stringp rest))))
      (lambda (_remaining) (cl-incf call-count) t)
      (lambda () (cl-incf call-count) t))
    (= call-count 1))
  100 3003)

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
    (should nskk--standard-romaji-rules))

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

  (nskk-deftest-table standard-romaji-rules-basic-punctuation
    :description "Standard rules contain ddskk-compatible basic punctuation"
    :columns (romaji expected-kana)
    :rows (("." "。")
           ("," "、")
           ("[" "「")
           ("]" "」"))
    :body
    (let ((entry (assoc romaji nskk--standard-romaji-rules)))
      (should entry)
      (should (equal (cadr entry) expected-kana))))

  (nskk-deftest-table standard-romaji-rules-z-prefix-symbols
    :description "Standard rules contain ddskk-compatible z-prefix symbols"
    :columns (romaji expected-kana)
    :rows (("z-" "〜")
           ("z." "…")
           ("z," "‥")
           ("z[" "『")
           ("z]" "』")
           ("z/" "・")
           ("zh" "←")
           ("zj" "↓")
           ("zk" "↑")
           ("zl" "→")
           ("z " "　"))
    :body
    (let ((entry (assoc romaji nskk--standard-romaji-rules)))
      (should entry)
      (should (equal (cadr entry) expected-kana))))

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

;;; nskk--convert-step-n/k behavior is tested above via CPS variant tests.

;;;
;;; nskk-convert-romaji--internal/k CPS variant
;;;

(nskk-describe "nskk-convert-romaji--internal/k"
  (nskk-it "calls on-done with the converted kana string"
    (let (result)
      (nskk-convert-romaji--internal/k "ka"
                                       (lambda (s) (setq result s))
                                       #'ignore)
      (should (equal result "か"))))

  (nskk-it "on-done receives a string for multi-syllable input"
    (let (result)
      (nskk-convert-romaji--internal/k "kana"
                                       (lambda (s) (setq result s))
                                       #'ignore)
      (should (stringp result))
      (should (string-match-p "か" result))
      (should (string-match-p "な" result))))

  (nskk-it "is consistent with the sync nskk-convert-romaji--internal variant"
    (nskk-deftest-table converter-internal-cps-sync-consistency
      :columns (romaji)
      :rows (("ka") ("shi") ("tsu") ("kka"))
      :body (let (cps-result)
              (nskk-convert-romaji--internal/k romaji
                                               (lambda (s) (setq cps-result s))
                                               #'ignore)
              (should (equal cps-result
                             (nskk-convert-romaji--internal romaji))))))

  (nskk-it "calls on-done exactly once"
    (let ((count 0))
      (nskk-convert-romaji--internal/k "ka"
                                       (lambda (_) (cl-incf count))
                                       #'ignore)
      (should (= count 1)))))

;;;
;;; defun/done /k variant tests
;;;

(nskk-describe "defun/done /k variants"
  (nskk-it "nskk-converter-remove-rule/k calls on-done with no arguments"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (nskk-converter-add-rule "tempkey/k" "テンプ")
      (let (done-called)
        (nskk-converter-remove-rule/k "tempkey/k"
          (lambda () (setq done-called t)))
        (should done-called)
        (should-not (nskk-converter-get-rule "tempkey/k")))))

  (nskk-it "nskk-converter-register-style/k calls on-done with no arguments"
    (let ((nskk--style-registry nskk--style-registry)
          (done-called nil))
      (nskk-converter-register-style/k 'test-register-style/k
        (lambda () t)
        (lambda () (setq done-called t)))
      (should done-called)))

  (nskk-it "nskk-converter-initialize/k calls on-done with no arguments"
    (nskk-prolog-test-with-isolated-db
      (let (done-called)
        (nskk-converter-initialize/k
          (lambda () (setq done-called t)))
        (should done-called)
        ;; Table should be populated after initialization
        (should (equal (nskk-converter-get-rule "ka") "か"))))))

;;;
;;; nskk--converter-populate-incomplete-markers
;;;

(nskk-describe "nskk--converter-populate-incomplete-markers"
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

;;;
;;; nskk--sokuon-p unit tests
;;;

(nskk-describe "nskk--sokuon-p"
  (nskk-it "returns non-nil for doubled k (kka)"
    (should (nskk--sokuon-p ?k "kka")))

  (nskk-it "returns non-nil for doubled t (tta)"
    (should (nskk--sokuon-p ?t "tta")))

  (nskk-it "returns non-nil for doubled s (ssa)"
    (should (nskk--sokuon-p ?s "ssa")))

  (nskk-it "returns nil for doubled n (nna) — n is in sokuon-blockers"
    (should-not (nskk--sokuon-p ?n "nna")))

  (nskk-it "returns nil for doubled a (aaa) — vowels are in sokuon-blockers"
    (should-not (nskk--sokuon-p ?a "aaa")))

  (nskk-it "returns nil when c0 does not match first char of remaining (k vs ka)"
    (should-not (nskk--sokuon-p ?k "ka")))

  (nskk-it "returns nil when remaining is too short (length < 2)"
    (should-not (nskk--sokuon-p ?k "k")))

  (nskk-it "returns nil for doubled i — vowel blocker"
    (should-not (nskk--sokuon-p ?i "ii")))

  (nskk-it "returns nil for doubled u — vowel blocker"
    (should-not (nskk--sokuon-p ?u "uu")))

  (nskk-it "returns nil for doubled e — vowel blocker"
    (should-not (nskk--sokuon-p ?e "ee")))

  (nskk-it "returns nil for doubled o — vowel blocker"
    (should-not (nskk--sokuon-p ?o "oo")))

  (nskk-it "returns nil for non-ASCII character — ASCII guard"
    ;; ?あ = #x3042, well above 128; ASCII guard must exclude it
    (should-not (nskk--sokuon-p ?あ "ああ"))))


;;;
;;; nskk--convert-step/k unit tests
;;;

(nskk-describe "nskk--convert-step/k"
  ;; on-kana: sokuon branch
  (nskk-it "calls on-kana with (っ ka) for doubled k (kka)"
    (let (got-kana got-rest)
      (nskk--convert-step/k "kka"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "っ"))
      (should (equal got-rest "ka"))))

  (nskk-it "calls on-kana with (っ ta) for doubled t (tta)"
    (let (got-kana got-rest)
      (nskk--convert-step/k "tta"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "っ"))
      (should (equal got-rest "ta"))))

  ;; on-kana: n-prefix → ん before consonant
  (nskk-it "calls on-kana with (ん b) for nb"
    (let (got-kana got-rest)
      (nskk--convert-step/k "nb"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should (equal got-rest "b"))))

  (nskk-it "calls on-kana with (ん k) for nk"
    (let (got-kana got-rest)
      (nskk--convert-step/k "nk"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should (equal got-rest "k"))))

  ;; on-kana: n-prefix falls through to table (na, ni, etc.)
  (nskk-it "calls on-kana with (な empty) for na"
    (let (got-kana got-rest)
      (nskk--convert-step/k "na"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "な"))
      (should (equal got-rest ""))))

  (nskk-it "calls on-kana with (に empty) for ni"
    (let (got-kana got-rest)
      (nskk--convert-step/k "ni"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "に"))
      (should (equal got-rest ""))))

  ;; on-kana: normal table match
  (nskk-it "calls on-kana with (か empty) for ka"
    (let (got-kana got-rest)
      (nskk--convert-step/k "ka"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "か"))
      (should (equal got-rest ""))))

  (nskk-it "calls on-kana with (しゃ empty) for sha"
    (let (got-kana got-rest)
      (nskk--convert-step/k "sha"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "しゃ"))
      (should (equal got-rest ""))))

  ;; on-partial: incomplete prefix
  (nskk-it "calls on-partial with k for incomplete prefix k"
    (let (got-partial)
      (nskk--convert-step/k "k"
        (lambda (_kana _rest) (should nil))
        (lambda (partial) (setq got-partial partial))
        (lambda () (should nil)))
      (should (equal got-partial "k"))))

  (nskk-it "calls on-partial with sh for incomplete prefix sh"
    (let (got-partial)
      (nskk--convert-step/k "sh"
        (lambda (_kana _rest) (should nil))
        (lambda (partial) (setq got-partial partial))
        (lambda () (should nil)))
      (should (equal got-partial "sh"))))

  ;; on-fail: no match
  (nskk-it "calls on-fail for digit input with no romaji entry (2)"
    (let (fail-called)
      (nskk--convert-step/k "2"
        (lambda (_kana _rest) (should nil))
        (lambda (_partial) (should nil))
        (lambda () (setq fail-called t)))
      (should fail-called)))

  (nskk-it "calls on-fail for digit input with no romaji entry (8)"
    (let (fail-called)
      (nskk--convert-step/k "8"
        (lambda (_kana _rest) (should nil))
        (lambda (_partial) (should nil))
        (lambda () (setq fail-called t)))
      (should fail-called)))

  (nskk-it "calls on-kana with (ん nil) for standalone n"
    (let (got-kana got-rest)
      (nskk--convert-step/k "n"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should (null got-rest))))

  (nskk-it "calls on-kana with (ん nil) for nn"
    (let (got-kana got-rest)
      (nskk--convert-step/k "nn"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should (null got-rest))))

  (nskk-it "calls on-kana with (ん nil) for n-quote"
    (let (got-kana got-rest)
      (nskk--convert-step/k "n'"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should (null got-rest))))

  (nskk-it "calls on-kana with (ん a) for n-quote-a — apostrophe consumed"
    (let (got-kana got-rest)
      (nskk--convert-step/k "n'a"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should (equal got-rest "a"))))

  (nskk-it "calls on-kana with (ん nk) for nnk — nn consumed, nk remains"
    (let (got-kana got-rest)
      (nskk--convert-step/k "nnk"
        (lambda (kana rest) (setq got-kana kana got-rest rest))
        (lambda (_partial) (should nil))
        (lambda () (should nil)))
      (should (equal got-kana "ん"))
      (should (equal got-rest "nk")))))


;;;
;;; Seeded PBTs for nskk--convert-step/k
;;;

;; Property: nskk--convert-step/k always calls exactly one continuation.
(nskk-property-test-seeded convert-step/k-pbt-calls-exactly-one-continuation
  ((input romaji-basic))
  (when (and (stringp input) (not (string-empty-p input)))
    (let ((call-count 0))
      (nskk--convert-step/k input
        (lambda (_kana _rest) (cl-incf call-count))
        (lambda (_partial)    (cl-incf call-count))
        (lambda ()            (cl-incf call-count)))
      (= call-count 1)))
  50 4001)

;; Property: nskk--convert-step/k on-kana always receives a non-empty kana string.
(nskk-property-test-seeded convert-step/k-pbt-on-kana-receives-string
  ((input romaji-basic))
  (when (and (stringp input) (not (string-empty-p input)))
    (let ((result t))
      (nskk--convert-step/k input
        (lambda (kana _rest) (setq result (and (stringp kana) (not (string-empty-p kana)))))
        (lambda (_partial) t)
        (lambda () t))
      result))
  50 4002)

;; Property: nskk--convert-step/k is deterministic — calling it twice on the
;; same input yields the same continuation dispatch.  (defun/3k has no sync
;; wrapper, so the old sync-vs-CPS consistency check does not apply.)
(nskk-property-test-seeded convert-step/k-pbt-consistent-with-sync
  ((input romaji-basic))
  (when (and (stringp input) (not (string-empty-p input)))
    (let ((result1 nil)
          (result2 nil))
      (nskk--convert-step/k input
        (lambda (kana rest)  (setq result1 (list :match kana rest)))
        (lambda (partial)    (setq result1 (list :partial partial)))
        (lambda ()           (setq result1 (list :fail))))
      (nskk--convert-step/k input
        (lambda (kana rest)  (setq result2 (list :match kana rest)))
        (lambda (partial)    (setq result2 (list :partial partial)))
        (lambda ()           (setq result2 (list :fail))))
      (equal result1 result2)))
  50 4003)

;;; Prolog fact table initialization tests

(nskk-describe "sokuon-blocker and hatsuon-blocker Prolog tables"
  (nskk-it "Vowels a i u e o are in sokuon-blocker table after initialization."
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (should (nskk-prolog-holds-p `(sokuon-blocker ,?a)))
      (should (nskk-prolog-holds-p `(sokuon-blocker ,?i)))
      (should (nskk-prolog-holds-p `(sokuon-blocker ,?u)))
      (should (nskk-prolog-holds-p `(sokuon-blocker ,?e)))
      (should (nskk-prolog-holds-p `(sokuon-blocker ,?o)))))

  (nskk-it "Character n is in sokuon-blocker table after initialization."
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (should (nskk-prolog-holds-p `(sokuon-blocker ,?n)))))

  (nskk-it "Typical consonants k s t are NOT in sokuon-blocker (they trigger sokuon)."
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (should-not (nskk-prolog-holds-p `(sokuon-blocker ,?k)))
      (should-not (nskk-prolog-holds-p `(sokuon-blocker ,?s)))
      (should-not (nskk-prolog-holds-p `(sokuon-blocker ,?t)))))

  (nskk-it "Vowels and y are in hatsuon-blocker table after initialization."
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (should (nskk-prolog-holds-p `(hatsuon-blocker ,?a)))
      (should (nskk-prolog-holds-p `(hatsuon-blocker ,?i)))
      (should (nskk-prolog-holds-p `(hatsuon-blocker ,?u)))
      (should (nskk-prolog-holds-p `(hatsuon-blocker ,?e)))
      (should (nskk-prolog-holds-p `(hatsuon-blocker ,?o)))
      (should (nskk-prolog-holds-p `(hatsuon-blocker ,?y)))))

  (nskk-it "Characters n and apostrophe are in hatsuon-blocker after initialization."
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (should (nskk-prolog-holds-p `(hatsuon-blocker ,?n)))
      (should (nskk-prolog-holds-p `(hatsuon-blocker ,?\')))))

  (nskk-it "Typical consonants k s t are NOT in hatsuon-blocker (they trigger ん)."
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (should-not (nskk-prolog-holds-p `(hatsuon-blocker ,?k)))
      (should-not (nskk-prolog-holds-p `(hatsuon-blocker ,?s)))
      (should-not (nskk-prolog-holds-p `(hatsuon-blocker ,?t))))))

(nskk-describe "nskk-convert-romaji/k via nskk-it-k"
  (nskk-it-k "converts hiragana romaji via nskk-convert-romaji/k"
    (nskk-convert-romaji/k "ka")
    :found (result)
      (should (stringp result))
      (should (equal result "か"))
    :not-found ()
      (ert-fail "nskk-convert-romaji/k must always call on-found")))

(nskk-describe "vowel-char and uppercase-vowel-char Prolog tables"
  (nskk-it "Lowercase vowels a i u e o are in vowel-char table after initialization."
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (should (nskk-prolog-holds-p `(vowel-char ,?a)))
      (should (nskk-prolog-holds-p `(vowel-char ,?i)))
      (should (nskk-prolog-holds-p `(vowel-char ,?u)))
      (should (nskk-prolog-holds-p `(vowel-char ,?e)))
      (should (nskk-prolog-holds-p `(vowel-char ,?o)))))

  (nskk-it "Typical consonants k s t are NOT in vowel-char table."
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (should-not (nskk-prolog-holds-p `(vowel-char ,?k)))
      (should-not (nskk-prolog-holds-p `(vowel-char ,?s)))
      (should-not (nskk-prolog-holds-p `(vowel-char ,?t)))))

  (nskk-it "Uppercase vowels A I U E O are in uppercase-vowel-char table after initialization."
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (should (nskk-prolog-holds-p `(uppercase-vowel-char ,?A)))
      (should (nskk-prolog-holds-p `(uppercase-vowel-char ,?I)))
      (should (nskk-prolog-holds-p `(uppercase-vowel-char ,?U)))
      (should (nskk-prolog-holds-p `(uppercase-vowel-char ,?E)))
      (should (nskk-prolog-holds-p `(uppercase-vowel-char ,?O)))))

  (nskk-it "Uppercase consonants K S T are NOT in uppercase-vowel-char table."
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (should-not (nskk-prolog-holds-p `(uppercase-vowel-char ,?K)))
      (should-not (nskk-prolog-holds-p `(uppercase-vowel-char ,?S)))
      (should-not (nskk-prolog-holds-p `(uppercase-vowel-char ,?T))))))

(provide 'nskk-converter-test)

;;; nskk-converter-test.el ends here
