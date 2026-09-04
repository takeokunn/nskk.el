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
;;; Code:
(require 'ert)

(require 'nskk-test-framework)

(require 'nskk-test-macros)

(require 'nskk-converter) ; Assumes converter implementation exists

(require 'nskk-kana)

(require 'nskk-pbt-generators)

(progn (require (quote cl-lib)) (defvar nskk-mode-map))

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
    (should (equal (nskk-convert-romaji "kan") "かん"))
    (should (equal (nskk-convert-romaji "kk") "っk"))
    (should (equal (nskk-convert-romaji "kya") "きゃ"))))

(nskk-describe "romaji-to-kana integration"
  (nskk-it "converts complete kana rows"
    (should (equal (nskk-convert-romaji "aiueo") "あいうえお"))
    (should (equal (nskk-convert-romaji "kakikukeko") "かきくけこ"))
    (should (equal (nskk-convert-romaji "sashisuseso") "さしすせそ"))))

(nskk-describe "converter-initialize"
  (nskk-it "is idempotent: subsequent calls are no-ops"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (nskk-converter-initialize)
      (should (equal (nskk-converter-get-rule "ka") "か"))
      (should (equal (nskk-convert-romaji "ka") "か")))))

;;;;
;;;; Property-Based Tests: Conversion Properties
;;;;
(nskk-property-test conversion-output-never-expands
  ((input romaji-string))
  (<= (length (nskk-convert-romaji input)) (length input))
  100)

(nskk-property-test conversion-min-compression-ratio
  ((input romaji-string))
  (>= (length (nskk-convert-romaji input)) (/ (length input) 4))
  100)

(nskk-property-test conversion-non-empty-output
  ((input romaji-string))
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
    (let ((long-romaji "aiueoaiueoaiueoaiueoaiueo")
          (expected    "あいうえおあいうえおあいうえおあいうえおあいうえお"))
      (should (equal (nskk-convert-romaji long-romaji) expected)))
    (should (equal (nskk-convert-romaji "kakikukekokakikukekokakikukeko")
                   "かきくけこかきくけこかきくけこ"))))

(nskk-describe "regression: fallback path"
  (nskk-it "appends unconvertible tail verbatim (internal-fallback-001)"
    (should (equal (nskk-convert-romaji "xyz") "xyz"))
    (should (equal (nskk-convert-romaji "kaxyz") "かxyz"))
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
        (should-not (equal (nskk-converter-get-rule "ka") "か"))))))

(nskk-describe "nskk-converter-define-style macro"
  (nskk-it "generates an init function and registers the style"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (let ((nskk--style-registry nskk--style-registry))
        (nskk-converter-define-style test-define-macro-style
          "Temporary style for macro validation test."
          ("zz" "zzテスト"))
        (nskk-converter-load-style 'test-define-macro-style)
        (should (equal (nskk-converter-get-rule "zz") "zzテスト"))
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
;;;
(nskk-property-test conversion-pbt-determinism
  ((input romaji-string))
  (let ((result1 (nskk-convert-romaji input))
        (result2 (nskk-convert-romaji input)))
    (equal result1 result2))
  100)

(nskk-property-test conversion-pbt-returns-string
  ((input romaji-string))
  (let ((result (nskk-convert-romaji input)))
    (stringp result))
  100)

(nskk-property-test conversion-pbt-no-crash-on-arbitrary-input
  ((input romaji-string))
  (condition-case nil
      (progn (nskk-convert-romaji input) t)
    (error nil))
  50)

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
(nskk-property-test-seeded converter-pbt-convert-returns-string-or-nil
  ((input romaji-basic))
  (let ((result (nskk-converter-convert input)))
    (or (null result)
        (and (consp result)
             (or (stringp (car result))
                 (eq (car result) :incomplete)))))
  100 1001)

(nskk-property-test-seeded converter-pbt-completions-returns-list-or-nil
  ((input romaji-basic))
  (let ((completions (nskk-converter-get-possible-completions input)))
    (or (null completions)
        (and (listp completions)
             (cl-every #'consp completions))))
  50 1002)

(nskk-property-test-seeded converter-pbt-convert-is-deterministic
  ((input romaji-basic))
  (let ((result1 (nskk-converter-convert input))
        (result2 (nskk-converter-convert input)))
    (equal result1 result2))
  50 1003)

(nskk-property-test-seeded converter-pbt-convert/k-consistent-with-sync
  ((input romaji-basic))
  (equal (nskk-converter-convert input)
         (nskk-converter-convert/k input
           #'cons                             ; on-match: (cons kana remaining)
           (lambda (r) (cons :incomplete r))  ; on-incomplete: (:incomplete . romaji)
           (lambda () nil)))                  ; on-fail: nil
  50 2001)

(nskk-property-test-seeded romaji/k-pbt-consistent-with-sync
  ((input romaji-string))
  (equal (nskk-convert-romaji input)
         (nskk-convert-romaji/k input #'identity (lambda () nil)))
  50 2002)

(nskk-property-test-seeded get-rule/k-pbt-consistent-with-sync
  ((input romaji-basic))
  (equal (nskk-converter-get-rule input)
         (nskk-converter-get-rule/k input #'identity (lambda () nil)))
  50 3001)

(nskk-property-test-seeded get-possible-completions/k-pbt-consistent-with-sync
  ((input romaji-basic))
  (equal (nskk-converter-get-possible-completions input)
         (nskk-converter-get-possible-completions/k input #'identity (lambda () nil)))
  50 3002)

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
      (nskk-converter-define-rules ("xp" "ぱてすと"))
      (nskk-converter-add-rule "xq" "くてすと")
      (should (equal (nskk-converter-get-rule "xp") "ぱてすと"))
      (should (equal (nskk-converter-get-rule "xq") "くてすと"))))

  (nskk-it "with zero pairs expands to a no-op progn"
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
        (should (equal (nskk-converter-get-rule "ka") "か"))))))

;;;
;;; nskk--converter-populate-incomplete-markers
;;;
(nskk-describe "nskk--converter-populate-incomplete-markers"
  (nskk-it "marks romaji prefixes as :incomplete in the conversion table"
    (should (eq (nskk-converter-lookup "k") :incomplete))
    (should (eq (nskk-converter-lookup "sh") :incomplete))
    (should (eq (nskk-converter-lookup "ts") :incomplete)))

  (nskk-it "does not overwrite complete entries with :incomplete"
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
    (should-not (nskk--sokuon-p ?あ "ああ"))))

;;;
;;; nskk--convert-step/k unit tests
;;;
(nskk-describe "nskk--convert-step/k"
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

(defmacro nskk-test-with-style-transaction-state (&rest body)
  "Run BODY with isolated converter style transaction state."
  (declare (indent 0)
           (debug t))
  `(let ((nskk--romaji-table (make-hash-table :test 'equal))
         (nskk--style-registry (copy-tree nskk--style-registry))
         (nskk--converter-style-transaction-hash-tables nil)
         (nskk--converter-style-transaction-variables nil))
     (nskk-prolog-with-database-fields
         ((database (make-hash-table :test 'equal))
          (database-tails (make-hash-table :test 'equal))
          (index-config (make-hash-table :test 'equal))
          (hash-indices (make-hash-table :test 'equal))
          (trie-indices (make-hash-table :test 'equal))
          (index-bucket-tail-cache (make-hash-table :test 'equal)))
       (nskk-prolog-set-index 'romaji-to-kana 2 :trie)
       (nskk-converter-add-rule "old" "旧")
       (nskk-prolog-set-index 'transaction-sentinel 1 :hash)
       (nskk-prolog-assert '((transaction-sentinel intact)))
       ,@body)))

(defun nskk-test--converter-style-state-references ()
  "Return the converter transaction state references."
  (list
    nskk--romaji-table
    (nskk-prolog-database)
    (nskk-prolog-database-tails)
    (nskk-prolog-index-config)
    (nskk-prolog-hash-indices)
    (nskk-prolog-trie-indices)
    (nskk-prolog-index-bucket-tail-cache)))

(defun nskk-test--should-retain-converter-style-state (references)
  "Assert that REFERENCES are still the live transaction state."
  (cl-mapc
    (lambda (before after)
      (should (eq before after)))
    references
    (nskk-test--converter-style-state-references)))

(defun nskk-test--load-style-condition (style)
  "Load STYLE and return the signaled condition type, if any."
  (condition-case
    condition
    (progn
      (nskk-converter-load-style style)
      nil)
    (quit (car condition))
    (error (car condition))))

(nskk-describe
  "style loading transactions"
  (nskk-it
    "rolls back initializer errors and quits"
    (dolist (condition (quote (error quit)))
      (nskk-test-with-style-transaction-state
        (let ((references (nskk-test--converter-style-state-references)))
          (nskk-converter-register-style
            (quote failing-initializer)
            (lambda ()
              (nskk-converter-add-rule "new" "新")
              (nskk-prolog-assert (quote ((style-staged partial))))
              (signal condition nil)))
          (should
            (eq (nskk-test--load-style-condition (quote failing-initializer)) condition))
          (nskk-test--should-retain-converter-style-state references)
          (should (equal (nskk-converter-get-rule "old") "旧"))
          (should-not (nskk-converter-get-rule "new"))
          (should (nskk-prolog-holds-p (quote (transaction-sentinel intact))))
          (should-not (nskk-prolog-holds-p (quote (style-staged partial))))))))
  (nskk-it
    "rolls back finalizer errors and quits"
    (dolist (condition (quote (error quit)))
      (nskk-test-with-style-transaction-state
        (let ((references (nskk-test--converter-style-state-references)))
          (nskk-converter-register-style
            (quote failing-finalizer)
            (lambda ()
              (nskk-converter-add-rule "new" "新")))
          (cl-letf
            (((symbol-function (quote nskk--converter-populate-incomplete-markers))
                (lambda ()
                  (nskk-prolog-assert (quote ((finalizer-staged partial))))
                  (signal condition nil))))
            (should
              (eq (nskk-test--load-style-condition (quote failing-finalizer)) condition)))
          (nskk-test--should-retain-converter-style-state references)
          (should (equal (nskk-converter-get-rule "old") "旧"))
          (should-not (nskk-converter-get-rule "new"))
          (should (nskk-prolog-holds-p (quote (transaction-sentinel intact))))
          (should-not (nskk-prolog-holds-p (quote (finalizer-staged partial))))))))
  (nskk-it
    "does not mutate state for an unknown style"
    (nskk-test-with-style-transaction-state
      (let ((references (nskk-test--converter-style-state-references)))
        (should-not (nskk-converter-load-style (quote transaction-unknown)))
        (nskk-test--should-retain-converter-style-state references)
        (should (equal (nskk-converter-get-rule "old") "旧"))
        (should (nskk-prolog-holds-p (quote (transaction-sentinel intact)))))))
  (nskk-it
    "publishes staged stores and preserves unrelated indexed facts"
    (nskk-test-with-style-transaction-state
      (let ((references (nskk-test--converter-style-state-references)))
        (nskk-converter-register-style
          (quote transaction-success)
          (lambda ()
            (nskk-converter-add-rule "new" "新")
            (nskk-prolog-assert (quote ((style-committed complete))))))
        (should
          (eq
            (nskk-converter-load-style (quote transaction-success))
            (quote transaction-success)))
        (cl-mapc
          (lambda (before after)
            (should-not (eq before after)))
          references
          (nskk-test--converter-style-state-references))
        (should-not (nskk-converter-get-rule "old"))
        (should (equal (nskk-converter-get-rule "new") "新"))
        (should (nskk-prolog-holds-p (quote (transaction-sentinel intact))))
        (should (nskk-prolog-holds-p (quote (style-committed complete)))))))
  (nskk-it
    "restores exact state when keymap publication signals"
    (dolist (condition (quote (error quit)))
      (nskk-test-with-style-transaction-state
        (let* ((references (nskk-test--converter-style-state-references))
               (mode-map-reference
              (let ((map (make-sparse-keymap)))
                (define-key map (kbd "C-c o") (function ignore))
                map))
               (mode-map-car (car mode-map-reference))
               (mode-map-cdr (cdr mode-map-reference))
               (extension-symbol (quote nskk-test--converter-extension-table))
               (extension-table (make-hash-table :test (quote equal)))
               (nskk--converter-style-transaction-hash-tables (list extension-symbol))
               (active-style nskk-converter-romaji-style)
               (condition-data (list "publication failure" (make-symbol "payload")))
               (replace-keymap-contents
              (symbol-function (quote nskk--converter-replace-keymap-contents)))
               signaled)
          (puthash (quote old) (quote intact) extension-table)
          (cl-progv
            (list extension-symbol (quote nskk-mode-map))
            (list extension-table mode-map-reference)
            (nskk-converter-register-style
              (quote failing-publication)
              (lambda ()
                (nskk-converter-add-rule "new" "新")
                (nskk-prolog-assert (quote ((style-published partial))))
                (puthash (quote new) (quote partial) (symbol-value extension-symbol))
                (define-key
                  (symbol-value (quote nskk-mode-map))
                  (kbd "C-c n")
                  (function ignore))))
            (cl-letf
              (((symbol-function (quote nskk--converter-replace-keymap-contents))
                  (lambda (target source)
                    (funcall replace-keymap-contents target source)
                    (setcar target (quote corrupted-keymap-head))
                    (setcdr target (list (quote corrupted-keymap-tail)))
                    (signal condition condition-data))))
              (setq signaled (condition-case
                  caught
                  (progn
                    (nskk-converter-load-style (quote failing-publication))
                    nil)
                  (quit caught)
                  (error caught))))
            (should (eq (car signaled) condition))
            (should (eq (cdr signaled) condition-data))
            (nskk-test--should-retain-converter-style-state references)
            (should (eq (symbol-value (quote nskk-mode-map)) mode-map-reference))
            (should (eq (car mode-map-reference) mode-map-car))
            (should (eq (cdr mode-map-reference) mode-map-cdr))
            (should (eq (symbol-value extension-symbol) extension-table))
            (should (eq (gethash (quote old) extension-table) (quote intact)))
            (should-not (gethash (quote new) extension-table))
            (should (eq nskk-converter-romaji-style active-style))
            (should (equal (nskk-converter-get-rule "old") "旧"))
            (should-not (nskk-converter-get-rule "new"))
            (should (nskk-prolog-holds-p (quote (transaction-sentinel intact))))
            (should-not (nskk-prolog-holds-p (quote (style-published partial)))))))))
  (nskk-it
  "keeps public state untouched when publication preparation signals"
  (dolist (condition-symbol '(error quit))
    (nskk-test-with-style-transaction-state
      (let* ((references (nskk-test--converter-style-state-references))
             (mode-map-reference
              (let ((map (make-sparse-keymap)))
                (define-key map (kbd "C-c C-t") #'ignore)
                map))
             (mode-map-car (car mode-map-reference))
             (mode-map-cdr (cdr mode-map-reference))
             (extension-symbol
              'nskk-test--converter-preparation-fault-extension-table)
             (extension-table (make-hash-table :test 'equal))
             (nskk--converter-style-transaction-hash-tables
              (list extension-symbol))
             (condition-payload (make-symbol "payload"))
             (condition-data (list "preparation failure" condition-payload))
             (replace-calls 0)
             state
             signaled)
        (puthash 'old 'intact extension-table)
        (cl-progv
            (list extension-symbol 'nskk-mode-map)
            (list extension-table mode-map-reference)
          (setq state (nskk--converter-stage-style-state #'ignore))
          (cl-letf (((symbol-function 'nskk-prolog-copy-term)
                     (lambda (_term)
                       (signal condition-symbol condition-data)))
                    ((symbol-function
                      'nskk--converter-replace-keymap-contents)
                     (lambda (&rest _arguments)
                       (setq replace-calls (1+ replace-calls)))))
            (setq signaled
                  (condition-case caught
                      (progn
                        (nskk--converter-publish-style-state state)
                        nil)
                    (quit caught)
                    (error caught))))
          (should (eq (car signaled) condition-symbol))
          (should (eq (cdr signaled) condition-data))
          (should (= replace-calls 0))
          (nskk-test--should-retain-converter-style-state references)
          (should (eq (symbol-value extension-symbol) extension-table))
          (should (eq (gethash 'old extension-table) 'intact))
          (should (eq (symbol-value 'nskk-mode-map) mode-map-reference))
          (should (eq (car mode-map-reference) mode-map-car))
          (should (eq (cdr mode-map-reference) mode-map-cdr))
          (should (equal (nskk-converter-get-rule "old") "旧"))
          (should
           (nskk-prolog-holds-p '(transaction-sentinel intact))))))))
(nskk-it
  "preserves cross-state aliases and detaches retained staged graphs"
  (nskk-test-with-style-transaction-state
    (let* ((references (nskk-test--converter-style-state-references))
           (shared (cons 'cross-shared nil))
           (extension-symbol
            'nskk-test--converter-cross-state-extension-table)
           (extension-table (make-hash-table :test 'equal))
           (mode-map-reference (make-sparse-keymap))
           (nskk--converter-style-transaction-hash-tables
            (list extension-symbol))
           state
           retained-staged-shared)
      (setcdr shared shared)
      (puthash 'nskk-test-cross-shared shared (nskk-prolog-index-config))
      (puthash 'nskk-test-cross-shared shared extension-table)
      (setcdr mode-map-reference
              (cons (cons 'nskk-test-cross-shared shared)
                    (cdr mode-map-reference)))
      (let ((mode-map-car (car mode-map-reference))
            (mode-map-cdr (cdr mode-map-reference)))
        (cl-progv
            (list extension-symbol 'nskk-mode-map)
            (list extension-table mode-map-reference)
          (setq
           state
           (nskk--converter-stage-style-state
            (lambda ()
              (let* ((staged-store
                      (gethash
                       'nskk-test-cross-shared
                       (nskk-prolog-index-config)))
                     (staged-extension
                      (gethash
                       'nskk-test-cross-shared
                       (symbol-value extension-symbol)))
                     (staged-map-value
                      (cdr
                       (assq
                        'nskk-test-cross-shared
                        (cdr (symbol-value 'nskk-mode-map))))))
                (should (eq staged-store staged-extension))
                (should (eq staged-store staged-map-value))
                (should-not (eq staged-store shared))
                (should (eq (cdr staged-store) staged-store))
                (setq retained-staged-shared staged-store)))))
          (nskk-test--should-retain-converter-style-state references)
          (should (eq (symbol-value extension-symbol) extension-table))
          (should (eq (symbol-value 'nskk-mode-map) mode-map-reference))
          (should (eq (car mode-map-reference) mode-map-car))
          (should (eq (cdr mode-map-reference) mode-map-cdr))
          (let* ((staged-store
                  (gethash
                   'nskk-test-cross-shared
                   (plist-get state :prolog-index-config)))
                 (staged-extension-table
                  (cdr
                   (assq
                    extension-symbol
                    (plist-get state :extension-hash-tables))))
                 (staged-extension
                  (gethash
                   'nskk-test-cross-shared
                   staged-extension-table))
                 (staged-mode-map (plist-get state :mode-map))
                 (staged-map-value
                  (cdr
                   (assq
                    'nskk-test-cross-shared
                    (cdr staged-mode-map)))))
            (should (eq staged-store retained-staged-shared))
            (should (eq staged-store staged-extension))
            (should (eq staged-store staged-map-value))
            (should (eq (cdr staged-store) staged-store))
            (nskk--converter-publish-style-state state)
            (let* ((public-store
                    (gethash
                     'nskk-test-cross-shared
                     (nskk-prolog-index-config)))
                   (public-extension-table
                    (symbol-value extension-symbol))
                   (public-extension
                    (gethash
                     'nskk-test-cross-shared
                     public-extension-table))
                   (public-map-value
                    (cdr
                     (assq
                      'nskk-test-cross-shared
                      (cdr mode-map-reference)))))
              (should
               (eq (symbol-value 'nskk-mode-map) mode-map-reference))
              (should (eq public-store public-extension))
              (should (eq public-store public-map-value))
              (should-not (eq public-store retained-staged-shared))
              (should (eq (cdr public-store) public-store))
              (setcar retained-staged-shared 'staged-mutated)
              (puthash
               'nskk-test-retained
               retained-staged-shared
               (plist-get state :prolog-index-config))
              (puthash
               'nskk-test-retained
               retained-staged-shared
               staged-extension-table)
              (setcdr
               staged-mode-map
               (cons
                (cons 'nskk-test-retained retained-staged-shared)
                (cdr staged-mode-map)))
              (should (eq (car public-store) 'cross-shared))
              (should-not
               (gethash
                'nskk-test-retained
                (nskk-prolog-index-config)))
              (should-not
               (gethash
                'nskk-test-retained
                public-extension-table))
              (should-not
               (assq
                'nskk-test-retained
                (cdr mode-map-reference))))))))))
(nskk-it
    "deeply isolates staged extension graphs across initializer faults and retry"
    (dolist (condition-symbol (quote (error quit)))
      (nskk-test-with-style-transaction-state
        (let* ((references (nskk-test--converter-style-state-references))
               (extension-symbol (quote nskk-test--converter-deep-extension-table))
               (extension-table
              (make-hash-table
                :test
                (quote equal)
                :size
                31
                :rehash-size
                1.7
                :rehash-threshold
                0.75))
               (key (list (quote key)))
               (shared (list (quote shared)))
               (nested
              (make-hash-table
                :test
                (quote eq)
                :size
                17
                :rehash-size
                2.0
                :rehash-threshold
                0.8))
               (backlink (copy-sequence "payload"))
               (payload (vector shared shared nested backlink nil))
               (condition-payload (make-symbol "payload"))
               (condition-data (list "initializer failure" condition-payload))
               (nskk--converter-style-transaction-hash-tables (list extension-symbol))
               (active-style nskk-converter-romaji-style)
               extension-metadata
               (attempt 0)
               first-staged
               retry-staged
               signaled)
          (aset payload 4 payload)
          (add-text-properties
            0
            (length backlink)
            (list (quote backlink) payload)
            backlink)
          (puthash shared key nested)
          (puthash key payload extension-table)
          (setq extension-metadata (list
              (hash-table-test extension-table)
              (hash-table-size extension-table)
              (hash-table-rehash-size extension-table)
              (hash-table-rehash-threshold extension-table)
              (hash-table-weakness extension-table)))
          (cl-progv
            (list extension-symbol)
            (list extension-table)
            (nskk-converter-register-style
              (quote deep-staging-retry)
              (lambda ()
                (setq attempt (1+ attempt))
                (let ((staged-table (symbol-value extension-symbol))
                      staged-key)
                  (maphash
                    (lambda (candidate _value)
                      (when (equal candidate key)
                        (setq staged-key candidate)))
                    staged-table)
                  (let* ((staged-payload (gethash staged-key staged-table))
                         (staged-shared (aref staged-payload 0))
                         (staged-nested (aref staged-payload 2))
                         (staged-backlink (aref staged-payload 3)))
                    (should-not (eq staged-table extension-table))
                    (should-not (eq staged-key key))
                    (should-not (eq staged-payload payload))
                    (should-not (eq staged-shared shared))
                    (should-not (eq staged-nested nested))
                    (should-not (eq staged-backlink backlink))
                    (should (eq (aref staged-payload 0) (aref staged-payload 1)))
                    (should (eq (aref staged-payload 4) staged-payload))
                    (should (eq (gethash staged-shared staged-nested) staged-key))
                    (should
                      (eq (get-text-property 0 (quote backlink) staged-backlink) staged-payload))
                    (should
                      (equal
                        (list
                          (hash-table-test staged-table)
                          (hash-table-size staged-table)
                          (hash-table-rehash-size staged-table)
                          (hash-table-rehash-threshold staged-table)
                          (hash-table-weakness staged-table))
                        extension-metadata))
                    (should (= (hash-table-count staged-table) 1))
                    (should (= (hash-table-count staged-nested) 1))
                    (should (equal staged-key (quote (key))))
                    (should (equal staged-shared (quote (shared))))
                    (should-not (gethash (quote partial) staged-table))
                    (should-not (gethash (quote partial) staged-nested))
                    (should-not (get-text-property 0 (quote mutation) staged-backlink))
                    (if (= attempt 1) (progn
                        (setq first-staged staged-table)
                        (setcar staged-key (quote mutated-key))
                        (setcar staged-shared (quote mutated-shared))
                        (aset staged-payload 4 nil)
                        (puthash (quote partial) (quote mutation) staged-nested)
                        (put-text-property 0 1 (quote mutation) (quote partial) staged-backlink)
                        (puthash (quote partial) staged-payload staged-table)
                        (signal condition-symbol condition-data))
                      (setq retry-staged staged-table)
                      (puthash (quote retry) (quote success) staged-table))))))
            (setq signaled (condition-case
                caught
                (progn
                  (nskk-converter-load-style (quote deep-staging-retry))
                  nil)
                (quit caught)
                (error caught)))
            (should (eq (car signaled) condition-symbol))
            (should (eq (cdr signaled) condition-data))
            (should (eq (cadr (cdr signaled)) condition-payload))
            (nskk-test--should-retain-converter-style-state references)
            (should (eq nskk-converter-romaji-style active-style))
            (should (eq (symbol-value extension-symbol) extension-table))
            (let (stored-key)
              (maphash
                (lambda (candidate _value)
                  (setq stored-key candidate))
                extension-table)
              (should (eq stored-key key)))
            (should (eq (gethash key extension-table) payload))
            (should (eq (aref payload 0) shared))
            (should (eq (aref payload 1) shared))
            (should (eq (aref payload 2) nested))
            (should (eq (aref payload 3) backlink))
            (should (eq (aref payload 4) payload))
            (should (eq (gethash shared nested) key))
            (should (eq (get-text-property 0 (quote backlink) backlink) payload))
            (should (equal key (quote (key))))
            (should (equal shared (quote (shared))))
            (should (= (hash-table-count extension-table) 1))
            (should (= (hash-table-count nested) 1))
            (should-not (gethash (quote partial) extension-table))
            (should-not (gethash (quote partial) nested))
            (should-not (get-text-property 0 (quote mutation) backlink))
            (should
              (equal
                (list
                  (hash-table-test extension-table)
                  (hash-table-size extension-table)
                  (hash-table-rehash-size extension-table)
                  (hash-table-rehash-threshold extension-table)
                  (hash-table-weakness extension-table))
                extension-metadata))
            (should
              (eq
                (nskk-converter-load-style (quote deep-staging-retry))
                (quote deep-staging-retry)))
            (should (= attempt 2))
            (should first-staged)
            (should retry-staged)
            (should-not (eq first-staged retry-staged))
            (progn
  (should-not (eq (symbol-value extension-symbol) retry-staged))
  (should (eq (gethash (quote retry) (symbol-value extension-symbol))
              (quote success)))
  (puthash (quote retained-mutation) (quote staged) retry-staged)
  (should-not
   (gethash (quote retained-mutation) (symbol-value extension-symbol))))
            (should-not (eq retry-staged extension-table))
            (should (eq (gethash (quote retry) retry-staged) (quote success)))
            (should (eq (gethash key extension-table) payload))
            (should (eq (aref payload 4) payload))
            (should-not (gethash (quote partial) extension-table))))))))

(progn
  (defun nskk-test--converter-catch-condition (operation)
    "Call OPERATION and return an error or quit condition."
    (condition-case
      condition
      (progn
        (funcall operation)
        nil)
      (quit condition)
      (error condition)))
  (defun nskk-test--converter-rule-state (romaji)
    "Return identity-sensitive Prolog state for ROMAJI."
    (let* ((key "romaji-to-kana/2")
           (index (gethash key (nskk-prolog-trie-indices)))
           (bucket (and index (nskk-trie-lookup index romaji)))
           (cache-entry (gethash key (nskk-prolog-index-bucket-tail-cache)))
           (cache-info (and cache-entry (gethash romaji (aref cache-entry 2)))))
      (list
        (gethash key (nskk-prolog-database))
        (gethash key (nskk-prolog-database-tails))
        index
        bucket
        cache-entry
        cache-info)))
  (defun nskk-test--converter-should-retain-rule-state (before romaji)
    "Assert that BEFORE is still the exact Prolog state for ROMAJI."
    (cl-mapc
      (lambda (old current)
        (should (eq old current)))
      before
      (nskk-test--converter-rule-state romaji)))
  (nskk-describe
    "converter rule ownership and transactions"
    (nskk-it
      "detaches cyclic shared string property graphs and returns fresh strings"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test (quote equal))))
          (let ((nskk--romaji-table (make-hash-table :test (quote equal)))
                (romaji (copy-sequence "own"))
                (kana (copy-sequence "K仮名"))
                (shared (cons (quote payload) nil)))
            (nskk-prolog-clear-database)
          (nskk-prolog-set-index (quote romaji-to-kana) 2 :trie)
          (setcdr shared shared)
          (add-text-properties 0 1 (list (quote payload) shared) romaji)
          (add-text-properties 0 1 (list (quote payload) shared) kana)
          (nskk-converter-add-rule romaji kana)
          (let* ((entry (nskk--converter-find-hash-entry "own"))
                 (stored-romaji (car entry))
                 (stored-kana (cadr entry))
                 (stored-key-property (get-text-property 0 (quote payload) stored-romaji))
                 (stored-value-property (get-text-property 0 (quote payload) stored-kana)))
            (should-not (eq stored-romaji romaji))
            (should-not (eq stored-kana kana))
            (should-not (eq stored-key-property shared))
            (should (eq stored-key-property stored-value-property))
            (should (eq (cdr stored-key-property) stored-key-property))
            (should
              (eq
                stored-kana
                (nth 2 (car (car (gethash "romaji-to-kana/2" (nskk-prolog-database)))))))
            (aset romaji 0 ?X)
            (aset kana 0 ?X)
            (let* ((first (nskk-converter-lookup "own"))
                   (second (nskk-converter-lookup "own"))
                   (first-property (get-text-property 0 (quote payload) first))
                   (second-property (get-text-property 0 (quote payload) second)))
              (should (equal first stored-kana))
              (should (equal second stored-kana))
              (should-not (eq first stored-kana))
              (should-not (eq first second))
              (should-not (eq first-property stored-key-property))
              (should-not (eq first-property second-property))
              (should (eq (cdr first-property) first-property))
              (should (eq (cdr second-property) second-property))
              (aset first 0 ?X)
              (should (equal second stored-kana))
              (should (equal (nskk-converter-get-rule "own") stored-kana))))))))
    (nskk-it
      "distinguishes present nil, incomplete, and non-string identities"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test (quote equal))))
          (let ((nskk--romaji-table (make-hash-table :test (quote equal)))
                (object (list (quote non-string))))
            (nskk-prolog-clear-database)
            (nskk-prolog-set-index (quote romaji-to-kana) 2 :trie)
            (nskk-converter-add-rule "nil-key" nil)
            (nskk-converter-add-rule "nil-keyx" "長")
            (nskk-converter-add-rule "object" object)
            (nskk--converter-populate-incomplete-markers)
            (let ((nil-entry (nskk--converter-find-hash-entry "nil-key"))
                  (object-entry (nskk--converter-find-hash-entry "object")))
              (should nil-entry)
              (should-not (cadr nil-entry))
              (should-not (nskk--converter-lookup-raw "nil-key"))
              (should (eq (nskk-converter-lookup "nil-key") nil))
              (should (equal (nskk-converter-lookup "nil-keyx") "長"))
              (should (eq (nskk-converter-lookup "nil-") :incomplete))
              (should (eq (cadr object-entry) object))
              (should (eq (nskk-converter-lookup "object") object))
              (should-not
                (nskk-prolog-query (list (quote romaji-to-kana) "object" (quote \?kana)))))))))
    (nskk-it
      "keeps non-string values out of Prolog and detaches their keys"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test (quote equal))))
          (let ((nskk--romaji-table (make-hash-table :test (quote equal)))
                (romaji (copy-sequence "metadata"))
                (value (list (quote metadata))))
            (nskk-prolog-clear-database)
            (nskk-prolog-set-index (quote romaji-to-kana) 2 :trie)
            (let ((prolog-before (nskk-test--converter-rule-state "metadata")))
              (nskk-converter-add-rule romaji value)
              (let ((entry (nskk--converter-find-hash-entry "metadata")))
                (should-not (eq (car entry) romaji))
                (should (eq (cadr entry) value))
                (should (eq (nskk-converter-lookup "metadata") value)))
              (nskk-test--converter-should-retain-rule-state prolog-before "metadata"))))))
    (nskk-it
      "replaces and removes only the first matching Prolog clause"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test (quote equal))))
          (let ((nskk--romaji-table (make-hash-table :test (quote equal))))
            (nskk-prolog-clear-database)
            (nskk-prolog-set-index (quote romaji-to-kana) 2 :trie)
          (let ((first (list (list (quote romaji-to-kana) "duplicate" "一")))
                (second (list (list (quote romaji-to-kana) "duplicate" "二")))
                (survivor (list (list (quote romaji-to-kana) "other" "他"))))
            (nskk-prolog-assert first)
            (nskk-prolog-assert second)
            (nskk-prolog-assert survivor)
            (nskk-converter-add-rule "duplicate" "新")
            (let ((database (gethash "romaji-to-kana/2" (nskk-prolog-database))))
              (should (equal (car database) second))
              (should (equal (cadr database) survivor))
              (should
                (equal
                  (car (car (last database)))
                  (list (quote romaji-to-kana) "duplicate" "新"))))
            (should
              (equal
                (nskk-prolog-query-value
                  (list (quote romaji-to-kana) "duplicate" (quote \?kana))
                  (quote \?kana))
                "二"))
            (should (equal (nskk-converter-lookup "duplicate") "新"))
            (nskk-converter-remove-rule "duplicate")
            (should
              (equal
                (nskk-prolog-query-value
                  (list (quote romaji-to-kana) "duplicate" (quote \?kana))
                  (quote \?kana))
                "新"))
            (should-not (nskk--converter-find-hash-entry "duplicate")))))))
    (nskk-it
      "restores exact hash entries for every journal failure boundary"
      (dolist (state (quote (absent present-nil present-value)))
        (dolist (fault-type (quote (error quit)))
          (dolist (timing (quote (before after)))
            (let* ((nskk--romaji-table (make-hash-table :test (quote equal)))
                   (old-key (copy-sequence "journal"))
                   (old-value (and (eq state (quote present-value)) (list (quote old-value))))
                   (marker (list state fault-type timing)))
              (unless (eq state (quote absent))
                (puthash old-key old-value nskk--romaji-table))
              (let ((caught
                    (nskk-test--converter-catch-condition
                      (lambda ()
                        (nskk--converter-call-with-hash-journal
                          (copy-sequence "journal")
                          (lambda ()
                            (when (eq timing (quote before))
                              (signal fault-type (list marker)))
                            (nskk--converter-replace-hash-entry
                              "journal"
                              (copy-sequence "journal")
                              (list (quote new-value)))
                            (signal fault-type (list marker))))))))
                (should (eq (car caught) fault-type))
                (should (eq (cadr caught) marker))
                (if (eq state (quote absent)) (progn
                    (should (= (hash-table-count nskk--romaji-table) 0))
                    (should-not (nskk--converter-find-hash-entry "journal")))
                  (let ((entry (nskk--converter-find-hash-entry "journal")))
                    (should (= (hash-table-count nskk--romaji-table) 1))
                    (should (eq (car entry) old-key))
                    (should (eq (cadr entry) old-value))))))))))
    (nskk-it
      "does not mutate either store when caller graph copying fails"
      (dolist (operation (quote (add remove)))
        (dolist (fault-type (quote (error quit)))
          (nskk-prolog-test-with-isolated-db
            (nskk-prolog-with-database-fields
                ((index-bucket-tail-cache (make-hash-table :test (quote equal))))
              (let ((nskk--romaji-table (make-hash-table :test (quote equal))))
              (nskk-prolog-clear-database)
              (nskk-prolog-set-index (quote romaji-to-kana) 2 :trie)
              (nskk-converter-add-rule "copy-failure" "旧")
              (let* ((entry-before (nskk--converter-find-hash-entry "copy-failure"))
                     (state-before (nskk-test--converter-rule-state "copy-failure"))
                     (marker (list operation fault-type))
                     caught)
                (cl-letf
                  (((symbol-function (quote nskk-prolog-copy-term))
                      (lambda (_term)
                        (signal fault-type (list marker)))))
                  (setq caught (nskk-test--converter-catch-condition
                      (lambda ()
                        (if (eq operation (quote add)) (nskk-converter-add-rule "copy-failure" "新")
                          (nskk-converter-remove-rule "copy-failure"))))))
                (should (eq (car caught) fault-type))
                (should (eq (cadr caught) marker))
                (let ((entry-after (nskk--converter-find-hash-entry "copy-failure")))
                  (should (eq (car entry-after) (car entry-before)))
                  (should (eq (cadr entry-after) (cadr entry-before))))
                (nskk-test--converter-should-retain-rule-state state-before "copy-failure")
                (if (eq operation (quote add)) (progn
                    (nskk-converter-add-rule "copy-failure" "新")
                    (should (equal (nskk-converter-lookup "copy-failure") "新")))
                  (nskk-converter-remove-rule "copy-failure")
                  (should-not (nskk--converter-find-hash-entry "copy-failure"))))))))))
    (nskk-it
      "rolls back add callbacks before and after hash publication"
      (dolist (fault-type (quote (error quit)))
        (dolist (timing (quote (before after)))
          (nskk-prolog-test-with-isolated-db
            (nskk-prolog-with-database-fields
                ((index-bucket-tail-cache (make-hash-table :test (quote equal))))
              (let ((nskk--romaji-table (make-hash-table :test (quote equal))))
              (nskk-prolog-clear-database)
              (nskk-prolog-set-index (quote romaji-to-kana) 2 :trie)
              (nskk-converter-add-rule "atomic-add" "旧")
              (let* ((entry-before (nskk--converter-find-hash-entry "atomic-add"))
                     (state-before (nskk-test--converter-rule-state "atomic-add"))
                     (marker (list fault-type timing))
                     (original (symbol-function (quote nskk--converter-replace-hash-entry)))
                     caught)
                (cl-letf
                  (((symbol-function (quote nskk--converter-replace-hash-entry))
                      (lambda (lookup-key new-key value)
                        (when (eq timing (quote before))
                          (signal fault-type (list marker)))
                        (funcall original lookup-key new-key value)
                        (signal fault-type (list marker)))))
                  (setq caught (nskk-test--converter-catch-condition
                      (lambda ()
                        (nskk-converter-add-rule "atomic-add" "新")))))
                (should (eq (car caught) fault-type))
                (should (eq (cadr caught) marker))
                (let ((entry-after (nskk--converter-find-hash-entry "atomic-add")))
                  (should (eq (car entry-after) (car entry-before)))
                  (should (eq (cadr entry-after) (cadr entry-before))))
                (nskk-test--converter-should-retain-rule-state state-before "atomic-add")
                (should
                  (equal
                    (nskk-prolog-query-value
                      (list (quote romaji-to-kana) "atomic-add" (quote \?kana))
                      (quote \?kana))
                    "旧"))
                (nskk-converter-add-rule "atomic-add" "新")
                (should (equal (nskk-converter-lookup "atomic-add") "新")))))))))
    (nskk-it
      "rolls back remove callbacks before and after hash deletion"
      (dolist (fault-type (quote (error quit)))
        (dolist (timing (quote (before after)))
          (nskk-prolog-test-with-isolated-db
            (nskk-prolog-with-database-fields
                ((index-bucket-tail-cache (make-hash-table :test (quote equal))))
              (let ((nskk--romaji-table (make-hash-table :test (quote equal))))
              (nskk-prolog-clear-database)
              (nskk-prolog-set-index (quote romaji-to-kana) 2 :trie)
              (nskk-converter-add-rule "atomic-remove" "旧")
              (let* ((entry-before (nskk--converter-find-hash-entry "atomic-remove"))
                     (state-before (nskk-test--converter-rule-state "atomic-remove"))
                     (marker (list fault-type timing))
                     (original (symbol-function (quote nskk--converter-delete-hash-entry)))
                     caught)
                (cl-letf
                  (((symbol-function (quote nskk--converter-delete-hash-entry))
                      (lambda (lookup-key)
                        (when (eq timing (quote before))
                          (signal fault-type (list marker)))
                        (funcall original lookup-key)
                        (signal fault-type (list marker)))))
                  (setq caught (nskk-test--converter-catch-condition
                      (lambda ()
                        (nskk-converter-remove-rule "atomic-remove")))))
                (should (eq (car caught) fault-type))
                (should (eq (cadr caught) marker))
                (let ((entry-after (nskk--converter-find-hash-entry "atomic-remove")))
                  (should (eq (car entry-after) (car entry-before)))
                  (should (eq (cadr entry-after) (cadr entry-before))))
                (nskk-test--converter-should-retain-rule-state state-before "atomic-remove")
                (should
                  (equal
                    (nskk-prolog-query-value
                      (list (quote romaji-to-kana) "atomic-remove" (quote \?kana))
                      (quote \?kana))
                    "旧"))
                (nskk-converter-remove-rule "atomic-remove")
                (should-not (nskk--converter-find-hash-entry "atomic-remove"))))))))))
  (progn
  (nskk-describe
    "unbound mode map style transactions"
    (nskk-it
      "preserves true unbound reads and unmodified staging"
      (nskk-test-with-style-transaction-state
        (let (state)
          (cl-progv
              (list (quote nskk-mode-map))
              (list (make-symbol "outer-mode-map"))
            (makunbound (quote nskk-mode-map))
            (should-not (boundp (quote nskk-mode-map)))
            (setq state
                  (nskk--converter-stage-style-state
                   (lambda ()
                     (should-not (boundp (quote nskk-mode-map)))
                     (should-error
                      (symbol-value (quote nskk-mode-map))
                      :type (quote void-variable)))))
            (should-not (boundp (quote nskk-mode-map)))
            (should-not (plist-get state :mode-map-bound-p))
            (should-not (plist-get state :mode-map))
            (setq state
                  (nskk--converter-stage-style-state (function ignore)))
            (should-not (boundp (quote nskk-mode-map)))
            (should-not (plist-get state :mode-map-bound-p))
            (should-not (plist-get state :mode-map))))))
    (nskk-it
      "contains initializer assignments across faults and retry"
      (dolist (condition-symbol (quote (error quit)))
        (nskk-test-with-style-transaction-state
          (let ((assigned-map (make-sparse-keymap))
                (condition-data (list "initializer failure" condition-symbol))
                retry-map
                state
                signaled)
            (cl-progv
                (list (quote nskk-mode-map))
                (list (make-symbol "outer-mode-map"))
              (makunbound (quote nskk-mode-map))
              (setq signaled
                    (condition-case caught
                        (progn
                          (nskk--converter-stage-style-state
                           (lambda ()
                             (setq nskk-mode-map assigned-map)
                             (should (boundp (quote nskk-mode-map)))
                             (should
                              (eq (symbol-value (quote nskk-mode-map))
                                  assigned-map))
                             (signal condition-symbol condition-data)))
                          nil)
                      (quit caught)
                      (error caught)))
              (should (eq (car signaled) condition-symbol))
              (should (eq (cdr signaled) condition-data))
              (should-not (boundp (quote nskk-mode-map)))
              (setq retry-map (make-sparse-keymap))
              (setq state
                    (nskk--converter-stage-style-state
                     (lambda ()
                       (setq nskk-mode-map retry-map))))
              (should-not (boundp (quote nskk-mode-map)))
              (should (plist-get state :mode-map-bound-p))
              (should (eq (plist-get state :mode-map) retry-map)))))))
    (nskk-it
      "publishes a detached initializer map and retains its shell"
      (nskk-test-with-style-transaction-state
        (let* ((extension-symbol
                (quote nskk-test--converter-unbound-mode-map-extension))
               (extension-table (make-hash-table :test (quote equal)))
               (nskk--converter-style-transaction-hash-tables
                (list extension-symbol))
               assigned-map
               public-shell
               shared
               staged-extension
               staged-shared
               state)
          (cl-progv
              (list extension-symbol (quote nskk-mode-map))
              (list extension-table (make-symbol "outer-mode-map"))
            (makunbound (quote nskk-mode-map))
            (setq state
                  (nskk--converter-stage-style-state
                   (lambda ()
                     (setq assigned-map (make-sparse-keymap))
                     (setq shared (cons (quote unbound-shared) nil))
                     (setcdr shared shared)
                     (puthash "unbound-shared" shared nskk--romaji-table)
                     (puthash "unbound-shared" shared
                              (symbol-value extension-symbol))
                     (setcdr assigned-map
                             (cons
                              (cons (quote nskk-test-unbound-shared) shared)
                              (cdr assigned-map)))
                     (setq nskk-mode-map assigned-map))))
            (should-not (boundp (quote nskk-mode-map)))
            (should (plist-get state :mode-map-bound-p))
            (should (eq (plist-get state :mode-map) assigned-map))
            (setq staged-shared
                  (gethash "unbound-shared"
                           (plist-get state :romaji-table)))
            (setq staged-extension
                  (cdr
                   (assq extension-symbol
                         (plist-get state :extension-hash-tables))))
            (should (eq staged-shared shared))
            (should (eq (gethash "unbound-shared" staged-extension)
                        staged-shared))
            (should
             (eq
              (cdr
               (assq (quote nskk-test-unbound-shared)
                     (cdr (plist-get state :mode-map))))
              staged-shared))
            (should (eq (cdr staged-shared) staged-shared))
            (nskk--converter-publish-style-state state)
            (should (boundp (quote nskk-mode-map)))
            (setq public-shell (symbol-value (quote nskk-mode-map)))
            (should-not (eq public-shell assigned-map))
            (let* ((public-shared
                    (gethash "unbound-shared" nskk--romaji-table))
                   (public-extension
                    (gethash "unbound-shared"
                             (symbol-value extension-symbol)))
                   (public-map-shared
                    (cdr
                     (assq (quote nskk-test-unbound-shared)
                           (cdr public-shell)))))
              (should (eq public-shared public-extension))
              (should (eq public-shared public-map-shared))
              (should-not (eq public-shared staged-shared))
              (should (eq (cdr public-shared) public-shared))
              (setcar staged-shared (quote retained-mutated))
              (setcdr assigned-map
                      (cons
                       (cons (quote nskk-test-retained) staged-shared)
                       (cdr assigned-map)))
              (should (eq (car public-shared) (quote unbound-shared)))
              (should-not
               (assq (quote nskk-test-retained) (cdr public-shell))))
            (let ((retry-state
                   (nskk--converter-stage-style-state
                    (lambda ()
                      (define-key
                       (symbol-value (quote nskk-mode-map))
                       [f24]
                       (function ignore))))))
              (should (eq (symbol-value (quote nskk-mode-map))
                          public-shell))
              (should-not (lookup-key public-shell [f24]))
              (nskk--converter-publish-style-state retry-state)
              (should (eq (symbol-value (quote nskk-mode-map))
                          public-shell))
              (should (eq (lookup-key public-shell [f24])
                          (function ignore)))))))))
  (progn
  (nskk-describe
    "mode map binding state publication"
    (nskk-it
      "publishes every boundness and value transition"
      (dolist (transition
               (quote ((unbound map)
                       (bound-nil bound-nil)
                       (bound-nil map)
                       (map bound-nil)
                       (map map)
                       (map unbound)
                       (bound-nil unbound)
                       (unbound unbound))))
        (nskk-test-with-style-transaction-state
          (let* ((initial-kind (nth 0 transition))
                 (target-kind (nth 1 transition))
                 (initial-map (make-sparse-keymap))
                 (target-map (make-sparse-keymap))
                 state
                 published-map)
            (define-key initial-map [f20] (function ignore))
            (define-key target-map [f21] (function forward-char))
            (cl-progv
                (list (quote nskk-mode-map))
                (list (make-symbol "outer-mode-map"))
              (pcase initial-kind
                ((quote unbound)
                 (makunbound (quote nskk-mode-map)))
                ((quote bound-nil)
                 (set (quote nskk-mode-map) nil))
                ((quote map)
                 (set (quote nskk-mode-map) initial-map)))
              (setq state
                    (nskk--converter-stage-style-state
                     (lambda ()
                       (pcase target-kind
                         ((quote unbound)
                          (makunbound (quote nskk-mode-map)))
                         ((quote bound-nil)
                          (set (quote nskk-mode-map) nil))
                         ((quote map)
                          (set (quote nskk-mode-map) target-map))))))
              (pcase initial-kind
                ((quote unbound)
                 (should-not (boundp (quote nskk-mode-map))))
                ((quote bound-nil)
                 (should (boundp (quote nskk-mode-map)))
                 (should-not (symbol-value (quote nskk-mode-map))))
                ((quote map)
                 (should (eq (symbol-value (quote nskk-mode-map))
                             initial-map))))
              (should
               (eq (plist-get state :mode-map-bound-p)
                   (not (eq target-kind (quote unbound)))))
              (pcase target-kind
                ((quote unbound)
                 (should-not (plist-get state :mode-map)))
                ((quote bound-nil)
                 (should-not (plist-get state :mode-map)))
                ((quote map)
                 (should (eq (plist-get state :mode-map) target-map))))
              (nskk--converter-publish-style-state state)
              (pcase target-kind
                ((quote unbound)
                 (should-not (boundp (quote nskk-mode-map))))
                ((quote bound-nil)
                 (should (boundp (quote nskk-mode-map)))
                 (should-not (symbol-value (quote nskk-mode-map))))
                ((quote map)
                 (should (boundp (quote nskk-mode-map)))
                 (setq published-map
                       (symbol-value (quote nskk-mode-map)))
                 (if (eq initial-kind (quote map))
                     (should (eq published-map initial-map))
                   (should-not (eq published-map target-map)))
                 (should (eq (lookup-key published-map [f21])
                             (function forward-char)))))))))))
  (progn
  (nskk-describe
    "style publication cleanup faults"
    (nskk-it
      "preserves the original condition across one-shot cleanup faults"
      (dolist (original-condition (quote (error quit)))
        (dolist (cleanup-condition (quote (error quit)))
          (dolist (cleanup-position (quote (before after)))
            (nskk-test-with-style-transaction-state
              (let* ((extension-a
                      (quote nskk-test--converter-cleanup-extension-a))
                     (extension-b
                      (quote nskk-test--converter-cleanup-extension-b))
                     (old-extension-a
                      (make-hash-table :test (quote equal)))
                     (old-extension-b
                      (make-hash-table :test (quote equal)))
                     (nskk--converter-style-transaction-hash-tables
                      (list extension-a extension-b))
                     (old-map (make-sparse-keymap))
                     (original-data
                      (list "original publication failure"
                            original-condition))
                     (cleanup-data
                      (list "cleanup failure" cleanup-condition))
                     (references
                      (nskk-test--converter-style-state-references))
                     original-replace
                     cleanup-watcher
                     old-map-car
                     old-map-cdr
                     cleanup-p
                     cleanup-faulted-p
                     cleanup-setting-p
                     caught
                     state)
                (define-key old-map [f19] (function backward-char))
                (setq old-map-car (car old-map))
                (setq old-map-cdr (cdr old-map))
                (puthash "old-a" t old-extension-a)
                (puthash "old-b" t old-extension-b)
                (cl-progv
                    (list extension-a extension-b
                          (quote nskk-mode-map))
                    (list old-extension-a old-extension-b old-map)
                  (setq state
                        (nskk--converter-stage-style-state
                         (lambda ()
                           (puthash "new" t
                                    (symbol-value extension-a))
                           (puthash "new" t
                                    (symbol-value extension-b))
                           (define-key
                            (symbol-value (quote nskk-mode-map))
                            [f22]
                            (function forward-char)))))
                  (setq original-replace
                        (symbol-function
                         (quote nskk--converter-replace-keymap-contents)))
                  (setq cleanup-watcher
                        (lambda (symbol value operation _where)
                          (when (and cleanup-p
                                     (eq operation (quote set))
                                     (eq symbol extension-a)
                                     (not cleanup-faulted-p)
                                     (not cleanup-setting-p))
                            (setq cleanup-faulted-p t)
                            (when (eq cleanup-position (quote after))
                              (setq cleanup-setting-p t)
                              (unwind-protect
                                  (set symbol value)
                                (setq cleanup-setting-p nil)))
                            (signal cleanup-condition cleanup-data))))
                  (unwind-protect
                      (progn
                        (add-variable-watcher extension-a cleanup-watcher)
                        (cl-letf
                            (((symbol-function
                               (quote
                                nskk--converter-replace-keymap-contents))
                              (lambda (target source)
                                (funcall original-replace target source)
                                (setq cleanup-p t)
                                (signal original-condition original-data))))
                          (setq caught
                                (condition-case condition
                                    (progn
                                      (nskk--converter-publish-style-state
                                       state)
                                      nil)
                                  (quit condition)
                                  (error condition)))))
                    (remove-variable-watcher extension-a cleanup-watcher))
                  (should (eq (car caught) original-condition))
                  (should (eq (cdr caught) original-data))
                  (should cleanup-faulted-p)
                  (nskk-test--should-retain-converter-style-state
                   references)
                  (should (eq (symbol-value extension-a)
                              old-extension-a))
                  (should (eq (symbol-value extension-b)
                              old-extension-b))
                  (should-not (gethash "new" old-extension-a))
                  (should-not (gethash "new" old-extension-b))
                  (should (boundp (quote nskk-mode-map)))
                  (should (eq (symbol-value (quote nskk-mode-map))
                              old-map))
                  (should (eq (car old-map) old-map-car))
                  (should (eq (cdr old-map) old-map-cdr))
                  (should-not (lookup-key old-map [f22]))
                  (nskk--converter-publish-style-state state)
                  (should (eq (symbol-value (quote nskk-mode-map))
                              old-map))
                  (should (eq (lookup-key old-map [f22])
                              (function forward-char)))
                  (should
                   (gethash "new" (symbol-value extension-a)))
                  (should
                   (gethash "new" (symbol-value extension-b)))))))))))
  (progn
  (nskk-describe
    "persistent style publication cleanup faults"
    (nskk-it
      "maximizes restoration and keeps the state retryable"
      (dolist (original-condition (quote (error quit)))
        (nskk-test-with-style-transaction-state
          (let* ((extension-a
                  (quote nskk-test--converter-persistent-extension-a))
                 (extension-b
                  (quote nskk-test--converter-persistent-extension-b))
                 (old-extension-a
                  (make-hash-table :test (quote equal)))
                 (old-extension-b
                  (make-hash-table :test (quote equal)))
                 (nskk--converter-style-transaction-hash-tables
                  (list extension-a extension-b))
                 (old-map (make-sparse-keymap))
                 (original-data
                  (list "persistent publication failure"
                        original-condition))
                 (references
                  (nskk-test--converter-style-state-references))
                 original-replace
                 cleanup-watcher
                 old-map-car
                 old-map-cdr
                 cleanup-p
                 (extension-fault-count 0)
                 caught
                 state)
            (define-key old-map [f18] (function backward-char))
            (setq old-map-car (car old-map))
            (setq old-map-cdr (cdr old-map))
            (puthash "old-a" t old-extension-a)
            (puthash "old-b" t old-extension-b)
            (cl-progv
                (list extension-a extension-b
                      (quote nskk-mode-map))
                (list old-extension-a old-extension-b old-map)
              (setq state
                    (nskk--converter-stage-style-state
                     (lambda ()
                       (puthash "new" t (symbol-value extension-a))
                       (puthash "new" t (symbol-value extension-b))
                       (define-key
                        (symbol-value (quote nskk-mode-map))
                        [f23]
                        (function forward-char)))))
              (setq original-replace
                    (symbol-function
                     (quote nskk--converter-replace-keymap-contents)))
              (setq cleanup-watcher
                    (lambda (symbol _value operation _where)
                      (when (and cleanup-p
                                 (eq operation (quote set))
                                 (eq symbol extension-a))
                        (setq extension-fault-count
                              (1+ extension-fault-count))
                        (error "persistent extension cleanup failure"))))
              (unwind-protect
                  (progn
                    (add-variable-watcher extension-a cleanup-watcher)
                    (cl-letf
                        (((symbol-function
                           (quote
                            nskk--converter-replace-keymap-contents))
                          (lambda (target source)
                            (funcall original-replace target source)
                            (setq cleanup-p t)
                            (signal original-condition original-data))))
                      (setq caught
                            (condition-case condition
                                (progn
                                  (nskk--converter-publish-style-state
                                   state)
                                  nil)
                              (quit condition)
                              (error condition)))))
                (remove-variable-watcher extension-a cleanup-watcher))
              (should (eq (car caught) original-condition))
              (should (eq (cdr caught) original-data))
              (should (= extension-fault-count 2))
              (nskk-test--should-retain-converter-style-state
               references)
              (should-not (eq (symbol-value extension-a)
                              old-extension-a))
              (should (gethash "new" (symbol-value extension-a)))
              (should (eq (symbol-value extension-b)
                          old-extension-b))
              (should-not (gethash "new" old-extension-b))
              (should (boundp (quote nskk-mode-map)))
              (should (eq (symbol-value (quote nskk-mode-map))
                          old-map))
              (should (eq (car old-map) old-map-car))
              (should (eq (cdr old-map) old-map-cdr))
              (should-not (lookup-key old-map [f23]))
              (nskk--converter-publish-style-state state)
              (should (eq (symbol-value (quote nskk-mode-map))
                          old-map))
              (should (eq (lookup-key old-map [f23])
                          (function forward-char)))
              (should (gethash "new" (symbol-value extension-a)))
              (should
               (gethash "new" (symbol-value extension-b)))))))))
  (provide (quote nskk-converter-test)))))))

;;; nskk-converter-test.el ends here
