;;; nskk-converter-test.el --- Converter Tests for NSKK  -*- lexical-binding: t; -*-
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
(require 'nskk-converter)
(require 'nskk-kana)
(require 'nskk-pbt-generators)
(require 'cl-lib)

(defvar nskk-mode-map)

(nskk-describe "romaji basic conversion"
  (nskk-deftest-table converter-vowels
    :description "Converts vowels to hiragana"
    :columns (input expected)
    :rows (("a" "あ")
           ("i" "い")
           ("u" "う")
           ("e" "え")
           ("o" "お"))
    :body (should (equal expected (nskk-converter-lookup input))))

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
           ("wo"  "を"))
    :body (should (equal expected (nskk-converter-lookup input))))

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
    :body (should (equal expected (nskk-converter-lookup input))))

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
    :body (should (equal expected (nskk-converter-lookup input))))

  (nskk-deftest-table converter-standalone-hatsuon
    :description "Standalone hatsuon sequences convert to ん via direct lookup"
    :columns (input expected)
    :rows (("nn" "ん")
           ("n'" "ん"))
    :body (should (equal expected (nskk-converter-lookup input))))

  (nskk-deftest-table converter-complete-words
    :description "Converts complete romaji words to hiragana by repeated longest-match lookup.
Words that require hatsuon-before-consonant or sokuon disambiguation are
excluded: that logic now lives in nskk-input.el and is exercised there (see
test/e2e/nskk-kana-input-e2e-test.el)."
    :columns (input expected)
    :rows (("sayounara"  "さようなら")
           ("arigatou"   "ありがとう")
           ("sakana"     "さかな")
           ("yama"       "やま")
           ("kawa"       "かわ")
           ("sora"       "そら"))
    :body (should (equal expected (nskk-test-convert-romaji input)))))

(nskk-describe "romaji-to-kana integration"
  (nskk-it "converts complete kana rows"
    (should (equal (nskk-test-convert-romaji "aiueo") "あいうえお"))
    (should (equal (nskk-test-convert-romaji "kakikukeko") "かきくけこ"))
    (should (equal (nskk-test-convert-romaji "sashisuseso") "さしすせそ"))))

(nskk-describe "converter-initialize"
  (nskk-it "is idempotent: subsequent calls are no-ops"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (nskk-converter-initialize)
      (should (equal (nskk-converter-lookup "ka") "か")))))

;;;;
;;;; Property-Based Tests: Conversion Properties
;;;;
(nskk-property-test conversion-output-never-expands
  ((input romaji-string))
  (<= (length (nskk-test-convert-romaji input)) (length input))
  100)

(nskk-property-test conversion-min-compression-ratio
  ((input romaji-string))
  (>= (length (nskk-test-convert-romaji input)) (/ (length input) 4))
  100)

(nskk-property-test conversion-non-empty-output
  ((input romaji-string))
  (or (string-empty-p input)
      (not (string-empty-p (nskk-test-convert-romaji input))))
  100)

(nskk-deftest-performance conversion-basic-performance
  "Basic romaji-to-kana conversion completes within time budget."
  (let ((test-string "konnichiwa"))
    (nskk-should-be-fast
     basic-conversion 5000
     (dotimes (_ 10000)
       (nskk-test-convert-romaji test-string)))))

(nskk-deftest-performance conversion-complex-performance
  "Complex romaji-to-kana conversion completes within time budget."
  (let ((test-string "konyakunishitekyouyakusuru"))
    (nskk-should-be-fast
     complex-conversion 5000
     (dotimes (_ 1000)
       (nskk-test-convert-romaji test-string)))))

(nskk-deftest-performance conversion-batch-performance
  "Batch romaji-to-kana conversion completes within time budget."
  (let ((test-strings '("aiueo" "kakikukeko" "sashisuseso"
                        "tachitsuteto" "naninuneno" "hahifuheho"
                        "mamimumemo" "yayuyo" "rariruro" "wawo")))
    (nskk-should-be-fast
     batch-conversion 5000
     (dotimes (_ 1000)
       (dolist (s test-strings)
         (nskk-test-convert-romaji s))))))

(nskk-deftest-performance conversion-long-input-performance
  "Long-string romaji-to-kana conversion completes within time budget."
  (let ((test-string "kakikukekokakikukekokakikukeko"))
    (nskk-should-be-fast
     long-input-conversion 5000
     (dotimes (_ 1000)
       (nskk-test-convert-romaji test-string)))))

(nskk-describe "ddskk punctuation rules"
  (nskk-deftest-table converter-basic-punctuation
    :description "Basic punctuation keys convert to Japanese punctuation"
    :columns (input expected)
    :rows (("."  "。")
           (","  "、")
           ("["  "「")
           ("]"  "」"))
    :body (should (equal expected (nskk-converter-lookup input))))

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
    :body (should (equal expected (nskk-converter-lookup input))))

  (nskk-it "katakana-passthrough: symbols produced by punctuation rules are not altered by hiragana-to-katakana conversion"
    (dolist (pair '(("。" . "。") ("、" . "、") ("「" . "「") ("」" . "」")
                    ("〜" . "〜") ("…" . "…") ("‥" . "‥") ("『" . "『") ("』" . "』")
                    ("・" . "・") ("←" . "←") ("↓" . "↓") ("↑" . "↑") ("→" . "→")
                    ("　" . "　")))
      (should (equal (cdr pair)
                     (nskk-kana-string-hiragana-to-katakana (car pair)))))))

(nskk-describe "regression: palatal conversion"
  (nskk-it "correctly handles palatal consonants"
    (should (equal (nskk-test-convert-romaji "toukyou") "とうきょう"))
    (should (equal (nskk-test-convert-romaji "kyouto") "きょうと"))
    (should (equal (nskk-test-convert-romaji "sushi") "すし"))))

(nskk-describe "regression: long string handling"
  (nskk-it "handles long inputs without truncation"
    (let ((long-romaji "aiueoaiueoaiueoaiueoaiueo")
          (expected    "あいうえおあいうえおあいうえおあいうえおあいうえお"))
      (should (equal (nskk-test-convert-romaji long-romaji) expected)))
    (should (equal (nskk-test-convert-romaji "kakikukekokakikukekokakikukeko")
                   "かきくけこかきくけこかきくけこ"))))

(nskk-describe "romaji fallback and passthrough behavior"
  (nskk-it "passes through input with no matching rule or prefix"
    (should (equal (nskk-test-convert-romaji "q") "q"))
    (should (equal (nskk-test-convert-romaji "123") "123"))
    (should (equal (nskk-test-convert-romaji " ") " ")))

  (nskk-it "falls back to the unconverted tail when a real prefix character never completes a rule"
    (should (equal (nskk-test-convert-romaji "xyz") "xyz"))
    (should (equal (nskk-test-convert-romaji "kaxyz") "かxyz")))

  (nskk-it "appends an unresolved incomplete tail verbatim"
    (should (equal (nskk-test-convert-romaji "kak") "かk")))

  (nskk-it "handles empty and nil input"
    (should (equal (nskk-test-convert-romaji "") ""))
    (should (equal (nskk-test-convert-romaji nil) nil))))

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
      (should (equal (nskk-converter-lookup "testkey") "テスト"))))

  (nskk-it "removes a conversion rule"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (nskk-converter-add-rule "tempkey" "テンプ")
      (should (nskk-converter-lookup "tempkey"))
      (nskk-converter-remove-rule "tempkey")
      (should-not (nskk-converter-lookup "tempkey"))))

  (nskk-it "gets an existing rule"
    (should (equal (nskk-converter-lookup "ka") "か")))

  (nskk-it "returns nil for nonexistent rule"
    (should-not (nskk-converter-lookup "nonexistent-romaji-key")))

  (nskk-it "overrides an existing rule"
    (nskk-prolog-test-with-isolated-db
      (nskk-converter-initialize)
      (nskk-converter-add-rule "ka" "カ")
      (should (equal (nskk-converter-lookup "ka") "カ")))))

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
      (should (equal (nskk-converter-lookup "ka") "か"))))

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
        (should (equal (nskk-converter-lookup "x") "エックス"))
        (should-not (equal (nskk-converter-lookup "ka") "か"))))))

(nskk-describe "public romaji table accessors"
  (nskk-it "nskk-set-romaji-table replaces what nskk-romaji-table returns"
    (let* ((original (nskk-romaji-table))
           (replacement (make-hash-table :test 'equal)))
      (puthash "test-key" "テスト値" replacement)
      (unwind-protect
          (progn
            (nskk-set-romaji-table replacement)
            (should (eq (nskk-romaji-table) replacement))
            (should (equal (gethash "test-key" (nskk-romaji-table)) "テスト値")))
        (nskk-set-romaji-table original))
      (should (eq (nskk-romaji-table) original)))))

(nskk-describe "nskk-initialize-romaji-table"
  (nskk-it "populates both the hash table and the Prolog facts from the standard rules"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--romaji-table (make-hash-table :test 'equal)))
        (nskk-prolog-set-index 'romaji-to-kana 2 :trie)
        (nskk-initialize-romaji-table)
        (should (equal (gethash "ka" nskk--romaji-table) "か"))
        (should (equal (gethash "shi" nskk--romaji-table) "し"))
        (should (equal (nskk-prolog-query-value
                        (list 'romaji-to-kana "ka" '\?kana) '\?kana)
                       "か"))
        (should (= (hash-table-count nskk--romaji-table)
                   (length nskk--standard-romaji-rules)))))))

(nskk-describe "style-transaction registration API"
  (nskk-it "nskk-converter-register-style-transaction-hash-table registers a symbol exactly once"
    (let ((nskk--converter-style-transaction-hash-tables nil))
      (nskk-converter-register-style-transaction-hash-table 'nskk-test--registered-hash-var)
      (should (equal (nskk-converter-style-transaction-hash-tables)
                     '(nskk-test--registered-hash-var)))
      (nskk-converter-register-style-transaction-hash-table 'nskk-test--registered-hash-var)
      (should (= (length (nskk-converter-style-transaction-hash-tables)) 1))))

  (nskk-it "nskk-converter-register-style-transaction-variable registers a symbol exactly once"
    (let ((nskk--converter-style-transaction-variables nil))
      (nskk-converter-register-style-transaction-variable 'nskk-test--registered-plain-var)
      (should (equal (nskk-converter-style-transaction-variables)
                     '(nskk-test--registered-plain-var)))
      (nskk-converter-register-style-transaction-variable 'nskk-test--registered-plain-var)
      (should (= (length (nskk-converter-style-transaction-variables)) 1))))

  (nskk-it "a hash-table variable registered via the public API is staged and published like a built-in root"
    ;; Mirrors the real usage in src/nskk-azik.el:273 and
    ;; test/nskk-e2e-helpers.el:225 — registering through the public function
    ;; (not by let-binding the private list directly) and confirming the
    ;; registered variable actually participates in the transaction.
    (nskk-test-with-style-transaction-state
      (let ((extension-symbol 'nskk-test--registration-api-extension-table)
            (extension-table (make-hash-table :test 'equal)))
        (cl-progv (list extension-symbol) (list extension-table)
          (nskk-converter-register-style-transaction-hash-table extension-symbol)
          (nskk-converter-register-style 'registration-api-style
            (lambda ()
              (puthash 'k 'v (symbol-value extension-symbol))))
          (nskk-converter-load-style 'registration-api-style)
          (should (eq (gethash 'k (symbol-value extension-symbol)) 'v)))))))

;;;
;;; Seeded Property-Based Tests
;;;
(nskk-property-test conversion-pbt-determinism
  ((input romaji-string))
  (let ((result1 (nskk-test-convert-romaji input))
        (result2 (nskk-test-convert-romaji input)))
    (equal result1 result2))
  100)

(nskk-property-test conversion-pbt-returns-string
  ((input romaji-string))
  (let ((result (nskk-test-convert-romaji input)))
    (stringp result))
  100)

(nskk-property-test conversion-pbt-no-crash-on-arbitrary-input
  ((input romaji-string))
  (condition-case nil
      (progn (nskk-test-convert-romaji input) t)
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
  :body (should (equal expected (nskk-converter-lookup input))))

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

(nskk-property-test-seeded get-possible-completions/k-pbt-consistent-with-sync
  ((input romaji-basic))
  (equal (nskk-converter-get-possible-completions input)
         (nskk-converter-get-possible-completions/k input #'identity (lambda () nil)))
  50 3002)

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
        (should-not (nskk-converter-lookup "tempkey/k")))))

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
        (should (equal (nskk-converter-lookup "ka") "か"))))))

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
;;; Prolog fact tables asserted by nskk-converter-initialize
;;;
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
    (dolist (condition '(error quit))
      (nskk-test-with-style-transaction-state
        (let ((references (nskk-test--converter-style-state-references)))
          (nskk-converter-register-style
            'failing-initializer
            (lambda ()
              (nskk-converter-add-rule "new" "新")
              (nskk-prolog-assert '((style-staged partial)))
              (signal condition nil)))
          (should
            (eq (nskk-test--load-style-condition 'failing-initializer) condition))
          (nskk-test--should-retain-converter-style-state references)
          (should (equal (nskk-converter-lookup "old") "旧"))
          (should-not (nskk-converter-lookup "new"))
          (should (nskk-prolog-holds-p '(transaction-sentinel intact)))
          (should-not (nskk-prolog-holds-p '(style-staged partial)))))))
  (nskk-it
    "rolls back finalizer errors and quits"
    (dolist (condition '(error quit))
      (nskk-test-with-style-transaction-state
        (let ((references (nskk-test--converter-style-state-references)))
          (nskk-converter-register-style
            'failing-finalizer
            (lambda ()
              (nskk-converter-add-rule "new" "新")))
          (cl-letf
            (((symbol-function 'nskk--converter-populate-incomplete-markers)
                (lambda ()
                  (nskk-prolog-assert '((finalizer-staged partial)))
                  (signal condition nil))))
            (should
              (eq (nskk-test--load-style-condition 'failing-finalizer) condition)))
          (nskk-test--should-retain-converter-style-state references)
          (should (equal (nskk-converter-lookup "old") "旧"))
          (should-not (nskk-converter-lookup "new"))
          (should (nskk-prolog-holds-p '(transaction-sentinel intact)))
          (should-not (nskk-prolog-holds-p '(finalizer-staged partial)))))))
  (nskk-it
    "does not mutate state for an unknown style"
    (nskk-test-with-style-transaction-state
      (let ((references (nskk-test--converter-style-state-references)))
        (should-not (nskk-converter-load-style 'transaction-unknown))
        (nskk-test--should-retain-converter-style-state references)
        (should (equal (nskk-converter-lookup "old") "旧"))
        (should (nskk-prolog-holds-p '(transaction-sentinel intact))))))
  (nskk-it
    "publishes staged stores and preserves unrelated indexed facts"
    (nskk-test-with-style-transaction-state
      (let ((references (nskk-test--converter-style-state-references)))
        (nskk-converter-register-style
          'transaction-success
          (lambda ()
            (nskk-converter-add-rule "new" "新")
            (nskk-prolog-assert '((style-committed complete)))))
        (should
          (eq
            (nskk-converter-load-style 'transaction-success)
            'transaction-success))
        (cl-mapc
          (lambda (before after)
            (should-not (eq before after)))
          references
          (nskk-test--converter-style-state-references))
        (should-not (nskk-converter-lookup "old"))
        (should (equal (nskk-converter-lookup "new") "新"))
        (should (nskk-prolog-holds-p '(transaction-sentinel intact)))
        (should (nskk-prolog-holds-p '(style-committed complete))))))
  (nskk-it
    "restores exact state when keymap publication signals"
    (dolist (condition '(error quit))
      (nskk-test-with-style-transaction-state
        (let* ((references (nskk-test--converter-style-state-references))
               (mode-map-reference
              (let ((map (make-sparse-keymap)))
                (define-key map (kbd "C-c o") #'ignore)
                map))
               (mode-map-car (car mode-map-reference))
               (mode-map-cdr (cdr mode-map-reference))
               (extension-symbol 'nskk-test--converter-extension-table)
               (extension-table (make-hash-table :test 'equal))
               (nskk--converter-style-transaction-hash-tables (list extension-symbol))
               (active-style nskk-converter-romaji-style)
               (condition-data (list "publication failure" (make-symbol "payload")))
               (replace-keymap-contents
              (symbol-function 'nskk--converter-replace-keymap-contents))
               signaled)
          (puthash 'old 'intact extension-table)
          (cl-progv
            (list extension-symbol 'nskk-mode-map)
            (list extension-table mode-map-reference)
            (nskk-converter-register-style
              'failing-publication
              (lambda ()
                (nskk-converter-add-rule "new" "新")
                (nskk-prolog-assert '((style-published partial)))
                (puthash 'new 'partial (symbol-value extension-symbol))
                (define-key
                  (symbol-value 'nskk-mode-map)
                  (kbd "C-c n")
                  #'ignore)))
            (cl-letf
              (((symbol-function 'nskk--converter-replace-keymap-contents)
                  (lambda (target source)
                    (funcall replace-keymap-contents target source)
                    (setcar target 'corrupted-keymap-head)
                    (setcdr target (list 'corrupted-keymap-tail))
                    (signal condition condition-data))))
              (setq signaled (condition-case
                  caught
                  (progn
                    (nskk-converter-load-style 'failing-publication)
                    nil)
                  (quit caught)
                  (error caught))))
            (should (eq (car signaled) condition))
            (should (eq (cdr signaled) condition-data))
            (nskk-test--should-retain-converter-style-state references)
            (should (eq (symbol-value 'nskk-mode-map) mode-map-reference))
            (should (eq (car mode-map-reference) mode-map-car))
            (should (eq (cdr mode-map-reference) mode-map-cdr))
            (should (eq (symbol-value extension-symbol) extension-table))
            (should (eq (gethash 'old extension-table) 'intact))
            (should-not (gethash 'new extension-table))
            (should (eq nskk-converter-romaji-style active-style))
            (should (equal (nskk-converter-lookup "old") "旧"))
            (should-not (nskk-converter-lookup "new"))
            (should (nskk-prolog-holds-p '(transaction-sentinel intact)))
            (should-not (nskk-prolog-holds-p '(style-published partial))))))))
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
          (should (equal (nskk-converter-lookup "old") "旧"))
          (should
           (nskk-prolog-holds-p '(transaction-sentinel intact)))))))))

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
            ((index-bucket-tail-cache (make-hash-table :test 'equal)))
          (let ((nskk--romaji-table (make-hash-table :test 'equal))
                (romaji (copy-sequence "own"))
                (kana (copy-sequence "K仮名"))
                (shared (cons 'payload nil)))
            (nskk-prolog-clear-database)
          (nskk-prolog-set-index 'romaji-to-kana 2 :trie)
          (setcdr shared shared)
          (add-text-properties 0 1 (list 'payload shared) romaji)
          (add-text-properties 0 1 (list 'payload shared) kana)
          (nskk-converter-add-rule romaji kana)
          (let* ((entry (nskk--converter-find-hash-entry "own"))
                 (stored-romaji (car entry))
                 (stored-kana (cadr entry))
                 (stored-key-property (get-text-property 0 'payload stored-romaji))
                 (stored-value-property (get-text-property 0 'payload stored-kana)))
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
                   (first-property (get-text-property 0 'payload first))
                   (second-property (get-text-property 0 'payload second)))
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
              (should (equal (nskk-converter-lookup "own") stored-kana))))))))
    (nskk-it
      "distinguishes present nil, incomplete, and non-string identities"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test 'equal)))
          (let ((nskk--romaji-table (make-hash-table :test 'equal))
                (object (list 'non-string)))
            (nskk-prolog-clear-database)
            (nskk-prolog-set-index 'romaji-to-kana 2 :trie)
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
                (nskk-prolog-query (list 'romaji-to-kana "object" '\?kana))))))))
    (nskk-it
      "keeps non-string values out of Prolog and detaches their keys"
      (nskk-prolog-test-with-isolated-db
        (nskk-prolog-with-database-fields
            ((index-bucket-tail-cache (make-hash-table :test 'equal)))
          (let ((nskk--romaji-table (make-hash-table :test 'equal))
                (romaji (copy-sequence "metadata"))
                (value (list 'metadata)))
            (nskk-prolog-clear-database)
            (nskk-prolog-set-index 'romaji-to-kana 2 :trie)
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
            ((index-bucket-tail-cache (make-hash-table :test 'equal)))
          (let ((nskk--romaji-table (make-hash-table :test 'equal)))
            (nskk-prolog-clear-database)
            (nskk-prolog-set-index 'romaji-to-kana 2 :trie)
          (let ((first (list (list 'romaji-to-kana "duplicate" "一")))
                (second (list (list 'romaji-to-kana "duplicate" "二")))
                (survivor (list (list 'romaji-to-kana "other" "他"))))
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
                  (list 'romaji-to-kana "duplicate" "新"))))
            (should
              (equal
                (nskk-prolog-query-value
                  (list 'romaji-to-kana "duplicate" '\?kana)
                  '\?kana)
                "二"))
            (should (equal (nskk-converter-lookup "duplicate") "新"))
            (nskk-converter-remove-rule "duplicate")
            (should
              (equal
                (nskk-prolog-query-value
                  (list 'romaji-to-kana "duplicate" '\?kana)
                  '\?kana)
                "新"))
            (should-not (nskk--converter-find-hash-entry "duplicate")))))))
    (nskk-it
      "restores exact hash entries for every journal failure boundary"
      (dolist (state '(absent present-nil present-value))
        (dolist (fault-type '(error quit))
          (dolist (timing '(before after))
            (let* ((nskk--romaji-table (make-hash-table :test 'equal))
                   (old-key (copy-sequence "journal"))
                   (old-value (and (eq state 'present-value) (list 'old-value)))
                   (marker (list state fault-type timing)))
              (unless (eq state 'absent)
                (puthash old-key old-value nskk--romaji-table))
              (let ((caught
                    (nskk-test--converter-catch-condition
                      (lambda ()
                        (nskk--converter-call-with-hash-journal
                          (copy-sequence "journal")
                          (lambda ()
                            (when (eq timing 'before)
                              (signal fault-type (list marker)))
                            (nskk--converter-replace-hash-entry
                              "journal"
                              (copy-sequence "journal")
                              (list 'new-value))
                            (signal fault-type (list marker))))))))
                (should (eq (car caught) fault-type))
                (should (eq (cadr caught) marker))
                (if (eq state 'absent) (progn
                    (should (= (hash-table-count nskk--romaji-table) 0))
                    (should-not (nskk--converter-find-hash-entry "journal")))
                  (let ((entry (nskk--converter-find-hash-entry "journal")))
                    (should (= (hash-table-count nskk--romaji-table) 1))
                    (should (eq (car entry) old-key))
                    (should (eq (cadr entry) old-value))))))))))
    (nskk-it
      "does not mutate either store when caller graph copying fails"
      (dolist (operation '(add remove))
        (dolist (fault-type '(error quit))
          (nskk-prolog-test-with-isolated-db
            (nskk-prolog-with-database-fields
                ((index-bucket-tail-cache (make-hash-table :test 'equal)))
              (let ((nskk--romaji-table (make-hash-table :test 'equal)))
              (nskk-prolog-clear-database)
              (nskk-prolog-set-index 'romaji-to-kana 2 :trie)
              (nskk-converter-add-rule "copy-failure" "旧")
              (let* ((entry-before (nskk--converter-find-hash-entry "copy-failure"))
                     (state-before (nskk-test--converter-rule-state "copy-failure"))
                     (marker (list operation fault-type))
                     caught)
                (cl-letf
                  (((symbol-function 'nskk-prolog-copy-term)
                      (lambda (_term)
                        (signal fault-type (list marker)))))
                  (setq caught (nskk-test--converter-catch-condition
                      (lambda ()
                        (if (eq operation 'add) (nskk-converter-add-rule "copy-failure" "新")
                          (nskk-converter-remove-rule "copy-failure"))))))
                (should (eq (car caught) fault-type))
                (should (eq (cadr caught) marker))
                (let ((entry-after (nskk--converter-find-hash-entry "copy-failure")))
                  (should (eq (car entry-after) (car entry-before)))
                  (should (eq (cadr entry-after) (cadr entry-before))))
                (nskk-test--converter-should-retain-rule-state state-before "copy-failure")
                (if (eq operation 'add) (progn
                    (nskk-converter-add-rule "copy-failure" "新")
                    (should (equal (nskk-converter-lookup "copy-failure") "新")))
                  (nskk-converter-remove-rule "copy-failure")
                  (should-not (nskk--converter-find-hash-entry "copy-failure"))))))))))
    (nskk-it
      "rolls back add callbacks before and after hash publication"
      (dolist (fault-type '(error quit))
        (dolist (timing '(before after))
          (nskk-prolog-test-with-isolated-db
            (nskk-prolog-with-database-fields
                ((index-bucket-tail-cache (make-hash-table :test 'equal)))
              (let ((nskk--romaji-table (make-hash-table :test 'equal)))
              (nskk-prolog-clear-database)
              (nskk-prolog-set-index 'romaji-to-kana 2 :trie)
              (nskk-converter-add-rule "atomic-add" "旧")
              (let* ((entry-before (nskk--converter-find-hash-entry "atomic-add"))
                     (state-before (nskk-test--converter-rule-state "atomic-add"))
                     (marker (list fault-type timing))
                     (original (symbol-function 'nskk--converter-replace-hash-entry))
                     caught)
                (cl-letf
                  (((symbol-function 'nskk--converter-replace-hash-entry)
                      (lambda (lookup-key new-key value)
                        (when (eq timing 'before)
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
                      (list 'romaji-to-kana "atomic-add" '\?kana)
                      '\?kana)
                    "旧"))
                (nskk-converter-add-rule "atomic-add" "新")
                (should (equal (nskk-converter-lookup "atomic-add") "新")))))))))
    (nskk-it
      "rolls back remove callbacks before and after hash deletion"
      (dolist (fault-type '(error quit))
        (dolist (timing '(before after))
          (nskk-prolog-test-with-isolated-db
            (nskk-prolog-with-database-fields
                ((index-bucket-tail-cache (make-hash-table :test 'equal)))
              (let ((nskk--romaji-table (make-hash-table :test 'equal)))
              (nskk-prolog-clear-database)
              (nskk-prolog-set-index 'romaji-to-kana 2 :trie)
              (nskk-converter-add-rule "atomic-remove" "旧")
              (let* ((entry-before (nskk--converter-find-hash-entry "atomic-remove"))
                     (state-before (nskk-test--converter-rule-state "atomic-remove"))
                     (marker (list fault-type timing))
                     (original (symbol-function 'nskk--converter-delete-hash-entry))
                     caught)
                (cl-letf
                  (((symbol-function 'nskk--converter-delete-hash-entry)
                      (lambda (lookup-key)
                        (when (eq timing 'before)
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
                      (list 'romaji-to-kana "atomic-remove" '\?kana)
                      '\?kana)
                    "旧"))
                (nskk-converter-remove-rule "atomic-remove")
                (should-not (nskk--converter-find-hash-entry "atomic-remove"))))))))))

(nskk-describe
    "unbound mode map style transactions"
    (nskk-it
      "preserves true unbound reads and unmodified staging"
      (nskk-test-with-style-transaction-state
        (let (state)
          (cl-progv
              (list 'nskk-mode-map)
              (list (make-symbol "outer-mode-map"))
            (makunbound 'nskk-mode-map)
            (should-not (boundp 'nskk-mode-map))
            (setq state
                  (nskk--converter-stage-style-state
                   (lambda ()
                     (should-not (boundp 'nskk-mode-map))
                     (should-error
                      (symbol-value 'nskk-mode-map)
                      :type 'void-variable))))
            (should-not (boundp 'nskk-mode-map))
            (should-not (plist-get state :mode-map-bound-p))
            (should-not (plist-get state :mode-map))
            (setq state
                  (nskk--converter-stage-style-state #'ignore))
            (should-not (boundp 'nskk-mode-map))
            (should-not (plist-get state :mode-map-bound-p))
            (should-not (plist-get state :mode-map))))))
    (nskk-it
      "contains initializer assignments across faults and retry"
      (dolist (condition-symbol '(error quit))
        (nskk-test-with-style-transaction-state
          (let ((assigned-map (make-sparse-keymap))
                (condition-data (list "initializer failure" condition-symbol))
                retry-map
                state
                signaled)
            (cl-progv
                (list 'nskk-mode-map)
                (list (make-symbol "outer-mode-map"))
              (makunbound 'nskk-mode-map)
              (setq signaled
                    (condition-case caught
                        (progn
                          (nskk--converter-stage-style-state
                           (lambda ()
                             (setq nskk-mode-map assigned-map)
                             (should (boundp 'nskk-mode-map))
                             (should
                              (eq (symbol-value 'nskk-mode-map)
                                  assigned-map))
                             (signal condition-symbol condition-data)))
                          nil)
                      (quit caught)
                      (error caught)))
              (should (eq (car signaled) condition-symbol))
              (should (eq (cdr signaled) condition-data))
              (should-not (boundp 'nskk-mode-map))
              (setq retry-map (make-sparse-keymap))
              (setq state
                    (nskk--converter-stage-style-state
                     (lambda ()
                       (setq nskk-mode-map retry-map))))
              (should-not (boundp 'nskk-mode-map))
              (should (plist-get state :mode-map-bound-p))
              (should (eq (plist-get state :mode-map) retry-map)))))))
    (nskk-it
      "publishes a detached initializer map and retains its shell"
      (nskk-test-with-style-transaction-state
        (let* ((extension-symbol
                'nskk-test--converter-unbound-mode-map-extension)
               (extension-table (make-hash-table :test 'equal))
               (nskk--converter-style-transaction-hash-tables
                (list extension-symbol))
               assigned-map
               public-shell
               shared
               staged-extension
               staged-shared
               state)
          (cl-progv
              (list extension-symbol 'nskk-mode-map)
              (list extension-table (make-symbol "outer-mode-map"))
            (makunbound 'nskk-mode-map)
            (setq state
                  (nskk--converter-stage-style-state
                   (lambda ()
                     (setq assigned-map (make-sparse-keymap))
                     (setq shared (cons 'unbound-shared nil))
                     (setcdr shared shared)
                     (puthash "unbound-shared" shared nskk--romaji-table)
                     (puthash "unbound-shared" shared
                              (symbol-value extension-symbol))
                     (setcdr assigned-map
                             (cons
                              (cons 'nskk-test-unbound-shared shared)
                              (cdr assigned-map)))
                     (setq nskk-mode-map assigned-map))))
            (should-not (boundp 'nskk-mode-map))
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
               (assq 'nskk-test-unbound-shared
                     (cdr (plist-get state :mode-map))))
              staged-shared))
            (should (eq (cdr staged-shared) staged-shared))
            (nskk--converter-publish-style-state state)
            (should (boundp 'nskk-mode-map))
            (setq public-shell (symbol-value 'nskk-mode-map))
            (should-not (eq public-shell assigned-map))
            (let* ((public-shared
                    (gethash "unbound-shared" nskk--romaji-table))
                   (public-extension
                    (gethash "unbound-shared"
                             (symbol-value extension-symbol)))
                   (public-map-shared
                    (cdr
                     (assq 'nskk-test-unbound-shared
                           (cdr public-shell)))))
              (should (eq public-shared public-extension))
              (should (eq public-shared public-map-shared))
              (should-not (eq public-shared staged-shared))
              (should (eq (cdr public-shared) public-shared))
              (setcar staged-shared 'retained-mutated)
              (setcdr assigned-map
                      (cons
                       (cons 'nskk-test-retained staged-shared)
                       (cdr assigned-map)))
              (should (eq (car public-shared) 'unbound-shared))
              (should-not
               (assq 'nskk-test-retained (cdr public-shell))))
            (let ((retry-state
                   (nskk--converter-stage-style-state
                    (lambda ()
                      (define-key
                       (symbol-value 'nskk-mode-map)
                       [f24]
                       #'ignore)))))
              (should (eq (symbol-value 'nskk-mode-map)
                          public-shell))
              (should-not (lookup-key public-shell [f24]))
              (nskk--converter-publish-style-state retry-state)
              (should (eq (symbol-value 'nskk-mode-map)
                          public-shell))
              (should (eq (lookup-key public-shell [f24])
                          #'ignore))))))))

(nskk-describe
    "mode map binding state publication"
    (nskk-it
      "publishes every boundness and value transition"
      (dolist (transition
               '((unbound map)
                       (bound-nil bound-nil)
                       (bound-nil map)
                       (map bound-nil)
                       (map map)
                       (map unbound)
                       (bound-nil unbound)
                       (unbound unbound)))
        (nskk-test-with-style-transaction-state
          (let* ((initial-kind (nth 0 transition))
                 (target-kind (nth 1 transition))
                 (initial-map (make-sparse-keymap))
                 (target-map (make-sparse-keymap))
                 state
                 published-map)
            (define-key initial-map [f20] #'ignore)
            (define-key target-map [f21] #'forward-char)
            (cl-progv
                (list 'nskk-mode-map)
                (list (make-symbol "outer-mode-map"))
              (pcase initial-kind
                ('unbound
                 (makunbound 'nskk-mode-map))
                ('bound-nil
                 (set 'nskk-mode-map nil))
                ('map
                 (set 'nskk-mode-map initial-map)))
              (setq state
                    (nskk--converter-stage-style-state
                     (lambda ()
                       (pcase target-kind
                         ('unbound
                          (makunbound 'nskk-mode-map))
                         ('bound-nil
                          (set 'nskk-mode-map nil))
                         ('map
                          (set 'nskk-mode-map target-map))))))
              (pcase initial-kind
                ('unbound
                 (should-not (boundp 'nskk-mode-map)))
                ('bound-nil
                 (should (boundp 'nskk-mode-map))
                 (should-not (symbol-value 'nskk-mode-map)))
                ('map
                 (should (eq (symbol-value 'nskk-mode-map)
                             initial-map))))
              (should
               (eq (plist-get state :mode-map-bound-p)
                   (not (eq target-kind 'unbound))))
              (pcase target-kind
                ('unbound
                 (should-not (plist-get state :mode-map)))
                ('bound-nil
                 (should-not (plist-get state :mode-map)))
                ('map
                 (should (eq (plist-get state :mode-map) target-map))))
              (nskk--converter-publish-style-state state)
              (pcase target-kind
                ('unbound
                 (should-not (boundp 'nskk-mode-map)))
                ('bound-nil
                 (should (boundp 'nskk-mode-map))
                 (should-not (symbol-value 'nskk-mode-map)))
                ('map
                 (should (boundp 'nskk-mode-map))
                 (setq published-map
                       (symbol-value 'nskk-mode-map))
                 (if (eq initial-kind 'map)
                     (should (eq published-map initial-map))
                   (should-not (eq published-map target-map)))
                 (should (eq (lookup-key published-map [f21])
                             #'forward-char))))))))))

(nskk-describe
    "style publication cleanup faults"
    (nskk-it
      "preserves the original condition across one-shot cleanup faults"
      (dolist (original-condition '(error quit))
        (dolist (cleanup-condition '(error quit))
          (dolist (cleanup-position '(before after))
            (nskk-test-with-style-transaction-state
              (let* ((extension-a
                      'nskk-test--converter-cleanup-extension-a)
                     (extension-b
                      'nskk-test--converter-cleanup-extension-b)
                     (old-extension-a
                      (make-hash-table :test 'equal))
                     (old-extension-b
                      (make-hash-table :test 'equal))
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
                (define-key old-map [f19] #'backward-char)
                (setq old-map-car (car old-map))
                (setq old-map-cdr (cdr old-map))
                (puthash "old-a" t old-extension-a)
                (puthash "old-b" t old-extension-b)
                (cl-progv
                    (list extension-a extension-b
                          'nskk-mode-map)
                    (list old-extension-a old-extension-b old-map)
                  (setq state
                        (nskk--converter-stage-style-state
                         (lambda ()
                           (puthash "new" t
                                    (symbol-value extension-a))
                           (puthash "new" t
                                    (symbol-value extension-b))
                           (define-key
                            (symbol-value 'nskk-mode-map)
                            [f22]
                            #'forward-char))))
                  (setq original-replace
                        (symbol-function
                         'nskk--converter-replace-keymap-contents))
                  (setq cleanup-watcher
                        (lambda (symbol value operation _where)
                          (when (and cleanup-p
                                     (eq operation 'set)
                                     (eq symbol extension-a)
                                     (not cleanup-faulted-p)
                                     (not cleanup-setting-p))
                            (setq cleanup-faulted-p t)
                            (when (eq cleanup-position 'after)
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
                               'nskk--converter-replace-keymap-contents)
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
                  (should (boundp 'nskk-mode-map))
                  (should (eq (symbol-value 'nskk-mode-map)
                              old-map))
                  (should (eq (car old-map) old-map-car))
                  (should (eq (cdr old-map) old-map-cdr))
                  (should-not (lookup-key old-map [f22]))
                  (nskk--converter-publish-style-state state)
                  (should (eq (symbol-value 'nskk-mode-map)
                              old-map))
                  (should (eq (lookup-key old-map [f22])
                              #'forward-char))
                  (should
                   (gethash "new" (symbol-value extension-a)))
                  (should
                   (gethash "new" (symbol-value extension-b)))))))))))

(nskk-describe
    "persistent style publication cleanup faults"
    (nskk-it
      "maximizes restoration and keeps the state retryable"
      (dolist (original-condition '(error quit))
        (nskk-test-with-style-transaction-state
          (let* ((extension-a
                  'nskk-test--converter-persistent-extension-a)
                 (extension-b
                  'nskk-test--converter-persistent-extension-b)
                 (old-extension-a
                  (make-hash-table :test 'equal))
                 (old-extension-b
                  (make-hash-table :test 'equal))
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
            (define-key old-map [f18] #'backward-char)
            (setq old-map-car (car old-map))
            (setq old-map-cdr (cdr old-map))
            (puthash "old-a" t old-extension-a)
            (puthash "old-b" t old-extension-b)
            (cl-progv
                (list extension-a extension-b
                      'nskk-mode-map)
                (list old-extension-a old-extension-b old-map)
              (setq state
                    (nskk--converter-stage-style-state
                     (lambda ()
                       (puthash "new" t (symbol-value extension-a))
                       (puthash "new" t (symbol-value extension-b))
                       (define-key
                        (symbol-value 'nskk-mode-map)
                        [f23]
                        #'forward-char))))
              (setq original-replace
                    (symbol-function
                     'nskk--converter-replace-keymap-contents))
              (setq cleanup-watcher
                    (lambda (symbol _value operation _where)
                      (when (and cleanup-p
                                 (eq operation 'set)
                                 (eq symbol extension-a))
                        (setq extension-fault-count
                              (1+ extension-fault-count))
                        (error "persistent extension cleanup failure"))))
              (unwind-protect
                  (progn
                    (add-variable-watcher extension-a cleanup-watcher)
                    (cl-letf
                        (((symbol-function
                           'nskk--converter-replace-keymap-contents)
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
              (should (boundp 'nskk-mode-map))
              (should (eq (symbol-value 'nskk-mode-map)
                          old-map))
              (should (eq (car old-map) old-map-car))
              (should (eq (cdr old-map) old-map-cdr))
              (should-not (lookup-key old-map [f23]))
              (nskk--converter-publish-style-state state)
              (should (eq (symbol-value 'nskk-mode-map)
                          old-map))
              (should (eq (lookup-key old-map [f23])
                          #'forward-char))
              (should (gethash "new" (symbol-value extension-a)))
              (should
               (gethash "new" (symbol-value extension-b)))))))))

(provide 'nskk-converter-test)

;;; nskk-converter-test.el ends here
