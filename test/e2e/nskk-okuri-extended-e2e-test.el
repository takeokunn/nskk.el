;;; nskk-okuri-extended-e2e-test.el --- E2E tests for extended okurigana patterns  -*- lexical-binding: t; -*-

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

;; E2E tests for multi-character, sokuon, and katakana okurigana patterns.
;; Each test injects its own dict entries to cover the specific okurigana key.
;;
;; "Multi-character okurigana" means the okurigana suffix appended after the
;; kanji stem contains more than one kana character.  Examples:
;;   変える (かえ*る): okurigana = "える" (2 chars)
;;   勝った (かっ*た): okurigana = "った" (2 chars, sokuon + kana)
;;
;; Dict key format (reminder):
;;   reading-kana + lowercase okurigana consonant (or vowel)
;;   e.g. "かe"  → ("変")  triggers 変える  (vowel okurigana + "ru" continuation)
;;        "かt"  → ("勝")  triggers 勝った  (sokuon okurigana, single "t" in key)
;;        "おもu" → ("思") triggers 思う    (vowel okurigana "u")
;;   katakana mode: reading stays katakana
;;        "カk"  → ("書")  triggers 書ク    (katakana reading + consonant)
;;
;; Sections:
;;   1. Katakana mode okurigana
;;   2. Property-based tests
;;
;; NOTE: Standard consonant okurigana (baseline), sokuon, and multi-character
;; vowel okurigana are covered in nskk-e2e-okurigana.el.  This file adds the
;; katakana-mode variants and property-based tests only.

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(eval-when-compile (require 'cl-lib))
(require 'nskk-pbt-generators)

(nskk-describe "katakana mode okurigana"
  (nskk-it "triggers conversion on KaKu in katakana mode (dict key カk)"
    ;; In katakana mode: Ka → ▽カ; K triggers okurigana with consonant k;
    ;; u completes "ku"→"く"→"ク".  Dict key is "カk" (katakana reading).
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "commits KaKu to 書ク in katakana mode via C-j"
    ;; Result is kanji + katakana okurigana: 書ク (not 書く).
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書ク")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits MiRu to 見ル in katakana mode via C-j"
    ;; MiRu in katakana mode: Mi → ▽ミ; R triggers okurigana with consonant r;
    ;; u completes "ru"→"る"→"ル".  Dict key "ミr" → candidate "見"; result 見ル.
    (let ((dict '(("ミr" . ("見")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Mi")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "見ル")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits vowel okurigana AI to 愛イ in katakana mode via C-j"
    ;; A (uppercase, katakana mode): starts preedit; vowel "a" → reading "ア".
    ;; I (uppercase): vowel okurigana trigger; immediately emits "イ" and fires
    ;;   conversion with key "アi" (katakana reading + vowel char).
    ;; C-j commits: buffer = 愛 + "イ".
    (let ((dict '(("アi" . ("愛")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "愛イ")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "katakana mode okurigana result differs from hiragana mode"
    ;; The same key sequence KaKu produces 書ク (katakana) in katakana mode
    ;; versus 書く (hiragana) in hiragana mode.
    ;; This test asserts the katakana-mode result directly and verifies that
    ;; the output is NOT the hiragana-mode output.
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        ;; Katakana mode: okurigana is katakana ク, not hiragana く.
        (nskk-e2e-assert-buffer "書ク")
        (should-not (equal (buffer-string) "書く")))))

  (nskk-it "sokuon okurigana KaTta produces 勝った in katakana mode"
    ;; In katakana mode the sokuon mechanism is the same as in hiragana mode
    ;; but the reading and okurigana kana are both katakana.
    ;; Ka → ▽カ; T → okurigana trigger (t), romaji="t";
    ;; t → "t"+"t" → sokuon → emits ッ (katakana sokuon) → fires conversion with key "カt";
    ;; a → "ta" → "た" → "タ" (appended after ッ).
    ;; C-j commits: buffer = kanji + "ッタ".
    ;;
    ;; Note: The resulting okurigana in katakana mode is "ッタ" (katakana).
    ;; The dict key is "カt" (katakana reading + single "t").
    (let ((dict '(("カt" . ("勝")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "勝ッタ")
        (nskk-e2e-assert-henkan-phase nil)))))

;;;;
;;;; Property-Based Tests
;;;;

(nskk-deftest-cases okuri-extended-known-patterns
  ((?k . "k") (?t . "t") (?r . "r") (?s . "s") (?n . "n"))
  :description "Each okurigana consonant is a valid single character"
  :body (should (characterp (car item))))

(nskk-property-test okuri-extended-okurigana-consonant-is-char
  ((c okurigana-consonant))
  (should (stringp c))
  50)

(nskk-property-test okuri-extended-pattern-is-string
  ((p okurigana-pattern))
  (should (stringp p))
  50)

(nskk-property-test okuri-extended-pattern-does-not-crash-e2e
  ((p okurigana-pattern))
  (nskk-e2e-with-buffer 'hiragana nil
    (condition-case err
        (nskk-e2e-type p)
      (error (ert-fail (format "okurigana pattern %s crashed: %s"
                               p (error-message-string err)))))
    t)
  30)

(nskk-describe "Okurigana extended property: consonant variety"
  (nskk-it "any okurigana consonant combined with Ka prefix does not crash"
    (dotimes (_ 25)
      (nskk-for-all ((c okurigana-consonant))
        (nskk-e2e-with-buffer 'hiragana nil
          (condition-case err
              (nskk-e2e-type (concat "Ka" c))
            (error (ert-fail (format "Ka+%s crashed: %s"
                                     c (error-message-string err)))))
          t)))))

(provide 'nskk-okuri-extended-e2e-test)

;;; nskk-okuri-extended-e2e-test.el ends here
