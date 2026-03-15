;;; nskk-henkan-e2e-test.el --- E2E conversion tests for NSKK  -*- lexical-binding: t; -*-

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

;; E2E tests for conversion/henkan (DDSKK §4.3 Conversion modes).
;; Covers: SPC conversion, candidate cycling (C-n/C-p/x), C-j/RET commit,
;; C-g cancel, backspace during conversion, sentence-level integration,
;; SPC/X key dispatch, edge cases.

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(eval-when-compile (require 'cl-lib))

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
;;;; Conversion Roundtrip Cases
;;;;

(nskk-deftest-table conversion-roundtrip
  :columns (reading romaji first-cand)
  :rows (("かんじ"   "Kanji"  "漢字")
         ("へんかん" "Henkan" "変換")
         ("にほん"   "Nihon"  "日本"))
  :body
  (nskk-e2e-with-buffer 'hiragana (list (cons reading (list first-cand)))
    (nskk-e2e-type romaji)
    (nskk-e2e-type "SPC")
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-buffer first-cand
                            (format "conversion %S → %S failed" romaji first-cand))))

;;;;
;;;; Henkan Phase Nil After Commit Cases
;;;;

(nskk-deftest-table henkan-phase-nil-after-commit
  :columns (romaji)
  :rows (("Kanji") ("Henkan") ("Nihon"))
  :body
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type romaji)
    (nskk-e2e-type "SPC")
    (nskk-e2e-type "C-j")
    (should (null (nskk-state-henkan-phase nskk-current-state)))))

;;;;
;;;; C-j During Preedit Tests
;;;;

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

;;;;
;;;; SPC Key in Various Modes
;;;;

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

;;;;
;;;; X Key Tests (Previous Candidate)
;;;;

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

;;;;
;;;; Sentence-Level Integration Tests
;;;;
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

  (nskk-it "restores kana reading to preedit (▽) after C-g cancel"
    (let ((dict '(("かわ" . ("川" "河")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "wa")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase 'on)
        (nskk-e2e-assert-buffer "▽かわ")))))

;;;;
;;;; Edge Case Tests
;;;;

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

;;;;
;;;; Candidate List Phase Tests
;;;;

;;; Test dictionary (shared by candidate list sections)

(defconst nskk-e2e--kanji-7cands-dict
  '(("かんじ" . ("漢字" "感じ" "幹事" "換字" "貫地" "刊事" "肝事")))
  "Seven-candidate dict entry for かんじ, used in candidate-list E2E tests.
Indices 0-6: 漢字 感じ 幹事 換字 貫地 刊事 肝事.")

;;;;
;;;; Entering List Phase
;;;;

(nskk-describe "entering candidate list phase"
  (nskk-it "sets henkan-phase to list after 5 SPC presses"
    ;; SPC#1 starts conversion (preedit→converting, count=1, shows 漢字).
    ;; SPC#2..#4 cycle inline (count=2,3,4 < 5 → select-next).
    ;; SPC#5 triggers show-list-next (count=5 >= 5) → phase = 'list.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")   ; SPC#1: start-conversion
        (nskk-e2e-type "SPC")   ; SPC#2: select-next
        (nskk-e2e-type "SPC")   ; SPC#3: select-next
        (nskk-e2e-type "SPC")   ; SPC#4: select-next
        (nskk-e2e-type "SPC"))  ; SPC#5: show-list-next → 'list
      (nskk-then
        (nskk-e2e-assert-henkan-phase 'list "Phase must be 'list after SPC x5"))))

  (nskk-it "sets nskk--henkan-candidate-list-active to non-nil"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))
      (nskk-then
        (should nskk--henkan-candidate-list-active))))

  (nskk-it "is still in converting state while in list phase"
    ;; nskk-converting-p returns t for all henkan phases including 'list.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))
      (nskk-then
        (nskk-e2e-assert-converting))))

  (nskk-context "before the 5th SPC"
    (nskk-it "is still in active (inline) phase after 4 SPC presses"
      ;; count=4 < 5: inline cycling, not yet list phase.
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
        (nskk-given
          (nskk-e2e-type "Kanji"))
        (nskk-when
          (nskk-e2e-type "SPC")  ; SPC#1: start-conversion
          (nskk-e2e-type "SPC")  ; SPC#2: select-next
          (nskk-e2e-type "SPC")  ; SPC#3: select-next
          (nskk-e2e-type "SPC")) ; SPC#4: select-next (count=4 < 5)
        (nskk-then
          (nskk-e2e-assert-henkan-phase 'active "Phase must be 'active before SPC x5")
          (should-not nskk--henkan-candidate-list-active))))))

;;;;
;;;; Key Selection in List Phase
;;;;

(nskk-describe "candidate selection by key in list phase"
  ;; In list phase after SPC x5: current-index = 3 (see commentary above).
  ;; nskk-candidate-list-select-by-key is active via nskk-henkan-select-candidate-by-key-function.
  ;; Pressing a key commits the candidate and ends conversion.

  (nskk-it "pressing 'a' selects candidate at page position 0 (index 3 = 換字)"
    ;; 'a' → pos=0 → absolute = 3 + 0 = 3 → "換字"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "a"))
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "換字" "Key 'a' in list phase must commit 換字 (index 3)"))))

  (nskk-it "pressing 's' selects candidate at page position 1 (index 4 = 貫地)"
    ;; 's' → pos=1 → absolute = 3 + 1 = 4 → "貫地"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "s")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "貫地" "Key 's' in list phase must commit 貫地 (index 4)")))

  (nskk-it "pressing 'd' selects candidate at page position 2 (index 5 = 刊事)"
    ;; 'd' → pos=2 → absolute = 3 + 2 = 5 → "刊事"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "d")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "刊事" "Key 'd' in list phase must commit 刊事 (index 5)")))

  (nskk-it "pressing 'f' selects candidate at page position 3 (index 6 = 肝事)"
    ;; 'f' → pos=3 → absolute = 3 + 3 = 6 → "肝事"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "f")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "肝事" "Key 'f' in list phase must commit 肝事 (index 6)")))

  (nskk-it "pressing 'j' does not commit when page position 4 is out of range"
    ;; 'j' → pos=4 → absolute = 3 + 4 = 7 → out of range for 7-candidate list.
    ;; nskk-candidate-list-select-by-key returns nil for out-of-range index,
    ;; so nskk--try-candidate-selection returns nil and 'j' is handled as
    ;; romaji self-insert (じ in hiragana mode), not a commit.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      ;; 'j' is out-of-range; conversion must still be active
      (nskk-e2e-type "j")
      (nskk-e2e-assert-converting)))

  (nskk-it "key selection clears conversion state (phase → nil)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-type "a")
      (nskk-e2e-assert-henkan-phase nil "After key selection phase must be nil"))))

;;;;
;;;; 'x' in List Phase (Previous Page)
;;;;

(nskk-describe "x key in list phase"
  (nskk-it "stays in list phase after x (previous page)"
    ;; x → nskk-previous-candidate → show-list-prev; henkan-phase stays 'list.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "x"))
      (nskk-then
        (nskk-e2e-assert-henkan-phase 'list "Phase must remain 'list after x"))))

  (nskk-it "remains in converting state after x"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "x")
      (nskk-e2e-assert-converting)))

  (nskk-it "key selection after x commits the correct candidate from previous page"
    ;; After SPC x5 → list phase (page start = index 3).
    ;; x → prev page: prev-start = 3 - 7 = -4 → clamped to 0.
    ;;   current-index = 0, page shows candidates 0..6 starting at index 0.
    ;; 'a' → pos=0 → absolute = 0 + 0 = 0 → "漢字"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "x")
      (nskk-e2e-assert-henkan-phase 'list)
      ;; After x (prev-start = max(0, 3-7) = 0), current-index = 0.
      ;; 'a' → pos=0 → absolute = 0 + 0 = 0 → "漢字"
      (nskk-e2e-type "a")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字" "After x then 'a', should commit 漢字 (index 0)"))))

;;;;
;;;; C-g in List Phase (Cancel Conversion)
;;;;

(nskk-describe "C-g in list phase"
  (nskk-it "rolls back to preedit (▽) state"
    ;; C-g → nskk-handle-cancel → 'rollback-to-reading → nskk-rollback-conversion.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "C-g"))
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase 'on "C-g in list phase must return to ▽ preedit state"))))

  (nskk-it "restores kana reading to preedit (▽) buffer after cancel"
    ;; nskk-rollback-conversion restores ▽ preedit with kana reading.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-not-converting)
      ;; Buffer should contain ▽ + kana reading (DDSKK-compatible rollback).
      (nskk-e2e-assert-buffer "▽かんじ" "C-g must return to ▽ preedit with kana reading")))

  (nskk-it "clears nskk--henkan-candidate-list-active after cancel"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (should nskk--henkan-candidate-list-active)
      (nskk-e2e-type "C-g")
      (should-not nskk--henkan-candidate-list-active))))

;;;;
;;;; RET in List Phase (Commit Current Candidate)
;;;;

(nskk-describe "RET in list phase"
  (nskk-it "commits the current candidate (page-start index) without newline"
    ;; RET → nskk-handle-return → 'commit-candidate → nskk-commit-current.
    ;; Current-index after SPC x5 = 3 → "換字".
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "RET"))
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase nil "After RET, phase must be nil")
        ;; RET commits current-index=3 → "換字"
        (nskk-e2e-assert-buffer "換字" "RET in list phase must commit current candidate 換字"))))

  (nskk-it "ends conversion state after RET"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-not-converting)))

  (nskk-it "does not insert a newline (buffer contains only the committed candidate)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "RET")
      ;; No trailing newline: buffer-string is exactly the committed kanji.
      (should-not (string-match-p "\n" (buffer-string))))))

;;;;
;;;; Candidate List Phase Properties
;;;;

(nskk-describe "candidate list phase properties"
  ;; Exhaustive test: each selection key (a/s/d/f) must produce a non-empty
  ;; committed string from the 7-candidate list when there is a valid mapping.
  ;; Keys j/k/l map to out-of-range indices for a page starting at index 3 with
  ;; only 4 remaining candidates (indices 3-6), so they must NOT commit.

  (nskk-property-test-exhaustive candidate-list-valid-keys-commit-and-end-conversion
    '(?a ?s ?d ?f)
    ;; item = one of the valid selection keys that maps to an in-range candidate.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      ;; Dispatch the selection key as a character event
      (nskk-e2e--dispatch-event item)
      ;; After a valid selection key, conversion must have ended
      (and (not (nskk-converting-p))
           ;; Buffer must contain a non-empty string (the committed candidate)
           (not (string-empty-p (buffer-string))))))

  (nskk-property-test-exhaustive candidate-list-out-of-range-keys-keep-converting
    '(?j ?k)
    ;; item = selection keys that map to out-of-range indices for a 7-candidate
    ;; list with page starting at index 3:
    ;;   'j' → pos=4 → absolute=7 (>= 7), 'k' → pos=5 → absolute=8 (>= 7).
    ;; NOTE: '?l' is excluded here because 'l' is bound to nskk-handle-l in
    ;; nskk-mode-map with key-action = kakutei-then-latin in converting state.
    ;; That dedicated handler fires BEFORE any candidate-selection logic,
    ;; so 'l' always commits the current candidate — it does not stay converting.
    ;; nskk-candidate-list-select-by-key returns nil → no commit → still converting.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e--dispatch-event item)
      ;; Out-of-range key: conversion must still be active
      (nskk-converting-p))))

;;;;
;;;; DEL in List Phase (Cancel Conversion)
;;;;

(nskk-describe "DEL key in list phase"
  ;; DEL in list phase is bound to `rollback-to-reading' in nskk-keymap.el:
  ;;   (backspace converting rollback-to-reading)
  ;; This is identical in effect to C-g: it calls nskk-rollback-conversion,
  ;; which returns to ▽ preedit state, sets henkan-phase to 'on, and clears
  ;; nskk--henkan-candidate-list-active.

  (nskk-it "rolls back to preedit (▽) state"
    ;; DEL → rollback-to-reading → nskk-rollback-conversion → phase = 'on.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")    ; SPC#1: start-conversion
        (nskk-e2e-type "SPC")    ; SPC#2: select-next
        (nskk-e2e-type "SPC")    ; SPC#3: select-next
        (nskk-e2e-type "SPC")    ; SPC#4: select-next
        (nskk-e2e-type "SPC"))   ; SPC#5: show-list-next → 'list
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "DEL"))
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase 'on "DEL in list phase must return to ▽ preedit state"))))

  (nskk-it "restores kana reading to preedit (▽) buffer (same as C-g)"
    ;; nskk-rollback-conversion restores ▽ preedit with kana reading.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-not-converting)
      ;; Buffer must contain ▽ + kana reading (DDSKK-compatible rollback).
      (nskk-e2e-assert-buffer "▽かんじ" "DEL must return to ▽ preedit with kana reading")))

  (nskk-it "clears nskk--henkan-candidate-list-active"
    ;; After cancel, the list-active flag must be nil (matching C-g behavior).
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (should nskk--henkan-candidate-list-active)
      (nskk-e2e-type "DEL")
      (should-not nskk--henkan-candidate-list-active))))

;;;;
;;;; SPC in List Phase (Next Page)
;;;;

;; 11-candidate dict so that SPC#6 advances to page 2 without exhaustion.
;; With per-page = min(7, 7) = 7:
;;   After SPC x5: list phase, current-index = 3.
;;   SPC#6: next-start = 3 + 7 = 10 < 11 → page 2 (index 10), no exhaustion.
(defconst nskk-e2e--kanji-11cands-dict
  '(("かんじ" . ("漢字" "感じ" "幹事" "換字" "貫地" "刊事" "肝事" "感事" "看事" "官事" "貫字")))
  "Eleven-candidate dict entry for かんじ, used to test next-page without exhaustion.
Indices 0-10: 漢字 感じ 幹事 換字 貫地 刊事 肝事 感事 看事 官事 貫字.")

(nskk-describe "SPC in list phase advances to next page"
  ;; Strategy A (7-cand dict): SPC#6 exhausts all 7 candidates.
  ;; nskk--exhaust-candidates fires, then nskk-start-registration is called.
  ;; read-from-minibuffer is mocked to return "" → registration cancelled.
  ;; Cancel path in nskk--exhaust-candidates wraps to index 0:
  ;;   current-index = 0, henkan-phase = 'list, candidate-list-active = t.

  (nskk-context "Strategy A: 7-candidate dict — SPC#6 exhausts and triggers registration"
    (nskk-it "SPC in list phase triggers registration prompt when candidates exhausted"
      ;; After SPC x5 → list phase (current-index=3).
      ;; SPC#6: next-start = 3 + 7 = 10 >= 7 → nskk--exhaust-candidates called.
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
        (nskk-given
          (nskk-e2e-type "Kanji"))
        (nskk-when
          (nskk-e2e-type "SPC")   ; SPC#1: start-conversion
          (nskk-e2e-type "SPC")   ; SPC#2: select-next
          (nskk-e2e-type "SPC")   ; SPC#3: select-next
          (nskk-e2e-type "SPC")   ; SPC#4: select-next
          (nskk-e2e-type "SPC"))  ; SPC#5: show-list-next → 'list
        (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
        (nskk-when
          (nskk-e2e-type "SPC"))  ; SPC#6: next-start=10 >= 7 → exhaust-candidates
        (nskk-then
          ;; After registration cancel: phase wraps back to 'list at index 0.
          (nskk-e2e-assert-henkan-phase 'list
            "After exhaustion and registration cancel, phase must remain 'list"))))

    (nskk-it "after registration cancel wraps current-index back to 0"
      ;; nskk--exhaust-candidates cancel path:
      ;;   (setf (nskk-state-current-index ...) 0) → index = 0.
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")  ; SPC#5 → list phase
        (nskk-e2e-assert-henkan-phase 'list)
        (nskk-e2e-type "SPC")  ; SPC#6 → exhaust-candidates → cancel → index=0
        (nskk-e2e-assert-henkan-phase 'list)
        ;; 'a' selects page position 0 from current-index=0 → index 0 = "漢字"
        (nskk-e2e-type "a")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "漢字"
          "After exhaustion cancel wrap, 'a' must commit index 0 = 漢字")))

    (nskk-it "after registration cancel nskk--henkan-candidate-list-active is t"
      ;; Cancel path in nskk--exhaust-candidates sets candidate-list-active = t.
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")  ; SPC#5 → list phase
        (nskk-e2e-type "SPC")  ; SPC#6 → exhaust → cancel → candidate-list-active=t
        (nskk-e2e-assert-henkan-phase 'list)
        (should nskk--henkan-candidate-list-active))))

  ;; Strategy B (11-cand dict): SPC#6 goes to page 2 without exhaustion.
  ;; per-page = min(7, 7) = 7; current-index=3 after SPC#5.
  ;; SPC#6: next-start = 3 + 7 = 10 < 11 → set current-index=10, show page 2.

  (nskk-context "Strategy B: 11-candidate dict — SPC#6 shows next page without exhaustion"
    (nskk-it "SPC in list phase advances to next page when candidates remain"
      ;; SPC#6 with 11 candidates: next-start = 3 + 7 = 10, which is < 11.
      ;; Phase must remain 'list (no exhaustion triggered).
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
        (nskk-given
          (nskk-e2e-type "Kanji"))
        (nskk-when
          (nskk-e2e-type "SPC")   ; SPC#1: start-conversion
          (nskk-e2e-type "SPC")   ; SPC#2: select-next
          (nskk-e2e-type "SPC")   ; SPC#3: select-next
          (nskk-e2e-type "SPC")   ; SPC#4: select-next
          (nskk-e2e-type "SPC"))  ; SPC#5: show-list-next → 'list, index=3
        (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
        (nskk-when
          (nskk-e2e-type "SPC"))  ; SPC#6: next-start=10 < 11 → page 2
        (nskk-then
          (nskk-e2e-assert-henkan-phase 'list "Phase must remain 'list after next-page SPC")
          (nskk-e2e-assert-converting))))

    (nskk-it "SPC next page stays in converting state"
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")  ; SPC#5 → list phase, index=3
        (nskk-e2e-assert-henkan-phase 'list)
        (nskk-e2e-type "SPC")  ; SPC#6 → page 2, index=10
        (nskk-e2e-assert-converting)))

    (nskk-it "key selection after next-page SPC commits correct candidate from page 2"
      ;; After SPC#6 with 11 candidates: current-index = 10 (index into candidates).
      ;; 'a' → page position 0 → absolute = 10 + 0 = 10 → "貫字" (index 10).
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")  ; SPC#5 → list phase, index=3
        (nskk-e2e-assert-henkan-phase 'list)
        (nskk-e2e-type "SPC")  ; SPC#6 → page 2, index=10
        (nskk-e2e-assert-henkan-phase 'list)
        ;; 'a' → pos=0 → absolute=10 → "貫字"
        (nskk-e2e-type "a")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "貫字"
          "After next-page SPC, 'a' must commit index 10 = 貫字")))))

;;;;
;;;; x at First Page (Boundary Behavior)
;;;;

(nskk-describe "x at first page in list phase"
  ;; After SPC x5: list phase, current-index = 3 (page start = index 3).
  ;; x → nskk--show-candidate-list-prev:
  ;;   prev-start = 3 - 7 = -4 → clamped to 0.
  ;;   current-index becomes 0.
  ;; x again when current-index = 0:
  ;;   prev-start = 0 - 7 = -7 → clamped to 0.
  ;;   current-index stays 0 → same page re-displayed, still in 'list.
  ;; This is NOT a cancel/exit from list phase.

  (nskk-it "x at first page stays in list phase (does not exit or cancel)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")   ; SPC#1
        (nskk-e2e-type "SPC")   ; SPC#2
        (nskk-e2e-type "SPC")   ; SPC#3
        (nskk-e2e-type "SPC")   ; SPC#4
        (nskk-e2e-type "SPC"))  ; SPC#5 → list phase, index=3
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "x")    ; prev-start = max(0, 3-7) = 0, index=0
        (nskk-e2e-type "x"))   ; prev-start = max(0, 0-7) = 0, index=0 (re-display)
      (nskk-then
        (nskk-e2e-assert-henkan-phase 'list
          "x at page-0 must not exit list phase"))))

  (nskk-it "x at first page still shows converting state"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")  ; list phase, index=3
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "x")    ; x at page-0 (after clamp to 0)
      (nskk-e2e-type "x")    ; x again at page-0
      (nskk-e2e-assert-converting)))

  (nskk-it "x at page-0 then 'a' commits index 0 = 漢字"
    ;; After SPC x5 → list, index=3.
    ;; x → prev-start = max(0, 3-7) = 0, current-index = 0.
    ;; x again → prev-start = max(0, 0-7) = 0, current-index = 0 (unchanged).
    ;; 'a' → pos=0 → absolute = 0 + 0 = 0 → "漢字"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))  ; list phase, index=3
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-when
        (nskk-e2e-type "x")    ; index → 0
        (nskk-e2e-type "x"))   ; index stays 0 (boundary clamp)
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-when
        (nskk-e2e-type "a"))   ; pos=0 → absolute=0 → "漢字"
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "漢字"
          "x at page-0 boundary then 'a' must commit 漢字 (index 0)"))))

  (nskk-it "nskk--henkan-candidate-list-active remains t after x at page-0"
    ;; x in list phase keeps candidate-list-active = t regardless of page boundary.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")  ; list phase
      (should nskk--henkan-candidate-list-active)
      (nskk-e2e-type "x")    ; prev page at page-0
      (nskk-e2e-type "x")    ; again at page-0
      (should nskk--henkan-candidate-list-active))))

;;;;
;;;; SPC Exhaustion → Registration (and Cancel Wraps Back)
;;;;

(nskk-describe "SPC exhaustion triggers registration in list phase"
  ;; With 7-candidate dict: per-page = min(7, 7) = 7.
  ;; After SPC x5: current-index = 3.
  ;; SPC#6 in list phase: next-start = 3 + 7 = 10 >= 7 → exhaustion.
  ;; nskk--exhaust-candidates → nskk-start-registration called.
  ;; read-from-minibuffer mocked → returns "" → registration cancelled.
  ;; Cancel path: current-index = 0, henkan-phase = 'list, candidate-list-active = t.

  (nskk-it "SPC in list phase exhausts candidates and triggers registration"
    ;; After exhaustion with cancelled registration, phase must be 'list (wrapped).
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")   ; SPC#1
        (nskk-e2e-type "SPC")   ; SPC#2
        (nskk-e2e-type "SPC")   ; SPC#3
        (nskk-e2e-type "SPC")   ; SPC#4
        (nskk-e2e-type "SPC"))  ; SPC#5 → list phase, index=3
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        ;; SPC#6: next-start=10 >= 7 → exhaust-candidates → registration cancelled.
        (nskk-e2e-type "SPC"))
      (nskk-then
        ;; Registration was cancelled (mock returned ""); wrap to list at index 0.
        (nskk-e2e-assert-henkan-phase 'list
          "After exhaustion and registration cancel, phase must remain 'list"))))

  (nskk-it "after registration cancel still in converting state"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")  ; SPC#5 → list
      (nskk-e2e-type "SPC")  ; SPC#6 → exhaust → cancel
      (nskk-e2e-assert-converting)))

  (nskk-it "after registration cancel index wraps to 0 and 'a' commits 漢字"
    ;; Cancel path: current-index = 0.
    ;; 'a' → pos=0 → absolute = 0 + 0 = 0 → "漢字"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")   ; SPC#5 → list phase, index=3
        (nskk-e2e-type "SPC"))  ; SPC#6 → exhaust → cancel → index=0
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-when
        (nskk-e2e-type "a"))    ; pos=0 → absolute=0+0=0 → "漢字"
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "漢字"
          "After exhaustion cancel, 'a' must commit 漢字 (index 0)"))))

  (nskk-it "nskk--henkan-candidate-list-active is t after registration cancel"
    ;; Cancel path in nskk--exhaust-candidates:
    ;;   (setq nskk--henkan-candidate-list-active t)
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")  ; SPC#5 → list
      (nskk-e2e-type "SPC")  ; SPC#6 → exhaust → cancel → active=t
      (nskk-e2e-assert-henkan-phase 'list)
      (should nskk--henkan-candidate-list-active)))

  (nskk-it "two successive exhaustion cycles both wrap back to list phase"
    ;; SPC#6 → exhaust → cancel → index=0, list phase.
    ;; SPC#7: now candidate-list-active=t, next-start = 0 + 7 = 7 >= 7 → exhaust again.
    ;; Cancel again → index=0, list phase.
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")   ; SPC#5 → list, index=3
        (nskk-e2e-type "SPC")   ; SPC#6 → exhaust → cancel → index=0
        (nskk-e2e-type "SPC"))  ; SPC#7 → exhaust again → cancel → index=0
      (nskk-then
        (nskk-e2e-assert-henkan-phase 'list
          "Second consecutive exhaustion must also wrap back to list phase")
        (nskk-e2e-assert-converting)))))

;;;;
;;;; Backspace/DEL Key Tests
;;;;

;;;;
;;;; DEL in Converting State
;;;;

(nskk-describe "DEL in converting state"
  (nskk-it "rolls back to preedit (▽) and restores kana reading"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      ;; Verify we are in converting state before DEL
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "DEL")
      ;; After rollback: no longer converting (▽ preedit is not converting)
      (nskk-e2e-assert-not-converting)
      ;; henkan-phase is 'on after rollback (back to ▽ preedit state)
      (nskk-e2e-assert-henkan-phase 'on "DEL from ▼ state: henkan-phase should be 'on (▽ preedit)")
      ;; Buffer contains ▽ + kana reading (DDSKK-compatible rollback)
      (nskk-e2e-assert-buffer "▽かんじ" "DEL from ▼ state: buffer should contain ▽ + kana reading")))

  (nskk-it "rolls back from 2nd candidate to preedit (▽)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; Advance to the 2nd candidate
      (nskk-e2e-type "SPC")
      ;; Still in converting state
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "DEL")
      ;; After rollback: no longer converting
      (nskk-e2e-assert-not-converting)
      ;; henkan-phase is 'on (back to ▽ preedit)
      (nskk-e2e-assert-henkan-phase 'on "DEL from ▼ 2nd candidate: henkan-phase should be 'on")
      ;; Buffer contains ▽ + kana reading
      (nskk-e2e-assert-buffer "▽かんじ" "DEL from ▼ 2nd candidate: buffer should contain ▽ + kana reading"))))

;;;;
;;;; DEL in Hiragana Preedit State (Non-abbrev)
;;;;

(nskk-describe "DEL in hiragana preedit state"
  (nskk-it "deletes last char and leaves empty preedit marker"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      ;; ▽ preedit is active
      (nskk-e2e-assert-henkan-phase 'on "After 'Ka': should be in ▽ preedit")
      (nskk-e2e-type "DEL")
      ;; The char か was deleted; preedit now has ▽ marker only
      ;; Buffer-string shows ▽ because the marker is still in buffer
      (nskk-e2e-assert-buffer "▽" "After DEL of 'か': buffer should show empty ▽ preedit")
      ;; henkan-phase is still 'on (preedit not yet cancelled, just char deleted)
      (nskk-e2e-assert-henkan-phase 'on "After DEL of 'か': henkan-phase should still be 'on")))

  (nskk-it "deletes chars progressively from right"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on "After 'Kanji': should be in ▽ preedit")
      ;; First DEL: removes じ
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽かん" "After 1st DEL: buffer should show ▽かん")
      (nskk-e2e-assert-henkan-phase 'on "After 1st DEL: should still be in ▽ preedit")
      ;; Second DEL: removes ん
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽か" "After 2nd DEL: buffer should show ▽か")
      (nskk-e2e-assert-henkan-phase 'on "After 2nd DEL: should still be in ▽ preedit")))

  (nskk-it "on empty preedit cancels preedit entirely"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type 'Ka' to enter ▽か preedit
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on "After 'Ka': should be in ▽ preedit")
      ;; First DEL: point is past ▽ → delete-preedit-char → removes か
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽" "After 1st DEL: buffer should show empty ▽")
      (nskk-e2e-assert-henkan-phase 'on "After 1st DEL: still in ▽ preedit (not yet cancelled)")
      ;; Second DEL: empty preedit (nothing after ▽) → cancel preedit
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-henkan-phase nil "After 2nd DEL: henkan-phase reset to nil (preedit cancelled)")
      (nskk-e2e-assert-buffer "" "After 2nd DEL: buffer is empty (preedit cancelled)")))

  (nskk-it "does not delete committed text when point drifted left of preedit"
    ;; Regression: if point is left of ▽ preedit boundary, DEL must not delete
    ;; committed text before preedit. It should safely clamp point to boundary.
    (nskk-e2e-with-buffer 'hiragana nil
      (insert "A")
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-buffer "A▽か" "Precondition: committed text + preedit")
      ;; Simulate point drift to the committed region (left of preedit boundary).
      (goto-char (point-min))
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "A▽か" "DEL must not delete committed text when point drifted left")
      (should (= (point) (+ 2 (length nskk-henkan-on-marker)))))))

;;;;
;;;; DEL in Normal State (No Preedit, No Conversion)
;;;;

(nskk-describe "DEL in normal state"
  (nskk-it "delegates to backward-delete-char in hiragana mode"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; lowercase 'a' in hiragana idle inserts あ without starting preedit
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ" "After typing 'a': buffer should contain あ")
      (nskk-e2e-assert-henkan-phase nil "After typing 'a': no preedit, henkan-phase nil")
      ;; DEL in normal hiragana state: backward-delete-char
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "" "After DEL: buffer should be empty")))

  (nskk-it "delegates to backward-delete-char in ascii mode"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "hello")
      (nskk-e2e-assert-buffer "hello" "After 'hello': buffer should contain hello")
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "hell" "After DEL: last char removed")))

  (nskk-it "does not crash on empty buffer"
    (nskk-e2e-with-buffer nil nil
      ;; Buffer is empty; DEL should not signal an error
      (condition-case err
          (nskk-e2e-type "DEL")
        (error
         (ert-fail (format "DEL on empty buffer raised an error: %s"
                           (error-message-string err)))))
      (nskk-e2e-assert-buffer "" "Empty buffer remains empty after DEL"))))

;;;;
;;;; DEL Cancel and Resume Behavior
;;;;

(nskk-describe "DEL cancel and resume behavior"
  (nskk-it "cancels conversion and allows re-entry after DEL"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Enter converting state
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; DEL: cancel conversion → rollback to ▽ preedit (DDSKK-compatible)
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase 'on "After DEL cancel: henkan-phase should be 'on (▽ preedit)")
      ;; After rollback the reading is back in ▽ preedit — user can edit and re-convert.
      (nskk-e2e-assert-buffer "▽かんじ" "After DEL cancel: buffer contains ▽ + kana reading"))))

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

  (nskk-it "q during preedit converts preedit to katakana without toggling mode"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "q")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "カ")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "/ during preedit commits kana then switches to abbrev"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "/")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer "か▽")
      (nskk-e2e-assert-mode 'abbrev))))

;;;;
;;;; Property: Post-Commit Buffer Non-Empty
;;;;

(nskk-property-test-exhaustive henkan-post-commit-buffer-non-empty
  '(("Kanji" . "漢字") ("Henkan" . "変換") ("Nihon" . "日本"))
  ;; Property: after successful conversion + commit, buffer is non-empty
  ;; and contains exactly the expected kanji candidate.
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type (car item))
    (nskk-e2e-type "SPC")
    (nskk-e2e-type "C-j")
    (and (not (string-empty-p (buffer-string)))
         (equal (buffer-string) (cdr item)))))

(provide 'nskk-henkan-e2e-test)

;;; nskk-henkan-e2e-test.el ends here
