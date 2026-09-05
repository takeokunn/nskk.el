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
;;;; Point position after C-j commit
;;;;

(nskk-describe "point position after C-j commit"
  (nskk-it "point is at point-max after C-j commit from converting state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")
      (should (= (point) (point-max)))))

  (nskk-it "point is at point-max after C-j commit with pre-existing buffer text"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ")
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "あ漢字")
      (should (= (point) (point-max)))))

  (nskk-it "point is strictly after where it was before commit"
    (nskk-e2e-with-buffer 'hiragana nil
      (let ((start (point)))
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-not-converting)
        (should (> (point) start)))))

  (nskk-it "point is at point-max after C-j kakutei on preedit (no conversion)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-henkan-phase nil)
      (should (= (point) (point-max))))))

;;;;
;;;; Point position after C-g cancel
;;;;

(nskk-describe "point position after C-g cancel"
  (nskk-it "point is within valid bounds after C-g from converting state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase 'on)
      (should (<= (point-min) (point)))
      (should (<= (point) (point-max)))))

  (nskk-it "point is at point-min after C-g from preedit state (empty buffer)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "")
      (should (= (point) (point-min)))))

  (nskk-it "point is at point-max after C-g from converting state reverts to preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer "▽かんじ")
      (should (= (point) (point-max)))))

  (nskk-it "point remains within valid bounds after C-g from converting with pre-existing text"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ")
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-not-converting)
      (should (<= (point-min) (point)))
      (should (<= (point) (point-max))))))

;;;;
;;;; No buffer artifacts during conversion
;;;;

(nskk-describe "buffer does not contain conversion overlay text"
  (nskk-it "buffer-string during ▼ state contains the ▼ marker"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (should (string-match-p "▼" (buffer-string)))))

  (nskk-it "buffer-string during ▼ state does not contain the committed candidate text"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (should (not (string-match-p "漢字" (buffer-string))))))

  (nskk-it "candidate text appears in buffer only after commit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (should (not (string-match-p "漢字" (buffer-string))))
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-not-converting)
      (should (string-match-p "漢字" (buffer-string)))
      (should (= (point) (point-max)))))

  (nskk-it "overlay shows candidate during ▼ state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-assert-overlay-shows "漢字")
      (should (not (string-match-p "漢字" (buffer-string))))))

  (nskk-it "point is within valid range throughout ▼ state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (should (<= (point-min) (point)))
      (should (>= (point-max) (point))))))

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
;;;; Point position during preedit (▽ state)
;;;;

(nskk-describe "point position during preedit (▽ state)"
  (nskk-it "point is at point-max after entering preedit mode"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "K")
      (nskk-e2e-assert-henkan-phase 'on)
      (should (= (point) (point-max)))))

  (nskk-it "point is at point-max after typing multiple preedit chars"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (should (= (point) (point-max)))
      (nskk-e2e-type "n")
      (should (= (point) (point-max)))
      (nskk-e2e-type "j")
      (should (= (point) (point-max)))))

  (nskk-it "point stays at point-max after typing full preedit sequence Kanji"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on)
      (should (= (point) (point-max)))))

  (nskk-it "point is at point-max when preedit follows pre-existing kana"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ")
      (let ((point-after-a (point)))
        (nskk-e2e-type "Kanji")
        (nskk-e2e-assert-henkan-phase 'on)
        (should (> (point) point-after-a))
        (should (= (point) (point-max)))))))

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

(nskk-describe "consecutive conversions in same buffer"
  (nskk-it "produces 漢字変換 from two sequential henkan words"
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

(nskk-describe "long reading conversion"
  (nskk-it "converts hiragana reading to 平仮名"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Hi")
      (nskk-e2e-type "ra")
      (nskk-e2e-type "ga")
      (nskk-e2e-type "na")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "平仮名")))

  (nskk-it "converts nihongo reading to 日本語"
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
        (should (nskk-henkan-candidate-list-active)))))

  (nskk-it "is still in converting state while in list phase"
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
          (should-not (nskk-henkan-candidate-list-active)))))))

;;;;
;;;; Key Selection in List Phase
;;;;

(nskk-describe "candidate selection by key in list phase"

  (nskk-it "pressing 'a' selects candidate at page position 0 (index 3 = 換字)"
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
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "s")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "貫地" "Key 's' in list phase must commit 貫地 (index 4)")))

  (nskk-it "pressing 'd' selects candidate at page position 2 (index 5 = 刊事)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "d")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "刊事" "Key 'd' in list phase must commit 刊事 (index 5)")))

  (nskk-it "pressing 'f' selects candidate at page position 3 (index 6 = 肝事)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "f")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "肝事" "Key 'f' in list phase must commit 肝事 (index 6)")))

  (nskk-it "pressing 'l' selects candidate at page position 6 in list phase"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "l")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "官事" "Key 'l' in list phase must commit 官事 (index 9)")))

  (nskk-it "pressing 'j' does not commit when page position 4 is out of range"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
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
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "x")
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "a")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字" "After x then 'a', should commit 漢字 (index 0)"))))

;;;;
;;;; C-g in List Phase (Cancel Conversion)
;;;;

(nskk-describe "C-g in list phase"
  (nskk-it "rolls back to preedit (▽) state"
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
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "▽かんじ" "C-g must return to ▽ preedit with kana reading")))

  (nskk-it "clears nskk--henkan-candidate-list-active after cancel"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (should (nskk-henkan-candidate-list-active))
      (nskk-e2e-type "C-g")
      (should-not (nskk-henkan-candidate-list-active)))))

;;;;
;;;; RET in List Phase (Commit Current Candidate)
;;;;

(nskk-describe "RET in list phase"
  (nskk-it "commits the current candidate (page-start index) without newline"
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
      (should-not (string-match-p "\n" (buffer-string))))))

;;;;
;;;; Candidate List Phase Properties
;;;;

(nskk-describe "candidate list phase properties"

  (nskk-property-test-exhaustive candidate-list-valid-keys-commit-and-end-conversion
    '(?a ?s ?d ?f)
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e--dispatch-event item)
      (and (not (nskk-converting-p))
           (not (string-empty-p (buffer-string))))))

  (nskk-property-test-exhaustive candidate-list-out-of-range-keys-keep-converting
    '(?j ?k)
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e--dispatch-event item)
      (nskk-converting-p))))

;;;;
;;;; DEL in List Phase (Cancel Conversion)
;;;;

(nskk-describe "DEL key in list phase"

  (nskk-it "rolls back to preedit (▽) state"
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
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "▽かんじ" "DEL must return to ▽ preedit with kana reading")))

  (nskk-it "clears nskk--henkan-candidate-list-active"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (should (nskk-henkan-candidate-list-active))
      (nskk-e2e-type "DEL")
      (should-not (nskk-henkan-candidate-list-active)))))

;;;;
;;;; SPC in List Phase (Next Page)
;;;;

(defconst nskk-e2e--kanji-11cands-dict
  '(("かんじ" . ("漢字" "感じ" "幹事" "換字" "貫地" "刊事" "肝事" "感事" "看事" "官事" "貫字")))
  "Eleven-candidate dict entry for かんじ, used to test next-page without exhaustion.
Indices 0-10: 漢字 感じ 幹事 換字 貫地 刊事 肝事 感事 看事 官事 貫字.")

(nskk-describe "SPC in list phase advances to next page"

  (nskk-context "Strategy A: 7-candidate dict — SPC#6 exhausts and triggers registration"
    (nskk-it "SPC in list phase triggers registration prompt when candidates exhausted"
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
          (nskk-e2e-assert-henkan-phase 'list
            "After exhaustion and registration cancel, phase must remain 'list"))))

    (nskk-it "after registration cancel wraps current-index back to 0"
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
        (nskk-e2e-type "a")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "漢字"
          "After exhaustion cancel wrap, 'a' must commit index 0 = 漢字")))

    (nskk-it "after registration cancel nskk--henkan-candidate-list-active is t"
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")  ; SPC#5 → list phase
        (nskk-e2e-type "SPC")  ; SPC#6 → exhaust → cancel → candidate-list-active=t
        (nskk-e2e-assert-henkan-phase 'list)
        (should (nskk-henkan-candidate-list-active)))))


  (nskk-context "Strategy B: 11-candidate dict — SPC#6 shows next page without exhaustion"
    (nskk-it "SPC in list phase advances to next page when candidates remain"
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
        (nskk-e2e-type "a")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "貫字"
          "After next-page SPC, 'a' must commit index 10 = 貫字")))))

;;;;
;;;; x at First Page (Boundary Behavior)
;;;;

(nskk-describe "x at first page in list phase"

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
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")  ; list phase
      (should (nskk-henkan-candidate-list-active))
      (nskk-e2e-type "x")    ; prev page at page-0
      (nskk-e2e-type "x")    ; again at page-0
      (should (nskk-henkan-candidate-list-active)))))

;;;;
;;;; SPC Exhaustion → Registration (and Cancel Wraps Back)
;;;;

(nskk-describe "SPC exhaustion triggers registration in list phase"

  (nskk-it "SPC in list phase exhausts candidates and triggers registration"
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
        (nskk-e2e-type "SPC"))
      (nskk-then
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
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")  ; SPC#5 → list
      (nskk-e2e-type "SPC")  ; SPC#6 → exhaust → cancel → active=t
      (nskk-e2e-assert-henkan-phase 'list)
      (should (nskk-henkan-candidate-list-active))))

  (nskk-it "two successive exhaustion cycles both wrap back to list phase"
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
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase 'on "DEL from ▼ state: henkan-phase should be 'on (▽ preedit)")
      (nskk-e2e-assert-buffer "▽かんじ" "DEL from ▼ state: buffer should contain ▽ + kana reading")))

  (nskk-it "rolls back from 2nd candidate to preedit (▽)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase 'on "DEL from ▼ 2nd candidate: henkan-phase should be 'on")
      (nskk-e2e-assert-buffer "▽かんじ" "DEL from ▼ 2nd candidate: buffer should contain ▽ + kana reading"))))

;;;;
;;;; DEL in Hiragana Preedit State (Non-abbrev)
;;;;

(nskk-describe "DEL in hiragana preedit state"
  (nskk-it "deletes last char and leaves empty preedit marker"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on "After 'Ka': should be in ▽ preedit")
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽" "After DEL of 'か': buffer should show empty ▽ preedit")
      (nskk-e2e-assert-henkan-phase 'on "After DEL of 'か': henkan-phase should still be 'on")))

  (nskk-it "deletes chars progressively from right"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on "After 'Kanji': should be in ▽ preedit")
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽かん" "After 1st DEL: buffer should show ▽かん")
      (nskk-e2e-assert-henkan-phase 'on "After 1st DEL: should still be in ▽ preedit")
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽か" "After 2nd DEL: buffer should show ▽か")
      (nskk-e2e-assert-henkan-phase 'on "After 2nd DEL: should still be in ▽ preedit")))

  (nskk-it "on empty preedit cancels preedit entirely"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on "After 'Ka': should be in ▽ preedit")
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽" "After 1st DEL: buffer should show empty ▽")
      (nskk-e2e-assert-henkan-phase 'on "After 1st DEL: still in ▽ preedit (not yet cancelled)")
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-henkan-phase nil "After 2nd DEL: henkan-phase reset to nil (preedit cancelled)")
      (nskk-e2e-assert-buffer "" "After 2nd DEL: buffer is empty (preedit cancelled)")))

  (nskk-it "does not delete committed text when point drifted left of preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (insert "A")
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-buffer "A▽か" "Precondition: committed text + preedit")
      (goto-char (point-min))
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "A▽か" "DEL must not delete committed text when point drifted left")
      (should (= (point) (+ 2 (length nskk-henkan-on-marker))))))

  (nskk-it "deletes pending romaji instead of committed kana (backspace-in-preedit bug)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "K")
      (nskk-e2e-type "a")
      (nskk-e2e-type "g")
      (nskk-e2e-assert-henkan-phase 'on "After 'Kag': should be in ▽ preedit")
      (should (equal (nskk-state-romaji-buffer) "g"))
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽か" "DEL with pending romaji: must clear romaji, not delete kana")
      (should (equal (nskk-state-romaji-buffer) ""))
      (nskk-e2e-assert-henkan-phase 'on "After DEL of pending romaji: still in ▽ preedit")))

  (nskk-it "deletes committed kana when no pending romaji (existing behavior preserved)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on "After 'Ka': should be in ▽ preedit")
      (should (equal (nskk-state-romaji-buffer) ""))
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽" "1st DEL with no pending romaji: delete committed kana")
      (nskk-e2e-assert-henkan-phase 'on "After 1st DEL: still in ▽ preedit")
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-henkan-phase nil "After 2nd DEL: preedit cancelled")
      (nskk-e2e-assert-buffer "" "After 2nd DEL: buffer is empty"))))

;;;;
;;;; DEL in Normal State (No Preedit, No Conversion)
;;;;

(nskk-describe "DEL in normal state"
  (nskk-it "delegates to backward-delete-char in hiragana mode"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ" "After typing 'a': buffer should contain あ")
      (nskk-e2e-assert-henkan-phase nil "After typing 'a': no preedit, henkan-phase nil")
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
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase 'on "After DEL cancel: henkan-phase should be 'on (▽ preedit)")
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
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type (car item))
    (nskk-e2e-type "SPC")
    (nskk-e2e-type "C-j")
    (and (not (string-empty-p (buffer-string)))
         (equal (buffer-string) (cdr item)))))

(provide 'nskk-henkan-e2e-test)

;;; nskk-henkan-e2e-test.el ends here
