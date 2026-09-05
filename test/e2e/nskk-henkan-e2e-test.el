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

  (nskk-it "commits via RET and inserts a newline"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-buffer "漢字\n")))

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

  (nskk-it "pressing 'a' selects candidate at page position 0 (index 4 = 貫地)"
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
        (nskk-e2e-assert-buffer "貫地" "Key 'a' must select index 4"))))

  (nskk-it "pressing 's' selects candidate at page position 1 (index 5 = 刊事)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "s")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "刊事" "Key 's' must select index 5")))

  (nskk-it "pressing 'd' selects candidate at page position 2 (index 6 = 肝事)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "d")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "肝事" "Key 'd' must select index 6")))

  (nskk-it "pressing 'f' selects candidate at page position 3 (index 7 = 感事)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "f")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "感事" "Key 'f' must select index 7")))

  (nskk-it "pressing 'l' selects candidate at page position 6 in list phase"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "l")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "貫字" "Key 'l' must select index 10")))

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
  (nskk-it "returns to inline phase after x on the first page"
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
        (nskk-e2e-assert-henkan-phase 'active "First-page x returns inline"))))

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
      (nskk-e2e-assert-henkan-phase 'active)
      (nskk-e2e-type "a")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "換字あ" "Inline input commits index 3 and inserts kana"))))

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
  (nskk-it "commits the current candidate (page-start index) with a newline"
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
        (nskk-e2e-assert-buffer "貫地\n" "RET in list phase commits the page-start candidate"))))

  (nskk-it "ends conversion state after RET"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-not-converting)))

  (nskk-it "inserts exactly one newline after the committed candidate"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (dotimes (_ 5) (nskk-e2e-type "SPC"))
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-buffer "貫地\n"))))

;;;;
;;;; Candidate List Phase Properties
;;;;

(nskk-describe "candidate list phase properties"

  (nskk-property-test-exhaustive candidate-list-valid-keys-commit-and-end-conversion
    '(?a ?s ?d ?f)
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
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
          (nskk-e2e-type "SPC"))  ; SPC#6: next-start=11 >= 7 → exhaust-candidates
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


  (nskk-context "Strategy B: three-candidate pages — SPC#6 shows next page without exhaustion"
    (nskk-it "SPC in list phase advances to next page when candidates remain"
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
        (setq-local nskk-henkan-number-to-display-candidates 3)
        (nskk-given
          (nskk-e2e-type "Kanji"))
        (nskk-when
          (nskk-e2e-type "SPC")   ; SPC#1: start-conversion
          (nskk-e2e-type "SPC")   ; SPC#2: select-next
          (nskk-e2e-type "SPC")   ; SPC#3: select-next
          (nskk-e2e-type "SPC")   ; SPC#4: select-next
          (nskk-e2e-type "SPC"))  ; SPC#5: show-list-next → 'list, index=4
        (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
        (nskk-when
          (nskk-e2e-type "SPC"))  ; SPC#6: next-start=7 < 11 → page 2
        (nskk-then
          (nskk-e2e-assert-henkan-phase 'list "Phase must remain 'list after next-page SPC")
          (nskk-e2e-assert-converting))))

    (nskk-it "SPC next page stays in converting state"
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
        (setq-local nskk-henkan-number-to-display-candidates 3)
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")  ; SPC#5 → list phase, index=4
        (nskk-e2e-assert-henkan-phase 'list)
        (nskk-e2e-type "SPC")  ; SPC#6 → page 2, index=7
        (nskk-e2e-assert-converting)))

    (nskk-it "key selection after next-page SPC commits correct candidate from page 2"
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
        (setq-local nskk-henkan-number-to-display-candidates 3)
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")  ; SPC#5 → list phase, index=4
        (nskk-e2e-assert-henkan-phase 'list)
        (nskk-e2e-type "SPC")  ; SPC#6 → page 2, index=7
        (nskk-e2e-assert-henkan-phase 'list)
        (nskk-e2e-type "a")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "感事"
          "After next-page SPC, 'a' must commit index 7")))))

;;;;
;;;; x at First Page (Boundary Behavior)
;;;;

(nskk-describe "x at first page in list phase"

  (nskk-it "x at first page returns to inline conversion"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")   ; SPC#1
        (nskk-e2e-type "SPC")   ; SPC#2
        (nskk-e2e-type "SPC")   ; SPC#3
        (nskk-e2e-type "SPC")   ; SPC#4
        (nskk-e2e-type "SPC"))  ; SPC#5 → list phase, index=4
      (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
      (nskk-when
        (nskk-e2e-type "x")
        (nskk-e2e-type "x"))
      (nskk-then
        (nskk-e2e-assert-henkan-phase 'active
          "First-page x returns to inline conversion"))))

  (nskk-it "x at first page still shows converting state"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")  ; list phase, index=4
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-e2e-type "x")    ; return inline
      (nskk-e2e-type "x")    ; previous inline candidate
      (nskk-e2e-assert-converting)))

  (nskk-it "two x presses from first page then a commit index 2 and insert kana"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-given
        (nskk-e2e-type "Kanji"))
      (nskk-when
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "SPC"))  ; list phase, index=4
      (nskk-e2e-assert-henkan-phase 'list)
      (nskk-when
        (nskk-e2e-type "x")
        (nskk-e2e-type "x"))
      (nskk-e2e-assert-henkan-phase 'active)
      (nskk-when
        (nskk-e2e-type "a"))
      (nskk-then
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "幹事あ"
          "Inline input commits index 2 and inserts kana"))))

  (nskk-it "nskk--henkan-candidate-list-active clears after first-page x"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-7cands-dict
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")  ; list phase
      (should (nskk-henkan-candidate-list-active))
      (nskk-e2e-type "x")    ; prev page at page-0
      (nskk-e2e-type "x")    ; previous inline candidate
      (should-not (nskk-henkan-candidate-list-active)))))

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
        (nskk-e2e-type "SPC"))  ; SPC#5 → list phase, index=4
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
        (nskk-e2e-type "SPC")   ; SPC#5 → list phase, index=4
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
        (nskk-e2e-type "SPC")   ; SPC#5 → list, index=4
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

(nskk-deftest-table ddskk-command-loop-boundaries
  :columns (keys expected phase)
  :rows (("Q" "▽" on)
         ("Kanji SPC x" "▽かんじ" on)
         ("KaKu C-g" "▽かく" on)
         ("KaK DEL" "▽か" on)
         ("Q 1 2 ko SPC C-j" "12個" nil)
         ("Q 1 2 ko SPC SPC C-j" "一二個" nil)
         ("; ;" "；" nil)
         ("K ; a" "；あ" nil)
         ("K a ;" "▽か*" on)
         ("K a ; ;" "▽か*" on)
         ("; k DEL" "▽" on))
  :body
  (nskk-e2e-with-buffer 'hiragana
      '(("かんじ" . ("漢字" "感じ"))
        ("かk" . ("書"))
        ("#こ" . ("#0個" "#2個" "#3個")))
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd keys)))
    (should (equal (buffer-string) expected))
    (should (= (point) (point-max)))
    (should (eq (nskk-state-henkan-phase nskk-current-state) phase))))

(nskk-deftest-table annotation-command-loop
  :columns (keys enabled expected)
  :rows (("Kanji SPC" t "note-one")
         ("Kanji SPC SPC" t "note-two")
         ("Kanji SPC SPC x" t "note-one")
         ("Kanji SPC SPC SPC" t nil)
         ("Kanji SPC C-j" t nil)
         ("Kanji SPC C-g" t nil)
         ("Kanji SPC" nil nil)
         ("KaKu" t "note-okuri")
         ("Q 1 2 ko SPC" t "note-number")
         ("Q 1 2 ko SPC C-j KaKu" t "note-okuri")
         ("Q 1 2 ko SPC C-j Kanji SPC" t "note-one"))
  :body
  (nskk-e2e-with-buffer 'hiragana
      '(("かんじ" . ("漢字" "感じ" "幹事"))
        ("かk" . ("書" "描"))
        ("#こ" . ("#0個" "#2個")))
    (let ((nskk-show-annotation enabled)
          (original-echo (symbol-function 'nskk--annotation-echo))
          echoed)
      (nskk-annotation-initialize)
      (nskk-annotation-register "かんじ" "漢字" "note-one")
      (nskk-annotation-register "かんじ" "感じ" "note-two")
      (nskk-annotation-register "かk" "書" "note-okuri")
      (nskk-annotation-register "#こ" "#0個" "note-number")
      (cl-letf (((symbol-function 'nskk--annotation-echo)
                 (lambda (format-string &rest args)
                   (setq echoed (and format-string (apply #'format format-string args)))
                   (apply original-echo format-string args))))
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (execute-kbd-macro (kbd keys))))
      (should (equal nskk--annotation-current expected))
      (if expected
          (should (and echoed (string-match-p (regexp-quote expected) echoed)))
        (should-not echoed)))))

(nskk-deftest-table candidate-list-ddskk-command-loop
  :columns (threshold keys expected)
  :rows ((1 "Kanji SPC SPC a C-j" "漢字")
         (1 "Kanji SPC SPC x a C-j" "かんじあ")
         (2 "Kanji SPC C-j" "漢字")
         (2 "Kanji SPC SPC a C-j" "感じ")
         (2 "Kanji SPC SPC SPC a C-j" "貫地")
         (2 "Kanji SPC SPC SPC x a C-j" "感じ")
         (2 "Kanji SPC SPC x C-j" "漢字")
         (2 "Kanji SPC SPC x a C-j" "漢字あ")
         (2 "Kanji SPC SPC C-g C-j" "かんじ")
         (5 "Kanji SPC SPC SPC SPC SPC a C-j" "貫地")
         (5 "Kanji SPC SPC SPC SPC SPC x C-j" "換字"))
  :body
  (nskk-e2e-with-buffer 'hiragana nskk-e2e--kanji-11cands-dict
    (let ((nskk-henkan-show-candidates-nth threshold)
          (nskk-henkan-number-to-display-candidates 3))
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (execute-kbd-macro (kbd keys)))
      (should (equal (buffer-string) expected)))))

(nskk-deftest-table candidate-annotation-ddskk-command-loop
  :columns (keys enabled expected fragments absent)
  :rows (("Kanji SPC SPC SPC a C-j" t "監事" ("感じ;note-two" "監事;note-four") nil)
         ("Kanji SPC SPC SPC x a C-j" t "感じ" ("監事;note-four" "感じ;note-two") nil)
         ("Kanji SPC SPC SPC a C-j" nil "監事" ("監事") "note-")
         ("Tabu SPC SPC a C-j" t "次" ("次;tab\tinside") nil)
         ("Q 1 2 ko SPC SPC s C-j" t "12個" ("a:12個;raw-note s:12個;literal-note") nil)
         ("KaKu SPC a C-j" t "描く" ("描;okuri-note") nil))
  :body
  (let ((original-show-list (symbol-function 'nskk-candidate-show-list)))
   (nskk-e2e-with-buffer 'hiragana
      '(("かんじ" . ("漢字" "感じ" "幹事" "監事" "完治" "莞爾"))
        ("たぶ" . ("初" "次" "後"))
        ("かk" . ("書" "描" "欠"))
        ("#こ" . ("先頭" "#0個" "12個" "#0コ")))
    (let ((nskk-show-annotation enabled)
          (nskk-henkan-show-candidates-nth 2)
          (nskk-henkan-number-to-display-candidates 2)
          (original-build (symbol-function 'nskk--candidate-build-string))
          pages)
      (nskk-annotation-initialize)
      (dolist (entry '(("かんじ" "感じ" "note-two") ("かんじ" "監事" "note-four")
                       ("たぶ" "次" "tab\tinside") ("かk" "描" "okuri-note")
                       ("#こ" "#0個" "raw-note") ("#こ" "12個" "literal-note")))
        (apply #'nskk-annotation-register entry))
      (cl-letf (((symbol-function 'nskk-candidate-show-list) original-show-list)
                ((symbol-function 'nskk--candidate-build-string)
                 (lambda (&rest args)
                   (let ((rendered (apply original-build args)))
                     (push (substring-no-properties rendered) pages)
                     rendered))))
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (execute-kbd-macro (kbd keys))))
      (should (equal (buffer-string) expected))
      (should pages)
      (let ((rendered (string-join (reverse pages) "\n")))
        (dolist (fragment fragments)
          (should (string-match-p (regexp-quote fragment) rendered)))
        (when absent (should-not (string-match-p absent rendered))))))))

(ert-deftest nskk-e2e-annotation-numeric-collision-inline ()
  (nskk-e2e-with-buffer 'hiragana '(("#こ" . ("#0個" "12個")))
    (let ((nskk-show-annotation t)
          (nskk-henkan-show-candidates-nth 5))
      (nskk-annotation-initialize)
      (nskk-annotation-register "#こ" "#0個" "raw-note")
      (nskk-annotation-register "#こ" "12個" "literal-note")
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (execute-kbd-macro (kbd "Q 1 2 ko SPC"))
        (should (equal nskk--annotation-current "raw-note"))
        (execute-kbd-macro (kbd "SPC"))
        (should (equal nskk--annotation-current "literal-note"))
        (execute-kbd-macro (kbd "x"))
        (should (equal nskk--annotation-current "raw-note"))))))

(ert-deftest nskk-e2e-study-preserves-numeric-collision-annotations ()
  (nskk-e2e-with-buffer 'hiragana '(("#こ" . ("#0個" "12個")))
    (let ((nskk-show-annotation t)
          (nskk-henkan-show-candidates-nth 5)
          (nskk--study-kakutei-ring '((:word "previous"))))
      (nskk--study-associate "previous" "12こ" "12個")
      (nskk-annotation-initialize)
      (nskk-annotation-register "#こ" "#0個" "raw-note")
      (nskk-annotation-register "#こ" "12個" "literal-note")
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (execute-kbd-macro (kbd "Q 1 2 ko SPC"))
        (should (= (length (nskk-state-candidates nskk-current-state)) 2))
        (should (equal nskk--annotation-current "raw-note"))
        (execute-kbd-macro (kbd "SPC"))
        (should (equal nskk--annotation-current "literal-note"))
        (execute-kbd-macro (kbd "C-j"))
        (should (equal (buffer-string) "12個"))))))

(provide 'nskk-henkan-e2e-test)

;;; nskk-henkan-e2e-test.el ends here
