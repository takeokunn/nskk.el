;;; nskk-okurigana-e2e-test.el --- E2E okurigana tests for NSKK  -*- lexical-binding: t; -*-

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
;;;; Okurigana (送り仮名) E2E Tests
;;;;

(nskk-describe "okurigana input"
  (nskk-it "enters preedit phase on Ka"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "sets okurigana consonant state on KaK"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "K")
      (should (eq (nskk-state-get-okurigana nskk-current-state) ?k))
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-not-converting)))

  (nskk-it "triggers conversion on KaKu showing 書"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "commits KaKu to 書く"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く"))))

  (nskk-it "commits MiRu to 見る"
    (let ((dict '(("みr" . ("見")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Mi")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "見る"))))

  (nskk-it "commits OkuRu to 送る"
    (let ((dict '(("おくr" . ("送")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Oku")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "送る"))))

  (nskk-it "commits KiKu to 聞く"
    (let ((dict '(("きk" . ("聞")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ki")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "聞く"))))

  (nskk-it "selects second candidate 効く from KiKu"
    (let ((dict '(("きk" . ("聞" "効")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ki")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "聞")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "効")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "効く"))))

  (nskk-it "selects second candidate 掛け from KaKe"
    (let ((dict '(("かk" . ("書" "掛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "e")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "掛")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "掛け"))))

  (nskk-it "selects second candidate 掛き from KaKi"
    (let ((dict '(("かk" . ("書" "掛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "i")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "掛")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "掛き"))))

  (nskk-it "cancels KaKu conversion with C-g"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "continues typing after committing 書く"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く")
        (nskk-e2e-type "no")
        (nskk-e2e-assert-buffer "書くの"))))

  (nskk-it "commits DekiRu to 出来る"
    (let ((dict '(("できr" . ("出来")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Deki")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "出来る"))))

  (nskk-it "commits HabikoRu to 蔓延る"
    (let ((dict '(("はびこr" . ("蔓延")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Habiko")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "蔓延る")))))

;;;;
;;;; Table: Okurigana no-crash (CvC patterns)
;;;;

(nskk-deftest-table okurigana-no-crash
  :columns (reading okuri-trigger okuri-suffix)
  :rows (("Ka" "K" "u")     ;; KaKu (書く)
         ("Mi" "R" "u")     ;; MiRu (見る)
         ("Ki" "K" "u")     ;; KiKu (聞く)
         ("Su" "R" "u")     ;; SuRu (する)
         ("Ha" "N" "a")     ;; HaNa (花な)
         ("No" "M" "u")     ;; NoMu (飲む)
         ("Ka" "E" "ru"))   ;; KaEru (変える)
  :body (nskk-e2e-with-buffer 'hiragana nil
          (nskk-e2e-type reading)
          (nskk-e2e-type okuri-trigger)
          (nskk-e2e-type okuri-suffix)
          (should (stringp (buffer-string)))
          (should (memq (nskk-current-mode) '(hiragana ascii)))))

;;;; Vowel Okurigana (母音送り仮名) E2E Tests

(nskk-describe "vowel okurigana"
  (nskk-it "converts AI to 愛い and commits"
    (let ((dict '(("あi" . ("愛" "哀")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "愛い"))))

  (nskk-it "does not produce double marker on AII"
    (let ((dict '(("あi" . ("愛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "愛"))))

  (nskk-it "converts OU to 負う and commits"
    (let ((dict '(("おu" . ("負")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "O")
        (nskk-e2e-type "U")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "負う"))))

  (nskk-it "converts aE to 与え and commits"
    (let ((dict '(("あe" . ("与")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "E")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "与え"))))

  (nskk-it "converts AU to 買う and commits"
    (let ((dict '(("あu" . ("買")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "U")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "買う"))))

  (nskk-it "cancels AI conversion with C-g without stale state"
    (let ((dict '(("あi" . ("愛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "continues typing after committing AI"
    (let ((dict '(("あi" . ("愛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "愛い")
        (nskk-e2e-type "su")
        (nskk-e2e-assert-buffer "愛いす"))))

  (nskk-it "allows uppercase next-word start after YoI without entering registration"
    (let ((dict '(("よi" . ("良"))
                  ("てんき" . ("天気")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "YoI")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "良")
        (nskk-e2e-type "Tenki")
        (nskk-e2e-assert-henkan-phase 'on)
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "天気")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "良い天気"))))

  (nskk-it "vowel after okurigana ▼ triggers implicit kakutei"
    (let ((dict '(("よi" . ("良")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "YoI")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "a")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "良いあ"))))

  (nskk-it "digit after okurigana ▼ triggers implicit kakutei"
    (let ((dict '(("よi" . ("良")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "YoI")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "3")
        (nskk-e2e-assert-not-converting)
        (should (string-match-p "良い3" (buffer-string))))))

  (nskk-it "symbol after okurigana ▼ triggers implicit kakutei"
    (let ((dict '(("よi" . ("良")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "YoI")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "!")
        (nskk-e2e-assert-not-converting)
        (should (string-match-p "良い" (buffer-string))))))

  (nskk-it "handles consonant okurigana KaKu followed by vowel okurigana AI"
    (let ((dict '(("かk" . ("書"))
                  ("あi" . ("愛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く")
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く愛い")))))

;;;; Okurigana Implicit Kakutei Tests

(nskk-describe "okurigana implicit kakutei"
  (nskk-it "consonant after okurigana ▼ triggers immediate kakutei"
    (let ((dict '(("おくr" . ("送")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "OKuRi")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "k")
        (nskk-e2e-assert-not-converting))))

  (nskk-it "vowel after okurigana ▼ triggers immediate kakutei"
    (let ((dict '(("おくr" . ("送")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "OKuRi")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "a")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "送りあ")))))

;;;; Regression Tests: Pending Romaji Discard on Okurigana Trigger

(nskk-describe "okurigana pending romaji discard regression"
  (nskk-it "KAkKu: pending k is discarded before okurigana marker, conversion succeeds"
    (let ((dict '(("かk" . ("開")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "A")
        (nskk-e2e-type "k")
        (nskk-e2e-type "K")
        (let ((content (buffer-string)))
          (should-not (string-match-p "k\\*" content)))
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (should-not (string-match-p "kk" (buffer-string))))))

  (nskk-it "KAnKu: pending n is flushed as ん before okurigana marker"
    (let ((dict '(("かんk" . ("暗")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "A")
        (nskk-e2e-type "n")
        (nskk-e2e-type "K")
        (let ((content (buffer-string)))
          (should (string-match-p "ん" content)))
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)))))

;;;; Regression Tests: Xh => ▽* (uppercase consonant with no kana in preedit)

(nskk-describe "Xh regression: no spurious ▽* with pending romaji and no kana"
  (nskk-it "XH in standard mode does not produce ▽*"
    (let ((dict '(("あ" . ("亜")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "X")
        (nskk-e2e-type "H")
        (should-not (string-match-p (regexp-quote (concat nskk-henkan-on-marker nskk-okurigana-marker))
                                    (buffer-string))))))

  (nskk-it "Xa in standard mode does produce ▽さ (sanity: normal romaji still works)"
    (let ((dict '(("あ" . ("亜")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "X")
        (nskk-e2e-type "a")
        (should-not (string-match-p (regexp-quote nskk-okurigana-marker)
                                    (buffer-string))))))

  (nskk-it "Ka followed by K still produces okurigana (regression guard)"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "K")
        (should (string-match-p (regexp-quote (concat nskk-henkan-on-marker "か" nskk-okurigana-marker))
                                (buffer-string)))))))

;;;;
;;;; Katakana Mode Okurigana (カタカナ送り仮名) E2E Tests
;;;;

(nskk-describe "katakana mode okurigana triggers conversion"
  (nskk-it "triggers conversion on KaKu in katakana mode"
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "commits KaKu to 書ク in katakana mode"
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書ク"))))

  (nskk-it "commits vowel okurigana AI to 愛イ in katakana mode"
    (let ((dict '(("アi" . ("愛")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "愛イ"))))

  (nskk-it "discards pending consonant before okurigana marker in katakana mode (T-E1 analogue)"
    (let ((dict '(("カk" . ("開")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "A")
        (nskk-e2e-type "k")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting))))

  (nskk-it "flushes pending n as ン before okurigana marker in katakana mode (T-E2 analogue)"
    (let ((dict '(("カンk" . ("暗")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "A")
        (nskk-e2e-type "n")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)))))

;;;;
;;;; Sokuon in Okurigana (促音送り仮名) E2E Tests
;;;;

(nskk-describe "sokuon in okurigana"
  (nskk-it "commits KaTTa sequence to 勝った"
    (let ((dict '(("かt" . ("勝")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "勝った"))))

  (nskk-it "commits UTTa sequence to 打った"
    (let ((dict '(("うt" . ("打")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "U")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "打った")))))

;;;;
;;;; Extended Okurigana Patterns (from nskk-okuri-extended-e2e-test)
;;;;

;;;;
;;;; Section 1: Standard Consonant Okurigana (Baseline)
;;;;

(nskk-describe "standard consonant okurigana (baseline)"
  (nskk-it "shows converting state on KaKu with dict entry"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "commits KaKu to 書く via C-j"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits MiRu to 見る via C-j"
    (let ((dict '(("みr" . ("見")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Mi")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "見る")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits YoMu to 読む via C-j"
    (let ((dict '(("よm" . ("読")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Yo")
        (nskk-e2e-type "M")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "読む")
        (nskk-e2e-assert-henkan-phase nil)))))

;;;;
;;;; Section 2: Sokuon Okurigana (促音送り仮名)
;;;;

(nskk-describe "sokuon okurigana (促音送り仮名)"
  (nskk-it "triggers conversion on KaTt showing 勝"
    (let ((dict '(("かt" . ("勝")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "t")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "勝"))))

  (nskk-it "commits KaTta to 勝った via C-j"
    (let ((dict '(("かt" . ("勝")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "勝った")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits UTta to 打った via C-j"
    (let ((dict '(("うt" . ("打")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "U")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "打った")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "sokuon okurigana henkan phase is nil after commit"
    (let ((dict '(("かt" . ("勝")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "allows continued input after sokuon okurigana commit"
    (let ((dict '(("かt" . ("勝")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "T")
        (nskk-e2e-type "ta")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "勝った")
        (nskk-e2e-type "no")
        (nskk-e2e-assert-buffer "勝ったの")))))

;;;;
;;;; Section 3: Multi-Character Vowel Okurigana
;;;;

(nskk-describe "multi-character vowel okurigana"
  (nskk-it "triggers conversion on KaE with dict entry かe"
    (let ((dict '(("かe" . ("変")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "E")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "変"))))

  (nskk-it "KaEru triggers kakutei on r, then ru completes as new input"
    (let ((dict '(("かe" . ("変")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "E")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "ru")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "変える"))))

  (nskk-it "commits OmoU to 思う via C-j"
    (let ((dict '(("おもu" . ("思")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Omo")
        (nskk-e2e-type "U")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "思う")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits NaGaReru to 流れる (3-mora reading + vowel okurigana)"
    (let ((dict '(("ながr" . ("流")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Naga")
        (nskk-e2e-type "R")
        (nskk-e2e-type "e")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "ru")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "流れる")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits KaEru and allows continued input after"
    (let ((dict '(("かe" . ("変")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "E")
        (nskk-e2e-type "ru")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "変える")
        (nskk-e2e-type "no")
        (nskk-e2e-assert-buffer "変えるの"))))

  (nskk-it "commits ArataMeru to 改める (5-mora reading + vowel okurigana)"
    (let ((dict '(("あらたm" . ("改")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Arata")
        (nskk-e2e-type "M")
        (nskk-e2e-type "e")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "ru")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "改める")
        (nskk-e2e-assert-henkan-phase nil)))))

;;;;
;;;; Section 4: Katakana Mode Okurigana (from nskk-okuri-extended-e2e-test)
;;;;

(nskk-describe "katakana mode okurigana (extended)"
  (nskk-it "triggers conversion on KaKu in katakana mode (dict key カk)"
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "commits KaKu to 書ク in katakana mode via C-j"
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書ク")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits MiRu to 見ル in katakana mode via C-j"
    (let ((dict '(("ミr" . ("見")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Mi")
        (nskk-e2e-type "R")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "見ル")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "commits vowel okurigana AI to 愛イ in katakana mode via C-j"
    (let ((dict '(("アi" . ("愛")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "愛イ")
        (nskk-e2e-assert-henkan-phase nil))))

  (nskk-it "katakana mode okurigana result differs from hiragana mode"
    (let ((dict '(("カk" . ("書")))))
      (nskk-e2e-with-buffer 'katakana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書ク")
        (should-not (equal (buffer-string) "書く")))))

  (nskk-it "sokuon okurigana KaTta produces 勝った in katakana mode"
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
;;;; Regression Tests: Uppercase Consonant Okurigana
;;;;

(nskk-describe "uppercase consonant okurigana regression"
  (nskk-it "KaK enters okurigana mode (uppercase consonant not normalized)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-type "K")  ; 大文字子音 → 送り仮名マーカー
      (should (eq (nskk-state-get-okurigana nskk-current-state) ?k))
      (nskk-e2e-assert-henkan-phase 'on)))

  (nskk-it "KaKu triggers conversion with okurigana"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")  ; 大文字子音 → 送り仮名マーカー
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "KaKu commits to 書く"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "KaKu")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書く"))))

  (nskk-it "KaKe commits to 書け (vowel-e okurigana)"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "e")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書け"))))

  (nskk-it "KAKe (all-shift) commits to 書け (uppercase-A reading)"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "A")   ; uppercase vowel: normalize-vowel-p=t → "か"
        (nskk-e2e-type "K")   ; uppercase consonant: okurigana trigger
        (nskk-e2e-type "e")   ; lowercase vowel: completes "ke"→"け"
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "書け"))))

  (nskk-it "MiRu commits to 見る with uppercase consonant"
    (let ((dict '(("みr" . ("見")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "MiR")
        (nskk-e2e-type "u")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "見る"))))

  (nskk-it "H O produces ▽ほ (uppercase vowel normalized to lowercase)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "H")
      (nskk-e2e-type "O")  ; 大文字母音 → 小文字化して読みとして処理
      (nskk-e2e-assert-henkan-phase 'on)
      (should (null (nskk-state-get-okurigana nskk-current-state)))
      (nskk-e2e-assert-buffer-matches "\u25BD\u307B")))  ; ▽ほ

  (nskk-it "K A produces ▽か (uppercase vowel normalized to lowercase)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "K")
      (nskk-e2e-type "A")  ; 大文字母音 → 小文字化
      (nskk-e2e-assert-henkan-phase 'on)
      (should (null (nskk-state-get-okurigana nskk-current-state)))
      (nskk-e2e-assert-buffer-matches "\u25BD\u304B")))  ; ▽か

  (nskk-it "mixed: KaKiKu with dict converts correctly"
    (let ((dict '(("かきk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "KaKiK")  ; KiK: iは読み、Kは送り仮名
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")))))

;;;;
;;;; Okurigana Registration (辞書登録) Tests
;;;;

(nskk-describe "okurigana registration: reading format"
  (nskk-it "shows stem*kana format in registration prompt (e.g. ほ*け for HOKE)"
    (let* ((captured-prompt nil))
      (nskk-e2e-with-buffer 'hiragana nskk--test-minimal-dict
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (prompt &rest _)
                     (setq captured-prompt prompt)
                     "")))
          (nskk-e2e-type "H")
          (nskk-e2e-type "O")
          (nskk-e2e-type "K")
          (nskk-e2e-type "E"))
        (should (stringp captured-prompt))
        (should (string-match-p "ほ\\*け" captured-prompt)))))

  (nskk-it "cancelling okurigana registration preserves preedit ▽ほけ (okuri-kana stays)"
    (nskk-e2e-with-buffer 'hiragana nskk--test-minimal-dict
      (nskk-e2e-type "H")
      (nskk-e2e-type "O")
      (nskk-e2e-type "K")
      (nskk-e2e-type "E")
      (nskk-e2e-assert-buffer "▽ほけ"
                              "Cancel preserves preedit with okuri-kana for multi-char stem reuse")
            (nskk-e2e-assert-henkan-phase 'on "Phase should be restored to on after cancel")))

  (nskk-it "successful okurigana registration inserts the registered word"
    (nskk-e2e-with-buffer 'hiragana nskk--test-minimal-dict
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "穂毛")))
        (nskk-e2e-type "H")
        (nskk-e2e-type "O")
        (nskk-e2e-type "K")
        (nskk-e2e-type "E"))
      (nskk-e2e-assert-buffer "穂毛" "Registered word should be inserted")
      (nskk-e2e-assert-henkan-phase nil "Phase should be nil after successful registration"))))

;;;;
;;;; PBT: Okurigana input invariants
;;;;

(nskk-deftest-table okurigana-basic-consonants
  :description "All standard okurigana consonant markers are uppercase ASCII"
  :columns (input _expected)
  :rows (("K" "K")
         ("S" "S")
         ("T" "T")
         ("N" "N")
         ("H" "H")
         ("M" "M")
         ("Y" "Y")
         ("R" "R")
         ("G" "G")
         ("Z" "Z")
         ("D" "D")
         ("B" "B")
         ("P" "P"))
  :body (should (and (stringp input)
                     (= 1 (length input))
                     (>= (aref input 0) ?A)
                     (<= (aref input 0) ?Z))))

(nskk-property-test okurigana-input-does-not-crash
  ((pattern okurigana-pattern))
  (condition-case nil
      (nskk-with-test-buffer 'hiragana
        (should (nskk-state-p nskk-current-state))
        t)
    (error nil))
  20)

(nskk-describe "Okurigana input properties"
  (nskk-it "okurigana consonant generator always produces uppercase single char"
    (nskk-for-all ((consonant okurigana-consonant))
      (should (stringp consonant))
      (should (= 1 (length consonant)))
      (should (>= (aref consonant 0) ?A))
      (should (<= (aref consonant 0) ?Z)))))


;;;;
;;;; Property: Post-Commit Okurigana Buffer Content
;;;;

(nskk-deftest-table okurigana-post-commit-content
  :columns (reading okuri-trigger okuri-suffix dict-key kanji expected)
  :rows (("Ka" "K" "u"  "かk" "書" "書く")   ;; vowel u
         ("Mi" "R" "u"  "みr" "見" "見る")   ;; vowel u, r-consonant
         ("Ki" "K" "u"  "きk" "聞" "聞く")   ;; vowel u, different reading
         ("No" "M" "u"  "のm" "飲" "飲む")   ;; vowel u, m-consonant
         ("Ha" "N" "a"  "はn" "話" "話な")   ;; vowel a
         ("Ka" "K" "i"  "かk" "書" "書き")   ;; vowel i (KAKe-like pattern)
         ("Ka" "K" "e"  "かk" "書" "書け")   ;; vowel e (KAKe pattern: 書け)
         ("Ka" "K" "o"  "かk" "書" "書こ")   ;; vowel o
         ("Mi" "R" "e"  "みr" "見" "見れ"))  ;; vowel e, r-consonant
  :body (nskk-e2e-with-buffer 'hiragana (list (cons dict-key (list kanji)))
          (nskk-e2e-type reading)
          (nskk-e2e-type okuri-trigger)
          (nskk-e2e-type okuri-suffix)
          (nskk-e2e-assert-converting)
          (nskk-e2e-type "C-j")
          (nskk-e2e-assert-henkan-phase nil)
          (nskk-e2e-assert-buffer expected
                                  (format "okurigana commit %S+%S+%S → %S failed"
                                          reading okuri-trigger okuri-suffix expected))))

;;;;
;;;; SPC during partial consonant okurigana
;;;;

(nskk-describe "SPC during partial consonant okurigana (TR-001 through TR-005)"
  (nskk-it "TR-001: SPC during ▽か*k rejects missing okurigana"
    (let ((dict '(("かk" . ("書" "佳")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "K")
        (should-error (nskk-e2e-type "SPC") :type 'error)
        (nskk-e2e-assert-henkan-phase 'on)
        (nskk-e2e-assert-buffer "▽か*"))))

  (nskk-it "TR-002: SPC during ▽か*k with empty dict retains preedit"
    (let ((dict '(("あ" . ("亜")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "K")
        (should-error (nskk-e2e-type "SPC") :type 'error)
        (nskk-e2e-assert-henkan-phase 'on)
        (nskk-e2e-assert-buffer "▽か*"))))

  (nskk-it "TR-003: rejected SPC during ▽か*k clears pending consonant"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "K")
        (should-error (nskk-e2e-type "SPC") :type 'error)
        (should (string-empty-p (nskk-state-romaji-buffer)))
        (nskk-e2e-assert-buffer "▽か*"))))

  (nskk-it "TR-004: repeated SPC still rejects absent okurigana"
    (let ((dict '(("かk" . ("書" "佳"))))
          (nskk-henkan-show-candidates-nth 4))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "K")
        (should-error (nskk-e2e-type "SPC") :type 'error)
        (should-error (nskk-e2e-type "SPC") :type 'error)
        (nskk-e2e-assert-buffer "▽か*"))))

  (nskk-it "TR-005: normal SPC conversion (▽か + SPC) still works (regression guard)"
    (let ((dict '(("か" . ("花" "香")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "花")))))

;;;;
;;;; Okurigana bug regression tests (OKU-2, OKU-4)
;;;;

(nskk-describe "Okurigana bug regression (OKU-2, OKU-4)"
  (nskk-it "OKU-2: exhaust-candidates passes okurigana-aware reading to registration"
    (let ((dict '(("かk" . ("書"))))
          (nskk-henkan-show-candidates-nth 2)
          (captured-prompt nil))
      (nskk-e2e-with-buffer 'hiragana dict
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (prompt &rest _)
                     (setq captured-prompt prompt)
                     "")))
          (nskk-e2e-type "K")
          (nskk-e2e-type "a")
          (nskk-e2e-type "K")
          (nskk-e2e-type "u")
          (nskk-e2e-assert-converting)
          (nskk-e2e-type "SPC")
          (nskk-e2e-type "SPC")
          (should captured-prompt)
          (should (string-match-p "か\\*く" captured-prompt))
          (let ((reading-part (if (string-match "\\] \\(.+\\): \\'" captured-prompt)
                                  (match-string 1 captured-prompt)
                                captured-prompt)))
            (should-not (string-match-p "書" reading-part)))))))

  (nskk-it "OKU-4: SPC without okurigana rejects conversion before continuation"
    (let ((dict '(("かk" . ("書" "掛"))))
          (on-found-called nil))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "K")
        (nskk-e2e-type "a")
        (nskk-e2e-type "K")
        (should-error
         (nskk-start-conversion/k
          (lambda (&rest _args)
            (setq on-found-called t))
          #'ignore
          #'ignore))
        (should-not on-found-called)
        (nskk-e2e-assert-buffer "▽か*")))))

;;;;
;;;; Post-command-handler okurigana guard (point-escape regression)

(nskk-describe "post-command-handler okurigana guard"
  (nskk-it "does not auto-commit vowel okurigana OI after post-command-handler"
    (let ((dict '(("おi" . ("推" "置")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "O")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "推")
        (nskk--post-command-handler)
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "推"))))

  (nskk-it "allows SPC cycling after post-command-handler on vowel okurigana"
    (let ((dict '(("おi" . ("推" "置")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "O")
        (nskk-e2e-type "I")
        (nskk--post-command-handler)
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "SPC")
        (nskk--post-command-handler)
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "置"))))

  (nskk-it "commits vowel okurigana OI with C-j after post-command-handler"
    (let ((dict '(("おi" . ("推")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "O")
        (nskk-e2e-type "I")
        (nskk--post-command-handler)
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "推い"))))

  (nskk-it "does not auto-commit consonant okurigana KaKu after post-command-handler"
    (let ((dict '(("かk" . ("書" "掛")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "u")
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書")
        (nskk--post-command-handler)
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "書"))))

  (nskk-it "does not auto-commit AI vowel okurigana after post-command-handler"
    (let ((dict '(("あi" . ("愛" "哀")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "A")
        (nskk-e2e-type "I")
        (nskk-e2e-assert-converting)
        (nskk--post-command-handler)
        (nskk-e2e-assert-converting)
        (nskk-e2e-assert-overlay-shows "愛")
        (nskk-e2e-type "SPC")
        (nskk--post-command-handler)
        (nskk-e2e-assert-overlay-shows "哀")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-buffer "哀い")))))

(provide 'nskk-okurigana-e2e-test)

;;; nskk-okurigana-e2e-test.el ends here
