;;; nskk-pending-features-e2e-test.el --- E2E tests for pending/unimplemented features  -*- lexical-binding: t; -*-

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

;; E2E tests for ddskk-compatible features in nskk.el.
;;
;; Features documented here:
;;   - Sticky shift (スティッキーシフト)
;;   - SKK numeric conversion (#0, #3, #4, #8 suffixes)
;;   - Dynamic completion (動的補完, Tab key)

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)

;;;;
;;;; Section 1: Sticky Shift (スティッキーシフト)
;;;;

;; In ddskk, sticky-shift mode allows typing ";" to act as a Shift key for
;; the immediately following character.  This lets users enter uppercase
;; letters (and thereby trigger preedit or okurigana) without holding Shift.
;;
;; Key rules:
;;   ";"  followed by a consonant letter  → treated as the uppercase consonant
;;                                          (e.g., ;k == K → starts ▽ preedit)
;;   ";"  followed by a vowel letter      → treated as uppercase vowel
;;                                          (e.g., ;a == A → uppercase okurigana trigger)
;;   ";;" (double semicolon)              → cancel sticky shift, self-insert ";"

(nskk-describe "sticky shift mode (スティッキーシフト)"
  (nskk-it "semicolon followed by consonant starts preedit (▽)"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; ";" acts as Shift; ";k" is equivalent to "K" → starts ▽ preedit.
      (nskk-e2e-type ";")
      (nskk-e2e-type "k")
      ;; Preedit (▽) phase must be active after ;k.
      (nskk-e2e-assert-henkan-phase 'on)))

  (nskk-it "double semicolon self-inserts a literal semicolon"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; ";;" cancels the sticky-shift state and inserts ";" literally.
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      ;; No preedit; buffer should contain only the semicolon character.
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer ";")))

  (nskk-it "semicolon followed by uppercase vowel triggers okurigana marker"
    ;; Okurigana lookup needs a matching dict entry (key = "かa").
    (let ((dict '(("かa" . ("蚊")))))
      (nskk-e2e-with-buffer 'hiragana dict
        ;; First start preedit normally: "K" → ▽ preedit begins.
        (nskk-e2e-type "Ka")   ; → ▽か
        ;; Then ";a" == "A": uppercase vowel inside preedit should act as the
        ;; okurigana marker, triggering conversion with the vowel as okurigana.
        (nskk-e2e-type ";")
        (nskk-e2e-type "a")
        ;; Conversion (▼) phase must be active.
        (nskk-e2e-assert-henkan-phase 'active)))))

;;;;
;;;; Section 2: SKK Numeric Conversion
;;;;

;; ddskk supports a "#" prefix in readings to trigger numeric conversion.
;; The digit suffix after "#" in the dictionary entry selects the conversion type:
;;
;;   #0 = kanji numerals (漢数字)                   e.g., 123 → 百二十三
;;   #3 = kanji with unit counter (漢数字+助数詞)    e.g., 5個
;;   #4 = kanji positional (序数)                   e.g., 1 → 第一
;;   #8 = full-width Arabic numerals (全角数字)      e.g., 123 → １２３
;;
;; During input, "#" followed by digits enters a numeric reading; pressing SPC
;; invokes nskk-numeric-convert to produce the appropriate candidate.

(defconst nskk-e2e--numeric-dict
  '(("#1-ji" . ("第#4時" "#8時"))
    ("#1-ko" . ("#0個" "#3個")))
  "Sample numeric conversion dictionary entries for pending-features tests.
Entries follow the ddskk # notation where the digit suffix specifies the
conversion type.  These entries are intentionally illustrative and may not
match the final implementation format.")

(nskk-describe "SKK numeric conversion"
  (nskk-it "converts reading with #0 suffix to kanji numerals"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--numeric-dict
      ;; Type the reading "#1-ko": "#" enters numeric mode, "1-ko" is literal.
      ;; The dict entry "#1-ko" maps to "#0個" → kanji numeral + 個.
      (nskk-e2e-type "#1-ko")
      (nskk-e2e-type "SPC")
      ;; With #0 conversion: "1" → "一", so candidate should be "一個".
      ;; Commit with C-j to flush from ▼ overlay into buffer text.
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "一個")))

  (nskk-it "converts reading with #8 suffix to full-width Arabic numerals"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--numeric-dict
      ;; The dict entry "#1-ji" maps to ("第#4時" "#8時").
      ;; First SPC → first candidate "第#4時" → "第一時".
      ;; Second SPC → next candidate "#8時" → "１時".
      (nskk-e2e-type "#1-ji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC") ; cycle to second candidate (#8時)
      ;; Commit to flush "１時" into buffer.
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "１時")))

  (nskk-it "converts reading with #4 suffix to positional kanji (序数)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--numeric-dict
      ;; The dict entry "#1-ji" maps to "第#4時" → 第 + positional kanji + 時.
      (nskk-e2e-type "#1-ji")
      (nskk-e2e-type "SPC")
      ;; With #4 conversion: "1" → "一", candidate = "第一時".
      ;; Commit to flush into buffer.
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "第一時"))))

;;;;
;;;; Section 3: Dynamic Completion (動的補完)
;;;;

;; ddskk supports dynamic completion (dcomp) via the Tab key during preedit
;; (▽ phase).  When the user has typed a partial reading, Tab looks up entries
;; in the dictionary whose keys start with the typed prefix, and completes the
;; reading inline.  Subsequent Tab presses cycle through additional completions.
;;
;; Expected behaviors:
;;   - Tab in ▽ preedit with a prefix that has dict matches → reading is
;;     completed to the first matching key; conversion phase stays 'on (preedit).
;;   - Tab again → cycles to the next matching completion.
;;   - Tab with no matches → no change (or a user-visible indication of no match).

(defconst nskk-e2e--dcomp-dict
  '(("かんじ" . ("漢字"))
    ("かんが" . ("考え"))
    ("かんしゃ" . ("感謝")))
  "Dictionary entries for dynamic completion tests.
All three readings share the prefix \\='かん\\=' to exercise prefix-based completion.")

(nskk-describe "dynamic completion via Tab key (動的補完)"
  (nskk-it "Tab in preedit completes reading from dictionary prefix"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
      ;; "Kan" → ▽ かん (partial reading, prefix of かんじ / かんが / かんしゃ).
      (nskk-e2e-type "Kan")
      ;; Tab should complete to the first dict key matching prefix "かん".
      (nskk-e2e-type "TAB")
      ;; Still in preedit (▽) phase — completion extends the reading, not converts.
      (nskk-e2e-assert-henkan-phase 'on)
      ;; Reading has been extended; e.g., preedit now shows ▽かんじ.
      ;; (Exact first completion order is implementation-defined.)
      (should (> (length (nskk-preedit-string)) (length "かん")))))

  (nskk-it "repeated Tab cycles through all completions"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
      (nskk-e2e-type "Kan")
      ;; First Tab → first completion (e.g., かんじ).
      (nskk-e2e-type "TAB")
      (let ((first-completion (nskk-preedit-string)))
        ;; Second Tab → next completion (e.g., かんが).
        (nskk-e2e-type "TAB")
        (let ((second-completion (nskk-preedit-string)))
          ;; Completions must be distinct.
          (should-not (equal first-completion second-completion))))))

  (nskk-it "Tab with no matching prefix does not change preedit"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
      ;; "Xyz" → ▽ (no such prefix in dict).
      (nskk-e2e-type "Xyz")
      (let ((reading-before (nskk-preedit-string)))
        ;; Tab should be a no-op (or ring the bell) when no completions exist.
        (nskk-e2e-type "TAB")
        ;; Preedit must be unchanged.
        (should (equal reading-before (nskk-preedit-string)))
        ;; Still in preedit phase.
        (nskk-e2e-assert-henkan-phase 'on)))))

(provide 'nskk-pending-features-e2e-test)
;;; nskk-pending-features-e2e-test.el ends here
