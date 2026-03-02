;;; nskk-romaji-edge-e2e-test.el --- E2E tests for romaji edge rows  -*- lexical-binding: t; -*-

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

;; E2E tests for romaji table rows not covered by buffer-romaji-table-comprehensive.
;;
;; GAP-8: This file covers:
;;   1. Small kana (xa/la rows)  -- ぁぃぅぇぉ
;;   2. Small youon (xya/lya rows) -- ゃゅょ
;;   3. Small tsu (xtsu/ltsu, xtu/ltu)
;;   4. V-row -- ゔ combinations
;;   5. Th-row (foreign extension) -- てぁ etc.
;;   6. Dh-row (foreign extension) -- でぁ etc.
;;   7. Wh-row (foreign extension) -- うぁ etc.
;;   8. n' apostrophe form -- ん
;;   9. Long vowel - (hyphen) -- ー in katakana mode
;;
;; All mappings verified against nskk-converter.el before writing tests.
;;
;; Test name format: nskk-e2e-buffer-romaji-*

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(eval-when-compile (require 'cl-lib))

;;;;
;;;; Section 1: Small Kana (xa/la rows)
;;;;

(nskk-describe "small kana romaji rows"
  (nskk-deftest-table buffer-romaji-small-kana-xa-row
    :columns (romaji kana)
    ;; la/li/lu/le/lo omitted: 'l' is bound to nskk-handle-l (Latin-mode switch)
    ;; and cannot be used as a romaji prefix in E2E key-event tests.
    :rows (("xa" "ぁ") ("xi" "ぃ") ("xu" "ぅ") ("xe" "ぇ") ("xo" "ぉ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana))))

  (nskk-deftest-table buffer-romaji-small-kana-xtsu
    :columns (romaji kana)
    ;; ltsu/ltu omitted: 'l' is a mode-switch key, cannot be used as romaji prefix.
    :rows (("xtsu" "っ") ("xtu" "っ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana))))

  (nskk-deftest-table buffer-romaji-small-kana-xya-row
    :columns (romaji kana)
    ;; lya/lyu/lyo omitted: 'l' is a mode-switch key, cannot be used as romaji prefix.
    :rows (("xya" "ゃ") ("xyu" "ゅ") ("xyo" "ょ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana)))))

;;;;
;;;; Section 2: V-row (ゔ combinations)
;;;;

(nskk-describe "v-row romaji"
  (nskk-deftest-table buffer-romaji-v-row
    :columns (romaji kana)
    :rows (("va"  "ゔぁ")
           ("vi"  "ゔぃ")
           ("vu"  "ゔ")
           ("ve"  "ゔぇ")
           ("vo"  "ゔぉ")
           ("vya" "ゔゃ")
           ("vyu" "ゔゅ")
           ("vyo" "ゔょ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana)))))

;;;;
;;;; Section 3: Foreign Extension Rows
;;;;

(nskk-describe "foreign extension romaji rows"
  (nskk-deftest-table buffer-romaji-th-row
    :columns (romaji kana)
    :rows (("tha" "てぁ")
           ("thi" "てぃ")
           ("thu" "てゅ")
           ("the" "てぇ")
           ("tho" "てょ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana))))

  (nskk-deftest-table buffer-romaji-dh-row
    :columns (romaji kana)
    :rows (("dha" "でぁ")
           ("dhi" "でぃ")
           ("dhu" "でゅ")
           ("dhe" "でぇ")
           ("dho" "でょ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana))))

  (nskk-deftest-table buffer-romaji-wh-row
    :columns (romaji kana)
    :rows (("wha" "うぁ")
           ("whi" "うぃ")
           ("whu" "う")
           ("whe" "うぇ")
           ("who" "うぉ"))
    :body (nskk-e2e-with-buffer 'hiragana nil
            (nskk-e2e-type romaji)
            (nskk-e2e-assert-buffer kana
                                    (format "romaji %S → %S failed" romaji kana)))))

;;;;
;;;; Section 4: N-apostrophe
;;;;

(nskk-describe "n apostrophe romaji"
  (nskk-it "n' produces hatsuon kana"
    ;; n' is a direct rule in the converter table AND handled by
    ;; nskk-convert-n--internal which checks (aref remaining 1) == 39 (ASCII ').
    ;; The apostrophe is consumed and does not appear in the output.
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "n'")
      (nskk-e2e-assert-buffer "ん"
                              "romaji \"n'\" → \"ん\" failed")))

  (nskk-it "n' followed by vowel separates hatsuon from the vowel"
    ;; n' followed by a vowel: apostrophe explicitly separates ん from the vowel.
    ;; Without apostrophe, "na" → "な"; with apostrophe, "n'" → "ん" then "a" → "あ".
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "n'a")
      (nskk-e2e-assert-buffer "んあ"
                              "romaji \"n'a\" → \"んあ\" failed"))))

;;;;
;;;; Section 5: Long Vowel in Katakana
;;;;

(nskk-describe "long vowel in katakana"
  (nskk-it "hyphen produces long vowel mark in katakana mode"
    ;; The - rule is defined in nskk-converter.el as:
    ;;   (nskk-converter-add-rule "-" "ー")
    ;; It applies in both hiragana and katakana mode (the converter does not
    ;; distinguish modes; katakana conversion uppercases the result).
    ;; We test in katakana mode as that is the primary use case.
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type "-")
      (nskk-e2e-assert-buffer "ー"
                              "romaji \"-\" → \"ー\" failed in katakana mode"))))

;;;;
;;;; Section 6: Mode Switch Clears Pending Romaji
;;;;

(nskk-describe "mode switch clears pending romaji input"

  (nskk-it "C-j with single pending consonant clears romaji buffer"
    ;; Type "k" -> nskk-convert-input-to-kana receives 'k', converter returns
    ;; :incomplete (no vowel yet), so nskk--romaji-buffer = "k" and nothing is
    ;; inserted into the buffer.
    ;;
    ;; C-j dispatches nskk-kakutei, which calls nskk--current-kakutei-state.
    ;; Since nskk--romaji-buffer is non-empty and there is no active ▼/▽,
    ;; the state is 'romaji-pending.  kakutei-action/2 maps:
    ;;   (romaji-pending clear-romaji)
    ;; The clear-romaji action calls nskk--clear-pending-romaji and sets
    ;; nskk--romaji-buffer to "".  No kana is output.  Mode stays hiragana.
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "k")
      ;; "k" is pending -- no kana committed to the buffer yet.
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-type "C-j")
      ;; C-j in romaji-pending -> clear-romaji: romaji buffer flushed silently.
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "C-j with two-char incomplete romaji clears romaji buffer"
    ;; Type "s" then "h": converter accumulates "sh" as incomplete (waiting
    ;; for i/a/u/e/o to complete "shi"/"sha"/etc.).  nskk--romaji-buffer = "sh".
    ;; Nothing is inserted into the buffer.
    ;;
    ;; C-j -> nskk-kakutei -> state = 'romaji-pending -> action = 'clear-romaji
    ;; -> nskk--romaji-buffer = "".  Buffer stays empty, mode stays hiragana.
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "s")
      (nskk-e2e-type "h")
      ;; "sh" is incomplete -- waiting for a vowel to complete the compound.
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-type "C-j")
      ;; clear-romaji flushes the buffer without emitting any character.
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "l mode switch clears pending romaji and switches to latin"
    ;; Type "k" -> nskk--romaji-buffer = "k", buffer empty (no kana yet).
    ;;
    ;; Press "l": nskk-handle-l calls (nskk-with-japanese-mode (nskk-set-mode-latin)).
    ;; Dispatch order inside the macro:
    ;;   1. (nskk-converting-p)                        -- no (no ▼ active)
    ;;   2. (and (nskk--has-preedit) japanese-mode-p)  -- no (no ▽ marker set)
    ;;   3. (nskk--japanese-mode-active-p)             -- yes (hiragana is japanese)
    ;; Branch 3 fires: calls (nskk-set-mode-latin) directly.
    ;; nskk-set-mode-latin -> nskk--set-mode 'latin -> nskk--clear-conversion-context
    ;; nskk--clear-conversion-context explicitly does:
    ;;   (setq nskk--romaji-buffer "")  ; discards "k" silently
    ;; The pending "k" is dropped (not output) as a side effect of the mode switch.
    ;; Result: buffer empty, mode = latin.
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "k")
      ;; "k" is pending in nskk--romaji-buffer; buffer is empty.
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-type "l")
      ;; nskk-set-mode-latin -> nskk--clear-conversion-context discards "k".
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-assert-mode 'latin))))

(provide 'nskk-romaji-edge-e2e-test)

;;; nskk-romaji-edge-e2e-test.el ends here
