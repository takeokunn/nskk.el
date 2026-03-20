;;; nskk-numeric-e2e-test.el --- E2E numeric conversion tests for NSKK  -*- lexical-binding: t; -*-

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

;; E2E tests for SKK numeric conversion (DDSKK §5.5).
;; Uses DDSKK-standard numeric type codes.

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)

;; DDSKK numeric type codes:
;;   #0 = 無変換 (literal, no change)          e.g., 1 → "1"
;;   #1 = 全角数字 (full-width Arabic digits)  e.g., 1 → "１"
;;   #2 = 漢数字 digit-by-digit               e.g., 1024 → "一〇二四"
;;   #3 = 漢数字 with place values             e.g., 1024 → "千二十四"
;;   #8 = comma-grouped decimal                e.g., 1024 → "1,024"
;;
;; During input, "#" followed by digits enters a numeric reading; pressing SPC
;; invokes nskk-numeric-convert to produce the appropriate candidate.

(defconst nskk-e2e--numeric-dict
  '(("#ji" . ("第#3時" "#1時"))
    ("#ko" . ("#0個" "#2個")))
  "Sample numeric conversion dictionary entries (DDSKK-standard codes).
#0 = literal (no conversion), #1 = full-width, #2 = kanji digit-by-digit,
#3 = kanji with place values, #8 = comma-grouped.")

(nskk-describe "SKK numeric conversion"
  (nskk-it "converts reading with #0 suffix to literal (no change)"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--numeric-dict
      ;; Type the reading "#1ko": "#" marks numeric reading, "1" is the input digit.
      ;; The dict entry "#ko" maps to "#0個" → literal + 個.
      (nskk-e2e-type "#1ko")
      (nskk-e2e-type "SPC")
      ;; With #0 conversion: "1" stays as "1" (literal), so candidate should be "1個".
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "1個")))

  (nskk-it "converts reading with #2 suffix to kanji digit-by-digit"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--numeric-dict
      ;; The dict entry "#ko" has second candidate "#2個" → kanji digit-by-digit + 個.
      (nskk-e2e-type "#1ko")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC") ; cycle to second candidate (#2個)
      ;; With #2 conversion: "1" → "一" (digit-by-digit), so candidate should be "一個".
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "一個")))

  (nskk-it "converts reading with #3 suffix to kanji with place values"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--numeric-dict
      ;; The dict entry "#ji" maps to "第#3時" → 第 + kanji with place values + 時.
      (nskk-e2e-type "#1ji")
      (nskk-e2e-type "SPC")
      ;; With #3 conversion: "1" → "一" (with place values), candidate = "第一時".
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "第一時")))

  (nskk-it "converts reading with #1 suffix to full-width Arabic numerals"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--numeric-dict
      ;; The dict entry "#ji" has second candidate "#1時" → full-width digit + 時.
      (nskk-e2e-type "#1ji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC") ; cycle to second candidate (#1時)
      ;; With #1 conversion: "1" → "１" (full-width), so candidate should be "１時".
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "１時"))))

;;;;
;;;; Table-Driven Conversion Tests
;;;;

;; Each row selects a candidate by pressing SPC spc-count times, then commits
;; with C-j, and asserts the resulting buffer content equals expected-result.
;;
;; Dict: "#ko" → ("#0個" "#2個")
;;   1 SPC → first candidate "#0個" → literal "1" → "1個"
;;   2 SPC → second candidate "#2個" → kanji digit-by-digit "1" → "一個"
;;
;; Dict: "#ji" → ("第#3時" "#1時")
;;   1 SPC → first candidate "第#3時" → place-value "1" → "第一時"
;;   2 SPC → second candidate "#1時" → full-width "1" → "１時"

(nskk-deftest-table numeric-type-codes
  :columns (input-reading spc-count expected-result)
  :rows (("#1ko"  1 "1個")      ; #0 = literal: "1" stays "1"
         ("#1ko"  2 "一個")     ; #2 = kanji digit-by-digit: "1" → "一"
         ("#1ji"  1 "第一時")   ; #3 = kanji with place values
         ("#1ji"  2 "１時"))    ; #1 = full-width: "1" → "１"
  :body
  (nskk-e2e-with-buffer 'hiragana nskk-e2e--numeric-dict
    (nskk-e2e-type input-reading)
    (dotimes (_ spc-count)
      (nskk-e2e-type "SPC"))
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-buffer expected-result
                             (format "Numeric: %S × %d SPC → %S"
                                     input-reading spc-count expected-result))))

;;;;
;;;; Multi-Digit Conversion Table
;;;;

(defconst nskk-e2e--numeric-dict-multi
  '(("#ko" . ("#0個" "#2個" "#3個")))
  "Numeric dict for multi-digit conversion tests.
Three candidates: #0 = literal, #2 = kanji digit-by-digit, #3 = place values.")

;; For input "#10ko":
;;   The numeric reading is "10", matched against "#ko" in the dict.
;;   1 SPC → "#0個" → literal "10" → "10個"
;;   2 SPC → "#2個" → kanji digit-by-digit "10" → "一〇個"
;;   3 SPC → "#3個" → kanji place values "10" → "十個"

(nskk-deftest-table numeric-multi-digit
  :columns (input-reading spc-count expected-result description)
  :rows (("#10ko"  1 "10個"   "#0: literal 10")
         ("#10ko"  2 "一〇個" "#2: kanji digit-by-digit 10 → 一〇")
         ("#10ko"  3 "十個"   "#3: kanji place values 10 → 十"))
  :body
  (nskk-e2e-with-buffer 'hiragana nskk-e2e--numeric-dict-multi
    (nskk-e2e-type input-reading)
    (dotimes (_ spc-count)
      (nskk-e2e-type "SPC"))
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-buffer expected-result description)))

;;;;
;;;; Property-Based Tests
;;;;

;; PBT 1: typing "#1ko" + SPC in hiragana mode never crashes.
;; (The generator draws any valid mode; we only run the body in hiragana so
;; the property is always checked with a consistent mode.)
(nskk-property-test-seeded numeric-hiragana-no-crash
  ((unused valid-mode))
  (condition-case _err
      (progn
        (nskk-e2e-with-buffer 'hiragana nskk-e2e--numeric-dict
          (nskk-e2e-type "#1ko")
          (nskk-e2e-type "SPC"))
        t)
    (error t))
  20)

;; PBT 2: "#1" in any valid mode never raises an unhandled error.
;; Crash-freedom across all modes (not just hiragana).
(nskk-property-test-seeded numeric-any-mode-no-crash
  ((mode valid-mode))
  (condition-case err
      (progn
        (nskk-e2e-with-buffer mode nil
          (nskk-e2e-type "#1"))
        t)
    (error
     (ert-fail (format "# input crashed in mode %s: %s"
                       mode (error-message-string err)))))
  20)

(provide 'nskk-numeric-e2e-test)

;;; nskk-numeric-e2e-test.el ends here
