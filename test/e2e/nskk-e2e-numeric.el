;;; nskk-e2e-numeric.el --- E2E numeric conversion tests for NSKK  -*- lexical-binding: t; -*-

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
;; Currently ert-skip; implementation pending (FR-002).
;; Uses DDSKK-standard numeric type codes.

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)

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

(provide 'nskk-e2e-numeric)

;;; nskk-e2e-numeric.el ends here
