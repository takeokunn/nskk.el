;;; nskk-e2e-dcomp.el --- E2E dynamic completion tests for NSKK  -*- lexical-binding: t; -*-

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

;; E2E tests for dynamic completion via TAB key (DDSKK §5.3).

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)

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

(provide 'nskk-e2e-dcomp)

;;; nskk-e2e-dcomp.el ends here
