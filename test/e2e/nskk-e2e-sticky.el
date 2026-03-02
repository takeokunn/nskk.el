;;; nskk-e2e-sticky.el --- E2E sticky shift tests for NSKK  -*- lexical-binding: t; -*-

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

;; E2E tests for sticky shift (DDSKK skk-sticky).
;; Currently ert-skip; implementation pending (FR-003).

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)

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
        (nskk-e2e-type "Ka")   ; → ▽ か
        ;; Then ";a" == "A": uppercase vowel inside preedit should act as the
        ;; okurigana marker, triggering conversion with the vowel as okurigana.
        (nskk-e2e-type ";")
        (nskk-e2e-type "a")
        ;; Conversion (▼) phase must be active.
        (nskk-e2e-assert-henkan-phase 'active)))))

(provide 'nskk-e2e-sticky)

;;; nskk-e2e-sticky.el ends here
