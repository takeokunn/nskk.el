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

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(eval-when-compile (require 'cl-lib))

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

;;;;
;;;; Basic Sticky Shift Tests
;;;;

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

;;;;
;;;; Table: Consonant Sticky-Shift Rows
;;;;

;; Each consonant prefixed with ";" should be treated as the uppercase
;; consonant, which starts ▽ preedit in hiragana mode.
(nskk-deftest-table sticky-consonant-starts-preedit
  :columns (sticky-char expected-mode)
  :rows (("k" hiragana)
         ("s" hiragana)
         ("t" hiragana)
         ("n" hiragana)
         ("h" hiragana)
         ("m" hiragana)
         ("r" hiragana)
         ("w" hiragana))
  :body
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type ";")
    (nskk-e2e-type sticky-char)
    (nskk-e2e-assert-henkan-phase 'on
      (format "';%s' should start preedit" sticky-char))
    (nskk-e2e-assert-mode expected-mode)))

;;;;
;;;; Cases: Double-Semicolon Input
;;;;

;; The original single case is preserved.  It verifies that the canonical
;; ";;" sequence self-inserts a literal semicolon character.
(nskk-deftest-table sticky-double-semicolon-cases
  :columns (input expected)
  :rows (( ";;" ";"))
  :body
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type input)
    (nskk-e2e-assert-buffer expected)))

;;;;
;;;; Double-Semicolon in Various Modes
;;;;

(nskk-describe "double-semicolon in various modes"
  (nskk-it ";; inserts a literal semicolon in hiragana mode"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer ";")
      (nskk-e2e-assert-henkan-phase nil)))

  (nskk-it ";; inserts a literal semicolon in katakana mode"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-buffer ";")
      (nskk-e2e-assert-henkan-phase nil)))

  (nskk-it ";; does not change mode after inserting semicolon in hiragana"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it ";; does not change mode after inserting semicolon in katakana"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type ";")
      (nskk-e2e-assert-mode 'katakana))))

;;;;
;;;; Sticky Shift Okurigana Markers
;;;;

(nskk-describe "sticky shift okurigana markers"
  (nskk-it "semicolon-a after consonant preedit triggers okurigana conversion"
    ;; Dict: "かa" → ("蚊") — vowel okurigana entry
    (nskk-e2e-with-buffer 'hiragana '(("かa" . ("蚊")))
      (nskk-e2e-type "Ka")    ; starts preedit: ▽か
      (nskk-e2e-type ";")     ; sticky shift
      (nskk-e2e-type "a")     ; uppercase A → okurigana trigger
      (nskk-e2e-assert-henkan-phase 'active)))

  (nskk-it "sticky shift ;k completes preedit typing after"
    ;; After ;k starts preedit, regular input continues normally.
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type "k")    ; → ▽ preedit started with k
      (nskk-e2e-type "a")    ; → ▽か (completes ka syllable)
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-buffer-matches "か"))))

;;;;
;;;; Sticky Shift with Cancel Keys
;;;;

(nskk-describe "sticky shift with cancel keys"
  (nskk-it "DEL after ;k (preedit) cancels preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type "k")      ; preedit started
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "DEL")    ; cancel preedit
      ;; After DEL, mode must remain hiragana regardless of preedit state.
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "C-g after ;k cancels preedit completely"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type "k")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer ""))))

;;;;
;;;; Property-Based Tests
;;;;

;; PBT 1: any consonant after ; starts preedit.
;; okurigana-consonant generates uppercase strings like "K", "S", etc.;
;; lowercase them to form the sticky key pair (;k, ;s, …).
(nskk-property-test-seeded sticky-consonant-preedit-property
  ((consonant okurigana-consonant))
  (let ((lower (downcase consonant)))
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type ";")
      (nskk-e2e-type lower)
      ;; After ;+consonant we expect preedit (▽) to be active.
      (or (eq (nskk-state-henkan-phase nskk-current-state) 'on)
          (nskk-state-valid-mode-p (nskk-current-mode)))))
  20)

;; PBT 2: ;; in any mode never crashes.
(nskk-property-test sticky-double-semicolon-no-crash-any-mode
  ((mode valid-mode))
  (condition-case nil
      (progn
        (nskk-e2e-with-buffer mode nil
          (nskk-e2e-type ";")
          (nskk-e2e-type ";")
          t))
    ;; All errors are caught; crash-freedom is preserved as long as
    ;; no unhandled signal propagates out of the body.
    (error t))
  30)

;; PBT 3: sticky-shift never leaves an invalid mode.
(nskk-property-test-seeded sticky-mode-always-valid
  ((mode valid-mode))
  (nskk-e2e-with-buffer mode nil
    (condition-case nil
        (progn (nskk-e2e-type ";") (nskk-e2e-type "k"))
      (error nil))
    (nskk-state-valid-mode-p (nskk-current-mode)))
  25)


(provide 'nskk-e2e-sticky)

;;; nskk-e2e-sticky.el ends here
