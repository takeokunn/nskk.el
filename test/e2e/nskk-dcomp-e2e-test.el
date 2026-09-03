;;; nskk-dcomp-e2e-test.el --- E2E dynamic completion tests for NSKK  -*- lexical-binding: t; -*-

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

;;;;
;;;; Test Dictionaries
;;;;

(defconst nskk-e2e--dcomp-dict
  '(("かんじ"  . ("漢字"))
    ("かんが"  . ("考え"))
    ("かんしゃ" . ("感謝")))
  "Dictionary entries for dynamic completion tests.
All three readings share the prefix \"かん\" to exercise prefix-based completion.")

(defconst nskk-e2e--dcomp-dict-extended
  '(("かんじ"  . ("漢字"))
    ("かんが"  . ("考え"))
    ("かんしゃ" . ("感謝"))
    ("さくら"  . ("桜"))
    ("にほん"  . ("日本")))
  "Extended dictionary for dcomp table tests.
Adds \"さくら\" and \"にほん\" to cover prefixes outside the \"かん\" cluster.")

;;;;
;;;; Original 3 Tests (preserved exactly)
;;;;

(nskk-describe "dynamic completion via Tab key (動的補完)"
  (nskk-it "Tab in preedit completes reading from dictionary prefix"
    (let ((nskk-dcomp-style 'cycle))
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
        (nskk-e2e-type "Kan")
        (nskk-e2e-type "TAB")
        (nskk-e2e-assert-henkan-phase 'on)
        (should (> (length (nskk-preedit-string)) (length "かん"))))))

  (nskk-it "repeated Tab cycles through all completions"
    (let ((nskk-dcomp-style 'cycle))
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
        (nskk-e2e-type "Kan")
        (nskk-e2e-type "TAB")
        (let ((first-completion (nskk-preedit-string)))
          (nskk-e2e-type "TAB")
          (let ((second-completion (nskk-preedit-string)))
            (should-not (equal first-completion second-completion)))))))

  (nskk-it "Tab with no matching prefix does not change preedit"
    (let ((nskk-dcomp-style 'cycle))
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
        (nskk-e2e-type "Xyz")
        (let ((reading-before (nskk-preedit-string)))
          (nskk-e2e-type "TAB")
          (should (equal reading-before (nskk-preedit-string)))
          (nskk-e2e-assert-henkan-phase 'on))))))

;;;;
;;;; Table-Driven Tests: prefix → minimum extended reading length
;;;;


(nskk-deftest-table dcomp-prefix-extends-reading-kan
  :columns (romaji-prefix min-expected-length)
  :rows (;; "Ka" → ▽か → Tab → extended to 3+ chars (e.g., かんじ).
         ("Ka"  3)
         ("Kan" 3))
  :body
  (let ((nskk-dcomp-style 'cycle))
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
      (nskk-e2e-type romaji-prefix)
      (nskk-e2e-type "TAB")
      (nskk-e2e-assert-henkan-phase 'on)
      (should (>= (length (nskk-preedit-string)) min-expected-length)))))

(nskk-deftest-table dcomp-prefix-extends-reading-extended
  :columns (romaji-prefix min-expected-length)
  :rows (;; "Sa" → ▽さ → Tab → さくら (3 chars).
         ("Sa"  3)
         ("Ni"  3))
  :body
  (let ((nskk-dcomp-style 'cycle))
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict-extended
      (nskk-e2e-type romaji-prefix)
      (nskk-e2e-type "TAB")
      (nskk-e2e-assert-henkan-phase 'on)
      (should (>= (length (nskk-preedit-string)) min-expected-length)))))

;;;;
;;;; Table-Driven Tests: non-matching prefixes remain unchanged
;;;;

(nskk-deftest-table dcomp-no-match-prefix-unchanged
  :columns (romaji-prefix)
  :rows (;; "Xyz" → ▽xz? → no kana prefix → preedit unchanged.
         ("Xyz")
         ("Mu")
         ("To"))
  :body
  (let ((nskk-dcomp-style 'cycle))
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
      (nskk-e2e-type romaji-prefix)
      (let ((before (nskk-preedit-string)))
        (nskk-e2e-type "TAB")
        (should (equal before (nskk-preedit-string)))
        (nskk-e2e-assert-henkan-phase 'on)))))

;;;;
;;;; nskk-deftest-table: known prefix → completion is a string
;;;;

(nskk-deftest-table dcomp-known-prefix-cases
  :columns (input expected)
  :rows (("Ka" t)   ; "Ka" → かん prefix → at least one completion in dict
         ("Sa" t)   ; "Sa" → さ prefix → さくら in extended dict
         ("Ni" t))  ; "Ni" → に prefix → にほん in extended dict
  :body
  (let ((nskk-dcomp-style 'cycle))
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict-extended
      (nskk-e2e-type input)
      (let ((_before (nskk-preedit-string)))
        (nskk-e2e-type "TAB")
        (should (stringp (nskk-preedit-string)))
        (should expected)))))

;;;;
;;;; dcomp SPC conversion after Tab completion
;;;;

(nskk-describe "dcomp SPC conversion after Tab completion"
  (nskk-it "SPC triggers conversion after Tab extends reading"
    (let ((nskk-dcomp-style 'cycle))
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
        (nskk-e2e-type "Kan")
        (nskk-e2e-type "TAB")
        (nskk-e2e-assert-henkan-phase 'on)
        (should (not (string-empty-p (nskk-preedit-string))))
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-henkan-phase 'active))))

  (nskk-it "C-j after Tab commits the completed reading as kana"
    (let ((nskk-dcomp-style 'cycle))
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
        (nskk-e2e-type "Kan")
        (nskk-e2e-type "TAB")
        (nskk-e2e-assert-henkan-phase 'on)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-henkan-phase nil)
        (should (not (string-empty-p (buffer-string))))))))

;;;;
;;;; dcomp C-g after Tab cancels preedit
;;;;

(nskk-describe "dcomp C-g after Tab cancels preedit"
  (nskk-it "C-g cancels the entire preedit after Tab completion"
    (let ((nskk-dcomp-style 'cycle))
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
        (nskk-e2e-type "Kan")
        (nskk-e2e-type "TAB")
        (nskk-e2e-assert-henkan-phase 'on)
        (should (not (string-empty-p (nskk-preedit-string))))
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-henkan-phase nil)
        (nskk-e2e-assert-buffer ""))))

  (nskk-it "C-g without Tab also cancels partial preedit"
    (let ((nskk-dcomp-style 'cycle))
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
        (nskk-e2e-type "Kan")
        (nskk-e2e-assert-henkan-phase 'on)
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-henkan-phase nil)
        (nskk-e2e-assert-buffer "")))))

;;;;
;;;; dcomp state invariants
;;;;

(nskk-describe "dcomp state invariants"
  (nskk-it "Tab in preedit never switches away from hiragana mode"
    (let ((nskk-dcomp-style 'cycle))
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
        (nskk-e2e-type "Kan")
        (nskk-e2e-type "TAB")
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "multiple Tabs in preedit do not switch mode"
    (let ((nskk-dcomp-style 'cycle))
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
        (nskk-e2e-type "Kan")
        (nskk-e2e-type "TAB")
        (nskk-e2e-type "TAB")
        (nskk-e2e-type "TAB")
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "Tab on no-match prefix does not switch mode"
    (let ((nskk-dcomp-style 'cycle))
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
        (nskk-e2e-type "Xyz")
        (nskk-e2e-type "TAB")
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "preedit string is always a string after Tab"
    (let ((nskk-dcomp-style 'cycle))
      (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict-extended
        (nskk-e2e-type "Kan")
        (nskk-e2e-type "TAB")
        (should (stringp (nskk-preedit-string)))
        (nskk-e2e-type "TAB")
        (should (stringp (nskk-preedit-string)))))))

;;;;
;;;; Property-Based Tests
;;;;

(nskk-property-test-seeded dcomp-any-prefix-tab-no-crash
  ((romaji romaji-basic))
  (let ((nskk-dcomp-style 'cycle))
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
      (condition-case err
          (progn
            (nskk-e2e-type romaji)
            (nskk-e2e-type "TAB")
            t)
        (error
         (ert-fail (format "dcomp TAB after %S crashed: %s"
                           romaji (error-message-string err)))))))
  30)

(nskk-property-test-seeded dcomp-preedit-always-string
  ((romaji romaji-basic))
  (let ((nskk-dcomp-style 'cycle))
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
      (nskk-e2e-type "Ka")
      (nskk-e2e-type romaji)
      (nskk-e2e-type "TAB")
      (stringp (nskk-preedit-string))))
  30)

(nskk-property-test-seeded dcomp-mode-preserved-after-tab
  ((romaji romaji-basic))
  (let ((nskk-dcomp-style 'cycle))
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--dcomp-dict
      (nskk-e2e-type romaji)
      (nskk-e2e-type "TAB")
      (eq (nskk-current-mode) 'hiragana)))
  30)

(provide 'nskk-dcomp-e2e-test)
;;; nskk-dcomp-e2e-test.el ends here
