;;; nskk-abbrev-e2e-test.el --- E2E abbrev mode tests for NSKK  -*- lexical-binding: t; -*-

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

;;;; Abbrev Mode Tests

(nskk-describe "abbrev mode basic behavior"
  (nskk-it "inserts ASCII letters directly in abbrev mode"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-type "te")
      (nskk-e2e-type "st")
      (nskk-e2e-assert-buffer "▽test")))

  (nskk-it "returns to hiragana from abbrev via C-j"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "self-inserts l in abbrev mode"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "l")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-buffer "▽l"))))

;;;;
;;;; Abbrev Mode — Input and Conversion Scenarios
;;;;

;;;; 1. ASCII chars in abbrev mode bypass Prolog routing

(nskk-describe "abbrev mode ASCII bypass"
  (nskk-it "inserts uppercase verbatim without triggering okurigana"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "Te")
      (nskk-e2e-type "st")
      (nskk-e2e-assert-buffer "▽Test")))

  (nskk-it "inserts n directly without accumulating in romaji buffer"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "nn")
      (nskk-e2e-assert-buffer "▽nn")))

  (nskk-it "inserts digits and symbols directly"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "1")
      (nskk-e2e-type "2")
      (nskk-e2e-type "3")
      (nskk-e2e-assert-buffer "▽123"))))

;;;; 2. SPC in abbrev mode — conversion trigger

(nskk-describe "abbrev mode SPC conversion"
  (nskk-it "triggers dictionary conversion after typing text"
    (let ((dict '(("test" . ("テスト")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "/")
        (nskk-e2e-assert-mode 'abbrev)
        (nskk-e2e-type "te")
        (nskk-e2e-type "st")
        (nskk-e2e-assert-buffer "▽test")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting))))

  (nskk-it "inserts space when preedit is empty immediately after /"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-not-converting))))

;;;; 3. Backspace in abbrev preedit

(nskk-describe "abbrev mode backspace"
  (nskk-it "deletes last character in abbrev preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "te")
      (nskk-e2e-type "st")
      (nskk-e2e-assert-buffer "▽test")
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽tes")
      (nskk-e2e-assert-mode 'abbrev)))

  (nskk-it "cancels preedit entirely on DEL at empty abbrev preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-assert-henkan-phase nil))))

;;;; 4. C-g cancel in abbrev preedit

(nskk-describe "abbrev mode C-g cancel"
  (nskk-it "cancels and clears preedit buffer with text"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "te")
      (nskk-e2e-assert-buffer "▽te")
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-assert-not-converting)))

  (nskk-it "cancels cleanly on empty abbrev preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-buffer "")
      (nskk-e2e-assert-not-converting))))

;;;; 5. q and L in abbrev preedit self-insert

(nskk-describe "abbrev mode self-insert for mode-switch keys"
  (nskk-it "q in abbrev preedit self-inserts q into preedit text"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "q")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-buffer "▽q")))

  (nskk-it "L in abbrev preedit self-inserts L into preedit text"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "L")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-buffer "▽L"))))

;;;; 6. C-j from abbrev idle (no preedit) returns to hiragana

(nskk-describe "abbrev mode C-j from idle"
  (nskk-it "C-j from abbrev idle returns to hiragana"
    (nskk-e2e-with-buffer 'abbrev nil
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "buffer is empty after C-j from abbrev idle"
    (nskk-e2e-with-buffer 'abbrev nil
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer ""))))

;;;; 7. Abbrev mode conversion via RET and SPC cycling

(nskk-describe "abbrev mode conversion via RET"
  (nskk-it "commits first candidate with RET after SPC conversion"
    (let ((dict '(("test" . ("テスト" "Test")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "/")
        (nskk-e2e-type "test")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "RET")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "テスト\n")
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "cycles to second candidate with SPC then commits with C-j"
    (let ((dict '(("test" . ("テスト" "Test")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "/")
        (nskk-e2e-type "test")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "Test")
        (nskk-e2e-assert-mode 'hiragana)))))

;;;; 8. C-g during abbrev conversion cancels back to preedit

(nskk-describe "abbrev mode C-g during conversion"
  (nskk-it "cancels conversion and restores reading text"
    (let ((dict '(("test" . ("テスト")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "/")
        (nskk-e2e-type "test")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-not-converting)))))

;;;; 9. Abbrev mode restores previous mode after confirmation

(nskk-describe "abbrev mode restores previous mode after confirmation"
  (nskk-it "C-j preedit commit restores hiragana"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "test")
      (nskk-e2e-assert-buffer "▽test")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "test")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "C-j preedit commit restores katakana"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "abc")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer "abc")
      (nskk-e2e-assert-mode 'katakana)))

  (nskk-it "SPC candidate commit with RET restores hiragana"
    (let ((dict '(("test" . ("テスト")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "/")
        (nskk-e2e-type "test")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "RET")
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "SPC candidate commit with C-j restores hiragana"
    (let ((dict '(("test" . ("テスト")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "/")
        (nskk-e2e-type "test")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-mode 'hiragana))))

  (nskk-it "C-g cancel still restores hiragana"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "test")
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-mode 'hiragana))))

;;;;
;;;; Property-Based Tests
;;;;

(nskk-deftest-table abbrev-ascii-sequences
  :columns (input expected)
  :rows (("test"  "▽test")
         ("hello" "▽hello")
         ("abc"   "▽abc"))
  :body
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-type input)
    (nskk-e2e-assert-buffer expected)))

(nskk-describe "Abbrev mode property"
  (nskk-it "/ entry does not crash in any mode"
    (dotimes (_ 20)
      (nskk-for-all ((mode valid-mode))
        (nskk-e2e-with-buffer mode nil
          (condition-case err
              (nskk-e2e-type "/")
            (error (ert-fail (format "Abbrev entry / crashed in mode %s: %s"
                                     mode (error-message-string err))))))))))


(nskk-describe "Abbrev property: ASCII bypass"
  (nskk-it "any ASCII sequence in abbrev shows ▽prefix"
    (dotimes (_ 20)
      (nskk-for-all ((r romaji-basic))
        (nskk-e2e-with-buffer 'hiragana nil
          (nskk-e2e-type "/")
          (nskk-e2e-type r)
          (should (string-prefix-p "▽" (buffer-string))))))))

(provide 'nskk-abbrev-e2e-test)

;;; nskk-abbrev-e2e-test.el ends here
