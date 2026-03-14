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

;; E2E tests for abbrev mode (DDSKK §5.6 Abbreviation).
;; Covers: ASCII bypass, SPC conversion, backspace, C-g cancel,
;; self-insert for mode-switch keys, C-j from idle, RET conversion.

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(eval-when-compile (require 'cl-lib))

;;;; Abbrev Mode Tests
;;
;; Current implementation inserts ASCII directly in abbrev mode
;; (full dictionary-assisted lookup is a future feature).

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
;;
;; These tests cover the two functions modified in the abbrev mode bug fix:
;;
;;   1. `nskk-self-insert' (nskk-input.el):
;;      In abbrev mode, all printable ASCII chars bypass the Prolog routing
;;      path and go directly to `nskk-process-abbrev-input'.  The existing
;;      `abbrev-typing-inserts-ascii' test above covers basic insertion, but
;;      does not verify that the Prolog input-route is truly bypassed for
;;      chars that would otherwise match a Japanese-mode rule (e.g. uppercase
;;      letters that normally start okurigana, or "n" which accumulates in
;;      the romaji buffer).
;;
;;   2. `nskk--current-key-state' (nskk-keymap.el):
;;      In abbrev mode with a conversion-start marker set, this now returns
;;      `preedit' even when `nskk--has-preedit' is false (i.e. the marker
;;      was set but point hasn't moved past the ▽ yet).  This makes SPC
;;      trigger `nskk-start-conversion' rather than self-insert.  The
;;      complementary guard inside `nskk-start-conversion' itself makes
;;      SPC immediately after "/" (empty abbrev preedit) a no-op.

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
  ;; q, l, L, and / are bound to nskk-handle-q / nskk-handle-l / nskk-handle-upper-l /
  ;; nskk-handle-slash in the mode map.  Each uses nskk-with-japanese-mode which
  ;; checks japanese-mode/1; abbrev is NOT a Japanese mode, so the macro falls
  ;; through to (self-insert-command 1).  The character therefore lands in the
  ;; buffer verbatim via Emacs's built-in self-insert, bypassing nskk-self-insert.
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
  ;; When abbrev mode is entered directly (no preedit marker set),
  ;; the kakutei state is direct-idle.  kakutei-action maps direct-idle →
  ;; enter-hiragana, so C-j switches to hiragana and leaves an empty buffer.
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
  ;; / + "test" + SPC triggers dictionary conversion.
  ;; RET (commit-candidate) commits the first candidate without a newline.
  (nskk-it "commits first candidate with RET after SPC conversion"
    (let ((dict '(("test" . ("テスト" "Test")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "/")
        (nskk-e2e-type "test")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "RET")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-buffer "テスト"))))

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
        (nskk-e2e-assert-buffer "Test")))))

;;;; 8. C-g during abbrev conversion cancels back to preedit

(nskk-describe "abbrev mode C-g during conversion"
  ;; After / + "test" + SPC enters conversion (▼), C-g should cancel conversion
  ;; and restore the preedit reading text to the buffer.
  (nskk-it "cancels conversion and restores reading text"
    (let ((dict '(("test" . ("テスト")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (nskk-e2e-type "/")
        (nskk-e2e-type "test")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-converting)
        (nskk-e2e-type "C-g")
        (nskk-e2e-assert-not-converting)))))

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
