;;; nskk-modeline-e2e-test.el --- E2E modeline tests for NSKK  -*- lexical-binding: t; -*-

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

;; E2E modeline tests for NSKK.
;;
;; Tests the `nskk-modeline-indicator' function in real nskk-mode contexts.
;; The modeline indicator is driven by mode-properties/5 Prolog facts and
;; is memoized per mode in `nskk--modeline-indicator-cache'.
;;
;; Expected modeline strings (ddskk-compatible):
;;   ascii/latin   → "SKK"
;;   hiragana      → "かな"
;;   katakana      → "カナ"
;;   katakana-半角  → "ｶﾅ"
;;   abbrev        → "aA"
;;   jisx0208-latin → "全英"
;;
;; Tests verify:
;;   1. Each mode produces the correct indicator string
;;   2. Indicator updates when mode changes (cache invalidation)
;;   3. Indicator is non-empty when nskk-mode is active
;;   4. Indicator is empty ("") when nskk-mode is not active
;;   5. Cache is keyed by mode symbol (same mode → same cached result)
;;   6. Indicator is a propertized string (not raw text)
;;   7. PBT: indicator always returns string in all modes
;;   8. PBT: indicator changes when mode changes

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(eval-when-compile (require 'cl-lib))

;;;;
;;;; Individual Mode Modeline Tests
;;;;

(nskk-describe "modeline indicator by mode"
  (nskk-it "shows SKK in ascii mode"
    (nskk-e2e-with-buffer nil nil
      (nskk-given (nskk-e2e-assert-mode 'ascii))
      (nskk-then  (nskk-e2e-assert-modeline-contains "SKK"))))

  (nskk-it "shows SKK in latin mode"
    (nskk-e2e-with-buffer 'latin nil
      (nskk-then (nskk-e2e-assert-modeline-contains "SKK"))))

  (nskk-it "shows かな in hiragana mode"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-then (nskk-e2e-assert-modeline-contains "かな"))))

  (nskk-it "shows fullwidth katakana indicator in katakana mode"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-then (nskk-e2e-assert-modeline-contains "カナ"))))

  (nskk-it "shows halfwidth katakana indicator in hankaku mode"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-then (nskk-e2e-assert-modeline-contains "ｶﾅ"))))

  (nskk-it "shows aA in abbrev mode"
    (nskk-e2e-with-buffer 'abbrev nil
      (nskk-then (nskk-e2e-assert-modeline-contains "aA"))))

  (nskk-it "shows 全英 in jisx0208-latin mode"
    (nskk-e2e-with-buffer 'jisx0208-latin nil
      (nskk-then (nskk-e2e-assert-modeline-contains "全英")))))

;;;;
;;;; Complete Mode-to-Indicator Table Test
;;;;

(nskk-describe "modeline all modes table"
  (nskk-it "all modes produce correct ddskk-compatible modeline strings"
    (let ((expected-indicators
           '((ascii         . "SKK")
             (latin         . "SKK")
             (hiragana      . "かな")
             (katakana      . "カナ")
             (katakana-半角  . "ｶﾅ")
             (abbrev        . "aA")
             (jisx0208-latin . "全英"))))
      (dolist (tc expected-indicators)
        (let ((mode (car tc))
              (expected (cdr tc)))
          (nskk-e2e-with-buffer mode nil
            (nskk-e2e-assert-modeline-contains
             expected
             (format "Mode %S should show %S in modeline" mode expected))))))))

;;;;
;;;; Modeline Update on Mode Switch Tests
;;;;

(nskk-describe "modeline update on transition"
  (nskk-it "updates when C-j switches ascii to hiragana"
    (nskk-e2e-with-buffer nil nil
      (nskk-given (nskk-e2e-assert-modeline-contains "SKK"))
      (nskk-when  (nskk-e2e-type "C-j"))
      (nskk-then
       (nskk-e2e-assert-mode 'hiragana)
       (nskk-e2e-assert-modeline-contains "かな"))))

  (nskk-it "updates when q switches hiragana to katakana"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-given (nskk-e2e-assert-modeline-contains "かな"))
      (nskk-when  (nskk-e2e-type "q"))
      (nskk-then
       (nskk-e2e-assert-mode 'katakana)
       (nskk-e2e-assert-modeline-contains "カナ"))))

  (nskk-it "updates when l switches hiragana to latin"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-given (nskk-e2e-assert-modeline-contains "かな"))
      (nskk-when  (nskk-e2e-type "l"))
      (nskk-then
       (nskk-e2e-assert-mode 'latin)
       (nskk-e2e-assert-modeline-contains "SKK"))))

  (nskk-it "updates when L switches hiragana to jisx0208-latin"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-given (nskk-e2e-assert-modeline-contains "かな"))
      (nskk-when  (nskk-e2e-type "L"))
      (nskk-then  (nskk-e2e-assert-modeline-contains "全英"))))

  (nskk-it "updates when / switches hiragana to abbrev"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-given (nskk-e2e-assert-modeline-contains "かな"))
      (nskk-when  (nskk-e2e-type "/"))
      (nskk-then  (nskk-e2e-assert-modeline-contains "aA"))))

  (nskk-it "tracks C-j then q then l transition sequence"
    (nskk-e2e-with-buffer nil nil
      ;; ascii → SKK
      (nskk-e2e-assert-modeline-contains "SKK")
      ;; C-j → hiragana → かな
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-modeline-contains "かな")
      ;; q → katakana → カナ
      (nskk-e2e-type "q")
      (nskk-e2e-assert-modeline-contains "カナ")
      ;; l → latin → SKK
      (nskk-e2e-type "l")
      (nskk-e2e-assert-modeline-contains "SKK"))))

;;;;
;;;; Modeline Structure Tests
;;;;

(nskk-describe "modeline indicator structure"
  (nskk-it "always returns a string"
    (nskk-e2e-with-buffer 'hiragana nil
      (let ((indicator (nskk-modeline-indicator)))
        (should (stringp indicator)))))

  (nskk-it "returns a propertized string not plain text"
    (nskk-e2e-with-buffer 'hiragana nil
      (let ((indicator (nskk-modeline-indicator)))
        (should (get-text-property 0 'face indicator)))))

  (nskk-it "is non-empty when nskk-mode is active"
    (nskk-e2e-with-buffer 'hiragana nil
      (let ((indicator (nskk-modeline-indicator)))
        (should (not (string-empty-p indicator))))))

  (nskk-it "returns empty string when nskk-current-state is nil"
    ;; Outside nskk-mode context, modeline should be empty
    (let ((nskk-current-state nil))
      (should (equal (nskk-modeline-indicator) "")))))

;;;;
;;;; Cache Behavior Tests
;;;;

(nskk-describe "modeline cache behavior"
  (nskk-it "invalidates cache when mode changes"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; First call populates cache for hiragana
      (let ((first-result (nskk-modeline-indicator)))
        (should (string-match-p "かな" first-result))
        ;; Second call with same mode returns cached result
        (let ((second-result (nskk-modeline-indicator)))
          (should (equal first-result second-result)))
        ;; Switch mode → cache should be invalidated
        (nskk-e2e-type "q")  ;; → katakana
        (let ((after-switch (nskk-modeline-indicator)))
          ;; New result should be for katakana
          (should (string-match-p "カナ" after-switch))))))

  (nskk-it "returns same text for the same mode symbol on repeated calls"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Call indicator multiple times for the same mode
      (let ((results (cl-loop repeat 5 collect (nskk-modeline-indicator))))
        ;; All should contain "かな"
        (dolist (r results)
          (should (string-match-p "かな" r)))))))

;;;;
;;;; Property-Based Tests (PBT)
;;;;

(nskk-property-test-exhaustive modeline-always-string
  '(ascii latin hiragana katakana katakana-半角 abbrev jisx0208-latin)
  (nskk-e2e-with-buffer item nil
    (stringp (nskk-modeline-indicator))))

(nskk-property-test-exhaustive modeline-non-empty-when-active
  '(ascii latin hiragana katakana katakana-半角 abbrev jisx0208-latin)
  (nskk-e2e-with-buffer item nil
    (let ((indicator (nskk-modeline-indicator)))
      (and (stringp indicator) (not (string-empty-p indicator))))))

(nskk-describe "PBT: modeline changes on mode switch"
  (nskk-it "indicator differs between japanese modes and direct modes"
    (dolist (jmode '(hiragana katakana))
      (dolist (dmode '(ascii latin))
        (nskk-e2e-with-buffer jmode nil
          (let ((jp-indicator (nskk-modeline-indicator)))
            (nskk--set-mode dmode)
            (nskk-modeline-update)
            (should (not (equal jp-indicator (nskk-modeline-indicator))))))))))

(nskk-property-test-exhaustive modeline-ddskk-compatible-strings
  '((ascii         . "SKK")
    (latin         . "SKK")
    (hiragana      . "かな")
    (katakana      . "カナ")
    (katakana-半角  . "ｶﾅ")
    (abbrev        . "aA")
    (jisx0208-latin . "全英"))
  (let ((mode (car item))
        (expected (cdr item)))
    (nskk-e2e-with-buffer mode nil
      (let ((indicator (nskk-modeline-indicator)))
        (and (stringp indicator)
             (string-match-p (regexp-quote expected) indicator))))))

(nskk-property-test-exhaustive modeline-help-echo
  '(ascii hiragana katakana abbrev jisx0208-latin)
  (nskk-e2e-with-buffer item nil
    (let* ((indicator (nskk-modeline-indicator))
           (help (get-text-property 0 'help-echo indicator)))
      (stringp help))))

(provide 'nskk-modeline-e2e-test)

;;; nskk-modeline-e2e-test.el ends here
