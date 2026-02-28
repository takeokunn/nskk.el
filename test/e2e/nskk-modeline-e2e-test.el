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

(nskk-deftest-e2e modeline-ascii-shows-skk
  "ascii mode → modeline indicator contains 'SKK'."
  (nskk-e2e-with-buffer nil nil
    (nskk-e2e-assert-mode 'ascii)
    (nskk-e2e-assert-modeline-contains "SKK")))

(nskk-deftest-e2e modeline-latin-shows-skk
  "latin mode → modeline indicator contains 'SKK'."
  (nskk-e2e-with-buffer 'latin nil
    (nskk-e2e-assert-modeline-contains "SKK")))

(nskk-deftest-e2e modeline-hiragana-shows-kana
  "hiragana mode → modeline indicator contains 'かな'."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-assert-modeline-contains "かな")))

(nskk-deftest-e2e modeline-katakana-shows-kana
  "katakana mode → modeline indicator contains 'カナ'."
  (nskk-e2e-with-buffer 'katakana nil
    (nskk-e2e-assert-modeline-contains "カナ")))

(nskk-deftest-e2e modeline-katakana-hankaku-shows-hankaku
  "katakana-半角 mode → modeline indicator contains 'ｶﾅ'."
  (nskk-e2e-with-buffer 'katakana-半角 nil
    (nskk-e2e-assert-modeline-contains "ｶﾅ")))

(nskk-deftest-e2e modeline-abbrev-shows-aa
  "abbrev mode → modeline indicator contains 'aA'."
  (nskk-e2e-with-buffer 'abbrev nil
    (nskk-e2e-assert-modeline-contains "aA")))

(nskk-deftest-e2e modeline-jisx0208-shows-full
  "jisx0208-latin mode → modeline indicator contains '全英'."
  (nskk-e2e-with-buffer 'jisx0208-latin nil
    (nskk-e2e-assert-modeline-contains "全英")))

;;;;
;;;; Complete Mode-to-Indicator Table Test
;;;;

(nskk-deftest-e2e modeline-all-modes-table
  "All modes produce correct ddskk-compatible modeline strings."
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
           (format "Mode %S should show %S in modeline" mode expected)))))))

;;;;
;;;; Modeline Update on Mode Switch Tests
;;;;

(nskk-deftest-e2e modeline-updates-on-cj
  "Modeline updates when C-j switches ascii → hiragana."
  (nskk-e2e-with-buffer nil nil
    ;; Initially ascii
    (nskk-e2e-assert-modeline-contains "SKK")
    ;; Press C-j → enter hiragana
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-mode 'hiragana)
    ;; Modeline should now show hiragana indicator
    (nskk-e2e-assert-modeline-contains "かな")))

(nskk-deftest-e2e modeline-updates-on-q
  "Modeline updates when q switches hiragana → katakana."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-assert-modeline-contains "かな")
    (nskk-e2e-type "q")
    (nskk-e2e-assert-mode 'katakana)
    (nskk-e2e-assert-modeline-contains "カナ")))

(nskk-deftest-e2e modeline-updates-on-l
  "Modeline updates when l switches hiragana → latin."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-assert-modeline-contains "かな")
    (nskk-e2e-type "l")
    (nskk-e2e-assert-mode 'latin)
    (nskk-e2e-assert-modeline-contains "SKK")))

(nskk-deftest-e2e modeline-updates-on-upper-l
  "Modeline updates when L switches hiragana → jisx0208-latin."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-assert-modeline-contains "かな")
    (nskk-e2e-type "L")
    (nskk-e2e-assert-modeline-contains "全英")))

(nskk-deftest-e2e modeline-updates-on-slash
  "Modeline updates when / switches hiragana → abbrev."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-assert-modeline-contains "かな")
    (nskk-e2e-type "/")
    (nskk-e2e-assert-modeline-contains "aA")))

(nskk-deftest-e2e modeline-sequence-cj-q-l
  "Modeline tracks C-j → q → l transition sequence."
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
    (nskk-e2e-assert-modeline-contains "SKK")))

;;;;
;;;; Modeline Structure Tests
;;;;

(nskk-deftest-e2e modeline-returns-string
  "nskk-modeline-indicator always returns a string."
  (nskk-e2e-with-buffer 'hiragana nil
    (let ((indicator (nskk-modeline-indicator)))
      (should (stringp indicator)))))

(nskk-deftest-e2e modeline-propertized
  "nskk-modeline-indicator returns a propertized string (not plain text)."
  (nskk-e2e-with-buffer 'hiragana nil
    (let ((indicator (nskk-modeline-indicator)))
      ;; Should have face property
      (should (get-text-property 0 'face indicator)))))

(nskk-deftest-e2e modeline-non-empty-when-active
  "nskk-modeline-indicator is non-empty when nskk-mode is active."
  (nskk-e2e-with-buffer 'hiragana nil
    (let ((indicator (nskk-modeline-indicator)))
      (should (not (string-empty-p indicator))))))

(nskk-deftest-e2e modeline-empty-without-state
  "nskk-modeline-indicator returns \"\" when nskk-current-state is nil."
  ;; Outside nskk-mode context, modeline should be empty
  (let ((nskk-current-state nil))
    (should (equal (nskk-modeline-indicator) ""))))

;;;;
;;;; Cache Behavior Tests
;;;;

(nskk-deftest-e2e modeline-cache-invalidated-on-mode-change
  "Modeline cache is invalidated when mode changes."
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

(nskk-deftest-e2e modeline-cache-keyed-by-mode
  "Modeline cache: same mode symbol returns same text."
  (nskk-e2e-with-buffer 'hiragana nil
    ;; Call indicator multiple times for the same mode
    (let ((results (cl-loop repeat 5 collect (nskk-modeline-indicator))))
      ;; All should contain "かな"
      (dolist (r results)
        (should (string-match-p "かな" r))))))

;;;;
;;;; Property-Based Tests (PBT)
;;;;

(ert-deftest nskk-e2e-pbt-modeline-always-string ()
  "PBT: nskk-modeline-indicator always returns a string in any mode."
  (let ((all-modes '(ascii latin hiragana katakana katakana-半角 abbrev jisx0208-latin))
        (errors nil))
    (dolist (mode all-modes)
      (condition-case err
          (nskk-e2e-with-buffer mode nil
            (let ((indicator (nskk-modeline-indicator)))
              (unless (stringp indicator)
                (push (list :mode mode :result indicator) errors))))
        (error
         (push (list :mode mode :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT modeline-always-string: %d failures:\n%S"
                        (length errors) errors)))))

(ert-deftest nskk-e2e-pbt-modeline-non-empty-when-active ()
  "PBT: Modeline is non-empty for all valid modes when nskk-mode is active."
  (let ((all-modes '(ascii latin hiragana katakana katakana-半角 abbrev jisx0208-latin))
        (errors nil))
    (dolist (mode all-modes)
      (condition-case err
          (nskk-e2e-with-buffer mode nil
            (let ((indicator (nskk-modeline-indicator)))
              (when (or (not (stringp indicator)) (string-empty-p indicator))
                (push (list :mode mode :indicator indicator) errors))))
        (error
         (push (list :mode mode :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT modeline-non-empty: %d failures:\n%S"
                        (length errors) errors)))))

(ert-deftest nskk-e2e-pbt-modeline-changes-on-mode-switch ()
  "PBT: Modeline text changes when switching between distinct mode groups."
  ;; Japanese modes show different strings than direct modes
  (let ((japanese-modes '(hiragana katakana))
        (direct-modes '(ascii latin))
        (errors nil))
    (dolist (jmode japanese-modes)
      (dolist (dmode direct-modes)
        (condition-case err
            (nskk-e2e-with-buffer jmode nil
              (let ((jp-indicator (nskk-modeline-indicator)))
                ;; Switch to direct mode
                (nskk--set-mode dmode)
                (nskk-modeline-update)
                (let ((direct-indicator (nskk-modeline-indicator)))
                  ;; They should be different
                  (when (equal jp-indicator direct-indicator)
                    (push (list :from jmode :to dmode
                                :indicator jp-indicator)
                          errors)))))
          (error
           (push (list :modes (list jmode dmode)
                       :error (error-message-string err))
                 errors)))))
    (when errors
      (ert-fail (format "E2E PBT modeline-changes-on-switch: %d failures:\n%S"
                        (length errors) errors)))))

(ert-deftest nskk-e2e-pbt-modeline-ddskk-compatible-strings ()
  "PBT: Verify all mode indicators are ddskk-compatible strings."
  (let ((expected-map '((ascii         . "SKK")
                        (latin         . "SKK")
                        (hiragana      . "かな")
                        (katakana      . "カナ")
                        (katakana-半角  . "ｶﾅ")
                        (abbrev        . "aA")
                        (jisx0208-latin . "全英")))
        (errors nil))
    (dolist (tc expected-map)
      (let ((mode (car tc))
            (expected (cdr tc)))
        (condition-case err
            (nskk-e2e-with-buffer mode nil
              (let ((indicator (nskk-modeline-indicator)))
                (unless (and (stringp indicator)
                             (string-match-p (regexp-quote expected) indicator))
                  (push (list :mode mode
                              :expected expected
                              :actual indicator)
                        errors))))
          (error
           (push (list :mode mode :error (error-message-string err)) errors)))))
    (when errors
      (ert-fail (format "E2E PBT modeline-ddskk-compatible: %d failures:\n%S"
                        (length errors) errors)))))

(ert-deftest nskk-e2e-pbt-modeline-help-echo ()
  "PBT: Modeline indicator has help-echo text property."
  (let ((all-modes '(ascii hiragana katakana abbrev jisx0208-latin))
        (errors nil))
    (dolist (mode all-modes)
      (condition-case err
          (nskk-e2e-with-buffer mode nil
            (let* ((indicator (nskk-modeline-indicator))
                   (help (get-text-property 0 'help-echo indicator)))
              (unless (stringp help)
                (push (list :mode mode :help help) errors))))
        (error
         (push (list :mode mode :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT modeline-help-echo: %d failures:\n%S"
                        (length errors) errors)))))

(provide 'nskk-modeline-e2e-test)

;;; nskk-modeline-e2e-test.el ends here
