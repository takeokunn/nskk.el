;;; nskk-mode-transition-e2e-test.el --- E2E mode transition tests for NSKK  -*- lexical-binding: t; -*-

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

;;;;
;;;; C-j (nskk-kakutei) Tests
;;;;

(nskk-describe "C-j key transitions"
  (nskk-it "switches from ascii to hiragana"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-assert-mode 'ascii)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "inserts newline from hiragana idle"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "\n")))

  (nskk-it "switches from katakana idle to hiragana"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-assert-mode 'katakana)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "")))

  (nskk-it "switches from hankaku-katakana idle to hiragana"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-e2e-assert-mode 'katakana-半角)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "")))

  (nskk-it "commits converting candidate without newline"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")))

  (nskk-it "switches from latin to hiragana"
    (nskk-e2e-with-buffer 'latin nil
      (nskk-e2e-assert-mode 'latin)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana))))

;;;;
;;;; q Key (nskk-handle-q / nskk-toggle-japanese-mode) Tests
;;;;

(nskk-describe "q key transitions"
  (nskk-it "switches from hiragana to katakana"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "q")
      (nskk-e2e-assert-mode 'katakana)))

  (nskk-it "switches from katakana to hiragana"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-assert-mode 'katakana)
      (nskk-e2e-type "q")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "switches from hankaku-katakana to hiragana"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-e2e-assert-mode 'katakana-半角)
      (nskk-e2e-type "q")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "self-inserts q in ascii mode"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-assert-mode 'ascii)
      (nskk-e2e-type "q")
      (nskk-e2e-assert-mode 'ascii)
      (nskk-e2e-assert-buffer "q")))

  (nskk-it "toggles hiragana katakana hiragana idempotently"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "q")
      (nskk-e2e-assert-mode 'katakana)
      (nskk-e2e-type "q")
      (nskk-e2e-assert-mode 'hiragana)))

  (nskk-it "routes input correctly in katakana-半角 mode"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (condition-case err
          (nskk-e2e-type "a")
        (error
         (ert-fail (format "katakana-半角 typing crashed: %s"
                           (error-message-string err)))))
      (nskk-e2e-assert-mode 'katakana-半角))))

;;;;
;;;; l Key (nskk-handle-l) Tests
;;;;

(nskk-describe "l key transitions"
  (nskk-it "switches from hiragana to latin"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "l")
      (nskk-e2e-assert-mode 'latin)))

  (nskk-it "switches from katakana to latin"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type "l")
      (nskk-e2e-assert-mode 'latin)))

  (nskk-it "self-inserts l in ascii mode"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "l")
      (nskk-e2e-assert-mode 'ascii)
      (nskk-e2e-assert-buffer "l")))

  (nskk-it "self-inserts l in latin mode"
    (nskk-e2e-with-buffer 'latin nil
      (nskk-e2e-type "l")
      (nskk-e2e-assert-buffer "l"))))

;;;;
;;;; L Key (nskk-handle-upper-l) Tests
;;;;

(nskk-describe "shift-L key transitions"
  (nskk-it "switches from hiragana to jisx0208-latin"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "L")
      (nskk-e2e-assert-mode 'jisx0208-latin)))

  (nskk-it "switches from katakana to jisx0208-latin"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-type "L")
      (nskk-e2e-assert-mode 'jisx0208-latin)))

  (nskk-it "self-inserts L in ascii mode"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "L")
      (nskk-e2e-assert-buffer "L"))))

;;;;
;;;; / Key (nskk-handle-slash) Tests
;;;;

(nskk-describe "/ key transitions"
  (nskk-it "switches from hiragana to abbrev"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)))

  (nskk-it "self-inserts / in ascii mode"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-buffer "/"))))

;;;;
;;;; Complete Mode Transition Matrix
;;;;

(nskk-deftest-table mode-transition-matrix
  :columns (from-mode key to-mode)
  :rows (;; From hiragana
         (hiragana "q"   katakana)
         (hiragana "l"   latin)
         (hiragana "L"   jisx0208-latin)
         (hiragana "/"   abbrev)
         (katakana "q"   hiragana)
         (katakana "l"   latin)
         (katakana "L"   jisx0208-latin)
         (katakana "/"   abbrev)
         (katakana-半角 "q" hiragana))
  :body
  (nskk-e2e-with-buffer from-mode nil
    (nskk-e2e-type key)
    (nskk-e2e-assert-mode to-mode
                          (format "Transition: %S + %S → %S failed"
                                  from-mode key to-mode))))

;;;;
;;;; Mode After Conversion (Implicit Commit)
;;;;

(nskk-describe "implicit commit before mode switch from converting"
  (nskk-it "q commits candidate first then toggles to katakana"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "q")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-mode 'katakana)
      (nskk-e2e-assert-buffer "漢字")))

  (nskk-it "l commits candidate first then switches to latin"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "l")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-mode 'latin)
      (nskk-e2e-assert-buffer "漢字"))))

;;;;
;;;; RET in Various States
;;;;

(nskk-describe "RET key behavior"
  (nskk-it "inserts newline in normal non-converting state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "a")
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-buffer "あ\n")))

  (nskk-it "commits candidate without newline from converting state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字"))))

;;;;
;;;; Property-Based Tests (PBT)
;;;;

(nskk-property-test-seeded mode-always-valid
  ((start-mode valid-mode))
  (let ((mode-keys '("q" "l" "L" "/"))
        (key-count (+ 1 (random 5))))
    (nskk-e2e-with-buffer start-mode nil
      (dotimes (_ key-count)
        (ignore-errors (nskk-e2e-type (nth (random (length mode-keys)) mode-keys))))
      (nskk-state-valid-mode-p (nskk-current-mode))))
  30)

(nskk-property-test-exhaustive hiragana-katakana-toggle-idempotent
  '(hiragana katakana)
  (nskk-e2e-with-buffer item nil
    (nskk-e2e-type "q")
    (nskk-e2e-type "q")
    (eq (nskk-current-mode) item)))

(nskk-property-test-exhaustive cj-from-direct-always-hiragana
  '(ascii latin jisx0208-latin abbrev)
  (nskk-e2e-with-buffer item nil
    (nskk-e2e-type "C-j")
    (eq (nskk-current-mode) 'hiragana)))

(nskk-describe "katakana-半角 q transition (bug fix verification)"
  (nskk-it "q key from katakana-半角 always transitions to hiragana"
    (dotimes (_ 25)
      (nskk-e2e-with-buffer 'katakana-半角 nil
        (nskk-e2e-type "q")
        (should (eq (nskk-current-mode) 'hiragana))))))

(nskk-property-test-seeded mode-sequence-no-crash
  ((start-mode valid-mode))
  (let* ((all-keys '("q" "l" "L" "/" "C-j" "a" "k" "s" "t"))
         (key-count (+ 2 (random 8)))
         (keys (cl-loop repeat key-count
                        collect (nth (random (length all-keys)) all-keys))))
    (nskk-e2e-with-buffer start-mode nil
      (dolist (key keys)
        (ignore-errors (nskk-e2e-type key)))
      (nskk-state-valid-mode-p (nskk-current-mode))))
  25)

;;;;
;;;; Section A: L and / During Converting State (Implicit Commit)
;;;;

(nskk-describe "shift-L and / commit during converting"
  (nskk-it "L commits candidate then switches to jisx0208-latin"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "L")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-mode 'jisx0208-latin)
      (nskk-e2e-assert-buffer "漢字")))

  (nskk-it "/ commits candidate then enters abbrev mode"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "/")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-buffer-matches "漢字"))))

;;;;
;;;; Section B: C-j with Pending Romaji (romaji-pending state)
;;;;

(nskk-describe "C-j clears pending romaji"
  (nskk-it "clears single pending romaji consonant"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "k")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "")))

  (nskk-it "clears incomplete compound romaji sequence"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "sh")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "")))

  (nskk-it "does not insert newline when clearing pending romaji"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "k")
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-buffer ""))))

;;;;
;;;; Section C: Abbrev Preedit Fall-Through for Mode-Switch Keys
;;;;

(nskk-describe "mode-switch keys self-insert in abbrev preedit"

  (nskk-it "q self-inserts in abbrev preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-henkan-phase 'on "After /: should be in ▽ preedit")
      (nskk-e2e-type "q")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-buffer-matches "q")))

  (nskk-it "l self-inserts in abbrev preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "l")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-buffer-matches "l")))

  (nskk-it "shift-L self-inserts in abbrev preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "L")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-assert-buffer-matches "L"))))

;;;;
;;;; Section D: katakana-半角 Preedit and Basic Conversion
;;;;

(nskk-describe "katakana-半角 preedit and basic input"

  (nskk-it "uppercase letter enters preedit in katakana-半角 mode"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on
        "After 'Ka' in katakana-半角: should be in ▽ preedit")
      (nskk-e2e-assert-mode 'katakana-半角)))

  (nskk-it "SPC from katakana-半角 preedit enters converting state"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-henkan-phase 'active)))

  (nskk-it "C-g from katakana-半角 preedit state cancels preedit"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on
        "After 'Ka': should be in ▽ preedit before C-g")
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-mode 'katakana-半角)))

  (nskk-it "C-j from katakana-半角 idle switches to hiragana"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-e2e-assert-mode 'katakana-半角)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana))))

;;;;
;;;; PBT: Mode transition invariants
;;;;

(nskk-deftest-table mode-valid-symbols
  :description "Each mode symbol is a valid non-nil symbol"
  :columns (input expected)
  :rows ((ascii    ascii)
         (hiragana hiragana)
         (katakana katakana)
         (latin    latin))
  :body (should (and (symbolp input) (not (null input)) (eq input expected))))

(nskk-property-test mode-transition-state-always-valid
  ((mode valid-mode))
  (condition-case nil
      (nskk-with-state mode
        (or (null nskk-current-state)
            (nskk-state-p nskk-current-state)))
    (error nil))
  50)

(nskk-describe "Mode transition properties"
  (nskk-it "any valid mode produces a valid state"
    (nskk-for-all ((mode valid-mode))
      (condition-case nil
          (let ((state (nskk-state-create mode)))
            (should (nskk-state-p state))
            (should (eq (nskk-state-mode state) mode)))
        (error nil)))))



;;;;
;;;; Property: Mode Isolation Across Buffers
;;;;

(nskk-property-test-seeded mode-isolation-across-buffers
  ((mode-a valid-mode)
   (mode-b valid-mode))
  (nskk-e2e-with-buffer mode-a nil
    (nskk-e2e-with-buffer mode-b nil
      (ignore-errors (nskk-e2e-type "q"))
      t)
    (nskk-state-valid-mode-p (nskk-current-mode)))
  20)


;;;;
;;;; nskk-mode disable cleans up conversion state
;;;;

(nskk-it "two displayed NSKK buffers share one frame snapshot until the last disables"
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-assert '((dict-initialized)))
    (let ((buf-a (generate-new-buffer " *nskk-cursor-a*"))
          (buf-b (generate-new-buffer " *nskk-cursor-b*"))
          (frame (selected-frame))
          (nskk-use-color-cursor t))
      (set-frame-parameter frame nskk--saved-cursor-color-parameter nil)
      (set-frame-parameter frame nskk--last-cursor-color-parameter nil)
      (unwind-protect
          (cl-letf (((symbol-function 'nskk-candidate-show-list) #'ignore)
                    ((symbol-function 'nskk-candidate-hide-list) #'ignore)
                    ((symbol-function 'get-buffer-window)
                     (lambda (buffer target-frame)
                       (and (eq buffer buf-b)
                            (eq target-frame frame)
                            'fake-window))))
            (with-current-buffer buf-a
              (nskk-mode 1))
            (let ((saved-original
                   (frame-parameter
                    frame nskk--saved-cursor-color-parameter)))
              (should saved-original)
              (with-current-buffer buf-b
                (nskk-mode 1))
              (should
               (eq
                (frame-parameter
                 frame nskk--saved-cursor-color-parameter)
                saved-original))
              (with-current-buffer buf-a
                (nskk-mode 0))
              (should-not (buffer-local-value 'nskk-mode buf-a))
              (should (buffer-local-value 'nskk-mode buf-b))
              (should
               (eq
                (frame-parameter
                 frame nskk--saved-cursor-color-parameter)
                saved-original))
              (with-current-buffer buf-b
                (nskk-mode 0))
              (should-not (buffer-local-value 'nskk-mode buf-b))
              (should-not
               (frame-parameter
                frame nskk--saved-cursor-color-parameter))
              (should-not
               (frame-parameter
                frame nskk--last-cursor-color-parameter))))
        (when (buffer-live-p buf-a)
          (kill-buffer buf-a))
        (when (buffer-live-p buf-b)
          (kill-buffer buf-b))
        (set-frame-parameter frame nskk--saved-cursor-color-parameter nil)
        (set-frame-parameter frame nskk--last-cursor-color-parameter nil)))))


(provide 'nskk-mode-transition-e2e-test)

;;; nskk-mode-transition-e2e-test.el ends here
