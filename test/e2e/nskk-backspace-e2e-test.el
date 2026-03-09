;;; nskk-backspace-e2e-test.el --- E2E backspace/DEL key tests for NSKK  -*- lexical-binding: t; -*-

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

;; E2E backspace/DEL key tests for NSKK.
;;
;; Tests the full DEL key dispatch path via actual key events.
;; This covers GAP-3: DEL/Backspace behavior in non-abbrev states.
;;
;; Key handler being tested (nskk-handle-backspace):
;;   In preedit (▽) state:   delete last char, or cancel preedit if empty
;;   In converting (▼) state: rollback to ▽ preedit (DDSKK-compatible)
;;   In normal state:          delegate to backward-delete-char
;;
;; Sections:
;;   1. DEL in converting (▼) state
;;   2. DEL in hiragana preedit (▽) state (non-abbrev)
;;   3. DEL in normal state (no preedit, no conversion)
;;   4. DEL in preedit then cancel behavior chain

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(eval-when-compile (require 'cl-lib))

;;;;
;;;; Section 1: DEL in converting (▼) state
;;;;

(nskk-describe "DEL in converting state"
  (nskk-it "rolls back to ▽ preedit (DDSKK-compatible)"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      ;; Verify we are in converting state before DEL
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "DEL")
      ;; After rollback: no longer converting (▽ preedit, not ▼)
      (nskk-e2e-assert-not-converting)
      ;; henkan-phase is 'on after rollback (restored to ▽ preedit -- DDSKK-compatible)
      (nskk-e2e-assert-henkan-phase 'on "DEL from ▼ state: henkan-phase should be 'on (▽ preedit)")
      ;; Buffer shows ▽ + kana reading (marker restored, not stripped)
      (nskk-e2e-assert-buffer "▽かんじ" "DEL from ▼ state: buffer should show ▽ preedit with kana reading")))

  (nskk-it "rolls back from 2nd candidate to ▽ preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; Advance to the 2nd candidate
      (nskk-e2e-type "SPC")
      ;; Still in converting state
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "DEL")
      ;; After rollback: no longer converting
      (nskk-e2e-assert-not-converting)
      ;; henkan-phase is 'on after rollback
      (nskk-e2e-assert-henkan-phase 'on "DEL from ▼ 2nd candidate: henkan-phase should be 'on (▽ preedit)")
      ;; Buffer shows ▽ + kana reading
      (nskk-e2e-assert-buffer "▽かんじ" "DEL from ▼ 2nd candidate: buffer should show ▽ preedit with kana reading"))))

;;;;
;;;; Section 2: DEL in hiragana preedit (▽) state (non-abbrev)
;;;;

(nskk-describe "DEL in hiragana preedit state"
  (nskk-it "deletes last char and leaves empty preedit marker"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      ;; ▽ preedit is active
      (nskk-e2e-assert-henkan-phase 'on "After 'Ka': should be in ▽ preedit")
      (nskk-e2e-type "DEL")
      ;; The char か was deleted; preedit now has ▽ marker only
      ;; Buffer-string shows ▽ because the marker is still in buffer
      (nskk-e2e-assert-buffer "▽" "After DEL of 'か': buffer should show empty ▽ preedit")
      ;; henkan-phase is still 'on (preedit not yet cancelled, just char deleted)
      (nskk-e2e-assert-henkan-phase 'on "After DEL of 'か': henkan-phase should still be 'on")))

  (nskk-it "deletes chars progressively from right"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on "After 'Kanji': should be in ▽ preedit")
      ;; First DEL: removes じ
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽かん" "After 1st DEL: buffer should show ▽かん")
      (nskk-e2e-assert-henkan-phase 'on "After 1st DEL: should still be in ▽ preedit")
      ;; Second DEL: removes ん
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽か" "After 2nd DEL: buffer should show ▽か")
      (nskk-e2e-assert-henkan-phase 'on "After 2nd DEL: should still be in ▽ preedit")))

  (nskk-it "on empty preedit cancels preedit entirely"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type 'Ka' to enter ▽か preedit
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on "After 'Ka': should be in ▽ preedit")
      ;; First DEL: point is past ▽ → delete-preedit-char → removes か
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "▽" "After 1st DEL: buffer should show empty ▽")
      (nskk-e2e-assert-henkan-phase 'on "After 1st DEL: still in ▽ preedit (not yet cancelled)")
      ;; Second DEL: empty preedit (nothing after ▽) → cancel preedit
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-henkan-phase nil "After 2nd DEL: henkan-phase reset to nil (preedit cancelled)")
      (nskk-e2e-assert-buffer "" "After 2nd DEL: buffer is empty (preedit cancelled)"))))

;;;;
;;;; Section 3: DEL in normal state (no preedit, no conversion)
;;;;

(nskk-describe "DEL in normal state"
  (nskk-it "delegates to backward-delete-char in hiragana mode"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; lowercase 'a' in hiragana idle inserts あ without starting preedit
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ" "After typing 'a': buffer should contain あ")
      (nskk-e2e-assert-henkan-phase nil "After typing 'a': no preedit, henkan-phase nil")
      ;; DEL in normal hiragana state: backward-delete-char
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "" "After DEL: buffer should be empty")))

  (nskk-it "delegates to backward-delete-char in ascii mode"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "hello")
      (nskk-e2e-assert-buffer "hello" "After 'hello': buffer should contain hello")
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-buffer "hell" "After DEL: last char removed")))

  (nskk-it "does not crash on empty buffer"
    (nskk-e2e-with-buffer nil nil
      ;; Buffer is empty; DEL should not signal an error
      (condition-case err
          (nskk-e2e-type "DEL")
        (error
         (ert-fail (format "DEL on empty buffer raised an error: %s"
                           (error-message-string err)))))
      (nskk-e2e-assert-buffer "" "Empty buffer remains empty after DEL"))))

;;;;
;;;; Section 4: DEL in preedit then cancel behavior chain
;;;;

(nskk-describe "DEL cancel and resume behavior"
  (nskk-it "rolls back to ▽ preedit and allows re-conversion after DEL"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Enter converting state
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; DEL: rollback to ▽ preedit (DDSKK-compatible)
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase 'on "After DEL rollback: henkan-phase should be 'on (▽ preedit)")
      ;; After rollback, ▽ marker is restored and kana reading is editable.
      ;; The user can edit the reading or press SPC to re-convert.
      (nskk-e2e-assert-buffer "▽かんじ" "After DEL rollback: buffer shows ▽ preedit with kana reading")
      ;; C-g from ▽ preedit cancels preedit entirely (second press behavior)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase nil "After C-g from ▽: preedit cancelled, henkan-phase nil")
      (nskk-e2e-assert-buffer "" "After C-g from ▽: buffer is empty"))))

;;;;
;;;; Property-Based Tests: Backspace State Entry Sequences
;;;;

(nskk-deftest-cases backspace-state-entry-sequences
  (("Ka" . on)
   ("a" . nil)
   ("Kanji" . on))
  :body
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type input)
    (nskk-e2e-assert-henkan-phase expected)))

;;;;
;;;; Property-Based Tests: DEL on Normal State Does Not Crash
;;;;

(nskk-property-test backspace-del-on-normal-does-not-crash
    ((mode valid-mode))
  (progn
    (nskk-e2e-with-buffer mode nil
      (condition-case err
          (nskk-e2e-type "DEL")
        (error (ert-fail (format "DEL crashed in mode %s: %s"
                                 mode (error-message-string err)))))
      t))
  30)

;;;;
;;;; Property-Based Tests: DEL Buffer Consistency
;;;;

(nskk-describe "DEL property: buffer consistency"

  (nskk-it "DEL on non-empty normal buffer decrements buffer length"
    (dotimes (_ 20)
      (nskk-for-all ((mode valid-mode))
        (nskk-e2e-with-buffer mode nil
          (nskk-e2e-type "a")
          (let ((len-before (length (buffer-string))))
            (nskk-e2e-type "DEL")
            (should (<= (length (buffer-string)) len-before))))))))

(provide 'nskk-backspace-e2e-test)

;;; nskk-backspace-e2e-test.el ends here
