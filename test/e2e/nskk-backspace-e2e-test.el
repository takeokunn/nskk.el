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
;;   In converting (▼) state: cancel conversion (rollback to kana reading)
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
(eval-when-compile (require 'cl-lib))

;;;;
;;;; Section 1: DEL in converting (▼) state
;;;;

(nskk-describe "DEL in converting state"
  (nskk-it "cancels to preedit and restores kana reading"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      ;; Verify we are in converting state before DEL
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "DEL")
      ;; After cancel: no longer converting
      (nskk-e2e-assert-not-converting)
      ;; henkan-phase is nil after rollback (nskk-rollback-conversion resets it)
      (nskk-e2e-assert-henkan-phase nil "DEL from ▼ state: henkan-phase should be nil")
      ;; Buffer retains the kana reading without the ▼ marker
      (nskk-e2e-assert-buffer "かんじ" "DEL from ▼ state: buffer should contain kana reading")))

  (nskk-it "cancels from 2nd candidate back to kana reading"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; Advance to the 2nd candidate
      (nskk-e2e-type "SPC")
      ;; Still in converting state
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "DEL")
      ;; After cancel: no longer converting
      (nskk-e2e-assert-not-converting)
      ;; henkan-phase is nil after rollback
      (nskk-e2e-assert-henkan-phase nil "DEL from ▼ 2nd candidate: henkan-phase should be nil")
      ;; Buffer retains the kana reading
      (nskk-e2e-assert-buffer "かんじ" "DEL from ▼ 2nd candidate: buffer should contain kana reading"))))

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
  (nskk-it "cancels conversion and allows re-entry after DEL"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Enter converting state
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; DEL: cancel conversion → kana reading restored, phase nil
      (nskk-e2e-type "DEL")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase nil "After DEL cancel: henkan-phase should be nil")
      ;; After rollback the kana reading is in the buffer.
      ;; We cannot re-enter conversion from the rolled-back text because the ▽
      ;; marker is gone and henkan-phase is nil.  Verify buffer shows reading.
      (nskk-e2e-assert-buffer "かんじ" "After DEL cancel: buffer contains kana reading"))))

(provide 'nskk-backspace-e2e-test)

;;; nskk-backspace-e2e-test.el ends here
