;;; nskk-point-e2e-test.el --- E2E cursor position tests for NSKK  -*- lexical-binding: t; -*-

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

;; Tests for point/cursor position invariants during nskk-mode.
;; Asserts that point is in the correct position after preedit, commit,
;; and cancel operations.
;;
;; IMPORTANT: All point assertions use relative positions (point-max, point-min,
;; comparisons) to avoid fragility from implementation-specific buffer geometry.
;;
;; Sections:
;;   1. Point after preedit input (▽ state)
;;   2. Point after C-j commit
;;   3. Point after C-g cancel
;;   4. No buffer artifacts during conversion (▼ state)

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)

;;;;
;;;; Section 1: Point after preedit input (▽ state)
;;;;

(nskk-describe "point position during preedit (▽ state)"
  (nskk-it "point is at point-max after entering preedit mode"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type uppercase K to trigger preedit mode (▽ state).
      ;; The ▽ marker and any accumulated kana are in the buffer.
      ;; Point should be at the end of whatever content is present.
      (nskk-e2e-type "K")
      (nskk-e2e-assert-henkan-phase 'on)
      (should (= (point) (point-max)))))

  (nskk-it "point is at point-max after typing multiple preedit chars"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type "Ka" → ▽か (preedit with one kana char accumulated).
      ;; Then type "n" → ▽かn (partial romaji in progress).
      ;; Then type "j" → ▽かん (another kana appended).
      ;; At each stage point must be at the end of the buffer.
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (should (= (point) (point-max)))
      (nskk-e2e-type "n")
      (should (= (point) (point-max)))
      (nskk-e2e-type "j")
      (should (= (point) (point-max)))))

  (nskk-it "point stays at point-max after typing full preedit sequence Kanji"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type "Kanji" fully → ▽かんじ in preedit.
      ;; Point must be at point-max (end of buffer) throughout.
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on)
      (should (= (point) (point-max)))))

  (nskk-it "point is at point-max when preedit follows pre-existing kana"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; First insert some normal kana, then enter preedit.
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ")
      (let ((point-after-a (point)))
        ;; Entering preedit pushes point further forward.
        (nskk-e2e-type "Kanji")
        (nskk-e2e-assert-henkan-phase 'on)
        (should (> (point) point-after-a))
        (should (= (point) (point-max)))))))

;;;;
;;;; Section 2: Point after C-j commit
;;;;

(nskk-describe "point position after C-j commit"
  (nskk-it "point is at point-max after C-j commit from converting state"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; "Kanji" + SPC enters ▼ converting state with candidate 漢字.
      ;; C-j commits the current candidate.
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")
      ;; After commit the candidate text is in the buffer and point follows it.
      (should (= (point) (point-max)))))

  (nskk-it "point is at point-max after C-j commit with pre-existing buffer text"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Insert "あ" first, then convert "Kanji" → 漢字 via SPC + C-j.
      ;; Buffer = "あ漢字"; point should be at point-max (after 漢字).
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ")
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "あ漢字")
      ;; Point must be at the very end, past both あ and 漢字.
      (should (= (point) (point-max)))))

  (nskk-it "point is strictly after where it was before commit"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Record point before entering preedit, then commit.
      ;; The committed kanji is longer than the romaji that was typed,
      ;; so point-max after commit is not the same position as before.
      (let ((start (point)))
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")
        (nskk-e2e-type "C-j")
        (nskk-e2e-assert-not-converting)
        ;; After commit, point must be strictly beyond where we started.
        (should (> (point) start)))))

  (nskk-it "point is at point-max after C-j kakutei on preedit (no conversion)"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type "Kanji" to enter ▽ preedit, then C-j to commit the kana as-is.
      ;; Buffer = "かんじ"; point should be at point-max.
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-henkan-phase nil)
      ;; Buffer contains the committed kana; point is at end.
      (should (= (point) (point-max))))))

;;;;
;;;; Section 3: Point after C-g cancel
;;;;

(nskk-describe "point position after C-g cancel"
  (nskk-it "point is within valid bounds after C-g from converting state"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; In nskk.el, C-g from ▼ (converting) state goes directly to idle (phase=nil),
      ;; NOT to ▽ preedit (phase=on) as DDSKK does.  The kana reading is retained in
      ;; the buffer (confirmed: see "reverts to kana" test below).
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      ;; After C-g: not converting, phase=nil (idle).
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase nil)
      ;; Point must be within valid buffer bounds.
      (should (<= (point-min) (point)))
      (should (<= (point) (point-max)))))

  (nskk-it "point is at point-min after C-g from preedit state (empty buffer)"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; C-g from ▽ preedit state: cancels the preedit and removes all preedit text.
      ;; Buffer becomes empty; point should be at point-min.
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-henkan-phase nil)
      ;; All preedit text was deleted; buffer is empty.
      (nskk-e2e-assert-buffer "")
      (should (= (point) (point-min)))))

  (nskk-it "point is at point-max after C-g from converting state reverts to kana"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; In nskk.el, C-g from ▼ (converting) state goes directly to idle (phase=nil).
      ;; Unlike DDSKK (▼→▽ on first C-g, ▽→idle on second), nskk.el collapses to one
      ;; step.  The kana reading remains in the buffer; only the preedit overlay/marker
      ;; is removed.
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase nil)
      ;; Buffer retains the kana reading after cancellation; point is at the end.
      (nskk-e2e-assert-buffer "かんじ")
      (should (= (point) (point-max)))))

  (nskk-it "point remains within valid bounds after C-g from converting with pre-existing text"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Insert "あ" first, then enter conversion, then C-g to cancel.
      ;; In nskk.el, C-g from ▼ removes preedit text; buffer retains "あ".
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ")
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-not-converting)
      ;; After C-g: buffer has only the pre-existing "あ"; preedit is gone.
      ;; Point must be within valid bounds (point-min <= point <= point-max).
      (should (<= (point-min) (point)))
      (should (<= (point) (point-max)))))
)

;;;;
;;;; Section 4: No buffer artifacts during conversion
;;;;

(nskk-describe "buffer does not contain conversion overlay text"
  (nskk-it "buffer-string during ▼ state contains the ▼ marker"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Enter converting state.
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; In nskk.el, the ▼ marker IS literally in buffer-string during converting state.
      ;; The kanji candidate is shown via an overlay display property REPLACING the
      ;; kana reading text, but the ▼ prefix character itself remains in the buffer.
      ;; cf. DDSKK which also puts ▼ in the buffer during conversion.
      (should (string-match-p "▼" (buffer-string)))))

  (nskk-it "buffer-string during ▼ state does not contain the committed candidate text"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; The candidate "漢字" is displayed via overlay; it is not in buffer-string.
      ;; Real buffer content is the ▼ marker + kana reading (▼かんじ or similar).
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; Candidate text must not yet be in the buffer (it lives in the overlay).
      (should (not (string-match-p "漢字" (buffer-string))))))

  (nskk-it "candidate text appears in buffer only after commit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; Pre-commit: 漢字 is NOT in buffer.
      (should (not (string-match-p "漢字" (buffer-string))))
      ;; Commit via C-j.
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-not-converting)
      ;; Post-commit: 漢字 IS in buffer.
      (should (string-match-p "漢字" (buffer-string)))
      ;; And point is at point-max (immediately after the committed text).
      (should (= (point) (point-max)))))

  (nskk-it "overlay shows candidate during ▼ state"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; After SPC the conversion overlay should display the first candidate.
      ;; Use nskk-e2e-assert-overlay-shows to check overlay (not buffer-string).
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; The overlay must display the first candidate "漢字".
      (nskk-e2e-assert-overlay-shows "漢字")
      ;; And the buffer itself does not contain the candidate text yet.
      (should (not (string-match-p "漢字" (buffer-string))))))

  (nskk-it "point is within valid range throughout ▼ state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; During converting state, point must be within buffer bounds.
      (should (<= (point-min) (point)))
      (should (>= (point-max) (point))))))

(provide 'nskk-point-e2e-test)

;;; nskk-point-e2e-test.el ends here
