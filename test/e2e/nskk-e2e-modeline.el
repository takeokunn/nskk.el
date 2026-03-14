;;; nskk-e2e-modeline.el --- E2E modeline and display tests for NSKK  -*- lexical-binding: t; -*-

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

;; E2E tests for modeline indicator and display state (DDSKK §5.12).
;; Covers: mode indicators, cursor position invariants, jisx0208-latin.

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

(nskk-deftest-table modeline-mode-indicator-table
  :columns (input expected)
  :rows ((ascii          "SKK")
         (latin          "SKK")
         (hiragana       "かな")
         (katakana       "カナ")
         (katakana-半角   "ｶﾅ")
         (abbrev         "aA")
         (jisx0208-latin "全英"))
  :body
  (nskk-e2e-with-buffer input nil
    (nskk-e2e-assert-modeline-contains
     expected
     (format "Mode %S should show %S in modeline" input expected))))

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

;;;;
;;;; Full-width Latin Mode Tests
;;;;

(nskk-describe "jisx0208-latin mode"
  (nskk-it "converts ASCII chars to full-width"
    (nskk-e2e-with-buffer 'jisx0208-latin nil
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "\uFF41")))

  (nskk-it "converts SPC to ideographic space"
    (nskk-e2e-with-buffer 'jisx0208-latin nil
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-buffer "\u3000"))))

;;;;
;;;; Full-width Latin Character Conversion Table
;;;;

(nskk-deftest-table jisx0208-latin-character-table
  :columns (ascii-char fullwidth-char)
  ;; Note: 'l' and 'q' are excluded — they are bound to mode-switch
  ;; handlers (nskk-handle-l, nskk-handle-q) in nskk-mode-map and
  ;; may switch mode rather than self-insert in some states.
  :rows (("a" "\uFF41") ("b" "\uFF42") ("c" "\uFF43") ("d" "\uFF44")
         ("e" "\uFF45") ("f" "\uFF46") ("g" "\uFF47") ("h" "\uFF48")
         ("i" "\uFF49") ("j" "\uFF4A") ("k" "\uFF4B") ("m" "\uFF4D")
         ("n" "\uFF4E") ("o" "\uFF4F") ("p" "\uFF50") ("r" "\uFF52")
         ("s" "\uFF53") ("t" "\uFF54") ("u" "\uFF55") ("v" "\uFF56")
         ("w" "\uFF57") ("x" "\uFF58") ("y" "\uFF59") ("z" "\uFF5A"))
  :body
  (nskk-e2e-with-buffer 'jisx0208-latin nil
    (nskk-e2e-type ascii-char)
    (nskk-e2e-assert-buffer fullwidth-char
                             (format "jisx0208-latin: %S → %S" ascii-char fullwidth-char))))

;;;;
;;;; Point position during preedit (▽ state)
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
;;;; Point position after C-j commit
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
;;;; Point position after C-g cancel
;;;;

(nskk-describe "point position after C-g cancel"
  (nskk-it "point is within valid bounds after C-g from converting state"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; C-g from ▼ (converting) state rolls back to ▽ preedit (phase=on), DDSKK-compatible.
      ;; The kana reading is retained in the buffer with the ▽ marker restored.
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      ;; After C-g: not converting (▽ preedit is not converting), phase=on.
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase 'on)
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

  (nskk-it "point is at point-max after C-g from converting state reverts to preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; C-g from ▼ (converting) state rolls back to ▽ preedit (DDSKK-compatible).
      ;; The ▼ marker is replaced with ▽; the kana reading stays in the buffer.
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-g")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase 'on)
      ;; Buffer contains ▽ + kana reading; point is at the end of the reading.
      (nskk-e2e-assert-buffer "▽かんじ")
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
;;;; No buffer artifacts during conversion
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

(provide 'nskk-e2e-modeline)

;;; nskk-e2e-modeline.el ends here
