;;; nskk-navigation-e2e-test.el --- E2E navigation key tests for NSKK  -*- lexical-binding: t; -*-

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

;; E2E navigation key tests for NSKK.
;;
;; Tests C-f (nskk-handle-ctrl-f) and C-b (nskk-handle-ctrl-b) via
;; actual key dispatch, covering all three dispatch states:
;;   converting (▼) -- kakutei-then-forward / kakutei-then-backward
;;   preedit (▽)    -- plain forward-char / backward-char
;;   normal          -- plain forward-char / backward-char
;;
;; Sections:
;;   1. C-f in converting (▼) state -- kakutei-then-forward
;;   2. C-b in converting (▼) state -- kakutei-then-backward
;;   3. C-f in preedit and normal states -- plain forward-char
;;   4. C-b in preedit and normal states -- plain backward-char
;;   5. C-f / C-b sequence tests
;;   6. C-a (beginning-of-line) -- kakutei-then-bol in converting, plain in others
;;   7. C-e (end-of-line) -- kakutei-then-eol in converting

;;; Code:

(require 'ert)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(eval-when-compile (require 'cl-lib))

;;;;
;;;; Section 1: C-f in converting (▼) state -- kakutei-then-forward
;;;;

(nskk-describe "C-f in converting state"
  (nskk-it "commits candidate and moves point forward"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type "Kanji" → enter ▽ preedit (かんじ), then SPC → enter ▼ converting
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; C-f: kakutei-then-forward -- commit 漢字, then forward-char
      (nskk-e2e-type "C-f")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "漢字")
      ;; nskk-commit-current replaces ▼かんじ (pos 1-5) with 漢字 (pos 1-3).
      ;; forward-char at end-of-buffer is silently swallowed; point stays at end.
      (should (= (point) (point-max)))))

  (nskk-it "leaves point at end of buffer after commit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-f")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")
      ;; After committing "漢字" (2 chars) and calling forward-char at end-of-buffer,
      ;; point should be at point-max (end-of-buffer is silently swallowed)
      (should (= (point) (point-max)))))

  (nskk-it "commits the second candidate when on 2nd candidate"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; SPC → first candidate (漢字), SPC again → second candidate (感じ)
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      ;; Now on second candidate
      (nskk-e2e-assert-converting)
      ;; C-f commits whichever candidate is current
      (nskk-e2e-type "C-f")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "感じ"))))

;;;;
;;;; Section 2: C-b in converting (▼) state -- kakutei-then-backward
;;;;

(nskk-describe "C-b in converting state"
  (nskk-it "commits candidate and moves point backward"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Insert あ first so there is a character before the conversion
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ")
      ;; Now enter conversion: かんじ → ▼ converting
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (let ((point-before (point)))
        ;; C-b: kakutei-then-backward -- commit 漢字, then backward-char
        (nskk-e2e-type "C-b")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase nil)
        (nskk-e2e-assert-buffer "あ漢字")
        ;; Point must have moved backward after commit
        (should (< (point) point-before)))))

  (nskk-it "moves point one char back from end after commit"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Commit "漢字" via C-b; buffer will be "漢字", point moves back one char
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-b")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")
      ;; "漢字" is 2 chars; point-max = 3.  backward-char from 3 → point = 2.
      ;; So point should be strictly less than point-max (moved back from end).
      (should (< (point) (point-max))))))

;;;;
;;;; Section 3: C-f in preedit and normal states -- plain forward-char
;;;;

(nskk-describe "C-f in preedit and normal state"
  (nskk-it "is plain forward-char in normal hiragana state"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type two kana characters: あい
      (nskk-e2e-type "a")
      (nskk-e2e-type "i")
      (nskk-e2e-assert-buffer "あい")
      ;; Move to beginning
      (goto-char (point-min))
      (should (= (point) 1))
      ;; C-f should move forward one character
      (nskk-e2e-type "C-f")
      (should (= (point) 2))
      ;; Buffer is unchanged
      (nskk-e2e-assert-buffer "あい")))

  (nskk-it "is plain forward-char in ascii mode"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "abc")
      (goto-char (point-min))
      (nskk-e2e-type "C-f")
      (should (= (point) 2))))

  (nskk-it "does not signal error at end of buffer"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "a")
      ;; Point is now at end (position 2)
      (should (= (point) 2))
      ;; C-f at end-of-buffer: condition-case swallows the error
      (nskk-e2e-type "C-f")
      ;; Point must not have moved beyond end
      (should (= (point) 2)))))

;;;;
;;;; Section 4: C-b in preedit and normal states -- plain backward-char
;;;;

(nskk-describe "C-b in preedit and normal state"
  (nskk-it "moves point backward one character in normal hiragana state"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Insert あい; point is at end (position 3)
      (nskk-e2e-type "a")
      (nskk-e2e-type "i")
      (nskk-e2e-assert-buffer "あい")
      ;; C-b should move backward one character
      (nskk-e2e-type "C-b")
      (should (= (point) 2))
      ;; Buffer is unchanged
      (nskk-e2e-assert-buffer "あい")))

  (nskk-it "does not signal error at beginning of buffer"
    (nskk-e2e-with-buffer nil nil
      ;; Empty buffer; point is at position 1 (beginning)
      (should (= (point) 1))
      ;; C-b at beginning: condition-case swallows the error
      (nskk-e2e-type "C-b")
      ;; Buffer still empty, point still at 1
      (nskk-e2e-assert-buffer "")
      (should (= (point) 1)))))

;;;;
;;;; Section 5: C-f / C-b sequence tests
;;;;

(nskk-describe "C-f and C-b sequences"
  (nskk-it "commit by C-f then C-b in normal state moves point backward"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Enter ▼ converting
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; C-f: commits 漢字, moves point forward (stays at end-of-buffer)
      (nskk-e2e-type "C-f")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")
      ;; Now in normal state: C-b is plain backward-char
      (nskk-e2e-type "C-b")
      ;; Buffer unchanged
      (nskk-e2e-assert-buffer "漢字"))))

;;;;
;;;; Section 6: C-a in converting (▼) state -- kakutei-then-bol
;;;;

(nskk-describe "C-a (beginning-of-line) in converting state"
  (nskk-it "commits candidate and moves to beginning of line"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type "Kanji" → enter ▽ preedit (かんじ), then SPC → enter ▼ converting
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; C-a: kakutei-then-bol -- commit 漢字, then beginning-of-line
      (nskk-e2e-type "C-a")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")
      ;; After commit, beginning-of-line moves point to start of line
      (should (= (point) (line-beginning-position)))))

  (nskk-it "commits preedit and moves to beginning of line"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type "Kanji" → enter ▽ preedit (かんじ), do NOT press SPC
      (nskk-e2e-type "Kanji")
      ;; Still in preedit, not converting
      (nskk-e2e-assert-henkan-phase 'on)
      ;; C-a in preedit state: kakutei-then-bol -- commit かんじ, then BOL
      (nskk-e2e-type "C-a")
      ;; preedit must have been committed -- henkan-phase is now nil
      (nskk-e2e-assert-henkan-phase nil)
      ;; reading is now in buffer as committed kana
      (nskk-e2e-assert-buffer "かんじ")
      ;; point is at beginning of line after commit
      (should (= (point) (line-beginning-position)))))

  (nskk-it "is plain beginning-of-line in normal hiragana state"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; No preedit; just press C-a -- should not crash
      (nskk-e2e-type "C-a")
      ;; Point must be at beginning of line
      (should (= (point) (line-beginning-position))))))

;;;;
;;;; Section 7: C-e in converting (▼) state -- kakutei-then-eol
;;;;

(nskk-describe "C-e (end-of-line) in converting state"
  (nskk-it "commits candidate and moves to end of line"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type "Kanji" → enter ▽ preedit (かんじ), then SPC → enter ▼ converting
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      ;; C-e: kakutei-then-eol -- commit 漢字, then end-of-line
      (nskk-e2e-type "C-e")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")
      ;; After commit, end-of-line moves point to end of line
      (should (= (point) (line-end-position))))))

;;;;
;;;; Section 8: C-e in preedit (▽) state -- plain end-of-line
;;;;

(nskk-describe "C-e in preedit state"
  (nskk-it "commits preedit and moves to end of line"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type "Kanji" → enter ▽ preedit (かんじ), do NOT press SPC
      (nskk-e2e-type "Kanji")
      ;; Still in preedit, not converting
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-not-converting)
      ;; C-e in preedit state: kakutei-then-eol -- commit かんじ, then EOL
      (nskk-e2e-type "C-e")
      ;; preedit must have been committed -- henkan-phase is now nil
      (nskk-e2e-assert-henkan-phase nil)
      ;; reading is now in buffer as committed kana
      (nskk-e2e-assert-buffer "かんじ")
      ;; point is at end of line after commit
      (should (= (point) (point-max)))))

  (nskk-it "commits preedit and leaves point at end of line"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Type "Ka" to start ▽ preedit (か)
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      ;; C-e: kakutei-then-eol -- commit か, then EOL
      (nskk-e2e-type "C-e")
      ;; preedit committed -- henkan-phase is now nil
      (nskk-e2e-assert-henkan-phase nil)
      ;; committed kana is in buffer
      (nskk-e2e-assert-buffer "か")
      ;; point is at end of line
      (should (= (point) (point-max))))))

;;;;
;;;; Section 9: C-e in normal state -- plain end-of-line
;;;;

(nskk-describe "C-e in normal state"
  (nskk-it "moves point to end of line in normal hiragana state"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Insert あいう; point is at end
      (nskk-e2e-type "a")
      (nskk-e2e-type "i")
      (nskk-e2e-type "u")
      (nskk-e2e-assert-buffer "あいう")
      ;; Move to beginning
      (goto-char (point-min))
      ;; C-e should move point to end of line
      (nskk-e2e-type "C-e")
      (should (= (point) (point-max)))
      ;; Buffer is unchanged
      (nskk-e2e-assert-buffer "あいう")
      ;; Not converting
      (nskk-e2e-assert-not-converting)))

  (nskk-it "moves point to end of line in ascii mode"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "abc")
      (nskk-e2e-assert-buffer "abc")
      ;; Move to beginning
      (goto-char (point-min))
      ;; C-e should move point to end of line
      (nskk-e2e-type "C-e")
      (should (= (point) (point-max)))
      ;; Buffer is unchanged
      (nskk-e2e-assert-buffer "abc")))

  (nskk-it "is a no-op at end of line in normal state"
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Insert あ; point is already at end
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ")
      ;; Point is already at end-of-line; C-e should keep it there
      (should (= (point) (point-max)))
      (nskk-e2e-type "C-e")
      (should (= (point) (point-max)))
      (nskk-e2e-assert-not-converting))))

;;;;
;;;; Property-Based Tests: Navigation Point Invariants
;;;;

(nskk-deftest-table navigation-point-invariants
  :columns (input _expected)
  :rows (("C-f" "forward")
         ("C-b" "backward"))
  :body
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "a")
    (nskk-e2e-type input)
    (should (>= (point) (point-min)))))

;;;;
;;;; Property-Based Tests: C-f in Normal Does Not Crash
;;;;

(nskk-property-test navigation-cf-in-normal-does-not-crash
    ((mode valid-mode))
  (progn
    (nskk-e2e-with-buffer mode nil
      (condition-case err
          (nskk-e2e-type "C-f")
        (error (ert-fail (format "C-f in mode %s raised error: %s"
                                 mode (error-message-string err)))))
      t))
  30)

;;;;
;;;; Property-Based Tests: Navigation Point Stability
;;;;

(nskk-describe "Navigation property: point stability"

  (nskk-it "C-f/C-b in any mode keeps point within buffer bounds"
    (dotimes (_ 25)
      (nskk-for-all ((mode valid-mode))
        (nskk-e2e-with-buffer mode nil
          (nskk-e2e-type "a")
          (nskk-e2e-type "C-f")
          (should (and (>= (point) (point-min)) (<= (point) (point-max))))
          (nskk-e2e-type "C-b")
          (should (and (>= (point) (point-min)) (<= (point) (point-max)))))))))

;;;;
;;;; Section 10: Unbound key implicit kakutei via post-command-handler
;;;;
;; These tests exercise the `nskk--post-command-handler` safety net path,
;; which commits conversion when an unbound command moves point away from
;; the overlay boundary.  Unlike sections 1–9 (which test bound keys that
;; call `nskk--commit-by-phase` explicitly), these simulate the interactive
;; command loop: pre-command-hook → command → post-command-hook.

(defmacro nskk-e2e--simulate-unbound-command (command)
  "Simulate running COMMAND as if from the interactive command loop.
Saves point in `nskk--point-before-command', runs COMMAND via
`call-interactively', then fires `nskk--post-command-handler'."
  (let ((cmd (gensym "cmd")))
    `(let ((,cmd ,command)
           (nskk--point-before-command (point)))
       (condition-case nil
           (call-interactively ,cmd)
         (error nil))
       (let ((this-command ,cmd))
         (nskk--post-command-handler)))))

(nskk-describe "unbound key implicit kakutei in converting state"
  (nskk-it "M-b commits candidate without residual text"
    (nskk-e2e-with-buffer 'hiragana nil
      (insert "test word ")
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e--simulate-unbound-command #'backward-word)
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "test word 漢字")))

  (nskk-it "M-f commits candidate when moving past overlay"
    (nskk-e2e-with-buffer 'hiragana nil
      (insert "before ")
      (save-excursion (insert " after"))
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e--simulate-unbound-command #'forward-word)
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "before 漢字 after")))

  (nskk-it "beginning-of-buffer into overlay commits correctly"
    (nskk-e2e-with-buffer 'hiragana nil
      (insert "a")
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e--simulate-unbound-command #'beginning-of-buffer)
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "a漢字"))))

(nskk-describe "unbound key implicit kakutei with okurigana"
  (nskk-it "M-b during okurigana conversion preserves kana suffix"
    (let ((dict '(("かk" . ("書")))))
      (nskk-e2e-with-buffer 'hiragana dict
        (insert "test ")
        (nskk-e2e-type "Ka")
        (nskk-e2e-type "K")
        (nskk-e2e-type "i")
        (nskk-e2e-assert-converting)
        ;; Point is past overlay-end (okurigana "き" sits after overlay).
        ;; M-b moves point backward — but since point was PAST overlay-end
        ;; before the command, the normal commit path (Branch B) fires via
        ;; the post-command-handler.
        (nskk-e2e--simulate-unbound-command #'backward-word)
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase nil)
        (nskk-e2e-assert-buffer "test 書き")))))

(nskk-describe "unbound key implicit kakutei in preedit state"
  (nskk-it "M-b commits kana as-is from preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (insert "test ")
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e--simulate-unbound-command #'backward-word)
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "test かんじ"))))

;;;;
;;;; Section 11: C-f/C-b in preedit-pending state (uppercase trigger, no kana yet)
;;;;
;; These tests verify the fix for Bug: preedit-pending → noop.
;; Before the fix, pressing C-f after typing only the uppercase trigger letter
;; (before completing the kana syllable) would leave the ▽ marker and
;; conversion-start marker alive while the cursor moved away.  Any subsequent
;; keypress would insert kana at the wrong position.
;;
;; After the fix (preedit-pending → henkan-kakutei), C-f cleans up correctly.

(nskk-describe "C-f in preedit-pending state"
  (nskk-it "cleans up henkan phase and marker when no kana emitted yet"
    ;; Type uppercase K but NOT the completing vowel → preedit-pending.
    ;; C-f must commit (remove ▽) and leave state idle.
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Dispatch K directly (uppercase triggers henkan-start).
      (nskk-e2e--dispatch-event ?K)
      ;; In preedit-pending: phase=on, conversion-start set, buf="k", no kana yet.
      (should (eq (nskk-state-henkan-phase nskk-current-state) 'on))
      ;; C-f (bound command) — triggers nskk-handle-ctrl-f.
      (nskk-e2e-type "C-f")
      ;; After fix: henkan-kakutei ran, state is clean.
      (nskk-e2e-assert-henkan-phase nil)
      (should (null (nskk--get-conversion-start)))
      ;; ▽ marker must be gone from the buffer.
      (should (not (string-search nskk-henkan-on-marker (buffer-string))))))

  (nskk-it "allows fresh preedit after cursor movement from preedit-pending"
    ;; After C-f cleans up preedit-pending, the next uppercase letter must start
    ;; a fresh preedit — not get normalize-vowel or broken stale state.
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e--dispatch-event ?K)  ; preedit-pending
      (nskk-e2e-type "C-f")          ; clean up via fix
      ;; Type a fresh preedit sequence: Ka → ▽か
      (nskk-e2e--dispatch-event ?K)
      (nskk-e2e--dispatch-event ?a)
      ;; Fresh preedit should be active with か in the buffer.
      (nskk-e2e-assert-henkan-phase 'on)
      (should (string-search "か" (buffer-string))))))

(nskk-describe "C-b in preedit-pending state"
  (nskk-it "cleans up henkan phase when moving backward before kana is emitted"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e--dispatch-event ?K)
      (should (eq (nskk-state-henkan-phase nskk-current-state) 'on))
      (nskk-e2e-type "C-b")
      (nskk-e2e-assert-henkan-phase nil)
      (should (null (nskk--get-conversion-start))))))

(provide 'nskk-navigation-e2e-test)

;;; nskk-navigation-e2e-test.el ends here
