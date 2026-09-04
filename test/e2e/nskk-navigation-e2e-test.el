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
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-f")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "漢字")
      (should (= (point) (point-max)))))

  (nskk-it "leaves point at end of buffer after commit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-f")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")
      (should (= (point) (point-max)))))

  (nskk-it "commits the second candidate when on 2nd candidate"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-f")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "感じ"))))

;;;;
;;;; Section 2: C-b in converting (▼) state -- kakutei-then-backward
;;;;

(nskk-describe "C-b in converting state"
  (nskk-it "commits candidate and moves point backward"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ")
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (let ((point-before (point)))
        (nskk-e2e-type "C-b")
        (nskk-e2e-assert-not-converting)
        (nskk-e2e-assert-henkan-phase nil)
        (nskk-e2e-assert-buffer "あ漢字")
        (should (< (point) point-before)))))

  (nskk-it "moves point one char back from end after commit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-type "C-b")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")
      (should (< (point) (point-max))))))

;;;;
;;;; Section 3: C-f in preedit and normal states -- plain forward-char
;;;;

(nskk-describe "C-f in preedit and normal state"
  (nskk-it "is plain forward-char in normal hiragana state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "a")
      (nskk-e2e-type "i")
      (nskk-e2e-assert-buffer "あい")
      (goto-char (point-min))
      (should (= (point) 1))
      (nskk-e2e-type "C-f")
      (should (= (point) 2))
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
      (should (= (point) 2))
      (nskk-e2e-type "C-f")
      (should (= (point) 2)))))

;;;;
;;;; Section 4: C-b in preedit and normal states -- plain backward-char
;;;;

(nskk-describe "C-b in preedit and normal state"
  (nskk-it "moves point backward one character in normal hiragana state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "a")
      (nskk-e2e-type "i")
      (nskk-e2e-assert-buffer "あい")
      (nskk-e2e-type "C-b")
      (should (= (point) 2))
      (nskk-e2e-assert-buffer "あい")))

  (nskk-it "does not signal error at beginning of buffer"
    (nskk-e2e-with-buffer nil nil
      (should (= (point) 1))
      (nskk-e2e-type "C-b")
      (nskk-e2e-assert-buffer "")
      (should (= (point) 1)))))

;;;;
;;;; Section 5: C-f / C-b sequence tests
;;;;

(nskk-describe "C-f and C-b sequences"
  (nskk-it "commit by C-f then C-b in normal state moves point backward"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-f")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")
      (nskk-e2e-type "C-b")
      (nskk-e2e-assert-buffer "漢字"))))

;;;;
;;;; Section 6: C-a in converting (▼) state -- kakutei-then-bol
;;;;

(nskk-describe "C-a (beginning-of-line) in converting state"
  (nskk-it "commits candidate and moves to beginning of line"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-a")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")
      (should (= (point) (line-beginning-position)))))

  (nskk-it "commits preedit and moves to beginning of line"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-a")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "かんじ")
      (should (= (point) (line-beginning-position)))))

  (nskk-it "is plain beginning-of-line in normal hiragana state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "C-a")
      (should (= (point) (line-beginning-position))))))

;;;;
;;;; Section 7: C-e in converting (▼) state -- kakutei-then-eol
;;;;

(nskk-describe "C-e (end-of-line) in converting state"
  (nskk-it "commits candidate and moves to end of line"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "C-e")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字")
      (should (= (point) (line-end-position))))))

;;;;
;;;; Section 8: C-e in preedit (▽) state -- plain end-of-line
;;;;

(nskk-describe "C-e in preedit state"
  (nskk-it "commits preedit and moves to end of line"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-type "C-e")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "かんじ")
      (should (= (point) (point-max)))))

  (nskk-it "commits preedit and leaves point at end of line"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Ka")
      (nskk-e2e-assert-henkan-phase 'on)
      (nskk-e2e-type "C-e")
      (nskk-e2e-assert-henkan-phase nil)
      (nskk-e2e-assert-buffer "か")
      (should (= (point) (point-max))))))

;;;;
;;;; Section 9: C-e in normal state -- plain end-of-line
;;;;

(nskk-describe "C-e in normal state"
  (nskk-it "moves point to end of line in normal hiragana state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "a")
      (nskk-e2e-type "i")
      (nskk-e2e-type "u")
      (nskk-e2e-assert-buffer "あいう")
      (goto-char (point-min))
      (nskk-e2e-type "C-e")
      (should (= (point) (point-max)))
      (nskk-e2e-assert-buffer "あいう")
      (nskk-e2e-assert-not-converting)))

  (nskk-it "moves point to end of line in ascii mode"
    (nskk-e2e-with-buffer nil nil
      (nskk-e2e-type "abc")
      (nskk-e2e-assert-buffer "abc")
      (goto-char (point-min))
      (nskk-e2e-type "C-e")
      (should (= (point) (point-max)))
      (nskk-e2e-assert-buffer "abc")))

  (nskk-it "is a no-op at end of line in normal state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "a")
      (nskk-e2e-assert-buffer "あ")
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

(defmacro nskk-e2e--simulate-unbound-command (command)
  (declare (indent 1))
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

(nskk-describe "C-f in preedit-pending state"
  (nskk-it "cleans up henkan phase and marker when no kana emitted yet"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e--dispatch-event ?K)
      (should (eq (nskk-state-henkan-phase nskk-current-state) 'on))
      (nskk-e2e-type "C-f")
      (nskk-e2e-assert-henkan-phase nil)
      (should (null (nskk-get-conversion-start)))
      (should (not (string-search nskk-henkan-on-marker (buffer-string))))))

  (nskk-it "allows fresh preedit after cursor movement from preedit-pending"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e--dispatch-event ?K)  ; preedit-pending
      (nskk-e2e-type "C-f")          ; clean up via fix
      (nskk-e2e--dispatch-event ?K)
      (nskk-e2e--dispatch-event ?a)
      (nskk-e2e-assert-henkan-phase 'on)
      (should (string-search "か" (buffer-string))))))

(nskk-describe "C-b in preedit-pending state"
  (nskk-it "cleans up henkan phase when moving backward before kana is emitted"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e--dispatch-event ?K)
      (should (eq (nskk-state-henkan-phase nskk-current-state) 'on))
      (nskk-e2e-type "C-b")
      (nskk-e2e-assert-henkan-phase nil)
      (should (null (nskk-get-conversion-start))))))

(provide 'nskk-navigation-e2e-test)

;;; nskk-navigation-e2e-test.el ends here
