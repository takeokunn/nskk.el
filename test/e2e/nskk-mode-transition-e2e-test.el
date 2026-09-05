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

  (nskk-it "preserves text from hiragana idle"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "")))

  (nskk-it "preserves katakana idle"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-assert-mode 'katakana)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'katakana)
      (nskk-e2e-assert-buffer "")))

  (nskk-it "preserves hankaku-katakana idle"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-e2e-assert-mode 'katakana-半角)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'katakana-半角)
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

  (nskk-it "commits candidate with newline from converting state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字\n"))))

;;;;
;;;; Property-Based Tests (PBT)
;;;;

(nskk-property-test-seeded mode-always-valid
  ((start-mode valid-mode))
  (let ((mode-keys '("q" "l" "L" "/"))
        (key-count (+ 1 (random 5))))
    (nskk-e2e-with-buffer start-mode nil
      (dotimes (_ key-count)
        (nskk-e2e-type (nth (random (length mode-keys)) mode-keys)))
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
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-e2e-type "q")
      (should (eq (nskk-current-mode) 'hiragana)))))

(nskk-property-test-seeded mode-sequence-no-crash
  ((start-mode valid-mode))
  (let* ((all-keys '("q" "l" "L" "/" "C-j" "a" "k" "s" "t"))
         (key-count (+ 2 (random 8)))
         (keys (cl-loop repeat key-count
                        collect (nth (random (length all-keys)) all-keys))))
    (nskk-e2e-with-buffer start-mode nil
      (dolist (key keys)
        (nskk-e2e-type key))
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

  (nskk-it "C-j from katakana-半角 idle preserves mode"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-e2e-assert-mode 'katakana-半角)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'katakana-半角))))

;;;;
;;;; PBT: Mode transition invariants
;;;;

;;;;
;;;; Property: Mode Isolation Across Buffers
;;;;

(nskk-property-test-seeded mode-isolation-across-buffers
  ((mode-a valid-mode)
   (mode-b valid-mode))
  (nskk-e2e-with-buffer mode-a nil
    (nskk-e2e-with-buffer mode-b nil
      (nskk-e2e-type "q")
      t)
    (eq (nskk-current-mode) mode-a))
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

(ert-deftest nskk-e2e-command-loop-undo-kakutei ()
  (let ((read-minibuffer (symbol-function 'read-from-minibuffer)))
    (dolist (invocation '("<f8>" "M-x nskk-undo-kakutei RET"))
      (nskk-e2e-with-buffer 'hiragana nil
        (buffer-enable-undo)
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (cl-letf (((symbol-function 'read-from-minibuffer) read-minibuffer))
            (local-set-key (kbd "<f8>") #'nskk-undo-kakutei)
            (execute-kbd-macro (kbd (concat "Kanji SPC C-j " invocation)))
            (should (eq (nskk-state-henkan-phase nskk-current-state) 'active))
            (should (equal (buffer-string) "▼感じ"))
            (should (= (nskk-state-current-index nskk-current-state) 1))
            (should-error (nskk-undo-kakutei) :type 'user-error)))))))

(ert-deftest nskk-e2e-command-loop-delayed-undo-kakutei ()
  (dolist (case '(("C-b" "▼感じ") ("a" "▼感じあ")
                  ("C-a a" "あ▼感じ")
                  ("C-b C-d" "▼感じ") ("C-b DEL" "▼感じ")))
    (nskk-e2e-with-buffer 'hiragana nil
      (buffer-enable-undo)
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (local-set-key (kbd "<f8>") #'nskk-undo-kakutei)
        (execute-kbd-macro (kbd (concat "Kanji SPC C-j " (car case) " <f8>")))
        (should (equal (buffer-string) (cadr case)))
        (should (eq (nskk-state-henkan-phase nskk-current-state) 'active))))))

(ert-deftest nskk-e2e-command-loop-normal-undo-preedit ()
  (nskk-e2e-with-buffer 'hiragana nil
    (buffer-enable-undo)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let (states last-command-event)
        (add-hook 'post-command-hook
                  (lambda ()
                    (when (eq last-command-event (aref (kbd "C-/") 0))
                      (push (list (buffer-string)
                                  (nskk-state-henkan-phase nskk-current-state))
                            states))) nil t)
        (execute-kbd-macro (kbd "Kanji SPC C-/ C-/ C-/ C-/ C-/"))
        (should (equal (nreverse states)
                       '(("▽かんじ" on) ("▽かん" on) ("▽か" on)
                         ("▽" on) ("" nil))))))))

(ert-deftest nskk-e2e-command-loop-normal-undo-commit ()
  (dolist (case '(("C-j" ("" nil))
                  ("RET" ("漢字" nil) ("▼漢字" nil) ("" nil))))
    (nskk-e2e-with-buffer 'hiragana nil
      (buffer-enable-undo)
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (let (states last-command-event)
          (add-hook 'post-command-hook
                    (lambda ()
                      (when (eq last-command-event (aref (kbd "C-/") 0))
                        (push (list (buffer-string)
                                    (nskk-state-henkan-phase nskk-current-state))
                              states))) nil t)
          (execute-kbd-macro
           (kbd (concat "Kanji SPC " (car case)
                        (mapconcat (lambda (_) " C-/") (cdr case) ""))))
          (should (equal (nreverse states) (cdr case))))))))

(ert-deftest nskk-e2e-command-loop-normal-undo-after-idle ()
  (nskk-e2e-with-buffer 'hiragana nil
    (buffer-enable-undo)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let (states last-command-event
            (undo-auto--last-boundary-cause undo-auto--last-boundary-cause)
            (undo-auto--undoably-changed-buffers nil))
        (add-hook 'pre-command-hook
                  (lambda ()
                    (when (eq last-command-event ?\s)
                      (undo-auto--boundaries 'command))) nil t)
        (add-hook 'post-command-hook
                  (lambda ()
                    (cond
                     ((eq last-command-event ?\s)
                      (undo-auto--boundary-timer)
                      (should (null (car buffer-undo-list))))
                     ((eq last-command-event (aref (kbd "C-/") 0))
                      (push (list (buffer-string)
                                  (nskk-state-henkan-phase nskk-current-state))
                            states)))) nil t)
        (execute-kbd-macro (kbd "Kanji SPC C-j C-/ C-/"))
        (should (equal (nreverse states) '(("▼漢字" nil) ("" nil))))))))

(ert-deftest nskk-e2e-command-loop-normal-undo-redo ()
  (nskk-e2e-with-buffer 'hiragana nil
    (buffer-enable-undo)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (local-set-key (kbd "<f9>") #'undo-redo)
      (let (states last-command-event)
        (add-hook 'post-command-hook
                  (lambda ()
                    (when (memq this-command '(undo undo-redo))
                      (push (list (buffer-string)
                                  (nskk-state-henkan-phase nskk-current-state))
                            states))) nil t)
        (execute-kbd-macro (kbd "Kanji SPC C-j C-/ <f9>"))
        (should (equal (nreverse states) '(("" nil) ("漢字" nil))))))))

(ert-deftest nskk-e2e-command-loop-normal-undo-pending-prefix ()
  (nskk-e2e-with-buffer 'hiragana nil
    (buffer-enable-undo)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "K a k C-/"))
      (should (equal (buffer-string) "▽か"))
      (should (equal (nskk-state-romaji-buffer) ""))
      (should (eq (nskk-state-henkan-phase nskk-current-state) 'on)))))

(ert-deftest nskk-e2e-command-loop-special-undo-last-candidate ()
  (nskk-e2e-with-buffer 'hiragana nil
    (buffer-enable-undo)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (local-set-key (kbd "<f8>") #'nskk-undo-kakutei)
      (execute-kbd-macro (kbd "Kanji SPC SPC SPC C-j <f8>"))
      (should (equal (buffer-string) "▼漢字"))
      (should (equal (nskk-state-candidates nskk-current-state)
                     '("幹事" "漢字" "感じ")))
      (should (= (nskk-state-current-index nskk-current-state) 1)))))

(ert-deftest nskk-e2e-command-loop-ddskk-undo-boundaries ()
  (dolist (case (append
                '(("Kanji SPC C-j C-/" "")
                  ("Kanji SPC SPC C-j C-/" "▼感じ")
                  ("Kanji SPC SPC SPC C-j C-/" "▼幹事"))
                (cl-loop for n in '(18 19 20 21)
                         append
                         (list
                          (list (concat (make-string n ?a) " C-j C-/")
                                (if (= n 21) (make-string 20 ?あ) ""))
                          (list (concat (make-string n ?a) " Kanji SPC C-j C-/")
                                (concat (make-string n ?あ)
                                        (nth (- n 18) '("▼かん" "▼か" "▼" ""))))))))
    (ert-info ((car case))
      (nskk-e2e-with-buffer 'hiragana nil
        (buffer-enable-undo)
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (execute-kbd-macro (kbd (car case)))
          (should (equal (buffer-string) (cadr case)))
          (should-not (nskk-state-henkan-phase nskk-current-state)))))))

(ert-deftest nskk-e2e-command-loop-ddskk-kakutei-modes ()
  (dolist (case '(("C-j" "" hiragana)
                  ("a C-j" "あ" hiragana)
                  ("q C-j" "" katakana)
                  ("q a C-j" "ア" katakana)
                  ("k C-j" "" hiragana)
                  ("Kanji C-j" "かんじ" hiragana)))
    (ert-info ((car case))
      (nskk-e2e-with-buffer 'hiragana nil
        (buffer-enable-undo)
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (execute-kbd-macro (kbd (car case)))
          (should (equal (buffer-string) (cadr case)))
          (should (eq (nskk-state-get-mode) (caddr case)))
          (should (equal (nskk-state-romaji-buffer) ""))
          (should-not (nskk-state-henkan-phase nskk-current-state)))))))

(ert-deftest nskk-e2e-command-loop-candidate-navigation-keeps-conversion ()
  (dolist (case '(("Kanji SPC SPC" "感じ")
                  ("Kanji SPC SPC SPC" "幹事")
                  ("KaKu SPC" "掛")))
    (ert-info ((car case))
      (nskk-e2e-with-buffer 'hiragana nil
        (buffer-enable-undo)
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (execute-kbd-macro (kbd (car case)))
          (nskk-e2e-assert-converting)
          (nskk-e2e-assert-overlay-shows (cadr case)))))))

(ert-deftest nskk-e2e-isearch-command-loop ()
  (dolist (case '((ascii "C-s k a n a RET" "kana" 11)
                  (katakana-半角 "C-s k a n a RET" "kana" 11)
                  (katakana-半角 "C-s C-j k a n a RET" "かな" 14)
                  (jisx0208-latin "C-s C-j k a n a RET" "かな" 14)
                  (hiragana "C-s k a n a RET" "かな" 14)
                  (hiragana "C-s q k a n a RET" "カナ" 17)
                  (hiragana "C-s l k a n a RET" "kana" 11)
                  (hiragana "C-s l C-j k a n a RET" "かな" 14)
                  (hiragana "C-s K a n a RET RET" "かな" 14)
                  (hiragana "C-s k a n a DEL RET" "か" 13)
                  (hiragana "C-s K a C-g k a n a RET" "かな" 14)
                  (hiragana "C-s k DEL k a n a RET" "っかな" 1)
                  (hiragana "C-s C-j RET" "\n" 24)
                  (hiragana "C-s k C-j RET" "\n" 24)
                  (hiragana "C-s K a TAB RET RET" "\tか" 1)
                  (hiragana "C-s k a n a TAB RET" "かな\t" 14)
                  (hiragana "C-s k C-g k a n a RET" "かな" 14)
                  (hiragana "C-s k a n a RET C-s k a n a RET" "かな" 19)))
    (ert-info ((format "%S" case))
      (let ((nskk--persistence-inhibited t)
            (nskk-search-auto-save-learning nil))
        (unwind-protect
            (progn
              (nskk-isearch-setup)
              (nskk-e2e-with-buffer (car case) nil
                (insert "start kana かな カナ か end\n")
                (goto-char (point-min))
                (save-window-excursion
                  (switch-to-buffer (current-buffer))
                  (unwind-protect
                      (progn
                        (execute-kbd-macro (kbd (nth 1 case)))
                        (should (equal isearch-string (nth 2 case)))
                        (should (= (point) (nth 3 case)))
                        (should-not isearch-mode)
                        (should (equal (buffer-string) "start kana かな カナ か end\n"))
                        (should (eq (nskk-state-mode nskk-current-state) (car case)))
                        (should-not nskk--isearch-input-sessions)
                        (should-not nskk--isearch-orig-buffer-stack)
                        (should-not nskk--isearch-orig-buffer)
                        (should-not overriding-terminal-local-map))
                    (when isearch-mode (isearch-done))))))
          (nskk-isearch-teardown))))))

(ert-deftest nskk-e2e-isearch-command-loop-direction ()
  (dolist (mode-case '((hiragana "か" 30 17 18)
                       (ascii "ka" 27 14 16)
                       (katakana "カ" 32 19 20)
                       (jisx0208-latin "ｋａ" 34 21 23)))
    (dolist (operation '(("C-r k a RET" t 2)
                         ("C-r k a C-r RET" t 3)
                         ("C-s k a C-s RET" nil 4)
                         ("C-s k a C-s C-r RET" nil 3)))
      (ert-info ((format "%S %S" mode-case operation))
        (let ((nskk--persistence-inhibited t)
              (nskk-search-auto-save-learning nil)
              (text "ka か カ ｋａ -- ka か カ ｋａ -- ka か カ ｋａ"))
          (unwind-protect
              (progn
                (nskk-isearch-setup)
                (nskk-e2e-with-buffer (car mode-case) nil
                  (insert text)
                  (goto-char (if (nth 1 operation) (point-max) (point-min)))
                  (save-window-excursion
                    (switch-to-buffer (current-buffer))
                    (unwind-protect
                        (progn
                          (execute-kbd-macro (kbd (car operation)))
                          (should (equal isearch-string (nth 1 mode-case)))
                          (should (= (point) (nth (nth 2 operation) mode-case)))
                          (should (equal (buffer-string) text))
                          (should (eq (nskk-state-mode nskk-current-state)
                                      (car mode-case)))
                          (should-not isearch-mode)
                          (should-not nskk--isearch-input-sessions)
                          (should-not nskk--isearch-orig-buffer-stack)
                          (should-not nskk--isearch-orig-buffer)
                          (should-not overriding-terminal-local-map))
                      (when isearch-mode (isearch-done))))))
            (nskk-isearch-teardown)))))))

(ert-deftest nskk-e2e-candidate-list-command-loop-routing ()
  (let ((show (symbol-function 'nskk-candidate-show-list))
        (hide (symbol-function 'nskk-candidate-hide-list))
        (nskk-henkan-show-candidates-nth 2)
        (nskk-henkan-number-to-display-candidates 2))
    (dolist (spec '(("RET a C-j" "感じ")
                    ("C-j a C-j" "感じ")
                    ("q a C-j" "感じ")
                    ("l a C-j" "感じ")
                    ("z a C-j" "感じ")
                    ("1 a C-j" "感じ")
                    ("C-n a C-j" "感じ")
                    ("C-a a C-j" "感じ")
                    ("C-e a C-j" "感じ")
                    ("C-p C-j" "漢字")
                    ("SPC C-p a C-j" "感じ")
                    ("DEL C-j" "漢字")
                    ("SPC DEL a C-j" "感じ")
                    ("C-g a C-j" "かんじあ")))
      (nskk-e2e-with-buffer 'hiragana
          '(("かんじ" . ("漢字" "感じ" "幹事" "監事" "完治")))
        (cl-letf (((symbol-function 'nskk-candidate-show-list) show)
                  ((symbol-function 'nskk-candidate-hide-list) hide))
          (switch-to-buffer (current-buffer))
          (execute-kbd-macro (kbd "K a n j i SPC SPC"))
          (should (nskk-henkan-candidate-list-active))
          (execute-kbd-macro (kbd (car spec)))
          (should (equal (buffer-string) (cadr spec)))
          (should-not (nskk-state-henkan-phase nskk-current-state))
          (should (equal (nskk-state-romaji-buffer) ""))
          (should (eq (nskk-state-get-mode) 'hiragana))
          (should-not (nskk-henkan-candidate-list-active)))))))

(ert-deftest nskk-e2e-candidate-list-command-loop-configured-keys ()
  (let ((show (symbol-function 'nskk-candidate-show-list))
        (hide (symbol-function 'nskk-candidate-hide-list))
        (nskk-henkan-show-candidates-nth 2)
        (nskk-henkan-number-to-display-candidates 2))
    (dolist (key '(?\r ?\C-j ?\177 ?q ?l ?1))
      (ert-info ((format "selection key %S" key))
        (nskk-e2e-with-buffer 'hiragana
            '(("かんじ" . ("漢字" "感じ" "幹事" "監事" "完治")))
          (let ((nskk-henkan-show-candidates-keys (list key ?s ?a))
                (nskk--candidate-key-facts-initialized nil))
            (nskk--candidate-init-key-facts)
            (cl-letf (((symbol-function 'nskk-candidate-show-list) show)
                      ((symbol-function 'nskk-candidate-hide-list) hide))
              (switch-to-buffer (current-buffer))
              (execute-kbd-macro (kbd "K a n j i SPC SPC"))
              (should (nskk-henkan-candidate-list-active))
              (let ((before (buffer-string))
                    (index (nskk-state-current-index nskk-current-state)))
                (execute-kbd-macro "a")
                (should (equal (buffer-string) before))
                (should (= (nskk-state-current-index nskk-current-state) index))
                (should (nskk-henkan-candidate-list-active))
                (should (equal (nskk-state-romaji-buffer) ""))
                (should (eq (nskk-state-get-mode) 'hiragana)))
              (execute-kbd-macro (vector key))
              (should (equal (buffer-string) "感じ"))
              (should-not (nskk-state-henkan-phase nskk-current-state))
              (should-not (nskk-henkan-candidate-list-active)))))))))

(provide 'nskk-mode-transition-e2e-test)

;;; nskk-mode-transition-e2e-test.el ends here
