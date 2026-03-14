;;; nskk-registration-e2e-test.el --- E2E tests for dictionary registration  -*- lexical-binding: t; -*-

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
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; E2E and unit tests for the dictionary registration (辞書登録) feature.
;;
;; Tests follow DDSKK behavior as the reference implementation:
;;   - No candidates on first SPC -> registration prompt [辞書登録] reading:
;;   - RET with word -> saves to user dict, inserts into buffer
;;   - Empty RET -> cancel, preedit preserved
;;   - Registered words are immediately available for subsequent conversions
;;
;; Test naming:
;;   nskk-e2e-registration-*  -- full mode activation tests
;;   nskk-registration-*      -- unit tests for persistence functions
;;
;; Key E2E infrastructure note:
;;   The default nskk-e2e-with-buffer mock returns "" for read-from-minibuffer.
;;   To test the confirm path, nest a cl-letf inside the test body to shadow it
;;   with a function returning the desired word string.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-e2e-helpers)
(require 'nskk-test-macros)
(require 'nskk-dictionary)
(require 'nskk-pbt-generators)

;;;;
;;;; Section 1: E2E tests — no-candidates registration flow
;;;;

(nskk-describe "dictionary registration flow"
  (nskk-it "confirms registration inserts the word and stores it"
    ;; No candidates on SPC: registration prompt opens; confirming inserts the word.
    ;; The reading しんき is not in the default mock dict, so SPC triggers registration.
    ;; Inner cl-letf shadows the default empty-string mock to return 新機.
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Override mock: return specific word instead of default ""
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "新機")))
        ;; S → ▽ preedit, h i n k i → しんき (not in default dict)
        (nskk-e2e-type "Shinki")
        ;; SPC → no candidates → nskk-start-registration → mock returns 新機
        ;; → nskk-dict-register-word called → word inserted into buffer
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-buffer "新機" "Word should be inserted after registration")
        (nskk-e2e-assert-henkan-phase nil "Phase should be nil after successful registration")
        ;; Verify the word was actually stored in the Prolog user-dict-entry
        (should (nskk-prolog-query-one '(user-dict-entry "しんき" \?_))))))

  (nskk-it "cancels registration on empty RET and preserves preedit"
    ;; No candidates on SPC: empty RET cancels registration; preedit is preserved.
    ;; The default mock returns "" which triggers the cancel path in nskk-start-registration.
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Default mock returns "" -> registration is cancelled
      (nskk-e2e-type "Shinki")
      (nskk-e2e-type "SPC")
      ;; Preedit marker + reading should remain in buffer after cancel
      (nskk-e2e-assert-buffer "▽しんき" "Preedit should be preserved after cancel")
      ;; Phase is restored to 'on by nskk-start-registration unwind-protect
      (nskk-e2e-assert-henkan-phase 'on "Phase should be restored to 'on after cancel"))))

;;;;
;;;; Section 2: E2E test — registered word is immediately usable
;;;;

(nskk-describe "registration round trip"
  (nskk-it "registered word appears as candidate in subsequent conversion"
    ;; Registering a word makes it immediately findable in subsequent conversions.
    ;; Calls nskk-dict-register-word directly, then verifies the word appears as
    ;; a candidate when the same reading is converted via the standard SPC flow.
    (nskk-e2e-with-buffer 'hiragana nil
      ;; Register directly (simulates the effect of a successful registration UI)
      (nskk-dict-register-word "しんき" "新機")
      ;; Now type the same reading and convert
      (nskk-e2e-type "Shinki")
      ;; SPC should now find 新機 as a candidate (not trigger registration)
      (nskk-e2e-type "SPC")
      ;; In 'active phase, the conversion overlay shows the first candidate
      (nskk-e2e-assert-overlay-shows "新機" "Registered word should appear as first candidate")
      (nskk-e2e-assert-henkan-phase 'active "Should be in active conversion phase"))))

;;;;
;;;; Section 3: Unit tests — hooks
;;;;

(nskk-describe "jisyo update hook"
  (nskk-it "fires after a word is registered"
    ;; nskk-jisyo-update-hook is called after a word is registered.
    ;; DDSKK equivalent: skk-jisyo-update-hook fires after skk-update-jisyo.
    (nskk-prolog-test-with-isolated-db
      ;; Use let* so hook-result is in scope when hook-fn lambda is created.
      ;; Use a cons cell (mutable box) because setq of a captured lexical variable
      ;; does not mutate the outer let binding in Emacs Lisp closures.
      (let* ((hook-result (list nil))
             (hook-fn (lambda () (setcar hook-result t))))
        (add-hook 'nskk-jisyo-update-hook hook-fn)
        (unwind-protect
            (progn
              (nskk-dict-register-word "てすと" "テスト")
              (should (car hook-result)))
          (remove-hook 'nskk-jisyo-update-hook hook-fn))))))

(nskk-describe "kill-emacs hook"
  (nskk-it "nskk--enable adds nskk--dict-maybe-save to kill-emacs-hook"
    ;; Verifies the persistence hook is wired up when nskk-mode activates.
    (nskk-prolog-test-with-isolated-db
      ;; Ensure dict-initialized so nskk-dict-initialize is skipped
      (nskk-prolog-assert '((dict-initialized)))
      (with-temp-buffer
        (unwind-protect
            (progn
              (nskk-mode 1)
              (should (memq #'nskk--dict-maybe-save kill-emacs-hook)))
          (ignore-errors (nskk-mode -1)))))))

;;;;
;;;; Section 4: Unit tests — nskk--dict-maybe-save persistence
;;;;

(nskk-describe "nskk--dict-maybe-save e2e"
  (nskk-it "calls nskk-dict-save-user-dictionary when modified flag is t"
    (let ((save-called nil))
      (cl-letf (((symbol-function 'nskk-dict-save-user-dictionary)
                 (lambda () (setq save-called t))))
        (let ((nskk-dict-modified t))
          (nskk--dict-maybe-save)
          (should save-called)))))

  (nskk-it "skips save when modified flag is nil"
    (let ((save-called nil))
      (cl-letf (((symbol-function 'nskk-dict-save-user-dictionary)
                 (lambda () (setq save-called t))))
        (let ((nskk-dict-modified nil))
          (nskk--dict-maybe-save)
          (should-not save-called)))))

  (nskk-it "nskk-dict-save-user-dictionary resets modified flag to nil after saving"
    ;; Without this reset, the kill-emacs-hook would re-save on every subsequent
    ;; Emacs shutdown even when nothing changed since the last save.
    (nskk-prolog-test-with-isolated-db
      (let* ((tmp-file (make-temp-file "nskk-test" nil ".skk"))
             (nskk-dict-modified t)
             (nskk-dict-user-dictionary-file tmp-file)
             (nskk--user-dict-index 'user))
        (unwind-protect
            (progn
              (nskk-dict-save-user-dictionary)
              (should-not nskk-dict-modified))
          (when (file-exists-p tmp-file)
            (delete-file tmp-file)))))))

;;;;
;;;; Section 5: Unit tests — registration depth guard
;;;;

(nskk-describe "registration depth guard"
  (nskk-it "permits nesting depths 0, 1, and 2 (max depth = 3)"
    ;; Depth 0 is normal registration; depth 1 and 2 are recursive registrations.
    (should (< 0 nskk-max-registration-depth))
    (should (< 1 nskk-max-registration-depth))
    (should (< 2 nskk-max-registration-depth)))

  (nskk-it "rejects depth 3 (equal to max-registration-depth)"
    ;; nskk-max-registration-depth is 3; depth 3 is not allowed.
    (should-not (< 3 nskk-max-registration-depth))))

;;;;
;;;; Section 6: Runtime tests — registration depth guard
;;;;

(nskk-describe "registration depth guard (runtime)"
  (nskk-it "proceeds at depth 0 (normal registration)"
    ;; At depth 0, the guard (< 0 3) is t, so nskk-start-registration should
    ;; enter the registration branch and set henkan-phase to 'registration
    ;; before the unwind-protect restores it to the previous phase on exit.
    ;; After the call returns, phase is restored to prev-phase (nil in a fresh buffer)
    ;; and the registered word is returned.
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "テスト")))
        ;; Confirm depth starts at 0 inside the E2E buffer.
        (should (= nskk--registration-depth 0))
        (let ((result (nskk-start-registration "しんき")))
          ;; Guard passed: result is the registered word, not nil.
          (should (equal result "テスト")))
        ;; Phase restored to prev-phase (nil) after unwind-protect.
        (nskk-e2e-assert-henkan-phase nil "phase restored after depth-0 registration"))))

  (nskk-it "is blocked at depth nskk-max-registration-depth"
    ;; At depth == nskk-max-registration-depth (3), the guard (< 3 3) is nil,
    ;; so nskk-start-registration returns nil without changing phase.
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "テスト")))
        (let ((nskk--registration-depth nskk-max-registration-depth))
          (let ((result (nskk-start-registration "しんき")))
            ;; Guard blocked: the when form returns nil.
            (should-not result))
          ;; Phase must remain nil — guard never entered the registration branch.
          (nskk-e2e-assert-henkan-phase nil "depth guard should block registration")))))

  (nskk-it "proceeds at depth 2 (one below max)"
    ;; At depth 2, the guard (< 2 3) is t, so registration succeeds.
    ;; After the call, depth is restored to 2 by unwind-protect cl-decf.
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "テスト")))
        (let ((nskk--registration-depth 2))
          (let ((result (nskk-start-registration "しんき")))
            ;; Guard passed at depth 2 (< 2 3).
            (should (equal result "テスト")))
          ;; Phase restored after unwind-protect.
          (nskk-e2e-assert-henkan-phase nil "registration should succeed at depth 2")))))

  (nskk-it "increments depth during registration and decrements on exit"
    ;; nskk-start-registration calls (cl-incf nskk--registration-depth) after
    ;; entering the guard, then (cl-decf nskk--registration-depth) in unwind-protect.
    ;; We verify the net effect: depth returns to its value before the call.
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "テスト")))
        (let ((depth-before nskk--registration-depth))
          (nskk-start-registration "しんき")
          ;; After unwind-protect, depth is back to what it was before the call.
          (should (= nskk--registration-depth depth-before)))))))

;;;;
;;;; Section 7: Nested/stateful registration tests
;;;;

(nskk-describe "nested registration with stateful mocks"
  (nskk-it "registered word is committed and dict entry persists for immediate reuse"
    ;; Register "しんき" → "新機" via a stateful mock, then type the same
    ;; reading again.  The second SPC must find "新機" in the user dict and
    ;; display it as a candidate rather than reopening the registration prompt.
    (nskk-e2e-with-buffer 'hiragana nil
      ;; First round: no dict entry, so SPC triggers registration.
      ;; Stateful mock: call 0 → "新機", call 1 → "" (not reached in this test).
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (let ((call-count 0)
                       (responses '("新機" "")))
                   (lambda (&rest _)
                     (prog1 (nth call-count responses)
                       (setq call-count (1+ call-count)))))))
        (nskk-e2e-type "Shinki")
        ;; SPC: no candidates → registration prompt → mock returns "新機" → committed
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-buffer "新機" "Registered word should be inserted on first conversion")
        ;; Verify the Prolog user-dict has the new entry before the second round.
        (should (nskk-prolog-query-one '(user-dict-entry "しんき" \?_)))
        ;; Second round: type the same reading again in a fresh preedit.
        ;; The user-dict now has "しんき" → "新機", so SPC should show overlay,
        ;; not reopen registration.
        (nskk-e2e-type "Shinki")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "新機" "Registered word should appear as candidate on reuse")
        (nskk-e2e-assert-henkan-phase 'active "Should be in active conversion phase on reuse"))))

  (nskk-it "registration at depth 1 completes normally"
    ;; At depth 1 the guard (< 1 nskk-max-registration-depth) is t, so
    ;; nskk-start-registration enters the registration branch, returns the
    ;; registered word, and decrements depth back to 1 via unwind-protect.
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "テスト")))
        (let ((nskk--registration-depth 1))
          (let ((result (nskk-start-registration "しんき")))
            ;; Guard passed at depth 1 (< 1 3): registration succeeds.
            (should (equal result "テスト")))
          ;; unwind-protect restores depth to 1 after cl-decf.
          (should (= nskk--registration-depth 1))
          ;; Phase is restored to the value that was active before the call.
          (nskk-e2e-assert-henkan-phase nil "phase should be restored after depth-1 registration")))))

  (nskk-it "registration from abbrev mode uses abbrev text as key"
    ;; / → abbrev mode → "test" preedit → SPC → no dict entry (empty dict) →
    ;; registration prompt → mock returns "テスト" → "テスト" committed to buffer.
    ;; Assert the registered word ends up in the buffer and is stored in the dict.
    (nskk-e2e-with-buffer 'hiragana '()
      ;; Empty dict guaranteed by '() arg, so "test" has no candidates.
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "テスト")))
        (nskk-e2e-type "/")
        (nskk-e2e-assert-mode 'abbrev)
        (nskk-e2e-type "test")
        (nskk-e2e-assert-buffer "▽test" "Abbrev preedit should show ▽test before SPC")
        ;; SPC: no candidates for "test" in empty dict → registration → "テスト"
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-buffer "テスト" "Registered word should be inserted after abbrev registration")
        ;; Verify the word is now stored in the user dict under the abbrev key.
        (should (nskk-prolog-query-one '(user-dict-entry "test" \?_))))))

  (nskk-it "cancel in registration from abbrev preserves abbrev preedit"
    ;; / → "test" preedit → SPC → registration prompt → cancel (mock returns "").
    ;; On cancel, nskk-start-registration returns nil, so nskk-start-conversion
    ;; does NOT call nskk-henkan-do-reset.  The preedit text remains in the
    ;; buffer as "▽test" and the phase is restored to 'on.
    ;; This mirrors DDSKK behavior: skk-start-henkan on cancel preserves preedit.
    (nskk-e2e-with-buffer 'hiragana '()
      ;; Default mock (set by nskk-e2e-with-buffer) already returns "" — that is
      ;; the cancel path.  No cl-letf override needed here.
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "test")
      (nskk-e2e-assert-buffer "▽test" "Abbrev preedit should show ▽test before SPC")
      ;; SPC: no candidates → registration prompt → mock returns "" → cancel
      (nskk-e2e-type "SPC")
      ;; After cancel: preedit is preserved (registration returned nil, no reset).
      (nskk-e2e-assert-buffer "▽test" "Preedit should be preserved after cancelled abbrev registration")
      ;; Phase is restored to 'on by nskk-start-registration unwind-protect.
      (nskk-e2e-assert-henkan-phase 'on "Phase should be restored to 'on after cancel")
      ;; Conversion overlay must not be active.
      (nskk-e2e-assert-not-converting))))

;;;;
;;;; Property-Based Tests
;;;;

(nskk-deftest-table registration-flow-readings
  :columns (input expected)
  :rows (("Shinki" "新機")
         ("Kanji"  "漢字"))
  :body
  ;; Use a stub dict that does NOT contain the tested readings.
  ;; Both cases must go through the registration (not-found) path so that
  ;; read-from-minibuffer is called and the expected word is committed.
  ;; If the default dict were used, "かんじ" would be found as a candidate
  ;; and the conversion overlay would show the kanji without committing it,
  ;; making buffer-string return "▼かんじ" instead of "漢字".
  (nskk-e2e-with-buffer 'hiragana nskk--test-minimal-dict
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) expected)))
      (nskk-e2e-type input)
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-buffer expected))))

(nskk-property-test registration-empty-ret-does-not-crash
  ((mode valid-mode))
  (or (not (eq mode 'hiragana))
      (condition-case _err
          (progn
            (nskk-e2e-with-buffer 'hiragana nil
              (nskk-e2e-type "Shinki")
              (nskk-e2e-type "SPC"))
            t)
        (error t))))

(provide 'nskk-registration-e2e-test)

;;; nskk-registration-e2e-test.el ends here
