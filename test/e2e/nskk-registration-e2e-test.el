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
  (nskk-it "nskk--enable adds nskk-dict--maybe-save to kill-emacs-hook"
    ;; Verifies the persistence hook is wired up when nskk-mode activates.
    (nskk-prolog-test-with-isolated-db
      ;; Ensure dict-initialized so nskk-dict-initialize is skipped
      (nskk-prolog-assert '((dict-initialized)))
      (with-temp-buffer
        (unwind-protect
            (progn
              (nskk-mode 1)
              (should (memq #'nskk-dict--maybe-save kill-emacs-hook)))
          (ignore-errors (nskk-mode -1)))))))

;;;;
;;;; Section 4: Unit tests — nskk-dict--maybe-save persistence
;;;;

(nskk-describe "nskk-dict--maybe-save"
  (nskk-it "calls nskk-dict-save-user-dictionary when modified flag is t"
    (let ((save-called nil))
      (cl-letf (((symbol-function 'nskk-dict-save-user-dictionary)
                 (lambda () (setq save-called t))))
        (let ((nskk-dict-modified t))
          (nskk-dict--maybe-save)
          (should save-called)))))

  (nskk-it "skips save when modified flag is nil"
    (let ((save-called nil))
      (cl-letf (((symbol-function 'nskk-dict-save-user-dictionary)
                 (lambda () (setq save-called t))))
        (let ((nskk-dict-modified nil))
          (nskk-dict--maybe-save)
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
    (nskk-prolog-test-with-isolated-db
      (should (nskk-prolog-query-one '(registration-allowed 0)))
      (should (nskk-prolog-query-one '(registration-allowed 1)))
      (should (nskk-prolog-query-one '(registration-allowed 2)))))

  (nskk-it "rejects depth 3 (equal to max-registration-depth)"
    ;; The Prolog rule: registration-allowed ?d :- max-registration-depth ?m, < ?d ?m.
    (nskk-prolog-test-with-isolated-db
      (should-not (nskk-prolog-query-one '(registration-allowed 3))))))

(provide 'nskk-registration-e2e-test)

;;; nskk-registration-e2e-test.el ends here
