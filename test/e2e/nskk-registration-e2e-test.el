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
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "新機")))
        (nskk-e2e-type "Shinki")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-buffer "新機" "Word should be inserted after registration")
        (nskk-e2e-assert-henkan-phase nil "Phase should be nil after successful registration")
        (should (nskk-prolog-query-one '(user-dict-entry "しんき" \?_))))))

  (nskk-it "cancels registration on empty RET and preserves preedit"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Shinki")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-buffer "▽しんき" "Preedit should be preserved after cancel")
      (nskk-e2e-assert-henkan-phase 'on "Phase should be restored to 'on after cancel"))))

;;;;
;;;; Section 2: E2E test — registered word is immediately usable
;;;;

(nskk-describe "registration round trip"
  (nskk-it "registered word appears as candidate in subsequent conversion"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-dict-register-word "しんき" "新機")
      (nskk-e2e-type "Shinki")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-overlay-shows "新機" "Registered word should appear as first candidate")
      (nskk-e2e-assert-henkan-phase 'active "Should be in active conversion phase"))))

;;;;
;;;; Section 3: Unit tests — hooks
;;;;

(nskk-describe "jisyo update hook"
  (nskk-it "fires after a word is registered"
    (nskk-prolog-test-with-isolated-db
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
    (nskk-prolog-test-with-isolated-db
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
    (should (< 0 nskk-max-registration-depth))
    (should (< 1 nskk-max-registration-depth))
    (should (< 2 nskk-max-registration-depth)))

  (nskk-it "rejects depth 3 (equal to max-registration-depth)"
    (should-not (< 3 nskk-max-registration-depth))))

;;;;
;;;; Section 6: Runtime tests — registration depth guard
;;;;

(nskk-describe "registration depth guard (runtime)"
  (nskk-it "proceeds at depth 0 (normal registration)"
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "テスト")))
        (should (= (nskk-state-registration-depth) 0))
        (let ((result (nskk-start-registration "しんき")))
          (should (equal result "テスト")))
        (nskk-e2e-assert-henkan-phase nil "phase restored after depth-0 registration"))))

  (nskk-it "is blocked at depth nskk-max-registration-depth"
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "テスト")))
        (nskk-state-set-registration-depth nskk-max-registration-depth)
        (let ((result (nskk-start-registration "しんき")))
          (should-not result))
        (nskk-e2e-assert-henkan-phase nil "depth guard should block registration"))))

  (nskk-it "proceeds at depth 2 (one below max)"
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "テスト")))
        (nskk-state-set-registration-depth 2)
        (let ((result (nskk-start-registration "しんき")))
          (should (equal result "テスト")))
        (nskk-e2e-assert-henkan-phase nil "registration should succeed at depth 2"))))

  (nskk-it "increments depth during registration and decrements on exit"
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "テスト")))
        (let ((depth-before (nskk-state-registration-depth)))
          (nskk-start-registration "しんき")
          (should (= (nskk-state-registration-depth) depth-before)))))))

;;;;
;;;; Section 7: Nested/stateful registration tests
;;;;

(nskk-describe "nested registration with stateful mocks"
  (nskk-it "registered word is committed and dict entry persists for immediate reuse"
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (let ((call-count 0)
                       (responses '("新機" "")))
                   (lambda (&rest _)
                     (prog1 (nth call-count responses)
                       (setq call-count (1+ call-count)))))))
        (nskk-e2e-type "Shinki")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-buffer "新機" "Registered word should be inserted on first conversion")
        (should (nskk-prolog-query-one '(user-dict-entry "しんき" \?_)))
        (nskk-e2e-type "Shinki")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-overlay-shows "新機" "Registered word should appear as candidate on reuse")
        (nskk-e2e-assert-henkan-phase 'active "Should be in active conversion phase on reuse"))))

  (nskk-it "registration at depth 1 completes normally"
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "テスト")))
        (nskk-state-set-registration-depth 1)
        (let ((result (nskk-start-registration "しんき")))
          (should (equal result "テスト")))
        (should (= (nskk-state-registration-depth) 1))
        (nskk-e2e-assert-henkan-phase nil "phase should be restored after depth-1 registration"))))

  (nskk-it "registration from abbrev mode uses abbrev text as key"
    (nskk-e2e-with-buffer 'hiragana '()
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "テスト")))
        (nskk-e2e-type "/")
        (nskk-e2e-assert-mode 'abbrev)
        (nskk-e2e-type "test")
        (nskk-e2e-assert-buffer "▽test" "Abbrev preedit should show ▽test before SPC")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-buffer "テスト" "Registered word should be inserted after abbrev registration")
        (nskk-e2e-assert-mode 'hiragana)
        (should (nskk-prolog-query-one '(user-dict-entry "test" \?_))))))

  (nskk-it "cancel in registration from abbrev preserves abbrev preedit"
    (nskk-e2e-with-buffer 'hiragana '()
      (nskk-e2e-type "/")
      (nskk-e2e-assert-mode 'abbrev)
      (nskk-e2e-type "test")
      (nskk-e2e-assert-buffer "▽test" "Abbrev preedit should show ▽test before SPC")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-buffer "▽test" "Preedit should be preserved after cancelled abbrev registration")
      (nskk-e2e-assert-henkan-phase 'on "Phase should be restored to 'on after cancel")
      (nskk-e2e-assert-not-converting))))

;;;;
;;;; Property-Based Tests
;;;;

(nskk-deftest-table registration-flow-readings
  :columns (input expected)
  :rows (("Shinki" "新機")
         ("Kanji"  "漢字"))
  :body
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

;;;;
;;;; Section 8: C-g cancellation tests
;;;;

(defconst nskk-e2e--reg-7cands-dict
  '(("かんじ" . ("漢字" "感じ" "幹事" "換字" "貫地" "刊事" "肝事")))
  "Seven-candidate dict for C-g exhaust-candidates registration tests.")

(nskk-describe "C-g cancellation in registration"
  (nskk-it "cancels registration on C-g and preserves preedit (no-candidates path)"
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) (signal 'quit nil))))
        (nskk-e2e-type "Shinki")
        (nskk-e2e-type "SPC")
        (nskk-e2e-assert-buffer "▽しんき" "Preedit should be preserved after C-g cancel")
        (nskk-e2e-assert-henkan-phase 'on "Phase should be restored to 'on after C-g")
        (nskk-e2e-assert-not-converting))))

  (nskk-it "C-g from registration does not leak registration depth"
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) (signal 'quit nil))))
        (let ((depth-before (nskk-state-registration-depth)))
          (nskk-e2e-type "Shinki")
          (nskk-e2e-type "SPC")
          (should (= (nskk-state-registration-depth) depth-before))))))

  (nskk-it "C-g during exhaust-candidates wraps to first candidate"
    (nskk-e2e-with-buffer 'hiragana nskk-e2e--reg-7cands-dict
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) (signal 'quit nil))))
        (nskk-e2e-type "Kanji")
        (nskk-e2e-type "SPC")   ; SPC#1: start-conversion
        (nskk-e2e-type "SPC")   ; SPC#2: select-next
        (nskk-e2e-type "SPC")   ; SPC#3: select-next
        (nskk-e2e-type "SPC")   ; SPC#4: select-next
        (nskk-e2e-type "SPC")   ; SPC#5: show-list-next → 'list
        (nskk-e2e-assert-henkan-phase 'list "Precondition: must be in list phase")
        (nskk-e2e-type "SPC")   ; SPC#6: exhaust → C-g caught → wrap to index 0
        (nskk-e2e-assert-henkan-phase 'list "After C-g exhaust: phase must remain 'list")
        (should (nskk-henkan-candidate-list-active))
        (nskk-e2e-type "a")
        (nskk-e2e-assert-buffer "漢字" "After C-g cancel wrap, 'a' must commit 漢字"))))

  (nskk-it "cancels registration with kana-in-registration enabled"
    (let ((nskk-use-kana-in-registration t))
      (nskk-e2e-with-buffer 'hiragana nil
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _) (signal 'quit nil))))
          (nskk-e2e-type "Shinki")
          (nskk-e2e-type "SPC")
          (nskk-e2e-assert-buffer "▽しんき" "Preedit preserved after C-g (kana path)")
          (nskk-e2e-assert-henkan-phase 'on "Phase restored to 'on (kana path)")
          (should (= (nskk-state-registration-depth) 0))))))

  (nskk-it "cancels registration with kana-in-registration disabled"
    (let ((nskk-use-kana-in-registration nil))
      (nskk-e2e-with-buffer 'hiragana nil
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _) (signal 'quit nil))))
          (nskk-e2e-type "Shinki")
          (nskk-e2e-type "SPC")
          (nskk-e2e-assert-buffer "▽しんき" "Preedit preserved after C-g (non-kana path)")
          (nskk-e2e-assert-henkan-phase 'on "Phase restored to 'on (non-kana path)")
          (should (= (nskk-state-registration-depth) 0))))))

  (nskk-it "restores henkan-phase on C-g during nested registration"
    (nskk-e2e-with-buffer 'hiragana nil
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) (signal 'quit nil))))
        (nskk-state-set-registration-depth 1)
        (let ((depth-before (nskk-state-registration-depth)))
          (nskk-e2e-type "Shinki")
          (nskk-e2e-type "SPC")
          (should (= (nskk-state-registration-depth) depth-before))))))

  (nskk-it "uses the production registration C-g map for every input state"
    (let ((cases
           (quote ((pending nskk--deferred-azik-state)
                   (pending nskk--deferred-vowel-shadow-state)
                   (pending nskk--azik-colon-okuri-pending)
                   (pending nskk--azik-colon-okuri-deferred)
                   (pending nskk--azik-sokuon-okuri-kana-pending)
                   (pending nskk--sticky-shift-pending)
                   (phase active)
                   (phase list)
                   (mode hiragana)
                   (mode katakana)
                   (mode abbrev)
                   (mode ascii)
                   (mode latin)
                   (mode jisx0208-latin))))
          (pending-symbols
           (quote (nskk--deferred-azik-state
                   nskk--deferred-vowel-shadow-state
                   nskk--azik-colon-okuri-pending
                   nskk--azik-colon-okuri-deferred
                   nskk--azik-sokuon-okuri-kana-pending
                   nskk--sticky-shift-pending)))
          (case-count 0))
      (dolist (case-spec cases)
        (let ((kind (car case-spec))
              (value (cadr case-spec)))
          (nskk-e2e-with-buffer (quote hiragana) nil
            (nskk-e2e-type "Shinki")
            (let* ((nskk-use-kana-in-registration t)
                   (outer-buffer (current-buffer))
                   (outer-state nskk-current-state)
                   (outer-phase (nskk-state-henkan-phase nskk-current-state))
                   (outer-mode (nskk-current-mode))
                   (outer-text (buffer-string))
                   (outer-depth (nskk-state-registration-depth))
                   (outer-pending-values
                    (mapcar (lambda (symbol)
                              (cons symbol (symbol-value symbol)))
                            pending-symbols))
                   (setup-count 0))
              (cl-letf (((symbol-function (quote read-from-minibuffer))
                         (lambda (&rest _args)
                           (with-temp-buffer
                             (run-hooks (quote minibuffer-setup-hook))
                             (cl-incf setup-count)
                             (should nskk-mode)
                             (should (eq (nskk-current-mode) (quote hiragana)))
                             (dolist (symbol pending-symbols)
                               (set (make-local-variable symbol) nil))
                             (pcase kind
                               ((quote pending)
                                (set (make-local-variable value) t))
                               ((quote phase)
                                (nskk-state-force-henkan-phase nskk-current-state value)
                                (nskk-set-henkan-candidate-list-active
                                 (eq value (quote list))))
                               ((quote mode)
                                (nskk-set-mode value))
                               (_ (ert-fail (format "Unknown case: %S" case-spec))))
                             (dolist (symbol pending-symbols)
                               (should (eq (not (null (symbol-value symbol)))
                                           (and (eq kind (quote pending))
                                                (eq symbol value)))))
                             (when (eq kind (quote phase))
                               (should (eq (nskk-state-henkan-phase nskk-current-state)
                                           value)))
                             (when (eq kind (quote mode))
                               (should (eq (nskk-current-mode) value)))
                             (let ((registration-map
                                    (cdr (assq (quote nskk-mode)
                                               minor-mode-overriding-map-alist))))
                               (should (keymapp registration-map))
                               (should (eq (keymap-parent registration-map)
                                           nskk-mode-map))
                               (should (eq (lookup-key registration-map (kbd "C-j"))
                                           (lookup-key registration-map (kbd "RET"))))
                               (should (eq (lookup-key registration-map (kbd "C-g"))
                                           (function abort-recursive-edit)))
                               (should (eq (key-binding (kbd "C-g"))
                                           (function abort-recursive-edit))))
                             (signal (quote quit) nil)))))
                (should-not (nskk--read-registration-entry "しんき")))
              (should (= setup-count 1))
              (should (eq (current-buffer) outer-buffer))
              (should (eq nskk-current-state outer-state))
              (should (eq (nskk-state-henkan-phase nskk-current-state) outer-phase))
              (should (eq (nskk-current-mode) outer-mode))
              (should (equal (buffer-string) outer-text))
              (should (= (nskk-state-registration-depth) outer-depth))
              (should (equal (mapcar (lambda (symbol)
                                       (cons symbol (symbol-value symbol)))
                                     pending-symbols)
                             outer-pending-values))))
          (cl-incf case-count)))
      (should (= case-count (length cases)))))
)

(provide 'nskk-registration-e2e-test)

;;; nskk-registration-e2e-test.el ends here
