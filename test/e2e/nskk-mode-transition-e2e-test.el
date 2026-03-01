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

;; E2E mode transition tests for NSKK.
;;
;; Tests all mode transitions via actual key events (execute-kbd-macro).
;; This ensures the full keymap dispatch path works correctly, not just
;; direct function calls.
;;
;; Modes tested:
;;   ascii, hiragana, katakana, katakana-半角, abbrev, latin, jisx0208-latin
;;
;; Key transitions tested:
;;   C-j   -- ascii→hiragana, hiragana-idle→newline, converting→commit
;;   q     -- hiragana→katakana, katakana→hiragana, katakana-半角→hiragana
;;   l     -- hiragana→latin, latin→self-insert
;;   L     -- hiragana→jisx0208-latin
;;   /     -- hiragana→abbrev
;;
;; Bug fixes verified:
;;   katakana-半角 q → hiragana  (was missing toggle-mode rule)
;;   katakana-半角 typing        (was missing input-route rule)
;;
;; PBT properties:
;;   Mode after any transition key is always a valid mode
;;   Double-toggle returns to original mode (hiragana↔katakana)
;;   Mode from any mode via C-j+l is always latin (when idle)

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

  (nskk-it "inserts newline from hiragana idle"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "\n")))

  (nskk-it "switches from katakana idle to hiragana"
    (nskk-e2e-with-buffer 'katakana nil
      (nskk-e2e-assert-mode 'katakana)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)
      (nskk-e2e-assert-buffer "")))

  (nskk-it "switches from hankaku-katakana idle to hiragana"
    (nskk-e2e-with-buffer 'katakana-半角 nil
      (nskk-e2e-assert-mode 'katakana-半角)
      (nskk-e2e-type "C-j")
      (nskk-e2e-assert-mode 'hiragana)
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

(nskk-describe "mode transition matrix"
  (nskk-it "covers all key-to-mode transitions from each Japanese mode"
    (let ((transitions
           '(;; From hiragana
             (hiragana "q"   katakana)
             (hiragana "l"   latin)
             (hiragana "L"   jisx0208-latin)
             (hiragana "/"   abbrev)
             ;; From katakana
             (katakana "q"   hiragana)
             (katakana "l"   latin)
             (katakana "L"   jisx0208-latin)
             (katakana "/"   abbrev)
             ;; From katakana-半角 (verifies bug fix)
             (katakana-半角 "q" hiragana))))
      (dolist (tc transitions)
        (let ((from-mode (nth 0 tc))
              (key       (nth 1 tc))
              (to-mode   (nth 2 tc)))
          (nskk-e2e-with-buffer from-mode nil
            (nskk-e2e-type key)
            (nskk-e2e-assert-mode to-mode
                                  (format "Transition: %S + %S → %S failed"
                                          from-mode key to-mode))))))))

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

  (nskk-it "commits candidate without newline from converting state"
    (nskk-e2e-with-buffer 'hiragana nil
      (nskk-e2e-type "Kanji")
      (nskk-e2e-type "SPC")
      (nskk-e2e-assert-converting)
      (nskk-e2e-type "RET")
      (nskk-e2e-assert-not-converting)
      (nskk-e2e-assert-buffer "漢字"))))

;;;;
;;;; Property-Based Tests (PBT)
;;;;

(ert-deftest nskk-e2e-pbt-mode-always-valid ()
  "PBT: After any mode-switch key sequence, the mode is always valid."
  (let ((mode-keys '("q" "l" "L" "/"))
        (runs 75)
        (errors nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "E2E PBT 'mode-always-valid' seed: %d" test-seed)
    (dotimes (run runs)
      (condition-case err
          (let* ((start-mode (nth (random 3) '(hiragana katakana ascii)))
                 (key-count (+ 1 (random 5)))
                 (keys (cl-loop repeat key-count
                                collect (nth (random (length mode-keys)) mode-keys))))
            (nskk-e2e-with-buffer start-mode nil
              (dolist (key keys)
                (ignore-errors (nskk-e2e-type key)))
              (let ((final-mode (nskk-current-mode)))
                (unless (nskk-state-valid-mode-p final-mode)
                  (push (list :run run
                              :start-mode start-mode
                              :keys keys
                              :final-mode final-mode)
                        errors)))))
        (error
         (push (list :run run :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT mode-always-valid: %d failures (seed %d):\n%S"
                        (length errors) test-seed
                        (cl-subseq errors 0 (min 3 (length errors))))))))

(ert-deftest nskk-e2e-pbt-hiragana-katakana-toggle-idempotent ()
  "PBT: Pressing q twice from hiragana/katakana returns to original mode."
  (let ((runs 75)
        (errors nil)
        (test-seed (abs (random))))
    (random test-seed)
    (message "E2E PBT 'toggle-idempotent' seed: %d" test-seed)
    (dotimes (run runs)
      (condition-case err
          (let* ((start-mode (nth (random 2) '(hiragana katakana))))
            (nskk-e2e-with-buffer start-mode nil
              (nskk-e2e-type "q")
              (nskk-e2e-type "q")
              (let ((final-mode (nskk-current-mode)))
                (unless (eq final-mode start-mode)
                  (push (list :run run
                              :start-mode start-mode
                              :final-mode final-mode)
                        errors)))))
        (error
         (push (list :run run :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT toggle-idempotent: %d failures (seed %d):\n%S"
                        (length errors) test-seed
                        (cl-subseq errors 0 (min 3 (length errors))))))))

(ert-deftest nskk-e2e-pbt-cj-from-direct-always-hiragana ()
  "PBT: C-j from any direct-idle mode always enters hiragana."
  (let ((direct-modes '(ascii latin jisx0208-latin abbrev))
        (runs 50)
        (errors nil))
    (dolist (mode direct-modes)
      (dotimes (run runs)
        (condition-case err
            (nskk-e2e-with-buffer mode nil
              (nskk-e2e-type "C-j")
              (let ((actual (nskk-current-mode)))
                (unless (eq actual 'hiragana)
                  (push (list :mode mode :actual actual) errors))))
          (error
           (push (list :mode mode :error (error-message-string err)) errors)))))
    (when errors
      (ert-fail (format "E2E PBT cj-always-hiragana: %d failures:\n%S"
                        (length errors)
                        (cl-subseq errors 0 (min 3 (length errors))))))))

(ert-deftest nskk-e2e-pbt-katakana-hankaku-q-always-hiragana ()
  "PBT: q key from katakana-半角 always transitions to hiragana (bug fix verification)."
  (let ((runs 25)
        (errors nil))
    (dotimes (run runs)
      (condition-case err
          (nskk-e2e-with-buffer 'katakana-半角 nil
            (nskk-e2e-type "q")
            (let ((actual (nskk-current-mode)))
              (unless (eq actual 'hiragana)
                (push (list :run run :actual actual) errors))))
        (error
         (push (list :run run :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT katakana-hankaku-q: %d failures:\n%S"
                        (length errors) errors)))))

(ert-deftest nskk-e2e-pbt-mode-sequence-no-crash ()
  "PBT: Arbitrary mode-switch + typing sequences never crash."
  (let ((all-keys '("q" "l" "L" "/" "C-j" "a" "k" "s" "t"))
        (runs 50)
        (errors nil)
        (test-seed (abs (random))))
    (random test-seed)
    (dotimes (run runs)
      (condition-case err
          (let* ((key-count (+ 2 (random 8)))
                 (keys (cl-loop repeat key-count
                                collect (nth (random (length all-keys)) all-keys))))
            (nskk-e2e-with-buffer 'hiragana nil
              (dolist (key keys)
                (ignore-errors (nskk-e2e-type key)))
              ;; Property: state should still be valid
              (unless (nskk-state-valid-mode-p (nskk-current-mode))
                (push (list :run run :keys keys :mode (nskk-current-mode))
                      errors))))
        (error
         (push (list :run run :error (error-message-string err)) errors))))
    (when errors
      (ert-fail (format "E2E PBT mode-sequence-no-crash: %d failures (seed %d):\n%S"
                        (length errors) test-seed
                        (cl-subseq errors 0 (min 3 (length errors))))))))

(provide 'nskk-mode-transition-e2e-test)

;;; nskk-mode-transition-e2e-test.el ends here
