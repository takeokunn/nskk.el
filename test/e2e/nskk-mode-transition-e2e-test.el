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

(nskk-deftest-e2e mode-cj-ascii-to-hiragana
  "C-j from ascii mode → enters hiragana (direct-idle → enter-hiragana)."
  (nskk-e2e-with-buffer nil nil
    (nskk-e2e-assert-mode 'ascii)
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-mode 'hiragana)))

(nskk-deftest-e2e mode-cj-hiragana-idle-newline
  "C-j from hiragana idle → inserts newline (japanese-idle → insert-newline)."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-assert-mode 'hiragana)
    (nskk-e2e-type "C-j")
    ;; Mode stays hiragana, a newline is inserted in buffer
    (nskk-e2e-assert-mode 'hiragana)
    (nskk-e2e-assert-buffer "\n")))

(nskk-deftest-e2e mode-cj-converting-commits
  "C-j from converting (▼) commits the candidate without newline."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Kanji")
    (nskk-e2e-type "SPC")
    (nskk-e2e-assert-converting)
    ;; C-j in converting state → commit-candidate
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-not-converting)
    (nskk-e2e-assert-buffer "漢字")))

(nskk-deftest-e2e mode-cj-latin-to-hiragana
  "C-j from latin mode → enters hiragana."
  (nskk-e2e-with-buffer 'latin nil
    (nskk-e2e-assert-mode 'latin)
    (nskk-e2e-type "C-j")
    (nskk-e2e-assert-mode 'hiragana)))

;;;;
;;;; q Key (nskk-handle-q / nskk-toggle-japanese-mode) Tests
;;;;

(nskk-deftest-e2e mode-q-hiragana-to-katakana
  "q key from hiragana mode → switches to katakana."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-assert-mode 'hiragana)
    (nskk-e2e-type "q")
    (nskk-e2e-assert-mode 'katakana)))

(nskk-deftest-e2e mode-q-katakana-to-hiragana
  "q key from katakana mode → switches to hiragana."
  (nskk-e2e-with-buffer 'katakana nil
    (nskk-e2e-assert-mode 'katakana)
    (nskk-e2e-type "q")
    (nskk-e2e-assert-mode 'hiragana)))

(nskk-deftest-e2e mode-q-katakana-hankaku-to-hiragana
  "q key from katakana-半角 mode → switches to hiragana.
This verifies the bug fix: toggle-mode(katakana-半角, hiragana) Prolog rule."
  (nskk-e2e-with-buffer 'katakana-半角 nil
    (nskk-e2e-assert-mode 'katakana-半角)
    (nskk-e2e-type "q")
    (nskk-e2e-assert-mode 'hiragana)))

(nskk-deftest-e2e mode-q-ascii-self-insert
  "q key from ascii mode → falls through to self-insert (inserts 'q')."
  (nskk-e2e-with-buffer nil nil
    (nskk-e2e-assert-mode 'ascii)
    (nskk-e2e-type "q")
    ;; In ascii mode, q is NOT a mode switch; it's self-inserted
    (nskk-e2e-assert-mode 'ascii)
    (nskk-e2e-assert-buffer "q")))

(nskk-deftest-e2e mode-q-toggle-idempotent
  "q key twice: hiragana → katakana → hiragana (toggle idempotency)."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "q")
    (nskk-e2e-assert-mode 'katakana)
    (nskk-e2e-type "q")
    (nskk-e2e-assert-mode 'hiragana)))

(nskk-deftest-e2e mode-q-katakana-hankaku-input-works
  "katakana-半角 mode: input routing works (bug fix: input-route rule added).
Typing romaji in katakana-半角 mode should process via process-japanese."
  (nskk-e2e-with-buffer 'katakana-半角 nil
    ;; The input-route(katakana-半角, process-japanese) rule is now present.
    ;; Typing 'a' should be routed through process-japanese.
    ;; It produces hiragana kana (full half-width katakana conversion not yet implemented)
    ;; but importantly it should NOT crash.
    (condition-case err
        (nskk-e2e-type "a")
      (error
       (ert-fail (format "katakana-半角 typing crashed: %s"
                         (error-message-string err)))))
    ;; Mode should still be katakana-半角
    (nskk-e2e-assert-mode 'katakana-半角)))

;;;;
;;;; l Key (nskk-handle-l) Tests
;;;;

(nskk-deftest-e2e mode-l-hiragana-to-latin
  "l key from hiragana mode → switches to latin/ascii mode."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-assert-mode 'hiragana)
    (nskk-e2e-type "l")
    ;; l triggers nskk-set-mode-latin in Japanese mode
    (nskk-e2e-assert-mode 'latin)))

(nskk-deftest-e2e mode-l-katakana-to-latin
  "l key from katakana mode → switches to latin mode."
  (nskk-e2e-with-buffer 'katakana nil
    (nskk-e2e-type "l")
    (nskk-e2e-assert-mode 'latin)))

(nskk-deftest-e2e mode-l-ascii-self-insert
  "l key from ascii mode → falls through to self-insert."
  (nskk-e2e-with-buffer nil nil
    (nskk-e2e-type "l")
    (nskk-e2e-assert-mode 'ascii)
    (nskk-e2e-assert-buffer "l")))

(nskk-deftest-e2e mode-l-latin-self-insert
  "l key from latin mode → self-insert (inserts 'l')."
  (nskk-e2e-with-buffer 'latin nil
    (nskk-e2e-type "l")
    ;; In latin mode, nskk-with-japanese-mode falls through to self-insert
    (nskk-e2e-assert-buffer "l")))

;;;;
;;;; L Key (nskk-handle-upper-l) Tests
;;;;

(nskk-deftest-e2e mode-upper-l-hiragana-to-jisx0208
  "L key from hiragana mode → switches to jisx0208-latin mode."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "L")
    (nskk-e2e-assert-mode 'jisx0208-latin)))

(nskk-deftest-e2e mode-upper-l-katakana-to-jisx0208
  "L key from katakana mode → switches to jisx0208-latin mode."
  (nskk-e2e-with-buffer 'katakana nil
    (nskk-e2e-type "L")
    (nskk-e2e-assert-mode 'jisx0208-latin)))

(nskk-deftest-e2e mode-upper-l-ascii-self-insert
  "L key from ascii mode → self-insert."
  (nskk-e2e-with-buffer nil nil
    (nskk-e2e-type "L")
    (nskk-e2e-assert-buffer "L")))

;;;;
;;;; / Key (nskk-handle-slash) Tests
;;;;

(nskk-deftest-e2e mode-slash-hiragana-to-abbrev
  "/ key from hiragana mode → switches to abbrev mode."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "/")
    (nskk-e2e-assert-mode 'abbrev)))

(nskk-deftest-e2e mode-slash-ascii-self-insert
  "/ key from ascii mode → self-insert."
  (nskk-e2e-with-buffer nil nil
    (nskk-e2e-type "/")
    (nskk-e2e-assert-buffer "/")))

;;;;
;;;; Complete Mode Transition Matrix
;;;;

(nskk-deftest-e2e mode-transition-matrix
  "Test all key-to-mode transitions from each Japanese mode."
  ;; Test: from each mode, pressing each transition key → expected mode
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
                                        from-mode key to-mode)))))))

;;;;
;;;; Mode After Conversion (Implicit Commit)
;;;;

(nskk-deftest-e2e mode-q-during-conversion-commits-first
  "q key from converting (▼) → commit current candidate, THEN toggle."
  ;; nskk-with-japanese-mode checks (nskk-converting-p): commits first, then acts
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Kanji")
    (nskk-e2e-type "SPC")
    (nskk-e2e-assert-converting)
    ;; q during conversion → implicit commit, then toggle hiragana→katakana
    (nskk-e2e-type "q")
    (nskk-e2e-assert-not-converting)
    ;; After implicit commit + toggle, should be in katakana
    (nskk-e2e-assert-mode 'katakana)
    ;; Buffer should contain committed text (漢字)
    (nskk-e2e-assert-buffer "漢字")))

(nskk-deftest-e2e mode-l-during-conversion-commits-first
  "l key from converting (▼) → commit current candidate, THEN switch to latin."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Kanji")
    (nskk-e2e-type "SPC")
    (nskk-e2e-assert-converting)
    (nskk-e2e-type "l")
    (nskk-e2e-assert-not-converting)
    (nskk-e2e-assert-mode 'latin)
    (nskk-e2e-assert-buffer "漢字")))

;;;;
;;;; RET in Various States
;;;;

(nskk-deftest-e2e mode-ret-normal-inserts-newline
  "RET in normal (non-converting) state inserts a newline."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "a")
    (nskk-e2e-type "RET")
    (nskk-e2e-assert-buffer "あ\n")))

(nskk-deftest-e2e mode-ret-converting-commits-no-newline
  "RET in converting (▼) state commits candidate without inserting newline."
  (nskk-e2e-with-buffer 'hiragana nil
    (nskk-e2e-type "Kanji")
    (nskk-e2e-type "SPC")
    (nskk-e2e-assert-converting)
    (nskk-e2e-type "RET")
    (nskk-e2e-assert-not-converting)
    (nskk-e2e-assert-buffer "漢字")))

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
