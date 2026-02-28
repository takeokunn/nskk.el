;;; nskk-input-routing-pbt-test.el --- PBT for input routing by mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n, testing, property-based

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

;; Property-Based Tests for input routing based on mode.
;;
;; This file tests that input is correctly routed based on the current mode:
;; - ASCII and Latin modes: direct character insertion
;; - Hiragana/Katakana modes: romaji to kana conversion
;; - Abbrev mode: abbreviation processing
;;
;; These tests are designed to detect regressions in mode handling,
;; particularly the bug where ASCII mode was incorrectly routed through
;; Japanese conversion instead of direct insertion.

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-state)
(require 'nskk-input)

;;;;
;;;; Helper Macros
;;;;

(defmacro nskk-input-routing-test-with-state (initial-mode &rest body)
  "Execute BODY with a fresh state initialized to INITIAL-MODE."
  (declare (indent 1))
  `(let ((nskk-current-state (nskk-state-create ,initial-mode))
         (nskk--romaji-buffer ""))
     ,@body))

;;;;
;;;; Generator for direct-insert modes
;;;;

(nskk-register-generator 'direct-insert-mode
  (lambda (&rest _args)
    (nskk-pbt--random-choice '(ascii latin))))

;;;;
;;;; Property 1: Direct-Insert Modes Insert Characters Directly
;;;;

(ert-deftest nskk-pbt-ascii-mode-direct-insert ()
  "Property: In ASCII mode, characters are inserted directly without conversion."
  (let ((runs 50)
        (failures nil)
        (test-chars '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z)))
    (dotimes (_ runs)
      (let ((char (nskk-pbt--random-choice test-chars)))
        (nskk-input-routing-test-with-state 'ascii
          (with-temp-buffer
            (setq last-command-event char)
            (nskk-self-insert 1)
            (let ((result (buffer-string)))
              ;; Should insert the character directly, not convert to kana
              (unless (string= result (char-to-string char))
                (push (list :char char :expected (char-to-string char) :actual result)
                      failures)))))))
    (when failures
      (ert-fail (format "ASCII mode direct-insert failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))

(ert-deftest nskk-pbt-latin-mode-direct-insert ()
  "Property: In Latin mode, characters are inserted directly without conversion."
  (let ((runs 50)
        (failures nil)
        (test-chars '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z)))
    (dotimes (_ runs)
      (let ((char (nskk-pbt--random-choice test-chars)))
        (nskk-input-routing-test-with-state 'latin
          (with-temp-buffer
            (setq last-command-event char)
            (nskk-self-insert 1)
            (let ((result (buffer-string)))
              ;; Should insert the character directly, not convert to kana
              (unless (string= result (char-to-string char))
                (push (list :char char :expected (char-to-string char) :actual result)
                      failures)))))))
    (when failures
      (ert-fail (format "Latin mode direct-insert failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))

;;;;
;;;; Property 2: ASCII and Latin Modes Are Equivalent for Input Routing
;;;;

(ert-deftest nskk-pbt-ascii-latin-equivalent ()
  "Property: ASCII and Latin modes produce identical output for the same input."
  (let ((runs 50)
        (failures nil)
        (test-chars '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z)))
    (dotimes (_ runs)
      (let ((char (nskk-pbt--random-choice test-chars)))
        ;; Test ASCII mode
        (let (ascii-result latin-result)
          (nskk-input-routing-test-with-state 'ascii
            (with-temp-buffer
              (setq last-command-event char)
              (nskk-self-insert 1)
              (setq ascii-result (buffer-string))))
          ;; Test Latin mode
          (nskk-input-routing-test-with-state 'latin
            (with-temp-buffer
              (setq last-command-event char)
              (nskk-self-insert 1)
              (setq latin-result (buffer-string))))
          ;; Results should be identical
          (unless (string= ascii-result latin-result)
            (push (list :char char :ascii ascii-result :latin latin-result)
                  failures)))))
    (when failures
      (ert-fail (format "ASCII/Latin equivalence failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))

;;;;
;;;; Property 3: Japanese Modes Convert Romaji to Kana
;;;;

(ert-deftest nskk-pbt-hiragana-mode-converts ()
  "Property: In Hiragana mode, vowels are converted to hiragana."
  (let ((runs 30)
        (failures nil)
        (vowel-map '((?a . "あ") (?i . "い") (?u . "う") (?e . "え") (?o . "お"))))
    (dolist (_ (number-sequence 1 runs))
      (let* ((pair (nskk-pbt--random-choice vowel-map))
             (char (car pair))
             (expected (cdr pair)))
        (nskk-input-routing-test-with-state 'hiragana
          (with-temp-buffer
            (setq last-command-event char)
            (nskk-self-insert 1)
            (let ((result (buffer-string)))
              (unless (string= result expected)
                (push (list :char char :expected expected :actual result)
                      failures)))))))
    (when failures
      (ert-fail (format "Hiragana mode conversion failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))

(ert-deftest nskk-pbt-katakana-mode-converts ()
  "Property: In Katakana mode, vowels are converted to katakana."
  (let ((runs 30)
        (failures nil)
        (vowel-map '((?a . "ア") (?i . "イ") (?u . "ウ") (?e . "エ") (?o . "オ"))))
    (dolist (_ (number-sequence 1 runs))
      (let* ((pair (nskk-pbt--random-choice vowel-map))
             (char (car pair))
             (expected (cdr pair)))
        (nskk-input-routing-test-with-state 'katakana
          (with-temp-buffer
            (setq last-command-event char)
            (nskk-self-insert 1)
            (let ((result (buffer-string)))
              (unless (string= result expected)
                (push (list :char char :expected expected :actual result)
                      failures)))))))
    (when failures
      (ert-fail (format "Katakana mode conversion failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))

;;;;
;;;; Property 4: Mode Routing Invariant
;;;;

(ert-deftest nskk-pbt-mode-routing-invariant ()
  "Property: All valid modes route input correctly without error."
  (let ((runs 60)
        (failures nil)
        (modes '(ascii latin hiragana katakana abbrev))
        (test-chars '(?a ?k ?s ?t ?n)))
    (dotimes (_ runs)
      (let ((mode (nskk-pbt--random-choice modes))
            (char (nskk-pbt--random-choice test-chars)))
        (condition-case err
            (nskk-input-routing-test-with-state mode
              (with-temp-buffer
                (setq last-command-event char)
                (nskk-self-insert 1)
                ;; Should complete without error
                t))
          (error
           (push (list :mode mode :char char :error err)
                 failures)))))
    (when failures
      (ert-fail (format "Mode routing invariant failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))

;;;;
;;;; Regression Test: ASCII Mode Bug Detection
;;;;

(ert-deftest nskk-pbt-regression-ascii-mode-bug ()
  "Regression test: Verify ASCII mode bug is fixed.

This test would FAIL if the bug where ASCII mode was incorrectly
routed through Japanese conversion is reintroduced.

The bug was: (eq (nskk-get-mode) 'latin) instead of
            (memq (nskk-get-mode) '(ascii latin))"
  ;; Test that typing 'a' in ASCII mode produces 'a', not 'あ'
  (nskk-input-routing-test-with-state 'ascii
    (with-temp-buffer
      (setq last-command-event ?a)
      (nskk-self-insert 1)
      (should (string= (buffer-string) "a")))))

;;;; Enhanced PBT Coverage
;;;;

;;;
;;; Shrinking Property: Input routing is deterministic
;;;
;;; Same character in same mode must always produce the same buffer content.
;;; When a failure is found, the framework attempts to shrink the scenario to
;;; the smallest character/mode pair that still exhibits the failure.
;;;

(nskk-property-test-with-shrinking input-routing-deterministic
  ((scenario input-scenario))
  ;; scenario plist: {:mode :key-sequence :expected-length}
  (let* ((mode (plist-get scenario :mode))
         ;; Pick a single character from the key sequence for determinism check
         (key-seq (plist-get scenario :key-sequence))
         (char-str (if key-seq (car key-seq) "a"))
         ;; char-str may be a special key string like "C-j"; only test single chars
         (single-char-p (and (stringp char-str)
                             (= (length char-str) 1)))
         (char-val (when single-char-p
                     (string-to-char char-str))))
    (if (not single-char-p)
        t  ; Skip non-single-char keys — not testable as direct insert
      ;; Run the same insert twice from identical initial states and compare
      (let (result-1 result-2)
        (nskk-input-routing-test-with-state mode
          (with-temp-buffer
            (setq last-command-event char-val)
            (nskk-self-insert 1)
            (setq result-1 (buffer-string))))
        (nskk-input-routing-test-with-state mode
          (with-temp-buffer
            (setq last-command-event char-val)
            (nskk-self-insert 1)
            (setq result-2 (buffer-string))))
        ;; Property: both runs must produce identical output
        (string= result-1 result-2))))
  60)


(provide 'nskk-input-routing-pbt-test)

;;; nskk-input-routing-pbt-test.el ends here
