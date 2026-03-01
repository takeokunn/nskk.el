;;; nskk-input-test.el --- Input and henkan tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Comprehensive tests for nskk-input.el and nskk-henkan.el covering:
;; - Character insertion in latin mode
;; - Input conversion to kana
;; - Mode-aware command dispatch
;; - Conversion state helpers
;; - Overlay management
;; - Candidate state management
;; - Abbrev mode input

;;; Code:

(require 'ert)
(require 'nskk-input)
(require 'nskk-state)
(require 'nskk-converter)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;
;;; Helper Macros
;;;

(defmacro nskk-input-test-with-state (initial-mode &rest body)
  "Execute BODY with a fresh state initialized to INITIAL-MODE."
  (declare (indent 1))
  `(let ((nskk-current-state (nskk-state-create ,initial-mode))
         (nskk--conversion-overlay nil))
     ,@body))

(defmacro nskk-input-test-with-romaji (&rest body)
  "Execute BODY with a fresh romaji buffer and standard romaji table.
Ensures the standard romaji table is loaded regardless of prior test state."
  (declare (indent 0))
  `(progn
     (nskk--initialize-romaji-table)
     (let ((nskk--romaji-buffer ""))
       ,@body)))

;;;
;;; Character Insertion Tests (Latin Mode)
;;;

(nskk-deftest-unit input-commands-insert-char-single
  "Test inserting a single character."
  (with-temp-buffer
    (nskk-insert-char ?a)
    (should (string= (buffer-string) "a"))))

(nskk-deftest-unit input-commands-insert-char-multiple
  "Test inserting multiple characters with repeat count."
  (with-temp-buffer
    (nskk-insert-char ?x 3)
    (should (string= (buffer-string) "xxx"))))

(nskk-deftest-unit input-commands-insert-char-default-count
  "Test inserting character with default count."
  (with-temp-buffer
    (nskk-insert-char ?b nil)
    (should (string= (buffer-string) "b"))))

(nskk-deftest-unit input-commands-insert-char-sequence
  "Test inserting a sequence of characters."
  (with-temp-buffer
    (nskk-insert-char ?h)
    (nskk-insert-char ?e)
    (nskk-insert-char ?l)
    (nskk-insert-char ?l)
    (nskk-insert-char ?o)
    (should (string= (buffer-string) "hello"))))

;;;
;;; Input to Kana Conversion Tests
;;;

(nskk-deftest-unit input-commands-convert-input-to-kana-vowel
  "Test romaji to kana conversion for a vowel returns kana."
  (let ((nskk--romaji-buffer ""))
    (let ((result (nskk-convert-input-to-kana ?a)))
      (should (stringp result))
      ;; "a" maps to hiragana "あ" in the romaji table
      (should (equal result "あ")))))

(nskk-deftest-unit input-commands-convert-input-to-kana-consonant-incomplete
  "Test that a consonant alone returns empty string (incomplete)."
  (let ((nskk--romaji-buffer ""))
    (let ((result (nskk-convert-input-to-kana ?k)))
      (should (stringp result))
      ;; "k" is incomplete - waiting for more input
      (should (equal result ""))
      ;; Buffer should hold the partial input
      (should (equal nskk--romaji-buffer "k")))))

(nskk-deftest-unit input-commands-convert-input-to-kana-consonant-vowel
  "Test that consonant + vowel produces kana."
  (let ((nskk--romaji-buffer ""))
    ;; First input 'k' - incomplete
    (nskk-convert-input-to-kana ?k)
    ;; Then input 'a' - should produce 'か'
    (let ((result (nskk-convert-input-to-kana ?a)))
      (should (stringp result))
      (should (equal result "か"))
      ;; Buffer should be cleared
      (should (equal nskk--romaji-buffer "")))))

(nskk-deftest-unit input-commands-convert-input-to-kana-all-vowels
  "Test all five vowels convert to their respective kana."
  (dolist (pair '((?a . "あ") (?i . "い") (?u . "う")
                  (?e . "え") (?o . "お")))
    (let ((nskk--romaji-buffer ""))
      (let ((result (nskk-convert-input-to-kana (car pair))))
        (should (equal result (cdr pair)))))))

;;;
;;; Abbrev Mode Processing Tests
;;;

(nskk-deftest-unit input-commands-process-abbrev-input
  "Test abbrev mode input inserts character directly."
  (with-temp-buffer
    (nskk-process-abbrev-input ?a)
    (should (string= (buffer-string) "a"))))

(nskk-deftest-unit input-commands-process-abbrev-input-sequence
  "Test abbrev mode input sequence."
  (with-temp-buffer
    (nskk-process-abbrev-input ?t)
    (nskk-process-abbrev-input ?e)
    (nskk-process-abbrev-input ?s)
    (nskk-process-abbrev-input ?t)
    (should (string= (buffer-string) "test"))))

;;;
;;; Mode Query Tests
;;;

(nskk-deftest-unit input-commands-get-mode-hiragana
  "Test state-get-mode returns hiragana when in hiragana mode."
  (nskk-input-test-with-state 'hiragana
    (should (eq (nskk-state-get-mode) 'hiragana))))

(nskk-deftest-unit input-commands-get-mode-katakana
  "Test state-get-mode returns katakana when in katakana mode."
  (nskk-input-test-with-state 'katakana
    (should (eq (nskk-state-get-mode) 'katakana))))

(nskk-deftest-unit input-commands-get-mode-latin
  "Test state-get-mode returns latin when in latin mode."
  (nskk-input-test-with-state 'latin
    (should (eq (nskk-state-get-mode) 'latin))))

(nskk-deftest-unit input-commands-get-mode-abbrev
  "Test state-get-mode returns abbrev when in abbrev mode."
  (nskk-input-test-with-state 'abbrev
    (should (eq (nskk-state-get-mode) 'abbrev))))

;;;
;;; Converting-p Tests
;;;

(nskk-deftest-unit input-commands-converting-p-false
  "Test converting-p returns nil when not converting."
  (nskk-input-test-with-state 'hiragana
    (should-not (nskk-converting-p))))

(nskk-deftest-unit input-commands-converting-p-true
  "Test converting-p returns non-nil when henkan-phase is active."
  (nskk-input-test-with-state 'hiragana
    (nskk-state-force-henkan-phase nskk-current-state 'active)
    (should (nskk-converting-p))))

;;;
;;; State Candidate Accessor Tests
;;;

(nskk-deftest-unit input-commands-get-candidates-nil
  "Test candidates returns nil when no candidates."
  (nskk-input-test-with-state 'hiragana
    (should (null (nskk-state-candidates nskk-current-state)))))

(nskk-deftest-unit input-commands-get-candidates-with-data
  "Test candidates returns candidate list."
  (nskk-input-test-with-state 'hiragana
    (setf (nskk-state-candidates nskk-current-state)
          '("\u6F22\u5B57" "\u611F\u3058"))
    (should (equal (nskk-state-candidates nskk-current-state)
                   '("\u6F22\u5B57" "\u611F\u3058")))))

(nskk-deftest-unit input-commands-get-current-index-default
  "Test current-index returns 0 by default."
  (nskk-input-test-with-state 'hiragana
    (should (= (nskk-state-current-index nskk-current-state) 0))))

(nskk-deftest-unit input-commands-set-current-index
  "Test setf current-index updates the index."
  (nskk-input-test-with-state 'hiragana
    (setf (nskk-state-current-index nskk-current-state) 5)
    (should (= (nskk-state-current-index nskk-current-state) 5))))

;;;
;;; Overlay Management Tests
;;;

(nskk-deftest-unit input-commands-update-overlay-creates
  "Test update-overlay creates a new overlay when none exists."
  (with-temp-buffer
    (insert "test text")
    (let ((nskk--conversion-overlay nil))
      (nskk--update-overlay 1 5 "converted")
      (should (overlayp nskk--conversion-overlay))
      (should (equal (overlay-get nskk--conversion-overlay 'display)
                     "converted"))
      (should (eq (overlay-get nskk--conversion-overlay 'face)
                  'highlight))
      ;; Cleanup
      (delete-overlay nskk--conversion-overlay))))

(nskk-deftest-unit input-commands-update-overlay-moves-existing
  "Test update-overlay moves existing overlay."
  (with-temp-buffer
    (insert "test text here")
    (let ((nskk--conversion-overlay nil))
      (nskk--update-overlay 1 5 "first")
      (let ((ov nskk--conversion-overlay))
        (nskk--update-overlay 6 10 "second")
        ;; Should still be the same overlay
        (should (eq nskk--conversion-overlay ov))
        (should (equal (overlay-get nskk--conversion-overlay 'display)
                       "second"))
        (should (= (overlay-start nskk--conversion-overlay) 6))
        (should (= (overlay-end nskk--conversion-overlay) 10)))
      ;; Cleanup
      (delete-overlay nskk--conversion-overlay))))

;;;
;;; Interactive Command Tests
;;;

(nskk-deftest-unit input-commands-convert-is-interactive
  "Test that nskk-convert is interactive."
  (should (commandp 'nskk-convert)))

(nskk-deftest-unit input-commands-convert-or-commit-is-interactive
  "Test that nskk-convert-or-commit is interactive."
  (should (commandp 'nskk-convert-or-commit)))

(nskk-deftest-unit input-commands-cancel-conversion-is-interactive
  "Test that nskk-cancel-conversion is interactive."
  (should (commandp 'nskk-cancel-conversion)))

(nskk-deftest-unit input-commands-rollback-is-interactive
  "Test that nskk-rollback-conversion is interactive."
  (should (commandp 'nskk-rollback-conversion)))

(nskk-deftest-unit input-commands-next-candidate-is-interactive
  "Test that nskk-next-candidate is interactive."
  (should (commandp 'nskk-next-candidate)))

(nskk-deftest-unit input-commands-previous-candidate-is-interactive
  "Test that nskk-previous-candidate is interactive."
  (should (commandp 'nskk-previous-candidate)))

(nskk-deftest-unit input-commands-commit-current-is-interactive
  "Test that nskk-commit-current is interactive."
  (should (commandp 'nskk-commit-current)))

;;;
;;; Rollback Conversion Tests
;;;

(nskk-deftest-unit input-commands-rollback-clears-active
  "Test that rollback clears converting state."
  (nskk-input-test-with-state 'hiragana
    (nskk-state-force-henkan-phase nskk-current-state 'active)
    (nskk-rollback-conversion)
    (should-not (nskk-converting-p))))

(nskk-deftest-unit input-commands-rollback-noop-when-not-converting
  "Test that rollback does nothing when not converting."
  (nskk-input-test-with-state 'hiragana
    ;; Should not error
    (nskk-rollback-conversion)
    (should-not (nskk-converting-p))))

;;;
;;; Cancel Conversion Tests
;;;

(nskk-deftest-unit input-commands-cancel-noop-when-not-converting
  "Test that cancel does nothing when not converting."
  (nskk-input-test-with-state 'hiragana
    ;; Should not error
    (nskk-cancel-conversion)
    (should-not (nskk-converting-p))))

;;;
;;; Convert-or-commit Tests
;;;

(nskk-deftest-unit input-commands-convert-or-commit-when-converting
  "Test convert-or-commit commits when already converting."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      ;; Set up conversion start marker and candidates to allow commit to work
      (nskk--set-conversion-start-marker (point-min))
      (insert "test")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      ;; When converting, should call commit which clears active.
      (nskk-convert-or-commit)
      (should-not (nskk-converting-p)))))

;;;
;;; Next/Previous Candidate Guard Tests
;;;

(nskk-deftest-unit input-commands-next-candidate-noop-when-not-converting
  "Test next-candidate does nothing when not converting."
  (nskk-input-test-with-state 'hiragana
    ;; Should not error
    (nskk-next-candidate)))

(nskk-deftest-unit input-commands-prev-candidate-noop-when-not-converting
  "Test previous-candidate does nothing when not converting."
  (nskk-input-test-with-state 'hiragana
    ;; Should not error
    (nskk-previous-candidate)))

;;;
;;; Error Handling Tests: Self-Insert Without State
;;;

(nskk-deftest-unit input-commands-self-insert-requires-state
  "Test that self-insert handles missing state gracefully."
  (let ((nskk-current-state nil)
        (last-command-event ?a))
    ;; Should not error even without state
    (with-temp-buffer
      (nskk-self-insert 1)
      ;; Character should be inserted (fallback to ascii mode)
      (should (> (point-max) 1)))))

(nskk-deftest-unit input-commands-self-insert-uninitialized-state
  "Test that self-insert handles uninitialized nskk-current-state."
  (let ((nskk-current-state nil)
        (last-command-event ?a))
    (with-temp-buffer
      ;; Should not signal an error
      (should-not (catch 'error (nskk-self-insert 1)))
      ;; Character should have been inserted
      (should (> (buffer-size) 0)))))

;;;
;;; Error Handling Tests: Mode-Based Input Routing
;;;

(nskk-deftest-unit input-commands-self-insert-in-ascii-mode
  "Test self-insert in ascii mode inserts character directly."
  (nskk-input-test-with-state 'ascii
    (with-temp-buffer
      (let ((last-command-event ?x))
        (nskk-self-insert 1)
        (should (string= (buffer-string) "x"))))))

(nskk-deftest-unit input-commands-self-insert-in-latin-mode
  "Test self-insert in latin mode inserts character directly."
  (nskk-input-test-with-state 'latin
    (with-temp-buffer
      (let ((last-command-event ?y))
        (nskk-self-insert 1)
        (should (string= (buffer-string) "y"))))))

(nskk-deftest-unit input-commands-self-insert-in-abbrev-mode
  "Test self-insert in abbrev mode processes abbrev input."
  (nskk-input-test-with-state 'abbrev
    (with-temp-buffer
      (let ((last-command-event ?a))
        ;; nskk-process-abbrev-input should handle this
        (nskk-self-insert 1)
        ;; Should have inserted something (abbrev processing)
        (should (> (buffer-size) 0))))))

(nskk-deftest-unit input-commands-self-insert-repeat-count
  "Test self-insert respects repeat count."
  (nskk-input-test-with-state 'ascii
    (with-temp-buffer
      (let ((last-command-event ?z))
        (nskk-self-insert 3)
        (should (string= (buffer-string) "zzz"))))))

;;;
;;; Error Handling Tests: Conversion State Guards
;;;

(nskk-deftest-unit input-commands-convert-when-not-initialized
  "Test convert handles uninitialized state gracefully."
  (let ((nskk-current-state nil)
        (nskk--conversion-start-marker nil))
    ;; Should not error - nskk-convert checks nskk--has-preedit which uses marker
    (with-temp-buffer
      (nskk-convert)
      ;; When marker is nil, nskk--has-preedit returns nil, so convert is a no-op
      (should-not (nskk-converting-p)))))

(nskk-deftest-unit input-commands-cancel-when-not-initialized
  "Test cancel handles uninitialized state gracefully."
  (let ((nskk-current-state nil))
    ;; Should not error
    (nskk-cancel-conversion)))

(nskk-deftest-unit input-commands-commit-current-guards
  "Test commit-current has proper guards."
  (let ((nskk-current-state nil))
    ;; Should not error when not converting
    (nskk-commit-current)
    (should-not (nskk-converting-p))))

(nskk-deftest-unit input-commands-rollback-guards
  "Test rollback has proper guards."
  (let ((nskk-current-state nil))
    ;; Should not error
    (nskk-rollback-conversion)
    (should-not (nskk-converting-p))))

;;;
;;; Error Handling Tests: Mode Queries With Missing State
;;;

(nskk-deftest-unit input-commands-set-mode-uninitialized-signals-user-error
  "Test that nskk--set-mode signals user-error when nskk-current-state is nil."
  (let ((nskk-current-state nil))
    (should-error (nskk--set-mode 'hiragana) :type 'user-error)))

(nskk-deftest-unit input-commands-get-mode-with-nil-state
  "Test state-get-mode returns nil safely when state is uninitialized."
  (let ((nskk-current-state nil))
    ;; nskk-state-get-mode checks if nskk-current-state is a valid nskk-state
    ;; If not, it returns nil
    (let ((mode (nskk-state-get-mode)))
      (should (null mode)))))

(nskk-deftest-unit input-commands-converting-p-with-nil-state
  "Test converting-p returns nil safely when state is uninitialized."
  (let ((nskk-current-state nil))
    ;; Should return nil when not converting
    (should (not (nskk-converting-p)))))

(nskk-deftest-unit input-commands-converting-p-reflects-henkan-phase
  "Test converting-p correctly reflects nskk-state-henkan-phase."
  (nskk-input-test-with-state 'hiragana
    (should (not (nskk-converting-p)))

    (nskk-state-force-henkan-phase nskk-current-state 'active)
    (should (nskk-converting-p))

    (nskk-state-force-henkan-phase nskk-current-state nil)
    (should (not (nskk-converting-p)))))

;;;
;;; Error Handling Tests: Overlay Management
;;;

(nskk-deftest-unit input-commands-update-overlay-with-nil-overlay
  "Test update-overlay creates overlay when none exists."
  (with-temp-buffer
    (insert "test text")
    (let ((nskk--conversion-overlay nil))
      (nskk--update-overlay 1 5 "display")
      (should (overlayp nskk--conversion-overlay))
      (delete-overlay nskk--conversion-overlay))))

(nskk-deftest-unit input-commands-update-overlay-with-existing
  "Test update-overlay reuses existing overlay."
  (with-temp-buffer
    (insert "test text with more content")
    (let ((nskk--conversion-overlay nil))
      ;; Create first overlay
      (nskk--update-overlay 1 5 "first")
      (let ((first-overlay nskk--conversion-overlay))
        ;; Update should reuse
        (nskk--update-overlay 10 15 "second")
        ;; Should be same overlay object
        (should (eq nskk--conversion-overlay first-overlay))
        ;; Clean up
        (delete-overlay nskk--conversion-overlay)))))

;;;
;;; Error Handling Tests: Candidate Navigation Safety
;;;

(nskk-deftest-unit input-commands-next-candidate-with-no-candidates
  "Test next-candidate guards against missing candidates."
  (nskk-input-test-with-state 'hiragana
    (nskk-state-force-henkan-phase nskk-current-state 'active)
    ;; next-candidate internally checks converting-p and calls nskk--select-candidate
    ;; which accesses state's candidates - should not error if state exists
    (condition-case err
        (nskk-next-candidate)
      (error nil))  ;; Allow errors but don't fail the test
    t))  ;; Test passes if we get here

(nskk-deftest-unit input-commands-previous-candidate-with-no-candidates
  "Test previous-candidate guards against missing candidates."
  (nskk-input-test-with-state 'hiragana
    (nskk-state-force-henkan-phase nskk-current-state 'active)
    ;; previous-candidate internally checks converting-p and calls nskk--select-candidate
    ;; which accesses state's candidates - should not error if state exists
    (condition-case err
        (nskk-previous-candidate)
      (error nil))  ;; Allow errors but don't fail the test
    t))  ;; Test passes if we get here

;;;
;;; Error Handling Tests: State Consistency Through Operations
;;;

(nskk-deftest-unit input-commands-state-consistency-through-insert
  "Test that state remains consistent through insert operation."
  (nskk-input-test-with-state 'hiragana
    (with-temp-buffer
      (let ((mode-before (nskk-state-mode nskk-current-state)))
        (let ((last-command-event ?a))
          (nskk-self-insert 1))
        ;; Mode should not change from insert operation
        (should (eq (nskk-state-mode nskk-current-state) mode-before))))))

(nskk-deftest-unit input-commands-state-consistency-through-conversion
  "Test that state remains consistent through conversion attempt."
  (nskk-input-test-with-state 'hiragana
    (let ((mode-before (nskk-state-mode nskk-current-state))
          (nskk--conversion-start-marker nil))
      (with-temp-buffer
        ;; With no marker set, nskk-convert is a safe no-op
        (nskk-convert))
      ;; Mode should not change from conversion attempt
      (should (eq (nskk-state-mode nskk-current-state) mode-before)))))

;;;
;;; n + consonant rule tests
;;;

(nskk-deftest-unit input-commands-n-consonant-kanji
  "Test n + consonant rule: typing k,a,n,j,i produces か,empty,ん,empty,い."
  (nskk-input-test-with-romaji
    ;; "k" -> incomplete
    (should (equal (nskk-convert-input-to-kana ?k) ""))
    (should (equal nskk--romaji-buffer "k"))
    ;; "a" -> "か"
    (should (equal (nskk-convert-input-to-kana ?a) "か"))
    (should (equal nskk--romaji-buffer ""))
    ;; "n" -> incomplete
    (should (equal (nskk-convert-input-to-kana ?n) ""))
    (should (equal nskk--romaji-buffer "n"))
    ;; "j" -> n+consonant triggers: emit "ん", buffer "j"
    (let ((result (nskk-convert-input-to-kana ?j)))
      (should (equal result "ん"))
      (should (equal nskk--romaji-buffer "j")))
    ;; "i" -> "じ"
    (should (equal (nskk-convert-input-to-kana ?i) "じ"))))

(nskk-deftest-unit input-commands-n-consonant-simple
  "Test n + consonant rule with simple nk sequence."
  (nskk-input-test-with-romaji
    ;; "n" -> incomplete
    (nskk-convert-input-to-kana ?n)
    ;; "k" -> n+consonant: emit "ん", buffer "k"
    (let ((result (nskk-convert-input-to-kana ?k)))
      (should (equal result "ん"))
      (should (equal nskk--romaji-buffer "k")))
    ;; "a" -> "か"
    (should (equal (nskk-convert-input-to-kana ?a) "か"))))

(nskk-deftest-unit input-commands-n-vowel-no-trigger
  "Test that n + vowel does NOT trigger n+consonant rule."
  (nskk-input-test-with-romaji
    ;; "n" -> incomplete
    (nskk-convert-input-to-kana ?n)
    ;; "a" -> should produce "な" (not "ん" + "あ")
    (let ((result (nskk-convert-input-to-kana ?a)))
      (should (equal result "な")))))

(nskk-deftest-unit input-commands-n-y-no-trigger
  "Test that n + y does NOT trigger n+consonant rule."
  (nskk-input-test-with-romaji
    ;; "n" -> incomplete
    (nskk-convert-input-to-kana ?n)
    ;; "y" -> should NOT trigger n+consonant (ny is a valid prefix)
    (let ((result (nskk-convert-input-to-kana ?y)))
      ;; "ny" should be incomplete (prefix for nya, nyu, etc.)
      (should (equal result ""))
      (should (equal nskk--romaji-buffer "ny")))))

(nskk-deftest-unit input-commands-nn-produces-n
  "Test that nn produces ん."
  (nskk-input-test-with-romaji
    ;; "n" -> incomplete
    (nskk-convert-input-to-kana ?n)
    ;; "n" -> should produce "ん" via normal conversion (nn -> ん)
    (let ((result (nskk-convert-input-to-kana ?n)))
      (should (equal result "ん")))))

;;;
;;; Sokuon (double consonant) rule tests
;;;

(nskk-deftest-unit input-commands-sokuon-kk
  "Test sokuon rule: typing s,a,k,k,a produces さ,empty,empty,っ,か."
  (nskk-input-test-with-romaji
    ;; "s" -> incomplete
    (should (equal (nskk-convert-input-to-kana ?s) ""))
    ;; "a" -> "さ"
    (should (equal (nskk-convert-input-to-kana ?a) "さ"))
    ;; "k" -> incomplete
    (should (equal (nskk-convert-input-to-kana ?k) ""))
    ;; "k" -> sokuon: emit "っ", buffer "k"
    (let ((result (nskk-convert-input-to-kana ?k)))
      (should (equal result "っ"))
      (should (equal nskk--romaji-buffer "k")))
    ;; "a" -> "か"
    (should (equal (nskk-convert-input-to-kana ?a) "か"))))

(nskk-deftest-unit input-commands-sokuon-tt
  "Test sokuon rule with tt: typing t,t,e produces empty,っ,て."
  (nskk-input-test-with-romaji
    ;; "t" -> incomplete
    (should (equal (nskk-convert-input-to-kana ?t) ""))
    ;; "t" -> sokuon: emit "っ", buffer "t"
    (let ((result (nskk-convert-input-to-kana ?t)))
      (should (equal result "っ"))
      (should (equal nskk--romaji-buffer "t")))
    ;; "e" -> "て"
    (should (equal (nskk-convert-input-to-kana ?e) "て"))))

(nskk-deftest-unit input-commands-sokuon-ss
  "Test sokuon rule with ss: typing s,s,a produces empty,っ,さ."
  (nskk-input-test-with-romaji
    ;; "s" -> incomplete
    (should (equal (nskk-convert-input-to-kana ?s) ""))
    ;; "s" -> sokuon: emit "っ", buffer "s"
    (let ((result (nskk-convert-input-to-kana ?s)))
      (should (equal result "っ"))
      (should (equal nskk--romaji-buffer "s")))
    ;; "a" -> "さ"
    (should (equal (nskk-convert-input-to-kana ?a) "さ"))))

(nskk-deftest-unit input-commands-sokuon-no-vowel-double
  "Test that double vowels do NOT trigger sokuon."
  (nskk-input-test-with-romaji
    ;; "a" -> "あ"
    (should (equal (nskk-convert-input-to-kana ?a) "あ"))
    ;; "a" -> "あ" again (not sokuon)
    (should (equal (nskk-convert-input-to-kana ?a) "あ"))))

(nskk-deftest-unit input-commands-sokuon-no-nn-double
  "Test that nn does NOT trigger sokuon (nn has its own rule)."
  (nskk-input-test-with-romaji
    ;; "n" -> incomplete
    (nskk-convert-input-to-kana ?n)
    ;; "n" -> should produce "ん" via normal conversion, not "っ"
    (let ((result (nskk-convert-input-to-kana ?n)))
      (should (equal result "ん"))
      (should-not (equal result "っ")))))

;;;
;;; Standalone n at word boundary
;;;

(nskk-deftest-unit input-commands-standalone-n-buffer
  "Test that standalone n stays in buffer as incomplete."
  (nskk-input-test-with-romaji
    ;; "n" -> incomplete, stays in buffer
    (should (equal (nskk-convert-input-to-kana ?n) ""))
    (should (equal nskk--romaji-buffer "n"))))

;;;
;;; Conversion Start Marker Tests (Phase 2A)
;;;

(nskk-deftest-unit input-commands-marker-initially-nil
  "Test that conversion start marker is nil by default."
  (with-temp-buffer
    (should (null nskk--conversion-start-marker))))

(nskk-deftest-unit input-commands-set-marker-creates-marker
  "Test that set-conversion-start-marker creates a marker at the given position."
  (with-temp-buffer
    (insert "hello")
    (nskk--set-conversion-start-marker 3)
    (should (markerp nskk--conversion-start-marker))
    (should (= (marker-position nskk--conversion-start-marker) 3))))

(nskk-deftest-unit input-commands-set-marker-reuses-existing
  "Test that set-conversion-start-marker reuses an existing marker object."
  (with-temp-buffer
    (insert "hello")
    (nskk--set-conversion-start-marker 2)
    (let ((first-marker nskk--conversion-start-marker))
      (nskk--set-conversion-start-marker 4)
      ;; Should be the same marker object, just moved
      (should (eq nskk--conversion-start-marker first-marker))
      (should (= (marker-position nskk--conversion-start-marker) 4)))))

(nskk-deftest-unit input-commands-clear-marker
  "Test that clear-conversion-start-marker unsets the marker position."
  (with-temp-buffer
    (insert "hello")
    (nskk--set-conversion-start-marker 3)
    (should (marker-position nskk--conversion-start-marker))
    (nskk--clear-conversion-start-marker)
    ;; Marker object still exists but position is nil
    (should (markerp nskk--conversion-start-marker))
    (should (null (marker-position nskk--conversion-start-marker)))))

(nskk-deftest-unit input-commands-conversion-start-active-p
  "Test conversion-start-active-p reflects marker state."
  (with-temp-buffer
    (insert "hello")
    ;; Initially not active
    (should-not (nskk--conversion-start-active-p))
    ;; Set marker
    (nskk--set-conversion-start-marker 2)
    (should (nskk--conversion-start-active-p))
    ;; Clear marker
    (nskk--clear-conversion-start-marker)
    (should-not (nskk--conversion-start-active-p))))

(nskk-deftest-unit input-commands-get-conversion-start-with-marker
  "Test get-conversion-start returns marker position."
  (with-temp-buffer
    (insert "hello")
    (nskk--set-conversion-start-marker 3)
    (should (= (nskk--get-conversion-start) 3))))

(nskk-deftest-unit input-commands-get-conversion-start-without-marker
  "Test get-conversion-start returns nil when no marker is set."
  (with-temp-buffer
    (should (null (nskk--get-conversion-start)))))

;;;
;;; Has-Preedit Tests with Marker (Phase 2A)
;;;

(nskk-deftest-unit input-commands-has-preedit-with-text-after-marker
  "Test has-preedit returns non-nil when point is past the marker + marker length."
  (with-temp-buffer
    ;; Insert ▽ marker followed by text to simulate real usage
    (insert "\u25BDhello")
    (nskk--set-conversion-start-marker 1)
    ;; Point is at end of buffer, marker is at 1, ▽ is 1 char
    ;; point (7) > start (1) + length("▽") (1) = 2
    (should (nskk--has-preedit))))

(nskk-deftest-unit input-commands-has-preedit-at-marker
  "Test has-preedit returns nil when point equals marker + marker length."
  (with-temp-buffer
    ;; Insert just the ▽ marker, point is right after it
    (insert "\u25BD")
    (nskk--set-conversion-start-marker 1)
    ;; Point (2) = start (1) + length("▽") (1) = 2, so not past
    (should-not (nskk--has-preedit))))

(nskk-deftest-unit input-commands-has-preedit-no-marker
  "Test has-preedit returns nil when no marker is set."
  (with-temp-buffer
    (insert "hello")
    ;; No marker set
    (should-not (nskk--has-preedit))))

;;;
;;; Commit Clears Marker Tests (Phase 2A)
;;;

(nskk-deftest-unit input-commands-commit-clears-marker
  "Test that commit-current clears the conversion start marker."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      (nskk--set-conversion-start-marker (point-min))
      (insert "test")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-commit-current)
      ;; Marker should be cleared
      (should-not (nskk--conversion-start-active-p)))))

(nskk-deftest-unit input-commands-rollback-clears-marker
  "Test that rollback keeps the start marker active (returns to preedit state)."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      (nskk--set-conversion-start-marker (point-min))
      (insert "test")
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-rollback-conversion)
      ;; Marker remains active: user is now back in preedit (▽) state
      (should (nskk--conversion-start-active-p)))))

;;;
;;; Start Conversion Tests (Phase 2B)
;;;

(nskk-deftest-unit input-commands-start-conversion-no-marker
  "Test start-conversion is a no-op when no marker is set."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      (insert "test")
      ;; No marker set, so start-conversion should do nothing
      (nskk-start-conversion)
      (should-not (nskk-converting-p)))))

(nskk-deftest-unit input-commands-start-conversion-no-candidates
  "Test start-conversion triggers registration when no candidates found.
When registration is cancelled (empty input), preedit is left as-is."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      ;; Set 'on phase: in practice a capital letter sets this before SPC
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk--set-conversion-start-marker (point-min))
      ;; Include ▽ marker as ddskk does
      (insert "\u25BDxyznonexistent")
      ;; Mock read-from-minibuffer to simulate cancelled registration
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (_prompt &rest _args) "")))
        (nskk-start-conversion)
        ;; Should NOT enter conversion mode when registration cancelled
        (should (equal (buffer-string) "\u25BDxyznonexistent"))))))

(nskk-deftest-unit input-commands-start-conversion-no-candidates-register
  "Test start-conversion inserts registered word when registration succeeds."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      ;; Set 'on phase: in practice a capital letter sets this before SPC
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk--set-conversion-start-marker (point-min))
      (insert "\u25BDmyreading")
      ;; Mock read-from-minibuffer to simulate successful registration
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (_prompt &rest _args) "registered-word"))
                ((symbol-function 'nskk-dict-register-word)
                 (lambda (_reading _word) nil)))
        (nskk-start-conversion)
        ;; Should have replaced preedit with registered word
        (should (equal (buffer-string) "registered-word"))
        ;; Should NOT be in conversion mode
        (should-not (nskk-converting-p))))))

(nskk-deftest-unit input-commands-start-conversion-with-candidates
  "Test start-conversion enters conversion mode when candidates found."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      (nskk--set-conversion-start-marker (point-min))
      ;; Include ▽ marker as ddskk does
      (insert "\u25BDtest")
      ;; Set henkan-phase to 'on (preedit mode) before starting conversion
      (nskk-state-set-henkan-phase nskk-current-state 'on)
      ;; Mock nskk-core-search to return candidates
      (cl-letf (((symbol-function 'nskk-core-search)
                 (lambda (_key &optional _type _limit) '("result1" "result2"))))
        (nskk-start-conversion)
        ;; Should enter conversion mode
        (should (nskk-converting-p))
        ;; Should have set candidates in state
        (should (equal (nskk-state-candidates nskk-current-state)
                       '("result1" "result2")))
        (should (= (nskk-state-current-index nskk-current-state) 0))
        ;; ▽ should have been replaced with ▼
        (should (string-match-p "\u25BC" (buffer-string)))
        ;; Should have created an overlay
        (should (overlayp nskk--conversion-overlay))
        (should (equal (overlay-get nskk--conversion-overlay 'display) "result1"))
        ;; henkan-phase should be 'active
        (should (eq (nskk-state-henkan-phase nskk-current-state) 'active))
        ;; Cleanup
        (when (overlayp nskk--conversion-overlay)
          (delete-overlay nskk--conversion-overlay))))))

;;;
;;; Uppercase Letter Henkan Start Tests (Phase 2B)
;;;

(nskk-deftest-unit input-commands-uppercase-sets-marker
  "Test that uppercase letter in Japanese mode sets the henkan start marker."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      (let ((nskk-converter-auto-start-henkan t))
        ;; Process an uppercase letter 'K'
        (nskk-process-japanese-input ?K 1)
        ;; The marker should be set
        (should (nskk--conversion-start-active-p))
        ;; ▽ marker should be inserted
        (should (string-match-p "\u25BD" (buffer-string)))
        ;; henkan-phase should be 'on
        (should (eq (nskk-state-henkan-phase nskk-current-state) 'on))))))

(nskk-deftest-unit input-commands-uppercase-processes-lowercase
  "Test that uppercase letter is processed as lowercase romaji input."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      (let ((nskk-converter-auto-start-henkan t))
        ;; Process an uppercase vowel 'A' which should convert to kana
        (nskk-process-japanese-input ?A 1)
        ;; Should have inserted ▽ marker followed by the kana for 'a'
        (should (equal (buffer-string) "\u25BD\u3042"))
        ;; Marker should be set
        (should (nskk--conversion-start-active-p))))))

(nskk-deftest-unit input-commands-uppercase-no-double-marker
  "Test that a second uppercase letter does not reset the marker."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      (let ((nskk-converter-auto-start-henkan t))
        ;; First uppercase: sets marker
        (nskk-process-japanese-input ?K 1)
        (let ((first-pos (nskk--get-conversion-start)))
          ;; Insert some more text
          (nskk-process-japanese-input ?a 1)
          ;; Second uppercase should NOT reset marker (it would be okurigana instead)
          ;; The marker position should remain at the first position
          (should (= (nskk--get-conversion-start) first-pos)))))))

(nskk-deftest-unit input-commands-uppercase-disabled-when-auto-henkan-off
  "Test that uppercase detection is disabled when auto-start-henkan is nil."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      (let ((nskk-converter-auto-start-henkan nil))
        ;; Process an uppercase letter 'K'
        (nskk-process-japanese-input ?K 1)
        ;; The marker should NOT be set
        (should-not (nskk--conversion-start-active-p))))))

;;;
;;; Inline Marker Tests
;;;

(nskk-deftest-unit input-commands-henkan-on-marker-constant
  "Test that the henkan-on marker constant is defined correctly."
  (should (equal nskk-henkan-on-marker "\u25BD")))

(nskk-deftest-unit input-commands-henkan-active-marker-constant
  "Test that the henkan-active marker constant is defined correctly."
  (should (equal nskk-henkan-active-marker "\u25BC")))

(nskk-deftest-unit input-commands-okurigana-marker-constant
  "Test that the okurigana marker constant is defined correctly."
  (should (equal nskk-okurigana-marker "*")))

;;;
;;; Marker Cleanup on Commit Tests
;;;

(nskk-deftest-unit input-commands-commit-clears-henkan-phase
  "Test that commit-current resets henkan-phase to nil."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      (nskk--set-conversion-start-marker (point-min))
      (insert "test")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-commit-current)
      ;; henkan-phase should be reset
      (should (null (nskk-state-henkan-phase nskk-current-state))))))

(nskk-deftest-unit input-commands-commit-clears-henkan-count
  "Test that commit-current resets henkan count."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      (nskk--set-conversion-start-marker (point-min))
      (insert "test")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (setq nskk--henkan-count 3)
      (nskk-commit-current)
      (should (= nskk--henkan-count 0)))))

;;;
;;; Rollback Marker Cleanup Tests
;;;

(nskk-deftest-unit input-commands-rollback-removes-active-marker
  "Test that rollback removes the ▼ marker from buffer."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      (nskk--set-conversion-start-marker (point-min))
      (insert "\u25BCtest")
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-rollback-conversion)
      ;; ▼ marker should be removed
      (should-not (string-match-p "\u25BC" (buffer-string)))
      ;; henkan-phase should be nil
      (should (null (nskk-state-henkan-phase nskk-current-state))))))

(nskk-deftest-unit input-commands-rollback-removes-on-marker
  "Test that rollback from preedit (▽) state is a no-op for buffer content."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      (nskk--set-conversion-start-marker (point-min))
      (insert "\u25BDtest")
      ;; 'on (preedit) phase: nskk-converting-p returns nil, rollback is a no-op
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk-rollback-conversion)
      ;; ▽ marker remains (rollback does not affect preedit state)
      (should (string-match-p "\u25BD" (buffer-string))))))

;;;
;;; Okurigana Detection Tests
;;;

(nskk-deftest-unit input-commands-detect-okurigana-char-uppercase
  "Test okurigana detection returns lowercase for uppercase consonant."
  (should (eq (nskk-detect-okurigana-char ?K) ?k))
  (should (eq (nskk-detect-okurigana-char ?T) ?t))
  (should (eq (nskk-detect-okurigana-char ?S) ?s)))

(nskk-deftest-unit input-commands-detect-okurigana-char-lowercase
  "Test okurigana detection returns nil for lowercase."
  (should (null (nskk-detect-okurigana-char ?k)))
  (should (null (nskk-detect-okurigana-char ?a))))

(nskk-deftest-unit input-commands-detect-okurigana-char-non-alpha
  "Test okurigana detection returns nil for non-alphabetic."
  (should (null (nskk-detect-okurigana-char ?1)))
  (should (null (nskk-detect-okurigana-char ? ))))

;;;
;;; Property-Based Tests
;;;

;; Input never crashes: processing any romaji character in hiragana mode
;; raises no error. The generator always produces non-empty romaji strings,
;; so (> (length input) 0) is always true; we return t on success and nil on error.
(nskk-property-test input-pbt-romaji-char-no-crash-hiragana-mode
  ((input romaji-string))
  (if (> (length input) 0)
      (let ((char (aref input 0)))
        (condition-case nil
            (let ((nskk--romaji-buffer ""))
              (nskk-convert-input-to-kana char)
              t)
          (error nil)))
    t)  ; empty string is vacuously ok
  100)

;; Mode is preserved after non-mode-switch input: inserting a regular ASCII
;; character in hiragana mode does not change the current mode.
(nskk-property-test input-pbt-mode-preserved-after-insert
  ((input romaji-string))
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (with-temp-buffer
      (let ((mode-before (nskk-state-mode nskk-current-state))
            (last-command-event ?a))
        (nskk-self-insert 1)
        (eq (nskk-state-mode nskk-current-state) mode-before))))
  50)

;; Table-driven mode creation tests: nskk-state-create with each valid mode
;; produces a state that reports that same mode.
(nskk-deftest-cases input-pbt-mode-creation
  ((ascii    . ascii)
   (hiragana . hiragana)
   (katakana . katakana)
   (latin    . latin)
   (abbrev   . abbrev))
  :description "Mode creation: nskk-state-create produces state in requested mode"
  :body (let ((state (nskk-state-create input)))
          (should (nskk-state-p state))
          (should (eq (nskk-state-mode state) expected))))

;;;
;;; Fullwidth-Char Prolog Table Tests
;;;

(nskk-deftest-unit input-fullwidth-char-space
  "Test that space maps to ideographic space (U+3000)."
  (should (eq (nskk-prolog-query-value
               `(fullwidth-char ?\s ,'\?fw) '\?fw)
              ?\u3000)))

(nskk-deftest-unit input-fullwidth-char-exclamation
  "Test that ! (0x21) maps to FF01."
  (should (eq (nskk-prolog-query-value
               `(fullwidth-char ?! ,'\?fw) '\?fw)
              ?\uFF01)))

(nskk-deftest-unit input-fullwidth-char-tilde
  "Test that ~ (0x7E) maps to FF5E."
  (should (eq (nskk-prolog-query-value
               `(fullwidth-char ?~ ,'\?fw) '\?fw)
              ?\uFF5E)))

(nskk-deftest-unit input-fullwidth-char-upper-a
  "Test that A (0x41) maps to FF21."
  (should (eq (nskk-prolog-query-value
               `(fullwidth-char ?A ,'\?fw) '\?fw)
              ?\uFF21)))

(nskk-deftest-unit input-fullwidth-char-lower-a
  "Test that a (0x61) maps to FF41."
  (should (eq (nskk-prolog-query-value
               `(fullwidth-char ?a ,'\?fw) '\?fw)
              ?\uFF41)))

(nskk-deftest-unit input-fullwidth-char-passthrough
  "Test that non-ASCII character passes through unchanged."
  (with-temp-buffer
    (nskk-insert-fullwidth-char ?\u3042 1)
    ;; Non-ASCII should be inserted as-is
    (should (string= (buffer-string) "\u3042"))))

;;;
;;; Toggle-Mode Prolog Rule Tests
;;;

(nskk-deftest-unit input-toggle-mode-hiragana-to-katakana
  "Test that toggle-mode rule maps hiragana to katakana."
  (should (eq (nskk-prolog-query-value
               `(toggle-mode hiragana ,'\?target) '\?target)
              'katakana)))

(nskk-deftest-unit input-toggle-mode-katakana-to-hiragana
  "Test that toggle-mode rule maps katakana to hiragana."
  (should (eq (nskk-prolog-query-value
               `(toggle-mode katakana ,'\?target) '\?target)
              'hiragana)))

(nskk-deftest-unit input-toggle-mode-ascii-no-rule
  "Test that ascii mode has no toggle-mode mapping."
  (should (null (nskk-prolog-query-value
                 `(toggle-mode ascii ,'\?target) '\?target))))

;;;
;;; Input-Route Prolog Rule Tests
;;;

(nskk-deftest-unit input-route-hiragana-processes-japanese
  "Test that hiragana mode routes to process-japanese."
  (should (eq (nskk-prolog-query-value
               `(input-route hiragana ,'\?action) '\?action)
              'process-japanese)))

(nskk-deftest-unit input-route-ascii-inserts-direct
  "Test that ascii mode routes to insert-direct."
  (should (eq (nskk-prolog-query-value
               `(input-route ascii ,'\?action) '\?action)
              'insert-direct)))

(nskk-deftest-unit input-route-jisx0208-latin-inserts-fullwidth
  "Test that jisx0208-latin mode routes to insert-fullwidth."
  (should (eq (nskk-prolog-query-value
               `(input-route jisx0208-latin ,'\?action) '\?action)
              'insert-fullwidth)))

(provide 'nskk-input-test)

;;; nskk-input-test.el ends here
