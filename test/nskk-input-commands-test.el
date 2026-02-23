;;; nskk-input-commands-test.el --- Input commands tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-input-commands.el covering:
;; - Character insertion in latin mode
;; - Input conversion to kana
;; - Mode-aware command dispatch
;; - Conversion state helpers
;; - Overlay management
;; - Candidate state management
;; - Abbrev mode input

;;; Code:

(require 'ert)
(require 'nskk-input-commands)
(require 'nskk-state)
(require 'nskk-test-framework)

;;;
;;; Helper Macros
;;;

(defmacro nskk-input-test-with-state (initial-mode &rest body)
  "Execute BODY with a fresh state initialized to INITIAL-MODE."
  (declare (indent 1))
  `(let ((nskk-current-state (nskk-state-create ,initial-mode))
         (nskk-converting-active nil)
         (nskk--conversion-overlay nil))
     ,@body))

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
  "Test get-mode returns hiragana when in hiragana mode."
  (nskk-input-test-with-state 'hiragana
    (should (eq (nskk-get-mode) 'hiragana))))

(nskk-deftest-unit input-commands-get-mode-katakana
  "Test get-mode returns katakana when in katakana mode."
  (nskk-input-test-with-state 'katakana
    (should (eq (nskk-get-mode) 'katakana))))

(nskk-deftest-unit input-commands-get-mode-latin
  "Test get-mode returns latin when in latin mode."
  (nskk-input-test-with-state 'latin
    (should (eq (nskk-get-mode) 'latin))))

(nskk-deftest-unit input-commands-get-mode-abbrev
  "Test get-mode returns abbrev when in abbrev mode."
  (nskk-input-test-with-state 'abbrev
    (should (eq (nskk-get-mode) 'abbrev))))

;;;
;;; Converting-p Tests
;;;

(nskk-deftest-unit input-commands-converting-p-false
  "Test converting-p returns nil when not converting."
  (nskk-input-test-with-state 'hiragana
    (should-not (nskk-converting-p))))

(nskk-deftest-unit input-commands-converting-p-true
  "Test converting-p returns non-nil when converting."
  (nskk-input-test-with-state 'hiragana
    (setq nskk-converting-active t)
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
  "Test that rollback clears converting-active."
  (nskk-input-test-with-state 'hiragana
    (setq nskk-converting-active t)
    (nskk-rollback-conversion)
    (should-not nskk-converting-active)))

(nskk-deftest-unit input-commands-rollback-noop-when-not-converting
  "Test that rollback does nothing when not converting."
  (nskk-input-test-with-state 'hiragana
    ;; Should not error
    (nskk-rollback-conversion)
    (should-not nskk-converting-active)))

;;;
;;; Cancel Conversion Tests
;;;

(nskk-deftest-unit input-commands-cancel-noop-when-not-converting
  "Test that cancel does nothing when not converting."
  (nskk-input-test-with-state 'hiragana
    ;; Should not error
    (nskk-cancel-conversion)
    (should-not nskk-converting-active)))

;;;
;;; Convert-or-commit Tests
;;;

(nskk-deftest-unit input-commands-convert-or-commit-when-converting
  "Test convert-or-commit commits when already converting."
  (with-temp-buffer
    (nskk-input-test-with-state 'hiragana
      ;; Set up a mark and candidates to allow commit to work
      (insert "test")
      (push-mark (point-min) t)
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (setq nskk-converting-active t)
      ;; When converting, should call commit which clears active.
      (nskk-convert-or-commit)
      (should-not nskk-converting-active))))

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

(provide 'nskk-input-commands-test)

;;; nskk-input-commands-test.el ends here
