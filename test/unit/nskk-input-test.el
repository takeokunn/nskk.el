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
;; - Full-width character mapping (nskk--fullwidth-char-table)
;; - Toggle-mode Prolog rules
;; - Input-route Prolog rules
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

(nskk-describe "nskk-insert-char behavior"
  (nskk-it "inserts a single character"
    (with-temp-buffer
      (nskk-given (nskk-insert-char ?a))
      (nskk-then  (should (string= (buffer-string) "a")))))

  (nskk-it "inserts multiple characters with repeat count"
    (with-temp-buffer
      (nskk-given (nskk-insert-char ?x 3))
      (nskk-then  (should (string= (buffer-string) "xxx")))))

  (nskk-it "inserts character with nil (default) count"
    (with-temp-buffer
      (nskk-given (nskk-insert-char ?b nil))
      (nskk-then  (should (string= (buffer-string) "b")))))

  (nskk-it "inserts a sequence of characters"
    (with-temp-buffer
      (nskk-given (progn
                    (nskk-insert-char ?h)
                    (nskk-insert-char ?e)
                    (nskk-insert-char ?l)
                    (nskk-insert-char ?l)
                    (nskk-insert-char ?o)))
      (nskk-then  (should (string= (buffer-string) "hello"))))))

;;;
;;; Input to Kana Conversion Tests
;;;

(nskk-describe "nskk-convert-input-to-kana behavior"
  (nskk-it "converts a single vowel to hiragana"
    (let ((nskk--romaji-buffer ""))
      (let ((result (nskk-convert-input-to-kana ?a)))
        (should (stringp result))
        (should (equal result "あ")))))

  (nskk-it "returns empty string for incomplete consonant input"
    (let ((nskk--romaji-buffer ""))
      (let ((result (nskk-convert-input-to-kana ?k)))
        (should (stringp result))
        (should (equal result ""))
        (should (equal nskk--romaji-buffer "k")))))

  (nskk-it "converts consonant + vowel to kana"
    (let ((nskk--romaji-buffer ""))
      (nskk-convert-input-to-kana ?k)
      (let ((result (nskk-convert-input-to-kana ?a)))
        (should (stringp result))
        (should (equal result "か"))
        (should (equal nskk--romaji-buffer "")))))

  (nskk-context "all five vowels"
    (nskk-deftest-table input-kana-vowels
      :description "Each vowel converts to its hiragana counterpart"
      :columns (char expected)
      :rows ((?a "あ") (?i "い") (?u "う") (?e "え") (?o "お"))
      :body (let ((nskk--romaji-buffer ""))
              (should (equal (nskk-convert-input-to-kana char) expected))))))

;;;
;;; Abbrev Mode Processing Tests
;;;

(nskk-describe "nskk-process-abbrev-input behavior"
  (nskk-it "inserts a single character directly"
    (with-temp-buffer
      (nskk-given (nskk-process-abbrev-input ?a))
      (nskk-then  (should (string= (buffer-string) "a")))))

  (nskk-it "inserts a sequence of characters"
    (with-temp-buffer
      (nskk-given (progn
                    (nskk-process-abbrev-input ?t)
                    (nskk-process-abbrev-input ?e)
                    (nskk-process-abbrev-input ?s)
                    (nskk-process-abbrev-input ?t)))
      (nskk-then  (should (string= (buffer-string) "test"))))))

;;;
;;; Mode Query Tests
;;;

(nskk-describe "nskk-state-get-mode behavior"
  (nskk-deftest-table input-get-mode
    :description "state-get-mode returns the current mode"
    :columns (mode)
    :rows ((hiragana) (katakana) (latin) (abbrev))
    :body (nskk-input-test-with-state mode
            (should (eq (nskk-state-get-mode) mode)))))

;;;
;;; State Candidate Accessor Tests
;;;

(nskk-describe "nskk-state candidate accessors"
  (nskk-it "candidates returns nil when no candidates are set"
    (nskk-input-test-with-state 'hiragana
      (nskk-then (should (null (nskk-state-candidates nskk-current-state))))))

  (nskk-it "candidates returns the candidate list when set"
    (nskk-input-test-with-state 'hiragana
      (nskk-given (setf (nskk-state-candidates nskk-current-state)
                        '("\u6F22\u5B57" "\u611F\u3058")))
      (nskk-then  (should (equal (nskk-state-candidates nskk-current-state)
                                 '("\u6F22\u5B57" "\u611F\u3058"))))))

  (nskk-it "current-index returns 0 by default"
    (nskk-input-test-with-state 'hiragana
      (nskk-then (should (= (nskk-state-current-index nskk-current-state) 0)))))

  (nskk-it "setf current-index updates the index"
    (nskk-input-test-with-state 'hiragana
      (nskk-given (setf (nskk-state-current-index nskk-current-state) 5))
      (nskk-then  (should (= (nskk-state-current-index nskk-current-state) 5))))))

;;;
;;; Overlay Management Tests
;;;

(nskk-describe "nskk--update-overlay behavior"
  (nskk-it "creates a new overlay when none exists"
    (with-temp-buffer
      (insert "test text")
      (let ((nskk--conversion-overlay nil))
        (nskk-given (nskk--update-overlay 1 5 "converted"))
        (nskk-then
         (should (overlayp nskk--conversion-overlay))
         (should (equal (overlay-get nskk--conversion-overlay 'display) "converted"))
         (should (eq (overlay-get nskk--conversion-overlay 'face) 'highlight)))
        (delete-overlay nskk--conversion-overlay))))

  (nskk-it "reuses and moves existing overlay"
    (with-temp-buffer
      (insert "test text here")
      (let ((nskk--conversion-overlay nil))
        (nskk--update-overlay 1 5 "first")
        (let ((ov nskk--conversion-overlay))
          (nskk-given (nskk--update-overlay 6 10 "second"))
          (nskk-then
           (should (eq nskk--conversion-overlay ov))
           (should (equal (overlay-get nskk--conversion-overlay 'display) "second"))
           (should (= (overlay-start nskk--conversion-overlay) 6))
           (should (= (overlay-end nskk--conversion-overlay) 10))))
        (delete-overlay nskk--conversion-overlay)))))

;;;
;;; Interactive Command Tests
;;;

(nskk-describe "interactive command availability"
  (nskk-deftest-table input-interactive-commands
    :description "Command is defined and interactive"
    :columns (cmd)
    :rows ((nskk-convert)
           (nskk-convert-or-commit)
           (nskk-cancel-conversion)
           (nskk-rollback-conversion)
           (nskk-next-candidate)
           (nskk-previous-candidate)
           (nskk-commit-current))
    :body (should (commandp cmd))))

;;;
;;; Rollback Conversion Tests
;;;

(nskk-describe "nskk-rollback-conversion behavior"
  (nskk-it "clears the active converting state"
    (nskk-input-test-with-state 'hiragana
      (nskk-given (nskk-state-force-henkan-phase nskk-current-state 'active))
      (nskk-when  (nskk-rollback-conversion))
      (nskk-then  (should-not (nskk-converting-p)))))

  (nskk-it "is a no-op when not converting"
    (nskk-input-test-with-state 'hiragana
      (nskk-when (nskk-rollback-conversion))
      (nskk-then (should-not (nskk-converting-p))))))

;;;
;;; Cancel Conversion Tests
;;;

(nskk-describe "nskk-cancel-conversion behavior"
  (nskk-it "is a no-op when not converting"
    (nskk-input-test-with-state 'hiragana
      (nskk-when (nskk-cancel-conversion))
      (nskk-then (should-not (nskk-converting-p))))))

;;;
;;; Convert-or-commit Tests
;;;

(nskk-describe "nskk-convert-or-commit behavior"
  (nskk-it "commits and exits conversion when already converting"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "test")
                      (nskk-state-set-candidates nskk-current-state '("result"))
                      (nskk-state-force-henkan-phase nskk-current-state 'active)))
        (nskk-when  (nskk-convert-or-commit))
        (nskk-then  (should-not (nskk-converting-p)))))))

;;;
;;; Next/Previous Candidate Guard Tests
;;;

(nskk-describe "candidate navigation guards"
  (nskk-it "next-candidate is a no-op when not converting"
    (nskk-input-test-with-state 'hiragana
      (nskk-when (nskk-next-candidate))))

  (nskk-it "previous-candidate is a no-op when not converting"
    (nskk-input-test-with-state 'hiragana
      (nskk-when (nskk-previous-candidate)))))

;;;
;;; Error Handling: Self-Insert Without State
;;;

(nskk-describe "nskk-self-insert robustness"
  (nskk-it "handles missing state gracefully (falls back to ascii)"
    (let ((nskk-current-state nil)
          (last-command-event ?a))
      (with-temp-buffer
        (nskk-when (nskk-self-insert 1))
        (nskk-then (should (> (point-max) 1))))))

  (nskk-it "does not signal an error with uninitialized state"
    (let ((nskk-current-state nil)
          (last-command-event ?a))
      (with-temp-buffer
        (should-not (catch 'error (nskk-self-insert 1)))
        (should (> (buffer-size) 0))))))

;;;
;;; Mode-Based Input Routing Tests
;;;

(nskk-describe "nskk-self-insert mode dispatch"
  (nskk-it "inserts character directly in ascii mode"
    (nskk-input-test-with-state 'ascii
      (with-temp-buffer
        (let ((last-command-event ?x))
          (nskk-when (nskk-self-insert 1))
          (nskk-then (should (string= (buffer-string) "x")))))))

  (nskk-it "inserts character directly in latin mode"
    (nskk-input-test-with-state 'latin
      (with-temp-buffer
        (let ((last-command-event ?y))
          (nskk-when (nskk-self-insert 1))
          (nskk-then (should (string= (buffer-string) "y")))))))

  (nskk-it "processes abbrev input in abbrev mode"
    (nskk-input-test-with-state 'abbrev
      (with-temp-buffer
        (let ((last-command-event ?a))
          (nskk-when (nskk-self-insert 1))
          (nskk-then (should (> (buffer-size) 0)))))))

  (nskk-it "respects repeat count"
    (nskk-input-test-with-state 'ascii
      (with-temp-buffer
        (let ((last-command-event ?z))
          (nskk-when (nskk-self-insert 3))
          (nskk-then (should (string= (buffer-string) "zzz"))))))))

;;;
;;; Error Handling: Conversion State Guards
;;;

(nskk-describe "conversion command guards with uninitialized state"
  (nskk-it "nskk-convert handles nil marker gracefully"
    (let ((nskk-current-state nil)
          (nskk--conversion-start-marker nil))
      (with-temp-buffer
        (nskk-when (nskk-convert))
        (nskk-then (should-not (nskk-converting-p))))))

  (nskk-it "nskk-cancel-conversion handles nil state gracefully"
    (let ((nskk-current-state nil))
      (nskk-when (nskk-cancel-conversion))))

  (nskk-it "nskk-commit-current is a no-op with nil state"
    (let ((nskk-current-state nil))
      (nskk-when (nskk-commit-current))
      (nskk-then (should-not (nskk-converting-p)))))

  (nskk-it "nskk-rollback-conversion is a no-op with nil state"
    (let ((nskk-current-state nil))
      (nskk-when (nskk-rollback-conversion))
      (nskk-then (should-not (nskk-converting-p))))))

;;;
;;; Error Handling: Mode Queries With Missing State
;;;

(nskk-describe "mode queries with nil state"
  (nskk-it "nskk--set-mode signals user-error when state is nil"
    (let ((nskk-current-state nil))
      (nskk-then (should-error (nskk--set-mode 'hiragana) :type 'user-error))))

  (nskk-it "nskk-state-get-mode returns nil safely"
    (let ((nskk-current-state nil))
      (nskk-then (should (null (nskk-state-get-mode)))))))

;;;
;;; Error Handling: Overlay Management
;;;

(nskk-describe "overlay management edge cases"
  (nskk-it "update-overlay creates overlay when none exists"
    (with-temp-buffer
      (insert "test text")
      (let ((nskk--conversion-overlay nil))
        (nskk-given (nskk--update-overlay 1 5 "display"))
        (nskk-then  (should (overlayp nskk--conversion-overlay)))
        (delete-overlay nskk--conversion-overlay))))

  (nskk-it "update-overlay reuses existing overlay object"
    (with-temp-buffer
      (insert "test text with more content")
      (let ((nskk--conversion-overlay nil))
        (nskk--update-overlay 1 5 "first")
        (let ((first-overlay nskk--conversion-overlay))
          (nskk-given (nskk--update-overlay 10 15 "second"))
          (nskk-then
           (should (eq nskk--conversion-overlay first-overlay)))
          (delete-overlay nskk--conversion-overlay))))))

;;;
;;; Error Handling: Candidate Navigation Safety
;;;

(nskk-describe "candidate navigation with no candidates"
  (nskk-it "next-candidate does not error when candidates are empty"
    (nskk-input-test-with-state 'hiragana
      (nskk-given (nskk-state-force-henkan-phase nskk-current-state 'active))
      (let ((caught nil))
        (condition-case err
            (nskk-next-candidate)
          (error (setq caught err)))
        (should-not caught))))

  (nskk-it "previous-candidate does not error when candidates are empty"
    (nskk-input-test-with-state 'hiragana
      (nskk-given (nskk-state-force-henkan-phase nskk-current-state 'active))
      (let ((caught nil))
        (condition-case err
            (nskk-previous-candidate)
          (error (setq caught err)))
        (should-not caught)))))

;;;
;;; State Consistency Tests
;;;

(nskk-describe "state consistency through operations"
  (nskk-it "mode is unchanged after self-insert"
    (nskk-input-test-with-state 'hiragana
      (with-temp-buffer
        (let ((mode-before (nskk-state-mode nskk-current-state)))
          (let ((last-command-event ?a))
            (nskk-self-insert 1))
          (should (eq (nskk-state-mode nskk-current-state) mode-before))))))

  (nskk-it "mode is unchanged after a no-op conversion attempt"
    (nskk-input-test-with-state 'hiragana
      (let ((mode-before (nskk-state-mode nskk-current-state))
            (nskk--conversion-start-marker nil))
        (with-temp-buffer
          (nskk-convert))
        (should (eq (nskk-state-mode nskk-current-state) mode-before))))))

;;;
;;; n + consonant rule tests
;;;

(nskk-describe "romaji n/nn handling"
  (nskk-it "n+consonant sequence (kanji) emits ん before the consonant"
    (nskk-input-test-with-romaji
      (should (equal (nskk-convert-input-to-kana ?k) ""))
      (should (equal nskk--romaji-buffer "k"))
      (should (equal (nskk-convert-input-to-kana ?a) "か"))
      (should (equal nskk--romaji-buffer ""))
      (should (equal (nskk-convert-input-to-kana ?n) ""))
      (should (equal nskk--romaji-buffer "n"))
      (let ((result (nskk-convert-input-to-kana ?j)))
        (should (equal result "ん"))
        (should (equal nskk--romaji-buffer "j")))
      (should (equal (nskk-convert-input-to-kana ?i) "じ"))))

  (nskk-it "nk emits ん and leaves k in buffer"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana ?n)
      (let ((result (nskk-convert-input-to-kana ?k)))
        (should (equal result "ん"))
        (should (equal nskk--romaji-buffer "k")))
      (should (equal (nskk-convert-input-to-kana ?a) "か"))))

  (nskk-it "n + vowel does NOT trigger n+consonant rule"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana ?n)
      (let ((result (nskk-convert-input-to-kana ?a)))
        (should (equal result "な")))))

  (nskk-it "n + y does NOT trigger n+consonant rule (ny is a valid prefix)"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana ?n)
      (let ((result (nskk-convert-input-to-kana ?y)))
        (should (equal result ""))
        (should (equal nskk--romaji-buffer "ny")))))

  (nskk-it "nn produces ん"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana ?n)
      (let ((result (nskk-convert-input-to-kana ?n)))
        (should (equal result "ん"))))))

;;;
;;; Sokuon (double consonant) rule tests
;;;

(nskk-describe "sokuon (っ) input"
  (nskk-it "sakka produces さっか"
    (nskk-input-test-with-romaji
      (should (equal (nskk-convert-input-to-kana ?s) ""))
      (should (equal (nskk-convert-input-to-kana ?a) "さ"))
      (should (equal (nskk-convert-input-to-kana ?k) ""))
      (let ((result (nskk-convert-input-to-kana ?k)))
        (should (equal result "っ"))
        (should (equal nskk--romaji-buffer "k")))
      (should (equal (nskk-convert-input-to-kana ?a) "か"))))

  (nskk-it "tt produces っ then て"
    (nskk-input-test-with-romaji
      (should (equal (nskk-convert-input-to-kana ?t) ""))
      (let ((result (nskk-convert-input-to-kana ?t)))
        (should (equal result "っ"))
        (should (equal nskk--romaji-buffer "t")))
      (should (equal (nskk-convert-input-to-kana ?e) "て"))))

  (nskk-it "ss produces っ then さ"
    (nskk-input-test-with-romaji
      (should (equal (nskk-convert-input-to-kana ?s) ""))
      (let ((result (nskk-convert-input-to-kana ?s)))
        (should (equal result "っ"))
        (should (equal nskk--romaji-buffer "s")))
      (should (equal (nskk-convert-input-to-kana ?a) "さ"))))

  (nskk-it "double vowels do NOT trigger sokuon"
    (nskk-input-test-with-romaji
      (should (equal (nskk-convert-input-to-kana ?a) "あ"))
      (should (equal (nskk-convert-input-to-kana ?a) "あ"))))

  (nskk-it "nn does NOT trigger sokuon (nn has its own rule)"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana ?n)
      (let ((result (nskk-convert-input-to-kana ?n)))
        (should (equal result "ん"))
        (should-not (equal result "っ")))))

  (nskk-it "nn clears buffer after emitting ん (ddskk compatibility)"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana ?n)
      (let ((result (nskk-convert-input-to-kana ?n)))
        (should (equal result "ん"))
        (should (equal nskk--romaji-buffer "")))))

  (nskk-it "nna converts to んあ with buffer cleared between"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana ?n)
      (let ((result1 (nskk-convert-input-to-kana ?n)))
        (should (equal result1 "ん"))
        (should (equal nskk--romaji-buffer "")))
      (let ((result2 (nskk-convert-input-to-kana ?a)))
        (should (equal result2 "あ"))))))

;;;
;;; Standalone n at word boundary
;;;

(nskk-describe "hatsuon (ん) input"
  (nskk-it "standalone n stays in buffer as incomplete romaji"
    (nskk-input-test-with-romaji
      (should (equal (nskk-convert-input-to-kana ?n) ""))
      (should (equal nskk--romaji-buffer "n")))))

;;;
;;; Conversion Start Marker Tests
;;;

(nskk-describe "conversion start marker"
  (nskk-it "is nil by default"
    (with-temp-buffer
      (should (null nskk--conversion-start-marker))))

  (nskk-it "set-conversion-start-marker creates a marker at the given position"
    (with-temp-buffer
      (insert "hello")
      (nskk-given (nskk--set-conversion-start-marker 3))
      (nskk-then
       (should (markerp nskk--conversion-start-marker))
       (should (= (marker-position nskk--conversion-start-marker) 3)))))

  (nskk-it "set-conversion-start-marker reuses the existing marker object"
    (with-temp-buffer
      (insert "hello")
      (nskk--set-conversion-start-marker 2)
      (let ((first-marker nskk--conversion-start-marker))
        (nskk-given (nskk--set-conversion-start-marker 4))
        (nskk-then
         (should (eq nskk--conversion-start-marker first-marker))
         (should (= (marker-position nskk--conversion-start-marker) 4))))))

  (nskk-it "clear-conversion-start-marker unsets the marker position"
    (with-temp-buffer
      (insert "hello")
      (nskk--set-conversion-start-marker 3)
      (should (marker-position nskk--conversion-start-marker))
      (nskk-given (nskk--clear-conversion-start-marker))
      (nskk-then
       (should (markerp nskk--conversion-start-marker))
       (should (null (marker-position nskk--conversion-start-marker))))))

  (nskk-it "conversion-start-active-p reflects marker state"
    (with-temp-buffer
      (insert "hello")
      (should-not (nskk--conversion-start-active-p))
      (nskk--set-conversion-start-marker 2)
      (should (nskk--conversion-start-active-p))
      (nskk--clear-conversion-start-marker)
      (should-not (nskk--conversion-start-active-p))))

  (nskk-it "get-conversion-start returns marker position"
    (with-temp-buffer
      (insert "hello")
      (nskk--set-conversion-start-marker 3)
      (should (= (nskk--get-conversion-start) 3))))

  (nskk-it "get-conversion-start returns nil when no marker is set"
    (with-temp-buffer
      (should (null (nskk--get-conversion-start))))))

;;;
;;; Has-Preedit Tests with Marker
;;;

(nskk-describe "nskk--has-preedit behavior"
  (nskk-it "returns non-nil when point is past the marker + marker length"
    (with-temp-buffer
      (insert "\u25BDhello")
      (nskk--set-conversion-start-marker 1)
      (should (nskk--has-preedit))))

  (nskk-it "returns nil when point equals marker + marker length (no text after)"
    (with-temp-buffer
      (insert "\u25BD")
      (nskk--set-conversion-start-marker 1)
      (should-not (nskk--has-preedit))))

  (nskk-it "returns nil when no marker is set"
    (with-temp-buffer
      (insert "hello")
      (should-not (nskk--has-preedit)))))

;;;
;;; Commit Clears Marker Tests
;;;

(nskk-describe "commit and rollback marker cleanup"
  (nskk-it "commit-current clears the conversion start marker"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "test")
                      (nskk-state-set-candidates nskk-current-state '("result"))
                      (nskk-state-force-henkan-phase nskk-current-state 'active)))
        (nskk-when  (nskk-commit-current))
        (nskk-then  (should-not (nskk--conversion-start-active-p))))))

  (nskk-it "rollback keeps the start marker active (returns to preedit)"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "test")
                      (nskk-state-force-henkan-phase nskk-current-state 'active)))
        (nskk-when  (nskk-rollback-conversion))
        (nskk-then  (should (nskk--conversion-start-active-p)))))))

;;;
;;; Start Conversion Tests
;;;

(nskk-describe "nskk-start-conversion behavior"
  (nskk-it "is a no-op when no marker is set"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (insert "test")
        (nskk-when (nskk-start-conversion))
        (nskk-then (should-not (nskk-converting-p))))))

  (nskk-it "triggers registration when no candidates found and cancellation leaves preedit"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk-state-force-henkan-phase nskk-current-state 'on)
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "\u25BDxyznonexistent")))
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (_prompt &rest _args) "")))
          (nskk-when (nskk-start-conversion))
          (nskk-then (should (equal (buffer-string) "\u25BDxyznonexistent")))))))

  (nskk-it "inserts registered word when registration succeeds"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk-state-force-henkan-phase nskk-current-state 'on)
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "\u25BDmyreading")))
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (_prompt &rest _args) "registered-word"))
                  ((symbol-function 'nskk-dict-register-word)
                   (lambda (_reading _word) nil)))
          (nskk-when (nskk-start-conversion))
          (nskk-then
           (should (equal (buffer-string) "registered-word"))
           (should-not (nskk-converting-p)))))))

  (nskk-it "enters conversion mode when candidates are found"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "\u25BDtest")
                      (nskk-state-set-henkan-phase nskk-current-state 'on)))
        (cl-letf (((symbol-function 'nskk-core-search)
                   (lambda (_key &optional _type _limit) '("result1" "result2"))))
          (nskk-when (nskk-start-conversion))
          (nskk-then
           (should (nskk-converting-p))
           (should (equal (nskk-state-candidates nskk-current-state) '("result1" "result2")))
           (should (= (nskk-state-current-index nskk-current-state) 0))
           (should (string-match-p "\u25BC" (buffer-string)))
           (should (overlayp nskk--conversion-overlay))
           (should (equal (overlay-get nskk--conversion-overlay 'display) "result1"))
           (should (eq (nskk-state-henkan-phase nskk-current-state) 'active)))
          (when (overlayp nskk--conversion-overlay)
            (delete-overlay nskk--conversion-overlay)))))))

;;;
;;; Uppercase Letter Henkan Start Tests
;;;

(nskk-describe "uppercase letter triggers henkan start"
  (nskk-it "sets the conversion start marker"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (let ((nskk-converter-auto-start-henkan t))
          (nskk-given (nskk-process-japanese-input ?K 1))
          (nskk-then
           (should (nskk--conversion-start-active-p))
           (should (string-match-p "\u25BD" (buffer-string)))
           (should (eq (nskk-state-henkan-phase nskk-current-state) 'on)))))))

  (nskk-it "processes the uppercase letter as lowercase romaji"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (let ((nskk-converter-auto-start-henkan t))
          (nskk-given (nskk-process-japanese-input ?A 1))
          (nskk-then
           (should (equal (buffer-string) "\u25BD\u3042"))
           (should (nskk--conversion-start-active-p)))))))

  (nskk-it "does not reset the marker on a second uppercase letter"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (let ((nskk-converter-auto-start-henkan t))
          (nskk-process-japanese-input ?K 1)
          (let ((first-pos (nskk--get-conversion-start)))
            (nskk-process-japanese-input ?a 1)
            (should (= (nskk--get-conversion-start) first-pos)))))))

  (nskk-it "is disabled when nskk-converter-auto-start-henkan is nil"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (let ((nskk-converter-auto-start-henkan nil))
          (nskk-given (nskk-process-japanese-input ?K 1))
          (nskk-then  (should-not (nskk--conversion-start-active-p))))))))

;;;
;;; Inline Marker Constant Tests
;;;

(nskk-describe "inline marker constants"
  (nskk-it "henkan-on marker is ▽"
    (should (equal nskk-henkan-on-marker "\u25BD")))

  (nskk-it "henkan-active marker is ▼"
    (should (equal nskk-henkan-active-marker "\u25BC")))

  (nskk-it "okurigana marker is *"
    (should (equal nskk-okurigana-marker "*"))))

;;;
;;; Marker Cleanup on Commit Tests
;;;

(nskk-describe "commit-current cleanup"
  (nskk-it "resets henkan-phase to nil"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "test")
                      (nskk-state-set-candidates nskk-current-state '("result"))
                      (nskk-state-force-henkan-phase nskk-current-state 'active)))
        (nskk-when  (nskk-commit-current))
        (nskk-then  (should (null (nskk-state-henkan-phase nskk-current-state)))))))

  (nskk-it "resets henkan count to 0"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "test")
                      (nskk-state-set-candidates nskk-current-state '("result"))
                      (nskk-state-force-henkan-phase nskk-current-state 'active)
                      (setq nskk--henkan-count 3)))
        (nskk-when  (nskk-commit-current))
        (nskk-then  (should (= nskk--henkan-count 0)))))))

;;;
;;; Rollback Marker Cleanup Tests
;;;

(nskk-describe "rollback-conversion buffer cleanup"
  (nskk-it "removes the ▼ marker from buffer"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "\u25BCtest")
                      (nskk-state-force-henkan-phase nskk-current-state 'active)))
        (nskk-when  (nskk-rollback-conversion))
        (nskk-then
         (should-not (string-match-p "\u25BC" (buffer-string)))
         (should (null (nskk-state-henkan-phase nskk-current-state)))))))

  (nskk-it "leaves ▽ marker intact when rolling back from preedit state"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "\u25BDtest")
                      (nskk-state-force-henkan-phase nskk-current-state 'on)))
        (nskk-when  (nskk-rollback-conversion))
        (nskk-then  (should (string-match-p "\u25BD" (buffer-string))))))))

;;;
;;; Property-Based Tests
;;;

;; Input never crashes: processing any romaji character in hiragana mode
;; raises no error. The generator always produces non-empty romaji strings,
;; so (> (length input) 0) is always true; we return t on success and nil on error.
(nskk-property-test-seeded input-pbt-romaji-char-no-crash-hiragana-mode
  ((input romaji-string))
  (if (> (length input) 0)
      (let ((char (aref input 0)))
        (condition-case nil
            (let ((nskk--romaji-buffer ""))
              (nskk-convert-input-to-kana char)
              t)
          (error nil)))
    t)  ; empty string is vacuously ok
  100 3001)

;; Mode is preserved after non-mode-switch input: inserting a regular ASCII
;; character in hiragana mode does not change the current mode.
(nskk-property-test-seeded input-pbt-mode-preserved-after-insert
  ((input romaji-string))
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (with-temp-buffer
      (let ((mode-before (nskk-state-mode nskk-current-state))
            (last-command-event ?a))
        (nskk-self-insert 1)
        (eq (nskk-state-mode nskk-current-state) mode-before))))
  50 3002)

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
;;; nskk--classify-romaji-input Tests
;;;

(nskk-describe "nskk--classify-romaji-input"
  (nskk-it "returns nn-double when last and new char are both n"
    (should (eq (nskk--classify-romaji-input ?n ?n nil)
                'nn-double)))

  (nskk-it "nn-double takes priority over match when result is also a string pair"
    ;; Priority: nn-double must beat match (clause 1 before clause 2)
    (should (eq (nskk--classify-romaji-input ?n ?n '("ん" . ""))
                'nn-double)))

  (nskk-it "returns match when converter returned a kana string"
    (should (eq (nskk--classify-romaji-input ?a nil '("あ" . ""))
                'match)))

  (nskk-it "returns n-consonant when last is n and new char is not a hatsuon-blocker"
    (should (eq (nskk--classify-romaji-input ?k ?n nil)
                'n-consonant)))

  (nskk-it "returns sokuon when same eligible consonant is doubled"
    (should (eq (nskk--classify-romaji-input ?k ?k nil)
                'sokuon)))

  (nskk-it "returns incomplete when converter returned :incomplete"
    (should (eq (nskk--classify-romaji-input ?k nil '(:incomplete . "k"))
                'incomplete)))

  (nskk-it "returns no-match as fallback"
    (should (eq (nskk--classify-romaji-input ?! nil nil)
                'no-match))))

;;;
;;; Fullwidth-Char Prolog Table Tests
;;;

(nskk-describe "nskk--fullwidth-char-table mappings"
  (nskk-deftest-table input-fullwidth-char-mappings
    :description "ASCII characters map to their fullwidth Unicode equivalents"
    :columns (char expected)
    :rows ((?\s ?\u3000)
           (?!  ?\uFF01)
           (?~  ?\uFF5E)
           (?A  ?\uFF21)
           (?a  ?\uFF41))
    :body (should (eq (gethash char nskk--fullwidth-char-table)
                      expected)))

  (nskk-it "non-ASCII character passes through unchanged"
    (with-temp-buffer
      (nskk-insert-fullwidth-char ?\u3042 1)
      (should (string= (buffer-string) "\u3042")))))

;;;
;;; Toggle-Mode Prolog Rule Tests
;;;

(nskk-describe "toggle-mode Prolog rules"
  (nskk-it "maps hiragana to katakana"
    (should (eq (nskk-prolog-query-value
                 `(toggle-mode hiragana ,'\?target) '\?target)
                'katakana)))

  (nskk-it "maps katakana to hiragana"
    (should (eq (nskk-prolog-query-value
                 `(toggle-mode katakana ,'\?target) '\?target)
                'hiragana)))

  (nskk-it "ascii mode has no toggle-mode mapping"
    (should (null (nskk-prolog-query-value
                   `(toggle-mode ascii ,'\?target) '\?target)))))

;;;
;;; Input-Route Prolog Rule Tests
;;;

(nskk-describe "input-route Prolog rules"
  (nskk-deftest-table input-route-rules
    :description "Each mode routes to the correct input action"
    :columns (mode expected-action)
    :rows ((hiragana       process-japanese)
           (ascii          insert-direct)
           (jisx0208-latin insert-fullwidth))
    :body (should (eq (nskk-prolog-query-value
                       `(input-route ,mode ,'\?action) '\?action)
                      expected-action))))

(provide 'nskk-input-test)

;;; nskk-input-test.el ends here
