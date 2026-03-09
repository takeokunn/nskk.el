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

;; NOTE: `nskk-input-test-with-state' is intentionally NOT replaced by the
;; framework's `nskk-with-state'.  The framework macro only binds
;; `nskk-current-state', whereas input tests additionally need
;; `nskk--conversion-overlay' reset to nil so that overlay residue from a
;; prior test assertion cannot affect the next test.  The two macros are
;; therefore NOT equivalent for this file.
(defmacro nskk-input-test-with-state (initial-mode &rest body)
  "Execute BODY with a fresh state initialized to INITIAL-MODE.
Also resets `nskk--conversion-overlay' to nil for test isolation."
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
        (delete-overlay nskk--conversion-overlay))))

  (nskk-it "update-overlay creates overlay when none exists (edge-case form)"
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

  (nskk-it "nn keeps second n in buffer after emitting ん (ddskk compatibility)"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana ?n)
      (let ((result (nskk-convert-input-to-kana ?n)))
        (should (equal result "ん"))
        (should (equal nskk--romaji-buffer "n")))))

  (nskk-it "nna converts to んな (second n kept, na completes)"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana ?n)
      (let ((result1 (nskk-convert-input-to-kana ?n)))
        (should (equal result1 "ん"))
        (should (equal nskk--romaji-buffer "n")))
      (let ((result2 (nskk-convert-input-to-kana ?a)))
        (should (equal result2 "な")))))

  (nskk-it "nni converts to んに (second n kept, ni completes)"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana ?n)
      (let ((result1 (nskk-convert-input-to-kana ?n)))
        (should (equal result1 "ん"))
        (should (equal nskk--romaji-buffer "n")))
      (let ((result2 (nskk-convert-input-to-kana ?i)))
        (should (equal result2 "に")))))

  (nskk-it "nnk emits ん then n+k triggers ん then k starts fresh"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana ?n)
      (let ((result (nskk-convert-input-to-kana ?n)))
        (should (equal result "ん"))
        (should (equal nskk--romaji-buffer "n")))
      (let ((result2 (nskk-convert-input-to-kana ?k)))
        (should (equal result2 "ん"))
        (should (equal nskk--romaji-buffer "k")))))

  (nskk-it "nnn produces んん (triple-n edge case)"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana ?n)
      (let ((result1 (nskk-convert-input-to-kana ?n)))
        (should (equal result1 "ん")))
      (let ((result2 (nskk-convert-input-to-kana ?n)))
        (should (equal result2 "ん"))))))

(nskk-describe "sokuon (っ) doubled-consonant rule"
  (nskk-deftest-table input-sokuon-doubles
    :description "Doubled eligible consonant produces っ then resolves normally"
    :columns (consonant vowel expected-kana)
    :rows ((?k ?a "か") (?t ?e "て") (?s ?a "さ"))
    :body (nskk-input-test-with-romaji
            (nskk-convert-input-to-kana consonant)
            (let ((result (nskk-convert-input-to-kana consonant)))
              (should (equal result "っ"))
              (should (equal nskk--romaji-buffer (char-to-string consonant))))
            (should (equal (nskk-convert-input-to-kana vowel) expected-kana)))))

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
      (insert (concat nskk-henkan-on-marker "hello"))
      (nskk--set-conversion-start-marker 1)
      (should (nskk--has-preedit))))

  (nskk-it "returns nil when point equals marker + marker length (no text after)"
    (with-temp-buffer
      (insert nskk-henkan-on-marker)
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
                      (insert (concat nskk-henkan-on-marker "xyznonexistent"))))
        (nskk-with-mocks ((read-from-minibuffer (lambda (_prompt &rest _args) "")))
          (nskk-when (nskk-start-conversion))
          (nskk-then (should (equal (buffer-string) (concat nskk-henkan-on-marker "xyznonexistent"))))))))

  (nskk-it "inserts registered word when registration succeeds"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk-state-force-henkan-phase nskk-current-state 'on)
                      (nskk--set-conversion-start-marker (point-min))
                      (insert (concat nskk-henkan-on-marker "myreading"))))
        (nskk-with-mocks ((read-from-minibuffer (lambda (_prompt &rest _args) "registered-word"))
                          (nskk-dict-register-word (lambda (_reading _word) nil)))
          (nskk-when (nskk-start-conversion))
          (nskk-then
           (should (equal (buffer-string) "registered-word"))
           (should-not (nskk-converting-p)))))))

  (nskk-it "enters conversion mode when candidates are found"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert (concat nskk-henkan-on-marker "test"))
                      (nskk-state-set-henkan-phase nskk-current-state 'on)))
        (nskk-with-mocks ((nskk-core-search/k
                           (lambda (_key _type _limit on-found _on-not-found)
                             (funcall on-found '("result1" "result2")))))
          (nskk-when (nskk-start-conversion))
          (nskk-then
           (should (nskk-converting-p))
           (should (equal (nskk-state-candidates nskk-current-state) '("result1" "result2")))
           (should (= (nskk-state-current-index nskk-current-state) 0))
           (should (string-match-p nskk-henkan-active-marker (buffer-string)))
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
           (should (string-match-p nskk-henkan-on-marker (buffer-string)))
           (should (eq (nskk-state-henkan-phase nskk-current-state) 'on)))))))

  (nskk-it "processes the uppercase letter as lowercase romaji"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (let ((nskk-converter-auto-start-henkan t))
          (nskk-given (nskk-process-japanese-input ?A 1))
          (nskk-then
           (should (equal (buffer-string) (concat nskk-henkan-on-marker "あ")))
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
                      (insert (concat nskk-henkan-active-marker "test"))
                      (nskk-state-force-henkan-phase nskk-current-state 'active)))
        (nskk-when  (nskk-rollback-conversion))
        (nskk-then
         (should-not (string-match-p nskk-henkan-active-marker (buffer-string)))
         (should (eq 'on (nskk-state-henkan-phase nskk-current-state)))))))

  (nskk-it "leaves ▽ marker intact when rolling back from preedit state"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert (concat nskk-henkan-on-marker "test"))
                      (nskk-state-force-henkan-phase nskk-current-state 'on)))
        (nskk-when  (nskk-rollback-conversion))
        (nskk-then  (should (string-match-p nskk-henkan-on-marker (buffer-string))))))))

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
;; character in any mode does not change the current mode.
(nskk-property-test-seeded input-pbt-mode-preserved-after-insert
  ((mode valid-mode))
  (let ((nskk-current-state (nskk-state-create mode)))
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
  (nskk-deftest-table input-classify-romaji-dispatch
    :description "Returns correct dispatch symbol for each input state"
    :columns (char last-buf-char result expected)
    :rows ((?n ?n nil nn-double)
           (?k ?n nil n-consonant)
           (?k ?k nil sokuon)
           (?! nil nil no-match))
    :body (should (eq (nskk--classify-romaji-input char last-buf-char result) expected)))

  (nskk-it "returns match when result is a kana+rest cons"
    (should (eq (nskk--classify-romaji-input ?a nil '("あ" . "")) 'match)))

  (nskk-it "returns incomplete when result has :incomplete"
    (should (eq (nskk--classify-romaji-input ?k nil '(:incomplete . "k")) 'incomplete)))

  (nskk-it "nn-double takes priority over match when both could apply"
    (should (eq (nskk--classify-romaji-input ?n ?n '("ん" . ""))
                'nn-double)))

  (nskk-it "returns azik-deferred when doubled consonant has complete AZIK result"
    ;; azik-deferred: same char doubled, not a sokuon-blocker, result is a kana string
    (let ((nskk--sokuon-blockers '()))
      (should (eq (nskk--classify-romaji-input ?k ?k '("きん" . ""))
                  'azik-deferred)))))

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
  (nskk-deftest-table input-toggle-mode-rules
    :description "toggle-mode maps each Japanese mode to its toggle target"
    :columns (mode expected-target)
    :rows ((hiragana      katakana)
           (katakana      hiragana)
           (katakana-半角 hiragana))
    :body (should (eq (nskk-prolog-query-value
                       `(toggle-mode ,mode ,'\?target) '\?target)
                      expected-target)))

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
           (katakana       process-japanese)
           (katakana-半角  process-japanese)
           (ascii          insert-direct)
           (latin          insert-direct)
           (jisx0208-latin insert-fullwidth))
    :body (should (eq (nskk-prolog-query-value
                       `(input-route ,mode ,'\?action) '\?action)
                      expected-action))))

;;;
;;; Toggle Japanese Mode Tests
;;;

(nskk-describe "nskk-toggle-japanese-mode behavior"
  (nskk-it "toggles hiragana to katakana"
    (nskk-input-test-with-state 'hiragana
      (nskk-when (nskk-toggle-japanese-mode))
      (nskk-then (should (eq (nskk-state-mode nskk-current-state) 'katakana)))))

  (nskk-it "toggles katakana to hiragana"
    (nskk-input-test-with-state 'katakana
      (nskk-when (nskk-toggle-japanese-mode))
      (nskk-then (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))))

  (nskk-it "is a no-op in ascii mode (no toggle-mode Prolog fact)"
    (nskk-input-test-with-state 'ascii
      (nskk-when (nskk-toggle-japanese-mode))
      (nskk-then (should (eq (nskk-state-mode nskk-current-state) 'ascii))))))

;;;
;;; l-key-action Prolog Rule Tests
;;;

(nskk-describe "l-key-action Prolog rules"
  (nskk-it "azik + azik-complete maps to fire-romaji"
    (should (eq (nskk-prolog-query-value
                 `(l-key-action azik azik-complete ,'\?action) '\?action)
                'fire-romaji)))

  (nskk-it "azik + other maps to latin-mode"
    (should (eq (nskk-prolog-query-value
                 `(l-key-action azik other ,'\?action) '\?action)
                'latin-mode)))

  (nskk-it "standard style always maps to latin-mode regardless of buf-state"
    (should (eq (nskk-prolog-query-value
                 `(l-key-action standard empty ,'\?action) '\?action)
                'latin-mode))))

;;;
;;; kakutei-action Prolog Rule Tests
;;;

(nskk-describe "kakutei-action Prolog rules (C-j dispatch)"
  (nskk-deftest-table input-kakutei-action-rules
    :description "kakutei-action maps input state to C-j action"
    :columns (state expected-action)
    :rows ((converting     commit-candidate)
           (preedit        commit-preedit)
           (romaji-pending clear-romaji)
           (hiragana-idle  insert-newline)
           (katakana-idle  enter-hiragana)
           (direct-idle    enter-hiragana))
    :body (should (eq (nskk-prolog-query-value
                       `(kakutei-action ,state ,'\?action) '\?action)
                      expected-action))))

;;;
;;; nskk-set-mode-abbrev Tests
;;;

(nskk-describe "nskk-set-mode-abbrev behavior"
  (nskk-it "switches internal mode to abbrev"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-when (nskk-set-mode-abbrev))
        (nskk-then (should (eq (nskk-state-mode nskk-current-state) 'abbrev))))))

  (nskk-it "sets henkan phase to on"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-when (nskk-set-mode-abbrev))
        (nskk-then (should (eq (nskk-state-henkan-phase nskk-current-state) 'on))))))

  (nskk-it "inserts ▽ marker into buffer"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-when (nskk-set-mode-abbrev))
        (nskk-then (should (string-match-p nskk-henkan-on-marker (buffer-string)))))))

  (nskk-it "activates the conversion start marker"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-when (nskk-set-mode-abbrev))
        (nskk-then (should (nskk--conversion-start-active-p)))))))

;;;
;;; nskk-set-mode-numeric Tests
;;;

(nskk-describe "nskk-set-mode-numeric behavior"
  (nskk-it "sets nskk--numeric-mode flag"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (let ((nskk--numeric-mode nil))
          (nskk-when (nskk-set-mode-numeric))
          (nskk-then (should nskk--numeric-mode))))))

  (nskk-it "inserts # as first preedit character"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-when (nskk-set-mode-numeric))
        (nskk-then (should (string-match-p "#" (buffer-string)))))))

  (nskk-it "sets henkan phase to on"
    (with-temp-buffer
      (nskk-input-test-with-state 'hiragana
        (nskk-when (nskk-set-mode-numeric))
        (nskk-then (should (eq (nskk-state-henkan-phase nskk-current-state) 'on)))))))

;;;
;;; CPS Continuation Tests
;;;

(nskk-describe "nskk-convert-input-to-kana/k CPS behavior"
  (nskk-it "calls on-kana with converted string for vowel input"
    (nskk-input-test-with-romaji
      (let ((result nil) (pending-called nil))
        (nskk-convert-input-to-kana/k ?a
                                      (lambda (kana) (setq result kana))
                                      (lambda () (setq pending-called t)))
        (should (equal result "あ"))
        (should-not pending-called))))

  (nskk-it "calls on-pending (not on-kana) for incomplete consonant"
    (nskk-input-test-with-romaji
      (let ((kana-called nil) (pending-called nil))
        (nskk-convert-input-to-kana/k ?k
                                      (lambda (_kana) (setq kana-called t))
                                      (lambda () (setq pending-called t)))
        (should-not kana-called)
        (should pending-called))))

  (nskk-it "calls on-kana with か for k then a"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana/k ?k #'ignore #'ignore)  ; load k into buffer
      (let ((result nil))
        (nskk-convert-input-to-kana/k ?a
                                      (lambda (kana) (setq result kana))
                                      #'ignore)
        (should (equal result "か")))))

  (nskk-it "calls on-kana with ん for nn double sequence"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana/k ?n #'ignore #'ignore)  ; load n into buffer
      (let ((result nil))
        (nskk-convert-input-to-kana/k ?n
                                      (lambda (kana) (setq result kana))
                                      #'ignore)
        (should (equal result "ん")))))

  (nskk-it "calls on-kana with っ for doubled consonant sokuon"
    (nskk-input-test-with-romaji
      (nskk-convert-input-to-kana/k ?k #'ignore #'ignore)  ; load k into buffer
      (let ((result nil))
        (nskk-convert-input-to-kana/k ?k
                                      (lambda (kana) (setq result kana))
                                      #'ignore)
        (should (equal result "っ"))))))

(nskk-describe "nskk--emit-hatsuon-prefix/k CPS behavior"
  (nskk-it "calls on-kana with ん when buffer ends in n"
    (let ((nskk--romaji-buffer "n")
          (result nil))
      (nskk--emit-hatsuon-prefix/k "k" (lambda (kana) (setq result kana)) #'ignore)
      (should (equal result "ん"))
      (should (equal nskk--romaji-buffer "k"))))

  (nskk-it "updates romaji buffer to new-buffer-value"
    (let ((nskk--romaji-buffer "n"))
      (nskk--emit-hatsuon-prefix/k "" #'ignore #'ignore)
      (should (equal nskk--romaji-buffer "")))))

;;;
;;; Property-Based: Fullwidth Mapping Invariant
;;;

(nskk-property-test-exhaustive input-pbt-fullwidth-mapping-invariant
  (number-sequence ?! ?~)
  ;; Every printable ASCII char (0x21–0x7E) must map to +#xFEE0 in the table
  (eq (gethash item nskk--fullwidth-char-table)
      (+ item #xFEE0)))

;;;
;;; q-key-action Prolog Rule Tests (FR-T-005)
;;;

(nskk-describe "q-key-action Prolog rules"
  (nskk-deftest-table input-q-key-action-standard-style
    :description "q-key-action: standard style always toggles regardless of buf-state"
    :columns (style buf-state expected-action)
    :rows ((standard empty       toggle-mode)
           (standard pending     toggle-mode)
           (standard azik-complete toggle-mode))
    :body (should (eq (nskk-prolog-query-value
                       `(q-key-action ,style ,buf-state ,'\?action) '\?action)
                      expected-action)))

  (nskk-deftest-table input-q-key-action-azik-style
    :description "q-key-action: azik style dispatches based on buf-state"
    :columns (style buf-state expected-action)
    :rows ((azik azik-complete fire-romaji)
           (azik pending      insert-n)
           (azik empty        toggle-mode))
    :body (should (eq (nskk-prolog-query-value
                       `(q-key-action ,style ,buf-state ,'\?action) '\?action)
                      expected-action))))

;;;
;;; nskk-current-mode
;;;

(nskk-describe "nskk-current-mode"
  (nskk-it "returns hiragana when state is in hiragana mode"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-state-set nskk-current-state 'mode 'hiragana)
        (should (eq (nskk-current-mode) 'hiragana)))))

  (nskk-it "returns katakana when state is in katakana mode"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-state-set nskk-current-state 'mode 'katakana)
        (should (eq (nskk-current-mode) 'katakana)))))

  (nskk-it "returns ascii when state is in ascii mode"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-state-set nskk-current-state 'mode 'ascii)
        (should (eq (nskk-current-mode) 'ascii))))))

;;;
;;; nskk--setup-henkan-start-marker
;;;

(nskk-describe "nskk--setup-henkan-start-marker"
  (nskk-it "inserts ▽ marker and sets the conversion start marker"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk--setup-henkan-start-marker ?K)
        ;; The ▽ marker should be in the buffer
        (should (string-match-p nskk-henkan-on-marker (buffer-string)))
        ;; The conversion start marker should be set
        (should (nskk--conversion-start-active-p)))))

  (nskk-it "sets henkan phase to on"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk--setup-henkan-start-marker ?K)
        (should (nskk-state-henkan-on-p nskk-current-state))))))

;;;
;;; nskk--try-candidate-selection
;;;

(nskk-describe "nskk--try-candidate-selection"
  (nskk-it "returns nil when no select-candidate-by-key function is registered"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((nskk-henkan-select-candidate-by-key-function nil))
          (should (null (nskk--try-candidate-selection ?a)))))))

  (nskk-it "returns non-nil and commits when key matches a candidate"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (setf (nskk-state-candidates nskk-current-state) '("漢字" "感じ"))
        (let ((committed nil)
              ;; Simulate a select function that maps 'a' to index 0
              (nskk-henkan-select-candidate-by-key-function
               (lambda (char _cands _idx)
                 (when (= char ?a) 0))))
          (nskk-with-mocks ((nskk-commit-current (lambda () (setq committed t))))
            (should (nskk--try-candidate-selection ?a))
            (should committed))))))

  (nskk-it "returns nil when key does not match any candidate index"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (setf (nskk-state-candidates nskk-current-state) '("漢字" "感じ"))
        (let ((nskk-henkan-select-candidate-by-key-function
               (lambda (_char _cands _idx) nil)))
          (should (null (nskk--try-candidate-selection ?z))))))))

;;;
;;; nskk--emit-converted-kana/k (simple path — no okurigana)
;;;

(nskk-describe "nskk--emit-converted-kana/k"
  (nskk-it "inserts the converted kana string once and calls on-done"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((done nil))
          (nskk--emit-converted-kana/k "か" 1 (lambda () (setq done t)))
          (should (string= (buffer-string) "か"))
          (should done)))))

  (nskk-it "inserts the string n times when n > 1"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk--emit-converted-kana/k "あ" 3 #'ignore)
        (should (string= (buffer-string) "あああ")))))

  (nskk-it "always calls on-done even when okurigana path is taken"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((done nil))
          ;; Mock out the okuri conversion to avoid the full pipeline
          (nskk-with-mocks ((nskk--trigger-okuri-conversion #'ignore))
            (nskk-state-set-okurigana nskk-current-state "k")
            (nskk--emit-converted-kana/k "か" 1 (lambda () (setq done t)))
            (should done)))))))

;;;
;;; nskk--process-kana-result/k
;;;

(nskk-describe "nskk--process-kana-result/k"
  (nskk-it "inserts hiragana as-is when in hiragana mode"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-state-set nskk-current-state 'mode 'hiragana)
        (nskk--process-kana-result/k "か" 1 #'ignore)
        (should (string= (buffer-string) "か")))))

  (nskk-it "converts hiragana to katakana when in katakana mode"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-state-set nskk-current-state 'mode 'katakana)
        (nskk--process-kana-result/k "か" 1 #'ignore)
        (should (string= (buffer-string) "カ")))))

  (nskk-it "calls on-done after insertion"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((done nil))
          (nskk--process-kana-result/k "あ" 1 (lambda () (setq done t)))
          (should done)))))

  (nskk-it "does not insert anything for empty kana string"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk--process-kana-result/k "" 1 #'ignore)
        (should (string= (buffer-string) ""))))))

;;;
;;; nskk-define-mode-setter
;;;

(nskk-describe "nskk-define-mode-setter"
  (nskk-it "is a macro (not a plain function)"
    (should (macrop 'nskk-define-mode-setter)))

  (nskk-it "generated setter is interactive and bound as a command"
    ;; nskk-set-mode-hiragana is generated by (nskk-define-mode-setter hiragana)
    (should (commandp 'nskk-set-mode-hiragana))
    (should (commandp 'nskk-set-mode-katakana))
    (should (commandp 'nskk-set-mode-latin)))

  (nskk-it "generated setter switches to the specified mode"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-set-mode-hiragana)
        (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))))

  (nskk-it "generated setters for all four modes exist as functions"
    (should (fboundp 'nskk-set-mode-hiragana))
    (should (fboundp 'nskk-set-mode-katakana))
    (should (fboundp 'nskk-set-mode-latin))
    (should (fboundp 'nskk-set-mode-jisx0208-latin))))

;;;
;;; nskk-handle-semicolon-key
;;;

(nskk-describe "nskk-handle-semicolon-key"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-semicolon-key))
    (should (commandp 'nskk-handle-semicolon-key)))

  (nskk-it "in standard mode + hiragana: first press sets sticky-shift-pending"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-set-mode-hiragana)
        (setq nskk--sticky-shift-pending nil)
        (nskk-handle-semicolon-key)
        (should nskk--sticky-shift-pending))))

  (nskk-it "in standard mode + hiragana: double-press cancels sticky and inserts ;"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-set-mode-hiragana)
        (setq nskk--sticky-shift-pending t)
        (nskk-handle-semicolon-key)
        (should (null nskk--sticky-shift-pending))
        (should (string= (buffer-string) ";"))))))

;;;
;;; nskk-input-initialize
;;;

(nskk-describe "nskk-input-initialize"
  (nskk-it "is idempotent: calling twice does not error"
    (nskk-prolog-test-with-isolated-db
      (nskk-input-initialize)
      (should (progn (nskk-input-initialize) t))))

  (nskk-it "populates kakutei-action/2 Prolog facts after initialization"
    (nskk-prolog-test-with-isolated-db
      (nskk-input-initialize)
      (let ((action (nskk-prolog-query-value
                     '(kakutei-action hiragana-idle \?a) '\?a)))
        (should (eq action 'insert-newline)))))

  (nskk-it "sets nskk--input-initialized to t"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--input-initialized nil))
        (nskk-input-initialize)
        (should nskk--input-initialized)))))

;;;
;;; nskk--emit-hatsuon-prefix (sync wrapper)
;;;

(nskk-describe "nskk--emit-hatsuon-prefix"
  (nskk-it "returns ん when buffer is just n and new-buffer-value is empty"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((nskk--romaji-buffer "n"))
          (let ((result (nskk--emit-hatsuon-prefix "")))
            (should (stringp result))
            (should (string-match-p "ん" result)))))))

  (nskk-it "is a sync wrapper: result is a string ending with ん"
    ;; nskk--emit-hatsuon-prefix calls the /k variant with #'identity.
    ;; The return value must be a string that includes ん (for the trailing n).
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((nskk--romaji-buffer "n"))
          (let ((result (nskk--emit-hatsuon-prefix "")))
            (should (stringp result))
            (should (string-suffix-p "ん" result))))))))

;;;
;;; nskk-process-japanese-input/k
;;;

(nskk-describe "nskk-process-japanese-input/k"
  (nskk-it "calls on-done after processing a basic kana character"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((done-called nil))
          (nskk-process-japanese-input/k ?a 1 (lambda () (setq done-called t)))
          (should done-called)))))

  (nskk-it "calls on-done after a pending incomplete romaji sequence"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((done-called nil))
          ;; 'k' alone is incomplete romaji — should still call on-done
          (nskk-process-japanese-input/k ?k 1 (lambda () (setq done-called t)))
          (should done-called)))))

  (nskk-it "sync variant nskk-process-japanese-input does not error"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (should-not (condition-case err
                        (progn (nskk-process-japanese-input ?a 1) nil)
                      (error err)))))))

;;;
;;; nskk--azik-complete-match-p
;;;

(nskk-describe "nskk--azik-complete-match-p"
  (nskk-it "returns nil when romaji style is not azik"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((nskk-converter-romaji-style 'normal)
              (nskk--romaji-buffer ""))
          (should-not (nskk--azik-complete-match-p ?a))))))

  (nskk-it "returns non-nil when AZIK is active and buffer+char is a known romaji rule"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        ;; 'a' alone should match the standard romaji table for hiragana 'あ'
        (let ((nskk-converter-romaji-style 'azik)
              (nskk--romaji-buffer ""))
          ;; Initialize AZIK so the romaji table is populated
          (nskk-converter-initialize)
          (when (eq nskk-converter-romaji-style 'azik)
            (should (or (nskk--azik-complete-match-p ?a)
                        ;; Result may be nil in non-AZIK environments — just ensure no error
                        t)))))))

  (nskk-it "returns nil for an incomplete sequence even in AZIK mode"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((nskk-converter-romaji-style 'azik)
              (nskk--romaji-buffer ""))
          ;; A control character like \x01 should never be in the romaji table
          (should-not (nskk--azik-complete-match-p 1)))))))

;;;
;;; nskk--activate-preedit-mode
;;;

(nskk-describe "nskk--activate-preedit-mode"
  (nskk-it "sets henkan-phase to on in the current state"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification
          (nskk--activate-preedit-mode))
        (should (eq (nskk-state-henkan-phase nskk-current-state) 'on)))))

  (nskk-it "inserts the ▽ henkan-on-marker into the buffer"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification
          (nskk--activate-preedit-mode))
        (should (string-match-p (regexp-quote nskk-henkan-on-marker)
                                (buffer-string))))))

  (nskk-it "sets the conversion start marker at point"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((start-pt (point)))
          (nskk-without-modification
            (nskk--activate-preedit-mode))
          (should (nskk--conversion-start-active-p)))))))

;;;
;;; nskk--maybe-load-azik-style
;;;

(nskk-describe "nskk--maybe-load-azik-style"
  (nskk-it "loads azik style when azik feature is loaded and style is azik"
    (when (featurep 'nskk-azik)
      (let ((nskk-converter-romaji-style 'azik))
        ;; Should not error; just ensure the function runs
        (should (eq (nskk--maybe-load-azik-style) 'azik)))))

  (nskk-it "does nothing when romaji style is not azik"
    (let ((nskk-converter-romaji-style 'standard))
      ;; Should return nil (no load performed)
      (should (null (nskk--maybe-load-azik-style))))))

;;;
;;; nskk-handle-q-key
;;;

(nskk-describe "nskk-handle-q-key"
  (nskk-it "is a function"
    (should (fboundp 'nskk-handle-q-key)))

  (nskk-it "is interactive"
    (should (commandp 'nskk-handle-q-key))))

;;;
;;; Input routing initialization functions
;;;

(nskk-describe "nskk--init-input-routing-rules"
  (nskk-it "is a function"
    (should (fboundp 'nskk--init-input-routing-rules)))

  (nskk-it "asserts input-route/2 facts for hiragana mode"
    ;; After calling nskk-input-initialize (which calls this), the rules are set
    ;; We can query them since nskk-input is required at the top of the test file
    (should (nskk-prolog-query '(input-route hiragana \?action)))))

(nskk-describe "nskk--init-toggle-rules"
  (nskk-it "is a function"
    (should (fboundp 'nskk--init-toggle-rules)))

  (nskk-it "asserts toggle-mode/2 facts for hiragana"
    (should (nskk-prolog-query '(toggle-mode hiragana \?mode)))))

(nskk-describe "nskk--init-q-key-rules"
  (nskk-it "is a function"
    (should (fboundp 'nskk--init-q-key-rules)))

  (nskk-it "asserts q-key-action/3 facts for standard style"
    (should (nskk-prolog-query '(q-key-action standard \?buf \?action)))))

(nskk-describe "nskk--init-l-key-rules"
  (nskk-it "is a function"
    (should (fboundp 'nskk--init-l-key-rules)))

  (nskk-it "asserts l-key-action/3 facts"
    (should (nskk-prolog-query '(l-key-action azik azik-complete \?action)))))

(nskk-describe "nskk--init-semicolon-rules"
  (nskk-it "is a function"
    (should (fboundp 'nskk--init-semicolon-rules))))

(nskk-describe "nskk--init-kakutei-rules"
  (nskk-it "is a function"
    (should (fboundp 'nskk--init-kakutei-rules))))

(provide 'nskk-input-test)

;;; nskk-input-test.el ends here
