;;; nskk-henkan-pipeline-integration-test.el --- Henkan pipeline integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Integration tests for the real henkan (conversion) pipeline.
;;
;; These tests exercise nskk-start-conversion, nskk-next-candidate,
;; nskk-previous-candidate, and nskk-commit-current WITHOUT mocking
;; nskk-core-search.  A mock dictionary (nskk-with-mock-dict) provides
;; real Prolog-backed candidates so that the full Layer-3 -> Layer-2 ->
;; Layer-1 call chain is exercised.

;;; Code:

(require 'ert)
(require 'nskk-henkan)
(require 'nskk-input)
(require 'nskk-keymap)
(require 'nskk-state)
(require 'nskk-converter)
(require 'nskk-test-framework)
(require 'nskk-integration-test)

;;;
;;; Test 1: Start Conversion Enters Converting Phase
;;;

(nskk-deftest-integration henkan-pipeline-start-conversion
  "Test that SPC on preedit triggers nskk-start-conversion and produces candidates."
  (nskk-with-mock-dict nil
    (nskk-integration-with-session 'hiragana
      ;; Type uppercase sequence to produce "▽かんじ"
      (nskk-integration--type-char ?K)
      (nskk-integration--type-char ?a)
      (nskk-integration--type-char ?n)
      (nskk-integration--type-char ?j)
      (nskk-integration--type-char ?i)
      ;; Verify preedit is active before conversion
      (should (nskk--conversion-start-active-p))
      ;; Trigger SPC to start conversion
      (let ((last-command-event ? ))
        (nskk-handle-space))
      ;; Verify conversion state
      (should (nskk-converting-p))
      ;; Verify candidates were loaded
      (should (nskk-state-candidates nskk-current-state))
      ;; Verify "漢字" is among the candidates
      (should (member "漢字" (nskk-state-candidates nskk-current-state))))))

;;;
;;; Test 2: First Candidate Is Displayed After Conversion Start
;;;

(nskk-deftest-integration henkan-pipeline-first-candidate-displayed
  "Test that the first candidate shown after SPC is '漢字' at index 0."
  (nskk-with-mock-dict nil
    (nskk-integration-with-session 'hiragana
      ;; Type "▽かんじ"
      (nskk-integration--type-char ?K)
      (nskk-integration--type-char ?a)
      (nskk-integration--type-char ?n)
      (nskk-integration--type-char ?j)
      (nskk-integration--type-char ?i)
      ;; Start conversion
      (let ((last-command-event ? ))
        (nskk-handle-space))
      ;; Verify the first candidate and index
      (nskk-should-equal "漢字" (nskk-state-current-candidate nskk-current-state))
      (should (= (nskk-state-current-index nskk-current-state) 0)))))

;;;
;;; Test 3: RET Commits First Candidate Without Newline
;;;

(nskk-deftest-integration henkan-pipeline-commit-first-candidate
  "Test that RET after conversion commits '漢字' without inserting a newline."
  (nskk-with-mock-dict nil
    (nskk-integration-with-session 'hiragana
      ;; Type "▽かんじ"
      (nskk-integration--type-char ?K)
      (nskk-integration--type-char ?a)
      (nskk-integration--type-char ?n)
      (nskk-integration--type-char ?j)
      (nskk-integration--type-char ?i)
      ;; Start conversion
      (let ((last-command-event ? ))
        (nskk-handle-space))
      ;; Commit first candidate
      (nskk-handle-return)
      ;; Verify conversion is no longer active
      (should-not (nskk-converting-p))
      (should-not (nskk--conversion-start-active-p))
      ;; Verify buffer contains only the committed kanji (no newline)
      (nskk-should-equal "漢字" (buffer-string)))))

;;;
;;; Test 4: SPC Advances to Next Candidate
;;;

(nskk-deftest-integration henkan-pipeline-next-candidate-via-space
  "Test that pressing SPC again during conversion advances to the next candidate."
  (nskk-with-mock-dict nil
    (nskk-integration-with-session 'hiragana
      ;; Type "▽かんじ"
      (nskk-integration--type-char ?K)
      (nskk-integration--type-char ?a)
      (nskk-integration--type-char ?n)
      (nskk-integration--type-char ?j)
      (nskk-integration--type-char ?i)
      ;; Start conversion (first SPC -> "漢字" at index 0)
      (let ((last-command-event ? ))
        (nskk-handle-space))
      ;; Advance to next candidate (second SPC)
      (let ((last-command-event ? ))
        (nskk-handle-space))
      ;; Verify second candidate "感じ" is now current
      (nskk-should-equal "感じ" (nskk-state-current-candidate nskk-current-state)))))

;;;
;;; Test 5: x Key Goes Back to Previous Candidate
;;;

(nskk-deftest-integration henkan-pipeline-previous-candidate-via-x
  "Test that pressing x during conversion moves back to the previous candidate."
  (nskk-with-mock-dict nil
    (nskk-integration-with-session 'hiragana
      ;; Type "▽かんじ"
      (nskk-integration--type-char ?K)
      (nskk-integration--type-char ?a)
      (nskk-integration--type-char ?n)
      (nskk-integration--type-char ?j)
      (nskk-integration--type-char ?i)
      ;; Start conversion (index 0 -> "漢字")
      (let ((last-command-event ? ))
        (nskk-handle-space))
      ;; Advance to next candidate (index 1 -> "感じ")
      (let ((last-command-event ? ))
        (nskk-handle-space))
      (nskk-should-equal "感じ" (nskk-state-current-candidate nskk-current-state))
      ;; Press x to go back (index 0 -> "漢字")
      (let ((last-command-event ?x))
        (nskk-handle-x))
      ;; Verify first candidate is current again
      (nskk-should-equal "漢字" (nskk-state-current-candidate nskk-current-state)))))

;;;
;;; Test 6: C-g Cancels Conversion and Restores Preedit
;;;

(nskk-deftest-integration henkan-pipeline-cancel-restores-preedit
  "Test that C-g during conversion cancels it and restores the kana reading.
C-g from conversion (▼) calls nskk-cancel-conversion-to-reading, which
removes the ▼ marker and overlay, resets all conversion state, and leaves
the plain kana reading in the buffer without any preedit (▽) marker."
  (nskk-with-mock-dict nil
    (nskk-integration-with-session 'hiragana
      ;; Type "▽かんじ"
      (nskk-integration--type-char ?K)
      (nskk-integration--type-char ?a)
      (nskk-integration--type-char ?n)
      (nskk-integration--type-char ?j)
      (nskk-integration--type-char ?i)
      ;; Start conversion
      (let ((last-command-event ? ))
        (nskk-handle-space))
      ;; Verify conversion is active
      (should (nskk-converting-p))
      ;; Cancel conversion via C-g handler
      (nskk-handle-cancel)
      ;; Verify conversion is no longer active
      (should-not (nskk-converting-p))
      ;; Verify preedit marker is gone (state fully cancelled)
      (should-not (nskk--conversion-start-active-p))
      ;; Verify kana reading is in buffer without ▽ marker
      (nskk-should-equal "かんじ" (buffer-string)))))

;;;
;;; Test 7: Single-Candidate Word Commits Correctly
;;;

(nskk-deftest-integration henkan-pipeline-sakura-single-candidate
  "Test that a word with a single dictionary candidate converts and commits correctly."
  (nskk-with-mock-dict nil
    (nskk-integration-with-session 'hiragana
      ;; Type "▽さくら"
      (dolist (c '(?S ?a ?k ?u ?r ?a))
        (nskk-integration--type-char c))
      ;; Start conversion
      (let ((last-command-event ? ))
        (nskk-handle-space))
      ;; Verify conversion is active
      (should (nskk-converting-p))
      ;; Commit the single candidate
      (nskk-handle-return)
      ;; Verify the buffer contains the expected kanji
      (nskk-should-equal "桜" (buffer-string)))))

;;;
;;; Test 8: Commit Clears All Conversion State
;;;

(nskk-deftest-integration henkan-pipeline-commit-clears-state
  "Test that committing a candidate resets candidates, index, and romaji buffer."
  (nskk-with-mock-dict nil
    (nskk-integration-with-session 'hiragana
      ;; Type "▽かんじ"
      (nskk-integration--type-char ?K)
      (nskk-integration--type-char ?a)
      (nskk-integration--type-char ?n)
      (nskk-integration--type-char ?j)
      (nskk-integration--type-char ?i)
      ;; Start conversion
      (let ((last-command-event ? ))
        (nskk-handle-space))
      ;; Commit first candidate
      (nskk-handle-return)
      ;; Verify all conversion state is cleared
      (should (null (nskk-state-candidates nskk-current-state)))
      (should (= (nskk-state-current-index nskk-current-state) 0))
      (nskk-should-equal "" nskk--romaji-buffer))))

(provide 'nskk-henkan-pipeline-integration-test)

;;; nskk-henkan-pipeline-integration-test.el ends here
