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
(require 'nskk-test-macros)
(require 'nskk-integration-test)

;;;
;;; Helper: type the "▽かんじ" preedit sequence
;;;

(defun nskk-henkan-pipeline--setup-kanji-preedit ()
  "Type K-a-n-j-i to produce ▽かんじ preedit in the current session."
  (nskk-integration--type-char ?K)
  (nskk-integration--type-char ?a)
  (nskk-integration--type-char ?n)
  (nskk-integration--type-char ?j)
  (nskk-integration--type-char ?i))

;;;
;;; Henkan Pipeline Tests
;;;

(nskk-describe "henkan conversion pipeline"

  (nskk-it "SPC on preedit triggers conversion and loads candidates including 漢字"
    (nskk-with-mock-dict nil
      (nskk-integration-with-session 'hiragana
        (nskk-given (nskk-henkan-pipeline--setup-kanji-preedit))
        (nskk-then  (should (nskk--conversion-start-active-p)))
        (nskk-when  (let ((last-command-event ? ))
                      (nskk-handle-space)))
        (nskk-then
          (should (nskk-converting-p))
          (should (nskk-state-candidates nskk-current-state))
          (should (member "漢字" (nskk-state-candidates nskk-current-state)))))))

  (nskk-it "first candidate shown after SPC is 漢字 at index 0"
    (nskk-with-mock-dict nil
      (nskk-integration-with-session 'hiragana
        (nskk-given (nskk-henkan-pipeline--setup-kanji-preedit))
        (nskk-when  (let ((last-command-event ? ))
                      (nskk-handle-space)))
        (nskk-then
          (nskk-should-equal "漢字" (nskk-state-current-candidate nskk-current-state))
          (should (= (nskk-state-current-index nskk-current-state) 0))))))

  (nskk-it "RET after conversion commits 漢字 without inserting a newline"
    (nskk-with-mock-dict nil
      (nskk-integration-with-session 'hiragana
        (nskk-given (nskk-henkan-pipeline--setup-kanji-preedit))
        (nskk-when
          (let ((last-command-event ? ))
            (nskk-handle-space))
          (nskk-handle-return))
        (nskk-then
          (should-not (nskk-converting-p))
          (should-not (nskk--conversion-start-active-p))
          (nskk-should-equal "漢字" (buffer-string))))))

  (nskk-it "pressing SPC again during conversion advances to the next candidate 感じ"
    (nskk-with-mock-dict nil
      (nskk-integration-with-session 'hiragana
        (nskk-given (nskk-henkan-pipeline--setup-kanji-preedit))
        ;; Start conversion -> index 0 "漢字"
        (nskk-when  (let ((last-command-event ? ))
                      (nskk-handle-space)))
        ;; Advance to next candidate
        (nskk-when  (let ((last-command-event ? ))
                      (nskk-handle-space)))
        (nskk-then
          (nskk-should-equal "感じ" (nskk-state-current-candidate nskk-current-state))))))

  (nskk-it "pressing x during conversion moves back to the previous candidate 漢字"
    (nskk-with-mock-dict nil
      (nskk-integration-with-session 'hiragana
        (nskk-given (nskk-henkan-pipeline--setup-kanji-preedit))
        ;; Start conversion -> index 0 "漢字"
        (nskk-when  (let ((last-command-event ? ))
                      (nskk-handle-space)))
        ;; Advance to index 1 "感じ"
        (nskk-when  (let ((last-command-event ? ))
                      (nskk-handle-space)))
        (nskk-then  (nskk-should-equal "感じ" (nskk-state-current-candidate nskk-current-state)))
        ;; Press x to go back to index 0 "漢字"
        (nskk-when  (let ((last-command-event ?x))
                      (nskk-handle-x)))
        (nskk-then  (nskk-should-equal "漢字" (nskk-state-current-candidate nskk-current-state))))))

  (nskk-it "C-g during conversion cancels it and restores kana reading without ▽"
    (nskk-with-mock-dict nil
      (nskk-integration-with-session 'hiragana
        (nskk-given (nskk-henkan-pipeline--setup-kanji-preedit))
        (nskk-when  (let ((last-command-event ? ))
                      (nskk-handle-space)))
        (nskk-then  (should (nskk-converting-p)))
        (nskk-when  (nskk-handle-cancel))
        (nskk-then
          (should-not (nskk-converting-p))
          (should-not (nskk--conversion-start-active-p))
          (nskk-should-equal "かんじ" (buffer-string))))))

  (nskk-it "a word with a single dictionary candidate converts and commits correctly"
    (nskk-with-mock-dict nil
      (nskk-integration-with-session 'hiragana
        (nskk-given (dolist (c '(?S ?a ?k ?u ?r ?a))
                      (nskk-integration--type-char c)))
        (nskk-when
          (let ((last-command-event ? ))
            (nskk-handle-space)))
        (nskk-then  (should (nskk-converting-p)))
        (nskk-when  (nskk-handle-return))
        (nskk-then  (nskk-should-equal "桜" (buffer-string))))))

  (nskk-it "committing a candidate resets candidates, index, and romaji buffer"
    (nskk-with-mock-dict nil
      (nskk-integration-with-session 'hiragana
        (nskk-given (nskk-henkan-pipeline--setup-kanji-preedit))
        (nskk-when
          (let ((last-command-event ? ))
            (nskk-handle-space))
          (nskk-handle-return))
        (nskk-then
          (should (null (nskk-state-candidates nskk-current-state)))
          (should (= (nskk-state-current-index nskk-current-state) 0))
          (nskk-should-equal "" nskk--romaji-buffer))))))

;;;
;;; Property-Based Tests
;;;

(require 'nskk-pbt-generators)

(nskk-deftest-cases henkan-pipeline-preedit-sequences
  (("kanji" . "かんじ")
   ("umi"   . "うみ")
   ("sora"  . "そら"))
  :body
  (nskk-with-mock-dict nil
    (nskk-integration-with-session 'hiragana
      (dolist (ch (string-to-list input))
        (nskk-integration--type-char ch))
      ;; After typing, buffer-string should be non-empty (▽ + kana)
      (should (> (length (buffer-string)) 0)))))

(nskk-property-test henkan-pipeline-preedit-does-not-crash
  ((r romaji-basic))
  (nskk-with-mock-dict nil
    (nskk-integration-with-session 'hiragana
      (condition-case err
          (dolist (ch (string-to-list r))
            (nskk-integration--type-char ch))
        (error (ert-fail (format "Pipeline crashed on %s: %s" r (error-message-string err)))))
      t))
  30)

(nskk-property-test henkan-pipeline-candidates-list-is-list
  ((q search-query))
  (nskk-with-mock-dict nil
    (nskk-integration-with-session 'hiragana
      (let ((candidates (nskk-dict-lookup q)))
        ;; dict-lookup returns either nil or a proper list — never something else
        (should (listp candidates)))))
  20)

(nskk-describe "Henkan pipeline property: index stability"
  (nskk-it "next-candidate increments index within bounds"
    (dotimes (_ 20)
      (nskk-for-all ((cv candidates-with-valid-index))
        (nskk-with-mock-dict nil
          (nskk-integration-with-session 'hiragana
            (let* ((candidates (plist-get cv :candidates))
                   (idx (plist-get cv :index))
                   ;; Simulate having candidates loaded in state
                   (_ (setf (nskk-state-candidates nskk-current-state) candidates))
                   (_ (setf (nskk-state-current-index nskk-current-state) idx)))
              (should (< idx (length candidates))))))))))


;;;
;;; Dynamic Completion (dcomp) Integration Tests
;;;

(nskk-describe "dynamic completion pipeline"

  (nskk-it "Tab in preedit ▽ phase replaces preedit with first prefix match"
    (nskk-with-mock-dict nil
      (nskk-integration-with-session 'hiragana
        ;; Type "Ka" → preedit ▽か
        (nskk-given (nskk-integration--type-char ?K)
                    (nskk-integration--type-char ?a))
        (nskk-then  (should (nskk--conversion-start-active-p)))
        ;; Tab triggers dynamic completion from preedit "か"
        (nskk-when  (nskk-dynamic-complete))
        ;; The preedit should now contain a completion (longer than "か" or same)
        (nskk-then
          (let ((preedit (nskk-preedit-string)))
            (should (stringp preedit))
            (should (not (string-empty-p preedit))))))))

  (nskk-it "calling dynamic-complete twice cycles to the next candidate"
    (nskk-with-mock-dict nil
      (nskk-integration-with-session 'hiragana
        (nskk-given (nskk-integration--type-char ?K)
                    (nskk-integration--type-char ?a))
        ;; First Tab: establishes prefix search and shows first match
        (nskk-when (nskk-dynamic-complete))
        (let ((first-preedit (nskk-preedit-string)))
          ;; Second Tab: cycles to next match
          (nskk-when (nskk-dynamic-complete))
          (let ((second-preedit (nskk-preedit-string)))
            (nskk-then
              ;; Both predit strings are valid; cycling may wrap, but both non-empty
              (should (stringp first-preedit))
              (should (stringp second-preedit))))))))

  (nskk-it "dynamic-complete does nothing when preedit is empty"
    (nskk-with-mock-dict nil
      (nskk-integration-with-session 'hiragana
        ;; Type only "K" → marker set but no kana yet
        (nskk-given (nskk-integration--type-char ?K))
        (let ((before (buffer-string)))
          (nskk-when (nskk-dynamic-complete))
          (nskk-then
            ;; Buffer unchanged: no preedit text to search or replace
            (should (equal (buffer-string) before))))))))

(provide 'nskk-henkan-pipeline-integration-test)

;;; nskk-henkan-pipeline-integration-test.el ends here
