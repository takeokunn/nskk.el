;;; nskk-candidate-window-test.el --- Candidate window tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-candidate-window.el covering:
;; - Candidate formatting and display
;; - Candidate selection and navigation
;; - Page navigation
;; - Overlay management (show/hide)
;; - Custom variable defaults
;; - Face definitions

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-candidate-window)
(require 'nskk-test-framework)

;;;
;;; Custom Variable Default Tests
;;;

(nskk-deftest-unit candidate-window-max-candidates-default
  "Test default max candidates per page."
  (should (= nskk-candidate-window-max-candidates 9)))

(nskk-deftest-unit candidate-window-show-numbers-default
  "Test default show-numbers setting."
  (should (eq nskk-candidate-window-show-numbers t)))

;;;
;;; Face Definition Tests
;;;

(nskk-deftest-unit candidate-face-defined
  "Test that candidate face is defined."
  (should (facep 'nskk-candidate-face)))

(nskk-deftest-unit candidate-selected-face-defined
  "Test that selected candidate face is defined."
  (should (facep 'nskk-candidate-selected-face)))

(nskk-deftest-unit candidate-annotation-face-defined
  "Test that annotation face is defined."
  (should (facep 'nskk-candidate-annotation-face)))

;;;
;;; Defgroup Tests
;;;

(nskk-deftest-unit candidate-window-group-exists
  "Test that nskk-candidate-window custom group exists."
  (should (get 'nskk-candidate-window 'custom-group)))

;;;
;;; Formatting Tests
;;;

(nskk-deftest-unit candidate-window-format-single
  "Test formatting a single candidate."
  (let ((nskk--candidate-index 0)
        (nskk-candidate-window-show-numbers t))
    (let ((result (nskk-candidate-window--format '("test") 0)))
      (should (stringp result))
      ;; Should contain the number prefix "1. "
      (should (string-match-p "1\\." result))
      ;; Should contain the candidate text
      (should (string-match-p "test" result)))))

(nskk-deftest-unit candidate-window-format-multiple
  "Test formatting multiple candidates."
  (let ((nskk--candidate-index 0)
        (nskk-candidate-window-show-numbers t))
    (let ((result (nskk-candidate-window--format
                   '("first" "second" "third") 0)))
      (should (stringp result))
      (should (string-match-p "1\\." result))
      (should (string-match-p "2\\." result))
      (should (string-match-p "3\\." result))
      (should (string-match-p "first" result))
      (should (string-match-p "second" result))
      (should (string-match-p "third" result)))))

(nskk-deftest-unit candidate-window-format-without-numbers
  "Test formatting candidates without numbers."
  (let ((nskk--candidate-index 0)
        (nskk-candidate-window-show-numbers nil))
    (let ((result (nskk-candidate-window--format '("test") 0)))
      (should (stringp result))
      ;; Should NOT contain number prefix
      (should-not (string-match-p "1\\." result))
      ;; Should still contain candidate text
      (should (string-match-p "test" result)))))

(nskk-deftest-unit candidate-window-format-selected-face
  "Test that the selected candidate gets the selected face."
  (let ((nskk--candidate-index 1)
        (nskk-candidate-window-show-numbers t))
    (let ((result (nskk-candidate-window--format
                   '("first" "selected" "third") 0)))
      ;; The result should contain text with nskk-candidate-selected-face
      ;; on the "selected" candidate (index 1)
      (should (stringp result)))))

(nskk-deftest-unit candidate-window-format-newline-separated
  "Test that multiple candidates are separated by newlines."
  (let ((nskk--candidate-index 0)
        (nskk-candidate-window-show-numbers t))
    (let ((result (nskk-candidate-window--format
                   '("one" "two" "three") 0)))
      ;; Count newlines in result
      (let ((newline-count 0))
        (dotimes (i (length result))
          (when (= (aref result i) ?\n)
            (cl-incf newline-count)))
        ;; 3 items should have 2 newlines
        (should (= newline-count 2))))))

(nskk-deftest-unit candidate-window-format-japanese-candidates
  "Test formatting Japanese candidates."
  (let ((nskk--candidate-index 0)
        (nskk-candidate-window-show-numbers t))
    (let ((result (nskk-candidate-window--format
                   '("\u6F22\u5B57" "\u611F\u3058" "\u5E7E\u6642") 0)))
      (should (stringp result))
      (should (string-match-p "\u6F22\u5B57" result))
      (should (string-match-p "\u611F\u3058" result))
      (should (string-match-p "\u5E7E\u6642" result)))))

;;;
;;; Annotation Tests
;;;

(nskk-deftest-unit candidate-window-annotation-returns-nil
  "Test annotation function returns nil for now."
  (should (null (nskk-candidate-window--annotation "test")))
  (should (null (nskk-candidate-window--annotation "\u6F22\u5B57")))
  (should (null (nskk-candidate-window--annotation nil))))

;;;
;;; Navigation Tests - Next/Prev Candidate
;;;

(nskk-deftest-unit candidate-window-next-basic
  "Test moving to next candidate."
  (let ((nskk--candidate-index 0))
    (should (nskk-candidate-window-next))
    (should (= nskk--candidate-index 1))))

(nskk-deftest-unit candidate-window-next-multiple
  "Test moving through multiple next candidates."
  (let ((nskk--candidate-index 0))
    (should (nskk-candidate-window-next))
    (should (= nskk--candidate-index 1))
    (should (nskk-candidate-window-next))
    (should (= nskk--candidate-index 2))))

(nskk-deftest-unit candidate-window-next-at-max
  "Test next candidate at max position returns nil."
  (let ((nskk--candidate-index (1- nskk-candidate-window-max-candidates)))
    (should-not (nskk-candidate-window-next))
    (should (= nskk--candidate-index (1- nskk-candidate-window-max-candidates)))))

(nskk-deftest-unit candidate-window-prev-basic
  "Test moving to previous candidate."
  (let ((nskk--candidate-index 3))
    (should (nskk-candidate-window-prev))
    (should (= nskk--candidate-index 2))))

(nskk-deftest-unit candidate-window-prev-at-zero
  "Test prev candidate at zero position returns nil."
  (let ((nskk--candidate-index 0))
    (should-not (nskk-candidate-window-prev))
    (should (= nskk--candidate-index 0))))

(nskk-deftest-unit candidate-window-next-prev-roundtrip
  "Test next then prev returns to original position."
  (let ((nskk--candidate-index 3))
    (nskk-candidate-window-next)
    (should (= nskk--candidate-index 4))
    (nskk-candidate-window-prev)
    (should (= nskk--candidate-index 3))))

;;;
;;; Page Navigation Tests
;;;

(nskk-deftest-unit candidate-window-next-page-basic
  "Test next page navigation."
  (let ((nskk--candidate-page 0)
        (nskk--candidate-index 3))
    ;; 20 candidates, max 9 per page -> 3 pages (0, 1, 2)
    (should (nskk-candidate-window-next-page 20))
    (should (= nskk--candidate-page 1))
    ;; Index should be reset to 0
    (should (= nskk--candidate-index 0))))

(nskk-deftest-unit candidate-window-next-page-last-page
  "Test next page at last page returns nil."
  (let ((nskk--candidate-page 2)
        (nskk--candidate-index 0))
    ;; 20 candidates, max 9 per page -> last page is 2
    (should-not (nskk-candidate-window-next-page 20))
    (should (= nskk--candidate-page 2))))

(nskk-deftest-unit candidate-window-prev-page-basic
  "Test previous page navigation."
  (let ((nskk--candidate-page 2)
        (nskk--candidate-index 5))
    (should (nskk-candidate-window-prev-page))
    (should (= nskk--candidate-page 1))
    ;; Index should be reset to 0
    (should (= nskk--candidate-index 0))))

(nskk-deftest-unit candidate-window-prev-page-at-first
  "Test prev page at first page returns nil."
  (let ((nskk--candidate-page 0)
        (nskk--candidate-index 0))
    (should-not (nskk-candidate-window-prev-page))
    (should (= nskk--candidate-page 0))))

(nskk-deftest-unit candidate-window-page-navigation-roundtrip
  "Test next then prev page returns to same page."
  (let ((nskk--candidate-page 1)
        (nskk--candidate-index 0))
    (nskk-candidate-window-next-page 30)
    (should (= nskk--candidate-page 2))
    (nskk-candidate-window-prev-page)
    (should (= nskk--candidate-page 1))))

;;;
;;; Overlay Show/Hide Tests
;;;

(nskk-deftest-unit candidate-window-show-creates-overlay
  "Test that show creates an overlay."
  (with-temp-buffer
    (insert "test text")
    (goto-char (point-min))
    (let ((nskk--candidate-overlay nil)
          (nskk--candidate-index 0))
      (nskk-candidate-window-show '("cand1" "cand2") 0)
      (should (overlayp nskk--candidate-overlay))
      ;; Cleanup
      (nskk-candidate-window-hide))))

(nskk-deftest-unit candidate-window-hide-removes-overlay
  "Test that hide removes the overlay."
  (with-temp-buffer
    (insert "test text")
    (goto-char (point-min))
    (let ((nskk--candidate-overlay nil)
          (nskk--candidate-index 0))
      (nskk-candidate-window-show '("cand1") 0)
      (should (overlayp nskk--candidate-overlay))
      (nskk-candidate-window-hide)
      (should (null nskk--candidate-overlay)))))

(nskk-deftest-unit candidate-window-hide-when-no-overlay
  "Test that hide when no overlay is safe."
  (let ((nskk--candidate-overlay nil))
    ;; Should not error
    (nskk-candidate-window-hide)
    (should (null nskk--candidate-overlay))))

(nskk-deftest-unit candidate-window-show-replaces-existing
  "Test that show replaces existing overlay."
  (with-temp-buffer
    (insert "test text")
    (goto-char (point-min))
    (let ((nskk--candidate-overlay nil)
          (nskk--candidate-index 0))
      (nskk-candidate-window-show '("first") 0)
      (let ((first-overlay nskk--candidate-overlay))
        (nskk-candidate-window-show '("second") 0)
        ;; Old overlay should be deleted
        (should-not (overlay-buffer first-overlay))
        ;; New overlay should exist
        (should (overlayp nskk--candidate-overlay)))
      ;; Cleanup
      (nskk-candidate-window-hide))))

(nskk-deftest-unit candidate-window-show-overlay-has-before-string
  "Test that the overlay has a before-string property."
  (with-temp-buffer
    (insert "test text")
    (goto-char (point-min))
    (let ((nskk--candidate-overlay nil)
          (nskk--candidate-index 0))
      (nskk-candidate-window-show '("cand1" "cand2") 0)
      (let ((before-str (overlay-get nskk--candidate-overlay 'before-string)))
        (should (stringp before-str))
        (should (string-match-p "cand1" before-str))
        (should (string-match-p "cand2" before-str)))
      ;; Cleanup
      (nskk-candidate-window-hide))))

(nskk-deftest-unit candidate-window-show-respects-max
  "Test that show only displays up to max candidates."
  (with-temp-buffer
    (insert "test text")
    (goto-char (point-min))
    (let ((nskk--candidate-overlay nil)
          (nskk--candidate-index 0)
          (candidates (cl-loop for i from 1 to 15
                               collect (format "cand%d" i))))
      ;; Show from start - should show at most 9
      (nskk-candidate-window-show candidates 0)
      (let ((before-str (overlay-get nskk--candidate-overlay 'before-string)))
        (should (string-match-p "cand1" before-str))
        (should (string-match-p "cand9" before-str))
        ;; cand10 should NOT be visible on first page
        (should-not (string-match-p "cand10" before-str)))
      ;; Cleanup
      (nskk-candidate-window-hide))))

;;;
;;; State Variable Tests
;;;

(nskk-deftest-unit candidate-window-page-starts-at-zero
  "Test that page number defaults to 0."
  (should (= nskk--candidate-page 0)))

(nskk-deftest-unit candidate-window-index-starts-at-zero
  "Test that candidate index defaults to 0."
  (should (= nskk--candidate-index 0)))

(provide 'nskk-candidate-window-test)

;;; nskk-candidate-window-test.el ends here
