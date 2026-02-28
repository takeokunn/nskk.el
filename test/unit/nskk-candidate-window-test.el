;;; nskk-candidate-window-test.el --- Candidate window tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-candidate-window.el covering:
;; - Echo area candidate list display
;; - Candidate selection by key
;; - Page state management (show/hide)
;; - Custom variable defaults
;; - Face definitions

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-candidate-window)
(require 'nskk-test-framework)

;;;
;;; Face Definition Tests
;;;

(nskk-deftest-unit candidate-key-face-defined
  "Test that candidate key face is defined."
  (should (facep 'nskk-candidate-key-face)))

(nskk-deftest-unit candidate-face-defined
  "Test that candidate face is defined."
  (should (facep 'nskk-candidate-face)))

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
;;; State Variable Tests
;;;

(nskk-deftest-unit candidate-list-page-starts-at-zero
  "Test that page number defaults to 0."
  (with-temp-buffer
    (should (= nskk--candidate-list-page 0))))

(nskk-deftest-unit candidate-list-active-starts-nil
  "Test that candidate list active defaults to nil."
  (with-temp-buffer
    (should (null nskk--candidate-list-active))))

;;;
;;; Show List Tests
;;;

(nskk-deftest-unit candidate-show-list-basic
  "Test showing candidates in echo area."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
          (nskk-henkan-number-to-display-candidates 7))
      (let ((result (nskk-candidate-show-list '("漢字" "感じ" "幹事") 0)))
        ;; Should return the page candidates
        (should (equal result '("漢字" "感じ" "幹事")))
        ;; Should mark as active
        (should nskk--candidate-list-active)))))

(nskk-deftest-unit candidate-show-list-pagination
  "Test that show-list respects per-page limit."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
          (nskk-henkan-number-to-display-candidates 3)
          (candidates '("一" "二" "三" "四" "五")))
      (let ((result (nskk-candidate-show-list candidates 0)))
        ;; Should only return first 3
        (should (= (length result) 3))
        (should (equal result '("一" "二" "三")))))))

(nskk-deftest-unit candidate-show-list-second-page
  "Test showing second page of candidates."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
          (nskk-henkan-number-to-display-candidates 3)
          (candidates '("一" "二" "三" "四" "五")))
      (let ((result (nskk-candidate-show-list candidates 3)))
        ;; Should return candidates 4 and 5
        (should (= (length result) 2))
        (should (equal result '("四" "五")))))))

(nskk-deftest-unit candidate-show-list-sets-page
  "Test that show-list correctly sets the page number."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
          (nskk-henkan-number-to-display-candidates 7))
      (nskk-candidate-show-list '("a" "b" "c" "d" "e" "f" "g" "h" "i") 7)
      (should (= nskk--candidate-list-page 1)))))

;;;
;;; List Active Predicate Tests
;;;

(nskk-deftest-unit candidate-list-active-p-when-inactive
  "Test list-active-p returns nil when not active."
  (with-temp-buffer
    (should-not (nskk-candidate-list-active-p))))

(nskk-deftest-unit candidate-list-active-p-when-active
  "Test list-active-p returns non-nil when active."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
          (nskk-henkan-number-to-display-candidates 7))
      (nskk-candidate-show-list '("test") 0)
      (should (nskk-candidate-list-active-p)))))

;;;
;;; Hide List Tests
;;;

(nskk-deftest-unit candidate-hide-list-resets-state
  "Test that hide-list resets all state."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
          (nskk-henkan-number-to-display-candidates 7))
      (nskk-candidate-show-list '("test") 0)
      (should nskk--candidate-list-active)
      (nskk-candidate-hide-list)
      (should-not nskk--candidate-list-active)
      (should (= nskk--candidate-list-page 0)))))

(nskk-deftest-unit candidate-hide-list-when-not-active
  "Test that hide-list is safe when not active."
  (with-temp-buffer
    ;; Should not error
    (nskk-candidate-hide-list)
    (should-not nskk--candidate-list-active)))

;;;
;;; Key Selection Tests
;;;

(nskk-deftest-unit candidate-select-by-key-first
  "Test selecting first candidate by key."
  (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
    (let ((result (nskk-candidate-list-select-by-key ?a '("漢字" "感じ" "幹事") 0)))
      (should (= result 0)))))

(nskk-deftest-unit candidate-select-by-key-middle
  "Test selecting middle candidate by key."
  (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
    (let ((result (nskk-candidate-list-select-by-key ?d '("漢字" "感じ" "幹事") 0)))
      (should (= result 2)))))

(nskk-deftest-unit candidate-select-by-key-with-offset
  "Test selecting candidate with page offset."
  (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
    (let ((result (nskk-candidate-list-select-by-key ?a '("一" "二" "三" "四" "五" "六" "七" "八") 7)))
      (should (= result 7)))))

(nskk-deftest-unit candidate-select-by-key-invalid
  "Test selecting with invalid key returns nil."
  (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
    (let ((result (nskk-candidate-list-select-by-key ?z '("漢字" "感じ") 0)))
      (should (null result)))))

(nskk-deftest-unit candidate-select-by-key-out-of-range
  "Test selecting beyond available candidates returns nil."
  (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
    (let ((result (nskk-candidate-list-select-by-key ?l '("漢字" "感じ") 0)))
      (should (null result)))))

(provide 'nskk-candidate-window-test)

;;; nskk-candidate-window-test.el ends here
