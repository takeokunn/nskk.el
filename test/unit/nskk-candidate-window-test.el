;;; nskk-candidate-window-test.el --- Candidate window tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Unit tests for nskk-candidate-window.el covering:
;; - Overlay-based candidate list display (after-string mechanism)
;; - Overlay lifecycle (create, reuse, delete)
;; - Candidate selection by key (Prolog-based)
;; - Prolog candidate-selection-key/2 fact initialization
;; - Page state management (show/hide)
;; - Custom variable defaults
;; - Face definitions

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-candidate-window)
(require 'nskk-prolog)
(require 'nskk-state)
(require 'nskk-test-framework)

(nskk-describe "face definitions"
  (nskk-it "candidate key face is defined"
    (should (facep 'nskk-candidate-key-face)))

  (nskk-it "candidate face is defined"
    (should (facep 'nskk-candidate-face))))

(nskk-describe "custom group"
  (nskk-it "nskk-candidate-window custom group exists"
    (should (get 'nskk-candidate-window 'custom-group))))

(nskk-describe "state variable defaults"
  (nskk-it "page number defaults to 0"
    (with-temp-buffer
      (should (= nskk--candidate-list-page 0))))

  (nskk-it "candidate list active defaults to nil"
    (with-temp-buffer
      (should (null nskk--candidate-list-active)))))

(nskk-describe "show candidate list"
  (nskk-it "returns the page candidates for a basic list"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7))
        (let ((result (nskk-candidate-show-list '("漢字" "感じ" "幹事") 0)))
          ;; Should return the page candidates
          (should (equal result '("漢字" "感じ" "幹事")))
          ;; Should mark as active
          (should nskk--candidate-list-active)))))

  (nskk-it "respects per-page limit for pagination"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
            (nskk-henkan-number-to-display-candidates 3)
            (candidates '("一" "二" "三" "四" "五")))
        (let ((result (nskk-candidate-show-list candidates 0)))
          ;; Should only return first 3
          (should (= (length result) 3))
          (should (equal result '("一" "二" "三")))))))

  (nskk-it "returns second page of candidates"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
            (nskk-henkan-number-to-display-candidates 3)
            (candidates '("一" "二" "三" "四" "五")))
        (let ((result (nskk-candidate-show-list candidates 3)))
          ;; Should return candidates 4 and 5
          (should (= (length result) 2))
          (should (equal result '("四" "五")))))))

  (nskk-it "correctly sets the page number"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7))
        (nskk-candidate-show-list '("a" "b" "c" "d" "e" "f" "g" "h" "i") 7)
        (should (= nskk--candidate-list-page 1))))))

(nskk-describe "list active predicate"
  (nskk-it "returns nil when not active"
    (with-temp-buffer
      (should-not (nskk-candidate-list-active-p))))

  (nskk-it "returns non-nil when active"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7))
        (nskk-candidate-show-list '("test") 0)
        (should (nskk-candidate-list-active-p))))))

(nskk-describe "hide candidate list"
  (nskk-it "resets all state after hide"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7))
        (nskk-candidate-show-list '("test") 0)
        (should nskk--candidate-list-active)
        (nskk-candidate-hide-list)
        (should-not nskk--candidate-list-active)
        (should (= nskk--candidate-list-page 0)))))

  (nskk-it "is safe to call when not active"
    (with-temp-buffer
      ;; Should not error
      (nskk-candidate-hide-list)
      (should-not nskk--candidate-list-active))))

(nskk-describe "overlay behavior"
  (nskk-it "creates an overlay in the current buffer"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7))
        (should (null nskk--candidate-overlay))
        (nskk-candidate-show-list '("漢字" "感じ" "幹事") 0)
        (should (overlayp nskk--candidate-overlay)))))

  (nskk-it "sets an after-string on the overlay starting with newline"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
            (nskk-henkan-number-to-display-candidates 3))
        (nskk-candidate-show-list '("候補1" "候補2") 0)
        (let ((after-str (overlay-get nskk--candidate-overlay 'after-string)))
          (should (stringp after-str))
          (should (string-prefix-p "\n" after-str))))))

  (nskk-it "includes candidate text in after-string"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
            (nskk-henkan-number-to-display-candidates 3))
        (nskk-candidate-show-list '("漢字" "感じ") 0)
        (let ((after-str (overlay-get nskk--candidate-overlay 'after-string)))
          (should (string-match-p "漢字" after-str))
          (should (string-match-p "感じ" after-str))))))

  (nskk-it "reuses the existing overlay on page change"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
            (nskk-henkan-number-to-display-candidates 3)
            (candidates '("一" "二" "三" "四" "五")))
        ;; First call creates overlay
        (nskk-candidate-show-list candidates 0)
        (let ((first-overlay nskk--candidate-overlay))
          (should (overlayp first-overlay))
          ;; Second call (page 2) must reuse — not create a new overlay
          (nskk-candidate-show-list candidates 3)
          (should (eq nskk--candidate-overlay first-overlay))))))

  (nskk-it "shows [残り N] in after-string when candidates remain"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
            (nskk-henkan-number-to-display-candidates 3)
            (candidates '("一" "二" "三" "四" "五")))
        (nskk-candidate-show-list candidates 0)
        (let ((after-str (overlay-get nskk--candidate-overlay 'after-string)))
          ;; 5 candidates, page size 3, so 2 remain
          (should (string-match-p "残り" after-str))
          (should (string-match-p "2" after-str))))))

  (nskk-it "does NOT show [残り N] when all candidates fit on one page"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7)
            (candidates '("一" "二" "三")))
        ;; 3 candidates, page size 7 -- all fit, remaining = 0
        (nskk-candidate-show-list candidates 0)
        (let ((after-str (overlay-get nskk--candidate-overlay 'after-string)))
          (should (stringp after-str))
          (should-not (string-match-p "残り" after-str))))))

  (nskk-it "deletes the overlay and sets it to nil on hide"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7))
        (nskk-candidate-show-list '("test") 0)
        (should (overlayp nskk--candidate-overlay))
        (nskk-candidate-hide-list)
        ;; Overlay variable must be nil after hide
        (should (null nskk--candidate-overlay)))))

  (nskk-it "falls back to point when conversion overlay is absent"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
            (nskk-henkan-number-to-display-candidates 3)
            ;; Ensure no conversion overlay is set
            (nskk--conversion-overlay nil))
        (nskk-candidate-show-list '("候補") 0)
        ;; Should succeed and create an overlay (at point fallback)
        (should (overlayp nskk--candidate-overlay)))))

  (nskk-it "is cleaned up after nskk--clear-conversion-context"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
            (nskk-henkan-number-to-display-candidates 7)
            ;; Provide the minimal state variables nskk--clear-conversion-context
            ;; accesses so it does not error in a plain temp buffer.
            (nskk--conversion-overlay nil)
            (nskk--conversion-start-marker (make-marker))
            (nskk--romaji-buffer "")
            (nskk--pending-romaji-overlay nil))
        ;; Show the candidate list so the overlay exists and active flag is set.
        (nskk-candidate-show-list '("漢字" "感じ" "幹事") 0)
        (should nskk--candidate-list-active)
        (should (overlayp nskk--candidate-overlay))
        ;; Wire hide function into the hook, mirroring what nskk--enable does.
        (add-hook 'nskk-henkan-hide-candidates-functions #'nskk-candidate-hide-list)
        (unwind-protect
            (nskk-with-current-state
              ;; nskk--clear-conversion-context calls run-hook-with-args on
              ;; nskk-henkan-hide-candidates-functions, which invokes
              ;; nskk-candidate-hide-list and should clear the overlay state.
              (nskk--clear-conversion-context)
              ;; Candidate list active flag must be cleared.
              (should-not nskk--candidate-list-active)
              ;; Candidate overlay must be deleted and set to nil.
              (should (null nskk--candidate-overlay)))
          (remove-hook 'nskk-henkan-hide-candidates-functions
                       #'nskk-candidate-hide-list))))))

(nskk-describe "Prolog key selection facts"
  (nskk-it "candidate-selection-key Prolog facts are initialized at load time"
    (should (nskk-prolog-query-one '(candidate-selection-key \?a \?pos))))

  (nskk-it "all 7 default selection keys map to positions 0-6"
    (cl-loop for key in '(?a ?s ?d ?f ?j ?k ?l)
             for expected-pos from 0
             do (should (= (nskk-prolog-query-value
                            `(candidate-selection-key ,key ,'\?pos) '\?pos)
                           expected-pos))))

  (nskk-it "non-selection keys have no Prolog fact"
    (should (null (nskk-prolog-query-one `(candidate-selection-key ,?z ,'\?pos))))))

(nskk-describe "candidate selection by key"
  (nskk-it "selects first candidate by key"
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
      (let ((result (nskk-candidate-list-select-by-key ?a '("漢字" "感じ" "幹事") 0)))
        (should (= result 0)))))

  (nskk-it "selects middle candidate by key"
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
      (let ((result (nskk-candidate-list-select-by-key ?d '("漢字" "感じ" "幹事") 0)))
        (should (= result 2)))))

  (nskk-it "selects candidate with page offset"
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
      (let ((result (nskk-candidate-list-select-by-key ?a '("一" "二" "三" "四" "五" "六" "七" "八") 7)))
        (should (= result 7)))))

  (nskk-it "returns nil for invalid key"
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
      (let ((result (nskk-candidate-list-select-by-key ?z '("漢字" "感じ") 0)))
        (should (null result)))))

  (nskk-it "returns nil when selection is beyond available candidates"
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
      (let ((result (nskk-candidate-list-select-by-key ?l '("漢字" "感じ") 0)))
        (should (null result)))))

  (nskk-it "returns nil when absolute-index equals length of candidates"
    ;; With candidates '(\"a\" \"b\" \"c\") (length 3) and current-index 3,
    ;; key ?a maps to position 0 so absolute-index = 3 + 0 = 3.
    ;; (< 3 3) is false, so the result must be nil.
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
      (let ((result (nskk-candidate-list-select-by-key ?a '("a" "b" "c") 3)))
        (should (null result)))))

  (nskk-it "returns valid index when absolute-index is last element"
    ;; With candidates '(\"a\" \"b\" \"c\" \"d\") (length 4) and current-index 3,
    ;; key ?a maps to position 0 so absolute-index = 3 + 0 = 3.
    ;; (< 3 4) is true, so the result must be 3.
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
      (let ((result (nskk-candidate-list-select-by-key ?a '("a" "b" "c" "d") 3)))
        (should (= result 3))))))

;;;
;;; PBT-001 — Pagination invariant (seeded PBT with manual random generation)
;;;

(require 'nskk-test-macros)

(nskk-property-test-exhaustive candidate-pagination-invariant
  '(3 4 5 6 7)
  ;; item = page-size; exhaustively test each page size in {3,4,5,6,7}
  (let* ((pg-size item)
         (n-candidates (+ 1 (random 20)))
         (all-candidates (cl-loop repeat n-candidates
                                  for i from 0
                                  collect (format "candidate-%d" i)))
         (nskk-henkan-show-candidates-keys
          (cl-subseq '(?a ?s ?d ?f ?j ?k ?l) 0 (min pg-size 7)))
         (nskk-henkan-number-to-display-candidates pg-size))
    (with-temp-buffer
      ;; First page must return min(pg-size, n-candidates) elements
      (let* ((result (nskk-candidate-show-list all-candidates 0))
             (expected-count (min pg-size n-candidates)))
        (= (length result) expected-count)))))

;;;
;;; PBT-002 — Selection key exhaustive test for all 7 default keys
;;;

(nskk-property-test-exhaustive candidate-selection-key-returns-valid-index
  '(?a ?s ?d ?f ?j ?k ?l)
  (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
        (candidates '("a" "b" "c" "d" "e" "f" "g")))
    (let ((result (nskk-candidate-list-select-by-key item candidates 0)))
      ;; Each key must return a number in [0, 6]
      (and (numberp result)
           (>= result 0)
           (<= result 6)))))

;;;
;;; PBT-003 — Key position monotonicity (table-driven)
;;;

(nskk-deftest-table candidate-key-position-monotonic
  :columns (key expected-position)
  :rows ((?a 0)
         (?s 1)
         (?d 2)
         (?f 3)
         (?j 4)
         (?k 5)
         (?l 6))
  :body (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
              (candidates '("a" "b" "c" "d" "e" "f" "g")))
          (let ((result (nskk-candidate-list-select-by-key key candidates 0)))
            (should (= result expected-position)))))

(provide 'nskk-candidate-window-test)

;;; nskk-candidate-window-test.el ends here
