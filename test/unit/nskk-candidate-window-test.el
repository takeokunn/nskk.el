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

;;;
;;; Face Definition Tests
;;;

(nskk-deftest-unit candidate-key-face-defined
  "Test that candidate key face is defined."
  (should (facep 'nskk-candidate-key-face)))

(nskk-deftest-unit candidate-face-defined
  "Test that candidate face is defined."
  (should (facep 'nskk-candidate-face)))

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
;;; Overlay Behavior Tests
;;;

(nskk-deftest-unit candidate-show-list-creates-overlay
  "Test that show-list creates an overlay in the current buffer."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
          (nskk-henkan-number-to-display-candidates 7))
      (should (null nskk--candidate-overlay))
      (nskk-candidate-show-list '("漢字" "感じ" "幹事") 0)
      (should (overlayp nskk--candidate-overlay)))))

(nskk-deftest-unit candidate-show-list-sets-after-string
  "Test that show-list sets an after-string on the overlay starting with newline."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
          (nskk-henkan-number-to-display-candidates 3))
      (nskk-candidate-show-list '("候補1" "候補2") 0)
      (let ((after-str (overlay-get nskk--candidate-overlay 'after-string)))
        (should (stringp after-str))
        (should (string-prefix-p "\n" after-str))))))

(nskk-deftest-unit candidate-show-list-after-string-contains-candidates
  "Test that the overlay after-string contains candidate text."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
          (nskk-henkan-number-to-display-candidates 3))
      (nskk-candidate-show-list '("漢字" "感じ") 0)
      (let ((after-str (overlay-get nskk--candidate-overlay 'after-string)))
        (should (string-match-p "漢字" after-str))
        (should (string-match-p "感じ" after-str))))))

(nskk-deftest-unit candidate-show-list-reuses-overlay-on-page-change
  "Test that a second show-list call reuses the existing overlay."
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

(nskk-deftest-unit candidate-show-list-remaining-count-in-after-string
  "Test that [残り N] appears in after-string when candidates remain."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
          (nskk-henkan-number-to-display-candidates 3)
          (candidates '("一" "二" "三" "四" "五")))
      (nskk-candidate-show-list candidates 0)
      (let ((after-str (overlay-get nskk--candidate-overlay 'after-string)))
        ;; 5 candidates, page size 3, so 2 remain
        (should (string-match-p "残り" after-str))
        (should (string-match-p "2" after-str))))))

(nskk-deftest-unit candidate-show-list-no-remaining-when-all-fit
  "Test that [残り N] does NOT appear when all candidates fit on one page."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
          (nskk-henkan-number-to-display-candidates 7)
          (candidates '("一" "二" "三")))
      ;; 3 candidates, page size 7 -- all fit, remaining = 0
      (nskk-candidate-show-list candidates 0)
      (let ((after-str (overlay-get nskk--candidate-overlay 'after-string)))
        (should (stringp after-str))
        (should-not (string-match-p "残り" after-str))))))

(nskk-deftest-unit candidate-hide-list-deletes-overlay
  "Test that hide-list deletes the overlay and sets it to nil."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))
          (nskk-henkan-number-to-display-candidates 7))
      (nskk-candidate-show-list '("test") 0)
      (should (overlayp nskk--candidate-overlay))
      (nskk-candidate-hide-list)
      ;; Overlay variable must be nil after hide
      (should (null nskk--candidate-overlay)))))

(nskk-deftest-unit candidate-show-list-fallback-to-point-without-conversion-overlay
  "Test that show-list falls back to point when conversion overlay is absent."
  (with-temp-buffer
    (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
          (nskk-henkan-number-to-display-candidates 3)
          ;; Ensure no conversion overlay is set
          (nskk--conversion-overlay nil))
      (nskk-candidate-show-list '("候補") 0)
      ;; Should succeed and create an overlay (at point fallback)
      (should (overlayp nskk--candidate-overlay)))))

(nskk-deftest-unit candidate-overlay-cleared-after-conversion-commit
  "Test that nskk--clear-conversion-context cleans up the candidate overlay.
Wires nskk-candidate-hide-list into nskk-henkan-hide-candidates-functions
(as nskk--enable does) then calls nskk--clear-conversion-context and
asserts that both nskk--candidate-list-active and nskk--candidate-overlay
are nil afterwards."
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
                     #'nskk-candidate-hide-list)))))

;;;
;;; Prolog Key Selection Facts Tests
;;;

(nskk-deftest-unit candidate-selection-key-facts-initialized
  "Test that candidate-selection-key Prolog facts are initialized at load time."
  (should (nskk-prolog-query-one '(candidate-selection-key \?a \?pos))))

(nskk-deftest-unit candidate-selection-key-default-mapping
  "Test that all 7 default selection keys map to positions 0-6."
  (cl-loop for key in '(?a ?s ?d ?f ?j ?k ?l)
           for expected-pos from 0
           do (should (= (nskk-prolog-query-value
                          `(candidate-selection-key ,key ,'\?pos) '\?pos)
                         expected-pos))))

(nskk-deftest-unit candidate-selection-key-invalid-returns-nil
  "Test that non-selection keys have no Prolog fact."
  (should (null (nskk-prolog-query-one `(candidate-selection-key ,?z ,'\?pos)))))

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

(nskk-deftest-unit candidate-select-by-key-boundary-at-length
  "Test that absolute-index == (length candidates) returns nil (out of bounds).
With candidates '(\"a\" \"b\" \"c\") (length 3) and current-index 3,
key ?a maps to position 0 so absolute-index = 3 + 0 = 3.
(< 3 3) is false, so the result must be nil."
  (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
    (let ((result (nskk-candidate-list-select-by-key ?a '("a" "b" "c") 3)))
      (should (null result)))))

(nskk-deftest-unit candidate-select-by-key-boundary-just-within
  "Test that absolute-index == (length candidates) - 1 returns a valid index.
With candidates '(\"a\" \"b\" \"c\" \"d\") (length 4) and current-index 3,
key ?a maps to position 0 so absolute-index = 3 + 0 = 3.
(< 3 4) is true, so the result must be 3."
  (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)))
    (let ((result (nskk-candidate-list-select-by-key ?a '("a" "b" "c" "d") 3)))
      (should (= result 3)))))

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
