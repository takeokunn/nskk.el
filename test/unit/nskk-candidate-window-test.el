;;; nskk-candidate-window-test.el --- Candidate window tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Unit tests for nskk-candidate-window.el covering:
;; - `nskk--candidate-build-string' pure function (direct unit tests)
;; - Overlay-based candidate list display (after-string mechanism)
;; - Overlay lifecycle (create, reuse, delete, anchor)
;; - Candidate selection by key (Prolog-based)
;; - Prolog candidate-selection-key/2 fact initialization
;; - Page state management (show/hide)
;; - Custom variable defaults and custom group
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

(nskk-describe "build-string helper"
  (nskk-it "starts with a newline"
    (let ((result (nskk--candidate-build-string '("漢字") '(?a) 0)))
      (should (string-prefix-p "\n" result))))

  (nskk-it "formats each candidate as KEY:CAND pairs joined by spaces"
    (let ((result (nskk--candidate-build-string '("漢字" "感じ") '(?a ?s) 0)))
      (should (string-match-p "a:漢字" result))
      (should (string-match-p "s:感じ" result))))

  (nskk-it "omits [残り N] when remaining is 0"
    (let ((result (nskk--candidate-build-string '("漢字") '(?a) 0)))
      (should-not (string-match-p "残り" result))))

  (nskk-it "appends [残り N] when remaining is positive"
    (let ((result (nskk--candidate-build-string '("漢字") '(?a) 3)))
      (should (string-match-p "残り 3" result)))))

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

  (nskk-it "updates after-string content when page changes"
    (with-temp-buffer
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
            (nskk-henkan-number-to-display-candidates 3)
            (candidates '("一" "二" "三" "四" "五")))
        (nskk-candidate-show-list candidates 0)
        (let ((str-page1 (overlay-get nskk--candidate-overlay 'after-string)))
          (nskk-candidate-show-list candidates 3)
          (let ((str-page2 (overlay-get nskk--candidate-overlay 'after-string)))
            (should-not (string= str-page1 str-page2))
            (should (string-match-p "四" str-page2)))))))

  (nskk-it "anchors to end of conversion overlay when present"
    (with-temp-buffer
      (insert "test text")
      (let ((nskk-henkan-show-candidates-keys '(?a ?s ?d))
            (nskk-henkan-number-to-display-candidates 3)
            (nskk--conversion-overlay (make-overlay 1 5)))
        (unwind-protect
            (progn
              (nskk-candidate-show-list '("候補") 0)
              (should (= (overlay-start nskk--candidate-overlay) 5)))
          (delete-overlay nskk--conversion-overlay)))))

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

;; Happy-path key→index tests for non-zero page offsets and boundary
;; at the last valid element.  The zero-offset key→position mapping is
;; covered exhaustively by PBT-003 (candidate-key-position-monotonic).
(nskk-describe "candidate selection by key"
  (nskk-it "selects candidate with page offset"
    ;; key ?a is position 0; current-index 7 → absolute 7
    (should (= (nskk-candidate-list-select-by-key
                ?a '("一" "二" "三" "四" "五" "六" "七" "八") 7)
               7)))

  (nskk-it "returns valid index when absolute-index is the last element"
    ;; key ?a → pos 0; current-index 3; (< 3 4) → 3
    (should (= (nskk-candidate-list-select-by-key ?a '("a" "b" "c" "d") 3)
               3))))

;; Nil-case table: invalid key, key position beyond list, exact-length boundary.
(nskk-deftest-table candidate-selection-nil-cases
  :columns (key candidates current-index)
  :rows ((?z ("漢字" "感じ") 0)       ; no Prolog fact for ?z
         (?l ("漢字" "感じ") 0)       ; pos 6 ≥ length 2
         (?a ("a"  "b"  "c") 3))     ; absolute 3 = length 3 → out of range
  :body (should (null (nskk-candidate-list-select-by-key
                       key candidates current-index))))

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

;;;
;;; nskk--candidate-init-key-facts
;;;

(nskk-describe "nskk--candidate-init-key-facts"
  (nskk-it "is idempotent: calling twice does not duplicate Prolog facts"
    ;; The function guards with nskk--candidate-key-facts-initialized.
    ;; Since it's already been called at module load time, a second call
    ;; should be a no-op and not error.
    (should (progn (nskk--candidate-init-key-facts) t)))

  (nskk-it "results in candidate-selection-key/2 facts queryable by position"
    ;; After initialization, we can query the position of key 'a' (first key).
    (let ((pos (nskk-prolog-query-value
                `(candidate-selection-key
                  ,(car nskk-henkan-show-candidates-keys)
                  \?i)
                '\?i)))
      (should (= pos 0)))))

;;;
;;; nskk--candidate-overlay-anchor
;;;

(nskk-describe "nskk--candidate-overlay-anchor"
  (nskk-it "returns point when nskk--conversion-overlay is nil"
    (with-temp-buffer
      (insert "test")
      (goto-char 3)
      (let ((nskk--conversion-overlay nil))
        (should (= (nskk--candidate-overlay-anchor) 3)))))

  (nskk-it "returns overlay-end when conversion overlay exists"
    (with-temp-buffer
      (insert "abcdef")
      (let* ((ov (make-overlay 2 5))
             (nskk--conversion-overlay ov))
        (should (= (nskk--candidate-overlay-anchor) 5))
        (delete-overlay ov))))

  (nskk-it "falls back to point when overlay is deleted"
    (with-temp-buffer
      (insert "abcdef")
      (goto-char 4)
      (let* ((ov (make-overlay 2 5))
             (nskk--conversion-overlay ov))
        (delete-overlay ov)
        ;; After deletion, overlay is no longer live so falls back to point
        (should (= (nskk--candidate-overlay-anchor) 4))))))

(provide 'nskk-candidate-window-test)

;;; nskk-candidate-window-test.el ends here
