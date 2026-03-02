;;; nskk-henkan-test.el --- Henkan pipeline tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-henkan.el covering:
;; - Feature loading and API availability
;; - New macros: nskk-henkan-dispatch, nskk-henkan-with-preedit,
;;   nskk-with-conversion-context
;; - New Prolog predicates: candidate-nav-next-action/3,
;;   candidate-nav-prev-action/2, search-result-action/2,
;;   convert-or-commit-action/2, okurigana-trigger/1,
;;   should-update-overlay/1
;; - Existing Prolog predicates: converting-phase/1, okurigana-char/2
;; - nskk-converting-p across all henkan phases
;; - nskk-detect-okurigana-char character classification
;; - nskk-next-candidate / nskk-previous-candidate dispatch
;; - nskk-commit-current candidate insertion
;; - nskk-convert / nskk-convert-or-commit control flow
;; - nskk-cancel-conversion / nskk-cancel-preedit state cleanup
;; - nskk-start-registration depth guard
;; - nskk-without-modification macro behavior

;;; Code:

(require 'ert)
(require 'nskk-henkan)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)

;;;
;;; henkan initialization
;;;

(nskk-describe "henkan initialization"
  (nskk-it "provides the nskk-henkan feature"
    (should (featurep 'nskk-henkan)))

  (nskk-it "can be required again safely"
    (should (require 'nskk-henkan)))

  (nskk-context "macro API availability"
    (nskk-deftest-table henkan-macro-api-defined
      :description "All henkan macros are fboundp"
      :columns (sym)
      :rows ((nskk-without-modification)
             (nskk-henkan-dispatch)
             (nskk-henkan-with-preedit)
             (nskk-with-conversion-context))
      :body (should (fboundp sym)))

    (nskk-deftest-table henkan-public-api-interactive
      :description "Public henkan commands are interactive"
      :columns (sym)
      :rows ((nskk-commit-current)
             (nskk-next-candidate)
             (nskk-previous-candidate)
             (nskk-convert)
             (nskk-convert-or-commit)
             (nskk-cancel-conversion)
             (nskk-cancel-preedit))
      :body (should (commandp sym)))

    (nskk-deftest-table henkan-function-api-defined
      :description "Public henkan functions are fboundp"
      :columns (sym)
      :rows ((nskk-core-search)
             (nskk-detect-okurigana-char)
             (nskk-process-okurigana-input))
      :body (should (fboundp sym)))

    (nskk-deftest-table henkan-deleted-api-removed
      :description "Parallel API symbols have been removed"
      :columns (sym)
      :rows ((nskk-henkan-start-conversion)
             (nskk-henkan-commit-conversion)
             (nskk-henkan-cancel-conversion)
             (nskk-henkan-in-conversion-p)
             (nskk-henkan-has-candidates-p)
             (nskk-henkan-get-current-candidate))
      :body (should-not (fboundp sym)))))

;;;
;;; nskk-without-modification Macro Tests
;;;

(nskk-describe "nskk-without-modification"
  (nskk-it "inhibits undo recording inside body"
    (with-temp-buffer
      (let (captured-undo-list)
        (nskk-without-modification
          (setq captured-undo-list buffer-undo-list))
        (should (eq captured-undo-list t)))))

  (nskk-it "inhibits modification hooks inside body"
    (with-temp-buffer
      (let (captured-inhibit)
        (nskk-without-modification
          (setq captured-inhibit inhibit-modification-hooks))
        (should (eq captured-inhibit t)))))

  (nskk-it "returns the value of its body"
    (nskk-then
      (should (equal (nskk-without-modification 42) 42))
      (should (equal (nskk-without-modification "hello") "hello"))
      (should (equal (nskk-without-modification (+ 1 2)) 3)))))

;;;
;;; nskk-henkan-dispatch Macro Tests
;;;

(nskk-describe "nskk-henkan-dispatch"
  (nskk-it "executes the first matching clause (show-overlay)"
    (let ((result nil))
      (nskk-henkan-dispatch action
          (nskk-prolog-query-value '(search-result-action has-candidates \?a) '\?a)
        (show-overlay    (setq result 'overlay-shown))
        (start-registration (setq result 'registration-started)))
      (should (eq result 'overlay-shown))))

  (nskk-it "executes the second matching clause (start-registration)"
    (let ((result nil))
      (nskk-henkan-dispatch action
          (nskk-prolog-query-value '(search-result-action no-candidates \?a) '\?a)
        (show-overlay    (setq result 'overlay-shown))
        (start-registration (setq result 'registration-started)))
      (should (eq result 'registration-started))))

  (nskk-it "binds the action symbol correctly"
    (let (captured-action)
      (nskk-henkan-dispatch my-action
          (nskk-prolog-query-value '(convert-or-commit-action converting \?a) '\?a)
        (commit-current (setq captured-action my-action)))
      (should (eq captured-action 'commit-current))))

  (nskk-it "matches a literal non-Prolog value"
    (let ((result nil))
      (nskk-henkan-dispatch my-action 'foo
        (foo (setq result 'matched-foo))
        (bar (setq result 'matched-bar)))
      (should (eq result 'matched-foo))))

  (nskk-it "returns nil when no clause matches"
    (let ((result 'unchanged))
      (nskk-henkan-dispatch my-action 'baz
        (foo (setq result 'matched-foo))
        (bar (setq result 'matched-bar)))
      (should (eq result 'unchanged)))))

;;;
;;; nskk-henkan-with-preedit Macro Tests
;;;

(nskk-describe "nskk-henkan-with-preedit"
  (nskk-it "does nothing when no marker is set"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker nil)
            (executed nil))
        (nskk-henkan-with-preedit _start
          (setq executed t))
        (should-not executed))))

  (nskk-it "does nothing when point is at the marker (no preedit text)"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker)))
        (set-marker nskk--conversion-start-marker (point-min))
        ;; Insert only the marker, point is AT the end of marker (no text after)
        (insert nskk-henkan-on-marker)
        (let ((executed nil))
          (nskk-henkan-with-preedit _start
            (setq executed t))
          (should-not executed)))))

  (nskk-it "executes body and binds start when preedit text exists"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker))
            (captured-start nil))
        (set-marker nskk--conversion-start-marker (point-min))
        (insert nskk-henkan-on-marker "かな")
        (nskk-henkan-with-preedit start
          (setq captured-start start))
        (should (equal captured-start (point-min)))))))

;;;
;;; nskk-with-conversion-context Macro Tests
;;;

(nskk-describe "nskk-with-conversion-context"
  (nskk-it "does nothing when not in converting state"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (executed nil))
      (nskk-with-conversion-context (_c _i)
        (setq executed t))
      (should-not executed)))

  (nskk-it "binds candidates and index when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            captured-candidates
            captured-index)
        (set-marker nskk--conversion-start-marker (point-min))
        (insert "test")
        (nskk-state-set-candidates nskk-current-state '("候補1" "候補2"))
        (setf (nskk-state-current-index nskk-current-state) 1)
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-conversion-context (candidates index)
          (setq captured-candidates candidates)
          (setq captured-index index))
        (should (equal captured-candidates '("候補1" "候補2")))
        (should (equal captured-index 1))))))

;;;
;;; Prolog Predicate Tests: converting-phase/1 and okurigana-char/2
;;;

(nskk-describe "converting-phase Prolog predicate"
  (nskk-deftest-table henkan-prolog-converting-phase-valid
    :description "converting-phase/1 succeeds for valid converting phases"
    :columns (phase)
    :rows ((active) (list) (registration))
    :body (should (nskk-prolog-query `(converting-phase ,phase))))

  (nskk-deftest-table henkan-prolog-converting-phase-invalid
    :description "converting-phase/1 fails for non-converting phases"
    :columns (phase)
    :rows ((nil) (on))
    :body (should-not (nskk-prolog-query `(converting-phase ,phase)))))

(nskk-describe "okurigana-char Prolog predicate"
  (nskk-it "maps uppercase A to lowercase a"
    (should (equal (nskk-prolog-query-value `(okurigana-char ,?A \?lc) '\?lc)
                   ?a)))

  (nskk-it "maps uppercase Z to lowercase z"
    (should (equal (nskk-prolog-query-value `(okurigana-char ,?Z \?lc) '\?lc)
                   ?z)))

  (nskk-it "does not map lowercase a"
    (should-not (nskk-prolog-query-value `(okurigana-char ,?a \?lc) '\?lc))))

;;;
;;; Prolog Predicate Tests: New Predicates
;;;

(nskk-describe "okurigana-trigger Prolog predicate"
  (nskk-it "succeeds for an uppercase letter"
    (should (nskk-prolog-query-one `(okurigana-trigger ,?K))))

  (nskk-it "fails for a lowercase letter"
    (should-not (nskk-prolog-query-one `(okurigana-trigger ,?k)))))

(nskk-describe "candidate-nav-next-action Prolog predicate"
  (nskk-it "returns select-next when count is below threshold"
    (should (equal (nskk-prolog-query-value '(candidate-nav-next-action 2 5 \?a) '\?a)
                   'select-next)))

  (nskk-it "returns show-list-next when count equals threshold"
    (should (equal (nskk-prolog-query-value '(candidate-nav-next-action 5 5 \?a) '\?a)
                   'show-list-next)))

  (nskk-it "returns show-list-next when count exceeds threshold"
    (should (equal (nskk-prolog-query-value '(candidate-nav-next-action 7 5 \?a) '\?a)
                   'show-list-next))))

(nskk-describe "candidate-nav-prev-action Prolog predicate"
  (nskk-it "returns show-list-prev when list is active"
    (should (equal (nskk-prolog-query-value '(candidate-nav-prev-action list-active \?a) '\?a)
                   'show-list-prev)))

  (nskk-it "returns select-prev when list is inactive"
    (should (equal (nskk-prolog-query-value '(candidate-nav-prev-action not-active \?a) '\?a)
                   'select-prev))))

(nskk-describe "search-result-action Prolog predicate"
  (nskk-it "returns show-overlay when candidates exist"
    (should (equal (nskk-prolog-query-value '(search-result-action has-candidates \?a) '\?a)
                   'show-overlay)))

  (nskk-it "returns start-registration when no candidates"
    (should (equal (nskk-prolog-query-value '(search-result-action no-candidates \?a) '\?a)
                   'start-registration))))

(nskk-describe "convert-or-commit-action Prolog predicate"
  (nskk-it "returns commit-current when converting"
    (should (equal (nskk-prolog-query-value '(convert-or-commit-action converting \?a) '\?a)
                   'commit-current)))

  (nskk-it "returns start-conversion when not converting"
    (should (equal (nskk-prolog-query-value '(convert-or-commit-action not-converting \?a) '\?a)
                   'start-conversion))))

(nskk-describe "nskk-max-registration-depth guard"
  (nskk-it "max-registration-depth is 3"
    (should (equal nskk-max-registration-depth 3)))

  (nskk-it "allows registration at depth 0"
    (should (< 0 nskk-max-registration-depth)))

  (nskk-it "allows registration at depth 2"
    (should (< 2 nskk-max-registration-depth)))

  (nskk-it "disallows registration at depth 3 (max depth)"
    (should-not (< 3 nskk-max-registration-depth)))

  (nskk-it "respects non-default value when customized"
    (let ((nskk-max-registration-depth 2))
      (should (< 0 nskk-max-registration-depth))
      (should (< 1 nskk-max-registration-depth))
      (should-not (< 2 nskk-max-registration-depth)))))

(nskk-describe "should-update-overlay Prolog predicate"
  (nskk-it "succeeds for active phase"
    (should (nskk-prolog-query '(should-update-overlay active))))

  (nskk-it "succeeds for list phase"
    (should (nskk-prolog-query '(should-update-overlay list))))

  (nskk-it "fails for on phase"
    (should-not (nskk-prolog-query '(should-update-overlay on)))))

;;;
;;; nskk-converting-p Tests
;;;

(nskk-describe "nskk-converting-p"
  (nskk-it "returns nil when state is nil"
    (let ((nskk-current-state nil))
      (should-not (nskk-converting-p))))

  (nskk-context "when henkan phase is set"
    (nskk-deftest-table henkan-converting-p-non-converting-phases
      :description "nskk-converting-p returns nil for non-converting phases"
      :columns (setup-fn)
      :rows ((nil))
      :body
      ;; phase nil: freshly created state
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (should-not (nskk-converting-p))))

    (nskk-it "returns nil when phase is nil (fresh state)"
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (should-not (nskk-converting-p))))

    (nskk-it "returns nil when phase is 'on (preedit, not converting)"
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (should-not (nskk-converting-p))))

    (nskk-it "returns non-nil when phase is 'active"
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (should (nskk-converting-p))))

    (nskk-it "returns non-nil when phase is 'list"
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-state-force-henkan-phase nskk-current-state 'list)
        (should (nskk-converting-p))))

    (nskk-it "returns non-nil when phase is 'registration"
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-state-force-henkan-phase nskk-current-state 'registration)
        (should (nskk-converting-p))))))

;;;
;;; nskk-detect-okurigana-char Tests
;;;

(nskk-describe "nskk-detect-okurigana-char"
  (nskk-it "detects A-Z as okurigana chars and returns their lowercase"
    (dolist (c (number-sequence ?A ?Z))
      (let ((result (nskk-detect-okurigana-char c)))
        (should result)
        (should (equal result (downcase c))))))

  (nskk-it "returns nil for lowercase letters"
    (dolist (c (number-sequence ?a ?z))
      (should-not (nskk-detect-okurigana-char c))))

  (nskk-it "returns nil for digits"
    (dolist (c (number-sequence ?0 ?9))
      (should-not (nskk-detect-okurigana-char c))))

  (nskk-it "returns nil for non-character inputs"
    (nskk-then
      (should-not (nskk-detect-okurigana-char nil))
      (should-not (nskk-detect-okurigana-char "K"))
      (should-not (nskk-detect-okurigana-char 'symbol)))))

;;;
;;; nskk-next-candidate Tests
;;;

(nskk-describe "henkan candidate navigation"
  (nskk-context "nskk-next-candidate"
    (nskk-it "does nothing when not converting"
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--henkan-count 0))
        (nskk-next-candidate)
        (should (equal nskk--henkan-count 0))))

    (nskk-it "calls nskk--select-candidate when count is below threshold"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--henkan-count 0)
              (nskk-henkan-show-candidates-nth 5)
              (nskk--conversion-start-marker (make-marker))
              select-called)
          (set-marker nskk--conversion-start-marker (point-min))
          (insert "test")
          (nskk-state-set-candidates nskk-current-state '("a" "b" "c"))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-with-mocks ((nskk--select-candidate (lambda (dir) (setq select-called dir))))
            (nskk-next-candidate)
            (should (eq select-called 'next))
            (should (equal nskk--henkan-count 1))))))

    (nskk-it "switches to list display at the threshold"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--henkan-count 4)  ;; one below threshold of 5
              (nskk-henkan-show-candidates-nth 5)
              (nskk--conversion-start-marker (make-marker))
              list-next-called)
          (set-marker nskk--conversion-start-marker (point-min))
          (insert "test")
          (nskk-state-set-candidates nskk-current-state '("a" "b" "c"))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-with-mocks ((nskk--show-candidate-list-next (lambda () (setq list-next-called t))))
            (nskk-next-candidate)
            (should list-next-called)
            (should (equal nskk--henkan-count 5)))))))

  (nskk-context "nskk-previous-candidate"
    (nskk-it "does nothing when not converting"
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--henkan-count 3))
        (nskk-previous-candidate)
        (should (equal nskk--henkan-count 3))))

    (nskk-it "calls nskk--select-candidate when list is inactive"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--henkan-count 3)
              (nskk-henkan--candidate-list-active nil)
              (nskk--conversion-start-marker (make-marker))
              select-called)
          (set-marker nskk--conversion-start-marker (point-min))
          (insert "test")
          (nskk-state-set-candidates nskk-current-state '("a" "b" "c"))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-with-mocks ((nskk--select-candidate (lambda (dir) (setq select-called dir))))
            (nskk-previous-candidate)
            (should (eq select-called 'previous))
            (should (equal nskk--henkan-count 2))))))

    (nskk-it "calls show-list-prev when list is active"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--henkan-count 5)
              (nskk-henkan--candidate-list-active t)
              (nskk--conversion-start-marker (make-marker))
              list-prev-called)
          (set-marker nskk--conversion-start-marker (point-min))
          (insert "test")
          (nskk-state-set-candidates nskk-current-state '("a" "b" "c"))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-with-mocks ((nskk--show-candidate-list-prev (lambda () (setq list-prev-called t))))
            (nskk-previous-candidate)
            (should list-prev-called)))))

    (nskk-it "does not decrement count below 0"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--henkan-count 0)
              (nskk-henkan--candidate-list-active nil)
              (nskk--conversion-start-marker (make-marker)))
          (set-marker nskk--conversion-start-marker (point-min))
          (insert "test")
          (nskk-state-set-candidates nskk-current-state '("a"))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-with-mocks ((nskk--select-candidate #'ignore))
            (nskk-previous-candidate)
            (should (equal nskk--henkan-count 0))))))))

;;;
;;; nskk-commit-current Tests
;;;

(nskk-describe "henkan commit"
  (nskk-it "does nothing when not converting"
    (with-temp-buffer
      (insert "unchanged")
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-commit-current)
        (should (equal (buffer-string) "unchanged")))))

  (nskk-it "inserts the candidate at current-index 0"
    (nskk-with-henkan-state 'active '("変換" "変換2")
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-commit-current)
      (should (equal (buffer-string) "変換"))
      (should-not (nskk-converting-p))
      (should (equal nskk--romaji-buffer ""))
      (should (equal nskk--henkan-count 0))))

  (nskk-it "inserts the candidate at current-index 2"
    (nskk-with-henkan-state 'active '("first" "second" "third")
      (setf (nskk-state-current-index nskk-current-state) 2)
      (nskk-commit-current)
      (should (equal (buffer-string) "third")))))

;;;
;;; nskk-convert Tests
;;;

(nskk-describe "henkan conversion (▼ phase)"
  (nskk-context "nskk-convert"
    (nskk-it "does nothing when no preedit exists"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker nil)
              start-conversion-called)
          (cl-letf (((symbol-function 'nskk-start-conversion)
                     (lambda () (setq start-conversion-called t))))
            (nskk-convert)
            (should-not start-conversion-called)))))

    (nskk-it "calls nskk-start-conversion when preedit text exists"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              start-conversion-called)
          (set-marker nskk--conversion-start-marker (point-min))
          (insert nskk-henkan-on-marker "かな")
          (cl-letf (((symbol-function 'nskk-start-conversion)
                     (lambda () (setq start-conversion-called t))))
            (nskk-convert)
            (should start-conversion-called))))))

  (nskk-context "nskk-convert-or-commit"
    (nskk-it "commits when in conversion state"
      (nskk-with-henkan-state 'active '("結果")
        (setf (nskk-state-current-index nskk-current-state) 0)
        (nskk-convert-or-commit)
        (should (equal (buffer-string) "結果"))
        (should-not (nskk-converting-p))))

    (nskk-it "starts conversion when preedit exists but not converting"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              start-conversion-called)
          (set-marker nskk--conversion-start-marker (point-min))
          (insert nskk-henkan-on-marker "かな")
          (cl-letf (((symbol-function 'nskk-start-conversion)
                     (lambda () (setq start-conversion-called t))))
            (nskk-convert-or-commit)
            (should start-conversion-called))))))

  (nskk-context "nskk-start-conversion direct behavior"
    (nskk-it "sets candidates and active phase when search returns results"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              (nskk--romaji-buffer "")
              (nskk--henkan-count 0)
              (nskk--conversion-overlay nil))
          (insert "▽かんじ")
          (set-marker nskk--conversion-start-marker (point-min))
          (goto-char (point-max))
          (nskk-state-force-henkan-phase nskk-current-state 'on)
          (nskk-with-mocks ((nskk-core-search (lambda (&rest _) '("漢字" "感じ")))
                            (nskk--update-overlay #'ignore)
                            (nskk--replace-marker-at #'ignore))
            (nskk-start-conversion)
            (should (equal (nskk-state-candidates nskk-current-state) '("漢字" "感じ")))
            (should (eq (nskk-state-henkan-phase nskk-current-state) 'active))))))

    (nskk-it "calls nskk-start-registration when no candidates found"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              (nskk--romaji-buffer "")
              (nskk--henkan-count 0)
              (nskk--conversion-overlay nil)
              (nskk--registration-depth 0)
              registration-called)
          (insert "▽てすと")
          (set-marker nskk--conversion-start-marker (point-min))
          (goto-char (point-max))
          (nskk-state-force-henkan-phase nskk-current-state 'on)
          (nskk-with-mocks ((nskk-core-search (lambda (&rest _) nil))
                            (nskk-start-registration (lambda (reading) (setq registration-called reading))))
            (nskk-start-conversion)
            (should registration-called)))))))

;;;
;;; henkan cancel
;;;

(nskk-describe "henkan cancel"
  (nskk-context "nskk-cancel-conversion"
    (nskk-it "does nothing when not converting"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              rollback-called)
          (cl-letf (((symbol-function 'nskk-rollback-conversion)
                     (lambda () (setq rollback-called t))))
            (nskk-cancel-conversion)
            (should-not rollback-called)))))

    (nskk-it "calls nskk-rollback-conversion when converting"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              rollback-called)
          (set-marker nskk--conversion-start-marker (point-min))
          (insert "test")
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (cl-letf (((symbol-function 'nskk-rollback-conversion)
                     (lambda () (setq rollback-called t))))
            (nskk-cancel-conversion)
            (should rollback-called))))))

  (nskk-context "nskk-cancel-preedit"
    (nskk-it "clears state variables and buffer"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              (nskk--romaji-buffer "ka")
              (nskk--henkan-count 3))
          (set-marker nskk--conversion-start-marker (point-min))
          (insert nskk-henkan-on-marker "か")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-cancel-preedit)
          (should (equal (buffer-string) ""))
          (should (equal nskk--romaji-buffer ""))
          (should (equal nskk--henkan-count 0))
          (should-not (nskk-state-henkan-phase nskk-current-state))))))

  (nskk-context "nskk-rollback-conversion"
    (nskk-it "resets count and restores preedit phase"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              (nskk--romaji-buffer "")
              (nskk--henkan-count 3)
              (nskk--conversion-overlay nil)
              (nskk-henkan--candidate-list-active nil))
          (insert "▼漢字")
          (set-marker nskk--conversion-start-marker (point-min))
          (goto-char (point-max))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-state-set-candidates nskk-current-state '("漢字" "感じ"))
          (cl-letf (((symbol-function 'nskk--restore-preedit) #'ignore)
                    ((symbol-function 'nskk--delete-marker-at) #'ignore)
                    ((symbol-function 'run-hook-with-args) #'ignore))
            (nskk-rollback-conversion)
            (should (= nskk--henkan-count 0))))))))

;;;
;;; nskk-start-registration Depth Guard Tests
;;;

(nskk-describe "henkan registration depth guard"
  (nskk-it "returns nil when depth is at maximum (3)"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--registration-depth 3))
      (should-not (nskk-start-registration "test"))))

  (nskk-it "proceeds and shows prompt when depth is below maximum"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--registration-depth 0)
          prompt-shown)
      ;; Set phase to `on' (preedit) so the nil->registration transition is valid
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (p) (setq prompt-shown p) ""))
                ((symbol-function 'nskk-dict-register-word)
                 #'ignore))
        (nskk-start-registration "てすと")
        (should prompt-shown)
        (should (string-match "辞書登録" prompt-shown))))))

;;;
;;; Seeded Property-Based Tests
;;;

;; Property: "converting-p state invariant"
;; For any henkan-phase, set it on a fresh state, then verify nskk-converting-p
;; matches whether the phase is in '(active list registration).
(nskk-property-test-seeded henkan-pbt-converting-p-invariant
  ((phase henkan-phase))
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (if phase
        (nskk-state-force-henkan-phase nskk-current-state phase)
      ;; nil phase: do not force any phase (state is freshly created)
      nil)
    (let ((converting (nskk-converting-p))
          (is-converting-phase (memq phase '(active list registration))))
      (nskk-assert-state-invariant nskk-current-state
        ;; converting-p is non-nil iff phase is active/list/registration
        (eq (not (null converting)) (not (null is-converting-phase))))
      t))
  100 2001)

;; Property: "henkan-dispatch never errors for valid prolog values"
;; Both search-result-action values (has-candidates, no-candidates) always
;; return a known symbol without signaling an error.
(nskk-property-test-seeded henkan-pbt-search-result-action-never-errors
  ((phase converting-phase))
  (condition-case err
      (let* ((has-cand-result
              (nskk-prolog-query-value '(search-result-action has-candidates \?a) '\?a))
             (no-cand-result
              (nskk-prolog-query-value '(search-result-action no-candidates \?a) '\?a)))
        (and (symbolp has-cand-result)
             (symbolp no-cand-result)
             (memq has-cand-result '(show-overlay start-registration))
             (memq no-cand-result  '(show-overlay start-registration))))
    (error nil))
  50 2003)

(provide 'nskk-henkan-test)

;;; nskk-henkan-test.el ends here
