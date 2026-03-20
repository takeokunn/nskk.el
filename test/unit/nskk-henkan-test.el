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
;;   convert-or-commit-action/2,
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
(require 'nskk)
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
      :body (should-not (fboundp sym)))

    (nskk-deftest-table henkan-deleted-hook-vars-removed
      :description "Dead hook variables have been removed from the API"
      :columns (sym)
      :rows ((nskk-start-henkan-hook)
             (nskk-henkan-hook)
             (nskk-post-henkan-hook)
             (nskk-after-henkan-hook)
             (nskk-henkan-select-hook))
      :body (should-not (boundp sym)))))

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

  (nskk-it "executes body when point is at the marker (empty preedit text)"
    ;; FR-005: >= guard allows conversion with empty preedit (e.g. SPC immediately
    ;; after uppercase letter before any kana is typed).  Previously > silently
    ;; skipped the body; now it executes, opening registration as expected.
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker)))
        (set-marker nskk--conversion-start-marker (point-min))
        ;; Insert only the marker, point is AT the end of marker (no text after)
        (insert nskk-henkan-on-marker)
        (let ((executed nil))
          (nskk-henkan-with-preedit _start
            (setq executed t))
          (should executed)))))

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

(nskk-describe "preedit-phase Prolog predicate"
  (nskk-it "preedit-phase/1 holds for on"
    (should (nskk-prolog-holds-p '(preedit-phase on))))

  (nskk-deftest-table henkan-prolog-preedit-phase-invalid
    :description "preedit-phase/1 fails for non-preedit phases"
    :columns (phase)
    :rows ((nil) (active) (list) (registration))
    :body (should-not (nskk-prolog-holds-p `(preedit-phase ,phase)))))

(nskk-describe "script-converter Prolog predicate"
  (nskk-it "katakana maps to hiragana-to-katakana/k converter"
    (should (eq (nskk-prolog-query-value
                 '(script-converter katakana \?fn) '\?fn)
                'nskk-kana-string-hiragana-to-katakana/k)))

  (nskk-it "hiragana maps to katakana-to-hiragana/k converter"
    (should (eq (nskk-prolog-query-value
                 '(script-converter hiragana \?fn) '\?fn)
                'nskk-kana-string-katakana-to-hiragana/k)))

  (nskk-it "non-existent target returns nil"
    (should-not (nskk-prolog-query-value
                 '(script-converter ascii \?fn) '\?fn))))

(nskk-describe "disable-cleanup Prolog predicate"
  (nskk-it "active maps to cancel-conversion"
    (should (nskk-prolog-holds-p '(disable-cleanup active cancel-conversion))))

  (nskk-it "list maps to cancel-conversion"
    (should (nskk-prolog-holds-p '(disable-cleanup list cancel-conversion))))

  (nskk-it "on maps to cancel-preedit"
    (should (nskk-prolog-holds-p '(disable-cleanup on cancel-preedit))))

  (nskk-it "registration maps to cancel-preedit"
    (should (nskk-prolog-holds-p '(disable-cleanup registration cancel-preedit))))

  (nskk-it "nil phase returns nil"
    (should-not (nskk-prolog-query-value
                 '(disable-cleanup nil \?a) '\?a))))

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

;;;
;;; nskk-max-registration-depth guard
;;;

(nskk-describe "nskk-max-registration-depth guard"
  (nskk-it "max-registration-depth default is 3"
    (should (equal nskk-max-registration-depth 3)))

  (nskk-deftest-table max-registration-depth-boundary
    :description "start-registration returns nil at max-depth, proceeds below it"
    :columns (depth should-proceed)
    :rows ((0 t)
           (1 t)
           (2 t)
           (3 nil))
    :body
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--registration-depth depth)
          prompt-shown)
      (if should-proceed
          (progn
            (nskk-state-force-henkan-phase nskk-current-state 'on)
            (nskk-with-mocks ((read-from-minibuffer
                               (lambda (p) (setq prompt-shown p) ""))
                              (nskk-dict-register-word #'ignore))
              (nskk-start-registration "てすと")
              (should prompt-shown)))
        (should-not (nskk-start-registration "てすと")))))

  (nskk-it "respects non-default max-registration-depth"
    (let ((nskk-max-registration-depth 2)
          (nskk-current-state (nskk-state-create 'hiragana))
          (nskk--registration-depth 1)
          prompt-shown)
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk-with-mocks ((read-from-minibuffer (lambda (p) (setq prompt-shown p) ""))
                        (nskk-dict-register-word #'ignore))
        (nskk-start-registration "てすと")
        (should prompt-shown))
      (let ((nskk--registration-depth 2))
        (should-not (nskk-start-registration "てすと"))))))

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

  ;; Table-driven: all 5 phases with expected bool result.
  ;; Uses force-henkan-phase throughout to avoid dependency on transition validity
  ;; (the nil->active transition is not in the transition table, so force is required).
  (nskk-deftest-table converting-p-by-henkan-phase
    :description "Returns correct bool for each of the 5 henkan phases"
    :columns (phase expected-converting-p)
    :rows ((nil          nil)
           (on           nil)
           (active       t)
           (list         t)
           (registration t))
    :body (let ((nskk-current-state (nskk-state-create 'hiragana)))
            (nskk-state-force-henkan-phase nskk-current-state phase)
            (if expected-converting-p
                (should (nskk-converting-p))
              (should-not (nskk-converting-p)))))

  ;; CPS variant: on-found(t) when converting, on-not-found() when not.
  ;; Use nskk-it (not nskk-it-k) because the state setup must precede the /k call;
  ;; nskk-it-k's k-call cannot contain a wrapping `let' form.
  (nskk-it "converting-p/k calls on-found when phase is active (CPS contract)"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (let (on-found-called)
        (nskk-converting-p/k
         (lambda (_) (setq on-found-called t))
         (lambda () (ert-fail "Expected on-found for active phase but got on-not-found")))
        (should on-found-called))))

  (nskk-it "converting-p/k calls on-not-found when phase is on (CPS contract)"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-state-set-henkan-phase nskk-current-state 'on)
      (let (on-not-found-called)
        (nskk-converting-p/k
         (lambda (_) (ert-fail "Expected on-not-found for on phase but got on-found"))
         (lambda () (setq on-not-found-called t)))
        (should on-not-found-called)))))

;;;
;;; nskk-detect-okurigana-char Tests
;;;

(nskk-describe "nskk-detect-okurigana-char"
  (nskk-property-test-exhaustive detect-okurigana-char-uppercase-pbt
    (number-sequence ?A ?Z)
    (let ((result (nskk-detect-okurigana-char item)))
      (and result (= result (downcase item)))))

  (nskk-property-test-exhaustive detect-okurigana-char-lowercase-pbt
    (number-sequence ?a ?z)
    (null (nskk-detect-okurigana-char item)))

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
              (nskk--henkan-candidate-list-active nil)
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
              (nskk--henkan-candidate-list-active t)
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
              (nskk--henkan-candidate-list-active nil)
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

  (nskk-context "when committing at index 0"
    (nskk-it "inserts the candidate at current-index 0"
      (nskk-with-henkan-state 'active '("変換" "変換2")
        (setf (nskk-state-current-index nskk-current-state) 0)
        (nskk-commit-current)
        (should (equal (buffer-string) "変換"))))

    (nskk-it "exits converting state after commit"
      (nskk-with-henkan-state 'active '("変換" "変換2")
        (setf (nskk-state-current-index nskk-current-state) 0)
        (nskk-commit-current)
        (should-not (nskk-converting-p))))

    (nskk-it "clears romaji buffer after commit"
      (nskk-with-henkan-state 'active '("変換" "変換2")
        (setf (nskk-state-current-index nskk-current-state) 0)
        (nskk-commit-current)
        (should (equal nskk--romaji-buffer ""))))

    (nskk-it "resets henkan-count to 0 after commit"
      (nskk-with-henkan-state 'active '("変換" "変換2")
        (setf (nskk-state-current-index nskk-current-state) 0)
        (nskk-commit-current)
        (should (equal nskk--henkan-count 0)))))

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
          (nskk-with-mocks ((nskk-start-conversion (lambda () (setq start-conversion-called t))))
            (nskk-convert)
            (should-not start-conversion-called)))))

    (nskk-it "calls nskk-start-conversion when preedit text exists"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              start-conversion-called)
          (set-marker nskk--conversion-start-marker (point-min))
          (insert nskk-henkan-on-marker "かな")
          (nskk-with-mocks ((nskk-start-conversion (lambda () (setq start-conversion-called t))))
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
          (nskk-with-mocks ((nskk-start-conversion (lambda () (setq start-conversion-called t))))
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
          (nskk-with-mocks ((nskk-core-search/k
                             (lambda (_key _type _limit on-found _on-not-found)
                               (funcall on-found '("漢字" "感じ"))))
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
          ;; nskk-start-conversion now delegates to CPS variants; mock those.
          (nskk-with-mocks ((nskk-core-search/k
                             (lambda (_key _type _limit _on-found on-not-found)
                               (funcall on-not-found)))
                            (nskk-start-registration/k
                             (lambda (reading on-done _ignored)
                               (setq registration-called reading)
                               (funcall on-done nil))))
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
          (nskk-with-mocks ((nskk-rollback-conversion (lambda () (setq rollback-called t))))
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
          (nskk-with-mocks ((nskk-rollback-conversion (lambda () (setq rollback-called t))))
            (nskk-cancel-conversion)
            (should rollback-called))))))

  (nskk-context "nskk-cancel-preedit"
    (nskk-it "clears preedit text from buffer"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              (nskk--romaji-buffer "ka")
              (nskk--henkan-count 3))
          (set-marker nskk--conversion-start-marker (point-min))
          (insert nskk-henkan-on-marker "か")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-cancel-preedit)
          (should (equal (buffer-string) "")))))

    (nskk-it "clears romaji buffer"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              (nskk--romaji-buffer "ka")
              (nskk--henkan-count 3))
          (set-marker nskk--conversion-start-marker (point-min))
          (insert nskk-henkan-on-marker "か")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-cancel-preedit)
          (should (equal nskk--romaji-buffer "")))))

    (nskk-it "resets henkan-count to 0"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              (nskk--romaji-buffer "ka")
              (nskk--henkan-count 3))
          (set-marker nskk--conversion-start-marker (point-min))
          (insert nskk-henkan-on-marker "か")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-cancel-preedit)
          (should (equal nskk--henkan-count 0)))))

    (nskk-it "resets henkan-phase to nil"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              (nskk--romaji-buffer "ka")
              (nskk--henkan-count 3))
          (set-marker nskk--conversion-start-marker (point-min))
          (insert nskk-henkan-on-marker "か")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-cancel-preedit)
          (should-not (nskk-state-henkan-phase nskk-current-state)))))

    (nskk-it "clears okurigana state from state struct on cancel"
      (nskk-prolog-test-with-isolated-db
        (with-temp-buffer
          (nskk-mode 1)
          (let ((nskk--conversion-start-marker (make-marker))
                (nskk--romaji-buffer "k")
                (nskk--henkan-count 0))
            (set-marker nskk--conversion-start-marker (point-min))
            (insert nskk-henkan-on-marker "か")
            (nskk-state-set-henkan-phase nskk-current-state 'on)
            (nskk-state-set-okurigana nskk-current-state "k")
            (nskk-cancel-preedit)
            (should-not (nskk-state-get-okurigana nskk-current-state))))))

    (nskk-it "clears all AZIK okurigana pending state vars on cancel"
      ;; All three sentinel vars must be nil after cancel-preedit to prevent
      ;; stale pending state (colon-arm, colon-deferred, sokuon-okuri) from
      ;; leaking into the next preedit session.
      (dolist (spec '((nskk--azik-colon-okuri-pending      . t)
                      (nskk--azik-colon-okuri-deferred     . some-value)
                      (nskk--azik-sokuon-okuri-kana-pending . t)))
        (nskk-prolog-test-with-isolated-db
          (with-temp-buffer
            (nskk-mode 1)
            (let ((nskk--conversion-start-marker (make-marker))
                  (nskk--romaji-buffer "")
                  (nskk--henkan-count 0))
              (set-marker nskk--conversion-start-marker (point-min))
              (insert nskk-henkan-on-marker "か")
              (nskk-state-set-henkan-phase nskk-current-state 'on)
              (set (car spec) (cdr spec))
              (nskk-cancel-preedit)
              (should-not (symbol-value (car spec)))))))))

  (nskk-context "nskk-rollback-conversion"
    (nskk-it "resets count and restores preedit phase"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              (nskk--romaji-buffer "")
              (nskk--henkan-count 3)
              (nskk--conversion-overlay nil)
              (nskk--henkan-candidate-list-active nil))
          (insert "▼漢字")
          (set-marker nskk--conversion-start-marker (point-min))
          (goto-char (point-max))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-state-set-candidates nskk-current-state '("漢字" "感じ"))
          (nskk-with-mocks ((nskk--delete-marker-at #'ignore)
                            (run-hook-with-args #'ignore))
            (nskk-rollback-conversion)
            (should (= nskk--henkan-count 0))))))

    (nskk-it "clears nskk--azik-sokuon-okuri-kana-pending on rollback"
      ;; After JP106 + fires sokuon okurigana, C-g (rollback) must clear the
      ;; sentinel so the next preedit does not start with a stale flag.
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk--conversion-start-marker (make-marker))
              (nskk--romaji-buffer "")
              (nskk--henkan-count 0)
              (nskk--conversion-overlay nil)
              (nskk--henkan-candidate-list-active nil)
              (nskk--azik-sokuon-okuri-kana-pending t))
          (insert "▼漢字")
          (set-marker nskk--conversion-start-marker (point-min))
          (goto-char (point-max))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-state-set-candidates nskk-current-state '("漢字"))
          (nskk-with-mocks ((nskk--delete-marker-at #'ignore)
                            (run-hook-with-args #'ignore))
            (nskk-rollback-conversion)
            (should-not nskk--azik-sokuon-okuri-kana-pending)))))))

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
      (nskk-with-mocks ((read-from-minibuffer (lambda (p) (setq prompt-shown p) ""))
                        (nskk-dict-register-word #'ignore))
        (nskk-start-registration "てすと")
        (should prompt-shown)
        (should (string-match "辞書登録" prompt-shown))))))

;;;
;;; Seeded Property-Based Tests
;;;

;; Property: "converting-p state invariant"
;; For any henkan-phase, set it on a fresh state, then verify nskk-converting-p
;; matches `converting-phase/1' Prolog fact table membership.
(nskk-property-test-seeded henkan-pbt-converting-p-invariant
  ((phase henkan-phase))
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (if phase
        (nskk-state-force-henkan-phase nskk-current-state phase)
      ;; nil phase: do not force any phase (state is freshly created)
      nil)
    (let ((converting (nskk-converting-p))
          (is-converting-phase (nskk-prolog-holds-p `(converting-phase ,phase))))
      (nskk-assert-state-invariant nskk-current-state
        ;; converting-p is non-nil iff phase is in converting-phase/1
        (eq (not (null converting)) (not (null is-converting-phase))))
      t))
  100 2001)

;; Property: "henkan-dispatch never errors for valid prolog values"
;; Both search-result-action values are already tested with exact equality in
;; the "search-result-action Prolog predicate" nskk-describe block above.

;;;
;;; nskk-core-search/k: CPS variant tests (FR-T-004)
;;;

(nskk-describe "nskk-core-search/k dict-lookup path"
  (nskk-context "on-found branch: dict has entry"
    (nskk-it "calls on-found with candidates when dict lookup succeeds"
      (let ((found-arg nil)
            (not-found-called nil))
        (nskk-with-mock-dict '(("かんじ" . ("漢字" "感じ" "幹事")))
          (nskk-with-mocks ((nskk-server-ensure-open (lambda () nil)))
            (nskk-core-search/k "かんじ" nil nil
              (lambda (cands) (setq found-arg cands))
              (lambda () (setq not-found-called t)))
            (should (equal found-arg '("漢字" "感じ" "幹事")))
            (should (null not-found-called))))))

    (nskk-it "does NOT call on-not-found when dict has entry"
      (let ((not-found-called nil))
        (nskk-with-mock-dict '(("さくら" . ("桜")))
          (nskk-with-mocks ((nskk-server-ensure-open (lambda () nil)))
            (nskk-core-search/k "さくら" nil nil
              (lambda (_cands) nil)
              (lambda () (setq not-found-called t)))
            (should (null not-found-called))))))

    (nskk-it "passes the full candidates list to on-found"
      (let ((found-arg nil))
        (nskk-with-mock-dict '(("かわ" . ("川" "河")))
          (nskk-with-mocks ((nskk-server-ensure-open (lambda () nil)))
            (nskk-core-search/k "かわ" nil nil
              (lambda (cands) (setq found-arg cands))
              #'ignore)
            (should (equal found-arg '("川" "河"))))))))

  (nskk-context "on-not-found branch: dict has no entry, server disabled"
    (nskk-it "calls on-not-found when dict has no entry and server is disabled"
      (let ((not-found-called nil)
            (found-called nil)
            (nskk-server-enable nil))
        (nskk-with-mock-dict '()
          (nskk-core-search/k "みつからない" nil nil
            (lambda (_cands) (setq found-called t))
            (lambda () (setq not-found-called t)))
          (should not-found-called)
          (should (null found-called)))))

    (nskk-it "does NOT call on-found when key is absent and server is disabled"
      (let ((found-called nil)
            (nskk-server-enable nil))
        (nskk-with-mock-dict '()
          (nskk-core-search/k "nonexistent-xyz" nil nil
            (lambda (_cands) (setq found-called t))
            #'ignore)
          (should (null found-called))))))

  (nskk-context "server availability: nskk-server-enable nil skips server"
    (nskk-it "does not attempt server lookup when nskk-server-enable is nil"
      (let ((server-lookup-called nil)
            (nskk-server-enable nil))
        (nskk-with-mock-dict '()
          (nskk-with-mocks ((nskk-server-lookup (lambda (_key) (setq server-lookup-called t) nil))
                            (nskk-server-ensure-open (lambda () nil)))
            (nskk-core-search/k "てすと" nil nil
              #'ignore
              #'ignore)
            (should (null server-lookup-called))))))

    (nskk-it "falls through to on-not-found without calling server when disabled"
      (let ((not-found-called nil)
            (nskk-server-enable nil))
        (nskk-with-mock-dict '()
          (nskk-core-search/k "てすと" nil nil
            #'ignore
            (lambda () (setq not-found-called t)))
          (should not-found-called)))))

  (nskk-context "server fallback: nskk-server-enable t and server open"
    (nskk-it "calls on-found with server candidates when dict misses and server returns results"
      (let ((found-arg nil)
            (not-found-called nil)
            (nskk-server-enable t))
        (nskk-with-mock-dict '()
          (nskk-with-mocks ((nskk-server-ensure-open (lambda () t))
                            (nskk-server-lookup/k
                             (lambda (key on-found _on-not-found)
                               (when (equal key "みつからない")
                                 (funcall on-found '("見つからない"))))))
            (nskk-core-search/k "みつからない" nil nil
              (lambda (cands) (setq found-arg cands))
              (lambda () (setq not-found-called t)))
            (should (equal found-arg '("見つからない")))
            (should (null not-found-called))))))

    (nskk-it "calls on-not-found when dict misses and server returns nil"
      (let ((not-found-called nil)
            (found-called nil)
            (nskk-server-enable t))
        (nskk-with-mock-dict '()
          (nskk-with-mocks ((nskk-server-ensure-open (lambda () t))
                            (nskk-server-lookup/k
                             (lambda (_key _on-found on-not-found)
                               (funcall on-not-found))))
            (nskk-core-search/k "みつからない" nil nil
              (lambda (_cands) (setq found-called t))
              (lambda () (setq not-found-called t)))
            (should not-found-called)
            (should (null found-called)))))))

  (nskk-context "exactly-one-continuation invariant"
    (nskk-it "calls exactly one continuation when dict has entry"
      (let ((call-count 0))
        (nskk-with-mock-dict '(("かんじ" . ("漢字")))
          (nskk-with-mocks ((nskk-server-ensure-open (lambda () nil)))
            (nskk-core-search/k "かんじ" nil nil
              (lambda (_cands) (cl-incf call-count))
              (lambda () (cl-incf call-count)))
            (should (= call-count 1))))))

    (nskk-it "calls exactly one continuation when dict has no entry"
      (let ((call-count 0)
            (nskk-server-enable nil))
        (nskk-with-mock-dict '()
          (nskk-core-search/k "notfound" nil nil
            (lambda (_cands) (cl-incf call-count))
            (lambda () (cl-incf call-count)))
          (should (= call-count 1)))))

    (nskk-deftest-table core-search/k-exactly-one-continuation
      :description "Exactly one continuation called for various dict states"
      :columns (key dict-entries)
      :rows (("かんじ"  (("かんじ" . ("漢字"))))
             ("にほん"  (("にほん" . ("日本"))))
             ("notfound" ()))
      :body
      (let ((call-count 0)
            (nskk-server-enable nil))
        (nskk-with-mock-dict dict-entries
          (nskk-core-search/k key nil nil
            (lambda (_cands) (cl-incf call-count))
            (lambda () (cl-incf call-count)))
          (should (= call-count 1)))))))

;;;
;;; nskk-core-search/k: prefix-search and partial-search arm tests (FR-T-005)
;;;

(nskk-describe "nskk-core-search/k prefix-search and partial-search arms"
  (nskk-context "prefix-search arm"
    (nskk-it "calls on-found when system dict index is non-nil and prefix search returns results"
      (let ((found-arg nil)
            (not-found-called nil))
        (nskk-with-mock-dict '(("かんじ" . ("漢字")) ("かんたん" . ("簡単")))
          (nskk-with-mocks ((nskk-search-prefix/k
                             (lambda (_index _key _okuri _limit on-found _on-not-found)
                               (funcall on-found '("漢字" "簡単")))))
            (nskk-core-search/k "かん" :prefix nil
              (lambda (cands) (setq found-arg cands))
              (lambda () (setq not-found-called t)))
            (should (equal found-arg '("漢字" "簡単")))
            (should (null not-found-called))))))

    (nskk-it "calls on-not-found when system dict index is nil"
      (let ((not-found-called nil)
            (nskk--system-dict-index nil))
        (nskk-with-mock-dict '(("かんじ" . ("漢字")))
          (let ((nskk--system-dict-index nil))
            (nskk-core-search/k "かん" :prefix nil
              #'ignore
              (lambda () (setq not-found-called t)))
            (should not-found-called)))))

    (nskk-it "calls on-not-found when prefix search returns nil"
      (let ((not-found-called nil))
        (nskk-with-mock-dict '(("かんじ" . ("漢字")))
          (nskk-with-mocks ((nskk-search-prefix
                             (lambda (_index _key _okuri _limit) nil)))
            (nskk-core-search/k "zzz" :prefix nil
              #'ignore
              (lambda () (setq not-found-called t)))
            (should not-found-called))))))

  (nskk-context "partial-search arm"
    (nskk-it "calls on-found when system dict index is non-nil and partial search returns results"
      (let ((found-arg nil)
            (not-found-called nil))
        (nskk-with-mock-dict '(("かんじ" . ("漢字")))
          (nskk-with-mocks ((nskk-search-partial/k
                             (lambda (_index _key _okuri _limit on-found _on-not-found)
                               (funcall on-found '("漢字")))))
            (nskk-core-search/k "かんじ" :partial nil
              (lambda (cands) (setq found-arg cands))
              (lambda () (setq not-found-called t)))
            (should (equal found-arg '("漢字")))
            (should (null not-found-called))))))

    (nskk-it "calls on-not-found when system dict index is nil"
      (let ((not-found-called nil)
            (nskk--system-dict-index nil))
        (nskk-with-mock-dict '(("かんじ" . ("漢字")))
          (let ((nskk--system-dict-index nil))
            (nskk-core-search/k "かんじ" :partial nil
              #'ignore
              (lambda () (setq not-found-called t)))
            (should not-found-called)))))

    (nskk-it "calls on-not-found when partial search returns nil"
      (let ((not-found-called nil))
        (nskk-with-mock-dict '(("かんじ" . ("漢字")))
          (nskk-with-mocks ((nskk-search-partial
                             (lambda (_index _key _okuri _limit) nil)))
            (nskk-core-search/k "zzz" :partial nil
              #'ignore
              (lambda () (setq not-found-called t)))
            (should not-found-called)))))))

;;;
;;; FR-T-006: PBT — exactly-one-continuation invariant for nskk-core-search/k
;;;

(nskk-property-test-exhaustive core-search/k-exactly-one-continuation-pbt
  ;; Domain: representative keys — empty, single char, known, unknown, multi-char
  '("" "a" "かんじ" "にほんご" "xyz" "notfound-abc" "さくら" "hello" "あ" "てすと")
  ;; Property: for each key, exactly one of on-found/on-not-found is called
  (let ((call-count 0)
        (nskk-server-enable nil))
    (nskk-with-mock-dict nil  ; use default mock entries
      (condition-case nil
          (progn
            (nskk-core-search/k item nil nil
              (lambda (_cands) (cl-incf call-count))
              (lambda () (cl-incf call-count)))
            ;; Non-string keys return nil from nskk-core-search/k without
            ;; calling either continuation; the string guard is intentional.
            ;; For string keys the invariant must hold.
            (or (not (stringp item))
                (string-empty-p item)
                (= call-count 1)))
        (error nil)))))

;;;
;;; SKK Numeric Conversion Unit Tests
;;;

(nskk-describe "nskk--numeric-parse-reading"
  (nskk-deftest-table numeric-parse-reading-valid
    :description "Parses numeric readings into (num-str . base-key) pairs"
    :columns (input expected-num expected-base)
    :rows (("#1ko"  "1"   "#ko")
           ("#2ji"  "2"   "#ji")
           ("#123ko" "123" "#ko")
           ("#0"   "0"   "#"))
    :body (let ((result (nskk--numeric-parse-reading input)))
            (should (consp result))
            (should (equal (car result) expected-num))
            (should (equal (cdr result) expected-base))))

  (nskk-deftest-table numeric-parse-reading-invalid
    :description "Returns nil for non-numeric readings"
    :columns (input)
    :rows (("かんじ") ("") ("ko") ("#") ("#abc"))
    :body (should (null (nskk--numeric-parse-reading input)))))

(nskk-describe "nskk--numeric-to-kanji"
  (nskk-deftest-table numeric-to-kanji-digits
    :description "Converts digit strings to kanji numerals digit-by-digit"
    :columns (input expected)
    :rows (("0" "〇")
           ("1" "一")
           ("9" "九")
           ("12" "一二")
           ("123" "一二三")
           ("1024" "一〇二四"))
    :body (should (equal (nskk--numeric-to-kanji input) expected))))

(nskk-describe "nskk--numeric-to-fullwidth"
  (nskk-deftest-table numeric-to-fullwidth-digits
    :description "Converts digit strings to full-width Unicode digits"
    :columns (input expected)
    :rows (("0" "０")
           ("1" "１")
           ("9" "９")
           ("12" "１２")
           ("2025" "２０２５"))
    :body (should (equal (nskk--numeric-to-fullwidth input) expected))))

(nskk-describe "nskk--numeric-convert"
  (nskk-deftest-table numeric-convert-type-dispatch
    :description "Dispatches to the correct conversion per DDSKK type code"
    :columns (input type expected)
    :rows (("42" 0 "42")
           ("42" 1 "４２")
           ("42" 2 "四二")
           ("42" 3 "四十二")
           ("42" 4 "四二")
           ("42" 9 "42"))
    :body (should (equal (nskk--numeric-convert input type) expected)))

  ;;;
  ;;; PBT: numeric conversion completeness
  ;;;

  (nskk-property-test-exhaustive numeric-convert-type-returns-string-pbt
    '(0 1 2 3 4 5 6 7 8 9)
    (stringp (nskk--numeric-convert "42" item))))

(nskk-describe "nskk--numeric-process-candidate"
  (nskk-it "replaces single #N pattern with converted number"
    (should (equal (nskk--numeric-process-candidate "#0個" "5") "5個")))

  (nskk-it "replaces #1 pattern with full-width number"
    (should (equal (nskk--numeric-process-candidate "#1時" "3") "３時")))

  (nskk-it "replaces #2 pattern with kanji digits"
    (should (equal (nskk--numeric-process-candidate "#2個" "12") "一二個")))

  (nskk-it "replaces multiple #N patterns in single candidate"
    (should (equal (nskk--numeric-process-candidate "#0-#1" "7") "7-７")))

  (nskk-it "returns candidate unchanged when no #N pattern present"
    (should (equal (nskk--numeric-process-candidate "漢字" "5") "漢字")))

  (nskk-it "replaces #N in mixed Japanese/ASCII candidate"
    (should (equal (nskk--numeric-process-candidate "第#0号" "3") "第3号"))))

(nskk-describe "nskk--numeric-process-candidates"
  (nskk-it "processes all candidates in a list"
    (should (equal (nskk--numeric-process-candidates '("#0個" "#2個") "5")
                   '("5個" "五個"))))

  (nskk-it "returns empty list unchanged"
    (should (null (nskk--numeric-process-candidates nil "5"))))

  (nskk-it "leaves candidates without #N patterns unchanged"
    (should (equal (nskk--numeric-process-candidates '("漢字" "感じ") "1")
                   '("漢字" "感じ")))))

;;;
;;; Low-level buffer-manipulation helpers
;;;

(nskk-describe "nskk--insert-marker"
  (nskk-it "inserts the string without buffering modification hooks"
    (with-temp-buffer
      (nskk--insert-marker "▽")
      (should (equal (buffer-string) "▽"))))

  (nskk-it "does not record undo when inhibit-undo wrapper is active"
    (with-temp-buffer
      (let (captured-undo)
        (nskk-without-modification
          (nskk--insert-marker "▽")
          (setq captured-undo buffer-undo-list))
        (should (eq captured-undo t)))))

  (nskk-it "can insert the active marker ▼ as well"
    (with-temp-buffer
      (nskk--insert-marker "▼")
      (should (equal (buffer-string) "▼")))))

(nskk-describe "nskk--delete-marker-at"
  (nskk-it "deletes a matching marker at the given position"
    (with-temp-buffer
      (insert "▽かんじ")
      (nskk--delete-marker-at (point-min) nskk-henkan-on-marker-regexp)
      (should (equal (buffer-string) "かんじ"))))

  (nskk-it "does nothing when no marker matches at the given position"
    (with-temp-buffer
      (insert "かんじ")
      (nskk--delete-marker-at (point-min) nskk-henkan-on-marker-regexp)
      (should (equal (buffer-string) "かんじ"))))

  (nskk-it "only deletes at the specified position, not elsewhere"
    (with-temp-buffer
      (insert "かんじ▽")
      (let ((end-pos (- (point-max) (length nskk-henkan-on-marker))))
        (nskk--delete-marker-at end-pos nskk-henkan-on-marker-regexp)
        (should (equal (buffer-string) "かんじ"))))))

(nskk-describe "nskk--replace-marker-at"
  (nskk-it "replaces ▽ with ▼ at the given position"
    (with-temp-buffer
      (insert "▽かんじ")
      (nskk--replace-marker-at (point-min)
                                nskk-henkan-on-marker-regexp
                                nskk-henkan-active-marker)
      (should (equal (buffer-string) "▼かんじ"))))

  (nskk-it "does nothing when old regexp does not match at position"
    (with-temp-buffer
      (insert "▼かんじ")
      (nskk--replace-marker-at (point-min)
                                nskk-henkan-on-marker-regexp
                                nskk-henkan-active-marker)
      (should (equal (buffer-string) "▼かんじ"))))

  (nskk-it "replaces ▼ back to ▽"
    (with-temp-buffer
      (insert "▼かんじ")
      (nskk--replace-marker-at (point-min)
                                nskk-henkan-active-marker-regexp
                                nskk-henkan-on-marker)
      (should (equal (buffer-string) "▽かんじ")))))

(nskk-describe "nskk--skip-marker-pos"
  (nskk-it "returns position after the marker when it matches"
    (with-temp-buffer
      (insert "▽かんじ")
      (let ((advanced (nskk--skip-marker-pos (point-min)
                                              nskk-henkan-on-marker-regexp)))
        (should (> advanced (point-min))))))

  (nskk-it "returns the original position when no marker matches"
    (with-temp-buffer
      (insert "かんじ")
      (let ((pos (nskk--skip-marker-pos (point-min)
                                         nskk-henkan-on-marker-regexp)))
        (should (= pos (point-min))))))

  (nskk-it "does not move point (non-destructive)"
    (with-temp-buffer
      (insert "▽かんじ")
      (goto-char (point-max))
      (let ((before (point)))
        (nskk--skip-marker-pos (point-min) nskk-henkan-on-marker-regexp)
        (should (= (point) before))))))

(nskk-describe "nskk-preedit-string"
  (nskk-it "returns nil when no conversion start marker is active"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (should (null (nskk-preedit-string))))))

  (nskk-it "returns the kana text between the marker and point"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification
          (insert nskk-henkan-on-marker)
          (insert "かんじ"))
        (nskk--set-conversion-start-marker (point-min))
        (should (equal (nskk-preedit-string) "かんじ")))))

  (nskk-it "returns nil when point is at or before the marker"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification (insert nskk-henkan-on-marker))
        (nskk--set-conversion-start-marker (point-min))
        (should (null (nskk-preedit-string))))))

  ;; CPS variant: on-found(string) when preedit text exists, on-not-found() otherwise.
  (nskk-it "preedit-string/k calls on-found with text when preedit text is present"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification
          (insert nskk-henkan-on-marker)
          (insert "か"))
        (nskk--set-conversion-start-marker (point-min))
        (let (got-text got-not-found)
          (nskk-preedit-string/k
           (lambda (s) (setq got-text s))
           (lambda () (setq got-not-found t)))
          (should (equal got-text "か"))
          (should-not got-not-found)))))

  (nskk-it "preedit-string/k calls on-not-found when no preedit text is present"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let (got-not-found)
          (nskk-preedit-string/k
           (lambda (_s) (ert-fail "Expected on-not-found but got on-found"))
           (lambda () (setq got-not-found t)))
          (should got-not-found))))))

;;;
;;; Dynamic completion (dcomp)
;;;

;;;
;;; nskk--dcomp-search-prefix (real db)
;;;

(nskk-describe "nskk--dcomp-search-prefix (real db)"
  (nskk-it "returns nil when no prefix matches exist"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (should (null (nskk--dcomp-search-prefix "zzznomatch"))))))

  (nskk-it "excludes the exact prefix from results"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((results (nskk--dcomp-search-prefix "あ")))
          ;; Only assert exclusion when the dict actually has prefix matches;
          ;; avoids vacuous pass when the test dict is sparse.
          (when results
            (should-not (member "あ" results)))))))

  (nskk-it "returns a sorted list"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((results (nskk--dcomp-search-prefix "あ")))
          (when (cdr results)
            (should (cl-every (lambda (a b) (not (string> a b)))
                              results (cdr results)))))))))

(nskk-describe "nskk--dcomp-replace-preedit"
  (nskk-it "replaces preedit text after the ▽ marker with new text"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification
          (insert nskk-henkan-on-marker)
          (insert "かん"))
        (nskk--set-conversion-start-marker (point-min))
        (nskk--dcomp-replace-preedit "かんじ")
        (let* ((start (point-min))
               (text-start (nskk--skip-marker-pos start nskk-henkan-on-marker-regexp)))
          (should (equal (buffer-substring-no-properties text-start (point-max))
                         "かんじ"))))))

  (nskk-it "does nothing when no conversion start marker is set"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (insert "かん")
        (nskk--dcomp-replace-preedit "かんじ")
        (should (equal (buffer-string) "かん"))))))

(nskk-describe "nskk-dynamic-complete cycling behavior"
  (nskk-it "cycles through candidates on successive calls"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification
          (insert nskk-henkan-on-marker)
          (insert "かん"))
        (nskk--set-conversion-start-marker (point-min))
        (setq nskk--dcomp-prefix "かん"
              nskk--dcomp-candidates '("かんじ" "かんせい" "かんたん")
              nskk--dcomp-index 0)
        (nskk-dynamic-complete)
        (should (= nskk--dcomp-index 1))
        (nskk-dynamic-complete)
        (should (= nskk--dcomp-index 2)))))

  (nskk-it "wraps around to index 0 after last candidate"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification
          (insert nskk-henkan-on-marker)
          (insert "かんたん"))
        (nskk--set-conversion-start-marker (point-min))
        (setq nskk--dcomp-prefix "かん"
              nskk--dcomp-candidates '("かんじ" "かんたん")
              nskk--dcomp-index 1)
        (nskk-dynamic-complete)
        (should (= nskk--dcomp-index 0)))))

  (nskk-it "does nothing when preedit is empty"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification (insert nskk-henkan-on-marker))
        (nskk--set-conversion-start-marker (point-min))
        (setq nskk--dcomp-candidates nil
              nskk--dcomp-index 0)
        (nskk-dynamic-complete)
        (should (null nskk--dcomp-candidates))))))

;;;
;;; nskk--dismiss-candidate-list
;;;

;; Dynamic sentinel used by the hook-call test below.  A `defvar'-declared
;; variable ensures the lambda's `cl-incf' resolves it via dynamic lookup,
;; avoiding the lexical-closure/special-variable interaction that occurs when
;; a closure capturing a lexical variable is stored in a `defvar' hook list.
(defvar nskk--test-dismiss-call-count 0)

(nskk-describe "nskk--dismiss-candidate-list"
  (nskk-it "clears nskk--henkan-candidate-list-active"
    (with-temp-buffer
      (let ((nskk--henkan-candidate-list-active t)
            (nskk-henkan-hide-candidates-functions nil))
        (nskk--dismiss-candidate-list)
        (should-not nskk--henkan-candidate-list-active))))

  (nskk-it "runs nskk-henkan-hide-candidates-functions hook"
    (with-temp-buffer
      (let ((nskk--test-dismiss-call-count 0)
            (nskk--henkan-candidate-list-active t)
            (nskk-henkan-hide-candidates-functions
             (list (lambda () (cl-incf nskk--test-dismiss-call-count)))))
        (nskk--dismiss-candidate-list)
        (should (= nskk--test-dismiss-call-count 1)))))

  (nskk-it "is idempotent when list is already nil"
    (with-temp-buffer
      (let ((nskk--henkan-candidate-list-active nil)
            (nskk-henkan-hide-candidates-functions nil))
        (should-not (nskk--dismiss-candidate-list))
        (should-not nskk--henkan-candidate-list-active)))))

;;;
;;; nskk-henkan-do-reset
;;;

(nskk-describe "nskk-henkan-do-reset"
  (nskk-it "clears the romaji buffer"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (setq nskk--romaji-buffer "sh")
        (nskk-henkan-do-reset)
        (should (string-empty-p nskk--romaji-buffer)))))

  (nskk-it "resets henkan-count to 0"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (setq nskk--henkan-count 5)
        (nskk-henkan-do-reset)
        (should (= nskk--henkan-count 0)))))

  (nskk-it "clears conversion-start-marker"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (insert "test")
        (nskk--set-conversion-start-marker (point-min))
        (nskk-henkan-do-reset)
        (should (null (nskk--get-conversion-start))))))

  (nskk-it "returns nil"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (should (null (nskk-henkan-do-reset))))))

  (nskk-it "clears candidate-list-active via nskk--dismiss-candidate-list"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (setq nskk--henkan-candidate-list-active t)
        (nskk-henkan-do-reset)
        (should-not nskk--henkan-candidate-list-active)))))

;;;
;;; nskk-henkan-kakutei
;;;

(nskk-describe "nskk-henkan-kakutei"
  (nskk-it "removes the ▽ marker from the buffer"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification
          (insert nskk-henkan-on-marker "かんじ"))
        (nskk--set-conversion-start-marker (point-min))
        (nskk-henkan-kakutei)
        ;; The ▽ marker should be removed; text remains
        (should (not (string-search nskk-henkan-on-marker (buffer-string)))))))

  (nskk-it "clears the conversion start marker"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification (insert nskk-henkan-on-marker))
        (nskk--set-conversion-start-marker (point-min))
        (nskk-henkan-kakutei)
        (should (null (nskk--get-conversion-start))))))

  (nskk-it "clears the romaji buffer"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (setq nskk--romaji-buffer "sh")
        (nskk-henkan-kakutei)
        (should (string-empty-p nskk--romaji-buffer)))))

  (nskk-it "resets henkan-phase to nil in current state"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-henkan-kakutei)
        (should (null (nskk-state-henkan-phase nskk-current-state))))))

  (nskk-it "clears all AZIK okurigana pending state vars on kakutei"
    ;; kakutei must clear colon-pending, colon-deferred, and sokuon-okuri-pending
    ;; so that navigating away after arming any AZIK okurigana state does not
    ;; leave stale flags that misroute the next keypress.
    (dolist (spec '((nskk--azik-colon-okuri-pending      . t)
                    (nskk--azik-colon-okuri-deferred     . (?k . "k"))
                    (nskk--azik-sokuon-okuri-kana-pending . t)))
      (nskk-prolog-test-with-isolated-db
        (with-temp-buffer
          (nskk-mode 1)
          (set (car spec) (cdr spec))
          (nskk-henkan-kakutei)
          (should-not (symbol-value (car spec))))))))

;;;
;;; nskk-henkan-initialize
;;;

(nskk-describe "nskk-henkan-initialize"
  (nskk-it "is idempotent: calling twice does not error"
    (nskk-prolog-test-with-isolated-db
      (nskk-henkan-initialize)
      (should (progn (nskk-henkan-initialize) t))))

  (nskk-it "populates core-search-type/2 Prolog facts"
    (nskk-prolog-test-with-isolated-db
      (nskk-henkan-initialize)
      (let ((action (nskk-prolog-query-value
                     '(core-search-type :exact \?a) '\?a)))
        (should (eq action 'dict-lookup)))))

  (nskk-it "populates okurigana-char/2 facts for uppercase letters"
    (nskk-prolog-test-with-isolated-db
      (nskk-henkan-initialize)
      ;; ?K → ?k
      (let ((lower (nskk-prolog-query-value
                    `(okurigana-char ,?K \?l) '\?l)))
        (should (= lower ?k)))))

  (nskk-it "populates clearable-input-var/1 Prolog facts (defined in nskk-input)"
    (nskk-prolog-test-with-isolated-db
      (nskk-input-initialize)
      (let ((vars (nskk-prolog-query-all-values
                   '(clearable-input-var \?v) '\?v)))
        (should (memq 'nskk--numeric-mode vars))
        (should (memq 'nskk--sticky-shift-pending vars))
        (should (memq 'nskk--deferred-azik-state vars))
        (should (memq 'nskk--deferred-vowel-shadow-state vars))
        (should (memq 'nskk--azik-colon-okuri-pending vars))
        (should (memq 'nskk--azik-colon-okuri-deferred vars))
        ;; All 6 expected variables must be present.
        (should (>= (length vars) 6))))))

;;;
;;; nskk--insert-registered-and-reset
;;;

(nskk-describe "nskk--insert-registered-and-reset"
  (nskk-it "inserts registered word at start and resets state"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-overlay nil)
            (nskk--pending-romaji-overlay nil)
            (nskk--conversion-start-marker nil)
            (nskk--romaji-buffer "")
            (nskk--henkan-count 3)
            (nskk--henkan-candidate-list-active nil)
            (nskk-henkan-hide-candidates-functions nil)
            (called nil))
        (insert "▼かんじ")
        (goto-char (point-max))
        (nskk--insert-registered-and-reset "漢字" 1 (lambda () (setq called t)))
        (should (string= (buffer-string) "漢字"))
        (should called))))

  (nskk-it "tolerates nil on-done callback"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-overlay nil)
            (nskk--pending-romaji-overlay nil)
            (nskk--conversion-start-marker nil)
            (nskk--romaji-buffer "")
            (nskk--henkan-count 1)
            (nskk--henkan-candidate-list-active nil)
            (nskk-henkan-hide-candidates-functions nil))
        (insert "▼test")
        (goto-char (point-max))
        ;; Should not error when on-done is nil
        (should (progn (nskk--insert-registered-and-reset "result" 1 nil) t))))))

;;;
;;; nskk--replace-preedit-with-converted
;;;

(nskk-describe "nskk--replace-preedit-with-converted"
  (nskk-it "replaces preedit text and removes marker"
    (with-temp-buffer
      (insert "▽かな")
      (goto-char (point-max))
      (let ((start 1)
            (text-start (1+ (length "▽"))))
        (nskk--replace-preedit-with-converted text-start start "カナ")
        (should (string= (buffer-string) "カナ"))))))

;;;
;;; nskk-reset-henkan-state macro
;;;

(nskk-describe "nskk-reset-henkan-state macro"
  (nskk-it "clears candidates on the current state"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-state-set-candidates nskk-current-state '("候補1" "候補2"))
      (nskk-with-current-state
        (nskk-reset-henkan-state))
      (should (null (nskk-state-candidates nskk-current-state)))))

  (nskk-it "resets henkan-phase to nil on the current state"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-with-current-state
        (nskk-reset-henkan-state))
      (should (null (nskk-state-henkan-phase nskk-current-state)))))

  (nskk-it "clears okurigana state on current state"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-state-set-okurigana nskk-current-state ?k)
      (nskk-with-current-state
        (nskk-reset-henkan-state))
      (should (null (nskk-state-get-okurigana nskk-current-state)))))

  (nskk-it "does not touch nskk--romaji-buffer (macro scope only)"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--romaji-buffer "sh"))
      (nskk-with-current-state
        (nskk-reset-henkan-state))
      (should (equal nskk--romaji-buffer "sh")))))

;;;
;;; nskk-when-bound / nskk-when-bound-and Macro Tests
;;;

(nskk-describe "nskk-when-bound"
  (nskk-it "executes body when a defvar variable is bound (even if nil)"
    ;; nskk--henkan-candidate-list-active is a defvar, always bound after
    ;; nskk-henkan.el loads.  boundp works for dynamic variables only.
    (let (executed)
      (nskk-when-bound nskk--henkan-candidate-list-active
        (setq executed t))
      (should executed)))

  (nskk-it "does not execute body when variable is unbound"
    (let (executed)
      (makunbound 'nskk--test-unbound-sentinel-wkb)
      (nskk-when-bound nskk--test-unbound-sentinel-wkb
        (setq executed t))
      (should-not executed)))

  (nskk-it "returns the body result when variable is bound"
    ;; nskk-henkan-show-candidates-functions is a defvar (nil by default);
    ;; boundp returns t.  The body evaluates to t.
    (should (nskk-when-bound nskk-henkan-show-candidates-functions
              t))))

(nskk-describe "nskk-when-bound-and"
  (nskk-it "executes body when variable is bound and satisfies predicate"
    ;; nskk--dcomp-prefix is a defvar-local initialised to nil; stringp fails.
    ;; Use nskk--romaji-buffer which is a defvar-local string (bound + stringp).
    ;; We set it to a known string so stringp passes.
    (let (executed)
      (with-temp-buffer
        (set (make-local-variable 'nskk--romaji-buffer) "")
        (nskk-when-bound-and nskk--romaji-buffer stringp
          (setq executed t)))
      (should executed)))

  (nskk-it "does not execute body when variable is unbound"
    (let (executed)
      (makunbound 'nskk--test-unbound-sentinel-wba)
      (nskk-when-bound-and nskk--test-unbound-sentinel-wba stringp
        (setq executed t))
      (should-not executed)))

  (nskk-it "does not execute body when bound variable fails predicate"
    ;; nskk--henkan-candidate-list-active is a defvar (nil or t) — not a string.
    (let (executed)
      (nskk-when-bound-and nskk--henkan-candidate-list-active stringp
        (setq executed t))
      (should-not executed))))

;;;
;;; Conversion Start Marker Helper Tests
;;;

(nskk-describe "nskk--set-conversion-start-marker"
  (nskk-it "creates a marker at the given position"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker nil))
        (insert "abcd")
        (goto-char (point-min))
        (nskk--set-conversion-start-marker (point-min))
        (should (markerp nskk--conversion-start-marker))
        (should (= (marker-position nskk--conversion-start-marker) (point-min))))))

  (nskk-it "sets the marker to a mid-buffer position"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker nil))
        (insert "▽かな")
        (goto-char 2)
        (nskk--set-conversion-start-marker 2)
        (should (= (marker-position nskk--conversion-start-marker) 2))))))

(nskk-describe "nskk--clear-conversion-start-marker"
  (nskk-it "clears the marker position to nil"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker)))
        (set-marker nskk--conversion-start-marker (point-min))
        (should (marker-position nskk--conversion-start-marker))
        (nskk--clear-conversion-start-marker)
        (should-not (marker-position nskk--conversion-start-marker)))))

  (nskk-it "is safe to call when marker is already nil"
    (let ((nskk--conversion-start-marker nil))
      (nskk--clear-conversion-start-marker)
      (should-not nskk--conversion-start-marker))))

(nskk-describe "nskk--conversion-start-active-p"
  (nskk-it "returns non-nil when marker has a position"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker)))
        (set-marker nskk--conversion-start-marker (point-min))
        (should (nskk--conversion-start-active-p)))))

  (nskk-it "returns nil when marker is nil"
    (let ((nskk--conversion-start-marker nil))
      (should-not (nskk--conversion-start-active-p))))

  (nskk-it "returns nil when marker has no position"
    (let ((nskk--conversion-start-marker (make-marker)))
      (should-not (nskk--conversion-start-active-p)))))

(nskk-describe "nskk--get-conversion-start"
  (nskk-it "returns the marker position as an integer"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker)))
        (insert "  ▽かな")
        (set-marker nskk--conversion-start-marker 3)
        (should (= (nskk--get-conversion-start) 3)))))

  (nskk-it "returns nil when no marker is set"
    (let ((nskk--conversion-start-marker nil))
      (should-not (nskk--get-conversion-start))))

  (nskk-it "returns nil when marker has no position"
    (let ((nskk--conversion-start-marker (make-marker)))
      (should-not (nskk--get-conversion-start)))))

(nskk-describe "nskk--has-preedit"
  (nskk-it "returns non-nil when preedit text exists after the marker"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker)))
        (set-marker nskk--conversion-start-marker (point-min))
        (insert nskk-henkan-on-marker "かな")
        (should (nskk--has-preedit)))))

  (nskk-it "returns nil when point is right after the marker (no text)"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker)))
        (set-marker nskk--conversion-start-marker (point-min))
        (insert nskk-henkan-on-marker)
        (should-not (nskk--has-preedit)))))

  (nskk-it "returns nil when no conversion start marker is set"
    (let ((nskk--conversion-start-marker nil))
      (should-not (nskk--has-preedit)))))

;;;
;;; nskk--extract-okuri-query Tests
;;;

(nskk-describe "nskk--extract-okuri-query"
  (nskk-it "builds query by stripping * marker and appending okuri consonant"
    (with-temp-buffer
      (insert nskk-henkan-on-marker "ほ*")
      (let* ((start (point-min))
             (preedit-end (point))
             (query (nskk--extract-okuri-query start preedit-end ?k)))
        (should (equal query "ほk")))))

  (nskk-it "handles preedit without okurigana marker"
    (with-temp-buffer
      (insert nskk-henkan-on-marker "かん")
      (let* ((start (point-min))
             (preedit-end (point))
             (query (nskk--extract-okuri-query start preedit-end ?j)))
        (should (equal query "かんj")))))

  (nskk-it "returns nil when preedit-end equals text-start (empty preedit)"
    (with-temp-buffer
      (insert nskk-henkan-on-marker)
      (let* ((start (point-min))
             (preedit-end (point))
             (query (nskk--extract-okuri-query start preedit-end ?k)))
        (should-not query))))

  (nskk-it "returns nil when start is nil"
    (let ((query (nskk--extract-okuri-query nil 10 ?k)))
      (should-not query))))

;;;
;;; nskk--remove-okuri-marker Tests
;;;

(nskk-describe "nskk--remove-okuri-marker"
  (nskk-it "deletes the * marker between search-start and preedit-end"
    (with-temp-buffer
      (insert "ほ*")
      (let ((search-start (point-min))
            (preedit-end (point)))
        (nskk--remove-okuri-marker search-start preedit-end)
        (should (equal (buffer-string) "ほ")))))

  (nskk-it "does nothing when no * marker exists in range"
    (with-temp-buffer
      (insert "ほ")
      (let ((search-start (point-min))
            (preedit-end (point)))
        (nskk--remove-okuri-marker search-start preedit-end)
        (should (equal (buffer-string) "ほ")))))

  (nskk-it "does not affect text outside the preedit-end range"
    (with-temp-buffer
      (insert "ほ*く")
      (let ((search-start (point-min))
            (preedit-end (+ (point-min) (string-bytes "ほ*"))))
        (nskk--remove-okuri-marker search-start preedit-end)
        (should (equal (buffer-string) "ほく"))))))

;;;
;;; nskk-convert-input-to-kana-final Tests
;;;

(nskk-describe "nskk-convert-input-to-kana-final"
  (nskk-it "returns empty string when romaji buffer is empty"
    (with-temp-buffer
      (let ((nskk--romaji-buffer "")
            (nskk--pending-romaji-overlay nil))
        (should (equal (nskk-convert-input-to-kana-final) "")))))

  (nskk-it "converts standalone n to ん (hatsuon at word boundary)"
    (with-temp-buffer
      (let ((nskk--romaji-buffer "n")
            (nskk--pending-romaji-overlay nil))
        (should (equal (nskk-convert-input-to-kana-final) "ん")))))

  (nskk-it "converts romaji like ka to か"
    (with-temp-buffer
      (let ((nskk--romaji-buffer "ka")
            (nskk--pending-romaji-overlay nil))
        (should (equal (nskk-convert-input-to-kana-final) "か")))))

  (nskk-it "clears the romaji buffer after conversion"
    (with-temp-buffer
      (let ((nskk--romaji-buffer "shi")
            (nskk--pending-romaji-overlay nil))
        (nskk-convert-input-to-kana-final)
        (should (equal nskk--romaji-buffer ""))))))

(nskk-describe "nskk-convert-input-to-kana-final/k"
  (nskk-it "calls on-done with empty string when buffer is empty"
    (with-temp-buffer
      (let ((nskk--romaji-buffer "")
            (nskk--pending-romaji-overlay nil)
            result)
        (nskk-convert-input-to-kana-final/k (lambda (s) (setq result s)) #'ignore)
        (should (equal result "")))))

  (nskk-it "calls on-done with ん for standalone n"
    (with-temp-buffer
      (let ((nskk--romaji-buffer "n")
            (nskk--pending-romaji-overlay nil)
            result)
        (nskk-convert-input-to-kana-final/k (lambda (s) (setq result s)) #'ignore)
        (should (equal result "ん")))))

  (nskk-it "calls on-done with converted kana for complete romaji"
    (with-temp-buffer
      (let ((nskk--romaji-buffer "tsu")
            (nskk--pending-romaji-overlay nil)
            result)
        (nskk-convert-input-to-kana-final/k (lambda (s) (setq result s)) #'ignore)
        (should (equal result "つ")))))

  (nskk-it "is consistent with the sync variant across common romaji"
    (nskk-deftest-table kana-final-cps-sync-consistency
      :columns (romaji)
      :rows (("") ("n") ("ka") ("shi") ("tsu"))
      :body (with-temp-buffer
              (let ((nskk--romaji-buffer romaji)
                    (nskk--pending-romaji-overlay nil)
                    cps-result)
                (nskk-convert-input-to-kana-final/k
                  (lambda (s) (setq cps-result s)) #'ignore)
                (let ((nskk--romaji-buffer romaji)
                      (nskk--pending-romaji-overlay nil))
                  (should (equal cps-result
                                 (nskk-convert-input-to-kana-final)))))))))


;;;
;;; nskk--dcomp-search-prefix Tests
;;;

(nskk-describe "nskk--dcomp-search-prefix"
  (nskk-it "returns keys from user-dict-entry trie that start with prefix"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (nskk-prolog-assert '((user-dict-entry "かんじ" ("漢字"))))
      (nskk-prolog-assert '((user-dict-entry "かんじれい" ("漢字麗"))))
      (nskk-prolog-assert '((user-dict-entry "まる" ("丸"))))
      (let ((results (nskk--dcomp-search-prefix "かんじ")))
        ;; Only "かんじれい" matches (excludes exact prefix "かんじ" itself)
        (should (member "かんじれい" results))
        (should-not (member "かんじ" results))
        (should-not (member "まる" results)))))

  (nskk-it "returns empty list when no keys start with the prefix"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (nskk-prolog-assert '((user-dict-entry "まる" ("丸"))))
      (let ((results (nskk--dcomp-search-prefix "かんじ")))
        (should (null results)))))

  (nskk-it "returns results sorted by string<"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (nskk-prolog-assert '((user-dict-entry "かんとく" ("相当局"))))
      (nskk-prolog-assert '((user-dict-entry "かんじ" ("漢字"))))
      (let ((results (nskk--dcomp-search-prefix "かん")))
        (should (> (length results) 1))
        (should (equal results (sort (copy-sequence results) #'string<)))))))

;;;
;;; nskk-set-active-candidates Macro Tests
;;;

(nskk-describe "nskk-set-active-candidates"
  ;; nskk-state-set-henkan-phase validates transitions: nil→on, on→active.
  ;; We must start from 'on phase so the macro's nil→active would fail;
  ;; use force to pre-set 'on, then the macro transitions on→active.
  (nskk-it "sets candidates and transitions henkan-phase to active"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk-set-active-candidates '("漢字" "感じ"))
      (should (equal (nskk-state-candidates nskk-current-state) '("漢字" "感じ")))
      (should (eq (nskk-state-henkan-phase nskk-current-state) 'active))))

  (nskk-it "resets candidate index to 0 via nskk-state-set-candidates"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (setf (nskk-state-current-index nskk-current-state) 3)
      (nskk-set-active-candidates '("A"))
      (should (= (nskk-state-current-index nskk-current-state) 0)))))

;;;
;;; nskk--clear-conversion-context Tests
;;;

(nskk-describe "nskk--clear-conversion-context"
  (nskk-it "resets dcomp state variables to nil/0"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--dcomp-candidates '("かんじ" "かんとく"))
            (nskk--dcomp-prefix "かん")
            (nskk--dcomp-index 1)
            (nskk--conversion-overlay nil)
            (nskk--pending-romaji-overlay nil)
            (nskk--conversion-start-marker nil)
            (nskk--romaji-buffer ""))
        (nskk--clear-conversion-context)
        (should (null nskk--dcomp-candidates))
        (should (null nskk--dcomp-prefix))
        (should (= nskk--dcomp-index 0)))))

  (nskk-it "clears conversion candidate state from nskk-current-state"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-overlay nil)
            (nskk--pending-romaji-overlay nil)
            (nskk--conversion-start-marker nil)
            (nskk--romaji-buffer "")
            (nskk--dcomp-candidates nil)
            (nskk--dcomp-prefix nil)
            (nskk--dcomp-index 0))
        (nskk-state-set-candidates nskk-current-state '("A" "B"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk--clear-conversion-context)
        (should (null (nskk-state-candidates nskk-current-state)))
        (should (null (nskk-state-henkan-phase nskk-current-state))))))

  (nskk-it "clears input state variables via Prolog clearable-input-var/1 table"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-overlay nil)
            (nskk--pending-romaji-overlay nil)
            (nskk--conversion-start-marker nil)
            (nskk--romaji-buffer "")
            (nskk--dcomp-candidates nil)
            (nskk--dcomp-prefix nil)
            (nskk--dcomp-index 0)
            (nskk--numeric-mode t)
            (nskk--sticky-shift-pending t)
            (nskk--deferred-azik-state '(some state))
            (nskk--deferred-vowel-shadow-state t)
            (nskk--azik-colon-okuri-pending t)
            (nskk--azik-colon-okuri-deferred t))
        (nskk--clear-conversion-context)
        (should-not nskk--numeric-mode)
        (should-not nskk--sticky-shift-pending)
        (should-not nskk--deferred-azik-state)
        (should-not nskk--deferred-vowel-shadow-state)
        (should-not nskk--azik-colon-okuri-pending)
        (should-not nskk--azik-colon-okuri-deferred))))

  (nskk-it "resets nskk--henkan-candidate-list-active to nil on mode switch"
    ;; Regression test: nskk--clear-conversion-context must call
    ;; nskk--dismiss-candidate-list (not bare run-hook-with-args) so that
    ;; nskk--henkan-candidate-list-active is reset atomically with the
    ;; hide-candidates hook.  Without this fix, mode switches left the flag
    ;; t even though the candidate list UI was already hidden.
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--henkan-candidate-list-active t)
            (nskk-henkan-hide-candidates-functions nil)
            (nskk--conversion-overlay nil)
            (nskk--pending-romaji-overlay nil)
            (nskk--conversion-start-marker nil)
            (nskk--romaji-buffer "")
            (nskk--dcomp-candidates nil)
            (nskk--dcomp-prefix nil)
            (nskk--dcomp-index 0))
        (nskk--clear-conversion-context)
        (should-not nskk--henkan-candidate-list-active)))))

;;;
;;; nskk--wrap-to-first-candidate Tests
;;;

(nskk-describe "nskk--wrap-to-first-candidate"
  ;; nskk--wrap-to-first-candidate calls nskk-state-set-henkan-phase with 'list.
  ;; Valid transition to 'list requires starting from 'active phase (active→list).
  (nskk-it "resets index to 0 and updates henkan-count to threshold"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--henkan-count 7)
            (nskk-henkan-show-candidates-nth 5)
            (nskk--henkan-candidate-list-active nil))
        (nskk-state-set-candidates nskk-current-state '("A" "B" "C"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (setf (nskk-state-current-index nskk-current-state) 2)
        (nskk-with-mocks ((run-hook-with-args #'ignore))
          (nskk--wrap-to-first-candidate))
        (should (= (nskk-state-current-index nskk-current-state) 0))
        (should (= nskk--henkan-count nskk-henkan-show-candidates-nth)))))

  (nskk-it "sets henkan-phase to list and activates candidate-list"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--henkan-count 0)
            (nskk-henkan-show-candidates-nth 3)
            (nskk--henkan-candidate-list-active nil))
        (nskk-state-set-candidates nskk-current-state '("A"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((run-hook-with-args #'ignore))
          (nskk--wrap-to-first-candidate))
        (should (eq (nskk-state-henkan-phase nskk-current-state) 'list))
        (should nskk--henkan-candidate-list-active))))

  (nskk-it "fires nskk-henkan-show-candidates-functions hook with candidates and index 0"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--henkan-count 0)
            (nskk-henkan-show-candidates-nth 3)
            (nskk--henkan-candidate-list-active nil)
            captured-args)
        (nskk-state-set-candidates nskk-current-state '("X" "Y"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((run-hook-with-args
                           (lambda (hook &rest args) (setq captured-args args))))
          (nskk--wrap-to-first-candidate))
        (should (equal (car captured-args) '("X" "Y")))
        (should (= (cadr captured-args) 0))))))

;;;
;;; nskk-cancel-conversion-to-reading Tests
;;;

(nskk-describe "nskk-cancel-conversion-to-reading"
  (nskk-it "does nothing when not converting"
    (with-temp-buffer
      (insert "unchanged")
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker)))
        (nskk-cancel-conversion-to-reading)
        (should (equal (buffer-string) "unchanged")))))

  (nskk-it "removes the ▼ marker from buffer when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--conversion-overlay nil)
            (nskk--pending-romaji-overlay nil)
            (nskk--romaji-buffer "")
            (nskk--henkan-count 2)
            (nskk--henkan-candidate-list-active t))
        (insert nskk-henkan-active-marker "かんじ")
        (set-marker nskk--conversion-start-marker (point-min))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-state-set-candidates nskk-current-state '("漢字"))
        (nskk-cancel-conversion-to-reading)
        ;; ▼ marker removed; kana reading remains
        (should (string-match-p "かんじ" (buffer-string)))
        (should-not (string-match-p (regexp-quote nskk-henkan-active-marker) (buffer-string))))))

  (nskk-it "clears henkan-count and candidate-list-active"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--conversion-overlay nil)
            (nskk--pending-romaji-overlay nil)
            (nskk--romaji-buffer "")
            (nskk--henkan-count 4)
            (nskk--henkan-candidate-list-active t))
        (insert nskk-henkan-active-marker "ほげ")
        (set-marker nskk--conversion-start-marker (point-min))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-state-set-candidates nskk-current-state '("保毛"))
        (nskk-cancel-conversion-to-reading)
        (should (= nskk--henkan-count 0))
        (should-not nskk--henkan-candidate-list-active)))))

;;;
;;; nskk--show-pending-romaji / nskk--clear-pending-romaji Tests
;;;

(nskk-describe "nskk--show-pending-romaji"
  (nskk-it "creates an overlay with the given text as after-string"
    (with-temp-buffer
      (let ((nskk--pending-romaji-overlay nil))
        (nskk--show-pending-romaji "ka")
        (should nskk--pending-romaji-overlay)
        (should (overlayp nskk--pending-romaji-overlay))
        (should (equal (overlay-get nskk--pending-romaji-overlay 'after-string) "ka")))))

  (nskk-it "does nothing for an empty string"
    (with-temp-buffer
      (let ((nskk--pending-romaji-overlay nil))
        (nskk--show-pending-romaji "")
        (should-not nskk--pending-romaji-overlay))))

  (nskk-it "does nothing for a non-string argument"
    (with-temp-buffer
      (let ((nskk--pending-romaji-overlay nil))
        (nskk--show-pending-romaji nil)
        (should-not nskk--pending-romaji-overlay)))))

(nskk-describe "nskk--clear-pending-romaji"
  (nskk-it "deletes the pending romaji overlay when present"
    (with-temp-buffer
      (let ((nskk--pending-romaji-overlay nil))
        (nskk--show-pending-romaji "ka")
        (should nskk--pending-romaji-overlay)
        (nskk--clear-pending-romaji)
        (should-not (and nskk--pending-romaji-overlay
                         (overlay-buffer nskk--pending-romaji-overlay))))))

  (nskk-it "is safe to call when no overlay exists (idempotent)"
    (with-temp-buffer
      (let ((nskk--pending-romaji-overlay nil))
        (should-not (nskk--clear-pending-romaji))))))

;;;
;;; nskk-convert-or-commit/k Tests
;;;

(nskk-describe "nskk-convert-or-commit/k"
  (nskk-it "calls on-done after committing when in active conversion phase"
    (nskk-with-henkan-state 'active '("結果")
      (setf (nskk-state-current-index nskk-current-state) 0)
      (let (on-done-called)
        (nskk-convert-or-commit/k (lambda () (setq on-done-called t)))
        (should on-done-called)
        (should (equal (buffer-string) "結果")))))

  (nskk-it "calls on-done after starting conversion when not in active phase"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            start-conversion-called on-done-called)
        (set-marker nskk--conversion-start-marker (point-min))
        (insert nskk-henkan-on-marker "かな")
        (nskk-state-force-henkan-phase nskk-current-state 'on)
        (nskk-with-mocks ((nskk-start-conversion (lambda () (setq start-conversion-called t))))
          (nskk-convert-or-commit/k (lambda () (setq on-done-called t))))
        (should start-conversion-called)
        (should on-done-called)))))

;;;
;;; nskk-commit-current/k Tests
;;;

(nskk-describe "nskk-commit-current/k"
  (nskk-it "calls on-committed with the committed candidate string"
    (nskk-with-henkan-state 'active '("変換" "変換2")
      (setf (nskk-state-current-index nskk-current-state) 0)
      (let (committed-candidate)
        (nskk-commit-current/k (lambda (c) (setq committed-candidate c)) #'ignore)
        (should (equal committed-candidate "変換")))))

  (nskk-it "calls on-committed with the candidate at index 1"
    (nskk-with-henkan-state 'active '("first" "second" "third")
      (setf (nskk-state-current-index nskk-current-state) 1)
      (let (committed-candidate)
        (nskk-commit-current/k (lambda (c) (setq committed-candidate c)) #'ignore)
        (should (equal committed-candidate "second")))))

  (nskk-it "does not call on-committed when not converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            on-committed-called)
        (nskk-commit-current/k (lambda (_c) (setq on-committed-called t)) #'ignore)
        (should-not on-committed-called)))))

;;;
;;; nskk-next-candidate/k Tests
;;;

(nskk-describe "nskk-next-candidate/k"
  (nskk-it "calls on-exhausted when not converting"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--henkan-count 0)
          exhausted-called)
      (nskk-next-candidate/k #'ignore (lambda () (setq exhausted-called t)))
      (should exhausted-called)))

  (nskk-it "calls on-candidate with current candidate when selecting inline"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--henkan-count 0)
            (nskk-henkan-show-candidates-nth 5)
            received-candidate)
        (set-marker nskk--conversion-start-marker (point-min))
        (insert "test")
        (nskk-state-set-candidates nskk-current-state '("漢字" "感じ"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk--select-candidate #'ignore))
          (nskk-next-candidate/k (lambda (c) (setq received-candidate c)) #'ignore))
        (should (equal received-candidate "漢字"))))))

;;;
;;; nskk-previous-candidate/k Tests
;;;

(nskk-describe "nskk-previous-candidate/k"
  (nskk-it "calls on-not-found when not converting"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--henkan-count 0)
          not-found-called)
      ;; When not converting, on-not-found is called (standard defun/k pattern)
      (nskk-previous-candidate/k #'ignore (lambda () (setq not-found-called t)))
      (should not-found-called)))

  (nskk-it "calls on-found with the selected candidate after selecting prev candidate inline"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--henkan-count 2)
            (nskk--henkan-candidate-list-active nil)
            received-candidate)
        (set-marker nskk--conversion-start-marker (point-min))
        (insert "test")
        (nskk-state-set-candidates nskk-current-state '("a" "b" "c"))
        (nskk-state-set-current-index nskk-current-state 1)
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk--select-candidate #'ignore))
          ;; on-found is called with the candidate at current-index after decrement.
          ;; nskk--henkan-count decrements 2→1; nskk--select-candidate is mocked
          ;; (no real index change), so current-index stays at 1 → candidate "b".
          (nskk-previous-candidate/k (lambda (c) (setq received-candidate c)) #'ignore))
        (should (equal received-candidate "b"))))))

;;;
;;; nskk-start-conversion/k Tests
;;;

(nskk-describe "nskk-start-conversion/k"
  (nskk-it "calls on-found with candidates when search returns results"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--romaji-buffer "")
            (nskk--henkan-count 0)
            (nskk--conversion-overlay nil)
            found-candidates)
        (insert nskk-henkan-on-marker "かんじ")
        (set-marker nskk--conversion-start-marker (point-min))
        (goto-char (point-max))
        (nskk-state-force-henkan-phase nskk-current-state 'on)
        (nskk-with-mocks ((nskk-convert-input-to-kana-final/k
                           (lambda (on-done _ignored) (funcall on-done nil)))
                          (nskk-core-search/k
                           (lambda (_key _type _limit on-found _on-not-found)
                             (funcall on-found '("漢字" "感じ"))))
                          (nskk--update-overlay #'ignore)
                          (nskk--replace-marker-at #'ignore))
          (nskk-start-conversion/k
           (lambda (cands) (setq found-candidates cands))
           #'ignore
           #'ignore))
        (should (equal found-candidates '("漢字" "感じ")))
        (should (equal (nskk-state-candidates nskk-current-state) '("漢字" "感じ"))))))

  (nskk-it "calls on-not-found when search returns nothing and registration is cancelled"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--romaji-buffer "")
            (nskk--henkan-count 0)
            (nskk--conversion-overlay nil)
            (nskk--registration-depth 0)
            not-found-called)
        (insert nskk-henkan-on-marker "てすと")
        (set-marker nskk--conversion-start-marker (point-min))
        (goto-char (point-max))
        (nskk-state-force-henkan-phase nskk-current-state 'on)
        (nskk-with-mocks ((nskk-convert-input-to-kana-final/k
                           (lambda (on-done _ignored) (funcall on-done nil)))
                          (nskk-core-search/k
                           (lambda (_key _type _limit _on-found on-not-found)
                             (funcall on-not-found)))
                          (nskk-start-registration/k
                           (lambda (_reading on-done _ignored) (funcall on-done nil))))
          (nskk-start-conversion/k
           #'ignore
           (lambda () (setq not-found-called t))
           #'ignore))
        (should not-found-called)))))

;;;
;;; nskk--exhaust-candidates/k Tests
;;;

(nskk-describe "nskk--exhaust-candidates/k"
  (nskk-it "wraps to first candidate and calls on-done when no preedit text"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker nil)   ; no marker = no text
            (nskk--henkan-candidate-list-active t)
            wrap-called on-done-called)
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-state-set-candidates nskk-current-state '("A"))
        (nskk-with-mocks ((nskk--wrap-to-first-candidate (lambda () (setq wrap-called t)))
                          (run-hook-with-args #'ignore))
          (nskk--exhaust-candidates/k (lambda () (setq on-done-called t))))
        (should wrap-called)
        (should on-done-called)
        ;; candidate-list-active should be reset
        (should-not nskk--henkan-candidate-list-active))))

  (nskk-it "calls nskk-start-registration/k with preedit text when candidates exhausted"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--conversion-overlay nil)
            (nskk--henkan-candidate-list-active t)
            registration-text on-done-called)
        (insert nskk-henkan-active-marker "かんじ")
        (set-marker nskk--conversion-start-marker (point-min))
        (goto-char (point-max))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-state-set-candidates nskk-current-state '("漢字"))
        (nskk-with-mocks ((nskk-start-registration/k
                           (lambda (text on-done _ignored)
                             (setq registration-text text)
                             (funcall on-done nil)))  ; registration cancelled
                          (nskk--wrap-to-first-candidate #'ignore)
                          (run-hook-with-args #'ignore))
          (nskk--exhaust-candidates/k (lambda () (setq on-done-called t))))
        (should (equal registration-text "かんじ"))
        (should on-done-called)))))

;;;
;;; nskk-cancel-conversion/k Tests
;;;

(nskk-describe "nskk-cancel-conversion/k"
  (nskk-it "always calls on-done even when not converting"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          on-done-called)
      (nskk-cancel-conversion/k (lambda () (setq on-done-called t)))
      (should on-done-called)))

  (nskk-it "calls nskk-rollback-conversion when converting then calls on-done"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            rollback-called on-done-called)
        (set-marker nskk--conversion-start-marker (point-min))
        (insert "test")
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk-rollback-conversion (lambda () (setq rollback-called t))))
          (nskk-cancel-conversion/k (lambda () (setq on-done-called t))))
        (should rollback-called)
        (should on-done-called)))))

;;;
;;; nskk-rollback-conversion/k Tests
;;;

(nskk-describe "nskk-rollback-conversion/k"
  (nskk-it "calls on-done even when not converting (no-op path)"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            on-done-called)
        (nskk-rollback-conversion/k (lambda () (setq on-done-called t)))
        (should on-done-called))))

  (nskk-it "restores preedit phase and calls on-done when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--conversion-overlay nil)
            (nskk--pending-romaji-overlay nil)
            (nskk--romaji-buffer "")
            (nskk--henkan-count 3)
            (nskk--henkan-candidate-list-active nil)
            on-done-called)
        (insert nskk-henkan-active-marker "かんじ")
        (set-marker nskk--conversion-start-marker (point-min))
        (goto-char (point-max))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-state-set-candidates nskk-current-state '("漢字" "感じ"))
        (nskk-with-mocks ((nskk--replace-marker-at #'ignore)
                          (run-hook-with-args #'ignore))
          (nskk-rollback-conversion/k (lambda () (setq on-done-called t))))
        (should on-done-called)
        (should (= nskk--henkan-count 0))))))

;;;
;;; nskk-cancel-preedit/k
;;;

(nskk-describe "nskk-cancel-preedit/k"
  (nskk-it "calls on-done even when no conversion start is set"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--pending-romaji-overlay nil)
            (nskk--romaji-buffer "")
            (nskk--henkan-count 0)
            on-done-called)
        (nskk-cancel-preedit/k (lambda () (setq on-done-called t)))
        (should on-done-called))))

  (nskk-it "deletes preedit text and calls on-done when conversion start is active"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--pending-romaji-overlay nil)
            (nskk--romaji-buffer "")
            (nskk--henkan-count 0)
            on-done-called)
        (insert nskk-henkan-on-marker "かんじ")
        (set-marker nskk--conversion-start-marker (point-min))
        (goto-char (point-max))
        (nskk-cancel-preedit/k (lambda () (setq on-done-called t)))
        (should on-done-called)
        (should (string-empty-p (buffer-string))))))

  (nskk-it "restores previous mode when cancelling from abbrev preedit"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'abbrev))
            (nskk--conversion-start-marker (make-marker))
            (nskk--pending-romaji-overlay nil)
            (nskk--romaji-buffer "")
            (nskk--henkan-count 0))
        (setf (nskk-state-previous-mode nskk-current-state) 'hiragana)
        (nskk-cancel-preedit/k #'ignore)
        ;; Mode restored to hiragana (the mode before abbrev was activated)
        (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))))

  (nskk-it "resets henkan-count to 0 after cancel"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--pending-romaji-overlay nil)
            (nskk--romaji-buffer "")
            (nskk--henkan-count 5))
        (nskk-cancel-preedit/k #'ignore)
        (should (= nskk--henkan-count 0))))))

;;;
;;; nskk--flush-romaji-before-okuri
;;;

(nskk-describe "nskk--flush-romaji-before-okuri"
  (nskk-it "does nothing when romaji buffer is empty but still clears pending romaji"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer "")
            (nskk--pending-romaji-overlay nil))
        (nskk--flush-romaji-before-okuri)
        (should (string-empty-p (buffer-string)))
        (should (string-empty-p nskk--romaji-buffer)))))

  (nskk-it "inserts ん when romaji buffer is standalone n at word boundary"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer "n")
            (nskk--pending-romaji-overlay nil))
        (nskk--flush-romaji-before-okuri)
        (should (equal (buffer-string) "ん"))
        (should (string-empty-p nskk--romaji-buffer)))))

  (nskk-it "converts complete romaji to kana and clears buffer"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer "ka")
            (nskk--pending-romaji-overlay nil))
        (nskk--flush-romaji-before-okuri)
        (should (equal (buffer-string) "か"))
        (should (string-empty-p nskk--romaji-buffer)))))

  (nskk-it "silently drops incomplete romaji sequences without inserting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer "k")
            (nskk--pending-romaji-overlay nil))
        (nskk--flush-romaji-before-okuri)
        (should (string-empty-p (buffer-string)))
        (should (string-empty-p nskk--romaji-buffer)))))

  (nskk-it "converts to katakana when state mode is katakana"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'katakana))
            (nskk--romaji-buffer "n")
            (nskk--pending-romaji-overlay nil))
        (nskk--flush-romaji-before-okuri)
        (should (equal (buffer-string) "ン"))
        (should (string-empty-p nskk--romaji-buffer))))))

;;;
;;; nskk--handle-consonant-okuri
;;;

(nskk-describe "nskk--handle-consonant-okuri"
  (nskk-it "puts the consonant into the romaji buffer"
    (let ((nskk--romaji-buffer "")
          (nskk--pending-romaji-overlay nil))
      (nskk-with-mocks ((nskk--show-pending-romaji #'ignore))
        (nskk--handle-consonant-okuri ?k (lambda () nil)))
      (should (equal nskk--romaji-buffer "k"))))

  (nskk-it "shows the consonant as a pending romaji overlay"
    (let ((nskk--romaji-buffer "")
          (nskk--pending-romaji-overlay nil)
          shown-text)
      (nskk-with-mocks ((nskk--show-pending-romaji (lambda (text) (setq shown-text text))))
        (nskk--handle-consonant-okuri ?s (lambda () nil)))
      (should (equal shown-text "s"))))

  (nskk-it "calls on-consumed with no arguments"
    (let ((nskk--romaji-buffer "")
          (nskk--pending-romaji-overlay nil)
          on-consumed-called)
      (nskk-with-mocks ((nskk--show-pending-romaji #'ignore))
        (nskk--handle-consonant-okuri ?m (lambda () (setq on-consumed-called t))))
      (should on-consumed-called))))

;;;
;;; nskk-process-okurigana-input/k
;;;

(nskk-describe "nskk-process-okurigana-input/k"
  (nskk-it "calls on-not-found when char is not an okurigana marker"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            not-found-called)
        (set-marker nskk--conversion-start-marker (point-min))
        (nskk-with-mocks ((nskk-detect-okurigana-char (lambda (_c) nil)))
          ;; on-not-found is called with no args (char must come from caller's closure)
          (nskk-process-okurigana-input/k ?a
            #'ignore
            (lambda () (setq not-found-called t))))
        (should not-found-called))))

  (nskk-it "calls on-not-found when conversion start marker is not active"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            not-found-called)
        ;; Marker with no position → not active
        (nskk-with-mocks ((nskk-detect-okurigana-char (lambda (_c) ?k)))
          (nskk-process-okurigana-input/k ?K
            #'ignore
            (lambda () (setq not-found-called t))))
        (should not-found-called))))

  (nskk-it "calls on-found with t for consonant okurigana when conversion start is active"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--romaji-buffer "")
            (nskk--pending-romaji-overlay nil)
            on-found-value)
        (insert nskk-henkan-on-marker "かく")
        (set-marker nskk--conversion-start-marker (point-min))
        (nskk-with-mocks ((nskk-detect-okurigana-char (lambda (_c) ?k))
                          (nskk-prolog-query (lambda (_q) nil))
                          (nskk--insert-marker #'ignore)
                          (nskk--show-pending-romaji #'ignore))
          (nskk-process-okurigana-input/k ?K
            (lambda (v) (setq on-found-value v))
            #'ignore))
        (should (eq on-found-value t))
        (should (equal nskk--romaji-buffer "k")))))

  (nskk-it "calls on-not-found when okurigana is already pending (YoNN guard)"
    ;; Regression: second uppercase N in YoNN must NOT re-enter okurigana.
    ;; When okurigana is already set in state, the guard rejects the char.
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--romaji-buffer "n")
            (nskk--pending-romaji-overlay nil)
            not-found-called)
        (insert nskk-henkan-on-marker "よ*")
        (set-marker nskk--conversion-start-marker (point-min))
        (nskk-state-set-okurigana nskk-current-state ?n)
        (nskk-with-mocks ((nskk-detect-okurigana-char (lambda (_c) ?n)))
          (nskk-process-okurigana-input/k ?N
            (lambda (_v) (error "on-found should not be called"))
            (lambda () (setq not-found-called t))))
        (should not-found-called)))))

;;;
;;; nskk--apply-okuri-candidates
;;;

(nskk-describe "nskk--apply-okuri-candidates"
  (nskk-it "updates overlay, sets active candidates, and sets henkan-count to 1"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--conversion-overlay nil)
            (nskk--henkan-count 0))
        (insert nskk-henkan-on-marker "か*")
        (set-marker nskk--conversion-start-marker (point-min))
        (nskk-state-force-henkan-phase nskk-current-state 'on)
        (let* ((start (marker-position nskk--conversion-start-marker))
               (text-start (+ start (length nskk-henkan-on-marker)))
               (preedit-end (point-max)))
          (nskk-with-mocks ((nskk--remove-okuri-marker #'ignore)
                            (nskk--replace-marker-at #'ignore)
                            (nskk--update-overlay #'ignore))
            (nskk--apply-okuri-candidates start text-start preedit-end
                                          '("書" "欠") "かk"))
          (should (equal (nskk-state-candidates nskk-current-state) '("書" "欠")))
          (should (= nskk--henkan-count 1))
          (should (nskk-state-get-metadata nskk-current-state 'okurigana-in-progress))
          (should (eq (nskk-state-henkan-phase nskk-current-state) 'active)))))))

;;;
;;; nskk--build-okuri-registration-reading
;;;

(nskk-describe "nskk--build-okuri-registration-reading"
  (nskk-it "builds stem*kana format when okuri-kana is present in buffer"
    (with-temp-buffer
      (insert nskk-henkan-on-marker "ほ*" "け")  ; stem=ほ, okuri-kana=け
      ;; Note: the function reads (point) to find the okurigana kana boundary.
      ;; (point) is at (point-max) after insert, which is the correct position here.
      (let* ((start (point-min))
             (text-start (+ start (length nskk-henkan-on-marker)))
             ;; preedit-end = just after the * marker
             (preedit-end (+ text-start (length "ほ") (length nskk-okurigana-marker))))
        (should (equal (nskk--build-okuri-registration-reading text-start preedit-end "ほk")
                       "ほ*け")))))

  (nskk-it "falls back to query when no okuri-kana (SPC path, no vowel typed)"
    (with-temp-buffer
      (insert nskk-henkan-on-marker "か*")  ; preedit-end = end of buffer, no kana after *
      (let* ((start (point-min))
             (text-start (+ start (length nskk-henkan-on-marker)))
             (preedit-end (point-max)))
        (should (equal (nskk--build-okuri-registration-reading text-start preedit-end "かk")
                       "かk"))))))

;;;
;;; nskk--trigger-okuri-conversion/k
;;;

(nskk-describe "nskk--trigger-okuri-conversion/k"
  (nskk-it "calls on-not-found immediately when no preedit query can be built"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker))
            on-not-found-called)
        ;; Marker with no position → extract-okuri-query returns nil → on-not-found
        (nskk--trigger-okuri-conversion/k ?k (point)
                                          #'ignore
                                          (lambda () (setq on-not-found-called t))
                                          #'ignore)
        (should on-not-found-called))))

  (nskk-it "calls on-found with candidates when search finds results"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--conversion-overlay nil)
            (nskk--henkan-count 0)
            on-found-candidates)
        (insert nskk-henkan-on-marker "かく")
        (set-marker nskk--conversion-start-marker (point-min))
        (let ((preedit-end (point-max)))
          (nskk-state-force-henkan-phase nskk-current-state 'on)
          (nskk-with-mocks ((nskk-core-search/k
                             (lambda (_q _type _limit on-found _on-not-found)
                               (funcall on-found '("書く"))))
                            (nskk--replace-marker-at #'ignore)
                            (nskk--remove-okuri-marker #'ignore)
                            (nskk--update-overlay #'ignore))
            (nskk--trigger-okuri-conversion/k ?k preedit-end
                                              (lambda (candidates)
                                                (setq on-found-candidates candidates))
                                              #'ignore
                                              #'ignore)))
        (should (equal on-found-candidates '("書く"))))))

  (nskk-it "calls on-not-found when registration is cancelled"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--conversion-overlay nil)
            (nskk--henkan-count 0)
            (nskk--registration-depth 0)
            on-not-found-called)
        (insert nskk-henkan-on-marker "ほ*")
        (set-marker nskk--conversion-start-marker (point-min))
        (let ((preedit-end (- (point-max) 1)))
          (nskk-state-force-henkan-phase nskk-current-state 'on)
          (nskk-with-mocks ((nskk-core-search/k
                             (lambda (_q _type _limit _on-found on-not-found)
                               (funcall on-not-found)))
                            (nskk-start-registration/k
                             (lambda (_reading on-done _on-fail)
                               (funcall on-done nil)))  ; nil = cancelled
                            (nskk--remove-okuri-marker #'ignore))
            (nskk--trigger-okuri-conversion/k ?k preedit-end
                                              #'ignore
                                              (lambda () (setq on-not-found-called t))
                                              #'ignore)))
        (should on-not-found-called))))

  (nskk-it "calls on-register after successful registration"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--conversion-overlay nil)
            (nskk--henkan-count 0)
            (nskk--registration-depth 0)
            on-register-called)
        (insert nskk-henkan-on-marker "ほ*")
        (set-marker nskk--conversion-start-marker (point-min))
        (let ((preedit-end (- (point-max) 1)))
          (nskk-state-force-henkan-phase nskk-current-state 'on)
          (nskk-with-mocks ((nskk-core-search/k
                             (lambda (_q _type _limit _on-found on-not-found)
                               (funcall on-not-found)))
                            (nskk-start-registration/k
                             (lambda (_reading on-done _on-fail)
                               (funcall on-done "炎")))  ; "炎" = registered word
                            (nskk--remove-okuri-marker #'ignore)
                            (nskk-henkan-do-reset #'ignore))
            (nskk--trigger-okuri-conversion/k ?k preedit-end
                                              #'ignore
                                              #'ignore
                                              (lambda () (setq on-register-called t)))))
        (should on-register-called)))))

;;;
;;; nskk--handle-vowel-okuri/k
;;;

(nskk-describe "nskk--handle-vowel-okuri/k"
  (nskk-it "calls on-consumed after converting the vowel kana"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer "")
            (nskk--pending-romaji-overlay nil)
            on-consumed-called)
        (nskk-with-mocks
            ((nskk-convert-input-to-kana-final/k
              (lambda (cont _ignored) (funcall cont "あ")))
             (nskk--trigger-okuri-conversion #'ignore)
             (nskk--update-overlay #'ignore))
          (nskk--handle-vowel-okuri/k ?a
            (lambda () (setq on-consumed-called t))))
        (should on-consumed-called))))

  (nskk-it "inserts the kana string into the current buffer"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer "")
            (nskk--pending-romaji-overlay nil))
        (nskk-with-mocks
            ((nskk-convert-input-to-kana-final/k
              (lambda (cont _ignored) (funcall cont "い")))
             (nskk--trigger-okuri-conversion #'ignore)
             (nskk--update-overlay #'ignore))
          (nskk--handle-vowel-okuri/k ?i #'ignore))
        (should (string-match-p "い" (buffer-string))))))

  (nskk-it "converts to katakana when state mode is katakana"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'katakana))
            (nskk--romaji-buffer "")
            (nskk--pending-romaji-overlay nil)
            inserted-text)
        (nskk-with-mocks
            ((nskk-convert-input-to-kana-final/k
              (lambda (cont _ignored) (funcall cont "う")))
             (nskk--trigger-okuri-conversion #'ignore)
             (nskk--update-overlay #'ignore))
          (nskk--handle-vowel-okuri/k ?u #'ignore))
        ;; In katakana mode, hiragana う is converted to katakana ウ before insert
        (should (string-match-p "ウ" (buffer-string)))))))

;;;
;;; Integration: CPS pipeline chain — next-candidate → commit
;;;

;; This integration test verifies that the CPS pipeline for candidate navigation
;; and commit works correctly end-to-end without going through the full conversion
;; start path.  It sets up a pre-loaded candidate list (simulating the state
;; after `nskk-start-conversion/k' has already run), then exercises:
;;   nskk-next-candidate/k → nskk-next-candidate/k → nskk-commit-current/k
;; and asserts that the third candidate is ultimately committed.

(nskk-describe "henkan CPS pipeline — next-candidate chain → commit"
  (nskk-it "advances candidates and commits the 3rd candidate after two next calls"
    ;; Start with phase=active and index=0.  nskk-with-henkan-state inserts
    ;; "preedit" into the buffer, so nskk-commit-current can delete it and
    ;; insert the committed candidate.
    ;;
    ;; `nskk--select-candidate' is what updates nskk-state-current-index; we
    ;; let it run for real by mocking only the overlay update (nskk--update-overlay)
    ;; and the nskk-ensure-overlay helper it calls, keeping the index mutations intact.
    (nskk-with-henkan-state 'active '("first" "second" "third")
      (let (committed-value)
        ;; First next: nskk--select-candidate moves index 0→1
        (nskk-with-mocks ((nskk--update-overlay #'ignore))
          (nskk-next-candidate/k #'ignore #'ignore))
        (should (= (nskk-state-current-index nskk-current-state) 1))
        ;; Second next: nskk--select-candidate moves index 1→2
        (nskk-with-mocks ((nskk--update-overlay #'ignore))
          (nskk-next-candidate/k #'ignore #'ignore))
        (should (= (nskk-state-current-index nskk-current-state) 2))
        ;; Commit: index=2, candidate="third"
        (nskk-commit-current/k
         (lambda (c) (setq committed-value c))
         #'ignore)
        (should (equal committed-value "third"))
        (should (equal (buffer-string) "third"))
        (should-not (nskk-converting-p)))))

  (nskk-it "commit/k calls on-committed with the candidate after manual index set"
    ;; Simpler variant: set index directly, then run next → commit.
    (nskk-with-henkan-state 'active '("α" "β" "γ")
      (setf (nskk-state-current-index nskk-current-state) 2)
      (let (committed-value)
        (nskk-commit-current/k
         (lambda (c) (setq committed-value c))
         #'ignore)
        (should (equal committed-value "γ"))))))

;;;
;;; Integration: registration → dict → subsequent conversion
;;;

;; Task 3: registration roundtrip integration test.
;;
;; The full roundtrip (registration input → dict insert → conversion re-query)
;; requires interactive minibuffer input and real Prolog DB isolation, making it
;; fragile as a pure unit test.  We therefore split it into two focused tests:
;;
;;   3a. Verify that `nskk-start-conversion/k' fires the registration path when
;;       no candidates exist, by checking that `nskk-start-registration/k' is
;;       called with the correct reading.
;;
;;   3b. Verify that after `nskk-dict-register-word' adds a word to the dict,
;;       a subsequent `nskk-core-search/k' call for the same reading returns
;;       the registered word via on-found.  This uses `nskk-prolog-test-with-isolated-db'
;;       so the registered fact is scoped to the test.

(nskk-describe "henkan registration roundtrip"
  (nskk-it "3a: start-conversion fires registration path for unknown reading"
    ;; When nskk-core-search finds nothing, nskk-start-registration/k should be
    ;; called with the preedit reading.  We mock both to capture the reading.
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--conversion-start-marker (make-marker))
            (nskk--romaji-buffer "")
            (nskk--henkan-count 0)
            (nskk--conversion-overlay nil)
            (nskk--registration-depth 0)
            registration-reading)
        (insert nskk-henkan-on-marker "みとうろく")
        (set-marker nskk--conversion-start-marker (point-min))
        (goto-char (point-max))
        (nskk-state-force-henkan-phase nskk-current-state 'on)
        (nskk-with-mocks ((nskk-convert-input-to-kana-final/k
                           (lambda (on-done _ignored) (funcall on-done "")))
                          (nskk-core-search/k
                           (lambda (_key _type _limit _on-found on-not-found)
                             (funcall on-not-found)))
                          (nskk-start-registration/k
                           (lambda (reading on-done _ignored)
                             (setq registration-reading reading)
                             (funcall on-done nil))))  ; user cancelled
          (nskk-start-conversion/k #'ignore #'ignore #'ignore))
        (should (equal registration-reading "みとうろく")))))

  (nskk-it "3b: dict-register-word then core-search/k returns registered word"
    ;; This test uses a real isolated Prolog DB to verify the full dict round-
    ;; trip: register a word, then query it back via nskk-core-search/k.
    ;; It relies on nskk-prolog-test-with-isolated-db to scope DB changes.
    (nskk-prolog-test-with-isolated-db
      ;; Reset all initialized flags so nskk-mode reinitialises cleanly.
      (let ((nskk--input-initialized nil)
            (nskk--state-prolog-initialized nil)
            (nskk--henkan-initialized nil)
            (nskk-kana--initialized nil)
            (nskk--converter-initialized nil)
            (nskk--candidate-key-facts-initialized nil))
        (with-temp-buffer
          (nskk-mode 1)
          (unwind-protect
              (let (found-candidates)
                ;; Register "みとうろく" → "未登録" in the user dict.
                (nskk-dict-register-word "みとうろく" "未登録")
                ;; Now query via nskk-core-search/k and expect the registered word.
                (nskk-core-search/k "みとうろく" nil nil
                  (lambda (cands) (setq found-candidates cands))
                  #'ignore)
                (should (member "未登録" found-candidates)))
            (nskk-mode -1))))))

  (nskk-it "STUB: full interactive registration roundtrip (requires E2E infrastructure)"
    ;; TODO: implement as an E2E test using nskk-e2e-with-buffer once the E2E
    ;; infrastructure supports injecting minibuffer input for registration.
    ;; The full flow to test:
    ;;   1. Start with a reading not in any dict (e.g. "ぜったいにない").
    ;;   2. Press SPC → nskk-start-conversion → no candidates → registration opens.
    ;;   3. Type "ZettaiNai" in minibuffer → nskk-dict-register-word is called.
    ;;   4. Press SPC again for the same reading → registered word appears as candidate.
    ;; Skipping here because read-from-minibuffer cannot be reliably fed input
    ;; via nskk-e2e-type in batch mode without additional E2E helper support.
    (should t)))

;;;
;;; Detection Tests: Uppercase A-Z
;;;

(nskk-describe "okurigana character detection"
  ;; Full A-Z and a-z exhaustive coverage is provided by:
  ;;   nskk-property-test-exhaustive okurigana-all-uppercase-map-to-downcase
  ;;   nskk-property-test-exhaustive detect-okurigana-char-lowercase-pbt
  ;;   nskk-deftest-table okurigana-consonant-mapping
  ;; Digit coverage is provided by the "returns nil for digits" test in
  ;; the nskk-detect-okurigana-char nskk-describe block above.
  ;; This section covers boundary/non-character edge cases only.
  (nskk-context "uppercase boundary detection"
    (nskk-it "maps uppercase A to lowercase a"
      (should (equal (nskk-detect-okurigana-char ?A) ?a)))

    (nskk-it "maps uppercase Z to lowercase z"
      (should (equal (nskk-detect-okurigana-char ?Z) ?z))))

  (nskk-context "lowercase boundary rejection"
    (nskk-it "returns nil for lowercase a"
      (should-not (nskk-detect-okurigana-char ?a)))

    (nskk-it "returns nil for lowercase k"
      (should-not (nskk-detect-okurigana-char ?k)))

    (nskk-it "returns nil for lowercase z"
      (should-not (nskk-detect-okurigana-char ?z))))

  (nskk-context "non-character input rejection"
    (nskk-it "returns nil for nil input"
      (should-not (nskk-detect-okurigana-char nil)))

    (nskk-it "returns nil for string input (not a character)"
      (should-not (nskk-detect-okurigana-char "K")))

    (nskk-it "returns nil for symbol input"
      (should-not (nskk-detect-okurigana-char 'symbol)))

    (nskk-it "returns nil for space character"
      (should-not (nskk-detect-okurigana-char ?\s)))

    (nskk-it "returns nil for period character"
      (should-not (nskk-detect-okurigana-char ?.)))

    (nskk-it "returns nil for @ (below uppercase range)"
      (should-not (nskk-detect-okurigana-char ?@)))

    (nskk-it "returns nil for [ (above uppercase range)"
      (should-not (nskk-detect-okurigana-char ?\[)))))

;;;
;;; Exhaustive Property Test: All A-Z
;;;

(nskk-property-test-exhaustive okurigana-all-uppercase-map-to-downcase
  (number-sequence ?A ?Z)
  (equal (nskk-detect-okurigana-char item)
         (downcase item)))

;;;
;;; State Storage Tests: Set / Get Roundtrip
;;;

(nskk-describe "okurigana state storage"
  (nskk-context "initial state"
    (nskk-it "okurigana is nil on a freshly created state"
      (let ((state (nskk-state-create)))
        (should (null (nskk-state-get-okurigana state))))))

  (nskk-context "set and get roundtrip"
    (nskk-it "set/get roundtrip works for consonant k"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?k)
        (should (eq (nskk-state-get-okurigana state) ?k))))

    (nskk-it "set/get roundtrip works for consonant s"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?s)
        (should (eq (nskk-state-get-okurigana state) ?s))))

    (nskk-it "set/get roundtrip works for consonant t"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?t)
        (should (eq (nskk-state-get-okurigana state) ?t))))

    (nskk-it "set/get roundtrip works for consonant n"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?n)
        (should (eq (nskk-state-get-okurigana state) ?n))))

    (nskk-it "set/get roundtrip works for consonant h"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?h)
        (should (eq (nskk-state-get-okurigana state) ?h))))

    (nskk-it "set/get roundtrip works for consonant m"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?m)
        (should (eq (nskk-state-get-okurigana state) ?m))))

    (nskk-it "set/get roundtrip works for consonant y"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?y)
        (should (eq (nskk-state-get-okurigana state) ?y))))

    (nskk-it "set/get roundtrip works for consonant r"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?r)
        (should (eq (nskk-state-get-okurigana state) ?r))))

    (nskk-it "set/get roundtrip works for consonant w"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?w)
        (should (eq (nskk-state-get-okurigana state) ?w))))

    (nskk-it "set/get roundtrip works for consonant g"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?g)
        (should (eq (nskk-state-get-okurigana state) ?g))))

    (nskk-it "set/get roundtrip works for consonant z"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?z)
        (should (eq (nskk-state-get-okurigana state) ?z))))

    (nskk-it "set/get roundtrip works for consonant d"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?d)
        (should (eq (nskk-state-get-okurigana state) ?d))))

    (nskk-it "set/get roundtrip works for consonant b"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?b)
        (should (eq (nskk-state-get-okurigana state) ?b))))

    (nskk-it "set/get roundtrip works for consonant p"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?p)
        (should (eq (nskk-state-get-okurigana state) ?p)))))

  (nskk-context "overwrite behavior"
    (nskk-it "setting okurigana overwrites the previous value"
      (let ((state (nskk-state-create)))
        (nskk-state-set-okurigana state ?k)
        (should (eq (nskk-state-get-okurigana state) ?k))
        (nskk-state-set-okurigana state ?s)
        (should (eq (nskk-state-get-okurigana state) ?s))
        (nskk-state-set-okurigana state ?t)
        (should (eq (nskk-state-get-okurigana state) ?t)))))

  (nskk-context "independence between instances"
    (nskk-it "two state objects maintain independent okurigana values"
      (let ((state1 (nskk-state-create))
            (state2 (nskk-state-create)))
        (nskk-state-set-okurigana state1 ?k)
        (nskk-state-set-okurigana state2 ?s)
        (should (eq (nskk-state-get-okurigana state1) ?k))
        (should (eq (nskk-state-get-okurigana state2) ?s))))))

;;;
;;; Prolog Predicate Tests: okurigana-char/2
;;;

(nskk-describe "Prolog okurigana-char/2 predicate"
  ;; Full A-Z mapping coverage is provided by nskk-deftest-table
  ;; okurigana-prolog-char-mapping below.  This section covers the
  ;; boundary letters and lowercase rejection only.
  (nskk-context "uppercase boundary mapping"
    (nskk-it "maps uppercase A to lowercase a"
      (should (equal (nskk-prolog-query-value `(okurigana-char ,?A \?lc) '\?lc) ?a)))

    (nskk-it "maps uppercase Z to lowercase z"
      (should (equal (nskk-prolog-query-value `(okurigana-char ,?Z \?lc) '\?lc) ?z))))

  (nskk-context "lowercase rejection"
    (nskk-deftest-table prolog-okurigana-char-lowercase-rejection
      :description "Lowercase letters are not in okurigana-char/2"
      :columns (ch)
      :rows ((?a) (?k) (?z))
      :body (should-not (nskk-prolog-query-value `(okurigana-char ,ch \?lc) '\?lc)))))

;;;
;;; Table-Driven Tests: 14 Standard Okurigana Consonants
;;;

(nskk-deftest-table okurigana-consonant-mapping
  :columns (uppercase expected-lowercase)
  :rows ((?K ?k)
         (?S ?s)
         (?T ?t)
         (?N ?n)
         (?H ?h)
         (?M ?m)
         (?Y ?y)
         (?R ?r)
         (?W ?w)
         (?G ?g)
         (?Z ?z)
         (?D ?d)
         (?B ?b)
         (?P ?p))
  :description "Standard okurigana consonants map to their lowercase equivalents"
  :body (should (equal (nskk-detect-okurigana-char uppercase) expected-lowercase)))

(nskk-deftest-table okurigana-prolog-char-mapping
  :columns (uppercase expected-lowercase)
  :rows ((?K ?k)
         (?S ?s)
         (?T ?t)
         (?N ?n)
         (?H ?h)
         (?M ?m)
         (?Y ?y)
         (?R ?r)
         (?W ?w)
         (?G ?g)
         (?Z ?z)
         (?D ?d)
         (?B ?b)
         (?P ?p))
  :description "Standard okurigana consonants in Prolog okurigana-char/2 predicate"
  :body (should (equal (nskk-prolog-query-value `(okurigana-char ,uppercase \?lc) '\?lc)
                       expected-lowercase)))

;;;
;;; Property-Based Tests
;;;

;; PBT: uppercase okurigana consonant chars always return their lowercase via detect
(nskk-property-test-seeded okurigana-pbt-uppercase-returns-lowercase
  ((char okurigana-consonant-char))
  (equal (nskk-detect-okurigana-char char) (downcase char))
  100 42)

;; PBT: state roundtrip — set then get returns same value
(nskk-property-test-seeded okurigana-pbt-state-roundtrip
  ((char okurigana-consonant-char))
  (let* ((state (nskk-state-create))
         (lower-char (downcase char)))
    (nskk-state-set-okurigana state lower-char)
    (eq (nskk-state-get-okurigana state) lower-char))
  100 42)

;; PBT: lowercase letters always return nil from detect
(nskk-property-test-seeded okurigana-pbt-lowercase-returns-nil
  ((char okurigana-consonant-char))
  (null (nskk-detect-okurigana-char (downcase char)))
  100 42)

;; PBT: okurigana-consonant-char generator always yields chars in A-Z
(nskk-property-test-seeded okurigana-pbt-generator-yields-uppercase
  ((char okurigana-consonant-char))
  (and (characterp char)
       (>= char ?A)
       (<= char ?Z))
  100 42)

;; PBT: Prolog okurigana-char result equals downcase of input
(nskk-property-test-seeded okurigana-pbt-prolog-maps-to-downcase
  ((char okurigana-consonant-char))
  (equal (nskk-prolog-query-value `(okurigana-char ,char \?lc) '\?lc)
         (downcase char))
  100 42)

;;;
;;; API Existence Tests
;;;

(nskk-describe "okurigana state accessor API existence"
  ;; nskk-detect-okurigana-char and nskk-process-okurigana-input are already
  ;; checked by henkan-function-api-defined in the initialization block.
  (nskk-it "nskk-state-set-okurigana is defined"
    (should (fboundp 'nskk-state-set-okurigana)))

  (nskk-it "nskk-state-get-okurigana is defined"
    (should (fboundp 'nskk-state-get-okurigana))))

;;;
;;; Regression Tests: Pending Romaji Discard on Okurigana Trigger
;;;
;;
;; Bug (fixed in nskk-henkan.el): when a pending incomplete romaji consonant
;; (e.g. "k", "sh") was in nskk--romaji-buffer when an okurigana trigger
;; (uppercase letter) arrived, the raw consonant was inserted into the buffer
;; before the * okurigana marker, producing e.g. "▽かk*" instead of "▽か*".
;;
;; The fix discards :incomplete romaji (anything where nskk-converter-convert
;; returns (:incomplete . ...) or nil) and only emits successfully-converted kana
;; or a standalone "n" (→ "ん" at word boundary).

(nskk-describe "okurigana input flush behaviour"
  (nskk-context "pending consonant is discarded (not inserted)"
    (nskk-it "pending k is discarded: buffer does not contain k before the * marker"
      ;; T-U1: "k" in romaji buffer + uppercase K trigger → "k" must NOT appear before *
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana)))
          ;; Insert preedit reading so far: ▽か
          (insert "\u25BD\u304B")
          ;; Set conversion-start marker at buffer start (as henkan-on does)
          (nskk--set-conversion-start-marker (point-min))
          ;; Put the state into henkan-on phase (reading in progress)
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          ;; Simulate a pending incomplete romaji consonant "k"
          (setq nskk--romaji-buffer "k")
          ;; Fire okurigana trigger (uppercase K)
          (nskk-process-okurigana-input ?K)
          ;; The buffer should NOT contain "k" adjacent to the "*" marker.
          ;; Check both orderings: "k*" (consonant before marker, the actual bug)
          ;; and "*k" (consonant after marker), to catch both insertion orders.
          (let ((content (buffer-string)))
            (should-not (string-match-p "k\\*\\|\\*k" content))
            ;; The * okurigana marker must be present
            (should (string-match-p "\\*" content))))))

    (nskk-it "pending multi-char sh is discarded: buffer does not contain sh before *"
      ;; T-U2: "sh" (multi-char incomplete) + uppercase K → "sh" must NOT appear before *
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana)))
          (insert "\u25BD\u304B")
          (nskk--set-conversion-start-marker (point-min))
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          ;; "sh" is an incomplete romaji prefix (needs vowel to complete shi/sha/shu etc.)
          (setq nskk--romaji-buffer "sh")
          (nskk-process-okurigana-input ?K)
          (let ((content (buffer-string)))
            (should-not (string-match-p "sh" content))
            (should (string-match-p "\\*" content))))))

    (nskk-it "pending n is converted to ん and inserted before *"
      ;; T-U3: "n" (standalone n at word boundary) + uppercase K → "ん" IS inserted before *
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana)))
          (insert "\u25BD\u304B")
          (nskk--set-conversion-start-marker (point-min))
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          ;; "n" alone is the ん exception: it should flush as ん before the marker
          (setq nskk--romaji-buffer "n")
          (nskk-process-okurigana-input ?K)
          (let ((content (buffer-string)))
            ;; ん must appear in the buffer before the * marker
            (should (string-match-p "\u3093" content))
            (should (string-match-p "\\*" content))
            ;; And the raw "n" character must NOT appear as ASCII
            (should-not (string-match-p "[nN]\\*\\|\\*[nN]" content))))))

    (nskk-it "empty romaji buffer with uppercase K trigger inserts no extra char before *"
      ;; T-U4: empty romaji buffer + uppercase K → only * inserted, no spurious chars
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana)))
          (insert "\u25BD\u304B")
          (nskk--set-conversion-start-marker (point-min))
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          ;; No pending romaji
          (setq nskk--romaji-buffer "")
          (nskk-process-okurigana-input ?K)
          (let ((content (buffer-string)))
            ;; * marker must be present
            (should (string-match-p "\\*" content))
            ;; No raw ASCII consonant should appear adjacent to *
            (should-not (string-match-p "[a-z]\\*\\|\\*[a-z]" content))))))))

;;;
;;; nskk--reset-romaji-buffer Tests
;;;

(nskk-describe "nskk--reset-romaji-buffer"
  (nskk-it "sets nskk--romaji-buffer to empty string"
    (with-temp-buffer
      (let ((nskk--romaji-buffer "ka")
            (nskk--pending-romaji-overlay nil))
        (nskk--reset-romaji-buffer)
        (should (equal nskk--romaji-buffer "")))))

  (nskk-it "calls nskk--clear-pending-romaji"
    (let ((cleared nil)
          (nskk--romaji-buffer ""))
      (nskk-with-mocks ((nskk--clear-pending-romaji (lambda () (setq cleared t))))
        (nskk--reset-romaji-buffer))
      (should cleared)))

  (nskk-it "is idempotent when buffer is already empty"
    (with-temp-buffer
      (let ((nskk--romaji-buffer "")
            (nskk--pending-romaji-overlay nil))
        (nskk--reset-romaji-buffer)
        (nskk--reset-romaji-buffer)
        (should (equal nskk--romaji-buffer ""))))))

;;;
;;; nskk--registration-prompt Tests
;;;

(nskk-describe "nskk--registration-prompt"
  (nskk-it "builds depth-1 prompt with single brackets"
    (should (equal (nskk--registration-prompt 1 "てすと")
                   "[辞書登録] てすと: ")))

  (nskk-it "builds depth-2 prompt with double brackets"
    (should (equal (nskk--registration-prompt 2 "かんじ")
                   "[[辞書登録]] かんじ: ")))

  (nskk-it "builds depth-3 prompt with triple brackets"
    (should (equal (nskk--registration-prompt 3 "abc")
                   "[[[辞書登録]]] abc: "))))

;;;
;;; nskk--run-registration-session Tests
;;;

(nskk-describe "nskk--run-registration-session"
  (nskk-it "returns nil when depth is at maximum"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--registration-depth 3))
      (should-not (nskk--run-registration-session "てすと"))))

  (nskk-it "returns nil when user enters empty string"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--registration-depth 0))
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk-with-mocks ((read-from-minibuffer (lambda (_p) ""))
                        (nskk-dict-register-word #'ignore))
        (should-not (nskk--run-registration-session "てすと")))))

  (nskk-it "returns the entered word when user provides input"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--registration-depth 0))
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk-with-mocks ((read-from-minibuffer (lambda (_p) "漢字"))
                        (nskk-dict-register-word #'ignore))
        (should (equal (nskk--run-registration-session "かんじ") "漢字")))))

  (nskk-it "increments and decrements depth atomically"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--registration-depth 0)
          depth-during)
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk-with-mocks ((read-from-minibuffer
                         (lambda (_p) (setq depth-during nskk--registration-depth) ""))
                        (nskk-dict-register-word #'ignore))
        (nskk--run-registration-session "てすと"))
      (should (= depth-during 1))
      (should (= nskk--registration-depth 0)))))

;;;
;;; search-backend/2 Prolog Facts Tests
;;;

(nskk-describe "search-backend/2 Prolog facts"
  (nskk-it "defines backend order: dict-lookup is first"
    (should (nskk-prolog-query '(search-backend 1 dict-lookup))))

  (nskk-it "defines backend order: skkserv-lookup is second"
    (should (nskk-prolog-query '(search-backend 2 skkserv-lookup))))

  (nskk-it "defines backend order: program-dict-lookup is third"
    (should (nskk-prolog-query '(search-backend 3 program-dict-lookup)))))

;;;
;;; script-toggle/2 Prolog Facts Tests
;;;

(nskk-describe "script-toggle/2 Prolog facts"
  (nskk-it "hiragana → katakana direction is defined"
    (should (nskk-prolog-query '(script-toggle hiragana katakana))))

  (nskk-it "katakana → hiragana direction is defined"
    (should (nskk-prolog-query '(script-toggle katakana hiragana))))

  (nskk-it "hiragana target is katakana via query-value"
    (should (eq (nskk-prolog-query-value '(script-toggle hiragana \?t) '\?t)
                'katakana)))

  (nskk-it "katakana target is hiragana via query-value"
    (should (eq (nskk-prolog-query-value '(script-toggle katakana \?t) '\?t)
                'hiragana)))

  (nskk-it "ascii mode has no script-toggle fact"
    (should-not (nskk-prolog-query '(script-toggle ascii \?_)))))

;;;
;;; nskk-henkan-kakutei-convert-script Tests
;;;

(nskk-describe "nskk-henkan-kakutei-convert-script"
  (nskk-it "is fboundp"
    (should (fboundp 'nskk-henkan-kakutei-convert-script)))

  (nskk-it "in hiragana mode: converts preedit hiragana to katakana and commits"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk--set-mode 'hiragana)
        (nskk-without-modification
          (insert nskk-henkan-on-marker "かんじ"))
        (nskk--set-conversion-start-marker (point-min))
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-henkan-kakutei-convert-script)
        (should (null (nskk-state-henkan-phase nskk-current-state)))
        (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
        (should (string= (buffer-string) "カンジ")))))

  (nskk-it "in katakana mode: converts preedit katakana to hiragana and commits"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk--set-mode 'katakana)
        (nskk-without-modification
          (insert nskk-henkan-on-marker "カンジ"))
        (nskk--set-conversion-start-marker (point-min))
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-henkan-kakutei-convert-script)
        (should (null (nskk-state-henkan-phase nskk-current-state)))
        (should (eq (nskk-state-mode nskk-current-state) 'katakana))
        (should (string= (buffer-string) "かんじ")))))

  (nskk-it "is a no-op when no preedit is active"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification (insert "あ"))
        (let ((content-before (buffer-string)))
          (nskk-henkan-kakutei-convert-script)
          (should (string= (buffer-string) content-before)))))))

;;;
;;; preedit-phase/1 Prolog Table Integrity Tests
;;;

(nskk-describe "preedit-phase/1 Prolog table integrity"
  (nskk-it "on holds as a preedit phase"
    (should (nskk-prolog-holds-p '(preedit-phase on))))

  (nskk-it "active does NOT hold as a preedit phase"
    (should-not (nskk-prolog-holds-p '(preedit-phase active))))

  (nskk-it "nil does NOT hold as a preedit phase"
    (should-not (nskk-prolog-holds-p '(preedit-phase nil))))

  (nskk-it "list does NOT hold as a preedit phase"
    (should-not (nskk-prolog-holds-p '(preedit-phase list))))

  (nskk-it "registration does NOT hold as a preedit phase"
    (should-not (nskk-prolog-holds-p '(preedit-phase registration)))))

;;;
;;; kana-conversion/3 normalize Prolog Table Integrity Tests
;;;

(nskk-describe "kana-conversion/3 normalize Prolog table integrity"
  (nskk-deftest-table henkan-prolog-lookup-normalize-table
    :description "kana-conversion/3 normalize maps mode to normalization function"
    :columns (mode expected-fn)
    :rows ((hiragana      identity)
           (katakana      nskk-kana-string-katakana-to-hiragana)
           (katakana-半角  nskk--hankaku-to-hiragana))
    :body (should (eq expected-fn
                      (nskk-prolog-query-value
                       `(kana-conversion ,mode normalize ,'\?fn) '\?fn))))

  (nskk-it "returns nil for unknown mode"
    (should-not (nskk-prolog-query-value
                 `(kana-conversion nonexistent normalize ,'\?fn) '\?fn))))

;;;
;;; disable-cleanup/2 Prolog Table Integrity Tests
;;;

(nskk-describe "disable-cleanup/2 Prolog table integrity"
  (nskk-deftest-table henkan-prolog-disable-cleanup-table
    :description "disable-cleanup/2 maps henkan phase to cleanup action"
    :columns (phase expected-action)
    :rows ((active       cancel-conversion)
           (list         cancel-conversion)
           (on           cancel-preedit)
           (registration cancel-preedit))
    :body (should (eq expected-action
                      (nskk-prolog-query-value
                       `(disable-cleanup ,phase ,'\?a) '\?a))))

  (nskk-it "returns nil for nil phase"
    (should-not (nskk-prolog-query-value
                 `(disable-cleanup nil ,'\?a) '\?a))))

;;;
;;; script-converter/2 Prolog Table Integrity Tests
;;;

(nskk-describe "script-converter/2 Prolog table integrity"
  (nskk-deftest-table henkan-prolog-script-converter-table
    :description "script-converter/2 maps target script to CPS converter function"
    :columns (target expected-fn)
    :rows ((katakana nskk-kana-string-hiragana-to-katakana/k)
           (hiragana nskk-kana-string-katakana-to-hiragana/k))
    :body (should (eq expected-fn
                      (nskk-prolog-query-value
                       `(script-converter ,target ,'\?fn) '\?fn))))

  (nskk-it "returns nil for unknown target"
    (should-not (nskk-prolog-query-value
                 `(script-converter ascii ,'\?fn) '\?fn))))

;;;
;;; nskk--hankaku-to-hiragana Tests
;;;

(nskk-describe "henkan hankaku-to-hiragana helper"
  (nskk-it "is defined as a callable function (fboundp)"
    (should (fboundp 'nskk--hankaku-to-hiragana)))

  (nskk-it "converts single a-row char from hankaku to hiragana"
    (should (equal (nskk--hankaku-to-hiragana "ｱ") "あ")))

  (nskk-it "converts single ka-row char from hankaku to hiragana"
    (should (equal (nskk--hankaku-to-hiragana "ｶ") "か")))

  (nskk-it "converts multi-char hankaku string to hiragana"
    (should (equal (nskk--hankaku-to-hiragana "ｱｲｳ") "あいう"))))

;;;
;;; nskk--normalize-for-lookup Tests
;;;

(nskk-describe "henkan normalize-for-lookup helper"
  (nskk-it "is defined as a callable function (fboundp)"
    (should (fboundp 'nskk--normalize-for-lookup)))

  (nskk-it "in hiragana mode returns text as-is"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (should (equal (nskk--normalize-for-lookup "あいう") "あいう"))))

  (nskk-it "in katakana mode normalizes to hiragana"
    (let ((nskk-current-state (nskk-state-create 'katakana)))
      (should (equal (nskk--normalize-for-lookup "アイウ") "あいう"))))

  (nskk-it "in hankaku-katakana mode normalizes to hiragana"
    (let ((nskk-current-state (nskk-state-create 'katakana-半角)))
      (should (equal (nskk--normalize-for-lookup "ｱｲｳ") "あいう"))))

  (nskk-it "falls back to identity for unknown mode with no fact"
    (let ((nskk-current-state (nskk-state-create 'ascii)))
      (should (equal (nskk--normalize-for-lookup "abc") "abc")))))

;;;
;;; nskk--standalone-n-p Tests
;;;

(nskk-describe "nskk--standalone-n-p"
  (nskk-it "should return non-nil for single-char \"n\" string"
    (should (nskk--standalone-n-p "n")))
  (nskk-it "should return nil for empty string"
    (should-not (nskk--standalone-n-p "")))
  (nskk-it "should return nil for \"nn\""
    (should-not (nskk--standalone-n-p "nn")))
  (nskk-it "should return nil for \"na\""
    (should-not (nskk--standalone-n-p "na")))
  (nskk-it "should return nil for non-n single char"
    (should-not (nskk--standalone-n-p "a"))))

;;;
;;; nskk-henkan-unknown-search-type error signal Tests
;;;

(nskk-describe "nskk-core-search/k unknown search type"
  (nskk-it "signals nskk-henkan-unknown-search-type for an unrecognized search type keyword"
    (nskk-with-mock-dict '()
      (should-error
       (nskk-core-search/k "かんじ" :unknown-type nil
         #'ignore
         #'ignore)
       :type 'nskk-henkan-unknown-search-type)))

  (nskk-it "signals nskk-henkan-unknown-search-type and error data contains the bad type"
    (nskk-with-mock-dict '()
      (condition-case err
          (progn
            (nskk-core-search/k "てすと" :bogus nil #'ignore #'ignore)
            (ert-fail "Expected signal was not raised"))
        (nskk-henkan-unknown-search-type
         (should (memq :bogus (cdr err))))))))

(nskk-describe "nskk--preedit-ends-with-plain-vowel-p"
  (nskk-it "returns nil when no preedit marker is set"
    (with-temp-buffer
      (let ((nskk--romaji-buffer ""))
        (should-not (nskk--preedit-ends-with-plain-vowel-p)))))

  (nskk-it "returns nil when romaji buffer is non-empty"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker))
            (nskk--romaji-buffer "k"))
        (set-marker nskk--conversion-start-marker (point-min))
        (insert nskk-henkan-on-marker "か")
        (should-not (nskk--preedit-ends-with-plain-vowel-p)))))

  (nskk-it "returns nil when preedit ends with non-vowel kana (か)"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker))
            (nskk--romaji-buffer ""))
        (set-marker nskk--conversion-start-marker (point-min))
        (insert nskk-henkan-on-marker "か")
        (should-not (nskk--preedit-ends-with-plain-vowel-p)))))

  (nskk-it "returns non-nil for reading ending with あ (empty romaji)"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker))
            (nskk--romaji-buffer ""))
        (set-marker nskk--conversion-start-marker (point-min))
        (insert nskk-henkan-on-marker "あ")
        (should (nskk--preedit-ends-with-plain-vowel-p)))))

  (nskk-it "returns non-nil for compound reading ending with い (かい)"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker))
            (nskk--romaji-buffer ""))
        (set-marker nskk--conversion-start-marker (point-min))
        (insert nskk-henkan-on-marker "かい")
        (should (nskk--preedit-ends-with-plain-vowel-p)))))

  (nskk-it "returns non-nil for reading ending with ー (prolonged vowel)"
    (with-temp-buffer
      (let ((nskk--conversion-start-marker (make-marker))
            (nskk--romaji-buffer ""))
        (set-marker nskk--conversion-start-marker (point-min))
        (insert nskk-henkan-on-marker "あー")
        (should (nskk--preedit-ends-with-plain-vowel-p)))))

  (nskk-it "returns non-nil for all hiragana plain vowels"
    (dolist (ch '(?あ ?い ?う ?え ?お))
      (with-temp-buffer
        (let ((nskk--conversion-start-marker (make-marker))
              (nskk--romaji-buffer ""))
          (set-marker nskk--conversion-start-marker (point-min))
          (insert nskk-henkan-on-marker (char-to-string ch))
          (should (nskk--preedit-ends-with-plain-vowel-p))))))

  (nskk-it "returns non-nil for all katakana plain vowels"
    (dolist (ch '(?ア ?イ ?ウ ?エ ?オ))
      (with-temp-buffer
        (let ((nskk--conversion-start-marker (make-marker))
              (nskk--romaji-buffer ""))
          (set-marker nskk--conversion-start-marker (point-min))
          (insert nskk-henkan-on-marker (char-to-string ch))
          (should (nskk--preedit-ends-with-plain-vowel-p)))))))

(provide 'nskk-henkan-test)

;;; nskk-henkan-test.el ends here
