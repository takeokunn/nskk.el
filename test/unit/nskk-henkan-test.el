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

(nskk-describe "stable candidate merging"
  (nskk-it "preserves order, first identity, properties, and input lists"
    (let* ((first (propertize "same" 'source 'primary))
           (duplicate (propertize "same" 'source 'secondary))
           (primary (list first "primary-only"))
           (secondary (list duplicate "secondary-only"))
           (primary-tail (cdr primary))
           (secondary-tail (cdr secondary))
           (primary-before (copy-sequence primary))
           (secondary-before (copy-sequence secondary))
           (result
            (nskk--merge-candidates-user-first primary secondary)))
      (should (equal (mapcar #'substring-no-properties result)
                     '("same" "primary-only" "secondary-only")))
      (should (eq (car result) first))
      (should (eq (get-text-property 0 'source (car result)) 'primary))
      (should (equal primary primary-before))
      (should (equal secondary secondary-before))
      (should (eq (cdr primary) primary-tail))
      (should (eq (cdr secondary) secondary-tail))))

  (nskk-it "uses one hash probe per input candidate at 5000 candidates"
    (let* ((primary
            (cl-loop for index below 2500
                     collect (format "candidate-%d" index)))
           (secondary (mapcar #'copy-sequence primary))
           (real-gethash (symbol-function 'gethash))
           (real-puthash (symbol-function 'puthash))
           (probes 0)
           (inserts 0)
           result)
      (cl-letf (((symbol-function 'gethash)
                 (lambda (key table &optional default)
                   (setq probes (1+ probes))
                   (funcall real-gethash key table default)))
                ((symbol-function 'puthash)
                 (lambda (key value table)
                   (setq inserts (1+ inserts))
                   (funcall real-puthash key value table))))
        (setq result
              (nskk--merge-candidates-user-first primary secondary)))
      (should (= probes 5000))
      (should (= inserts 2500))
      (should (= (length result) 2500))
      (should (equal result primary)))))
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
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (executed nil))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker nil)
            
        (nskk-henkan-with-preedit _start
          (setq executed t))
        (should-not executed))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "executes body when point is at the marker (empty preedit text)"
    ;; FR-005: >= guard allows conversion with empty preedit (e.g. SPC immediately
    ;; after uppercase letter before any kana is typed).  Previously > silently
    ;; skipped the body; now it executes, opening registration as expected.
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        ;; Insert only the marker, point is AT the end of marker (no text after)
        (insert nskk-henkan-on-marker)
        (let ((executed nil))
          (nskk-henkan-with-preedit _start
            (setq executed t))
          (should executed)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "executes body and binds start when preedit text exists"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (captured-start nil))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert nskk-henkan-on-marker "かな")
        (nskk-henkan-with-preedit start
          (setq captured-start start))
        (should (equal captured-start (point-min))))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      ))))

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
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            captured-candidates
            captured-index)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert "test")
        (nskk-state-set-candidates nskk-current-state '("候補1" "候補2"))
        (setf (nskk-state-current-index nskk-current-state) 1)
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-conversion-context (candidates index)
          (setq captured-candidates candidates)
          (setq captured-index index))
        (should (equal captured-candidates '("候補1" "候補2")))
        (should (equal captured-index 1)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      ))))

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
          (nskk-test-saved-registration-depth (nskk-state-registration-depth))
          prompt-shown)
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth depth)
            
      (if should-proceed
          (progn
            (nskk-state-force-henkan-phase nskk-current-state 'on)
            (nskk-with-mocks ((read-from-minibuffer
                               (lambda (p) (setq prompt-shown p) ""))
                              (nskk-dict-register-word #'ignore))
              (nskk-start-registration "てすと")
              (should prompt-shown)))
        (should-not (nskk-start-registration "てすと"))))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))

  (nskk-it "respects non-default max-registration-depth"
    (let ((nskk-max-registration-depth 2)
          (nskk-current-state (nskk-state-create 'hiragana))
          (nskk-test-saved-registration-depth (nskk-state-registration-depth))
          prompt-shown)
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 1)
            
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk-with-mocks ((read-from-minibuffer (lambda (p) (setq prompt-shown p) ""))
                        (nskk-dict-register-word #'ignore))
        (nskk-start-registration "てすと")
        (should prompt-shown))
      (let ((nskk-test-saved-registration-depth (nskk-state-registration-depth)))
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 2)
            
        (should-not (nskk-start-registration "てすと")))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      )))

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
            (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 0)
            
        (nskk-next-candidate)
        (should (equal (nskk-state-henkan-count) 0)))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))

    (nskk-it "calls nskk--select-candidate when count is below threshold"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count))
              (nskk-henkan-show-candidates-nth 5)
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              select-called)
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 0)
            (nskk-state-set-conversion-start-marker (make-marker))
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert "test")
          (nskk-state-set-candidates nskk-current-state '("a" "b" "c"))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-with-mocks ((nskk--select-candidate (lambda (dir) (setq select-called dir))))
            (nskk-next-candidate)
            (should (eq select-called 'next))
            (should (equal (nskk-state-henkan-count) 1))))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

    (nskk-it "switches to list display at the threshold"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count))  ;; one below threshold of 5
              (nskk-henkan-show-candidates-nth 5)
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              list-next-called)
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 4)
            (nskk-state-set-conversion-start-marker (make-marker))
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert "test")
          (nskk-state-set-candidates nskk-current-state '("a" "b" "c"))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-with-mocks ((nskk--show-candidate-list-next (lambda () (setq list-next-called t))))
            (nskk-next-candidate)
            (should list-next-called)
            (should (equal (nskk-state-henkan-count) 5))))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      ))))

  (nskk-context "nskk-previous-candidate"
    (nskk-it "does nothing when not converting"
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 3)
            
        (nskk-previous-candidate)
        (should (equal (nskk-state-henkan-count) 3)))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))

    (nskk-it "calls nskk--select-candidate when list is inactive"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count))
              (nskk--henkan-candidate-list-active nil)
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              select-called)
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 3)
            (nskk-state-set-conversion-start-marker (make-marker))
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert "test")
          (nskk-state-set-candidates nskk-current-state '("a" "b" "c"))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-with-mocks ((nskk--select-candidate (lambda (dir) (setq select-called dir))))
            (nskk-previous-candidate)
            (should (eq select-called 'previous))
            (should (equal (nskk-state-henkan-count) 2))))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

    (nskk-it "calls show-list-prev when list is active"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count))
              (nskk--henkan-candidate-list-active t)
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              list-prev-called)
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 5)
            (nskk-state-set-conversion-start-marker (make-marker))
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert "test")
          (nskk-state-set-candidates nskk-current-state '("a" "b" "c"))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-with-mocks ((nskk--show-candidate-list-prev (lambda () (setq list-prev-called t))))
            (nskk-previous-candidate)
            (should list-prev-called)))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

    (nskk-it "does not decrement count below 0"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count))
              (nskk--henkan-candidate-list-active nil)
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 0)
            (nskk-state-set-conversion-start-marker (make-marker))
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert "test")
          (nskk-state-set-candidates nskk-current-state '("a"))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-with-mocks ((nskk--select-candidate #'ignore))
            (nskk-previous-candidate)
            (should (equal (nskk-state-henkan-count) 0))))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))))

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
        (should (equal (nskk-state-romaji-buffer) ""))))

    (nskk-it "resets henkan-count to 0 after commit"
      (nskk-with-henkan-state 'active '("変換" "変換2")
        (setf (nskk-state-current-index nskk-current-state) 0)
        (nskk-commit-current)
        (should (equal (nskk-state-henkan-count) 0)))))

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
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              start-conversion-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker nil)
            
          (nskk-with-mocks ((nskk-start-conversion (lambda () (setq start-conversion-called t))))
            (nskk-convert)
            (should-not start-conversion-called)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

    (nskk-it "calls nskk-start-conversion when preedit text exists"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              start-conversion-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert nskk-henkan-on-marker "かな")
          (nskk-with-mocks ((nskk-start-conversion (lambda () (setq start-conversion-called t))))
            (nskk-convert)
            (should start-conversion-called)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      ))))

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
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              start-conversion-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert nskk-henkan-on-marker "かな")
          (nskk-with-mocks ((nskk-start-conversion (lambda () (setq start-conversion-called t))))
            (nskk-convert-or-commit)
            (should start-conversion-called)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      ))))

  (nskk-context "nskk-start-conversion direct behavior"
    (nskk-it "sets candidates and active phase when search returns results"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count))
              (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 0)
            (nskk-state-set-conversion-overlay nil)
            
          (insert "▽かんじ")
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (goto-char (point-max))
          (nskk-state-force-henkan-phase nskk-current-state 'on)
          (nskk-with-mocks ((nskk-core-search/k
                             (lambda (_key _type _limit on-found _on-not-found)
                               (funcall on-found '("漢字" "感じ"))))
                            (nskk--update-overlay #'ignore)
                            (nskk--replace-marker-at #'ignore))
            (nskk-start-conversion)
            (should (equal (nskk-state-candidates nskk-current-state) '("漢字" "感じ")))
            (should (eq (nskk-state-henkan-phase nskk-current-state) 'active))))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay))
      )))

    (nskk-it "calls nskk-start-registration when no candidates found"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count))
              (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
              (nskk-test-saved-registration-depth (nskk-state-registration-depth))
              registration-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 0)
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-registration-depth 0)
            
          (insert "▽てすと")
          (set-marker (nskk-state-conversion-start-marker) (point-min))
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
            (should registration-called)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      )))))

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
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              rollback-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert "test")
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-with-mocks ((nskk-rollback-conversion (lambda () (setq rollback-called t))))
            (nskk-cancel-conversion)
            (should rollback-called)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      ))))

  (nskk-context "nskk-cancel-preedit"
    (nskk-it "clears preedit text from buffer"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "ka")
            (nskk-state-set-henkan-count 3)
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert nskk-henkan-on-marker "か")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-cancel-preedit)
          (should (equal (buffer-string) "")))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

    (nskk-it "clears romaji buffer"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "ka")
            (nskk-state-set-henkan-count 3)
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert nskk-henkan-on-marker "か")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-cancel-preedit)
          (should (equal (nskk-state-romaji-buffer) "")))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

    (nskk-it "resets henkan-count to 0"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "ka")
            (nskk-state-set-henkan-count 3)
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert nskk-henkan-on-marker "か")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-cancel-preedit)
          (should (equal (nskk-state-henkan-count) 0)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

    (nskk-it "resets henkan-phase to nil"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "ka")
            (nskk-state-set-henkan-count 3)
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert nskk-henkan-on-marker "か")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-cancel-preedit)
          (should-not (nskk-state-henkan-phase nskk-current-state)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

    (nskk-it "clears okurigana state from state struct on cancel"
      (nskk-prolog-test-with-isolated-db
        (with-temp-buffer
          (nskk-mode 1)
          (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
                (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
                (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "k")
            (nskk-state-set-henkan-count 0)
            
            (set-marker (nskk-state-conversion-start-marker) (point-min))
            (insert nskk-henkan-on-marker "か")
            (nskk-state-set-henkan-phase nskk-current-state 'on)
            (nskk-state-set-okurigana nskk-current-state "k")
            (nskk-cancel-preedit)
            (should-not (nskk-state-get-okurigana nskk-current-state)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))))

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
            (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
                  (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
                  (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 0)
            
              (set-marker (nskk-state-conversion-start-marker) (point-min))
              (insert nskk-henkan-on-marker "か")
              (nskk-state-set-henkan-phase nskk-current-state 'on)
              (set (car spec) (cdr spec))
              (nskk-cancel-preedit)
              (should-not (symbol-value (car spec))))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))))))

  (nskk-context "nskk-rollback-conversion"
    (nskk-it "resets count and restores preedit phase"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count))
              (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
              (nskk--henkan-candidate-list-active nil))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 3)
            (nskk-state-set-conversion-overlay nil)
            
          (insert "▼漢字")
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (goto-char (point-max))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-state-set-candidates nskk-current-state '("漢字" "感じ"))
          (nskk-with-mocks ((nskk--delete-marker-at #'ignore)
                            (run-hook-with-args #'ignore))
            (nskk-rollback-conversion)
            (should (= (nskk-state-henkan-count) 0))))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay))
      )))

    (nskk-it "clears nskk--azik-sokuon-okuri-kana-pending on rollback"
      ;; After JP106 + fires sokuon okurigana, C-g (rollback) must clear the
      ;; sentinel so the next preedit does not start with a stale flag.
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count))
              (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
              (nskk--henkan-candidate-list-active nil)
              (nskk--azik-sokuon-okuri-kana-pending t))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 0)
            (nskk-state-set-conversion-overlay nil)
            
          (insert "▼漢字")
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (goto-char (point-max))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (nskk-state-set-candidates nskk-current-state '("漢字"))
          (nskk-with-mocks ((nskk--delete-marker-at #'ignore)
                            (run-hook-with-args #'ignore))
            (nskk-rollback-conversion)
            (should-not (nskk-azik-sokuon-okuri-kana-pending))))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay))
      )))))

;;;
;;; nskk-start-registration Depth Guard Tests
;;;

(nskk-describe "henkan registration depth guard"
  (nskk-it "returns nil when depth is at maximum (3)"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-test-saved-registration-depth (nskk-state-registration-depth)))
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 3)
            
      (should-not (nskk-start-registration "test")))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))

  (nskk-it "proceeds and shows prompt when depth is below maximum"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-test-saved-registration-depth (nskk-state-registration-depth))
          prompt-shown)
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 0)
            
      ;; Set phase to `on' (preedit) so the nil->registration transition is valid
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk-with-mocks ((read-from-minibuffer (lambda (p) (setq prompt-shown p) ""))
                        (nskk-dict-register-word #'ignore))
        (nskk-start-registration "てすと")
        (should prompt-shown)
        (should (string-match-p "辞書登録" prompt-shown))))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      )))

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

(nskk-describe "nskk-insert-marker"
  (nskk-it "inserts the string without buffering modification hooks"
    (with-temp-buffer
      (nskk-insert-marker "▽")
      (should (equal (buffer-string) "▽"))))

  (nskk-it "does not record undo when inhibit-undo wrapper is active"
    (with-temp-buffer
      (let (captured-undo)
        (nskk-without-modification
          (nskk-insert-marker "▽")
          (setq captured-undo buffer-undo-list))
        (should (eq captured-undo t)))))

  (nskk-it "can insert the active marker ▼ as well"
    (with-temp-buffer
      (nskk-insert-marker "▼")
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
        (nskk-set-conversion-start-marker (point-min))
        (should (equal (nskk-preedit-string) "かんじ")))))

  (nskk-it "returns nil when point is at or before the marker"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification (insert nskk-henkan-on-marker))
        (nskk-set-conversion-start-marker (point-min))
        (should (null (nskk-preedit-string))))))

  ;; CPS variant: on-found(string) when preedit text exists, on-not-found() otherwise.
  (nskk-it "preedit-string/k calls on-found with text when preedit text is present"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification
          (insert nskk-henkan-on-marker)
          (insert "か"))
        (nskk-set-conversion-start-marker (point-min))
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

  (nskk-it "returns a list of strings that are not the prefix itself"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (let ((results (nskk--dcomp-search-prefix "あ")))
          (when results
            (should (cl-every #'stringp results))
            (should-not (member "あ" results))))))))

(nskk-describe "nskk--dcomp-replace-preedit"
  (nskk-it "replaces preedit text after the ▽ marker with new text"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification
          (insert nskk-henkan-on-marker)
          (insert "かん"))
        (nskk-set-conversion-start-marker (point-min))
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
        (nskk-set-conversion-start-marker (point-min))
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
        (nskk-set-conversion-start-marker (point-min))
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
        (nskk-set-conversion-start-marker (point-min))
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
             (lambda () (cl-incf nskk--test-dismiss-call-count))))
        (nskk--dismiss-candidate-list)
        (should (= nskk--test-dismiss-call-count 1)))))

  (nskk-it "runs every cleanup and re-signals the first error or quit fail-closed"
    (dolist (injected '((error dismiss-error payload)
                        (quit dismiss-quit payload)))
      (with-temp-buffer
        (let ((cleanup-order nil)
              (nskk--henkan-candidate-list-active t)
              caught)
          (let ((nskk-henkan-hide-candidates-functions
                 (list (lambda ()
                         (push 'first cleanup-order)
                         (signal (car injected) (cdr injected)))
                       (lambda ()
                         (push 'second cleanup-order)))))
            (condition-case condition
                (nskk--dismiss-candidate-list)
              ((error quit)
               (setq caught condition))))
          (should (equal caught injected))
          (should (equal cleanup-order '(second first)))
          (should-not nskk--henkan-candidate-list-active)))))

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
        (nskk-state-set-romaji-buffer "sh")
        (nskk-henkan-do-reset)
        (should (string-empty-p (nskk-state-romaji-buffer))))))

  (nskk-it "resets henkan-count to 0"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-state-set-henkan-count 5)
        (nskk-henkan-do-reset)
        (should (= (nskk-state-henkan-count) 0)))))

  (nskk-it "clears conversion-start-marker"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (insert "test")
        (nskk-set-conversion-start-marker (point-min))
        (nskk-henkan-do-reset)
        (should (null (nskk-get-conversion-start))))))

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
        (nskk-set-conversion-start-marker (point-min))
        (nskk-henkan-kakutei)
        ;; The ▽ marker should be removed; text remains
        (should (not (string-search nskk-henkan-on-marker (buffer-string)))))))

  (nskk-it "clears the conversion start marker"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification (insert nskk-henkan-on-marker))
        (nskk-set-conversion-start-marker (point-min))
        (nskk-henkan-kakutei)
        (should (null (nskk-get-conversion-start))))))

  (nskk-it "clears the romaji buffer"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-state-set-romaji-buffer "sh")
        (nskk-henkan-kakutei)
        (should (string-empty-p (nskk-state-romaji-buffer))))))

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
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk--henkan-candidate-list-active nil)
            (nskk-henkan-hide-candidates-functions nil)
            (called nil))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-conversion-start-marker nil)
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 3)
            
        (insert "▼かんじ")
        (goto-char (point-max))
        (nskk--insert-registered-and-reset "漢字" 1 (lambda () (setq called t)))
        (should (string= (buffer-string) "漢字"))
        (should called))
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

  (nskk-it "tolerates nil on-done callback"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk--henkan-candidate-list-active nil)
            (nskk-henkan-hide-candidates-functions nil))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-conversion-start-marker nil)
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 1)
            
        (insert "▼test")
        (goto-char (point-max))
        ;; Should not error when on-done is nil
        (should (progn (nskk--insert-registered-and-reset "result" 1 nil) t)))
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))))

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

  (nskk-it "does not touch nskk-state-romaji-buffer (macro scope only)"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "sh")
            
      (nskk-with-current-state
        (nskk-reset-henkan-state))
      (should (equal (nskk-state-romaji-buffer) "sh")))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      )))

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

(defvar nskk--test-sentinel-string-wba)

(nskk-describe "nskk-when-bound-and"
  (nskk-it "executes body when variable is bound and satisfies predicate"
    ;; nskk--dcomp-prefix is a defvar-local initialised to nil; stringp fails.
    ;; Use a dedicated dynamic sentinel variable that is bound and a string
    ;; (stringp passes) to exercise the macro's own dispatch logic.  `boundp'
    ;; only sees dynamic bindings, so the sentinel must be `defvar'd above.
    (let (executed)
      (let ((nskk--test-sentinel-string-wba ""))
        (nskk-when-bound-and nskk--test-sentinel-string-wba stringp
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

(nskk-describe "nskk-set-conversion-start-marker"
  (nskk-it "creates a marker at the given position"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker nil)
            
        (insert "abcd")
        (goto-char (point-min))
        (nskk-set-conversion-start-marker (point-min))
        (should (markerp (nskk-state-conversion-start-marker)))
        (should (= (marker-position (nskk-state-conversion-start-marker)) (point-min))))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "sets the marker to a mid-buffer position"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker nil)
            
        (insert "▽かな")
        (goto-char 2)
        (nskk-set-conversion-start-marker 2)
        (should (= (marker-position (nskk-state-conversion-start-marker)) 2)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      ))))

(nskk-describe "nskk--clear-conversion-start-marker"
  (nskk-it "clears the marker position to nil"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (should (marker-position (nskk-state-conversion-start-marker)))
        (nskk--clear-conversion-start-marker)
        (should-not (marker-position (nskk-state-conversion-start-marker))))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "is safe to call when marker is already nil"
    (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker nil)
            
      (nskk--clear-conversion-start-marker)
      (should-not (nskk-state-conversion-start-marker)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

(nskk-describe "nskk-conversion-start-active-p"
  (nskk-it "returns non-nil when marker has a position"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (should (nskk-conversion-start-active-p)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "returns nil when marker is nil"
    (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker nil)
            
      (should-not (nskk-conversion-start-active-p)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      ))

  (nskk-it "returns nil when marker has no position"
    (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
      (should-not (nskk-conversion-start-active-p)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

(nskk-describe "nskk-get-conversion-start"
  (nskk-it "returns the marker position as an integer"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        (insert "  ▽かな")
        (set-marker (nskk-state-conversion-start-marker) 3)
        (should (= (nskk-get-conversion-start) 3)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "returns nil when no marker is set"
    (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker nil)
            
      (should-not (nskk-get-conversion-start)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      ))

  (nskk-it "returns nil when marker has no position"
    (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
      (should-not (nskk-get-conversion-start)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

(nskk-describe "nskk-has-preedit"
  (nskk-it "returns non-nil when preedit text exists after the marker"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert nskk-henkan-on-marker "かな")
        (should (nskk-has-preedit)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "returns nil when point is right after the marker (no text)"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert nskk-henkan-on-marker)
        (should-not (nskk-has-preedit)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "returns nil when no conversion start marker is set"
    (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker nil)
            
      (should-not (nskk-has-preedit)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

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
      (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (should (equal (nskk-convert-input-to-kana-final) "")))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "converts standalone n to ん (hatsuon at word boundary)"
    (with-temp-buffer
      (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "n")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (should (equal (nskk-convert-input-to-kana-final) "ん")))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "converts romaji like ka to か"
    (with-temp-buffer
      (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "ka")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (should (equal (nskk-convert-input-to-kana-final) "か")))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "clears the romaji buffer after conversion"
    (with-temp-buffer
      (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "shi")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-convert-input-to-kana-final)
        (should (equal (nskk-state-romaji-buffer) "")))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      ))))

(nskk-describe "nskk-convert-input-to-kana-final/k"
  (nskk-it "calls on-done with empty string when buffer is empty"
    (with-temp-buffer
      (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            result)
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-convert-input-to-kana-final/k (lambda (s) (setq result s)) #'ignore)
        (should (equal result "")))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "calls on-done with ん for standalone n"
    (with-temp-buffer
      (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            result)
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "n")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-convert-input-to-kana-final/k (lambda (s) (setq result s)) #'ignore)
        (should (equal result "ん")))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "calls on-done with converted kana for complete romaji"
    (with-temp-buffer
      (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            result)
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "tsu")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-convert-input-to-kana-final/k (lambda (s) (setq result s)) #'ignore)
        (should (equal result "つ")))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "is consistent with the sync variant across common romaji"
    (nskk-deftest-table kana-final-cps-sync-consistency
      :columns (romaji)
      :rows (("") ("n") ("ka") ("shi") ("tsu"))
      :body (with-temp-buffer
              (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
                    (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
                    cps-result)
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer romaji)
            (nskk-state-set-pending-romaji-overlay nil)
            
                (nskk-convert-input-to-kana-final/k
                  (lambda (s) (setq cps-result s)) #'ignore)
                (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
                      (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer romaji)
            (nskk-state-set-pending-romaji-overlay nil)
            
                  (should (equal cps-result
                                 (nskk-convert-input-to-kana-final))))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      ))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))))


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

  (nskk-it "returns all matching keys as strings"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (nskk-prolog-assert '((user-dict-entry "かんとく" ("相当局"))))
      (nskk-prolog-assert '((user-dict-entry "かんじ" ("漢字"))))
      (let ((results (nskk--dcomp-search-prefix "かん")))
        (should (> (length results) 1))
        (should (cl-every #'stringp results))
        (should (member "かんとく" results))
        (should (member "かんじ" results)))))

  (nskk-it "returns user-dict entries before system-dict entries"
    (nskk-prolog-test-with-isolated-db
      ;; Set up trie indexes for both dictionaries
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (nskk-prolog-set-index 'system-dict-entry 2 :trie)
      ;; Assert system-dict entries
      (nskk-prolog-assert '((system-dict-entry "かんぱい" ("乾杯"))))
      (nskk-prolog-assert '((system-dict-entry "かんそう" ("乾燥"))))
      ;; Assert user-dict entries
      (nskk-prolog-assert '((user-dict-entry "かんしゃ" ("感謝"))))
      (nskk-prolog-assert '((user-dict-entry "かんが" ("考"))))
      (let ((results (nskk--dcomp-search-prefix "かん")))
        ;; User-dict keys should appear before system-dict keys
        (should results)
        (let ((user-keys '("かんしゃ" "かんが"))
              (sys-keys '("かんぱい" "かんそう")))
          ;; All user-dict keys should be in results
          (dolist (k user-keys)
            (should (member k results)))
          ;; All system-dict keys should be in results
          (dolist (k sys-keys)
            (should (member k results)))
          ;; User-dict keys should come before system-dict keys
          (let ((last-user-pos (apply #'max (mapcar (lambda (k) (cl-position k results :test #'equal)) user-keys)))
                (first-sys-pos (apply #'min (mapcar (lambda (k) (cl-position k results :test #'equal)) sys-keys))))
            (should (< last-user-pos first-sys-pos)))))))

  (nskk-it "deduplicates keys present in both user-dict and system-dict"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (nskk-prolog-set-index 'system-dict-entry 2 :trie)
      (nskk-prolog-assert '((user-dict-entry "かんじ" ("漢字"))))
      (nskk-prolog-assert '((system-dict-entry "かんじ" ("漢字"))))
      (nskk-prolog-assert '((system-dict-entry "かんそう" ("乾燥"))))
      (let ((results (nskk--dcomp-search-prefix "かん")))
        ;; "かんじ" should appear exactly once (from user-dict)
        (should (= 1 (cl-count "かんじ" results :test #'equal)))
        ;; "かんそう" should appear (from system-dict)
        (should (member "かんそう" results)))))

  (nskk-it "returns system-dict entries when user-dict is empty"
    (nskk-prolog-test-with-isolated-db
      (nskk-prolog-set-index 'user-dict-entry 2 :trie)
      (nskk-prolog-set-index 'system-dict-entry 2 :trie)
      (nskk-prolog-assert '((system-dict-entry "かんそう" ("乾燥"))))
      (let ((results (nskk--dcomp-search-prefix "かん")))
        (should results)
        (should (member "かんそう" results))))))

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
;;; nskk-clear-conversion-context Tests
;;;

(nskk-describe "nskk-clear-conversion-context"
  (nskk-it "resets dcomp state variables to nil/0"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--dcomp-candidates '("かんじ" "かんとく"))
            (nskk--dcomp-prefix "かん")
            (nskk--dcomp-index 1)
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-conversion-start-marker nil)
            (nskk-state-set-romaji-buffer "")
            
        (nskk-clear-conversion-context)
        (should (null nskk--dcomp-candidates))
        (should (null nskk--dcomp-prefix))
        (should (= nskk--dcomp-index 0)))
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      )))

  (nskk-it "clears conversion candidate state from nskk-current-state"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk--dcomp-candidates nil)
            (nskk--dcomp-prefix nil)
            (nskk--dcomp-index 0))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-conversion-start-marker nil)
            (nskk-state-set-romaji-buffer "")
            
        (nskk-state-set-candidates nskk-current-state '("A" "B"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-clear-conversion-context)
        (should (null (nskk-state-candidates nskk-current-state)))
        (should (null (nskk-state-henkan-phase nskk-current-state))))
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      )))

  (nskk-it "clears input state variables via Prolog clearable-input-var/1 table"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk--dcomp-candidates nil)
            (nskk--dcomp-prefix nil)
            (nskk--dcomp-index 0)
            (nskk--numeric-mode t)
            (nskk--sticky-shift-pending 'okurigana)
            (nskk--deferred-azik-state '(some state))
            (nskk--deferred-vowel-shadow-state t)
            (nskk--azik-colon-okuri-pending t)
            (nskk--azik-colon-okuri-deferred t))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-conversion-start-marker nil)
            (nskk-state-set-romaji-buffer "")
            
        (nskk-clear-conversion-context)
        (should-not (nskk-numeric-mode))
        (should-not (nskk-sticky-shift-pending))
        (should-not (nskk-deferred-azik-state))
        (should-not (nskk-deferred-vowel-shadow-state))
        (should-not (nskk-azik-colon-okuri-pending))
        (should-not (nskk-azik-colon-okuri-deferred)))
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      )))

  (nskk-it "nskk-clear-azik-pending-state clears sticky-shift-pending"
    ;; Regression: sticky-shift-pending was missing from the dolist in
    ;; nskk-clear-azik-pending-state, causing stale sticky state after
    ;; kakutei/cancel/rollback.
    (with-temp-buffer
      (let ((nskk--sticky-shift-pending 'immediate))
        (nskk-clear-azik-pending-state)
        (should-not (nskk-sticky-shift-pending)))))

  (nskk-it "nskk-clear-azik-pending-state clears sticky-shift-pending in okurigana state"
    (with-temp-buffer
      (let ((nskk--sticky-shift-pending 'okurigana))
        (nskk-clear-azik-pending-state)
        (should-not (nskk-sticky-shift-pending)))))

  (nskk-it "resets nskk--henkan-candidate-list-active to nil on mode switch"
    ;; Regression test: nskk-clear-conversion-context must call
    ;; nskk--dismiss-candidate-list (not bare run-hook-with-args) so that
    ;; nskk--henkan-candidate-list-active is reset atomically with the
    ;; hide-candidates hook.  Without this fix, mode switches left the flag
    ;; t even though the candidate list UI was already hidden.
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--henkan-candidate-list-active t)
            (nskk-henkan-hide-candidates-functions nil)
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk--dcomp-candidates nil)
            (nskk--dcomp-prefix nil)
            (nskk--dcomp-index 0))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-conversion-start-marker nil)
            (nskk-state-set-romaji-buffer "")
            
        (nskk-clear-conversion-context)
        (should-not nskk--henkan-candidate-list-active))
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      )))

  (nskk-it "runs every cleanup after hide/inline faults and preserves the first condition"
    (dolist (invocation '(direct mode-switch))
      (dolist (injection '(hide-first hide-second inline))
        (dolist (condition-type '(error quit))
          (ert-info ((format "invocation=%S injection=%S condition=%S"
                             invocation injection condition-type))
            (with-temp-buffer
              (insert "x")
              (let* ((nskk-current-state (nskk-state-create 'hiragana))
                     (conversion-overlay
                      (make-overlay (point-min) (point-max)))
                     (pending-overlay
                      (make-overlay (point-min) (point-max)))
                     (dcomp-overlay
                      (make-overlay (point-min) (point-max)))
                     (inline-overlay
                      (make-overlay (point-min) (point-max)))
                     (marker (copy-marker (point-min)))
                     (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
                     (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
                     (nskk-test-saved-dcomp-multiple-overlay (nskk-state-dcomp-multiple-overlay))
                     (nskk--inline-overlay inline-overlay)
                     (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
                     (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
                     (nskk--dcomp-candidates '("dirty"))
                     (nskk--dcomp-prefix "dirty")
                     (nskk--dcomp-index 1)
                     (nskk--henkan-candidate-list-active t)
                     (nskk--numeric-mode t)
                     (nskk--sticky-shift-pending 'okurigana)
                     (nskk--deferred-azik-state '(dirty))
                     (nskk--deferred-vowel-shadow-state '(dirty))
                     (nskk--azik-colon-okuri-pending t)
                     (nskk--azik-colon-okuri-deferred '(dirty))
                     (nskk--azik-sokuon-okuri-kana-pending t)
                     (primary-payload (list 'primary-payload))
                     (primary-data (list "primary-cleanup" primary-payload))
                     (secondary-data
                      (list "secondary-cleanup" (list 'secondary-payload)))
                     (secondary-type
                      (if (eq condition-type 'error) 'quit 'error))
                     (expected-mode
                      (if (eq invocation 'mode-switch)
                          'katakana
                        'hiragana))
                     callback-overlays
                     cleanup-order
                     cleanup-inhibit-quit
                     caught)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-overlay conversion-overlay)
            (nskk-state-set-pending-romaji-overlay pending-overlay)
            (nskk-state-set-dcomp-multiple-overlay dcomp-overlay)
            (nskk-state-set-conversion-start-marker marker)
            (nskk-state-set-romaji-buffer "dirty")
            
                (setf (nskk-state-candidates nskk-current-state) '("A" "B")
                      (nskk-state-current-index nskk-current-state) 1
                      (nskk-state-metadata nskk-current-state)
                      '(okurigana "k" okurigana-in-progress t))
                (nskk-state-force-henkan-phase nskk-current-state 'active)
                (cl-labels
                    ((record-cleanup (name)
                       (push name cleanup-order)
                       (push inhibit-quit cleanup-inhibit-quit))
                     (recreate-early-state ()
                       (let ((overlay
                              (make-overlay (point-min) (point-max))))
                         (push overlay callback-overlays)
                         (nskk-state-set-conversion-overlay overlay))))
                  (let ((nskk-henkan-hide-candidates-functions
                         (list
                          (lambda ()
                            (record-cleanup 'hide-first)
                            (when (eq injection 'hide-first)
                              (recreate-early-state)
                              (signal condition-type primary-data)))
                          (lambda ()
                            (record-cleanup 'hide-second)
                            (when (eq injection 'hide-second)
                              (recreate-early-state)
                              (signal condition-type primary-data))))))
                    (cl-letf (((symbol-function 'nskk-inline-hide)
                               (lambda ()
                                 (record-cleanup 'inline)
                                 (setq nskk--henkan-candidate-list-active t)
                                 (if (eq injection 'inline)
                                     (progn
                                       (recreate-early-state)
                                       (signal condition-type primary-data))
                                   (signal secondary-type secondary-data)))))
                      (condition-case condition
                          (pcase invocation
                            ('direct
                             (nskk-clear-conversion-context))
                            ('mode-switch
                             (nskk-set-mode 'katakana)))
                        ((error quit)
                         (setq caught condition))))))
                (should caught)
                (should (eq (car caught) condition-type))
                (should (eq (cdr caught) primary-data))
                (should (eq (caddr caught) primary-payload))
                (should (equal (nreverse cleanup-order)
                               '(hide-first hide-second inline)))
                (should (= (length cleanup-inhibit-quit) 3))
                (should-not (memq nil cleanup-inhibit-quit))
                (should-not (nskk-state-conversion-overlay))
                (should-not (overlay-buffer conversion-overlay))
                (dolist (overlay callback-overlays)
                  (should-not (overlay-buffer overlay)))
                (should-not (nskk-state-pending-romaji-overlay))
                (should-not (overlay-buffer pending-overlay))
                (should-not (nskk-state-dcomp-multiple-overlay))
                (should-not (overlay-buffer dcomp-overlay))
                (should-not nskk--inline-overlay)
                (should-not (overlay-buffer inline-overlay))
                (should-not (marker-position marker))
                (should (equal (nskk-state-romaji-buffer) ""))
                (should-not nskk--dcomp-candidates)
                (should-not nskk--dcomp-prefix)
                (should (= nskk--dcomp-index 0))
                (should-not nskk--henkan-candidate-list-active)
                (should-not (nskk-numeric-mode))
                (should-not (nskk-sticky-shift-pending))
                (should-not (nskk-deferred-azik-state))
                (should-not (nskk-deferred-vowel-shadow-state))
                (should-not (nskk-azik-colon-okuri-pending))
                (should-not (nskk-azik-colon-okuri-deferred))
                (should-not (nskk-azik-sokuon-okuri-kana-pending))
                (should-not
                 (nskk-state-candidates nskk-current-state))
                (should (= (nskk-state-current-index nskk-current-state) 0))
                (should-not
                 (nskk-state-henkan-phase nskk-current-state))
                (should-not
                 (plist-get (nskk-state-metadata nskk-current-state)
                            'okurigana))
                (should-not
                 (plist-get (nskk-state-metadata nskk-current-state)
                            'okurigana-in-progress))
                (should (eq (nskk-state-mode nskk-current-state)
                            expected-mode)))
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-dcomp-multiple-overlay nskk-test-saved-dcomp-multiple-overlay)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      ))))))))

;;;
;;; nskk--wrap-to-first-candidate Tests
;;;

(nskk-describe "nskk--show-candidate-list-next"
  (nskk-it "rolls back state and cleans up after show hook errors and quits"
    (dolist (injected '((error next-show-error payload)
                        (quit next-show-quit payload)))
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count))
              (nskk-henkan-number-to-display-candidates 2)
              (nskk-henkan-show-candidates-keys '(?a ?s))
              (nskk--henkan-candidate-list-active t)
              cleanup-order
              observed)
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 17)
            
          (nskk-state-set-candidates
           nskk-current-state '("A" "B" "C" "D" "E"))
          (nskk-state-force-henkan-phase nskk-current-state 'list)
          (setf (nskk-state-current-index nskk-current-state) 1)
          (let ((nskk-henkan-show-candidates-functions
                 (lambda (_candidates index)
                   (setq observed
                         (list index
                               (nskk-state-current-index nskk-current-state)
                               (nskk-state-henkan-phase nskk-current-state)
                               (nskk-state-henkan-count)
                               nskk--henkan-candidate-list-active))
                   (setf (nskk-state-current-index nskk-current-state) 4
                         (nskk-state-henkan-phase nskk-current-state)
                         'registration)
                   (nskk-state-set-henkan-count -1)
                   (signal (car injected) (cdr injected))))
                (nskk-henkan-hide-candidates-functions
                 (list
                  (lambda ()
                    (push 'first cleanup-order)
                    (setf (nskk-state-current-index nskk-current-state) 4
                          (nskk-state-henkan-phase nskk-current-state)
                          'registration)
                    (progn (nskk-state-set-henkan-count -2) (setq nskk--henkan-candidate-list-active t))
                    (signal 'error '(cleanup-error payload)))
                  (lambda ()
                    (push 'second cleanup-order)
                    (setq nskk--henkan-candidate-list-active t)))))
            (let ((caught
                   (condition-case condition
                       (progn
                         (nskk--show-candidate-list-next)
                         nil)
                     ((error quit) condition))))
              (should (equal caught injected))))
          (should (equal observed '(3 3 list 17 t)))
          (should (equal cleanup-order '(second first)))
          (should (= (nskk-state-current-index nskk-current-state) 1))
          (should (eq (nskk-state-henkan-phase nskk-current-state) 'list))
          (should (= (nskk-state-henkan-count) 17))
          (should-not nskk--henkan-candidate-list-active))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))))

(nskk-describe "nskk--show-candidate-list-prev"
  (nskk-it "rolls back state and cleans up after show hook errors and quits"
    (dolist (injected '((error prev-show-error payload)
                        (quit prev-show-quit payload)))
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count))
              (nskk-henkan-number-to-display-candidates 2)
              (nskk-henkan-show-candidates-keys '(?a ?s))
              (nskk--henkan-candidate-list-active t)
              cleanup-order
              observed)
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 19)
            
          (nskk-state-set-candidates
           nskk-current-state '("A" "B" "C" "D" "E"))
          (nskk-state-force-henkan-phase nskk-current-state 'list)
          (setf (nskk-state-current-index nskk-current-state) 3)
          (let ((nskk-henkan-show-candidates-functions
                 (lambda (_candidates index)
                   (setq observed
                         (list index
                               (nskk-state-current-index nskk-current-state)
                               (nskk-state-henkan-phase nskk-current-state)
                               (nskk-state-henkan-count)
                               nskk--henkan-candidate-list-active))
                   (setf (nskk-state-current-index nskk-current-state) 4
                         (nskk-state-henkan-phase nskk-current-state)
                         'registration)
                   (nskk-state-set-henkan-count -1)
                   (signal (car injected) (cdr injected))))
                (nskk-henkan-hide-candidates-functions
                 (list
                  (lambda ()
                    (push 'first cleanup-order)
                    (setf (nskk-state-current-index nskk-current-state) 4
                          (nskk-state-henkan-phase nskk-current-state)
                          'registration)
                    (progn (nskk-state-set-henkan-count -2) (setq nskk--henkan-candidate-list-active t))
                    (signal 'error '(cleanup-error payload)))
                  (lambda ()
                    (push 'second cleanup-order)
                    (setq nskk--henkan-candidate-list-active t)))))
            (let ((caught
                   (condition-case condition
                       (progn
                         (nskk--show-candidate-list-prev)
                         nil)
                     ((error quit) condition))))
              (should (equal caught injected))))
          (should (equal observed '(1 1 list 19 t)))
          (should (equal cleanup-order '(second first)))
          (should (= (nskk-state-current-index nskk-current-state) 3))
          (should (eq (nskk-state-henkan-phase nskk-current-state) 'list))
          (should (= (nskk-state-henkan-count) 19))
          (should-not nskk--henkan-candidate-list-active))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))))

(nskk-describe "nskk--wrap-to-first-candidate"
  ;; nskk--wrap-to-first-candidate calls nskk-state-set-henkan-phase with 'list.
  ;; Valid transition to 'list requires starting from 'active phase.
  (nskk-it "resets index to 0 and updates henkan-count to threshold"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk-henkan-show-candidates-nth 5)
            (nskk--henkan-candidate-list-active nil))
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 7)
            
        (nskk-state-set-candidates nskk-current-state '("A" "B" "C"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (setf (nskk-state-current-index nskk-current-state) 2)
        (nskk-with-mocks ((run-hook-with-args #'ignore))
	  (nskk--wrap-to-first-candidate))
        (should (= (nskk-state-current-index nskk-current-state) 0))
        (should (= (nskk-state-henkan-count) nskk-henkan-show-candidates-nth)))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

  (nskk-it "sets henkan-phase to list and activates candidate-list"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk-henkan-show-candidates-nth 3)
            (nskk--henkan-candidate-list-active nil))
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 0)
            
        (nskk-state-set-candidates nskk-current-state '("A"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((run-hook-with-args #'ignore))
	  (nskk--wrap-to-first-candidate))
        (should (eq (nskk-state-henkan-phase nskk-current-state) 'list))
        (should nskk--henkan-candidate-list-active))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

  (nskk-it "fires nskk-henkan-show-candidates-functions hook with candidates and index 0"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk-henkan-show-candidates-nth 3)
            (nskk--henkan-candidate-list-active nil)
            captured-args)
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 0)
            
        (nskk-state-set-candidates nskk-current-state '("X" "Y"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((run-hook-with-args
                           (lambda (_hook &rest args)
                             (setq captured-args args))))
	  (nskk--wrap-to-first-candidate))
        (should (equal (car captured-args) '("X" "Y")))
        (should (= (cadr captured-args) 0)))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

  (nskk-it "rolls back state and cleans up after show hook errors and quits"
    (dolist (injected '((error wrap-show-error payload)
                        (quit wrap-show-quit payload)))
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-test-saved-henkan-count (nskk-state-henkan-count))
              (nskk-henkan-show-candidates-nth 3)
              (nskk--henkan-candidate-list-active t)
              cleanup-order
              observed)
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 7)
            
          (nskk-state-set-candidates nskk-current-state '("A" "B" "C"))
          (nskk-state-force-henkan-phase nskk-current-state 'active)
          (setf (nskk-state-current-index nskk-current-state) 2)
          (let ((nskk-henkan-show-candidates-functions
                 (lambda (_candidates index)
                   (setq observed
                         (list index
                               (nskk-state-current-index nskk-current-state)
                               (nskk-state-henkan-phase nskk-current-state)
                               (nskk-state-henkan-count)
                               nskk--henkan-candidate-list-active))
                   (setf (nskk-state-current-index nskk-current-state) 1
                         (nskk-state-henkan-phase nskk-current-state)
                         'registration)
                   (nskk-state-set-henkan-count -1)
                   (signal (car injected) (cdr injected))))
                (nskk-henkan-hide-candidates-functions
                 (list
                  (lambda ()
                    (push 'first cleanup-order)
                    (setf (nskk-state-current-index nskk-current-state) 1
                          (nskk-state-henkan-phase nskk-current-state)
                          'registration)
                    (progn (nskk-state-set-henkan-count -2) (setq nskk--henkan-candidate-list-active t))
                    (signal 'error '(cleanup-error payload)))
                  (lambda ()
                    (push 'second cleanup-order)
                    (setq nskk--henkan-candidate-list-active t)))))
            (let ((caught
                   (condition-case condition
                       (progn
                         (nskk--wrap-to-first-candidate)
                         nil)
                     ((error quit) condition))))
              (should (equal caught injected))))
          (should (equal observed '(0 0 list 3 t)))
          (should (equal cleanup-order '(second first)))
          (should (= (nskk-state-current-index nskk-current-state) 2))
          (should (eq (nskk-state-henkan-phase nskk-current-state) 'active))
          (should (= (nskk-state-henkan-count) 7))
          (should-not nskk--henkan-candidate-list-active))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))))

;;;
;;; nskk-cancel-conversion-to-reading Tests
;;;

(nskk-describe "nskk-cancel-conversion-to-reading"
  (nskk-it "does nothing when not converting"
    (with-temp-buffer
      (insert "unchanged")
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        (nskk-cancel-conversion-to-reading)
        (should (equal (buffer-string) "unchanged")))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "removes the ▼ marker from buffer when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk--henkan-candidate-list-active t))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 2)
            
        (insert nskk-henkan-active-marker "かんじ")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-state-set-candidates nskk-current-state '("漢字"))
        (nskk-cancel-conversion-to-reading)
        ;; ▼ marker removed; kana reading remains
        (should (string-match-p "かんじ" (buffer-string)))
        (should-not (string-match-p (regexp-quote nskk-henkan-active-marker) (buffer-string))))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

  (nskk-it "clears henkan-count and candidate-list-active"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk--henkan-candidate-list-active t))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 4)
            
        (insert nskk-henkan-active-marker "ほげ")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-state-set-candidates nskk-current-state '("保毛"))
        (nskk-cancel-conversion-to-reading)
        (should (= (nskk-state-henkan-count) 0))
        (should-not nskk--henkan-candidate-list-active))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))))

;;;
;;; nskk-show-pending-romaji / nskk-clear-pending-romaji Tests
;;;

(nskk-describe "nskk-show-pending-romaji"
  (nskk-it "creates an overlay with the given text as after-string"
    (with-temp-buffer
      (let ((nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-show-pending-romaji "ka")
        (should (nskk-state-pending-romaji-overlay))
        (should (overlayp (nskk-state-pending-romaji-overlay)))
        (should (equal (overlay-get (nskk-state-pending-romaji-overlay) 'after-string) "ka")))
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "does nothing for an empty string"
    (with-temp-buffer
      (let ((nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-show-pending-romaji "")
        (should-not (nskk-state-pending-romaji-overlay)))
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "does nothing for a non-string argument"
    (with-temp-buffer
      (let ((nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-show-pending-romaji nil)
        (should-not (nskk-state-pending-romaji-overlay)))
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      ))))

(nskk-describe "nskk-clear-pending-romaji"
  (nskk-it "deletes the pending romaji overlay when present"
    (with-temp-buffer
      (let ((nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-show-pending-romaji "ka")
        (should (nskk-state-pending-romaji-overlay))
        (nskk-clear-pending-romaji)
        (should-not (and (nskk-state-pending-romaji-overlay)
                         (overlay-buffer (nskk-state-pending-romaji-overlay)))))
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "is safe to call when no overlay exists (idempotent)"
    (with-temp-buffer
      (let ((nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-pending-romaji-overlay nil)
            
        (should-not (nskk-clear-pending-romaji)))
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      ))))

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
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            start-conversion-called on-done-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert nskk-henkan-on-marker "かな")
        (nskk-state-force-henkan-phase nskk-current-state 'on)
        (nskk-with-mocks ((nskk-start-conversion (lambda () (setq start-conversion-called t))))
          (nskk-convert-or-commit/k (lambda () (setq on-done-called t))))
        (should start-conversion-called)
        (should on-done-called))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      ))))

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
          (nskk-test-saved-henkan-count (nskk-state-henkan-count))
          exhausted-called)
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 0)
            
      (nskk-next-candidate/k #'ignore (lambda () (setq exhausted-called t)))
      (should exhausted-called))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))

  (nskk-it "calls on-candidate with current candidate when selecting inline"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk-henkan-show-candidates-nth 5)
            received-candidate)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-henkan-count 0)
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert "test")
        (nskk-state-set-candidates nskk-current-state '("漢字" "感じ"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk--select-candidate #'ignore))
          (nskk-next-candidate/k (lambda (c) (setq received-candidate c)) #'ignore))
        (should (equal received-candidate "漢字")))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))))

;;;
;;; nskk-previous-candidate/k Tests
;;;

(nskk-describe "nskk-previous-candidate/k"
  (nskk-it "calls on-not-found when not converting"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-test-saved-henkan-count (nskk-state-henkan-count))
          not-found-called)
      (unwind-protect
          (progn
            (nskk-state-set-henkan-count 0)
            
      ;; When not converting, on-not-found is called (standard defun/k pattern)
      (nskk-previous-candidate/k #'ignore (lambda () (setq not-found-called t)))
      (should not-found-called))
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))

  (nskk-it "calls on-found with the selected candidate after selecting prev candidate inline"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk--henkan-candidate-list-active nil)
            received-candidate)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-henkan-count 2)
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert "test")
        (nskk-state-set-candidates nskk-current-state '("a" "b" "c"))
        (nskk-state-set-current-index nskk-current-state 1)
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk--select-candidate #'ignore))
          ;; on-found is called with the candidate at current-index after decrement.
          ;; nskk-state-henkan-count decrements 2→1; nskk--select-candidate is mocked
          ;; (no real index change), so current-index stays at 1 → candidate "b".
          (nskk-previous-candidate/k (lambda (c) (setq received-candidate c)) #'ignore))
        (should (equal received-candidate "b")))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))))

;;;
;;; nskk-start-conversion/k Tests
;;;

(nskk-describe "nskk-start-conversion/k"
  (nskk-it "calls on-found with candidates when search returns results"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            found-candidates)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 0)
            (nskk-state-set-conversion-overlay nil)
            
        (insert nskk-henkan-on-marker "かんじ")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
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
        (should (equal (nskk-state-candidates nskk-current-state) '("漢字" "感じ"))))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay))
      )))

  (nskk-it "calls on-not-found when search returns nothing and registration is cancelled"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-registration-depth (nskk-state-registration-depth))
            not-found-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 0)
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-registration-depth 0)
            
        (insert nskk-henkan-on-marker "てすと")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
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
        (should not-found-called))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))))

;;;
;;; nskk--exhaust-candidates/k Tests
;;;

(nskk-describe "nskk--exhaust-candidates/k"
  (nskk-it "wraps to first candidate and calls on-done when no preedit text"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))   ; no marker = no text
            (nskk--henkan-candidate-list-active t)
            wrap-called on-done-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker nil)
            
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-state-set-candidates nskk-current-state '("A"))
        (nskk-with-mocks ((nskk--wrap-to-first-candidate (lambda () (setq wrap-called t)))
                          (run-hook-with-args #'ignore))
          (nskk--exhaust-candidates/k (lambda () (setq on-done-called t))))
        (should wrap-called)
        (should on-done-called)
        ;; candidate-list-active should be reset
        (should-not nskk--henkan-candidate-list-active))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "calls nskk-start-registration/k with preedit text when candidates exhausted"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk--henkan-candidate-list-active t)
            registration-text on-done-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-conversion-overlay nil)
            
        (insert nskk-henkan-active-marker "かんじ")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
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
        (should on-done-called))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay))
      )))

  (nskk-it "registers under the okurigana dict key, showing stem*kana in the prompt"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk--henkan-candidate-list-active t)
            registration-text display-reading)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-conversion-overlay nil)
            
        (insert nskk-henkan-active-marker "ほけ")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (goto-char (point-max))
        ;; Overlay covers the stem "ほ"; the okuri kana "け" follows it.
        (nskk-state-set-conversion-overlay (make-overlay 2 3))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-state-set-candidates nskk-current-state '("褒"))
        (nskk-state-put-metadata nskk-current-state 'okurigana-in-progress t)
        (nskk-state-put-metadata nskk-current-state 'okurigana-query "ほk")
        (nskk-with-mocks ((nskk-start-registration/k
                           (lambda (text on-done _ignored)
                             (setq registration-text text
                                   display-reading nskk--registration-display-reading)
                             (funcall on-done nil)))  ; registration cancelled
                          (nskk--wrap-to-first-candidate #'ignore)
                          (run-hook-with-args #'ignore))
          (nskk--exhaust-candidates/k #'ignore))
        ;; Dictionary key, not the display form, must be registered:
        ;; lookup appends okuri consonants to the stem ("ほ" + "k").
        (should (equal registration-text "ほk"))
        (should (equal display-reading "ほ*け")))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay))
      ))))

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
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            rollback-called on-done-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert "test")
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk-rollback-conversion (lambda () (setq rollback-called t))))
          (nskk-cancel-conversion/k (lambda () (setq on-done-called t))))
        (should rollback-called)
        (should on-done-called))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      ))))

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
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk--henkan-candidate-list-active nil)
            on-done-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 3)
            
        (insert nskk-henkan-active-marker "かんじ")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (goto-char (point-max))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-state-set-candidates nskk-current-state '("漢字" "感じ"))
        (nskk-with-mocks ((nskk--replace-marker-at #'ignore)
                          (run-hook-with-args #'ignore))
          (nskk-rollback-conversion/k (lambda () (setq on-done-called t))))
        (should on-done-called)
        (should (= (nskk-state-henkan-count) 0)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))))

;;;
;;; nskk-cancel-preedit/k
;;;

(nskk-describe "nskk-cancel-preedit/k"
  (nskk-it "calls on-done even when no conversion start is set"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            on-done-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 0)
            
        (nskk-cancel-preedit/k (lambda () (setq on-done-called t)))
        (should on-done-called))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

  (nskk-it "deletes preedit text and calls on-done when conversion start is active"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            on-done-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 0)
            
        (insert nskk-henkan-on-marker "かんじ")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (goto-char (point-max))
        (nskk-cancel-preedit/k (lambda () (setq on-done-called t)))
        (should on-done-called)
        (should (string-empty-p (buffer-string))))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

  (nskk-it "restores previous mode when cancelling from abbrev preedit"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'abbrev))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 0)
            
        (setf (nskk-state-previous-mode nskk-current-state) 'hiragana)
        (nskk-cancel-preedit/k #'ignore)
        ;; Mode restored to hiragana (the mode before abbrev was activated)
        (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

  (nskk-it "resets henkan-count to 0 after cancel"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 5)
            
        (nskk-cancel-preedit/k #'ignore)
        (should (= (nskk-state-henkan-count) 0)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))))

;;;
;;; nskk--restore-abbrev-mode
;;;

(nskk-describe "nskk--restore-abbrev-mode"
  (nskk-it "restores previous-mode when was-abbrev is t"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'abbrev)))
        (setf (nskk-state-previous-mode nskk-current-state) 'hiragana)
        (nskk--restore-abbrev-mode t)
        (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))))

  (nskk-it "no-op when was-abbrev is nil"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk--restore-abbrev-mode nil)
        (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))))

  (nskk-it "no-op when previous-mode is abbrev"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'abbrev)))
        (setf (nskk-state-previous-mode nskk-current-state) 'abbrev)
        (nskk--restore-abbrev-mode t)
        (should (eq (nskk-state-mode nskk-current-state) 'abbrev)))))

  (nskk-it "clears nskk--numeric-mode when was-abbrev is t"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'abbrev))
            (nskk--numeric-mode t))
        (setf (nskk-state-previous-mode nskk-current-state) 'hiragana)
        (nskk--restore-abbrev-mode t)
        (should-not (nskk-numeric-mode)))))

  (nskk-it "does not clear nskk--numeric-mode when was-abbrev is nil"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--numeric-mode t))
        (nskk--restore-abbrev-mode nil)
        (should (nskk-numeric-mode)))))

  (nskk-it "no-op when previous-mode is nil"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'abbrev)))
        (setf (nskk-state-previous-mode nskk-current-state) nil)
        (nskk--restore-abbrev-mode t)
        (should (eq (nskk-state-mode nskk-current-state) 'abbrev))))))

;;;
;;; nskk-henkan-kakutei abbrev mode restore
;;;

(nskk-describe "nskk-henkan-kakutei abbrev mode restore"
  (nskk-it "restores previous mode after preedit commit from abbrev"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'abbrev))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 0)
            
        (setf (nskk-state-previous-mode nskk-current-state) 'hiragana)
        (insert nskk-henkan-on-marker "test")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (goto-char (point-max))
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-henkan-kakutei)
        (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

  (nskk-it "does not change mode after preedit commit from hiragana"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 0)
            
        (insert nskk-henkan-on-marker "かんじ")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (goto-char (point-max))
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-henkan-kakutei)
        (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))))

;;;
;;; nskk-flush-romaji-before-okuri
;;;

(nskk-describe "nskk-flush-romaji-before-okuri"
  (nskk-it "does nothing when romaji buffer is empty but still clears pending romaji"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-flush-romaji-before-okuri)
        (should (string-empty-p (buffer-string)))
        (should (string-empty-p (nskk-state-romaji-buffer))))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "inserts ん when romaji buffer is standalone n at word boundary"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "n")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-flush-romaji-before-okuri)
        (should (equal (buffer-string) "ん"))
        (should (string-empty-p (nskk-state-romaji-buffer))))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "converts complete romaji to kana and clears buffer"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "ka")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-flush-romaji-before-okuri)
        (should (equal (buffer-string) "か"))
        (should (string-empty-p (nskk-state-romaji-buffer))))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "silently drops incomplete romaji sequences without inserting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "k")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-flush-romaji-before-okuri)
        (should (string-empty-p (buffer-string)))
        (should (string-empty-p (nskk-state-romaji-buffer))))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "converts to katakana when state mode is katakana"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'katakana))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "n")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-flush-romaji-before-okuri)
        (should (equal (buffer-string) "ン"))
        (should (string-empty-p (nskk-state-romaji-buffer))))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      ))))

;;;
;;; nskk--handle-consonant-okuri
;;;

(nskk-describe "nskk--handle-consonant-okuri"
  (nskk-it "puts the consonant into the romaji buffer"
    (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
          (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-pending-romaji-overlay nil)
            
      (nskk-with-mocks ((nskk-show-pending-romaji #'ignore))
        (nskk--handle-consonant-okuri ?k (lambda () nil)))
      (should (equal (nskk-state-romaji-buffer) "k")))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      ))

  (nskk-it "shows the consonant as a pending romaji overlay"
    (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
          (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
          shown-text)
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-pending-romaji-overlay nil)
            
      (nskk-with-mocks ((nskk-show-pending-romaji (lambda (text) (setq shown-text text))))
        (nskk--handle-consonant-okuri ?s (lambda () nil)))
      (should (equal shown-text "s")))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      ))

  (nskk-it "calls on-consumed with no arguments"
    (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
          (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
          on-consumed-called)
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-pending-romaji-overlay nil)
            
      (nskk-with-mocks ((nskk-show-pending-romaji #'ignore))
        (nskk--handle-consonant-okuri ?m (lambda () (setq on-consumed-called t))))
      (should on-consumed-called))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

;;;
;;; nskk-process-okurigana-input/k
;;;

(nskk-describe "nskk-process-okurigana-input/k"
  (nskk-it "calls on-not-found when char is not an okurigana marker"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            not-found-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (nskk-with-mocks ((nskk-detect-okurigana-char (lambda (_c) nil)))
          ;; on-not-found is called with no args (char must come from caller's closure)
          (nskk-process-okurigana-input/k ?a
            #'ignore
            (lambda () (setq not-found-called t))))
        (should not-found-called))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "calls on-not-found when conversion start marker is not active"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            not-found-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        ;; Marker with no position → not active
        (nskk-with-mocks ((nskk-detect-okurigana-char (lambda (_c) ?k)))
          (nskk-process-okurigana-input/k ?K
            #'ignore
            (lambda () (setq not-found-called t))))
        (should not-found-called))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "calls on-found with t for consonant okurigana when conversion start is active"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            on-found-value)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (insert nskk-henkan-on-marker "かく")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (nskk-with-mocks ((nskk-detect-okurigana-char (lambda (_c) ?k))
                          (nskk-prolog-query (lambda (_q) nil))
                          (nskk-insert-marker #'ignore)
                          (nskk-show-pending-romaji #'ignore))
          (nskk-process-okurigana-input/k ?K
            (lambda (v) (setq on-found-value v))
            #'ignore))
        (should (eq on-found-value t))
        (should (equal (nskk-state-romaji-buffer) "k")))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "calls on-not-found when okurigana is already pending (YoNN guard)"
    ;; Regression: second uppercase N in YoNN must NOT re-enter okurigana.
    ;; When okurigana is already set in state, the guard rejects the char.
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            not-found-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "n")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (insert nskk-henkan-on-marker "よ*")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (nskk-state-set-okurigana nskk-current-state ?n)
        (nskk-with-mocks ((nskk-detect-okurigana-char (lambda (_c) ?n)))
          (nskk-process-okurigana-input/k ?N
            (lambda (_v) (error "on-found should not be called"))
            (lambda () (setq not-found-called t))))
        (should not-found-called))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      ))))

;;;
;;; nskk--apply-okuri-candidates
;;;

(nskk-describe "nskk--apply-okuri-candidates"
  (nskk-it "updates overlay, sets active candidates, and sets henkan-count to 1"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-henkan-count 0)
            
        (insert nskk-henkan-on-marker "か*")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (nskk-state-force-henkan-phase nskk-current-state 'on)
        (let* ((start (marker-position (nskk-state-conversion-start-marker)))
               (text-start (+ start (length nskk-henkan-on-marker)))
               (preedit-end (point-max)))
          (nskk-with-mocks ((nskk--remove-okuri-marker #'ignore)
                            (nskk--replace-marker-at #'ignore)
                            (nskk--update-overlay #'ignore))
            (nskk--apply-okuri-candidates start text-start preedit-end
                                          '("書" "欠") "かk"))
          (should (equal (nskk-state-candidates nskk-current-state) '("書" "欠")))
          (should (= (nskk-state-henkan-count) 1))
          (should (nskk-state-get-metadata nskk-current-state 'okurigana-in-progress))
          (should (eq (nskk-state-henkan-phase nskk-current-state) 'active))))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      ))))

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
;;; nskk-trigger-okuri-conversion/k
;;;

(nskk-describe "nskk-trigger-okuri-conversion/k"
  (nskk-it "calls on-not-found immediately when no preedit query can be built"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            on-not-found-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            
        ;; Marker with no position → extract-okuri-query returns nil → on-not-found
        (nskk-trigger-okuri-conversion/k ?k (point)
                                          #'ignore
                                          (lambda () (setq on-not-found-called t))
                                          #'ignore)
        (should on-not-found-called))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker))
      )))

  (nskk-it "calls on-found with candidates when search finds results"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            on-found-candidates)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-henkan-count 0)
            
        (insert nskk-henkan-on-marker "かく")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (let ((preedit-end (point-max)))
          (nskk-state-force-henkan-phase nskk-current-state 'on)
          (nskk-with-mocks ((nskk-core-search/k
                             (lambda (_q _type _limit on-found _on-not-found)
                               (funcall on-found '("書く"))))
                            (nskk--replace-marker-at #'ignore)
                            (nskk--remove-okuri-marker #'ignore)
                            (nskk--update-overlay #'ignore))
            (nskk-trigger-okuri-conversion/k ?k preedit-end
                                              (lambda (candidates)
                                                (setq on-found-candidates candidates))
                                              #'ignore
                                              #'ignore)))
        (should (equal on-found-candidates '("書く"))))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

  (nskk-it "calls on-not-found when registration is cancelled"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk-test-saved-registration-depth (nskk-state-registration-depth))
            on-not-found-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-henkan-count 0)
            (nskk-state-set-registration-depth 0)
            
        (insert nskk-henkan-on-marker "ほ*")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (let ((preedit-end (- (point-max) 1)))
          (nskk-state-force-henkan-phase nskk-current-state 'on)
          (nskk-with-mocks ((nskk-core-search/k
                             (lambda (_q _type _limit _on-found on-not-found)
                               (funcall on-not-found)))
                            (nskk-start-registration/k
                             (lambda (_reading on-done _on-fail)
                               (funcall on-done nil)))  ; nil = cancelled
                            (nskk--remove-okuri-marker #'ignore))
            (nskk-trigger-okuri-conversion/k ?k preedit-end
                                              #'ignore
                                              (lambda () (setq on-not-found-called t))
                                              #'ignore)))
        (should on-not-found-called))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      )))

  (nskk-it "calls on-register after successful registration"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk-test-saved-registration-depth (nskk-state-registration-depth))
            on-register-called)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-henkan-count 0)
            (nskk-state-set-registration-depth 0)
            
        (insert nskk-henkan-on-marker "ほ*")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
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
            (nskk-trigger-okuri-conversion/k ?k preedit-end
                                              #'ignore
                                              #'ignore
                                              (lambda () (setq on-register-called t)))))
        (should on-register-called))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))))

;;;
;;; nskk--handle-vowel-okuri/k
;;;

(nskk-describe "nskk--handle-vowel-okuri/k"
  (nskk-it "calls on-consumed after converting the vowel kana"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            on-consumed-called)
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-with-mocks
            ((nskk-convert-input-to-kana-final/k
              (lambda (cont _ignored) (funcall cont "あ")))
             (nskk-trigger-okuri-conversion #'ignore)
             (nskk--update-overlay #'ignore))
          (nskk--handle-vowel-okuri/k ?a
            (lambda () (setq on-consumed-called t))))
        (should on-consumed-called))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "inserts the kana string into the current buffer"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-with-mocks
            ((nskk-convert-input-to-kana-final/k
              (lambda (cont _ignored) (funcall cont "い")))
             (nskk-trigger-okuri-conversion #'ignore)
             (nskk--update-overlay #'ignore))
          (nskk--handle-vowel-okuri/k ?i #'ignore))
        (should (string-match-p "い" (buffer-string))))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "converts to katakana when state mode is katakana"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'katakana))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
            _inserted-text)
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-with-mocks
            ((nskk-convert-input-to-kana-final/k
              (lambda (cont _ignored) (funcall cont "う")))
             (nskk-trigger-okuri-conversion #'ignore)
             (nskk--update-overlay #'ignore))
          (nskk--handle-vowel-okuri/k ?u #'ignore))
        ;; In katakana mode, hiragana う is converted to katakana ウ before insert
        (should (string-match-p "ウ" (buffer-string))))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      ))))

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

;; Task 3: registration roundtrip coverage.
;;
;; Tests 3a and 3b cover the registration and dictionary lookup seams at unit
;; level.  The complete registration-to-reuse flow is covered by
;; `registered word is committed and dict entry persists for immediate reuse'
;; in test/e2e/nskk-registration-e2e-test.el:258.

(nskk-describe "henkan registration roundtrip"
  (nskk-it "3a: start-conversion fires registration path for unknown reading"
    ;; When nskk-core-search finds nothing, nskk-start-registration/k should be
    ;; called with the preedit reading.  We mock both to capture the reading.
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-henkan-count (nskk-state-henkan-count))
            (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
            (nskk-test-saved-registration-depth (nskk-state-registration-depth))
            registration-reading)
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-henkan-count 0)
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-registration-depth 0)
            
        (insert nskk-henkan-on-marker "みとうろく")
        (set-marker (nskk-state-conversion-start-marker) (point-min))
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
        (should (equal registration-reading "みとうろく")))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      )))

  (nskk-it "3b: dict-register-word then core-search/k returns registered word"
    ;; This test uses a real isolated Prolog DB to verify the full dict round-
    ;; trip: register a word, then query it back via nskk-core-search/k.
    ;; It relies on nskk-prolog-test-with-isolated-db to scope DB changes.
    (nskk-prolog-test-with-isolated-db
      ;; Reset all initialized flags so nskk-mode reinitialises cleanly.
      (let ((nskk--input-initialized nil)
            (nskk--state-prolog-initialized nil)
            (nskk--henkan-initialized nil)
            (nskk--kana-initialized nil)
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
            (nskk-mode -1)))))))

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
;; (e.g. "k", "sh") was in nskk-state-romaji-buffer when an okurigana trigger
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
          (nskk-set-conversion-start-marker (point-min))
          ;; Put the state into henkan-on phase (reading in progress)
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          ;; Simulate a pending incomplete romaji consonant "k"
          (nskk-state-set-romaji-buffer "k")
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
          (nskk-set-conversion-start-marker (point-min))
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          ;; "sh" is an incomplete romaji prefix (needs vowel to complete shi/sha/shu etc.)
          (nskk-state-set-romaji-buffer "sh")
          (nskk-process-okurigana-input ?K)
          (let ((content (buffer-string)))
            (should-not (string-match-p "sh" content))
            (should (string-match-p "\\*" content))))))

    (nskk-it "pending n is converted to ん and inserted before *"
      ;; T-U3: "n" (standalone n at word boundary) + uppercase K → "ん" IS inserted before *
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana)))
          (insert "\u25BD\u304B")
          (nskk-set-conversion-start-marker (point-min))
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          ;; "n" alone is the ん exception: it should flush as ん before the marker
          (nskk-state-set-romaji-buffer "n")
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
          (nskk-set-conversion-start-marker (point-min))
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          ;; No pending romaji
          (nskk-state-set-romaji-buffer "")
          (nskk-process-okurigana-input ?K)
          (let ((content (buffer-string)))
            ;; * marker must be present
            (should (string-match-p "\\*" content))
            ;; No raw ASCII consonant should appear adjacent to *
            (should-not (string-match-p "[a-z]\\*\\|\\*[a-z]" content))))))))

;;;
;;; nskk-reset-romaji-buffer Tests
;;;

(nskk-describe "nskk-reset-romaji-buffer"
  (nskk-it "sets nskk-state-romaji-buffer to empty string"
    (with-temp-buffer
      (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "ka")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-reset-romaji-buffer)
        (should (equal (nskk-state-romaji-buffer) "")))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      )))

  (nskk-it "calls nskk-clear-pending-romaji"
    (let ((cleared nil)
          (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "")
            
      (nskk-with-mocks ((nskk-clear-pending-romaji (lambda () (setq cleared t))))
        (nskk-reset-romaji-buffer))
      (should cleared))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      ))

  (nskk-it "is idempotent when buffer is already empty"
    (with-temp-buffer
      (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
            (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "")
            (nskk-state-set-pending-romaji-overlay nil)
            
        (nskk-reset-romaji-buffer)
        (nskk-reset-romaji-buffer)
        (should (equal (nskk-state-romaji-buffer) "")))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay))
      ))))

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

(nskk-describe "nskk--run-registration-session/k"
  (nskk-it "calls on-found with nil when depth is at maximum"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-test-saved-registration-depth (nskk-state-registration-depth))
          (result 'unset))
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth nskk-max-registration-depth)
            
      (nskk--run-registration-session/k "てすと"
        (lambda (r) (setq result r))
        #'ignore)
      (should-not result))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))

  (nskk-it "calls on-found with nil when user enters empty string"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-test-saved-registration-depth (nskk-state-registration-depth))
          (result 'unset))
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 0)
            
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk-with-mocks ((read-from-minibuffer (lambda (_p) ""))
                        (nskk-dict-register-word #'ignore))
        (nskk--run-registration-session/k "てすと"
          (lambda (r) (setq result r))
          (lambda () (error "on-not-found must not be called"))))
      (should-not result))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))

  (nskk-it "calls on-found with the entered word when user provides input"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-test-saved-registration-depth (nskk-state-registration-depth))
          (result nil))
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 0)
            
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk-with-mocks ((read-from-minibuffer (lambda (_p) "漢字"))
                        (nskk-dict-register-word #'ignore))
        (nskk--run-registration-session/k "かんじ"
          (lambda (r) (setq result r))
          (lambda () (error "on-not-found must not be called"))))
      (should (equal result "漢字")))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))

  (nskk-it "increments and decrements depth atomically"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-test-saved-registration-depth (nskk-state-registration-depth))
          depth-during)
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 0)
            
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk-with-mocks ((read-from-minibuffer
                         (lambda (_p) (setq depth-during (nskk-state-registration-depth)) ""))
                        (nskk-dict-register-word #'ignore))
        (nskk--run-registration-session/k "てすと"
          (lambda (_r) nil)
          (lambda () (error "on-not-found must not be called"))))
      (should (= depth-during 1))
      (should (= (nskk-state-registration-depth) 0)))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))

  (nskk-it "calls on-found with nil and restores depth on C-g (quit signal)"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-test-saved-registration-depth (nskk-state-registration-depth))
          (result 'unset))
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 0)
            
      (nskk-state-force-henkan-phase nskk-current-state 'on)
      (nskk-with-mocks ((read-from-minibuffer (lambda (_p) (signal 'quit nil)))
                        (nskk-dict-register-word #'ignore))
        (nskk--run-registration-session/k "てすと"
          (lambda (r) (setq result r))
          (lambda () (error "on-not-found must not be called"))))
      (should (null result))
      (should (= (nskk-state-registration-depth) 0)))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))

  (nskk-it "preserves the primary condition and all surrounding state across cleanup faults"
    (dolist (primary-type '(error quit))
      (dolist (cleanup-type '(error quit))
        (dolist (timing '(before after))
          (with-temp-buffer
            (let* ((nskk-current-state (nskk-state-create 'abbrev))
                   (nskk-test-saved-registration-depth (nskk-state-registration-depth))
                   (marker (copy-marker (point-min)))
                   (marker-position (marker-position marker))
                   (conversion-overlay (make-overlay (point-min) (point-min)))
                   (pending-overlay (make-overlay (point-min) (point-min)))
                   (inline-overlay (make-overlay (point-min) (point-min)))
                   (candidates (list "candidate-1" "candidate-2"))
                   (azik-deferred (list 'azik-deferred))
                   (deferred-azik-state (list 'deferred-azik-state))
                   (deferred-vowel-state (list 'deferred-vowel-state))
                   (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
                   (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
                   (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
                   (nskk--inline-overlay inline-overlay)
                   (nskk--henkan-candidate-list-active t)
                   (nskk-test-saved-henkan-count (nskk-state-henkan-count))
                   (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
                   (nskk--azik-colon-okuri-pending t)
                   (nskk--azik-colon-okuri-deferred azik-deferred)
                   (nskk--azik-sokuon-okuri-kana-pending t)
                   (nskk--deferred-azik-state deferred-azik-state)
                   (nskk--deferred-vowel-shadow-state deferred-vowel-state)
                   (nskk--sticky-shift-pending t)
                   (nskk--numeric-mode t)
                   (primary-payload (list 'primary-payload))
                   (primary-data (list "primary-condition" primary-payload))
                   (inline-data (list "inline-cleanup" (list 'inline-payload)))
                   (phase-data (list "phase-cleanup" (list 'phase-payload)))
                   (real-force
                    (symbol-function 'nskk-state-force-henkan-phase))
                   (cleanup-inhibit-quit nil)
                   (inline-calls 0)
                   (restore-calls 0)
                   callback-called
                   caught)
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 0)
            (nskk-state-set-conversion-start-marker marker)
            (nskk-state-set-conversion-overlay conversion-overlay)
            (nskk-state-set-pending-romaji-overlay pending-overlay)
            (nskk-state-set-henkan-count 4)
            (nskk-state-set-romaji-buffer "k")
            
              (setf (nskk-state-candidates nskk-current-state) candidates
                    (nskk-state-current-index nskk-current-state) 1
                    (nskk-state-previous-mode nskk-current-state) 'katakana)
              (nskk-state-force-henkan-phase nskk-current-state 'on)
              (cl-letf (((symbol-function 'nskk-inline-show-registration-badge)
                         #'ignore)
                        ((symbol-function 'nskk--read-registration-entry)
                         (lambda (_reading)
                           (signal primary-type primary-data)))
                        ((symbol-function 'nskk-inline-hide)
                         (lambda ()
                           (cl-incf inline-calls)
                           (push inhibit-quit cleanup-inhibit-quit)
                           (when (eq timing 'after)
                             (delete-overlay nskk--inline-overlay)
                             (setq nskk--inline-overlay nil))
                           (signal cleanup-type inline-data)))
                        ((symbol-function 'nskk-state-force-henkan-phase)
                         (lambda (state phase)
                           (if (eq phase 'registration)
                               (funcall real-force state phase)
                             (cl-incf restore-calls)
                             (push inhibit-quit cleanup-inhibit-quit)
                             (when (eq timing 'after)
                               (funcall real-force state phase))
                             (signal (if (eq cleanup-type 'error)
                                         'quit
                                       'error)
                                     phase-data)))))
                (condition-case condition
                    (nskk--run-registration-session/k "reading"
                      (lambda (_result) (setq callback-called t))
                      #'ignore)
                  ((error quit)
                   (setq caught condition))))
              (should (eq (car caught) primary-type))
              (should (eq (cdr caught) primary-data))
              (should (eq (caddr caught) primary-payload))
              (should (= inline-calls 1))
              (should (= restore-calls 1))
              (should-not (memq nil cleanup-inhibit-quit))
              (should-not callback-called)
              (should (= (nskk-state-registration-depth) 0))
              (should (eq (nskk-state-henkan-phase nskk-current-state) 'on))
              (should-not nskk--inline-overlay)
              (should-not (overlay-buffer inline-overlay))
              (should (eq (nskk-state-conversion-start-marker) marker))
              (should (= (marker-position marker) marker-position))
              (should (eq (nskk-state-conversion-overlay) conversion-overlay))
              (should (eq (overlay-buffer conversion-overlay) (current-buffer)))
              (should (eq (nskk-state-pending-romaji-overlay) pending-overlay))
              (should (eq (overlay-buffer pending-overlay) (current-buffer)))
              (should nskk--henkan-candidate-list-active)
              (should (eq (nskk-state-candidates nskk-current-state)
                          candidates))
              (should (= (nskk-state-current-index nskk-current-state) 1))
              (should (= (nskk-state-henkan-count) 4))
              (should (equal (nskk-state-romaji-buffer) "k"))
              (should (nskk-azik-colon-okuri-pending))
              (should (eq (nskk-azik-colon-okuri-deferred) azik-deferred))
              (should (nskk-azik-sokuon-okuri-kana-pending))
              (should (eq (nskk-deferred-azik-state) deferred-azik-state))
              (should (eq (nskk-deferred-vowel-shadow-state)
                          deferred-vowel-state))
              (should (nskk-sticky-shift-pending))
              (should (nskk-numeric-mode))
              (should (eq (nskk-state-mode nskk-current-state) 'abbrev))
              (should
               (eq (nskk-state-previous-mode nskk-current-state) 'katakana)))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth)
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      ))))))

  (nskk-it "re-signals the first cleanup condition after a successful body"
    (dolist (cleanup-type '(error quit))
      (dolist (timing '(before after))
        (with-temp-buffer
          (let* ((nskk-current-state (nskk-state-create 'hiragana))
                 (nskk-test-saved-registration-depth (nskk-state-registration-depth))
                 (inline-overlay (make-overlay (point-min) (point-min)))
                 (nskk--inline-overlay inline-overlay)
                 (first-payload (list 'first-cleanup-payload))
                 (first-data (list "first-cleanup" first-payload))
                 (later-data (list "later-cleanup" (list 'later-payload)))
                 (real-force
                  (symbol-function 'nskk-state-force-henkan-phase))
                 (cleanup-inhibit-quit nil)
                 (commit-calls 0)
                 callback-called
                 caught)
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 0)
            
            (nskk-state-force-henkan-phase nskk-current-state 'on)
            (cl-letf (((symbol-function 'nskk-inline-show-registration-badge)
                       #'ignore)
                      ((symbol-function 'nskk--read-registration-entry)
                       (lambda (_reading) "word"))
                      ((symbol-function 'nskk--commit-registration-word)
                       (lambda (_reading _entry)
                         (cl-incf commit-calls)))
                      ((symbol-function 'nskk-inline-hide)
                       (lambda ()
                         (push inhibit-quit cleanup-inhibit-quit)
                         (when (eq timing 'after)
                           (delete-overlay nskk--inline-overlay)
                           (setq nskk--inline-overlay nil))
                         (signal cleanup-type first-data)))
                      ((symbol-function 'nskk-state-force-henkan-phase)
                       (lambda (state phase)
                         (if (eq phase 'registration)
                             (funcall real-force state phase)
                           (push inhibit-quit cleanup-inhibit-quit)
                           (funcall real-force state phase)
                           (signal (if (eq cleanup-type 'error)
                                       'quit
                                     'error)
                                   later-data)))))
              (condition-case condition
                  (nskk--run-registration-session/k "reading"
                    (lambda (_result) (setq callback-called t))
                    #'ignore)
                ((error quit)
                 (setq caught condition))))
            (should (= commit-calls 1))
            (should-not callback-called)
            (should (eq (car caught) cleanup-type))
            (should (eq (cdr caught) first-data))
            (should (eq (caddr caught) first-payload))
            (should-not (memq nil cleanup-inhibit-quit))
            (should (= (nskk-state-registration-depth) 0))
            (should (eq (nskk-state-henkan-phase nskk-current-state) 'on))
            (should-not nskk--inline-overlay)
            (should-not (overlay-buffer inline-overlay)))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      )))))

  (nskk-it "allows a clean retry after a failed session"
    (with-temp-buffer
      (let* ((nskk-current-state (nskk-state-create 'hiragana))
             (nskk-test-saved-registration-depth (nskk-state-registration-depth))
             (inline-overlay (make-overlay (point-min) (point-min)))
             (nskk--inline-overlay inline-overlay)
             (primary-payload (list 'retry-primary-payload))
             (primary-data (list "retry-primary" primary-payload))
             (cleanup-data (list "retry-cleanup"))
             (attempts 0)
             (hide-calls 0)
             (results nil)
             caught)
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 0)
            
        (nskk-state-force-henkan-phase nskk-current-state 'on)
        (cl-letf (((symbol-function 'nskk-inline-show-registration-badge)
                   #'ignore)
                  ((symbol-function 'nskk--read-registration-entry)
                   (lambda (_reading)
                     (cl-incf attempts)
                     (if (= attempts 1)
                         (signal 'error primary-data)
                       "word")))
                  ((symbol-function 'nskk--commit-registration-word)
                   #'ignore)
                  ((symbol-function 'nskk-inline-hide)
                   (lambda ()
                     (cl-incf hide-calls)
                     (if (= hide-calls 1)
                         (signal 'quit cleanup-data)
                       (when (overlayp nskk--inline-overlay)
                         (delete-overlay nskk--inline-overlay))
                       (setq nskk--inline-overlay nil)))))
          (condition-case condition
              (nskk--run-registration-session/k "reading"
                (lambda (result) (push result results))
                #'ignore)
            ((error quit)
             (setq caught condition)))
          (should (eq (car caught) 'error))
          (should (eq (cdr caught) primary-data))
          (should (eq (caddr caught) primary-payload))
          (should (= (nskk-state-registration-depth) 0))
          (should (eq (nskk-state-henkan-phase nskk-current-state) 'on))
          (should-not nskk--inline-overlay)
          (should-not (overlay-buffer inline-overlay))
          (nskk--run-registration-session/k "reading"
            (lambda (result) (push result results))
            #'ignore))
        (should (= attempts 2))
        (should (= hide-calls 2))
        (should (equal results '("word")))
        (should (= (nskk-state-registration-depth) 0))
        (should (eq (nskk-state-henkan-phase nskk-current-state) 'on)))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))))

;;;
;;; nskk--read-registration-entry Tests
;;;

(nskk-describe "nskk--read-registration-entry"
  (nskk-it "returns nil on C-g (quit signal)"
    (let ((nskk-test-saved-registration-depth (nskk-state-registration-depth)))
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 0)
            
      (nskk-with-mocks ((read-from-minibuffer (lambda (_p) (signal 'quit nil))))
        (should-not (nskk--read-registration-entry "てすと"))))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))

  (nskk-it "returns nil on empty string"
    (let ((nskk-test-saved-registration-depth (nskk-state-registration-depth)))
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 0)
            
      (nskk-with-mocks ((read-from-minibuffer (lambda (_p) "")))
        (should-not (nskk--read-registration-entry "てすと"))))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      ))

  (nskk-it "returns entry string when user provides input"
    (let ((nskk-test-saved-registration-depth (nskk-state-registration-depth)))
      (unwind-protect
          (progn
            (nskk-state-set-registration-depth 0)
            
      (nskk-with-mocks ((read-from-minibuffer (lambda (_p) "漢字")))
        (should (equal (nskk--read-registration-entry "かんじ") "漢字"))))
        (nskk-state-set-registration-depth nskk-test-saved-registration-depth))
      )))

;;;
;;; registration minibuffer keymap Tests
;;;

(nskk-describe "registration minibuffer keymap"
  (nskk-it "locks reg-map bindings (C-g, RET, C-j, parent) for kana-input registration"
    (let* ((exit-fn #'ignore)
           (reg-map (let ((map (make-sparse-keymap)))
                      (set-keymap-parent map nskk-mode-map)
                      (define-key map (kbd "C-j") exit-fn)
                      (define-key map (kbd "RET") exit-fn)
                      (define-key map (kbd "C-g") #'abort-recursive-edit)
                      map)))
      (should (eq (lookup-key reg-map (kbd "C-g")) #'abort-recursive-edit))
      (should (eq (lookup-key reg-map (kbd "RET")) exit-fn))
      (should (eq (lookup-key reg-map (kbd "C-j")) exit-fn))
      (should (eq (keymap-parent reg-map) nskk-mode-map)))))

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
        (nskk-set-mode 'hiragana)
        (nskk-without-modification
          (insert nskk-henkan-on-marker "かんじ"))
        (nskk-set-conversion-start-marker (point-min))
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-henkan-kakutei-convert-script)
        (should (null (nskk-state-henkan-phase nskk-current-state)))
        (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
        (should (string= (buffer-string) "カンジ")))))

  (nskk-it "in katakana mode: converts preedit katakana to hiragana and commits"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-set-mode 'katakana)
        (nskk-without-modification
          (insert nskk-henkan-on-marker "カンジ"))
        (nskk-set-conversion-start-marker (point-min))
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

(nskk-describe "nskk-preedit-ends-with-plain-vowel-p"
  (nskk-it "returns nil when no preedit marker is set"
    (with-temp-buffer
      (let ((nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer)))
      (unwind-protect
          (progn
            (nskk-state-set-romaji-buffer "")
            
        (should-not (nskk-preedit-ends-with-plain-vowel-p)))
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      )))

  (nskk-it "returns nil when romaji buffer is non-empty"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "k")
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert nskk-henkan-on-marker "か")
        (should-not (nskk-preedit-ends-with-plain-vowel-p)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      )))

  (nskk-it "returns nil when preedit ends with non-vowel kana (か)"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert nskk-henkan-on-marker "か")
        (should-not (nskk-preedit-ends-with-plain-vowel-p)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      )))

  (nskk-it "returns non-nil for reading ending with あ (empty romaji)"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert nskk-henkan-on-marker "あ")
        (should (nskk-preedit-ends-with-plain-vowel-p)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      )))

  (nskk-it "returns non-nil for compound reading ending with い (かい)"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert nskk-henkan-on-marker "かい")
        (should (nskk-preedit-ends-with-plain-vowel-p)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      )))

  (nskk-it "returns non-nil for reading ending with ー (prolonged vowel)"
    (with-temp-buffer
      (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
            (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (insert nskk-henkan-on-marker "あー")
        (should (nskk-preedit-ends-with-plain-vowel-p)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      )))

  (nskk-it "returns non-nil for all hiragana plain vowels"
    (dolist (ch '(?あ ?い ?う ?え ?お))
      (with-temp-buffer
        (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert nskk-henkan-on-marker (char-to-string ch))
          (should (nskk-preedit-ends-with-plain-vowel-p)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      ))))

  (nskk-it "returns non-nil for all katakana plain vowels"
    (dolist (ch '(?ア ?イ ?ウ ?エ ?オ))
      (with-temp-buffer
        (let ((nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
              (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-romaji-buffer "")
            
          (set-marker (nskk-state-conversion-start-marker) (point-min))
          (insert nskk-henkan-on-marker (char-to-string ch))
          (should (nskk-preedit-ends-with-plain-vowel-p)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer))
      )))))

;;;
;;; SKK Numeric Conversion (数値変換)
;;;

(nskk-describe "nskk--numeric-parse-reading"
  (nskk-it "parses single-digit reading into num-str and base-key"
    (should (equal (nskk--numeric-parse-reading "#1ko") '("1" . "#ko"))))

  (nskk-it "parses multi-digit reading"
    (should (equal (nskk--numeric-parse-reading "#123ji") '("123" . "#ji"))))

  (nskk-it "parses two-digit reading with suffix"
    (should (equal (nskk--numeric-parse-reading "#10ko") '("10" . "#ko"))))

  (nskk-it "returns nil when no # prefix"
    (should (null (nskk--numeric-parse-reading "1ko"))))

  (nskk-it "returns nil for plain kana reading"
    (should (null (nskk--numeric-parse-reading "こ"))))

  (nskk-it "returns nil when # has no following digits"
    (should (null (nskk--numeric-parse-reading "#ko")))))

(nskk-describe "nskk--numeric-to-kanji"
  (nskk-deftest-table numeric-to-kanji
    :description "Each digit converts to its kanji equivalent"
    :columns (input expected)
    :rows (("0" "〇")
           ("1" "一")
           ("5" "五")
           ("9" "九")
           ("10" "一〇")
           ("12" "一二")
           ("1024" "一〇二四")
           ("9999" "九九九九"))
    :body (should (equal (nskk--numeric-to-kanji input) expected))))

(nskk-describe "nskk--numeric-to-fullwidth"
  (nskk-deftest-table numeric-to-fullwidth
    :description "Each digit shifts to full-width Unicode range"
    :columns (input expected)
    :rows (("0" "０")
           ("1" "１")
           ("9" "９")
           ("123" "１２３")
           ("1024" "１０２４"))
    :body (should (equal (nskk--numeric-to-fullwidth input) expected))))

(nskk-describe "nskk--n-to-kanji-place"
  (nskk-it "converts single-digit integers"
    (should (equal (nskk--n-to-kanji-place 1) "一"))
    (should (equal (nskk--n-to-kanji-place 9) "九")))

  (nskk-it "drops leading 一 for 十 (10 → 十 not 一十)"
    (should (equal (nskk--n-to-kanji-place 10) "十")))

  (nskk-it "produces correct tens with remainder"
    (should (equal (nskk--n-to-kanji-place 11) "十一"))
    (should (equal (nskk--n-to-kanji-place 20) "二十"))
    (should (equal (nskk--n-to-kanji-place 21) "二十一")))

  (nskk-it "drops leading 一 for 百 (100 → 百 not 一百)"
    (should (equal (nskk--n-to-kanji-place 100) "百")))

  (nskk-it "produces correct hundreds"
    (should (equal (nskk--n-to-kanji-place 101) "百一"))
    (should (equal (nskk--n-to-kanji-place 200) "二百"))
    (should (equal (nskk--n-to-kanji-place 110) "百十")))

  (nskk-it "drops leading 一 for 千 (1000 → 千 not 一千)"
    (should (equal (nskk--n-to-kanji-place 1000) "千")))

  (nskk-it "produces correct thousands"
    (should (equal (nskk--n-to-kanji-place 1001) "千一"))
    (should (equal (nskk--n-to-kanji-place 2000) "二千"))
    (should (equal (nskk--n-to-kanji-place 1024) "千二十四")))

  (nskk-it "keeps leading 一 for 万 (10000 → 一万)"
    (should (equal (nskk--n-to-kanji-place 10000) "一万")))

  (nskk-it "produces correct ten-thousands"
    (should (equal (nskk--n-to-kanji-place 20000) "二万"))
    (should (equal (nskk--n-to-kanji-place 12345) "一万二千三百四十五"))))

(nskk-describe "nskk--numeric-to-place-values"
  (nskk-it "handles zero as special case 〇"
    (should (equal (nskk--numeric-to-place-values "0") "〇")))

  (nskk-deftest-table numeric-to-place-values
    :description "Converts number string to kanji place values"
    :columns (input expected)
    :rows (("1"     "一")
           ("10"    "十")
           ("100"   "百")
           ("1000"  "千")
           ("1024"  "千二十四")
           ("10000" "一万"))
    :body (should (equal (nskk--numeric-to-place-values input) expected))))

(nskk-describe "nskk--numeric-convert"
  (nskk-deftest-table numeric-convert-dispatch
    :description "Dispatches on type code to correct conversion"
    :columns (type input expected)
    :rows ((0 "42" "42")         ; #0 = literal (no change)
           (1 "42" "４２")       ; #1 = full-width
           (2 "42" "四二")       ; #2 = kanji digit-by-digit
           (3 "42" "四十二")     ; #3 = kanji place values
           (4 "42" "四二")       ; #4 = same as #2
           (8 "42" "42")         ; #8 falls through to literal
           (9 "42" "42"))        ; unknown type falls through to literal
    :body (should (equal (nskk--numeric-convert input type) expected)))

  (nskk-it "type 2 and type 4 produce identical output"
    (should (equal (nskk--numeric-convert "1024" 2)
                   (nskk--numeric-convert "1024" 4)))))

(nskk-describe "nskk--numeric-process-candidate"
  (nskk-it "replaces single #N pattern in template"
    (should (equal (nskk--numeric-process-candidate "第#3時" "1") "第一時")))

  (nskk-it "replaces #0 pattern as literal"
    (should (equal (nskk--numeric-process-candidate "#0個" "42") "42個")))

  (nskk-it "replaces #1 pattern as full-width"
    (should (equal (nskk--numeric-process-candidate "#1時" "3") "３時")))

  (nskk-it "replaces #2 pattern as kanji digit-by-digit"
    (should (equal (nskk--numeric-process-candidate "#2個" "10") "一〇個")))

  (nskk-it "replaces #3 pattern as kanji place values"
    (should (equal (nskk--numeric-process-candidate "#3個" "10") "十個")))

  (nskk-it "leaves template unchanged when no #N pattern present"
    (should (equal (nskk--numeric-process-candidate "そのまま" "42") "そのまま")))

  (nskk-it "replaces multiple #N patterns in a single candidate"
    (should (equal (nskk--numeric-process-candidate "#1と#2" "5") "５と五"))))

(nskk-describe "nskk--numeric-process-candidates"
  (nskk-it "processes a list of candidates with the same num-str"
    (should (equal (nskk--numeric-process-candidates '("#0個" "#2個") "10")
                   '("10個" "一〇個"))))

  (nskk-it "processes mixed-type candidate list"
    (should (equal (nskk--numeric-process-candidates '("#0個" "#2個" "#3個") "10")
                   '("10個" "一〇個" "十個"))))

  (nskk-it "returns empty list for empty candidate list"
    (should (equal (nskk--numeric-process-candidates '() "42") '()))))

;;;
;;; Section: undo-kakutei
;;;

(nskk-describe "nskk--last-kakutei-record"
  (nskk-it "is nil by default"
    (with-temp-buffer
      (should (null nskk--last-kakutei-record))))

  (nskk-it "can be set and read as a plist"
    (with-temp-buffer
      (setq nskk--last-kakutei-record
            (list :reading "かんじ" :candidates '("漢字")
                  :index 0 :committed-text "漢字"
                  :buffer-start 1 :buffer-end 3
                  :mode 'hiragana :registered-p nil
                  :registered-reading nil
                  :registered-word nil))
      (should (equal "かんじ"
                     (plist-get nskk--last-kakutei-record
                                :reading)))
      (should (equal '("漢字")
                     (plist-get nskk--last-kakutei-record
                                :candidates))))))

(nskk-describe "nskk-invalidate-undo-kakutei"
  (nskk-it "clears a non-nil record"
    (with-temp-buffer
      (setq nskk--last-kakutei-record '(:reading "x"))
      (nskk-invalidate-undo-kakutei)
      (should (null nskk--last-kakutei-record))))

  (nskk-it "is a no-op when record is already nil"
    (with-temp-buffer
      (setq nskk--last-kakutei-record nil)
      (nskk-invalidate-undo-kakutei)
      (should (null nskk--last-kakutei-record)))))

(nskk-describe "nskk-undo-kakutei"
  (nskk-it "falls through to undo when no record exists"
    (with-temp-buffer
      (setq nskk--last-kakutei-record nil)
      ;; undo with no undo info signals user-error
      (should-error (nskk-undo-kakutei) :type 'user-error)))

  (nskk-it "restores buffer text and sets active phase"
    (with-temp-buffer
      (setq-local nskk-current-state (nskk-state-create 'hiragana))
      (insert "漢字")
      (setq nskk--last-kakutei-record
            (list :reading "かんじ"
                  :candidates '("漢字" "感じ")
                  :index 0
                  :committed-text "漢字"
                  :buffer-start 1
                  :buffer-end 3
                  :mode 'hiragana
                  :registered-p nil
                  :registered-reading nil
                  :registered-word nil))
      (nskk-undo-kakutei)
      ;; Record should be invalidated
      (should (null nskk--last-kakutei-record))
      ;; Phase should be active (▼)
      (should (eq (nskk-state-henkan-phase nskk-current-state)
                  'active))
      ;; Candidates restored
      (should (equal '("漢字" "感じ")
                     (nskk-state-candidates nskk-current-state)))
      ;; Index restored
      (should (= 0 (nskk-state-current-index nskk-current-state)))
      ;; Buffer should contain ▼ + candidate
      (should (string-match-p "▼漢字"
                              (buffer-substring-no-properties
                               (point-min) (point-max))))))

  (nskk-it "restores the okurigana suffix after the candidate"
    (with-temp-buffer
      (setq-local nskk-current-state (nskk-state-create 'hiragana))
      ;; Committed okurigana conversion: candidate "書" + okuri "く".
      (insert "書く")
      (setq nskk--last-kakutei-record
            (list :reading "かk"
                  :candidates '("書" "描")
                  :index 0
                  :committed-text "書く"
                  :okuri-kana "く"
                  :buffer-start 1
                  :buffer-end 3
                  :mode 'hiragana
                  :registered-p nil
                  :registered-reading nil
                  :registered-word nil))
      (nskk-undo-kakutei)
      ;; The okurigana kana must survive the undo: ▼ + 書 + く.
      (should (equal "▼書く"
                     (buffer-substring-no-properties
                      (point-min) (point-max))))
      ;; Overlay covers only the candidate, not the okuri kana.
      (should (overlayp (nskk-state-conversion-overlay)))
      (should (= (overlay-end (nskk-state-conversion-overlay)) 3))
      ;; Okurigana metadata restored for consistent follow-up commits.
      (should (nskk-state-get-metadata nskk-current-state
                                       'okurigana-in-progress))
      (should (equal "かk"
                     (nskk-state-get-metadata nskk-current-state
                                              'okurigana-query)))))

  (nskk-it "does not revert when buffer text has changed"
    (with-temp-buffer
      (setq-local nskk-current-state (nskk-state-create 'hiragana))
      (insert "modified")
      (let ((record
             (list :reading "かんじ"
                   :candidates '("漢字")
                   :index 0
                   :committed-text "漢字"
                   :buffer-start 1
                   :buffer-end 3
                   :mode 'hiragana
                   :registered-p nil
                   :registered-reading nil
                   :registered-word nil)))
        (setq nskk--last-kakutei-record record)
        (nskk-undo-kakutei)
        ;; A mismatch is not a completed undo and must remain retryable.
        (should (eq record nskk--last-kakutei-record))
        (should (equal "modified"
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))))

  (nskk-it "unregisters a word when registered-p is set"
    (nskk-prolog-test-with-isolated-db
      (let ((nskk--user-dict-index 'user)
            (nskk-dict-modified nil))
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-dict-register-word "てすと" "テスト")
        (with-temp-buffer
          (setq-local nskk-current-state
                      (nskk-state-create 'hiragana))
          (insert "テスト")
          (setq nskk--last-kakutei-record
                (list :reading "てすと"
                      :candidates '("テスト")
                      :index 0
                      :committed-text "テスト"
                      :buffer-start 1
                      :buffer-end 4
                      :mode 'hiragana
                      :registered-p t
                      :registered-reading "てすと"
                      :registered-word "テスト"))
          (nskk-undo-kakutei)
          ;; Word should be unregistered
          (should (null (nskk-dict-lookup "てすと"))))))))

;;;
;;; nskk-purge-from-jisyo
;;;

(nskk-describe "nskk-purge-from-jisyo"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-purge-from-jisyo))
    (should (commandp 'nskk-purge-from-jisyo)))

  (nskk-it "does nothing when not in converting mode"
    (with-temp-buffer
      (let ((unregister-called nil))
        (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                          (nskk-dict-unregister-word
                           (lambda (_r _c) (setq unregister-called t))))
          (nskk-purge-from-jisyo)
          (should-not unregister-called)))))

  (nskk-it "does nothing when user answers no to confirmation"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (unregister-called nil))
        (nskk-state-set-candidates nskk-current-state '("候補A" "候補B"))
        (setf (nskk-state-current-index nskk-current-state) 0)
        (nskk-state-put-metadata nskk-current-state 'henkan-reading "よみ")
        (nskk-with-mocks ((nskk-converting-p (lambda () t))
                          (yes-or-no-p (lambda (_prompt) nil))
                          (nskk-dict-unregister-word
                           (lambda (_r _c) (setq unregister-called t))))
          (nskk-purge-from-jisyo)
          (should-not unregister-called)))))

  (nskk-it "purges candidate and updates state when multiple remain"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (unregistered-reading nil)
            (unregistered-candidate nil)
            (overlay-updated nil))
        (nskk-state-set-candidates nskk-current-state '("候補A" "候補B" "候補C"))
        (setf (nskk-state-current-index nskk-current-state) 0)
        (nskk-state-put-metadata nskk-current-state 'henkan-reading "よみ")
        (nskk-with-mocks ((nskk-converting-p (lambda () t))
                          (yes-or-no-p (lambda (_prompt) t))
                          (nskk-dict-unregister-word
                           (lambda (r c)
                             (setq unregistered-reading r
                                   unregistered-candidate c)))
                          (nskk--update-overlay
                           (lambda (_start _end _text)
                             (setq overlay-updated t)))
                          (nskk-get-conversion-start (lambda () 1))
                          (overlay-end (lambda (_ov) 10)))
          (let ((nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
                (nskk-henkan-active-marker "▼"))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-overlay (make-overlay 1 1))
            
            (nskk-purge-from-jisyo)
            ;; Should have called unregister with correct args
            (should (equal unregistered-reading "よみ"))
            (should (equal unregistered-candidate "候補A"))
            ;; Candidates should be updated (候補A removed)
            (should (equal (nskk-state-candidates nskk-current-state)
                           '("候補B" "候補C")))
            ;; Index should be adjusted
            (should (= (nskk-state-current-index nskk-current-state) 0))
            ;; Overlay should be updated
            (should overlay-updated))
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay))
      )))))

  (nskk-it "cancels conversion when purging the last candidate"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (cancel-called nil))
        (nskk-state-set-candidates nskk-current-state '("唯一"))
        (setf (nskk-state-current-index nskk-current-state) 0)
        (nskk-state-put-metadata nskk-current-state 'henkan-reading "ゆいいつ")
        (nskk-with-mocks ((nskk-converting-p (lambda () t))
                          (yes-or-no-p (lambda (_prompt) t))
                          (nskk-dict-unregister-word #'ignore)
                          (nskk-cancel-conversion-to-reading
                           (lambda () (setq cancel-called t))))
          (nskk-purge-from-jisyo)
          (should cancel-called))))))

;;;
;;; nskk-completion-at-point (CAPF backend)
;;;

(nskk-describe "nskk-completion-at-point"
  (nskk-it "returns nil when no preedit is active"
    (with-temp-buffer
      (nskk-mode 1)
      (should (null (nskk-completion-at-point)))))

  (nskk-it "returns nil when preedit is empty (cursor at marker)"
    (with-temp-buffer
      (nskk-mode 1)
      (nskk-without-modification (insert nskk-henkan-on-marker))
      (nskk-set-conversion-start-marker (point-min))
      (should (null (nskk-completion-at-point)))))

  (nskk-it "returns a completion spec when preedit has text"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        (nskk-without-modification (insert nskk-henkan-on-marker "かん"))
        (nskk-set-conversion-start-marker (point-min))
        (let ((spec (nskk-completion-at-point)))
          ;; spec is (start end table . plist)
          (should spec)
          (should (= (nth 0 spec) (1+ (point-min))))  ;; after ▽
          (should (= (nth 1 spec) (point)))
          (should (functionp (nth 2 spec)))            ;; completion table
          (should (eq (plist-get (nthcdr 3 spec) :exclusive) 'no))))))

  (nskk-it "completion table returns prefix matches from dictionary"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (nskk-mode 1)
        ;; Initialize trie index and register test entries in user dictionary
        (nskk-prolog-retract-all 'user-dict-entry 2)
        (nskk-prolog-set-index 'user-dict-entry 2 :trie)
        (nskk-prolog-assert '((user-dict-entry "かんじ" ("漢字"))))
        (nskk-prolog-assert '((user-dict-entry "かんが" ("感が"))))
        (nskk-prolog-assert '((user-dict-entry "きょう" ("今日"))))
        (nskk-without-modification (insert nskk-henkan-on-marker "かん"))
        (nskk-set-conversion-start-marker (point-min))
        (let* ((spec (nskk-completion-at-point))
               (table (nth 2 spec))
               (completions (all-completions "かん" table)))
          (should (member "かんじ" completions))
          (should (member "かんが" completions))
          (should-not (member "きょう" completions)))))))

(ert-deftest nskk-undo-kakutei-test/restores-owned-state-on-failure ()
  "Restore every owned object after each injectable undo failure."
  (dolist (spec '((before-change error)
                  (before-change quit)
                  (after-change error)
                  (after-change quit)
                  (unregister-before error)
                  (unregister-before quit)
                  (unregister-after error)
                  (unregister-after quit)
                  (overlay error)
                  (overlay quit)
                  (state error)
                  (state quit)
                  (read-only buffer-read-only)))
    (let ((phase (car spec))
          (failure-type (cadr spec)))
      (ert-info ((format "phase=%S condition=%S" phase failure-type))
        (nskk-prolog-test-with-isolated-db
          (let ((nskk--user-dict-index 'user)
                (nskk-dict-modified nil))
            (nskk-prolog-set-index 'user-dict-entry 2 :trie)
            (nskk-dict-register-word "てすと" "既存")
            (nskk-dict-register-word "てすと" "テスト")
            (setq nskk-dict-modified nil)
            (with-temp-buffer
              (let* ((state-candidates (list "元候補"))
                     (metadata-value (list 'owned-metadata))
                     (state-metadata
                      (list 'owned-object metadata-value))
                     (state (nskk-state-create 'hiragana))
                     (record
                      (list :reading "てすと"
                            :candidates '("テスト")
                            :index 0
                            :committed-text "テスト"
                            :buffer-start 1
                            :buffer-end 4
                            :mode 'hiragana
                            :registered-p t
                            :registered-reading "てすと"
                            :registered-word "テスト"))
                     marker
                     overlay)
                (setf (nskk-state-candidates state) state-candidates
                      (nskk-state-current-index state) 7
                      (nskk-state-henkan-phase state) 'on
                      (nskk-state-metadata state) state-metadata)
                (setq-local nskk-current-state state)
                (insert "テスト")
                (goto-char 2)
                (progn (setq marker (copy-marker 2 t)) (nskk-state-set-conversion-start-marker marker) (setq overlay (make-overlay 2 3 nil nil t)) (nskk-state-set-conversion-overlay overlay) (setq nskk--last-kakutei-record record))
                (let ((overlay-value (list 'owned-overlay-property)))
                  (overlay-put overlay 'face 'bold)
                  (overlay-put overlay 'nskk-test-object overlay-value)
                  (let* ((saved-text (buffer-string))
                         (saved-point (point))
                         (saved-state (copy-sequence state))
                         (saved-marker-buffer (marker-buffer marker))
                         (saved-marker-position (marker-position marker))
                         (saved-marker-insertion-type
                          (marker-insertion-type marker))
                         (saved-overlay-buffer (overlay-buffer overlay))
                         (saved-overlay-start (overlay-start overlay))
                         (saved-overlay-end (overlay-end overlay))
                         (saved-overlay-properties
                          (copy-sequence (overlay-properties overlay)))
                         (saved-dict-candidates
                          (copy-tree (nskk-dict-lookup "てすと")))
                         (saved-dict-modified nskk-dict-modified)
                         (real-unregister
                          (symbol-function
                           'nskk-dict-unregister-word))
                         (real-update-overlay
                          (symbol-function 'nskk--update-overlay))
                         (real-put-metadata
                          (symbol-function 'nskk-state-put-metadata)))
                    (setq-local before-change-functions
                                (when (eq phase 'before-change)
                                  (list
                                   (lambda (&rest _)
                                     (signal failure-type
                                             '("injected"))))))
                    (setq-local after-change-functions
                                (when (eq phase 'after-change)
                                  (list
                                   (lambda (&rest _)
                                     (signal failure-type
                                             '("injected"))))))
                    (setq buffer-read-only (eq phase 'read-only))
                    (cl-letf
                        (((symbol-function
                           'nskk-dict-unregister-word)
                          (lambda (reading word)
                            (pcase phase
                              ('unregister-before
                               (signal failure-type '("injected")))
                              ('unregister-after
                               (prog1
                                   (funcall real-unregister reading word)
                                 (signal failure-type '("injected"))))
                              (_
                               (funcall real-unregister reading word)))))
                         ((symbol-function 'nskk--update-overlay)
                          (lambda (&rest args)
                            (prog1 (apply real-update-overlay args)
                              (when (eq phase 'overlay)
                                (signal failure-type '("injected"))))))
                         ((symbol-function 'nskk-state-put-metadata)
                          (lambda (target key value)
                            (prog1
                                (funcall real-put-metadata
                                         target key value)
                              (when (and (eq phase 'state)
                                         (eq key 'henkan-reading))
                                (signal failure-type '("injected")))))))
                      (let (caught)
                        (condition-case err
                            (nskk-undo-kakutei)
                          ((error quit)
                           (setq caught (car err))))
                        (should (eq failure-type caught))))
                    (should (equal saved-text (buffer-string)))
                    (should (= saved-point (point)))
                    (should (eq state nskk-current-state))
                    (should (equal saved-state nskk-current-state))
                    (should (eq state-candidates
                                (nskk-state-candidates
                                 nskk-current-state)))
                    (should (eq state-metadata
                                (nskk-state-metadata
                                 nskk-current-state)))
                    (should (eq metadata-value
                                (plist-get
                                 (nskk-state-metadata
                                  nskk-current-state)
                                 'owned-object)))
                    (should (eq marker
                                (nskk-state-conversion-start-marker)))
                    (should (eq saved-marker-buffer
                                (marker-buffer marker)))
                    (should (= saved-marker-position
                               (marker-position marker)))
                    (should (eq saved-marker-insertion-type
                                (marker-insertion-type marker)))
                    (should (eq overlay (nskk-state-conversion-overlay)))
                    (should (eq saved-overlay-buffer
                                (overlay-buffer overlay)))
                    (should (= saved-overlay-start
                               (overlay-start overlay)))
                    (should (= saved-overlay-end
                               (overlay-end overlay)))
                    (should (equal saved-overlay-properties
                                   (overlay-properties overlay)))
                    (should (eq overlay-value
                                (overlay-get overlay
                                             'nskk-test-object)))
                    (should (eq record nskk--last-kakutei-record))
                    (should (equal saved-dict-candidates
                                   (nskk-dict-lookup "てすと")))
                    (should (eq saved-dict-modified
                                nskk-dict-modified))))))))))))

(nskk-describe "untrusted henkan display properties"
    (nskk-it "sanitizes the dictionary registration prompt copy"
      (let* ((reading (propertize "よみ"
                                 'display "spoofed"
                                 'keymap (make-sparse-keymap)
                                 'local-map (make-sparse-keymap)
                                 'mouse-face 'highlight
                                 'help-echo "untrusted"
                                 'face 'error
                                 'nskk-no-learn t))
             (reading-copy (copy-sequence reading))
             (prompt (nskk--registration-prompt 1 reading)))
        (should (equal prompt "[辞書登録] よみ: "))
        (dolist (property '(display keymap local-map mouse-face help-echo face))
          (should-not
           (text-property-not-all 0 (length prompt) property nil prompt)))
        (should (equal reading reading-copy))
        (should (eq (get-text-property 0 'nskk-no-learn reading) t))))
    (nskk-it "sanitizes dcomp copies before applying trusted faces"
      (let* ((first (propertize "かんじ"
                               'display "spoofed first"
                               'keymap (make-sparse-keymap)
                               'local-map (make-sparse-keymap)
                               'mouse-face 'highlight
                               'help-echo "first help"
                               'face 'error
                               'nskk-no-learn t))
             (second (propertize "かんせい"
                                'display "spoofed second"
                                'keymap (make-sparse-keymap)
                                'local-map (make-sparse-keymap)
                                'mouse-face 'highlight
                                'help-echo "second help"
                                'face 'error
                                'nskk-no-learn t))
             (first-copy (copy-sequence first))
             (second-copy (copy-sequence second))
             (rendered
              (nskk--dcomp-multiple-build-string
               (list first second) 0 "かん")))
        (should (equal (substring-no-properties rendered)
                       "  かんじ\n  かんせい"))
        (dolist (property '(display keymap local-map mouse-face help-echo))
          (should-not
           (text-property-not-all 0 (length rendered) property nil rendered)))
        (dolist (index '(0 1 5 6 7))
          (should-not (get-text-property index 'face rendered)))
        (dolist (index '(2 3 4))
          (should (eq (get-text-property index 'face rendered)
                      'nskk-dcomp-multiple-selected-face)))
        (dolist (index '(8 9))
          (should (eq (get-text-property index 'face rendered)
                      'nskk-dcomp-multiple-face)))
        (dolist (index '(10 11))
          (should (eq (get-text-property index 'face rendered)
                      'nskk-dcomp-multiple-trailing-face)))
        (should (equal first first-copy))
        (should (equal second second-copy))
        (should (eq (get-text-property 0 'nskk-no-learn first) t))
        (should (eq (get-text-property 0 'nskk-no-learn second) t))))
    (nskk-it "sanitizes purge confirmation without changing state values"
      (let* ((candidate (propertize "候補"
                                   'display "spoofed candidate"
                                   'keymap (make-sparse-keymap)
                                   'local-map (make-sparse-keymap)
                                   'mouse-face 'highlight
                                   'help-echo "candidate help"
                                   'face 'error
                                   'nskk-no-learn t))
             (reading (propertize "よみ"
                                 'display "spoofed reading"
                                 'keymap (make-sparse-keymap)
                                 'local-map (make-sparse-keymap)
                                 'mouse-face 'highlight
                                 'help-echo "reading help"
                                 'face 'error
                                 'nskk-no-learn t))
             (candidate-copy (copy-sequence candidate))
             (reading-copy (copy-sequence reading))
             (nskk-current-state (nskk-state-create 'hiragana))
             prompt)
        (nskk-state-set-candidates nskk-current-state (list candidate))
        (setf (nskk-state-current-index nskk-current-state) 0)
        (nskk-state-put-metadata
         nskk-current-state 'henkan-reading reading)
        (cl-letf (((symbol-function 'nskk-converting-p) (lambda () t))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (actual-prompt)
                     (setq prompt actual-prompt)
                     nil)))
          (nskk-purge-from-jisyo))
        (should (equal prompt "Really purge \"候補\" (よみ)? "))
        (dolist (property '(display keymap local-map mouse-face help-echo face))
          (should-not
           (text-property-not-all 0 (length prompt) property nil prompt)))
        (should (eq (car (nskk-state-candidates nskk-current-state))
                    candidate))
        (should (eq (nskk-state-get-metadata
                     nskk-current-state 'henkan-reading)
                    reading))
        (should (equal candidate candidate-copy))
        (should (equal reading reading-copy))
        (should (eq (get-text-property 0 'nskk-no-learn candidate) t))
        (should (eq (get-text-property 0 'nskk-no-learn reading) t)))))
  (progn
  (defconst nskk-test--commit-cleanup-flags
    '(nskk--azik-colon-okuri-pending
      nskk--azik-colon-okuri-deferred
      nskk--azik-sokuon-okuri-kana-pending
      nskk--deferred-azik-state
      nskk--deferred-vowel-shadow-state
      nskk--sticky-shift-pending))

  (defun nskk-test--call-with-failure-safe-commit-fixture
      (mode list-active candidate reading callback)
    (with-temp-buffer
      (let* ((state (nskk-state-create mode))
             (candidates (list candidate "次"))
             (nskk-current-state state)
             (nskk-test-saved-conversion-start-marker (nskk-state-conversion-start-marker))
             (nskk-test-saved-conversion-overlay (nskk-state-conversion-overlay))
             (nskk-test-saved-pending-romaji-overlay (nskk-state-pending-romaji-overlay))
             (nskk-test-saved-romaji-buffer (nskk-state-romaji-buffer))
             (nskk-test-saved-henkan-count (nskk-state-henkan-count))
             (nskk--henkan-candidate-list-active list-active)
             (nskk--azik-colon-okuri-pending t)
             (nskk--azik-colon-okuri-deferred t)
             (nskk--azik-sokuon-okuri-kana-pending t)
             (nskk--deferred-azik-state t)
             (nskk--deferred-vowel-shadow-state t)
             (nskk--sticky-shift-pending t)
             (nskk--numeric-mode (eq mode 'abbrev))
             (nskk--last-kakutei-record nil)
             (nskk-henkan-hide-candidates-functions nil))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker (make-marker))
            (nskk-state-set-conversion-overlay nil)
            (nskk-state-set-pending-romaji-overlay nil)
            (nskk-state-set-romaji-buffer "dirty")
            (nskk-state-set-henkan-count 4)
            
        (setf (nskk-state-previous-mode state) 'hiragana)
        (insert nskk-henkan-active-marker
                (substring-no-properties candidate))
        (set-marker (nskk-state-conversion-start-marker) (point-min))
        (progn (nskk-state-set-conversion-overlay (make-overlay
               (+ (point-min) (length nskk-henkan-active-marker))
               (point-max))) (nskk-state-set-pending-romaji-overlay (make-overlay (point-max) (point-max))))
        (nskk-state-set-candidates state candidates)
        (setf (nskk-state-current-index state) 0)
        (nskk-state-force-henkan-phase state 'active)
        (nskk-state-put-metadata state 'henkan-reading reading)
        (goto-char (point-max))
        (funcall callback
                 candidate mode reading state candidates
                 (nskk-state-conversion-overlay) (nskk-state-pending-romaji-overlay)))
        (nskk-state-set-conversion-start-marker nskk-test-saved-conversion-start-marker)
        (nskk-state-set-conversion-overlay nskk-test-saved-conversion-overlay)
        (nskk-state-set-pending-romaji-overlay nskk-test-saved-pending-romaji-overlay)
        (nskk-state-set-romaji-buffer nskk-test-saved-romaji-buffer)
        (nskk-state-set-henkan-count nskk-test-saved-henkan-count))
      )))

  (defun nskk-test--redirty-commit-cleanup-state ()
    (if (overlayp (nskk-state-conversion-overlay))
        (move-overlay (nskk-state-conversion-overlay) (point-min) (point-max))
      (nskk-state-set-conversion-overlay (make-overlay (point-min) (point-max))))
    (if (overlayp (nskk-state-pending-romaji-overlay))
        (move-overlay (nskk-state-pending-romaji-overlay) (point-max) (point-max))
      (nskk-state-set-pending-romaji-overlay (make-overlay (point-max) (point-max))))
    (nskk-set-conversion-start-marker (point-min))
    (progn (nskk-state-set-romaji-buffer "redirtied") (nskk-state-set-henkan-count 9) (setq nskk--henkan-candidate-list-active t))
    (dolist (symbol nskk-test--commit-cleanup-flags)
      (set symbol t))
    (nskk-state-set-candidates nskk-current-state '("汚染" "状態"))
    (setf (nskk-state-current-index nskk-current-state) 1)
    (nskk-state-force-henkan-phase nskk-current-state 'list)
    (nskk-state-put-metadata nskk-current-state 'okurigana t)
    (nskk-state-put-metadata
     nskk-current-state 'okurigana-in-progress t))

  (progn
  (defun nskk-test--assert-failure-safe-commit-clean-and-undo
      (candidate original-mode reading original-state original-candidates
                 original-overlay original-pending-overlay)
    (should (equal (buffer-string)
                   (substring-no-properties candidate)))
    (should (eq nskk-current-state original-state))
    (should-not (overlay-buffer original-overlay))
    (should-not (overlay-buffer original-pending-overlay))
    (should-not (nskk-state-conversion-overlay))
    (should-not (nskk-state-pending-romaji-overlay))
    (should (markerp (nskk-state-conversion-start-marker)))
    (should-not (marker-position (nskk-state-conversion-start-marker)))
    (should (equal (nskk-state-romaji-buffer) ""))
    (should (= (nskk-state-henkan-count) 0))
    (should-not nskk--henkan-candidate-list-active)
    (dolist (symbol nskk-test--commit-cleanup-flags)
      (should-not (symbol-value symbol)))
    (should-not (nskk-state-candidates nskk-current-state))
    (should (= (nskk-state-current-index nskk-current-state) 0))
    (should-not (nskk-state-henkan-phase nskk-current-state))
    (should-not
     (nskk-state-get-metadata nskk-current-state 'okurigana))
    (should-not
     (nskk-state-get-metadata
      nskk-current-state 'okurigana-in-progress))
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
    (should-not (nskk-numeric-mode))
    (let ((record nskk--last-kakutei-record))
      (should record)
      (should (eq (plist-get record :candidates) original-candidates))
      (should (eq (plist-get record :mode) original-mode))
      (should (equal (plist-get record :reading) reading))
      (should (equal (plist-get record :committed-text)
                     (substring-no-properties candidate)))
      (should (= (plist-get record :buffer-start) (point-min)))
      (should (= (plist-get record :buffer-end) (point-max))))
    (nskk-undo-kakutei)
    (should-not nskk--last-kakutei-record)
    (should (eq nskk-current-state original-state))
    (should (equal (buffer-string)
                   (concat nskk-henkan-active-marker
                           (substring-no-properties candidate))))
    (should (eq (nskk-state-candidates nskk-current-state)
                original-candidates))
    (should (= (nskk-state-current-index nskk-current-state) 0))
    (should (eq (nskk-state-henkan-phase nskk-current-state) 'active))
    (should (equal
             (nskk-state-get-metadata
              nskk-current-state 'henkan-reading)
             reading))
    (should (eq (nskk-state-mode nskk-current-state) original-mode))
    (should (marker-position (nskk-state-conversion-start-marker)))
    (should (overlay-buffer (nskk-state-conversion-overlay))))

  (progn
  (ert-deftest nskk-test-commit-learning-fault-matrix ()
    (dolist (list-active '(nil t))
      (dolist (mode '(hiragana abbrev))
        (dolist (failure-type '(error quit))
          (dolist (stage '(study search))
            (ert-info
                ((format "list=%S mode=%S failure=%S stage=%S"
                         list-active mode failure-type stage))
              (let ((candidate (propertize "候補" 'source 'original))
                    (reading "よみ")
                    (study-calls 0)
                    (search-calls 0)
                    (first-hide-calls 0)
                    (second-hide-calls 0)
                    caught)
                (nskk-test--call-with-failure-safe-commit-fixture
                 mode list-active candidate reading
                 (lambda (fixture-candidate fixture-mode fixture-reading
                          fixture-state fixture-candidates
                          fixture-overlay fixture-pending-overlay)
                   (setq nskk-henkan-hide-candidates-functions
                         (list (lambda () (cl-incf first-hide-calls))
                               (lambda () (cl-incf second-hide-calls))))
                   (cl-letf
                       (((symbol-function 'nskk-study-after-kakutei)
                         (lambda (&rest _args)
                           (cl-incf study-calls)
                           (when (eq stage 'study)
                             (signal failure-type
                                     '(nskk-test-learning-fault payload)))))
                        ((symbol-function 'nskk-search-learn)
                         (lambda (&rest _args)
                           (cl-incf search-calls)
                           (when (eq stage 'search)
                             (signal failure-type
                                     '(nskk-test-learning-fault payload))))))
                     (condition-case condition
                         (nskk-commit-current)
                       ((error quit)
                        (setq caught condition))))
                   (should
                    (equal caught
                           (list failure-type
                                 'nskk-test-learning-fault 'payload)))
                   (should (= first-hide-calls 1))
                   (should (= second-hide-calls 1))
                   (should (= study-calls 1))
                   (should (= search-calls
                              (if (eq stage 'search) 1 0)))
                   (should (eq (get-text-property
                                0 'source fixture-candidate)
                               'original))
                   (nskk-test--assert-failure-safe-commit-clean-and-undo
                    fixture-candidate fixture-mode fixture-reading
                    fixture-state fixture-candidates fixture-overlay
                    fixture-pending-overlay))))))))))

  (progn
  (ert-deftest nskk-test-commit-cleanup-fault-matrix ()
    (dolist (list-active '(nil t))
      (dolist (mode '(hiragana abbrev))
        (dolist (failure-type '(error quit))
          (dolist (stage '(hide overlay marker romaji azik state abbrev))
            (ert-info
                ((format "list=%S mode=%S failure=%S stage=%S"
                         list-active mode failure-type stage))
              (let ((candidate (propertize "候補" 'source 'original))
                    (reading "よみ")
                    (study-calls 0)
                    (search-calls 0)
                    (first-hide-calls 0)
                    (second-hide-calls 0)
                    (faulted nil)
                    caught)
                (nskk-test--call-with-failure-safe-commit-fixture
                 mode list-active candidate reading
                 (lambda (fixture-candidate fixture-mode fixture-reading
                          fixture-state fixture-candidates
                          fixture-overlay fixture-pending-overlay)
                   (let ((real-delete-overlay
                          (symbol-function 'delete-overlay))
                         (real-clear-marker
                          (symbol-function
                           'nskk--clear-conversion-start-marker))
                         (real-reset-romaji
                          (symbol-function 'nskk-reset-romaji-buffer))
                         (real-clear-azik
                          (symbol-function
                           'nskk-clear-azik-pending-state))
                         (real-set-candidates
                          (symbol-function 'nskk-state-set-candidates))
                         (real-restore-abbrev
                          (symbol-function 'nskk--restore-abbrev-mode)))
                     (setq nskk-henkan-hide-candidates-functions
                           (list
                            (lambda ()
                              (cl-incf first-hide-calls)
                              (when (and (eq stage 'hide) (not faulted))
                                (setq faulted t)
                                (nskk-test--redirty-commit-cleanup-state)
                                (signal
                                 failure-type
                                 '(nskk-test-cleanup-fault payload))))
                            (lambda () (cl-incf second-hide-calls))))
                     (cl-letf
                         (((symbol-function 'delete-overlay)
                           (lambda (overlay)
                             (prog1 (funcall real-delete-overlay overlay)
                               (when (and (eq stage 'overlay) (not faulted))
                                 (setq faulted t)
                                 (nskk-test--redirty-commit-cleanup-state)
                                 (signal
                                  failure-type
                                  '(nskk-test-cleanup-fault payload))))))
                          ((symbol-function
                            'nskk--clear-conversion-start-marker)
                           (lambda ()
                             (prog1 (funcall real-clear-marker)
                               (when (and (eq stage 'marker) (not faulted))
                                 (setq faulted t)
                                 (nskk-test--redirty-commit-cleanup-state)
                                 (signal
                                  failure-type
                                  '(nskk-test-cleanup-fault payload))))))
                          ((symbol-function 'nskk-reset-romaji-buffer)
                           (lambda ()
                             (prog1 (funcall real-reset-romaji)
                               (when (and (eq stage 'romaji) (not faulted))
                                 (setq faulted t)
                                 (nskk-test--redirty-commit-cleanup-state)
                                 (signal
                                  failure-type
                                  '(nskk-test-cleanup-fault payload))))))
                          ((symbol-function 'nskk-clear-azik-pending-state)
                           (lambda ()
                             (prog1 (funcall real-clear-azik)
                               (when (and (eq stage 'azik) (not faulted))
                                 (setq faulted t)
                                 (nskk-test--redirty-commit-cleanup-state)
                                 (signal
                                  failure-type
                                  '(nskk-test-cleanup-fault payload))))))
                          ((symbol-function 'nskk-state-set-candidates)
                           (lambda (state candidates)
                             (prog1
                                 (funcall real-set-candidates state candidates)
                               (when (and (eq stage 'state) (not faulted))
                                 (setq faulted t)
                                 (nskk-test--redirty-commit-cleanup-state)
                                 (signal
                                  failure-type
                                  '(nskk-test-cleanup-fault payload))))))
                          ((symbol-function 'nskk--restore-abbrev-mode)
                           (lambda (was-abbrev)
                             (prog1 (funcall real-restore-abbrev was-abbrev)
                               (when (and (eq stage 'abbrev) (not faulted))
                                 (setq faulted t)
                                 (nskk-test--redirty-commit-cleanup-state)
                                 (signal
                                  failure-type
                                  '(nskk-test-cleanup-fault payload))))))
                          ((symbol-function 'nskk-study-after-kakutei)
                           (lambda (&rest _args)
                             (cl-incf study-calls)))
                          ((symbol-function 'nskk-search-learn)
                           (lambda (&rest _args)
                             (cl-incf search-calls))))
                       (condition-case condition
                           (nskk-commit-current)
                         ((error quit)
                          (setq caught condition)))))
                   (should faulted)
                   (should
                    (equal caught
                           (list failure-type
                                 'nskk-test-cleanup-fault 'payload)))
                   (should (= first-hide-calls 1))
                   (should (= second-hide-calls 1))
                   (should (= study-calls 0))
                   (should (= search-calls 0))
                   (nskk-test--assert-failure-safe-commit-clean-and-undo
                    fixture-candidate fixture-mode fixture-reading
                    fixture-state fixture-candidates fixture-overlay
                    fixture-pending-overlay))))))))))

  (progn
  (ert-deftest nskk-test-commit-cleanup-preserves-first-condition ()
    (let ((candidate (propertize "候補" 'source 'original))
          (reading "よみ")
          (study-calls 0)
          (search-calls 0)
          (first-hide-calls 0)
          (second-hide-calls 0)
          (romaji-faulted nil)
          caught)
      (nskk-test--call-with-failure-safe-commit-fixture
       'abbrev t candidate reading
       (lambda (fixture-candidate fixture-mode fixture-reading
                fixture-state fixture-candidates fixture-overlay
                fixture-pending-overlay)
         (let ((real-reset-romaji
                (symbol-function 'nskk-reset-romaji-buffer)))
           (setq nskk-henkan-hide-candidates-functions
                 (list
                  (lambda ()
                    (cl-incf first-hide-calls)
                    (nskk-test--redirty-commit-cleanup-state)
                    (signal 'error
                            '(nskk-test-first-cleanup-fault payload)))
                  (lambda () (cl-incf second-hide-calls))))
           (cl-letf
               (((symbol-function 'nskk-reset-romaji-buffer)
                 (lambda ()
                   (prog1 (funcall real-reset-romaji)
                     (unless romaji-faulted
                       (setq romaji-faulted t)
                       (nskk-test--redirty-commit-cleanup-state)
                       (signal 'quit
                               '(nskk-test-later-cleanup-fault payload))))))
                ((symbol-function 'nskk-study-after-kakutei)
                 (lambda (&rest _args)
                   (cl-incf study-calls)))
                ((symbol-function 'nskk-search-learn)
                 (lambda (&rest _args)
                   (cl-incf search-calls))))
             (condition-case condition
                 (nskk-commit-current)
               ((error quit)
                (setq caught condition)))))
         (should
          (equal caught
                 '(error nskk-test-first-cleanup-fault payload)))
         (should (= first-hide-calls 1))
         (should (= second-hide-calls 1))
         (should romaji-faulted)
         (should (= study-calls 0))
         (should (= search-calls 0))
         (nskk-test--assert-failure-safe-commit-clean-and-undo
          fixture-candidate fixture-mode fixture-reading fixture-state
          fixture-candidates fixture-overlay fixture-pending-overlay)))))

  (progn
  (ert-deftest nskk-test-commit-normal-no-reading-no-learn ()
    (dolist (scenario '(normal no-reading no-learn))
      (ert-info ((format "scenario=%S" scenario))
        (let* ((reading (unless (eq scenario 'no-reading) "よみ"))
               (candidate
                (propertize "候補"
                            'source 'original
                            'nskk-no-learn (eq scenario 'no-learn)))
               study-args
               search-args
               result)
          (nskk-test--call-with-failure-safe-commit-fixture
           'hiragana nil candidate reading
           (lambda (fixture-candidate fixture-mode fixture-reading
                    fixture-state fixture-candidates fixture-overlay
                    fixture-pending-overlay)
             (cl-letf
                 (((symbol-function 'nskk-study-after-kakutei)
                   (lambda (&rest args)
                     (setq study-args args)))
                  ((symbol-function 'nskk-search-learn)
                   (lambda (&rest args)
                     (setq search-args args))))
               (setq result (nskk-commit-current)))
             (should (eq result fixture-candidate))
             (if fixture-reading
                 (progn
                   (should (equal (car study-args) fixture-reading))
                   (should (eq (cadr study-args) fixture-candidate))
                   (should (= (nth 2 study-args) 0))
                   (should (equal (car search-args) fixture-reading))
                   (should (eq (cadr search-args) fixture-candidate)))
               (should-not study-args)
               (should-not search-args))
             (should
              (eq (get-text-property 0 'source fixture-candidate)
                  'original))
             (should
              (eq (get-text-property 0 'nskk-no-learn fixture-candidate)
                  (eq scenario 'no-learn)))
             (nskk-test--assert-failure-safe-commit-clean-and-undo
              fixture-candidate fixture-mode fixture-reading fixture-state
              fixture-candidates fixture-overlay
              fixture-pending-overlay)))))))

  (provide 'nskk-henkan-test)))))))

;;; nskk-henkan-test.el ends here
