;;; nskk-refactoring-test.el --- Unit tests for refactored macros/helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for: nskk-prolog-deffacts, nskk-define-goal-handler,
;;            nskk--emit-hatsuon-prefix, nskk--key-state-base,
;;            nskk-when-prolog-holds, nskk-ensure-overlay, nskk-delete-overlay,
;;            nskk-search--post-process-results, nskk-process-japanese-input

;;; Code:

(require 'ert)
(require 'nskk-test-macros)
(require 'nskk-test-framework)
(require 'nskk-prolog)
(require 'nskk-input)
(require 'nskk-keymap)
(require 'nskk-henkan)
(require 'nskk-search)
(eval-when-compile (require 'cl-lib))

;;;
;;; nskk-prolog-deffacts Tests
;;;

(nskk-deftest-unit prolog-deffacts-macro-expansion-empty
  "Empty fact list expands to a single (progn) with no assertions."
  ;; macroexpand operates on the form without evaluating: verify no
  ;; nskk-prolog-<- calls appear in the expansion for zero rows.
  (let ((expansion (macroexpand '(nskk-prolog-deffacts test-pred))))
    (should (eq (car expansion) 'progn))
    (should (null (cdr expansion)))))

(nskk-deftest-unit prolog-deffacts-macro-expansion-single-row
  "Single row expands to exactly one nskk-prolog-<- call."
  (let* ((expansion (macroexpand '(nskk-prolog-deffacts my-pred
                                    (a b c))))
         (calls (cdr expansion)))
    (should (eq (car expansion) 'progn))
    (should (= (length calls) 1))
    ;; The call should be (nskk-prolog-<- (my-pred a b c))
    (let ((call (car calls)))
      (should (eq (car call) 'nskk-prolog-<-))
      (should (equal (cadr call) '(my-pred a b c))))))

(nskk-deftest-unit prolog-deffacts-macro-expansion-multiple-rows
  "Multiple rows expand to one nskk-prolog-<- call per row, in order."
  (let* ((expansion (macroexpand '(nskk-prolog-deffacts key-act
                                    (space converting next-candidate)
                                    (space preedit   start-conversion)
                                    (space normal    self-insert))))
         (calls (cdr expansion)))
    (should (eq (car expansion) 'progn))
    (should (= (length calls) 3))
    ;; First call
    (should (equal (cadr (nth 0 calls)) '(key-act space converting next-candidate)))
    ;; Second call
    (should (equal (cadr (nth 1 calls)) '(key-act space preedit   start-conversion)))
    ;; Third call
    (should (equal (cadr (nth 2 calls)) '(key-act space normal    self-insert)))))

(nskk-deftest-unit prolog-deffacts-runtime-fact-order
  "Facts asserted via nskk-prolog-deffacts are matched in insertion order."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-set-index 'test-order-pred 2 :hash)
    (nskk-prolog-deffacts test-order-pred
      (key first)
      (key second)
      (key third))
    ;; The first matching result for (test-order-pred key ?v) must be `first'.
    (let ((val (nskk-prolog-query-value '(test-order-pred key \?v) '\?v)))
      (should (eq val 'first)))))

(nskk-deftest-unit prolog-deffacts-runtime-all-rows-asserted
  "All rows in nskk-prolog-deffacts are asserted to the Prolog database."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    ;; Use list index so all facts are retrievable.
    (nskk-prolog-deffacts test-allrows-pred
      (row1 val-a)
      (row2 val-b)
      (row3 val-c))
    ;; All three rows must resolve.
    (should (nskk-prolog-query-value '(test-allrows-pred row1 \?v) '\?v))
    (should (nskk-prolog-query-value '(test-allrows-pred row2 \?v) '\?v))
    (should (nskk-prolog-query-value '(test-allrows-pred row3 \?v) '\?v))))

(nskk-deftest-unit prolog-deffacts-runtime-independent-predicates
  "Facts from different nskk-prolog-deffacts calls do not interfere."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-deffacts pred-alpha
      (k1 v1))
    (nskk-prolog-deffacts pred-beta
      (k2 v2))
    ;; pred-alpha should not return facts for pred-beta's key and vice versa.
    (should (null (nskk-prolog-query-value '(pred-alpha k2 \?v) '\?v)))
    (should (null (nskk-prolog-query-value '(pred-beta  k1 \?v) '\?v)))
    ;; But each predicate should resolve its own fact.
    (should (eq (nskk-prolog-query-value '(pred-alpha k1 \?v) '\?v) 'v1))
    (should (eq (nskk-prolog-query-value '(pred-beta  k2 \?v) '\?v) 'v2))))

(nskk-deftest-unit prolog-deffacts-runtime-empty-noop
  "nskk-prolog-deffacts with zero rows asserts nothing."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    ;; Evaluate empty deffacts (no rows) — must not signal an error.
    (eval '(nskk-prolog-deffacts empty-noop-pred))
    ;; No fact should be present.
    (should (null (nskk-prolog-query-value '(empty-noop-pred \?a \?b) '\?a)))))

;;;
;;; nskk-define-goal-handler Tests
;;;

;; Note on handler test design: `nskk-prolog--goal-handlers' is an ordered alist
;; in which the `normal' entry (with :match t) is the catch-all and is appended
;; last.  `nskk-define-goal-handler' appends new entries AFTER existing ones, so
;; a freshly registered handler sits AFTER `normal' and would never be reached by
;; `nskk-prolog-prove'.  The dispatch tests below therefore call
;; `nskk-prolog--dispatch-goal' directly with a controlled, minimal handlers list
;; that puts the test handler first, avoiding the catch-all ordering issue.

(nskk-deftest-unit define-goal-handler-registration
  "After nskk-define-goal-handler, an entry exists in nskk-prolog--goal-handlers."
  (let ((saved-handlers nskk-prolog--goal-handlers))
    (unwind-protect
        (progn
          (nskk-define-goal-handler test-reg-handler (goal rest subst k)
            :match (and (consp goal) (eq (car goal) 'test-reg-goal))
            :body (funcall k subst))
          (let ((entry (assq 'test-reg-handler nskk-prolog--goal-handlers)))
            (should entry)
            (should (eq (nth 0 entry) 'test-reg-handler))
            (should (functionp (nth 1 entry)))
            (should (functionp (nth 2 entry)))))
      (setq nskk-prolog--goal-handlers saved-handlers))))

(nskk-deftest-unit define-goal-handler-match-fn-called
  "The match predicate of a registered handler is invoked for dispatch."
  (let ((saved-handlers nskk-prolog--goal-handlers)
        (match-called nil))
    (unwind-protect
        (progn
          (nskk-define-goal-handler test-match-handler-2 (goal rest subst k)
            :match (progn (setq match-called t)
                          (and (consp goal) (eq (car goal) 'test-match-sentinel-2)))
            :body (funcall k subst))
          ;; Call dispatch directly with a handler list that puts our handler FIRST,
          ;; so it is tried before the `normal' catch-all.
          (let* ((our-entry (assq 'test-match-handler-2 nskk-prolog--goal-handlers))
                 (nskk-prolog--goal-handlers (list our-entry)))
            (nskk-prolog--dispatch-goal '(test-match-sentinel-2) nil nil #'identity))
          (should match-called))
      (setq nskk-prolog--goal-handlers saved-handlers))))

(nskk-deftest-unit define-goal-handler-body-invoked-when-match
  "The body of a matching handler is executed and calls on-solution."
  (let ((saved-handlers nskk-prolog--goal-handlers)
        (body-called nil)
        (solution-received nil))
    (unwind-protect
        (progn
          (nskk-define-goal-handler test-body-handler-2 (goal rest subst k)
            :match (and (consp goal) (eq (car goal) 'test-body-sentinel-2))
            :body
            (setq body-called t)
            (funcall k subst))
          ;; Place our handler first so it is matched before the `normal' catch-all.
          (let* ((our-entry (assq 'test-body-handler-2 nskk-prolog--goal-handlers))
                 (nskk-prolog--goal-handlers (list our-entry)))
            (nskk-prolog--dispatch-goal
             '(test-body-sentinel-2) nil nil
             (lambda (s) (setq solution-received t))))
          (should body-called)
          (should solution-received))
      (setq nskk-prolog--goal-handlers saved-handlers))))

(nskk-deftest-unit define-goal-handler-idempotency-no-duplicate
  "Re-defining a handler with the same name updates in-place; no duplicate is added."
  (let ((saved-handlers nskk-prolog--goal-handlers))
    (unwind-protect
        (progn
          (nskk-define-goal-handler test-idem-handler (goal rest subst k)
            :match (and (consp goal) (eq (car goal) 'test-idem-goal))
            :body (funcall k subst))
          (let ((count-before
                 (length (cl-remove-if-not
                          (lambda (e) (eq (car e) 'test-idem-handler))
                          nskk-prolog--goal-handlers))))
            ;; Re-register with the same name.
            (nskk-define-goal-handler test-idem-handler (goal rest subst k)
              :match (and (consp goal) (eq (car goal) 'test-idem-goal))
              :body (funcall k subst))
            (let ((count-after
                   (length (cl-remove-if-not
                            (lambda (e) (eq (car e) 'test-idem-handler))
                            nskk-prolog--goal-handlers))))
              ;; Count must not increase.
              (should (= count-before count-after)))))
      (setq nskk-prolog--goal-handlers saved-handlers))))

(nskk-deftest-unit define-goal-handler-reload-updates-body
  "Re-defining a handler replaces the match and body functions in place."
  (let ((saved-handlers nskk-prolog--goal-handlers)
        (call-log nil))
    (unwind-protect
        (progn
          ;; First registration: body records 'first.
          (nskk-define-goal-handler test-reload-handler (goal rest subst k)
            :match (and (consp goal) (eq (car goal) 'test-reload-goal))
            :body
            (push 'first call-log)
            (funcall k subst))
          ;; Second registration with same name: body records 'second.
          (nskk-define-goal-handler test-reload-handler (goal rest subst k)
            :match (and (consp goal) (eq (car goal) 'test-reload-goal))
            :body
            (push 'second call-log)
            (funcall k subst))
          ;; Dispatch directly via the entry so ordering doesn't matter.
          (let* ((our-entry (assq 'test-reload-handler nskk-prolog--goal-handlers))
                 (nskk-prolog--goal-handlers (list our-entry)))
            (nskk-prolog--dispatch-goal '(test-reload-goal) nil nil #'identity))
          ;; The second (updated) body should have been called; first must not.
          (should (equal call-log '(second)))
          (should-not (member 'first call-log)))
      (setq nskk-prolog--goal-handlers saved-handlers))))

;;;
;;; nskk--emit-hatsuon-prefix Tests
;;;

(nskk-deftest-unit emit-hatsuon-prefix-n-plus-n-sets-buffer
  "n+n case: emit-hatsuon-prefix with \"n\" sets nskk--romaji-buffer to \"n\"."
  ;; nskk--romaji-buffer is defvar-local; bind it for isolation.
  (let ((nskk--romaji-buffer "n"))
    (let ((result (nskk--emit-hatsuon-prefix "n")))
      ;; Buffer must be updated to the new value "n".
      (nskk-should-equal "n" nskk--romaji-buffer)
      ;; Result must end with ん.
      (should (string-suffix-p "\u3093" result)))))

(nskk-deftest-unit emit-hatsuon-prefix-n-plus-n-returns-hatsuon
  "n+n case: return value is (prefix-kana + ん) where prefix-kana is empty."
  (let ((nskk--romaji-buffer "n"))
    (let ((result (nskk--emit-hatsuon-prefix "n")))
      ;; When buffer was just \"n\", prefix-without-n is \"\", so prefix-kana
      ;; is \"\" and the full result is just ん.
      (nskk-should-equal "\u3093" result))))

(nskk-deftest-unit emit-hatsuon-prefix-n-plus-consonant-sets-buffer
  "n+consonant case: emit-hatsuon-prefix with \"k\" sets buffer to \"k\"."
  (let ((nskk--romaji-buffer "n"))
    (nskk--emit-hatsuon-prefix "k")
    (nskk-should-equal "k" nskk--romaji-buffer)))

(nskk-deftest-unit emit-hatsuon-prefix-n-plus-consonant-returns-hatsuon
  "n+consonant case: return value is ん (no leading kana when buffer was \"n\")."
  (let ((nskk--romaji-buffer "n"))
    (let ((result (nskk--emit-hatsuon-prefix "k")))
      (nskk-should-equal "\u3093" result))))

(nskk-deftest-unit emit-hatsuon-prefix-new-buffer-value-stored
  "The new-buffer-value argument is always stored in nskk--romaji-buffer."
  (let ((nskk--romaji-buffer "n"))
    (nskk--emit-hatsuon-prefix "m")
    (nskk-should-equal "m" nskk--romaji-buffer))
  (let ((nskk--romaji-buffer "n"))
    (nskk--emit-hatsuon-prefix "abc")
    (nskk-should-equal "abc" nskk--romaji-buffer)))

(nskk-deftest-unit emit-hatsuon-prefix-with-prefix-converts-prefix-kana
  "When buffer is \"sn\", prefix-without-n is \"s\" and is converted to kana."
  ;; nskk-converter-convert "s" returns (:incomplete . "s"), which is not a
  ;; string result, so prefix-kana must be "".  Verify the function does not
  ;; signal and still emits ん.
  (let ((nskk--romaji-buffer "sn"))
    (let ((result (nskk--emit-hatsuon-prefix "n")))
      ;; Result must still contain ん.
      (should (string-suffix-p "\u3093" result))
      ;; Buffer is updated to the new-buffer-value.
      (nskk-should-equal "n" nskk--romaji-buffer))))

(nskk-deftest-unit emit-hatsuon-prefix-empty-prefix-produces-only-hatsuon
  "When buffer ends with \"n\" and prefix-without-n is empty, result is just ん."
  (let ((nskk--romaji-buffer "n"))
    (nskk-should-equal "\u3093" (nskk--emit-hatsuon-prefix "t"))))

(nskk-deftest-unit emit-hatsuon-prefix-does-not-signal-error
  "nskk--emit-hatsuon-prefix does not signal an error for typical inputs."
  (let ((nskk--romaji-buffer "n"))
    (should (nskk-should-not-error (nskk--emit-hatsuon-prefix "n")))
    (setq nskk--romaji-buffer "n")
    (should (nskk-should-not-error (nskk--emit-hatsuon-prefix "k")))
    (setq nskk--romaji-buffer "sn")
    (should (nskk-should-not-error (nskk--emit-hatsuon-prefix "n")))))

;;;
;;; nskk--key-state-base Tests
;;;

(nskk-deftest-unit key-state-base-returns-converting-when-converting
  "Returns `converting' when nskk-converting-p returns non-nil."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () t))
            ((symbol-function 'nskk--has-preedit)  (lambda () nil)))
    (should (eq (nskk--key-state-base) 'converting))))

(nskk-deftest-unit key-state-base-returns-preedit-when-has-preedit
  "Returns `preedit' when nskk--has-preedit returns non-nil and not converting."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit)  (lambda () t)))
    (should (eq (nskk--key-state-base) 'preedit))))

(nskk-deftest-unit key-state-base-returns-nil-when-neither
  "Returns nil when neither nskk-converting-p nor nskk--has-preedit is non-nil."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit)  (lambda () nil)))
    (should (null (nskk--key-state-base)))))

(nskk-deftest-unit key-state-base-converting-takes-priority-over-preedit
  "Returns `converting' when both nskk-converting-p and nskk--has-preedit are true."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () t))
            ((symbol-function 'nskk--has-preedit)  (lambda () t)))
    (should (eq (nskk--key-state-base) 'converting))))

(nskk-deftest-unit key-state-base-return-type-is-symbol
  "Return value is always a symbol (converting, preedit) or nil."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () t))
            ((symbol-function 'nskk--has-preedit)  (lambda () nil)))
    (let ((result (nskk--key-state-base)))
      (should (or (null result) (symbolp result)))))
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit)  (lambda () t)))
    (let ((result (nskk--key-state-base)))
      (should (or (null result) (symbolp result)))))
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit)  (lambda () nil)))
    (let ((result (nskk--key-state-base)))
      (should (null result)))))

(nskk-deftest-unit key-state-base-does-not-signal-error
  "nskk--key-state-base does not signal an error for any combination."
  ;; nskk-should-not-error returns the body value, which may be nil when neither
  ;; converting nor preedit.  Use condition-case directly so nil return is fine.
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit)  (lambda () nil)))
    (condition-case err
        (nskk--key-state-base)
      (error (ert-fail (format "Unexpected error: %s" err)))))
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () t))
            ((symbol-function 'nskk--has-preedit)  (lambda () nil)))
    (condition-case err
        (nskk--key-state-base)
      (error (ert-fail (format "Unexpected error: %s" err))))))

;;;
;;; nskk-prolog--dispatch-goal Additional Tests
;;;

(nskk-deftest-unit dispatch-goal-error-on-no-handler
  "nskk-prolog--dispatch-goal signals an error when no handler matches."
  ;; Bind nskk-prolog--goal-handlers to nil (empty alist) so no entry
  ;; can match the goal, forcing the error path.
  (let ((nskk-prolog--goal-handlers nil))
    (should-error (nskk-prolog--dispatch-goal '(unknown-goal) nil nil #'identity)
                  :type 'error)))

(nskk-deftest-unit dispatch-goal-first-handler-wins
  "When two handlers both match a goal, the first-registered one runs; the second does not."
  (nskk-prolog-test-with-isolated-db
    (let* ((first-fired nil)
           (second-fired nil)
           ;; Build a substitution we can pass into the handlers.
           (subst nil)
           (nskk-prolog--goal-handlers
            (list
             ;; First handler: always matches, sets first-fired.
             (list 'first-h
                   (lambda (_g) t)
                   (lambda (_g _r _s k)
                     (setq first-fired t)
                     (funcall k subst)))
             ;; Second handler: also always matches, sets second-fired.
             (list 'second-h
                   (lambda (_g) t)
                   (lambda (_g _r _s k)
                     (setq second-fired t)
                     (funcall k subst))))))
      (nskk-prolog--dispatch-goal '(any-goal) nil subst #'identity)
      (should first-fired)
      (should-not second-fired))))

(nskk-deftest-unit assertz-handler-as-prolog-goal
  "An (assertz ...) goal inside a rule body dynamically adds a fact via dispatch."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    ;; Assert a rule whose body contains an assertz goal.
    (nskk-prolog-<- (my-setup-rule)
      (assertz (my-dynamic-fact x)))
    ;; Firing the rule should cause (my-dynamic-fact x) to be asserted.
    (nskk-prolog-query-one '(my-setup-rule))
    ;; Now the fact should be queryable.
    (should (nskk-prolog-query-one '(my-dynamic-fact x)))))

(nskk-deftest-unit retract-handler-as-prolog-goal
  "An (retract ...) goal inside a rule body dynamically removes a fact via dispatch."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    ;; Assert a fact that will be removed.
    (nskk-prolog-<- (my-removable-fact y))
    ;; Assert a rule whose body contains a retract goal.
    (nskk-prolog-<- (my-teardown-rule)
      (retract (my-removable-fact y)))
    ;; Verify the fact is present before firing the rule.
    (should (nskk-prolog-query-one '(my-removable-fact y)))
    ;; Fire the rule; this should retract the fact.
    (nskk-prolog-query-one '(my-teardown-rule))
    ;; The fact should now be gone.
    (should (null (nskk-prolog-query-one '(my-removable-fact y))))))

(nskk-deftest-unit deffacts-with-unary-fact
  "nskk-prolog-deffacts correctly handles 1-arity facts (single-element rows)."
  ;; Verify macro expansion: a unary row (my-flag active) should expand to
  ;; (nskk-prolog-<- (my-flag active)).
  (let* ((expansion (macroexpand '(nskk-prolog-deffacts my-flag (active))))
         (calls (cdr expansion)))
    (should (eq (car expansion) 'progn))
    (should (= (length calls) 1))
    (let ((call (car calls)))
      (should (eq (car call) 'nskk-prolog-<-))
      (should (equal (cadr call) '(my-flag active)))))
  ;; Verify runtime behaviour: both unary facts are queryable after assertion.
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-set-index 'my-unary-pred 1 :hash)
    (nskk-prolog-deffacts my-unary-pred
      (enabled)
      (disabled))
    (should (nskk-prolog-query-one '(my-unary-pred enabled)))
    (should (nskk-prolog-query-one '(my-unary-pred disabled)))))

;;;
;;; nskk-when-prolog-holds Tests
;;;

(nskk-deftest-unit when-prolog-holds-is-macro
  "nskk-when-prolog-holds is defined as a macro."
  (should (fboundp 'nskk-when-prolog-holds))
  (should (macrop (symbol-function 'nskk-when-prolog-holds))))

(nskk-deftest-unit when-prolog-holds-expansion-uses-prolog-query
  "Macro expansion uses nskk-prolog-query (not nskk-prolog-query-one) as the guard.
This avoids nil-ambiguity: nskk-prolog-query-one returns nil for both
ground-query-with-no-solution and ground-query-that-succeeds (the latter
returns t, not nil), but that subtlety goes away with nskk-prolog-query which
returns a list of substitutions (non-nil) for success, nil for failure."
  (let* ((expansion (macroexpand '(nskk-when-prolog-holds (foo x) (bar))))
         ;; expansion: (when (nskk-prolog-query (foo x)) (bar))
         ;; (when TEST BODY): TEST is cadr, so guard is (cadr expansion)
         (guard (cadr expansion)))
    ;; The guard must be a nskk-prolog-query call
    (should (consp guard))
    (should (eq (car guard) 'nskk-prolog-query))))

(nskk-deftest-unit when-prolog-holds-body-runs-when-query-has-solutions
  "Body executes when the Prolog query returns at least one solution."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (nskk-prolog-<- (test-wphold-flag active))
    (let ((ran nil))
      (nskk-when-prolog-holds (test-wphold-flag active)
        (setq ran t))
      (should ran))))

(nskk-deftest-unit when-prolog-holds-body-skipped-when-no-solutions
  "Body does NOT execute when the Prolog query has no solutions."
  (nskk-prolog-test-with-isolated-db
    (nskk-prolog-clear-database)
    (let ((ran nil))
      (nskk-when-prolog-holds (test-wphold-no-such-pred x)
        (setq ran t))
      (should-not ran))))

(nskk-deftest-unit when-prolog-holds-nil-ambiguity-ground-query
  "nskk-prolog-query-one returns nil for a ground query with no solution AND
t for a ground query that succeeds — this creates nil-ambiguity.
nskk-when-prolog-holds uses nskk-prolog-query (returns a list) so it is
unambiguous: empty list means no solution, non-nil list means solution exists."
  (nskk-prolog-test-with-isolated-db
    ;; Verify the nil-ambiguity of nskk-prolog-query-one for ground queries.
    ;; (converting-phase active) is asserted by nskk-henkan-initialize but
    ;; after nskk-prolog-clear-database it will be absent, so query-one => nil.
    (nskk-prolog-clear-database)
    (should (null (nskk-prolog-query-one '(converting-phase active))))
    ;; Now add the fact and verify query-one returns t (not a subst, not nil).
    (nskk-prolog-<- (converting-phase active))
    (should (eq t (nskk-prolog-query-one '(converting-phase active))))
    ;; Crucially: nskk-prolog-query returns a NON-NIL list for a successful
    ;; ground query, resolving the ambiguity that affects nskk-prolog-query-one.
    (let ((result (nskk-prolog-query '(converting-phase active))))
      (should result)
      (should (listp result)))
    ;; And nskk-when-prolog-holds correctly executes its body.
    (let ((ran nil))
      (nskk-when-prolog-holds (converting-phase active)
        (setq ran t))
      (should ran))))

;;;
;;; nskk-ensure-overlay Tests
;;;

(nskk-deftest-unit ensure-overlay-creates-when-nil
  "nskk-ensure-overlay creates a new overlay when the variable is nil."
  (with-temp-buffer
    (let ((ov nil))
      (nskk-ensure-overlay ov (point-min) (point-max))
      (should (overlayp ov)))))

(nskk-deftest-unit ensure-overlay-reuses-existing-overlay
  "nskk-ensure-overlay reuses the existing overlay (eq identity preserved)."
  (with-temp-buffer
    (let ((ov (make-overlay (point-min) (point-max))))
      (let ((original-ov ov))
        (nskk-ensure-overlay ov (point-min) (point-max))
        (should (eq ov original-ov))))))

(nskk-deftest-unit ensure-overlay-moves-existing-overlay
  "When an existing overlay is reused, its position is updated."
  (with-temp-buffer
    (insert "hello world")
    (let ((ov (make-overlay 1 5)))
      ;; Now ensure the overlay covers the full buffer
      (nskk-ensure-overlay ov (point-min) (point-max))
      (should (= (overlay-start ov) (point-min)))
      (should (= (overlay-end ov) (point-max))))))

(nskk-deftest-unit ensure-overlay-applies-properties
  "Properties passed to nskk-ensure-overlay are set on the overlay."
  (with-temp-buffer
    (let ((ov nil))
      (nskk-ensure-overlay ov (point-min) (point-max)
                           'display "X" 'face 'bold)
      (should (equal (overlay-get ov 'display) "X"))
      (should (eq (overlay-get ov 'face) 'bold)))))

(nskk-deftest-unit ensure-overlay-properties-applied-to-reused-overlay
  "Properties are applied even when the overlay already exists."
  (with-temp-buffer
    (let ((ov (make-overlay (point-min) (point-max))))
      (overlay-put ov 'display "old")
      (nskk-ensure-overlay ov (point-min) (point-max)
                           'display "new")
      (should (equal (overlay-get ov 'display) "new")))))

;;;
;;; nskk-delete-overlay Tests
;;;

(nskk-deftest-unit delete-overlay-noop-when-nil
  "nskk-delete-overlay does nothing and signals no error when variable is nil."
  (let ((ov nil))
    (condition-case err
        (nskk-delete-overlay ov)
      (error (ert-fail (format "Unexpected error: %s" err))))
    (should (null ov))))

(nskk-deftest-unit delete-overlay-deletes-and-sets-to-nil
  "nskk-delete-overlay deletes the overlay and sets the variable to nil."
  (with-temp-buffer
    (let ((ov (make-overlay (point-min) (point-max))))
      (should (overlayp ov))
      (nskk-delete-overlay ov)
      ;; The variable must be nil after deletion
      (should (null ov)))))

(nskk-deftest-unit delete-overlay-overlay-is-actually-deleted
  "After nskk-delete-overlay, the overlay object is no longer live."
  (with-temp-buffer
    (let* ((ov (make-overlay (point-min) (point-max)))
           (ov-copy ov))
      (nskk-delete-overlay ov)
      ;; overlay-buffer of a deleted overlay returns nil
      (should (null (overlay-buffer ov-copy))))))

;;;
;;; nskk-search--post-process-results Tests
;;;

(nskk-deftest-unit search-post-process-removes-duplicates
  "nskk-search--post-process-results removes duplicate keys."
  (nskk-prolog-test-with-isolated-db
    (let* ((e1 (make-nskk-dict-entry :key "あ" :candidates '("亜")))
           (e2 (make-nskk-dict-entry :key "あ" :candidates '("亜")))
           (results `(("あ" . ,e1) ("あ" . ,e2)))
           (processed (nskk-search--post-process-results results nil nil)))
      (should (= (length processed) 1)))))

(nskk-deftest-unit search-post-process-applies-limit
  "nskk-search--post-process-results truncates results to LIMIT."
  (nskk-prolog-test-with-isolated-db
    (let* ((e1 (make-nskk-dict-entry :key "a" :candidates '("v1")))
           (e2 (make-nskk-dict-entry :key "b" :candidates '("v2")))
           (e3 (make-nskk-dict-entry :key "c" :candidates '("v3")))
           (results `(("a" . ,e1) ("b" . ,e2) ("c" . ,e3)))
           (processed (nskk-search--post-process-results results nil 2)))
      (should (= (length processed) 2)))))

(nskk-deftest-unit search-post-process-nil-limit-returns-all
  "When LIMIT is nil, nskk-search--post-process-results returns all results."
  (nskk-prolog-test-with-isolated-db
    (let* ((e1 (make-nskk-dict-entry :key "a" :candidates '("v1")))
           (e2 (make-nskk-dict-entry :key "b" :candidates '("v2")))
           (e3 (make-nskk-dict-entry :key "c" :candidates '("v3")))
           (results `(("a" . ,e1) ("b" . ,e2) ("c" . ,e3)))
           (processed (nskk-search--post-process-results results nil nil)))
      (should (= (length processed) 3)))))

(nskk-deftest-unit search-post-process-empty-results-returns-nil
  "nskk-search--post-process-results returns nil for empty input."
  (nskk-prolog-test-with-isolated-db
    (let ((processed (nskk-search--post-process-results nil nil nil)))
      (should (null processed)))))

(nskk-deftest-unit search-post-process-filters-by-okuri-type
  "nskk-search--post-process-results filters entries by okuri-type."
  (nskk-prolog-test-with-isolated-db
    (let* ((e-ari  (make-nskk-dict-entry :key "か" :candidates '("書") :okuri 'okuri-ari))
           (e-nasi (make-nskk-dict-entry :key "か" :candidates '("花") :okuri 'okuri-nasi))
           (results `(("か-ari"  . ,e-ari)
                      ("か-nasi" . ,e-nasi)))
           (processed (nskk-search--post-process-results results 'okuri-ari nil)))
      ;; Only the okuri-ari entry should survive
      (should (= (length processed) 1))
      (should (eq (nskk-dict-entry-okuri (cdar processed)) 'okuri-ari)))))

;;;
;;; nskk-process-japanese-input early-return refactoring Tests
;;;

(nskk-deftest-unit process-japanese-input-function-exists
  "nskk-process-japanese-input is defined and callable."
  (should (fboundp 'nskk-process-japanese-input)))

(nskk-deftest-unit process-japanese-input-okurigana-returns-early
  "When nskk-process-okurigana-input consumes the char (non-nil return),
nskk-process-japanese-input returns early without inserting kana."
  (with-temp-buffer
    (nskk-mode 1)
    (unwind-protect
        (progn
          ;; nskk-process-okurigana-input returns t => early return
          ;; with no further kana insertion side-effects.
          (cl-letf (((symbol-function 'nskk-process-okurigana-input)
                     (lambda (_char) t))
                    ((symbol-function 'nskk-convert-input-to-kana)
                     (lambda (_char)
                       (ert-fail "nskk-convert-input-to-kana must NOT be called after okurigana early return"))))
            ;; Pass a lowercase char so is-henkan-start is nil (uppercase check fails)
            (nskk-process-japanese-input ?k 1)
            ;; If we reach here without ert-fail, the early-return worked.
            (should t)))
      (nskk-mode -1))))

(nskk-deftest-unit process-japanese-input-non-okurigana-proceeds
  "When nskk-process-okurigana-input returns nil, normal kana conversion proceeds."
  (with-temp-buffer
    (nskk-mode 1)
    (unwind-protect
        (progn
          (nskk-set-mode-hiragana)
          ;; Type 'a' — okurigana should return nil (not in okurigana state),
          ;; and normal kana conversion must proceed, inserting 'あ'.
          (nskk-process-japanese-input ?a 1)
          (should (string= (buffer-string) "あ")))
      (nskk-mode -1))))

(provide 'nskk-refactoring-test)

;;; nskk-refactoring-test.el ends here
