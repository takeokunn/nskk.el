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
;;   convert-or-commit-action/2, registration-allowed/1,
;;   okurigana-trigger/1, should-update-overlay/1
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

;;;
;;; Feature Loading Tests
;;;

(nskk-deftest-unit henkan-feature-provided
  "Test that nskk-henkan feature is provided."
  (should (featurep 'nskk-henkan)))

(nskk-deftest-unit henkan-require-idempotent
  "Test that requiring nskk-henkan again is safe."
  (should (require 'nskk-henkan)))

;;;
;;; Macro API Availability Tests
;;;

(nskk-deftest-unit henkan-macro-without-modification-defined
  "Test that nskk-without-modification macro is defined."
  (should (fboundp 'nskk-without-modification)))

(nskk-deftest-unit henkan-macro-dispatch-defined
  "Test that nskk-henkan-dispatch macro is defined."
  (should (fboundp 'nskk-henkan-dispatch)))

(nskk-deftest-unit henkan-macro-with-preedit-defined
  "Test that nskk-henkan-with-preedit macro is defined."
  (should (fboundp 'nskk-henkan-with-preedit)))

(nskk-deftest-unit henkan-macro-conversion-context-defined
  "Test that nskk-with-conversion-context macro is defined."
  (should (fboundp 'nskk-with-conversion-context)))

;;;
;;; nskk-without-modification Macro Tests
;;;

(nskk-deftest-unit henkan-without-modification-inhibits-undo
  "Test that nskk-without-modification inhibits undo recording."
  (with-temp-buffer
    (let (captured-undo-list)
      (nskk-without-modification
        (setq captured-undo-list buffer-undo-list))
      (should (eq captured-undo-list t)))))

(nskk-deftest-unit henkan-without-modification-inhibits-hooks
  "Test that nskk-without-modification inhibits modification hooks."
  (with-temp-buffer
    (let (captured-inhibit)
      (nskk-without-modification
        (setq captured-inhibit inhibit-modification-hooks))
      (should (eq captured-inhibit t)))))

(nskk-deftest-unit henkan-without-modification-returns-body-value
  "Test that nskk-without-modification returns the value of its body."
  (should (equal (nskk-without-modification 42) 42))
  (should (equal (nskk-without-modification "hello") "hello"))
  (should (equal (nskk-without-modification (+ 1 2)) 3)))

;;;
;;; nskk-henkan-dispatch Macro Tests
;;;

(nskk-deftest-unit henkan-dispatch-selects-matching-clause
  "Test that nskk-henkan-dispatch executes the matching pcase clause."
  (let ((result nil))
    (nskk-henkan-dispatch action
        (nskk-prolog-query-value '(search-result-action has-candidates \?a) '\?a)
      (show-overlay    (setq result 'overlay-shown))
      (start-registration (setq result 'registration-started)))
    (should (eq result 'overlay-shown))))

(nskk-deftest-unit henkan-dispatch-selects-second-clause
  "Test that nskk-henkan-dispatch matches the second clause."
  (let ((result nil))
    (nskk-henkan-dispatch action
        (nskk-prolog-query-value '(search-result-action no-candidates \?a) '\?a)
      (show-overlay    (setq result 'overlay-shown))
      (start-registration (setq result 'registration-started)))
    (should (eq result 'registration-started))))

(nskk-deftest-unit henkan-dispatch-binds-action-sym
  "Test that nskk-henkan-dispatch binds the action symbol correctly."
  (let (captured-action)
    (nskk-henkan-dispatch my-action
        (nskk-prolog-query-value '(convert-or-commit-action converting \?a) '\?a)
      (commit-current (setq captured-action my-action)))
    (should (eq captured-action 'commit-current))))

;;;
;;; nskk-henkan-with-preedit Macro Tests
;;;

(nskk-deftest-unit henkan-with-preedit-no-marker-does-nothing
  "Test that nskk-henkan-with-preedit does nothing when no marker is set."
  (with-temp-buffer
    (let ((nskk--conversion-start-marker nil)
          (executed nil))
      (nskk-henkan-with-preedit _start
        (setq executed t))
      (should-not executed))))

(nskk-deftest-unit henkan-with-preedit-marker-at-point-does-nothing
  "Test that nskk-henkan-with-preedit does nothing when point is at marker."
  (with-temp-buffer
    (let ((nskk--conversion-start-marker (make-marker)))
      (set-marker nskk--conversion-start-marker (point-min))
      ;; Insert only the ▽ marker, point is AT the end of marker (no text after)
      (insert nskk-henkan-on-marker)
      (let ((executed nil))
        (nskk-henkan-with-preedit _start
          (setq executed t))
        (should-not executed)))))

(nskk-deftest-unit henkan-with-preedit-executes-with-text
  "Test that nskk-henkan-with-preedit executes body when preedit text exists."
  (with-temp-buffer
    (let ((nskk--conversion-start-marker (make-marker))
          (captured-start nil))
      (set-marker nskk--conversion-start-marker (point-min))
      (insert nskk-henkan-on-marker "かな")
      (nskk-henkan-with-preedit start
        (setq captured-start start))
      (should (equal captured-start (point-min))))))

;;;
;;; nskk-with-conversion-context Macro Tests
;;;

(nskk-deftest-unit henkan-conversion-context-not-converting
  "Test that nskk-with-conversion-context does nothing when not converting."
  (let ((nskk-current-state (nskk-state-create 'hiragana))
        (executed nil))
    (nskk-with-conversion-context (_c _i)
      (setq executed t))
    (should-not executed)))

(nskk-deftest-unit henkan-conversion-context-binds-candidates
  "Test that nskk-with-conversion-context binds candidates and index."
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
      (should (equal captured-index 1)))))

;;;
;;; Prolog Predicate Tests: Existing Predicates
;;;

(nskk-deftest-unit henkan-prolog-converting-phase-active
  "Test that active is a valid converting phase."
  (should (nskk-prolog-query '(converting-phase active))))

(nskk-deftest-unit henkan-prolog-converting-phase-list
  "Test that list is a valid converting phase."
  (should (nskk-prolog-query '(converting-phase list))))

(nskk-deftest-unit henkan-prolog-converting-phase-registration
  "Test that registration is a valid converting phase."
  (should (nskk-prolog-query '(converting-phase registration))))

(nskk-deftest-unit henkan-prolog-converting-phase-nil-not-valid
  "Test that nil is not a valid converting phase."
  (should-not (nskk-prolog-query '(converting-phase nil))))

(nskk-deftest-unit henkan-prolog-converting-phase-on-not-valid
  "Test that on is not a valid converting phase (it is pre-conversion)."
  (should-not (nskk-prolog-query '(converting-phase on))))

(nskk-deftest-unit henkan-prolog-okurigana-char-uppercase-a
  "Test that uppercase A maps to lowercase a."
  (should (equal (nskk-prolog-query-value `(okurigana-char ,?A \?lc) '\?lc)
                 ?a)))

(nskk-deftest-unit henkan-prolog-okurigana-char-uppercase-z
  "Test that uppercase Z maps to lowercase z."
  (should (equal (nskk-prolog-query-value `(okurigana-char ,?Z \?lc) '\?lc)
                 ?z)))

(nskk-deftest-unit henkan-prolog-okurigana-char-lowercase-not-mapped
  "Test that lowercase a is not in okurigana-char."
  (should-not (nskk-prolog-query-value `(okurigana-char ,?a \?lc) '\?lc)))

;;;
;;; Prolog Predicate Tests: New Predicates
;;;

(nskk-deftest-unit henkan-prolog-okurigana-trigger-uppercase
  "Test that okurigana-trigger succeeds for an uppercase letter."
  (should (nskk-prolog-query-one `(okurigana-trigger ,?K))))

(nskk-deftest-unit henkan-prolog-okurigana-trigger-lowercase-fails
  "Test that okurigana-trigger fails for a lowercase letter."
  (should-not (nskk-prolog-query-one `(okurigana-trigger ,?k))))

(nskk-deftest-unit henkan-prolog-candidate-nav-next-below-threshold
  "Test that candidate-nav-next-action returns select-next when count < threshold."
  (should (equal (nskk-prolog-query-value '(candidate-nav-next-action 2 5 \?a) '\?a)
                 'select-next)))

(nskk-deftest-unit henkan-prolog-candidate-nav-next-at-threshold
  "Test that candidate-nav-next-action returns show-list-next when count >= threshold."
  (should (equal (nskk-prolog-query-value '(candidate-nav-next-action 5 5 \?a) '\?a)
                 'show-list-next)))

(nskk-deftest-unit henkan-prolog-candidate-nav-next-above-threshold
  "Test that candidate-nav-next-action returns show-list-next when count > threshold."
  (should (equal (nskk-prolog-query-value '(candidate-nav-next-action 7 5 \?a) '\?a)
                 'show-list-next)))

(nskk-deftest-unit henkan-prolog-candidate-nav-prev-list-active
  "Test that candidate-nav-prev-action returns show-list-prev when list is active."
  (should (equal (nskk-prolog-query-value '(candidate-nav-prev-action list-active \?a) '\?a)
                 'show-list-prev)))

(nskk-deftest-unit henkan-prolog-candidate-nav-prev-not-active
  "Test that candidate-nav-prev-action returns select-prev when list is inactive."
  (should (equal (nskk-prolog-query-value '(candidate-nav-prev-action not-active \?a) '\?a)
                 'select-prev)))

(nskk-deftest-unit henkan-prolog-search-result-has-candidates
  "Test that search-result-action returns show-overlay when candidates exist."
  (should (equal (nskk-prolog-query-value '(search-result-action has-candidates \?a) '\?a)
                 'show-overlay)))

(nskk-deftest-unit henkan-prolog-search-result-no-candidates
  "Test that search-result-action returns start-registration when no candidates."
  (should (equal (nskk-prolog-query-value '(search-result-action no-candidates \?a) '\?a)
                 'start-registration)))

(nskk-deftest-unit henkan-prolog-convert-or-commit-converting
  "Test that convert-or-commit-action returns commit-current when converting."
  (should (equal (nskk-prolog-query-value '(convert-or-commit-action converting \?a) '\?a)
                 'commit-current)))

(nskk-deftest-unit henkan-prolog-convert-or-commit-not-converting
  "Test that convert-or-commit-action returns start-conversion when not converting."
  (should (equal (nskk-prolog-query-value '(convert-or-commit-action not-converting \?a) '\?a)
                 'start-conversion)))

(nskk-deftest-unit henkan-prolog-max-registration-depth-is-3
  "Test that max-registration-depth is 3."
  (should (equal (nskk-prolog-query-value '(max-registration-depth \?d) '\?d) 3)))

(nskk-deftest-unit henkan-prolog-registration-allowed-depth-0
  "Test that registration is allowed at depth 0."
  (should (nskk-prolog-query-one '(registration-allowed 0))))

(nskk-deftest-unit henkan-prolog-registration-allowed-depth-2
  "Test that registration is allowed at depth 2."
  (should (nskk-prolog-query-one '(registration-allowed 2))))

(nskk-deftest-unit henkan-prolog-registration-not-allowed-at-max
  "Test that registration is not allowed at depth 3 (max depth)."
  (should-not (nskk-prolog-query-one '(registration-allowed 3))))

(nskk-deftest-unit henkan-prolog-should-update-overlay-active
  "Test that should-update-overlay succeeds for active phase."
  (should (nskk-prolog-query '(should-update-overlay active))))

(nskk-deftest-unit henkan-prolog-should-update-overlay-list
  "Test that should-update-overlay succeeds for list phase."
  (should (nskk-prolog-query '(should-update-overlay list))))

(nskk-deftest-unit henkan-prolog-should-update-overlay-on-fails
  "Test that should-update-overlay fails for on phase."
  (should-not (nskk-prolog-query '(should-update-overlay on))))

;;;
;;; nskk-converting-p Tests
;;;

(nskk-deftest-unit henkan-converting-p-nil-when-no-state
  "Test that nskk-converting-p returns nil when state is nil."
  (let ((nskk-current-state nil))
    (should-not (nskk-converting-p))))

(nskk-deftest-unit henkan-converting-p-nil-phase
  "Test that nskk-converting-p returns nil when henkan phase is nil."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (should-not (nskk-converting-p))))

(nskk-deftest-unit henkan-converting-p-on-phase
  "Test that nskk-converting-p returns nil in 'on phase (preedit, not converting)."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (nskk-state-set-henkan-phase nskk-current-state 'on)
    (should-not (nskk-converting-p))))

(nskk-deftest-unit henkan-converting-p-active-phase
  "Test that nskk-converting-p returns non-nil in 'active phase."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (nskk-state-force-henkan-phase nskk-current-state 'active)
    (should (nskk-converting-p))))

(nskk-deftest-unit henkan-converting-p-list-phase
  "Test that nskk-converting-p returns non-nil in 'list phase."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (nskk-state-force-henkan-phase nskk-current-state 'list)
    (should (nskk-converting-p))))

(nskk-deftest-unit henkan-converting-p-registration-phase
  "Test that nskk-converting-p returns non-nil in 'registration phase."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (nskk-state-force-henkan-phase nskk-current-state 'registration)
    (should (nskk-converting-p))))

;;;
;;; nskk-detect-okurigana-char Tests
;;;

(nskk-deftest-unit henkan-detect-okurigana-a-to-z
  "Test that uppercase A through Z are detected as okurigana chars."
  (dolist (c (number-sequence ?A ?Z))
    (let ((result (nskk-detect-okurigana-char c)))
      (should result)
      (should (equal result (downcase c))))))

(nskk-deftest-unit henkan-detect-okurigana-lowercase-nil
  "Test that lowercase letters are not okurigana chars."
  (dolist (c (number-sequence ?a ?z))
    (should-not (nskk-detect-okurigana-char c))))

(nskk-deftest-unit henkan-detect-okurigana-digit-nil
  "Test that digits are not okurigana chars."
  (dolist (c (number-sequence ?0 ?9))
    (should-not (nskk-detect-okurigana-char c))))

(nskk-deftest-unit henkan-detect-okurigana-non-char-nil
  "Test that nskk-detect-okurigana-char returns nil for non-character input."
  (should-not (nskk-detect-okurigana-char nil))
  (should-not (nskk-detect-okurigana-char "K"))
  (should-not (nskk-detect-okurigana-char 'symbol)))

;;;
;;; nskk-next-candidate Tests
;;;

(nskk-deftest-unit henkan-next-candidate-does-nothing-when-not-converting
  "Test that nskk-next-candidate does nothing when not converting."
  (let ((nskk-current-state (nskk-state-create 'hiragana))
        (nskk--henkan-count 0))
    (nskk-next-candidate)
    (should (equal nskk--henkan-count 0))))

(nskk-deftest-unit henkan-next-candidate-inline-mode
  "Test that nskk-next-candidate calls nskk--select-candidate below threshold."
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
      (cl-letf (((symbol-function 'nskk--select-candidate)
                 (lambda (dir) (setq select-called dir))))
        (nskk-next-candidate)
        (should (eq select-called 'next))
        (should (equal nskk--henkan-count 1))))))

(nskk-deftest-unit henkan-next-candidate-list-mode-at-threshold
  "Test that nskk-next-candidate switches to list display at the threshold."
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
      (cl-letf (((symbol-function 'nskk--show-candidate-list-next)
                 (lambda () (setq list-next-called t))))
        (nskk-next-candidate)
        (should list-next-called)
        (should (equal nskk--henkan-count 5))))))

;;;
;;; nskk-previous-candidate Tests
;;;

(nskk-deftest-unit henkan-previous-candidate-does-nothing-when-not-converting
  "Test that nskk-previous-candidate does nothing when not converting."
  (let ((nskk-current-state (nskk-state-create 'hiragana))
        (nskk--henkan-count 3))
    (nskk-previous-candidate)
    (should (equal nskk--henkan-count 3))))

(nskk-deftest-unit henkan-previous-candidate-inline-mode
  "Test that nskk-previous-candidate calls nskk--select-candidate when list inactive."
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
      (cl-letf (((symbol-function 'nskk--select-candidate)
                 (lambda (dir) (setq select-called dir))))
        (nskk-previous-candidate)
        (should (eq select-called 'previous))
        (should (equal nskk--henkan-count 2))))))

(nskk-deftest-unit henkan-previous-candidate-list-mode
  "Test that nskk-previous-candidate calls show-list-prev when list is active."
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
      (cl-letf (((symbol-function 'nskk--show-candidate-list-prev)
                 (lambda () (setq list-prev-called t))))
        (nskk-previous-candidate)
        (should list-prev-called)))))

(nskk-deftest-unit henkan-previous-candidate-count-floor-at-zero
  "Test that nskk-previous-candidate does not decrement count below 0."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--henkan-count 0)
          (nskk-henkan--candidate-list-active nil)
          (nskk--conversion-start-marker (make-marker)))
      (set-marker nskk--conversion-start-marker (point-min))
      (insert "test")
      (nskk-state-set-candidates nskk-current-state '("a"))
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (cl-letf (((symbol-function 'nskk--select-candidate) #'ignore))
        (nskk-previous-candidate)
        (should (equal nskk--henkan-count 0))))))

;;;
;;; nskk-commit-current Tests
;;;

(nskk-deftest-unit henkan-commit-current-does-nothing-when-not-converting
  "Test that nskk-commit-current does nothing when not converting."
  (with-temp-buffer
    (insert "unchanged")
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-commit-current)
      (should (equal (buffer-string) "unchanged")))))

(nskk-deftest-unit henkan-commit-current-inserts-candidate
  "Test that nskk-commit-current inserts the selected candidate."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--conversion-start-marker (make-marker))
          (nskk--romaji-buffer "")
          (nskk--henkan-count 0)
          (nskk-henkan--candidate-list-active nil))
      (set-marker nskk--conversion-start-marker (point-min))
      (insert "preedit")
      (nskk-state-set-candidates nskk-current-state '("変換" "変換2"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-commit-current)
      (should (equal (buffer-string) "変換"))
      (should-not (nskk-converting-p))
      (should (equal nskk--romaji-buffer ""))
      (should (equal nskk--henkan-count 0)))))

(nskk-deftest-unit henkan-commit-current-uses-index
  "Test that nskk-commit-current uses the current index."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--conversion-start-marker (make-marker))
          (nskk--romaji-buffer "")
          (nskk--henkan-count 0)
          (nskk-henkan--candidate-list-active nil))
      (set-marker nskk--conversion-start-marker (point-min))
      (insert "preedit")
      (nskk-state-set-candidates nskk-current-state '("first" "second" "third"))
      (setf (nskk-state-current-index nskk-current-state) 2)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-commit-current)
      (should (equal (buffer-string) "third")))))

;;;
;;; nskk-convert Tests
;;;

(nskk-deftest-unit henkan-convert-does-nothing-without-preedit
  "Test that nskk-convert does nothing when no preedit exists."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--conversion-start-marker nil)
          start-conversion-called)
      (cl-letf (((symbol-function 'nskk-start-conversion)
                 (lambda () (setq start-conversion-called t))))
        (nskk-convert)
        (should-not start-conversion-called)))))

(nskk-deftest-unit henkan-convert-calls-start-conversion-with-preedit
  "Test that nskk-convert calls nskk-start-conversion when preedit exists."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--conversion-start-marker (make-marker))
          start-conversion-called)
      (set-marker nskk--conversion-start-marker (point-min))
      (insert nskk-henkan-on-marker "かな")
      (cl-letf (((symbol-function 'nskk-start-conversion)
                 (lambda () (setq start-conversion-called t))))
        (nskk-convert)
        (should start-conversion-called)))))

;;;
;;; nskk-convert-or-commit Tests
;;;

(nskk-deftest-unit henkan-convert-or-commit-commits-when-converting
  "Test that nskk-convert-or-commit commits when in conversion state."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--conversion-start-marker (make-marker))
          (nskk--romaji-buffer "")
          (nskk--henkan-count 0)
          (nskk-henkan--candidate-list-active nil))
      (set-marker nskk--conversion-start-marker (point-min))
      (insert "preedit")
      (nskk-state-set-candidates nskk-current-state '("結果"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-convert-or-commit)
      (should (equal (buffer-string) "結果"))
      (should-not (nskk-converting-p)))))

(nskk-deftest-unit henkan-convert-or-commit-converts-with-preedit
  "Test that nskk-convert-or-commit converts when preedit exists (not converting)."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--conversion-start-marker (make-marker))
          start-conversion-called)
      (set-marker nskk--conversion-start-marker (point-min))
      (insert nskk-henkan-on-marker "かな")
      (cl-letf (((symbol-function 'nskk-start-conversion)
                 (lambda () (setq start-conversion-called t))))
        (nskk-convert-or-commit)
        (should start-conversion-called)))))

;;;
;;; nskk-cancel-conversion Tests
;;;

(nskk-deftest-unit henkan-cancel-conversion-does-nothing-when-not-converting
  "Test that nskk-cancel-conversion does nothing when not converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          rollback-called)
      (cl-letf (((symbol-function 'nskk-rollback-conversion)
                 (lambda () (setq rollback-called t))))
        (nskk-cancel-conversion)
        (should-not rollback-called)))))

(nskk-deftest-unit henkan-cancel-conversion-calls-rollback-when-converting
  "Test that nskk-cancel-conversion calls nskk-rollback-conversion when converting."
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
        (should rollback-called)))))

;;;
;;; nskk-cancel-preedit Tests
;;;

(nskk-deftest-unit henkan-cancel-preedit-clears-state
  "Test that nskk-cancel-preedit clears state variables."
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
      (should-not (nskk-state-henkan-phase nskk-current-state)))))

;;;
;;; nskk-start-registration Depth Guard Tests
;;;

(nskk-deftest-unit henkan-registration-depth-guard-at-max
  "Test that nskk-start-registration returns nil when depth is at maximum."
  (let ((nskk-current-state (nskk-state-create 'hiragana))
        (nskk--registration-depth 3))  ;; at max
    (should-not (nskk-start-registration "test"))))

(nskk-deftest-unit henkan-registration-depth-guard-below-max
  "Test that nskk-start-registration proceeds when depth is below maximum."
  (let ((nskk-current-state (nskk-state-create 'hiragana))
        (nskk--registration-depth 0)
        prompt-shown)
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (p) (setq prompt-shown p) ""))
              ((symbol-function 'nskk-dict-register-word)
               #'ignore))
      (nskk-start-registration "てすと")
      (should prompt-shown)
      (should (string-match "辞書登録" prompt-shown)))))

;;;
;;; Public API Existence Tests
;;;

(nskk-deftest-unit henkan-api-core-search-defined
  "Test that nskk-core-search is defined."
  (should (fboundp 'nskk-core-search)))

(nskk-deftest-unit henkan-api-detect-okurigana-char-defined
  "Test that nskk-detect-okurigana-char is defined."
  (should (fboundp 'nskk-detect-okurigana-char)))

(nskk-deftest-unit henkan-api-process-okurigana-input-defined
  "Test that nskk-process-okurigana-input is defined."
  (should (fboundp 'nskk-process-okurigana-input)))

(nskk-deftest-unit henkan-api-commit-current-interactive
  "Test that nskk-commit-current is an interactive command."
  (should (commandp 'nskk-commit-current)))

(nskk-deftest-unit henkan-api-next-candidate-interactive
  "Test that nskk-next-candidate is an interactive command."
  (should (commandp 'nskk-next-candidate)))

(nskk-deftest-unit henkan-api-previous-candidate-interactive
  "Test that nskk-previous-candidate is an interactive command."
  (should (commandp 'nskk-previous-candidate)))

(nskk-deftest-unit henkan-api-convert-interactive
  "Test that nskk-convert is an interactive command."
  (should (commandp 'nskk-convert)))

(nskk-deftest-unit henkan-api-convert-or-commit-interactive
  "Test that nskk-convert-or-commit is an interactive command."
  (should (commandp 'nskk-convert-or-commit)))

(nskk-deftest-unit henkan-api-cancel-conversion-interactive
  "Test that nskk-cancel-conversion is an interactive command."
  (should (commandp 'nskk-cancel-conversion)))

(nskk-deftest-unit henkan-api-cancel-preedit-interactive
  "Test that nskk-cancel-preedit is an interactive command."
  (should (commandp 'nskk-cancel-preedit)))

;;;
;;; Deleted API Verification Tests
;;;

(nskk-deftest-unit henkan-deleted-start-conversion-removed
  "Test that nskk-henkan-start-conversion (parallel API) was removed."
  (should-not (fboundp 'nskk-henkan-start-conversion)))

(nskk-deftest-unit henkan-deleted-commit-conversion-removed
  "Test that nskk-henkan-commit-conversion (parallel API) was removed."
  (should-not (fboundp 'nskk-henkan-commit-conversion)))

(nskk-deftest-unit henkan-deleted-cancel-conversion-removed
  "Test that nskk-henkan-cancel-conversion (parallel API) was removed."
  (should-not (fboundp 'nskk-henkan-cancel-conversion)))

(nskk-deftest-unit henkan-deleted-in-conversion-p-removed
  "Test that nskk-henkan-in-conversion-p (parallel API) was removed."
  (should-not (fboundp 'nskk-henkan-in-conversion-p)))

(nskk-deftest-unit henkan-deleted-has-candidates-p-removed
  "Test that nskk-henkan-has-candidates-p (parallel API) was removed."
  (should-not (fboundp 'nskk-henkan-has-candidates-p)))

(nskk-deftest-unit henkan-deleted-get-current-candidate-removed
  "Test that nskk-henkan-get-current-candidate (parallel API) was removed."
  (should-not (fboundp 'nskk-henkan-get-current-candidate)))

(provide 'nskk-henkan-test)

;;; nskk-henkan-test.el ends here
