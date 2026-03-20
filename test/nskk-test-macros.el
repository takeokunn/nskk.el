;;; nskk-test-macros.el --- NSKK Test Helper Macros  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: Japanese, input, method, test, macros
;; Homepage: https://github.com/takeokunn/nskk.el

;; This file is part of NSKK.

;; NSKK is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; NSKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with NSKK.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides test helper macros for NSKK, including property-based
;; testing (PBT) macros, performance testing macros, and other test utilities.
;;
;; Features:
;; - Property-based testing macros
;; - Performance benchmarking macros
;; - Environment setup macros
;; - Assertion helper macros
;; - Mock and fixture macros
;; - Behavior DSL (nskk-describe/nskk-it/nskk-context/nskk-given/nskk-when/nskk-then)
;; - Table-driven data providers (nskk-deftest-table)
;; - Composable fixture system (nskk-deffixture/nskk-with-fixtures)
;; - Contract-based PBT (nskk-property-from-contract)

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(eval-when-compile (require 'cl-lib))

;; Optional require for PBT generators (may not exist in all environments)
(when (locate-library "nskk-pbt-generators")
  (require 'nskk-pbt-generators))


;;;;
;;;; Customization Variables for Property Tests
;;;;


(defcustom nskk-test-state-machine-runs 50
  "Default number of runs for state machine tests."
  :type 'integer
  :group 'nskk-test)

(defcustom nskk-test-sequence-runs 75
  "Default number of runs for sequence-based tests."
  :type 'integer
  :group 'nskk-test)


;;;;
;;;; Property-Based Testing Macros
;;;;

(defmacro nskk-property-test (name generators property &optional runs)
  "Define a property-based test.
NAME: Test name
GENERATORS: ((var generator-name)...) pairs
PROPERTY: Property expression to test
RUNS: Number of test runs (default: nskk-test-property-runs)"
  (declare (indent 3))
  `(ert-deftest ,(intern (format "nskk-property-%s" name)) ()
     (let ((runs (or ,runs nskk-test-property-runs))
           (failures nil))
       (dotimes (_ runs)
         (let ,(mapcar (lambda (gen)
                         `(,(car gen) (nskk-generate ',(cadr gen))))
                       generators)
           (condition-case err
               (unless ,property
                 (push (list ,@(mapcar #'car generators)) failures))
             (error
              (push (list 'error ,@(mapcar #'car generators) err)
                    failures)))))
       (when failures
         (ert-fail (format "Property failed for %d cases:\n%S"
                           (length failures)
                           (take 5 failures)))))))

(defmacro nskk-for-all (generators &rest body)
  "Test BODY for all values from GENERATORS."
  (declare (indent 1))
  `(let ,(mapcar (lambda (gen)
                   `(,(car gen) (nskk-generate ',(cadr gen))))
                 generators)
     ,@body))


;;;;
;;;; Shrinking Support Functions
;;;;

(defun nskk--test-shrink-list (list)
  "Return a list of smaller versions of LIST for shrinking.
Returns the first shrinking candidate that reduces size.
Note: this is a local helper; use `nskk-pbt-shrink.el' for full shrinking."
  (when list
    ;; Try removing first element
    (if (> (length list) 1)
        (cdr list)
      nil)))

(defun nskk--test-shrink-string (str)
  "Return a smaller version of STR for shrinking.
Returns the first shrinking candidate that reduces length.
Note: this is a local helper; use `nskk-pbt-shrink.el' for full shrinking."
  (when (and str (> (length str) 1))
    (substring str 1)))

(defun nskk--test-shrink-integer (n)
  "Return a smaller version of integer N for shrinking.
Returns N/2 if N is non-zero, otherwise nil.
Note: this is a local helper; use `nskk-pbt-shrink.el' for full shrinking."
  (when (and n (not (zerop n)))
    (/ n 2)))


;;;;
;;;; Seeded Property-Based Testing Macros
;;;;

(defmacro nskk-property-test-seeded (name generators property &optional runs seed)
  "Define a property-based test with seed tracking for reproducibility.
NAME: Test name
GENERATORS: ((var generator-name)...) pairs
PROPERTY: Property expression to test
RUNS: Number of test runs (default: nskk-test-property-runs)
SEED: Random seed for reproducibility (default: random)"
  (declare (indent 3))
  `(ert-deftest ,(intern (format "nskk-property-%s" name)) ()
     (let ((test-seed (or ,seed (abs (random))))
           (runs (or ,runs nskk-test-property-runs))
           (failures nil))
       (random test-seed)
       (message "Property test '%s' seed: %d" ',name test-seed)
       (dotimes (_ runs)
         (let ,(mapcar (lambda (gen)
                         `(,(car gen) (nskk-generate ',(cadr gen))))
                       generators)
           (condition-case err
               (unless ,property
                 (push (list :seed test-seed ,@(mapcar #'car generators)) failures))
             (error
              (push (list :seed test-seed :error ,@(mapcar #'car generators) err)
                    failures)))))
       (when failures
         (ert-fail (format "Property failed for %d cases (seed: %d):\n%S"
                           (length failures) test-seed
                           (take 5 failures)))))))

(defmacro nskk-property-test-with-shrinking (name generators property &optional runs seed)
  "Define a property-based test with automatic shrinking on failure.
Extends `nskk-property-test-seeded' with shrinking support.
When a failure is detected, attempts to find the minimal failing case.
NAME: Test name
GENERATORS: ((var generator-name)...) pairs
PROPERTY: Property expression to test
RUNS: Number of test runs (default: nskk-test-property-runs)
SEED: Random seed for reproducibility (default: random)"
  (declare (indent 3))
  `(ert-deftest ,(intern (format "nskk-property-shrinking-%s" name)) ()
     (let ((test-seed (or ,seed (abs (random))))
           (runs (or ,runs nskk-test-property-runs))
           (failure-case nil)
           (minimal-case nil))
       (random test-seed)
       (message "Property test (with shrinking) '%s' seed: %d" ',name test-seed)
       ;; First pass: find a failure
       (dotimes (_ runs)
         (unless failure-case
           (let ,(mapcar (lambda (gen)
                           `(,(car gen) (nskk-generate ',(cadr gen))))
                         generators)
             (condition-case err
                 (unless ,property
                   (setq failure-case (list ,@(mapcar #'car generators)))
                   (message "Found failure case, attempting to shrink..."))
               (error
                (setq failure-case (list :error ,@(mapcar #'car generators) err))))))
         ;; Move to next random state if no failure yet
         (unless failure-case
           (random)))
       ;; If we found a failure, try to shrink it
       (when failure-case
         (let* ((shrunk-values
                 (cl-loop for val in failure-case
                          for gen in ',generators
                          collect (nskk--shrink-value val (cadr gen))))
                (shrunk-case shrunk-values))
           ;; Try the shrunk case
           (condition-case _err
               (let ,(cl-loop for gen in generators
                              for i from 0
                              collect `(,(car gen) (nth ,i shrunk-case)))
                 (unless ,property
                   (setq minimal-case shrunk-case)))
             (error nil)))
         ;; Report results
         (if minimal-case
             (ert-fail (format "Property failed (seed: %d)\nOriginal case: %S\nMinimal case: %S"
                               test-seed failure-case minimal-case))
           (ert-fail (format "Property failed (seed: %d)\nFailing case: %S"
                             test-seed failure-case)))))))

(defun nskk--shrink-value (value generator-type)
  "Attempt to shrink VALUE based on GENERATOR-TYPE.
Returns a potentially smaller value of the same type."
  (pcase generator-type
    ((or 'romaji-string 'hiragana-string 'kanji-string 'string)
     (nskk--test-shrink-string value))
    ((or 'integer 'positive-integer 'natnum)
     (nskk--test-shrink-integer value))
    ('list (nskk--test-shrink-list value))
    (_ value)))  ; Return unchanged if no shrinker available


;;;;
;;;; State Machine Test Macro
;;;;

(defmacro nskk-state-machine-test (name initial-state transitions property &optional runs)
  "Define a state machine property test.
NAME: Test name
INITIAL-STATE: Expression to create initial state
TRANSITIONS: List of (trigger target-state) pairs
PROPERTY: Invariant that should hold after any transition
RUNS: Number of transition sequences to test
      (default: nskk-test-state-machine-runs)

The test generates random sequences of transitions and verifies that
the PROPERTY invariant holds after each transition."
  (declare (indent 3))
  `(ert-deftest ,(intern (format "nskk-state-machine-%s" name)) ()
     (let ((runs (or ,runs nskk-test-state-machine-runs))
           (failures nil)
           (test-seed (abs (random))))
       (random test-seed)
       (message "State machine test '%s' seed: %d" ',name test-seed)
       (dotimes (run runs)
         (let ((state ,initial-state)
               (steps-taken nil)
               (max-steps 20)
               (step-count 0))
           ;; Execute random sequence of transitions
           (while (and (< step-count max-steps)
                       (< (random 10) 8))  ; 80% chance to continue
             (let* ((transition-idx (random (length ',transitions)))
                    (transition (nth transition-idx ',transitions))
                    (trigger (car transition))
                    (target-state-fn (cadr transition)))
               (push (list :step step-count :trigger trigger) steps-taken)
               (setq state (funcall target-state-fn state trigger))
               (cl-incf step-count)
               ;; Check invariant after each transition
               (condition-case err
                   (unless (funcall ,property state)
                     (push (list :seed test-seed
                                 :run run
                                 :steps (nreverse steps-taken)
                                 :final-state state)
                           failures))
                 (error
                  (push (list :seed test-seed
                              :run run
                              :error err
                              :steps (nreverse steps-taken))
                        failures)))))
           ;; Also check final state
           (condition-case err
               (unless (funcall ,property state)
                 (push (list :seed test-seed
                             :run run
                             :final-check t
                             :steps (nreverse steps-taken)
                             :final-state state)
                       failures))
             (error
              (push (list :seed test-seed
                          :run run
                          :final-check t
                          :error err
                          :steps (nreverse steps-taken))
                    failures)))))
       (when failures
         (ert-fail (format "State machine invariant failed for %d cases (seed: %d):\n%S"
                           (length failures) test-seed
                           (take 3 failures)))))))


;;;;
;;;; Sequence Test Macro
;;;;

(defmacro nskk-sequence-test (name key-sequence-generator setup property &optional runs)
  "Define a sequence-based property test.
NAME: Test name
KEY-SEQUENCE-GENERATOR: Generator name for key sequences
SETUP: Setup expression before each test
PROPERTY: Invariant that should hold after sequence execution
RUNS: Number of sequences to test (default: nskk-test-sequence-runs)

The test generates random key sequences, executes them, and verifies
that the PROPERTY invariant holds."
  (declare (indent 3))
  `(ert-deftest ,(intern (format "nskk-sequence-%s" name)) ()
     (let ((runs (or ,runs nskk-test-sequence-runs))
           (failures nil)
           (test-seed (abs (random))))
       (random test-seed)
       (message "Sequence test '%s' seed: %d" ',name test-seed)
       (dotimes (run runs)
         ;; Generate key sequence
         (let* ((key-sequence (nskk-generate ',key-sequence-generator))
                (execution-context (progn ,setup)))
           (condition-case err
               ;; Execute the sequence (implementation depends on context)
               (progn
                 (dolist (key key-sequence)
                   ;; Simulate key press in context
                   (setq execution-context
                         (nskk--simulate-key execution-context key)))
                 ;; Check property after sequence execution
                 (unless (funcall ,property execution-context)
                   (push (list :seed test-seed
                               :run run
                               :key-sequence key-sequence
                               :final-context execution-context)
                         failures)))
             (error
              (push (list :seed test-seed
                          :run run
                          :error err
                          :key-sequence key-sequence)
                    failures)))))
       (when failures
         (ert-fail (format "Sequence property failed for %d cases (seed: %d):\n%S"
                           (length failures) test-seed
                           (take 3 failures)))))))

(defun nskk--simulate-key (context key)
  "Simulate KEY press in CONTEXT.
Returns updated context after processing the key.
Handles nskk-state objects, plists, and other context types."
  (cond
   ;; Handle nskk-state objects
   ((and (fboundp 'nskk-state-p) (nskk-state-p context))
    (nskk--simulate-key-for-state context key))
   ;; Handle plist contexts
   ((listp context)
    (plist-put context :last-key key))
   ;; Fallback for unknown context types
   (t
    (list :last-key key :input (if (stringp key) key "")))))

(defun nskk--simulate-key-for-state (state key)
  "Process KEY press on nskk-state STATE.
Returns updated state."
  (when (nskk-state-p state)
    (cond
     ;; Mode switch keys
     ((string= key "C-j")
      (nskk-state-set state 'mode 'hiragana)
      state)
     ((string= key "q")
      (let ((current-mode (nskk-state-mode state)))
        (cond
         ((eq current-mode 'hiragana)
          (nskk-state-set state 'mode 'katakana))
         ((eq current-mode 'katakana)
          (nskk-state-set state 'mode 'hiragana))
         (t state))
        state))
     ((string= key "l")
      (nskk-state-set state 'mode 'latin)
      state)
     ((string= key ";")
      (nskk-state-set state 'mode 'abbrev)
      state)
     ;; Regular character input (single character strings)
     ((and (stringp key) (= (length key) 1))
      (let ((current-buffer (nskk-state-input-buffer state)))
        (nskk-state-set state 'input-buffer (concat current-buffer key))
        state))
     ;; Unknown key - pass through unchanged
     (t state))))


;;;;
;;;; Performance Testing Macros
;;;;

(defmacro nskk-should-be-fast (name threshold-ms &rest body)
  "Assert that BODY completes within THRESHOLD-MS milliseconds."
  (declare (indent 2))
  `(let ((start-time (current-time)))
     ,@body
     (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
            (elapsed-ms (* 1000 elapsed)))
       (when (> elapsed-ms ,threshold-ms)
         (ert-fail (format "Too slow: %s took %.2fms (threshold: %dms)"
                           ',name elapsed-ms ,threshold-ms))))))


;;;;
;;;; Environment Setup Macros
;;;;

(defmacro nskk-with-temp-dictionary (&rest body)
  "Execute BODY with a temporary test dictionary loaded via Prolog.
Uses `nskk-with-mock-dict' with default test entries and provides
full Prolog database isolation via `nskk-prolog-test-with-isolated-db'."
  (declare (indent 0))
  `(nskk-with-mock-dict nil
     ,@body))

(defmacro nskk-with-clean-state (&rest body)
  "Execute BODY with clean NSKK state."
  (declare (indent 0))
  `(let ((nskk-current-state nil))
     (unwind-protect
         (progn ,@body)
       (setq nskk-current-state nil))))


;;;;
;;;; Assertion Helper Macros
;;;;

(defmacro nskk-should-not-error (&rest body)
  "Assert that BODY does not signal any error."
  (declare (indent 0))
  `(condition-case err
       (progn ,@body)
     (error
      (ert-fail (format "Unexpected error: %s" err)))))


;;;;
;;;; Integration Test Macros
;;;;

(defmacro nskk-scenario-test (name scenario &rest body)
  "Define a scenario-based integration test.
NAME: Test name
SCENARIO: Scenario description
BODY: Test implementation"
  (declare (indent 2))
  `(ert-deftest ,(intern (format "nskk-scenario-%s" name)) ()
     ,scenario
     (nskk-with-clean-state
       (nskk-with-temp-dictionary
         ,@body))))


;;;;
;;;; Test Composition Macros
;;;;

(defmacro nskk-test-suite (name &rest tests)
  "Define a test suite containing TESTS."
  (declare (indent 1))
  `(defun ,(intern (format "nskk-test-suite-%s" name)) ()
     (interactive)
     (message "Running test suite: %s" ',name)
     ,@(mapcar (lambda (test)
                 `(progn
                    (message "  Running: %s" ',test)
                    (funcall ',test)))
               tests)
     (message "Test suite %s is complete" ',name)))



;;;;
;;;; Behavior DSL (describe/it/given/when/then)
;;;;

(defun nskk--normalize-test-name (str)
  "Normalize STR to a valid ERT test name component.
Converts to lowercase, replaces non-alphanumeric chars with hyphens,
collapses multiple hyphens, and strips leading/trailing hyphens."
  (let* ((lower (downcase str))
         (replaced (replace-regexp-in-string "[^a-z0-9]+" "-" lower))
         (stripped (replace-regexp-in-string "^-+\\|-+$" "" replaced)))
    stripped))

(defmacro nskk-given (&rest setups)
  "Document SETUP phase of a test.  Evaluates SETUPS sequentially.
Use inside `nskk-it' to clarify test preconditions."
  (declare (indent 0))
  `(progn ,@setups))

(defmacro nskk-when (&rest actions)
  "Document ACTION phase of a test.  Evaluates ACTIONS sequentially.
Use inside `nskk-it' to clarify the operation under test."
  (declare (indent 0))
  `(progn ,@actions))

(defmacro nskk-then (&rest assertions)
  "Document and execute ASSERTIONS phase of a test.
Use inside `nskk-it' to clarify expected outcomes."
  (declare (indent 0))
  `(progn ,@assertions))

(defun nskk--expand-it-form (prefix it-form)
  "Expand a single IT-FORM into an `ert-deftest' with PREFIX.
IT-FORM must be (nskk-it BEHAVIOR &rest BODY).
Returns an `ert-deftest' form or signals an error."
  (unless (and (listp it-form)
               (eq (car it-form) 'nskk-it)
               (>= (length it-form) 2))
    (error "nskk--expand-it-form: expected (nskk-it BEHAVIOR ...), got %S" it-form))
  (let* ((behavior (cadr it-form))
         (body (cddr it-form))
         (test-name (intern (format "nskk-it/%s/%s"
                                    prefix
                                    (nskk--normalize-test-name behavior)))))
    `(ert-deftest ,test-name ()
       ,behavior
       (let ((nskk--test-mode t))
         (nskk--test-setup)
         (unwind-protect
             (progn ,@body)
           (nskk--test-teardown))))))

(defmacro nskk-describe (description &rest body)
  "Group related test behaviors under DESCRIPTION.
Each `nskk-it' or `nskk-it-k' form in BODY expands to an independent
`ert-deftest'.  Each `nskk-context' form in BODY provides sub-grouping.
Non-nskk-it/nskk-it-k/nskk-context forms are emitted as-is (for shared
let/defvar etc).

Test names follow the pattern: nskk-it/DESCRIPTION/BEHAVIOR
where both DESCRIPTION and BEHAVIOR are normalized (lowercase, hyphens).

Example:
  (nskk-describe \"romaji conversion\"
    (nskk-it \"converts ka to か\"
      (nskk-given (let ((state (nskk-state-create \\='hiragana))))
      (nskk-when  (nskk-process-input state \"ka\"))
      (nskk-then  (should (equal (nskk-state-buffer state) \"か\"))))))"
  (declare (indent 1))
  (let ((prefix (nskk--normalize-test-name description)))
    `(progn
       ,@(mapcar
          (lambda (form)
            (cond
             ;; nskk-it → expand to named ert-deftest
             ((and (listp form) (eq (car form) 'nskk-it))
              (nskk--expand-it-form prefix form))
             ;; nskk-it-k → expand CPS /k test to named ert-deftest
             ((and (listp form) (eq (car form) 'nskk-it-k))
              (nskk--expand-it-k-form prefix form))
             ;; nskk-context → nested group with sub-prefix
             ((and (listp form) (eq (car form) 'nskk-context))
              (let* ((ctx-desc (cadr form))
                     (ctx-body (cddr form))
                     (ctx-prefix (format "%s/%s"
                                         prefix
                                         (nskk--normalize-test-name ctx-desc))))
                `(progn
                   ,@(mapcar
                      (lambda (inner)
                        (cond
                         ((and (listp inner) (eq (car inner) 'nskk-it))
                          (nskk--expand-it-form ctx-prefix inner))
                         ((and (listp inner) (eq (car inner) 'nskk-it-k))
                          (nskk--expand-it-k-form ctx-prefix inner))
                         (t inner)))
                      ctx-body))))
             ;; Other forms pass through (shared variables, constants, etc.)
             (t form)))
          body))))

(defmacro nskk-context (_description &rest body)
  "Provide sub-grouping within a `nskk-describe' block.
_DESCRIPTION is a string label used only by `nskk-describe' for test naming.
When used outside `nskk-describe', expands to a plain progn (no test naming).
BODY may contain `nskk-it' forms and regular Emacs Lisp forms."
  (declare (indent 1))
  `(progn ,@body))

(defmacro nskk-it (behavior &rest body)
  "Define a single test behavior BEHAVIOR with BODY.
When used inside `nskk-describe', the containing describe macro renames
this test.  When used standalone, expands to a unit test named
nskk-unit-BEHAVIOR (normalized).

BEHAVIOR should be a string describing what is being tested."
  (declare (indent 1))
  (let ((test-name (intern (format "nskk-unit-%s"
                                   (nskk--normalize-test-name behavior)))))
    `(ert-deftest ,test-name ()
       ,behavior
       (let ((nskk--test-mode t))
         (nskk--test-setup)
         (unwind-protect
             (progn ,@body)
           (nskk--test-teardown))))))

(defun nskk--parse-it-k-clauses (rest)
  "Parse :found and :not-found clauses from REST keyword-argument list.
REST is the portion of an `nskk-it-k' form after the /k call expression.

Expected shape:
  :found (BINDING) BODY-FOUND...
  :not-found () BODY-NOT-FOUND...   ; optional

Returns a plist with keys:
  :found-binding   — symbol (or _ for ignored)
  :found-body      — list of forms
  :not-found-body  — list of forms (default: single ert-fail form)"
  (let (found-binding found-body not-found-body)
    ;; :found clause — required
    (unless (eq (car rest) :found)
      (error "nskk-it-k: expected :found keyword, got %S" (car rest)))
    (setq rest (cdr rest))
    (let ((binding-list (car rest)))
      (unless (listp binding-list)
        (error "nskk-it-k: :found binding must be a list, got %S" binding-list))
      (setq found-binding (if (null binding-list) '_ (car binding-list)))
      (setq rest (cdr rest)))
    ;; collect found-body forms until :not-found or end
    (while (and rest (not (eq (car rest) :not-found)))
      (push (car rest) found-body)
      (setq rest (cdr rest)))
    (setq found-body (nreverse found-body))
    ;; :not-found clause — optional
    (if (eq (car rest) :not-found)
        (progn
          (setq rest (cdr rest))
          (let ((binding-list (car rest)))
            (unless (listp binding-list)
              (error "nskk-it-k: :not-found binding must be a list, got %S"
                     binding-list))
            (setq rest (cdr rest)))
          (setq not-found-body (or rest
                                   '((ert-fail "on-not-found called unexpectedly")))))
      (setq not-found-body '((ert-fail "on-not-found called unexpectedly"))))
    (list :found-binding found-binding
          :found-body    found-body
          :not-found-body not-found-body)))

(defun nskk--expand-it-k-form (prefix it-k-form)
  "Expand a single IT-K-FORM into an `ert-deftest' with PREFIX.
IT-K-FORM must be (nskk-it-k BEHAVIOR K-CALL :found (BINDING) BODY...).
Returns an `ert-deftest' form or signals an error."
  (unless (and (listp it-k-form)
               (eq (car it-k-form) 'nskk-it-k)
               (>= (length it-k-form) 4))
    (error "nskk--expand-it-k-form: expected (nskk-it-k BEHAVIOR K-CALL ...), got %S"
           it-k-form))
  (let* ((behavior  (cadr it-k-form))
         (k-call    (caddr it-k-form))
         (rest      (cdddr it-k-form))
         (clauses   (nskk--parse-it-k-clauses rest))
         (f-binding (plist-get clauses :found-binding))
         (f-body    (plist-get clauses :found-body))
         (nf-body   (plist-get clauses :not-found-body))
         (test-name (intern (format "nskk-it/%s/%s"
                                    prefix
                                    (nskk--normalize-test-name behavior)))))
    `(ert-deftest ,test-name ()
       ,behavior
       (let ((nskk--test-mode t))
         (nskk--test-setup)
         (unwind-protect
             ,(append k-call
                      (list `(lambda (,f-binding) ,@f-body)
                            `(lambda () ,@nf-body)))
           (nskk--test-teardown))))))

(defmacro nskk-it-k (behavior k-call &rest clauses)
  "Define a CPS /k function test with automatic continuation injection.
BEHAVIOR is a string describing the test.
K-CALL is the /k function call expression WITHOUT the two continuation
arguments — they are synthesized from the :found and :not-found clauses.

CLAUSES keyword syntax:
  :found (BINDING) BODY-FOUND...
    BINDING is the symbol that receives the on-found value.
    Use _ to ignore the value.
  :not-found () BODY-NOT-FOUND...   (optional)
    Body executed when on-not-found is called.
    When omitted, on-not-found signals `ert-fail'.

The macro expands to an `nskk-it' form whose body appends the two
continuation lambdas to K-CALL.

Example:
  (nskk-it-k \"retrieves stored value\"
    (nskk-cache-lru-get/k cache \"key\")
    :found (result)
      (should (equal result \"expected\"))
    :not-found ()
      (ert-fail \"Expected found but got not-found\"))

When used standalone (outside `nskk-describe'), expands to an ERT test
named nskk-unit-BEHAVIOR (normalized).  Inside `nskk-describe' or
`nskk-context', the containing macro supplies the prefix."
  (declare (indent 2))
  (let* ((parsed   (nskk--parse-it-k-clauses clauses))
         (f-binding (plist-get parsed :found-binding))
         (f-body    (plist-get parsed :found-body))
         (nf-body   (plist-get parsed :not-found-body))
         (test-name (intern (format "nskk-unit-%s"
                                    (nskk--normalize-test-name behavior)))))
    `(ert-deftest ,test-name ()
       ,behavior
       (let ((nskk--test-mode t))
         (nskk--test-setup)
         (unwind-protect
             ,(append k-call
                      (list `(lambda (,f-binding) ,@f-body)
                            `(lambda () ,@nf-body)))
           (nskk--test-teardown))))))


;;;;
;;;; Table-Driven Data Providers
;;;;

(defmacro nskk-deftest-table (name &rest keys)
  "Define parameterized unit tests from a multi-column table.
NAME: base name (tests get prefix nskk-unit-)
KEYS: keyword arguments:
  :columns (COL1 COL2 ...) - column variable names (bound in :body)
  :rows    ((VAL1 VAL2 ...) ...) - rows of test data
  :description STR  - shared description string
  :body FORM        - test body using the column variables

Each row expands to: nskk-unit-NAME/row-00, nskk-unit-NAME/row-01, ...

Example:
  (nskk-deftest-table dict-search
    :columns (input mode expected-count)
    :rows    ((\"か\" hiragana 3)
              (\"ai\" ascii    1))
    :body    (should (= expected-count
                        (length (nskk-search-dict input mode)))))"
  (declare (indent 1))
  (let ((columns (plist-get keys :columns))
        (rows (plist-get keys :rows))
        (description (plist-get keys :description))
        (body (plist-get keys :body)))
    (unless columns (error "nskk-deftest-table: :columns is required"))
    (unless rows    (error "nskk-deftest-table: :rows is required"))
    (unless body    (error "nskk-deftest-table: :body is required"))
    `(progn
       ,@(cl-loop for row in rows
                  for i from 0
                  collect
                  (let* ((test-name (intern (format "nskk-unit-%s/row-%02d" name i)))
                         (doc (or description
                                  (format "Row %02d: %S" i row)))
                         (bindings (cl-mapcar (lambda (col val) `(,col ',val))
                                              columns row)))
                    `(ert-deftest ,test-name ()
                       ,doc
                       (let ,bindings
                         ,body)))))))


;;;;
;;;; Composable Fixture System
;;;;

(defmacro nskk-deffixture (name arglist &rest macro-body)
  "Define a named, composable fixture macro NAME.
NAME: macro name (conventionally with-SOMETHING)
ARGLIST: explicit arguments to the fixture (excluding the implicit body)
MACRO-BODY: the macro expansion body (may reference `body' for wrapped forms)

The generated macro signature is: (NAME ARGLIST... &rest body)
where BODY is the test code to run inside the fixture.

Example:
  (nskk-deffixture with-hiragana-state ()
    `(let ((state (nskk-state-create \\='hiragana)))
       (unwind-protect (progn ,@body)
         (nskk-state-reset state))))

  (nskk-deffixture with-mock-dict (entries)
    `(nskk-with-mock-dict ,entries ,@body))"
  (declare (indent 2))
  `(defmacro ,name ,(append arglist '(&rest body))
     (declare (indent ,(length arglist)))
     ,@macro-body))

(defmacro nskk-with-fixtures (fixtures &rest body)
  "Compose FIXTURES as nested wrappers around BODY.
FIXTURES: list of fixture invocations.  Each element is either:
  - A symbol: (fixture-name)  — fixture with no extra args
  - A list:   (fixture-name arg1 arg2 ...) — fixture with args
Fixtures are applied left-to-right (leftmost is outermost wrapper).

Example:
  (nskk-with-fixtures (with-hiragana-state
                       (with-mock-dict nil))
    (should (nskk-state-p state)))"
  (declare (indent 1))
  (if (null fixtures)
      `(progn ,@body)
    (let* ((first (car fixtures))
           (rest  (cdr fixtures))
           (call  (if (listp first) first (list first))))
      `(,@call
        (nskk-with-fixtures ,rest
          ,@body)))))


;;;;
;;;; Contract-Based Property Testing
;;;;

(defmacro nskk-property-from-contract (fn &rest keys)
  "Generate a property-based test from FN's behavioral contract.
FN: unary function symbol under test (called as (FN input))
KEYS: keyword arguments:
  :precondition FORM  - expression that generates one valid input value
                        (bound as `input' in postcondition/invariant)
  :postcondition FORM - property to verify (uses `input' and `result')
  :invariant FORM     - additional invariant checked on `result'
  :runs N             - number of test runs (default: 100)
  :name SYM           - override test name (default: nskk-contract-FN)

The generated test name is: nskk-contract-FN

Example:
  (nskk-property-from-contract nskk-convert-romaji
    :precondition  (nskk-generate \\='romaji-pattern)
    :postcondition (stringp result)
    :invariant     (>= (length result) 0)
    :runs 100)"
  (declare (indent 1))
  (let* ((precond   (plist-get keys :precondition))
         (postcond  (plist-get keys :postcondition))
         (invariant (plist-get keys :invariant))
         (runs      (or (plist-get keys :runs) 100))
         (override  (plist-get keys :name))
         (test-name (or override (intern (format "nskk-contract-%s" fn)))))
    (unless precond
      (error "nskk-property-from-contract: :precondition is required"))
    (unless postcond
      (error "nskk-property-from-contract: :postcondition is required"))
    `(ert-deftest ,test-name ()
       ,(format "Contract test: verifies %s honors its pre/postconditions." fn)
       (let ((failures nil)
             (test-seed (abs (random)))
             (runs ,runs))
         (random test-seed)
         (message "Contract test `%s' seed: %d" ',fn test-seed)
         (dotimes (_ runs)
           (condition-case err
               (let* ((input  ,precond)
                      (result (funcall #',fn input)))
                 (unless ,postcond
                   (push (list :postcondition-failed
                               :input input :result result)
                         failures))
                 ,@(when invariant
                     `((unless ,invariant
                         (push (list :invariant-failed
                                     :input input :result result)
                               failures)))))
             (error
              (push (list :error-during-call :fn ',fn :err err) failures))))
         (when failures
           (ert-fail
            (format "Contract for `%s' violated (seed: %d) — %d/%d cases failed:\n%S"
                    ',fn test-seed (length failures) runs
                    (seq-take failures 5))))))))


;;;;
;;;; Henkan State Assertion Macros
;;;;

(defmacro nskk-should-convert-to (romaji expected)
  "Assert that ROMAJI converts to EXPECTED kana via `nskk-convert-romaji'."
  `(should (equal (nskk-convert-romaji ,romaji) ,expected)))

(defmacro nskk-with-henkan-state (phase candidates &rest body)
  "Execute BODY with henkan state set to PHASE with CANDIDATES.
Provides bindings: `nskk-current-state', `nskk--conversion-start-marker',
`nskk--romaji-buffer', `nskk--henkan-count',
`nskk--henkan-candidate-list-active'."
  (declare (indent 2))
  `(with-temp-buffer
     (let ((nskk-current-state (nskk-state-create 'hiragana))
           (nskk--conversion-start-marker (make-marker))
           (nskk--romaji-buffer "")
           (nskk--henkan-count 0)
           (nskk--henkan-candidate-list-active nil))
       (set-marker nskk--conversion-start-marker (point-min))
       (insert "preedit")
       (when ,candidates
         (nskk-state-set-candidates nskk-current-state ,candidates))
       (nskk-state-force-henkan-phase nskk-current-state ,phase)
       ,@body)))


;;;;
;;;; Exhaustive and Invariant Property Macros
;;;;

(defmacro nskk-property-test-exhaustive (name domain property)
  "Define a property test that checks PROPERTY for every element in DOMAIN.
NAME: test name (produces nskk-exhaustive-NAME)
DOMAIN: expression that evaluates to a list of all values to test
PROPERTY: form evaluated with `item' bound to each domain element"
  (declare (indent 2))
  `(ert-deftest ,(intern (format "nskk-exhaustive-%s" name)) ()
     ,(format "Exhaustive property test: %s" name)
     (let ((failures nil))
       (dolist (item ,domain)
         (condition-case err
             (unless ,property
               (push item failures))
           (error (push (list :error item err) failures))))
       (when failures
         (ert-fail (format "Exhaustive property failed for %d/%d items:\n%S"
                           (length failures)
                           (length ,domain)
                           (seq-take failures 10)))))))

(defmacro nskk-assert-state-invariant (state-form &rest invariants)
  "Assert that STATE-FORM satisfies all INVARIANTS.
Each invariant is a form evaluated with `state' bound to STATE-FORM's value.
Fails immediately on the first violated invariant."
  (declare (indent 1))
  `(let ((state ,state-form))
     ,@(mapcar (lambda (inv)
                 `(unless ,inv
                    (ert-fail (format "State invariant violated: %S\nState: %S"
                                      ',inv state))))
               invariants)))


(provide 'nskk-test-macros)

;;; nskk-test-macros.el ends here
