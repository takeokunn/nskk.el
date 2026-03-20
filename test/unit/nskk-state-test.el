;;; nskk-state-test.el --- State management tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-state.el covering:
;; - State creation and initialization
;; - Getters and setters
;; - Mode transitions and validation
;; - Buffer management
;; - Candidate navigation
;; - Metadata operations
;; - Undo/redo stack management

;;; Code:

(require 'ert)
(require 'nskk-state)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)

(eval-when-compile (require 'cl-lib))


;;;;
;;;; Local helpers for sequence-based state-struct tests
;;;; (pure nskk-state operations; no nskk-input dependency)
;;;;

(defun nskk-state-test--process-key (state key)
  "Process a single KEY press on STATE, returning updated state.
KEY is a string representing the key (e.g., \"a\", \"C-j\", \"q\").
Delegates to `nskk--simulate-key-for-state' from nskk-test-macros."
  (nskk--simulate-key-for-state state key))

(defun nskk-state-test--execute-keys (state key-sequence)
  "Execute KEY-SEQUENCE on STATE, returning updated state."
  (let ((current-state state))
    (dolist (key key-sequence current-state)
      (setq current-state (nskk-state-test--process-key current-state key)))))

(defun nskk-state-test--valid-state-p (state)
  "Check if STATE has valid structure and values."
  (and (nskk-state-p state)
       (nskk-state-valid-mode-p (nskk-state-mode state))
       (stringp (nskk-state-input-buffer state))
       (stringp (nskk-state-converted-buffer state))
       (listp (nskk-state-candidates state))
       (integerp (nskk-state-current-index state))
       (>= (nskk-state-current-index state) 0)
       (nskk-state-valid-mode-p (nskk-state-previous-mode state))
       (listp (nskk-state-undo-stack state))
       (listp (nskk-state-redo-stack state))))

(defun nskk-state-test--buffer-bounds-p (state)
  "Check if STATE buffer lengths are within reasonable bounds."
  (and (nskk-state-p state)
       (let ((input-len (length (nskk-state-input-buffer state)))
             (converted-len (length (nskk-state-converted-buffer state))))
         (and (>= input-len 0)
              (>= converted-len 0)
              (<= input-len 1000)
              (<= converted-len 1000)))))

(defun nskk-state-test--romaji-buffer-consistent-p (state)
  "Check if romaji buffer in STATE contains only valid characters."
  (let ((romaji (nskk-state-input-buffer state)))
    (and (stringp romaji)
         (or (string-empty-p romaji)
             (string-match-p "^[a-zA-Z]*$" romaji)))))

(defun nskk-state-test--mode-valid-p (state)
  "Check if mode in STATE is one of the valid NSKK modes."
  (nskk-state-valid-mode-p (nskk-state-mode state)))

(defun nskk-state-test--states-equal-p (state1 state2)
  "Check if STATE1 and STATE2 have equivalent mode and buffer content."
  (and (nskk-state-p state1)
       (nskk-state-p state2)
       (eq (nskk-state-mode state1) (nskk-state-mode state2))
       (string= (nskk-state-input-buffer state1)
                (nskk-state-input-buffer state2))
       (string= (nskk-state-converted-buffer state1)
                (nskk-state-converted-buffer state2))))

(defun nskk-state-test--simulate-undo (state)
  "Simulate undo operation on STATE, returning updated state."
  (when (and (nskk-state-p state)
             (nskk-state-undo-stack state))
    (let* ((undo-item (car (nskk-state-undo-stack state)))
           (remaining-stack (cdr (nskk-state-undo-stack state)))
           (redo-stack (nskk-state-redo-stack state)))
      (setf (nskk-state-redo-stack state)
            (cons (list :mode (nskk-state-mode state)
                        :input-buffer (nskk-state-input-buffer state))
                  redo-stack))
      (when (listp undo-item)
        (when (plist-get undo-item :mode)
          (setf (nskk-state-mode state) (plist-get undo-item :mode)))
        (when (plist-get undo-item :input-buffer)
          (setf (nskk-state-input-buffer state) (plist-get undo-item :input-buffer))))
      (setf (nskk-state-undo-stack state) remaining-stack)))
  state)

(defun nskk-state-test--simulate-redo (state)
  "Simulate redo operation on STATE, returning updated state."
  (when (and (nskk-state-p state)
             (nskk-state-redo-stack state))
    (let* ((redo-item (car (nskk-state-redo-stack state)))
           (remaining-stack (cdr (nskk-state-redo-stack state)))
           (undo-stack (nskk-state-undo-stack state)))
      (setf (nskk-state-undo-stack state)
            (cons (list :mode (nskk-state-mode state)
                        :input-buffer (nskk-state-input-buffer state))
                  undo-stack))
      (when (listp redo-item)
        (when (plist-get redo-item :mode)
          (setf (nskk-state-mode state) (plist-get redo-item :mode)))
        (when (plist-get redo-item :input-buffer)
          (setf (nskk-state-input-buffer state) (plist-get redo-item :input-buffer))))
      (setf (nskk-state-redo-stack state) remaining-stack)))
  state)

(defun nskk-state-test--simulate-japanese-input (state key-sequence)
  "Simulate Japanese input KEY-SEQUENCE on STATE, returning updated state."
  (let ((current-state state))
    (dolist (key key-sequence current-state)
      (when (and (stringp key) (= (length key) 1))
        (let ((char (aref key 0)))
          (when (and (<= ?a char) (<= char ?z))
            (let ((current-buffer (nskk-state-input-buffer current-state)))
              (nskk-state-set current-state 'input-buffer
                              (concat current-buffer key)))))))))

(defun nskk-state-test--get-mode-with-state/k (mode on-found on-not-found)
  "Invoke `nskk-state-get-mode/k' with temporary MODE as current state."
  (let ((nskk-current-state (nskk-state-create mode)))
    (nskk-state-get-mode/k on-found on-not-found)))

;; Test Constants

(defconst nskk-state-test-valid-modes
  '(ascii hiragana katakana katakana-半角 abbrev latin jisx0208-latin)
  "Valid NSKK modes for testing.")

;;;
;;; State Creation Tests
;;;

(nskk-describe "nskk-state-create"
  (nskk-it "creates state with default (ascii) mode and all nil fields"
    (let ((state (nskk-state-create)))
      (nskk-then
        (should (nskk-state-p state))
        (should (eq (nskk-state-mode state) 'ascii))
        (should (string= (nskk-state-input-buffer state) ""))
        (should (string= (nskk-state-converted-buffer state) ""))
        (should (null (nskk-state-candidates state)))
        (should (= (nskk-state-current-index state) 0))
        (should (null (nskk-state-henkan-position state)))
        (should (null (nskk-state-marker-position state)))
        (should (null (nskk-state-undo-stack state)))
        (should (null (nskk-state-redo-stack state)))
        (should (null (nskk-state-henkan-phase state)))
        (should (null (nskk-state-metadata state))))))

  (nskk-it "creates hiragana state"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-then
        (should (nskk-state-p state))
        (should (eq (nskk-state-mode state) 'hiragana))
        (should (eq (nskk-state-previous-mode state) 'hiragana)))))

  (nskk-it "creates katakana state"
    (let ((state (nskk-state-create 'katakana)))
      (nskk-then
        (should (nskk-state-p state))
        (should (eq (nskk-state-mode state) 'katakana)))))

  (nskk-it "falls back to ascii for invalid mode"
    (let ((state (nskk-state-create 'invalid-mode)))
      (nskk-then
        (should (nskk-state-p state))
        (should (eq (nskk-state-mode state) 'ascii)))))

  (nskk-it "creates jisx0208-latin state"
    (let ((state (nskk-state-create 'jisx0208-latin)))
      (nskk-then
        (should (nskk-state-p state))
        (should (eq (nskk-state-mode state) 'jisx0208-latin)))))

  (nskk-it "creates valid state for all valid modes"
    (dolist (mode nskk-state-test-valid-modes)
      (let ((state (nskk-state-create mode)))
        (should (nskk-state-p state))
        (should (eq (nskk-state-mode state) mode))))))

;;;
;;; nskk-state-create/k CPS Tests
;;;

;; Table-driven: all valid modes → on-found called with a valid state.
(nskk-describe "nskk-state-create/k"
  (nskk-deftest-table state-create/k-valid-modes
    :description "Calls on-found with a valid nskk-state for every valid mode"
    :columns (mode)
    :rows ((hiragana) (katakana) (katakana-半角) (abbrev) (ascii) (latin) (jisx0208-latin))
    :body (let (got-state got-not-found)
            (nskk-state-create/k mode
              (lambda (s) (setq got-state s))
              (lambda () (setq got-not-found t)))
            (should (nskk-state-p got-state))
            (should (eq (nskk-state-mode got-state) mode))
            (should-not got-not-found)))

  ;; NOTE: nskk-state-create/k has an &optional initial-mode parameter.
  ;; The /k variant signature is (initial-mode on-found on-not-found), so
  ;; nil must be passed explicitly as initial-mode to get the default (ascii).
  (nskk-it "calls on-found with default (ascii) state when nil mode given"
    (let (got-state)
      (nskk-state-create/k nil
        (lambda (s) (setq got-state s))
        (lambda () (ert-fail "Expected on-found for nskk-state-create/k")))
      (should (nskk-state-p got-state))
      (should (eq (nskk-state-mode got-state) 'ascii))))

  (nskk-it "calls on-found with ascii fallback for invalid mode"
    (let (got-state)
      (nskk-state-create/k 'not-a-valid-mode
        (lambda (s) (setq got-state s))
        (lambda () (ert-fail "Expected on-found even for invalid mode (fallback to ascii)")))
      (should (nskk-state-p got-state))
      (should (eq (nskk-state-mode got-state) 'ascii)))))

;;;
;;; Getter Tests
;;;

(nskk-describe "nskk-state-get"
  (nskk-it "returns correct value for valid symbol slots"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-then
        (should (eq (nskk-state-get state 'mode) 'hiragana))
        (should (string= (nskk-state-get state 'input-buffer) ""))
        (should (= (nskk-state-get state 'current-index) 0)))))

  (nskk-it "accepts string slot name"
    (let ((state (nskk-state-create 'katakana)))
      (nskk-then
        (should (eq (nskk-state-get state "mode") 'katakana)))))

  (nskk-it "returns nil for invalid slot"
    (let ((state (nskk-state-create)))
      (nskk-then
        (should (null (nskk-state-get state 'invalid-slot))))))

  (nskk-it "returns nil for nil state"
    (nskk-then
      (should (null (nskk-state-get nil 'mode)))))

  (nskk-it "returns non-nil for slots with defaults and nil for optional slots"
    (let ((state (nskk-state-create)))
      (nskk-then
        ;; These slots have non-nil default values
        (dolist (slot '(mode input-buffer converted-buffer
                            current-index previous-mode))
          (should (nskk-state-get state slot)))
        ;; These slots default to nil - verify they are accessible without error
        (dolist (slot '(candidates henkan-position marker-position
                                  undo-stack redo-stack henkan-phase metadata))
          (should-not (nskk-state-get state slot)))))))

;;;
;;; Setter Tests
;;;

(nskk-describe "nskk-state-set"
  (nskk-it "sets mode and updates previous-mode"
    (let ((state (nskk-state-create 'ascii)))
      (nskk-then
        (should (eq (nskk-state-set state 'mode 'hiragana) 'hiragana))
        (should (eq (nskk-state-mode state) 'hiragana))
        (should (eq (nskk-state-previous-mode state) 'ascii)))))

  (nskk-it "sets input-buffer"
    (let ((state (nskk-state-create)))
      (nskk-then
        (should (equal (nskk-state-set state 'input-buffer "test") "test"))
        (should (string= (nskk-state-input-buffer state) "test")))))

  (nskk-it "sets converted-buffer"
    (let ((state (nskk-state-create)))
      (nskk-then
        (should (equal (nskk-state-set state 'converted-buffer "converted") "converted"))
        (should (string= (nskk-state-converted-buffer state) "converted")))))

  (nskk-it "sets candidates"
    (let ((state (nskk-state-create))
          (candidates '("candidate1" "candidate2" "candidate3")))
      (nskk-then
        (should (eq (nskk-state-set state 'candidates candidates) candidates))
        (should (equal (nskk-state-candidates state) candidates)))))

  (nskk-it "sets current-index"
    (let ((state (nskk-state-create)))
      (nskk-then
        (should (eq (nskk-state-set state 'current-index 5) 5))
        (should (= (nskk-state-current-index state) 5)))))

  (nskk-it "sets henkan-position"
    (let ((state (nskk-state-create)))
      (nskk-then
        (should (eq (nskk-state-set state 'henkan-position 10) 10))
        (should (= (nskk-state-henkan-position state) 10)))))

  (nskk-it "raises error for invalid mode and leaves state unchanged"
    (let ((state (nskk-state-create 'ascii)))
      (nskk-then
        (should-error (nskk-state-set state 'mode 'invalid-mode))
        (should (eq (nskk-state-mode state) 'ascii)))))

  (nskk-it "returns nil for nil state"
    (nskk-then
      (should (null (nskk-state-set nil 'mode 'hiragana)))))

  (nskk-it "returns nil for non-existent slot"
    (let ((state (nskk-state-create)))
      (nskk-then
        (should (null (nskk-state-set state 'not-a-slot 'value))))))

  (nskk-it "accepts multiple nil-state slot/value combinations"
    (nskk-then
      (should (null (nskk-state-set nil 'mode 'hiragana)))
      (should (null (nskk-state-set nil 'input-buffer "test")))))

  (nskk-it "tracks previous-mode through multiple transitions"
    (let ((state (nskk-state-create 'ascii)))
      ;; Initial state
            (should (eq (nskk-state-previous-mode state) 'ascii))

      ;; First transition
      (nskk-state-set state 'mode 'hiragana)
      (should (eq (nskk-state-mode state) 'hiragana))
      (should (eq (nskk-state-previous-mode state) 'ascii))

      ;; Second transition
      (nskk-state-set state 'mode 'katakana)
      (should (eq (nskk-state-mode state) 'katakana))
      (should (eq (nskk-state-previous-mode state) 'hiragana))

      ;; Third transition
      (nskk-state-set state 'mode 'latin)
      (should (eq (nskk-state-mode state) 'latin))
      (should (eq (nskk-state-previous-mode state) 'katakana))))

  (nskk-it "accepts all valid modes and rejects invalid ones"
    (let ((state (nskk-state-create 'ascii)))
      (dolist (mode '(ascii hiragana katakana katakana-半角 abbrev latin jisx0208-latin))
        (let ((result (nskk-state-set state 'mode mode)))
          (should (eq result mode))
          (should (eq (nskk-state-mode state) mode))))

      ;; Invalid modes should raise error and not change state
      (nskk-state-set state 'mode 'hiragana)
      (should-error (nskk-state-set state 'mode 'not-a-mode))
      (should (eq (nskk-state-mode state) 'hiragana)))))

;;;
;;; Mode Validation Tests
;;;

(nskk-describe "nskk-state-valid-mode-p"
  (nskk-deftest-table state-valid-mode-p-accepts-valid-modes
    :description "valid-mode-p returns t for each valid mode symbol"
    :columns (mode)
    :rows ((ascii) (hiragana) (katakana) (katakana-半角) (abbrev) (latin) (jisx0208-latin))
    :body (should (nskk-state-valid-mode-p mode)))

  (nskk-deftest-table state-valid-mode-p-rejects-invalid-modes
    :description "valid-mode-p returns nil for non-mode symbols, strings, and numbers"
    :columns (non-mode)
    :rows ((invalid-mode) (mode) (nil) ("hiragana") (42) (()))
    :body (should-not (nskk-state-valid-mode-p non-mode))))

;;;
;;; Henkan Mode Predicate Tests
;;;

(nskk-describe "nskk-state henkan-phase predicates"
  (nskk-context "nskk-state-in-henkan-mode-p"
    (nskk-it "returns true when henkan-phase is on"
      (let ((state (nskk-state-create 'hiragana)))
        (nskk-state-set-henkan-phase state 'on)
        (should (nskk-state-in-henkan-mode-p state))))

    (nskk-it "returns true when henkan-phase is active"
      (let ((state (nskk-state-create 'hiragana)))
        (nskk-state-force-henkan-phase state 'active)
        (should (nskk-state-in-henkan-mode-p state))))

    (nskk-it "returns true when henkan-phase is list"
      (let ((state (nskk-state-create 'hiragana)))
        (nskk-state-force-henkan-phase state 'list)
        (should (nskk-state-in-henkan-mode-p state))))

    (nskk-it "returns true when henkan-phase is registration"
      (let ((state (nskk-state-create 'hiragana)))
        (nskk-state-force-henkan-phase state 'registration)
        (should (nskk-state-in-henkan-mode-p state))))

    (nskk-it "returns false when henkan-phase is nil"
      (let ((state (nskk-state-create 'hiragana)))
        (should (not (nskk-state-in-henkan-mode-p state))))))

  (nskk-context "nskk-state-henkan-on-p"
    (nskk-it "returns true when phase is on"
      (let ((state (nskk-state-create 'hiragana)))
        (nskk-state-set-henkan-phase state 'on)
        (should (nskk-state-henkan-on-p state))))

    (nskk-it "returns false when phase is nil or active"
      (let ((state (nskk-state-create 'hiragana)))
        (should (not (nskk-state-henkan-on-p state)))
        (nskk-state-force-henkan-phase state 'active)
        (should (not (nskk-state-henkan-on-p state))))))

  (nskk-context "nskk-state-henkan-active-p"
    (nskk-it "returns true when phase is active"
      (let ((state (nskk-state-create 'hiragana)))
        (nskk-state-force-henkan-phase state 'active)
        (should (nskk-state-henkan-active-p state))))

    (nskk-it "returns false when phase is nil or on"
      (let ((state (nskk-state-create 'hiragana)))
        (should (not (nskk-state-henkan-active-p state)))
        (nskk-state-set-henkan-phase state 'on)
        (should (not (nskk-state-henkan-active-p state)))))))

;;;
;;; Henkan Phase Transition Tests
;;;

(nskk-describe "nskk-state henkan-phase"
  (nskk-it "transitions through all phases in sequence"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-state-set-henkan-phase state 'on)
      (should (eq (nskk-state-henkan-phase state) 'on))
      (nskk-state-set-henkan-phase state 'active)
      (should (eq (nskk-state-henkan-phase state) 'active))
      (nskk-state-set-henkan-phase state 'list)
      (should (eq (nskk-state-henkan-phase state) 'list))
      (nskk-state-set-henkan-phase state 'registration)
      (should (eq (nskk-state-henkan-phase state) 'registration))
      (nskk-state-set-henkan-phase state nil)
      (should (null (nskk-state-henkan-phase state)))))

  (nskk-it "signals error for invalid transitions (skip nil -> active)"
    (let ((state (nskk-state-create 'hiragana)))
      ;; nil -> active is not a valid transition (must go nil -> on -> active)
      (should-error (nskk-state-set-henkan-phase state 'active))
      ;; nil -> list is not valid
      (should-error (nskk-state-set-henkan-phase state 'list))
      ;; nil -> registration is not valid
      (should-error (nskk-state-set-henkan-phase state 'registration))
      ;; Verify valid transitions still work
      (nskk-state-set-henkan-phase state 'on)
      (should (eq (nskk-state-henkan-phase state) 'on))
      (nskk-state-set-henkan-phase state 'active)
      (should (eq (nskk-state-henkan-phase state) 'active))))

  (nskk-it "force-henkan-phase bypasses transition validation"
    (let ((state (nskk-state-create 'hiragana)))
      ;; nil -> active would fail with set-henkan-phase but works with force
      (nskk-state-force-henkan-phase state 'active)
      (should (eq (nskk-state-henkan-phase state) 'active))
      ;; active -> list -> nil via force
      (nskk-state-force-henkan-phase state 'list)
      (should (eq (nskk-state-henkan-phase state) 'list))
      (nskk-state-force-henkan-phase state nil)
      (should (null (nskk-state-henkan-phase state)))
      ;; Still validates the phase itself
      (should-error (nskk-state-force-henkan-phase state 'invalid-phase))))

  (nskk-it "same-phase transitions are allowed (no-op)"
    (let ((state (nskk-state-create 'hiragana)))
      ;; nil -> nil should succeed
      (nskk-state-set-henkan-phase state nil)
      (should (null (nskk-state-henkan-phase state)))
      ;; on -> on should succeed
      (nskk-state-set-henkan-phase state 'on)
      (nskk-state-set-henkan-phase state 'on)
      (should (eq (nskk-state-henkan-phase state) 'on)))))

;;;
;;; Mode Transition Tests
;;;

(nskk-describe "nskk-state-transition"
  (nskk-it "succeeds for correct from-mode"
    (let ((state (nskk-state-create 'ascii)))
      (nskk-then
        (should (nskk-state-transition state 'ascii 'hiragana))
        (should (eq (nskk-state-mode state) 'hiragana)))))

  (nskk-it "succeeds for all valid mode combinations"
    (dolist (from-mode nskk-state-test-valid-modes)
      (dolist (to-mode nskk-state-test-valid-modes)
        (let ((state (nskk-state-create from-mode)))
          (should (nskk-state-transition state from-mode to-mode))
          (should (eq (nskk-state-mode state) to-mode))))))

  (nskk-it "returns nil for wrong from-mode and leaves state unchanged"
    (let ((state (nskk-state-create 'ascii)))
      (nskk-then
        (should (not (nskk-state-transition state 'hiragana 'katakana)))
        (should (eq (nskk-state-mode state) 'ascii)))))

  (nskk-it "returns nil for invalid to-mode and leaves state unchanged"
    (let ((state (nskk-state-create 'ascii)))
      (nskk-then
        (should (not (nskk-state-transition state 'ascii 'invalid-mode)))
        (should (eq (nskk-state-mode state) 'ascii)))))

  (nskk-it "returns nil for nil state"
    (nskk-then
      (should (not (nskk-state-transition nil 'ascii 'hiragana)))))

  (nskk-it "validates that from-mode must match current mode"
    (let ((state (nskk-state-create 'ascii)))
      ;; Correct from-mode should succeed
      (should (nskk-state-transition state 'ascii 'hiragana))
      (should (eq (nskk-state-mode state) 'hiragana))

      ;; Wrong from-mode should fail
      (should (not (nskk-state-transition state 'ascii 'katakana)))
      (should (eq (nskk-state-mode state) 'hiragana))))

  (nskk-it "validates target mode is valid"
    (let ((state (nskk-state-create 'ascii)))
      (nskk-then
        (should (not (nskk-state-transition state 'ascii 'not-a-mode)))
        (should (eq (nskk-state-mode state) 'ascii)))))

  (nskk-it "maintains validation through a sequence of transitions"
    (let ((state (nskk-state-create 'ascii)))
      (should (nskk-state-transition state 'ascii 'hiragana))
      (should (nskk-state-transition state 'hiragana 'katakana))
      (should (nskk-state-transition state 'katakana 'latin))
      (should (nskk-state-transition state 'latin 'abbrev))
      (should (nskk-state-transition state 'abbrev 'ascii))

      ;; Final state should be ascii
      (should (eq (nskk-state-mode state) 'ascii)))))

;;;
;;; State Reset Tests
;;;

(nskk-describe "nskk-state-reset"
  (nskk-it "clears all mutable state while preserving mode"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-given
        (nskk-state-set state 'input-buffer "test")
        (nskk-state-set state 'converted-buffer "converted")
        (nskk-state-set state 'candidates '("a" "b"))
        (nskk-state-set state 'current-index 1)
        (nskk-state-set state 'henkan-position 5)
        (nskk-state-set state 'undo-stack '(("a" . "b")))
        (nskk-state-set state 'redo-stack '(("c" . "d")))
        (nskk-state-set state 'metadata '(:key "value")))
      (nskk-when
        (nskk-state-reset state))
      (nskk-then
        (should (eq (nskk-state-mode state) 'hiragana))
        (should (string= (nskk-state-input-buffer state) ""))
        (should (string= (nskk-state-converted-buffer state) ""))
        (should (null (nskk-state-candidates state)))
        (should (= (nskk-state-current-index state) 0))
        (should (null (nskk-state-henkan-position state)))
        (should (null (nskk-state-marker-position state)))
        (should (null (nskk-state-undo-stack state)))
        (should (null (nskk-state-redo-stack state)))
        (should (null (nskk-state-henkan-phase state)))
        (should (null (nskk-state-metadata state))))))

  (nskk-it "returns nil for nil state"
    (nskk-then
      (should (not (nskk-state-reset nil)))))

  (nskk-it "preserves mode when reset"
    (let ((state (nskk-state-create 'katakana)))
      (nskk-given
        (nskk-state-set state 'input-buffer "test")
        (nskk-state-set state 'henkan-position 5)
        (nskk-state-set-candidates state '("a" "b" "c")))
      (nskk-when
        (nskk-state-reset state))
      (nskk-then
        (should (eq (nskk-state-mode state) 'katakana))
        (should (string= (nskk-state-input-buffer state) ""))
        (should (null (nskk-state-henkan-position state)))
        (should (null (nskk-state-candidates state)))))))

;;;
;;; Buffer Management Tests
;;;

(nskk-describe "nskk-state buffer operations"
  (nskk-context "nskk-state-append-input"
    (nskk-it "appends single ASCII characters"
      (let ((state (nskk-state-create)))
        (nskk-state-append-input state ?a)
        (should (string= (nskk-state-input-buffer state) "a"))
        (nskk-state-append-input state ?b)
        (should (string= (nskk-state-input-buffer state) "ab"))))

    (nskk-it "appends Japanese characters"
      (let ((state (nskk-state-create)))
        (nskk-state-append-input state ?\u3042)
        (should (string= (nskk-state-input-buffer state) "\u3042"))
        (nskk-state-append-input state ?\u3044)
        (should (string= (nskk-state-input-buffer state) "\u3042\u3044")))))

  (nskk-context "nskk-state-delete-last-char"
    (nskk-it "deletes last character and returns it"
      (let ((state (nskk-state-create)))
        (nskk-state-set state 'input-buffer "abc")
        (should (eq (nskk-state-delete-last-char state) ?c))
        (should (string= (nskk-state-input-buffer state) "ab"))
        (should (eq (nskk-state-delete-last-char state) ?b))
        (should (string= (nskk-state-input-buffer state) "a"))))

    (nskk-it "returns nil for empty buffer"
      (let ((state (nskk-state-create)))
        (should (null (nskk-state-delete-last-char state)))
        (should (string= (nskk-state-input-buffer state) "")))))

  (nskk-context "nskk-state-clear-input"
    (nskk-it "empties the input buffer"
      (let ((state (nskk-state-create)))
        (nskk-state-set state 'input-buffer "test input")
        (nskk-state-clear-input state)
        (should (string= (nskk-state-input-buffer state) "")))))

  (nskk-it "handles a sequence of append, delete, and clear"
    (let ((state (nskk-state-create)))
      (nskk-state-append-input state ?t)
      (nskk-state-append-input state ?e)
      (nskk-state-append-input state ?s)
      (should (string= (nskk-state-input-buffer state) "tes"))

      (should (eq (nskk-state-delete-last-char state) ?s))
      (should (string= (nskk-state-input-buffer state) "te"))

      (nskk-state-clear-input state)
      (should (string= (nskk-state-input-buffer state) ""))))

  (nskk-it "mode change does not affect buffer operations"
    (let ((state (nskk-state-create 'ascii)))
      
      (nskk-state-set state 'mode 'hiragana)
      (should (eq (nskk-state-mode state) 'hiragana))

      (nskk-state-append-input state ?a)
      (should (string= (nskk-state-input-buffer state) "a"))

      (let ((mode (nskk-state-mode state)))
        (should (eq mode 'hiragana))))))

;;;
;;; Candidate Management Tests
;;;

(nskk-describe "nskk-state candidates"
  (nskk-it "set-candidates resets index to 0"
    (let ((state (nskk-state-create)))
      (nskk-given
        (nskk-state-set state 'current-index 5))
      (nskk-when
        (nskk-state-set-candidates state '("a" "b" "c")))
      (nskk-then
        (should (= (nskk-state-current-index state) 0))
        (should (equal (nskk-state-candidates state) '("a" "b" "c"))))))

  (nskk-it "current-candidate returns the candidate at current-index"
    (let ((state (nskk-state-create)))
      (nskk-state-set-candidates state '("first" "second" "third"))
      (should (string= (nskk-state-current-candidate state) "first"))

      (nskk-state-set state 'current-index 1)
      (should (string= (nskk-state-current-candidate state) "second"))

      (nskk-state-set state 'current-index 2)
      (should (string= (nskk-state-current-candidate state) "third"))))

  (nskk-it "next-candidate advances index and wraps around"
    (let ((state (nskk-state-create)))
      (nskk-state-set-candidates state '("a" "b" "c"))

      (should (string= (nskk-state-next-candidate state) "b"))
      (should (= (nskk-state-current-index state) 1))

      (should (string= (nskk-state-next-candidate state) "c"))
      (should (= (nskk-state-current-index state) 2))

      ;; Should wrap around
      (should (string= (nskk-state-next-candidate state) "a"))
      (should (= (nskk-state-current-index state) 0))))

  (nskk-it "next-candidate stays at index 0 for a single candidate"
    (let ((state (nskk-state-create)))
      (nskk-state-set-candidates state '("only"))
      (should (string= (nskk-state-next-candidate state) "only"))
      (should (= (nskk-state-current-index state) 0))))

  (nskk-it "next-candidate returns nil when no candidates"
    (let ((state (nskk-state-create)))
      (should (null (nskk-state-next-candidate state)))))

  (nskk-it "previous-candidate decrements index and wraps around"
    (let ((state (nskk-state-create)))
      (nskk-state-set-candidates state '("a" "b" "c"))
      (nskk-state-set state 'current-index 1)

      (should (string= (nskk-state-previous-candidate state) "a"))
      (should (= (nskk-state-current-index state) 0))

      ;; Should wrap around
      (should (string= (nskk-state-previous-candidate state) "c"))
      (should (= (nskk-state-current-index state) 2))))

  (nskk-it "previous-candidate returns nil when no candidates"
    (let ((state (nskk-state-create)))
      (should (null (nskk-state-previous-candidate state)))))

  (nskk-it "supports a full navigation cycle"
    (let ((state (nskk-state-create)))
      (nskk-state-set-candidates state '("one" "two" "three" "four"))

      (should (string= (nskk-state-current-candidate state) "one"))

      (nskk-state-next-candidate state)
      (should (string= (nskk-state-current-candidate state) "two"))

      (nskk-state-previous-candidate state)
      (should (string= (nskk-state-current-candidate state) "one"))

      ;; Navigate to end
      (nskk-state-next-candidate state)
      (nskk-state-next-candidate state)
      (nskk-state-next-candidate state)
      (should (string= (nskk-state-current-candidate state) "four"))

      ;; Next should wrap to beginning
      (nskk-state-next-candidate state)
      (should (string= (nskk-state-current-candidate state) "one"))

      ;; Previous should wrap to end
      (nskk-state-previous-candidate state)
      (should (string= (nskk-state-current-candidate state) "four")))))

;;;
;;; Metadata Tests
;;;

(nskk-describe "nskk-state metadata"
  (nskk-it "returns nil when metadata is empty"
    (let ((state (nskk-state-create)))
      (should (null (nskk-state-get-metadata state :key)))))

  (nskk-it "stores and retrieves a single value"
    (let ((state (nskk-state-create)))
      (nskk-when
        (nskk-state-put-metadata state :test-key "test-value"))
      (nskk-then
        (should (string= (nskk-state-get-metadata state :test-key) "test-value")))))

  (nskk-it "stores and retrieves multiple values of different types"
    (let ((state (nskk-state-create)))
      (nskk-state-put-metadata state :key1 "value1")
      (nskk-state-put-metadata state :key2 "value2")
      (nskk-state-put-metadata state :key3 123)
      (nskk-state-put-metadata state :key4 '(a b c))

      (should (string= (nskk-state-get-metadata state :key1) "value1"))
      (should (string= (nskk-state-get-metadata state :key2) "value2"))
      (should (= (nskk-state-get-metadata state :key3) 123))
      (should (equal (nskk-state-get-metadata state :key4) '(a b c)))))

  (nskk-it "overwrites an existing value"
    (let ((state (nskk-state-create)))
      (nskk-state-put-metadata state :key "original")
      (should (string= (nskk-state-get-metadata state :key) "original"))

      (nskk-state-put-metadata state :key "updated")
      (should (string= (nskk-state-get-metadata state :key) "updated"))))

  (nskk-it "is cleared on reset"
    (let ((state (nskk-state-create)))
      (nskk-given
        (nskk-state-put-metadata state :key1 "value1")
        (nskk-state-put-metadata state :key2 "value2"))
      (nskk-when
        (nskk-state-reset state))
      (nskk-then
        (should (null (nskk-state-get-metadata state :key1)))
        (should (null (nskk-state-get-metadata state :key2)))))))

;;;
;;; Generated Metadata Setter Tests
;;;

(nskk-describe "nskk-define-metadata-setter generated functions"
  (nskk-deftest-table state-metadata-setter-roundtrip
    :description "Each generated setter stores and retrieves its value via metadata"
    :columns (setter getter key value)
    :rows ((nskk-state-set-okurigana    nskk-state-get-okurigana    okurigana        "k")
           (nskk-state-set-remaining-romaji nskk-state-get-metadata remaining-romaji "ro")
           (nskk-state-set-kana-type    nskk-state-get-metadata     kana-type        hiragana)
           (nskk-state-set-width-type   nskk-state-get-metadata     width-type       full))
    :body (let ((state (nskk-state-create)))
            (funcall setter state value)
            (if (eq setter 'nskk-state-set-okurigana)
                (should (equal (funcall getter state) value))
              (should (equal (funcall getter state key) value)))))

  (nskk-it "nskk-state-get-okurigana returns nil before any set"
    (let ((state (nskk-state-create)))
      (should (null (nskk-state-get-okurigana state)))))

  (nskk-it "nskk-state-set-okurigana stores consonant for okuri-nashi lookup"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-state-set-okurigana state "k")
      (should (equal (nskk-state-get-okurigana state) "k"))
      ;; Overwrite
      (nskk-state-set-okurigana state "t")
      (should (equal (nskk-state-get-okurigana state) "t"))))

  (nskk-it "metadata setters return nil for nil state"
    (should (null (nskk-state-set-okurigana nil "k")))
    (should (null (nskk-state-set-remaining-romaji nil "ro")))
    (should (null (nskk-state-set-kana-type nil 'hiragana)))
    (should (null (nskk-state-set-width-type nil 'full)))))

;;;
;;; CPS Variant Tests
;;;

(nskk-describe "nskk-state-append-input/k"
  (nskk-it "calls on-done with new buffer on success"
    (let ((state (nskk-state-create))
          result)
      (nskk-state-append-input/k state ?a
                                 (lambda (buf) (setq result buf))
                                 (lambda () (should nil)))
      (should (string= result "a"))
      (should (string= (nskk-state-input-buffer state) "a"))))

  (nskk-it "calls on-fail for non-character input"
    (let ((state (nskk-state-create))
          failed)
      (nskk-state-append-input/k state "not-a-char"
                                 (lambda (_) (should nil))
                                 (lambda () (setq failed t)))
      (should failed)))

  (nskk-it "calls on-fail for nil state"
    (let (failed)
      (nskk-state-append-input/k nil ?a
                                 (lambda (_) (should nil))
                                 (lambda () (setq failed t)))
      (should failed))))

(nskk-describe "nskk-state-delete-last-char/k"
  (nskk-it "calls on-deleted with the removed character"
    (let ((state (nskk-state-create))
          deleted-char)
      (nskk-state-set state 'input-buffer "ab")
      (nskk-state-delete-last-char/k state
                                     (lambda (ch) (setq deleted-char ch))
                                     (lambda () (should nil)))
      (should (eq deleted-char ?b))
      (should (string= (nskk-state-input-buffer state) "a"))))

  (nskk-it "calls on-empty when buffer is already empty"
    (let ((state (nskk-state-create))
          was-empty)
      (nskk-state-delete-last-char/k state
                                     (lambda (_) (should nil))
                                     (lambda () (setq was-empty t)))
      (should was-empty))))

(nskk-describe "nskk-state-transition/k"
  (nskk-it "calls on-found with t for a valid transition"
    (let ((state (nskk-state-create 'ascii))
          succeeded)
      (nskk-state-transition/k state 'ascii 'hiragana
                               (lambda (_) (setq succeeded t))
                               (lambda () (should nil)))
      (should succeeded)
      (should (eq (nskk-state-mode state) 'hiragana))))

  (nskk-it "calls on-not-found for wrong from-mode"
    (let ((state (nskk-state-create 'ascii))
          failed)
      (nskk-state-transition/k state 'hiragana 'katakana
                               (lambda (_) (should nil))
                               (lambda () (setq failed t)))
      (should failed)
      (should (eq (nskk-state-mode state) 'ascii))))

  (nskk-it "calls on-not-found for invalid to-mode"
    (let ((state (nskk-state-create 'ascii))
          failed)
      (nskk-state-transition/k state 'ascii 'not-a-mode
                               (lambda (_) (should nil))
                               (lambda () (setq failed t)))
      (should failed))))

(nskk-describe "nskk-state-next-candidate/k and nskk-state-previous-candidate/k"
  (nskk-it "next-candidate/k calls on-candidate with the next candidate"
    (let ((state (nskk-state-create))
          result)
      (nskk-state-set-candidates state '("a" "b" "c"))
      (nskk-state-next-candidate/k state
                                   (lambda (cand) (setq result cand))
                                   (lambda () (should nil)))
      (should (string= result "b"))))

  (nskk-it "next-candidate/k calls on-empty when no candidates"
    (let ((state (nskk-state-create))
          was-empty)
      (nskk-state-next-candidate/k state
                                   (lambda (_) (should nil))
                                   (lambda () (setq was-empty t)))
      (should was-empty)))

  (nskk-it "previous-candidate/k calls on-candidate with the previous candidate"
    (let ((state (nskk-state-create))
          result)
      (nskk-state-set-candidates state '("a" "b" "c"))
      (nskk-state-set state 'current-index 2)
      (nskk-state-previous-candidate/k state
                                       (lambda (cand) (setq result cand))
                                       (lambda () (should nil)))
      (should (string= result "b"))))

  (nskk-it "previous-candidate/k calls on-empty when no candidates"
    (let ((state (nskk-state-create))
          was-empty)
      (nskk-state-previous-candidate/k state
                                       (lambda (_) (should nil))
                                       (lambda () (setq was-empty t)))
      (should was-empty))))

(nskk-describe "CPS /k variants with nskk-it-k"
  (nskk-it-k "nskk-state-get/k returns the requested slot value"
    (nskk-state-get/k
     (let ((state (nskk-state-create 'hiragana)))
       (nskk-state-set state 'input-buffer "ka")
       state)
     'input-buffer)
    :found (result)
      (should (equal result "ka"))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-get/k"))

  (nskk-it-k "nskk-state-set/k sets mode and returns the new mode"
    (nskk-state-set/k (nskk-state-create 'ascii) 'mode 'katakana)
    :found (result)
      (should (eq result 'katakana))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-set/k"))

  (nskk-it-k "nskk-state-append-input/k appends one character"
    (nskk-state-append-input/k (nskk-state-create) ?a)
    :found (result)
      (should (equal result "a"))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-append-input/k"))

  (nskk-it-k "nskk-state-delete-last-char/k returns the deleted character"
    (nskk-state-delete-last-char/k
     (let ((state (nskk-state-create)))
       (nskk-state-set state 'input-buffer "ab")
       state))
    :found (result)
      (should (eq result ?b))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-delete-last-char/k"))

  (nskk-it-k "nskk-state-transition/k transitions to a valid target mode"
    (nskk-state-transition/k (nskk-state-create 'ascii) 'ascii 'hiragana)
    :found (result)
      (should (eq result t))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-transition/k"))

  (nskk-it-k "nskk-state-next-candidate/k returns the next candidate"
    (nskk-state-next-candidate/k
     (let ((state (nskk-state-create)))
       (nskk-state-set-candidates state '("a" "b" "c"))
       state))
    :found (result)
      (should (equal result "b"))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-next-candidate/k"))

  (nskk-it-k "nskk-state-previous-candidate/k returns the previous candidate"
    (nskk-state-previous-candidate/k
     (let ((state (nskk-state-create)))
       (nskk-state-set-candidates state '("a" "b" "c"))
       (nskk-state-set state 'current-index 2)
       state))
    :found (result)
      (should (equal result "b"))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-previous-candidate/k"))

  (nskk-it-k "nskk-state-current-candidate/k returns current candidate"
    (nskk-state-current-candidate/k
     (let ((state (nskk-state-create)))
       (nskk-state-set-candidates state '("a" "b" "c"))
       (nskk-state-set state 'current-index 1)
       state))
    :found (result)
      (should (equal result "b"))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-current-candidate/k"))

  (nskk-it-k "nskk-state-get-metadata/k returns stored metadata"
    (nskk-state-get-metadata/k
     (let ((state (nskk-state-create)))
       (nskk-state-put-metadata state :foo "bar")
       state)
     :foo)
    :found (result)
      (should (equal result "bar"))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-get-metadata/k"))

  (nskk-it-k "nskk-state-get-okurigana/k returns stored okurigana"
    (nskk-state-get-okurigana/k
     (let ((state (nskk-state-create)))
       (nskk-state-set-okurigana state "k")
       state))
    :found (result)
      (should (equal result "k"))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-get-okurigana/k"))

  (nskk-it-k "nskk-state-get-mode/k returns current-state mode"
    (nskk-state-test--get-mode-with-state/k 'katakana)
    :found (result)
      (should (eq result 'katakana))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-get-mode/k"))

  (nskk-it-k "nskk-state-henkan-on-p/k returns t when phase is on"
    (nskk-state-henkan-on-p/k
     (let ((state (nskk-state-create 'hiragana)))
       (nskk-state-set-henkan-phase state 'on)
       state))
    :found (result)
      (should (eq result t))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-henkan-on-p/k"))

  (nskk-it-k "nskk-state-henkan-active-p/k returns t when phase is active"
    (nskk-state-henkan-active-p/k
     (let ((state (nskk-state-create 'hiragana)))
       (nskk-state-force-henkan-phase state 'active)
       state))
    :found (result)
      (should (eq result t))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-henkan-active-p/k")))

;;;
;;; Japanese Mode Classification Tests
;;;

(nskk-describe "japanese-mode Prolog predicate"
  (nskk-deftest-table state-japanese-mode-true-modes
    :description "japanese-mode/1 is true for Japanese modes"
    :columns (mode)
    :rows ((hiragana) (katakana) (katakana-半角))
    :body (should (nskk-prolog-query `(japanese-mode ,mode))))

  (nskk-deftest-table state-japanese-mode-false-modes
    :description "japanese-mode/1 is false for non-Japanese modes"
    :columns (mode)
    :rows ((ascii) (latin) (jisx0208-latin) (abbrev))
    :body (should-not (nskk-prolog-query `(japanese-mode ,mode)))))

;;;
;;; Property-Based Tests
;;;

;; Inline valid modes list (no external dep needed)
(defconst nskk-state-pbt--valid-modes
  '(ascii hiragana katakana katakana-半角 abbrev latin jisx0208-latin)
  "Valid modes for state property-based tests.")

(nskk-property-test state-pbt-mode-set-invariant
  ((input romaji-string))
  ;; For any valid mode, nskk-state-set-mode sets that mode.
  ;; Generate a random valid mode from the inline list.
  (let* ((mode (nth (random (length nskk-state-pbt--valid-modes))
                    nskk-state-pbt--valid-modes))
         (state (nskk-state-create)))
    (nskk-state-set state 'mode mode)
    (eq (nskk-state-mode state) mode))
  100)

(nskk-property-test state-pbt-buffer-append-grows
  ((input romaji-string))
  ;; Appending a character to input-buffer always grows it or keeps it the same length.
  ;; The romaji-string generator always produces non-empty strings.
  (let ((state (nskk-state-create)))
    (let ((before-len (length (nskk-state-input-buffer state))))
      (if (not (string-empty-p input))
          (progn
            (nskk-state-append-input state (aref input 0))
            (let ((after-len (length (nskk-state-input-buffer state))))
              (>= after-len before-len)))
        t)))  ; vacuously ok for empty strings
  100)

(nskk-property-test state-pbt-created-state-is-valid
  ((input romaji-string))
  ;; State is always valid after creation: nskk-state-p returns t for any created state.
  ;; input is used only to drive the PBT loop; we test creation of states with each mode.
  (let* ((mode (nth (random (length nskk-state-pbt--valid-modes))
                    nskk-state-pbt--valid-modes))
         (state (nskk-state-create mode)))
    (nskk-state-p state))
  100)

(nskk-property-test state-pbt-reset-clears-buffers
  ((input romaji-string))
  ;; After nskk-state-reset, mode stays but buffers are cleared.
  (let* ((mode (nth (random (length nskk-state-pbt--valid-modes))
                    nskk-state-pbt--valid-modes))
         (state (nskk-state-create mode)))
    ;; Set some state
    (nskk-state-set state 'input-buffer input)
    (nskk-state-set state 'converted-buffer input)
    ;; Reset
    (nskk-state-reset state)
    ;; Mode preserved, buffers cleared
    (and (eq (nskk-state-mode state) mode)
         (string= (nskk-state-input-buffer state) "")
         (string= (nskk-state-converted-buffer state) "")))
  100)

;;;
;;; Prolog Predicate Tests: mode-properties/5
;;;

(nskk-describe "mode-properties Prolog predicate"
  (nskk-deftest-table state-prolog-mode-properties-display
    :description "mode-properties/5 returns correct display string"
    :columns (mode expected-display)
    :rows ((hiragana       "かな")
           (katakana       "カナ")
           (abbrev         "aA")
           (ascii          "SKK")
           (latin          "SKK")
           (jisx0208-latin "全英"))
    :body (let ((display (nskk-prolog-query-value
                          `(mode-properties ,mode ,'\?s ,'\?f ,'\?h ,'\?c) '\?s)))
            (should (equal display expected-display))))

  (nskk-it "returns a result for katakana-半角"
    (should (nskk-prolog-query-one
             `(mode-properties katakana-半角 ,'\?s ,'\?f ,'\?h ,'\?c))))

  (nskk-it "returns nil for unknown mode"
    (should-not (nskk-prolog-query-one
                 `(mode-properties nonexistent ,'\?s ,'\?f ,'\?h ,'\?c)))))

;;;
;;; Prolog Predicate Tests: state-slot-default/2
;;;

(nskk-describe "state-slot-default Prolog predicate"
  (nskk-deftest-table state-prolog-slot-defaults
    :description "state-slot-default/2 returns the correct initial value for each slot"
    :columns (slot expected)
    :rows ((input-buffer      "")
           (converted-buffer  "")
           (candidates        nil)
           (current-index     0)
           (henkan-position   nil)
           (marker-position   nil)
           (undo-stack        nil)
           (redo-stack        nil)
           (henkan-phase      nil)
           (metadata          nil))
    :body (let ((val (nskk-prolog-query-value
                      `(state-slot-default ,slot ,'\?v) '\?v)))
            (should (equal val expected))))

  (nskk-it "returns nil for unknown slot"
    (should-not (nskk-prolog-query-one '(state-slot-default nonexistent \?v)))))

;;;
;;; Property-Based Tests for CPS Variants
;;;

(nskk-property-test state-pbt-append-cps-consistent-with-sync
  ((input romaji-string))
  ;; append-input/k must produce the same result as the sync variant.
  (when (not (string-empty-p input))
    (let* ((char (aref input 0))
           (state1 (nskk-state-create))
           (state2 (nskk-state-create))
           sync-result cps-result)
      (setq sync-result (nskk-state-append-input state1 char))
      (nskk-state-append-input/k state2 char
                                 (lambda (buf) (setq cps-result buf))
                                 (lambda () nil))
      (equal sync-result cps-result)))
  100)

(nskk-property-test state-pbt-transition-cps-consistent-with-sync
  ((input romaji-string))
  ;; transition/k must set mode iff the sync variant returns t.
  (let* ((modes '(ascii hiragana katakana abbrev latin jisx0208-latin))
         (from (nth (random (length modes)) modes))
         (to   (nth (random (length modes)) modes))
         (state1 (nskk-state-create from))
         (state2 (nskk-state-create from))
         sync-ok cps-ok)
    (setq sync-ok (if (nskk-state-transition state1 from to) t nil))
    (nskk-state-transition/k state2 from to
                             (lambda (_) (setq cps-ok t))
                             (lambda () (setq cps-ok nil)))
    (eq sync-ok cps-ok))
  100)

;;;
;;; Integration Tests
;;;

(nskk-describe "nskk-state full lifecycle scenarios"
  (nskk-it "inputs characters then converts and navigates candidates then resets"
    (let ((state (nskk-state-create 'hiragana)))
      ;; Input phase
      (nskk-state-append-input state ?k)
      (nskk-state-append-input state ?a)
      (nskk-state-append-input state ?n)
      (nskk-state-append-input state ?j)
      (should (string= (nskk-state-input-buffer state) "kanj"))

      ;; Conversion phase - set henkan-phase to on
      (nskk-state-set-henkan-phase state 'on)
      (should (nskk-state-in-henkan-mode-p state))

      ;; Candidates phase - transition to active
      (nskk-state-set-henkan-phase state 'active)
      (nskk-state-set-candidates state '("\u6f22\u5b57" "\u611f\u3058" "\u5e7e\u6642"))
      (should (string= (nskk-state-current-candidate state) "\u6f22\u5b57"))

      (nskk-state-next-candidate state)
      (should (string= (nskk-state-current-candidate state) "\u611f\u3058"))

      (nskk-state-next-candidate state)
      (should (string= (nskk-state-current-candidate state) "\u5e7e\u6642"))

      ;; Reset for next input
      (nskk-state-reset state)
      (should (string= (nskk-state-input-buffer state) ""))
      (should (null (nskk-state-candidates state)))
      (should (not (nskk-state-in-henkan-mode-p state)))))

  (nskk-it "switches modes and tracks previous-mode through ascii hiragana katakana"
    (let ((state (nskk-state-create 'ascii)))
      
      ;; Switch to hiragana
      (nskk-state-transition state 'ascii 'hiragana)
      (should (eq (nskk-state-previous-mode state) 'ascii))

      ;; Input in hiragana
      (nskk-state-append-input state ?\u3042)
      (should (string= (nskk-state-input-buffer state) "\u3042"))

      ;; Switch to katakana
      (nskk-state-transition state 'hiragana 'katakana)
      (should (eq (nskk-state-previous-mode state) 'hiragana))

      ;; Input in katakana
      (nskk-state-clear-input state)
      (nskk-state-append-input state ?\u30a2)
      (should (string= (nskk-state-input-buffer state) "\u30a2"))

      ;; Switch back to ascii
      (nskk-state-transition state 'katakana 'ascii)
      (should (eq (nskk-state-previous-mode state) 'katakana))))

  (nskk-it "corrects a typo with delete then re-input then navigates candidates"
    (let ((state (nskk-state-create 'hiragana)))
      ;; Start typing
      (nskk-state-append-input state ?t)
      (nskk-state-append-input state ?o)
      (nskk-state-append-input state ?u)
      (nskk-state-append-input state ?k)
      (nskk-state-append-input state ?y)
      (should (string= (nskk-state-input-buffer state) "touky"))

      ;; Oops, made a mistake - delete last character
      (nskk-state-delete-last-char state)
      (should (string= (nskk-state-input-buffer state) "touk"))

      ;; Add correct character
      (nskk-state-append-input state ?o)
      (should (string= (nskk-state-input-buffer state) "touko"))

      ;; Set conversion phase and get candidates
      (nskk-state-force-henkan-phase state 'active)
      (nskk-state-set-candidates state '("\u6771\u4eac" "\u767b\u6821" "\u6e21\u822a"))
      (should (string= (nskk-state-current-candidate state) "\u6771\u4eac"))

      ;; Navigate through candidates
      (nskk-state-next-candidate state)
      (should (string= (nskk-state-current-candidate state) "\u767b\u6821"))

      (nskk-state-previous-candidate state)
      (should (string= (nskk-state-current-candidate state) "\u6771\u4eac")))))

;;;
;;; nskk-state-get-mode and nskk-with-current-state
;;;

(nskk-describe "nskk-state-get-mode"
  (nskk-it "returns the mode of nskk-current-state"
    (nskk-with-state 'hiragana
      (should (eq (nskk-state-get-mode) 'hiragana))))

  (nskk-it "reflects mode changes in nskk-current-state"
    (nskk-with-state 'ascii
      (should (eq (nskk-state-get-mode) 'ascii))
      (nskk-state-set nskk-current-state 'mode 'katakana)
      (should (eq (nskk-state-get-mode) 'katakana))))

  (nskk-it "returns nil when nskk-current-state is nil"
    (let ((nskk-current-state nil))
      (should (null (nskk-state-get-mode))))))

(nskk-describe "nskk-with-current-state"
  (nskk-it "binds nskk-current-state and executes body"
    (nskk-with-state 'hiragana
      (nskk-with-current-state
        (should (nskk-state-p nskk-current-state))
        (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))))

  (nskk-it "body result is the value of the macro form"
    (nskk-with-state 'ascii
      (let ((result (nskk-with-current-state
                      (nskk-state-get-mode))))
        (should (eq result 'ascii)))))

  (nskk-it "body can modify current state slots"
    (nskk-with-state 'hiragana
      (nskk-with-current-state
        (nskk-state-append-input nskk-current-state ?a))
      (should (string= (nskk-state-input-buffer nskk-current-state) "a")))))

;;;
;;; nskk-ensure-marker macro
;;;

(nskk-describe "nskk-ensure-marker"
  (nskk-it "creates a marker positioned at the given point"
    (with-temp-buffer
      (insert "hello world")
      (let (my-marker)
        (nskk-ensure-marker my-marker 5)
        (should (markerp my-marker))
        (should (= (marker-position my-marker) 5)))))

  (nskk-it "moves an existing marker to a new position"
    (with-temp-buffer
      (insert "hello world")
      (let ((my-marker (make-marker)))
        (set-marker my-marker 3)
        (nskk-ensure-marker my-marker 8)
        (should (= (marker-position my-marker) 8)))))

  (nskk-it "marker is in the current buffer"
    (with-temp-buffer
      (insert "test")
      (let (my-marker)
        (nskk-ensure-marker my-marker 2)
        (should (markerp my-marker))
        (should (eq (marker-buffer my-marker) (current-buffer)))))))

;;;
;;; nskk-state-initialize-prolog
;;;

(nskk-describe "nskk-state-initialize-prolog"
  (nskk-it "populates mode-properties/5 Prolog facts after initialization"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((result (nskk-prolog-query-one
                     '(mode-properties hiragana \?s \?f \?h \?c))))
        (should result))))

  (nskk-it "is idempotent: calling twice does not cause errors"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (should (progn (nskk-state-initialize-prolog) t)))))


;;;
;;; nskk-with-candidates Macro Tests
;;;

(nskk-describe "nskk-with-candidates"
  (nskk-it "executes body when state has candidates"
    (let ((state (nskk-state-create 'hiragana))
          executed)
      (nskk-state-set-candidates state '("漢字" "感じ"))
      (nskk-with-candidates state
        (ignore candidates index)
        (setq executed t))
      (should executed)))

  (nskk-it "binds candidates and index in body"
    (let ((state (nskk-state-create 'hiragana))
          captured-candidates
          captured-index)
      (nskk-state-set-candidates state '("漢字" "感じ"))
      (setf (nskk-state-current-index state) 1)
      (nskk-with-candidates state
        (setq captured-candidates candidates)
        (setq captured-index index))
      (should (equal captured-candidates '("漢字" "感じ")))
      (should (= captured-index 1))))

  (nskk-it "does not execute body when candidates is nil"
    (let ((state (nskk-state-create 'hiragana))
          executed)
      ;; No candidates set -> nskk-state-candidates returns nil
      (nskk-with-candidates state
        (ignore candidates index)
        (setq executed t))
      (should-not executed)))

  (nskk-it "does not execute body when state is not an nskk-state struct"
    (let (executed)
      (nskk-with-candidates nil
        (ignore candidates index)
        (setq executed t))
      (should-not executed))))

;;;
;;; nskk-state-slot-dispatch Macro Tests
;;;

(nskk-describe "nskk-state-slot-dispatch"
  (nskk-it "sets the mode slot when key matches 'mode"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-state-slot-dispatch state 'mode 'katakana mode candidates)
      (should (eq (nskk-state-mode state) 'katakana))))

  (nskk-it "sets the candidates slot when key matches 'candidates"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-state-slot-dispatch state 'candidates '("漢字") mode candidates)
      (should (equal (nskk-state-candidates state) '("漢字")))))

  (nskk-it "returns the new value when slot matches"
    (let ((state (nskk-state-create 'hiragana)))
      (let ((result (nskk-state-slot-dispatch state 'mode 'latin mode candidates)))
        (should (eq result 'latin)))))

  (nskk-it "returns nil when key does not match any slot"
    (let ((state (nskk-state-create 'hiragana)))
      (let ((result (nskk-state-slot-dispatch state 'nonexistent-slot 42 mode candidates)))
        (should-not result))))

  (nskk-it "does not mutate state when key does not match"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-state-slot-dispatch state 'nonexistent-slot 'katakana mode candidates)
      (should (eq (nskk-state-mode state) 'hiragana)))))

;;;;
;;;; Sequence-Based State-Struct Tests (moved from integration layer)
;;;;

(nskk-describe "state sequence: deterministic replay"
  (nskk-it "should produce the same result for the same key sequence"
    (let ((runs 75)
          (failures nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'deterministic-replay' seed: %d" test-seed)
      (dotimes (run runs)
        (let* ((key-sequence (nskk-generate 'key-sequence))
               (initial-state1 (nskk-state-create 'hiragana))
               (initial-state2 (nskk-state-create 'hiragana))
               (result1 (nskk-state-test--execute-keys initial-state1 key-sequence))
               (result2 (nskk-state-test--execute-keys initial-state2 key-sequence)))
          (unless (nskk-state-test--states-equal-p result1 result2)
            (push (list :seed test-seed
                        :run run
                        :key-sequence key-sequence
                        :result1-mode (nskk-state-mode result1)
                        :result2-mode (nskk-state-mode result2)
                        :result1-buffer (nskk-state-input-buffer result1)
                        :result2-buffer (nskk-state-input-buffer result2))
                  failures))))
      (when failures
        (ert-fail (format "Determinism failed for %d cases (seed: %d):\n%S"
                          (length failures) test-seed
                          (take 3 failures)))))))

(nskk-describe "state sequence: undo/redo invariant"
  (nskk-it "should return to original state after undo then redo"
    (let ((runs 75)
          (failures nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'undo-redo-invariant' seed: %d" test-seed)
      (dotimes (run runs)
        (let* ((initial-state (nskk-state-create 'hiragana))
               ;; Setup: add something to undo stack
               (state-with-history
                (progn
                  (setf (nskk-state-undo-stack initial-state)
                        (list (list :mode 'ascii :input-buffer "test")))
                  (setf (nskk-state-mode initial-state) 'hiragana)
                  (setf (nskk-state-input-buffer initial-state) "")
                  initial-state))
               ;; Record original state
               (original-mode (nskk-state-mode state-with-history))
               (original-buffer (nskk-state-input-buffer state-with-history))
               ;; Apply undo then redo
               (after-undo (nskk-state-test--simulate-undo
                            (copy-sequence state-with-history)))
               (after-redo (nskk-state-test--simulate-redo after-undo)))
          ;; Check if we returned to original state
          (when (and after-undo after-redo)
            (unless (and (eq (nskk-state-mode after-redo) original-mode)
                         (string= (nskk-state-input-buffer after-redo) original-buffer))
              (push (list :seed test-seed
                          :run run
                          :original-mode original-mode
                          :original-buffer original-buffer
                          :final-mode (nskk-state-mode after-redo)
                          :final-buffer (nskk-state-input-buffer after-redo))
                    failures)))))
      (when failures
        (ert-fail (format "Undo/Redo invariant failed for %d cases (seed: %d):\n%S"
                          (length failures) test-seed
                          (take 3 failures)))))))

(nskk-describe "state sequence: typing Japanese"
  (nskk-it "should not crash when typing a valid romaji sequence"
    (let ((runs 75)
          (errors nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'typing-japanese-no-crash' seed: %d" test-seed)
      (dotimes (run runs)
        (condition-case err
            (let* ((state (nskk-state-create 'hiragana))
                   (romaji-seq (nskk-generate 'romaji-basic))
                   ;; Convert romaji string to key sequence
                   (key-seq (cl-loop for char across romaji-seq
                                     collect (char-to-string char)))
                   (final-state (nskk-state-test--simulate-japanese-input
                                 state key-seq)))
              ;; Just verify we got a valid state back
              (unless (nskk-state-p final-state)
                (push (list :seed test-seed :run run :error "Invalid final state")
                      errors)))
          (error
           (push (list :seed test-seed :run run :error (error-message-string err))
                 errors))))
      (when errors
        (ert-fail (format "Typing Japanese failed with %d errors (seed: %d):\n%S"
                          (length errors) test-seed
                          (take 3 errors)))))))

(nskk-describe "state sequence: invalid romaji"
  (nskk-it "should not crash when processing invalid romaji sequences"
    (let ((runs 75)
          (errors nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'invalid-romaji-no-crash' seed: %d" test-seed)
      (dotimes (run runs)
        (condition-case err
            (let* ((state (nskk-state-create 'hiragana))
                   ;; Generate potentially invalid sequences
                   (key-seq (nskk-generate 'key-sequence))
                   (final-state (nskk-state-test--execute-keys state key-seq)))
              ;; Verify state is still valid
              (unless (nskk-state-test--valid-state-p final-state)
                (push (list :seed test-seed :run run :error "State corrupted")
                      errors)))
          (error
           (push (list :seed test-seed :run run :error (error-message-string err))
                 errors))))
      (when errors
        (ert-fail (format "Invalid romaji handling failed with %d errors (seed: %d):\n%S"
                          (length errors) test-seed
                          (take 3 errors)))))))

(nskk-describe "state sequence: mode switch idempotent toggle"
  (nskk-it "should return to original mode after pressing toggle key twice"
    (let ((runs 75)
          (failures nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'mode-switch-idempotent-toggle' seed: %d" test-seed)
      (dotimes (run runs)
        ;; Test hiragana <-> katakana toggle with 'q' key
        (let* ((initial-mode (nskk--pbt-random-choice '(hiragana katakana)))
               (state (nskk-state-create initial-mode))
               ;; Press 'q' twice
               (after-first (nskk-state-test--process-key state "q"))
               (after-second (nskk-state-test--process-key after-first "q")))
          ;; After two 'q' presses, mode should return to initial
          (unless (eq (nskk-state-mode after-second) initial-mode)
            (push (list :seed test-seed
                        :run run
                        :initial-mode initial-mode
                        :after-first (nskk-state-mode after-first)
                        :after-second (nskk-state-mode after-second))
                  failures))))
      (when failures
        (ert-fail (format "Mode switch idempotency failed for %d cases (seed: %d):\n%S"
                          (length failures) test-seed
                          (take 3 failures)))))))

(nskk-describe "state sequence: mode switch consistency"
  (nskk-it "should always produce the same mode from the same mode switch key"
    (let ((runs 75)
          (failures nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'mode-switch-consistent' seed: %d" test-seed)
      (dotimes (run runs)
        (let* ((initial-mode (nskk-generate 'valid-mode))
               (switch-key (nskk--pbt-random-choice '("C-j" "l" ";")))
               (state1 (nskk-state-create initial-mode))
               (state2 (nskk-state-create initial-mode))
               (result1 (nskk-state-test--process-key state1 switch-key))
               (result2 (nskk-state-test--process-key state2 switch-key)))
          ;; Both should produce the same mode
          (unless (eq (nskk-state-mode result1) (nskk-state-mode result2))
            (push (list :seed test-seed
                        :run run
                        :initial-mode initial-mode
                        :switch-key switch-key
                        :result1-mode (nskk-state-mode result1)
                        :result2-mode (nskk-state-mode result2))
                  failures))))
      (when failures
        (ert-fail (format "Mode switch consistency failed for %d cases (seed: %d):\n%S"
                          (length failures) test-seed
                          (take 3 failures)))))))

(nskk-describe "state sequence: empty sequence"
  (nskk-it "should leave state unchanged when processing an empty key sequence"
    (let ((runs 75)
          (failures nil))
      (dotimes (run runs)
        (let* ((initial-mode (nskk-generate 'valid-mode))
               (state (nskk-state-create initial-mode))
               (result (nskk-state-test--execute-keys state nil)))
          (unless (nskk-state-test--states-equal-p state result)
            (push (list :run run
                        :initial-mode initial-mode
                        :result-mode (nskk-state-mode result))
                  failures))))
      (when failures
        (ert-fail (format "Empty sequence test failed for %d cases:\n%S"
                          (length failures) (take 3 failures)))))))

(nskk-describe "state sequence: long sequence"
  (nskk-it "should not overflow or crash on very long key sequences"
    (let ((runs 75)
          (errors nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'long-sequence-no-overflow' seed: %d" test-seed)
      (dotimes (run runs)
        (condition-case err
            (let* ((state (nskk-state-create 'hiragana))
                   ;; Generate a long sequence (50-100 keys)
                   (long-seq (nskk-generate 'key-sequence-of-length
                                            (nskk--pbt-random-int 50 100)))
                   (final-state (nskk-state-test--execute-keys state long-seq)))
              ;; Verify state is still valid
              (unless (nskk-state-test--valid-state-p final-state)
                (push (list :seed test-seed :run run :error "Invalid state")
                      errors))
              ;; Verify buffer bounds
              (unless (nskk-state-test--buffer-bounds-p final-state)
                (push (list :seed test-seed :run run :error "Buffer overflow")
                      errors)))
          (error
           (push (list :seed test-seed :run run :error (error-message-string err))
                 errors))))
      (when errors
        (ert-fail (format "Long sequence test failed with %d errors (seed: %d):\n%S"
                          (length errors) test-seed
                          (take 3 errors)))))))

(nskk-describe "state sequence: mixed typing and mode switch"
  (nskk-it "should maintain state integrity through mixed typing and mode switching"
    (let ((runs 75)
          (failures nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'mixed-typing-mode-switch' seed: %d" test-seed)
      (dotimes (run runs)
        (let* ((state (nskk-state-create 'hiragana))
               (key-seq (nskk-generate 'key-sequence))
               (final-state (nskk-state-test--execute-keys state key-seq)))
          ;; State should always be valid regardless of sequence
          (unless (and (nskk-state-test--valid-state-p final-state)
                       (nskk-state-test--buffer-bounds-p final-state)
                       (nskk-state-test--mode-valid-p final-state))
            (push (list :seed test-seed
                        :run run
                        :key-sequence (take 10 key-seq)
                        :valid-p (nskk-state-test--valid-state-p final-state)
                        :bounds-p (nskk-state-test--buffer-bounds-p final-state)
                        :mode-p (nskk-state-test--mode-valid-p final-state))
                  failures))))
      (when failures
        (ert-fail (format "Mixed sequence test failed for %d cases (seed: %d):\n%S"
                          (length failures) test-seed
                          (take 3 failures)))))))

(nskk-describe "state sequence: pure typing"
  (nskk-it "should accumulate all typed letters in the input buffer"
    (let ((runs 75)
          (failures nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'pure-typing' seed: %d" test-seed)
      (dotimes (run runs)
        (let* ((state (nskk-state-create 'hiragana))
               (key-seq (nskk-generate 'typing-key-sequence))
               (final-state (nskk-state-test--execute-keys state key-seq))
               (buffer (nskk-state-input-buffer final-state))
               (expected-length (length key-seq)))
          ;; Buffer should contain all typed characters
          (unless (= (length buffer) expected-length)
            (push (list :seed test-seed
                        :run run
                        :key-count expected-length
                        :buffer-length (length buffer)
                        :buffer buffer)
                  failures))))
      (when failures
        (ert-fail (format "Pure typing test failed for %d cases (seed: %d):\n%S"
                          (length failures) test-seed
                          (take 3 failures)))))))

(nskk-describe "state sequence: mode switch only"
  (nskk-it "should not affect the input buffer when only mode switches are pressed"
    (let ((runs 75)
          (failures nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'mode-switch-only' seed: %d" test-seed)
      (dotimes (run runs)
        (let* ((state (nskk-state-create 'hiragana))
               (mode-keys '("C-j" "q" "l" ";"))
               ;; Generate sequence of only mode switches
               (key-seq (cl-loop repeat (nskk--pbt-random-int 1 20)
                                 collect (nskk--pbt-random-choice mode-keys)))
               (final-state (nskk-state-test--execute-keys state key-seq))
               (buffer (nskk-state-input-buffer final-state)))
          ;; Buffer should be empty (no character input)
          (unless (string-empty-p buffer)
            (push (list :seed test-seed
                        :run run
                        :key-sequence key-seq
                        :buffer buffer)
                  failures))))
      (when failures
        (ert-fail (format "Mode switch only test failed for %d cases (seed: %d):\n%S"
                          (length failures) test-seed
                          (take 3 failures)))))))

(nskk-describe "state sequence: stack size bounded"
  (nskk-it "should keep undo/redo stack size bounded after long sequences"
    (let ((runs 75)
          (failures nil)
          (max-stack-size 100)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'stack-size-bounded' seed: %d" test-seed)
      (dotimes (run runs)
        (let* ((state (nskk-state-create 'hiragana))
               (key-seq (nskk-generate 'key-sequence-of-length
                                       (nskk--pbt-random-int 10 50)))
               (final-state (nskk-state-test--execute-keys state key-seq))
               (undo-size (length (nskk-state-undo-stack final-state)))
               (redo-size (length (nskk-state-redo-stack final-state)))
               (total-size (+ undo-size redo-size)))
          ;; Total stack size should be bounded
          (when (> total-size max-stack-size)
            (push (list :seed test-seed
                        :run run
                        :undo-size undo-size
                        :redo-size redo-size
                        :total-size total-size
                        :max-allowed max-stack-size)
                  failures))))
      (when failures
        (ert-fail (format "Stack size bounded test failed for %d cases (seed: %d):\n%S"
                          (length failures) test-seed
                          (take 3 failures)))))))


;;;
;;; nskk-ensure-overlay Tests
;;;

(nskk-describe "nskk-ensure-overlay"
  (nskk-context "overlay creation"
    (nskk-it "creates a new overlay when the variable is nil"
      (with-temp-buffer
        (let ((ov nil))
          (nskk-ensure-overlay ov (point-min) (point-max))
          (should (overlayp ov)))))

    (nskk-it "applies properties to the newly created overlay"
      (with-temp-buffer
        (let ((ov nil))
          (nskk-ensure-overlay ov (point-min) (point-max)
                               'display "X" 'face 'bold)
          (should (equal (overlay-get ov 'display) "X"))
          (should (eq (overlay-get ov 'face) 'bold))))))

  (nskk-context "overlay reuse"
    (nskk-it "reuses the existing overlay preserving eq identity"
      (with-temp-buffer
        (let ((ov (make-overlay (point-min) (point-max))))
          (let ((original-ov ov))
            (nskk-ensure-overlay ov (point-min) (point-max))
            (should (eq ov original-ov))))))

    (nskk-it "moves the existing overlay to the new position"
      (with-temp-buffer
        (insert "hello world")
        (let ((ov (make-overlay 1 5)))
          ;; Now ensure the overlay covers the full buffer
          (nskk-ensure-overlay ov (point-min) (point-max))
          (should (= (overlay-start ov) (point-min)))
          (should (= (overlay-end ov) (point-max))))))

    (nskk-it "applies properties even when the overlay already exists"
      (with-temp-buffer
        (let ((ov (make-overlay (point-min) (point-max))))
          (overlay-put ov 'display "old")
          (nskk-ensure-overlay ov (point-min) (point-max)
                               'display "new")
          (should (equal (overlay-get ov 'display) "new")))))))

;;;
;;; nskk-delete-overlay Tests
;;;

(nskk-describe "nskk-delete-overlay"
  (nskk-it "does nothing and signals no error when variable is nil"
    (let ((ov nil))
      (condition-case err
          (nskk-delete-overlay ov)
        (error (ert-fail (format "Unexpected error: %s" err))))
      (should (null ov))))

  (nskk-it "deletes the overlay and sets the variable to nil"
    (with-temp-buffer
      (let ((ov (make-overlay (point-min) (point-max))))
        (should (overlayp ov))
        (nskk-delete-overlay ov)
        ;; The variable must be nil after deletion
        (should (null ov)))))

  (nskk-it "the overlay object is no longer live after deletion"
    (with-temp-buffer
      (let* ((ov (make-overlay (point-min) (point-max)))
             (ov-copy ov))
        (nskk-delete-overlay ov)
        ;; overlay-buffer of a deleted overlay returns nil
        (should (null (overlay-buffer ov-copy)))))))

;;;
;;; nskk-ensure-marker Tests
;;;

(nskk-describe "nskk-ensure-marker macro"
  (nskk-it "creates a new marker when VAR is nil"
    (with-temp-buffer
      (let ((m nil))
        (nskk-ensure-marker m (point-min))
        (should (markerp m)))))

  (nskk-it "reuses the same marker object (eq identity)"
    (with-temp-buffer
      (let ((m (make-marker)))
        (set-marker m (point-min))
        (let ((original m))
          (nskk-ensure-marker m (point-max))
          (should (eq m original))))))

  (nskk-it "sets marker to the correct position"
    (with-temp-buffer
      (insert "hello")
      (let ((m nil))
        (nskk-ensure-marker m 3)
        (should (= (marker-position m) 3)))))

  (nskk-it "repositions an existing marker to the new position"
    (with-temp-buffer
      (insert "hello world")
      (let ((m (make-marker)))
        (set-marker m 1)
        (nskk-ensure-marker m 6)
        (should (= (marker-position m) 6))))))

;;;
;;; mode-category/2 Prolog Table Integrity Tests
;;;

(nskk-describe "mode-category/2 Prolog table integrity"
  (nskk-deftest-table state-prolog-mode-category-table
    :description "mode-category/2 maps input mode to orthogonal category"
    :columns (mode expected-category)
    :rows ((hiragana      japanese)
           (katakana      japanese)
           (katakana-半角  japanese)
           (abbrev        marker-mode)
           (ascii         other)
           (latin         other)
           (jisx0208-latin other))
    :body (should (eq expected-category
                      (nskk-prolog-query-value
                       `(mode-category ,mode ,'\?c) '\?c))))

  (nskk-it "returns nil for unknown mode"
    (should-not (nskk-prolog-query-value
                 `(mode-category nonexistent ,'\?c) '\?c))))

;;;
;;; Static Cache / Prolog Fact Invariant Tests
;;;
;;; These tests verify that the static `defconst' caches defined for
;;; hot-path performance remain in sync with the authoritative Prolog facts.
;;; A failure here means the caches need to be updated to match the facts.
;;;

(nskk-describe "nskk--valid-modes cache invariant"
  (nskk-it "every Prolog valid-mode fact is in the static set"
    (let ((prolog-modes
           (nskk-prolog-query-all-values '(valid-mode \?m) '\?m)))
      (dolist (m prolog-modes)
        (should (memq m nskk--valid-modes)))))

  (nskk-it "every static-set mode is known to Prolog"
    (dolist (m nskk--valid-modes)
      (should (nskk-prolog-holds-p `(valid-mode ,m)))))

  (nskk-it "count matches between Prolog facts and static set"
    (let ((prolog-modes
           (nskk-prolog-query-all-values '(valid-mode \?m) '\?m)))
      (should (= (length prolog-modes) (length nskk--valid-modes))))))

(nskk-describe "nskk--valid-henkan-phases cache invariant"
  (nskk-it "every Prolog valid-henkan-phase fact is in the static list"
    (let ((prolog-phases
           (nskk-prolog-query-all-values '(valid-henkan-phase \?p) '\?p)))
      (dolist (p prolog-phases)
        (should (memq p nskk--valid-henkan-phases)))))

  (nskk-it "every static-list phase is known to Prolog"
    (dolist (p nskk--valid-henkan-phases)
      (should (nskk-prolog-holds-p `(valid-henkan-phase ,p)))))

  (nskk-it "nil is a valid henkan phase in both"
    (should (memq nil nskk--valid-henkan-phases))
    (should (nskk-prolog-holds-p '(valid-henkan-phase nil)))))

(nskk-describe "nskk--valid-henkan-transitions cache invariant"
  (nskk-it "every static transition is known to Prolog"
    (dolist (pair nskk--valid-henkan-transitions)
      (let ((from (car pair))
            (to   (cdr pair)))
        (should (nskk-prolog-holds-p
                 `(valid-henkan-transition ,from ,to))))))

  (nskk-it "nil→on is present in both"
    (should (nskk--henkan-transition-valid-p nil 'on))
    (should (nskk-prolog-holds-p '(valid-henkan-transition nil on))))

  (nskk-it "on→active is present in both"
    (should (nskk--henkan-transition-valid-p 'on 'active))
    (should (nskk-prolog-holds-p '(valid-henkan-transition on active)))))

(nskk-describe "nskk--state-slot-defaults cache invariant"
  (nskk-it "input-buffer default matches Prolog"
    (should (equal (plist-get nskk--state-slot-defaults :input-buffer)
                   (nskk-prolog-query-value
                    '(state-slot-default input-buffer \?v) '\?v))))

  (nskk-it "henkan-phase default matches Prolog"
    (should (equal (plist-get nskk--state-slot-defaults :henkan-phase)
                   (nskk-prolog-query-value
                    '(state-slot-default henkan-phase \?v) '\?v))))

  (nskk-it "current-index default matches Prolog"
    (should (equal (plist-get nskk--state-slot-defaults :current-index)
                   (nskk-prolog-query-value
                    '(state-slot-default current-index \?v) '\?v)))))

(provide 'nskk-state-test)

;;; nskk-state-test.el ends here
