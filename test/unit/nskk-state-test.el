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
      (should (eq (nskk-state-mode state) 'ascii))
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
  (nskk-it "returns true for all valid modes"
    (dolist (mode nskk-state-test-valid-modes)
      (should (nskk-state-valid-mode-p mode))))

  (nskk-it "returns false for invalid modes"
    (nskk-then
      (should (not (nskk-state-valid-mode-p 'invalid-mode)))
      (should (not (nskk-state-valid-mode-p nil)))
      (should (not (nskk-state-valid-mode-p "hiragana")))
      (should (not (nskk-state-valid-mode-p 123)))))

  (nskk-it "validates all modes comprehensively"
    ;; Valid modes should pass
    (should (nskk-state-valid-mode-p 'ascii))
    (should (nskk-state-valid-mode-p 'hiragana))
    (should (nskk-state-valid-mode-p 'katakana))
    (should (nskk-state-valid-mode-p 'katakana-半角))
    (should (nskk-state-valid-mode-p 'abbrev))
    (should (nskk-state-valid-mode-p 'latin))
    (should (nskk-state-valid-mode-p 'jisx0208-latin))

    ;; Invalid modes should fail
    (should (not (nskk-state-valid-mode-p 'invalid)))
    (should (not (nskk-state-valid-mode-p 'mode)))
    (should (not (nskk-state-valid-mode-p nil)))
    (should (not (nskk-state-valid-mode-p "hiragana")))
    (should (not (nskk-state-valid-mode-p 42)))
    (should (not (nskk-state-valid-mode-p '())))))

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
      (should (eq (nskk-state-mode state) 'ascii))

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
      (if (> (length input) 0)
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
;;; Prolog Predicate Tests: preedit-phase/1
;;;

(nskk-describe "preedit-phase Prolog predicate"
  (nskk-deftest-table state-prolog-preedit-phase-known
    :description "preedit-phase/1 succeeds for known phases"
    :columns (phase)
    :rows ((normal) (preedit))
    :body (should (nskk-prolog-query-one `(preedit-phase ,phase))))

  (nskk-it "returns nil for unknown phase 'on'"
    (should-not (nskk-prolog-query-one '(preedit-phase on)))))

;;;
;;; Prolog Predicate Tests: registration-phase/1
;;;

(nskk-describe "registration-phase Prolog predicate"
  (nskk-deftest-table state-prolog-registration-phase-known
    :description "registration-phase/1 succeeds for known phases"
    :columns (phase)
    :rows ((active) (list))
    :body (should (nskk-prolog-query-one `(registration-phase ,phase))))

  (nskk-it "returns nil for invalid phase 'normal'"
    (should-not (nskk-prolog-query-one '(registration-phase normal)))))

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
      (should (eq (nskk-state-mode state) 'ascii))

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

(provide 'nskk-state-test)

;;; nskk-state-test.el ends here
