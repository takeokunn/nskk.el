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

(nskk-deftest-unit state-create-default
  "Test state creation with default mode."
  (let ((state (nskk-state-create)))
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
    (should (null (nskk-state-metadata state)))))

(nskk-deftest-unit state-create-hiragana
  "Test state creation with hiragana mode."
  (let ((state (nskk-state-create 'hiragana)))
    (should (nskk-state-p state))
    (should (eq (nskk-state-mode state) 'hiragana))
    (should (eq (nskk-state-previous-mode state) 'hiragana))))

(nskk-deftest-unit state-create-katakana
  "Test state creation with katakana mode."
  (let ((state (nskk-state-create 'katakana)))
    (should (nskk-state-p state))
    (should (eq (nskk-state-mode state) 'katakana))))

(nskk-deftest-unit state-create-invalid-mode
  "Test state creation with invalid mode (should default to ascii)."
  (let ((state (nskk-state-create 'invalid-mode)))
    (should (nskk-state-p state))
    (should (eq (nskk-state-mode state) 'ascii))))

(nskk-deftest-unit state-create-all-valid-modes
  "Test state creation with all valid modes."
  (dolist (mode nskk-state-test-valid-modes)
    (let ((state (nskk-state-create mode)))
      (should (nskk-state-p state))
      (should (eq (nskk-state-mode state) mode)))))

;;;
;;; Getter Tests
;;;

(nskk-deftest-unit state-get-valid-slot
  "Test nskk-state-get with valid slot."
  (let ((state (nskk-state-create 'hiragana)))
    (should (eq (nskk-state-get state 'mode) 'hiragana))
    (should (string= (nskk-state-get state 'input-buffer) ""))
    (should (= (nskk-state-get state 'current-index) 0))))

(nskk-deftest-unit state-get-string-slot
  "Test nskk-state-get with string slot name."
  (let ((state (nskk-state-create 'katakana)))
    (should (eq (nskk-state-get state "mode") 'katakana))))

(nskk-deftest-unit state-get-invalid-slot
  "Test nskk-state-get with invalid slot."
  (let ((state (nskk-state-create)))
    (should (null (nskk-state-get state 'invalid-slot)))))

(nskk-deftest-unit state-get-nil-state
  "Test nskk-state-get with nil state."
  (should (null (nskk-state-get nil 'mode))))

(nskk-deftest-unit state-get-all-slots
  "Test nskk-state-get for all state slots that have non-nil defaults."
  (let ((state (nskk-state-create)))
    ;; These slots have non-nil default values
    (dolist (slot '(mode input-buffer converted-buffer
                        current-index previous-mode))
      (should (nskk-state-get state slot)))
    ;; These slots default to nil - verify they are accessible without error
    (dolist (slot '(candidates henkan-position marker-position
                              undo-stack redo-stack henkan-phase metadata))
      (should-not (nskk-state-get state slot)))))

;;;
;;; Setter Tests
;;;

(nskk-deftest-unit state-set-mode
  "Test nskk-state-set for mode."
  (let ((state (nskk-state-create 'ascii)))
    (should (eq (nskk-state-set state 'mode 'hiragana) 'hiragana))
    (should (eq (nskk-state-mode state) 'hiragana))
    (should (eq (nskk-state-previous-mode state) 'ascii))))

(nskk-deftest-unit state-set-input-buffer
  "Test nskk-state-set for input-buffer."
  (let ((state (nskk-state-create)))
    (should (equal (nskk-state-set state 'input-buffer "test") "test"))
    (should (string= (nskk-state-input-buffer state) "test"))))

(nskk-deftest-unit state-set-converted-buffer
  "Test nskk-state-set for converted-buffer."
  (let ((state (nskk-state-create)))
    (should (equal (nskk-state-set state 'converted-buffer "converted") "converted"))
    (should (string= (nskk-state-converted-buffer state) "converted"))))

(nskk-deftest-unit state-set-candidates
  "Test nskk-state-set for candidates."
  (let ((state (nskk-state-create))
        (candidates '("candidate1" "candidate2" "candidate3")))
    (should (eq (nskk-state-set state 'candidates candidates) candidates))
    (should (equal (nskk-state-candidates state) candidates))))

(nskk-deftest-unit state-set-current-index
  "Test nskk-state-set for current-index."
  (let ((state (nskk-state-create)))
    (should (eq (nskk-state-set state 'current-index 5) 5))
    (should (= (nskk-state-current-index state) 5))))

(nskk-deftest-unit state-set-henkan-position
  "Test nskk-state-set for henkan-position."
  (let ((state (nskk-state-create)))
    (should (eq (nskk-state-set state 'henkan-position 10) 10))
    (should (= (nskk-state-henkan-position state) 10))))

(nskk-deftest-unit state-set-invalid-mode
  "Test nskk-state-set with invalid mode (should raise error)."
  (let ((state (nskk-state-create 'ascii)))
    (should-error (nskk-state-set state 'mode 'invalid-mode))
    (should (eq (nskk-state-mode state) 'ascii))))

(nskk-deftest-unit state-set-nil-state
  "Test nskk-state-set with nil state."
  (should (null (nskk-state-set nil 'mode 'hiragana))))

;;;
;;; Mode Validation Tests
;;;

(nskk-deftest-unit state-valid-mode-all-valid
  "Test nskk-state-valid-mode-p for all valid modes."
  (dolist (mode nskk-state-test-valid-modes)
    (should (nskk-state-valid-mode-p mode))))

(nskk-deftest-unit state-valid-mode-invalid
  "Test nskk-state-valid-mode-p with invalid modes."
  (should (not (nskk-state-valid-mode-p 'invalid-mode)))
  (should (not (nskk-state-valid-mode-p nil)))
  (should (not (nskk-state-valid-mode-p "hiragana")))
  (should (not (nskk-state-valid-mode-p 123))))

(nskk-deftest-unit state-in-henkan-mode-true-on
  "Test nskk-state-in-henkan-mode-p when henkan-phase is on."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set-henkan-phase state 'on)
    (should (nskk-state-in-henkan-mode-p state))))

(nskk-deftest-unit state-in-henkan-mode-true-active
  "Test nskk-state-in-henkan-mode-p when henkan-phase is active."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-force-henkan-phase state 'active)
    (should (nskk-state-in-henkan-mode-p state))))

(nskk-deftest-unit state-in-henkan-mode-true-list
  "Test nskk-state-in-henkan-mode-p when henkan-phase is list."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-force-henkan-phase state 'list)
    (should (nskk-state-in-henkan-mode-p state))))

(nskk-deftest-unit state-in-henkan-mode-true-registration
  "Test nskk-state-in-henkan-mode-p when henkan-phase is registration."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-force-henkan-phase state 'registration)
    (should (nskk-state-in-henkan-mode-p state))))

(nskk-deftest-unit state-in-henkan-mode-false-nil-phase
  "Test nskk-state-in-henkan-mode-p with nil henkan-phase."
  (let ((state (nskk-state-create 'hiragana)))
    (should (not (nskk-state-in-henkan-mode-p state)))))

(nskk-deftest-unit state-henkan-on-p-true
  "Test nskk-state-henkan-on-p when phase is on."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set-henkan-phase state 'on)
    (should (nskk-state-henkan-on-p state))))

(nskk-deftest-unit state-henkan-on-p-false
  "Test nskk-state-henkan-on-p when phase is not on."
  (let ((state (nskk-state-create 'hiragana)))
    (should (not (nskk-state-henkan-on-p state)))
    (nskk-state-force-henkan-phase state 'active)
    (should (not (nskk-state-henkan-on-p state)))))

(nskk-deftest-unit state-henkan-active-p-true
  "Test nskk-state-henkan-active-p when phase is active."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-force-henkan-phase state 'active)
    (should (nskk-state-henkan-active-p state))))

(nskk-deftest-unit state-henkan-active-p-false
  "Test nskk-state-henkan-active-p when phase is not active."
  (let ((state (nskk-state-create 'hiragana)))
    (should (not (nskk-state-henkan-active-p state)))
    (nskk-state-set-henkan-phase state 'on)
    (should (not (nskk-state-henkan-active-p state)))))

(nskk-deftest-unit state-set-henkan-phase-transitions
  "Test nskk-state-set-henkan-phase transitions through all phases."
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

(nskk-deftest-unit state-set-henkan-phase-invalid-transition
  "Test that invalid henkan phase transitions signal an error."
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

(nskk-deftest-unit state-force-henkan-phase-bypasses-validation
  "Test that force-henkan-phase bypasses transition validation."
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

(nskk-deftest-unit state-set-henkan-phase-same-phase-noop
  "Test that same-phase transitions are allowed (no-op)."
  (let ((state (nskk-state-create 'hiragana)))
    ;; nil -> nil should succeed
    (nskk-state-set-henkan-phase state nil)
    (should (null (nskk-state-henkan-phase state)))
    ;; on -> on should succeed
    (nskk-state-set-henkan-phase state 'on)
    (nskk-state-set-henkan-phase state 'on)
    (should (eq (nskk-state-henkan-phase state) 'on))))

(nskk-deftest-unit state-create-jisx0208-latin
  "Test state creation with jisx0208-latin mode."
  (let ((state (nskk-state-create 'jisx0208-latin)))
    (should (nskk-state-p state))
    (should (eq (nskk-state-mode state) 'jisx0208-latin))))

;;;
;;; Mode Transition Tests
;;;

(nskk-deftest-unit state-transition-success
  "Test successful mode transition."
  (let ((state (nskk-state-create 'ascii)))
    (should (nskk-state-transition state 'ascii 'hiragana))
    (should (eq (nskk-state-mode state) 'hiragana))))

(nskk-deftest-unit state-transition-all-modes
  "Test transitions between all valid mode combinations."
  (dolist (from-mode nskk-state-test-valid-modes)
    (dolist (to-mode nskk-state-test-valid-modes)
      (let ((state (nskk-state-create from-mode)))
        (should (nskk-state-transition state from-mode to-mode))
        (should (eq (nskk-state-mode state) to-mode))))))

(nskk-deftest-unit state-transition-wrong-from
  "Test mode transition with wrong from-mode."
  (let ((state (nskk-state-create 'ascii)))
    (should (not (nskk-state-transition state 'hiragana 'katakana)))
    (should (eq (nskk-state-mode state) 'ascii))))

(nskk-deftest-unit state-transition-invalid-to
  "Test mode transition with invalid to-mode."
  (let ((state (nskk-state-create 'ascii)))
    (should (not (nskk-state-transition state 'ascii 'invalid-mode)))
    (should (eq (nskk-state-mode state) 'ascii))))

(nskk-deftest-unit state-transition-nil-state
  "Test mode transition with nil state."
  (should (not (nskk-state-transition nil 'ascii 'hiragana))))

;;;
;;; State Reset Tests
;;;

(nskk-deftest-unit state-reset-default
  "Test state reset with default behavior."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set state 'input-buffer "test")
    (nskk-state-set state 'converted-buffer "converted")
    (nskk-state-set state 'candidates '("a" "b"))
    (nskk-state-set state 'current-index 1)
    (nskk-state-set state 'henkan-position 5)
    (nskk-state-set state 'undo-stack '(("a" . "b")))
    (nskk-state-set state 'redo-stack '(("c" . "d")))
    (nskk-state-set state 'metadata '(:key "value"))

    (nskk-state-reset state)

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
    (should (null (nskk-state-metadata state)))))

(nskk-deftest-unit state-reset-nil-state
  "Test state reset with nil state."
  (should (not (nskk-state-reset nil))))

;;;
;;; Buffer Management Tests
;;;

(nskk-deftest-unit state-append-input-single
  "Test appending single character to input buffer."
  (let ((state (nskk-state-create)))
    (nskk-state-append-input state ?a)
    (should (string= (nskk-state-input-buffer state) "a"))
    (nskk-state-append-input state ?b)
    (should (string= (nskk-state-input-buffer state) "ab"))))

(nskk-deftest-unit state-append-input-japanese
  "Test appending Japanese characters to input buffer."
  (let ((state (nskk-state-create)))
    (nskk-state-append-input state ?\u3042)
    (should (string= (nskk-state-input-buffer state) "\u3042"))
    (nskk-state-append-input state ?\u3044)
    (should (string= (nskk-state-input-buffer state) "\u3042\u3044"))))

(nskk-deftest-unit state-delete-last-char
  "Test deleting last character from input buffer."
  (let ((state (nskk-state-create)))
    (nskk-state-set state 'input-buffer "abc")
    (should (eq (nskk-state-delete-last-char state) ?c))
    (should (string= (nskk-state-input-buffer state) "ab"))
    (should (eq (nskk-state-delete-last-char state) ?b))
    (should (string= (nskk-state-input-buffer state) "a"))))

(nskk-deftest-unit state-delete-last-char-empty
  "Test deleting from empty input buffer."
  (let ((state (nskk-state-create)))
    (should (null (nskk-state-delete-last-char state)))
    (should (string= (nskk-state-input-buffer state) ""))))

(nskk-deftest-unit state-clear-input
  "Test clearing input buffer."
  (let ((state (nskk-state-create)))
    (nskk-state-set state 'input-buffer "test input")
    (nskk-state-clear-input state)
    (should (string= (nskk-state-input-buffer state) ""))))

(nskk-deftest-unit state-buffer-operations-sequence
  "Test sequence of buffer operations."
  (let ((state (nskk-state-create)))
    (nskk-state-append-input state ?t)
    (nskk-state-append-input state ?e)
    (nskk-state-append-input state ?s)
    (should (string= (nskk-state-input-buffer state) "tes"))

    (should (eq (nskk-state-delete-last-char state) ?s))
    (should (string= (nskk-state-input-buffer state) "te"))

    (nskk-state-clear-input state)
    (should (string= (nskk-state-input-buffer state) ""))))

;;;
;;; Candidate Management Tests
;;;

(nskk-deftest-unit state-set-candidates-reset
  "Test setting candidates resets index."
  (let ((state (nskk-state-create)))
    (nskk-state-set state 'current-index 5)
    (nskk-state-set-candidates state '("a" "b" "c"))
    (should (= (nskk-state-current-index state) 0))
    (should (equal (nskk-state-candidates state) '("a" "b" "c")))))

(nskk-deftest-unit state-current-candidate
  "Test getting current candidate."
  (let ((state (nskk-state-create)))
    (nskk-state-set-candidates state '("first" "second" "third"))
    (should (string= (nskk-state-current-candidate state) "first"))

    (nskk-state-set state 'current-index 1)
    (should (string= (nskk-state-current-candidate state) "second"))

    (nskk-state-set state 'current-index 2)
    (should (string= (nskk-state-current-candidate state) "third"))))

(nskk-deftest-unit state-next-candidate
  "Test moving to next candidate."
  (let ((state (nskk-state-create)))
    (nskk-state-set-candidates state '("a" "b" "c"))

    (should (string= (nskk-state-next-candidate state) "b"))
    (should (= (nskk-state-current-index state) 1))

    (should (string= (nskk-state-next-candidate state) "c"))
    (should (= (nskk-state-current-index state) 2))

    ;; Should wrap around
    (should (string= (nskk-state-next-candidate state) "a"))
    (should (= (nskk-state-current-index state) 0))))

(nskk-deftest-unit state-next-candidate-single
  "Test next candidate with single entry."
  (let ((state (nskk-state-create)))
    (nskk-state-set-candidates state '("only"))
    (should (string= (nskk-state-next-candidate state) "only"))
    (should (= (nskk-state-current-index state) 0))))

(nskk-deftest-unit state-next-candidate-nil-candidates
  "Test next candidate with no candidates."
  (let ((state (nskk-state-create)))
    (should (null (nskk-state-next-candidate state)))))

(nskk-deftest-unit state-previous-candidate
  "Test moving to previous candidate."
  (let ((state (nskk-state-create)))
    (nskk-state-set-candidates state '("a" "b" "c"))
    (nskk-state-set state 'current-index 1)

    (should (string= (nskk-state-previous-candidate state) "a"))
    (should (= (nskk-state-current-index state) 0))

    ;; Should wrap around
    (should (string= (nskk-state-previous-candidate state) "c"))
    (should (= (nskk-state-current-index state) 2))))

(nskk-deftest-unit state-previous-candidate-nil-candidates
  "Test previous candidate with no candidates."
  (let ((state (nskk-state-create)))
    (should (null (nskk-state-previous-candidate state)))))

(nskk-deftest-unit state-candidate-navigation
  "Test full candidate navigation cycle."
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
    (should (string= (nskk-state-current-candidate state) "four"))))

;;;
;;; Metadata Tests
;;;

(nskk-deftest-unit state-get-metadata-nil
  "Test getting metadata from nil metadata."
  (let ((state (nskk-state-create)))
    (should (null (nskk-state-get-metadata state :key)))))

(nskk-deftest-unit state-put-metadata-single
  "Test putting single metadata value."
  (let ((state (nskk-state-create)))
    (nskk-state-put-metadata state :test-key "test-value")
    (should (string= (nskk-state-get-metadata state :test-key) "test-value"))))

(nskk-deftest-unit state-put-metadata-multiple
  "Test putting multiple metadata values."
  (let ((state (nskk-state-create)))
    (nskk-state-put-metadata state :key1 "value1")
    (nskk-state-put-metadata state :key2 "value2")
    (nskk-state-put-metadata state :key3 123)
    (nskk-state-put-metadata state :key4 '(a b c))

    (should (string= (nskk-state-get-metadata state :key1) "value1"))
    (should (string= (nskk-state-get-metadata state :key2) "value2"))
    (should (= (nskk-state-get-metadata state :key3) 123))
    (should (equal (nskk-state-get-metadata state :key4) '(a b c)))))

(nskk-deftest-unit state-put-metadata-overwrite
  "Test overwriting metadata value."
  (let ((state (nskk-state-create)))
    (nskk-state-put-metadata state :key "original")
    (should (string= (nskk-state-get-metadata state :key) "original"))

    (nskk-state-put-metadata state :key "updated")
    (should (string= (nskk-state-get-metadata state :key) "updated"))))

(nskk-deftest-unit state-metadata-with-reset
  "Test that reset clears metadata."
  (let ((state (nskk-state-create)))
    (nskk-state-put-metadata state :key1 "value1")
    (nskk-state-put-metadata state :key2 "value2")

    (nskk-state-reset state)

    (should (null (nskk-state-get-metadata state :key1)))
    (should (null (nskk-state-get-metadata state :key2)))))

;;;
;;; Error Handling Tests: Mode Validation
;;;

(nskk-deftest-unit state-valid-mode-p-comprehensive
  "Test nskk-state-valid-mode-p validates all modes correctly."
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
  (should (not (nskk-state-valid-mode-p '()))))

(nskk-deftest-unit state-set-mode-boundary-cases
  "Test nskk-state-set mode setter with boundary cases."
  (let ((state (nskk-state-create 'ascii)))
    ;; Setting to each valid mode should work
    (dolist (mode '(ascii hiragana katakana katakana-半角 abbrev latin jisx0208-latin))
      (let ((result (nskk-state-set state 'mode mode)))
        (should (eq result mode))
        (should (eq (nskk-state-mode state) mode))))

    ;; Invalid modes should raise error and not change state
    (nskk-state-set state 'mode 'hiragana)
    (should-error (nskk-state-set state 'mode 'not-a-mode))
    (should (eq (nskk-state-mode state) 'hiragana))))

(nskk-deftest-unit state-set-mode-previous-tracking
  "Test that previous-mode is tracked through multiple transitions."
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

;;;
;;; Error Handling Tests: State Transition Validation
;;;

(nskk-deftest-unit state-transition-validates-from-mode
  "Test that transition validates current mode matches from-mode."
  (let ((state (nskk-state-create 'ascii)))
    ;; Correct from-mode should succeed
    (should (nskk-state-transition state 'ascii 'hiragana))
    (should (eq (nskk-state-mode state) 'hiragana))

    ;; Wrong from-mode should fail
    (should (not (nskk-state-transition state 'ascii 'katakana)))
    (should (eq (nskk-state-mode state) 'hiragana))))

(nskk-deftest-unit state-transition-validates-to-mode
  "Test that transition validates target mode is valid."
  (let ((state (nskk-state-create 'ascii)))
    ;; Invalid to-mode should fail
    (should (not (nskk-state-transition state 'ascii 'not-a-mode)))
    (should (eq (nskk-state-mode state) 'ascii))))

(nskk-deftest-unit state-transition-rejects-nil-state
  "Test that transition fails safely with nil state."
  (should (not (nskk-state-transition nil 'ascii 'hiragana))))

(nskk-deftest-unit state-transition-sequence-validation
  "Test multiple transitions maintain validation."
  (let ((state (nskk-state-create 'ascii)))
    ;; Build a sequence of transitions
    (should (nskk-state-transition state 'ascii 'hiragana))
    (should (nskk-state-transition state 'hiragana 'katakana))
    (should (nskk-state-transition state 'katakana 'latin))
    (should (nskk-state-transition state 'latin 'abbrev))
    (should (nskk-state-transition state 'abbrev 'ascii))

    ;; Final state should be ascii
    (should (eq (nskk-state-mode state) 'ascii))))

;;;
;;; Error Handling Tests: State Get/Set With Invalid Input
;;;

(nskk-deftest-unit state-set-with-nil-state
  "Test that state-set handles nil state gracefully."
  (should (null (nskk-state-set nil 'mode 'hiragana)))
  (should (null (nskk-state-set nil 'input-buffer "test"))))

(nskk-deftest-unit state-set-rejects-invalid-slot
  "Test that state-set rejects non-existent slots."
  (let ((state (nskk-state-create)))
    ;; Setting invalid slot should return nil
    (should (null (nskk-state-set state 'not-a-slot 'value)))))

(nskk-deftest-unit state-get-with-nil-state
  "Test that state-get handles nil state gracefully."
  (should (null (nskk-state-get nil 'mode)))
  (should (null (nskk-state-get nil 'input-buffer))))

(nskk-deftest-unit state-get-with-invalid-slot
  "Test that state-get handles invalid slot names gracefully."
  (let ((state (nskk-state-create)))
    (should (null (nskk-state-get state 'not-a-slot)))))

;;;
;;; Error Handling Tests: Mode-Sensitive Operations
;;;

(nskk-deftest-unit state-mode-change-affects-operations
  "Test that mode changes properly affect state behavior."
  (let ((state (nskk-state-create 'ascii)))
    ;; In ascii mode
    (should (eq (nskk-state-mode state) 'ascii))

    ;; Switch to hiragana
    (nskk-state-set state 'mode 'hiragana)
    (should (eq (nskk-state-mode state) 'hiragana))

    ;; State operations should work consistently
    (nskk-state-append-input state ?a)
    (should (string= (nskk-state-input-buffer state) "a"))

    ;; Mode-aware operations should respect current mode
    (let ((mode (nskk-state-mode state)))
      (should (eq mode 'hiragana)))))

(nskk-deftest-unit state-reset-preserves-mode
  "Test that reset preserves the current mode."
  (let ((state (nskk-state-create 'katakana)))
    ;; Set up state
    (nskk-state-set state 'input-buffer "test")
    (nskk-state-set state 'henkan-position 5)
    (nskk-state-set-candidates state '("a" "b" "c"))

    ;; Reset
    (nskk-state-reset state)

    ;; Mode should be preserved
    (should (eq (nskk-state-mode state) 'katakana))
    ;; But other state should be cleared
    (should (string= (nskk-state-input-buffer state) ""))
    (should (null (nskk-state-henkan-position state)))
    (should (null (nskk-state-candidates state)))))

;;;
;;; Integration Tests
;;;

(nskk-deftest-integration state-full-input-workflow
  "Test full workflow: input, convert, navigate candidates."
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

(nskk-deftest-integration state-mode-switching-workflow
  "Test workflow with multiple mode switches."
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

(nskk-deftest-integration state-error-correction-workflow
  "Test workflow with error correction (delete and re-input)."
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
    (should (string= (nskk-state-current-candidate state) "\u6771\u4eac"))))

;;;
;;; Japanese Mode Classification Tests
;;;

(nskk-deftest-unit state-japanese-mode-hiragana
  "Test japanese-mode/1 is true for hiragana."
  (should (nskk-prolog-query '(japanese-mode hiragana))))

(nskk-deftest-unit state-japanese-mode-katakana
  "Test japanese-mode/1 is true for katakana."
  (should (nskk-prolog-query '(japanese-mode katakana))))

(nskk-deftest-unit state-japanese-mode-ascii-false
  "Test japanese-mode/1 is false for ascii."
  (should-not (nskk-prolog-query '(japanese-mode ascii))))

(nskk-deftest-unit state-japanese-mode-latin-false
  "Test japanese-mode/1 is false for latin."
  (should-not (nskk-prolog-query '(japanese-mode latin))))

(nskk-deftest-unit state-japanese-mode-jisx0208-false
  "Test japanese-mode/1 is false for jisx0208-latin."
  (should-not (nskk-prolog-query '(japanese-mode jisx0208-latin))))

(nskk-deftest-unit state-japanese-mode-abbrev-false
  "Test japanese-mode/1 is false for abbrev."
  (should-not (nskk-prolog-query '(japanese-mode abbrev))))

(nskk-deftest-unit state-japanese-mode-katakana-hankaku
  "Test japanese-mode/1 is true for katakana-半角 (half-width katakana)."
  (should (nskk-prolog-query '(japanese-mode katakana-半角))))

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

(provide 'nskk-state-test)

;;; nskk-state-test.el ends here
