;;; nskk-state-test.el --- State management tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; State management tests.

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

(defun nskk-state-test--get-mode-with-state/k (mode on-found on-not-found)
  "Invoke `nskk-state-get-mode/k' with temporary MODE as current state."
  (let ((nskk-current-state (nskk-state-create mode)))
    (nskk-state-get-mode/k on-found on-not-found)))

(defun nskk-state-test--toggle-kana-mode (state)
  "Toggle STATE's mode between hiragana and katakana.
Mirrors the legacy \\='q\\=' mode-switch key semantics; other modes are
left unchanged."
  (nskk-state-set-mode
   state
   (pcase (nskk-state-mode state)
     ('hiragana 'katakana)
     ('katakana 'hiragana)
     (other other)))
  state)

(defun nskk-state-test--random-op ()
  "Generate one random state-mutation op for sequence property tests.
An op is a list whose head names the surviving `nskk-state' API it
drives: `:mode' (via `nskk-state-set'), `:phase' (via
`nskk-state-set-henkan-phase'), `:candidates' (via `nskk-state-set'),
or `:metadata' (via `nskk-state-put-metadata')."
  (pcase (nskk--pbt-random 4)
    (0 (list :mode (nskk-generate 'valid-mode)))
    (1 (list :phase (nskk--pbt-random-choice nskk--valid-henkan-phases)))
    (2 (list :candidates (cl-loop repeat (nskk--pbt-random-int 0 3)
                                  collect (nskk-generate 'hiragana-string))))
    (_ (list :metadata (nskk--pbt-random-choice '(:a :b :c))
             (nskk-generate 'romaji-string)))))

(defun nskk-state-test--random-ops (n)
  "Generate a list of N random ops via `nskk-state-test--random-op'."
  (cl-loop repeat n collect (nskk-state-test--random-op)))

(defun nskk-state-test--apply-op (state op)
  "Apply OP to STATE and return STATE.
An invalid henkan-phase transition is swallowed so that replaying the
same OP list against two independently-created states stays
deterministic regardless of which phase each op lands on."
  (pcase op
    (`(:mode ,mode) (nskk-state-set state 'mode mode))
    (`(:phase ,phase) (ignore-errors (nskk-state-set-henkan-phase state phase)))
    (`(:candidates ,candidates) (nskk-state-set state 'candidates candidates))
    (`(:metadata ,key ,value) (nskk-state-put-metadata state key value)))
  state)

(defun nskk-state-test--apply-ops (state ops)
  "Apply OPS in order to STATE and return STATE."
  (dolist (op ops state)
    (nskk-state-test--apply-op state op)))

(defun nskk-state-test--states-equal-p (state1 state2)
  "Return non-nil if STATE1 and STATE2 agree on mode, henkan-phase,
candidates, current-index, and metadata."
  (and (nskk-state-p state1)
       (nskk-state-p state2)
       (eq (nskk-state-mode state1) (nskk-state-mode state2))
       (eq (nskk-state-henkan-phase state1) (nskk-state-henkan-phase state2))
       (equal (nskk-state-candidates state1) (nskk-state-candidates state2))
       (= (nskk-state-current-index state1) (nskk-state-current-index state2))
       (equal (nskk-state-metadata state1) (nskk-state-metadata state2))))


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
      (nskk-state-set state 'current-index 2)
      (nskk-then
        (should (eq (nskk-state-set state 'candidates candidates) candidates))
        (should (eq (nskk-state-candidates state) candidates))
        (should (= (nskk-state-current-index state) 0)))))

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
      (nskk-state-set state 'input-buffer "unchanged")
      (nskk-state-set state 'candidates '("a" "b"))
      (nskk-state-set state 'current-index 1)
      (let ((before (copy-nskk-state state)))
        (nskk-then
          (should-error (nskk-state-set state 'mode 'invalid-mode))
          (should (equal state before))))))

  (nskk-it "raises error for invalid henkan-phase and leaves state unchanged"
    (let ((state (nskk-state-create 'ascii)))
      (nskk-state-set state 'input-buffer "unchanged")
      (nskk-state-set state 'candidates '("a" "b"))
      (nskk-state-set state 'current-index 1)
      (let ((before (copy-nskk-state state)))
        (nskk-then
          (should-error (nskk-state-set state 'henkan-phase 'not-a-real-phase))
          (should (equal state before))))))

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
            (should (eq (nskk-state-previous-mode state) 'ascii))

      (nskk-state-set state 'mode 'hiragana)
      (should (eq (nskk-state-mode state) 'hiragana))
      (should (eq (nskk-state-previous-mode state) 'ascii))

      (nskk-state-set state 'mode 'katakana)
      (should (eq (nskk-state-mode state) 'katakana))
      (should (eq (nskk-state-previous-mode state) 'hiragana))

      (nskk-state-set state 'mode 'latin)
      (should (eq (nskk-state-mode state) 'latin))
      (should (eq (nskk-state-previous-mode state) 'katakana))))

  (nskk-it "accepts all valid modes and rejects invalid ones"
    (let ((state (nskk-state-create 'ascii)))
      (dolist (mode '(ascii hiragana katakana katakana-半角 abbrev latin jisx0208-latin))
        (let ((result (nskk-state-set state 'mode mode)))
          (should (eq result mode))
          (should (eq (nskk-state-mode state) mode))))

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
      (should-error (nskk-state-set-henkan-phase state 'active))
      (should-error (nskk-state-set-henkan-phase state 'list))
      (should-error (nskk-state-set-henkan-phase state 'registration))
      (nskk-state-set-henkan-phase state 'on)
      (should (eq (nskk-state-henkan-phase state) 'on))
      (nskk-state-set-henkan-phase state 'active)
      (should (eq (nskk-state-henkan-phase state) 'active))))

  (nskk-it "force-henkan-phase bypasses transition validation"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-state-force-henkan-phase state 'active)
      (should (eq (nskk-state-henkan-phase state) 'active))
      (nskk-state-force-henkan-phase state 'list)
      (should (eq (nskk-state-henkan-phase state) 'list))
      (nskk-state-force-henkan-phase state nil)
      (should (null (nskk-state-henkan-phase state)))
      (should-error (nskk-state-force-henkan-phase state 'invalid-phase))))

  (nskk-it "same-phase transitions are allowed (no-op)"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-state-set-henkan-phase state nil)
      (should (null (nskk-state-henkan-phase state)))
      (nskk-state-set-henkan-phase state 'on)
      (nskk-state-set-henkan-phase state 'on)
      (should (eq (nskk-state-henkan-phase state) 'on)))))

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
        (should (equal (nskk-state-candidates state) '("a" "b" "c")))))))

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
      (should (string= (nskk-state-get-metadata state :key) "updated")))))

;;;
;;; nskk-state-set-okurigana / nskk-state-get-okurigana Tests
;;;

(nskk-describe "nskk-state-set-okurigana and nskk-state-get-okurigana"
  (nskk-it "nskk-state-get-okurigana returns nil before any set"
    (let ((state (nskk-state-create)))
      (should (null (nskk-state-get-okurigana state)))))

  (nskk-it "nskk-state-set-okurigana stores consonant for okuri-nashi lookup"
    (let ((state (nskk-state-create 'hiragana)))
      (nskk-state-set-okurigana state "k")
      (should (equal (nskk-state-get-okurigana state) "k"))
      (nskk-state-set-okurigana state "t")
      (should (equal (nskk-state-get-okurigana state) "t"))))

  (nskk-it "nskk-state-set-okurigana returns nil for nil state"
    (should (null (nskk-state-set-okurigana nil "k")))))

;;;
;;; CPS /k variants with nskk-it-k
;;;

(nskk-describe "CPS /k variants with nskk-it-k"
  (nskk-it-k "nskk-state-set/k sets mode and returns the new mode"
    (nskk-state-set/k (nskk-state-create 'ascii) 'mode 'katakana)
    :found (result)
      (should (eq result 'katakana))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-set/k"))

  (nskk-it "nskk-state-set/k treats nil as success for a present key"
    (let ((state (nskk-state-create))
          (found-calls 0)
          (not-found-calls 0)
          (result :not-called))
      (nskk-state-set/k
       state 'metadata nil
       (lambda (value)
         (setq found-calls (1+ found-calls)
               result value))
       (lambda ()
         (setq not-found-calls (1+ not-found-calls))))
      (should (= found-calls 1))
      (should (= not-found-calls 0))
      (should-not result)
      (should-not (nskk-state-metadata state))))

  (nskk-it "nskk-state-set/k calls only not-found for an unknown key"
    (let ((state (nskk-state-create))
          (unknown-key "nskk-state-test-unknown-key-8675309")
          (found-calls 0)
          (not-found-calls 0))
      (should-not (intern-soft unknown-key))
      (nskk-state-set state 'metadata '(:kept t))
      (nskk-state-set/k
       state unknown-key :ignored
       (lambda (_value)
         (setq found-calls (1+ found-calls)))
       (lambda ()
         (setq not-found-calls (1+ not-found-calls))))
      (should (= found-calls 0))
      (should (= not-found-calls 1))
      (should-not (intern-soft unknown-key))
      (should (equal (nskk-state-metadata state) '(:kept t)))))

  (nskk-it "nskk-state-set/k propagates validation errors without continuations"
    (let ((state (nskk-state-create 'ascii))
          (found-calls 0)
          (not-found-calls 0))
      (nskk-state-set state 'input-buffer "unchanged")
      (nskk-state-set state 'candidates '("a" "b"))
      (nskk-state-set state 'current-index 1)
      (let ((before (copy-nskk-state state)))
        (should-error
         (nskk-state-set/k
          state 'mode 'invalid-mode
          (lambda (_value)
            (setq found-calls (1+ found-calls)))
          (lambda ()
            (setq not-found-calls (1+ not-found-calls))))
         :type 'error)
        (should (= found-calls 0))
        (should (= not-found-calls 0))
        (should (equal state before)))))

  (nskk-it "nskk-state-set/k propagates henkan-phase validation errors without continuations"
    (let ((state (nskk-state-create 'ascii))
          (found-calls 0)
          (not-found-calls 0))
      (nskk-state-set state 'input-buffer "unchanged")
      (nskk-state-set state 'candidates '("a" "b"))
      (nskk-state-set state 'current-index 1)
      (let ((before (copy-nskk-state state)))
        (should-error
         (nskk-state-set/k
          state 'henkan-phase 'not-a-real-phase
          (lambda (_value)
            (setq found-calls (1+ found-calls)))
          (lambda ()
            (setq not-found-calls (1+ not-found-calls))))
         :type 'error)
        (should (= found-calls 0))
        (should (= not-found-calls 0))
        (should (equal state before)))))

  (nskk-it "nskk-state-set/k propagates error and quit from the selected continuation"
    (dolist (condition '(error quit))
      (dolist (row '((metadata nil found)
                     (definitely-unknown :ignored not-found)))
        (let* ((state (nskk-state-create))
               (key (nth 0 row))
               (value (nth 1 row))
               (selected (nth 2 row))
               (found-calls 0)
               (not-found-calls 0)
               (payload (list condition selected))
               caught)
          (setq caught
                (condition-case signaled
                    (progn
                      (nskk-state-set/k
                       state key value
                       (lambda (_result)
                         (setq found-calls (1+ found-calls))
                         (when (eq selected 'found)
                           (signal condition (list payload))))
                       (lambda ()
                         (setq not-found-calls (1+ not-found-calls))
                         (when (eq selected 'not-found)
                           (signal condition (list payload)))))
                      nil)
                  ((error quit) signaled)))
          (should (eq (car caught) condition))
          (should (eq (cadr caught) payload))
          (should (= found-calls (if (eq selected 'found) 1 0)))
          (should (= not-found-calls
                     (if (eq selected 'not-found) 1 0)))))))

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

  (nskk-it "nskk-state-get-metadata/k calls on-not-found for an invalid state"
    (let ((found-calls 0)
          (not-found-calls 0))
      (nskk-state-get-metadata/k
       nil :foo
       (lambda (_value) (setq found-calls (1+ found-calls)))
       (lambda () (setq not-found-calls (1+ not-found-calls))))
      (should (= found-calls 0))
      (should (= not-found-calls 1))))

  (nskk-it-k "nskk-state-get-okurigana/k returns stored okurigana"
    (nskk-state-get-okurigana/k
     (let ((state (nskk-state-create)))
       (nskk-state-set-okurigana state "k")
       state))
    :found (result)
      (should (equal result "k"))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-get-okurigana/k"))

  (nskk-it "nskk-state-get-okurigana/k calls on-found with nil when okurigana is unset"
    (let ((state (nskk-state-create))
          (found-calls 0)
          (not-found-calls 0)
          (result :not-called))
      (nskk-state-get-okurigana/k
       state
       (lambda (value)
         (setq found-calls (1+ found-calls)
               result value))
       (lambda () (setq not-found-calls (1+ not-found-calls))))
      (should (= found-calls 1))
      (should (= not-found-calls 0))
      (should-not result)))

  (nskk-it "nskk-state-get-okurigana/k calls on-not-found for an invalid state"
    (let ((found-calls 0)
          (not-found-calls 0))
      (nskk-state-get-okurigana/k
       nil
       (lambda (_value) (setq found-calls (1+ found-calls)))
       (lambda () (setq not-found-calls (1+ not-found-calls))))
      (should (= found-calls 0))
      (should (= not-found-calls 1))))

  (nskk-it-k "nskk-state-get-mode/k returns current-state mode"
    (nskk-state-test--get-mode-with-state/k 'katakana)
    :found (result)
      (should (eq result 'katakana))
    :not-found ()
      (ert-fail "Expected on-found for nskk-state-get-mode/k"))

  (nskk-it "nskk-state-get-mode/k calls on-not-found when no current state is active"
    (let ((nskk-current-state nil)
          (found-calls 0)
          (not-found-calls 0))
      (nskk-state-get-mode/k
       (lambda (_value) (setq found-calls (1+ found-calls)))
       (lambda () (setq not-found-calls (1+ not-found-calls))))
      (should (= found-calls 0))
      (should (= not-found-calls 1)))))

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
;;;

(defconst nskk-state-pbt--valid-modes
  '(ascii hiragana katakana katakana-半角 abbrev latin jisx0208-latin)
  "Valid modes for state property-based tests.")

(nskk-property-test state-pbt-mode-set-invariant
  ((input romaji-string))
  (let* ((mode (nth (random (length nskk-state-pbt--valid-modes))
                    nskk-state-pbt--valid-modes))
         (state (nskk-state-create)))
    (nskk-state-set state 'mode mode)
    (eq (nskk-state-mode state) mode))
  100)

(nskk-property-test state-pbt-created-state-is-valid
  ((input romaji-string))
  (let* ((mode (nth (random (length nskk-state-pbt--valid-modes))
                    nskk-state-pbt--valid-modes))
         (state (nskk-state-create mode)))
    (nskk-state-p state))
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
                 `(mode-properties nonexistent ,'\?s ,'\?f ,'\?h ,'\?c))))

  (nskk-it "returns nil for direct, which is an alias rather than a fact"
    (should-not (nskk-prolog-query-one
                 `(mode-properties direct ,'\?s ,'\?f ,'\?h ,'\?c)))))

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
        (nskk-state-set nskk-current-state 'input-buffer "a"))
      (should (string= (nskk-state-input-buffer nskk-current-state) "a")))))

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

  (nskk-it "is idempotent: a second call leaves initialization state intact"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (nskk-state-initialize-prolog)
      (should nskk--state-prolog-initialized)
      (should (nskk-prolog-query-one
               '(mode-properties hiragana \?s \?f \?h \?c))))))

;;;;
;;;; Sequence-Based State-Struct Tests (moved from integration layer)
;;;;

(nskk-describe "state sequence: deterministic replay"
  (nskk-it "should produce the same result for the same operation sequence"
    (let ((runs 75)
          (failures nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'deterministic-replay' seed: %d" test-seed)
      (dotimes (run runs)
        (let* ((ops (nskk-state-test--random-ops (nskk--pbt-random-int 5 30)))
               (result1 (nskk-state-test--apply-ops (nskk-state-create 'hiragana) ops))
               (result2 (nskk-state-test--apply-ops (nskk-state-create 'hiragana) ops)))
          (unless (nskk-state-test--states-equal-p result1 result2)
            (push (list :seed test-seed
                        :run run
                        :ops ops
                        :mode1 (nskk-state-mode result1)
                        :mode2 (nskk-state-mode result2)
                        :phase1 (nskk-state-henkan-phase result1)
                        :phase2 (nskk-state-henkan-phase result2))
                  failures))))
      (when failures
        (ert-fail (format "Determinism failed for %d cases (seed: %d):\n%S"
                          (length failures) test-seed
                          (take 3 failures)))))))

(nskk-describe "state sequence: mode switch idempotent toggle"
  (nskk-it "should return to original mode after toggling twice"
    (let ((runs 75)
          (failures nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'mode-switch-idempotent-toggle' seed: %d" test-seed)
      (dotimes (run runs)
        (let* ((initial-mode (nskk--pbt-random-choice '(hiragana katakana)))
               (state (nskk-state-create initial-mode)))
          (nskk-state-test--toggle-kana-mode state)
          (nskk-state-test--toggle-kana-mode state)
          (unless (eq (nskk-state-mode state) initial-mode)
            (push (list :seed test-seed
                        :run run
                        :initial-mode initial-mode
                        :final-mode (nskk-state-mode state))
                  failures))))
      (when failures
        (ert-fail (format "Mode switch idempotency failed for %d cases (seed: %d):\n%S"
                          (length failures) test-seed
                          (take 3 failures)))))))

(nskk-describe "state sequence: mode switch consistency"
  (nskk-it "should always produce the same result for the same mode transition"
    (let ((runs 75)
          (failures nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'mode-switch-consistent' seed: %d" test-seed)
      (dotimes (run runs)
        (let* ((initial-mode (nskk-generate 'valid-mode))
               (target-mode (nskk-generate 'valid-mode))
               (state1 (nskk-state-create initial-mode))
               (state2 (nskk-state-create initial-mode)))
          (nskk-state-set-mode state1 target-mode)
          (nskk-state-set-mode state2 target-mode)
          (unless (and (eq (nskk-state-mode state1) (nskk-state-mode state2))
                       (eq (nskk-state-previous-mode state1)
                           (nskk-state-previous-mode state2)))
            (push (list :seed test-seed
                        :run run
                        :initial-mode initial-mode
                        :target-mode target-mode
                        :mode1 (nskk-state-mode state1)
                        :mode2 (nskk-state-mode state2))
                  failures))))
      (when failures
        (ert-fail (format "Mode switch consistency failed for %d cases (seed: %d):\n%S"
                          (length failures) test-seed
                          (take 3 failures)))))))

(defconst nskk-state-test--expected-transitions
  '((nil . on)
    (on . active) (on . registration) (on . nil)
    (active . on) (active . nil) (active . list)
    (list . on) (list . nil) (list . registration)
    (registration . nil) (registration . list))
  "Phase transitions the henkan state machine is specified to permit.
Hand-authored from the phase-machine specification rather than read from
`nskk--valid-henkan-transitions', so that this oracle does not run through
the same table and predicate the implementation consults.  A divergence
between this list and the source table is exactly what these tests exist
to catch, so the duplication is deliberate.")

(nskk-describe "state sequence: phase-transition legality"
  (nskk-it "should never accept a transition absent from the specified set"
    (let ((runs 75)
          (failures nil)
          (test-seed (abs (random))))
      (random test-seed)
      (message "Sequence test 'phase-transition-legality' seed: %d" test-seed)
      (dotimes (run runs)
        (let* ((state (nskk-state-create 'hiragana))
               (steps (nskk--pbt-random-int 20 60)))
          (dotimes (_ steps)
            (let* ((before (nskk-state-henkan-phase state))
                   (target (nskk--pbt-random-choice nskk--valid-henkan-phases))
                   (legal (or (eq before target)
                              (member (cons before target)
                                      nskk-state-test--expected-transitions)))
                   (errored nil))
              (condition-case nil
                  (nskk-state-set-henkan-phase state target)
                (error (setq errored t)))
              (cond
               ((and legal errored)
                (push (list :seed test-seed :run run :before before :target target
                            :reason "legal transition was rejected")
                      failures))
               ((and (not legal) (not errored))
                (push (list :seed test-seed :run run :before before :target target
                            :reason "illegal transition was accepted")
                      failures))
               (legal
                (unless (eq (nskk-state-henkan-phase state) target)
                  (push (list :seed test-seed :run run :before before :target target
                              :reason "phase not updated after legal transition")
                        failures)))
               (t
                (unless (eq (nskk-state-henkan-phase state) before)
                  (push (list :seed test-seed :run run :before before :target target
                              :reason "phase mutated after rejected transition")
                        failures))))))))
      (when failures
        (ert-fail (format "Phase-transition legality failed for %d cases (seed: %d):\n%S"
                          (length failures) test-seed
                          (take 5 failures)))))))


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
        (should (null ov)))))

  (nskk-it "the overlay object is no longer live after deletion"
    (with-temp-buffer
      (let* ((ov (make-overlay (point-min) (point-max)))
             (ov-copy ov))
        (nskk-delete-overlay ov)
        (should (null (overlay-buffer ov-copy))))))

  (nskk-it "repairs a non-overlay drift value to nil"
    (let ((ov 'stale-overlay-reference))
      (nskk-delete-overlay ov)
      (should (null ov))))

  (nskk-it "clears the variable before re-signaling delete error or quit"
    (dolist (condition '(error quit))
      (with-temp-buffer
        (let* ((ov (make-overlay (point-min) (point-max)))
               (saved-overlay ov)
               (payload (list condition))
               (original-delete (symbol-function 'delete-overlay))
               caught)
          (unwind-protect
              (cl-letf (((symbol-function 'delete-overlay)
                         (lambda (_overlay)
                           (signal condition (list payload)))))
                (setq caught
                      (condition-case signaled
                          (progn
                            (nskk-delete-overlay ov)
                            nil)
                        ((error quit) signaled)))
                (should (eq (car caught) condition))
                (should (eq (cadr caught) payload))
                (should (null ov)))
            (funcall original-delete saved-overlay)))))))

;;;
;;; Buffer-Local Getter/Setter Generator Macro Tests
;;;

(nskk-describe "nskk-define-buffer-local-getter and nskk-define-buffer-local-setter"
  (nskk-it "round-trips a value through the generated accessor pair and returns it"
    (with-temp-buffer
      (should (= (nskk-state-set-henkan-count 3) 3))
      (should (= (nskk-state-henkan-count) 3))
      (should (= (nskk-state-set-henkan-count 7) 7))
      (should (= (nskk-state-henkan-count) 7))))

  (nskk-it "keeps the value buffer-local across two buffers"
    (let ((buf1 (generate-new-buffer "nskk-state-test-buf1"))
          (buf2 (generate-new-buffer "nskk-state-test-buf2")))
      (unwind-protect
          (progn
            (with-current-buffer buf1
              (nskk-state-set-henkan-count 5))
            (with-current-buffer buf2
              (nskk-state-set-henkan-count 9))
            (should (= (with-current-buffer buf1 (nskk-state-henkan-count)) 5))
            (should (= (with-current-buffer buf2 (nskk-state-henkan-count)) 9)))
        (kill-buffer buf1)
        (kill-buffer buf2)))))

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
  (nskk-it "every mode-properties/5 fact is in the static set"
    (let ((prolog-modes
           (nskk-prolog-query-all-values '(mode-properties \?m \?d \?f \?h \?c) '\?m)))
      (should prolog-modes)
      (dolist (m prolog-modes)
        (should (memq m nskk--valid-modes)))))

  (nskk-it "every static-set mode has a mode-properties/5 fact"
    (dolist (m nskk--valid-modes)
      (should (nskk-prolog-query-one
               `(mode-properties ,m ,'\?d ,'\?f ,'\?h ,'\?c)))))

  (nskk-it "count matches between mode-properties/5 facts and static set"
    (let ((prolog-modes
           (nskk-prolog-query-all-values '(mode-properties \?m \?d \?f \?h \?c) '\?m)))
      (should (= (length prolog-modes) (length nskk--valid-modes))))))

(nskk-describe "nskk--valid-henkan-transitions cache invariant"
  (nskk-it "every table entry is accepted by nskk--henkan-transition-valid-p"
    (dolist (pair nskk--valid-henkan-transitions)
      (should (nskk--henkan-transition-valid-p (car pair) (cdr pair)))))

  (nskk-deftest-table state-henkan-transition-rejects-absent-pairs
    :description "Pairs absent from nskk--valid-henkan-transitions are rejected"
    :columns (from to)
    :rows ((nil active) (nil list) (nil registration)
           (active registration) (list active)
           (registration on) (registration active))
    :body (should-not (nskk--henkan-transition-valid-p from to))))

(provide 'nskk-state-test)

;;; nskk-state-test.el ends here
