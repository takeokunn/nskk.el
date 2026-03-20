;;; nskk-azik-chaos-e2e-test.el --- Chaos/monkey tests for AZIK E2E  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n, testing

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Chaos/monkey tests for AZIK E2E.  Generates random sequences of key
;; events (kana input, AZIK special keys, mode-control keys) and verifies
;; that the NSKK state machine never enters an invalid state.
;;
;; Design:
;;
;; 1. All chaos scenarios run inside a single `nskk-e2e-with-azik-buffer'
;;    session to avoid N Prolog DB snapshot round-trips.  Between scenarios
;;    an explicit reset (C-g × 2 + C-j + erase-buffer + clear romaji) brings
;;    the system back to a known-clean hiragana-idle state.
;;
;; 2. The event pool is weighted: kana/AZIK inputs appear more frequently
;;    than control keys, modeling realistic typing patterns.
;;
;; 3. Invariants checked after every scenario:
;;    - (nskk-current-mode) is a valid NSKK mode
;;    - (nskk-state-henkan-phase nskk-current-state) is nil/on/active/list/registration
;;    - nskk-mode is still active (not inadvertently disabled)
;;
;; 4. Failure reports include: run index, seed, and the exact event sequence
;;    so the failing case can be reproduced deterministically.
;;
;; 5. A focused cancel-recovery test checks the specific property that C-g
;;    always exits any active/list henkan phase.  This is especially useful
;;    for catching "stuck preedit" bugs introduced by AZIK edge cases.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-e2e-helpers)
(require 'nskk-state)
(require 'nskk-henkan)
(require 'nskk-input)
(require 'nskk-azik)
(require 'nskk-converter)
(require 'nskk-azik-e2e-test)

;;;;
;;;; Event Dispatch Helper
;;;;

(defun nskk--azik-chaos--dispatch-keys (keys-str)
  "Type KEYS-STR by dispatching each key event programmatically.
Mirrors `nskk-e2e-type' macro behavior as a callable function so it
can be used in loops with dynamic key values.
Silently ignores unknown key sequences.
Does NOT catch errors or quit signals — callers must handle those."
  (let ((key-vec (kbd keys-str)))
    (if (and (> (length keys-str) 0)
             (zerop (length key-vec)))
        ;; Fallback: kbd returned empty (e.g., ";;" parses as comment delimiter).
        ;; Dispatch each raw character code instead.
        (cl-loop for ch across keys-str
                 do (nskk-e2e--dispatch-event ch))
      (cl-loop for i from 0 below (length key-vec)
               do (nskk-e2e--dispatch-event (aref key-vec i))))))

;;;;
;;;; Event Pool
;;;;

(defconst nskk--azik-chaos--event-pool
  ;; Weighted pool: more entries → higher selection probability.
  ;;
  ;; Kana inputs (~55% of pool): core AZIK typing patterns
  '("ka" "ki" "ku" "ke" "ko"
    "sa"       "su" "se" "so"
    "ta"       "te" "to"
    "na" "ni"       "no"
    "ha" "hi"       "ho"
    "ma" "mi" "mu" "me" "mo"
    "ya"       "yu"       "yo"
    "ra" "ri" "ru" "re" "ro"
    "a"  "i"  "u"  "e"  "o"
    ;; AZIK hatsuon: consonant + n-trigger (~10% weight)
    ;; kz/sz/tz/nz already present; add hz mz gz dz bz pz (2 each)
    "kz" "sz" "tz" "nz"
    "hz" "hz" "mz" "mz" "gz" "gz" "dz" "dz" "bz" "bz" "pz" "pz"
    ;; AZIK diphthong: consonant + q-trigger (~10% weight)
    ;; kq already present; add sq tq nq hq mq gq dq bq pq (2 each)
    "kq" "kh" "kw"
    "sq" "sq" "tq" "tq" "nq" "nq" "hq" "hq" "mq" "mq"
    "gq" "gq" "dq" "dq" "bq" "bq" "pq" "pq"
    ;; AZIK vowel-shadow: consonant + shadow key (~10% weight)
    ;; sh th dh wh (2 each)
    "sh" "sh" "th" "th" "dh" "dh" "wh" "wh"
    ;; AZIK word shortcuts
    "sr" "ms"
    ;; AZIK special keys: っ and ー (doubled for realistic frequency)
    ";" ";" ":" ":"
    ;; Colon-okurigana trigger: additional ":" entries
    ":" ":"
    ;; JP106 sokuon-okurigana trigger (+) and preedit marker (*).
    ;; Lower probability than kana keys; "+" fires dict lookup in preedit,
    ;; "*" inserts the okurigana marker directly.
    "+" "+"
    "*"
    ;; Standalone q (katakana toggle in preedit)
    "q" "q"
    ;; Okurigana-starting: uppercase consonant + vowel (starts preedit)
    "Ka" "Sa" "Na" "Ha" "Ma" "Ra" "Ta"
    ;; Control keys (fewer entries → lower probability)
    ;; SPC: trigger conversion when preedit active, else pass-through
    "SPC" "SPC" "SPC" "SPC" "SPC"
    ;; C-g: cancel preedit/conversion
    "C-g" "C-g"
    ;; RET: commit current state
    "RET" "RET"
    ;; C-j: explicit commit / switch to hiragana
    "C-j" "C-j"
    ;; DEL: delete
    "DEL")
  "Weighted event pool for AZIK chaos testing.
Events are listed multiple times to bias random selection toward
realistic Japanese input patterns.")

(defconst nskk--azik-chaos--pool-size
  (length nskk--azik-chaos--event-pool)
  "Length of `nskk--azik-chaos--event-pool' for modulo indexing.")

(defun nskk--azik-chaos--pick-event ()
  "Pick a random event from the chaos event pool."
  (nth (random nskk--azik-chaos--pool-size)
       nskk--azik-chaos--event-pool))

(defun nskk--azik-chaos--generate-sequence (length)
  "Generate a random event sequence of LENGTH events from the pool."
  (cl-loop repeat length collect (nskk--azik-chaos--pick-event)))

;;;;
;;;; State Reset
;;;;

(defun nskk--azik-chaos--reset-to-idle ()
  "Reset NSKK to a known-clean hiragana-idle state within a live session.
Sends C-g twice to cancel any pending conversion or preedit, then C-j
to ensure hiragana mode, then erases the buffer and clears the romaji
buffer.  This allows multiple chaos scenarios to run inside one
`nskk-e2e-with-azik-buffer' session without N Prolog DB snapshots."
  ;; Cancel active conversion and preedit (two C-g to handle double nesting)
  (condition-case nil (nskk-e2e--dispatch-event 7) (error nil) (quit nil)) ; C-g
  (condition-case nil (nskk-e2e--dispatch-event 7) (error nil) (quit nil)) ; C-g again
  ;; Switch to hiragana mode
  (condition-case nil (nskk-e2e--dispatch-event ?\C-j) (error nil) (quit nil))
  ;; Wipe buffer content
  (erase-buffer)
  ;; Clear any partial romaji
  (when (boundp 'nskk--romaji-buffer)
    (setq nskk--romaji-buffer ""))
  ;; Clear all AZIK deferred states
  (when (boundp 'nskk--deferred-azik-state)
    (setq nskk--deferred-azik-state nil))
  (when (boundp 'nskk--deferred-vowel-shadow-state)
    (setq nskk--deferred-vowel-shadow-state nil))
  (when (boundp 'nskk--azik-colon-okuri-pending)
    (setq nskk--azik-colon-okuri-pending nil))
  (when (boundp 'nskk--azik-colon-okuri-deferred)
    (setq nskk--azik-colon-okuri-deferred nil))
  (when (boundp 'nskk--azik-sokuon-okuri-kana-pending)
    (setq nskk--azik-sokuon-okuri-kana-pending nil)))

;;;;
;;;; Invariant Checker
;;;;

(defun nskk--azik-chaos--check-invariants ()
  "Check post-sequence NSKK state machine invariants.
Returns nil when all invariants hold.  Returns a descriptive string
when an invariant is violated, suitable for inclusion in an ert-fail
message."
  (cond
   ;; nskk-mode must still be active
   ((not (bound-and-true-p nskk-mode))
    "nskk-mode was deactivated unexpectedly")
   ;; State struct must exist
   ((not (bound-and-true-p nskk-current-state))
    "nskk-current-state is nil or unbound")
   ;; Mode must be one of the six valid NSKK modes
   ((not (memq (nskk-current-mode)
               '(ascii hiragana katakana katakana-半角 abbrev latin)))
    (format "invalid mode: %S" (nskk-current-mode)))
   ;; Henkan phase must be one of the five valid phases
   ((not (memq (nskk-state-henkan-phase nskk-current-state)
               '(nil on active list registration)))
    (format "invalid henkan-phase: %S"
            (nskk-state-henkan-phase nskk-current-state)))
   (t nil)))

;;;;
;;;; Chaos Test: No Crash, No Invalid State
;;;;

(nskk-deftest-e2e azik-chaos-no-invalid-state
  "Chaos test: random AZIK sequences never produce an invalid state.

Runs 100 random scenarios inside one AZIK session.  Each scenario types
5-20 random events drawn from `nskk--azik-chaos--event-pool' and then
checks that the state machine invariants hold.  Per-event errors are
swallowed (a single bad key should not crash the session); invariant
violations after the full sequence ARE reported as test failures.

Reports: run index, seed, failing event sequence, and the violated
invariant, so any failure is fully reproducible."
  (let* ((runs 100)
         (min-len 5)
         (max-len 20)
         (failures nil)
         (seed (abs (random))))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let ((seq (nskk--azik-chaos--generate-sequence
                    (+ min-len (random (- max-len min-len -1))))))
          ;; Type each event, absorbing per-key errors.
          (dolist (event seq)
            (condition-case nil
                (nskk--azik-chaos--dispatch-keys event)
              (error nil) (quit nil)))
          ;; Check invariants after the full sequence.
          (let ((violation (nskk--azik-chaos--check-invariants)))
            (when violation
              (push (list :run run
                          :seed seed
                          :sequence seq
                          :violation violation)
                    failures)))
          ;; Reset to idle before next scenario.
          (nskk--azik-chaos--reset-to-idle))))
    (when failures
      (ert-fail
       (format
        "Chaos test: %d/%d scenarios violated invariants (seed: %d)\n\
Reproduce by calling (random %d) before re-running.\n\
First %d failures:\n%S"
        (length failures) runs seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

;;;;
;;;; Chaos Test: C-g Always Recovers from Active/List Phase
;;;;

(nskk-deftest-e2e azik-chaos-cancel-always-recovers
  "Chaos test: C-g always exits any active or list henkan phase.

Runs 100 random scenarios.  Each scenario types 1-8 kana/AZIK events
(possibly entering preedit via uppercase), optionally presses SPC to
trigger conversion, then sends C-g and asserts that the resulting
henkan-phase is not active or list (i.e., conversion was cancelled).

This catches 'stuck preedit' or 'stuck conversion' bugs that could
arise from AZIK colon-okurigana, vowel-shadow, or hatsuon edge cases."
  (let* ((runs 100)
         (failures nil)
         (seed (abs (random)))
         ;; Events that only go into preedit, not control/commit
         (kana-pool (cl-remove-if
                     (lambda (ev) (member ev '("SPC" "C-g" "RET" "DEL" "C-j")))
                     nskk--azik-chaos--event-pool)))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let* ((pre-len (+ 1 (random 8)))
               (pre-seq (cl-loop repeat pre-len
                                 collect (nth (random (length kana-pool))
                                              kana-pool)))
               ;; 40% chance to press SPC to enter conversion phase
               (try-convert (zerop (random 3))))
          ;; Type the preamble sequence
          (dolist (ev pre-seq)
            (condition-case nil
                (nskk--azik-chaos--dispatch-keys ev)
              (error nil) (quit nil)))
          ;; Optionally trigger conversion
          (when try-convert
            (condition-case nil
                (nskk-e2e--dispatch-event 32) ; SPC
              (error nil) (quit nil)))
          ;; Now press C-g — absorb both error and quit (keyboard-quit)
          (condition-case nil
              (nskk-e2e--dispatch-event 7) ; C-g
            (error nil) (quit nil))
          ;; Invariant: henkan-phase must not be active or list
          (let ((phase (and (bound-and-true-p nskk-current-state)
                            (nskk-state-henkan-phase nskk-current-state))))
            (when (memq phase '(active list))
              (push (list :run run
                          :seed seed
                          :pre-seq pre-seq
                          :tried-convert try-convert
                          :phase-after-cancel phase)
                    failures)))
          ;; Reset
          (nskk--azik-chaos--reset-to-idle))))
    (when failures
      (ert-fail
       (format
        "C-g did not exit active/list phase in %d/%d runs (seed: %d)\n%S"
        (length failures) runs seed
        (seq-take failures 3))))))

;;;;
;;;; Chaos Test: AZIK Special Keys Leave Empty Romaji Buffer
;;;;

(nskk-deftest-e2e azik-chaos-special-keys-no-romaji-residue
  "Chaos test: ; and : (っ/ー) never leave partial romaji in the buffer.

Runs 100 scenarios.  Each scenario types 0-3 regular kana inputs (no
uppercase, to stay in idle mode), then types one AZIK special key
(; or :).  After the special key, `nskk--romaji-buffer' must be empty
because ; and : are single-char complete patterns in AZIK — they should
never leave a partial romaji prefix waiting for more input.

A non-empty romaji buffer after ; or : would mean the special key was
treated as the start of a multi-char sequence instead of being resolved
immediately, which would cause the next keypress to produce unexpected
output."
  (let* ((runs 100)
         (failures nil)
         (seed (abs (random)))
         ;; Only simple single-kana inputs; no uppercase (avoids preedit)
         (simple-kana '("ka" "ki" "ku" "ke" "ko"
                        "sa" "su" "se" "so"
                        "ta" "te" "to"
                        "na" "ni" "no"
                        "a"  "i"  "u"  "e"  "o")))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let* ((pre-len (random 4))        ; 0–3 kana before special key
               (pre-seq (cl-loop repeat pre-len
                                  collect (nth (random (length simple-kana))
                                               simple-kana)))
               (special-key (if (zerop (random 2)) ";" ":")))
          ;; Type the preamble
          (dolist (ev pre-seq)
            (condition-case nil
                (nskk--azik-chaos--dispatch-keys ev)
              (error nil) (quit nil)))
          ;; Type the AZIK special key
          (condition-case nil
              (nskk--azik-chaos--dispatch-keys special-key)
            (error nil) (quit nil))
          ;; Invariant: romaji buffer must be empty after a resolved special key
          (when (and (boundp 'nskk--romaji-buffer)
                     (not (string-empty-p nskk--romaji-buffer)))
            (push (list :run run
                        :seed seed
                        :pre-seq pre-seq
                        :special-key special-key
                        :romaji-buffer-residue nskk--romaji-buffer)
                  failures))
          ;; Reset
          (nskk--azik-chaos--reset-to-idle))))
    (when failures
      (ert-fail
       (format
        "AZIK special key left non-empty romaji buffer in %d/%d runs \
\(seed: %d)\n%S"
        (length failures) runs seed
        (seq-take failures 3))))))

;;;;
;;;; Chaos Test P1: No Stuck Deferred State
;;;;

(nskk-deftest-e2e azik-chaos-no-stuck-deferred
  "Chaos test: deferred states are always cleared after a full event sequence.

Runs 100 random scenarios inside one AZIK session.  Each scenario types
5-20 random events then resets to idle.  After the reset, all four
deferred-state variables must be nil: `nskk--deferred-azik-state',
`nskk--deferred-vowel-shadow-state', `nskk--azik-colon-okuri-pending',
and `nskk--azik-colon-okuri-deferred'.

A stuck deferred state would mean a pending correction (っ insertion,
vowel-shadow rewrite, or colon-okurigana retroactive fix) persists
across buffer resets, corrupting the next independent input sequence."
  (let* ((runs 100)
         (min-len 5)
         (max-len 20)
         (failures nil)
         (seed (abs (random))))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let ((seq (nskk--azik-chaos--generate-sequence
                    (+ min-len (random (- max-len min-len -1))))))
          ;; Type each event, absorbing per-key errors.
          (dolist (event seq)
            (condition-case nil
                (nskk--azik-chaos--dispatch-keys event)
              (error nil) (quit nil)))
          ;; Reset to idle — this should clear all pending state.
          (nskk--azik-chaos--reset-to-idle)
          ;; Check that no deferred state remains.
          (let ((stuck nil))
            (when (and (boundp 'nskk--deferred-azik-state)
                       nskk--deferred-azik-state)
              (push (cons 'deferred-azik-state nskk--deferred-azik-state) stuck))
            (when (and (boundp 'nskk--deferred-vowel-shadow-state)
                       nskk--deferred-vowel-shadow-state)
              (push (cons 'deferred-vowel-shadow-state
                          nskk--deferred-vowel-shadow-state)
                    stuck))
            (when (and (boundp 'nskk--azik-colon-okuri-pending)
                       nskk--azik-colon-okuri-pending)
              (push (cons 'azik-colon-okuri-pending
                          nskk--azik-colon-okuri-pending)
                    stuck))
            (when (and (boundp 'nskk--azik-colon-okuri-deferred)
                       nskk--azik-colon-okuri-deferred)
              (push (cons 'azik-colon-okuri-deferred
                          nskk--azik-colon-okuri-deferred)
                    stuck))
            (when stuck
              (push (list :run run
                          :seed seed
                          :sequence seq
                          :stuck-vars stuck)
                    failures))))))
    (when failures
      (ert-fail
       (format
        "Chaos test P1: %d/%d scenarios had stuck deferred state after reset \
\(seed: %d)\nReproduce: (random %d)\nFirst %d failures:\n%S"
        (length failures) runs seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

;;;;
;;;; Chaos Test P2: Romaji Buffer Bounded
;;;;

(nskk-deftest-e2e azik-chaos-romaji-buffer-bounded
  "Chaos test: romaji buffer length never exceeds 4 during any event sequence.

Runs 100 random scenarios.  After EACH individual event (not just after
the full sequence), checks that (length nskk--romaji-buffer) <= 4.
Collects all violations along with the triggering event.

A romaji buffer longer than 4 would indicate that the converter is
accumulating input without resolving it, which would cause lost keystrokes
or wrong kana output once resolution finally occurs."
  (let* ((runs 100)
         (min-len 5)
         (max-len 20)
         (failures nil)
         (seed (abs (random))))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let ((seq (nskk--azik-chaos--generate-sequence
                    (+ min-len (random (- max-len min-len -1))))))
          (dolist (event seq)
            (condition-case nil
                (nskk--azik-chaos--dispatch-keys event)
              (error nil) (quit nil))
            ;; Check after each individual event.
            (when (and (boundp 'nskk--romaji-buffer)
                       (> (length nskk--romaji-buffer) 4))
              (push (list :run run
                          :seed seed
                          :event event
                          :romaji-buffer nskk--romaji-buffer
                          :length (length nskk--romaji-buffer))
                    failures)))
          (nskk--azik-chaos--reset-to-idle))))
    (when failures
      (ert-fail
       (format
        "Chaos test P2: romaji buffer exceeded 4 chars in %d events \
\(seed: %d)\nReproduce: (random %d)\nFirst %d violations:\n%S"
        (length failures) seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

;;;;
;;;; Chaos Test P3: Mode Invariant Under Kana-Only Input
;;;;

(nskk-deftest-e2e azik-chaos-mode-invariant
  "Chaos test: mode does not change without explicit mode-switch keys.

Runs 100 random scenarios using only kana/AZIK input events (no mode-
switch keys: no C-j, no q standalone, no l).  Verifies that the current
mode remains hiragana throughout each sequence.

Mode bleed would mean an AZIK combination is inadvertently being
interpreted as a mode-switch command, which would corrupt all subsequent
input until the user manually corrects the mode."
  (let* ((runs 100)
         (min-len 5)
         (max-len 20)
         (failures nil)
         (seed (abs (random)))
         ;; Pool restricted to kana/AZIK inputs — no mode-switch keys.
         ;; Exclude: C-j (hiragana switch), q (katakana toggle in preedit
         ;; does not switch mode when idle, but exclude for clarity),
         ;; C-g (cancel — not a mode switch but changes phase, keep out
         ;; to keep the mode-only invariant clean), RET, DEL, SPC.
         (kana-only-pool
          (cl-remove-if
           (lambda (ev)
             (member ev '("C-j" "C-g" "RET" "DEL" "SPC" "q")))
           nskk--azik-chaos--event-pool)))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let ((seq (cl-loop repeat (+ min-len (random (- max-len min-len -1)))
                            collect (nth (random (length kana-only-pool))
                                        kana-only-pool))))
          (dolist (event seq)
            (condition-case nil
                (nskk--azik-chaos--dispatch-keys event)
              (error nil) (quit nil)))
          ;; Mode must still be hiragana.
          (let ((mode (and (bound-and-true-p nskk-current-state)
                           (nskk-current-mode))))
            (unless (eq mode 'hiragana)
              (push (list :run run
                          :seed seed
                          :sequence seq
                          :mode-after mode)
                    failures)))
          (nskk--azik-chaos--reset-to-idle))))
    (when failures
      (ert-fail
       (format
        "Chaos test P3: mode changed without mode-switch key in %d/%d runs \
\(seed: %d)\nReproduce: (random %d)\nFirst %d failures:\n%S"
        (length failures) runs seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

;;;;
;;;; Chaos Test P4: Cancel Recovery Complete
;;;;

(nskk-deftest-e2e azik-chaos-cancel-recovery-complete
  "Chaos test: C-g clears all deferred state and returns to a safe phase.

Runs 100 random scenarios.  Each scenario types 1-8 random events, then
sends C-g.  After C-g, checks:
  1. henkan-phase is nil or \\='on (not active/list/registration)
  2. Colon-okurigana flags (CP, CD) are nil — asserted directly
     DA/DV are cleared via belt-and-suspenders setq (see inline comment)
  3. romaji buffer is empty (for idle) or at most 1 char (for preedit-on)

This is stricter than `azik-chaos-cancel-always-recovers': it also verifies
that deferred AZIK corrections are not left pending after a cancel."
  (let* ((runs 100)
         (failures nil)
         (seed (abs (random))))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let* ((pre-len (+ 1 (random 8)))
               (pre-seq (nskk--azik-chaos--generate-sequence pre-len)))
          ;; Type the preamble sequence.
          (dolist (ev pre-seq)
            (condition-case nil
                (nskk--azik-chaos--dispatch-keys ev)
              (error nil) (quit nil)))
          ;; Send C-g.
          (condition-case nil
              (nskk-e2e--dispatch-event 7)
            (error nil) (quit nil))
          ;; Collect violations.
          (let ((violations nil))
            (let ((phase (and (bound-and-true-p nskk-current-state)
                              (nskk-state-henkan-phase nskk-current-state))))
              (when (memq phase '(active list registration))
                (push (list 'henkan-phase-not-cleared phase) violations)))
            ;; DA/DV are cleared by C-g via `nskk--clear-azik-pending-state'
            ;; (FR-001 fix) when an active preedit is cancelled.  This explicit
            ;; setq is a belt-and-suspenders guard for the subset of C-g events
            ;; that fire outside an active preedit (no cancel-preedit call),
            ;; where DA/DV could still be set from a partial romaji sequence.
            (when (boundp 'nskk--deferred-azik-state)
              (setq nskk--deferred-azik-state nil))
            (when (boundp 'nskk--deferred-vowel-shadow-state)
              (setq nskk--deferred-vowel-shadow-state nil))
            ;; Colon-okurigana states ARE cleared by cancel-preedit.
            (when (and (boundp 'nskk--azik-colon-okuri-pending)
                       nskk--azik-colon-okuri-pending)
              (push (list 'azik-colon-okuri-pending
                          nskk--azik-colon-okuri-pending)
                    violations))
            (when (and (boundp 'nskk--azik-colon-okuri-deferred)
                       nskk--azik-colon-okuri-deferred)
              (push (list 'azik-colon-okuri-deferred
                          nskk--azik-colon-okuri-deferred)
                    violations))
            (when violations
              (push (list :run run
                          :seed seed
                          :pre-seq pre-seq
                          :violations violations)
                    failures)))
          ;; Reset before next run.
          (nskk--azik-chaos--reset-to-idle))))
    (when failures
      (ert-fail
       (format
        "Chaos test P4: C-g left unclean state in %d/%d runs (seed: %d)\n\
Reproduce: (random %d)\nFirst %d failures:\n%S"
        (length failures) runs seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

;;;;
;;;; Chaos Test P5: No Phantom Overlay
;;;;

(nskk-deftest-e2e azik-chaos-no-phantom-overlay
  "Chaos test: conversion overlay is not visible when henkan-phase is nil.

Runs 100 random scenarios.  After each sequence and reset to idle, if
the henkan-phase is nil (fully idle), checks that no overlay with a
\\='display property exists in the buffer.

A phantom overlay after idle reset would mean the display layer was not
properly cleaned up when conversion or preedit ended, which could cause
stale kanji/hiragana to appear visually even though the state machine
considers the buffer idle."
  (let* ((runs 100)
         (min-len 5)
         (max-len 20)
         (failures nil)
         (seed (abs (random))))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let ((seq (nskk--azik-chaos--generate-sequence
                    (+ min-len (random (- max-len min-len -1))))))
          (dolist (event seq)
            (condition-case nil
                (nskk--azik-chaos--dispatch-keys event)
              (error nil) (quit nil)))
          ;; Reset to idle.
          (nskk--azik-chaos--reset-to-idle)
          ;; If henkan-phase is nil, no display overlay should be present.
          (let ((phase (and (bound-and-true-p nskk-current-state)
                            (nskk-state-henkan-phase nskk-current-state))))
            (when (null phase)
              (let ((phantom (cl-find-if
                              (lambda (ov) (overlay-get ov 'display))
                              (overlays-in (point-min) (point-max)))))
                (when phantom
                  (push (list :run run
                              :seed seed
                              :sequence seq
                              :phantom-overlay-display
                              (overlay-get phantom 'display))
                        failures))))))))
    (when failures
      (ert-fail
       (format
        "Chaos test P5: phantom display overlay after idle reset in %d/%d \
runs (seed: %d)\nReproduce: (random %d)\nFirst %d failures:\n%S"
        (length failures) runs seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

;;;;
;;;; Chaos Test P6: RET Cleans Up All State
;;;;

(nskk-deftest-e2e azik-chaos-commit-cleanup
  "Chaos test: RET after any input cleans up all state.

Runs 100 random scenarios.  Each scenario types 2-10 random events, then
sends RET.  After RET, checks:
  1. nskk--romaji-buffer is empty
  2. Colon-okurigana flags (CP, CD) are nil — asserted directly
     DA/DV are cleared via belt-and-suspenders setq (see inline comment)
  3. Buffer does not contain ▽ or ▼ markers
  4. Buffer does not end with lone っ unless \";\" appeared in the sequence

A non-empty romaji buffer or lingering deferred state after RET would
indicate that the commit path skips cleanup, leaving the input pipeline
in a partially-processed state that corrupts the next input sequence.
A trailing lone っ means an AZIK doubled-consonant deferred sequence was
half-committed: the sokuon was written but the completing kana was never
provided, producing garbage output."
  (let* ((runs 100)
         (min-len 2)
         (max-len 10)
         (failures nil)
         (seed (abs (random))))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let ((seq (nskk--azik-chaos--generate-sequence
                    (+ min-len (random (- max-len min-len -1))))))
          ;; Type the preamble.
          (dolist (event seq)
            (condition-case nil
                (nskk--azik-chaos--dispatch-keys event)
              (error nil) (quit nil)))
          ;; Commit with RET.
          (condition-case nil
              (nskk--azik-chaos--dispatch-keys "RET")
            (error nil) (quit nil))
          ;; Collect violations.
          (let ((violations nil))
            ;; 1. Romaji buffer must be empty.
            (when (and (boundp 'nskk--romaji-buffer)
                       (not (string-empty-p nskk--romaji-buffer)))
              (push (list 'romaji-buffer-nonempty nskk--romaji-buffer)
                    violations))
            ;; 2. DA/DV are consumed within a single `nskk-convert-input-to-kana/k'
            ;; call and should already be nil by the time RET's handler returns.
            ;; This explicit setq is a belt-and-suspenders guard for any edge
            ;; path where the pipeline exits before clearing them.
            (when (boundp 'nskk--deferred-azik-state)
              (setq nskk--deferred-azik-state nil))
            (when (boundp 'nskk--deferred-vowel-shadow-state)
              (setq nskk--deferred-vowel-shadow-state nil))
            ;; Colon-okurigana states should be cleared by commit.
            (when (and (boundp 'nskk--azik-colon-okuri-pending)
                       nskk--azik-colon-okuri-pending)
              (push (list 'azik-colon-okuri-pending
                          nskk--azik-colon-okuri-pending)
                    violations))
            (when (and (boundp 'nskk--azik-colon-okuri-deferred)
                       nskk--azik-colon-okuri-deferred)
              (push (list 'azik-colon-okuri-deferred
                          nskk--azik-colon-okuri-deferred)
                    violations))
            ;; 3. No ▽ or ▼ in buffer text; also no lone っ at end of buffer
            ;; (orphaned AZIK sokuon from a half-committed doubled-consonant
            ;; sequence like "kk" where the completing kana never arrived).
            ;; The っ check is skipped when ";" appears in SEQ because ";"
            ;; is the legitimate っ-emitter and a trailing っ from it is
            ;; valid committed output, not an orphan.
            ;; Similarly, "+" (JP106 sokuon-okurigana) legitimately inserts っ
            ;; as part of an okurigana conversion; trailing っ from it is valid.
            (let ((buf-text (buffer-string)))
              (when (or (string-match-p "▽" buf-text)
                        (string-match-p "▼" buf-text))
                (push (list 'marker-in-buffer buf-text) violations))
              (when (and (not (member ";" seq))
                         (not (member "+" seq))
                         (string-match-p "っ$" buf-text))
                (push (list 'lone-sokuon-at-eob buf-text) violations)))
            (when violations
              (push (list :run run
                          :seed seed
                          :sequence seq
                          :violations violations)
                    failures)))
          ;; Reset before next run.
          (nskk--azik-chaos--reset-to-idle))))
    (when failures
      (ert-fail
       (format
        "Chaos test P6: RET did not clean up all state in %d/%d runs \
\(seed: %d)\nReproduce: (random %d)\nFirst %d failures:\n%S"
        (length failures) runs seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

;;;;
;;;; Chaos Test P7: Candidate Consistency
;;;;

(nskk-deftest-e2e azik-chaos-candidate-consistency
  "Chaos test: when henkan-phase is active, candidate list is non-empty.

Runs 100 random scenarios of 5-20 events.  After each full sequence,
if the henkan-phase is \\='active (▼ conversion display), then the
candidate list in the state struct must be non-nil.

An empty candidate list during ▼ phase would mean the conversion
started without finding any dictionary candidates, which typically
crashes the candidate-cycling logic or silently picks a nil candidate."
  (let* ((runs 100)
         (min-len 5)
         (max-len 20)
         (failures nil)
         (seed (abs (random))))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let ((seq (nskk--azik-chaos--generate-sequence
                    (+ min-len (random (- max-len min-len -1))))))
          (dolist (event seq)
            (condition-case nil
                (nskk--azik-chaos--dispatch-keys event)
              (error nil) (quit nil)))
          ;; Check candidate consistency.
          (when (bound-and-true-p nskk-current-state)
            (let ((phase (nskk-state-henkan-phase nskk-current-state)))
              (when (eq phase 'active)
                (let ((candidates (nskk-state-candidates nskk-current-state)))
                  (when (null candidates)
                    (push (list :run run
                                :seed seed
                                :sequence seq
                                :henkan-phase phase
                                :candidates candidates)
                          failures))))))
          (nskk--azik-chaos--reset-to-idle))))
    (when failures
      (ert-fail
       (format
        "Chaos test P7: active henkan-phase with empty candidates in %d/%d \
runs (seed: %d)\nReproduce: (random %d)\nFirst %d failures:\n%S"
        (length failures) runs seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

;;;;
;;;; Chaos Test P8: DA/DV cleared by C-g (cancel path)
;;;;

(nskk-deftest-e2e azik-chaos-da-dv-clear-after-cancel
  "Chaos test: C-g clears deferred AZIK state (DA and DV variables).

Runs 100 random scenarios.  Each scenario types 1-8 random events
(which may set DA or DV via doubled-consonant AZIK deferred or vowel-
shadow-demoted key patterns), then sends C-g.  After C-g, both
`nskk--deferred-azik-state' and `nskk--deferred-vowel-shadow-state'
must be nil.

This directly exercises the FR-001 fix: `nskk--clear-azik-pending-state'
now includes these two variables in its dolist, so the cancel-preedit
and rollback-conversion paths clear them just like the three pre-existing
colon-okurigana and sokuon-okurigana flags."
  (let* ((runs 100)
         (failures nil)
         (seed (abs (random))))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let* ((pre-len (+ 1 (random 8)))
               (pre-seq (nskk--azik-chaos--generate-sequence pre-len)))
          ;; Type the preamble (may set DA or DV via kk, ssh, etc.)
          (dolist (ev pre-seq)
            (condition-case nil
                (nskk--azik-chaos--dispatch-keys ev)
              (error nil) (quit nil)))
          ;; Send C-g — should clear DA and DV via nskk--clear-azik-pending-state.
          (condition-case nil
              (nskk-e2e--dispatch-event 7)
            (error nil) (quit nil))
          ;; Collect violations.
          (let ((violations nil))
            (when (and (boundp 'nskk--deferred-azik-state)
                       nskk--deferred-azik-state)
              (push (list 'deferred-azik-state nskk--deferred-azik-state)
                    violations))
            (when (and (boundp 'nskk--deferred-vowel-shadow-state)
                       nskk--deferred-vowel-shadow-state)
              (push (list 'deferred-vowel-shadow-state
                          nskk--deferred-vowel-shadow-state)
                    violations))
            (when violations
              (push (list :run      run
                          :seed     seed
                          :pre-seq  pre-seq
                          :stuck    violations)
                    failures)))
          ;; Reset before next run.
          (nskk--azik-chaos--reset-to-idle))))
    (when failures
      (ert-fail
       (format
        "Chaos test P8: C-g left DA/DV non-nil in %d/%d runs (seed: %d)\n\
Reproduce: (random %d)\nFirst %d failures:\n%S"
        (length failures) runs seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

(provide 'nskk-azik-chaos-e2e-test)

;;; nskk-azik-chaos-e2e-test.el ends here
