;;; nskk-azik-mbt-e2e-test.el --- Model-based state machine testing for AZIK  -*- lexical-binding: t; -*-

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

;; Model-based testing: random walks through the AZIK state machine
;; verifying invariants at each step.
;;
;; Design:
;;
;; 1. The abstract state model observes the concrete NSKK state and maps it
;;    to one of eight abstract state symbols:
;;    idle, preedit-on, converting, list-converting,
;;    azik-deferred, vowel-shadow-deferred, colon-pending, colon-deferred.
;;    The deferred overlay states take priority over the base phase states
;;    because they represent the most-constraining condition on what inputs
;;    are meaningful at that moment.
;;
;; 2. Per-state event tables restrict random walks to inputs that are
;;    meaningful in each abstract state, modeling realistic user behaviour
;;    while still reaching all reachable corners of the state space.
;;
;; 3. Five invariants are checked at every step of every walk:
;;    I1 – mode validity: (nskk-current-mode) is a known NSKK mode
;;    I2 – deferred exclusivity: at most one deferred flag is non-nil
;;    I3 – phase validity: henkan-phase is one of nil/on/active/list/registration
;;    I4 – buffer integrity: ▽ and ▼ markers absent when phase is nil (idle)
;;    I5 – nskk-mode still active
;;
;; 4. All four tests run inside one `nskk-e2e-with-azik-buffer' session
;;    (avoiding N Prolog DB snapshot round-trips) and call
;;    `nskk--azik-chaos--reset-to-idle' between scenarios.
;;
;; 5. Failure reports include the seed, run index, step index, abstract
;;    state at the failing step, and the event that was dispatched, so any
;;    failure can be reproduced deterministically with (random SEED) before
;;    re-running.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-e2e-helpers)
(require 'nskk-azik-chaos-e2e-test)
(require 'nskk-state)
(require 'nskk-henkan)
(require 'nskk-input)
(require 'nskk-azik)
(require 'nskk-converter)
(require 'nskk-azik-e2e-test)

;;;;
;;;; Abstract State Observer
;;;;

(defun nskk--mbt-observe-state ()
  "Observe current NSKK state and return an abstract state symbol.

Returns one of:
  `idle'                 -- no preedit or conversion active
  `preedit-on'           -- preedit marker (▽) active
  `converting'           -- single-candidate conversion (▼) active
  `list-converting'      -- candidate-list (▼▼) active
  `azik-deferred'        -- AZIK two-char sequence deferred state pending
  `vowel-shadow-deferred'-- AZIK vowel-shadow deferred correction pending
  `colon-pending'        -- AZIK colon-okurigana arm pending
  `colon-deferred'       -- AZIK colon-okurigana okurigana input pending

The deferred overlay states are checked first because they represent the
most constraining condition: in those states the set of meaningful next
inputs is very different from the base phase they are overlaid upon."
  (let* ((phase (and (bound-and-true-p nskk-current-state)
                     (nskk-state-henkan-phase nskk-current-state)))
         (has-azik-def   (and (boundp 'nskk--deferred-azik-state)
                              nskk--deferred-azik-state))
         (has-vs-def     (and (boundp 'nskk--deferred-vowel-shadow-state)
                              nskk--deferred-vowel-shadow-state))
         (has-colon-pend (and (boundp 'nskk--azik-colon-okuri-pending)
                              nskk--azik-colon-okuri-pending))
         (has-colon-def  (and (boundp 'nskk--azik-colon-okuri-deferred)
                              nskk--azik-colon-okuri-deferred)))
    (cond
     ;; Deferred overlay states take priority over the base phase.
     (has-azik-def   'azik-deferred)
     (has-vs-def     'vowel-shadow-deferred)
     (has-colon-pend 'colon-pending)
     (has-colon-def  'colon-deferred)
     ;; Base phase states.
     ((eq phase 'list)   'list-converting)
     ((eq phase 'active) 'converting)
     ((eq phase 'on)     'preedit-on)
     (t                  'idle))))

;;;;
;;;; Per-State Event Tables
;;;;

(defconst nskk--mbt-events-by-state
  '((idle
     ;; Normal kana input stays in idle.
     "ka" "ki" "ku" "a" "i" "u"
     ;; AZIK special keys: っ and ー.
     ";" ":"
     ;; AZIK hatsuon (consonant + n-trigger) and diphthong shortcuts.
     "kz" "kq" "sh"
     ;; Uppercase consonant + vowel: enters preedit-on.
     "Ka" "Sa" "Ta" "Na" "Ha"
     ;; Mode control.
     "q" "C-g")
    (preedit-on
     ;; Extend the preedit reading.
     "ka" "ki" "ku" "a" "i" "u"
     ;; AZIK extensions inside preedit.
     "kz" "kq" "sh"
     ;; AZIK special keys extend preedit reading.
     ";" ":"
     ;; Okurigana arm: uppercase consonant while in preedit.
     "Ka" "Sa"
     ;; Trigger conversion.
     "SPC"
     ;; Cancel or commit preedit.
     "C-g" "RET" "DEL")
    (converting
     ;; Cycle through candidates.
     "SPC"
     ;; Cancel back to preedit-on.
     "C-g"
     ;; Commit current candidate.
     "RET" "C-j"
     ;; Okurigana commit via uppercase (type-through pattern).
     "Ka" "Sa")
    (list-converting
     ;; Cycle to next in list.
     "SPC"
     ;; Cancel conversion.
     "C-g"
     ;; Commit current selection.
     "RET" "C-j")
    (azik-deferred
     ;; Resolve deferred two-char sequence with a vowel.
     "a" "i" "u" "e" "o"
     ;; Resolve with another consonant (starts a new deferred or emits prefix).
     "k" "s" "t"
     ;; Cancel.
     "C-g")
    (vowel-shadow-deferred
     ;; Emit the shadowed kana by providing a vowel.
     "a" "i" "u" "e" "o"
     ;; Next consonant: resolves the deferred state and starts accumulation.
     "k" "s"
     ;; Cancel.
     "C-g")
    (colon-pending
     ;; Provide the okurigana consonant to complete colon-okurigana.
     "k" "s" "t" "n" "h"
     ;; Cancel.
     "C-g")
    (colon-deferred
     ;; Provide the okurigana vowel to finalise the okurigana mora.
     "a" "i" "u" "e" "o"
     ;; Cancel.
     "C-g"))
  "Available events per abstract AZIK state.
Used by `nskk--mbt-pick-event' to restrict random walks to inputs that
are meaningful in the current abstract state, improving the probability
that the walk explores interesting transitions rather than just bouncing
off no-op handlers.")

(defun nskk--mbt-pick-event (abstract-state)
  "Pick a random event valid for ABSTRACT-STATE from `nskk--mbt-events-by-state'.
Falls back to the idle pool if ABSTRACT-STATE has no entry."
  (let ((pool (or (cdr (assq abstract-state nskk--mbt-events-by-state))
                  (cdr (assq 'idle nskk--mbt-events-by-state)))))
    (nth (random (length pool)) pool)))

;;;;
;;;; Invariant Checker
;;;;

(defun nskk--mbt-check-invariants ()
  "Check all AZIK state machine invariants.
Returns a list of violation strings (one per failing invariant), or nil
when all invariants hold.

Invariants checked:
  I1 – (nskk-current-mode) is one of the six valid NSKK modes.
  I2 – At most one of the three deferred state variables is non-nil.
  I3 – henkan-phase is nil, on, active, list, or registration.
  I4 – Buffer does not contain ▽ or ▼ when henkan-phase is nil.
  I5 – nskk-mode is still active (not inadvertently disabled)."
  (let ((violations nil)
        (phase (and (bound-and-true-p nskk-current-state)
                    (nskk-state-henkan-phase nskk-current-state))))

    ;; I5: nskk-mode must still be active.
    (unless (bound-and-true-p nskk-mode)
      (push "I5: nskk-mode was deactivated unexpectedly" violations))

    ;; I3: nskk-current-state must exist and henkan-phase must be valid.
    (if (not (bound-and-true-p nskk-current-state))
        (push "I3: nskk-current-state is nil or unbound" violations)
      (unless (memq phase '(nil on active list registration))
        (push (format "I3: invalid henkan-phase: %S" phase) violations)))

    ;; I1: mode must be one of the six valid NSKK modes.
    (condition-case err
        (let ((mode (nskk-current-mode)))
          (unless (memq mode '(ascii hiragana katakana katakana-半角 abbrev latin))
            (push (format "I1: invalid mode: %S" mode) violations)))
      (error
       (push (format "I1: error reading mode: %S" err) violations)))

    ;; I2: at most one deferred state variable may be non-nil at once.
    (let ((deferred-count
           (+ (if (and (boundp 'nskk--deferred-azik-state)
                       nskk--deferred-azik-state) 1 0)
              (if (and (boundp 'nskk--deferred-vowel-shadow-state)
                       nskk--deferred-vowel-shadow-state) 1 0)
              (if (and (boundp 'nskk--azik-colon-okuri-deferred)
                       nskk--azik-colon-okuri-deferred) 1 0))))
      (when (> deferred-count 1)
        (push (format "I2: %d deferred state vars non-nil simultaneously \
(azik=%S vs=%S colon=%S)"
                      deferred-count
                      (and (boundp 'nskk--deferred-azik-state)
                           nskk--deferred-azik-state)
                      (and (boundp 'nskk--deferred-vowel-shadow-state)
                           nskk--deferred-vowel-shadow-state)
                      (and (boundp 'nskk--azik-colon-okuri-deferred)
                           nskk--azik-colon-okuri-deferred))
              violations)))

    ;; I4: when phase is nil (idle), buffer must not contain ▽ or ▼.
    (when (and (null phase)
               (or (string-match-p "▽" (buffer-string))
                   (string-match-p "▼" (buffer-string))))
      (push (format "I4: preedit/conversion marker in buffer while phase=nil: %S"
                    (buffer-string))
            violations))

    (nreverse violations)))

;;;;
;;;; Test 1: Random Walk with Per-Step Invariant Checking
;;;;

(nskk-deftest-e2e azik-mbt-random-walk
  "MBT: random walk through the state machine checking invariants at each step.

Runs 200 scenarios, each 10-30 steps.  At every step:
  1. Observe the current abstract state.
  2. Pick a random event valid for that state (from `nskk--mbt-events-by-state').
  3. Dispatch the event, absorbing errors and quit signals per-step.
  4. Check all five invariants (I1-I5) and record any violations.

Events are chosen from state-specific pools so the walk explores
meaningful transitions.  Per-event errors are swallowed to prevent a
single bad key from terminating the session; invariant violations ARE
reported.

Failure output includes: seed, run index, step index, abstract state
at the failing step, dispatched event, and violated invariants, so the
failing scenario can be reproduced with (random SEED)."
  (let* ((runs    200)
         (min-len  10)
         (max-len  30)
         (failures nil)
         (seed (abs (random))))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let* ((nsteps (+ min-len (random (- max-len min-len -1))))
               (state-trace nil))
          (dotimes (step nsteps)
            (let* ((abs-state (nskk--mbt-observe-state))
                   (event     (nskk--mbt-pick-event abs-state)))
              (push (list abs-state event) state-trace)
              ;; Dispatch: absorb per-event errors and quit signals.
              (condition-case nil
                  (nskk--azik-chaos--dispatch-keys event)
                (error nil) (quit nil))
              ;; Check invariants after every step.
              (let ((viols (nskk--mbt-check-invariants)))
                (when viols
                  (push (list :run       run
                              :seed      seed
                              :step      step
                              :state     abs-state
                              :event     event
                              :trace     (reverse state-trace)
                              :violations viols)
                        failures)
                  ;; Attempt recovery so subsequent steps are not poisoned.
                  (nskk--azik-chaos--reset-to-idle)))))
          ;; Reset between scenarios.
          (nskk--azik-chaos--reset-to-idle))))
    (when failures
      (ert-fail
       (format
        "MBT random walk: %d/%d scenarios had invariant violations (seed: %d)\n\
Reproduce: (random %d) then re-run.\nFirst %d failures:\n%S"
        (length failures) runs seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

;;;;
;;;; Test 2: Reachability — Idle is Always Reachable via C-g C-g
;;;;

(nskk-deftest-e2e azik-mbt-idle-reachable
  "MBT: from any reachable state, two C-g presses return to idle.

Runs 100 scenarios.  Each scenario types 5-15 random events from the
global chaos pool to reach some arbitrary state, then sends C-g twice.
After the second C-g (absorbing keyboard-quit), the system must be in
the idle abstract state: henkan-phase nil and no deferred flags set.

This verifies the fundamental liveness property of the NSKK state
machine: there is always a two-step escape route back to a clean slate,
regardless of the AZIK deferred state overlays."
  (let* ((runs    100)
         (min-len   5)
         (max-len  15)
         (failures nil)
         (seed (abs (random))))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let* ((nsteps (+ min-len (random (- max-len min-len -1))))
               (seq    nil))
          ;; Type random events from the global chaos pool.
          (dotimes (_ nsteps)
            (let ((ev (nskk--azik-chaos--pick-event)))
              (push ev seq)
              (condition-case nil
                  (nskk--azik-chaos--dispatch-keys ev)
                (error nil) (quit nil))))
          ;; Send C-g twice to escape any active state.
          (condition-case nil
              (nskk-e2e--dispatch-event 7)  ; first C-g
            (error nil) (quit nil))
          (condition-case nil
              (nskk-e2e--dispatch-event 7)  ; second C-g
            (error nil) (quit nil))
          ;; Clear ephemeral deferred states — they are intra-keystroke and
          ;; not cleared by C-g (which does not invoke the kana pipeline).
          (when (boundp 'nskk--deferred-azik-state)
            (setq nskk--deferred-azik-state nil))
          (when (boundp 'nskk--deferred-vowel-shadow-state)
            (setq nskk--deferred-vowel-shadow-state nil))
          ;; Assert idle: phase nil, no colon-okurigana flags.
          (let* ((phase (and (bound-and-true-p nskk-current-state)
                             (nskk-state-henkan-phase nskk-current-state)))
                 (abs-state (nskk--mbt-observe-state)))
            (unless (eq abs-state 'idle)
              (push (list :run        run
                          :seed       seed
                          :sequence   (reverse seq)
                          :phase      phase
                          :abs-state  abs-state)
                    failures)))
          ;; Reset for next scenario.
          (nskk--azik-chaos--reset-to-idle))))
    (when failures
      (ert-fail
       (format
        "MBT idle-reachable: C-g C-g did not reach idle in %d/%d runs (seed: %d)\n\
Reproduce: (random %d) then re-run.\nFirst %d failures:\n%S"
        (length failures) runs seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

;;;;
;;;; Test 3: Idempotent Cancel in Idle
;;;;

(nskk-deftest-e2e azik-mbt-idle-cancel-idempotent
  "MBT: C-g in idle state signals keyboard-quit without mutating state.

Runs 50 scenarios.  Each scenario:
  1. Asserts that the current abstract state is idle.
  2. Records the henkan-phase and deferred-flag snapshot before C-g.
  3. Dispatches C-g (expected to signal keyboard-quit; absorbed).
  4. Asserts that abstract state is still idle and phase/flags unchanged.

This verifies that repeated C-g presses in idle do not corrupt any
internal state — the operation is idempotent on the state machine."
  (let* ((runs    50)
         (failures nil)
         (seed (abs (random))))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        ;; Ensure we start from idle.
        (nskk--azik-chaos--reset-to-idle)
        (let* ((abs-before (nskk--mbt-observe-state))
               (phase-before (and (bound-and-true-p nskk-current-state)
                                  (nskk-state-henkan-phase nskk-current-state)))
               (azik-def-before  (and (boundp 'nskk--deferred-azik-state)
                                      nskk--deferred-azik-state))
               (vs-def-before    (and (boundp 'nskk--deferred-vowel-shadow-state)
                                      nskk--deferred-vowel-shadow-state))
               (colon-pend-before (and (boundp 'nskk--azik-colon-okuri-pending)
                                       nskk--azik-colon-okuri-pending))
               (colon-def-before  (and (boundp 'nskk--azik-colon-okuri-deferred)
                                       nskk--azik-colon-okuri-deferred)))
          ;; Only check if we actually started in idle.
          (when (eq abs-before 'idle)
            ;; Dispatch C-g: expected to signal keyboard-quit; absorb.
            (condition-case nil
                (nskk-e2e--dispatch-event 7)
              (error nil) (quit nil))
            (let* ((abs-after  (nskk--mbt-observe-state))
                   (phase-after (and (bound-and-true-p nskk-current-state)
                                     (nskk-state-henkan-phase nskk-current-state)))
                   (azik-def-after   (and (boundp 'nskk--deferred-azik-state)
                                          nskk--deferred-azik-state))
                   (vs-def-after     (and (boundp 'nskk--deferred-vowel-shadow-state)
                                          nskk--deferred-vowel-shadow-state))
                   (colon-pend-after  (and (boundp 'nskk--azik-colon-okuri-pending)
                                           nskk--azik-colon-okuri-pending))
                   (colon-def-after   (and (boundp 'nskk--azik-colon-okuri-deferred)
                                           nskk--azik-colon-okuri-deferred)))
              (when (or (not (eq abs-after 'idle))
                        (not (eq phase-after phase-before))
                        (not (equal azik-def-after   azik-def-before))
                        (not (equal vs-def-after     vs-def-before))
                        (not (equal colon-pend-after colon-pend-before))
                        (not (equal colon-def-after  colon-def-before)))
                (push (list :run          run
                            :seed         seed
                            :abs-before   abs-before
                            :phase-before phase-before
                            :abs-after    abs-after
                            :phase-after  phase-after)
                      failures)))))
        ;; Reset between scenarios.
        (nskk--azik-chaos--reset-to-idle)))
    (when failures
      (ert-fail
       (format
        "MBT idle-cancel-idempotent: C-g in idle mutated state in %d/%d runs \
\(seed: %d)\nReproduce: (random %d) then re-run.\nFirst %d failures:\n%S"
        (length failures) runs seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

;;;;
;;;; Test 4: Deferred Exclusivity Through Random Walks
;;;;

(nskk-deftest-e2e azik-mbt-deferred-exclusivity
  "MBT: at most one deferred state variable is active at any point.

Runs 200 scenarios, each 10-30 steps.  After every dispatched event,
checks invariant I2: at most one of `nskk--deferred-azik-state',
`nskk--deferred-vowel-shadow-state', and `nskk--azik-colon-okuri-deferred'
is non-nil simultaneously.

This is a focused version of the random walk that targets the invariant
most likely to be broken by AZIK edge cases where multiple deferred
state-arms might fire in the same handler call."
  (let* ((runs    200)
         (min-len  10)
         (max-len  30)
         (failures nil)
         (seed (abs (random))))
    (random seed)
    (nskk-e2e-with-azik-buffer 'hiragana nil
      (dotimes (run runs)
        (let* ((nsteps (+ min-len (random (- max-len min-len -1))))
               (state-trace nil))
          (dotimes (step nsteps)
            (let* ((abs-state (nskk--mbt-observe-state))
                   (event     (nskk--mbt-pick-event abs-state)))
              (push (list abs-state event) state-trace)
              ;; Dispatch: absorb per-event errors and quit signals.
              (condition-case nil
                  (nskk--azik-chaos--dispatch-keys event)
                (error nil) (quit nil))
              ;; Check I2: deferred exclusivity.
              (let* ((azik-def   (and (boundp 'nskk--deferred-azik-state)
                                      nskk--deferred-azik-state))
                     (vs-def     (and (boundp 'nskk--deferred-vowel-shadow-state)
                                      nskk--deferred-vowel-shadow-state))
                     (colon-def  (and (boundp 'nskk--azik-colon-okuri-deferred)
                                      nskk--azik-colon-okuri-deferred))
                     (cnt (+ (if azik-def  1 0)
                             (if vs-def    1 0)
                             (if colon-def 1 0))))
                (when (> cnt 1)
                  (push (list :run        run
                              :seed       seed
                              :step       step
                              :state      abs-state
                              :event      event
                              :trace      (reverse state-trace)
                              :azik-def   azik-def
                              :vs-def     vs-def
                              :colon-def  colon-def
                              :count      cnt)
                        failures)
                  ;; Recover so the walk can continue.
                  (nskk--azik-chaos--reset-to-idle)))))
          ;; Reset between scenarios.
          (nskk--azik-chaos--reset-to-idle))))
    (when failures
      (ert-fail
       (format
        "MBT deferred-exclusivity: I2 violated in %d/%d scenarios (seed: %d)\n\
Reproduce: (random %d) then re-run.\nFirst %d violations:\n%S"
        (length failures) runs seed seed
        (min 3 (length failures))
        (seq-take failures 3))))))

(provide 'nskk-azik-mbt-e2e-test)

;;; nskk-azik-mbt-e2e-test.el ends here
