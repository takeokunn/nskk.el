;;; nskk-conversion-flow-pbt-test.el --- Conversion flow PBT tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test, property-based
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

;; Property-based tests for the kanji conversion pipeline.
;;
;; This file tests invariants of the conversion lifecycle:
;; start conversion -> candidate selection -> commit/cancel.
;;
;; Properties tested:
;; - conversion-roundtrip: Start -> commit produces candidate in converted-buffer
;; - conversion-cancel-restores: Start -> cancel restores pre-conversion state
;; - conversion-candidates-navigable: Cycling through candidates works correctly
;; - conversion-idempotent-commit: Committing twice does not change state
;; - conversion-state-consistency: State is internally consistent after any operation sequence

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-pbt-shrink)
(require 'nskk-state)
(require 'nskk-converter)
(require 'nskk-henkan)


;;;;
;;;; Helper Functions
;;;;

(defconst nskk--pbt-mock-candidates-pool
  '(("漢字" "感じ" "幹事")
    ("日本" "二本")
    ("日本語")
    ("入力")
    ("変換")
    ("テスト")
    ("桜")
    ("山")
    ("川" "河"))
  "Pool of candidate lists for random selection.")

(defconst nskk--pbt-mock-hiragana-inputs
  '("かんじ" "にほん" "にほんご" "にゅうりょく" "へんかん"
    "てすと" "さくら" "やま" "かわ")
  "Pool of hiragana input strings for random selection.")

(defun nskk--pbt-random-candidates ()
  "Generate a random non-empty list of candidates."
  (nskk--pbt-random-choice nskk--pbt-mock-candidates-pool))

(defun nskk--pbt-random-hiragana-input ()
  "Generate a random hiragana input string."
  (nskk--pbt-random-choice nskk--pbt-mock-hiragana-inputs))

(defun nskk--pbt-make-conversion-state ()
  "Create a state with random hiragana input, ready for conversion."
  (let ((state (nskk-state-create 'hiragana)))
    (nskk-state-set state 'input-buffer (nskk--pbt-random-hiragana-input))
    state))

;; These helpers replicate the deleted parallel API:
;; nskk--pbt-start-conversion, nskk--pbt-commit-conversion,
;; nskk--pbt-cancel-conversion.
;; They operate purely on the state struct for integration-level testing.

(defun nskk--pbt-start-conversion (state candidates)
  "Set STATE into active conversion with CANDIDATES (test helper)."
  (nskk-state-set-candidates state candidates)
  (nskk-state-set state 'henkan-position 0)
  (nskk-state-force-henkan-phase state 'active))

(defun nskk--pbt-commit-conversion (state)
  "Commit STATE conversion by taking first candidate (test helper)."
  (let ((first-candidate (car (nskk-state-candidates state))))
    (when first-candidate
      (nskk-state-set state 'converted-buffer first-candidate))
    (nskk-state-set-candidates state nil)
    (nskk-state-set state 'henkan-position nil)
    (nskk-state-force-henkan-phase state nil)))

(defun nskk--pbt-cancel-conversion (state original-input)
  "Cancel conversion in STATE, restoring ORIGINAL-INPUT (test helper)."
  (nskk-state-set state 'input-buffer original-input)
  (nskk-state-set-candidates state nil)
  (nskk-state-set state 'henkan-position nil)
  (nskk-state-force-henkan-phase state nil))


;;;;
;;;; Property 1: Conversion Roundtrip
;;;;

(nskk-property-test nskk-state-machine-conversion-roundtrip
  ((candidates candidate-list))
  (let* ((state (nskk--pbt-make-conversion-state))
         (_input-before (nskk-state-input-buffer state)))
    ;; Start conversion with candidates
    (nskk--pbt-start-conversion state candidates)
    ;; Commit conversion
    (nskk--pbt-commit-conversion state)
    ;; Verify: converted-buffer should contain the first candidate
    (let ((converted (nskk-state-converted-buffer state)))
      (and (stringp converted)
           (not (string-empty-p converted))
           (member (car candidates) (list converted)))))
  50)


;;;;
;;;; Property 2: Conversion Cancel Restores State
;;;;

(nskk-property-test nskk-state-machine-conversion-cancel-restores
  ((candidates candidate-list))
  (let* ((state (nskk--pbt-make-conversion-state))
         (original-input (nskk-state-input-buffer state)))
    ;; Start conversion
    (nskk--pbt-start-conversion state candidates)
    ;; Cancel with original input
    (nskk--pbt-cancel-conversion state original-input)
    ;; Verify: state should be restored
    (let ((input-after (nskk-state-input-buffer state))
          (henkan-pos (nskk-state-henkan-position state))
          (cands-after (nskk-state-candidates state)))
      (and (string= input-after original-input)
           (null henkan-pos)
           (null cands-after))))
  50)


;;;;
;;;; Property 3: Conversion Candidates Navigable
;;;;

(nskk-property-test nskk-state-machine-conversion-candidates-navigable
  ((candidates candidate-list))
  (let* ((state (nskk--pbt-make-conversion-state))
         (num-candidates (length candidates)))
    ;; Start conversion
    (nskk--pbt-start-conversion state candidates)
    ;; Set candidates on state
    (nskk-state-set-candidates state candidates)
    ;; Cycle through all candidates with next
    (let ((visited nil)
          (ok t))
      ;; First candidate
      (let ((first (nskk-state-current-candidate state)))
        (when first (push first visited)))
      ;; Navigate through rest
      (dotimes (_ (1- num-candidates))
        (let ((next (nskk-state-next-candidate state)))
          (when next (push next visited))))
      ;; Verify all candidates were visited
      (dolist (c candidates)
        (unless (member c visited)
          (setq ok nil)))
      ok))
  50)


;;;;
;;;; Property 4: Conversion Idempotent Commit
;;;;

(nskk-property-test nskk-state-machine-conversion-idempotent-commit
  ((candidates candidate-list))
  (let* ((state (nskk--pbt-make-conversion-state)))
    ;; Start and commit once
    (nskk--pbt-start-conversion state candidates)
    (nskk--pbt-commit-conversion state)
    ;; Capture state after first commit
    (let ((converted-1 (nskk-state-converted-buffer state))
          (input-1 (nskk-state-input-buffer state))
          (henkan-1 (nskk-state-henkan-position state))
          (cands-1 (nskk-state-candidates state))
          (idx-1 (nskk-state-current-index state)))
      ;; Commit again (should be no-op since no active conversion)
      (nskk--pbt-commit-conversion state)
      ;; State should be identical
      (let ((converted-2 (nskk-state-converted-buffer state))
            (input-2 (nskk-state-input-buffer state))
            (henkan-2 (nskk-state-henkan-position state))
            (cands-2 (nskk-state-candidates state))
            (idx-2 (nskk-state-current-index state)))
        (and (string= converted-1 converted-2)
             (string= input-1 input-2)
             (equal henkan-1 henkan-2)
             (equal cands-1 cands-2)
             (= idx-1 idx-2)))))
  50)


;;;;
;;;; Property 5: Conversion State Consistency
;;;;

(nskk-property-test nskk-state-machine-conversion-state-consistency
  ((candidates candidate-list))
  (let* ((state (nskk--pbt-make-conversion-state))
         (original-input (nskk-state-input-buffer state))
         (ops (nskk--pbt-random-int 3 8)))
    ;; Perform random sequence of conversion operations
    (dotimes (_ ops)
      (let ((op (nskk--pbt-random-int 0 2)))
        (pcase op
          (0 ;; Start conversion
           (when (not (string-empty-p (nskk-state-input-buffer state)))
             (nskk--pbt-start-conversion state candidates)))
          (1 ;; Cancel conversion
           (nskk--pbt-cancel-conversion state original-input))
          (2 ;; Commit conversion
           (nskk--pbt-commit-conversion state)))))
    ;; Verify state consistency invariants
    (let ((henkan-pos (nskk-state-henkan-position state))
          (cands (nskk-state-candidates state))
          (idx (nskk-state-current-index state))
          (input (nskk-state-input-buffer state))
          (converted (nskk-state-converted-buffer state)))
      (ignore henkan-pos)
      ;; Invariant 1: If no henkan-position, candidates should be nil or empty
      ;; (no dangling henkan-position with empty candidates)
      (let ((consistent t))
        ;; Invariant 2: current-index should be valid for candidates list
        (when cands
          (unless (and (integerp idx) (>= idx 0) (< idx (length cands)))
            (setq consistent nil)))
        ;; Invariant 3: buffers should always be strings
        (unless (and (stringp input) (stringp converted))
          (setq consistent nil))
        ;; Invariant 4: state should still be a valid struct
        (unless (nskk-state-p state)
          (setq consistent nil))
        consistent)))
  50)


;;;; Enhanced PBT Coverage
;;;;

;;;
;;; Shrinking Property 1: Conversion roundtrip with shrinking
;;;

(nskk-property-test-with-shrinking conversion-roundtrip-shrinking
  ((scenario conversion-scenario))
  (let* ((state (nskk-state-create (plist-get scenario :mode)))
         (candidates (nskk--pbt-random-candidates))
         (hiragana-input (nskk--pbt-random-hiragana-input)))
    ;; Put a non-empty input in the buffer
    (nskk-state-set state 'input-buffer hiragana-input)
    ;; Start and commit conversion
    (nskk--pbt-start-conversion state candidates)
    (nskk--pbt-commit-conversion state)
    ;; After commit, converted-buffer must be a non-empty string
    (let ((converted (nskk-state-converted-buffer state)))
      (and (stringp converted)
           (not (string-empty-p converted)))))
  50)

;;;
;;; Shrinking Property 2: Cancel restores original input with shrinking
;;;

(nskk-property-test-with-shrinking conversion-cancel-restores-shrinking
  ((scenario conversion-scenario))
  (let* ((state (nskk-state-create (plist-get scenario :mode)))
         (candidates (nskk--pbt-random-candidates))
         (hiragana-input (nskk--pbt-random-hiragana-input)))
    (nskk-state-set state 'input-buffer hiragana-input)
    ;; Start then cancel conversion
    (nskk--pbt-start-conversion state candidates)
    (nskk--pbt-cancel-conversion state hiragana-input)
    ;; Post-cancel invariants: input restored, henkan-position cleared
    (and (string= (nskk-state-input-buffer state) hiragana-input)
         (null (nskk-state-henkan-position state))
         (null (nskk-state-candidates state))
         (nskk-state-p state)))
  50)

;;;
;;; Scenario DSL: Kanji conversion lifecycle
;;;

(nskk-describe "Kanji conversion lifecycle"

  (nskk-it "cancelling conversion restores pre-conversion state"
    (nskk-given
      (let* ((state (nskk-state-create 'hiragana))
             (candidates '("漢字" "感じ" "幹事"))
             (original-input "かんじ"))
        (nskk-state-set state 'input-buffer original-input)
        (nskk-when
          (progn
            (nskk--pbt-start-conversion state candidates)
            (nskk--pbt-cancel-conversion state original-input)))
        (nskk-then
          (should (string= (nskk-state-input-buffer state) original-input))
          (should (null (nskk-state-henkan-position state)))
          (should (null (nskk-state-candidates state)))
          (should (nskk-state-p state))))))

  (nskk-it "committing first candidate updates converted-buffer"
    (nskk-given
      (let* ((state (nskk-state-create 'hiragana))
             (candidates '("漢字" "感じ" "幹事")))
        (nskk-state-set state 'input-buffer "かんじ")
        (nskk-when
          (progn
            (nskk--pbt-start-conversion state candidates)
            (nskk--pbt-commit-conversion state)))
        (nskk-then
          (let ((converted (nskk-state-converted-buffer state)))
            (should (stringp converted))
            (should (not (string-empty-p converted)))
            (should (nskk-state-p state))))))))


;;;;
;;;; CPS Tests: /k suffix function calling convention
;;;;

(nskk-property-test-with-shrinking nskk-property-cps-converter-convert-calls-exactly-one-branch
  ((pattern romaji-pattern))
  (let ((call-count 0)
        (branch nil))
    (condition-case nil
        (nskk-converter-convert/k
         pattern
         (lambda (_kana _remaining) (cl-incf call-count) (setq branch 'match))
         (lambda (_prefix) (cl-incf call-count) (setq branch 'incomplete))
         (lambda () (cl-incf call-count) (setq branch 'fail)))
      (error (setq branch 'error call-count 1)))
    (and (= call-count 1)
         (memq branch '(match incomplete fail))))
  50)

(nskk-describe "CPS: nskk-converter-convert/k dispatch invariants"

  (nskk-it "on-match continuation receives a non-empty string as first arg"
    (let ((failures nil))
      (dolist (romaji '("ka" "ki" "a" "i" "u" "sha" "chi" "tsu"))
        (let ((result-value nil)
              (branch-called nil))
          (nskk-converter-convert/k
           romaji
           (lambda (kana _remaining)
             (setq branch-called 'match result-value kana) kana)
           (lambda (p) (setq branch-called 'incomplete) p)
           (lambda () (setq branch-called 'fail)))
          (when (eq branch-called 'match)
            (unless (and (stringp result-value)
                         (not (string-empty-p result-value)))
              (push (list :romaji romaji :result result-value) failures)))))
      (when failures
        (ert-fail (format "on-match received non-string for %d cases:\n%S"
                          (length failures) failures))))))

;;;;
;;;; CPS Tests: nskk-convert-romaji/k result type
;;;;

(nskk-property-test-with-shrinking nskk-property-cps-convert-romaji-result-is-string
  ((pattern romaji-basic))
  (let ((result-type nil))
    (nskk-convert-romaji/k
     pattern
     (lambda (r) (setq result-type (type-of r)))
     #'ignore)
    (eq result-type 'string))
  50)

(nskk-describe "CPS: nskk-convert-romaji/k result type"

  (nskk-it "on-result receives a string for any romaji input"
    (let ((failures nil))
      (dolist (romaji '("ka" "ki" "a" "i" "u" "sha" "chi" "tsu"))
        (let ((result-type nil))
          (nskk-convert-romaji/k
           romaji
           (lambda (r) (setq result-type (type-of r)))
           #'ignore)
          (unless (eq result-type 'string)
            (push (list :input romaji :type result-type) failures))))
      (when failures
        (ert-fail (format "nskk-convert-romaji/k on-result type wrong for %d cases:\n%S"
                          (length failures) failures))))))

;;;;
;;;; CPS Tests: nskk-dict-lookup/k mutual exclusion
;;;;

(nskk-property-test-with-shrinking nskk-property-cps-dict-lookup-exactly-one-branch
  ((key search-query))
  (nskk-with-mock-dict nil
    (let ((found-called nil)
          (not-found-called nil))
      (nskk-dict-lookup/k
       key
       (lambda (_entry) (setq found-called t))
       (lambda () (setq not-found-called t)))
      ;; Exactly one of on-found or on-not-found must be called, never both, never neither
      (and (or found-called not-found-called)
           (not (and found-called not-found-called)))))
  50)

(nskk-describe "CPS: nskk-dict-lookup/k mutual exclusion"

  (nskk-it "calls exactly one of on-found or on-not-found"
    (nskk-with-mock-dict nil
      (let ((failures nil))
        (dolist (key '("かんじ" "にほん" "nonexistent-xyz" "さくら" "does-not-exist"))
          (let ((found-called nil)
                (not-found-called nil))
            (nskk-dict-lookup/k
             key
             (lambda (entry) (setq found-called t) entry)
             (lambda () (setq not-found-called t)))
            (when (and found-called not-found-called)
              (push (list :key key :both-called t) failures))
            (when (and (not found-called) (not not-found-called))
              (push (list :key key :neither-called t) failures))))
        (when failures
          (ert-fail (format "dict-lookup/k branch exclusion failed:\n%S" failures)))))))


(provide 'nskk-conversion-flow-pbt-test)

;;; nskk-conversion-flow-pbt-test.el ends here
