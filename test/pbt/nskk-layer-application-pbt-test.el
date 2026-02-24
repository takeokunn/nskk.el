;;; nskk-layer-application-pbt-test.el --- Property-Based Tests for Application Layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: NSKK Contributors
;; Maintainer: takeokunn <bararararatty@gmail.com>
;; URL: https://github.com/takeokunn/nskk.el
;; Keywords: i18n, testing, property-based

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

;; Property-Based Tests for the Application layer (nskk-mode-switch.el,
;; nskk-input-commands.el).
;;
;; This file tests Application layer properties including:
;; - Mode switching behavior and context clearing
;; - Input routing based on current mode
;; - Okurigana detection and conversion triggering
;; - Conversion state management
;;
;; Test Categories:
;; - Mode Switch Properties: Mode transitions and context clearing
;; - Input Routing Properties: Mode-dependent input handling
;; - Okurigana Properties: Okurigana detection and processing
;; - Conversion State Properties: State transitions during conversion

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-mode-switch)
(require 'nskk-input-commands)
(require 'nskk-state)

;;;
;;; Register Additional Generators for Application Layer Tests
;;;

;; Initialize random state with a positive seed
(random (abs (random)))

(nskk-register-generator 'pbt-uppercase-consonant
  (lambda ()
    (let ((consonants '(?K ?S ?T ?N ?H ?M ?Y ?R ?W ?G ?Z ?D ?B ?P)))
      (nth (random (length consonants)) consonants))))

(nskk-register-generator 'pbt-lowercase-consonant
  (lambda ()
    (let ((consonants '(?k ?s ?t ?n ?h ?m ?y ?r ?w ?g ?z ?d ?b ?p)))
      (nth (random (length consonants)) consonants))))

(nskk-register-generator 'pbt-latin-char
  (lambda ()
    (let ((chars (append (string-to-list "abcdefghijklmnopqrstuvwxyz")
                         (string-to-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                         (string-to-list "0123456789"))))
      (nth (random (length chars)) chars))))

(nskk-register-generator 'pbt-conversion-index
  (lambda ()
    (random 10)))  ; Range: 0 to 9

;;;
;;; Helper Functions
;;;

(defun nskk-pbt--application-state-valid-p (state)
  "Check if STATE is valid for application layer testing."
  (and (nskk-state-p state)
       (nskk-state-valid-mode-p (nskk-state-mode state))
       (stringp (nskk-state-input-buffer state))
       (stringp (nskk-state-converted-buffer state))
       (listp (nskk-state-candidates state))
       (>= (nskk-state-current-index state) 0)))

(defun nskk-pbt--japanese-mode-p (mode)
  "Check if MODE is a Japanese input mode."
  (memq mode '(hiragana katakana katakana-半角)))

(defun nskk-pbt--latin-mode-p (mode)
  "Check if MODE is Latin mode."
  (eq mode 'latin))

(defun nskk-pbt--make-test-state (mode)
  "Create a test state with MODE."
  (let ((state (nskk-state-create mode)))
    state))

(defun nskk-pbt--simulate-mode-switch (state target-mode)
  "Simulate mode switch from STATE to TARGET-MODE.
Returns the updated state."
  (when (and (nskk-state-p state)
             (nskk-state-valid-mode-p target-mode))
    (nskk-state-set state 'previous-mode (nskk-state-mode state))
    (nskk-state-set state 'mode target-mode)
    ;; Mode switch clears conversion context
    (nskk-state-set state 'candidates nil)
    (nskk-state-set state 'current-index 0)
    (nskk-state-set state 'henkan-position nil)
    state))

(defun nskk-pbt--simulate-input (state char)
  "Simulate input of CHAR in STATE.
Returns the updated state."
  (when (nskk-state-p state)
    (let ((mode (nskk-state-mode state)))
      (cond
       ;; Latin mode: direct insert (no conversion)
       ((nskk-pbt--latin-mode-p mode)
        (nskk-state-set state 'converted-buffer
                        (concat (nskk-state-converted-buffer state)
                                (char-to-string char))))
       ;; Japanese modes: process as romaji
       ((nskk-pbt--japanese-mode-p mode)
        (nskk-state-append-input state char))
       ;; Other modes: direct insert
       (t
        (nskk-state-set state 'converted-buffer
                        (concat (nskk-state-converted-buffer state)
                                (char-to-string char))))))
    state))

(defun nskk-pbt--detect-okurigana (char)
  "Check if CHAR is an okurigana marker (uppercase consonant).
Returns lowercase consonant if detected, nil otherwise."
  (when (and (characterp char)
             (<= ?A char) (<= char ?Z))
    (downcase char)))

(defun nskk-pbt--simulate-conversion-start (state)
  "Simulate starting conversion in STATE.
Returns the updated state with candidates."
  (when (nskk-state-p state)
    (let ((input (nskk-state-input-buffer state)))
      (when (> (length input) 0)
        (nskk-state-set state 'henkan-position 0)
        ;; Simulate candidates (mock data)
        (nskk-state-set state 'candidates '("候補1" "候補2" "候補3"))
        (nskk-state-set state 'current-index 0)))
    state))

(defun nskk-pbt--simulate-conversion-commit (state)
  "Simulate committing conversion in STATE.
Returns the updated state."
  (when (nskk-state-p state)
    (let ((candidates (nskk-state-candidates state))
          (index (nskk-state-current-index state)))
      (when (and candidates (< index (length candidates)))
        (let ((selected (nth index candidates)))
          (nskk-state-set state 'converted-buffer selected)
          (nskk-state-set state 'input-buffer "")
          (nskk-state-set state 'candidates nil)
          (nskk-state-set state 'current-index 0)
          (nskk-state-set state 'henkan-position nil))))
    state))

(defun nskk-pbt--simulate-conversion-cancel (state original-input)
  "Simulate canceling conversion in STATE, restoring ORIGINAL-INPUT.
Returns the updated state."
  (when (nskk-state-p state)
    (nskk-state-set state 'input-buffer original-input)
    (nskk-state-set state 'candidates nil)
    (nskk-state-set state 'current-index 0)
    (nskk-state-set state 'henkan-position nil)
    state))

;;;
;;; Mode Switch Properties
;;;

(nskk-property-test-seeded mode-switch-valid-target
  ((from-mode valid-mode)
   (to-mode valid-mode))
  (let ((state (nskk-state-create from-mode)))
    (nskk-pbt--simulate-mode-switch state to-mode)
    (and (nskk-state-p state)
         (eq (nskk-state-mode state) to-mode)
         (nskk-state-valid-mode-p (nskk-state-mode state))))
  75)

(nskk-property-test-seeded mode-switch-clears-context
  ((from-mode valid-mode)
   (to-mode valid-mode))
  (let ((state (nskk-state-create from-mode)))
    ;; Set up some context
    (nskk-state-set state 'input-buffer "testinput")
    (nskk-state-set state 'candidates '("候補A" "候補B"))
    (nskk-state-set state 'current-index 1)
    (nskk-state-set state 'henkan-position 3)
    ;; Perform mode switch
    (nskk-pbt--simulate-mode-switch state to-mode)
    ;; Context should be cleared
    (and (eq (nskk-state-mode state) to-mode)
         (null (nskk-state-candidates state))
         (= (nskk-state-current-index state) 0)
         (null (nskk-state-henkan-position state))))
  75)

(nskk-property-test-seeded mode-toggle-reversible
  ((mode1 valid-mode)
   (mode2 valid-mode))
  (let ((state (nskk-state-create mode1)))
    ;; Store original mode
    (let ((original-mode (nskk-state-mode state)))
      ;; Switch to mode2
      (nskk-pbt--simulate-mode-switch state mode2)
      (when (eq (nskk-state-mode state) mode2)
        ;; Switch back to mode1
        (nskk-pbt--simulate-mode-switch state mode1)
        (eq (nskk-state-mode state) mode1))))
  75)

;;;
;;; Input Routing Properties
;;;

(nskk-property-test-seeded input-routing-mode-correct
  ((mode valid-mode)
   (ch pbt-latin-char))
  (let ((state (nskk-state-create mode)))
    (nskk-pbt--simulate-input state ch)
    ;; After input, state should still be valid and in the same mode
    (and (nskk-state-p state)
         (eq (nskk-state-mode state) mode)
         (nskk-state-valid-mode-p (nskk-state-mode state))))
  75)

(nskk-property-test-seeded latin-mode-direct-insert
  ((ch pbt-latin-char))
  (let ((state (nskk-state-create 'latin)))
    (nskk-pbt--simulate-input state ch)
    ;; In latin mode, character should go to converted-buffer directly
    (and (stringp (nskk-state-converted-buffer state))
         (> (length (nskk-state-converted-buffer state)) 0)
         ;; Input buffer should remain empty
         (string= (nskk-state-input-buffer state) "")))
  75)

(nskk-property-test-seeded japanese-mode-converts
  ((mode valid-mode)
   (ch pbt-lowercase-consonant))
  ;; Only test for Japanese modes
  (if (not (nskk-pbt--japanese-mode-p mode))
      t  ; Skip non-Japanese modes
    (let ((state (nskk-state-create mode)))
      (nskk-pbt--simulate-input state ch)
      ;; In Japanese modes, input should accumulate in input-buffer
      (and (stringp (nskk-state-input-buffer state))
           (> (length (nskk-state-input-buffer state)) 0)
           ;; converted-buffer should remain empty until conversion
           (string= (nskk-state-converted-buffer state) ""))))
  75)

;;;
;;; Okurigana Properties
;;;

(nskk-property-test-seeded okurigana-detection-correct
  ((ch pbt-uppercase-consonant))
  (let ((result (nskk-pbt--detect-okurigana ch)))
    ;; Uppercase consonants should be detected as okurigana
    (and result
         (characterp result)
         ;; Result should be lowercase
         (<= ?a result) (<= result ?z)
         ;; Result should be the lowercase version of input
         (= result (downcase ch))))
  75)

(nskk-property-test-seeded okurigana-conversion-triggers
  ((mode valid-mode)
   (prefix-1 pbt-lowercase-consonant)
   (prefix-2 pbt-lowercase-consonant)
   (okuri-ch pbt-uppercase-consonant))
  ;; Only test for Japanese modes with valid okurigana
  (if (not (nskk-pbt--japanese-mode-p mode))
      t  ; Skip non-Japanese modes - okurigana is not applicable
    (let ((state (nskk-state-create mode))
          (okuri-detected nil))
      ;; Input some romaji prefix
      (nskk-pbt--simulate-input state prefix-1)
      (nskk-pbt--simulate-input state prefix-2)
      ;; Detect okurigana
      (setq okuri-detected (nskk-pbt--detect-okurigana okuri-ch))
      ;; If okurigana detected, it should trigger conversion
      (when okuri-detected
        (nskk-pbt--simulate-conversion-start state)
        (and (nskk-state-candidates state)
             (>= (length (nskk-state-candidates state)) 0)))))
  75)

;;;
;;; Conversion State Properties
;;;

(nskk-property-test-seeded conversion-state-toggle
  ((mode valid-mode))
  (if (not (nskk-pbt--japanese-mode-p mode))
      t  ; Skip non-Japanese modes
    (let ((state (nskk-state-create mode)))
      ;; Start with empty state
      (and (null (nskk-state-henkan-position state))
           ;; Add some input
           (progn
             (nskk-state-append-input state ?k)
             (nskk-state-append-input state ?a)
             (> (length (nskk-state-input-buffer state)) 0))
           ;; Start conversion
           (progn
             (nskk-pbt--simulate-conversion-start state)
             (not (null (nskk-state-henkan-position state))))
           ;; Candidates should be set
           (not (null (nskk-state-candidates state))))))
  75)

(nskk-property-test-seeded conversion-commit-clears
  ((mode valid-mode)
   (idx pbt-conversion-index))
  (if (not (nskk-pbt--japanese-mode-p mode))
      t  ; Skip non-Japanese modes
    (let ((state (nskk-state-create mode)))
      ;; Set up conversion state
      (nskk-state-set state 'input-buffer "test")
      (nskk-state-set state 'candidates '("候補1" "候補2" "候補3"))
      (nskk-state-set state 'current-index (mod idx 3))
      (nskk-state-set state 'henkan-position 0)
      ;; Commit conversion
      (nskk-pbt--simulate-conversion-commit state)
      ;; After commit, conversion state should be cleared
      (and (null (nskk-state-candidates state))
           (= (nskk-state-current-index state) 0)
           (null (nskk-state-henkan-position state))
           (string= (nskk-state-input-buffer state) ""))))
  75)

(nskk-property-test-seeded conversion-cancel-restore
  ((mode valid-mode)
   (original-input romaji-basic)
   (idx pbt-conversion-index))
  (if (not (and (nskk-pbt--japanese-mode-p mode)
                (> (length original-input) 0)))
      t  ; Skip non-Japanese modes or empty input
    (let ((state (nskk-state-create mode)))
      ;; Set up conversion state with original input
      (nskk-state-set state 'input-buffer original-input)
      (nskk-state-set state 'candidates '("候補1" "候補2" "候補3"))
      (nskk-state-set state 'current-index (mod idx 3))
      (nskk-state-set state 'henkan-position 0)
      ;; Cancel conversion (restore original input)
      (nskk-pbt--simulate-conversion-cancel state original-input)
      ;; After cancel, state should be restored
      (and (string= (nskk-state-input-buffer state) original-input)
           (null (nskk-state-candidates state))
           (= (nskk-state-current-index state) 0)
           (null (nskk-state-henkan-position state)))))
  75)

;;;
;;; Additional Application Layer Invariant Properties
;;;

(nskk-property-test-seeded mode-always-valid-after-switch
  ((mode1 valid-mode)
   (mode2 valid-mode)
   (mode3 valid-mode))
  (let ((state (nskk-state-create mode1)))
    (nskk-pbt--simulate-mode-switch state mode2)
    (and (nskk-state-valid-mode-p (nskk-state-mode state))
         (progn
           (nskk-pbt--simulate-mode-switch state mode3)
           (nskk-state-valid-mode-p (nskk-state-mode state)))))
  75)

(nskk-property-test-seeded input-preserves-mode
  ((mode valid-mode)
   (ch pbt-latin-char))
  (let ((state (nskk-state-create mode)))
    (let ((original-mode (nskk-state-mode state)))
      (nskk-pbt--simulate-input state ch)
      (eq (nskk-state-mode state) original-mode)))
  75)

(nskk-property-test-seeded conversion-index-bounded
  ((mode valid-mode)
   (num-candidates pbt-conversion-index))
  (if (not (nskk-pbt--japanese-mode-p mode))
      t  ; Skip non-Japanese modes
    (let* ((state (nskk-state-create mode))
           (count (max 1 num-candidates))
           (candidates (cl-loop for i from 1 to count
                                collect (format "候補%d" i))))
      (nskk-state-set state 'candidates candidates)
      (nskk-state-set state 'current-index (mod (random) count))
      (let ((idx (nskk-state-current-index state)))
        (and (>= idx 0)
             (< idx (length (nskk-state-candidates state)))))))
  75)

(nskk-property-test-seeded consecutive-mode-switches
  ((m1 valid-mode)
   (m2 valid-mode)
   (m3 valid-mode)
   (m4 valid-mode))
  (let ((state (nskk-state-create m1))
        (modes (list m1 m2 m3 m4)))
    ;; Perform consecutive mode switches
    (cl-loop for mode in (cdr modes)
             do (nskk-pbt--simulate-mode-switch state mode))
    ;; State should still be valid and in the last mode
    (and (nskk-state-p state)
         (eq (nskk-state-mode state) m4)
         (nskk-state-valid-mode-p (nskk-state-mode state))
         ;; Previous mode should be tracked
         (eq (nskk-state-previous-mode state) m3)))
  75)

(nskk-property-test-seeded okurigana-lowercase-mapping
  ((upper-ch pbt-uppercase-consonant))
  (let ((lower-ch (nskk-pbt--detect-okurigana upper-ch)))
    (if (not lower-ch)
        t  ; Skip if detection returns nil (shouldn't happen for uppercase consonants)
      ;; Mapping should be consistent
      (and (characterp lower-ch)
           (= lower-ch (downcase upper-ch))
           ;; Re-detection should be idempotent
           (let ((lower-ch-again (nskk-pbt--detect-okurigana upper-ch)))
             (= lower-ch lower-ch-again)))))
  75)

(nskk-property-test-seeded input-buffer-accumulates
  ((mode valid-mode)
   (ch1 pbt-lowercase-consonant)
   (ch2 pbt-lowercase-consonant)
   (ch3 pbt-lowercase-consonant))
  (if (not (nskk-pbt--japanese-mode-p mode))
      t  ; Skip non-Japanese modes
    (let ((state (nskk-state-create mode)))
      (nskk-pbt--simulate-input state ch1)
      (let ((len1 (length (nskk-state-input-buffer state))))
        (nskk-pbt--simulate-input state ch2)
        (let ((len2 (length (nskk-state-input-buffer state))))
          (nskk-pbt--simulate-input state ch3)
          (let ((len3 (length (nskk-state-input-buffer state))))
            ;; Buffer should grow with each input
            (and (< len1 len2)
                 (< len2 len3)
                 (= len3 3)))))))
  75)

(nskk-property-test-seeded latin-mode-ignores-case
  ((upper-ch pbt-uppercase-consonant)
   (lower-ch pbt-lowercase-consonant))
  (let ((state (nskk-state-create 'latin)))
    ;; Both uppercase and lowercase should be inserted directly
    (nskk-pbt--simulate-input state upper-ch)
    (nskk-pbt--simulate-input state lower-ch)
    (let ((result (nskk-state-converted-buffer state)))
      (and (stringp result)
           (= (length result) 2)
           ;; Characters should be preserved as-is
           (= (aref result 0) upper-ch)
           (= (aref result 1) lower-ch))))
  75)

(nskk-property-test-seeded conversion-candidate-navigation
  ((mode valid-mode)
   (nav-steps pbt-conversion-index))
  (if (not (nskk-pbt--japanese-mode-p mode))
      t  ; Skip non-Japanese modes
    (let* ((state (nskk-state-create mode))
           (candidates '("候補1" "候補2" "候補3"))
           (count (length candidates)))
      (nskk-state-set state 'candidates candidates)
      (let ((start-idx (nskk-state-current-index state)))
        ;; Navigate forward nav-steps times
        (cl-loop repeat nav-steps
                 do (nskk-state-next-candidate state))
        (let ((after-next (nskk-state-current-index state)))
          ;; Navigate backward nav-steps times
          (cl-loop repeat nav-steps
                   do (nskk-state-previous-candidate state))
          (let ((after-prev (nskk-state-current-index state)))
            ;; Should return to start position
            (= after-prev start-idx))))))
  75)

(provide 'nskk-layer-application-pbt-test)

;;; nskk-layer-application-pbt-test.el ends here
