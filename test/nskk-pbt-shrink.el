;;; nskk-pbt-shrink.el --- Shrinking Strategies for NSKK PBT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: Japanese, input, method, test, property-based, shrinking
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

;; This file provides shrinking strategies for Property-Based Testing (PBT)
;; in NSKK. Shrinking is the process of reducing a failing test case to its
;; minimal form while preserving the failure.
;;
;; Features:
;; - Sequence shrinking (binary search approach)
;; - String shrinking (UTF-8 safe)
;; - State shrinking (mode simplification)
;; - Generic shrinking dispatcher
;; - Iterative shrinking loop with progress reporting

;;; Code:

(require 'cl-lib)
(require 'nskk-test-framework)
(require 'nskk-state)


;;;;
;;;; Customization
;;;;

(defgroup nskk-pbt-shrink nil
  "NSKK PBT shrinking configuration."
  :prefix "nskk-pbt-shrink-"
  :group 'nskk-test)

(defcustom nskk-pbt-shrink-max-iterations 100
  "Maximum number of shrinking iterations.
Prevents infinite loops in pathological cases."
  :type 'integer
  :group 'nskk-pbt-shrink)

(defcustom nskk-pbt-shrink-verbose nil
  "Enable verbose output during shrinking."
  :type 'boolean
  :group 'nskk-pbt-shrink)


;;;;
;;;; Sequence Shrinking
;;;;

(defun nskk-shrink-sequence (sequence property-fn)
  "Shrink SEQUENCE to minimal failing case for PROPERTY-FN.
Uses binary search approach: remove from middle first, then from ends.
Returns minimal sequence that still fails PROPERTY-FN."
  (let ((current sequence)
        (iterations 0)
        (improved t))
    (while (and improved
                (< iterations nskk-pbt-shrink-max-iterations)
                (> (length current) 1))
      (setq improved nil)
      (cl-incf iterations)

      ;; Try to shrink using binary approach
      (let ((shrunk (nskk--shrink-sequence-step current property-fn)))
        (when (and shrunk
                   (< (length shrunk) (length current)))
          (setq current shrunk
                improved t))

        ;; Report progress if verbose
        (when nskk-pbt-shrink-verbose
          (nskk-shrink-report iterations current sequence))))

    current))

(defun nskk--shrink-sequence-step (sequence property-fn)
  "Perform one shrinking step on SEQUENCE for PROPERTY-FN.
Tries removing from middle first, then from ends.
Returns smaller sequence that still fails PROPERTY-FN, or nil."
  (when (< (length sequence) 2)
    (cl-return-from nskk--shrink-sequence-step nil))

  (let* ((len (length sequence))
         (mid (/ len 2)))

    ;; Strategy 1: Remove middle portion (binary search style)
    (let ((without-middle (append (cl-subseq sequence 0 (max 0 (1- mid)))
                                  (cl-subseq sequence (min len (1+ mid))))))
      (when (and (> (length without-middle) 0)
                 (funcall property-fn without-middle))
        (cl-return-from nskk--shrink-sequence-step without-middle)))

    ;; Strategy 2: Remove from left half
    (let ((without-left (cl-subseq sequence mid)))
      (when (funcall property-fn without-left)
        (cl-return-from nskk--shrink-sequence-step without-left)))

    ;; Strategy 3: Remove from right half
    (let ((without-right (cl-subseq sequence 0 mid)))
      (when (funcall property-fn without-right)
        (cl-return-from nskk--shrink-sequence-step without-right)))

    ;; Strategy 4: Remove first element
    (let ((without-first (cl-subseq sequence 1)))
      (when (funcall property-fn without-first)
        (cl-return-from nskk--shrink-sequence-step without-first)))

    ;; Strategy 5: Remove last element
    (let ((without-last (cl-subseq sequence 0 (1- len))))
      (when (funcall property-fn without-last)
        (cl-return-from nskk--shrink-sequence-step without-last)))

    ;; Strategy 6: Try removing individual elements
    (cl-block element-removal
      (dotimes (i len)
        (let ((without-i (append (cl-subseq sequence 0 i)
                                 (cl-subseq sequence (1+ i)))))
          (when (and (> (length without-i) 0)
                     (funcall property-fn without-i))
            (cl-return-from nskk--shrink-sequence-step without-i)))))

    ;; No further shrinking possible
    nil))


;;;;
;;;; String Shrinking
;;;;

(defun nskk-shrink-string (string property-fn)
  "Shrink STRING to minimal failing case for PROPERTY-FN.
Removes characters from middle first, then from ends.
Maintains valid UTF-8 encoding."
  (let ((current string)
        (iterations 0)
        (improved t))
    (while (and improved
                (< iterations nskk-pbt-shrink-max-iterations)
                (> (length current) 1))
      (setq improved nil)
      (cl-incf iterations)

      (let ((shrunk (nskk--shrink-string-step current property-fn)))
        (when (and shrunk
                   (< (length shrunk) (length current)))
          (setq current shrunk
                improved t))

        (when nskk-pbt-shrink-verbose
          (nskk-shrink-report iterations current string))))

    current))

(defun nskk--shrink-string-step (string property-fn)
  "Perform one shrinking step on STRING for PROPERTY-FN.
Returns smaller string that still fails PROPERTY-FN, or nil."
  (when (< (length string) 1)
    (cl-return-from nskk--shrink-string-step nil))

  ;; Handle UTF-8: work with characters, not bytes
  (let* ((chars (string-to-list string))
         (len (length chars)))

    (when (< len 2)
      (cl-return-from nskk--shrink-string-step nil))

    (let ((mid (/ len 2)))

      ;; Strategy 1: Remove from middle
      (let ((without-middle
             (concat (cl-subseq string 0 (max 0 (1- mid)))
                     (cl-subseq string (min len (1+ mid))))))
        (when (and (> (length without-middle) 0)
                   (funcall property-fn without-middle))
          (cl-return-from nskk--shrink-string-step without-middle)))

      ;; Strategy 2: Remove left half
      (let ((without-left (cl-subseq string mid)))
        (when (funcall property-fn without-left)
          (cl-return-from nskk--shrink-string-step without-left)))

      ;; Strategy 3: Remove right half
      (let ((without-right (cl-subseq string 0 mid)))
        (when (funcall property-fn without-right)
          (cl-return-from nskk--shrink-string-step without-right)))

      ;; Strategy 4: Remove first character
      (let ((without-first (cl-subseq string 1)))
        (when (funcall property-fn without-first)
          (cl-return-from nskk--shrink-string-step without-first)))

      ;; Strategy 5: Remove last character
      (let ((without-last (cl-subseq string 0 (1- len))))
        (when (funcall property-fn without-last)
          (cl-return-from nskk--shrink-string-step without-last)))

      ;; Strategy 6: Try removing individual characters
      (cl-block char-removal
        (dotimes (i len)
          (let ((without-i (concat (cl-subseq string 0 i)
                                   (cl-subseq string (1+ i)))))
            (when (and (> (length without-i) 0)
                       (funcall property-fn without-i))
              (cl-return-from nskk--shrink-string-step without-i)))))

      nil)))


;;;;
;;;; State Shrinking
;;;;

(defconst nskk--mode-simplification-order
  '(ascii latin hiragana katakana katakana-半角 abbrev)
  "Order of mode complexity for shrinking (simpler modes first).")

(defun nskk-shrink-state (state property-fn)
  "Shrink NSKK STATE to minimal failing case for PROPERTY-FN.
Simplifies mode, clears buffers, and reduces candidate lists."
  (let ((current state)
        (iterations 0)
        (improved t))
    (while (and improved
                (< iterations nskk-pbt-shrink-max-iterations))
      (setq improved nil)
      (cl-incf iterations)

      (let ((shrunk (nskk--shrink-state-step current property-fn)))
        (when shrunk
          (setq current shrunk
                improved t))

        (when nskk-pbt-shrink-verbose
          (nskk-shrink-report iterations current state))))

    current))

(defun nskk--shrink-state-step (state property-fn)
  "Perform one shrinking step on STATE for PROPERTY-FN.
Returns simplified state that still fails PROPERTY-FN, or nil."
  (unless (nskk-state-p state)
    (cl-return-from nskk--shrink-state-step nil))

  ;; Create a copy of the state for modification
  (let ((shrunk-state (nskk--copy-state state)))

    ;; Strategy 1: Simplify mode
    (let ((simpler-mode (nskk--get-simpler-mode (nskk-state-mode shrunk-state))))
      (when simpler-mode
        (setf (nskk-state-mode shrunk-state) simpler-mode)
        (when (funcall property-fn shrunk-state)
          (cl-return-from nskk--shrink-state-step shrunk-state))
        ;; Revert if not failing
        (setq shrunk-state (nskk--copy-state state))))

    ;; Strategy 2: Clear input buffer
    (when (> (length (nskk-state-input-buffer shrunk-state)) 0)
      (setf (nskk-state-input-buffer shrunk-state) "")
      (when (funcall property-fn shrunk-state)
        (cl-return-from nskk--shrink-state-step shrunk-state))
      (setq shrunk-state (nskk--copy-state state)))

    ;; Strategy 3: Clear converted buffer
    (when (> (length (nskk-state-converted-buffer shrunk-state)) 0)
      (setf (nskk-state-converted-buffer shrunk-state) "")
      (when (funcall property-fn shrunk-state)
        (cl-return-from nskk--shrink-state-step shrunk-state))
      (setq shrunk-state (nskk--copy-state state)))

    ;; Strategy 4: Reduce candidate list
    (when (> (length (nskk-state-candidates shrunk-state)) 1)
      (let ((candidates (nskk-state-candidates shrunk-state)))
        ;; Try keeping only first candidate
        (setf (nskk-state-candidates shrunk-state) (list (car candidates)))
        (when (funcall property-fn shrunk-state)
          (cl-return-from nskk--shrink-state-step shrunk-state))
        (setq shrunk-state (nskk--copy-state state))))

    ;; Strategy 5: Clear undo/redo stacks
    (when (nskk-state-undo-stack shrunk-state)
      (setf (nskk-state-undo-stack shrunk-state) nil)
      (when (funcall property-fn shrunk-state)
        (cl-return-from nskk--shrink-state-step shrunk-state))
      (setq shrunk-state (nskk--copy-state state)))

    (when (nskk-state-redo-stack shrunk-state)
      (setf (nskk-state-redo-stack shrunk-state) nil)
      (when (funcall property-fn shrunk-state)
        (cl-return-from nskk--shrink-state-step shrunk-state))
      (setq shrunk-state (nskk--copy-state state)))

    ;; Strategy 6: Clear metadata
    (when (nskk-state-metadata shrunk-state)
      (setf (nskk-state-metadata shrunk-state) nil)
      (when (funcall property-fn shrunk-state)
        (cl-return-from nskk--shrink-state-step shrunk-state)))

    nil))

(defun nskk--copy-state (state)
  "Create a deep copy of STATE."
  (when (nskk-state-p state)
    (make-nskk-state
     :mode (nskk-state-mode state)
     :input-buffer (nskk-state-input-buffer state)
     :converted-buffer (nskk-state-converted-buffer state)
     :candidates (copy-sequence (nskk-state-candidates state))
     :current-index (nskk-state-current-index state)
     :henkan-position (nskk-state-henkan-position state)
     :marker-position (nskk-state-marker-position state)
     :previous-mode (nskk-state-previous-mode state)
     :undo-stack (copy-tree (nskk-state-undo-stack state))
     :redo-stack (copy-tree (nskk-state-redo-stack state))
     :metadata (copy-tree (nskk-state-metadata state)))))

(defun nskk--get-simpler-mode (current-mode)
  "Get a simpler mode than CURRENT-MODE.
Returns nil if CURRENT-MODE is already the simplest."
  (let ((idx (cl-position current-mode nskk--mode-simplification-order)))
    (when (and idx (> idx 0))
      (nth (1- idx) nskk--mode-simplification-order))))


;;;;
;;;; Generic Shrinking Dispatcher
;;;;

(defun nskk-shrink (value property-fn)
  "Generic shrinker that dispatches by type.
Shrinks VALUE to minimal failing case for PROPERTY-FN.
Returns smaller value of same type, or original if cannot shrink further."
  (pcase value
    ((pred stringp)    (nskk-shrink-string value property-fn))
    ((pred listp)      (nskk-shrink-sequence value property-fn))
    ((pred vectorp)
     (let ((shrunk (nskk-shrink-sequence (append value nil) property-fn)))
       (if (listp shrunk) (vconcat shrunk) shrunk)))
    ((pred nskk-state-p) (nskk-shrink-state value property-fn))
    ((pred integerp)   (nskk-shrink-integer value property-fn))
    ((pred floatp)     (nskk-shrink-float value property-fn))
    (_ value)))

(defun nskk-shrink-integer (value property-fn)
  "Shrink integer VALUE to minimal failing case for PROPERTY-FN.
Tries values closer to 0."
  (let ((current value)
        (iterations 0)
        (improved t))
    (while (and improved
                (< iterations nskk-pbt-shrink-max-iterations)
                (not (zerop current)))
      (setq improved nil)
      (cl-incf iterations)

      ;; Try halving the value
      (let ((half (/ current 2)))
        (unless (zerop half)
          (when (funcall property-fn half)
            (setq current half
                  improved t))))

      ;; Try decrementing
      (unless improved
        (when (and (> current 1)
                   (funcall property-fn (1- current)))
          (setq current (1- current)
                improved t)))

      ;; Try incrementing (for negative values)
      (unless improved
        (when (and (< current -1)
                   (funcall property-fn (1+ current)))
          (setq current (1+ current)
                improved t)))

      (when nskk-pbt-shrink-verbose
        (nskk-shrink-report iterations current value)))

    current))

(defun nskk-shrink-float (value property-fn)
  "Shrink float VALUE to minimal failing case for PROPERTY-FN.
Tries values closer to 0.0."
  (let ((current value)
        (iterations 0)
        (improved t))
    (while (and improved
                (< iterations nskk-pbt-shrink-max-iterations)
                (> (abs current) 0.0001))
      (setq improved nil)
      (cl-incf iterations)

      ;; Try halving the value
      (let ((half (/ current 2.0)))
        (when (> (abs half) 0.0001)
          (when (funcall property-fn half)
            (setq current half
                  improved t))))

      (when nskk-pbt-shrink-verbose
        (nskk-shrink-report iterations current value)))

    current))


;;;;
;;;; Shrinking Loop
;;;;

(defun nskk-shrink-loop (initial-value property-fn &optional type-hint)
  "Iteratively shrink INITIAL-VALUE until no improvement for PROPERTY-FN.
TYPE-HINT can be \\='sequence, \\='string, \\='state, or nil for auto-detection.
Returns minimal failing case."
  (let ((current initial-value)
        (iterations 0)
        (shrinks 0)
        (improved t))

    ;; Select appropriate shrinker based on type hint or auto-detection
    (let ((shrinker (pcase type-hint
                      ('sequence #'nskk-shrink-sequence)
                      ('string #'nskk-shrink-string)
                      ('state #'nskk-shrink-state)
                      (_ #'nskk-shrink))))

      (while (and improved
                  (< iterations nskk-pbt-shrink-max-iterations))
        (setq improved nil)
        (cl-incf iterations)

        (let* ((before-size (nskk--measure-size current))
               (shrunk (funcall shrinker current property-fn))
               (after-size (nskk--measure-size shrunk)))

          (when (and shrunk
                     (< after-size before-size))
            (setq current shrunk
                  improved t)
            (cl-incf shrinks)

            (when nskk-pbt-shrink-verbose
              (message "[NSKK Shrink] Iteration %d: size %d -> %d"
                       iterations before-size after-size)))))

      ;; Final report
      (when nskk-pbt-shrink-verbose
        (message "[NSKK Shrink] Completed: %d iterations, %d shrinks"
                 iterations shrinks)
        (message "[NSKK Shrink] Minimal case: %S" current))

      current)))

(defun nskk--measure-size (value)
  "Measure the size of VALUE for shrinking comparison."
  (pcase value
    ((or (pred stringp) (pred listp) (pred vectorp)) (length value))
    ((pred nskk-state-p)
     (+ (length (nskk-state-input-buffer value))
        (length (nskk-state-converted-buffer value))
        (length (nskk-state-candidates value))
        (if (nskk-state-undo-stack value) 1 0)
        (if (nskk-state-redo-stack value) 1 0)
        (if (nskk-state-metadata value) 1 0)))
    ((pred numberp) (abs value))
    (_ 1)))


;;;;
;;;; Shrinking Report
;;;;

(defun nskk-shrink-report (iterations current original)
  "Display shrinking progress report.
ITERATIONS is the current iteration count.
CURRENT is the current shrunk value.
ORIGINAL is the original value before shrinking."
  (let* ((orig-size (nskk--measure-size original))
         (curr-size (nskk--measure-size current))
         (reduction-percent (if (> orig-size 0)
                               (* 100 (/ (float (- orig-size curr-size)) orig-size))
                             0)))
    (message "[NSKK Shrink] Iteration %d: %d -> %d (%.1f%% reduction)"
             iterations orig-size curr-size reduction-percent)))

(defun nskk-shrink-report-final (original final)
  "Display final shrinking report.
ORIGINAL is the value before shrinking.
FINAL is the minimal value after shrinking."
  (let ((orig-size (nskk--measure-size original))
        (final-size (nskk--measure-size final)))
    (message "[NSKK Shrink] Final result:")
    (message "  Original size: %d" orig-size)
    (message "  Final size:    %d" final-size)
    (message "  Reduction:     %.1f%%"
             (if (> orig-size 0)
                 (* 100 (/ (float (- orig-size final-size)) orig-size))
               0))
    (message "  Minimal case:  %S" final)))


;;;;
;;;; Integration with PBT Framework
;;;;

(defun nskk-pbt-shrink-failing-case (value property-fn &optional type-hint)
  "Shrink a failing test case VALUE for PROPERTY-FN.
This is the main entry point for shrinking in PBT.
TYPE-HINT specifies the type of VALUE for optimized shrinking."
  (when nskk-pbt-shrink-verbose
    (message "[NSKK Shrink] Starting shrink process...")
    (message "[NSKK Shrink] Original value: %S" value))

  (let ((result (nskk-shrink-loop value property-fn type-hint)))
    (when nskk-pbt-shrink-verbose
      (nskk-shrink-report-final value result))
    result))

(provide 'nskk-pbt-shrink)

;;; nskk-pbt-shrink.el ends here
