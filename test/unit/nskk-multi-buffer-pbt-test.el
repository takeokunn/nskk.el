;;; nskk-multi-buffer-pbt-test.el --- Multi-buffer PBT tests -*- lexical-binding: t; -*-

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

;; Property-based tests for buffer-local state isolation.
;;
;; This file tests that NSKK state is properly buffer-local and
;; that changes in one buffer do not leak into another.
;;
;; Properties tested:
;; - buffer-local-state-isolation: State changes in one buffer don't affect another
;; - buffer-local-mode-isolation: Mode changes in one buffer don't affect another
;; - buffer-local-romaji-isolation: Romaji buffer in one doesn't leak to another

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-state)


;;;;
;;;; Helper Functions
;;;;

(defun nskk-pbt--create-buffer-with-state (mode)
  "Create a temp buffer with nskk-current-state initialized to MODE."
  (let ((buf (generate-new-buffer " *nskk-pbt-test*")))
    (with-current-buffer buf
      (setq-local nskk-current-state (nskk-state-create mode)))
    buf))

(defun nskk-pbt--cleanup-test-buffer (buf)
  "Kill test buffer BUF safely."
  (when (buffer-live-p buf)
    (kill-buffer buf)))


;;;;
;;;; Property 1: Buffer-Local State Isolation
;;;;

(ert-deftest nskk-state-machine-buffer-local-state-isolation ()
  "State changes in one buffer do not affect another."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((mode1 (nskk-pbt--generate-valid-mode))
             (mode2 (nskk-pbt--generate-valid-mode))
             (buf1 (nskk-pbt--create-buffer-with-state mode1))
             (buf2 (nskk-pbt--create-buffer-with-state mode2)))
        (unwind-protect
            (progn
              ;; Modify state in buffer 1
              (with-current-buffer buf1
                (nskk-state-set nskk-current-state 'input-buffer "hello")
                (nskk-state-set nskk-current-state 'candidates '("a" "b" "c"))
                (nskk-state-set nskk-current-state 'henkan-position 5))
              ;; Verify buffer 2 is unaffected
              (with-current-buffer buf2
                (let ((input2 (nskk-state-input-buffer nskk-current-state))
                      (cands2 (nskk-state-candidates nskk-current-state))
                      (henkan2 (nskk-state-henkan-position nskk-current-state)))
                  (unless (and (string= input2 "")
                               (null cands2)
                               (null henkan2))
                    (push (list :mode1 mode1 :mode2 mode2
                                :buf2-input input2
                                :buf2-candidates cands2
                                :buf2-henkan henkan2)
                          failures)))))
          (nskk-pbt--cleanup-test-buffer buf1)
          (nskk-pbt--cleanup-test-buffer buf2))))
    (when failures
      (ert-fail (format "Buffer state isolation failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Property 2: Buffer-Local Mode Isolation
;;;;

(ert-deftest nskk-state-machine-buffer-local-mode-isolation ()
  "Mode changes in one buffer do not affect another."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((initial-mode (nskk-pbt--generate-valid-mode))
             (buf1 (nskk-pbt--create-buffer-with-state initial-mode))
             (buf2 (nskk-pbt--create-buffer-with-state initial-mode)))
        (unwind-protect
            (progn
              ;; Change mode in buffer 1 multiple times
              (with-current-buffer buf1
                (dotimes (_ (nskk-pbt--random-int 2 5))
                  (let ((new-mode (nskk-pbt--generate-valid-mode)))
                    (nskk-state-set nskk-current-state 'mode new-mode))))
              ;; Verify buffer 2 still has the original mode
              (with-current-buffer buf2
                (let ((mode2 (nskk-state-mode nskk-current-state)))
                  (unless (eq mode2 initial-mode)
                    (push (list :initial-mode initial-mode
                                :buf2-mode mode2
                                :buf1-mode (with-current-buffer buf1
                                             (nskk-state-mode nskk-current-state)))
                          failures)))))
          (nskk-pbt--cleanup-test-buffer buf1)
          (nskk-pbt--cleanup-test-buffer buf2))))
    (when failures
      (ert-fail (format "Buffer mode isolation failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Property 3: Buffer-Local Romaji Isolation
;;;;

(ert-deftest nskk-state-machine-buffer-local-romaji-isolation ()
  "Romaji buffer in one buffer does not leak to another."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((buf1 (nskk-pbt--create-buffer-with-state 'hiragana))
             (buf2 (nskk-pbt--create-buffer-with-state 'hiragana))
             (romaji-input (nskk-pbt--generate-input-buffer 10)))
        (unwind-protect
            (progn
              ;; Set input-buffer (simulating romaji accumulation) in buffer 1
              (with-current-buffer buf1
                (nskk-state-set nskk-current-state 'input-buffer romaji-input))
              ;; Verify buffer 2 input-buffer is still empty
              (with-current-buffer buf2
                (let ((input2 (nskk-state-input-buffer nskk-current-state)))
                  (unless (string= input2 "")
                    (push (list :romaji-set romaji-input
                                :buf2-input input2)
                          failures)))))
          (nskk-pbt--cleanup-test-buffer buf1)
          (nskk-pbt--cleanup-test-buffer buf2))))
    (when failures
      (ert-fail (format "Romaji buffer isolation failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Additional Property: Multiple Buffers Independent State
;;;;

(ert-deftest nskk-state-machine-multiple-buffers-independent ()
  "Creating and modifying state across multiple buffers remains independent."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((num-buffers (nskk-pbt--random-int 2 5))
             (buffers nil)
             (modes nil))
        (unwind-protect
            (progn
              ;; Create buffers with random modes
              (dotimes (_ num-buffers)
                (let ((mode (nskk-pbt--generate-valid-mode)))
                  (push (nskk-pbt--create-buffer-with-state mode) buffers)
                  (push mode modes)))
              (setq buffers (nreverse buffers))
              (setq modes (nreverse modes))
              ;; Modify each buffer independently
              (cl-loop for buf in buffers
                       for i from 0
                       do (with-current-buffer buf
                            (nskk-state-set nskk-current-state 'input-buffer
                                            (format "buf-%d" i))))
              ;; Verify each buffer has its own input
              (cl-loop for buf in buffers
                       for i from 0
                       for expected-mode in modes
                       do (with-current-buffer buf
                            (let ((input (nskk-state-input-buffer nskk-current-state))
                                  (mode (nskk-state-mode nskk-current-state)))
                              (unless (and (string= input (format "buf-%d" i))
                                           (eq mode expected-mode))
                                (push (list :buffer-idx i
                                            :expected-input (format "buf-%d" i)
                                            :actual-input input
                                            :expected-mode expected-mode
                                            :actual-mode mode)
                                      failures))))))
          ;; Cleanup all buffers
          (dolist (buf buffers)
            (nskk-pbt--cleanup-test-buffer buf)))))
    (when failures
      (ert-fail (format "Multi-buffer independence failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Seeded PBT: Buffer state isolation — each buffer's mode is independent
;;;;

(nskk-property-test-seeded buffer-state-isolation
  ((scenario multi-buffer-scenario))
  (let* ((states (mapcar (lambda (entry)
                           (nskk-state-create (plist-get entry :mode)))
                         scenario))
         ;; Verify initial mode matches the scenario entry
         (modes-correct
          (cl-every (lambda (state entry)
                      (eq (nskk-state-mode state) (plist-get entry :mode)))
                    states scenario)))
    ;; Now mutate the first state's mode and verify others are unaffected
    (when (and modes-correct (>= (length states) 2))
      (let* ((state1 (car states))
             (rest-states (cdr states))
             (rest-modes-before (mapcar #'nskk-state-mode rest-states))
             (new-mode (if (eq (nskk-state-mode state1) 'hiragana) 'ascii 'hiragana)))
        (nskk-state-set state1 'mode new-mode)
        ;; After mutating state1, remaining states should be unchanged
        (cl-every (lambda (state mode-before)
                    (eq (nskk-state-mode state) mode-before))
                  rest-states rest-modes-before))))
  30)


;;;;
;;;; Seeded PBT: Mode independence — two states hold independent candidate lists
;;;;

(nskk-property-test-seeded mode-independence-candidates
  ((mode1 valid-mode)
   (mode2 valid-mode))
  (let* ((state1 (nskk-state-create mode1))
         (state2 (nskk-state-create mode2))
         (cands1 '("漢字" "感じ" "幹事"))
         (cands2 '("日本" "二本" "入本")))
    (nskk-state-set-candidates state1 cands1)
    (nskk-state-set-candidates state2 cands2)
    (and (equal (nskk-state-candidates state1) cands1)
         (equal (nskk-state-candidates state2) cands2)
         (not (equal (nskk-state-candidates state1)
                     (nskk-state-candidates state2)))))
  30)


(provide 'nskk-multi-buffer-pbt-test)

;;; nskk-multi-buffer-pbt-test.el ends here
