;;; nskk-okurigana-pbt-test.el --- Okurigana PBT tests -*- lexical-binding: t; -*-

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

;; Property-based tests for okurigana detection and state management.
;;
;; This file tests invariants of the okurigana subsystem:
;; - Uppercase letters A-Z are detected as okurigana markers
;; - Lowercase letters are not detected as okurigana markers
;; - Non-alpha characters are not detected as okurigana markers
;; - Setting and getting okurigana in state is consistent
;;
;; Properties tested:
;; - okurigana-uppercase-detected: All A-Z are detected as okurigana markers
;; - okurigana-lowercase-not-detected: Lowercase a-z return nil
;; - okurigana-non-alpha-not-detected: Numbers, symbols return nil
;; - okurigana-state-set-get-roundtrip: Set/get okurigana in state is consistent

;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-state)
(require 'nskk-input)


;;;;
;;;; Helper Data
;;;;

(defconst nskk-pbt--uppercase-chars
  (string-to-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  "All uppercase ASCII letters.")

(defconst nskk-pbt--lowercase-chars
  (string-to-list "abcdefghijklmnopqrstuvwxyz")
  "All lowercase ASCII letters.")

(defconst nskk-pbt--non-alpha-chars
  (append (string-to-list "0123456789")
          (string-to-list "!@#$%^&*()-=[]{}|;':\",./<>?")
          (list ?\t ?\n ?\s))
  "Non-alphabetic characters for testing.")


;;;;
;;;; Property 1: Uppercase Characters Detected as Okurigana
;;;;

(ert-deftest nskk-state-machine-okurigana-uppercase-detected ()
  "All uppercase A-Z are detected as okurigana markers."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((char (nskk-pbt--random-choice nskk-pbt--uppercase-chars))
             (result (nskk-detect-okurigana-char char)))
        (unless (and result
                     (characterp result)
                     (= result (downcase char)))
          (push (list :char char
                      :char-name (char-to-string char)
                      :result result
                      :expected (downcase char))
                failures))))
    (when failures
      (ert-fail (format "Uppercase okurigana detection failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Property 2: Lowercase Characters Not Detected as Okurigana
;;;;

(ert-deftest nskk-state-machine-okurigana-lowercase-not-detected ()
  "Lowercase a-z are not detected as okurigana markers."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((char (nskk-pbt--random-choice nskk-pbt--lowercase-chars))
             (result (nskk-detect-okurigana-char char)))
        (when result
          (push (list :char char
                      :char-name (char-to-string char)
                      :result result)
                failures))))
    (when failures
      (ert-fail (format "Lowercase chars incorrectly detected for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Property 3: Non-Alpha Characters Not Detected as Okurigana
;;;;

(ert-deftest nskk-state-machine-okurigana-non-alpha-not-detected ()
  "Numbers, symbols, and other non-alpha characters return nil."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((char (nskk-pbt--random-choice nskk-pbt--non-alpha-chars))
             (result (nskk-detect-okurigana-char char)))
        (when result
          (push (list :char char
                      :result result)
                failures))))
    (when failures
      (ert-fail (format "Non-alpha chars incorrectly detected for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Property 4: Okurigana State Set-Get Roundtrip
;;;;

(ert-deftest nskk-state-machine-okurigana-state-set-get-roundtrip ()
  "Setting and getting okurigana in state is consistent."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((state (nskk-state-create 'hiragana))
             (okuri-char (nskk-pbt--random-choice nskk-pbt--lowercase-chars)))
        ;; Set okurigana
        (nskk-state-set-okurigana state okuri-char)
        ;; Get okurigana
        (let ((retrieved (nskk-state-get-okurigana state)))
          (unless (equal retrieved okuri-char)
            (push (list :set okuri-char
                        :retrieved retrieved)
                  failures)))))
    (when failures
      (ert-fail (format "Okurigana set-get roundtrip failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Additional Property: Okurigana Detection Determinism
;;;;

(ert-deftest nskk-state-machine-okurigana-detection-deterministic ()
  "Calling okurigana detection twice on the same char gives the same result."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (let* ((all-chars (append nskk-pbt--uppercase-chars
                                nskk-pbt--lowercase-chars
                                nskk-pbt--non-alpha-chars))
             (char (nskk-pbt--random-choice all-chars))
             (result1 (nskk-detect-okurigana-char char))
             (result2 (nskk-detect-okurigana-char char)))
        (unless (equal result1 result2)
          (push (list :char char :result1 result1 :result2 result2)
                failures))))
    (when failures
      (ert-fail (format "Okurigana detection not deterministic for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


;;;;
;;;; Additional Property: Okurigana Nil State Safety
;;;;

(ert-deftest nskk-state-machine-okurigana-nil-state-safe ()
  "Okurigana operations on nil state do not crash."
  (let ((runs 50)
        (failures nil))
    (dotimes (_ runs)
      (condition-case err
          (progn
            ;; These should return nil without crash
            (nskk-state-set-okurigana nil (nskk-pbt--random-choice nskk-pbt--lowercase-chars))
            (nskk-state-get-okurigana nil)
            ;; Also test detect on nil
            (nskk-detect-okurigana-char nil))
        (error
         (push (list :error err) failures))))
    (when failures
      (ert-fail (format "Nil state safety failed for %d cases:\n%S"
                        (length failures)
                        (take 5 failures))))))


(provide 'nskk-okurigana-pbt-test)

;;; nskk-okurigana-pbt-test.el ends here
