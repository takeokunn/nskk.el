;;; nskk-sequence-test.el --- Sequence-based property tests for NSKK -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Contributors

;; Author: takeokunn <bararararatty@gmail.com>
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


;;; Code:

(require 'ert)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-state)
(require 'nskk-input)
(require 'nskk-e2e-helpers)

(eval-when-compile (require 'cl-lib))


;;;;
;;;; Helper Functions for Sequence Testing
;;;;

(defun nskk-sequence-test--valid-state-p (state)
  "Check if STATE has valid structure and values."
  (and (nskk-state-p state)
       (nskk-state-valid-mode-p (nskk-state-mode state))
       (stringp (nskk-state-input-buffer state))
       (stringp (nskk-state-converted-buffer state))
       (listp (nskk-state-candidates state))
       (integerp (nskk-state-current-index state))
       (>= (nskk-state-current-index state) 0)
       (nskk-state-valid-mode-p (nskk-state-previous-mode state))
       (listp (nskk-state-undo-stack state))
       (listp (nskk-state-redo-stack state))))

(defun nskk-sequence-test--corpus (seed)
  "Generate the complete typing corpus with isolated random SEED."
  (let ((nskk--pbt-current-seed nil)
        (nskk--pbt-seed-state nil))
    (nskk-pbt-set-seed seed)
    (cl-loop repeat nskk-test-sequence-runs
             collect (nskk-generate 'typing-key-sequence))))

(defun nskk-sequence-test--run (mode corpus observer)
  "Enter CORPUS through MODE's key bindings, calling OBSERVER after each key."
  (let ((nskk-converter-romaji-style 'standard))
    (cl-loop for keys in corpus for trial from 0 do
             (nskk-e2e-with-buffer mode nil
               (cl-loop for key in keys for index from 0 do
                        (ert-info ((format "seed=%S trial=%d event=%d keys=%S buffer=%S"
                                           nskk-test-random-seed trial index keys
                                           (buffer-string)))
                          (let* ((events (kbd key))
                                 (command (key-binding events))
                                 (last-command-event (aref events 0)))
                            (should (= (length events) 1))
                            (should (commandp command))
                            (let ((this-command command))
                              (call-interactively command)))
                          (funcall observer index keys)))))))

(ert-deftest nskk-sequence-state-never-corrupt ()
  (let* ((nskk-test-random-seed (or nskk-test-random-seed 459))
         (corpus (nskk-sequence-test--corpus nskk-test-random-seed)))
    (nskk-sequence-test--run
     'hiragana '(("a"))
     (lambda (_index _keys) (should (equal (buffer-string) "あ"))))
    (nskk-sequence-test--run
     'hiragana corpus
     (lambda (_index _keys)
       (should (nskk-sequence-test--valid-state-p nskk-current-state))))))

(ert-deftest nskk-sequence-buffer-bounds-valid ()
  (let* ((nskk-test-random-seed (or nskk-test-random-seed 459))
         (corpus (nskk-sequence-test--corpus nskk-test-random-seed)))
    (nskk-sequence-test--run
     'ascii corpus
     (lambda (index keys)
       (should (equal (buffer-string)
                      (mapconcat #'identity (cl-subseq keys 0 (1+ index)) "")))
       (should (= (point) (point-max)))
       (should (= (point-min) 1))))))

(ert-deftest nskk-sequence-mode-always-valid ()
  (let* ((nskk-test-random-seed (or nskk-test-random-seed 459))
         (corpus (nskk-sequence-test--corpus nskk-test-random-seed)))
    (nskk-sequence-test--run
     'hiragana '(("a" "q" "a" "l" "a" "C-j" "a"))
     (lambda (index _keys)
       (should (eq (nskk-state-mode nskk-current-state)
                   (nth index '(hiragana katakana katakana latin latin hiragana hiragana))))
       (should (equal (buffer-string)
                      (nth index '("あ" "あ" "あア" "あア" "あアa" "あアa" "あアaあ"))))))
    (nskk-sequence-test--run
     'hiragana corpus
     (lambda (_index _keys)
       (should (nskk-state-valid-mode-p (nskk-state-mode nskk-current-state)))))))

(ert-deftest nskk-sequence-romaji-buffer-consistent ()
  (let* ((nskk-test-random-seed (or nskk-test-random-seed 459))
         (corpus (nskk-sequence-test--corpus nskk-test-random-seed)))
    (nskk-sequence-test--run
     'hiragana '(("k" "a"))
     (lambda (index _keys)
       (should (equal (nskk-state-romaji-buffer) (nth index '("k" ""))))
       (should (equal (buffer-string) (nth index '("" "か"))))))
    (nskk-sequence-test--run
     'hiragana corpus
     (lambda (_index _keys)
       (should (string-match-p "\\`[a-z]*\\'" (nskk-state-romaji-buffer)))))))


(provide 'nskk-sequence-test)

;;; nskk-sequence-test.el ends here
