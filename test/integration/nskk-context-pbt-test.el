;;; nskk-context-pbt-test.el --- Property-based tests for nskk-context -*- lexical-binding: t; -*-

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

;; Property-based tests for `nskk-context'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk-pbt-generators)
(require 'nskk-context)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-input)


;;;;
;;;; Local Generators: Major Mode & Syntax Position
;;;;
;;;; `nskk-pbt-generators.el' has no generator for major modes or for
;;;; syntax-table positions (string/comment vs. code), so both are built
;;;; here from the seeded random primitives it already provides.
;;;;

(defconst nskk-context-pbt--prog-modes
  '(emacs-lisp-mode python-mode)
  "Prog-mode-derived major modes used by the context PBT generators.")

(defconst nskk-context-pbt--comment-prefix
  '((emacs-lisp-mode . ";; ")
    (python-mode . "# "))
  "Line-comment prefix per major mode in `nskk-context-pbt--prog-modes'.")

(defun nskk-context-pbt--generate-prog-mode ()
  "Generate a random prog-mode-derived major mode symbol."
  (nskk--pbt-random-choice nskk-context-pbt--prog-modes))

(defun nskk-context-pbt--random-code-text (min-len max-len)
  "Generate random alphanumeric/space text with length in [MIN-LEN, MAX-LEN].
Contains no quote, hash, or semicolon characters, so it can never
accidentally open or close a string or comment of its own."
  (let ((chars (append (string-to-list "abcdefghijklmnopqrstuvwxyz")
                       (string-to-list "0123456789 "))))
    (nskk--pbt-random-string chars (nskk--pbt-random-int min-len max-len))))

(defun nskk-context-pbt--insert-string-and-goto ()
  "Insert a string literal at point and move point strictly inside its body."
  (let* ((body (nskk-context-pbt--random-code-text 3 15))
         (start (point)))
    (insert (format "\"%s\"" body))
    (goto-char (+ start 1 (nskk--pbt-random-int 0 (1- (length body)))))))

(defun nskk-context-pbt--insert-comment-and-goto (major-mode-sym)
  "Insert a line comment for MAJOR-MODE-SYM and move point inside its body."
  (let* ((prefix (cdr (assq major-mode-sym nskk-context-pbt--comment-prefix)))
         (body (nskk-context-pbt--random-code-text 3 15))
         (start (point)))
    (insert prefix body)
    (goto-char (+ start (length prefix) (nskk--pbt-random-int 0 (1- (length body)))))))

(defun nskk-context-pbt--setup-japanese-context-buffer (major-mode-sym)
  "Insert content into the current buffer so point sits inside a string
literal or a comment, chosen at random.  MAJOR-MODE-SYM selects the
comment syntax to use."
  (if (nskk--pbt-random-bool)
      (nskk-context-pbt--insert-string-and-goto)
    (nskk-context-pbt--insert-comment-and-goto major-mode-sym)))

(defun nskk-context-pbt--setup-code-buffer ()
  "Insert plain code text into the current buffer and move point to a
random position inside it, outside of any string or comment."
  (let* ((body (nskk-context-pbt--random-code-text 3 15))
         (start (point)))
    (insert body)
    (goto-char (+ start (nskk--pbt-random-int 0 (length body))))))


;;;;
;;;; Helper Functions
;;;;

(defun nskk-context-pbt--make-buffer (major-mode-sym initial-mode)
  "Create a live temp buffer with MAJOR-MODE-SYM activated,
`nskk-current-state' initialized to INITIAL-MODE, and `nskk-mode' bound
non-nil buffer-locally so `nskk--context-post-command' treats NSKK as active."
  (let ((buf (generate-new-buffer " *nskk-context-pbt-test*")))
    (with-current-buffer buf
      (funcall major-mode-sym)
      (setq-local nskk-current-state (nskk-state-create initial-mode))
      (setq-local nskk-mode t))
    buf))

(defun nskk-context-pbt--cleanup-buffer (buf)
  "Kill test buffer BUF safely."
  (when (buffer-live-p buf)
    (kill-buffer buf)))


;;;;
;;;; Property 1: No switch while point is inside a string or comment
;;;;

(nskk-describe "context switch suppressed inside strings and comments"
  (nskk-it "leaves the NSKK mode unchanged for any number of calls with point in a string or comment"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((runs 50)
            (failures nil)
            (cases-exercised 0)
            (nskk-context-check-interval 0)
            (nskk-context-programming-mode t))
        (dotimes (_ runs)
          (let* ((major-mode-sym (nskk-context-pbt--generate-prog-mode))
                 (initial-mode (nskk--pbt-generate-valid-mode))
                 (repeat (nskk--pbt-random-int 1 8))
                 (buf (nskk-context-pbt--make-buffer major-mode-sym initial-mode)))
            (unwind-protect
                (with-current-buffer buf
                  (nskk-context-pbt--setup-japanese-context-buffer major-mode-sym)
                  (if (not (and (bound-and-true-p nskk-mode)
                                (nskk--context-programming-mode-p)
                                (nskk--context-in-japanese-context-p)))
                      (push (list :precondition-failed t
                                  :major-mode major-mode-sym
                                  :initial-mode initial-mode)
                            failures)
                    (cl-incf cases-exercised)
                    (dotimes (call-index repeat)
                      (nskk--context-post-command)
                      (let ((mode-now (nskk-state-get-mode)))
                        (unless (eq mode-now initial-mode)
                          (push (list :major-mode major-mode-sym
                                      :initial-mode initial-mode
                                      :call-index call-index
                                      :mode-now mode-now)
                                failures))))))
              (nskk-context-pbt--cleanup-buffer buf))))
        (should (> cases-exercised 0))
        (when failures
          (ert-fail (format "String/comment suppression failed for %d cases:\n%S"
                            (length failures)
                            (take 5 failures))))))))


;;;;
;;;; Property 2: Convergence to latin outside strings and comments
;;;;

(nskk-describe "context switch converges to latin in code"
  (nskk-it "reaches (or stays at) latin from any initial mode and stays idempotent on further calls"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((runs 50)
            (failures nil)
            (cases-exercised 0)
            (nskk-context-check-interval 0)
            (nskk-context-programming-mode t))
        (dotimes (_ runs)
          (let* ((major-mode-sym (nskk-context-pbt--generate-prog-mode))
                 (initial-mode (nskk--pbt-generate-valid-mode))
                 (repeat (nskk--pbt-random-int 1 8))
                 (buf (nskk-context-pbt--make-buffer major-mode-sym initial-mode)))
            (unwind-protect
                (with-current-buffer buf
                  (nskk-context-pbt--setup-code-buffer)
                  (if (not (and (bound-and-true-p nskk-mode)
                                (nskk--context-programming-mode-p)
                                (not (nskk--context-in-japanese-context-p))))
                      (push (list :precondition-failed t
                                  :major-mode major-mode-sym
                                  :initial-mode initial-mode)
                            failures)
                    (cl-incf cases-exercised)
                    ;; `ascii' and `latin' are already-ASCII states: the mode
                    ;; must simply stay put rather than "converge" anywhere.
                    (let ((expected (if (memq initial-mode '(ascii latin))
                                         initial-mode
                                       'latin)))
                      (dotimes (call-index repeat)
                        (nskk--context-post-command)
                        (let ((mode-now (nskk-state-get-mode)))
                          (unless (eq mode-now expected)
                            (push (list :major-mode major-mode-sym
                                        :initial-mode initial-mode
                                        :call-index call-index
                                        :expected expected
                                        :mode-now mode-now)
                                  failures)))))))
              (nskk-context-pbt--cleanup-buffer buf))))
        (should (> cases-exercised 0))
        (when failures
          (ert-fail (format "Convergence-to-latin property failed for %d cases:\n%S"
                            (length failures)
                            (take 5 failures))))))))


;;;;
;;;; Property 3: Toggling never duplicates the hook or leaks the counter
;;;;

(nskk-describe "context-mode toggling keeps hook and counter state clean"
  (nskk-it "never duplicates the post-command hook entry and zeroes the counter on every disable"
    (let ((runs 50)
          (failures nil)
          (cases-exercised 0))
      (dotimes (_ runs)
        (let ((buf (generate-new-buffer " *nskk-context-pbt-toggle-test*"))
              (toggles (nskk--pbt-random-int 1 10)))
          (unwind-protect
              (with-current-buffer buf
                (dotimes (_ toggles)
                  (let ((enable (nskk--pbt-random-bool)))
                    (cl-incf cases-exercised)
                    (if enable
                        (nskk-context-mode 1)
                      ;; Bump the counter to a nonzero value first so the
                      ;; zero-after-disable check exercises a real reset
                      ;; rather than an already-zero counter.
                      (progn
                        (setq nskk--context-command-count (nskk--pbt-random-int 1 20))
                        (nskk-context-mode -1)))
                    (let ((hook-entries (cl-count #'nskk--context-post-command
                                                  (buffer-local-value 'post-command-hook
                                                                       (current-buffer)))))
                      (if enable
                          (unless (= hook-entries 1)
                            (push (list :action 'enable :hook-entries hook-entries) failures))
                        (unless (and (= hook-entries 0)
                                     (= nskk--context-command-count 0))
                          (push (list :action 'disable
                                      :hook-entries hook-entries
                                      :count nskk--context-command-count)
                                failures)))))))
            (nskk-context-pbt--cleanup-buffer buf))))
      (should (> cases-exercised 0))
      (when failures
        (ert-fail (format "Toggle hook/counter invariant failed for %d cases:\n%S"
                          (length failures)
                          (take 5 failures)))))))


(provide 'nskk-context-pbt-test)

;;; nskk-context-pbt-test.el ends here
