;;; nskk-context-test.el --- Tests for nskk-context.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-context.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-context)
(require 'nskk-input)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Programming Mode Detection

(nskk-describe "nskk--context-programming-mode-p"
  (nskk-it "returns non-nil in emacs-lisp-mode (prog-mode derived)"
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((nskk-context-programming-mode t))
        (should (nskk--context-programming-mode-p)))))

  (nskk-it "returns nil in text-mode (not prog-mode derived)"
    (with-temp-buffer
      (text-mode)
      (let ((nskk-context-programming-mode t))
        (should (null (nskk--context-programming-mode-p))))))

  (nskk-it "returns nil when nskk-context-programming-mode is nil"
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((nskk-context-programming-mode nil))
        (should (null (nskk--context-programming-mode-p))))))

  (nskk-it "accepts list of specific modes"
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((nskk-context-programming-mode '(emacs-lisp-mode)))
        (should (nskk--context-programming-mode-p)))))

  (nskk-it "returns nil for non-matching mode list"
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((nskk-context-programming-mode '(python-mode)))
        (should (null (nskk--context-programming-mode-p))))))

  ;; nil satisfies `listp', so the nil case above exercises the list arm, not
  ;; the catch-all.  A bare symbol is the only value that reaches `(_ nil)'.
  (nskk-it "returns nil for a bare non-list symbol"
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((nskk-context-programming-mode 'not-a-mode-list))
        (should (null (nskk--context-programming-mode-p)))))))

;;;; Japanese Context Detection

(nskk-describe "nskk--context-in-japanese-context-p"
  (nskk-it "returns nil at top-level code in emacs-lisp-mode"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun foo () ")
      (should (null (nskk--context-in-japanese-context-p)))))

  (nskk-it "returns non-nil inside a string literal"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun foo () \"inside string")
      (should (nskk--context-in-japanese-context-p))))

  (nskk-it "returns non-nil inside a line comment"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert ";; this is a comment")
      (should (nskk--context-in-japanese-context-p)))))

;;;; Post-Command Handler

(nskk-describe "nskk--context-post-command"
  (nskk-it "switches to latin when point sits in code in a programming buffer"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo bar)")
        (setq-local nskk-mode t)
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-context-check-interval 0)
              (nskk-context-programming-mode t))
          (nskk--context-post-command))
        (should (eq (nskk-state-get-mode) 'latin)))))

  (nskk-it "leaves mode unchanged when point is inside a string literal"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo () \"inside string")
        (setq-local nskk-mode t)
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-context-check-interval 0)
              (nskk-context-programming-mode t))
          (nskk--context-post-command))
        (should (eq (nskk-state-get-mode) 'hiragana)))))

  (nskk-it "leaves mode unchanged when point is inside a line comment"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert ";; this is a comment")
        (setq-local nskk-mode t)
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-context-check-interval 0)
              (nskk-context-programming-mode t))
          (nskk--context-post-command))
        (should (eq (nskk-state-get-mode) 'hiragana)))))

  (nskk-it "does not redundantly switch when already in ascii mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo bar)")
        (setq-local nskk-mode t)
        (setq nskk-current-state (nskk-state-create 'ascii))
        (let ((nskk-context-check-interval 0)
              (nskk-context-programming-mode t)
              (switch-count 0))
          (cl-letf (((symbol-function 'nskk--context-switch-to-ascii)
                     (lambda () (cl-incf switch-count))))
            (nskk--context-post-command))
          (should (= switch-count 0))
          (should (eq (nskk-state-get-mode) 'ascii))))))

  (nskk-it "does not redundantly switch when already in latin mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo bar)")
        (setq-local nskk-mode t)
        (setq nskk-current-state (nskk-state-create 'latin))
        (let ((nskk-context-check-interval 0)
              (nskk-context-programming-mode t)
              (switch-count 0))
          (cl-letf (((symbol-function 'nskk--context-switch-to-ascii)
                     (lambda () (cl-incf switch-count))))
            (nskk--context-post-command))
          (should (= switch-count 0))
          (should (eq (nskk-state-get-mode) 'latin))))))

  (nskk-it "leaves mode unchanged when nskk-mode is nil"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo bar)")
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-context-check-interval 0)
              (nskk-context-programming-mode t)
              (switch-count 0))
          (cl-letf (((symbol-function 'nskk--context-switch-to-ascii)
                     (lambda () (cl-incf switch-count))))
            (nskk--context-post-command))
          (should (= switch-count 0))
          (should (eq (nskk-state-get-mode) 'hiragana))))))

  (nskk-it "leaves mode unchanged in a non-programming buffer"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (text-mode)
        (setq-local nskk-mode t)
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-context-check-interval 0)
              (nskk-context-programming-mode t)
              (switch-count 0))
          (cl-letf (((symbol-function 'nskk--context-switch-to-ascii)
                     (lambda () (cl-incf switch-count))))
            (nskk--context-post-command))
          (should (= switch-count 0))
          (should (eq (nskk-state-get-mode) 'hiragana))))))

  (nskk-it "performs exactly 3 context checks over 9 calls when check-interval is 3"
    (with-temp-buffer
      (setq-local nskk-mode t)
      (let ((nskk-context-check-interval 3)
            (check-count 0))
        (cl-letf (((symbol-function 'nskk--context-programming-mode-p)
                   (lambda () (cl-incf check-count) nil)))
          (dotimes (_ 9)
            (nskk--context-post-command)))
        (should (= check-count 3)))))

  (nskk-it "switch-to-ascii emits nskk-context-mode-off-message"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-context-mode-off-message "context-test-off-message")
              (captured nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) captured))))
            (nskk--context-switch-to-ascii))
          (should (member "context-test-off-message" captured)))))))

;;;; CPS Continuations

(nskk-describe "nskk-context CPS continuations"
  (nskk-it "switch-to-ascii/k invokes its continuation exactly once"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((done 0))
          (cl-letf (((symbol-function 'message) #'ignore))
            (nskk--context-switch-to-ascii/k (lambda () (cl-incf done))))
          (should (= done 1))
          (should (eq (nskk-state-get-mode) 'latin))))))

  (nskk-it "post-command/k invokes its continuation once even when the check is a no-op"
    (with-temp-buffer
      (text-mode)
      (let ((done 0)
            (nskk-context-check-interval 0))
        (nskk--context-post-command/k (lambda () (cl-incf done)))
        (should (= done 1))))))

;;;; Minor Mode Hook Registration

(nskk-describe "nskk-context-mode"
  (nskk-it "adds post-command-hook when enabled"
    (with-temp-buffer
      (nskk-context-mode 1)
      (unwind-protect
          (should (memq #'nskk--context-post-command
                        (buffer-local-value 'post-command-hook (current-buffer))))
        (nskk-context-mode -1))))

  (nskk-it "removes post-command-hook when disabled"
    (with-temp-buffer
      (nskk-context-mode 1)
      (nskk-context-mode -1)
      (should-not (memq #'nskk--context-post-command
                        (buffer-local-value 'post-command-hook (current-buffer))))))

  (nskk-it "resets internal state on disable"
    (with-temp-buffer
      (setq nskk--context-command-count 5)
      (nskk-context-mode 1)
      (nskk-context-mode -1)
      (should (= nskk--context-command-count 0)))))

;;;; Maybe Enable Helper

(nskk-describe "nskk--context-maybe-enable"
  (nskk-it "enables context-mode in prog-mode buffer"
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((nskk-context-programming-mode t))
        (nskk--context-maybe-enable)
        (unwind-protect
            (should nskk-context-mode)
          (nskk-context-mode -1)))))

  (nskk-it "does not enable context-mode in text-mode buffer"
    (with-temp-buffer
      (text-mode)
      (let ((nskk-context-programming-mode t))
        (nskk--context-maybe-enable)
        (should (null nskk-context-mode))))))

;;;; Globalized Mode

(nskk-describe "nskk-context-global-mode"
  (nskk-it "turns the mode on in a prog-mode buffer and leaves text-mode alone"
    (let ((prog-buf (generate-new-buffer " *nskk-ctx-prog*"))
          (text-buf (generate-new-buffer " *nskk-ctx-text*")))
      (unwind-protect
          (progn
            (with-current-buffer prog-buf (emacs-lisp-mode))
            (with-current-buffer text-buf (text-mode))
            (nskk-context-global-mode 1)
            (should (buffer-local-value 'nskk-context-mode prog-buf))
            (should-not (buffer-local-value 'nskk-context-mode text-buf)))
        (nskk-context-global-mode -1)
        (dolist (buf (list prog-buf text-buf))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

(provide 'nskk-context-test)

;;; nskk-context-test.el ends here
