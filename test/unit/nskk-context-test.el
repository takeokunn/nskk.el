;;; nskk-context-test.el --- Tests for nskk-context.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-context.el covering:
;; - Function existence (fboundp)
;; - Customization variables
;; - nskk--context-programming-mode-p in prog-mode vs text-mode
;; - nskk--context-in-japanese-context-p (string/comment syntax)
;; - nskk--context-get-current-mode returns nil / mode symbol
;; - Minor mode hook registration and teardown
;; - nskk--context-maybe-enable only activates in prog-mode

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-context)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Function Existence

(nskk-describe "nskk-context function existence"
  (nskk-it "nskk-context-mode is defined"
    (should (fboundp 'nskk-context-mode)))
  (nskk-it "nskk-context-global-mode is defined"
    (should (fboundp 'nskk-context-global-mode)))
  (nskk-it "nskk--context-post-command is defined"
    (should (fboundp 'nskk--context-post-command)))
  (nskk-it "nskk--context-programming-mode-p is defined"
    (should (fboundp 'nskk--context-programming-mode-p)))
  (nskk-it "nskk--context-in-japanese-context-p is defined"
    (should (fboundp 'nskk--context-in-japanese-context-p)))
  (nskk-it "nskk--context-get-current-mode is defined"
    (should (fboundp 'nskk--context-get-current-mode)))
  (nskk-it "nskk--context-switch-to-ascii is defined"
    (should (fboundp 'nskk--context-switch-to-ascii)))
  (nskk-it "nskk--context-maybe-enable is defined"
    (should (fboundp 'nskk--context-maybe-enable))))

;;;; Customization Variables

(nskk-describe "nskk-context customization variables"
  (nskk-it "nskk-context-programming-mode is defined"
    (should (boundp 'nskk-context-programming-mode)))
  (nskk-it "nskk-context-programming-mode defaults to t"
    (should (eq nskk-context-programming-mode t)))
  (nskk-it "nskk-context-mode-off-message is a string"
    (should (stringp nskk-context-mode-off-message)))
  (nskk-it "nskk-context-check-interval is a natural number"
    (should (natnump nskk-context-check-interval))))

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

;;;; Get Current Mode

(nskk-describe "nskk--context-get-current-mode"
  (nskk-it "returns nil when nskk-current-state is nil"
    (with-temp-buffer
      (let ((nskk-current-state nil))
        (should (null (nskk--context-get-current-mode))))))

  (nskk-it "returns mode symbol from nskk-current-state"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (should (eq (nskk--context-get-current-mode) 'hiragana)))))

  (nskk-it "returns ascii mode when in ascii state"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'ascii))
        (should (eq (nskk--context-get-current-mode) 'ascii))))))

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
      (setq nskk--context-was-ascii t
            nskk--context-command-count 5)
      (nskk-context-mode 1)
      (nskk-context-mode -1)
      (should (null nskk--context-was-ascii))
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

(provide 'nskk-context-test)

;;; nskk-context-test.el ends here
