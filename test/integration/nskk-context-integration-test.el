;;; nskk-context-integration-test.el --- Cross-module integration tests for nskk-context.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Exercises `nskk-context-mode' against the real cross-module wiring:
;; `nskk-state-get-mode' (hard `require'), `nskk-set-mode-latin' (macro-generated
;; in nskk-input.el, only `declare-function'd), and `nskk-mode' (undeclared here,
;; read through `bound-and-true-p').  test/unit/nskk-context-test.el exercises
;; those same real functions for its happy-path cases and never loads `nskk.el';
;; this file additionally drives the real `nskk-mode' / `nskk-context-mode'
;; minor-mode commands (I1), cross-buffer isolation (I2), and the
;; nskk.el/nskk-input.el-absent regression guard (I3).

;;; Code:

(require 'ert)
(require 'nskk)
(require 'nskk-context)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

(defconst nskk-context-integration-test--src-dir
  (expand-file-name "../../src/" (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the project's `src/' directory.
Captured while this file is being loaded (when `load-file-name' is still
valid) so the I3 regression guard can point a child Emacs process at it.")

;;;; I1: Full stack, no stubs

(nskk-describe "context mode full-stack integration"
  (nskk-it "switches to latin through the real nskk-set-mode-latin path when point sits in code, and does not switch back inside a string"
    (with-temp-buffer
      (emacs-lisp-mode)
      (nskk-mode 1)
      (unwind-protect
          (let ((nskk-context-check-interval 0))
            (nskk-set-mode-hiragana)
            (nskk-context-mode 1)
            (insert "(defun foo ()\n  (+ 1 2))\n")
            (insert "\"inside-string-marker\"\n")
            ;; Point in real code, outside any string/comment.
            (goto-char (point-min))
            (search-forward "1 2")
            (should (eq (nskk-state-get-mode) 'hiragana))
            (nskk--context-post-command)
            (should (eq (nskk-state-get-mode) 'latin))

            ;; Reset to hiragana, then move point inside a string literal:
            ;; the handler must not switch back to latin from there.
            (nskk-set-mode-hiragana)
            (goto-char (point-min))
            (search-forward "inside-string-marker")
            (backward-char 3)
            (should (nskk--context-in-japanese-context-p))
            (nskk--context-post-command)
            (should (eq (nskk-state-get-mode) 'hiragana)))
        (nskk-context-mode -1)
        (nskk-mode -1)))))

;;;; I2: Buffer-local isolation

(defun nskk-context-integration-test--make-buffer (name mode)
  "Create a prog-mode buffer named NAME with `nskk-mode' and
`nskk-context-mode' enabled and its state switched to MODE via the real
`nskk-set-mode-MODE' setter."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (emacs-lisp-mode)
      (nskk-mode 1)
      (funcall (intern (format "nskk-set-mode-%s" (symbol-name mode))))
      (nskk-context-mode 1))
    buf))

(nskk-describe "context mode buffer-local isolation"
  (nskk-it "driving the post-command hook in one buffer does not alter another buffer's mode"
    (let (buf-a buf-b)
      (unwind-protect
          (let ((nskk-context-check-interval 0))
            (setq buf-a (nskk-context-integration-test--make-buffer " *nskk-context-it-a*" 'hiragana))
            (setq buf-b (nskk-context-integration-test--make-buffer " *nskk-context-it-b*" 'katakana))
            (with-current-buffer buf-a
              (insert "(defun foo ()\n  (+ 1 2))\n"))
            (with-current-buffer buf-b
              (insert "(defun bar ()\n  (+ 3 4))\n"))

            (with-current-buffer buf-a
              (should (eq (nskk-state-get-mode) 'hiragana)))
            (with-current-buffer buf-b
              (should (eq (nskk-state-get-mode) 'katakana)))

            ;; Drive the hook only in buffer A, from code (not string/comment).
            (with-current-buffer buf-a
              (goto-char (point-min))
              (search-forward "1 2")
              (nskk--context-post-command))

            (with-current-buffer buf-a
              (should (eq (nskk-state-get-mode) 'latin)))
            (with-current-buffer buf-b
              (should (eq (nskk-state-get-mode) 'katakana))))
        (dolist (buf (list buf-a buf-b))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (ignore-errors (nskk-context-mode -1))
              (ignore-errors (nskk-mode -1)))
            (kill-buffer buf)))))))

;;;; I3: Regression guard -- nskk.el / nskk-input.el never loaded

;; `nskk-context-global-mode' carries an ;;;###autoload cookie, so it can be
;; turned on in a session where nskk.el and nskk-input.el were never loaded.
;; In that state `nskk-mode' is unbound and `nskk-set-mode-latin' is unfboundp.
;; An earlier draft replaced the `(bound-and-true-p nskk-mode)' guard in
;; `nskk--context-post-command' with a bare `(defvar nskk-mode)' forward
;; declaration, which made every post-command run signal `void-variable
;; nskk-mode' in exactly this state.  Reproduced honestly via a child Emacs
;; batch process that requires only `nskk-context' -- inside the running test
;; image `nskk' is already loaded, so `nskk-mode' cannot be made unbound by
;; simply omitting a `require'.

(defconst nskk-context-integration-test--i3-probe-form
  '(progn
     (require 'nskk-context)
     (when (boundp 'nskk-mode)
       (error "precondition violated: nskk-mode is already bound in the child process"))
     (when (fboundp 'nskk-set-mode-latin)
       (error "precondition violated: nskk-set-mode-latin is fboundp in the child process"))
     (with-temp-buffer
       (nskk-context-mode 1)
       (dotimes (_ 5)
         (nskk--context-post-command)))
     (princ "NSKK-CONTEXT-I3-PROBE-OK")
     (kill-emacs 0))
  "Form run in a child Emacs process for the I3 regression guard.
Kept as a top-level constant (rather than inlined `prin1'-to-string of a
`let'-bound form) so its exact text is easy to read in a failure report.")

(nskk-describe "context mode regression guard: nskk.el and nskk-input.el never loaded"
  (nskk-it "enabling nskk-context-mode and running the post-command hook repeatedly signals no error"
    (should (file-exists-p (expand-file-name "nskk-context.el" nskk-context-integration-test--src-dir)))
    (let ((probe-file (make-temp-file "nskk-context-i3-probe-" nil ".el"
                                       (concat (prin1-to-string
                                                nskk-context-integration-test--i3-probe-form)
                                               "\n"))))
      (unwind-protect
          (with-temp-buffer
            (let ((status (call-process (or (executable-find "emacs") "emacs")
                                         nil (current-buffer) nil
                                         "-Q" "--batch"
                                         "-L" nskk-context-integration-test--src-dir
                                         "-l" probe-file)))
              (should (= status 0))
              (should (string-match-p "NSKK-CONTEXT-I3-PROBE-OK" (buffer-string)))))
        (when (file-exists-p probe-file)
          (delete-file probe-file))))))

(provide 'nskk-context-integration-test)

;;; nskk-context-integration-test.el ends here
