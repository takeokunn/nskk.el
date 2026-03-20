;;; nskk-isearch-test.el --- Tests for nskk-isearch.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-isearch.el covering:
;; - Function existence (fboundp)
;; - nskk-isearch-mode-string-alist customization
;; - nskk--isearch-mode-string for each NSKK mode
;; - nskk-isearch-setup/teardown hook registration
;; - Internal state management

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nskk-isearch)
(require 'nskk-state)
(require 'nskk-prolog)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;; Function Existence

(nskk-describe "nskk-isearch function existence"
  (nskk-it "nskk-isearch-setup is defined"
    (should (fboundp 'nskk-isearch-setup)))
  (nskk-it "nskk-isearch-teardown is defined"
    (should (fboundp 'nskk-isearch-teardown)))
  (nskk-it "nskk--isearch-mode-string is defined"
    (should (fboundp 'nskk--isearch-mode-string)))
  (nskk-it "nskk--isearch-prompt-advice is defined"
    (should (fboundp 'nskk--isearch-prompt-advice))))

;;;; Customization

(nskk-describe "nskk-isearch-mode-string-alist"
  (nskk-it "is an alist"
    (should (listp nskk-isearch-mode-string-alist)))
  (nskk-it "has entry for hiragana"
    (should (assq 'hiragana nskk-isearch-mode-string-alist)))
  (nskk-it "has entry for katakana"
    (should (assq 'katakana nskk-isearch-mode-string-alist)))
  (nskk-it "has entry for ascii"
    (should (assq 'ascii nskk-isearch-mode-string-alist)))
  (nskk-it "has entry for jisx0208-latin"
    (should (assq 'jisx0208-latin nskk-isearch-mode-string-alist)))
  (nskk-it "all entries have string values"
    (dolist (entry nskk-isearch-mode-string-alist)
      (should (stringp (cdr entry))))))

;;;; Mode String Lookup

(nskk-describe "nskk--isearch-mode-string with no orig-buffer"
  (nskk-it "returns nil when orig-buffer is nil"
    (let ((nskk--isearch-orig-buffer nil))
      (should (null (nskk--isearch-mode-string))))))

(nskk-describe "nskk--isearch-mode-string with dead buffer"
  (nskk-it "returns nil for dead buffer"
    (let ((buf (generate-new-buffer " *nskk-test-dead*")))
      (kill-buffer buf)
      (let ((nskk--isearch-orig-buffer buf))
        (should (null (nskk--isearch-mode-string)))))))

(nskk-describe "nskk--isearch-mode-string with live buffer"
  (nskk-it "returns nil when nskk-current-state is nil in orig-buffer"
    (nskk-prolog-test-with-isolated-db
      (with-temp-buffer
        (let ((nskk-current-state nil)
              (nskk--isearch-orig-buffer (current-buffer)))
          (should (null (nskk--isearch-mode-string)))))))

  (nskk-it "returns hiragana indicator string for hiragana mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk--isearch-orig-buffer (current-buffer)))
          (let ((result (nskk--isearch-mode-string)))
            (should (stringp result))
            (should (equal result (cdr (assq 'hiragana nskk-isearch-mode-string-alist)))))))))

  (nskk-it "returns katakana indicator for katakana mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'katakana))
        (let ((nskk--isearch-orig-buffer (current-buffer)))
          (let ((result (nskk--isearch-mode-string)))
            (should (equal result (cdr (assq 'katakana nskk-isearch-mode-string-alist)))))))))

  (nskk-it "returns ascii indicator for ascii mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'ascii))
        (let ((nskk--isearch-orig-buffer (current-buffer)))
          (let ((result (nskk--isearch-mode-string)))
            (should (equal result (cdr (assq 'ascii nskk-isearch-mode-string-alist)))))))))

  (nskk-it "returns jisx0208-latin indicator for jisx0208-latin mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'jisx0208-latin))
        (let ((nskk--isearch-orig-buffer (current-buffer)))
          (let ((result (nskk--isearch-mode-string)))
            (should (equal result (cdr (assq 'jisx0208-latin nskk-isearch-mode-string-alist)))))))))

  (nskk-it "returns abbrev indicator for abbrev mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'abbrev))
        (let ((nskk--isearch-orig-buffer (current-buffer)))
          (let ((result (nskk--isearch-mode-string)))
            (should (equal result (cdr (assq 'abbrev nskk-isearch-mode-string-alist))))))))))

;;;; Prompt Advice

(nskk-describe "nskk--isearch-prompt-advice"
  (nskk-it "returns orig-prompt unchanged when nskk-isearch-enable is nil"
    (let ((nskk-isearch-enable nil)
          (nskk--isearch-orig-buffer nil))
      (let ((result (nskk--isearch-prompt-advice (lambda () "I-search: "))))
        (should (equal result "I-search: ")))))

  (nskk-it "returns orig-prompt unchanged when no mode string available"
    (let ((nskk-isearch-enable t)
          (nskk--isearch-orig-buffer nil))
      (let ((result (nskk--isearch-prompt-advice (lambda () "I-search: "))))
        (should (equal result "I-search: ")))))

  (nskk-it "prepends mode string to orig-prompt when mode is active"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (with-temp-buffer
        (setq nskk-current-state (nskk-state-create 'hiragana))
        (let ((nskk-isearch-enable t)
              (nskk--isearch-orig-buffer (current-buffer)))
          (let ((result (nskk--isearch-prompt-advice (lambda () "I-search: "))))
            (should (stringp result))
            (should (string-prefix-p "[か]" result))
            (should (string-suffix-p "I-search: " result))))))))

;;;; Setup / Teardown

(nskk-describe "nskk-isearch-setup"
  (nskk-it "adds hook to isearch-mode-hook"
    (unwind-protect
        (progn
          (nskk-isearch-setup)
          (should (memq #'nskk--isearch-setup isearch-mode-hook)))
      (nskk-isearch-teardown)))
  (nskk-it "adds hook to isearch-mode-end-hook"
    (unwind-protect
        (progn
          (nskk-isearch-setup)
          (should (memq #'nskk--isearch-teardown isearch-mode-end-hook)))
      (nskk-isearch-teardown)))
  (nskk-it "installs advice on isearch-message-prefix"
    (unwind-protect
        (progn
          (nskk-isearch-setup)
          (should (advice-member-p #'nskk--isearch-prompt-advice 'isearch-message-prefix)))
      (nskk-isearch-teardown))))

(nskk-describe "nskk-isearch-teardown"
  (nskk-it "removes isearch-mode-hook"
    (nskk-isearch-setup)
    (nskk-isearch-teardown)
    (should-not (memq #'nskk--isearch-setup isearch-mode-hook)))
  (nskk-it "removes isearch-mode-end-hook"
    (nskk-isearch-setup)
    (nskk-isearch-teardown)
    (should-not (memq #'nskk--isearch-teardown isearch-mode-end-hook)))
  (nskk-it "removes advice from isearch-message-prefix"
    (nskk-isearch-setup)
    (nskk-isearch-teardown)
    (should-not (advice-member-p #'nskk--isearch-prompt-advice 'isearch-message-prefix))))

;;;; Internal Hooks

(nskk-describe "nskk--isearch-setup hook"
  (nskk-it "records current buffer as orig-buffer"
    (with-temp-buffer
      (let ((nskk--isearch-orig-buffer nil))
        (nskk--isearch-setup)
        (should (eq nskk--isearch-orig-buffer (current-buffer)))))))

(nskk-describe "nskk--isearch-teardown hook"
  (nskk-it "clears orig-buffer"
    (with-temp-buffer
      (setq nskk--isearch-orig-buffer (current-buffer))
      (nskk--isearch-teardown)
      (should (null nskk--isearch-orig-buffer)))))

(provide 'nskk-isearch-test)

;;; nskk-isearch-test.el ends here
