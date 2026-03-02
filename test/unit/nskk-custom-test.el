;;; nskk-custom-test.el --- Tests for nskk-custom.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-custom.el covering:
;; - defcustom default values
;; - defcustom custom-variable-p registration
;; - defface existence

;;; Code:

(require 'ert)
(require 'nskk-custom)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

;;;
;;; Feature Loading Tests
;;;

(nskk-describe "nskk-custom feature loading"
  (nskk-it "provides the nskk-custom feature"
    (should (featurep 'nskk-custom)))

  (nskk-it "requiring nskk-custom again is safe (idempotent)"
    (should (require 'nskk-custom))))

;;;
;;; defcustom Registration Tests
;;;

(nskk-deftest-table custom-variable-registration
  :columns (var)
  :rows ((nskk-state-default-mode)
         (nskk-state-undo-limit)
         (nskk-converter-use-sokuon)
         (nskk-converter-n-processing-mode)
         (nskk-converter-auto-start-henkan)
         (nskk-converter-romaji-style)
         (nskk-search-sort-method)
         (nskk-search-fuzzy-threshold)
         (nskk-search-enable-cache)
         (nskk-search-learning-file)
         (nskk-search-auto-save)
         (nskk-search-auto-save-interval)
         (nskk-jisyo-files)
         (nskk-modeline-format)
         (nskk-use-color-cursor)
         (nskk-henkan-show-candidates-nth)
         (nskk-henkan-number-to-display-candidates)
         (nskk-henkan-show-candidates-keys)
         (nskk-max-registration-depth)
         (nskk-debug-enabled)
         (nskk-debug-max-entries))
  :description "Every nskk-custom defcustom is registered as a custom variable"
  :body (should (custom-variable-p var)))

;;;
;;; defcustom Default Value Tests
;;;

(nskk-deftest-table custom-symbol-defaults
  :columns (var expected)
  :rows ((nskk-state-default-mode          ascii)
         (nskk-converter-n-processing-mode smart)
         (nskk-converter-romaji-style       standard)
         (nskk-search-sort-method           frequency))
  :description "Symbol-typed defcustom variables have correct default values"
  :body (should (eq (default-value var) expected)))

(nskk-deftest-table custom-integer-defaults
  :columns (var expected)
  :rows ((nskk-state-undo-limit               100)
         (nskk-search-fuzzy-threshold          3)
         (nskk-search-auto-save-interval       300)
         (nskk-henkan-show-candidates-nth      5)
         (nskk-henkan-number-to-display-candidates 7)
         (nskk-max-registration-depth          3)
         (nskk-debug-max-entries               1000))
  :description "Integer-typed defcustom variables have correct default values"
  :body (should (= (default-value var) expected)))

(nskk-deftest-table custom-boolean-true-defaults
  :columns (var)
  :rows ((nskk-converter-use-sokuon)
         (nskk-converter-auto-start-henkan)
         (nskk-search-enable-cache)
         (nskk-search-auto-save)
         (nskk-use-color-cursor))
  :description "Boolean defcustom variables that default to t"
  :body (should (eq (default-value var) t)))

(nskk-deftest-table custom-boolean-nil-defaults
  :columns (var)
  :rows ((nskk-jisyo-files)
         (nskk-debug-enabled))
  :description "defcustom variables that default to nil"
  :body (should (null (default-value var))))

(nskk-describe "nskk-custom string defaults"
  (nskk-it "nskk-search-learning-file defaults to ~/.emacs.d/nskk/learning.dat"
    (should (equal (default-value 'nskk-search-learning-file)
                   "~/.emacs.d/nskk/learning.dat")))

  (nskk-it "nskk-modeline-format defaults to \" %m\""
    (should (equal (default-value 'nskk-modeline-format) " %m"))))

(nskk-describe "nskk-henkan-show-candidates-keys default"
  (nskk-it "defaults to the list (?a ?s ?d ?f ?j ?k ?l)"
    (should (equal (default-value 'nskk-henkan-show-candidates-keys)
                   '(?a ?s ?d ?f ?j ?k ?l))))

  (nskk-it "default value is a list of 7 characters"
    (should (= (length (default-value 'nskk-henkan-show-candidates-keys)) 7)))

  (nskk-it "all elements of the default are characterp"
    (should (cl-every #'characterp
                      (default-value 'nskk-henkan-show-candidates-keys)))))

;;;
;;; defface Existence Tests
;;;

(nskk-deftest-table cursor-face-existence
  :columns (face)
  :rows ((nskk-cursor-hiragana)
         (nskk-cursor-katakana)
         (nskk-cursor-latin)
         (nskk-cursor-jisx0208-latin)
         (nskk-cursor-abbrev))
  :description "All NSKK cursor faces are defined via facep"
  :body (should (facep face)))

(provide 'nskk-custom-test)

;;; nskk-custom-test.el ends here
