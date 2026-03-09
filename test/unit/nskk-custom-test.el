;;; nskk-custom-test.el --- Tests for nskk-custom.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-custom.el covering:
;; - defcustom custom-variable-p registration
;; - defcustom default values (symbol, integer, boolean, nil, string, list)
;; - defcustom :safe predicate acceptance (valid typed values)
;; - defcustom :safe predicate rejection (wrong-typed values)
;; - defgroup custom-group registration
;; - defface existence and :background attribute presence

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

(nskk-deftest-table custom-nil-defaults
  :columns (var)
  :rows ((nskk-jisyo-files)
         (nskk-debug-enabled))
  :description "defcustom variables that default to nil (boolean or list type)"
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

(nskk-deftest-table cursor-face-has-background
  :columns (face)
  :rows ((nskk-cursor-hiragana)
         (nskk-cursor-katakana)
         (nskk-cursor-latin)
         (nskk-cursor-jisx0208-latin)
         (nskk-cursor-abbrev))
  :description "All NSKK cursor faces specify a :background attribute"
  :body (should (face-attribute face :background nil t)))

;;;
;;; defgroup Registration Tests
;;;

(nskk-deftest-table custom-group-registration
  :columns (group)
  :rows ((nskk)
         (nskk-ui)
         (nskk-state)
         (nskk-converter)
         (nskk-search)
         (nskk-modeline)
         (nskk-henkan)
         (nskk-candidate-window)
         (nskk-debug))
  :description "All NSKK defgroups are registered as custom groups"
  :body (should (get group 'group-documentation)))

;;;
;;; :safe Predicate Tests
;;;

(nskk-deftest-table safe-predicate-accepts-valid-values
  :columns (var valid-value)
  :rows ((nskk-state-undo-limit                    42)
         (nskk-state-undo-limit                    0)
         (nskk-search-fuzzy-threshold              0)
         (nskk-search-auto-save-interval           60)
         (nskk-henkan-show-candidates-nth          3)
         (nskk-henkan-number-to-display-candidates 10)
         (nskk-max-registration-depth              1)
         (nskk-debug-max-entries                   500)
         (nskk-converter-use-sokuon                t)
         (nskk-converter-use-sokuon                nil)
         (nskk-converter-auto-start-henkan         t)
         (nskk-converter-auto-start-henkan         nil)
         (nskk-search-enable-cache                 t)
         (nskk-search-enable-cache                 nil)
         (nskk-search-auto-save                    t)
         (nskk-use-color-cursor                    nil)
         (nskk-debug-enabled                       nil)
         (nskk-state-default-mode                  ascii)
         (nskk-state-default-mode                  hiragana)
         (nskk-converter-n-processing-mode         strict)
         (nskk-converter-romaji-style              azik)
         (nskk-search-sort-method                  kana)
         (nskk-modeline-format                     " SKK")
         (nskk-search-learning-file                "/tmp/test.dat")
         (nskk-jisyo-files                         nil)
         (nskk-jisyo-files                         ("/path/to/SKK-JISYO.L"))
         (nskk-henkan-show-candidates-keys         (?a ?s ?d))
         (nskk-henkan-show-candidates-keys         nil))
  :description ":safe predicate accepts valid-typed values"
  :body (should (funcall (get var 'safe-local-variable) valid-value)))

(nskk-deftest-table safe-predicate-rejects-invalid-types
  :columns (var invalid-value)
  :rows ((nskk-state-undo-limit               "string")
         (nskk-state-undo-limit               3.14)
         (nskk-search-fuzzy-threshold         t)
         (nskk-henkan-show-candidates-nth     3.14)
         (nskk-converter-use-sokuon           42)
         (nskk-converter-use-sokuon           "true")
         (nskk-converter-auto-start-henkan    1)
         (nskk-state-default-mode             42)
         (nskk-state-default-mode             "ascii")
         (nskk-converter-n-processing-mode    42)
         (nskk-modeline-format                42)
         (nskk-search-learning-file           42)
         (nskk-jisyo-files                    "not-a-list")
         (nskk-jisyo-files                    (1 2 3))
         (nskk-henkan-show-candidates-keys    ("a" "b")))
  :description ":safe predicate rejects wrong-typed values"
  :body (should-not (funcall (get var 'safe-local-variable) invalid-value)))

(provide 'nskk-custom-test)

;;; nskk-custom-test.el ends here
