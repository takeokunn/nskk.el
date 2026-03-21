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
         (nskk-converter-auto-start-henkan)
         (nskk-converter-romaji-style)
         (nskk-search-sort-method)
         (nskk-search-fuzzy-threshold)
         (nskk-search-learning-file)
         (nskk-modeline-format)
         (nskk-use-color-cursor)
         (nskk-henkan-show-candidates-nth)
         (nskk-henkan-number-to-display-candidates)
         (nskk-henkan-show-candidates-keys)
         (nskk-max-registration-depth)
         (nskk-debug-enabled)
         (nskk-debug-max-entries)
         (nskk-show-tooltip)
         (nskk-dcomp-multiple-activate)
         (nskk-dcomp-multiple-rows)
         (nskk-kakutei-jisyo))
  :description "Every nskk-custom defcustom is registered as a custom variable"
  :body (should (custom-variable-p var)))

;;;
;;; defcustom Default Value Tests
;;;

(nskk-deftest-table custom-symbol-defaults
  :columns (var expected)
  :rows ((nskk-state-default-mode          ascii)
         (nskk-converter-romaji-style       standard)
         (nskk-search-sort-method           frequency))
  :description "Symbol-typed defcustom variables have correct default values"
  :body (should (eq (default-value var) expected)))

(nskk-deftest-table custom-integer-defaults
  :columns (var expected)
  :rows ((nskk-search-fuzzy-threshold          3)
         (nskk-henkan-show-candidates-nth      5)
         (nskk-henkan-number-to-display-candidates 7)
         (nskk-max-registration-depth          3)
         (nskk-debug-max-entries               1000))
  :description "Integer-typed defcustom variables have correct default values"
  :body (should (= (default-value var) expected)))

(nskk-deftest-table custom-boolean-true-defaults
  :columns (var)
  :rows ((nskk-converter-auto-start-henkan)
         (nskk-use-color-cursor))
  :description "Boolean defcustom variables that default to t"
  :body (should (eq (default-value var) t)))

(nskk-deftest-table custom-integer-extended-defaults
  :columns (var expected)
  :rows ((nskk-dcomp-multiple-rows 7))
  :description "Integer-typed dcomp defcustom variables have correct default values"
  :body (should (= (default-value var) expected)))

(nskk-deftest-table custom-nil-defaults
  :columns (var)
  :rows ((nskk-debug-enabled)
         (nskk-show-tooltip)
         (nskk-dcomp-multiple-activate)
         (nskk-kakutei-jisyo))
  :description "defcustom variables that default to nil (boolean or file-or-nil type)"
  :body (should (null (default-value var))))

(nskk-describe "nskk-custom string defaults"
  (nskk-it "nskk-search-learning-file defaults to user-emacs-directory/nskk/learning.dat"
    (should (equal (default-value 'nskk-search-learning-file)
                   (expand-file-name "nskk/learning.dat" user-emacs-directory))))

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

(nskk-deftest-table dcomp-face-existence
  :columns (face)
  :rows ((nskk-dcomp-face)
         (nskk-dcomp-multiple-face)
         (nskk-dcomp-multiple-trailing-face)
         (nskk-dcomp-multiple-selected-face))
  :description "All NSKK dcomp faces are defined via facep"
  :body (should (facep face)))

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
         (nskk-dcomp)
         (nskk-kakutei-jisyo)
         (nskk-debug))
  :description "All NSKK defgroups are registered as custom groups"
  :body (should (get group 'group-documentation)))

;;;
;;; :safe Predicate Tests
;;;

(nskk-deftest-table safe-predicate-accepts-valid-values
  :columns (var valid-value)
  :rows ((nskk-search-fuzzy-threshold              0)
         (nskk-henkan-show-candidates-nth          3)
         (nskk-henkan-number-to-display-candidates 10)
         (nskk-max-registration-depth              1)
         (nskk-debug-max-entries                   500)
         (nskk-dcomp-multiple-rows                 5)
         (nskk-converter-auto-start-henkan         t)
         (nskk-converter-auto-start-henkan         nil)
         (nskk-use-color-cursor                    nil)
         (nskk-debug-enabled                       nil)
         (nskk-show-tooltip                        t)
         (nskk-show-tooltip                        nil)
         (nskk-dcomp-multiple-activate             t)
         (nskk-dcomp-multiple-activate             nil)
         (nskk-kakutei-jisyo                       nil)
         (nskk-kakutei-jisyo                       "/tmp/test-kakutei.dat")
         (nskk-state-default-mode                  ascii)
         (nskk-state-default-mode                  hiragana)
         (nskk-converter-romaji-style              azik)
         (nskk-search-sort-method                  kana)
         (nskk-modeline-format                     " SKK")
         (nskk-search-learning-file                "/tmp/test.dat")
         (nskk-henkan-show-candidates-keys         (?a ?s ?d))
         (nskk-henkan-show-candidates-keys         nil))
  :description ":safe predicate accepts valid-typed values"
  :body (should (funcall (get var 'safe-local-variable) valid-value)))

(nskk-deftest-table safe-predicate-rejects-invalid-types
  :columns (var invalid-value)
  :rows ((nskk-search-fuzzy-threshold         t)
         (nskk-henkan-show-candidates-nth     3.14)
         (nskk-dcomp-multiple-rows            "seven")
         (nskk-converter-auto-start-henkan    1)
         (nskk-show-tooltip                   1)
         (nskk-dcomp-multiple-activate        0)
         (nskk-state-default-mode             42)
         (nskk-state-default-mode             "ascii")
         (nskk-modeline-format                42)
         (nskk-search-learning-file           42)
         (nskk-kakutei-jisyo                  42)
         (nskk-henkan-show-candidates-keys    ("a" "b")))
  :description ":safe predicate rejects wrong-typed values"
  :body (should-not (funcall (get var 'safe-local-variable) invalid-value)))

;;;
;;; Property-Based Tests
;;;

;; PBT-001 — Integer custom vars accept positive integers (seeded)
;; All natnum-typed defcustom variables must satisfy natnump for any
;; non-negative integer value — the type predicate used as their :safe guard.
(nskk-property-test-seeded custom-pbt-integer-vars-accept-positive
  ((n romaji-string))
  ;; Use a literal list of representative values rather than a generator,
  ;; since no small-positive-integer generator exists in the project.
  (cl-every (lambda (v)
              (and (natnump v)
                   (funcall (get 'nskk-debug-max-entries 'safe-local-variable) v)
                   (funcall (get 'nskk-search-fuzzy-threshold 'safe-local-variable) v)
                   (funcall (get 'nskk-henkan-show-candidates-nth 'safe-local-variable) v)
                   (funcall (get 'nskk-henkan-number-to-display-candidates 'safe-local-variable) v)
                   (funcall (get 'nskk-max-registration-depth 'safe-local-variable) v)))
            '(0 1 5 10 50 100 500 1000))
  20 42)

;; PBT-002 — String custom vars accept strings (seeded)
;; String-typed defcustom variables must accept any string value through
;; their :safe predicate.
(nskk-property-test-seeded custom-pbt-string-vars-accept-strings
  ((s romaji-string))
  (and (stringp s)
       (funcall (get 'nskk-modeline-format 'safe-local-variable) s)
       (funcall (get 'nskk-search-learning-file 'safe-local-variable) s))
  30 42)

;; PBT-003 — Boolean custom vars accept only t or nil (exhaustive via seeded)
;; Boolean-typed defcustom variables must accept t and nil and reject
;; any non-boolean value.
(nskk-property-test-seeded custom-pbt-boolean-vars-reject-non-booleans
  ((n romaji-string))
  (let ((bool-vars '(nskk-converter-auto-start-henkan
                     nskk-use-color-cursor
                     nskk-debug-enabled)))
    (cl-every (lambda (var)
                (let ((pred (get var 'safe-local-variable)))
                  (and (funcall pred t)
                       (funcall pred nil)
                       (not (funcall pred 1))
                       (not (funcall pred "true"))
                       (not (funcall pred 42)))))
              bool-vars))
  20 42)

;; PBT-004 — List custom vars accept lists of the right element type (seeded)
;; List-typed defcustom variables must accept nil and valid-typed lists,
;; and reject non-list values.
(nskk-property-test-seeded custom-pbt-list-vars-accept-valid-lists
  ((n romaji-string))
  (let ((keys-pred  (get 'nskk-henkan-show-candidates-keys 'safe-local-variable)))
    (and
     ;; nskk-henkan-show-candidates-keys: nil and character lists
     (funcall keys-pred nil)
     (funcall keys-pred '(?a ?s ?d))
     (funcall keys-pred '(?a ?s ?d ?f ?j ?k ?l))
     (not (funcall keys-pred '("a" "s" "d")))
     (not (funcall keys-pred "asd"))))
  20 42)

(provide 'nskk-custom-test)

;;; nskk-custom-test.el ends here
