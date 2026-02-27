;;; nskk-custom-test.el --- Custom variable tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-custom.el covering:
;; - Defcustom default values
;; - Custom variable types
;; - Custom groups
;; - Variable documentation

;;; Code:

(require 'ert)
(require 'nskk-custom)
(require 'nskk-test-framework)

;;;
;;; Custom Group Tests
;;;

(nskk-deftest-unit custom-group-nskk-exists
  "Test that nskk custom group is defined."
  (should (get 'nskk 'custom-group)))

(nskk-deftest-unit custom-group-nskk-core-exists
  "Test that nskk-core custom group is defined."
  (should (get 'nskk-core 'custom-group)))

(nskk-deftest-unit custom-group-nskk-converter-exists
  "Test that nskk-converter custom group is defined."
  (should (get 'nskk-converter 'custom-group)))

(nskk-deftest-unit custom-group-nskk-state-exists
  "Test that nskk-state custom group is defined."
  (should (get 'nskk-state 'custom-group)))

(nskk-deftest-unit custom-group-nskk-dictionary-exists
  "Test that nskk-dictionary custom group is defined."
  (should (get 'nskk-dictionary 'custom-group)))

(nskk-deftest-unit custom-group-nskk-cache-exists
  "Test that nskk-cache custom group is defined."
  (should (get 'nskk-cache 'custom-group)))

(nskk-deftest-unit custom-group-nskk-ui-exists
  "Test that nskk-ui custom group is defined."
  (should (get 'nskk-ui 'custom-group)))

;;;
;;; Converter Settings Defaults
;;;

(nskk-deftest-unit custom-converter-use-sokuon-default
  "Test default value of nskk-converter-use-sokuon."
  (should (eq nskk-converter-use-sokuon t)))

(nskk-deftest-unit custom-converter-n-processing-default
  "Test default value of nskk-converter-n-processing-mode."
  (should (eq nskk-converter-n-processing-mode 'smart)))

(nskk-deftest-unit custom-converter-auto-start-henkan-default
  "Test default value of nskk-converter-auto-start-henkan."
  (should (eq nskk-converter-auto-start-henkan t)))

;;;
;;; State Settings Defaults
;;;

(nskk-deftest-unit custom-state-default-mode
  "Test default value of nskk-state-default-mode."
  (should (eq nskk-state-default-mode 'ascii)))

(nskk-deftest-unit custom-state-undo-limit-default
  "Test default value of nskk-state-undo-limit."
  (should (= nskk-state-undo-limit 100)))

;;;
;;; Dictionary Settings Defaults
;;;

(nskk-deftest-unit custom-dict-user-dictionary-file-default
  "Test default value of nskk-dict-user-dictionary-file."
  (should (stringp nskk-dict-user-dictionary-file))
  (should (string-match-p "jisyo" nskk-dict-user-dictionary-file)))

(nskk-deftest-unit custom-dict-system-dictionary-files-default
  "Test default value of nskk-dict-system-dictionary-files.
Default is nil to enable auto-detection."
  (should (null nskk-dict-system-dictionary-files)))

(nskk-deftest-unit custom-dict-cache-enabled-default
  "Test default value of nskk-dict-cache-enabled."
  (should (eq nskk-dict-cache-enabled t)))

;;;
;;; Cache Settings Defaults
;;;

(nskk-deftest-unit custom-cache-default-capacity
  "Test default value of nskk-cache-default-capacity."
  (should (= nskk-cache-default-capacity 1000)))

(nskk-deftest-unit custom-cache-strategy-default
  "Test default value of nskk-cache-strategy."
  (should (eq nskk-cache-strategy 'lru)))

;;;
;;; UI Settings Defaults
;;;

(nskk-deftest-unit custom-candidate-window-page-size-default
  "Test default value of nskk-candidate-window-page-size."
  (should (= nskk-candidate-window-page-size 7)))

(nskk-deftest-unit custom-candidate-window-use-annotation-default
  "Test default value of nskk-candidate-window-use-annotation."
  (should (eq nskk-candidate-window-use-annotation t)))

(nskk-deftest-unit custom-candidate-window-position-default
  "Test default value of nskk-candidate-window-position."
  (should (eq nskk-candidate-window-position 'bottom)))

(nskk-deftest-unit custom-minibuffer-show-inline-default
  "Test default value of nskk-minibuffer-show-inline-candidate."
  (should (eq nskk-minibuffer-show-inline-candidate t)))

(nskk-deftest-unit custom-modeline-format-default
  "Test default value of nskk-modeline-format."
  (should (stringp nskk-modeline-format))
  (should (equal nskk-modeline-format "[%m]")))

(nskk-deftest-unit custom-modeline-mode-names-default
  "Test default value of nskk-modeline-mode-names."
  (should (listp nskk-modeline-mode-names))
  ;; Should have entries for all expected modes (ddskk-style)
  (should (assq 'ascii nskk-modeline-mode-names))
  (should (assq 'hiragana nskk-modeline-mode-names))
  (should (assq 'katakana nskk-modeline-mode-names))
  (should (assq 'abbrev nskk-modeline-mode-names))
  (should (assq 'latin nskk-modeline-mode-names))
  (should (assq 'jisx0208-latin nskk-modeline-mode-names)))

(nskk-deftest-unit custom-modeline-mode-names-values
  "Test modeline mode name display values (ddskk-style)."
  (should (equal (alist-get 'ascii nskk-modeline-mode-names) "SKK"))
  (should (equal (alist-get 'hiragana nskk-modeline-mode-names) "\u304B\u306A"))
  (should (equal (alist-get 'katakana nskk-modeline-mode-names) "\u30AB\u30CA"))
  (should (equal (alist-get 'abbrev nskk-modeline-mode-names) "aA"))
  (should (equal (alist-get 'latin nskk-modeline-mode-names) "SKK"))
  (should (equal (alist-get 'jisx0208-latin nskk-modeline-mode-names) "\u5168\u82F1")))

;;;
;;; Dictionary Path Settings
;;;

(nskk-deftest-unit custom-large-dictionary-default
  "Test default value of nskk-large-dictionary."
  (should (null nskk-large-dictionary)))

;;;
;;; Variable Type Validation Tests
;;;

(nskk-deftest-unit custom-boolean-variables-are-boolean
  "Test that boolean custom variables have boolean values."
  (dolist (var '(nskk-converter-use-sokuon
                 nskk-converter-auto-start-henkan
                 nskk-dict-cache-enabled
                 nskk-candidate-window-use-annotation
                 nskk-minibuffer-show-inline-candidate
                 nskk-use-color-cursor))
    (should (booleanp (symbol-value var)))))

(nskk-deftest-unit custom-integer-variables-are-integers
  "Test that integer custom variables have integer values."
  (dolist (var '(nskk-state-undo-limit
                 nskk-cache-default-capacity
                 nskk-candidate-window-page-size
                 nskk-henkan-show-candidates-nth
                 nskk-henkan-number-to-display-candidates))
    (should (integerp (symbol-value var)))))

(nskk-deftest-unit custom-string-variables-are-strings
  "Test that string custom variables have string values."
  (dolist (var '(nskk-dict-user-dictionary-file
                 nskk-modeline-format))
    (should (stringp (symbol-value var)))))

(nskk-deftest-unit custom-symbol-variables-are-symbols
  "Test that symbol custom variables have symbol values."
  (dolist (var '(nskk-converter-n-processing-mode
                 nskk-state-default-mode
                 nskk-cache-strategy
                 nskk-candidate-window-position))
    (should (symbolp (symbol-value var)))))

;;;
;;; Custom Variable Documentation Tests
;;;

(nskk-deftest-unit custom-variables-have-docstrings
  "Test that all custom variables have documentation strings."
  (dolist (var '(nskk-converter-use-sokuon
                 nskk-converter-n-processing-mode
                 nskk-converter-auto-start-henkan
                 nskk-state-default-mode
                 nskk-state-undo-limit
                 nskk-dict-user-dictionary-file
                 nskk-dict-system-dictionary-files
                 nskk-dict-cache-enabled
                 nskk-cache-default-capacity
                 nskk-cache-strategy
                 nskk-candidate-window-page-size
                 nskk-candidate-window-use-annotation
                 nskk-candidate-window-position
                 nskk-minibuffer-show-inline-candidate
                 nskk-modeline-format
                 nskk-modeline-mode-names
                 nskk-large-dictionary
                 nskk-henkan-show-candidates-nth
                 nskk-henkan-number-to-display-candidates
                 nskk-henkan-show-candidates-keys
                 nskk-use-color-cursor
                 nskk-cursor-hiragana-color
                 nskk-cursor-katakana-color
                 nskk-cursor-latin-color
                 nskk-cursor-jisx0208-latin-color
                 nskk-cursor-abbrev-color))
    (let ((doc (documentation-property var 'variable-documentation)))
      (should (stringp doc))
      (should (> (length doc) 0)))))

;;;
;;; Choice Type Validation Tests
;;;

(nskk-deftest-unit custom-n-processing-valid-choices
  "Test that n-processing-mode has a valid choice value."
  (should (memq nskk-converter-n-processing-mode '(smart strict loose))))

(nskk-deftest-unit custom-default-mode-valid-choices
  "Test that default-mode has a valid choice value."
  (should (memq nskk-state-default-mode '(ascii hiragana katakana))))

(nskk-deftest-unit custom-cache-strategy-valid-choices
  "Test that cache-strategy has a valid choice value."
  (should (memq nskk-cache-strategy '(lru lfu))))

(nskk-deftest-unit custom-window-position-valid-choices
  "Test that window-position has a valid choice value."
  (should (memq nskk-candidate-window-position '(bottom top inline))))

;;;
;;; Positive Integer Validation Tests
;;;

(nskk-deftest-unit custom-undo-limit-positive
  "Test that undo-limit is a positive integer."
  (should (> nskk-state-undo-limit 0)))

(nskk-deftest-unit custom-cache-capacity-positive
  "Test that cache-capacity is a positive integer."
  (should (> nskk-cache-default-capacity 0)))

(nskk-deftest-unit custom-page-size-positive
  "Test that page-size is a positive integer."
  (should (> nskk-candidate-window-page-size 0)))

;;;
;;; Henkan (Conversion) Settings Defaults
;;;

(nskk-deftest-unit custom-henkan-show-candidates-nth-default
  "Test default value of nskk-henkan-show-candidates-nth."
  (should (= nskk-henkan-show-candidates-nth 5)))

(nskk-deftest-unit custom-henkan-number-to-display-default
  "Test default value of nskk-henkan-number-to-display-candidates."
  (should (= nskk-henkan-number-to-display-candidates 7)))

(nskk-deftest-unit custom-henkan-show-candidates-keys-default
  "Test default value of nskk-henkan-show-candidates-keys."
  (should (listp nskk-henkan-show-candidates-keys))
  (should (= (length nskk-henkan-show-candidates-keys) 7))
  (should (equal nskk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l))))

;;;
;;; Cursor Color Settings Defaults
;;;

(nskk-deftest-unit custom-use-color-cursor-default
  "Test default value of nskk-use-color-cursor."
  (should (eq nskk-use-color-cursor t)))

(nskk-deftest-unit custom-cursor-color-variables-are-strings
  "Test that cursor color variables have string values."
  (dolist (var '(nskk-cursor-hiragana-color
                 nskk-cursor-katakana-color
                 nskk-cursor-latin-color
                 nskk-cursor-jisx0208-latin-color
                 nskk-cursor-abbrev-color))
    (should (stringp (symbol-value var)))))

(nskk-deftest-unit custom-cursor-jisx0208-latin-color-default
  "Test default value of nskk-cursor-jisx0208-latin-color."
  (should (equal nskk-cursor-jisx0208-latin-color "gold")))

(nskk-deftest-unit custom-cursor-abbrev-color-default
  "Test default value of nskk-cursor-abbrev-color."
  (should (equal nskk-cursor-abbrev-color "royalblue")))

(nskk-deftest-unit custom-default-mode-includes-jisx0208-latin
  "Test that default-mode choice type includes jisx0208-latin."
  (should (memq nskk-state-default-mode '(ascii hiragana katakana jisx0208-latin))))

(provide 'nskk-custom-test)

;;; nskk-custom-test.el ends here
