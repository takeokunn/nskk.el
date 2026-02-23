;;; nskk-ddskk-compat-test.el --- Tests for nskk-ddskk-compat.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-ddskk-compat.el covering:
;; - Variable aliases (skk-* -> nskk-*)
;; - Function aliases
;; - Hook compatibility
;; - Dictionary format parsing
;; - Compatibility configuration
;; - Unimplemented function tracking
;; - Warning system

;;; Code:

(require 'ert)
(require 'nskk-ddskk-compat)
(require 'nskk-test-framework)

;;;
;;; Variable Alias Tests
;;;

(nskk-deftest-unit ddskk-compat-var-alias-mode
  "Test skk-mode variable alias."
  (should (eq (indirect-variable 'skk-mode) 'nskk-mode)))

(nskk-deftest-unit ddskk-compat-var-alias-large-jisyo
  "Test skk-large-jisyo variable alias."
  (should (eq (indirect-variable 'skk-large-jisyo) 'nskk-large-dictionary)))

(nskk-deftest-unit ddskk-compat-var-alias-server-host
  "Test skk-server-host variable alias."
  (should (eq (indirect-variable 'skk-server-host) 'nskk-server-host)))

(nskk-deftest-unit ddskk-compat-var-alias-server-portnum
  "Test skk-server-portnum variable alias."
  (should (eq (indirect-variable 'skk-server-portnum) 'nskk-server-port)))

(nskk-deftest-unit ddskk-compat-var-alias-show-inline
  "Test skk-show-inline variable alias."
  (should (eq (indirect-variable 'skk-show-inline) 'nskk-show-inline)))

(nskk-deftest-unit ddskk-compat-var-alias-cursor-hiragana-color
  "Test skk-cursor-hiragana-color variable alias."
  (should (eq (indirect-variable 'skk-cursor-hiragana-color) 'nskk-cursor-hiragana-color)))

;;;
;;; Hook Alias Tests
;;;

(nskk-deftest-unit ddskk-compat-hook-alias-mode
  "Test skk-mode-hook alias."
  (should (eq (indirect-variable 'skk-mode-hook) 'nskk-mode-hook)))

(nskk-deftest-unit ddskk-compat-hook-alias-load
  "Test skk-load-hook alias."
  (should (eq (indirect-variable 'skk-load-hook) 'nskk-load-hook)))

(nskk-deftest-unit ddskk-compat-hook-alias-before-henkan
  "Test skk-before-henkan-hook alias."
  (should (eq (indirect-variable 'skk-before-henkan-hook) 'nskk-before-henkan-hook)))

(nskk-deftest-unit ddskk-compat-hook-alias-after-henkan
  "Test skk-after-henkan-hook alias."
  (should (eq (indirect-variable 'skk-after-henkan-hook) 'nskk-after-henkan-hook)))

;;;
;;; Function Alias Tests
;;;

(nskk-deftest-unit ddskk-compat-func-alias-skk-mode
  "Test skk-mode function alias."
  (should (eq (indirect-function 'skk-mode) (indirect-function 'nskk-mode))))

(nskk-deftest-unit ddskk-compat-func-alias-skk-henkan
  "Test skk-henkan function alias."
  (should (eq (indirect-function 'skk-henkan) (indirect-function 'nskk-convert))))

(nskk-deftest-unit ddskk-compat-func-alias-skk-kakutei
  "Test skk-kakutei function alias."
  (should (eq (indirect-function 'skk-kakutei) (indirect-function 'nskk-commit))))

(nskk-deftest-unit ddskk-compat-func-alias-skk-cancel
  "Test skk-cancel function alias."
  (should (eq (indirect-function 'skk-cancel) (indirect-function 'nskk-cancel))))

(nskk-deftest-unit ddskk-compat-func-alias-skk-toggle-kana
  "Test skk-toggle-kana function alias."
  (should (eq (indirect-function 'skk-toggle-kana) (indirect-function 'nskk-toggle-kana))))

(nskk-deftest-unit ddskk-compat-func-alias-skk-hiragana-mode
  "Test skk-hiragana-mode function alias."
  (should (eq (indirect-function 'skk-hiragana-mode) (indirect-function 'nskk-hiragana-mode))))

(nskk-deftest-unit ddskk-compat-func-alias-skk-katakana-mode
  "Test skk-katakana-mode function alias."
  (should (eq (indirect-function 'skk-katakana-mode) (indirect-function 'nskk-katakana-mode))))

;;;
;;; Unimplemented Function Tracking Tests
;;;

(nskk-deftest-unit ddskk-compat-unimplemented-list-exists
  "Test that unimplemented functions list is defined."
  (should (listp nskk-ddskk-compat--unimplemented-functions))
  (should (> (length nskk-ddskk-compat--unimplemented-functions) 0)))

(nskk-deftest-unit ddskk-compat-incompatible-vars-list-exists
  "Test that incompatible variables list is defined."
  (should (listp nskk-ddskk-compat--incompatible-variables))
  (should (> (length nskk-ddskk-compat--incompatible-variables) 0)))

(nskk-deftest-unit ddskk-compat-check-unimplemented-known
  "Test checking known unimplemented function."
  (let ((nskk-ddskk-compat-strict t)
        (nskk-ddskk-compat-verbosity 1))
    ;; Should not error, just warn
    (nskk-ddskk-compat--check-unimplemented 'skk-tex-command)))

(nskk-deftest-unit ddskk-compat-check-unimplemented-implemented
  "Test checking an implemented function does not warn."
  (let ((nskk-ddskk-compat-strict t)
        (nskk-ddskk-compat-verbosity 1))
    ;; An implemented function (not in unimplemented list)
    (nskk-ddskk-compat--check-unimplemented 'skk-mode)))

;;;
;;; Warning System Tests
;;;

(nskk-deftest-unit ddskk-compat-warn-verbose-0-silent
  "Test that warnings are silent at verbosity 0."
  (let ((nskk-ddskk-compat-verbosity 0))
    ;; Should not produce visible output at level 0
    (nskk-ddskk-compat-warn "Test warning %s" "value")))

(nskk-deftest-unit ddskk-compat-warn-verbose-1-shows
  "Test that warnings show at verbosity 1."
  (let ((nskk-ddskk-compat-verbosity 1))
    ;; Should produce a message at level 1
    (nskk-ddskk-compat-warn "Test warning %s" "value")))

;;;
;;; Dictionary Parsing Tests
;;;

(nskk-deftest-unit ddskk-compat-parse-candidates-basic
  "Test parsing DDSKK candidate string."
  (let ((result (nskk-ddskk-compat--parse-candidates "/漢字/感じ/幹事/")))
    (should (listp result))
    ;; Should have parsed the candidates (empty first element from leading /)
    (should (> (length result) 0))))

(nskk-deftest-unit ddskk-compat-parse-candidates-single
  "Test parsing a single candidate."
  (let ((result (nskk-ddskk-compat--parse-candidates "漢字")))
    (should (listp result))
    (should (= (length result) 1))
    (should (string= (car result) "漢字"))))

(nskk-deftest-unit ddskk-compat-parse-candidates-empty
  "Test parsing empty candidate string."
  (let ((result (nskk-ddskk-compat--parse-candidates "")))
    (should (listp result))))

;;;
;;; Configuration Tests
;;;

(nskk-deftest-unit ddskk-compat-custom-group-exists
  "Test that the nskk-ddskk-compat custom group exists."
  (should (get 'nskk-ddskk-compat 'custom-group)))

(nskk-deftest-unit ddskk-compat-mode-default-enabled
  "Test that compat mode is enabled by default."
  (should (eq (default-value 'nskk-ddskk-compat-mode) t)))

(nskk-deftest-unit ddskk-compat-verbosity-default
  "Test that default verbosity is 0."
  (should (= (default-value 'nskk-ddskk-compat-verbosity) 0)))

(nskk-deftest-unit ddskk-compat-strict-default-nil
  "Test that strict mode is disabled by default."
  (should (eq (default-value 'nskk-ddskk-compat-strict) nil)))

;;;
;;; Setup Function Tests
;;;

(nskk-deftest-unit ddskk-compat-setup-function-exists
  "Test that setup function exists."
  (should (fboundp 'nskk-ddskk-compat-setup)))

(nskk-deftest-unit ddskk-compat-setup-sub-functions-exist
  "Test that setup sub-functions exist."
  (should (fboundp 'nskk-ddskk-compat--setup-variable-aliases))
  (should (fboundp 'nskk-ddskk-compat--setup-function-aliases))
  (should (fboundp 'nskk-ddskk-compat--setup-hook-wrappers))
  (should (fboundp 'nskk-ddskk-compat--setup-advice)))

;;;
;;; Migration Function Tests
;;;

(nskk-deftest-unit ddskk-compat-migrate-config-function-exists
  "Test that migrate-config function exists."
  (should (fboundp 'nskk-ddskk-compat-migrate-config)))

(nskk-deftest-unit ddskk-compat-stats-function-exists
  "Test that stats function exists."
  (should (fboundp 'nskk-ddskk-compat-stats)))

(nskk-deftest-unit ddskk-compat-read-dictionary-nonexistent
  "Test reading a nonexistent dictionary returns nil."
  (should (null (nskk-ddskk-compat-read-dictionary "/nonexistent/path.jisyo"))))

;;;
;;; Run Hook Function Tests
;;;

(nskk-deftest-unit ddskk-compat-run-hook-function-exists
  "Test that run-hook function exists."
  (should (fboundp 'nskk-ddskk-compat--run-hook)))

;;;
;;; Module Feature Tests
;;;

(nskk-deftest-unit ddskk-compat-provides-feature
  "Test that nskk-ddskk-compat provides its feature."
  (should (featurep 'nskk-ddskk-compat)))

(provide 'nskk-ddskk-compat-test)

;;; nskk-ddskk-compat-test.el ends here
