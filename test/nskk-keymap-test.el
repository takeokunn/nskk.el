;;; nskk-keymap-test.el --- Keymap tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-keymap.el covering:
;; - Feature loading
;; - Module provides the expected feature
;; - Keymap module loads without error
;; - nskk-mode-map structure and key bindings
;; - Application layer API availability via keymap dependency
;; NOTE: nskk-mode-map state-aware dispatch bindings are currently disabled.
;; Global bindings (C-x C-j, C-j) defined in nskk.el are tested here.

;;; Code:

(require 'ert)
(require 'nskk-keymap)
(require 'nskk-state)
(require 'nskk-test-framework)

;;;
;;; Feature Loading Tests
;;;

(nskk-deftest-unit keymap-feature-provided
  "Test that nskk-keymap feature is provided."
  (should (featurep 'nskk-keymap)))

(nskk-deftest-unit keymap-require-idempotent
  "Test that requiring nskk-keymap again is safe."
  (should (require 'nskk-keymap)))

(nskk-deftest-unit keymap-depends-on-layer-application
  "Test that nskk-layer-application is loaded as dependency."
  (should (featurep 'nskk-layer-application)))

;;;
;;; nskk-mode-map Structure Tests
;;;

(nskk-deftest-unit keymap-mode-map-is-keymap
  "Test that nskk-mode-map is a valid keymap."
  (should (keymapp nskk-mode-map)))

(nskk-deftest-unit keymap-mode-map-is-sparse-keymap
  "Test that nskk-mode-map is a sparse keymap (starts with keymap symbol)."
  (should (eq (car nskk-mode-map) 'keymap)))

(nskk-deftest-unit keymap-c-x-c-j-bound
  "Test that C-x C-j is bound in nskk-mode-map."
  (let ((binding (lookup-key nskk-mode-map (kbd "C-x C-j"))))
    (should binding)
    (should (commandp binding))))

(nskk-deftest-unit keymap-c-j-bound
  "Test that C-j is bound in nskk-mode-map."
  (let ((binding (lookup-key nskk-mode-map (kbd "C-j"))))
    (should binding)
    (should (commandp binding))))

;;;
;;; Application Layer API Availability Tests
;;;

(nskk-deftest-unit keymap-app-layer-toggle-defined
  "Test that nskk-toggle-japanese-mode is available from app layer."
  (should (fboundp 'nskk-toggle-japanese-mode)))

(nskk-deftest-unit keymap-app-layer-commit-defined
  "Test that nskk-commit is available from app layer."
  (should (fboundp 'nskk-commit)))

(nskk-deftest-unit keymap-app-layer-cancel-defined
  "Test that nskk-cancel is available from app layer."
  (should (fboundp 'nskk-cancel)))

(nskk-deftest-unit keymap-app-layer-convert-defined
  "Test that nskk-convert-or-commit-selection is available from app layer."
  (should (fboundp 'nskk-convert-or-commit-selection)))

(nskk-deftest-unit keymap-app-layer-hiragana-defined
  "Test that nskk-enter-hiragana-mode is available from app layer."
  (should (fboundp 'nskk-enter-hiragana-mode)))

(nskk-deftest-unit keymap-app-layer-katakana-defined
  "Test that nskk-enter-katakana-mode is available from app layer."
  (should (fboundp 'nskk-enter-katakana-mode)))

(nskk-deftest-unit keymap-app-layer-latin-defined
  "Test that nskk-enter-latin-mode is available from app layer."
  (should (fboundp 'nskk-enter-latin-mode)))

(nskk-deftest-unit keymap-app-layer-abbrev-defined
  "Test that nskk-enter-abbrev-mode is available from app layer."
  (should (fboundp 'nskk-enter-abbrev-mode)))

;;;
;;; Interactive Command Tests
;;;

(nskk-deftest-unit keymap-toggle-is-interactive
  "Test nskk-toggle-japanese-mode is an interactive command."
  (should (commandp 'nskk-toggle-japanese-mode)))

(nskk-deftest-unit keymap-commit-is-interactive
  "Test nskk-commit is an interactive command."
  (should (commandp 'nskk-commit)))

(nskk-deftest-unit keymap-cancel-is-interactive
  "Test nskk-cancel is an interactive command."
  (should (commandp 'nskk-cancel)))

(nskk-deftest-unit keymap-convert-or-commit-is-interactive
  "Test nskk-convert-or-commit-selection is an interactive command."
  (should (commandp 'nskk-convert-or-commit-selection)))

(nskk-deftest-unit keymap-hiragana-mode-is-interactive
  "Test nskk-enter-hiragana-mode is an interactive command."
  (should (commandp 'nskk-enter-hiragana-mode)))

(nskk-deftest-unit keymap-katakana-mode-is-interactive
  "Test nskk-enter-katakana-mode is an interactive command."
  (should (commandp 'nskk-enter-katakana-mode)))

;;;
;;; Behavioral Tests for Application Layer Commands via Keymap
;;;

(nskk-deftest-unit keymap-mode-switch-via-app-layer
  "Test that mode switching works when invoked through app layer API."
  (let ((nskk-current-state (nskk-state-create 'ascii))
        (nskk-converting-active nil))
    (nskk-enter-hiragana-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
    (nskk-enter-katakana-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))
    (nskk-enter-latin-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))))

(nskk-deftest-unit keymap-toggle-japanese-mode-behavior
  "Test that toggle-japanese-mode works correctly via keymap dependency."
  (let ((nskk-current-state (nskk-state-create 'hiragana))
        (nskk-converting-active nil))
    (nskk-toggle-japanese-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))
    (nskk-toggle-japanese-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

(provide 'nskk-keymap-test)

;;; nskk-keymap-test.el ends here
