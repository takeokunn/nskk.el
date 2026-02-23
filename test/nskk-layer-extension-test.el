;;; nskk-layer-extension-test.el --- Tests for nskk-layer-extension.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk-layer-extension.el covering:
;; - Hook variable definitions
;; - Extension management (load, unload, loaded-p)
;; - Event subscription helpers
;; - Hook to event bridge
;; - Extension initialization

;;; Code:

(require 'ert)
(require 'nskk-layer-extension)
(require 'nskk-test-framework)

;;;
;;; Hook Variable Tests
;;;

(nskk-deftest-unit layer-ext-mode-hook-defined
  "Test that nskk-mode-hook is defined."
  (should (boundp 'nskk-mode-hook)))

(nskk-deftest-unit layer-ext-mode-off-hook-defined
  "Test that nskk-mode-off-hook is defined."
  (should (boundp 'nskk-mode-off-hook)))

(nskk-deftest-unit layer-ext-input-mode-hook-defined
  "Test that nskk-input-mode-hook is defined."
  (should (boundp 'nskk-input-mode-hook)))

(nskk-deftest-unit layer-ext-start-henkan-hook-defined
  "Test that nskk-start-henkan-hook is defined."
  (should (boundp 'nskk-start-henkan-hook)))

(nskk-deftest-unit layer-ext-henkan-hook-defined
  "Test that nskk-henkan-hook is defined."
  (should (boundp 'nskk-henkan-hook)))

(nskk-deftest-unit layer-ext-post-henkan-hook-defined
  "Test that nskk-post-henkan-hook is defined."
  (should (boundp 'nskk-post-henkan-hook)))

(nskk-deftest-unit layer-ext-after-henkan-hook-defined
  "Test that nskk-after-henkan-hook is defined."
  (should (boundp 'nskk-after-henkan-hook)))

(nskk-deftest-unit layer-ext-henkan-select-hook-defined
  "Test that nskk-henkan-select-hook is defined."
  (should (boundp 'nskk-henkan-select-hook)))

(nskk-deftest-unit layer-ext-search-jisyo-hook-defined
  "Test that nskk-search-jisyo-hook is defined."
  (should (boundp 'nskk-search-jisyo-hook)))

(nskk-deftest-unit layer-ext-jisyo-update-hook-defined
  "Test that nskk-jisyo-update-hook is defined."
  (should (boundp 'nskk-jisyo-update-hook)))

(nskk-deftest-unit layer-ext-save-history-hook-defined
  "Test that nskk-save-history-hook is defined."
  (should (boundp 'nskk-save-history-hook)))

(nskk-deftest-unit layer-ext-start-henkan-hook-permanent
  "Test that nskk-start-henkan-hook is marked permanent-local."
  (should (get 'nskk-start-henkan-hook 'permanent-local)))

;;;
;;; Extension Management Tests
;;;

(nskk-deftest-unit layer-ext-extensions-loaded-var-exists
  "Test that nskk-extensions-loaded variable exists."
  (should (boundp 'nskk-extensions-loaded)))

(nskk-deftest-unit layer-ext-load-nonexistent-returns-nil
  "Test that loading a non-existent extension returns nil."
  (should (not (nskk-extension-load 'nskk-nonexistent-extension-xyz))))

(nskk-deftest-unit layer-ext-loaded-p-false-for-unknown
  "Test that loaded-p returns nil for unknown extension."
  (should (not (nskk-extension-loaded-p 'nskk-nonexistent-extension-xyz))))

(nskk-deftest-unit layer-ext-load-existing-feature
  "Test loading an already-provided feature."
  (let ((nskk-extensions-loaded nil))
    ;; nskk-events is already loaded, so this should succeed
    (should (nskk-extension-load 'nskk-events))
    (should (nskk-extension-loaded-p 'nskk-events))))

(nskk-deftest-unit layer-ext-unload-loaded-extension
  "Test unloading a loaded extension."
  (let ((nskk-extensions-loaded '(test-ext)))
    (should (nskk-extension-unload 'test-ext))
    (should (not (nskk-extension-loaded-p 'test-ext)))))

(nskk-deftest-unit layer-ext-unload-not-loaded-returns-nil
  "Test unloading a not-loaded extension returns nil."
  (let ((nskk-extensions-loaded nil))
    (should (not (nskk-extension-unload 'never-loaded)))))

;;;
;;; Event Subscription Helper Tests
;;;

(nskk-deftest-unit layer-ext-event-prop-extracts-value
  "Test that nskk-event-prop extracts property from plist."
  (should (string= (nskk-event-prop :name '(:name "test" :value 42)) "test"))
  (should (= (nskk-event-prop :value '(:name "test" :value 42)) 42))
  (should (null (nskk-event-prop :missing '(:name "test")))))

(nskk-deftest-unit layer-ext-hook-to-event-bridge-exists
  "Test that hook-to-event-bridge function exists."
  (should (fboundp 'nskk-hook-to-event-bridge)))

;;;
;;; Extension Init Tests
;;;

(nskk-deftest-unit layer-ext-extension-init-exists
  "Test that extension init function exists."
  (should (fboundp 'nskk-extension-init)))

;;;
;;; Module Feature Tests
;;;

(nskk-deftest-unit layer-ext-provides-feature
  "Test that nskk-layer-extension provides its feature."
  (should (featurep 'nskk-layer-extension)))

(provide 'nskk-layer-extension-test)

;;; nskk-layer-extension-test.el ends here
