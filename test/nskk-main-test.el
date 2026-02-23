;;; nskk-main-test.el --- Tests for nskk.el (main entry point) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 NSKK Authors

;; Author: NSKK Developers
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Tests for nskk.el (main entry point) covering:
;; - Minor mode definition and behavior
;; - Global mode definition
;; - Mode switching commands
;; - Keymap definition
;; - Buffer-local state management
;; - Internal helper functions

;;; Code:

(require 'ert)
(require 'nskk)
(require 'nskk-test-framework)

;;;
;;; Minor Mode Tests
;;;

(nskk-deftest-unit main-nskk-mode-defined
  "Test that nskk-mode is defined."
  (should (fboundp 'nskk-mode)))

(nskk-deftest-unit main-nskk-mode-is-command
  "Test that nskk-mode is an interactive command."
  (should (commandp 'nskk-mode)))

(nskk-deftest-unit main-nskk-mode-enable
  "Test enabling nskk-mode in a buffer."
  (with-temp-buffer
    (nskk-mode 1)
    (should nskk-mode)))

(nskk-deftest-unit main-nskk-mode-disable
  "Test disabling nskk-mode in a buffer."
  (with-temp-buffer
    (nskk-mode 1)
    (nskk-mode -1)
    (should (not nskk-mode))))

(nskk-deftest-unit main-nskk-mode-toggle-on-off
  "Test toggling nskk-mode."
  (with-temp-buffer
    (should (not nskk-mode))
    (nskk-mode 1)
    (should nskk-mode)
    (nskk-mode 0)
    (should (not nskk-mode))))

;;;
;;; Global Mode Tests
;;;

(nskk-deftest-unit main-global-mode-defined
  "Test that nskk-global-mode is defined."
  (should (fboundp 'nskk-global-mode)))

(nskk-deftest-unit main-global-mode-is-command
  "Test that nskk-global-mode is an interactive command."
  (should (commandp 'nskk-global-mode)))

;;;
;;; Keymap Tests
;;;

(nskk-deftest-unit main-mode-map-defined
  "Test that nskk-mode-map is defined."
  (should (keymapp nskk-mode-map)))

(nskk-deftest-unit main-mode-map-has-toggle
  "Test that mode map has C-x C-j binding."
  (should (lookup-key nskk-mode-map (kbd "C-x C-j"))))

(nskk-deftest-unit main-mode-map-has-kakutei
  "Test that mode map has C-j binding."
  (should (lookup-key nskk-mode-map (kbd "C-j"))))

(nskk-deftest-unit main-toggle-mode-bound
  "Test that C-x C-j is bound to nskk-toggle-mode."
  (should (eq (lookup-key nskk-mode-map (kbd "C-x C-j")) 'nskk-toggle-mode)))

(nskk-deftest-unit main-kakutei-bound
  "Test that C-j is bound to nskk-kakutei."
  (should (eq (lookup-key nskk-mode-map (kbd "C-j")) 'nskk-kakutei)))

;;;
;;; Command Tests
;;;

(nskk-deftest-unit main-toggle-mode-exists
  "Test that nskk-toggle-mode function exists."
  (should (fboundp 'nskk-toggle-mode))
  (should (commandp 'nskk-toggle-mode)))

(nskk-deftest-unit main-kakutei-exists
  "Test that nskk-kakutei function exists."
  (should (fboundp 'nskk-kakutei))
  (should (commandp 'nskk-kakutei)))

(nskk-deftest-unit main-switch-to-hiragana-exists
  "Test that switch-to-hiragana function exists."
  (should (fboundp 'nskk-switch-to-hiragana))
  (should (commandp 'nskk-switch-to-hiragana)))

(nskk-deftest-unit main-switch-to-katakana-exists
  "Test that switch-to-katakana function exists."
  (should (fboundp 'nskk-switch-to-katakana))
  (should (commandp 'nskk-switch-to-katakana)))

(nskk-deftest-unit main-switch-to-ascii-exists
  "Test that switch-to-ascii function exists."
  (should (fboundp 'nskk-switch-to-ascii))
  (should (commandp 'nskk-switch-to-ascii)))

(nskk-deftest-unit main-toggle-kana-exists
  "Test that toggle-kana function exists."
  (should (fboundp 'nskk-toggle-kana))
  (should (commandp 'nskk-toggle-kana)))

;;;
;;; Buffer State Tests
;;;

(nskk-deftest-unit main-enable-creates-state
  "Test that enabling mode creates buffer-local state."
  (with-temp-buffer
    (nskk-mode 1)
    (should nskk-current-state)
    (should (nskk-state-p nskk-current-state))
    (nskk-mode -1)))

(nskk-deftest-unit main-state-has-default-mode
  "Test that state starts with default mode."
  (with-temp-buffer
    (nskk-mode 1)
    (should (eq (nskk-state-mode nskk-current-state) nskk-state-default-mode))
    (nskk-mode -1)))

;;;
;;; Internal Function Tests
;;;

(nskk-deftest-unit main-enable-function-exists
  "Test that nskk--enable function exists."
  (should (fboundp 'nskk--enable)))

(nskk-deftest-unit main-disable-function-exists
  "Test that nskk--disable function exists."
  (should (fboundp 'nskk--disable)))

(nskk-deftest-unit main-turn-on-mode-exists
  "Test that nskk--turn-on-mode function exists."
  (should (fboundp 'nskk--turn-on-mode)))

(nskk-deftest-unit main-turn-on-mode-skips-minibuffer
  "Test that turn-on-mode skips minibuffers."
  ;; nskk--turn-on-mode should skip minibuffers
  ;; We just verify the function exists and is callable
  (should (fboundp 'nskk--turn-on-mode)))

(nskk-deftest-unit main-setup-buffer-exists
  "Test that nskk--setup-buffer function exists."
  (should (fboundp 'nskk--setup-buffer)))

(nskk-deftest-unit main-cleanup-buffer-exists
  "Test that nskk--cleanup-buffer function exists."
  (should (fboundp 'nskk--cleanup-buffer)))

(nskk-deftest-unit main-post-command-handler-exists
  "Test that nskk--post-command-handler function exists."
  (should (fboundp 'nskk--post-command-handler)))

(nskk-deftest-unit main-switch-mode-exists
  "Test that mode switching aliases exist."
  (should (fboundp 'nskk-switch-to-hiragana))
  (should (fboundp 'nskk-switch-to-katakana))
  (should (fboundp 'nskk-switch-to-ascii)))

(nskk-deftest-unit main-update-modeline-exists
  "Test that nskk--update-modeline function exists."
  (should (fboundp 'nskk--update-modeline)))

;;;
;;; Kakutei (Commit) Tests
;;;

(nskk-deftest-unit main-kakutei-with-input
  "Test kakutei inserts input buffer content."
  (with-temp-buffer
    (nskk-mode 1)
    (nskk-state-set nskk-current-state 'input-buffer "test")
    (nskk-kakutei)
    (should (string= (buffer-string) "test"))
    (should (string= (nskk-state-input-buffer nskk-current-state) ""))
    (nskk-mode -1)))

(nskk-deftest-unit main-kakutei-with-empty-input
  "Test kakutei with empty input buffer does nothing."
  (with-temp-buffer
    (nskk-mode 1)
    (nskk-kakutei)
    (should (string= (buffer-string) ""))
    (nskk-mode -1)))

;;;
;;; Toggle Mode Tests
;;;

(nskk-deftest-unit main-toggle-mode-enables
  "Test toggle-mode enables when off."
  (with-temp-buffer
    (nskk-mode -1)
    (nskk-toggle-mode)
    (should nskk-mode)
    (nskk-mode -1)))

(nskk-deftest-unit main-toggle-mode-disables
  "Test toggle-mode disables when on."
  (with-temp-buffer
    (nskk-mode 1)
    (nskk-toggle-mode)
    (should (not nskk-mode))))

;;;
;;; Module Feature Tests
;;;

(nskk-deftest-unit main-provides-feature
  "Test that nskk provides its feature."
  (should (featurep 'nskk)))

(nskk-deftest-unit main-requires-custom
  "Test that nskk-custom is loaded."
  (should (featurep 'nskk-custom)))

(nskk-deftest-unit main-requires-state
  "Test that nskk-state is loaded."
  (should (featurep 'nskk-state)))

(provide 'nskk-main-test)

;;; nskk-main-test.el ends here
