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

;;;
;;; Handler Dispatch Tests
;;;

(nskk-deftest-unit keymap-handle-q-toggles-in-hiragana
  "Test handle-q toggles to katakana when in hiragana."
  (let ((nskk-current-state (nskk-state-create 'hiragana))
        (nskk-converting-active nil))
    (nskk-handle-q)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(nskk-deftest-unit keymap-handle-q-inserts-in-ascii
  "Test handle-q self-inserts when in ascii mode."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'ascii))
          (nskk-converting-active nil)
          (last-command-event ?q))
      (nskk-handle-q)
      (should (equal (buffer-string) "q")))))

(nskk-deftest-unit keymap-handle-q-inserts-when-no-state
  "Test handle-q self-inserts when state is nil."
  (with-temp-buffer
    (let ((nskk-current-state nil)
          (last-command-event ?q))
      (nskk-handle-q)
      (should (equal (buffer-string) "q")))))

(nskk-deftest-unit keymap-handle-l-enters-latin
  "Test handle-l enters latin mode when in hiragana."
  (let ((nskk-current-state (nskk-state-create 'hiragana))
        (nskk-converting-active nil))
    (nskk-handle-l)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))))

(nskk-deftest-unit keymap-handle-l-inserts-in-ascii
  "Test handle-l self-inserts when in ascii mode."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'ascii))
          (nskk-converting-active nil)
          (last-command-event ?l))
      (nskk-handle-l)
      (should (equal (buffer-string) "l")))))

(nskk-deftest-unit keymap-handle-space-inserts-when-no-preedit
  "Test handle-space inserts space when no preedit."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-converting-active nil)
          (last-command-event ? ))
      (nskk-handle-space)
      (should (equal (buffer-string) " ")))))

(nskk-deftest-unit keymap-handle-space-starts-conversion
  "Test handle-space starts conversion when preedit exists."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-converting-active nil))
      ;; Set up preedit: marker at beginning, ▽ marker then text after it
      (nskk--set-conversion-start-marker (point-min))
      (insert "\u25BDtest")
      ;; Mock search to return candidates
      (cl-letf (((symbol-function 'nskk-core-search)
                 (lambda (_k &optional _t _l)
                   '("result"))))
        (nskk-handle-space)
        (should nskk-converting-active)
        ;; Cleanup overlay
        (when (overlayp nskk--conversion-overlay)
          (delete-overlay nskk--conversion-overlay))))))

(nskk-deftest-unit keymap-handle-return-newline-when-not-converting
  "Test handle-return inserts newline when not converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-converting-active nil))
      (nskk-handle-return)
      (should (equal (buffer-string) "\n")))))

(nskk-deftest-unit keymap-handle-return-commits-and-newline-when-converting
  "Test handle-return commits and inserts newline when in conversion."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-converting-active nil))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (setq nskk-converting-active t)
      (nskk-handle-return)
      (should-not nskk-converting-active)
      ;; ddskk style: commit + newline
      (should (equal (buffer-string) "result\n")))))

;;;
;;; New Handler Tests (L, /, x, C-g)
;;;

(nskk-deftest-unit keymap-handle-upper-l-enters-jisx0208-latin
  "Test handle-upper-l enters jisx0208-latin mode when in hiragana."
  (let ((nskk-current-state (nskk-state-create 'hiragana))
        (nskk-converting-active nil))
    (nskk-handle-upper-l)
    (should (eq (nskk-state-mode nskk-current-state) 'jisx0208-latin))))

(nskk-deftest-unit keymap-handle-upper-l-inserts-in-ascii
  "Test handle-upper-l self-inserts when in ascii mode."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'ascii))
          (nskk-converting-active nil)
          (last-command-event ?L))
      (nskk-handle-upper-l)
      (should (equal (buffer-string) "L")))))

(nskk-deftest-unit keymap-handle-upper-l-implicit-kakutei
  "Test handle-upper-l does implicit kakutei when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-converting-active nil))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (setq nskk-converting-active t)
      (nskk-handle-upper-l)
      ;; Should have committed and switched mode
      (should-not nskk-converting-active)
      (should (eq (nskk-state-mode nskk-current-state) 'jisx0208-latin)))))

(nskk-deftest-unit keymap-handle-slash-enters-abbrev
  "Test handle-slash enters abbrev mode when in hiragana."
  (let ((nskk-current-state (nskk-state-create 'hiragana))
        (nskk-converting-active nil))
    (nskk-handle-slash)
    (should (eq (nskk-state-mode nskk-current-state) 'abbrev))))

(nskk-deftest-unit keymap-handle-slash-inserts-in-ascii
  "Test handle-slash self-inserts when in ascii mode."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'ascii))
          (nskk-converting-active nil)
          (last-command-event ?/))
      (nskk-handle-slash)
      (should (equal (buffer-string) "/")))))

(nskk-deftest-unit keymap-handle-x-inserts-when-not-converting
  "Test handle-x self-inserts when not converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-converting-active nil)
          (last-command-event ?x))
      (nskk-handle-x)
      (should (equal (buffer-string) "x")))))

(nskk-deftest-unit keymap-handle-cancel-keyboard-quit-when-not-converting
  "Test handle-cancel calls keyboard-quit when not converting."
  (let ((nskk-current-state (nskk-state-create 'hiragana))
        (nskk-converting-active nil)
        (nskk--conversion-start-marker nil)
        (quit-called nil))
    ;; keyboard-quit signals 'quit which is not catchable by should-error
    ;; in batch mode, so we mock keyboard-quit instead
    (cl-letf (((symbol-function 'keyboard-quit)
               (lambda () (setq quit-called t))))
      (nskk-handle-cancel)
      (should quit-called))))

(nskk-deftest-unit keymap-handle-q-implicit-kakutei
  "Test handle-q does implicit kakutei when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-converting-active nil))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (setq nskk-converting-active t)
      (nskk-handle-q)
      ;; Should have committed and toggled mode
      (should-not nskk-converting-active))))

(nskk-deftest-unit keymap-handle-l-implicit-kakutei
  "Test handle-l does implicit kakutei when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk-converting-active nil))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (setq nskk-converting-active t)
      (nskk-handle-l)
      ;; Should have committed and entered latin
      (should-not nskk-converting-active)
      (should (eq (nskk-state-mode nskk-current-state) 'latin)))))

;;;
;;; Handler Function Existence Tests
;;;

(nskk-deftest-unit keymap-handle-upper-l-defined
  "Test that nskk-handle-upper-l is defined."
  (should (fboundp 'nskk-handle-upper-l))
  (should (commandp 'nskk-handle-upper-l)))

(nskk-deftest-unit keymap-handle-slash-defined
  "Test that nskk-handle-slash is defined."
  (should (fboundp 'nskk-handle-slash))
  (should (commandp 'nskk-handle-slash)))

(nskk-deftest-unit keymap-handle-x-defined
  "Test that nskk-handle-x is defined."
  (should (fboundp 'nskk-handle-x))
  (should (commandp 'nskk-handle-x)))

(nskk-deftest-unit keymap-handle-cancel-defined
  "Test that nskk-handle-cancel is defined."
  (should (fboundp 'nskk-handle-cancel))
  (should (commandp 'nskk-handle-cancel)))

(provide 'nskk-keymap-test)

;;; nskk-keymap-test.el ends here
