;;; nskk-keymap-test.el --- Keymap tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Comprehensive tests for nskk-keymap.el covering:
;; - Feature loading
;; - Module provides the expected feature
;; - Keymap module loads without error
;; - nskk-mode-map structure and key bindings
;; - Input commands API availability via keymap dependency
;; NOTE: nskk-mode-map state-aware dispatch bindings are currently disabled.
;; Global bindings (C-x C-j, C-j) defined in nskk.el are tested here.

;;; Code:

(require 'ert)
(require 'nskk-keymap)
(require 'nskk-henkan)
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

(nskk-deftest-unit keymap-depends-on-input
  "Test that nskk-input is loaded as dependency."
  (should (featurep 'nskk-input)))

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
;;; Input Commands API Availability Tests
;;;

(nskk-deftest-unit keymap-app-layer-toggle-defined
  "Test that nskk-toggle-japanese-mode is available from input commands."
  (should (fboundp 'nskk-toggle-japanese-mode)))

(nskk-deftest-unit keymap-app-layer-commit-defined
  "Test that nskk-commit-current is available from input commands."
  (should (fboundp 'nskk-commit-current)))

(nskk-deftest-unit keymap-app-layer-cancel-defined
  "Test that nskk-cancel-conversion is available from input commands."
  (should (fboundp 'nskk-cancel-conversion)))

(nskk-deftest-unit keymap-app-layer-convert-defined
  "Test that nskk-convert-or-commit is available from input commands."
  (should (fboundp 'nskk-convert-or-commit)))

(nskk-deftest-unit keymap-app-layer-hiragana-defined
  "Test that nskk-set-mode-hiragana is available from input commands."
  (should (fboundp 'nskk-set-mode-hiragana)))

(nskk-deftest-unit keymap-app-layer-katakana-defined
  "Test that nskk-set-mode-katakana is available from input commands."
  (should (fboundp 'nskk-set-mode-katakana)))

(nskk-deftest-unit keymap-app-layer-latin-defined
  "Test that nskk-set-mode-latin is available from input commands."
  (should (fboundp 'nskk-set-mode-latin)))

(nskk-deftest-unit keymap-app-layer-abbrev-defined
  "Test that nskk-set-mode-abbrev is available from input commands."
  (should (fboundp 'nskk-set-mode-abbrev)))

;;;
;;; Interactive Command Tests
;;;

(nskk-deftest-unit keymap-toggle-is-interactive
  "Test nskk-toggle-japanese-mode is an interactive command."
  (should (commandp 'nskk-toggle-japanese-mode)))

(nskk-deftest-unit keymap-commit-is-interactive
  "Test nskk-commit-current is an interactive command."
  (should (commandp 'nskk-commit-current)))

(nskk-deftest-unit keymap-cancel-is-interactive
  "Test nskk-cancel-conversion is an interactive command."
  (should (commandp 'nskk-cancel-conversion)))

(nskk-deftest-unit keymap-convert-or-commit-is-interactive
  "Test nskk-convert-or-commit is an interactive command."
  (should (commandp 'nskk-convert-or-commit)))

(nskk-deftest-unit keymap-hiragana-mode-is-interactive
  "Test nskk-set-mode-hiragana is an interactive command."
  (should (commandp 'nskk-set-mode-hiragana)))

(nskk-deftest-unit keymap-katakana-mode-is-interactive
  "Test nskk-set-mode-katakana is an interactive command."
  (should (commandp 'nskk-set-mode-katakana)))

(nskk-deftest-unit keymap-latin-mode-is-interactive
  "Test nskk-set-mode-latin is an interactive command."
  (should (commandp 'nskk-set-mode-latin)))

(nskk-deftest-unit keymap-abbrev-mode-is-interactive
  "Test nskk-set-mode-abbrev is an interactive command."
  (should (commandp 'nskk-set-mode-abbrev)))

(nskk-deftest-unit keymap-app-layer-jisx0208-latin-defined
  "Test that nskk-set-mode-jisx0208-latin is available from input commands."
  (should (fboundp 'nskk-set-mode-jisx0208-latin)))

(nskk-deftest-unit keymap-jisx0208-latin-mode-is-interactive
  "Test nskk-set-mode-jisx0208-latin is an interactive command."
  (should (commandp 'nskk-set-mode-jisx0208-latin)))

;;;
;;; Behavioral Tests for Input Commands via Keymap
;;;

(nskk-deftest-unit keymap-mode-switch-via-app-layer
  "Test that mode switching works when invoked through input commands API."
  (let ((nskk-current-state (nskk-state-create 'ascii)))
    (nskk-set-mode-hiragana)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
    (nskk-set-mode-katakana)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))
    (nskk-set-mode-latin)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))))

(nskk-deftest-unit keymap-toggle-japanese-mode-behavior
  "Test that toggle-japanese-mode works correctly via keymap dependency."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (nskk-toggle-japanese-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))
    (nskk-toggle-japanese-mode)
    (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))

;;;
;;; Handler Dispatch Tests
;;;

(nskk-deftest-unit keymap-handle-q-toggles-in-hiragana
  "Test handle-q toggles to katakana when in hiragana."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (nskk-handle-q)
    (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

(nskk-deftest-unit keymap-handle-q-inserts-in-ascii
  "Test handle-q self-inserts when in ascii mode."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'ascii))
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
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (nskk-handle-l)
    (should (eq (nskk-state-mode nskk-current-state) 'latin))))

(nskk-deftest-unit keymap-handle-l-inserts-in-ascii
  "Test handle-l self-inserts when in ascii mode."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'ascii))
          (last-command-event ?l))
      (nskk-handle-l)
      (should (equal (buffer-string) "l")))))

(nskk-deftest-unit keymap-handle-space-inserts-when-no-preedit
  "Test handle-space inserts space when no preedit."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (last-command-event ? ))
      (nskk-handle-space)
      (should (equal (buffer-string) " ")))))

(nskk-deftest-unit keymap-handle-space-starts-conversion
  "Test handle-space starts conversion when preedit exists."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      ;; Set up preedit: marker at beginning, ▽ marker then text after it
      (nskk--set-conversion-start-marker (point-min))
      (insert "\u25BDtest")
      ;; Set henkan-phase to 'on (preedit mode) before pressing space
      (nskk-state-set-henkan-phase nskk-current-state 'on)
      ;; Mock search to return candidates
      (cl-letf (((symbol-function 'nskk-core-search)
                 (lambda (_k &optional _t _l)
                   '("result"))))
        (nskk-handle-space)
        (should (nskk-converting-p))
        ;; Cleanup overlay
        (when (overlayp nskk--conversion-overlay)
          (delete-overlay nskk--conversion-overlay))))))

(nskk-deftest-unit keymap-handle-return-newline-when-not-converting
  "Test handle-return inserts newline when not converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-handle-return)
      (should (equal (buffer-string) "\n")))))

(nskk-deftest-unit keymap-handle-return-commits-and-newline-when-converting
  "Test handle-return commits and inserts newline when in conversion."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-handle-return)
      (should-not (nskk-converting-p))
      ;; ddskk style: commit + newline
      (should (equal (buffer-string) "result\n")))))

;;;
;;; New Handler Tests (L, /, x, C-g)
;;;

(nskk-deftest-unit keymap-handle-upper-l-enters-jisx0208-latin
  "Test handle-upper-l enters jisx0208-latin mode when in hiragana."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (nskk-handle-upper-l)
    (should (eq (nskk-state-mode nskk-current-state) 'jisx0208-latin))))

(nskk-deftest-unit keymap-handle-upper-l-inserts-in-ascii
  "Test handle-upper-l self-inserts when in ascii mode."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'ascii))
          (last-command-event ?L))
      (nskk-handle-upper-l)
      (should (equal (buffer-string) "L")))))

(nskk-deftest-unit keymap-handle-upper-l-implicit-kakutei
  "Test handle-upper-l does implicit kakutei when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-handle-upper-l)
      ;; Should have committed and switched mode
      (should-not (nskk-converting-p))
      (should (eq (nskk-state-mode nskk-current-state) 'jisx0208-latin)))))

(nskk-deftest-unit keymap-handle-slash-enters-abbrev
  "Test handle-slash enters abbrev mode when in hiragana."
  (let ((nskk-current-state (nskk-state-create 'hiragana)))
    (nskk-handle-slash)
    (should (eq (nskk-state-mode nskk-current-state) 'abbrev))))

(nskk-deftest-unit keymap-handle-slash-inserts-in-ascii
  "Test handle-slash self-inserts when in ascii mode."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'ascii))
          (last-command-event ?/))
      (nskk-handle-slash)
      (should (equal (buffer-string) "/")))))

(nskk-deftest-unit keymap-handle-x-inserts-when-not-converting
  "Test handle-x self-inserts when not converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (last-command-event ?x))
      (nskk-handle-x)
      (should (equal (buffer-string) "x")))))

(nskk-deftest-unit keymap-handle-cancel-keyboard-quit-when-not-converting
  "Test handle-cancel calls keyboard-quit when not converting."
  (let ((nskk-current-state (nskk-state-create 'hiragana))
        (nskk--conversion-start-marker nil)
        (quit-called nil))
    ;; keyboard-quit signals 'quit which is not catchable by should-error
    ;; in batch mode, so we mock keyboard-quit instead
    (cl-letf (((symbol-function 'keyboard-quit)
               (lambda () (setq quit-called t))))
      (nskk-handle-cancel)
      (should quit-called))))

(nskk-deftest-unit keymap-handle-x-previous-candidate-when-converting
  "Test handle-x calls nskk-previous-candidate when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (prev-candidate-called nil))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (cl-letf (((symbol-function 'nskk-previous-candidate)
                 (lambda () (setq prev-candidate-called t))))
        (nskk-handle-x)
        (should prev-candidate-called)))))

(nskk-deftest-unit keymap-handle-space-next-candidate-when-converting
  "Test handle-space calls nskk-next-candidate when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (next-candidate-called nil))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (cl-letf (((symbol-function 'nskk-next-candidate)
                 (lambda () (setq next-candidate-called t))))
        (nskk-handle-space)
        (should next-candidate-called)))))

(nskk-deftest-unit keymap-handle-cancel-cancels-when-converting
  "Test handle-cancel calls nskk-cancel-conversion when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (cancel-called nil))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (cl-letf (((symbol-function 'nskk-cancel-conversion)
                 (lambda () (setq cancel-called t))))
        (nskk-handle-cancel)
        (should cancel-called)))))

(nskk-deftest-unit keymap-handle-q-implicit-kakutei
  "Test handle-q does implicit kakutei when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-handle-q)
      ;; Should have committed and toggled mode
      (should-not (nskk-converting-p)))))

(nskk-deftest-unit keymap-handle-l-implicit-kakutei
  "Test handle-l does implicit kakutei when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-handle-l)
      ;; Should have committed and entered latin
      (should-not (nskk-converting-p))
      (should (eq (nskk-state-mode nskk-current-state) 'latin)))))

;;;
;;; Handler Function Existence Tests
;;;

(nskk-deftest-unit keymap-handle-q-defined
  "Test that nskk-handle-q is defined and interactive."
  (should (fboundp 'nskk-handle-q))
  (should (commandp 'nskk-handle-q)))

(nskk-deftest-unit keymap-handle-l-defined
  "Test that nskk-handle-l is defined and interactive."
  (should (fboundp 'nskk-handle-l))
  (should (commandp 'nskk-handle-l)))

(nskk-deftest-unit keymap-handle-upper-l-defined
  "Test that nskk-handle-upper-l is defined and interactive."
  (should (fboundp 'nskk-handle-upper-l))
  (should (commandp 'nskk-handle-upper-l)))

(nskk-deftest-unit keymap-handle-slash-defined
  "Test that nskk-handle-slash is defined and interactive."
  (should (fboundp 'nskk-handle-slash))
  (should (commandp 'nskk-handle-slash)))

(nskk-deftest-unit keymap-handle-x-defined
  "Test that nskk-handle-x is defined and interactive."
  (should (fboundp 'nskk-handle-x))
  (should (commandp 'nskk-handle-x)))

(nskk-deftest-unit keymap-handle-space-defined
  "Test that nskk-handle-space is defined and interactive."
  (should (fboundp 'nskk-handle-space))
  (should (commandp 'nskk-handle-space)))

(nskk-deftest-unit keymap-handle-return-defined
  "Test that nskk-handle-return is defined and interactive."
  (should (fboundp 'nskk-handle-return))
  (should (commandp 'nskk-handle-return)))

(nskk-deftest-unit keymap-handle-cancel-defined
  "Test that nskk-handle-cancel is defined and interactive."
  (should (fboundp 'nskk-handle-cancel))
  (should (commandp 'nskk-handle-cancel)))

(provide 'nskk-keymap-test)

;;; nskk-keymap-test.el ends here
