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

(nskk-deftest-unit keymap-handle-return-commits-no-newline-when-converting
  "Test handle-return commits without newline when in conversion."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (nskk-handle-return)
      (should-not (nskk-converting-p))
      ;; nskk style: commit only (no newline)
      (should (equal (buffer-string) "result")))))

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

;;;
;;; nskk--current-kakutei-state Tests
;;;

(nskk-deftest-unit keymap-kakutei-state-converting
  "Test nskk--current-kakutei-state returns converting when nskk-converting-p is true."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () t))
            ((symbol-function 'nskk--has-preedit) (lambda () nil)))
    (should (eq (nskk--current-kakutei-state) 'converting))))

(nskk-deftest-unit keymap-kakutei-state-preedit
  "Test nskk--current-kakutei-state returns preedit when nskk--has-preedit is true."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit) (lambda () t)))
    (should (eq (nskk--current-kakutei-state) 'preedit))))

(nskk-deftest-unit keymap-kakutei-state-romaji-pending
  "Test nskk--current-kakutei-state returns romaji-pending when nskk--romaji-buffer is non-empty."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit) (lambda () nil)))
    (let ((nskk--romaji-buffer "k"))
      (should (eq (nskk--current-kakutei-state) 'romaji-pending)))))

(nskk-deftest-unit keymap-kakutei-state-japanese-idle
  "Test nskk--current-kakutei-state returns japanese-idle in hiragana mode with no pending input."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit) (lambda () nil)))
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--romaji-buffer ""))
      (should (eq (nskk--current-kakutei-state) 'japanese-idle)))))

(nskk-deftest-unit keymap-kakutei-state-japanese-idle-katakana
  "Test nskk--current-kakutei-state returns japanese-idle in katakana mode."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit) (lambda () nil)))
    (let ((nskk-current-state (nskk-state-create 'katakana))
          (nskk--romaji-buffer ""))
      (should (eq (nskk--current-kakutei-state) 'japanese-idle)))))

(nskk-deftest-unit keymap-kakutei-state-japanese-idle-katakana-半角
  "Test nskk--current-kakutei-state returns japanese-idle in katakana-半角 mode."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit) (lambda () nil)))
    (let ((nskk-current-state (nskk-state-create 'katakana-半角))
          (nskk--romaji-buffer ""))
      (should (eq (nskk--current-kakutei-state) 'japanese-idle)))))

(nskk-deftest-unit keymap-kakutei-state-direct-idle
  "Test nskk--current-kakutei-state returns direct-idle in ascii mode with no pending input."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit) (lambda () nil)))
    (let ((nskk-current-state (nskk-state-create 'ascii))
          (nskk--romaji-buffer ""))
      (should (eq (nskk--current-kakutei-state) 'direct-idle)))))

;;;
;;; C-n and C-p Handler Tests
;;;

(nskk-deftest-unit keymap-handle-ctrl-n-defined
  "Test that nskk-handle-ctrl-n is defined and interactive."
  (should (fboundp 'nskk-handle-ctrl-n))
  (should (commandp 'nskk-handle-ctrl-n)))

(nskk-deftest-unit keymap-handle-ctrl-p-defined
  "Test that nskk-handle-ctrl-p is defined and interactive."
  (should (fboundp 'nskk-handle-ctrl-p))
  (should (commandp 'nskk-handle-ctrl-p)))

(nskk-deftest-unit keymap-handle-ctrl-n-calls-next-candidate-when-converting
  "Test handle-ctrl-n calls nskk-next-candidate when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (called nil))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (cl-letf (((symbol-function 'nskk-next-candidate)
                 (lambda () (setq called t))))
        (nskk-handle-ctrl-n)
        (should called)))))

(nskk-deftest-unit keymap-handle-ctrl-p-calls-previous-candidate-when-converting
  "Test handle-ctrl-p calls nskk-previous-candidate when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (called nil))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (cl-letf (((symbol-function 'nskk-previous-candidate)
                 (lambda () (setq called t))))
        (nskk-handle-ctrl-p)
        (should called)))))

(nskk-deftest-unit keymap-handle-ctrl-n-calls-next-line-when-not-converting
  "Test handle-ctrl-n calls next-line when not in conversion mode."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (called nil))
      (cl-letf (((symbol-function 'next-line)
                 (lambda (&rest _) (interactive) (setq called t))))
        (nskk-handle-ctrl-n)
        (should called)))))

(nskk-deftest-unit keymap-handle-ctrl-p-calls-previous-line-when-not-converting
  "Test handle-ctrl-p calls previous-line when not in conversion mode."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (called nil))
      (cl-letf (((symbol-function 'previous-line)
                 (lambda (&rest _) (interactive) (setq called t))))
        (nskk-handle-ctrl-p)
        (should called)))))

(nskk-deftest-unit keymap-ctrl-n-bound-in-mode-map
  "Test that C-n is bound in nskk-mode-map."
  (let ((binding (lookup-key nskk-mode-map (kbd "C-n"))))
    (should (eq binding 'nskk-handle-ctrl-n))))

(nskk-deftest-unit keymap-ctrl-p-bound-in-mode-map
  "Test that C-p is bound in nskk-mode-map."
  (let ((binding (lookup-key nskk-mode-map (kbd "C-p"))))
    (should (eq binding 'nskk-handle-ctrl-p))))

(nskk-deftest-unit keymap-handle-ctrl-n-next-line-when-no-state
  "Test handle-ctrl-n calls next-line when nskk-current-state is nil."
  (with-temp-buffer
    (let ((nskk-current-state nil)
          (called nil))
      (cl-letf (((symbol-function 'next-line)
                 (lambda (&rest _) (interactive) (setq called t))))
        (nskk-handle-ctrl-n)
        (should called)))))

(nskk-deftest-unit keymap-handle-ctrl-p-previous-line-when-no-state
  "Test handle-ctrl-p calls previous-line when nskk-current-state is nil."
  (with-temp-buffer
    (let ((nskk-current-state nil)
          (called nil))
      (cl-letf (((symbol-function 'previous-line)
                 (lambda (&rest _) (interactive) (setq called t))))
        (nskk-handle-ctrl-p)
        (should called)))))

;;;
;;; C-f and C-b Handler Tests
;;;

(nskk-deftest-unit keymap-handle-ctrl-f-defined
  "Test that nskk-handle-ctrl-f is defined and interactive."
  (should (fboundp 'nskk-handle-ctrl-f))
  (should (commandp 'nskk-handle-ctrl-f)))

(nskk-deftest-unit keymap-handle-ctrl-b-defined
  "Test that nskk-handle-ctrl-b is defined and interactive."
  (should (fboundp 'nskk-handle-ctrl-b))
  (should (commandp 'nskk-handle-ctrl-b)))

(nskk-deftest-unit keymap-ctrl-f-bound-in-mode-map
  "Test that C-f is bound in nskk-mode-map."
  (let ((binding (lookup-key nskk-mode-map (kbd "C-f"))))
    (should (eq binding 'nskk-handle-ctrl-f))))

(nskk-deftest-unit keymap-ctrl-b-bound-in-mode-map
  "Test that C-b is bound in nskk-mode-map."
  (let ((binding (lookup-key nskk-mode-map (kbd "C-b"))))
    (should (eq binding 'nskk-handle-ctrl-b))))

(nskk-deftest-unit keymap-right-arrow-bound-in-mode-map
  "Test that [right] is bound to nskk-handle-ctrl-f in nskk-mode-map."
  (let ((binding (lookup-key nskk-mode-map [right])))
    (should (eq binding 'nskk-handle-ctrl-f))))

(nskk-deftest-unit keymap-left-arrow-bound-in-mode-map
  "Test that [left] is bound to nskk-handle-ctrl-b in nskk-mode-map."
  (let ((binding (lookup-key nskk-mode-map [left])))
    (should (eq binding 'nskk-handle-ctrl-b))))

(nskk-deftest-unit keymap-down-arrow-bound-in-mode-map
  "Test that [down] is bound to nskk-handle-ctrl-n in nskk-mode-map."
  (let ((binding (lookup-key nskk-mode-map [down])))
    (should (eq binding 'nskk-handle-ctrl-n))))

(nskk-deftest-unit keymap-up-arrow-bound-in-mode-map
  "Test that [up] is bound to nskk-handle-ctrl-p in nskk-mode-map."
  (let ((binding (lookup-key nskk-mode-map [up])))
    (should (eq binding 'nskk-handle-ctrl-p))))

(nskk-deftest-unit keymap-handle-ctrl-f-commits-and-moves-forward-when-converting
  "Test handle-ctrl-f commits current candidate then moves forward when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (commit-called nil)
          (forward-called nil))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (cl-letf (((symbol-function 'nskk-commit-current)
                 (lambda () (setq commit-called t)))
                ((symbol-function 'forward-char)
                 (lambda (&rest _) (interactive) (setq forward-called t))))
        (nskk-handle-ctrl-f)
        (should commit-called)
        (should forward-called)))))

(nskk-deftest-unit keymap-handle-ctrl-f-moves-forward-when-not-converting
  "Test handle-ctrl-f calls forward-char when not in conversion mode."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (called nil))
      (cl-letf (((symbol-function 'forward-char)
                 (lambda (&rest _) (interactive) (setq called t))))
        (nskk-handle-ctrl-f)
        (should called)))))

(nskk-deftest-unit keymap-handle-ctrl-f-moves-forward-when-no-state
  "Test handle-ctrl-f calls forward-char when nskk-current-state is nil."
  (with-temp-buffer
    (let ((nskk-current-state nil)
          (called nil))
      (cl-letf (((symbol-function 'forward-char)
                 (lambda (&rest _) (interactive) (setq called t))))
        (nskk-handle-ctrl-f)
        (should called)))))

(nskk-deftest-unit keymap-handle-ctrl-f-moves-forward-when-preedit
  "Test handle-ctrl-f calls forward-char (not nskk-commit-current) in preedit state."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (forward-called nil)
          (commit-called nil))
      (cl-letf (((symbol-function 'nskk-converting-p)
                 (lambda () nil))
                ((symbol-function 'nskk--has-preedit)
                 (lambda () t))
                ((symbol-function 'forward-char)
                 (lambda (&rest _) (interactive) (setq forward-called t)))
                ((symbol-function 'nskk-commit-current)
                 (lambda () (setq commit-called t))))
        (nskk-handle-ctrl-f)
        (should forward-called)
        (should-not commit-called)))))

(nskk-deftest-unit keymap-handle-ctrl-b-commits-and-moves-backward-when-converting
  "Test handle-ctrl-b commits current candidate then moves backward when converting."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (commit-called nil)
          (backward-called nil))
      (nskk--set-conversion-start-marker (point-min))
      (insert "preedit")
      (setf (nskk-state-candidates nskk-current-state) '("result"))
      (setf (nskk-state-current-index nskk-current-state) 0)
      (nskk-state-force-henkan-phase nskk-current-state 'active)
      (cl-letf (((symbol-function 'nskk-commit-current)
                 (lambda () (setq commit-called t)))
                ((symbol-function 'backward-char)
                 (lambda (&rest _) (interactive) (setq backward-called t))))
        (nskk-handle-ctrl-b)
        (should commit-called)
        (should backward-called)))))

(nskk-deftest-unit keymap-handle-ctrl-b-moves-backward-when-not-converting
  "Test handle-ctrl-b calls backward-char when not in conversion mode."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (called nil))
      (cl-letf (((symbol-function 'backward-char)
                 (lambda (&rest _) (interactive) (setq called t))))
        (nskk-handle-ctrl-b)
        (should called)))))

(nskk-deftest-unit keymap-handle-ctrl-b-moves-backward-when-no-state
  "Test handle-ctrl-b calls backward-char when nskk-current-state is nil."
  (with-temp-buffer
    (let ((nskk-current-state nil)
          (called nil))
      (cl-letf (((symbol-function 'backward-char)
                 (lambda (&rest _) (interactive) (setq called t))))
        (nskk-handle-ctrl-b)
        (should called)))))

(nskk-deftest-unit keymap-handle-ctrl-b-moves-backward-when-preedit
  "Test handle-ctrl-b calls backward-char (not nskk-commit-current) in preedit state."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (backward-called nil)
          (commit-called nil))
      (cl-letf (((symbol-function 'nskk-converting-p)
                 (lambda () nil))
                ((symbol-function 'nskk--has-preedit)
                 (lambda () t))
                ((symbol-function 'backward-char)
                 (lambda (&rest _) (interactive) (setq backward-called t)))
                ((symbol-function 'nskk-commit-current)
                 (lambda () (setq commit-called t))))
        (nskk-handle-ctrl-b)
        (should backward-called)
        (should-not commit-called)))))

;;;
;;; nskk--current-key-state Tests
;;;
;; These tests cover the abbrev-mode branch added to nskk--current-key-state:
;; when mode is 'abbrev AND the conversion-start marker is set, the function
;; must return 'preedit (so SPC dispatches 'start-conversion) rather than
;; falling through to 'normal (which would self-insert a space).

(nskk-deftest-unit keymap-key-state-converting
  "nskk--current-key-state returns 'converting when nskk-converting-p is true."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () t))
            ((symbol-function 'nskk--has-preedit) (lambda () nil))
            ((symbol-function 'nskk--get-conversion-start) (lambda () nil)))
    (should (eq (nskk--current-key-state) 'converting))))

(nskk-deftest-unit keymap-key-state-preedit
  "nskk--current-key-state returns 'preedit when nskk--has-preedit is true."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit) (lambda () t))
            ((symbol-function 'nskk--get-conversion-start) (lambda () nil)))
    (should (eq (nskk--current-key-state) 'preedit))))

(nskk-deftest-unit keymap-key-state-normal-in-hiragana-without-preedit
  "nskk--current-key-state returns 'normal in hiragana mode with no preedit."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit) (lambda () nil))
            ((symbol-function 'nskk--get-conversion-start) (lambda () nil)))
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (should (eq (nskk--current-key-state) 'normal)))))

(nskk-deftest-unit keymap-key-state-abbrev-with-marker-is-preedit
  "nskk--current-key-state returns 'preedit in abbrev mode when the conversion
marker is set, even when nskk--has-preedit is false.
This is the core fix: SPC immediately after / must route to 'start-conversion."
  (with-temp-buffer
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
              ((symbol-function 'nskk--has-preedit) (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'abbrev)))
        ;; Simulate the marker being set (as nskk-set-mode-abbrev does).
        (nskk--set-conversion-start-marker (point-min))
        (should (eq (nskk--current-key-state) 'preedit))))))

(nskk-deftest-unit keymap-key-state-abbrev-without-marker-is-normal
  "nskk--current-key-state returns 'normal in abbrev mode when no marker is set.
Without a conversion-start marker there is no preedit context, so SPC should
self-insert a space just as it does in ascii/latin mode."
  (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
            ((symbol-function 'nskk--has-preedit) (lambda () nil))
            ((symbol-function 'nskk--get-conversion-start) (lambda () nil)))
    (let ((nskk-current-state (nskk-state-create 'abbrev)))
      (should (eq (nskk--current-key-state) 'normal)))))

(nskk-deftest-unit keymap-key-state-non-abbrev-mode-with-marker-is-normal
  "nskk--current-key-state returns 'normal for latin mode even with a marker.
The abbrev branch in nskk--current-key-state must check mode eq 'abbrev
explicitly; other direct-insert modes (latin, ascii) must not be affected."
  (with-temp-buffer
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
              ((symbol-function 'nskk--has-preedit) (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'latin)))
        (nskk--set-conversion-start-marker (point-min))
        (should (eq (nskk--current-key-state) 'normal))))))

(nskk-deftest-unit keymap-key-state-abbrev-marker-takes-priority-over-normal
  "nskk--current-key-state: abbrev-with-marker branch fires before 'normal fallthrough.
Regression guard: before the fix, abbrev mode with marker fell through to
'normal because only nskk-converting-p and nskk--has-preedit were checked."
  (with-temp-buffer
    ;; Stub both guards to nil — pre-fix code returns 'normal from here.
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
              ((symbol-function 'nskk--has-preedit) (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'abbrev)))
        (nskk--set-conversion-start-marker (point-min))
        ;; Must be 'preedit, not 'normal.
        (should-not (eq (nskk--current-key-state) 'normal))
        (should (eq (nskk--current-key-state) 'preedit))))))

;;;
;;; nskk-self-insert abbrev-bypass Tests
;;;
;; These unit tests verify that nskk-self-insert routes ALL chars to
;; nskk-process-abbrev-input in abbrev mode, bypassing the Prolog
;; input-route query entirely.

(nskk-deftest-unit keymap-self-insert-abbrev-bypasses-prolog-for-uppercase
  "In abbrev mode, nskk-self-insert calls nskk-process-abbrev-input for uppercase.
Uppercase letters normally trigger okurigana processing via Prolog.
The abbrev short-circuit must fire before the Prolog query."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'abbrev))
          (last-command-event ?T)
          (prolog-called nil))
      (cl-letf (((symbol-function 'nskk-prolog-query-value)
                 (lambda (&rest _) (setq prolog-called t) nil)))
        (nskk-self-insert 1)
        ;; Prolog must not have been consulted.
        (should-not prolog-called)
        ;; The character must have been inserted.
        (should (> (buffer-size) 0))))))

(nskk-deftest-unit keymap-self-insert-abbrev-bypasses-prolog-for-n
  "In abbrev mode, nskk-self-insert calls nskk-process-abbrev-input for 'n'.
'n' normally accumulates in the romaji buffer awaiting a vowel.
The abbrev short-circuit must bypass all romaji-buffer logic."
  (with-temp-buffer
    (let ((nskk-current-state (nskk-state-create 'abbrev))
          (last-command-event ?n)
          (prolog-called nil))
      (cl-letf (((symbol-function 'nskk-prolog-query-value)
                 (lambda (&rest _) (setq prolog-called t) nil)))
        (nskk-self-insert 1)
        (should-not prolog-called)
        (should (> (buffer-size) 0))))))

(provide 'nskk-keymap-test)

;;; nskk-keymap-test.el ends here
