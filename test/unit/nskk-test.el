;;; nskk-test.el --- Tests for nskk.el (main entry point) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
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
(require 'cl-lib)
(require 'nskk)
(require 'nskk-test-framework)
(require 'nskk-test-macros)

(nskk-describe "nskk-mode definition"
  (nskk-it "is defined as a function"
    (should (fboundp 'nskk-mode)))

  (nskk-it "is an interactive command"
    (should (commandp 'nskk-mode)))

  (nskk-it "enables nskk-mode in a buffer"
    (with-temp-buffer
      (nskk-mode 1)
      (should nskk-mode)))

  (nskk-it "disables nskk-mode in a buffer"
    (with-temp-buffer
      (nskk-mode 1)
      (nskk-mode -1)
      (should (not nskk-mode))))

  (nskk-it "toggles nskk-mode on and off"
    (with-temp-buffer
      (should (not nskk-mode))
      (nskk-mode 1)
      (should nskk-mode)
      (nskk-mode 0)
      (should (not nskk-mode)))))

(nskk-describe "nskk-global-mode definition"
  (nskk-it "is defined as a function"
    (should (fboundp 'nskk-global-mode)))

  (nskk-it "is an interactive command"
    (should (commandp 'nskk-global-mode))))

(nskk-describe "nskk-mode-map keymap"
  (nskk-it "is defined as a keymap"
    (should (keymapp nskk-mode-map)))

  (nskk-it "has C-x C-j binding"
    (should (lookup-key nskk-mode-map (kbd "C-x C-j"))))

  (nskk-it "has C-j binding"
    (should (lookup-key nskk-mode-map (kbd "C-j")))))

(nskk-deftest-table main-keymap-bindings
  :columns (key expected-command)
  :rows (("C-x C-j" nskk-toggle-mode)
         ("C-j"     nskk-kakutei)
         ("L"       nskk-handle-upper-l)
         ("/"       nskk-handle-slash)
         ("x"       nskk-handle-x)
         ("C-g"     nskk-handle-cancel))
  :body (should (eq expected-command (lookup-key nskk-mode-map (kbd key)))))

(nskk-deftest-table main-command-existence
  :columns (fn)
  :rows ((nskk-toggle-mode)
         (nskk-kakutei))
  :body (progn
          (should (fboundp fn))
          (should (commandp fn))))

(nskk-describe "buffer-local state"
  (nskk-it "creates state when enabling mode"
    (nskk-with-test-buffer nil
      (should nskk-current-state)
      (should (nskk-state-p nskk-current-state))))

  (nskk-it "initializes state with default mode"
    (nskk-with-test-buffer nil
      (should (eq (nskk-state-mode nskk-current-state) nskk-state-default-mode)))))

(nskk-deftest-table main-internal-functions-exist
  :columns (fn)
  :rows ((nskk--enable)
         (nskk--disable)
         (nskk--turn-on-mode)
         (nskk--setup-buffer)
         (nskk--cleanup-buffer)
         (nskk--post-command-handler))
  :body (should (fboundp fn)))

(nskk-describe "nskk--turn-on-mode"
  (nskk-it "skips minibuffers (function is callable)"
    ;; nskk--turn-on-mode should skip minibuffers
    ;; We just verify the function exists and is callable
    (should (fboundp 'nskk--turn-on-mode))))

(nskk-deftest-table main-kakutei-to-hiragana-transitions
  :description "C-j (kakutei) switches various modes to hiragana"
  :columns (initial-mode)
  :rows ((nil)
         (latin)
         (jisx0208-latin)
         (abbrev))
  :body (nskk-with-test-buffer initial-mode
          (nskk-when  (nskk-kakutei))
          (nskk-then  (nskk-should-mode 'hiragana))))

(nskk-describe "kakutei behavior"
  (nskk-it "inserts a newline when already in hiragana with no preedit"
    (nskk-with-test-buffer 'hiragana
      (electric-indent-local-mode -1)
      (nskk-kakutei)
      (nskk-should-mode 'hiragana)
      (nskk-should-buffer "\n")))

  (nskk-it "switches fullwidth katakana to hiragana with no preedit"
    (nskk-with-test-buffer 'katakana
      (nskk-when  (nskk-kakutei))
      (nskk-then  (nskk-should-mode 'hiragana))
      (nskk-should-buffer "")))

  (nskk-it "switches half-width katakana to hiragana with no preedit"
    (nskk-with-test-buffer nil
      ;; No nskk-set-mode-katakana-半角 exists; use nskk-state-transition directly.
      (nskk-state-transition nskk-current-state (nskk-state-mode nskk-current-state) 'katakana-半角)
      (nskk-given (nskk-should-mode 'katakana-半角))
      (nskk-when  (nskk-kakutei))
      (nskk-then  (nskk-should-mode 'hiragana))
      (nskk-should-buffer "")))

  (nskk-it "clears pending romaji buffer in hiragana and stays in hiragana"
    (nskk-with-test-buffer 'hiragana
      (nskk-given (setq nskk--romaji-buffer "k"))
      (nskk-when  (nskk-kakutei))
      (nskk-then
       (should (string= nskk--romaji-buffer ""))
       (nskk-should-mode 'hiragana))))

  (nskk-it "clears pending romaji buffer in katakana and stays in katakana"
    ;; romaji-pending has higher priority than katakana-idle in the state classifier,
    ;; so C-j with pending romaji does NOT switch mode to hiragana.
    (nskk-with-test-buffer 'katakana
      (nskk-given (setq nskk--romaji-buffer "k"))
      (nskk-when  (nskk-kakutei))
      (nskk-then
       (should (string= nskk--romaji-buffer ""))
       (nskk-should-mode 'katakana))))

  (nskk-it "commits current candidate when in converting state"
    (nskk-with-test-buffer 'hiragana
      (nskk-with-mocks ((nskk-converting-p (lambda () t))
                        (nskk-commit-current (lambda () (insert "確定"))))
        (nskk-kakutei)
        (nskk-should-buffer "確定"))))

  (nskk-it "commits preedit text when in preedit state"
    (nskk-with-test-buffer 'hiragana
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit (lambda () t))
                        (nskk-henkan-kakutei (lambda () (insert "変換"))))
        (nskk-kakutei)
        (nskk-should-buffer "変換")))))

(nskk-describe "nskk-toggle-mode"
  (nskk-it "enables nskk-mode when off"
    (with-temp-buffer
      (nskk-mode -1)
      (nskk-toggle-mode)
      (should nskk-mode)
      (nskk-mode -1)))

  (nskk-it "disables nskk-mode when on"
    (with-temp-buffer
      (nskk-mode 1)
      (nskk-toggle-mode)
      (should (not nskk-mode)))))

(nskk-describe "nskk core module features"
  (nskk-it "nskk provides its feature"
    (should (featurep 'nskk)))

  (nskk-it "nskk-state is loaded"
    (should (featurep 'nskk-state))))

(nskk-describe "nskk--setup-buffer behavior"
  (nskk-it "adds nskk--post-command-handler to buffer-local post-command-hook"
    (with-temp-buffer
      (nskk-given (nskk--setup-buffer))
      (nskk-then
       (should (memq 'nskk--post-command-handler
                     (buffer-local-value 'post-command-hook (current-buffer)))))))

  (nskk-it "is idempotent when called twice"
    (with-temp-buffer
      (nskk--setup-buffer)
      (nskk--setup-buffer)
      (should (= 1 (cl-count 'nskk--post-command-handler
                              (buffer-local-value 'post-command-hook (current-buffer))))))))

(nskk-describe "nskk--cleanup-buffer behavior"
  (nskk-it "removes nskk--post-command-handler from buffer-local post-command-hook"
    (with-temp-buffer
      (nskk-given (nskk--setup-buffer))
      (nskk-when  (nskk--cleanup-buffer))
      (nskk-then
       (should-not (memq 'nskk--post-command-handler
                         (buffer-local-value 'post-command-hook (current-buffer)))))))

  (nskk-it "is safe to call when hook is not set"
    (with-temp-buffer
      (nskk-then (should-not (nskk--cleanup-buffer))))))

(nskk-describe "nskk--enable and nskk--disable behavior"
  (nskk-it "nskk--enable creates nskk-current-state when nil"
    (with-temp-buffer
      (let ((nskk-current-state nil))
        (nskk-with-mocks ((nskk-modeline-update (lambda () nil))
                          (nskk-candidate-show-list (lambda () nil))
                          (nskk-candidate-hide-list (lambda () nil)))
          (nskk-given (nskk--enable))
          (nskk-then (should (nskk-state-p nskk-current-state)))))))

  (nskk-it "nskk--disable sets nskk-current-state to nil"
    (with-temp-buffer
      (nskk-mode 1)
      (nskk-given (nskk--disable))
      (nskk-then (should (null nskk-current-state))))))

(nskk-describe "nskk--post-command-handler behavior"
  (nskk-it "is a no-op when nskk-mode is nil"
    (with-temp-buffer
      (let ((nskk-mode nil)
            (commit-called nil))
        (nskk-with-mocks ((nskk-commit-current
                           (lambda () (setq commit-called t))))
          (nskk--post-command-handler)
          (should-not commit-called)))))

  (nskk-it "calls nskk-modeline-update when nskk-mode is active"
    (with-temp-buffer
      (nskk-mode 1)
      (let ((update-called nil))
        (nskk-with-mocks ((nskk-modeline-update
                           (lambda () (setq update-called t))))
          (nskk--post-command-handler)
          (should update-called)))
      (nskk-mode -1))))

(provide 'nskk-test)

;;; nskk-test.el ends here
