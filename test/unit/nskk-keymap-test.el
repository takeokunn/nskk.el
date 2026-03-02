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
(require 'nskk-test-macros)

;;;
;;; Feature Loading Tests
;;;

(nskk-describe "nskk-keymap feature loading"
  (nskk-it "provides the nskk-keymap feature"
    (should (featurep 'nskk-keymap)))

  (nskk-it "requiring nskk-keymap again is safe (idempotent)"
    (should (require 'nskk-keymap)))

  (nskk-it "loads nskk-input as a dependency"
    (should (featurep 'nskk-input))))

;;;
;;; nskk-mode-map Structure Tests
;;;

(nskk-describe "nskk-mode-map structure"
  (nskk-it "is a valid keymap"
    (should (keymapp nskk-mode-map)))

  (nskk-it "is a sparse keymap (car is keymap symbol)"
    (should (eq (car nskk-mode-map) 'keymap)))

  (nskk-context "global key bindings"
    (nskk-it "C-x C-j is bound to an interactive command"
      (let ((binding (lookup-key nskk-mode-map (kbd "C-x C-j"))))
        (should binding)
        (should (commandp binding))))

    (nskk-it "C-j is bound to an interactive command"
      (let ((binding (lookup-key nskk-mode-map (kbd "C-j"))))
        (should binding)
        (should (commandp binding))))))

;;;
;;; Input Commands API Availability Tests
;;;

(nskk-describe "input commands API availability"
  (nskk-deftest-table keymap-api-commands-defined
    :description "Input command function is defined (fboundp)"
    :columns (fn)
    :rows ((nskk-toggle-japanese-mode)
           (nskk-commit-current)
           (nskk-cancel-conversion)
           (nskk-convert-or-commit)
           (nskk-set-mode-hiragana)
           (nskk-set-mode-katakana)
           (nskk-set-mode-latin)
           (nskk-set-mode-abbrev)
           (nskk-set-mode-jisx0208-latin))
    :body (should (fboundp fn))))

;;;
;;; Interactive Command Tests
;;;

(nskk-describe "interactive command availability"
  (nskk-deftest-table keymap-commands-interactive
    :description "Command is interactive (commandp)"
    :columns (cmd)
    :rows ((nskk-toggle-japanese-mode)
           (nskk-commit-current)
           (nskk-cancel-conversion)
           (nskk-convert-or-commit)
           (nskk-set-mode-hiragana)
           (nskk-set-mode-katakana)
           (nskk-set-mode-latin)
           (nskk-set-mode-abbrev)
           (nskk-set-mode-jisx0208-latin))
    :body (should (commandp cmd))))

;;;
;;; Behavioral Tests for Input Commands via Keymap
;;;

(nskk-describe "mode switching via input commands API"
  (nskk-it "switches through hiragana, katakana, and latin"
    (let ((nskk-current-state (nskk-state-create 'ascii)))
      (nskk-given (nskk-set-mode-hiragana))
      (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
      (nskk-given (nskk-set-mode-katakana))
      (should (eq (nskk-state-mode nskk-current-state) 'katakana))
      (nskk-given (nskk-set-mode-latin))
      (nskk-then  (should (eq (nskk-state-mode nskk-current-state) 'latin))))))

(nskk-describe "nskk-toggle-japanese-mode behavior"
  (nskk-it "toggles hiragana to katakana and back"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-when  (nskk-toggle-japanese-mode))
      (should (eq (nskk-state-mode nskk-current-state) 'katakana))
      (nskk-when  (nskk-toggle-japanese-mode))
      (nskk-then  (should (eq (nskk-state-mode nskk-current-state) 'hiragana))))))

;;;
;;; nskk-handle-q behavior
;;;

(nskk-describe "nskk-handle-q behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-q))
    (should (commandp 'nskk-handle-q)))

  (nskk-it "toggles to katakana when in hiragana"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-when (nskk-handle-q))
      (nskk-then (should (eq (nskk-state-mode nskk-current-state) 'katakana)))))

  (nskk-it "self-inserts 'q' when in ascii mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii))
            (last-command-event ?q))
        (nskk-when (nskk-handle-q))
        (nskk-then (should (equal (buffer-string) "q"))))))

  (nskk-it "self-inserts 'q' when state is nil"
    (with-temp-buffer
      (let ((nskk-current-state nil)
            (last-command-event ?q))
        (nskk-when (nskk-handle-q))
        (nskk-then (should (equal (buffer-string) "q"))))))

  (nskk-it "does implicit kakutei then toggles when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "preedit")
                      (nskk-state-set-candidates nskk-current-state '("result"))
                      (nskk-state-force-henkan-phase nskk-current-state 'active)))
        (nskk-when (nskk-handle-q))
        (nskk-then (should-not (nskk-converting-p))))))

  (nskk-context "abbrev mode regression"
    (nskk-it "self-inserts 'q' in abbrev mode even with active preedit"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'abbrev))
              (last-command-event ?q))
          (nskk--set-conversion-start-marker (point-min))
          (insert "\u25BDemai")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-when (nskk-handle-q))
          (nskk-then
           (should (eq (nskk-state-mode nskk-current-state) 'abbrev))
           (should (string-suffix-p "q" (buffer-string)))))))))

;;;
;;; nskk-handle-l behavior
;;;

(nskk-describe "nskk-handle-l behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-l))
    (should (commandp 'nskk-handle-l)))

  (nskk-it "enters latin mode when in hiragana"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-when (nskk-handle-l))
      (nskk-then (should (eq (nskk-state-mode nskk-current-state) 'latin)))))

  (nskk-it "self-inserts 'l' when in ascii mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii))
            (last-command-event ?l))
        (nskk-when (nskk-handle-l))
        (nskk-then (should (equal (buffer-string) "l"))))))

  (nskk-it "does implicit kakutei then enters latin when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "preedit")
                      (nskk-state-set-candidates nskk-current-state '("result"))
                      (nskk-state-force-henkan-phase nskk-current-state 'active)))
        (nskk-when (nskk-handle-l))
        (nskk-then
         (should-not (nskk-converting-p))
         (should (eq (nskk-state-mode nskk-current-state) 'latin))))))

  (nskk-context "abbrev mode regression"
    (nskk-it "self-inserts 'l' in abbrev mode even with active preedit"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'abbrev))
              (last-command-event ?l))
          (nskk--set-conversion-start-marker (point-min))
          (insert "\u25BDemai")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-when (nskk-handle-l))
          (nskk-then
           (should (eq (nskk-state-mode nskk-current-state) 'abbrev))
           (should (string-suffix-p "l" (buffer-string)))))))))

;;;
;;; nskk-handle-upper-l behavior
;;;

(nskk-describe "nskk-handle-upper-l behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-upper-l))
    (should (commandp 'nskk-handle-upper-l)))

  (nskk-it "enters jisx0208-latin mode when in hiragana"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-when (nskk-handle-upper-l))
      (nskk-then (should (eq (nskk-state-mode nskk-current-state) 'jisx0208-latin)))))

  (nskk-it "self-inserts 'L' when in ascii mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii))
            (last-command-event ?L))
        (nskk-when (nskk-handle-upper-l))
        (nskk-then (should (equal (buffer-string) "L"))))))

  (nskk-it "does implicit kakutei then switches mode when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "preedit")
                      (nskk-state-set-candidates nskk-current-state '("result"))
                      (nskk-state-force-henkan-phase nskk-current-state 'active)))
        (nskk-when (nskk-handle-upper-l))
        (nskk-then
         (should-not (nskk-converting-p))
         (should (eq (nskk-state-mode nskk-current-state) 'jisx0208-latin))))))

  (nskk-context "abbrev mode regression"
    (nskk-it "self-inserts 'L' in abbrev mode even with active preedit"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'abbrev))
              (last-command-event ?L))
          (nskk--set-conversion-start-marker (point-min))
          (insert "\u25BDemai")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-when (nskk-handle-upper-l))
          (nskk-then
           (should (eq (nskk-state-mode nskk-current-state) 'abbrev))
           (should (string-suffix-p "L" (buffer-string)))))))))

;;;
;;; nskk-handle-slash behavior
;;;

(nskk-describe "nskk-handle-slash behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-slash))
    (should (commandp 'nskk-handle-slash)))

  (nskk-it "enters abbrev mode when in hiragana"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-when (nskk-handle-slash))
      (nskk-then (should (eq (nskk-state-mode nskk-current-state) 'abbrev)))))

  (nskk-it "self-inserts '/' when in ascii mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii))
            (last-command-event ?/))
        (nskk-when (nskk-handle-slash))
        (nskk-then (should (equal (buffer-string) "/"))))))

  (nskk-context "abbrev mode regression"
    (nskk-it "self-inserts '/' in abbrev mode even with active preedit"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'abbrev))
              (last-command-event ?/))
          (nskk--set-conversion-start-marker (point-min))
          (insert "\u25BDhttp:")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-when (nskk-handle-slash))
          (nskk-then
           (should (eq (nskk-state-mode nskk-current-state) 'abbrev))
           (should (string-suffix-p "/" (buffer-string)))))))))

;;;
;;; nskk-handle-x behavior
;;;

(nskk-describe "nskk-handle-x behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-x))
    (should (commandp 'nskk-handle-x)))

  (nskk-it "accumulates 'x' in romaji buffer when not converting"
    (nskk-with-test-buffer 'hiragana
      (let ((last-command-event ?x))
        (nskk-when (nskk-handle-x))
        (nskk-then
         (should (equal nskk--romaji-buffer "x"))
         (should (equal (buffer-string) ""))))))

  (nskk-it "calls nskk-previous-candidate when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (prev-candidate-called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (cl-letf (((symbol-function 'nskk-previous-candidate)
                   (lambda () (setq prev-candidate-called t))))
          (nskk-when (nskk-handle-x))
          (nskk-then (should prev-candidate-called)))))))

;;;
;;; nskk-handle-space behavior
;;;

(nskk-describe "nskk-handle-space behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-space))
    (should (commandp 'nskk-handle-space)))

  (nskk-it "inserts a space when no preedit"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (last-command-event ? ))
        (nskk-when (nskk-handle-space))
        (nskk-then (should (equal (buffer-string) " "))))))

  (nskk-it "starts conversion when preedit exists"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk--set-conversion-start-marker (point-min))
        (insert "\u25BDtest")
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (cl-letf (((symbol-function 'nskk-core-search)
                   (lambda (_k &optional _t _l)
                     '("result"))))
          (nskk-when (nskk-handle-space))
          (nskk-then (should (nskk-converting-p)))
          (when (overlayp nskk--conversion-overlay)
            (delete-overlay nskk--conversion-overlay))))))

  (nskk-it "calls nskk-next-candidate when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (next-candidate-called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (cl-letf (((symbol-function 'nskk-next-candidate)
                   (lambda () (setq next-candidate-called t))))
          (nskk-when (nskk-handle-space))
          (nskk-then (should next-candidate-called)))))))

;;;
;;; nskk-handle-return behavior
;;;

(nskk-describe "nskk-handle-return behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-return))
    (should (commandp 'nskk-handle-return)))

  (nskk-it "inserts newline when not converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-when (nskk-handle-return))
        (nskk-then (should (equal (buffer-string) "\n"))))))

  (nskk-it "commits without newline when in conversion"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "preedit")
                      (nskk-state-set-candidates nskk-current-state '("result"))
                      (nskk-state-force-henkan-phase nskk-current-state 'active)))
        (nskk-when (nskk-handle-return))
        (nskk-then
         (should-not (nskk-converting-p))
         (should (equal (buffer-string) "result")))))))

;;;
;;; nskk-handle-cancel behavior
;;;

(nskk-describe "nskk-handle-cancel behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-cancel))
    (should (commandp 'nskk-handle-cancel)))

  (nskk-it "calls keyboard-quit when not converting"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (nskk--conversion-start-marker nil)
          (quit-called nil))
      (cl-letf (((symbol-function 'keyboard-quit)
                 (lambda () (setq quit-called t))))
        (nskk-when (nskk-handle-cancel))
        (nskk-then (should quit-called)))))

  (nskk-it "calls nskk-cancel-conversion-to-reading when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (cancel-called nil))
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "preedit")
                      (nskk-state-set-candidates nskk-current-state '("result"))
                      (nskk-state-force-henkan-phase nskk-current-state 'active)))
        (cl-letf (((symbol-function 'nskk-cancel-conversion-to-reading)
                   (lambda () (setq cancel-called t))))
          (nskk-when (nskk-handle-cancel))
          (nskk-then (should cancel-called)))))))

;;;
;;; nskk--current-kakutei-state Tests
;;;

(nskk-describe "nskk--current-kakutei-state behavior"
  (nskk-it "returns 'converting when nskk-converting-p is true"
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () t))
              ((symbol-function 'nskk--has-preedit) (lambda () nil)))
      (should (eq (nskk--current-kakutei-state) 'converting))))

  (nskk-it "returns 'preedit when nskk--has-preedit is true"
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
              ((symbol-function 'nskk--has-preedit) (lambda () t)))
      (should (eq (nskk--current-kakutei-state) 'preedit))))

  (nskk-it "returns 'romaji-pending when nskk--romaji-buffer is non-empty"
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
              ((symbol-function 'nskk--has-preedit) (lambda () nil)))
      (let ((nskk--romaji-buffer "k"))
        (should (eq (nskk--current-kakutei-state) 'romaji-pending)))))

  (nskk-it "returns 'hiragana-idle in hiragana mode with no pending input"
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
              ((symbol-function 'nskk--has-preedit) (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer ""))
        (should (eq (nskk--current-kakutei-state) 'hiragana-idle)))))

  (nskk-it "returns 'katakana-idle in fullwidth katakana mode"
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
              ((symbol-function 'nskk--has-preedit) (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'katakana))
            (nskk--romaji-buffer ""))
        (should (eq (nskk--current-kakutei-state) 'katakana-idle)))))

  (nskk-it "returns 'katakana-idle in half-width katakana mode"
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
              ((symbol-function 'nskk--has-preedit) (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'katakana-半角))
            (nskk--romaji-buffer ""))
        (should (eq (nskk--current-kakutei-state) 'katakana-idle)))))

  (nskk-it "returns 'direct-idle in ascii mode with no pending input"
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
              ((symbol-function 'nskk--has-preedit) (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'ascii))
            (nskk--romaji-buffer ""))
        (should (eq (nskk--current-kakutei-state) 'direct-idle))))))

;;;
;;; C-n and C-p Handler Tests
;;;

(nskk-describe "nskk-handle-ctrl-n behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-ctrl-n))
    (should (commandp 'nskk-handle-ctrl-n)))

  (nskk-it "C-n is bound in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map (kbd "C-n"))))
      (should (eq binding 'nskk-handle-ctrl-n))))

  (nskk-it "[down] is bound to nskk-handle-ctrl-n in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map [down])))
      (should (eq binding 'nskk-handle-ctrl-n))))

  (nskk-it "calls next-line when not in conversion mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (called nil))
        (cl-letf (((symbol-function 'next-line)
                   (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-n))
          (nskk-then (should called))))))

  (nskk-it "calls next-line when nskk-current-state is nil"
    (with-temp-buffer
      (let ((nskk-current-state nil)
            (called nil))
        (cl-letf (((symbol-function 'next-line)
                   (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-n))
          (nskk-then (should called)))))))

(nskk-describe "nskk-handle-ctrl-p behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-ctrl-p))
    (should (commandp 'nskk-handle-ctrl-p)))

  (nskk-it "C-p is bound in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map (kbd "C-p"))))
      (should (eq binding 'nskk-handle-ctrl-p))))

  (nskk-it "[up] is bound to nskk-handle-ctrl-p in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map [up])))
      (should (eq binding 'nskk-handle-ctrl-p))))

  (nskk-it "calls nskk-previous-candidate when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (cl-letf (((symbol-function 'nskk-previous-candidate)
                   (lambda () (setq called t))))
          (nskk-when (nskk-handle-ctrl-p))
          (nskk-then (should called))))))

  (nskk-it "calls previous-line when not in conversion mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (called nil))
        (cl-letf (((symbol-function 'previous-line)
                   (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-p))
          (nskk-then (should called))))))

  (nskk-it "calls previous-line when nskk-current-state is nil"
    (with-temp-buffer
      (let ((nskk-current-state nil)
            (called nil))
        (cl-letf (((symbol-function 'previous-line)
                   (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-p))
          (nskk-then (should called)))))))

;;;
;;; Helper Function for Cursor Key Tests
;;;

(defun nskk-test-setup-converting (preedit candidate)
  "Setup converting mode for PREEDIT text with CANDIDATE.
PREEDIT should already be in buffer starting at point.
Sets conversion-start-marker at point, advances past PREEDIT, and configures state."
  (nskk--set-conversion-start-marker (point))
  (forward-char (length preedit))
  (nskk-state-set-candidates nskk-current-state (list candidate))
  (nskk-state-force-henkan-phase nskk-current-state 'active))

;;;
;;; Cursor Key Behavior Changes - Commit Then Move (▼ converting mode)
;;;

(nskk-describe "cursor key commit-then-move behavior"
  (nskk-it "C-n commits candidate and moves to next line in converting mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer ""))
        (insert "あいうえお\nかきくけこ")
        (goto-char (point-min))
        (nskk-test-setup-converting "あい" "愛")
        (nskk-when (nskk-handle-ctrl-n))
        (nskk-then
         (should (string= (buffer-string) "愛うえお\nかきくけこ"))
         (should (= (line-number-at-pos) 2))))))

  (nskk-it "C-p commits candidate and moves to previous line in converting mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer ""))
        (insert "あいうえお\nかきくけこ")
        (goto-char (point-min))
        (forward-line 1)
        (nskk-test-setup-converting "か" "書")
        (nskk-when (nskk-handle-ctrl-p))
        (nskk-then
         (should (string= (buffer-string) "あいうえお\n書きくけこ"))
         (should (= (line-number-at-pos) 1))))))

  (nskk-it "[down] commits candidate and moves to next line in converting mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer ""))
        (insert "あいうえお\nかきくけこ")
        (goto-char (point-min))
        (nskk-test-setup-converting "あい" "愛")
        (nskk-when (nskk-handle-ctrl-n))  ;; Use handler directly (arrow keys map to same handler)
        (nskk-then
         (should (string= (buffer-string) "愛うえお\nかきくけこ"))
         (should (= (line-number-at-pos) 2))))))

  (nskk-it "[up] commits candidate and moves to previous line in converting mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer ""))
        (insert "あいうえお\nかきくけこ")
        (goto-char (point-min))
        (forward-line 1)
        (nskk-test-setup-converting "か" "書")
        (nskk-when (nskk-handle-ctrl-p))  ;; Use handler directly (arrow keys map to same handler)
        (nskk-then
         (should (string= (buffer-string) "あいうえお\n書きくけこ"))
         (should (= (line-number-at-pos) 1))))))

  (nskk-it "C-n at buffer end during conversion silently ignores end-of-buffer"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer ""))
        (insert "あい")
        (goto-char (point-min))
        (nskk-test-setup-converting "あい" "愛")
        (nskk-when (nskk-handle-ctrl-n))
        (nskk-then
         (should (string= (buffer-string) "愛"))
         (should (= (point) (point-max)))))))

  (nskk-it "C-p at buffer beginning during conversion silently ignores beginning-of-buffer"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer ""))
        (insert "あい")
        (goto-char (point-min))
        (nskk-test-setup-converting "あい" "愛")
        (nskk-when (nskk-handle-ctrl-p))
        (nskk-then
         (should (string= (buffer-string) "愛"))
         (should (= (point) (point-min))))))))

;;;
;;; C-f and C-b Handler Tests
;;;

(nskk-describe "nskk-handle-ctrl-f behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-ctrl-f))
    (should (commandp 'nskk-handle-ctrl-f)))

  (nskk-it "C-f is bound in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map (kbd "C-f"))))
      (should (eq binding 'nskk-handle-ctrl-f))))

  (nskk-it "[right] is bound to nskk-handle-ctrl-f in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map [right])))
      (should (eq binding 'nskk-handle-ctrl-f))))

  (nskk-it "commits then moves forward when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (commit-called nil)
            (forward-called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (cl-letf (((symbol-function 'nskk-commit-current)
                   (lambda () (setq commit-called t)))
                  ((symbol-function 'forward-char)
                   (lambda (&rest _) (interactive) (setq forward-called t))))
          (nskk-when (nskk-handle-ctrl-f))
          (nskk-then
           (should commit-called)
           (should forward-called))))))

  (nskk-it "calls forward-char when not converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (called nil))
        (cl-letf (((symbol-function 'forward-char)
                   (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-f))
          (nskk-then (should called))))))

  (nskk-it "calls forward-char when nskk-current-state is nil"
    (with-temp-buffer
      (let ((nskk-current-state nil)
            (called nil))
        (cl-letf (((symbol-function 'forward-char)
                   (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-f))
          (nskk-then (should called))))))

  (nskk-it "calls forward-char (not commit) in preedit state"
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
          (nskk-when (nskk-handle-ctrl-f))
          (nskk-then
           (should forward-called)
           (should-not commit-called)))))))

(nskk-describe "nskk-handle-ctrl-b behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-ctrl-b))
    (should (commandp 'nskk-handle-ctrl-b)))

  (nskk-it "C-b is bound in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map (kbd "C-b"))))
      (should (eq binding 'nskk-handle-ctrl-b))))

  (nskk-it "[left] is bound to nskk-handle-ctrl-b in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map [left])))
      (should (eq binding 'nskk-handle-ctrl-b))))

  (nskk-it "commits then moves backward when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (commit-called nil)
            (backward-called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (cl-letf (((symbol-function 'nskk-commit-current)
                   (lambda () (setq commit-called t)))
                  ((symbol-function 'backward-char)
                   (lambda (&rest _) (interactive) (setq backward-called t))))
          (nskk-when (nskk-handle-ctrl-b))
          (nskk-then
           (should commit-called)
           (should backward-called))))))

  (nskk-it "calls backward-char when not converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (called nil))
        (cl-letf (((symbol-function 'backward-char)
                   (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-b))
          (nskk-then (should called))))))

  (nskk-it "calls backward-char when nskk-current-state is nil"
    (with-temp-buffer
      (let ((nskk-current-state nil)
            (called nil))
        (cl-letf (((symbol-function 'backward-char)
                   (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-b))
          (nskk-then (should called))))))

  (nskk-it "calls backward-char (not commit) in preedit state"
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
          (nskk-when (nskk-handle-ctrl-b))
          (nskk-then
           (should backward-called)
           (should-not commit-called)))))))

;;;
;;; nskk--current-key-state Tests
;;;
;; These tests cover the abbrev-mode branch added to nskk--current-key-state:
;; when mode is 'abbrev AND the conversion-start marker is set, the function
;; must return 'preedit (so SPC dispatches 'start-conversion) rather than
;; falling through to 'normal (which would self-insert a space).

(nskk-describe "nskk--current-key-state behavior"
  (nskk-it "returns 'converting when nskk-converting-p is true"
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () t))
              ((symbol-function 'nskk--has-preedit) (lambda () nil))
              ((symbol-function 'nskk--get-conversion-start) (lambda () nil)))
      (should (eq (nskk--current-key-state) 'converting))))

  (nskk-it "returns 'preedit when nskk--has-preedit is true"
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
              ((symbol-function 'nskk--has-preedit) (lambda () t))
              ((symbol-function 'nskk--get-conversion-start) (lambda () nil)))
      (should (eq (nskk--current-key-state) 'preedit))))

  (nskk-it "returns 'normal in hiragana mode with no preedit"
    (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
              ((symbol-function 'nskk--has-preedit) (lambda () nil))
              ((symbol-function 'nskk--get-conversion-start) (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (should (eq (nskk--current-key-state) 'normal)))))

  (nskk-context "abbrev mode + marker"
    (nskk-it "returns 'preedit in abbrev mode when conversion marker is set"
      (with-temp-buffer
        (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
                  ((symbol-function 'nskk--has-preedit) (lambda () nil)))
          (let ((nskk-current-state (nskk-state-create 'abbrev)))
            (nskk--set-conversion-start-marker (point-min))
            (should (eq (nskk--current-key-state) 'preedit))))))

    (nskk-it "returns 'normal in abbrev mode when no marker is set"
      (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
                ((symbol-function 'nskk--has-preedit) (lambda () nil))
                ((symbol-function 'nskk--get-conversion-start) (lambda () nil)))
        (let ((nskk-current-state (nskk-state-create 'abbrev)))
          (should (eq (nskk--current-key-state) 'normal)))))

    (nskk-it "returns 'normal for latin mode even with a marker (abbrev-only branch)"
      (with-temp-buffer
        (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
                  ((symbol-function 'nskk--has-preedit) (lambda () nil)))
          (let ((nskk-current-state (nskk-state-create 'latin)))
            (nskk--set-conversion-start-marker (point-min))
            (should (eq (nskk--current-key-state) 'normal))))))

    (nskk-it "abbrev-with-marker branch fires before 'normal fallthrough (regression)"
      (with-temp-buffer
        (cl-letf (((symbol-function 'nskk-converting-p) (lambda () nil))
                  ((symbol-function 'nskk--has-preedit) (lambda () nil)))
          (let ((nskk-current-state (nskk-state-create 'abbrev)))
            (nskk--set-conversion-start-marker (point-min))
            (should-not (eq (nskk--current-key-state) 'normal))
            (should (eq (nskk--current-key-state) 'preedit))))))))

;;;
;;; nskk-self-insert abbrev-bypass Tests
;;;
;; These unit tests verify that nskk-self-insert routes ALL chars to
;; nskk-process-abbrev-input in abbrev mode, bypassing the Prolog
;; input-route query entirely.

(nskk-describe "nskk-self-insert abbrev-mode Prolog bypass"
  (nskk-it "bypasses Prolog for uppercase letters in abbrev mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'abbrev))
            (last-command-event ?T)
            (prolog-called nil))
        (cl-letf (((symbol-function 'nskk-prolog-query-value)
                   (lambda (&rest _) (setq prolog-called t) nil)))
          (nskk-when (nskk-self-insert 1))
          (nskk-then
           (should-not prolog-called)
           (should (> (buffer-size) 0)))))))

  (nskk-it "bypasses Prolog for 'n' in abbrev mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'abbrev))
            (last-command-event ?n)
            (prolog-called nil))
        (cl-letf (((symbol-function 'nskk-prolog-query-value)
                   (lambda (&rest _) (setq prolog-called t) nil)))
          (nskk-when (nskk-self-insert 1))
          (nskk-then
           (should-not prolog-called)
           (should (> (buffer-size) 0))))))))

;;;
;;; nskk-handle-ctrl-a behavior
;;;

(nskk-describe "nskk-handle-ctrl-a behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-ctrl-a))
    (should (commandp 'nskk-handle-ctrl-a)))

  (nskk-it "C-a is bound in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map (kbd "C-a"))))
      (should (eq binding 'nskk-handle-ctrl-a))))

  (nskk-it "[home] is bound to nskk-handle-ctrl-a in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map [home])))
      (should (eq binding 'nskk-handle-ctrl-a))))

  (nskk-it "commits then moves to beginning-of-line when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (commit-called nil)
            (bol-called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (cl-letf (((symbol-function 'nskk-commit-current)
                   (lambda () (setq commit-called t)))
                  ((symbol-function 'beginning-of-line)
                   (lambda (&rest _) (interactive) (setq bol-called t))))
          (nskk-when (nskk-handle-ctrl-a))
          (nskk-then
           (should commit-called)
           (should bol-called))))))

  (nskk-it "calls beginning-of-line when not converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (called nil))
        (cl-letf (((symbol-function 'beginning-of-line)
                   (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-a))
          (nskk-then (should called))))))

  (nskk-it "calls beginning-of-line when nskk-current-state is nil"
    (with-temp-buffer
      (let ((nskk-current-state nil)
            (called nil))
        (cl-letf (((symbol-function 'beginning-of-line)
                   (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-a))
          (nskk-then (should called))))))

  (nskk-it "calls beginning-of-line in preedit state without committing"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (commit-called nil)
            (bol-called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "▽ka")
        (cl-letf (((symbol-function 'nskk-commit-current)
                   (lambda () (setq commit-called t)))
                  ((symbol-function 'beginning-of-line)
                   (lambda (&rest _) (interactive) (setq bol-called t))))
          (nskk-when (nskk-handle-ctrl-a))
          (nskk-then
           (should-not commit-called)
           (should bol-called)))))))

;;;
;;; nskk-handle-ctrl-e behavior
;;;

(nskk-describe "nskk-handle-ctrl-e behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-ctrl-e))
    (should (commandp 'nskk-handle-ctrl-e)))

  (nskk-it "C-e is bound in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map (kbd "C-e"))))
      (should (eq binding 'nskk-handle-ctrl-e))))

  (nskk-it "[end] is bound to nskk-handle-ctrl-e in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map [end])))
      (should (eq binding 'nskk-handle-ctrl-e))))

  (nskk-it "commits then moves to end-of-line when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (commit-called nil)
            (eol-called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (cl-letf (((symbol-function 'nskk-commit-current)
                   (lambda () (setq commit-called t)))
                  ((symbol-function 'end-of-line)
                   (lambda (&rest _) (interactive) (setq eol-called t))))
          (nskk-when (nskk-handle-ctrl-e))
          (nskk-then
           (should commit-called)
           (should eol-called))))))

  (nskk-it "calls end-of-line when not converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (called nil))
        (cl-letf (((symbol-function 'end-of-line)
                   (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-e))
          (nskk-then (should called))))))

  (nskk-it "calls end-of-line when nskk-current-state is nil"
    (with-temp-buffer
      (let ((nskk-current-state nil)
            (called nil))
        (cl-letf (((symbol-function 'end-of-line)
                   (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-e))
          (nskk-then (should called))))))

  (nskk-it "calls end-of-line in preedit state without committing"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (commit-called nil)
            (eol-called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "▽ka")
        (cl-letf (((symbol-function 'nskk-commit-current)
                   (lambda () (setq commit-called t)))
                  ((symbol-function 'end-of-line)
                   (lambda (&rest _) (interactive) (setq eol-called t))))
          (nskk-when (nskk-handle-ctrl-e))
          (nskk-then
           (should-not commit-called)
           (should eol-called)))))))

;;;
;;; nskk-handle-backspace behavior
;;;

(nskk-describe "nskk-handle-backspace behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-backspace))
    (should (commandp 'nskk-handle-backspace)))

  (nskk-it "DEL is bound in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map (kbd "DEL"))))
      (should (eq binding 'nskk-handle-backspace))))

  (nskk-it "deletes last character when preedit has content"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk--set-conversion-start-marker (point-min))
        (insert "▽ka")
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-when (nskk-handle-backspace))
        (nskk-then (should-not (string-suffix-p "a" (buffer-string)))))))

  (nskk-it "calls nskk-cancel-preedit when preedit is empty"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (cancel-preedit-called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "▽")
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (cl-letf (((symbol-function 'nskk-cancel-preedit)
                   (lambda () (setq cancel-preedit-called t))))
          (nskk-when (nskk-handle-backspace))
          (nskk-then (should cancel-preedit-called))))))

  (nskk-it "calls nskk-cancel-conversion-to-reading when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (cancel-called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (cl-letf (((symbol-function 'nskk-cancel-conversion-to-reading)
                   (lambda () (setq cancel-called t))))
          (nskk-when (nskk-handle-backspace))
          (nskk-then (should cancel-called))))))

  (nskk-it "deletes backward char when no preedit (normal state)"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii)))
        (insert "abc")
        (nskk-when (nskk-handle-backspace))
        (nskk-then (should (equal (buffer-string) "ab")))))))

(provide 'nskk-keymap-test)

;;; nskk-keymap-test.el ends here
