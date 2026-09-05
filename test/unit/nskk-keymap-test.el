;;; nskk-keymap-test.el --- Keymap tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 NSKK Authors

;; Author: takeokunn <bararararatty@gmail.com>
;; Keywords: japanese, input, test

;; This file is part of NSKK.

;;; Commentary:

;; Keymap tests.

;;; Code:

(require 'ert)
(require 'nskk-keymap)
(require 'nskk-henkan)
(require 'nskk-state)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk)  ; needed for nskk-mode-map

;;;
;;; State Accessor Test Helper
;;;

(defmacro nskk-test-with-romaji-buffer (value &rest body)
  "Execute BODY with the current buffer's romaji buffer set to VALUE.
Restores the prior value on exit via `unwind-protect', since the romaji
buffer is accessor-only buffer-local state owned by `nskk-state.el'."
  (declare (indent 1))
  (let ((saved (make-symbol "saved")))
    `(let ((,saved (nskk-state-romaji-buffer)))
       (unwind-protect
           (progn
             (nskk-state-set-romaji-buffer ,value)
             ,@body)
         (nskk-state-set-romaji-buffer ,saved)))))

;;;
;;; Nav Handler Test Helpers
;;;

(defmacro nskk-deftest-nav-handler (_key handler nav-fn)
  "Generate standard tests for a commit-then-navigate key handler.
KEY is a symbol like `ctrl-f'.  HANDLER is the command symbol.
NAV-FN is the fallthrough navigation command symbol (e.g. `forward-char')."
  (declare (indent 3))
  `(progn
     (nskk-it ,(format "commits then %s when converting" nav-fn)
       (let ((commit-called nil)
             (nav-called nil))
         (nskk-with-mocks ((nskk-converting-p (lambda () t))
                           (nskk-has-preedit (lambda () nil))
                           (nskk-commit-current (lambda () (setq commit-called t)))
                           (,nav-fn (lambda (&rest _) (interactive) (setq nav-called t))))
           (call-interactively ',handler))
         (should commit-called)
         (should nav-called)))

     (nskk-it ,(format "calls %s when not converting (normal state)" nav-fn)
       (let ((nav-called nil))
         (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                           (nskk-has-preedit (lambda () nil))
                           (,nav-fn (lambda (&rest _) (interactive) (setq nav-called t))))
           (let ((nskk-current-state (nskk-state-create)))
             (call-interactively ',handler)))
         (should nav-called)))

     (nskk-it ,(format "calls %s when nskk-current-state is nil" nav-fn)
       (let ((nav-called nil))
         (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                           (nskk-has-preedit (lambda () nil))
                           (,nav-fn (lambda (&rest _) (interactive) (setq nav-called t))))
           (let ((nskk-current-state nil))
             (call-interactively ',handler)))
         (should nav-called)))

     (nskk-it ,(format "calls nskk-henkan-kakutei (not nskk-commit-current) then %s in preedit state" nav-fn)
       (let ((commit-called nil)
             (kakutei-called nil)
             (nav-called nil))
         (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                           (nskk-has-preedit (lambda () t))
                           (nskk-get-conversion-start (lambda () 1))
                           (nskk-commit-current (lambda () (setq commit-called t)))
                           (nskk-henkan-kakutei (lambda () (setq kakutei-called t)))
                           (,nav-fn (lambda (&rest _) (interactive) (setq nav-called t))))
           (let* ((preedit-state (nskk-state-create 'hiragana))
                  (_ (nskk-state-force-henkan-phase preedit-state 'on))
                  (nskk-current-state preedit-state))
             (call-interactively ',handler)))
         (should-not commit-called)
         (should kakutei-called)
         (should nav-called)))))

;;;
;;; Key Handler Command Existence Tests
;;;

(nskk-describe "nskk-handle-* commands: defined and interactive"
  (nskk-deftest-table keymap-handle-commands-exist
    :description "Handler command is defined (fboundp) and interactive (commandp)"
    :columns (cmd)
    :rows ((nskk-handle-q)
           (nskk-handle-l)
           (nskk-handle-upper-l)
           (nskk-handle-slash)
           (nskk-handle-x)
           (nskk-handle-space)
           (nskk-handle-return)
           (nskk-handle-cancel)
           (nskk-handle-ctrl-n)
           (nskk-handle-ctrl-p)
           (nskk-handle-ctrl-f)
           (nskk-handle-ctrl-b)
           (nskk-handle-ctrl-a)
           (nskk-handle-ctrl-e)
           (nskk-handle-backspace)
           (nskk-handle-tab)
           (nskk-handle-hash))
    :body (progn (should (fboundp cmd))
                 (should (commandp cmd)))))

;;;
;;; Behavioral Tests for Input Commands via Keymap
;;;

(nskk-describe "mode switching via input commands API"
  (nskk-it "switches through hiragana, katakana, and latin"
    (let ((nskk-current-state (nskk-state-create 'ascii)))
      (nskk-set-mode-hiragana)
      (should (eq (nskk-state-mode nskk-current-state) 'hiragana))
      (nskk-set-mode-katakana)
      (should (eq (nskk-state-mode nskk-current-state) 'katakana))
      (nskk-set-mode-latin)
      (should (eq (nskk-state-mode nskk-current-state) 'latin)))))

(nskk-describe "nskk-toggle-japanese-mode behavior (keymap)"
  (nskk-it "toggles hiragana to katakana and back"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-toggle-japanese-mode)
      (should (eq (nskk-state-mode nskk-current-state) 'katakana))
      (nskk-toggle-japanese-mode)
      (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))))

;;;
;;; nskk-handle-q behavior
;;;

(nskk-describe "nskk-handle-q behavior"
  (nskk-it "toggles to katakana when in hiragana"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-handle-q)
      (should (eq (nskk-state-mode nskk-current-state) 'katakana))))

  (nskk-it "self-inserts 'q' when in ascii mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii))
            (last-command-event ?q))
        (nskk-handle-q)
        (should (equal (buffer-string) "q")))))

  (nskk-it "self-inserts 'q' when state is nil"
    (with-temp-buffer
      (let ((nskk-current-state nil)
            (last-command-event ?q))
        (nskk-handle-q)
        (should (equal (buffer-string) "q")))))

  (nskk-it "does implicit kakutei then toggles when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-handle-q)
        (should-not (nskk-converting-p)))))

  (nskk-context "abbrev mode regression"
    (nskk-it "self-inserts 'q' in abbrev mode even with active preedit"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'abbrev))
              (last-command-event ?q))
          (nskk-set-conversion-start-marker (point-min))
          (insert "▽emai")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-handle-q)
          (should (eq (nskk-state-mode nskk-current-state) 'abbrev))
          (should (string-suffix-p "q" (buffer-string)))))))

  (nskk-context "AZIK preedit q dispatch"
    (nskk-it "delegates to nskk-handle-q-key in AZIK preedit with empty romaji"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-converter-romaji-style 'azik)
              (delegated nil))
          (nskk-state-set-romaji-buffer "")
          (nskk-set-conversion-start-marker (point-min))
          (insert "▽か")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (cl-letf (((symbol-function 'nskk-handle-q-key) (lambda () (setq delegated t)))
                    ((symbol-function 'nskk-henkan-kakutei-convert-script) (lambda () (error "must not call"))))
            (nskk-handle-q))
          (should delegated))))

    (nskk-it "calls convert-script in standard preedit with empty romaji"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'hiragana))
              (nskk-converter-romaji-style 'roman)
              (converted nil))
          (nskk-state-set-romaji-buffer "")
          (nskk-set-conversion-start-marker (point-min))
          (insert "▽か")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (cl-letf (((symbol-function 'nskk-henkan-kakutei-convert-script) (lambda () (setq converted t)))
                    ((symbol-function 'nskk-handle-q-key) (lambda () (error "must not call"))))
            (nskk-handle-q))
          (should converted))))))

;;;
;;; nskk-handle-l behavior
;;;

(nskk-describe "nskk-handle-l behavior"
  (nskk-it "enters latin mode when in hiragana"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-handle-l)
      (should (eq (nskk-state-mode nskk-current-state) 'latin))))

  (nskk-it "self-inserts 'l' when in ascii mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii))
            (last-command-event ?l))
        (nskk-handle-l)
        (should (equal (buffer-string) "l")))))

  (nskk-it "does implicit kakutei then enters latin when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-handle-l)
        (should-not (nskk-converting-p))
        (should (eq (nskk-state-mode nskk-current-state) 'latin)))))

  (nskk-context "abbrev mode regression"
    (nskk-it "self-inserts 'l' in abbrev mode even with active preedit"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'abbrev))
              (last-command-event ?l))
          (nskk-set-conversion-start-marker (point-min))
          (insert "▽emai")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-handle-l)
          (should (eq (nskk-state-mode nskk-current-state) 'abbrev))
          (should (string-suffix-p "l" (buffer-string)))))))

  (nskk-context "AZIK table priority"
    (nskk-it "fires romaji via azik-complete-match-p even when romaji buffer is empty"
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (process-called nil))
        (nskk-with-mocks ((nskk--azik-complete-match-p (lambda (_) t))
                          (nskk-process-japanese-input (lambda (_c _n) (setq process-called t))))
          (nskk-handle-l)
          (should process-called)
          (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))))

    (nskk-it "still fires romaji for zl -> -> in standard mode (nskk--romaji-has-match-p path)"
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (process-called nil))
        (nskk-with-mocks ((nskk--azik-complete-match-p (lambda (_) nil))
                          (nskk--romaji-has-match-p    (lambda (_) t))
                          (nskk-process-japanese-input (lambda (_c _n) (setq process-called t))))
          (nskk-handle-l)
          (should process-called)
          (should (eq (nskk-state-mode nskk-current-state) 'hiragana)))))

    (nskk-it "switches to latin mode when neither check fires"
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-with-mocks ((nskk--azik-complete-match-p (lambda (_) nil))
                          (nskk--romaji-has-match-p    (lambda (_) nil)))
          (nskk-handle-l)
          (should (eq (nskk-state-mode nskk-current-state) 'latin)))))))

;;;
;;; nskk-handle-upper-l behavior
;;;

(nskk-describe "nskk-handle-upper-l behavior"
  (nskk-it "enters jisx0208-latin mode when in hiragana"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-handle-upper-l)
      (should (eq (nskk-state-mode nskk-current-state) 'jisx0208-latin))))

  (nskk-it "self-inserts 'L' when in ascii mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii))
            (last-command-event ?L))
        (nskk-handle-upper-l)
        (should (equal (buffer-string) "L")))))

  (nskk-it "does implicit kakutei then switches mode when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-handle-upper-l)
        (should-not (nskk-converting-p))
        (should (eq (nskk-state-mode nskk-current-state) 'jisx0208-latin)))))

  (nskk-context "abbrev mode regression"
    (nskk-it "self-inserts 'L' in abbrev mode even with active preedit"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'abbrev))
              (last-command-event ?L))
          (nskk-set-conversion-start-marker (point-min))
          (insert "▽emai")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-handle-upper-l)
          (should (eq (nskk-state-mode nskk-current-state) 'abbrev))
          (should (string-suffix-p "L" (buffer-string))))))))

;;;
;;; nskk-handle-upper-x behavior
;;;

(nskk-describe "nskk-handle-upper-x behavior"
  (nskk-it "calls nskk-purge-from-jisyo when converting"
    (let ((purge-called nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () t))
                        (nskk-purge-from-jisyo
                         (lambda () (setq purge-called t))))
        (nskk-handle-upper-x)
        (should purge-called))))

  (nskk-it "calls nskk-self-insert when not converting"
    (let ((self-insert-called nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-self-insert
                         (lambda (_n) (setq self-insert-called t))))
        (nskk-handle-upper-x)
        (should self-insert-called)))))

;;;
;;; nskk-handle-slash behavior
;;;

(nskk-describe "nskk-handle-slash behavior"
  (nskk-it "enters abbrev mode when in hiragana"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-handle-slash)
      (should (eq (nskk-state-mode nskk-current-state) 'abbrev))))

  (nskk-it "self-inserts '/' when in ascii mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii))
            (last-command-event ?/))
        (nskk-handle-slash)
        (should (equal (buffer-string) "/")))))

  (nskk-context "abbrev mode regression"
    (nskk-it "self-inserts '/' in abbrev mode even with active preedit"
      (with-temp-buffer
        (let ((nskk-current-state (nskk-state-create 'abbrev))
              (last-command-event ?/))
          (nskk-set-conversion-start-marker (point-min))
          (insert "▽http:")
          (nskk-state-set-henkan-phase nskk-current-state 'on)
          (nskk-handle-slash)
          (should (eq (nskk-state-mode nskk-current-state) 'abbrev))
          (should (string-suffix-p "/" (buffer-string))))))))

;;;
;;; nskk-handle-x behavior
;;;

(nskk-describe "nskk-handle-x behavior"
  (nskk-it "accumulates 'x' in romaji buffer when not converting"
    (nskk-with-test-buffer 'hiragana
      (let ((last-command-event ?x))
        (nskk-handle-x)
        (should (equal (nskk-state-romaji-buffer) "x"))
        (should (equal (buffer-string) "")))))

  (nskk-it "calls nskk-previous-candidate when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (prev-candidate-called nil))
        (nskk-set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk-previous-candidate (lambda () (setq prev-candidate-called t))))
          (nskk-handle-x)
          (should prev-candidate-called))))))

;;;
;;; nskk-handle-space behavior
;;;

(nskk-describe "nskk-handle-space behavior"
  (nskk-it "inserts a space when no preedit"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (last-command-event ? ))
        (nskk-handle-space)
        (should (equal (buffer-string) " ")))))

  (nskk-it "starts conversion when preedit exists"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-set-conversion-start-marker (point-min))
        (insert "▽test")
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-with-mocks ((nskk-core-search/k
                           (lambda (_k _t _l on-found _on-not-found)
                             (funcall on-found '("result")))))
          (nskk-handle-space)
          (should (nskk-converting-p))
          (when (overlayp (nskk-state-conversion-overlay))
            (delete-overlay (nskk-state-conversion-overlay)))))))

  (nskk-it "calls nskk-next-candidate when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (next-candidate-called nil))
        (nskk-set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk-next-candidate (lambda () (setq next-candidate-called t))))
          (nskk-handle-space)
          (should next-candidate-called))))))

;;;
;;; nskk-handle-return behavior
;;;

(nskk-describe "nskk-handle-return behavior"
  (nskk-it "inserts newline when not converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-handle-return)
        (should (equal (buffer-string) "\n")))))

  (nskk-it "commits without newline when in conversion"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-handle-return)
        (should-not (nskk-converting-p))
        (should (equal (buffer-string) "result")))))

  (nskk-it "key-action/3 has explicit preedit row for return (kakutei-and-newline)"
    (should (eq (nskk-prolog-query-value
                 `(key-action return preedit ,'\?action) '\?action)
                'kakutei-and-newline)))

  (nskk-it "calls nskk-henkan-kakutei then newline when in preedit"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (kakutei-called nil)
            (newline-called nil))
        (nskk-set-conversion-start-marker (point-min))
        (insert "▽か")
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-with-mocks ((nskk-henkan-kakutei (lambda () (setq kakutei-called t)))
                          (newline             (lambda () (setq newline-called t))))
          (nskk-handle-return)
          (should kakutei-called)
          (should newline-called)))))

  (nskk-context "fall-through in normal state"
    (nskk-it "delegates to local RET binding when nskk-mode is active (corfu-style passthrough)"
      (with-temp-buffer
        (nskk-mode 1)
        (unwind-protect
            (let ((passthrough-called nil))
              (local-set-key (kbd "RET")
                             (lambda () (interactive) (setq passthrough-called t)))
              (let ((nskk-current-state (nskk-state-create 'hiragana)))
                (call-interactively 'nskk-handle-return))
              (should passthrough-called))
          (nskk-mode -1))))

    (nskk-it "falls back to newline when key-binding returns nil in normal state"
      (with-temp-buffer
        (let ((newline-called nil))
          (nskk-with-mocks ((key-binding (lambda (_key) nil))
                            (newline     (lambda () (setq newline-called t))))
            (let ((nskk-current-state (nskk-state-create 'hiragana)))
              (call-interactively 'nskk-handle-return)))
          (should newline-called))))

    (nskk-it "does not raise wrong-type-argument for keyboard-macro RET bindings"
      (with-temp-buffer
        (let ((error-raised nil))
          (condition-case _err
              (nskk-with-mocks ((key-binding       (lambda (_key) "test"))
                                (execute-kbd-macro (lambda (&rest _) nil)))
                (let ((nskk-current-state (nskk-state-create 'hiragana)))
                  (call-interactively 'nskk-handle-return)))
            (wrong-type-argument (setq error-raised t)))
          (should-not error-raised))))))

;;;
;;; nskk-handle-cancel behavior
;;;

(nskk-describe "nskk-handle-cancel behavior"
  (nskk-it "calls keyboard-quit when not converting"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (quit-called nil)
          (saved-marker (nskk-state-conversion-start-marker)))
      (unwind-protect
          (progn
            (nskk-state-set-conversion-start-marker nil)
            (nskk-with-mocks ((keyboard-quit (lambda () (setq quit-called t))))
              (nskk-handle-cancel)
              (should quit-called)))
        (nskk-state-set-conversion-start-marker saved-marker))))

  (nskk-it "calls nskk-rollback-conversion when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (rollback-called nil))
        (nskk-set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk-rollback-conversion (lambda () (setq rollback-called t))))
          (nskk-handle-cancel)
          (should rollback-called)))))

  (nskk-it "calls nskk-cancel-preedit when in preedit state"
    (let ((cancel-called nil)
          (nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () t))
                        (nskk-get-conversion-start (lambda () 1))
                        (nskk-cancel-preedit (lambda () (setq cancel-called t))))
        (nskk-handle-cancel))
      (should cancel-called))))

;;;
;;; nskk-current-kakutei-state Tests
;;;

(nskk-describe "nskk-current-kakutei-state behavior"
  (nskk-it "returns 'converting when nskk-converting-p is true"
    (nskk-with-mocks ((nskk-converting-p (lambda () t))
                      (nskk-has-preedit (lambda () nil)))
      (should (eq (nskk-current-kakutei-state) 'converting))))

  (nskk-it "returns 'preedit when nskk-has-preedit is true"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () t))
                        (nskk-get-conversion-start (lambda () 1)))
        (should (eq (nskk-current-kakutei-state) 'preedit)))))

  (nskk-it "returns 'romaji-pending when (nskk-state-romaji-buffer) is non-empty"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk-has-preedit (lambda () nil)))
      (nskk-test-with-romaji-buffer "k"
        (should (eq (nskk-current-kakutei-state) 'romaji-pending)))))

  (nskk-it "returns 'hiragana-idle in hiragana mode with no pending input"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk-has-preedit (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-test-with-romaji-buffer ""
          (should (eq (nskk-current-kakutei-state) 'hiragana-idle))))))

  (nskk-it "returns 'katakana-idle in fullwidth katakana mode"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk-has-preedit (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'katakana)))
        (nskk-test-with-romaji-buffer ""
          (should (eq (nskk-current-kakutei-state) 'katakana-idle))))))

  (nskk-it "returns 'katakana-idle in half-width katakana mode"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk-has-preedit (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'katakana-半角)))
        (nskk-test-with-romaji-buffer ""
          (should (eq (nskk-current-kakutei-state) 'katakana-idle))))))

  (nskk-it "returns 'direct-idle in ascii mode with no pending input"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk-has-preedit (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'ascii)))
        (nskk-test-with-romaji-buffer ""
          (should (eq (nskk-current-kakutei-state) 'direct-idle)))))))

;;;
;;; C-n and C-p Handler Tests (using nskk-deftest-nav-handler macro)
;;;

(nskk-describe "nskk-handle-ctrl-n behavior"
  (nskk-it "calls nskk-next-candidate when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (next-candidate-called nil))
        (nskk-set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk-next-candidate (lambda () (setq next-candidate-called t))))
          (nskk-handle-ctrl-n)
          (should next-candidate-called)))))

  (nskk-it "calls next-line when not converting (normal state)"
    (let ((nav-called nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil))
                        (next-line (lambda (&rest _) (interactive) (setq nav-called t))))
        (let ((nskk-current-state (nskk-state-create)))
          (call-interactively 'nskk-handle-ctrl-n)))
      (should nav-called)))

  (nskk-it "calls next-line when nskk-current-state is nil"
    (let ((nav-called nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil))
                        (next-line (lambda (&rest _) (interactive) (setq nav-called t))))
        (let ((nskk-current-state nil))
          (call-interactively 'nskk-handle-ctrl-n)))
      (should nav-called)))

  (nskk-it "calls nskk-henkan-kakutei (not nskk-commit-current) then next-line in preedit state"
    (let ((commit-called nil)
          (kakutei-called nil)
          (nav-called nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () t))
                        (nskk-get-conversion-start (lambda () 1))
                        (nskk-commit-current (lambda () (setq commit-called t)))
                        (nskk-henkan-kakutei (lambda () (setq kakutei-called t)))
                        (next-line (lambda (&rest _) (interactive) (setq nav-called t))))
        (let* ((preedit-state (nskk-state-create 'hiragana))
               (_ (nskk-state-force-henkan-phase preedit-state 'on))
               (nskk-current-state preedit-state))
          (call-interactively 'nskk-handle-ctrl-n)))
      (should-not commit-called)
      (should kakutei-called)
      (should nav-called))))

(nskk-describe "nskk-handle-ctrl-p behavior"
  (nskk-it "calls nskk-previous-candidate when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (prev-candidate-called nil))
        (nskk-set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk-previous-candidate (lambda () (setq prev-candidate-called t))))
          (nskk-handle-ctrl-p)
          (should prev-candidate-called)))))

  (nskk-it "calls previous-line when not converting (normal state)"
    (let ((nav-called nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil))
                        (previous-line (lambda (&rest _) (interactive) (setq nav-called t))))
        (let ((nskk-current-state (nskk-state-create)))
          (call-interactively 'nskk-handle-ctrl-p)))
      (should nav-called)))

  (nskk-it "calls previous-line when nskk-current-state is nil"
    (let ((nav-called nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil))
                        (previous-line (lambda (&rest _) (interactive) (setq nav-called t))))
        (let ((nskk-current-state nil))
          (call-interactively 'nskk-handle-ctrl-p)))
      (should nav-called)))

  (nskk-it "calls nskk-henkan-kakutei (not nskk-commit-current) then previous-line in preedit state"
    (let ((commit-called nil)
          (kakutei-called nil)
          (nav-called nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () t))
                        (nskk-get-conversion-start (lambda () 1))
                        (nskk-commit-current (lambda () (setq commit-called t)))
                        (nskk-henkan-kakutei (lambda () (setq kakutei-called t)))
                        (previous-line (lambda (&rest _) (interactive) (setq nav-called t))))
        (let* ((preedit-state (nskk-state-create 'hiragana))
               (_ (nskk-state-force-henkan-phase preedit-state 'on))
               (nskk-current-state preedit-state))
          (call-interactively 'nskk-handle-ctrl-p)))
      (should-not commit-called)
      (should kakutei-called)
      (should nav-called))))

;;;
;;; Helper Function for Cursor Key Tests
;;;

(defun nskk-test-setup-converting (preedit candidate)
  "Setup converting mode for PREEDIT text with CANDIDATE.
PREEDIT should already be in buffer starting at point.
Sets conversion-start-marker at point, advances past PREEDIT,
and configures state."
  (nskk-set-conversion-start-marker (point))
  (forward-char (length preedit))
  (nskk-state-set-candidates nskk-current-state (list candidate))
  (nskk-state-force-henkan-phase nskk-current-state 'active))

;;;
;;; Cursor Key Behavior Changes - Commit Then Move (▼ converting mode)
;;;

(nskk-describe "cursor key commit-then-move behavior"
  (nskk-it "C-n calls nskk-next-candidate in converting mode (does not commit)"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (next-called nil))
        (nskk-state-set-romaji-buffer "")
        (insert "あいうえお\nかきくけこ")
        (goto-char (point-min))
        (nskk-test-setup-converting "あい" "愛")
        (nskk-with-mocks ((nskk-next-candidate (lambda () (setq next-called t))))
          (nskk-handle-ctrl-n))
        (should next-called)
        (should (eq (nskk-state-henkan-phase nskk-current-state) 'active)))))

  (nskk-it "C-p shows previous candidate in converting mode (does not commit)"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (prev-called nil))
        (nskk-state-set-romaji-buffer "")
        (insert "あいうえお\nかきくけこ")
        (goto-char (point-min))
        (forward-line 1)
        (nskk-test-setup-converting "か" "書")
        (nskk-with-mocks ((nskk-previous-candidate (lambda () (setq prev-called t))))
          (nskk-handle-ctrl-p))
        (should prev-called)
        (should (eq (nskk-state-henkan-phase nskk-current-state) 'active)))))

  (nskk-it "C-n in converting mode calls next-candidate (no end-of-buffer error)"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (next-called nil))
        (nskk-state-set-romaji-buffer "")
        (insert "あい")
        (goto-char (point-min))
        (nskk-test-setup-converting "あい" "愛")
        (nskk-with-mocks ((nskk-next-candidate (lambda () (setq next-called t))))
          (nskk-handle-ctrl-n))
        (should next-called)
        (should (eq (nskk-state-henkan-phase nskk-current-state) 'active)))))

  (nskk-it "C-p in converting mode calls previous-candidate (no beginning-of-buffer error)"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (prev-called nil))
        (nskk-state-set-romaji-buffer "")
        (insert "あい")
        (goto-char (point-min))
        (nskk-test-setup-converting "あい" "愛")
        (nskk-with-mocks ((nskk-previous-candidate (lambda () (setq prev-called t))))
          (nskk-handle-ctrl-p))
        (should prev-called)
        (should (eq (nskk-state-henkan-phase nskk-current-state) 'active))))))

;;;
;;; C-f and C-b Handler Tests
;;;

(nskk-describe "nskk-handle-ctrl-f behavior"
  (nskk-deftest-nav-handler ctrl-f nskk-handle-ctrl-f forward-char))

(nskk-describe "nskk-handle-ctrl-b behavior"
  (nskk-deftest-nav-handler ctrl-b nskk-handle-ctrl-b backward-char))

;;;
;;; nskk--current-key-state Tests
;;;

(nskk-describe "nskk--current-key-state behavior"
  (nskk-it "returns 'converting when nskk-converting-p is true"
    (nskk-with-mocks ((nskk-converting-p (lambda () t))
                      (nskk-has-preedit (lambda () nil))
                      (nskk-get-conversion-start (lambda () nil)))
      (should (eq (nskk--current-key-state) 'converting))))

  (nskk-it "returns 'preedit when nskk-has-preedit is true in Japanese mode"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () t))
                        (nskk-get-conversion-start (lambda () 1)))
        (should (eq (nskk--current-key-state) 'preedit)))))

  (nskk-it "returns 'normal in hiragana mode with no preedit"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk-has-preedit (lambda () nil))
                      (nskk-get-conversion-start (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (should (eq (nskk--current-key-state) 'normal)))))

  (nskk-context "abbrev mode + marker"
    (nskk-it "returns 'preedit in abbrev mode when conversion marker is set"
      (with-temp-buffer
        (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                          (nskk-has-preedit (lambda () nil)))
          (let ((nskk-current-state (nskk-state-create 'abbrev)))
            (nskk-set-conversion-start-marker (point-min))
            (should (eq (nskk--current-key-state) 'preedit))))))

    (nskk-it "returns 'normal in abbrev mode when no marker is set"
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil))
                        (nskk-get-conversion-start (lambda () nil)))
        (let ((nskk-current-state (nskk-state-create 'abbrev)))
          (should (eq (nskk--current-key-state) 'normal)))))

    (nskk-it "returns 'normal for latin mode even with a marker (abbrev-only branch)"
      (with-temp-buffer
        (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                          (nskk-has-preedit (lambda () nil)))
          (let ((nskk-current-state (nskk-state-create 'latin)))
            (nskk-set-conversion-start-marker (point-min))
            (should (eq (nskk--current-key-state) 'normal))))))

    (nskk-it "abbrev-with-marker branch fires before 'normal fallthrough (regression)"
      (with-temp-buffer
        (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                          (nskk-has-preedit (lambda () nil)))
          (let ((nskk-current-state (nskk-state-create 'abbrev)))
            (nskk-set-conversion-start-marker (point-min))
            (should-not (eq (nskk--current-key-state) 'normal))
            (should (eq (nskk--current-key-state) 'preedit))))))))

;;;
;;; nskk-handle-ctrl-a behavior
;;;

(nskk-describe "nskk-handle-ctrl-a behavior"
  (nskk-deftest-nav-handler ctrl-a nskk-handle-ctrl-a beginning-of-line))

;;;
;;; nskk-handle-ctrl-e behavior
;;;

(nskk-describe "nskk-handle-ctrl-e behavior"
  (nskk-deftest-nav-handler ctrl-e nskk-handle-ctrl-e end-of-line))

;;;
;;; nskk-handle-backspace behavior
;;;

(nskk-describe "nskk-handle-backspace behavior"
  (nskk-it "deletes last character when preedit has content"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-set-conversion-start-marker (point-min))
        (insert "▽ka")
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-handle-backspace)
        (should-not (string-suffix-p "a" (buffer-string))))))

  (nskk-it "calls nskk-cancel-preedit when preedit is empty"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (cancel-preedit-called nil))
        (nskk-set-conversion-start-marker (point-min))
        (insert "▽")
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-with-mocks ((nskk-cancel-preedit (lambda () (setq cancel-preedit-called t))))
          (nskk-handle-backspace)
          (should cancel-preedit-called)))))

  (nskk-it "calls nskk-rollback-conversion when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (rollback-called nil))
        (nskk-set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk-rollback-conversion (lambda () (setq rollback-called t))))
          (nskk-handle-backspace)
          (should rollback-called)))))

  (nskk-it "deletes backward char when no preedit (normal state)"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii)))
        (insert "abc")
        (nskk-handle-backspace)
        (should (equal (buffer-string) "ab")))))

  (nskk-it "does not delete committed text when point drifted left of preedit"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (insert "A▽ka")
        (nskk-set-conversion-start-marker 2)
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (goto-char 1)
        (nskk-handle-backspace)
        (should (equal (buffer-string) "A▽ka"))
        (should (= (point) 3))))))

;;;
;;; nskk--backspace-retract-pending
;;;

(nskk-describe "nskk--backspace-retract-pending"
  (nskk-it "returns non-nil and clears single-char romaji buffer"
    (let ((clear-called nil))
      (nskk-with-mocks ((nskk-clear-pending-romaji (lambda () (setq clear-called t))))
        (with-temp-buffer
          (nskk-state-set-romaji-buffer "k")
          (should (nskk--backspace-retract-pending))
          (should (equal (nskk-state-romaji-buffer) ""))
          (should clear-called)))))

  (nskk-it "returns non-nil and truncates multi-char romaji buffer"
    (let ((show-arg nil))
      (nskk-with-mocks ((nskk-show-pending-romaji (lambda (s) (setq show-arg s))))
        (with-temp-buffer
          (nskk-state-set-romaji-buffer "sh")
          (should (nskk--backspace-retract-pending))
          (should (equal (nskk-state-romaji-buffer) "s"))
          (should (equal show-arg "s"))))))

  (nskk-it "returns nil when all pending state is empty"
    (with-temp-buffer
      (let ((nskk--deferred-azik-state nil)
            (nskk--deferred-vowel-shadow-state nil)
            (nskk--azik-colon-okuri-pending nil)
            (nskk--azik-colon-okuri-deferred nil))
        (nskk-state-set-romaji-buffer "")
        (should-not (nskk--backspace-retract-pending)))))

  (nskk-it "returns non-nil and clears DA (deferred-azik-state)"
    (with-temp-buffer
      (let ((nskk--deferred-azik-state (cons ?k "きん")))
        (nskk-state-set-romaji-buffer "")
        (insert "きん")
        (goto-char (point-max))
        (should (nskk--backspace-retract-pending))
        (should-not (nskk-deferred-azik-state))
        (should (equal (buffer-string) "")))))

  (nskk-it "returns non-nil and clears DV (deferred-vowel-shadow-state)"
    (with-temp-buffer
      (let ((nskk--deferred-vowel-shadow-state
             (nskk--make-deferred-vowel-shadow "sh" "すう")))
        (nskk-state-set-romaji-buffer "")
        (insert "すう")
        (goto-char (point-max))
        (should (nskk--backspace-retract-pending))
        (should-not (nskk-deferred-vowel-shadow-state))
        (should (equal (buffer-string) "")))))

  (nskk-it "restores DA and overlay when a before-change hook errors"
    (with-temp-buffer
      (let* ((payload (cons ?k "きん"))
             (romaji (copy-sequence "entry"))
             (display (copy-sequence "entry-display"))
             (overlay (make-overlay 2 3))
             (nskk--deferred-azik-state payload)
             (nskk--deferred-vowel-shadow-state nil)
             (nskk--azik-colon-okuri-pending nil)
             (nskk--azik-colon-okuri-deferred nil)
             (calls 0))
        (nskk-state-set-romaji-buffer romaji)
        (nskk-state-set-pending-romaji-overlay overlay)
        (insert "AきんZ")
        (goto-char 4)
        (move-overlay overlay 2 3)
        (overlay-put overlay 'after-string display)
        (overlay-put overlay 'face 'bold)
        (add-hook
         'before-change-functions
         (lambda (&rest _)
           (setq calls (1+ calls))
           (when (= calls 1)
             (nskk-clear-pending-romaji)
             (error "injected before-change failure")))
         nil t)
        (should-error (nskk--backspace-retract-pending))
        (should (equal (buffer-string) "AきんZ"))
        (should (= (point) 4))
        (should (eq (nskk-deferred-azik-state) payload))
        (should (eq (nskk-state-romaji-buffer) romaji))
        (should (eq (nskk-state-pending-romaji-overlay) overlay))
        (should (eq (overlay-buffer overlay) (current-buffer)))
        (should (= (overlay-start overlay) 2))
        (should (= (overlay-end overlay) 3))
        (should (eq (overlay-get overlay 'after-string) display))
        (should (eq (overlay-get overlay 'face) 'bold)))))

  (nskk-it "restores DV and overlay when an after-change hook quits"
    (with-temp-buffer
      (let* ((payload (nskk--make-deferred-vowel-shadow "sh" "すう"))
             (romaji (copy-sequence "entry"))
             (display (copy-sequence "entry-display"))
             (overlay (make-overlay 2 3))
             (nskk--deferred-azik-state nil)
             (nskk--deferred-vowel-shadow-state payload)
             (nskk--azik-colon-okuri-pending nil)
             (nskk--azik-colon-okuri-deferred nil)
             (calls 0)
             (caught nil))
        (nskk-state-set-romaji-buffer romaji)
        (nskk-state-set-pending-romaji-overlay overlay)
        (insert "AすうZ")
        (goto-char 4)
        (move-overlay overlay 2 3)
        (overlay-put overlay 'after-string display)
        (overlay-put overlay 'face 'bold)
        (add-hook
         'after-change-functions
         (lambda (&rest _)
           (setq calls (1+ calls))
           (when (= calls 1)
             (nskk-show-pending-romaji "changed")
             (nskk-set-azik-colon-okuri-pending 'corrupted)
             (signal 'quit '(injected after-change quit))))
         nil t)
        (condition-case _
            (nskk--backspace-retract-pending)
          (quit (setq caught t)))
        (should caught)
        (should (equal (buffer-string) "AすうZ"))
        (should (= (point) 4))
        (should (eq (nskk-deferred-vowel-shadow-state) payload))
        (should-not (nskk-azik-colon-okuri-pending))
        (should (eq (nskk-state-romaji-buffer) romaji))
        (should (eq (nskk-state-pending-romaji-overlay) overlay))
        (should (eq (overlay-buffer overlay) (current-buffer)))
        (should (= (overlay-start overlay) 2))
        (should (= (overlay-end overlay) 3))
        (should (eq (overlay-get overlay 'after-string) display))
        (should (eq (overlay-get overlay 'face) 'bold)))))

  (nskk-it "restores CP when deletion fails in a read-only buffer"
    (with-temp-buffer
      (let* ((payload (list ?t "か"))
             (romaji (copy-sequence "entry"))
             (display (copy-sequence "entry-display"))
             (overlay (make-overlay 1 2))
             (nskk--deferred-azik-state nil)
             (nskk--deferred-vowel-shadow-state nil)
             (nskk--azik-colon-okuri-pending payload)
             (nskk--azik-colon-okuri-deferred nil))
        (nskk-state-set-romaji-buffer romaji)
        (nskk-state-set-pending-romaji-overlay overlay)
        (insert "AかZ")
        (goto-char 3)
        (move-overlay overlay 1 2)
        (overlay-put overlay 'after-string display)
        (let ((buffer-read-only t))
          (should-error (nskk--backspace-retract-pending)))
        (should (equal (buffer-string) "AかZ"))
        (should (= (point) 3))
        (should (eq (nskk-azik-colon-okuri-pending) payload))
        (should (eq (nskk-state-romaji-buffer) romaji))
        (should (eq (nskk-state-pending-romaji-overlay) overlay))
        (should (= (overlay-start overlay) 1))
        (should (= (overlay-end overlay) 2))
        (should (eq (overlay-get overlay 'after-string) display)))))

  (nskk-it "restores CD and all UI state when reset errors"
    (with-temp-buffer
      (let* ((payload (cons ?t "t"))
             (romaji (copy-sequence "entry"))
             (display (copy-sequence "entry-display"))
             (overlay (make-overlay 1 2))
             (original-reset (symbol-function 'nskk-reset-romaji-buffer))
             (nskk--deferred-azik-state nil)
             (nskk--deferred-vowel-shadow-state nil)
             (nskk--azik-colon-okuri-pending nil)
             (nskk--azik-colon-okuri-deferred payload))
        (nskk-state-set-romaji-buffer romaji)
        (nskk-state-set-pending-romaji-overlay overlay)
        (insert "AtZ")
        (goto-char 3)
        (move-overlay overlay 1 2)
        (overlay-put overlay 'after-string display)
        (overlay-put overlay 'face 'bold)
        (cl-letf (((symbol-function 'nskk-reset-romaji-buffer)
                   (lambda ()
                     (funcall original-reset)
                     (nskk-set-deferred-azik-state 'corrupted
                           (nskk-deferred-vowel-shadow-state) 'corrupted
                           (nskk-azik-colon-okuri-pending) 'corrupted)
                     (error "injected reset failure"))))
          (should-error (nskk--backspace-retract-pending)))
        (should (equal (buffer-string) "AtZ"))
        (should (= (point) 3))
        (should-not (nskk-deferred-azik-state))
        (should-not (nskk-deferred-vowel-shadow-state))
        (should-not (nskk-azik-colon-okuri-pending))
        (should (eq (nskk-azik-colon-okuri-deferred) payload))
        (should (eq (nskk-state-romaji-buffer) romaji))
        (should (eq (nskk-state-pending-romaji-overlay) overlay))
        (should (eq (overlay-buffer overlay) (current-buffer)))
        (should (= (overlay-start overlay) 1))
        (should (= (overlay-end overlay) 2))
        (should (eq (overlay-get overlay 'after-string) display))
        (should (eq (overlay-get overlay 'face) 'bold)))))

  (nskk-it "restores single romaji and overlay when clear quits"
    (with-temp-buffer
      (let* ((romaji (copy-sequence "k"))
             (display (copy-sequence "entry-display"))
             (overlay (make-overlay 1 2))
             (original-clear (symbol-function 'nskk-clear-pending-romaji))
             (nskk--deferred-azik-state nil)
             (nskk--deferred-vowel-shadow-state nil)
             (nskk--azik-colon-okuri-pending nil)
             (nskk--azik-colon-okuri-deferred nil)
             (caught nil))
        (nskk-state-set-romaji-buffer romaji)
        (nskk-state-set-pending-romaji-overlay overlay)
        (insert "AZ")
        (goto-char 2)
        (move-overlay overlay 1 2)
        (overlay-put overlay 'after-string display)
        (overlay-put overlay 'face 'bold)
        (cl-letf (((symbol-function 'nskk-clear-pending-romaji)
                   (lambda ()
                     (funcall original-clear)
                     (nskk-set-deferred-vowel-shadow-state 'corrupted)
                     (signal 'quit '(injected clear quit)))))
          (condition-case _
              (nskk--backspace-retract-pending)
            (quit (setq caught t))))
        (should caught)
        (should (equal (buffer-string) "AZ"))
        (should (= (point) 2))
        (should-not (nskk-deferred-vowel-shadow-state))
        (should (eq (nskk-state-romaji-buffer) romaji))
        (should (eq (nskk-state-pending-romaji-overlay) overlay))
        (should (eq (overlay-buffer overlay) (current-buffer)))
        (should (= (overlay-start overlay) 1))
        (should (= (overlay-end overlay) 2))
        (should (eq (overlay-get overlay 'after-string) display))
        (should (eq (overlay-get overlay 'face) 'bold)))))

  (nskk-it "restores multi romaji and overlay when show errors"
    (with-temp-buffer
      (let* ((romaji (copy-sequence "sh"))
             (display (copy-sequence "entry-display"))
             (overlay (make-overlay 1 2))
             (original-show (symbol-function 'nskk-show-pending-romaji))
             (nskk--deferred-azik-state nil)
             (nskk--deferred-vowel-shadow-state nil)
             (nskk--azik-colon-okuri-pending nil)
             (nskk--azik-colon-okuri-deferred nil))
        (nskk-state-set-romaji-buffer romaji)
        (nskk-state-set-pending-romaji-overlay overlay)
        (insert "AZ")
        (goto-char 2)
        (move-overlay overlay 1 2)
        (overlay-put overlay 'after-string display)
        (overlay-put overlay 'face 'bold)
        (cl-letf (((symbol-function 'nskk-show-pending-romaji)
                   (lambda (value)
                     (funcall original-show value)
                     (nskk-set-azik-colon-okuri-deferred 'corrupted)
                     (error "injected show failure"))))
          (should-error (nskk--backspace-retract-pending)))
        (should (equal (buffer-string) "AZ"))
        (should (= (point) 2))
        (should-not (nskk-azik-colon-okuri-deferred))
        (should (eq (nskk-state-romaji-buffer) romaji))
        (should (eq (nskk-state-pending-romaji-overlay) overlay))
        (should (eq (overlay-buffer overlay) (current-buffer)))
        (should (= (overlay-start overlay) 1))
        (should (= (overlay-end overlay) 2))
        (should (eq (overlay-get overlay 'after-string) display))
        (should (eq (overlay-get overlay 'face) 'bold)))))

  (nskk-it "commits romaji and overlay update together"
    (with-temp-buffer
      (let* ((overlay (make-overlay 1 2))
             (nskk--deferred-azik-state nil)
             (nskk--deferred-vowel-shadow-state nil)
             (nskk--azik-colon-okuri-pending nil)
             (nskk--azik-colon-okuri-deferred nil))
        (nskk-state-set-romaji-buffer "sh")
        (nskk-state-set-pending-romaji-overlay overlay)
        (insert "AZ")
        (goto-char 2)
        (move-overlay overlay 1 2)
        (overlay-put overlay 'after-string "old")
        (should (nskk--backspace-retract-pending))
        (should (equal (buffer-string) "AZ"))
        (should (= (point) 2))
        (should (equal (nskk-state-romaji-buffer) "s"))
        (should (eq (nskk-state-pending-romaji-overlay) overlay))
        (should (eq (overlay-buffer overlay) (current-buffer)))
        (should (= (overlay-start overlay) 2))
        (should (= (overlay-end overlay) 2))
        (should (equal (overlay-get overlay 'after-string) "s"))))))

;;;
;;; nskk--backspace-in-preedit
;;;

(nskk-describe "nskk--backspace-in-preedit"
  (nskk-it "deletes last character when preedit has content"
    (let ((deleted nil))
      (nskk-with-mocks ((nskk-get-conversion-start (lambda () (point-min)))
                        (delete-char (lambda (n) (setq deleted n))))
        (with-temp-buffer
          (insert "▽ab")
          (goto-char (point-max))
          (let ((nskk-henkan-on-marker "▽"))
            (nskk--backspace-in-preedit))))
      (should (equal deleted -1))))

  (nskk-it "calls nskk-cancel-preedit when preedit is empty (point at marker boundary)"
    (let ((cancel-called nil))
      (nskk-with-mocks ((nskk-get-conversion-start (lambda () 1))
                        (nskk-cancel-preedit (lambda () (setq cancel-called t))))
        (with-temp-buffer
          (insert "▽")
          (goto-char (point-max))
          (let ((nskk-henkan-on-marker "▽"))
            (nskk--backspace-in-preedit)))
        (should cancel-called))))

  (nskk-it "moves point to preedit boundary when point drifted left"
    (let ((cancel-called nil)
          (delete-called nil))
      (nskk-with-mocks ((nskk-cancel-preedit (lambda () (setq cancel-called t)))
                        (delete-char (lambda (_n) (setq delete-called t))))
        (with-temp-buffer
          (let ((nskk-henkan-on-marker "▽"))
            (insert "A▽ka")
            (goto-char 1)
            (nskk-set-conversion-start-marker 2)
            (nskk--backspace-in-preedit)
            (should (= (point) 3))
            (should (equal (buffer-string) "A▽ka"))
            (should-not cancel-called)
            (should-not delete-called))))))

  (nskk-it "BS clears single-char romaji buffer"
    (let ((clear-called nil))
      (nskk-with-mocks ((nskk-get-conversion-start (lambda () (point-min)))
                        (nskk-clear-pending-romaji (lambda () (setq clear-called t))))
        (with-temp-buffer
          (let ((nskk-henkan-on-marker "▽"))
            (nskk-state-set-romaji-buffer "g")
            (insert "▽ほ")
            (goto-char (point-max))
            (nskk--backspace-in-preedit)
            (should (equal (nskk-state-romaji-buffer) ""))
            (should clear-called)
            (should (equal (buffer-string) "▽ほ")))))))

  (nskk-it "BS truncates multi-char romaji buffer"
    (let ((show-arg nil))
      (nskk-with-mocks ((nskk-get-conversion-start (lambda () (point-min)))
                        (nskk-show-pending-romaji (lambda (s) (setq show-arg s))))
        (with-temp-buffer
          (let ((nskk-henkan-on-marker "▽"))
            (nskk-state-set-romaji-buffer "ky")
            (insert "▽ほ")
            (goto-char (point-max))
            (nskk--backspace-in-preedit)
            (should (equal (nskk-state-romaji-buffer) "k"))
            (should (equal show-arg "k"))
            (should (equal (buffer-string) "▽ほ")))))))

  (nskk-it "BS truncates romaji \"k\" to empty and clears pending"
    (let ((clear-called nil))
      (nskk-with-mocks ((nskk-get-conversion-start (lambda () (point-min)))
                        (nskk-clear-pending-romaji (lambda () (setq clear-called t))))
        (with-temp-buffer
          (let ((nskk-henkan-on-marker "▽"))
            (nskk-state-set-romaji-buffer "k")
            (insert "▽ほ")
            (goto-char (point-max))
            (nskk--backspace-in-preedit)
            (should (equal (nskk-state-romaji-buffer) ""))
            (should clear-called)
            (should (equal (buffer-string) "▽ほ")))))))

  (nskk-it "BS rolls back DA (deferred-azik-state)"
    (nskk-with-mocks ((nskk-get-conversion-start (lambda () 1))
                      (nskk-cancel-preedit (lambda () nil)))
      (with-temp-buffer
        (let ((nskk-henkan-on-marker "▽")
              (nskk--deferred-azik-state (cons ?k "きん")))
          (nskk-state-set-romaji-buffer "")
          (insert "▽きん")
          (goto-char (point-max))
          (nskk--backspace-in-preedit)
          (should-not (nskk-deferred-azik-state))
          (should (equal (buffer-string) "▽"))))))

  (nskk-it "BS rolls back DV payload with continuation policy"
    (nskk-with-mocks ((nskk-get-conversion-start (lambda () 1))
                      (nskk-cancel-preedit (lambda () nil)))
      (with-temp-buffer
        (let ((nskk-henkan-on-marker "▽")
              (nskk--deferred-vowel-shadow-state
               (nskk--make-deferred-vowel-shadow
                "ch" "ちゅう"
                nskk--deferred-vowel-shadow-uppercase-vowel-continue-policy)))
          (nskk-state-set-romaji-buffer "")
          (insert "▽ちゅう")
          (goto-char (point-max))
          (nskk--backspace-in-preedit)
          (should-not (nskk-deferred-vowel-shadow-state))
          (should (equal (buffer-string) "▽"))))))

  (nskk-it "BS rolls back CP (colon-okuri-pending) deletes * marker"
    (nskk-with-mocks ((nskk-get-conversion-start (lambda () 1)))
      (with-temp-buffer
        (let ((nskk-henkan-on-marker "▽")
              (nskk--azik-colon-okuri-pending t))
          (nskk-state-set-romaji-buffer "")
          (insert "▽ほ*")
          (goto-char (point-max))
          (nskk--backspace-in-preedit)
          (should-not (nskk-azik-colon-okuri-pending))
          (should (equal (buffer-string) "▽ほ"))))))

  (nskk-it "BS rolls back CD (colon-okuri-deferred) deletes placeholder and resets romaji"
    (let ((reset-called nil))
      (nskk-with-mocks ((nskk-get-conversion-start (lambda () 1))
                        (nskk-reset-romaji-buffer (lambda () (setq reset-called t))))
        (with-temp-buffer
          (let ((nskk-henkan-on-marker "▽")
                (nskk--azik-colon-okuri-deferred (cons ?t "t")))
            (nskk-state-set-romaji-buffer "t")
            (insert "▽ほ*t")
            (goto-char (point-max))
            (nskk--backspace-in-preedit)
            (should-not (nskk-azik-colon-okuri-deferred))
            (should reset-called)
            (should (equal (buffer-string) "▽ほ*")))))))

  (nskk-it "DA rollback causes empty preedit triggers cancel-preedit"
    (let ((cancel-called nil))
      (nskk-with-mocks ((nskk-get-conversion-start (lambda () 1))
                        (nskk-cancel-preedit (lambda () (setq cancel-called t))))
        (with-temp-buffer
          (let ((nskk-henkan-on-marker "▽")
                (nskk--deferred-azik-state (cons ?k "きん")))
            (nskk-state-set-romaji-buffer "")
            (insert "▽きん")
            (goto-char (point-max))
            (nskk--backspace-in-preedit)
            (should cancel-called))))))

  (nskk-it "romaji empty and committed kana calls delete-char"
    (let ((deleted nil))
      (nskk-with-mocks ((nskk-get-conversion-start (lambda () 1))
                        (delete-char (lambda (n) (setq deleted n))))
        (with-temp-buffer
          (let ((nskk-henkan-on-marker "▽")
                (nskk--deferred-azik-state nil)
                (nskk--deferred-vowel-shadow-state nil)
                (nskk--azik-colon-okuri-pending nil)
                (nskk--azik-colon-okuri-deferred nil))
            (nskk-state-set-romaji-buffer "")
            (insert "▽か")
            (goto-char (point-max))
            (nskk--backspace-in-preedit)
            (should (equal deleted -1)))))))

  (nskk-it "romaji empty and preedit empty calls cancel-preedit"
    (let ((cancel-called nil))
      (nskk-with-mocks ((nskk-get-conversion-start (lambda () 1))
                        (nskk-cancel-preedit (lambda () (setq cancel-called t))))
        (with-temp-buffer
          (let ((nskk-henkan-on-marker "▽")
                (nskk--deferred-azik-state nil)
                (nskk--deferred-vowel-shadow-state nil)
                (nskk--azik-colon-okuri-pending nil)
                (nskk--azik-colon-okuri-deferred nil))
            (nskk-state-set-romaji-buffer "")
            (insert "▽")
            (goto-char (point-max))
            (nskk--backspace-in-preedit)
            (should cancel-called))))))

  (nskk-it "consecutive BS reduces romaji then deletes kana"
    (let ((show-called nil)
          (clear-called nil)
          (deleted nil))
      (nskk-with-mocks ((nskk-get-conversion-start (lambda () 1))
                        (nskk-show-pending-romaji (lambda (_s) (setq show-called t)))
                        (nskk-clear-pending-romaji (lambda () (setq clear-called t)))
                        (delete-char (lambda (n) (setq deleted n))))
        (with-temp-buffer
          (let ((nskk-henkan-on-marker "▽")
                (nskk--deferred-azik-state nil)
                (nskk--deferred-vowel-shadow-state nil)
                (nskk--azik-colon-okuri-pending nil)
                (nskk--azik-colon-okuri-deferred nil))
            (nskk-state-set-romaji-buffer "ky")
            (insert "▽ほ")
            (goto-char (point-max))
            (nskk--backspace-in-preedit)
            (should (equal (nskk-state-romaji-buffer) "k"))
            (should show-called)
            (nskk--backspace-in-preedit)
            (should (equal (nskk-state-romaji-buffer) ""))
            (should clear-called)
            (nskk--backspace-in-preedit)
            (should (equal deleted -1))))))))

;;;
;;; nskk-handle-tab behavior
;;;

(nskk-describe "nskk-handle-tab behavior"
  (nskk-it "calls nskk-dynamic-complete when preedit active"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk-dcomp-style 'cycle)
            (complete-called nil))
        (nskk-set-conversion-start-marker (point-min))
        (insert "▽ka")
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-with-mocks ((nskk-dynamic-complete (lambda () (setq complete-called t))))
          (nskk-handle-tab)
          (should complete-called)))))

  (nskk-it "delegates to major-mode TAB binding when not in preedit"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (major-mode-called nil))
        (let ((test-map (make-sparse-keymap)))
          (define-key test-map "\t"
            (lambda () (interactive) (setq major-mode-called t)))
          (use-local-map test-map)
          (nskk-handle-tab)
          (should major-mode-called)))))

  (nskk-it "falls back to indent-for-tab-command when no major-mode TAB binding"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (indent-called nil))
        (use-local-map (make-sparse-keymap))
        (nskk-with-mocks ((indent-for-tab-command (lambda (&rest _) (interactive) (setq indent-called t))))
          (nskk-handle-tab)
          (should indent-called)))))

  (nskk-it "delegates to major-mode TAB binding when converting (pass-through rule)"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (major-mode-called nil))
        (nskk-set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (let ((test-map (make-sparse-keymap)))
          (define-key test-map "\t"
            (lambda () (interactive) (setq major-mode-called t)))
          (use-local-map test-map)
          (nskk-handle-tab)
          (should major-mode-called))))))

;;;
;;; nskk-handle-hash behavior
;;;

(nskk-describe "nskk-handle-hash behavior"
  (nskk-it "calls nskk-set-mode-numeric when in hiragana"
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (numeric-called nil))
      (nskk-with-mocks ((nskk-set-mode-numeric (lambda () (setq numeric-called t))))
        (nskk-handle-hash)
        (should numeric-called))))

  (nskk-it "self-inserts '#' when in ascii mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii))
            (last-command-event ?#))
        (nskk-handle-hash)
        (should (equal (buffer-string) "#")))))

  (nskk-it "does implicit kakutei then calls nskk-set-mode-numeric when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (numeric-called nil))
        (nskk-set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk-set-mode-numeric (lambda () (setq numeric-called t))))
          (nskk-handle-hash)
          (should-not (nskk-converting-p))
          (should numeric-called))))))

;;;
;;; key-action/3 Prolog Dispatch Table Integrity Tests
;;;

(nskk-describe "key-action/3 Prolog dispatch table integrity"
  (nskk-deftest-table keymap-prolog-key-action-table
    :description "key-action/3 maps (key state) to expected action"
    :columns (key state expected-action)
    :rows (;; Space
           (space converting next-candidate)
           (space preedit   start-conversion)
           (space normal    self-insert)
           (return converting commit-candidate)
           (return preedit  kakutei-and-newline)
           (return normal   newline)
           (cancel converting rollback-to-reading)
           (cancel preedit   cancel-preedit)
           (cancel normal    keyboard-quit)
           (x converting previous-candidate)
           (x normal    self-insert)
           (ctrl-n converting next-candidate)
           (ctrl-n preedit    kakutei-then-next-line)
           (ctrl-n normal     next-line)
            (ctrl-p converting previous-candidate)
            (ctrl-p preedit    kakutei-then-previous-line)
            (ctrl-p normal     previous-line)
           (ctrl-f converting kakutei-then-forward)
           (ctrl-f preedit    kakutei-then-forward)
           (ctrl-f normal     forward-char)
           (ctrl-b converting kakutei-then-backward)
           (ctrl-b preedit    kakutei-then-backward)
           (ctrl-b normal     backward-char)
           (ctrl-a converting kakutei-then-bol)
           (ctrl-a preedit    kakutei-then-bol)
           (ctrl-a normal     beginning-of-line)
           (ctrl-e converting kakutei-then-eol)
           (ctrl-e preedit    kakutei-then-eol)
           (ctrl-e normal     end-of-line)
           (backspace preedit    delete-preedit-char)
           (backspace converting rollback-to-reading)
           (backspace normal     backward-delete)
           (tab preedit    dynamic-complete)
           (tab converting pass-through)
           (tab normal     pass-through))
    :body (should (eq expected-action
                      (nskk-prolog-query-value
                       (list 'key-action key state '\?a) '\?a)))))

;;;
;;; l-key-action/3 Prolog Dispatch Table Tests
;;;

(nskk-describe "l-key-action/3 Prolog dispatch table"
  (nskk-it "azik + azik-complete maps to fire-romaji"
    (should (eq (nskk-prolog-query-value
                 `(l-key-action azik azik-complete ,'\?action) '\?action)
                'fire-romaji)))

  (nskk-it "azik + other maps to latin-mode"
    (should (eq (nskk-prolog-query-value
                 `(l-key-action azik other ,'\?action) '\?action)
                'latin-mode)))

  (nskk-it "standard style always maps to latin-mode regardless of buf-state"
    (should (eq (nskk-prolog-query-value
                 `(l-key-action standard other ,'\?action) '\?action)
                'latin-mode)))

  (nskk-it "standard style maps to fire-romaji for azik-complete state (supports zl)"
    (should (eq (nskk-prolog-query-value
                 `(l-key-action standard azik-complete ,'\?action) '\?action)
                'fire-romaji))))

;;;
;;; state-classify/4 Prolog Table Tests
;;;

(nskk-describe "state-classify/4 Prolog table completeness"
  (nskk-it "converting phase always yields converting regardless of text and mode"
    (dolist (text '(has-text no-text))
      (dolist (cat '(japanese marker-mode other))
        (should (eq (nskk-prolog-query-value
                     `(state-classify converting ,text ,cat \?c) '\?c)
                    'converting)))))

  (nskk-it "henkan-on + japanese + has-text yields preedit-japanese"
    (should (nskk-prolog-holds-p
             '(state-classify henkan-on has-text japanese preedit-japanese))))

  (nskk-it "henkan-on + japanese + no-text yields preedit-pending"
    (should (nskk-prolog-holds-p
             '(state-classify henkan-on no-text japanese preedit-pending))))

  (nskk-it "henkan-on + marker-mode always yields preedit-marker"
    (dolist (text '(has-text no-text))
      (should (nskk-prolog-holds-p
               `(state-classify henkan-on ,text marker-mode preedit-marker)))))

  (nskk-it "idle + japanese always yields idle-japanese"
    (dolist (text '(has-text no-text))
      (should (nskk-prolog-holds-p
               `(state-classify idle ,text japanese idle-japanese)))))

  (nskk-it "idle + non-japanese always yields idle-direct"
    (dolist (text '(has-text no-text))
      (dolist (cat '(marker-mode other))
        (should (nskk-prolog-holds-p
                 `(state-classify idle ,text ,cat idle-direct))))))

  ;; Guards the range, not the mapping: a row added with a classification
  ;; outside this set would be silently swallowed by the `(or ... 'default)'
  ;; fallback in each of the three downstream classifiers.
  (nskk-it "every input combination yields one of the six known classifications"
    (let ((known '(converting preedit-japanese preedit-pending
                   preedit-marker idle-japanese idle-direct))
          (queried 0))
      (dolist (phase '(converting henkan-on idle))
        (dolist (text '(has-text no-text))
          (dolist (cat '(japanese marker-mode other))
            (let ((classification (nskk-prolog-query-value
                                   `(state-classify ,phase ,text ,cat \?c) '\?c)))
              (should classification)
              (should (memq classification known))
              (setq queried (1+ queried))))))
      (should (= queried 18)))))

;;;
;;; kakutei-active-state/3 Prolog Table Tests
;;;

(nskk-describe "kakutei-active-state/3 Prolog table integrity"
  (nskk-it "converting maps to converting for both text variants"
    (dolist (text '(has-text no-text))
      (should (eq (nskk-prolog-query-value
                   `(kakutei-active-state converting ,text \?s) '\?s)
                  'converting))))

  (nskk-it "preedit-japanese maps to preedit for both text variants"
    (dolist (text '(has-text no-text))
      (should (eq (nskk-prolog-query-value
                   `(kakutei-active-state preedit-japanese ,text \?s) '\?s)
                  'preedit))))

  (nskk-it "preedit-pending maps to preedit for both text variants"
    (dolist (text '(has-text no-text))
      (should (eq (nskk-prolog-query-value
                   `(kakutei-active-state preedit-pending ,text \?s) '\?s)
                  'preedit))))

  (nskk-it "preedit-marker with has-text maps to preedit"
    (should (eq (nskk-prolog-query-value
                 '(kakutei-active-state preedit-marker has-text \?s) '\?s)
                'preedit)))

  (nskk-it "preedit-marker with no-text returns nil (falls through to idle)"
    (should-not (nskk-prolog-query-value
                 '(kakutei-active-state preedit-marker no-text \?s) '\?s))))

;;;
;;; mode-switch-preaction/2 preedit-pending Tests
;;;

(nskk-describe "mode-switch-preaction/2 preedit-pending row"
  (nskk-it "preedit-pending maps to henkan-kakutei (not noop)"
    (should (eq (nskk-prolog-query-value
                 '(mode-switch-preaction preedit-pending \?a) '\?a)
                'henkan-kakutei))))

;;;
;;; nskk-define-key-handler
;;;

(nskk-describe "nskk-define-key-handler"
  (nskk-it "is a macro (not a plain function)"
    (should (macrop 'nskk-define-key-handler))))

;;;
;;; nskk--japanese-mode-class Tests
;;;

(nskk-describe "nskk--japanese-mode-class"
  (nskk-it "returns 'converting when in henkan-active state"
    (nskk-with-mocks ((nskk-converting-p (lambda () t))
                      (nskk-has-preedit (lambda () nil)))
      (should (eq (nskk--japanese-mode-class) 'converting))))

  (nskk-it "returns 'preedit-japanese when preedit is active in hiragana mode"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () t))
                        (nskk-get-conversion-start (lambda () 1)))
        (should (eq (nskk--japanese-mode-class) 'preedit-japanese)))))

  (nskk-it "returns 'other when preedit is active in abbrev mode (not Japanese)"
    (let ((nskk-current-state (nskk-state-create 'abbrev)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () t))
                        (nskk-get-conversion-start (lambda () 1)))
        (should (eq (nskk--japanese-mode-class) 'other)))))

  (nskk-it "returns 'idle-japanese when in hiragana mode with no preedit"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil)))
        (should (eq (nskk--japanese-mode-class) 'idle-japanese)))))

  (nskk-it "returns 'other when in ascii mode"
    (let ((nskk-current-state (nskk-state-create 'ascii)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil)))
        (should (eq (nskk--japanese-mode-class) 'other)))))

  (nskk-it "returns 'other when nskk-current-state is nil"
    (let ((nskk-current-state nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil)))
        (should (eq (nskk--japanese-mode-class) 'other))))))

;;;
;;; nskk--execute-preaction Tests
;;;

(nskk-describe "nskk--execute-preaction"
  (nskk-it "calls nskk-commit-current for 'commit-current"
    (let ((called nil))
      (nskk-with-mocks ((nskk-commit-current (lambda () (setq called t))))
        (nskk--execute-preaction 'commit-current))
      (should called)))

  (nskk-it "calls nskk-henkan-kakutei for 'henkan-kakutei"
    (let ((called nil))
      (nskk-with-mocks ((nskk-henkan-kakutei (lambda () (setq called t))))
        (nskk--execute-preaction 'henkan-kakutei))
      (should called)))

  (nskk-it "does nothing for 'noop"
    (let ((commit-called nil)
          (kakutei-called nil))
      (nskk-with-mocks ((nskk-commit-current (lambda () (setq commit-called t)))
                        (nskk-henkan-kakutei (lambda () (setq kakutei-called t))))
        (nskk--execute-preaction 'noop))
      (should-not commit-called)
      (should-not kakutei-called))))

;;;
;;; nskk--with-japanese-mode/k Tests
;;;

(nskk-describe "nskk--with-japanese-mode/k"
  (nskk-it "calls on-found in hiragana idle mode"
    (let ((found-called nil)
          (nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil)))
        (nskk--with-japanese-mode/k
         (lambda (_) (setq found-called t))
         (lambda () nil)))
      (should found-called)))

  (nskk-it "calls on-not-found in ascii mode"
    (let ((not-found-called nil)
          (nskk-current-state (nskk-state-create 'ascii)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil)))
        (nskk--with-japanese-mode/k
         (lambda (_) nil)
         (lambda () (setq not-found-called t))))
      (should not-found-called)))

  (nskk-it "executes commit-current pre-action then calls on-found in converting state"
    (let ((commit-called nil)
          (found-called nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () t))
                        (nskk-has-preedit (lambda () nil))
                        (nskk-commit-current (lambda () (setq commit-called t))))
        (nskk--with-japanese-mode/k
         (lambda (_) (setq found-called t))
         (lambda () nil)))
      (should commit-called)
      (should found-called))))

;;;
;;; nskk-define-mode-switch-handler Tests
;;;

(nskk-describe "nskk-define-mode-switch-handler"
  (nskk-it "is a macro (not a plain function)"
    (should (macrop 'nskk-define-mode-switch-handler))))

;;;
;;; nskk-classify-state Tests
;;;

(nskk-describe "nskk-classify-state"
  (nskk-it "returns 'converting when nskk-converting-p is true"
    (nskk-with-mocks ((nskk-converting-p (lambda () t))
                      (nskk-has-preedit (lambda () nil)))
      (should (eq (nskk-classify-state) 'converting))))

  (nskk-it "returns 'preedit-japanese when preedit in hiragana mode"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () t))
                        (nskk-get-conversion-start (lambda () 1)))
        (should (eq (nskk-classify-state) 'preedit-japanese)))))

  (nskk-it "returns 'preedit-marker when abbrev with marker set"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'abbrev)))
        (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                          (nskk-has-preedit (lambda () nil)))
          (nskk-set-conversion-start-marker (point-min))
          (should (eq (nskk-classify-state) 'preedit-marker))))))

  (nskk-it "returns 'preedit-pending when hiragana with marker set but no preedit text"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                          (nskk-has-preedit (lambda () nil)))
          (nskk-set-conversion-start-marker (point-min))
          (should (eq (nskk-classify-state) 'preedit-pending))))))

  (nskk-it "returns 'idle-japanese when hiragana idle"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil))
                        (nskk-get-conversion-start (lambda () nil)))
        (should (eq (nskk-classify-state) 'idle-japanese)))))

  (nskk-it "returns 'idle-direct when ascii mode"
    (let ((nskk-current-state (nskk-state-create 'ascii)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil))
                        (nskk-get-conversion-start (lambda () nil)))
        (should (eq (nskk-classify-state) 'idle-direct)))))

  (nskk-it "returns 'idle-direct when nskk-current-state is nil"
    (let ((nskk-current-state nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-has-preedit (lambda () nil)))
        (should (eq (nskk-classify-state) 'idle-direct)))))

  (nskk-it "converting takes priority over preedit"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () t))
                        (nskk-has-preedit (lambda () t))
                        (nskk-get-conversion-start (lambda () 1)))
        (should (eq (nskk-classify-state) 'converting))))))

;;;
;;; nskk--japanese-mode-active-p Tests
;;;

(nskk-describe "nskk--japanese-mode-active-p"
  (nskk-it "returns non-nil for hiragana mode"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (should (nskk--japanese-mode-active-p))))

  (nskk-it "returns non-nil for katakana mode"
    (let ((nskk-current-state (nskk-state-create 'katakana)))
      (should (nskk--japanese-mode-active-p))))

  (nskk-it "returns nil for ascii mode"
    (let ((nskk-current-state (nskk-state-create 'ascii)))
      (should-not (nskk--japanese-mode-active-p))))

  (nskk-it "returns nil when nskk-current-state is nil"
    (let ((nskk-current-state nil))
      (should-not (nskk--japanese-mode-active-p)))))

;;;
;;; nskk--safe-nav-command Tests
;;;

(nskk-describe "nskk--safe-nav-command"
  (nskk-it "is a macro (not a plain function)"
    (should (macrop 'nskk--safe-nav-command)))

  (nskk-it "calls the wrapped command interactively when it succeeds"
    (let (called)
      (nskk-with-mocks ((call-interactively (lambda (cmd) (setq called cmd))))
        (nskk--safe-nav-command #'next-line end-of-buffer))
      (should (eq called #'next-line))))

  (nskk-it "silently swallows the specified error type"
    (nskk-with-mocks ((call-interactively (lambda (_cmd) (signal 'end-of-buffer nil))))
      (should-not (nskk--safe-nav-command #'next-line end-of-buffer))))

  (nskk-it "does not swallow other error types"
    (nskk-with-mocks ((call-interactively (lambda (_cmd) (error "unexpected"))))
      (should-error (nskk--safe-nav-command #'next-line end-of-buffer)
                    :type 'error))))

;;;
;;; key-state-map/2 Prolog Table Integrity Tests
;;;

(nskk-describe "key-state-map/2 Prolog table integrity"
  (nskk-deftest-table keymap-prolog-key-state-map-table
    :description "key-state-map/2 maps rich state to simple dispatch state"
    :columns (rich-state expected-simple)
    :rows ((converting       converting)
           (preedit-japanese preedit)
           (preedit-pending  preedit)
           (preedit-marker   preedit)
           (idle-japanese    normal)
           (idle-direct      normal))
    :body (should (eq expected-simple
                      (nskk-prolog-query-value
                       (list 'key-state-map rich-state '\?s) '\?s)))))

;;;
;;; mode-class-map/2 Prolog Table Integrity Tests
;;;

(nskk-describe "mode-class-map/2 Prolog table integrity"
  (nskk-deftest-table keymap-prolog-mode-class-map-table
    :description "mode-class-map/2 maps rich state to mode-switch class"
    :columns (rich-state expected-class)
    :rows ((converting       converting)
           (preedit-japanese preedit-japanese)
           (preedit-pending  preedit-pending)
           (preedit-marker   other)
           (idle-japanese    idle-japanese)
           (idle-direct      other))
    :body (should (eq expected-class
                      (nskk-prolog-query-value
                       (list 'mode-class-map rich-state '\?c) '\?c)))))

;;;
;;; q-key-dispatch/3 Prolog Table Integrity Tests
;;;

(nskk-describe "q-key-dispatch/3 Prolog table integrity"
  ;; `nskk-handle-q' delegates its `fire-romaji' arm to `nskk-handle-q-key'
  ;; unconditionally, which is correct only while no standard row produces
  ;; `fire-romaji'.  Changing a standard row's action below to `fire-romaji'
  ;; would route standard style to `q-key-action/3''s `(standard ?buf
  ;; toggle-mode)' catch-all instead of converting the script.
  (nskk-deftest-table keymap-prolog-q-key-dispatch-table
    :description "q-key-dispatch/3 maps (class style) to q-key action"
    :columns (cls style expected-action)
    :rows ((preedit-japanese azik     fire-romaji)
           (preedit-japanese standard convert-script)
           (preedit-pending  azik     fire-romaji)
           (preedit-pending  standard convert-script)
           (converting       azik     mode-switch)
           (converting       standard mode-switch)
           (idle-japanese    azik     mode-switch)
           (idle-japanese    standard mode-switch)
           (idle-direct      azik     self-insert)
           (idle-direct      standard self-insert)
           (preedit-marker   azik     self-insert)
           (preedit-marker   standard self-insert))
    :body (should (eq expected-action
                      (nskk-prolog-query-value
                       (list 'q-key-dispatch cls style '\?a) '\?a)))))

;;;
;;; nskk-define-nav-handler Tests
;;;

(nskk-describe "nskk-define-nav-handler"
  (nskk-it "is a macro (not a plain function)"
    (should (macrop 'nskk-define-nav-handler))))

;;;
;;; nskk-compute-phase Tests
;;;

(nskk-describe "nskk-compute-phase"
  (nskk-it "returns 'converting when nskk-converting-p is true"
    (nskk-with-mocks ((nskk-converting-p (lambda () t)))
      (should (eq (nskk-compute-phase) 'converting))))

  (nskk-it "returns 'henkan-on when state exists and conversion-start is set"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-get-conversion-start (lambda () 1)))
        (should (eq (nskk-compute-phase) 'henkan-on)))))

  (nskk-it "returns 'idle when no state"
    (let ((nskk-current-state nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil)))
        (should (eq (nskk-compute-phase) 'idle)))))

  (nskk-it "returns 'idle when state exists but no conversion-start"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk-get-conversion-start (lambda () nil)))
        (should (eq (nskk-compute-phase) 'idle)))))

  (nskk-it "converting takes priority over henkan-on"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () t))
                        (nskk-get-conversion-start (lambda () 1)))
        (should (eq (nskk-compute-phase) 'converting))))))

;;;
;;; nskk--compute-text-presence Tests
;;;

(nskk-describe "nskk--compute-text-presence"
  (nskk-it "returns 'has-text when nskk-has-preedit is true"
    (nskk-with-mocks ((nskk-has-preedit (lambda () t)))
      (should (eq (nskk--compute-text-presence) 'has-text))))

  (nskk-it "returns 'no-text when nskk-has-preedit is false"
    (nskk-with-mocks ((nskk-has-preedit (lambda () nil)))
      (should (eq (nskk--compute-text-presence) 'no-text)))))

;;;
;;; nskk--compute-mode-category Tests
;;;

(nskk-describe "nskk--compute-mode-category"
  (nskk-it "returns 'other when nskk-current-state is nil"
    (let ((nskk-current-state nil))
      (should (eq (nskk--compute-mode-category) 'other))))

  (nskk-it "returns 'japanese for hiragana mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (should (eq (nskk--compute-mode-category) 'japanese)))))

  (nskk-it "returns 'japanese for katakana mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((nskk-current-state (nskk-state-create 'katakana)))
        (should (eq (nskk--compute-mode-category) 'japanese)))))

  (nskk-it "returns 'marker-mode for abbrev mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((nskk-current-state (nskk-state-create 'abbrev)))
        (should (eq (nskk--compute-mode-category) 'marker-mode)))))

  (nskk-it "returns 'other for ascii mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((nskk-current-state (nskk-state-create 'ascii)))
        (should (eq (nskk--compute-mode-category) 'other)))))

  (nskk-it "returns 'other for jisx0208-latin mode"
    (nskk-prolog-test-with-isolated-db
      (nskk-state-initialize-prolog)
      (let ((nskk-current-state (nskk-state-create 'jisx0208-latin)))
        (should (eq (nskk--compute-mode-category) 'other))))))

(provide 'nskk-keymap-test)

;;; nskk-keymap-test.el ends here
