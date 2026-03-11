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

;;; Code:

(require 'ert)
(require 'nskk-keymap)
(require 'nskk-henkan)
(require 'nskk-state)
(require 'nskk-test-framework)
(require 'nskk-test-macros)
(require 'nskk)  ; needed for nskk-mode-map

;;;
;;; Nav Handler Test Helpers
;;;

(defmacro nskk-deftest-nav-handler (key handler kbd-key arrow-key nav-fn)
  "Generate standard tests for a commit-then-navigate key handler.
KEY is a symbol like `ctrl-f'.  HANDLER is the command symbol.
KBD-KEY is the kbd string (e.g. \"C-f\").  ARROW-KEY is the arrow kbd string.
NAV-FN is the fallthrough navigation command symbol (e.g. `forward-char')."
  `(progn
     (nskk-it ,(format "%s is defined and interactive" handler)
       (should (commandp ',handler)))

     (nskk-it ,(format "%s is bound in nskk-mode-map" kbd-key)
       (should (eq (lookup-key nskk-mode-map (kbd ,kbd-key)) ',handler)))

     (nskk-it ,(format "%s is bound to %s in nskk-mode-map" arrow-key handler)
       (should (eq (lookup-key nskk-mode-map (kbd ,arrow-key)) ',handler)))

     (nskk-it ,(format "commits then %s when converting" nav-fn)
       (let ((commit-called nil)
             (nav-called nil))
         (nskk-with-mocks ((nskk-converting-p (lambda () t))
                           (nskk--has-preedit (lambda () nil))
                           (nskk-commit-current (lambda () (setq commit-called t)))
                           (,nav-fn (lambda (&rest _) (interactive) (setq nav-called t))))
           (call-interactively ',handler))
         (should commit-called)
         (should nav-called)))

     (nskk-it ,(format "calls %s when not converting (normal state)" nav-fn)
       (let ((nav-called nil))
         (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                           (nskk--has-preedit (lambda () nil))
                           (,nav-fn (lambda (&rest _) (interactive) (setq nav-called t))))
           (let ((nskk-current-state (nskk-state-create)))
             (call-interactively ',handler)))
         (should nav-called)))

     (nskk-it ,(format "calls %s when nskk-current-state is nil" nav-fn)
       (let ((nav-called nil))
         (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                           (nskk--has-preedit (lambda () nil))
                           (,nav-fn (lambda (&rest _) (interactive) (setq nav-called t))))
           (let ((nskk-current-state nil))
             (call-interactively ',handler)))
         (should nav-called)))

     (nskk-it ,(format "calls %s (not commit) in preedit state" nav-fn)
       (let ((commit-called nil)
             (nav-called nil))
         (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                           (nskk--has-preedit (lambda () t))
                           (nskk-commit-current (lambda () (setq commit-called t)))
                           (,nav-fn (lambda (&rest _) (interactive) (setq nav-called t))))
           (call-interactively ',handler))
         (should-not commit-called)
         (should nav-called)))))

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
;;; nskk--l-key-dispatch-state
;;;

(nskk-describe "nskk--l-key-dispatch-state"
  (nskk-it "is defined as a callable function (fboundp)"
    (should (fboundp 'nskk--l-key-dispatch-state)))

  (nskk-it "returns (azik . azik-complete) when azik style and complete match"
    (let ((nskk-converter-romaji-style 'azik))
      (nskk-with-mocks ((nskk--azik-complete-match-p (lambda (_) t)))
        (should (equal (nskk--l-key-dispatch-state) '(azik . azik-complete))))))

  (nskk-it "returns (azik . other) when azik style but no complete match"
    (let ((nskk-converter-romaji-style 'azik))
      (nskk-with-mocks ((nskk--azik-complete-match-p (lambda (_) nil)))
        (should (equal (nskk--l-key-dispatch-state) '(azik . other))))))

  (nskk-it "returns (standard . other) when standard style"
    (let ((nskk-converter-romaji-style 'standard))
      (should (equal (nskk--l-key-dispatch-state) '(standard . other)))))

  (nskk-it "does not call nskk--azik-complete-match-p in standard mode"
    ;; Ensure the azik-complete check is gated on style being azik.
    (let ((nskk-converter-romaji-style 'standard)
          (check-called nil))
      (nskk-with-mocks ((nskk--azik-complete-match-p (lambda (_) (setq check-called t) nil)))
        (nskk--l-key-dispatch-state)
        (should-not check-called)))))

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
        (nskk-with-mocks ((nskk-previous-candidate (lambda () (setq prev-candidate-called t))))
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
        (nskk-with-mocks ((nskk-core-search/k
                           (lambda (_k _t _l on-found _on-not-found)
                             (funcall on-found '("result")))))
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
        (nskk-with-mocks ((nskk-next-candidate (lambda () (setq next-candidate-called t))))
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
         (should (equal (buffer-string) "result"))))))

  (nskk-it "key-action/3 has no explicit preedit row for return (wildcard fires)"
    ;; Intentional: no (return preedit ...) rule; the _ arm in nskk-handle-return
    ;; calls (newline) for preedit state. This matches DDSKK behavior.
    (should-not (nskk-prolog-query-value
                 `(key-action return preedit ,'\?action) '\?action))))

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
      (nskk-with-mocks ((keyboard-quit (lambda () (setq quit-called t))))
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
        (nskk-with-mocks ((nskk-cancel-conversion-to-reading (lambda () (setq cancel-called t))))
          (nskk-when (nskk-handle-cancel))
          (nskk-then (should cancel-called))))))

  (nskk-it "calls nskk-cancel-preedit when in preedit state"
    (let ((cancel-called nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit (lambda () t))
                        (nskk-cancel-preedit (lambda () (setq cancel-called t))))
        (nskk-handle-cancel))
      (should cancel-called))))

;;;
;;; nskk--current-kakutei-state Tests
;;;

(nskk-describe "nskk--current-kakutei-state behavior"
  (nskk-it "returns 'converting when nskk-converting-p is true"
    (nskk-with-mocks ((nskk-converting-p (lambda () t))
                      (nskk--has-preedit (lambda () nil)))
      (should (eq (nskk--current-kakutei-state) 'converting))))

  (nskk-it "returns 'preedit when nskk--has-preedit is true"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk--has-preedit (lambda () t)))
      (should (eq (nskk--current-kakutei-state) 'preedit))))

  (nskk-it "returns 'romaji-pending when nskk--romaji-buffer is non-empty"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk--has-preedit (lambda () nil)))
      (let ((nskk--romaji-buffer "k"))
        (should (eq (nskk--current-kakutei-state) 'romaji-pending)))))

  (nskk-it "returns 'hiragana-idle in hiragana mode with no pending input"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk--has-preedit (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (nskk--romaji-buffer ""))
        (should (eq (nskk--current-kakutei-state) 'hiragana-idle)))))

  (nskk-it "returns 'katakana-idle in fullwidth katakana mode"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk--has-preedit (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'katakana))
            (nskk--romaji-buffer ""))
        (should (eq (nskk--current-kakutei-state) 'katakana-idle)))))

  (nskk-it "returns 'katakana-idle in half-width katakana mode"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk--has-preedit (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'katakana-半角))
            (nskk--romaji-buffer ""))
        (should (eq (nskk--current-kakutei-state) 'katakana-idle)))))

  (nskk-it "returns 'direct-idle in ascii mode with no pending input"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk--has-preedit (lambda () nil)))
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
        (nskk-with-mocks ((next-line (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-n))
          (nskk-then (should called))))))

  (nskk-it "calls next-line when nskk-current-state is nil"
    (with-temp-buffer
      (let ((nskk-current-state nil)
            (called nil))
        (nskk-with-mocks ((next-line (lambda (&rest _) (interactive) (setq called t))))
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

  (nskk-it "commits candidate and calls previous-line when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((nskk-commit-current (lambda () (setq called t))))
          (nskk-when (nskk-handle-ctrl-p))
          (nskk-then (should called))))))

  (nskk-it "calls previous-line when not in conversion mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (called nil))
        (nskk-with-mocks ((previous-line (lambda (&rest _) (interactive) (setq called t))))
          (nskk-when (nskk-handle-ctrl-p))
          (nskk-then (should called))))))

  (nskk-it "calls previous-line when nskk-current-state is nil"
    (with-temp-buffer
      (let ((nskk-current-state nil)
            (called nil))
        (nskk-with-mocks ((previous-line (lambda (&rest _) (interactive) (setq called t))))
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
  (nskk-deftest-nav-handler ctrl-f nskk-handle-ctrl-f "C-f" "<right>" forward-char))

(nskk-describe "nskk-handle-ctrl-b behavior"
  (nskk-deftest-nav-handler ctrl-b nskk-handle-ctrl-b "C-b" "<left>" backward-char))

;;;
;;; nskk--current-key-state Tests
;;;
;; These tests cover the abbrev-mode branch added to nskk--current-key-state:
;; when mode is 'abbrev AND the conversion-start marker is set, the function
;; must return 'preedit (so SPC dispatches 'start-conversion) rather than
;; falling through to 'normal (which would self-insert a space).

(nskk-describe "nskk--current-key-state behavior"
  (nskk-it "returns 'converting when nskk-converting-p is true"
    (nskk-with-mocks ((nskk-converting-p (lambda () t))
                      (nskk--has-preedit (lambda () nil))
                      (nskk--get-conversion-start (lambda () nil)))
      (should (eq (nskk--current-key-state) 'converting))))

  (nskk-it "returns 'preedit when nskk--has-preedit is true"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk--has-preedit (lambda () t))
                      (nskk--get-conversion-start (lambda () nil)))
      (should (eq (nskk--current-key-state) 'preedit))))

  (nskk-it "returns 'normal in hiragana mode with no preedit"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk--has-preedit (lambda () nil))
                      (nskk--get-conversion-start (lambda () nil)))
      (let ((nskk-current-state (nskk-state-create 'hiragana)))
        (should (eq (nskk--current-key-state) 'normal)))))

  (nskk-context "abbrev mode + marker"
    (nskk-it "returns 'preedit in abbrev mode when conversion marker is set"
      (with-temp-buffer
        (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                          (nskk--has-preedit (lambda () nil)))
          (let ((nskk-current-state (nskk-state-create 'abbrev)))
            (nskk--set-conversion-start-marker (point-min))
            (should (eq (nskk--current-key-state) 'preedit))))))

    (nskk-it "returns 'normal in abbrev mode when no marker is set"
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit (lambda () nil))
                        (nskk--get-conversion-start (lambda () nil)))
        (let ((nskk-current-state (nskk-state-create 'abbrev)))
          (should (eq (nskk--current-key-state) 'normal)))))

    (nskk-it "returns 'normal for latin mode even with a marker (abbrev-only branch)"
      (with-temp-buffer
        (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                          (nskk--has-preedit (lambda () nil)))
          (let ((nskk-current-state (nskk-state-create 'latin)))
            (nskk--set-conversion-start-marker (point-min))
            (should (eq (nskk--current-key-state) 'normal))))))

    (nskk-it "abbrev-with-marker branch fires before 'normal fallthrough (regression)"
      (with-temp-buffer
        (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                          (nskk--has-preedit (lambda () nil)))
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
        (nskk-with-mocks ((nskk-prolog-query-value (lambda (&rest _) (setq prolog-called t) nil)))
          (nskk-when (nskk-self-insert 1))
          (nskk-then
           (should-not prolog-called)
           (should (> (buffer-size) 0)))))))

  (nskk-it "bypasses Prolog for 'n' in abbrev mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'abbrev))
            (last-command-event ?n)
            (prolog-called nil))
        (nskk-with-mocks ((nskk-prolog-query-value (lambda (&rest _) (setq prolog-called t) nil)))
          (nskk-when (nskk-self-insert 1))
          (nskk-then
           (should-not prolog-called)
           (should (> (buffer-size) 0))))))))

;;;
;;; nskk-handle-ctrl-a behavior
;;;

(nskk-describe "nskk-handle-ctrl-a behavior"
  (nskk-deftest-nav-handler ctrl-a nskk-handle-ctrl-a "C-a" "<home>" beginning-of-line))

;;;
;;; nskk-handle-ctrl-e behavior
;;;

(nskk-describe "nskk-handle-ctrl-e behavior"
  (nskk-deftest-nav-handler ctrl-e nskk-handle-ctrl-e "C-e" "<end>" end-of-line))

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
        (nskk-with-mocks ((nskk-cancel-preedit (lambda () (setq cancel-preedit-called t))))
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
        (nskk-with-mocks ((nskk-cancel-conversion-to-reading (lambda () (setq cancel-called t))))
          (nskk-when (nskk-handle-backspace))
          (nskk-then (should cancel-called))))))

  (nskk-it "deletes backward char when no preedit (normal state)"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii)))
        (insert "abc")
        (nskk-when (nskk-handle-backspace))
        (nskk-then (should (equal (buffer-string) "ab")))))))

;;;
;;; nskk--backspace-in-preedit
;;;

(nskk-describe "nskk--backspace-in-preedit"
  (nskk-it "is defined as a callable function (fboundp)"
    (should (fboundp 'nskk--backspace-in-preedit)))

  (nskk-it "deletes last character when preedit has content"
    ;; Preedit has content: point is beyond start + marker length → delete-char -1 fires.
    (let ((deleted nil))
      (nskk-with-mocks ((nskk--get-conversion-start (lambda () (point-min)))
                        (delete-char (lambda (n) (setq deleted n))))
        (with-temp-buffer
          (insert "▽ab")
          (goto-char (point-max))
          (let ((nskk-henkan-on-marker "▽"))
            (nskk--backspace-in-preedit))))
      (should (equal deleted -1))))

  (nskk-it "calls nskk-cancel-preedit when preedit is empty (point at marker boundary)"
    (let ((cancel-called nil))
      (nskk-with-mocks ((nskk--get-conversion-start (lambda () 1))
                        (nskk-cancel-preedit (lambda () (setq cancel-called t))))
        ;; point == start + length("▽") means no content chars → cancel
        (with-temp-buffer
          (insert "▽")
          (goto-char (point-max))
          (let ((nskk-henkan-on-marker "▽"))
            (nskk--backspace-in-preedit)))
        (should cancel-called)))))

;;;
;;; nskk-handle-tab behavior
;;;

(nskk-describe "nskk-handle-tab behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-tab))
    (should (commandp 'nskk-handle-tab)))

  (nskk-it "TAB is bound in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map (kbd "TAB"))))
      (should (eq binding 'nskk-handle-tab))))

  (nskk-it "calls nskk-dynamic-complete when preedit active"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (complete-called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "▽ka")
        (nskk-state-set-henkan-phase nskk-current-state 'on)
        (nskk-with-mocks ((nskk-dynamic-complete (lambda () (setq complete-called t))))
          (nskk-when (nskk-handle-tab))
          (nskk-then (should complete-called))))))

  (nskk-it "calls indent-for-tab-command when not in preedit"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (indent-called nil))
        (nskk-with-mocks ((indent-for-tab-command (lambda (&rest _) (interactive) (setq indent-called t))))
          (nskk-when (nskk-handle-tab))
          (nskk-then (should indent-called))))))

  (nskk-it "calls indent-for-tab-command when converting (pass-through rule)"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (indent-called nil))
        (nskk--set-conversion-start-marker (point-min))
        (insert "preedit")
        (nskk-state-set-candidates nskk-current-state '("result"))
        (nskk-state-force-henkan-phase nskk-current-state 'active)
        (nskk-with-mocks ((indent-for-tab-command (lambda (&rest _) (interactive) (setq indent-called t))))
          (nskk-when (nskk-handle-tab))
          (nskk-then (should indent-called)))))))

;;;
;;; nskk-handle-hash behavior
;;;

(nskk-describe "nskk-handle-hash behavior"
  (nskk-it "is defined and interactive"
    (should (fboundp 'nskk-handle-hash))
    (should (commandp 'nskk-handle-hash)))

  (nskk-it "# is bound in nskk-mode-map"
    (let ((binding (lookup-key nskk-mode-map (kbd "#"))))
      (should (eq binding 'nskk-handle-hash))))

  (nskk-it "calls nskk-set-mode-numeric when in hiragana"
    ;; nskk-set-mode-numeric reuses abbrev mode internally (sets mode='abbrev
    ;; and nskk--numeric-mode flag).  We verify the right function is called.
    (let ((nskk-current-state (nskk-state-create 'hiragana))
          (numeric-called nil))
      (nskk-with-mocks ((nskk-set-mode-numeric (lambda () (setq numeric-called t))))
        (nskk-when (nskk-handle-hash))
        (nskk-then (should numeric-called)))))

  (nskk-it "self-inserts '#' when in ascii mode"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'ascii))
            (last-command-event ?#))
        (nskk-when (nskk-handle-hash))
        (nskk-then (should (equal (buffer-string) "#"))))))

  (nskk-it "does implicit kakutei then calls nskk-set-mode-numeric when converting"
    (with-temp-buffer
      (let ((nskk-current-state (nskk-state-create 'hiragana))
            (numeric-called nil))
        (nskk-given (progn
                      (nskk--set-conversion-start-marker (point-min))
                      (insert "preedit")
                      (nskk-state-set-candidates nskk-current-state '("result"))
                      (nskk-state-force-henkan-phase nskk-current-state 'active)))
        (nskk-with-mocks ((nskk-set-mode-numeric (lambda () (setq numeric-called t))))
          (nskk-when (nskk-handle-hash))
          (nskk-then
           (should-not (nskk-converting-p))
           (should numeric-called)))))))

;;;
;;; key-action/3 Prolog Dispatch Table Integrity Tests
;;;
;; Exhaustively verify every (key state) pair in the fact table maps to the
;; expected action symbol.  This is a white-box integrity check ensuring
;; the data layer is self-consistent and complete.

(nskk-describe "key-action/3 Prolog dispatch table integrity"
  (nskk-deftest-table keymap-prolog-key-action-table
    :description "key-action/3 maps (key state) to expected action"
    :columns (key state expected-action)
    :rows (;; Space
           (space converting next-candidate)
           (space preedit   start-conversion)
           (space normal    self-insert)
           ;; Return
           (return converting commit-candidate)
           (return normal   newline)
           ;; Cancel
           (cancel converting rollback-to-reading)
           (cancel preedit   cancel-preedit)
           (cancel normal    keyboard-quit)
           ;; X
           (x converting previous-candidate)
           (x normal    self-insert)
           ;; C-n
           (ctrl-n converting kakutei-then-next-line)
           (ctrl-n preedit    next-line)
           (ctrl-n normal     next-line)
           ;; C-p
           (ctrl-p converting kakutei-then-previous-line)
           (ctrl-p preedit    previous-line)
           (ctrl-p normal     previous-line)
           ;; C-f
           (ctrl-f converting kakutei-then-forward)
           (ctrl-f preedit    forward-char)
           (ctrl-f normal     forward-char)
           ;; C-b
           (ctrl-b converting kakutei-then-backward)
           (ctrl-b preedit    backward-char)
           (ctrl-b normal     backward-char)
           ;; C-a
           (ctrl-a converting kakutei-then-bol)
           (ctrl-a preedit    beginning-of-line)
           (ctrl-a normal     beginning-of-line)
           ;; C-e
           (ctrl-e converting kakutei-then-eol)
           (ctrl-e preedit    end-of-line)
           (ctrl-e normal     end-of-line)
           ;; Backspace
           (backspace preedit    delete-preedit-char)
           (backspace converting rollback-to-reading)
           (backspace normal     backward-delete)
           ;; Tab
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

  (nskk-it "standard style maps to latin-mode for any buf-state (wildcard \\?buf)"
    ;; The (standard \?buf latin-mode) wildcard matches any buf-state value.
    (should (eq (nskk-prolog-query-value
                 `(l-key-action standard azik-complete ,'\?action) '\?action)
                'latin-mode))))

;;;
;;; preedit-marker-mode/1 Prolog Table Tests
;;;

(nskk-describe "preedit-marker-mode/1 Prolog table integrity"
  (nskk-it "abbrev mode is a preedit-marker-mode"
    (should (nskk-prolog-holds-p '(preedit-marker-mode abbrev))))

  (nskk-it "hiragana mode is a preedit-marker-mode"
    (should (nskk-prolog-holds-p '(preedit-marker-mode hiragana))))

  (nskk-it "katakana mode is a preedit-marker-mode"
    (should (nskk-prolog-holds-p '(preedit-marker-mode katakana))))

  (nskk-it "katakana hankaku mode is a preedit-marker-mode"
    (should (nskk-prolog-holds-p '(preedit-marker-mode katakana-半角))))

  (nskk-it "ascii mode is NOT a preedit-marker-mode"
    (should-not (nskk-prolog-holds-p '(preedit-marker-mode ascii))))

  (nskk-it "latin mode is NOT a preedit-marker-mode"
    (should-not (nskk-prolog-holds-p '(preedit-marker-mode latin)))))

;;;
;;; nskk-define-key-handler
;;;

(nskk-describe "nskk-define-key-handler"
  (nskk-it "is a macro (not a plain function)"
    (should (macrop 'nskk-define-key-handler)))

  (nskk-it "generates interactive commands for key handlers"
    ;; All handlers created by nskk-define-key-handler should be interactive
    (should (commandp 'nskk-handle-space))
    (should (commandp 'nskk-handle-return))
    (should (commandp 'nskk-handle-x))
    (should (commandp 'nskk-handle-ctrl-f))
    (should (commandp 'nskk-handle-ctrl-b)))

  (nskk-it "all generated handlers follow the nskk-handle-KEY naming convention"
    (dolist (handler '(nskk-handle-space nskk-handle-return nskk-handle-x
                       nskk-handle-ctrl-n nskk-handle-ctrl-p
                       nskk-handle-ctrl-f nskk-handle-ctrl-b
                       nskk-handle-ctrl-a nskk-handle-ctrl-e))
      (should (fboundp handler)))))

;;;
;;; nskk--japanese-mode-class Tests
;;;

(nskk-describe "nskk--japanese-mode-class"
  (nskk-it "returns 'converting when in henkan-active state"
    (nskk-with-mocks ((nskk-converting-p (lambda () t))
                      (nskk--has-preedit (lambda () nil)))
      (should (eq (nskk--japanese-mode-class) 'converting))))

  (nskk-it "returns 'preedit-japanese when preedit is active in hiragana mode"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit (lambda () t)))
        (should (eq (nskk--japanese-mode-class) 'preedit-japanese)))))

  (nskk-it "returns 'other when preedit is active in abbrev mode (not Japanese)"
    (let ((nskk-current-state (nskk-state-create 'abbrev)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit (lambda () t)))
        (should (eq (nskk--japanese-mode-class) 'other)))))

  (nskk-it "returns 'idle-japanese when in hiragana mode with no preedit"
    (let ((nskk-current-state (nskk-state-create 'hiragana)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit (lambda () nil)))
        (should (eq (nskk--japanese-mode-class) 'idle-japanese)))))

  (nskk-it "returns 'other when in ascii mode"
    (let ((nskk-current-state (nskk-state-create 'ascii)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit (lambda () nil)))
        (should (eq (nskk--japanese-mode-class) 'other)))))

  (nskk-it "returns 'other when nskk-current-state is nil"
    (let ((nskk-current-state nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit (lambda () nil)))
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
                        (nskk--has-preedit (lambda () nil)))
        (nskk--with-japanese-mode/k
         (lambda (_) (setq found-called t))
         (lambda () nil)))
      (should found-called)))

  (nskk-it "calls on-not-found in ascii mode"
    (let ((not-found-called nil)
          (nskk-current-state (nskk-state-create 'ascii)))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit (lambda () nil)))
        (nskk--with-japanese-mode/k
         (lambda (_) nil)
         (lambda () (setq not-found-called t))))
      (should not-found-called)))

  (nskk-it "executes commit-current pre-action then calls on-found in converting state"
    (let ((commit-called nil)
          (found-called nil))
      (nskk-with-mocks ((nskk-converting-p (lambda () t))
                        (nskk--has-preedit (lambda () nil))
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
    (should (macrop 'nskk-define-mode-switch-handler)))

  (nskk-it "generates interactive commands for mode-switch handlers"
    (should (commandp 'nskk-handle-q))
    (should (commandp 'nskk-handle-upper-l))
    (should (commandp 'nskk-handle-slash))
    (should (commandp 'nskk-handle-hash)))

  (nskk-it "all generated handlers follow the nskk-handle-KEY naming convention"
    (dolist (handler '(nskk-handle-q nskk-handle-upper-l
                       nskk-handle-slash nskk-handle-hash))
      (should (fboundp handler)))))

;;;
;;; nskk--key-state-base Tests
;;;

(nskk-describe "nskk--key-state-base"
  (nskk-it "returns 'converting when nskk-converting-p is true"
    (nskk-with-mocks ((nskk-converting-p (lambda () t))
                      (nskk--has-preedit (lambda () nil)))
      (should (eq (nskk--key-state-base) 'converting))))

  (nskk-it "returns 'preedit when has-preedit is true but not converting"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk--has-preedit (lambda () t)))
      (should (eq (nskk--key-state-base) 'preedit))))

  (nskk-it "returns nil when neither converting nor preedit"
    (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                      (nskk--has-preedit (lambda () nil)))
      (should-not (nskk--key-state-base))))

  (nskk-it "converting takes priority over preedit"
    (nskk-with-mocks ((nskk-converting-p (lambda () t))
                      (nskk--has-preedit (lambda () t)))
      (should (eq (nskk--key-state-base) 'converting)))))

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
;;; nskk--setup-azik-toggle-key
;;;

(nskk-describe "nskk--setup-azik-toggle-key"
  (nskk-it "is defined as a callable function (fboundp)"
    (should (fboundp 'nskk--setup-azik-toggle-key)))

  (nskk-it "does nothing when nskk-azik-keyboard-type is not bound"
    ;; Without nskk-azik loaded, the variable is unbound — the function is a no-op
    (if (boundp 'nskk-azik-keyboard-type)
        ;; AZIK is loaded: function will run and bind keys; just verify no error
        (progn (nskk--setup-azik-toggle-key) t)
      ;; AZIK not loaded: function should be a silent no-op returning nil
      (should (null (nskk--setup-azik-toggle-key)))))

  (nskk-it "binds @ key when keyboard type is jp106"
    (when (boundp 'nskk-mode-map)
      (let* ((saved-binding (keymap-lookup nskk-mode-map "@"))
             (nskk-azik-keyboard-type 'jp106))
        (unwind-protect
            (progn
              (nskk--setup-azik-toggle-key)
              (should (eq (keymap-lookup nskk-mode-map "@")
                          #'nskk-toggle-japanese-mode)))
          ;; Restore original binding
          (when saved-binding
            (keymap-set nskk-mode-map "@" saved-binding))))))

  (nskk-it "binds [ key when keyboard type is us101"
    (when (boundp 'nskk-mode-map)
      (let* ((saved-binding (keymap-lookup nskk-mode-map "["))
             (nskk-azik-keyboard-type 'us101))
        (unwind-protect
            (progn
              (nskk--setup-azik-toggle-key)
              (should (eq (keymap-lookup nskk-mode-map "[")
                          #'nskk-toggle-japanese-mode)))
          (when saved-binding
            (keymap-set nskk-mode-map "[" saved-binding))))))

  (nskk-it "binds @ key for unknown keyboard type (default fallback)"
    (let ((test-map (make-sparse-keymap))
          (nskk-azik-keyboard-type 'unknown-type))
      (cl-letf (((symbol-value 'nskk-mode-map) test-map))
        (nskk--setup-azik-toggle-key)
        (should (eq (lookup-key test-map "@") #'nskk-toggle-japanese-mode))))))

;;;
;;; nskk--key-state-base Tests
;;;

(nskk-describe "nskk--key-state-base"
  (nskk-context "converting state"
    (nskk-it "returns converting when nskk-converting-p returns non-nil"
      (nskk-with-mocks ((nskk-converting-p (lambda () t))
                        (nskk--has-preedit  (lambda () nil)))
        (should (eq (nskk--key-state-base) 'converting))))

    (nskk-it "returns converting when both nskk-converting-p and nskk--has-preedit are true"
      (nskk-with-mocks ((nskk-converting-p (lambda () t))
                        (nskk--has-preedit  (lambda () t)))
        (should (eq (nskk--key-state-base) 'converting)))))

  (nskk-context "preedit state"
    (nskk-it "returns preedit when nskk--has-preedit returns non-nil and not converting"
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit  (lambda () t)))
        (should (eq (nskk--key-state-base) 'preedit)))))

  (nskk-context "nil state"
    (nskk-it "returns nil when neither nskk-converting-p nor nskk--has-preedit is non-nil"
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit  (lambda () nil)))
        (should (null (nskk--key-state-base))))))

  (nskk-context "return type invariant"
    (nskk-it "return value is always a symbol (converting, preedit) or nil"
      (nskk-with-mocks ((nskk-converting-p (lambda () t))
                        (nskk--has-preedit  (lambda () nil)))
        (let ((result (nskk--key-state-base)))
          (should (or (null result) (symbolp result)))))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit  (lambda () t)))
        (let ((result (nskk--key-state-base)))
          (should (or (null result) (symbolp result)))))
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit  (lambda () nil)))
        (let ((result (nskk--key-state-base)))
          (should (null result))))))

  (nskk-context "error safety"
    (nskk-it "does not signal an error for any combination of inputs"
      ;; nskk-should-not-error returns the body value, which may be nil when neither
      ;; converting nor preedit.  Use condition-case directly so nil return is fine.
      (nskk-with-mocks ((nskk-converting-p (lambda () nil))
                        (nskk--has-preedit  (lambda () nil)))
        (condition-case err
            (nskk--key-state-base)
          (error (ert-fail (format "Unexpected error: %s" err)))))
      (nskk-with-mocks ((nskk-converting-p (lambda () t))
                        (nskk--has-preedit  (lambda () nil)))
        (condition-case err
            (nskk--key-state-base)
          (error (ert-fail (format "Unexpected error: %s" err))))))))

;;;
;;; Property-based invariant tests
;;;

(nskk-describe "nskk-mode-map structural invariants"
  (nskk-context "all bindings are callable"
    (nskk-it "every command bound in nskk-mode-map satisfies fboundp"
      (map-keymap
       (lambda (_key cmd)
         (when (symbolp cmd)
           (should (fboundp cmd))))
       nskk-mode-map)))

  (nskk-context "map is non-empty"
    (nskk-it "nskk-mode-map is a non-empty keymap"
      (should (keymapp nskk-mode-map))
      (let ((count 0))
        (map-keymap (lambda (_k _v) (cl-incf count)) nskk-mode-map)
        (should (> count 0))))))

;; Exhaustive test: for every critical key, the binding in nskk-mode-map
;; is a symbol that satisfies fboundp after nskk-mode is enabled.
(nskk-property-test-exhaustive keymap-critical-keys-survive-mode-enable
  '("C-j" "L" "C-g")
  (with-temp-buffer
    (nskk-mode 1)
    (let ((binding (lookup-key nskk-mode-map (kbd item))))
      (nskk-mode -1)
      (and binding (symbolp binding) (fboundp binding)))))

(provide 'nskk-keymap-test)

;;; nskk-keymap-test.el ends here
